
# setup
library(dplyr)
library(ggplot2)
library(thematic)
library(lubridate)
library(scales)
library(tictoc)
# prophet modeling
library(prophet)
library(dygraphs)
# shiny and UI extras
library(shiny)
library(shinydashboard)
library(bslib)
library(bsicons)
#library(shinydashboardPlus)
#library(shinythemes)
library(shinyjs)
#library(rintrojs)
library(shinycssloaders)
library(shinyWidgets)

#### Setup ####
set.seed(12923)
getwd()
#setwd("/Users/natebender/Desktop/repo/r_learning_overall/r_learning/")
df <- read.csv("data/df_timeseries_orig.csv")
df <- df %>%
  mutate(
    bldg_name = as.factor(bldg_name),
    year = factor(year, levels = sort(unique(year)), ordered = TRUE),  # Dynamic levels for year
    date = as.Date(date),
    month = factor(month, levels = month.abb, ordered = TRUE),  # Use month.abb for abbreviated month names
    day = factor(day, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"), ordered = TRUE)  # Abbreviated day names
  )

forecast_static_min <- max(df$date)+1 # possible predictions start at latest historical day +1
potential_forecast_window <- 365*3 # can forecast up to three years beyond the day after the latest historical date
potential_forecast_window_end <- forecast_static_min + potential_forecast_window


#### Func: Summary boxes #### 
create_stat_box_outputs <- function(output, summaryboxes_filter, df, stat_type, input, session) {
  # stat_type should be one of "mean", "min", "max"
  
  # Value Output
  output[[paste0("box", stat_type, "_value")]] <- renderText({
    current_value <- match.fun(stat_type)(summaryboxes_filter()[[input$variable]])
    round(current_value, 2)
  })
  
  # Icon Output
  output[[paste0("box", stat_type, "_icon")]] <- renderUI({
    temp_ovrall <- df %>% 
      filter(bldg_name == input$eda_building)
    overall_value <- match.fun(stat_type)(temp_ovrall[[input$variable]])
    pct_change <- calc_pct_chng(match.fun(stat_type)(summaryboxes_filter()[[input$variable]]), overall_value)
    
    icon_color_up <- "#00cb21"
    icon_color_down <- "#ff6969"
    icon_color_equal <- "#bababa"
    
    icon_name <- ifelse(pct_change > 0, "chevron-up", ifelse(pct_change < 0, "chevron-down", "equals"))
    icon_color <- ifelse(pct_change > 0, icon_color_up, ifelse(pct_change < 0, icon_color_down, icon_color_equal))
    
    shiny::icon(icon_name, style = if (!is.null(icon_color)) paste0("color:", icon_color))
  })
  
  output[[paste0("box", stat_type, "_pctchange")]] <- renderText({
    temp_ovrall <- df %>%
      filter(bldg_name == input$eda_building)
    current_values <- summaryboxes_filter()[[input$variable]]
    overall_value <- match.fun(stat_type)(temp_ovrall[[input$variable]], na.rm = TRUE)
    pct_change <- calc_pct_chng(match.fun(stat_type)(current_values, na.rm = TRUE), overall_value)
    
    if (is.na(pct_change)) {
      return("NA or calculation error")
    } else {
      # Determine the comparison text based on whether current_value is higher or lower than overall_value
      comparison_text <- ifelse(pct_change > 0, "% higher than overall: ", "% lower than overall: ")
      return(paste0(round(abs(pct_change), 1), comparison_text, round(overall_value, 2)))
    }
  })
}

# Function to calculate percentage change
calc_pct_chng <- function(current_value, overall_value) {
  tiny_value <- 1e-6  # Small value to avoid division by zero
  
  if (overall_value == 0) {
    overall_value <- tiny_value
  }
  
  if (is.na(current_value) || is.na(overall_value)) {
    return(NA)
  } else {
    return((current_value - overall_value) / overall_value * 100)
  }
}

# Func: Energy Prices ########################################
# ************************************************************
generate_energyprices <- function(df, total_dates, potential_total_length, useradjust_energyprices) {
  # Get the latest price from historical data
  current_price <- df %>%
    filter(date == max(date)) %>%
    pull(price_per_kwh)
  
  # Calculate the percentage adjustment from the user input
  pcnt_adjustment <- 1 + (useradjust_energyprices / 100)
  
  # Create a df to store future energy price data and dates for filtering later on
  # "ds" aligns with the existing filtering code used for the prophet model
  future_energy_prices <- data.frame(ds = total_dates, price_per_kwh = numeric(length = potential_total_length))
  
  for (i in 1:potential_total_length) {
    # Apply a very small daily change to simulate long-term trends
    yearly_trend <- rnorm(1, mean = 0, sd = 0.001)
    
    # Simulate daily variation with a tighter range
    daily_change <- rnorm(1, mean = 0, sd = 0.001)
    
    # Less frequent and significant random events
    if (runif(1) < 0.001) {
      event_change <- rnorm(1, mean = 0, sd = 0.002)
    } else {
      event_change <- 0
    }
    
    # Calculate the new price with tightened constraints and user adjustment
    new_price <- max(0.10, min(0.50, (current_price + yearly_trend + daily_change + event_change) * pcnt_adjustment))
    future_energy_prices$price_per_kwh[i] <- new_price
    
    # Update the current price for the next day
    current_price <- new_price
  }
  
  return(future_energy_prices)
}

# ************************************************************
# ************************************************************
# Func: Sqft Per Person ######################################
# ************************************************************

generate_sqft_per_person <- function(df, pred_building, total_dates, potential_total_length, mean_factor_weekday, mean_factor_weekend,
                                     scale_factor_weekday, scale_factor_weekend, sd_factor_weekday, sd_factor_weekend, useradjust_sqft_per_person) {
  
  pred_bldg_area <- df %>% 
    filter(bldg_name == pred_building) %>% 
    summarise(bldg_area = first(bldg_area)) %>% 
    pull(bldg_area)
  
  weekdays <- ifelse(lubridate::wday(total_dates) %in% c(1, 7), 0, 1) # 0 for weekend, 1 for weekday
  num_ppl_raw <- numeric(length = potential_total_length)
  sqft_per_person <- numeric(length = potential_total_length)
  pcnt_adjustment = 1 - (useradjust_sqft_per_person / 100)
  
  for (i in 1:(potential_total_length)) {
    if (weekdays[i] == 1) {
      # Generate more people on weekdays
      mean_factor <- (300 + mean_factor_weekday[pred_building]) * pcnt_adjustment
      scale_factor <- 5 + scale_factor_weekday[pred_building]
      sd_factor <- 20 + sd_factor_weekday[pred_building]   
    } else {
      # Generate fewer people on weekends
      mean_factor <- (70 + mean_factor_weekend[pred_building]) * pcnt_adjustment
      scale_factor <- 5 + scale_factor_weekend[pred_building]
      sd_factor <- 10 + sd_factor_weekend[pred_building]   
    }
    
    num_ppl_raw[i] <- as.integer(pmax(0, pmin(500, rgamma(1, shape = 2, scale = scale_factor) + 1 +
                                                abs(round(rnorm(1, mean = mean_factor, sd = sd_factor), 0)))))
    
    sqft_per_person[i] <- if (num_ppl_raw[i] > 0) pred_bldg_area / num_ppl_raw[i] else NA
    
  }
  
  return(sqft_per_person)
}

# ************************************************************
# Func: HVAC Efficiency ######################################
# ************************************************************
# Function to generate HVAC efficiency
generate_hvac_efficiency <- function(df, pred_building, total_dates, potential_total_length, useradjust_hvac_efficiency) {
  # Conversion factor (Efficiency increase per $1,000)
  #efficiency_per_thousand = 0.005
  efficiency_per_thousand = 0.25
  
  
  # Total efficiency increase based on user investment
  total_efficiency_change = useradjust_hvac_efficiency * efficiency_per_thousand / 1000
  
  # Get the latest HVAC efficiency from historical data
  latest_hvac_efficiency <- df %>%
    filter(bldg_name == pred_building, date == max(date)) %>%
    pull(hvac_efficiency)
  
  hvac_efficiency <- numeric(length = potential_total_length) # initialize placeholder var
  current_hvac <- latest_hvac_efficiency
  
  # Simulate the efficiency rating for this building across the total forecast length
  for (i in 1:potential_total_length) {
    # Apply a yearly decline to simulate aging
    yearly_decline <- (-.02) # add a very small daily negative constant to simulate long-term degradation
    # Simulate daily variation
    daily_change <- sample(c(-.20, 0, .20), 1, prob = c(0.045, 0.96, 0.005)) # Mostly no daily change, slightly better chance of small bad change
    investment <- sample(c(0, total_efficiency_change), 1, prob = c(0.95, 0.05)) # simulating if the investment or disinvestment gets applied
    # Random events: significant increase or decrease
    if (runif(1) < 0.004) { # very low chance for a significant positive or negative change to efficiency
      event_change <- sample(-10:10, 1)
    } else {
      event_change <- 0
    }
    
    # Calculate the new efficiency
    new_hvac <- max(1, min(100, current_hvac + yearly_decline + daily_change + event_change + investment))
    hvac_efficiency[i] <- new_hvac
    
    # Update the current efficiency for the next day
    current_hvac <- new_hvac
  }
  return(hvac_efficiency)
}

# Func: Equipment Efficiency ####
# Function to generate the equipment efficiency rating
generate_equip_efficiency <- function(df, pred_building, total_dates, potential_total_length, useradjust_equip_efficiency) {
  # Conversion factor (Efficiency increase per $1,000)
  #efficiency_per_thousand = 0.003
  efficiency_per_thousand = 0.15
  
  # Total efficiency increase based on user investment
  total_efficiency_change = useradjust_equip_efficiency * efficiency_per_thousand / 1000
  
  # Get the latest HVAC efficiency from historical data
  latest_equip_efficiency <- df %>%
    filter(bldg_name == pred_building, date == max(date)) %>%
    pull(equip_efficiency)
  
  equip_efficiency <- numeric(length = potential_total_length) # initialize placeholder var
  current_equip_efficiency <- latest_equip_efficiency
  
  # Simulate the efficiency rating for this building across the total forecast length
  for (i in 1:potential_total_length) {
    # Apply a yearly decline to simulate aging
    yearly_decline <- (-.02) # add a very small daily negative constant to simulate long-term degradation
    # Simulate daily variation
    daily_change <- sample(c(-.20, 0, .20), 1, prob = c(0.045, 0.96, 0.005)) # Mostly no daily change, slightly better chance of small bad change
    investment <- sample(c(0, total_efficiency_change), 1, prob = c(0.95, 0.05)) # simulating if the investment or disinvestment gets applied
    # Random events: significant increase or decrease
    if (runif(1) < 0.004) { # very low chance for a significant positive or negative change to efficiency
      event_change <- sample(-10:10, 1)
    } else {
      event_change <- 0
    }
    
    # Calculate the new efficiency
    new_equip_efficiency <- max(1, min(100, current_equip_efficiency + yearly_decline + daily_change + event_change + investment))
    equip_efficiency[i] <- new_equip_efficiency
    
    # Update the current efficiency for the next day
    current_equip_efficiency <- new_equip_efficiency
  }
  return(equip_efficiency)
}


# Func: Adding regressors to prophet future dataframe ####
add_future_regressor <- function(df, future_df, pred_building, variable_name, future_variable) {
  # Extract historical data for the specified variable
  historical_data <- df %>%
    filter(bldg_name == pred_building) %>%
    pull({{variable_name}})
  
  # Combine historical data with future predictions
  combined_data <- c(historical_data, future_variable)
  
  # Check if lengths match
  if (length(combined_data) != nrow(future_df)) {
    stop("Length of combined data does not match the length of the future dataframe")
  }
  
  # Add the combined data to the future dataframe
  future_df[[variable_name]] <- combined_data
  
  return(future_df)
}


# UI Improvements and Themes #####

# pretty-fy things a bit
pretty_building_names <- c("budapest" = "Grand Budapest Hotel", 
                           "nakatomi" = "Nakatomi Plaza", 
                           "wayne_manor" = "Wayne Manor")

pretty_variable_names <- c(
  "bldg_area" = "Building Area (sqft)",
  "sqft_per_person" = "Square Feet Per Person",
  "num_ppl_raw" = "Number of People",
  "temp" = "Temperature (F)", 
  "wind_speed" = "Wind Speed (mph)", 
  "cloud_cover" = "Cloud Cover (%)", 
  "equip_efficiency" = "Equipment Efficiency Rating", 
  "hvac_efficiency" = "HVAC Efficiency Rating", 
  "total" = "Total Energy Use (KWh)",
  "price_per_kwh" = "Price ($/KWh)",
  "daily_cost" = "Daily Cost ($)"
)

get_breaks <- function(variable, data) {
  if (!variable %in% names(data)) {
    print(paste("Variable", variable, "not found in data."))
    return()
  }
  
  values <- data[[variable]]
  if (is.numeric(values) && any(!is.na(values))) {
    unique_values <- unique(values)
    range_val <- range(unique_values, na.rm = TRUE)
    range_diff <- diff(range_val)
    
    if (length(unique_values) == 1) {
      # Handle single value case by creating breaks around that value
      single_value <- unique_values[1]
      seq(from = single_value - 5, to = single_value + 5, by = 1)
    } else {
      # Adjust breaks calculation for limited range variables
      if (range_diff <= 1) {
        possible_steps <- c(0.1, 0.2, 0.5)
      } else if (range_diff <= 10) {
        possible_steps <- 1
      } else if (range_diff <= 100) {
        possible_steps <- c(5, 10, 20)
      } else {
        magnitude <- floor(log10(range_diff))
        base_breaks <- 10^magnitude
        possible_steps <- c(1, 2, 5) * base_breaks
      }
      
      target_break_count <- 10
      by_interval <- possible_steps[which.min(abs(target_break_count - (range_diff / possible_steps)))]
      from <- floor(range_val[1] / by_interval) * by_interval
      to <- ceiling(range_val[2] / by_interval) * by_interval
      
      seq(from = from, to = to, by = by_interval)
    }
  } else {
    print("Data is not numeric or all values are NA.")
    return(NULL)
  }
}


eda_inputs <- list(
  
  selectInput("eda_building", "Building",
              choices = c("Grand Budapest Hotel" = "budapest",
                          "Nakatomi Plaza" = "nakatomi",
                          "Wayne Manor" = "wayne_manor")),
  selectInput("variable", "Variable",
              choices = c("Square Feet Per Person" = "sqft_per_person",
                          "Number of People" = "num_ppl_raw",
                          "Temperature (F)" = "temp",
                          "Wind Speed (mph)" = "wind_speed",
                          "Cloud Cover (%)" = "cloud_cover",
                          "Equipment Efficiency Rating" = "equip_efficiency",
                          "HVAC Efficiency Rating" = "hvac_efficiency",
                          "Building Size (Sq Ft)" = "bldg_area",
                          "Total Energy Use (KWh)" = "total")),
  sliderInput("time_period", "Time Period",
              min = min(df$date),
              max = max(df$date),
              value = c(min(df$date), max(df$date)),
              timeFormat="%Y-%m-%d",
              step = 1),
  uiOutput("day_type_selector"),
  actionButton("eda_reset", "Reset")  # Add reset button
)

scenario_inputs <- list(
  selectInput("scenario_building", "Building",
              choices = c("Grand Budapest Hotel" = "budapest",
                          "Nakatomi Plaza" = "nakatomi",
                          "Wayne Manor" = "wayne_manor")),
  # HTML for Forecast Dates label and help icon on the same line
  tags$div(
    HTML('
            <div style="display: flex; justify-content: space-between; align-items: center; margin-bottom: 0px;">
                <label for="scenario_range" style="margin-right: 3px;">Forecast Dates</label>
                <span id="scenario_dates_help" class="help-icon" style="cursor: pointer;">?</span>
            </div>')
  ),
  # Slider input without a label, as it's included in the HTML above
  sliderInput("scenario_range", label = NULL,
              min = min(df$date),
              max = max(df$date) + potential_forecast_window,
              value = c(min(df$date), max(df$date) + potential_forecast_window),
              timeFormat = "%Y-%m-%d",
              step = 1),
  tags$div(
    HTML('
            <div style="display: flex; justify-content: space-between; align-items: center; margin-bottom: 0px;">
                <label for="useradjust_sqft_per_person" style="margin-right: 3px;">Percent Adjustment: Square Feet Per Person</label>
                <span id="scenario_useradj_sqftppl" class="help-icon" style="cursor: pointer;">?</span>
            </div>')
  ),
  sliderInput("useradjust_sqft_per_person", label = NULL,
              min = -100, max = 100, value = 0, post = "%"),
  tags$div(
    HTML('
            <div style="display: flex; justify-content: space-between; align-items: center; margin-bottom: 0px;">
                <label for="useradjust_sqft_per_person" style="margin-right: 3px;">Investment: Equipment Efficiency</label>
                <span id="scenario_useradj_equip_efficiency" class="help-icon" style="cursor: pointer;">?</span>
            </div>')
  ),
  sliderInput("useradjust_equip_efficiency", label = NULL,
              min = -100000, max = 100000, value = 0, step = 100, pre = "$"),
  sliderInput("useradjust_hvac_efficiency", "Investment: HVAC Efficiency",
              min = -100000, max = 100000, value = 0, step = 100, pre = "$"),
  sliderInput("useradjust_energyprices", "Percent Adjustment: Price ($/KWh)",
              min = -100, max = 100, value = 0, post = "%"),
  actionButton("scenario_reset", "Reset"),
  actionButton("run_scenario", "Run Scenario")
)


# Themes stuff
# Enable thematic
thematic::thematic_shiny(font = "auto")
# Change ggplot2's default "gray" theme
theme_set(theme_bw(base_size = 16))

# Create dark and light themes with the custom Sass
dark <- bs_theme(
  bootswatch = "superhero",
  primary = "#384a54"
)

light <- bs_theme(
  bootswatch = "cosmo",
  primary = "#e4e4e4"
)


# UI #########################################################################
ui <- page_navbar(
  useShinyjs(),  # Initialize shinyjs
  theme = dark,
  
  tags$head(
    tags$style(HTML('
            .help-icon {
                padding: 0 5px;
                border-radius: 50%;
                cursor: pointer;
                display: inline-block;
                text-align: center;
                width: 20px;
                height: 20px;
                line-height: 20px;
                font-size: 14px;
            }
            .help-icon-light {
                border: 1px solid #d53b22; /* Dark border for light theme */
            }
            .help-icon-dark {
                border: 1px solid #d53b22; /* White border for dark theme */
            }
        ')),
    # replace the regular skip button with an X in the upper right corner
    tags$style("
            .introjs-skipbutton {
                background: transparent;
                border: none;
                color: #2d2d2d;
                font-weight: normal;
                position: absolute;
                top: 2.5px;
                right: 2.5px;
            }
        ")
  ),
  
  title = "Energy Insights Dashboard",
  # Removing the global sidebar, as each tab will have its own
  # sidebar = sidebar(eda_inputs, width = 400),
  nav_spacer(),
  
  # First tab with its unique sidebar
  nav_panel(
    "Data Exploration",
    icon = icon("bar-chart"),
    layout_sidebar(
      sidebar = sidebar(
        width = 400,
        eda_inputs,
        tags$div(style = "height: 200px;"),  # Add empty space before the materialSwitch
        actionButton("start_tour", "Take the tour"),  # Add the custom button
        materialSwitch(inputId = "theme_toggle", label = "Toggle theme", status = "primary")  # Place the dynamically rendered switch
      ),
      layout_columns(
        value_box(
          theme = "primary",
          title = HTML("<strong>", "Average", "</strong>"),
          value = textOutput("boxmean_value"),
          showcase = uiOutput("boxmean_icon"),
          textOutput("boxmean_pctchange"),
          max_height = "150px"  # Set maximum height for the showcase
        ),
        value_box(
          theme = "primary",
          title = HTML("<strong>", "Minimum", "</strong>"),
          value = textOutput("boxmin_value"),
          showcase = uiOutput("boxmin_icon"),
          textOutput("boxmin_pctchange"),
          max_height = "150px"  # Set maximum height for the showcase
        ),
        value_box(
          theme = "primary",
          title = HTML("<strong>", "Maximum", "</strong>"),
          value = textOutput("boxmax_value"),
          showcase = uiOutput("boxmax_icon"),
          textOutput("boxmax_pctchange"),
          max_height = "150px"  # Set maximum height for the showcase
        )
      ),
      card(shinycssloaders::withSpinner(plotOutput("edaPlot")), width=12)
    )
  ),
  
  # Second tab with a different unique sidebar
  nav_panel(
    "Scenario Planning", 
    icon = icon("sliders"),
    layout_sidebar(
      sidebar = sidebar(
        width = 420,
        scenario_inputs,  # Call the scenario inputs here
        tags$div(style = "height: 200px;"),  # Add empty space before the materialSwitch
        actionButton("start_tour", "Take the tour"),  # Add the custom button
        materialSwitch(inputId = "theme_toggle", label = "Toggle theme", status = "primary")  # Place the dynamically rendered switch
      ),
      navset_card_underline(
        nav_panel(
          title = "Energy Predictions",
          # Your main content for the Scenario Planning tab goes here
          layout_columns(
            value_box(
              theme = "primary",
              title = HTML("<strong>", "Predicted Energy", "</strong>"),
              value = textOutput("energy_totals"),
              showcase = shiny::icon("bolt"),
              max_height = "150px"  # Set maximum height for the showcase
            ),
            value_box(
              theme = "primary",
              title = HTML("<strong>", "Predicted Cost", "</strong>"),
              value = textOutput("forecast_total_cost"),
              showcase = uiOutput("forecast_icon"),
              textOutput("forecast_subtitle"),
              max_height = "150px"  # Set maximum height for the showcase
            ),
            value_box(
              theme = "primary",
              title = HTML("<strong>", "Baseline Cost", "</strong>"),
              value = textOutput("baseline_cost"),
              showcase = shiny::icon("dollar-sign"),
              max_height = "150px"  # Set maximum height for the showcase
            )
          ),
          hidden(
            div(id = 'scenario_plotdiv',
                card(withSpinner(dygraphOutput("scenario_plot"), id = "dygraph_spinner")))),
          card(plotOutput("testplot"))
          
        ),
        nav_panel(
          title = "Forecast Details",
          selectInput("selected_variable", "Variable", 
                      choices = c("Square Feet Per Person" = "sqft_per_person", 
                                  "Equipment Efficiency" = "equip_efficiency", 
                                  "HVAC Efficiency" = "hvac_efficiency", 
                                  "Price ($/KWh)" = "price_per_kwh", 
                                  "Daily Cost ($)" = "daily_cost")),
          value_box(
            theme = "primary",
            title = HTML("<strong>", "Baseline Cost test", "</strong>"),
            value = textOutput("baseline_costtest"),
            showcase = shiny::icon("dollar-sign"),
            max_height = "150px"  # Set maximum height for the showcase
          ),
          card(plotOutput("scenario_details_plot"), width=12)
          
        )
      )  
    )
  )
)


#### Server ######################################################################
# New server logic (removes the `+ theme_bw()` part)
server <- function(input, output, session) {
  
  observe({
    session$setCurrentTheme(if (isTRUE(input$theme_toggle)) light else dark)
  })
  
  # Define reactive expression for the current theme
  current_theme <- reactive({
    if (isTRUE(input$theme_toggle)) {
      value_box_theme(bg = "#9aa5b1", fg = "black")  # Customize for light theme
    } else { 
      value_box_theme(bg = "#243b53", fg = "white")  # Customize for dark theme
    }
  })
  
  # Define a reactive expression for the color palettes
  reactive_color_palette <- reactive({
    if (isTRUE(input$theme_toggle)) {
      # Palette for Light Mode - Muted colors
      c("#cb9820", "#00B1D2", "#243b53", "#3c8dbc", "#F7CAC9", "#92A8D1", "#955251", "#B565A7", "#009B77")
    } else {
      # Palette for Dark Mode - Brighter colors
      c("#cb9820", "#00B1D2", "#d9e2ec", "#F7CAC9", "#92A8D1", "#FFD400", "#00B1D2")
    }
  })
  
  # COMMENT OUT FOR NOW UNTIL READY TO IMPLEMENT TOUR
  # observe for starting tour when Start Tour button is clicked
  # observeEvent(input$start_tour, {
  #     introjs(session, 
  #             events = get_tour_events()
  #     )
  # })
  
  # Ensure the events list for the tour is the same no matter if the tour starts automatically or with the Start Tour button
  # This events list switches from the EDA Plots tab to the Scenario tab at the right moment (currentStep>=10) in the tour. 
  # get_tour_events <- function() {
  #     list(
  #         "onchange" = I("if (this._currentStep<7) {
  #     $('a[data-value=\\\"scenario_planning\\\"]').removeClass('active');
  #     $('a[data-value=\\\"eda_plots\\\"]').addClass('active');
  #     $('a[data-value=\\\"eda_plots\\\"]').trigger('click');
  #   }
  #   if (this._currentStep>=7) {
  #     $('a[data-value=\\\"eda_plots\\\"]').removeClass('active');
  #     $('a[data-value=\\\"scenario_planning\\\"]').addClass('active');
  #     $('a[data-value=\\\"scenario_planning\\\"]').trigger('click');
  #   }")
  #     )
  # }
  
  # observe({
  #     introjs(session,
  #             options = list("nextLabel" = "Next",
  #                            "prevLabel" = "Previous",
  #                            "skipLabel" = "X"),
  #             events = get_tour_events()
  #     )
  # })
  
  # Scenario - dates help button
  observe({
    shinyjs::onclick("scenario_dates_help", {
      showModal(modalDialog(
        title = "Scenario Forecast Range",
        "You can ask the Prophet forecasting model to predict energy usage for any of the existing historical dates and/or up to three years into the future (predictions farther out are unreliable). 
                Note that adjustments to any of the building characteristics below though will only apply to *future* predictions of those characteristics. The historical data remains unchanged under all scenarios.",
        easyClose = TRUE,
        footer = NULL
      ))})
  })
  
  # Scenario - sqft per person help button
  observe({
    shinyjs::onclick("scenario_useradj_sqftppl", {
      showModal(modalDialog(
        title = "Adjusted Square Footage Per Person",
        "This adjusts the distribution of future daily predictions for square footage per person up or down by a percentage. 
                The adjustment is general, not exact, because of randomness and probability statements built into the simulations. 
                The predictions still follow their historical patterns, but each daily prediction takes into account the user-defined percentage adjustment. 
                The historical data remains unchanged under all scenarios.",
        easyClose = TRUE,
        footer = NULL
      ))})
  })
  
  # Scenario - equip efficiency help button
  observe({
    shinyjs::onclick("scenario_useradj_equip_efficiency", {
      showModal(modalDialog(
        title = "Adjusted Equipment Efficiency Rating",
        HTML("<style>em { margin-right: 2px; }</style>
                  This adjusts the future daily predictions for the plug-in equipment efficiency rating up or down.
                  The ratio of investment to equipment rating is 0.15 change per $1000 — so for every $1k invested, the <em>investment</em> value in the rating simulation goes up by .15.<br><br>
                  The rating's simulations are governed by several factors: a <em>yearly decline</em> constant that simulates age-related degradation; a <em>daily change</em> value that is most often static but occasionally fluctuates up or down by a small amount, representing slow, small changes in the aggregate efficiency of equipment inside the building; an <em>event change</em> value that very rarely introduces a large positive or negative change to the rating, representing infrequent major upgrades or breakdowns; and lastly the <em>investment</em> value.<br><br>
                  The adjustment is general, not exact, because of randomness and probability statements built into the simulations.
                  The historical data remains unchanged under all scenarios."),
        easyClose = TRUE,
        footer = NULL
      ))
    })
  })
  
  #  ***************************************************************    
  # EDA tab ********************************************************
  # Reset button functionality
  observeEvent(input$eda_reset, {
    updateSelectInput(session, "eda_building", selected = "budapest")
    updateSelectInput(session, "variable", selected = "sqft_per_person")
    updateSelectInput(session, "day_type_selector", selected = "all")
    updateSliderInput(session, "time_period", value = c(min(df$date), max(df$date)))
  })
  
  # Reactive expression for summary stats boxes
  summaryboxes_filter <- reactive({
    start_date <- as.Date(input$time_period[1], origin="1970-01-01")
    end_date <- as.Date(input$time_period[2], origin="1970-01-01")
    
    summaryboxes_df <- df %>% 
      filter(bldg_name == input$eda_building, date >= start_date & date <= end_date) %>% 
      select(date, bldg_name, !!sym(input$variable), weekday)
    
    if (!is.null(input$day_type) && input$variable %in% c("sqft_per_person", "num_ppl_raw")) {
      if(input$day_type == "weekdays") {
        summaryboxes_df <- summaryboxes_df %>% filter(weekday == 1)
      } else if(input$day_type == "weekends") {
        summaryboxes_df <- summaryboxes_df %>% filter(weekday == 0)
      }
    }
    return(summaryboxes_df)
  })
  
  # Call the function for mean, min, max
  create_stat_box_outputs(output, summaryboxes_filter, df, "mean", input, session)
  create_stat_box_outputs(output, summaryboxes_filter, df, "min", input, session)
  create_stat_box_outputs(output, summaryboxes_filter, df, "max", input, session)
  
  # EDA: Observer for the conditional day_type selector that displays if "sqft_per_person" or "num_ppl_raw" are selected
  output$day_type_selector <- renderUI({
    if (input$variable %in% c("sqft_per_person", "num_ppl_raw")) {
      selectInput("day_type", "Day Type",
                  choices = c("All" = "all",
                              "Weekdays" = "weekdays",
                              "Weekends" = "weekends"))
    }
  })
  
  observeEvent(input$plot, {
    output$edaPlot <- renderPlot({
      selected_var <- input$variable  
      pretty_var <- pretty_variable_names[selected_var]  
      start_date <- as.Date(input$time_period[1], origin="1970-01-01")
      end_date <- as.Date(input$time_period[2], origin="1970-01-01")
      
      plot_df <- df %>% 
        filter(bldg_name == input$eda_building, date >= start_date & date <= end_date)
      
      # Create 'day_type' only for specific variables
      if (input$variable %in% c("sqft_per_person", "num_ppl_raw") && !is.null(input$day_type)) {
        plot_df <- plot_df %>% 
          mutate(day_type = ifelse(weekday == 1, "Weekday", "Weekend"))
      }
      
      font_color <- if (isTRUE(input$theme_toggle)) "black" else "white"
      
      # Base plot with conditional color aesthetic
      p <- ggplot(plot_df, aes(x = date, y = !!sym(selected_var))) +
        geom_line() +
        labs(x = "Date", y = pretty_var) +
        theme(
          axis.title.x = element_text(size = 15, color = font_color, margin = margin(t = 10)),
          axis.title.y = element_text(size = 15, color = font_color, margin = margin(r = 10)),
          legend.title = element_text(size = 15, color = font_color),
          legend.text = element_text(size = 13, color = font_color)
        ) +
        scale_color_manual(values = reactive_color_palette()) +
        scale_y_continuous(breaks = get_breaks(selected_var, plot_df),
                           labels = scales::label_comma(accuracy = 0.1))
      
      # Add color aesthetic only if day_type exists
      if ("day_type" %in% names(plot_df)) {
        p <- p + aes(color = day_type) + labs(color = "Day Type")
      }
      
      p
    })
  }, ignoreNULL = FALSE)
  
  
  #  *****************************************************************************    
  # Scenario Planning tab ********************************************************
  
  # Flag to check if scenario rendering has started
  renderingStarted <- reactiveVal(FALSE)
  
  observeEvent(input$scenario_reset, {
    updateSelectInput(session, "scenario_building", selected = "budapest")
    updateSliderInput(session, "scenario_range", value = c(min(df$date), max(df$date) + potential_forecast_window))
    updateSliderInput(session, "useradjust_sqft_per_person", value = 0)
    updateSliderInput(session, "useradjust_equip_efficiency", value = 0)
    updateSliderInput(session, "useradjust_hvac_efficiency", value = 0)
    updateSliderInput(session, "useradjust_energyprices", value = 0)
  })
  
  # Server code for Scenario Planning - run the full simulations
  observeEvent(input$run_scenario, {
    
    tictoc::tic()
    output$scenario_plot <- renderUI({}) # render a blank plot
    #       output$scenario_details_plot <- renderUI({}) # render a blank plot
    output$energy_totals <- renderText({}) # render a blank area
    output$forecast_total_cost <- renderText({}) # render a blank area
    output$baseline_cost <- renderText({}) # render a blank area
    shinyjs::show('scenario_plotdiv')
    shinyjs::show("dygraph_spinner")
    
    # Prepare historical data with adjusted regressors
    scenario_data <- df %>%
      filter(bldg_name == input$scenario_building) %>%
      rename(ds = date, y = total) %>% 
      mutate(ds = with_tz(ds, tzone = "UTC")) 
    
    set.seed(12923) # set seed again for consistency
    
    # Create a Prophet model
    m <- prophet(interval.width = .8) # .8 is the default confidence interval 
    # # Add each predictor as a regressor
    m <- add_regressor(m, 'sqft_per_person')
    m <- add_regressor(m, 'equip_efficiency')
    m <- add_regressor(m, 'hvac_efficiency')
    
    # Fit the model with scenario data
    m <- fit.prophet(m, df = scenario_data)
    
    # Create future dataframe
    future_full <- make_future_dataframe(m, periods = potential_forecast_window) # "potential_forecast_window" is the maximum length of possible future forecasts.
    
    if (!is.null(input$scenario_range) && length(input$scenario_range) == 2) { # leave these as "scenario_range"
      forecast_window_start <- as.Date(input$scenario_range[1], tz = "UTC")
      forecast_window_end <- as.Date(input$scenario_range[2], tz = "UTC")
      
      total_dates <- seq.Date(forecast_static_min, potential_forecast_window_end, by = "day")
      
      potential_total_length <- length(total_dates)
      
      # building-specific adjustments for sqft_per_person
      mean_factor_weekday <- c(nakatomi = 80, wayne_manor = -150, budapest = -7)
      mean_factor_weekend <- c(nakatomi = 170, wayne_manor = 45, budapest = 10)
      scale_factor_weekday <- c(nakatomi = 1, wayne_manor = -1, budapest = 1)
      scale_factor_weekend <- c(nakatomi = 0, wayne_manor = -1, budapest = 1)
      sd_factor_weekday <- c(nakatomi = 10, wayne_manor = -69, budapest = -7)
      sd_factor_weekend <- c(nakatomi = 10, wayne_manor = -29, budapest = -7)
      
      set.seed(12923)
      future_full <- add_future_regressor(
        df = df,
        future_df = future_full,
        pred_building = input$scenario_building,
        variable_name = "sqft_per_person",
        future_variable = generate_sqft_per_person(df, pred_building = input$scenario_building,
                                                   total_dates, potential_forecast_window,
                                                   mean_factor_weekday, mean_factor_weekend,
                                                   scale_factor_weekday, scale_factor_weekend,
                                                   sd_factor_weekday, sd_factor_weekend,
                                                   useradjust_sqft_per_person = input$useradjust_sqft_per_person)
      )
      
      set.seed(12923)
      future_full <- add_future_regressor(
        df = df,
        future_df = future_full,
        pred_building = input$scenario_building,
        variable_name = "hvac_efficiency",
        future_variable = generate_hvac_efficiency(df, pred_building = input$scenario_building,
                                                   total_dates, potential_forecast_window,
                                                   useradjust_hvac_efficiency = input$useradjust_hvac_efficiency)
      )
      
      set.seed(12923)
      future_full <- add_future_regressor(
        df = df,
        future_df = future_full,
        pred_building = input$scenario_building,
        variable_name = "equip_efficiency",
        future_variable = generate_equip_efficiency(df,
                                                    pred_building = input$scenario_building,
                                                    total_dates, potential_forecast_window,
                                                    useradjust_equip_efficiency = input$useradjust_equip_efficiency)
      )
      
      # Make predictions
      future_full_preds <- predict(m, future_full)
      
      future_full_preds <- future_full_preds %>%
        mutate(ds = with_tz(ds, tzone = "UTC")) # match time zones to avoid error messages
      
      # Filter the full scenario to only include the selected date range
      filtered_scenario <- future_full_preds %>%
        filter(ds >= forecast_window_start & ds <= forecast_window_end)
      
      
      
      
      # Ensure that 'price_per_kwh' vector is of the same length as 'filtered_scenario$yhat'
      set.seed(12923)
      prices_df <- generate_energyprices(df, total_dates, potential_total_length, input$useradjust_energyprices) # generate full simulations in order to filter as needed later
      
      filtered_prices <- prices_df %>%
        filter(ds >= forecast_window_start & ds <= forecast_window_end)
      
      filtered_scenario$daily_cost <- filtered_scenario$yhat * filtered_prices$price_per_kwh
      
      # print("filtered scenario")
      # print(head(filtered_scenario))
      # 
      # print("filtered prices")
      # print(head(filtered_prices))
      
      # print("FORECAST: filtered prices first five prices:")
      # print(filtered_prices$price_per_kwh[0:5])
      
      # Calculate the total predicted energy usage multiplied by the price for each day
      forecast_energy_cost <- sum(filtered_scenario$yhat * filtered_prices$price_per_kwh) #row-wise cost calc
      forecast_total_cost <- sum(forecast_energy_cost + input$useradjust_hvac_efficiency + input$useradjust_equip_efficiency)
    }
    
    # BASELINE MODEL #####################################################################################
    
    # Create future dataframe using same prophet model
    base_future_full <- make_future_dataframe(m, periods = potential_forecast_window) # "potential_forecast_window" is the maximum length of possible future forecasts.
    
    if (!is.null(input$scenario_range) && length(input$scenario_range) == 2) { # leave these as "scenario_range"
      
      set.seed(12923)
      base_future_full <- add_future_regressor(
        df = df,
        future_df = base_future_full,
        pred_building = input$scenario_building,
        variable_name = "sqft_per_person",
        future_variable = generate_sqft_per_person(df, pred_building = input$scenario_building,
                                                   total_dates, potential_forecast_window,
                                                   mean_factor_weekday, mean_factor_weekend,
                                                   scale_factor_weekday, scale_factor_weekend,
                                                   sd_factor_weekday, sd_factor_weekend,
                                                   useradjust_sqft_per_person = 0)
      )
      
      set.seed(12923)
      base_future_full <- add_future_regressor(
        df = df,
        future_df = base_future_full,
        pred_building = input$scenario_building,
        variable_name = "hvac_efficiency",
        future_variable = generate_hvac_efficiency(df, pred_building = input$scenario_building,
                                                   total_dates, potential_forecast_window,
                                                   useradjust_hvac_efficiency = 0)
      )
      
      set.seed(12923)
      base_future_full <- add_future_regressor(
        df = df,
        future_df = base_future_full,
        pred_building = input$scenario_building,
        variable_name = "equip_efficiency",
        future_variable = generate_equip_efficiency(df,
                                                    pred_building = input$scenario_building,
                                                    total_dates, potential_forecast_window,
                                                    useradjust_equip_efficiency = 0)
      )
      
      # Make predictions
      base_future_full_preds <- predict(m, base_future_full)
      base_future_full_preds <- base_future_full_preds %>%
        mutate(ds = with_tz(ds, tzone = "UTC")) # match time zones to avoid error messages
      
      # Filter the full scenario to only include the selected date range
      base_filtered_scenario <- base_future_full_preds %>%
        filter(ds >= forecast_window_start & ds <= forecast_window_end)
      
      
      
    }
    set.seed(12923)
    base_prices_df <- generate_energyprices(df, total_dates, potential_total_length, useradjust_energyprices = 0) # generate full simulations in order to filter as needed later
    
    base_filtered_prices <- base_prices_df %>%
      filter(ds >= forecast_window_start & ds <= forecast_window_end)
    
    base_filtered_scenario$daily_cost <- base_filtered_scenario$yhat * base_filtered_prices$price_per_kwh
    
    # print("baseline filtered variables")
    # print(head(base_filtered_scenario))
    # 
    # print("baseline filtered prices")
    # print(head(base_filtered_prices))
    
    # Calculate the total predicted energy usage multiplied by the price for each day
    base_filtered_energy_cost <- sum(base_filtered_scenario$yhat * base_filtered_prices$price_per_kwh) #row-wise cost calc
    
    output$energy_totals <- renderText({
      total_energy_use <- comma(round(sum(filtered_scenario$yhat),1))
      text <- paste(total_energy_use, " KWh")
      return(text)
    })
    
    output$forecast_total_cost <- renderText({
      forecast_total_cost <- comma(round(forecast_total_cost,1))
      return(forecast_total_cost)
    })
    
    output$forecast_icon <- renderUI({
      cost_diff <- forecast_total_cost - base_filtered_energy_cost
      
      # Define icon colors
      icon_color_up <- "#00cb21"  # Green for higher
      icon_color_down <- "#ff6969"  # Red for lower
      icon_color_equal <- "#bababa"  # Grey for equal
      
      # Determine the icon name and color
      icon_name <- ifelse(cost_diff > 0, "chevron-up",
                          ifelse(cost_diff < 0, "chevron-down", "equals"))
      icon_color <- ifelse(cost_diff > 0, icon_color_up,
                           ifelse(cost_diff < 0, icon_color_down, icon_color_equal))
      
      # Return the icon with the appropriate color
      shiny::icon(icon_name, style = if (!is.null(icon_color)) paste0("color:", icon_color))
    })
    
    output$forecast_subtitle <- renderText({
      # dynamic color and subtitle text
      cost_diff <- forecast_total_cost - base_filtered_energy_cost
      subtitle_text <- if (cost_diff > 0) {
        paste(scales::dollar_format()(cost_diff), "more than baseline")
      } else if (cost_diff < 0) {
        paste(scales::dollar_format()(abs(cost_diff)), "less than baseline")
      } else {
        NULL # No subtitle when they're equal
      }
      return(subtitle_text)
    })
    
    output$baseline_cost <- renderText({
      base_filtered_energy_cost <- comma(round(base_filtered_energy_cost,1))
      return(base_filtered_energy_cost)
    })
    
    
    output$scenario_plot <- renderDygraph({
      dyplot.prophet(m, filtered_scenario)
    })
    
    
    # hide('dygraph_spinner')
    # shinyjs::show('scenario_plotdiv')
    # 
    
    output$baseline_costtest <- renderText({
      test <- comma(round(base_filtered_energy_cost,4))
      return(test)
    })
    
    output$scenario_details_plot <- renderPlot({
      variable <- input$selected_variable
      pretty_label <- pretty_variable_names[variable]
      
      # Prepare data for the selected variable
      if (variable == "daily_cost") {
        scenario_data <- filtered_scenario %>% select(ds, daily_cost)
        baseline_data <- base_filtered_scenario %>% select(ds, daily_cost)
      } else if (variable == "price_per_kwh") {
        scenario_data <- filtered_prices %>% select(ds, price_per_kwh)
        baseline_data <- base_filtered_prices %>% select(ds, price_per_kwh)
      } else {
        scenario_data <- filtered_scenario %>% select(ds, !!sym(variable))
        baseline_data <- base_filtered_scenario %>% select(ds, !!sym(variable))
      }
      
      # Combine and plot data
      combined_data <- rbind(data.frame(ds = scenario_data$ds, value = scenario_data[[2]], Group = "Scenario"),
                             data.frame(ds = baseline_data$ds, value = baseline_data[[2]], Group = "Baseline"))
      
      font_color <- if (isTRUE(input$theme_toggle)) "black" else "white"
      
      ggplot(combined_data, aes(x = ds, y = value, color = Group)) +
        geom_line()
      
      # ggplot(combined_data, aes(x = ds, y = value, color = Group, group = Group)) +
      #     geom_line() +
      #     labs(x = "Date", y = pretty_label) +
      #     theme(
      #         axis.title.x = element_text(size = 15, color = font_color, margin = margin(t = 10)),
      #         axis.title.y = element_text(size = 15, color = font_color, margin = margin(r = 10)),
      #         legend.title = element_text(size = 15, color = font_color),
      #         legend.text = element_text(size = 13, color = font_color)
      #     ) +
      #     scale_color_manual(values = reactive_color_palette()) +
      #     scale_y_continuous(breaks = get_breaks("value", combined_data),
      #                        labels = scales::label_comma(accuracy = 0.1))
      
      print(variable)
      print(head(combined_data))
      
      
      print("breaks and labels:")
      breaks <- get_breaks("value", combined_data)
      print(breaks)
      label_function <- scales::label_comma(accuracy = .1)
      print(label_function(breaks))
      
      
    })
    
    hide('dygraph_spinner')
    shinyjs::show('scenario_plotdiv')
    tictoc::toc()
    
    
    
  })
  
  
}

# Run app ####
shinyApp(ui, server)