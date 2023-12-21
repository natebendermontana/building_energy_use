
# setup
library(dplyr)
library(ggplot2)
library(lubridate)
# prophet modeling
library(prophet)
library(dygraphs)
# shiny and UI extras
library(shiny)
library(shinydashboard)
#library(shinydashboardPlus)
library(shinythemes)
library(shinyjs)
library(rintrojs)
library(shinycssloaders)

# ************************************************************
# Setup ######################################################
# ************************************************************

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


# ************************************************************
# ************************************************************
# Func: Num People ###########################################
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
  pcnt_adjustment = 1 + (useradjust_sqft_per_person / 100)
    
    for (i in 1:(potential_total_length)) {
        if (weekdays[i] == 1) {
            # Generate more people on weekdays
            mean_factor <- 300 + mean_factor_weekday[pred_building]*pcnt_adjustment
            scale_factor <- 5 + scale_factor_weekday[pred_building]
            sd_factor <- 20 + sd_factor_weekday[pred_building]   
        } else {
            # Generate fewer people on weekends
            mean_factor <- 70 + mean_factor_weekend[pred_building]*pcnt_adjustment
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
    efficiency_per_thousand = 0.001
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
    efficiency_per_thousand = 0.001
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

# UI ##########################

ui <- shinyUI(
    fluidPage(
      useShinyjs(),
        introjsUI(),
        tags$head(
            tags$script(HTML("
  function startIntroJS() {
    introJs().start();
  }

  $(document).ready(function() {
    $('.main-header').attr({
      'data-step': '1',
      'data-intro': 'Welcome to the [app name], a data exploration and prediction engine for a trio of noteworthy fictional buildings: Nakatomi Plaza, Wayne Manor, and the Grand Budapest Hotel.'
    });

    // Optionally, start the tour automatically after a brief delay
    setTimeout(startIntroJS, 500); 
  });
")),
tags$style("
          .introjs-tooltip {
              width: 400px; /* Custom width */
              max-width: 450px; /* Maximum width */
          }"
)
        ),
dashboardPage(
    dashboardHeader(title = "Energy Use Analysis and Forecast"),
    dashboardSidebar(
        width = 220,
        sidebarMenu(
            id = "tabs",
            introBox(
                menuItem("EDA Plots", tabName = "eda_plots", icon = icon("bar-chart")),
                data.step = 2,
                data.intro = "In this app you can explore the unique characteristics and energy use patterns I created from scratch for each building, predict future energy use based on historical patterns, and even run energy use predictions based on custom scenarios of where you control the building characteristics.
              <br><br> First, explore the fictional data with exploratory data analysis (EDA) visualizations."
            ),
            introBox(
                menuItem("Scenario Planning", tabName = "scenario_planning", icon = icon("sliders")),
                data.step = 3,
                data.intro = "Use the Scenario Planning tab for future planning and predictions based on custom parameters."
            ),
            introBox(
                actionButton("start_tour", "Take the tour"),  # Add the custom button
                class = "tour-button-container",  # Add this class
                data.step = 13,
                data.intro = "Click here to start this tour again at any time."
            )
        )
    ),
    dashboardBody(
      tags$style(
        type = 'text/css', 
        '.bg-black {background-color: #575757!important; }'
      ),
      tags$style(HTML(
        ".skin-blue .main-header .logo:hover {font-weight: bold; background-color: #155974; color: #ffffff}
   .skin-blue .main-header .logo {font-weight: bold; background-color: #155974; color: #ffffff}
   .tab-pane {text-align: center;}
   .skin-blue .main-header .navbar {background-color: #155974;}
   .skin-blue .main-sidebar {background-color: #e1e1e1;}
   .skin-blue .main-sidebar .sidebar .sidebar-menu a{background-color: #e1e1e1; color: #3c3c3c; height: 50px; line-height: 50px;} /* Increased height and line height */
   .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{background-color: #e1e1e1; color: #ffffff; border-left: 4px solid #e1e1e1;}
   .skin-blue .main-sidebar .sidebar .sidebar-menu > li:hover > a {background-color: #dcdcdc; color: #3c3c3c;}
   .skin-blue .background a{background-color: #e1e1e1}
   .tour-button-container {padding-top: 15px;} /* Apply padding to the container */
  ")
      ),
    tags$style(HTML(
      "
      /* Increase font size of the tab texts */
      .skin-blue .main-sidebar .sidebar-menu > li > a {
        font-size: 18px; /* Adjust the size as needed */
      }
  
      /* Increase the size of the icons in the tabs */
      .skin-blue .main-sidebar .sidebar-menu > li > a > i {
        font-size: 24px; /* Adjust the icon size as needed */
      }
      "
    )),
      
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
        "),
        tabItems(
            tabItem(tabName = "eda_plots",
                    fluidRow(
                        box(title = "Filters", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                            fluidRow(
                                column(6,
                                       introBox(
                                           selectInput("eda_building", "Building",
                                                       choices = c("Grand Budapest Hotel" = "budapest",
                                                                   "Nakatomi Plaza" = "nakatomi",
                                                                   "Wayne Manor" = "wayne_manor")),
                                           data.step = 4,
                                           data.intro = "Each building is set up with unique characteristics and usage patterns that affect its energy usage."
                                       )
                                ),
                                column(6,
                                       introBox(
                                           selectInput("variable", "Variable",
                                                       choices = c("Square Feet Per Person" = "sqft_per_person",
                                                                   "Number of People" = "num_ppl_raw",
                                                                   "Temperature (F)" = "temp",
                                                                   "Wind Speed (mph)" = "wind_speed",
                                                                   "Cloud Cover (%)" = "cloud_cover",
                                                                   "Equipment Efficiency Rating" = "equip_efficiency",
                                                                   "HVAC Efficiency Rating" = "hvac_efficiency",
                                                                   "Building Size" = "bldg_area",
                                                                   "Total Energy Use (KWh)" = "total")),
                                           data.step = 5,
                                           data.intro = "Here in the EDA Plots tab, you can choose a variable to see how it's been simulated for a particular building."
                                       )
                                )
                            ),
                            fluidRow(
                                column(12,
                                       introBox(
                                           sliderInput("time_period", "Time Period",
                                                       min = min(df$date),
                                                       max = max(df$date),
                                                       value = c(min(df$date), max(df$date)),
                                                       timeFormat="%Y-%m-%d",
                                                       step = 1),
                                           data.step = 6,
                                           data.intro = "Zoom in or out to a particular time frame."
                                       )
                                )
                            ),
                            fluidRow(column(6, uiOutput("day_type_selector"))),

                        ),
                        fluidRow(
                          column(4, uiOutput("box_avg")),
                          column(4, uiOutput("box_min")),
                          column(4, uiOutput("box_max"))
                        ),
                        introBox(
                            box(shinycssloaders::withSpinner(plotOutput("edaPlot")), width=12),
                            data.step = 7,
                            data.intro = "The plot will dynamically update as you choose new date ranges or variables to explore."
                        )
                    )
            ),
            
            tabItem(tabName = "scenario_planning",
                    fluidRow(
                        box(title = "Filters", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                            fluidRow(
                                column(4,
                                       introBox(
                                           selectInput("scenario_building", "Building",
                                                       choices = c("Grand Budapest Hotel" = "budapest",
                                                                   "Nakatomi Plaza" = "nakatomi",
                                                                   "Wayne Manor" = "wayne_manor")),
                                           data.step = 8,
                                           data.intro = "Now we're in the Scenario Planning tab. This uses a time series forecasting model call Prophet, developed by Facebook, to make predictions about the buildings' historic or future energy use.
                                       <br><br> Note: the [X] adjustable parameters below only apply to predictions about *future* energy use. They do not affect the model's predictions about the historical energy use."
                                       )
                                ),
                                column(8, 
                                       introBox(
                                           sliderInput("scenario_range", "Dates to Forecast",
                                                       min = min(df$date),
                                                       max = max(df$date) + potential_forecast_window,
                                                       value = c(min(df$date),
                                                                 max(df$date) + potential_forecast_window),
                                                       timeFormat="%Y-%m-%d",
                                                       step = 1),
                                           data.step = 9,
                                           data.intro = "You can use the model to forecast over the historical data as well as make future predictions. 
                                         The forecast is limited to three years in advance; predictions farther out than that are unreliable."
                                       )
                                )
                            ),
                            fluidRow(
                                column(4,
                                       introBox(
                                           sliderInput("useradjust_sqft_per_person", "Percent adjustment: Square Feet Per Person",
                                                       min = -100, max = 100, value = 0, post = "%"),
                                           data.step = 10,
                                           data.intro = "Do you expect more or fewer people in this building in the future? This slider adjusts the *distribution* of people upwards or downwards by a percentage.
                              <br><br> But remember, it's inverted — more sqft per person means the building is less densely occupied. For instance, setting the slider to 30% means that the future energy predictions will be based on the historical patterns of square footage per person shifted upwards by 30%."
                                       )
                                ),
                              column(4,
                                     introBox(
                                         sliderInput("useradjust_equip_efficiency", "Investment: Equipment Efficiency",
                                                     min = -100000, max = 100000, value = 0, step = 100, pre = "$"),
                                         data.step = 11,
                                         data.intro = "How much do you want to invest in the efficiency of non-HVAC equipment inside the building? Equipment efficiency is set up to simulate an aggregate efficiency rating of all the plug loads (everything that plugs into the walls) in the building. 
                                         Increases in this rating are tied to decreases in the overall energy use. 
                                         <br><br> I've set this slider up so that every $1000 invested is associated with a 0.001 increase in the daily value for this rating. However, the same historical patterns governing this variable apply to the future predictions."
                                     )),
                              column(4,
                                     introBox(
                                         sliderInput("useradjust_hvac_efficiency", "Investment: HVAC Efficiency",
                                                     min = -100000, max = 100000, value = 0, step = 100, pre = "$"),
                                         data.step = 12,
                                         data.intro = "How much do you want to invest in the HVAC system efficiency? This variable is set up to simulate the energy efficiency of the building's HVAC system, a major driver of energy use
                                         <br><br> I've set this slider up so that every $1000 invested is associated with a 0.001 increase in the daily value for this rating. Like the Equipment Efficiency slider, though, the historical patterns governing this variable also apply to the future predictions.
                                         Age-related efficiency declines and random breakdowns can still occur, so investing doesn't completely guarantee high efficiency."
                                     ))
                            ),
                            fluidRow(
                                column(12, offset = 0,
                                       actionButton("run_scenario", "Run Scenario",
                                       style = 'color: #ffffff; background-color: #155974; border-color: #155974; 
                                                                font-size: 110%; margin-top: 20px;')
                                )
                            )
                        )
                    ),
                    hidden(div(id = 'scenario_plotdiv', 
                        withSpinner(dygraphOutput("scenario_plot"), id = "dygraph_spinner"),
                        withSpinner(verbatimTextOutput("scenario_details"), id = "dygraph_spinner")))                         
            )
        )
    )
)
)
)


# Server ####
server <- function(input, output, session) {
  
  # Flag to check if scenario rendering has started
  renderingStarted <- reactiveVal(FALSE)
  
    # Ensure the events list for the tour is the same no matter if the tour starts automatically or with the Start Tour button
    # This events list switches from the EDA Plots tab to the Scenario tab at the right moment (currentStep>=10) in the tour. 
    get_tour_events <- function() {
        list(
            "onchange" = I("if (this._currentStep<7) {
        $('a[data-value=\\\"scenario_planning\\\"]').removeClass('active');
        $('a[data-value=\\\"eda_plots\\\"]').addClass('active');
        $('a[data-value=\\\"eda_plots\\\"]').trigger('click');
      }
      if (this._currentStep>=7) {
        $('a[data-value=\\\"eda_plots\\\"]').removeClass('active');
        $('a[data-value=\\\"scenario_planning\\\"]').addClass('active');
        $('a[data-value=\\\"scenario_planning\\\"]').trigger('click');
      }")
        )
    }
    
    # observe for starting tour automatically on app load
    observe({
        introjs(session,
                options = list("nextLabel" = "Next",
                               "prevLabel" = "Previous",
                               "skipLabel" = "X"),
                events = get_tour_events()
        )
    })
    # observe for starting tour when Start Tour button is clicked
    observeEvent(input$start_tour, {
        introjs(session, 
                events = get_tour_events()
        )
    })
    
    # pretty-fy things a bit
    pretty_building_names <- c("budapest" = "Grand Budapest Hotel", 
                               "nakatomi" = "Nakatomi Plaza", 
                               "wayne_manor" = "Wayne Manor")
    
    pretty_variable_names <- c("bldg_area" = "Building Area (sqft)",
                               "sqft_per_person" = "Square Feet Per Person",
                               "num_ppl_raw" = "Number of People",
                               "temp" = "Temperature (F)", 
                               "wind_speed" = "Wind Speed (mph)", 
                               "cloud_cover" = "Cloud Cover (%)", 
                               "equip_efficiency" = "Equipment Efficiency Rating", 
                               "hvac_efficiency" = "HVAC Efficiency Rating", 
                               "total" = "Total Energy Use (KWh)")
    
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
    
      
    # Function to calculate percentage change
    calc_pct_chng <- function(current_value, overall_value) {
      if (overall_value == 0 || is.na(current_value) || is.na(overall_value)) {
        return(NA)
      } else {
        return((current_value - overall_value) / overall_value * 100)
      }
    }
      
    # Mean Box
    output$box_avg <- renderUI({
      current_mean <- mean(summaryboxes_filter()[[input$variable]], na.rm = TRUE)
      overall_mean <- mean(df[[input$variable]], na.rm = TRUE)
      pct_change <- calc_pct_chng(current_mean, overall_mean)
      
      #color <- ifelse(pct_change >= 0, "green", "red")
      pct_change_text <- paste0(formatC(pct_change, format = "f", digits = 1), "%")
      
      # Determine icon and color based on pct_change
      icon_color_up <- "#00cb21"  # A custom green color (you can use hex codes)
      icon_color_down <- "#ff6969"  # A custom red color
      icon_color_equal <- "#bababa"  # A custom blue color for 'equals'
      
      icon_name <- ifelse(pct_change > 0, "chevron-up", ifelse(pct_change < 0, "chevron-down", "equals"))
      icon_color <- ifelse(pct_change > 0, icon_color_up, ifelse(pct_change < 0, icon_color_down, icon_color_equal))
      color <- ifelse(pct_change > 0, "green", ifelse(pct_change < 0, "red", "black"))
      fill <- pct_change != 0
      
      infoBox(
        title = "Average",
        icon = shiny::icon(icon_name, style = if (!is.null(color)) paste0("color:", icon_color)),
        value = round(current_mean, 2),
        subtitle = paste0("Compared to overall: ", pct_change_text),
        width = 12,
        color = color,
        fill = FALSE
      )
    })
      
      # Min Box
      output$box_min <- renderUI({
        current_min <- min(summaryboxes_filter()[[input$variable]])
        overall_min <- min(df[[input$variable]])
        pct_change <- calc_pct_chng(current_min, overall_min)
        pct_change_text <- paste0(formatC(pct_change, format = "f", digits = 1), "%")
        
        # Determine icon and color based on pct_change
        icon_color_up <- "#00cb21"  # A custom green color (you can use hex codes)
        icon_color_down <- "#ff6969"  # A custom red color
        icon_color_equal <- "#bababa"  # A custom blue color for 'equals'
        
        icon_name <- ifelse(pct_change > 0, "chevron-up", ifelse(pct_change < 0, "chevron-down", "equals"))
        icon_color <- ifelse(pct_change > 0, icon_color_up, ifelse(pct_change < 0, icon_color_down, icon_color_equal))
        color <- ifelse(pct_change > 0, "green", ifelse(pct_change < 0, "red", "black"))
        fill <- pct_change != 0
        
        infoBox(
          title = "Minimum",
          icon = shiny::icon(icon_name, style = if (!is.null(color)) paste0("color:", icon_color)),
          value = round(current_min, 2),
          subtitle = paste0("Compared to overall: ", pct_change_text),
          width = 12,
          color = color,
          fill = FALSE
        )
      })
      
      # Max Box
      output$box_max <- renderUI({
        current_max <- max(summaryboxes_filter()[[input$variable]])
        overall_max <- max(df[[input$variable]])
        pct_change <- calc_pct_chng(current_max, overall_max)
        pct_change_text <- paste0(formatC(pct_change, format = "f", digits = 1), "%")
        
        # Determine icon and color based on pct_change
        icon_color_up <- "#00cb21"  # A custom green color (you can use hex codes)
        icon_color_down <- "#ff6969"  # A custom red color
        icon_color_equal <- "#bababa"  # A custom blue color for 'equals'
        
        icon_name <- ifelse(pct_change > 0, "chevron-up", ifelse(pct_change < 0, "chevron-down", "equals"))
        icon_color <- ifelse(pct_change > 0, icon_color_up, ifelse(pct_change < 0, icon_color_down, icon_color_equal))
        color <- ifelse(pct_change > 0, "green", ifelse(pct_change < 0, "red", "black"))
        fill <- pct_change != 0
        
        infoBox(
          title = "Maximum",
          icon = shiny::icon(icon_name, style = if (!is.null(color)) paste0("color:", icon_color)),
          value = round(current_max, 2),
          subtitle = paste0("Compared to overall: ", pct_change_text),
          width = 12,
          color = color,
          fill = FALSE
        )
      })
    
    # EDA: Observer for the conditional day_type selector that displays if "sqft_per_person" or "num_ppl_raw" are selected
    output$day_type_selector <- renderUI({
      if (input$variable %in% c("sqft_per_person", "num_ppl_raw")) {
        selectInput("day_type", "Day Type",
                    choices = c("All" = "all",
                                "Weekdays" = "weekdays",
                                "Weekends" = "weekends"))
      }
    })
    
    # EDA Plots
    observeEvent(input$plot, {
        output$edaPlot <- renderPlot({
            selected_bldg <- pretty_building_names[input$eda_building]
            selected_var <- pretty_variable_names[input$variable]
            start_date <- as.Date(input$time_period[1], origin="1970-01-01")
            end_date <- as.Date(input$time_period[2], origin="1970-01-01")
            
            plot_df <- df %>% 
                filter(bldg_name == input$eda_building, date >= start_date & date <= end_date)
            
            # Additional filtering for day type if 'sqft_per_person' or 'num_ppl_raw' is selected
            if (!is.null(input$day_type) && input$variable %in% c("sqft_per_person", "num_ppl_raw")) {
              if(input$day_type == "weekdays") {
                plot_df <- plot_df %>% filter(weekday == 1)
              } else if(input$day_type == "weekends") {
                plot_df <- plot_df %>% filter(weekday == 0)
              }
              # No additional filtering for "All"
            }
            
            
            # Check if 'sqft_per_person' or 'num_ppl_raw' is the selected variable, plot differently if so.
            if (!is.null(input$day_type) && input$variable %in% c("sqft_per_person", "num_ppl_raw")) {
              plot_df <- plot_df %>%
                mutate(day_type = ifelse(weekday == 1, "Weekday", "Weekend"))
              
              # Change y-axis label based on selected variable
              y_axis_label <- if (input$variable == "sqft_per_person") {
                "Square Feet Per Person"
              } else {
                "Number of People"
              }
              
              day_type_colors <- c("Weekday" = "#264165", "Weekend" = "#10adc6")
              available_day_types <- unique(plot_df$day_type)
              colors_to_use <- day_type_colors[available_day_types]
              
              ggplot(plot_df, aes(x = date, y = !!sym(input$variable), color = day_type)) +
                geom_line() +
                labs(x = "Date", y = y_axis_label, color = "Day Type") +
                theme_minimal() +
                theme(axis.title = element_text(size = 16),  
                      axis.text = element_text(size = 13),   
                      legend.title = element_text(size = 15),
                      legend.text = element_text(size = 13))+
                    scale_color_manual(values = colors_to_use)
            } else {
                ggplot(plot_df, aes_string(x = "date", y = input$variable)) +
                  geom_line(color = "#373737", alpha = .8) +  # Set specific color for lines
                  labs(x = "Date", y = selected_var) +
                  theme_minimal() +
                  theme(axis.title = element_text(size = 15),  
                        axis.text = element_text(size = 13),   
                        legend.title = element_text(size = 13))
            }
        })
    }, ignoreNULL = FALSE)
  
    
    # Server code for Scenario Planning - run the full simulations
    observeEvent(input$run_scenario, {
      
      output$scenario_plot <- renderUI({}) # render a blank plot
      output$scenario_details <- renderUI({}) # render a blank area
      shinyjs::show('scenario_plotdiv')
      shinyjs::show("dygraph_spinner")

        # Prepare historical data with adjusted regressors
        scenario_data <- df %>%
            filter(bldg_name == input$scenario_building) %>%
            rename(ds = date, y = total) %>% 
            mutate(ds = with_tz(ds, tzone = "UTC")) 
        
        
        # Create a Prophet model
        m <- prophet()
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
            
            future_full <- add_future_regressor(
                df = df,
                future_df = future_full,
                pred_building = input$scenario_building,
                variable_name = "hvac_efficiency",
                future_variable = generate_hvac_efficiency(df, pred_building = input$scenario_building,
                                                           total_dates, potential_forecast_window,
                                                           useradjust_hvac_efficiency = input$useradjust_hvac_efficiency)
            )
            
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
            
            hide('dygraph_spinner')
            
            output$scenario_plot <- renderDygraph({
              dyplot.prophet(m, filtered_scenario)
            })
            
            output$scenario_details <- renderPrint({
                summary <- filtered_scenario %>%
                    summarise(Start_Date = min(ds), End_Date = max(ds), Total_kwh = sum(yhat))
                print(summary)
            })
            shinyjs::hide("dygraph_spinner")
        }
    })
    
} # closes server


# Run the application
shinyApp(ui, server)

