
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
library(shinydashboardPlus)
library(shinyjs)
library(rintrojs)
library(shinycssloaders)

# Setup ####
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


# Variable creation functions #
# Func: Num People ####
# building-specific adjustments to the num_ppl distributions
generate_num_ppl <- function(pred_building, total_dates, potential_total_length, mean_factor_weekday, mean_factor_weekend,
                             scale_factor_weekday, scale_factor_weekend, sd_factor_adjust, useradjust_num_ppl) {
    
    weekdays <- ifelse(lubridate::wday(total_dates) %in% c(1, 7), 0, 1) # 0 for weekend, 1 for weekday
    num_ppl <- numeric(length = potential_total_length)
    pcnt_adjustment = 1 + (useradjust_num_ppl / 100)
    
    for (i in 1:(potential_total_length)) {
        if (weekdays[i] == 1) {
            # Generate more people on weekdays
            mean_factor <- 300 + mean_factor_weekday[pred_building]*pcnt_adjustment
            scale_factor <- 5 + scale_factor_weekday[pred_building]
            sd_factor <- 20 + sd_factor_adjust[pred_building]   
        } else {
            # Generate fewer people on weekends
            mean_factor <- 70 + mean_factor_weekend[pred_building]*pcnt_adjustment
            scale_factor <- 5 + scale_factor_weekend[pred_building]
            sd_factor <- 10 + sd_factor_adjust[pred_building]   
        }
        
        num_ppl[i] <- as.integer(pmax(0, pmin(500, rgamma(1, shape = 2, scale = scale_factor) + 1 +
                                                  abs(round(rnorm(1, mean = mean_factor, sd = sd_factor), 0)))))
    }
    
    return(num_ppl)
}

# Func: HVAC Efficiency ####
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







# original UI ####
# ui <- dashboardPage(
#     dashboardHeader(title = "Energy Use Analysis and Forecast"),
#     dashboardSidebar(
#         width = 220,
#         sidebarMenu(
#             menuItem("EDA Plots", tabName = "eda_plots", icon = icon("line-chart")),
#             #           menuItem("Energy Use Prediction", tabName = "total_prediction", icon = icon('chart-line')),
#             menuItem("Scenario Planning", tabName = "scenario_planning", icon = icon("sliders"))
#         )
#     ),
#     dashboardBody(
#         tabItems(
#             # EDA Plots tab content
#             tabItem(tabName = "eda_plots",
#                     fluidRow(
#                         box(title = "Filters", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
#                             fluidRow(
#                                 column(6,
#                                        selectInput("eda_building", "Choose Building", choices = c("Grand Budapest Hotel" = "budapest",
#                                                                                                   "Nakatomi Plaza" = "nakatomi",
#                                                                                                   "Wayne Manor" = "wayne_manor"))
#                                 ),
#                                 column(6,
#                                        selectInput("variable", "Choose Variable",
#                                                    choices = c("Number of People" = "num_ppl",
#                                                                "Temperature (F)" = "temp",
#                                                                "Wind Speed (mph)" = "wind_speed",
#                                                                "Cloud Cover (%)" = "cloud_cover",
#                                                                "Equipment Efficiency Rating" = "equip_efficiency",
#                                                                "HVAC Efficiency Rating" = "hvac_efficiency",
#                                                                "Building Size" = "bldg_area",
#                                                                "Total" = "total"))
#                                 )
#                             ),
#                             fluidRow(
#                                 column(12,
#                                        sliderInput("time_period", "Select Time Period",
#                                                    min = min(df$date),
#                                                    max = max(df$date),
#                                                    value = c(min(df$date), max(df$date)),
#                                                    timeFormat="%Y-%m-%d",
#                                                    step = 1)
#                                 )
#                             )
#                         ),
#                         box(plotOutput("edaPlot"), width=12)
#                     )
#             ),
#              # Scenario Planning tab content
#             tabItem(tabName = "scenario_planning",
#                     fluidRow(
#                         box(
#                             title = "Filters", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
#                             fluidRow(
#                                 column(6,
#                                        selectInput("scenario_building", "Choose Building",
#                                                    choices = c("Grand Budapest Hotel" = "budapest",
#                                                                "Nakatomi Plaza" = "nakatomi",
#                                                                "Wayne Manor" = "wayne_manor"))
#                                 ),
#                                 column(6,
#                                        sliderInput("scenario_range", "Dates to Forecast",
#                                                    min = max(df$date) + 1,
#                                                    max = max(df$date) + potential_forecast_window,
#                                                    value = c(min(df$date)+1, max(df$date)+potential_forecast_window),
#                                                    timeFormat = "%Y-%m-%d",
#                                                    step = 1)
#                                 ),
#                                 column(6,
#                                        sliderInput("useradjust_num_ppl", "Adjustment: Number of People",
#                                                    min = -100, max = 100, value = 0)
#                                 ),
#                                 column(6,
#                                        sliderInput("useradjust_equip_efficiency", "Investment: Equipment Efficiency",
#                                                    min = -100000, max = 100000, value = 0, step = 100)
#                                 ),
#                                 column(6,
#                                        sliderInput("useradjust_hvac_efficiency", "Investment: HVAC Efficiency",
#                                                    min = -100000, max = 100000, value = 0, step = 100)
#                                 ),
#                                 column(6,
#                                        actionButton("run_scenario", "Run Scenario")
#                                 )
#                             )
#                         )
#                     ),
#                     dygraphOutput("scenario_plot"),
#                     verbatimTextOutput("scenario_details")
#             )
#         )
#     )
# )





##### UI FOR DEBUGGING ######
# jsCode <- "
# shinyjs.startIntro = function() {
#   var intro = introJs();
#   intro.start();
# 
#   intro.onchange(function(targetElement) {
#     Shiny.setInputValue('currentStep', intro._currentStep);
#   });
# }
# "

# ABSOLUTE BARE-BONES UI FOR TESTING
# ui <- fluidPage(
#     dashboardPage(
#         dashboardHeader(title = "Energy Use Analysis and Forecast"),
#         dashboardSidebar(
#             sidebarMenu(
#                 menuItem("EDA Plots", tabName = "eda_plots", icon = icon("bar-chart")),
#                 menuItem("Scenario Planning", tabName = "scenario_planning", icon = icon("sliders"))
#             )
#         ),
#         dashboardBody(
#             tabItems(
#                 id = "main_tabitems",
#                 tabItem(tabName = "eda_plots", "EDA Plots Content"),
#                 tabItem(tabName = "scenario_planning", "Scenario Planning Content")
#             )
#         )
#     )
# )


# UI ####
# JavaScript code for Intro.js step change and start



# js_code <- "
# shinyjs.startIntro = function() {
#   var intro = introJs();
#   intro.start();
# 
#   intro.onafterchange(function(targetElement) {
#     console.log('Current Step:', intro._currentStep);
#     Shiny.setInputValue('currentStep', intro._currentStep);
#   });
# }
# "

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
            )
            # introBox(
            #   actionButton("start_tour", "Take the Tour"),  # Add the custom button
            #   data.step = 12,
            #   data.intro = "Click here to start this tour again at any time."
            # )
          )
        ),
        dashboardBody(
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
                            box(title = "Options", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                fluidRow(
                                    column(6,
                                           introBox(
                                           selectInput("eda_building", "Choose Building",
                                                       choices = c("Grand Budapest Hotel" = "budapest",
                                                                   "Nakatomi Plaza" = "nakatomi",
                                                                   "Wayne Manor" = "wayne_manor")),
                                           data.step = 4,
                                           data.intro = "Each building is set up with unique characteristics and usage patterns that affect its energy usage."
                                           )
                                    ),
                                    column(6,
                                           introBox(
                                           selectInput("variable", "Choose Variable",
                                                       choices = c("Number of People" = "num_ppl",
                                                                   "Temperature (F)" = "temp",
                                                                   "Wind Speed (mph)" = "wind_speed",
                                                                   "Cloud Cover (%)" = "cloud_cover",
                                                                   "Equipment Efficiency Rating" = "equip_efficiency",
                                                                   "HVAC Efficiency Rating" = "hvac_efficiency",
                                                                   "Building Size" = "bldg_area",
                                                                   "Total" = "total")),
                                           data.step = 5,
                                           data.intro = "Here in the EDA Plots tab, you can choose a variable to see how it's been simulated for a particular building."
                                           )
                                    )
                                )
                                )
                            ),
                                fluidRow(
                                    column(12,
                                           introBox(
                                           sliderInput("time_period", "Select Time Period",
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
                        introBox(
                          box(shinycssloaders::withSpinner(plotOutput("edaPlot")), width=12),
                          data.step = 7,
                          data.intro = "The plot will dynamically update as you choose new date ranges or variables to explore."
                        )
                        ),
                
                tabItem(tabName = "scenario_planning",
                        fluidRow(
 #                         introBox(
                            box(title = "Scenario Parameters",
                              selectInput("scenario_building", "Choose Building",
                                              choices = c("Grand Budapest Hotel" = "budapest",
                                                          "Nakatomi Plaza" = "nakatomi",
                                                          "Wayne Manor" = "wayne_manor")),
                                sliderInput("scenario_range", "Dates to Forecast",
                                            min = min(df$date),
                                            max = max(df$date) + potential_forecast_window,
                                            value = c(min(df$date),
                                                      max(df$date)+ potential_forecast_window),
                                            timeFormat="%Y-%m-%d",
                                            step = 1),
                              
#                            data.step = 8,
#                            data.intro = "Choose your building and timeframe for forecasting. You can use the model to forecast over the historical data as well as make future predictions.
#                            <br><br> The forecast is limited to three years in advance; predictions farther out than that are unreliable."
#                            ),
                          
#                              introBox(
                              sliderInput("useradjust_num_ppl", "Adjustment: Number of People",
                                            min = -100, max = 100, value = 0),
#                              data.step = 9,
#                              data.intro = "Forecasted people explanation."
#                              ),
#                              introBox(
                                sliderInput("useradjust_equip_efficiency", "Investment: Equipment Efficiency",
                                            min = -100000, max = 100000, value = 0, step = 100),
#                                data.step = 10,
#                                data.intro = "Equip efficiency explanation."
#                              ),
#                              introBox(
                                sliderInput("useradjust_hvac_efficiency", "Investment: HVAC Efficiency",
                                            min = -100000, max = 100000, value = 0, step = 100),
#                              data.step = 11,
#                              data.intro = "HVAC efficiency explanation."
#                            ),
                            actionButton("run_scenario", "Run Scenario")
                                )),
                        hidden(div(id = 'scenario_plotdiv', withSpinner(dygraphOutput("scenario_plot")),
                                   withSpinner(verbatimTextOutput("scenario_details"))))
        )
    )
)
)
)
)






# Server ####
server <- function(input, output, session) {
    
  # Ensure the events list for the tour is the same no matter if the tour starts automatically or with the Start Tour button
  # This events list switches from the EDA Plots tab to the Scenario tab at the right moment (currentStep>=10) in the tour. 
    get_tour_events <- function() {
      list(
        "onchange" = I("if (this._currentStep<=7) {
        $('a[data-value=\\\"scenario_planning\\\"]').removeClass('active');
        $('a[data-value=\\\"eda_plots\\\"]').addClass('active');
        $('a[data-value=\\\"eda_plots\\\"]').trigger('click');
      }
      if (this._currentStep>=8) {
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
                               "num_ppl" = "Number of People", 
                               "temp" = "Temperature (F)", 
                               "wind_speed" = "Wind Speed (mph)", 
                               "cloud_cover" = "Cloud Cover (%)", 
                               "equip_efficiency" = "Equipment Efficiency Rating", 
                               "hvac_efficiency" = "HVAC Efficiency Rating", 
                               "total" = "Total")
    
    # EDA Plots
    observeEvent(input$plot, {
        output$edaPlot <- renderPlot({
            selected_bldg <- pretty_building_names[input$eda_building]
            selected_var <- pretty_variable_names[input$variable]
            start_date <- as.Date(input$time_period[1], origin="1970-01-01")
            end_date <- as.Date(input$time_period[2], origin="1970-01-01")
            
            plot_df <- df %>% 
                filter(bldg_name == input$eda_building, date >= start_date & date <= end_date)
            
            # Check if 'num_ppl' is the selected variable, plot differently if so. 
            if (input$variable == "num_ppl") {
                plot_df <- plot_df %>% 
                    mutate(day_type = ifelse(weekday == 1, "Weekday", "Weekend")) %>%
                    group_by(date, day_type) %>%
                    summarise(num_ppl = sum(num_ppl))
                
                ggplot(plot_df, aes(x = date, y = num_ppl, color = day_type)) +
                    geom_line() +
                    labs(x = "Date", y = "Number of People") +
                    theme_minimal()
            } else {
                ggplot(plot_df, aes_string(x = "date", y = input$variable)) +
                    geom_line() +
                    labs(x = "Date", y = selected_var) +
                    theme_minimal()
            }
        })
    }, ignoreNULL = FALSE)
    
    # observe event to show/hide the scenarioplot loading animation
    observe({
        toggle(id = 'scenario_plot', condition = FALSE)
        if(input$run_scenario > 0 ){
            show('scenario_plotdiv')
            toggle(id = 'scenario_plot', condition = TRUE)
        }
    })
    
    # Server code for Scenario Planning - run the full simulations
    observeEvent(input$run_scenario, {
        # Prepare historical data with adjusted regressors
        scenario_data <- df %>%
            filter(bldg_name == input$scenario_building) %>%
            rename(ds = date, y = total) %>% 
            mutate(ds = with_tz(ds, tzone = "UTC")) 
        
        
        # Create a Prophet model
        m <- prophet()
        # # Add each predictor as a regressor
        m <- add_regressor(m, 'num_ppl')
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
            
            # building-specific adjustments for num_ppl
            mean_factor_weekday <- c(nakatomi = 60, wayne_manor = -200, budapest = 0)
            mean_factor_weekend <- c(nakatomi = 150, wayne_manor = -40, budapest = 0)
            scale_factor_weekday <- c(nakatomi = 0, wayne_manor = -1, budapest = 1)
            scale_factor_weekend <- c(nakatomi = 0, wayne_manor = -1, budapest = 1)
            sd_factor_adjust <- c(nakatomi = 20, wayne_manor = -10, budapest = 2)
            future_full <- add_future_regressor(
                df = df,
                future_df = future_full,
                pred_building = input$scenario_building,
                variable_name = "num_ppl",
                future_variable = generate_num_ppl(pred_building = input$scenario_building,
                                                   total_dates, potential_forecast_window,
                                                   mean_factor_weekday, mean_factor_weekend,
                                                   scale_factor_weekday, scale_factor_weekend,
                                                   sd_factor_adjust,
                                                   useradjust_num_ppl = input$useradjust_num_ppl)
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
            
            output$scenario_plot <- renderDygraph({
                dyplot.prophet(m, filtered_scenario)
            })
            
            output$scenario_details <- renderPrint({
                summary <- filtered_scenario %>%
                    summarise(Start_Date = min(ds), End_Date = max(ds), Total_kwh = sum(yhat))
                print(summary)
            })
        }
    })
    
} # closes server


# Run the application
shinyApp(ui, server)







# 
# 
# ui <- fluidPage(
#   useShinyjs(),
#   selectInput(inputId = "something",
#               label = "Select something:",
#               choices = c('','None', 'All', 'Some'),
#               selected = ''),
#   hidden(div(id = 'test', withSpinner(textOutput(outputId = "text"))))
# )
# 
# server <- function(input, output) {
#   observe({
#   print(paste("initial load:", nchar(input$something)))
#   })
# 
#   observe({
#     toggle(id = 'text', condition = FALSE)
# 
#     if(nchar(input$something) > 0 ){
#       show('test')
#       toggle(id = 'text', condition = TRUE)
#       Sys.sleep(1)
#       output$text <- renderText(paste("You chose ", input$something))
#     }
#     print(paste("after render:", nchar(input$something)))
# 
#   })
# }
# 
# shinyApp(ui, server)


# ui = shinyUI(tagList(
#   introjsUI(),
#   navbarPage(
#     "Old Faithful Geyser Data",
# 
#     tabPanel(
#       id = "fTab",
#       "First tab",
#       introBox(
#         h1("Basic Usage"),
#         data.step = 1,
#         data.intro = "This is a tooltip"
#       ),
#       sliderInput(
#         "bins",
#         "Number of bins:",
#         min = 1,
#         max = 50,
#         value = 30
#       ),
# 
#       plotOutput("distPlot"),
#       actionButton("startButton", "Help")
#     ),
#     tabPanel(
#       tabName = "sTab",
#       "Second tab",
#       id = "tt",
#       introBox(
#         h1("Basic Usage 2"),
#         data.step = 2,
#         data.intro = "This is a second tooltip"
#       ),
#       sliderInput(
#         "bins2",
#         "Number of bins:",
#         min = 1,
#         max = 50,
#         value = 30
#       ),
# 
#       plotOutput("distPlot2")
#     )
#   )
# ))
# 
# server = shinyServer(function(input, output, session) {
#   output$distPlot <- renderPlot({
#     x    <- faithful[, 2]
#     bins <- seq(min(x), max(x), length.out = input$bins + 1)
# 
# 
#     hist(x,
#          breaks = bins,
#          col = 'darkgray',
#          border = 'white')
# 
#   })
#   output$distPlot2 <- renderPlot({
#     x    <- faithful[, 2]
#     bins <- seq(min(x), max(x), length.out = input$bins2 + 1)
# 
# 
#     hist(x,
#          breaks = bins,
#          col = 'darkgray',
#          border = 'white')
# 
#   })
# 
#   observeEvent(input$startButton, {
#     introjs(
#       session,
#       events = list(
#         "onchange" = I("if (this._currentStep==0) {
#         $('a[data-value=\"Second tab\"]').removeClass('active');
#         $('a[data-value=\"First tab\"]').addClass('active');
#         $('a[data-value=\"First tab\"]').trigger('click');
#   }
#         if (this._currentStep==1) {
#         $('a[data-value=\"First tab\"]').removeClass('active');
#         $('a[data-value=\"Second tab\"]').addClass('active');
#         $('a[data-value=\"Second tab\"]').trigger('click');
#         }"
#       )
#       )
#     )
#   })
# 
# })
# 
# shinyApp(ui = ui, server = server)
