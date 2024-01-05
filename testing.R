# PSO stuff ##############
# library(prophet)
# library(pso)
# library(dplyr)
# library(lubridate)
# library(tictoc)
# library(ggplot2)
# library(plotly)
# 
# # Dynamic efficiency factor function
# efficiency_factor <- function(investment, base_factor, max_factor) {
#   # Linear or non-linear relationship can be defined here
#   # For simplicity, using a linear relationship capped at max_factor
#   factor = base_factor + 0.0005 * investment
#   return(min(factor, max_factor))
# }
# 
# calculate_cost <- function(hvac_efficiency, equip_efficiency) {
#   base_energy_consumption = 50000
#   energy_price_per_kwh = 0.2
#   
#   # Define the optimal points for both efficiencies
#   optimal_hvac = 27
#   optimal_equip = 25
#   
#   # Calculate the deviation from the optimal point
#   hvac_deviation = (hvac_efficiency - optimal_hvac)^2
#   equip_deviation = (equip_efficiency - optimal_equip)^2
#   
#   # Adjust the efficiency factors based on the deviation
#   hvac_efficiency_factor = 0.03 * (1 - hvac_deviation / optimal_hvac^2)
#   equip_efficiency_factor = 0.03 * (1 - equip_deviation / optimal_equip^2)
#   
#   # Ensure the efficiency factors do not become negative
#   hvac_efficiency_factor = max(hvac_efficiency_factor, 0)
#   equip_efficiency_factor = max(equip_efficiency_factor, 0)
#   
#   # Adjusted energy consumption based on efficiencies
#   adjusted_energy_consumption = base_energy_consumption * (1 - hvac_efficiency_factor * hvac_efficiency) * (1 - equip_efficiency_factor * equip_efficiency)
#   
#   # Inverted parabolic interaction term for cost
#   # This term will be minimum (hence most beneficial) at the optimal points
#   interaction_penalty = 1 + (hvac_deviation / optimal_hvac^2 + equip_deviation / optimal_equip^2)
#   
#   # Total cost calculation including the interaction term
#   total_cost = (adjusted_energy_consumption * energy_price_per_kwh) * interaction_penalty
#   return(total_cost)
# }
# 
# 
# objective_function <- function(x) {
#   hvac_efficiency = x[1]
#   equip_efficiency = x[2]
#   return(calculate_cost(hvac_efficiency, equip_efficiency))
# }
# 
# # PSO settings
# lower_bounds = c(0, 0)  # Lower bounds for HVAC and equipment efficiencies
# upper_bounds = c(100, 100)  # Upper bounds for HVAC and equipment efficiencies
# 
# 
# num_iter <- 30
# pso_results <- data.frame(hvac_efficiency = numeric(num_iter), equip_efficiency = numeric(num_iter), total_cost = numeric(num_iter))
# set.seed(123)
# tic()
# for (i in 1:num_iter) {
#   initial_params <- runif(2, min = lower_bounds, max = upper_bounds)
#   result <- psoptim(par = initial_params, fn = objective_function, lower = lower_bounds, upper = upper_bounds)
#   pso_results[i, ] <- c(result$par[1], result$par[2], result$value)
# }
# toc()
# 
# hist(pso_results$total_cost, main = "Distribution of Total Costs", xlab = "Total Cost", ylab = "Frequency")
# plot(pso_results$hvac_efficiency, pso_results$equip_efficiency, main = "HVAC vs. Equipment Efficiency", xlab = "HVAC Efficiency", ylab = "Equipment Efficiency", pch = 19)
# 
# 
# efficiency_values <- seq(0, 100, by = 1)
# hvac_costs_noise <- sapply(efficiency_values, function(eff) calculate_cost(eff, 50))  # Keep equipment efficiency constant
# equip_costs_noise <- sapply(efficiency_values, function(eff) calculate_cost(50, eff))  # Keep HVAC efficiency constant
# 
# # Plotting
# plot(efficiency_values, hvac_costs_noise, type = 'l', col = 'blue', xlab = 'Efficiency', ylab = 'Cost', main = 'Power Law Cost')
# lines(efficiency_values, equip_costs_noise, col = 'red')
# legend("topleft", legend = c("HVAC", "Equipment"), col = c("blue", "red"), lty = 1)
# 
#     
# # ALL POSSIBLE COMBINATIONS
# hvac_range <- seq(0, 100, by = 1)  # Range for hvac_efficiency
# equip_range <- seq(0, 100, by = 1)  # Range for equip_efficiency
# combinations <- expand.grid(hvac_efficiency = hvac_range, equip_efficiency = equip_range)
# 
# # Calculate total costs for each combination and add to the dataframe
# combinations$total_cost <- apply(combinations, 1, function(row) calculate_cost(row['hvac_efficiency'], row['equip_efficiency']))
# 
# # Find the combination with the lowest cost
# min_cost_row <- combinations[which.min(combinations$total_cost), ]
# 
# # Create the scatter plot
# ggplot(combinations, aes(x = hvac_efficiency, y = equip_efficiency, color = total_cost)) +
#   geom_point() +  # Plot all points
#   geom_point(data = min_cost_row, aes(x = hvac_efficiency, y = equip_efficiency), color = "black", size = 3) +  # Highlight the point with the minimum cost
#   scale_color_gradient(low = "blue", high = "red") +  # Color gradient for total cost
#   labs(title = "Total Cost for HVAC and Equipment Efficiencies",
#        x = "HVAC Efficiency",
#        y = "Equipment Efficiency",
#        color = "Total Cost") +
#   theme_minimal()


##############################

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
library(rintrojs)
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


############## Testing code ########

# Define parameters
mean_factor_weekday <- c(nakatomi = 80, wayne_manor = -150, budapest = -7)
mean_factor_weekend <- c(nakatomi = 170, wayne_manor = 45, budapest = 10)
scale_factor_weekday <- c(nakatomi = 1, wayne_manor = -1, budapest = 1)
scale_factor_weekend <- c(nakatomi = 0, wayne_manor = -1, budapest = 1)
sd_factor_weekday <- c(nakatomi = 10, wayne_manor = -69, budapest = -7)
sd_factor_weekend <- c(nakatomi = 10, wayne_manor = -29, budapest = -7)

forecast_static_min <- max(df$date) + 1
potential_forecast_window <- 365 * 3
potential_forecast_window_end <- forecast_static_min + potential_forecast_window

# Create the sequence of dates for future predictions
total_dates <- seq.Date(forecast_static_min, potential_forecast_window_end, by = "day")
potential_total_length <- length(total_dates)

# Run the function for testing
sqft_per_person_values <- generate_sqft_per_person(
  df, "nakatomi", total_dates, potential_total_length,
  mean_factor_weekday, mean_factor_weekend,
  scale_factor_weekday, scale_factor_weekend,
  sd_factor_weekday, sd_factor_weekend, 0  # useradjust_sqft_per_person set to 0
)

# Create a dataframe for plotting
plot_data <- data.frame(
  Date = total_dates,
  SqftPerPerson = sqft_per_person_values
)

# Plot using ggplot2
ggplot(plot_data, aes(x = Date, y = SqftPerPerson)) +
  geom_line() +
  labs(title = "Square Feet Per Person Over Time", x = "Date", y = "Square Feet Per Person") +
  theme_minimal()


# hvac efficiency
hvac_values <- generate_hvac_efficiency(
  df, "nakatomi", total_dates, potential_total_length, 0  # useradjust_sqft_per_person set to 0
)

# Create a dataframe for plotting
plot_data <- data.frame(
  Date = total_dates,
  hvac_plot = hvac_values
)

# Plot using ggplot2
ggplot(plot_data, aes(x = Date, y = hvac_plot)) +
  geom_line() +
  labs(title = "hvac Over Time", x = "Date", y = "hvac eff") +
  theme_minimal()

# equipment efficiency
equip_values <- generate_equip_efficiency(
  df, "nakatomi", total_dates, potential_total_length, 0  # useradjust_sqft_per_person set to 0
)

# Create a dataframe for plotting
plot_data <- data.frame(
  Date = total_dates,
  equip_plot = equip_values
)

# Plot using ggplot2
ggplot(plot_data, aes(x = Date, y = equip_plot)) +
  geom_line() +
  labs(title = "equipment eff Over Time", x = "Date", y = "equipment eff") +
  theme_minimal()

#####################################################
library(prophet)
library(dplyr)
library(lubridate)

# Create a simple fake dataset
set.seed(123)
ds <- seq(as.Date('2020-01-01'), as.Date('2021-01-01'), by = 'day')
y <- rnorm(length(ds), 50, 10)  # Random daily values
sqft_per_person <- rnorm(length(ds), 200, 20)  # Random regressor values
df <- data.frame(ds = ds, y = y, sqft_per_person = sqft_per_person)

# Prepare the Prophet model with 'sqft_per_person' as a regressor
m <- prophet()
m <- add_regressor(m, 'sqft_per_person')

# Fit the model
m <- fit.prophet(m, df)

# Create future dataframe for predictions
future <- make_future_dataframe(m, periods = 365)
future$sqft_per_person <- rnorm(nrow(future), 200, 20)  # Add future values for the regressor using same historical patterns

# Make predictions
forecast <- predict(m, future)

# Compare the regressor values before and after prediction
pre_post_comparison <- data.frame(
  Date = future$ds,
  Original_sqft_per_person = future$sqft_per_person,
  Predicted_sqft_per_person = forecast$sqft_per_person
)

# Print the first few rows to inspect
print(head(pre_post_comparison))
