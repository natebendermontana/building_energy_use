library(prophet)
library(dplyr)
library(ggplot2)
library(here)
library(lubridate)
library(tidyr)

potential_forecast_window <- 365*3 # can forecast up to three years


# Variable creation functions #
# Num People ####
generate_num_ppl <- function(pred_building, total_dates, forecast_total_length, mean_factor_weekday, mean_factor_weekend,
                             scale_factor_weekday, scale_factor_weekend, sd_factor_adjust, useradjust_num_ppl) {
  
  weekdays <- ifelse(lubridate::wday(total_dates) %in% c(1, 7), 0, 1) # 0 for weekend, 1 for weekday
  num_ppl <- numeric(length = forecast_total_length)
  pcnt_adjustment = 1 + (useradjust_num_ppl / 100)
  
  for (i in 1:(forecast_total_length)) {
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

# HVAC Efficiency ####
# Function to generate HVAC efficiency
generate_hvac_efficiency <- function(df, pred_building, total_dates, forecast_total_length, useradjust_hvac_efficiency) {
  # Conversion factor (Efficiency increase per $1,000)
  efficiency_per_thousand = 0.001
  # Total efficiency increase based on user investment
  total_efficiency_change = useradjust_hvac_efficiency * efficiency_per_thousand / 1000
  
  # Get the latest HVAC efficiency from historical data
  latest_hvac_efficiency <- df %>%
    filter(bldg_name == pred_building, date == max(date)) %>%
    pull(hvac_efficiency)
  
  hvac_efficiency <- numeric(length = forecast_total_length) # initialize placeholder var
  current_hvac <- latest_hvac_efficiency
  
  # Simulate the efficiency rating for this building across the total forecast length
  for (i in 1:forecast_total_length) {
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

# Equipment Efficiency ####
# Function to generate the equipment efficiency rating
generate_equip_efficiency <- function(df, pred_building, total_dates, forecast_total_length, useradjust_equip_efficiency) {
  # Conversion factor (Efficiency increase per $1,000)
  efficiency_per_thousand = 0.001
  # Total efficiency increase based on user investment
  total_efficiency_change = useradjust_equip_efficiency * efficiency_per_thousand / 1000
  
  # Get the latest HVAC efficiency from historical data
  latest_equip_efficiency <- df %>%
    filter(bldg_name == pred_building, date == max(date)) %>%
    pull(equip_efficiency)
  
  equip_efficiency <- numeric(length = forecast_total_length) # initialize placeholder var
  current_equip_efficiency <- latest_equip_efficiency
  
  # Simulate the efficiency rating for this building across the total forecast length
  for (i in 1:forecast_total_length) {
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


# Adding regressors to prophet future dataframe ####
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


# Load data etc
df <- read.csv(here("data", "df_timeseries_orig.csv"))

df <- df %>%
  mutate(
    bldg_name = as.factor(bldg_name),
    year = factor(year, levels = sort(unique(year)), ordered = TRUE),  # Dynamic levels for year
    date = as.Date(date),
    month = factor(month, levels = month.abb, ordered = TRUE),  # Use month.abb for abbreviated month names
    day = factor(day, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"), ordered = TRUE)  # Abbreviated day names
  )

pred_building = "nakatomi"
forecast_static_min <- as.Date("2023-12-13")
forecast_window_start <- as.Date("2018-01-01")
forecast_window_end <- as.Date("2026-03-28")
forecast_dates <- seq.Date(forecast_window_start, forecast_window_end, by = "day")
total_dates <- seq.Date(forecast_static_min, forecast_window_end, by = "day")

forecast_total_length <- length(total_dates)
forecast_window_length <- length(forecast_dates)

mean_factor_weekday <- c(nakatomi = 60, wayne_manor = -200, budapest = 0)
mean_factor_weekend <- c(nakatomi = 150, wayne_manor = -40, budapest = 0)
scale_factor_weekday <- c(nakatomi = 0, wayne_manor = -1, budapest = 1)
scale_factor_weekend <- c(nakatomi = 0, wayne_manor = -1, budapest = 1)
sd_factor_adjust <- c(nakatomi = 20, wayne_manor = -10, budapest = 2)

useradjust_num_ppl <- 0
useradjust_hvac_efficiency <- 0
useradjust_equip_efficiency <- 100000


# useradjust_hvac_efficiency <- 100000
# future_hvac_efficiency_100 <- generate_hvac_efficiency(df, pred_building, total_dates, forecast_total_length, useradjust_hvac_efficiency)
# 
# useradjust_hvac_efficiency <- 0
# future_hvac_efficiency <- generate_hvac_efficiency(df, pred_building, total_dates, forecast_total_length, useradjust_hvac_efficiency)
#  
# future_hvac_efficiency_neg50 <- generate_hvac_efficiency(df, pred_building, total_dates, forecast_total_length, useradjust_hvac_efficiency)
# 
#     
# future_num_ppl_100 <- generate_num_ppl(pred_building,
#                                       forecast_dates, forecast_length,
#                                       mean_factor_weekday, mean_factor_weekend,
#                                       scale_factor_weekday, scale_factor_weekend,
#                                       sd_factor_adjust, useradjust_num_ppl)
# 
# useradjust_num_ppl <- 50
# future_num_ppl_50 <- generate_num_ppl(pred_building,
#                                              forecast_dates, forecast_length,
#                                              mean_factor_weekday, mean_factor_weekend,
#                                              scale_factor_weekday, scale_factor_weekend, 
#                                              sd_factor_adjust, useradjust_num_ppl)
# 
# useradjust_num_ppl <- 0
# future_num_ppl <- generate_num_ppl(pred_building,
#                                              forecast_dates, forecast_length,
#                                              mean_factor_weekday, mean_factor_weekend,
#                                              scale_factor_weekday, scale_factor_weekend, 
#                                              sd_factor_adjust, useradjust_num_ppl)
# 
# 
# test_df <- tibble(
#   future_hvac_efficiency_100 = future_hvac_efficiency_100,
#   #  future_num_ppl_50 = future_num_ppl_50,
#   future_hvac_efficiency = future_hvac_efficiency,
#   future_hvac_efficiency_neg50 = future_hvac_efficiency_neg50,
#   total_dates = total_dates)
# 
# # Reshape the data to long format
# long_test_df <- test_df %>%
#   pivot_longer(cols = -total_dates,
#                names_to = "variable",
#                values_to = "num_ppl")
# 
# # Extract unique variable names
# unique_variables <- unique(long_test_df$variable)
# title_string <- paste("Comparison of", paste(unique_variables, collapse = " and "), "(Adjusted vs. Non-Adjusted)")
# 
# # Plotting
# ggplot(long_test_df, aes(x = total_dates, y = num_ppl, color = variable)) +
#   geom_line(linewidth = 1.3) +
#   labs(
#     title = title_string,
#     x = "Date",
#     y = "Value"
#   ) +
#   theme_minimal()


# Prophet model ####
data_selected <- df %>%
  filter(bldg_name == pred_building) %>%
  #            select(date, total) %>%
  rename(ds = date, y = total) %>% 
  mutate(ds = with_tz(ds, tzone = "UTC")) 

# Create data for future regressors



# print("PREDICTION SECTION df")
# print(head(data_selected))

m <- prophet(seasonality.mode = "additive", yearly.seasonality = TRUE, weekly.seasonality = TRUE, daily.seasonality = FALSE)
m <- add_regressor(m, 'num_ppl')
m <- add_regressor(m, 'hvac_efficiency')
m <- add_regressor(m, 'equip_efficiency')
m <- fit.prophet(m, data_selected)


# m <- prophet(data_selected, 
#              add_regressor(num_ppl),
#              seasonality.mode = "additive", # additive is the default
#              yearly.seasonality = T,
#              weekly.seasonality = T,
#              daily.seasonality = F) # make prophet model based on the data

future_full <- make_future_dataframe(m, periods = forecast_total_length) # create an empty future dataframe for the desired num of periods aka days

future_full <- add_future_regressor(
  df = df,
  future_df = future_full,
  pred_building = pred_building,
  variable_name = "num_ppl",
  future_variable = generate_num_ppl(pred_building,
                                     total_dates, forecast_total_length,
                                     mean_factor_weekday, mean_factor_weekend,
                                     scale_factor_weekday, scale_factor_weekend, 
                                     sd_factor_adjust, useradjust_num_ppl)
)

future_full <- add_future_regressor(
  df = df,
  future_df = future_full,
  pred_building = pred_building,
  variable_name = "hvac_efficiency",
  future_variable = generate_hvac_efficiency(df, pred_building,
                                             total_dates, forecast_total_length,
                                             useradjust_hvac_efficiency)
)

future_full <- add_future_regressor(
  df = df,
  future_df = future_full,
  pred_building = pred_building,
  variable_name = "equip_efficiency",
  future_variable = generate_equip_efficiency(df, pred_building,
                                              total_dates, forecast_total_length,
                                              useradjust_equip_efficiency)
)

forecast_full <- predict(m, future_full)

# print("PREDICTION SECTION - Predictions future df")
# print(head(forecast_full))

# Check if the forecast range inputs are available
# if (!is.null(input$forecast_range) && length(input$forecast_range) == 2) {
#   forecast_start_date <- as.Date(input$forecast_range[1])
#   forecast_end_date <- as.Date(input$forecast_range[2])

forecast_full <- forecast_full %>%
  mutate(ds = with_tz(ds, tzone = "UTC")) # match time zones to avoid error messages


# Filter the full forecast to only include the selected date range
filtered_forecast <- forecast_full %>%
  filter(ds >= forecast_window_start & ds <= forecast_window_end)


# plot(m, filtered_forecast) +
#       labs(title = paste(pred_building, "Forecast"), x = "Date", y = "Total Energy Use")

prophet_plot_components(m, filtered_forecast)

summary <- filtered_forecast %>% 
  summarise(Start_Date = min(ds), End_Date = max(ds), Total_kwh = sum(yhat))
print(summary)

dyplot.prophet(m, filtered_forecast)




# METRICS - TBD
# cv <- prophet::cross_validation(m, horizon = 500, units="days")
# 
# cv_metrics <- prophet::performance_metrics(cv)
# 
# plot_cross_validation_metric(cv, metric = "mae")
