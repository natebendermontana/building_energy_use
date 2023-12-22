##########################################################################
forecast_static_min <- max(df$date)+1 # possible predictions start at latest historical day +1
potential_forecast_window <- 365*3 # can forecast up to three years beyond the day after the latest historical date
potential_forecast_window_end <- forecast_static_min + potential_forecast_window

forecast_window_start <- as.Date("2024-01-01", tz = "UTC")
forecast_window_end <- as.Date("2024-12-31", tz = "UTC")
total_dates <- seq.Date(forecast_static_min, potential_forecast_window_end, by = "day")
potential_total_length <- length(total_dates)

## BASELINE ##
set.seed(12923)
base_prices_df <- generate_energyprices(df, total_dates, potential_total_length, useradjust_energyprices = 0) # generate full simulations in order to filter as needed later
# print(paste("prices_df dates: ", head(prices_df$ds)))
print("BASELINE: unfiltered prices & dates:")
print(paste("Date:", base_prices_df$ds[1:3], "Price:", base_prices_df$price_per_kwh[1:3]))

base_filtered_prices <- base_prices_df %>%
  filter(ds >= forecast_window_start & ds <= forecast_window_end)

print("BASELINE: filtered prices dates:")
print(min(base_filtered_prices$ds))
print(max(base_filtered_prices$ds))

print("BASELINE: filtered prices first prices:")
print(paste("Date:", base_filtered_prices$ds[1:3], "Price:", base_filtered_prices$price_per_kwh[1:3]))

# Calculate the total predicted energy usage multiplied by the price for each day
base_filtered_energy_cost <- sum(base_filtered_scenario$yhat * base_filtered_prices$price_per_kwh) #row-wise cost calc


##########################################################################
## FORECAST ##
set.seed(12923)
prices_df <- generate_energyprices(df, total_dates, potential_total_length, useradjust_energyprices = 0) # generate full simulations in order to filter as needed later
# print(paste("prices_df dates: ", head(prices_df$ds)))
# print(paste("prices_df price: ", head(prices_df$price_per_kwh)))
# print(min(prices_df$ds))
# print(max(prices_df$ds))

filtered_prices <- prices_df %>%
  filter(ds >= forecast_window_start & ds <= forecast_window_end)

print("FORECAST: filtered prices dates:")
print(min(filtered_prices$ds))
print(max(filtered_prices$ds))

print("FORECAST: filtered prices first prices:")
print(paste("Date:", filtered_prices$ds[1:3], "Price:", filtered_prices$price_per_kwh[1:3]))


# Calculate the total predicted energy usage multiplied by the price for each day
filtered_energy_cost <- sum(filtered_scenario$yhat * filtered_prices$price_per_kwh) #row-wise cost calc