library(tibble)
library(tidyr)
library(lubridate)
library(MASS)
library(ggplot2)
library(GGally)
library(here)
library(reshape2)
library(dplyr)
library(Hmisc)

# Set a seed for reproducibility
set.seed(12923)

#### SETUP, VARIABILITY & UNIQUENESS SETTINGS #####################
# Time period
start_date <- as.Date("2018-01-01")
end_date <- as.Date("2023-12-13")
dates <- seq.Date(start_date, end_date, by = "day")
days <- length(dates)

buildings <- c("nakatomi", "wayne_manor", "budapest")
noise_factor <- .5 # amount of noise to introduce in each var
wind_speed_adjust <- c(nakatomi = 2, wayne_manor = 0, budapest = -2)
# Building-specific temperature adjustments to simulate the 
# bldgs being in different areas of the city
temp_adjust <- c(nakatomi = 3, wayne_manor = 0, budapest = -3)
# no adjust to cloud_cover; it's all the same grey everywhere in Seattle :)
# number of people - in the num_ppl section


bldg_names <- rep(buildings, each = days)
dates_rep <- rep(dates, times = length(buildings))
weekdays <- ifelse(wday(dates_rep, week_start = 1) %in% c(6, 7), 0, 1) # 0 for weekend, 1 for weekday, with Mon being the first day of the week
weekdays_singlebldg <- ifelse(wday(dates, week_start = 1) %in% c(6, 7), 0, 1) # 0 for weekend, 1 for weekday, with Mon being the first day of the week

# Unique ID for each row
ids <- seq_len(days * length(buildings))



#### ENERGY COSTS ###########################
simulate_energy_prices <- function(start_date, end_date) {
  dates <- seq.Date(as.Date(start_date), as.Date(end_date), by = "day")
  current_price <- 0.225 # Initial average price
  
  # Create a matrix to store price data
  price_data <- matrix(nrow = length(dates), ncol = 1)
  
  for (date_idx in 1:length(dates)) {
    # Apply a very small daily change to simulate long-term trends
    yearly_trend <- rnorm(1, mean = 0.00001, sd = 0.001) 
    
    # Simulate daily variation with a tighter range
    daily_change <- rnorm(1, mean = 0, sd = 0.001) 
    
    # Less frequent and significant random events
    if (runif(1) < 0.001) { # Reduced chance for a significant event
      event_change <- rnorm(1, mean = 0, sd = 0.002)
    } else {
      event_change <- 0
    }
    
    # Calculate the new price with tightened constraints
    new_price <- max(0.10, min(0.5, current_price + yearly_trend + daily_change + event_change))
    price_data[date_idx] <- new_price
    
    # Update the current price for the next day
    current_price <- new_price
  }
  
  # Return a data frame of dates and simulated prices
  data.frame(date = dates, price_per_kwh = price_data)
}

# Use the function with the provided start and end dates
temp_prices <- simulate_energy_prices(start_date, end_date)



#### BUILDING FOOTPRINT ##################### 
# (assume units: sq ft) (assuming they remain constant over the time period for simplicity)
# bldg_area <- round(runif(length(buildings), min = 5000, max = 25000)) # if you want to scramble bldg_size each run
bldg_area <- c(21110, 7260, 15200) # nakatomi, wayne_manor, budapest
bldg_area_named <- setNames(bldg_area, c("nakatomi", "wayne_manor", "budapest"))
bldg_area_rep <- rep(bldg_area, each = days)

#### NUMBER OF PEOPLE #####################
# number of people in each building, with reduced occupants on weekends and customized to each building
# num_ppl <- numeric(length = days * length(buildings))
# for (i in 1:(days * length(buildings))) {
#   building_name <- bldg_names[i]
#   if (weekdays[i] == 1) {
#     # Generate more people on weekdays
#     mean_factor <- 300 + mean_factor_weekday[building_name]
#     scale_factor <- 5 + scale_factor_weekday[building_name]
#     sd_factor <- 70 + sd_factor_adjust[building_name]   
#   } else {
#     # Generate fewer people on weekends
#     mean_factor <- 100 + mean_factor_weekend[building_name]
#     scale_factor <- 5 + scale_factor_weekend[building_name]
#     sd_factor <- 30 + sd_factor_adjust[building_name]   
#   }
#   
#   num_ppl[i] <- as.integer(pmax(0, pmin(2000, rgamma(1, shape = 2, scale = scale_factor) + 1 +
#                                           abs(round(rnorm(1, mean = mean_factor, sd = sd_factor), 0)))))
# }
# plot(num_ppl)
# 
# sqft_per_person <- numeric(length = length(num_ppl))
# 
# # Loop through the num_ppl array
# for (i in 1:length(num_ppl)) {
#   if (num_ppl[i] > 0) {
#     sqft_per_person[i] <- bldg_area_rep[i] / num_ppl[i]
#   } else {
#     sqft_per_person[i] <- NA  # Assign NA (or a large number) when there are no people
#   }
# }
# 
# plot(sqft_per_person)  # Plot the square feet per person

mean_factor_weekday <- c(nakatomi = 80, wayne_manor = -150, budapest = -7)
mean_factor_weekend <- c(nakatomi = 170, wayne_manor = 70, budapest = 10)
scale_factor_weekday <- c(nakatomi = 1, wayne_manor = -1, budapest = 1)
scale_factor_weekend <- c(nakatomi = 0, wayne_manor = -1, budapest = 1)
sd_factor_weekday <- c(nakatomi = 10, wayne_manor = -19, budapest = -7)
sd_factor_weekend <- c(nakatomi = 10, wayne_manor = -9, budapest = -7)


generate_num_ppl <- function(building_name, dates, weekdays_singlebldg, mean_factor_weekday, mean_factor_weekend,
                             scale_factor_weekday, scale_factor_weekend, sd_factor_weekday, sd_factor_weekend, bldg_area_named) {
  
  num_ppl_raw <- numeric(length = length(dates))
  sqft_per_person <- numeric(length = length(dates))
  
  for (i in 1:length(dates)) {
    if (weekdays_singlebldg[i] == 1) {  # Weekday
      mean_factor <- 300 + mean_factor_weekday[building_name]
      scale_factor <- 5 + scale_factor_weekday[building_name]
      sd_factor <- 20 + sd_factor_weekday[building_name]
    } else {  # Weekend
      mean_factor <- 70 + mean_factor_weekend[building_name]
      scale_factor <- 5 + scale_factor_weekend[building_name]
      sd_factor <- 10 + sd_factor_weekend[building_name]
    }
    
    num_ppl_raw[i] <- as.integer(pmax(0, pmin(2000, rgamma(1, shape = 2, scale = scale_factor) + 1 +
                                                abs(round(rnorm(1, mean = mean_factor, sd = sd_factor), 0)))))
    
    # Apply capping based on quantiles
#    u_limit <- quantile(uncapped_value, 0.95)  # 95th percentile as upper limit
#    l_limit <- quantile(uncapped_value, 0.05)  # 5th percentile as lower limit
#    num_ppl_raw[i] <- pmin(u_limit, pmax(l_limit, uncapped_value))
    
    sqft_per_person[i] <- if (num_ppl_raw[i] > 0) bldg_area_named[building_name] / num_ppl_raw[i] else NA
  }
  
  building_area <- rep(bldg_area_named[building_name], length(dates))
  
  return(data.frame(date = dates, weekdays = weekdays_singlebldg, bldg_name = building_name, bldg_area = building_area, num_ppl_raw = num_ppl_raw, sqft_per_person = sqft_per_person))
}

overall_ppl <- do.call(rbind, lapply(buildings, function(building_name) {
  generate_num_ppl(building_name, dates, weekdays_singlebldg, mean_factor_weekday, mean_factor_weekend,
                   scale_factor_weekday, scale_factor_weekend, sd_factor_weekday, sd_factor_weekend, bldg_area_named)}))

# check distributions if needed
# overall_ppl %>%
#   filter(bldg_name == "wayne_manor") %>%
#     ggplot(aes(x = date, y = num_ppl_raw, color = factor(weekdays))) + # 1=weekday, 0=weekend
#     geom_point() +  # Scatter plot
#     labs(title = "num people",
#          x = "Date",
#          y = "num people",
#          color = "Day Type") +
#     # scale_color_manual(values = c("blue", "red"),
#     #                    labels = c("Weekend", "Weekday")) +
#     theme_minimal()
# 
# describe(overall_ppl$num_ppl_raw)
#   
# overall_ppl %>%
#   filter(bldg_name == "wayne_manor") %>%
#   ggplot(aes(x = date, y = sqft_per_person, color = factor(weekdays))) +
#   geom_point() +  # Scatter plot
#   labs(title = "sqft_per_person",
#        x = "Date",
#        y = "sqft_per_person",
#        color = "Day Type") +
#   # scale_color_manual(values = c("blue", "red"),
#   #                    labels = c("Weekend", "Weekday")) +
#   theme_minimal()

#### /x/INSULATION QUALITY #####################
#### removing for now
# insulation <- as.integer(pmax(1, pmin(100, ceiling(rgamma(days * length(buildings), shape = 5, scale = 8.5) + 1) +
#                                         abs(round(rnorm(days * length(buildings), mean = 0, sd = noise_factor), 0)))))

#### /x/WINDOWS QUALITY #####################
#### removing for now
# windows <- as.integer(runif(days * length(buildings), min = 1, max = 100))

#### HVAC SYSTEM QUALITY #####################
# Initialize the starting hvac_efficiency for each building
start_hvac <- sample(40:100, length(buildings), replace = TRUE)

# Create a matrix to hold the efficiency data
hvac_data <- matrix(nrow = days, ncol = length(buildings))
colnames(hvac_data) <- buildings

# Simulate the efficiency rating for each building and each date
for (bldg in seq_along(buildings)) {
  current_hvac <- start_hvac[bldg]
  for (date_idx in 1:length(dates)) {
    # Apply a yearly decline to simulate aging
    yearly_decline <- (as.integer(-.02)) # add a very small daily negative constant to simulate long-term degradation
    # Simulate daily variation
    daily_change <- sample(c(-.20, 0, .20), 1, prob = c(0.045, 0.96, 0.005)) # Mostly no daily change, slightly better chance of small bad change
    # Random events: significant increase or decrease
    if (runif(1) < 0.004) { # 1% chance for a significant event, so roughly 3-4 times a year
      event_change <- sample(-10:10, 1) # equal chance of good or bad change
    } else {
      event_change <- 0
    }
    
    # Calculate the new efficiency
    new_hvac <- max(1, min(100, current_hvac + yearly_decline + daily_change + event_change))
    hvac_data[date_idx, bldg] <- new_hvac
    
    # Update the current efficiency for the next day
    current_hvac <- new_hvac
  }
}

# Reshape the matrix into a long format for plotting
hvac_long <- melt(hvac_data, varnames = c("date_idx", "bldg_name"), value.name = "hvac_efficiency")
hvac_long$date <- dates[hvac_long$date_idx]

hvac_long$date <- as.Date(hvac_long$date)
hvac_long$bldg_name <- as.factor(hvac_long$bldg_name)

ggplot(hvac_long, aes(x = date_idx, y = hvac_efficiency, group = bldg_name, color = bldg_name)) +
  geom_line() +
  theme_minimal() +
  labs(title = "HVAC Quality Over Time",
       x = "Row (Date Index)",
       y = "Rating") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.title = element_blank())

#### AMBIENT TEMPERATURE #####################
# Simulate temp across the year for a place like Seattle.
avg_low_temp <- 37  # Average low in Fahrenheit in winter
avg_high_temp <- 79 # Average high in Fahrenheit in summer
mean_temp <- (avg_low_temp + avg_high_temp) / 2
amplitude <- (avg_high_temp - avg_low_temp) / 2

# Temperature simulation
temp <- numeric(length = days * length(buildings))
for (i in 1:(days * length(buildings))) {
  building_name <- bldg_names[i]
  day_of_year <- as.numeric(format(dates_rep[i], "%j"))
  
  # Adjust mean temperature based on building
  mean_temp_bldg <- mean_temp + temp_adjust[building_name]
  
  # Sine wave for base temperature
  base_temp <- mean_temp_bldg + amplitude * sin(2 * pi * day_of_year / 365 - pi/2)
  
  # Adding random variation
  temp[i] <- as.integer(pmax(0, pmin(110, ceiling(base_temp + rnorm(1, mean = 0, sd = 5)))))
}


#### WIND SPEED #####################
# in mph
avg_wind_speed_low <- 3   # Average low wind speed in mph
avg_wind_speed_high <- 7 # Average high wind speed in mph
mean_wind_speed <- (avg_wind_speed_low + avg_wind_speed_high) / 2
amplitude_wind_speed <- (avg_wind_speed_high - avg_wind_speed_low) / 2

# Building-specific wind speed adjustments down at top in "variability and uniqueness" section

# Assuming 'days', 'bldg_names', and 'dates_rep' are defined as in your temperature simulation
wind_speed <- numeric(length = days * length(buildings))

for (i in 1:(days * length(buildings))) {
  building_name <- bldg_names[i]
  day_of_year <- as.numeric(format(dates_rep[i], "%j"))
  
  # Adjust mean wind speed based on building
  mean_wind_speed_bldg <- mean_wind_speed + wind_speed_adjust[building_name]
  
  # Sine wave for base wind speed + some random noise
  base_wind_speed_mph <- mean_wind_speed_bldg + amplitude_wind_speed * sin(2 * pi * day_of_year / 365 - pi/2) + rnorm(1, mean = 0, sd = 2)
  
  # Adding random variation
  wind_speed[i] <- as.integer(pmax(0, pmin(40, ceiling(base_wind_speed_mph + rnorm(1, mean = 0, sd = 2)))))
}

#### CLOUD COVER #####################
# percentage of cloud cover
avg_clear <- 0
avg_cloudy <- 100
clouds_amplitude <- (avg_cloudy - avg_clear) / 2
clouds_mean <- (avg_clear + avg_cloudy) / 2


# Phase shift to align the sine wave peaks and troughs with the cloudiest and clearest months
# Assuming January (cloudiest) aligns with the peak and August (clearest) with the trough
phase_shift <- pi / 2  # Adjusted based on the month (e.g., -pi/2 if peak is in January)

# Define the clearer and cloudier periods
# day_start_clear <- as.numeric(format(as.Date("2023-06-20"), "%j"))
# day_end_clear <- as.numeric(format(as.Date("2023-10-08"), "%j"))

cloud_cover <- numeric(length = days)

for (i in 1:(days * length(buildings))) {
  building_name <- bldg_names[i]
  day_of_year <- as.numeric(format(dates_rep[i], "%j"))
  
  # # Determine if the day is in the clearer or cloudier part of the year
  # in_clearer_period <- day_of_year >= day_start_clear && day_of_year <= day_end_clear
  # 
  # # Adjust amplitude based on the time of the year
  # adjusted_amplitude <- ifelse(in_clearer_period, amplitude_cloud_cover / 2, amplitude_cloud_cover)
  # 
  # Calculate cloud cover using a sine wave
  cloud_cover[i] <- clouds_mean + clouds_amplitude * sin(2 * pi * day_of_year / 365 + phase_shift)
  
  # Introduce random variation
  random_variation <- rnorm(1, mean = 0, sd = 5)
  cloud_cover[i] <- cloud_cover[i] + random_variation
  
  # Adjustments to ensure cloud cover stays within 0-100%
  cloud_cover[i] <- pmax(0, pmin(100, cloud_cover[i]))
}



#### /x/HVAC TYPE ##################### 
#### removing for now
# fuzzy_hvac_type <- function(insulation, hvac) {
#   probabilities <- c(
#     # Increase the probability of "MasterBlaster" for lower values of insulation and HVAC
#     "MasterBlaster" = pnorm(c(quantile(as.numeric(insulation), 0.85), quantile(as.numeric(hvac), 0.85)), mean = c(20, 20), sd = c(5, 5)),
#     
#     # Increase the probability of "Coolio" for higher values of insulation and HVAC
#     "Coolio" = pnorm(c(quantile(as.numeric(insulation), 0.35), quantile(as.numeric(hvac), 0.35)), mean = c(70, 70), sd = c(5, 5)),
#     
#     "Trane" = 1
#   )
#   
#   # Normalize the probabilities so they sum up to 1
#   probabilities <- probabilities / sum(probabilities)
#   
#   # Remove the percentage values from the names
#   names(probabilities) <- sub("\\.\\d+%$", "", names(probabilities))
#   
#   hvac_types <- names(probabilities)
#   assigned_type <- sample(hvac_types, size = 1, prob = probabilities)
#   
#   return(assigned_type)
# }
# 
# # Assuming hvac_type depends on insulation and hvac
# hvac_type <- sapply(1:(days * length(buildings)), function(i) fuzzy_hvac_type(insulation[i], hvac[i]))

#### EQUIPMENT EFFICIENCY ##################### 
# Initialize the starting efficiency for each building
# start_efficiency <- sample(40:100, length(buildings), replace = TRUE)
start_efficiency <- c(nakatomi = 60, wayne_manor = 40, budapest = 80)

# Create a matrix to hold the efficiency data
efficiency_data <- matrix(nrow = days, ncol = length(buildings))
colnames(efficiency_data) <- buildings

# Simulate the efficiency rating for each building and each date
for (bldg in seq_along(buildings)) {
  current_efficiency <- start_efficiency[bldg]
  for (date_idx in 1:length(dates)) {
    # Apply a yearly decline to simulate aging
    yearly_decline <- (as.integer(-.01)) # add a very small daily negative constant to simulate long-term degradation
    # Simulate daily variation
    daily_change <- sample(c(-.5, 0, .5), 1, prob = c(0.1, 0.8, 0.1)) # Mostly no change, equal chance of small good/bad change
    # Random events: significant increase or decrease
    if (runif(1) < 0.005) { # chance for a significant event, so roughly 3-4 times a year
      event_change <- sample(-10:10, 1)
    } else {
      event_change <- 0
    }
    
    # Calculate the new efficiency
    new_efficiency <- max(1, min(100, current_efficiency + yearly_decline + daily_change + event_change))
    efficiency_data[date_idx, bldg] <- new_efficiency
    
    # Update the current efficiency for the next day
    current_efficiency <- new_efficiency
  }
}

# Reshape the matrix into a long format for plotting
efficiency_long <- melt(efficiency_data, varnames = c("date_idx", "bldg_name"), value.name = "equip_efficiency")
efficiency_long$date <- dates[efficiency_long$date_idx]

efficiency_long$date <- as.Date(efficiency_long$date)
efficiency_long$bldg_name <- as.factor(efficiency_long$bldg_name)

ggplot(efficiency_long, aes(x = date_idx, y = equip_efficiency, group = bldg_name, color = bldg_name)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Equipment Efficiency Over Time",
       x = "Row (Date Index)",
       y = "Efficiency Rating") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.title = element_blank())



# Placeholder for total energy use
total <- rep(NA, days * length(buildings))

#### COMBINE VARS INTO TIBBLE #####################
df <- tibble(
  id = ids,
  bldg_name = as.factor(bldg_names),
  bldg_area = as.integer(bldg_area_rep),
  date = as.Date(dates_rep),
  year = as.integer(year(dates_rep)),
  month = month(dates_rep, label=T),
  day = wday(dates_rep, label=T),
  weekday = as.integer(weekdays),
  temp = as.integer(temp),
  wind_speed = as.integer(wind_speed),
  cloud_cover = as.integer(cloud_cover),
  total = total
)

overall_ppl <- overall_ppl[, c("date", "bldg_name", "num_ppl_raw", "sqft_per_person")]
df <- merge(df, overall_ppl, by = c("date", "bldg_name"), all.x = TRUE)

df <- merge(df, efficiency_long[, c("date", "bldg_name", "equip_efficiency")], by = c("date", "bldg_name"))
df <- merge(df, hvac_long[, c("date", "bldg_name", "hvac_efficiency")], by = c("date", "bldg_name"))
df <- merge(df, temp_prices, by = "date")



# ggplot(df, aes(x = date, y = wind_speed, color = bldg_name)) + 
#   geom_line(alpha=.5) +
#   labs(title = "Wind Speed by Building", x = "Date", y = "Wind Speed (mph)") +
#   theme_minimal() +
#   scale_color_brewer(palette = "Set1") # Use a color palette that is distinct and clear
# 
# ggplot(df, aes(x = date, y = cloud_cover)) + 
#   geom_line(alpha=.5) +
#   labs(title = "Cloud cover", x = "Date", y = "cloud cover (%)") +
#   theme_minimal() +
#   scale_color_brewer(palette = "Set1") # Use a color palette that is distinct and clear

# double check HVAC types
# ggplot(df, aes(x = insulation, y = hvac, color = hvac_type)) +
#   geom_point() +
#   labs(title = "Scatterplot of Insulation and HVAC Values",
#        x = "Insulation",
#        y = "HVAC",
#        color = "HVAC Type")


#### LINEAR MODEL PREDICTING TOTAL ENERGY USE #####################
# Generate coefficients for the linear model
coefficients <- c(
  bldg_area = .05,
  sqft_per_person = -.5,
  hvac_efficiency = -2,
  equip_efficiency = -2.2,
  temp = 1.1,
  wind_speed = .05,
  cloud_cover = .05
)

# Simulate the total energy use based on a linear model
df <- df %>%
  mutate(
    total = 
      bldg_area * coefficients["bldg_area"] +
      sqft_per_person * coefficients["sqft_per_person"] +
      hvac_efficiency * coefficients["hvac_efficiency"] +
      equip_efficiency * coefficients["equip_efficiency"] +
      temp * coefficients["temp"] +
      wind_speed * coefficients["wind_speed"] +
      cloud_cover * coefficients["cloud_cover"] +
      abs(rnorm(days, mean = 10, sd = 5))  # Adding some random noise
  )

# fix the col order
df <- df %>%
  select(
    date, price_per_kwh, bldg_name, id, bldg_area, year, month, day, weekday, num_ppl_raw,
    sqft_per_person, temp, wind_speed, cloud_cover, equip_efficiency,
    hvac_efficiency, total
  )

#### FINAL CHECKS #####################
# Create an index vector for variable order
# var_order <- c("bldg_area", "num_ppl_raw", "sqft_per_person", "equip_efficiency", 
#                "hvac_efficiency", "temp", "wind_speed", 
#                "cloud_cover", "total")
# col_indices <- match(var_order, names(df))

# Display parallel coordinates plot grouped by "hvac_type"
# ggparcoord(data = df,
#            columns = col_indices,
#            alphaLines = 0.1,
#            groupColumn = "bldg_name") + 
#   scale_color_manual(values = c("nakatomi" = "#20639b", "wayne_manor" = "grey", "budapest" = "grey"))
# 
# ggparcoord(data = df,
#            columns = col_indices,
#            alphaLines = 0.1,
#            groupColumn = "bldg_name") + 
#   scale_color_manual(values = c("nakatomi" = "grey", "wayne_manor" = "#20639b", "budapest" = "grey"))
# 
# ggparcoord(data = df,
#            columns = col_indices,
#            alphaLines = 0.1,
#            groupColumn = "bldg_name") + 
#   scale_color_manual(values = c("nakatomi" = "grey", "wayne_manor" = "grey", "budapest" = "#20639b"))


# 
# bldg <- "nakatomi"
# df %>% 
#   filter(bldg_name == bldg) %>% 
#   ggplot(aes(x = date, y = total)) +
#   geom_point() +
#   labs(title = paste("Total Energy Usage for", bldg, "Building"),
#        x = "Date",
#        y = "Total Energy Usage")
# 
# bldg <- "wayne_manor"
# df %>% 
#   filter(bldg_name == bldg) %>% 
#   ggplot(aes(x = date, y = total)) +
#   geom_point() +
#   labs(title = paste("Total Energy Usage for", bldg, "Building"),
#        x = "Date",
#        y = "Total Energy Usage")
# 
# bldg <- "budapest"
# df %>% 
#   filter(bldg_name == bldg) %>% 
#   ggplot(aes(x = date, y = total)) +
#   geom_point() +
#   labs(title = paste("Total Energy Usage for", bldg, "Building"),
#        x = "Date",
#        y = "Total Energy Usage")


# df %>%
#   # Filter for "nakatomi" building
#   filter(bldg_name == "budapest" & year(date) == 2019 & month(date)==2) %>%
#   # Keep date for plotting, exclude other non-numeric and specific columns
#   dplyr::select(date, num_ppl) %>%
#   # Gather numeric variables for scaling
#   pivot_longer(cols = -date, names_to = "variable", values_to = "value") %>%
#   # Scale numeric variables
#   group_by(variable) %>%
#   #  mutate(scaled_value = scale(value)) %>%
#   ungroup() %>%
#   # Plot
#   ggplot(aes(x = date, y = value, color = variable)) +
#   geom_line() +
#   labs(title = "Scaled Variables Over Time for Nakatomi Building",
#        x = "Date", y = "Value") +
#   theme_minimal() +
#   scale_color_viridis_d() +
#   theme(legend.position = "bottom")


#### WRITE FILE #####################
write.csv(df, here("data", "df_timeseries_orig.csv"), row.names = F)



