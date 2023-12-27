
library(prophet)
library(pso)
library(dplyr)
library(lubridate)
library(tictoc)
library(ggplot2)
library(plotly)

# Dynamic efficiency factor function
efficiency_factor <- function(investment, base_factor, max_factor) {
  # Linear or non-linear relationship can be defined here
  # For simplicity, using a linear relationship capped at max_factor
  factor = base_factor + 0.0005 * investment
  return(min(factor, max_factor))
}

calculate_cost <- function(hvac_efficiency, equip_efficiency) {
  base_energy_consumption = 50000
  energy_price_per_kwh = 0.2
  
  # Define the optimal points for both efficiencies
  optimal_hvac = 27
  optimal_equip = 25
  
  # Calculate the deviation from the optimal point
  hvac_deviation = (hvac_efficiency - optimal_hvac)^2
  equip_deviation = (equip_efficiency - optimal_equip)^2
  
  # Adjust the efficiency factors based on the deviation
  hvac_efficiency_factor = 0.03 * (1 - hvac_deviation / optimal_hvac^2)
  equip_efficiency_factor = 0.03 * (1 - equip_deviation / optimal_equip^2)
  
  # Ensure the efficiency factors do not become negative
  hvac_efficiency_factor = max(hvac_efficiency_factor, 0)
  equip_efficiency_factor = max(equip_efficiency_factor, 0)
  
  # Adjusted energy consumption based on efficiencies
  adjusted_energy_consumption = base_energy_consumption * (1 - hvac_efficiency_factor * hvac_efficiency) * (1 - equip_efficiency_factor * equip_efficiency)
  
  # Inverted parabolic interaction term for cost
  # This term will be minimum (hence most beneficial) at the optimal points
  interaction_penalty = 1 + (hvac_deviation / optimal_hvac^2 + equip_deviation / optimal_equip^2)
  
  # Total cost calculation including the interaction term
  total_cost = (adjusted_energy_consumption * energy_price_per_kwh) * interaction_penalty
  return(total_cost)
}


objective_function <- function(x) {
  hvac_efficiency = x[1]
  equip_efficiency = x[2]
  return(calculate_cost(hvac_efficiency, equip_efficiency))
}

# PSO settings
lower_bounds = c(0, 0)  # Lower bounds for HVAC and equipment efficiencies
upper_bounds = c(100, 100)  # Upper bounds for HVAC and equipment efficiencies


num_iter <- 30
pso_results <- data.frame(hvac_efficiency = numeric(num_iter), equip_efficiency = numeric(num_iter), total_cost = numeric(num_iter))
set.seed(123)
tic()
for (i in 1:num_iter) {
  initial_params <- runif(2, min = lower_bounds, max = upper_bounds)
  result <- psoptim(par = initial_params, fn = objective_function, lower = lower_bounds, upper = upper_bounds)
  pso_results[i, ] <- c(result$par[1], result$par[2], result$value)
}
toc()

hist(pso_results$total_cost, main = "Distribution of Total Costs", xlab = "Total Cost", ylab = "Frequency")
plot(pso_results$hvac_efficiency, pso_results$equip_efficiency, main = "HVAC vs. Equipment Efficiency", xlab = "HVAC Efficiency", ylab = "Equipment Efficiency", pch = 19)


efficiency_values <- seq(0, 100, by = 1)
hvac_costs_noise <- sapply(efficiency_values, function(eff) calculate_cost(eff, 50))  # Keep equipment efficiency constant
equip_costs_noise <- sapply(efficiency_values, function(eff) calculate_cost(50, eff))  # Keep HVAC efficiency constant

# Plotting
plot(efficiency_values, hvac_costs_noise, type = 'l', col = 'blue', xlab = 'Efficiency', ylab = 'Cost', main = 'Power Law Cost')
lines(efficiency_values, equip_costs_noise, col = 'red')
legend("topleft", legend = c("HVAC", "Equipment"), col = c("blue", "red"), lty = 1)

    
# ALL POSSIBLE COMBINATIONS
hvac_range <- seq(0, 100, by = 1)  # Range for hvac_efficiency
equip_range <- seq(0, 100, by = 1)  # Range for equip_efficiency
combinations <- expand.grid(hvac_efficiency = hvac_range, equip_efficiency = equip_range)

# Calculate total costs for each combination and add to the dataframe
combinations$total_cost <- apply(combinations, 1, function(row) calculate_cost(row['hvac_efficiency'], row['equip_efficiency']))

# Find the combination with the lowest cost
min_cost_row <- combinations[which.min(combinations$total_cost), ]

# Create the scatter plot
ggplot(combinations, aes(x = hvac_efficiency, y = equip_efficiency, color = total_cost)) +
  geom_point() +  # Plot all points
  geom_point(data = min_cost_row, aes(x = hvac_efficiency, y = equip_efficiency), color = "black", size = 3) +  # Highlight the point with the minimum cost
  scale_color_gradient(low = "blue", high = "red") +  # Color gradient for total cost
  labs(title = "Total Cost for HVAC and Equipment Efficiencies",
       x = "HVAC Efficiency",
       y = "Equipment Efficiency",
       color = "Total Cost") +
  theme_minimal()


