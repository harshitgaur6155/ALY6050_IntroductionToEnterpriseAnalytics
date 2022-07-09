#-------- ALY6050_Module4Project_GaurH --------#

############################## PART 1 ##############################

# Annual Demand
annual_demand <- 16000

# Unit Cost
unit_cost <- 77

# Unit Holding Cost (Percentage converted to Fraction)
unit_holding_cost <- (17/100)

# Ordering Cost
ordering_cost <- 222

# No. of days in a year (considered a non-leap year)
no_of_days <- 365

# We wish to select the Order-Quantity that minimizes the Total Cost (Holding + Ordering).

# Let's first investigate how the Total-Cost changes with Order-Quantity
order_qty <- seq(100,8000,by=100)
total_cost <- (order_qty * ((unit_cost*unit_holding_cost)/no_of_days) * (order_qty/(annual_demand/no_of_days)) * (annual_demand/order_qty)) +
              (ordering_cost * (annual_demand/order_qty))
plot(order_qty, total_cost, 'l', main = "Total Cost vs Total Quantity", xlab = "Order Quantity (in Numeric)", ylab = "Total Cost (in Dollars)")

# From the plot, it looks like the minimum total cost is attained 
# when order-quantity is approximately around 300 - 700

# Zooming In:
order_qty <- seq(300,700,by=20)
total_cost <- (order_qty * ((unit_cost*unit_holding_cost)/no_of_days) * (order_qty/(annual_demand/no_of_days)) * (annual_demand/order_qty)) +
              (ordering_cost * (annual_demand/order_qty))
plot(order_qty, total_cost, 'l', main = "Total Cost vs Total Quantity", xlab = "Order Quantity (in Numeric)", ylab = "Total Cost (in Dollars)")

# Minimum Total-Cost Value:
total_cost[which.min(total_cost)]

# Minimum Order-Quantity Value:
order_qty[which.min(total_cost)]

## Using the optimise() function to solve this minimization problem.
order_qty_interval <- c(450,600)

## Function related to total_cost~order_qty to use for optimise() function
total_cost_fn <- function(ord_qty) {
  return ((ord_qty * ((77*(17/100))/365) * (ord_qty/(16000/365)) * (16000/ord_qty)) + (222 * (16000/ord_qty)))
}

## Use of optimise() function for the minimization problem.
optimise_cost_qty <- optimise(f = total_cost_fn, interval = order_qty_interval,
                              lower = min(order_qty_interval),
                              upper = max(order_qty_interval),
                              maximum = FALSE)

# Output the result of optimization (Minimum & Objective).
paste('Minimum Order Quantity : ', optimise_cost_qty$minimum)
paste('Objective Total Cost (Minimized) : ', optimise_cost_qty$objective)


############################## PART 2 ##############################

lower_demand_val <- 13000
upper_demand_val <- 18000
peak_demand_val <- 16000

# Random Number Generation for simulation
rand_simulation <- runif(1000)

# Triangle Distribution Method Formula
# K	= (c - a) / (b - a)
# M	= (b - a) * (c - a)
# N = (b - a) * (b - c)

K <- (peak_demand_val - lower_demand_val) / (upper_demand_val - lower_demand_val)
M <- (upper_demand_val - lower_demand_val) * (peak_demand_val - lower_demand_val)
N <- (upper_demand_val - lower_demand_val) * (upper_demand_val - peak_demand_val)

# If  r <= K, x = a + sqrt(r * M), Else x = b - sqrt((1 - r) * N)

triangular_dist_simulation <- ifelse(rand_simulation <= K, 
                                 round(lower_demand_val + sqrt(rand_simulation * M), 0), 
                                 round(upper_demand_val - sqrt((1 - rand_simulation) * N), 0))

simulation_df <-data.frame(matrix(ncol = 4, nrow = 0))
col_names_simulation_df <- c("Demand Value", "Order Quantity", "Minimum Total Cost", "Annual Number of Orders")
colnames(simulation_df) <- col_names_simulation_df

# Running the Simulation to find the Expected Values.
len_triangular_dist_sim <- length(triangular_dist_simulation)

for (j in 1:len_triangular_dist_sim) {
  optimise_cost_fn = function(ord_qty) {
    annual_demand <- triangular_dist_simulation[j]
    unit_cost <- 77
    ordering_cost <- 222
    unit_holding_cost <- 0.17
    
    total_cost <- (ordering_cost * (annual_demand/ord_qty)) + 
                  (ord_qty * ((unit_cost*unit_holding_cost)/no_of_days) * (ord_qty/(annual_demand/no_of_days)) * (annual_demand/ord_qty))
  }
  
  order_qty_threshold <- c(250, 7500)
  
  cost_optimization <- optimise(f = optimise_cost_fn, interval = order_qty_threshold,
                                lower = min(order_qty_threshold),
                                upper = max(order_qty_threshold),
                                maximum = FALSE)
  
  simulation_df[j, "Demand Value"] = triangular_dist_simulation[j]
  simulation_df[j, "Order Quantity"] = cost_optimization$minimum
  simulation_df[j, "Minimum Total Cost"] = cost_optimization$objective
  simulation_df[j, "Annual Number of Orders"] = triangular_dist_simulation[j]/cost_optimization$minimum
}

# Summary
summary(simulation_df)

# Testing the Best Fit of the distributions.
install.packages("fitdistrplus")
install.packages("logspline")
library(fitdistrplus)
library(logspline)

descdist(simulation_df$`Order Quantity`, discrete = FALSE)
descdist(simulation_df$`Minimum Total Cost`, discrete = FALSE)
descdist(simulation_df$`Annual Number of Orders`, discrete = FALSE)

# Fit Test for 'Order Quantity'
fit.weibull <- fitdist(simulation_df$`Order Quantity`, "weibull")
fit.gamma <- fitdist(simulation_df$`Order Quantity`, "gamma")
fit.norm <- fitdist(simulation_df$`Order Quantity`, "norm")
fit.unif <- fitdist(simulation_df$`Order Quantity`, "unif")

plot(fit.weibull)
plot(fit.gamma)
plot(fit.norm)
plot(fit.unif)

# Fit Test for 'Minimum Total Cost'
fit.weibull <- fitdist(simulation_df$`Minimum Total Cost`, "weibull")
fit.gamma <- fitdist(simulation_df$`Minimum Total Cost`, "gamma")
fit.norm <- fitdist(simulation_df$`Minimum Total Cost`, "norm")
fit.unif <- fitdist(simulation_df$`Minimum Total Cost`, "unif")

plot(fit.weibull)
plot(fit.gamma)
plot(fit.norm)
plot(fit.unif)

# Fit Test for 'Annual Number of Orders'
fit.weibull <- fitdist(simulation_df$`Annual Number of Orders`, "weibull")
fit.gamma <- fitdist(simulation_df$`Annual Number of Orders`, "gamma")
fit.norm <- fitdist(simulation_df$`Annual Number of Orders`, "norm")
fit.unif <- fitdist(simulation_df$`Annual Number of Orders`, "unif")

plot(fit.weibull)
plot(fit.gamma)
plot(fit.norm)
plot(fit.unif)

shapiro.test(simulation_df$`Order Quantity`)
shapiro.test(simulation_df$`Minimum Total Cost`)
shapiro.test(simulation_df$`Annual Number of Orders`)

#-------- END --------#

