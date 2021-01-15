rm(list=ls())
cat("\014")

source("mm_lib.R")

# Choose the number of people and products
n <- 500
products <- c("coca", "fanta", "pepsi", "sprite")

time_max <- 100


# Create the initial random graph with Clients and Producers
population <- init_population(n, products)
producers <- list_producers(population, products)
layout_P = layout_with_fr(population)

# Assign initial preferences

Na <- length(V(population))
Np <- length(products)
preferences <- init_preferences(Np, Na)

# Assign initial prices
mean_price <- 10
prices <- rnorm(Np, mean=mean_price, sd = mean_price/10)

# Initialization of initial sales
population <- product_choise(population, preferences, products)



## Biasing
population_with_links <- add_links(population, products, metric = 2, n_links = round(n * 0.1,0))
population_with_producers <- add_producers(population, products, metric = 2, n_producers = round(n * 0.03,0))

### SCENARIO 1 - NORMAL COEFFICIENTS
# Market Simulation Unbiased
# cat("\n ===== Unbiased ===== \n")
# preferences <- market_simulation(population, products, preferences, prices, time_max, coefficients = c(1, 1, 1, 1))
# # plot_population(population, products, layout_P)
# 
# # Market Simulation adding more edges
# cat("\n ===== Edges Modified ===== \n")
# preferences_with_links <- market_simulation(population_with_links, products, preferences, prices, time_max, coefficients = c(1, 1, 1, 1))
# 
# # Market Simulation adding more producers
# cat("\n ===== Producers Added ===== \n")
# preferences_with_producers <- market_simulation(population_with_producers, products, preferences, prices, time_max, coefficients = c(1, 1, 1, 1))


### SCENARIO 2 -PRICE IS CONSTANT
## coefficient = c(k_price_variation, k_distance, k_price, k_neightbors)

# ## Market Simulation Unbiased
# cat("\n ===== Unbiased ===== \n")
# preferences_price <- market_simulation(population, products, preferences, prices, time_max, coefficients = c(0, 1, 1, 1))
# 
# ## Market Simulation adding more edges
# cat("\n ===== Edges Modified ===== \n")
# preferences_with_links_price <- market_simulation(population_with_links, products, preferences, prices, time_max, coefficients = c(0, 1, 1, 1))
# 
# ## Market Simulation adding more producers
# cat("\n ===== Producers Added ===== \n")
# preferences_with_producers_price <- market_simulation(population_with_producers, products, preferences, prices, time_max, coefficients = c(0, 1, 1, 1))

### SCENARIO 3 - PRICE IS NOT RELEVANT ANYMORE for modifing the preferences
## coefficient = c(k_price_variation, k_distance, k_price, k_neightbors)

## Market Simulation Unbiased
cat("\n ===== Unbiased ===== \n")
preferences_price <- market_simulation(population, products, preferences, prices, time_max, coefficients = c(1, 1, 0, 1))

## Market Simulation adding more edges
cat("\n ===== Edges Modified ===== \n")
preferences_with_links_price <- market_simulation(population_with_links, products, preferences, prices, time_max, coefficients = c(1, 1, 0, 1))

## Market Simulation adding more producers
cat("\n ===== Producers Added ===== \n")
preferences_with_producers_price <- market_simulation(population_with_producers, products, preferences, prices, time_max, coefficients = c(1, 1, 0, 1))