rm(list=ls())
cat("\014")

source("mm_lib.R")

# Choose the number of people and products
n <- 100
products <- c("coca", "fanta", "pepsi", "sprite")

time_max <- 50


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

population <- product_choise(population, preferences, products)

preferences <- market_simulation(population, products, preferences, prices, time_max)
# plot_population(population, products, layout_P)

population_with_links <- add_links(population, products, metric = 2, n_links = round(n * 0.1,0))
preferences_with_links <- market_simulation(population_with_links, products, preferences, prices, time_max)


population_with_producers <- add_producers(population, products, metric = 2, n_producers = round(n * 0.03,0))
preferences_with_producers <- market_simulation(population_with_producers, products, preferences, prices, time_max)

# price_2_buy
#p2b = a/(price^(2+b))

# price_2_produce
# p2p = cost + a*price^(2+b)

# if (old_price * old_production < new_price * new_production)