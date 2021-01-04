rm(list=ls())
cat("\014")

source("mm_lib.R")

# Choose the number of people and products
n <- 50
products <- c("coca", "fanta", "pepsi", "sprite")

time_max <- 10


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

clients_ids <- which(V(population)$class == "C")
for(c in clients_ids){
  p = runif(1)

  for(i in 1:Np){
    if(p <= sum(preferences[1:i,c])){
      V(population)[c]$product <- products[i]
      break
    }
  }
}

preferences <- market_simulation(population, products, preferences, prices, time_max)
plot_population(population, products, layout_P)




