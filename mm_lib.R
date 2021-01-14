library(igraph)
library(RColorBrewer)



# Funtion to generate a specific random graph
generate_graph <- function(selection, number_of_nodes, probability = 0.01){
  if(selection == 1){
    # erdos-renyi
    g_erdos <- erdos.renyi.game(number_of_nodes, probability,
                                type = c("gnp"), directed = FALSE, loops = TRUE)
    return(g_erdos)
  }
  else if (selection == 2){
    # watts-strogatz
    neighborhood_siz = round(runif(1,0,round(number_of_nodes/10, 0)),0)
    g_watts <- sample_smallworld(1, number_of_nodes, neighborhood_siz,
                                 probability)
    return(g_watts)
  }
  else {
    g_scalefree <- sample_pa(number_of_nodes, m = 2, directed = FALSE)
    return(g_scalefree)
  }
}

# Function that assigns preferences to each person randomly
# Return preferences matrix
init_preferences <- function(Np, Na){
  preferences <- matrix(0L, Np, Na)
  for(a in 1:Na){
    
    for(p in 1:(Np-1)){
      totalp <- 1 - sum(preferences[,a])
      preferences[p,a] <- runif(1,0,totalp)
    }
    
    preferences[Np, a] <- 1 - sum(preferences[,a])
  }
  
  return(preferences)
}


# Create the initiali random graph with Clients and Producers
# Return the population graph
init_population <- function(n, products){
  
  population <- generate_graph(3, n)
  Na <- length(V(population))
  
  V(population)$class <- "C" # Clients
  
  
  Np <- length(products)
  nproducteur = 1
  
  for(p in products){
    i <- 0
    while(i < nproducteur){
      id = sample(1:Na, 1)
      
      if(V(population)[id]$class == "C"){
        V(population)[id]$class <- "P"
        V(population)[id]$product <- p
        
        i <- i + 1
      }
    }
  }
  
  return(population)
}


# Return a list of lists with the index of each producers
list_producers <- function(population, products){
  Np <- length(products)
  
  producers <- list()
  for(p in 1:Np){
    producers[[p]] <- list()
  }
  
  producers_ids <- which(V(population)$class == "P")
  
  for(id in producers_ids){
    for(p in 1:Np){
      if(V(population)[id]$product == products[p]){
        producers[[p]][length(producers[p])] <- id
        break
      }
    }
  }
  
  return(producers)
}

market_simulation <- function(population, products, preferences, prices, time_max){
  
  Np <- length(products)
  k_prices <- 1
  
  prices_matrix <- matrix(0L, time_max, Np)
  prices_matrix[1:2,] <- prices
  sales_matrix <- matrix(0L, time_max, Np)
  for(p in 1:Np){
    clients_p <- which(V(population)$product == products[p])
    sales_matrix[1,p] <- length(clients_p)
  }
  
  #print(preferences[,50])
  for (t in 2:time_max){
    preferences <- update_preferences(population, Np, Na, preferences, prices, producers_list, products)
    
    # debug preferences function:
    #message("PREFERENCES N 50 at t = : ", t, "\n")
    #print(preferences[,50])
    #print(V(population)[50]$product)
    #message("\n\n")
    #•print(t)
    
    # Updating Product Preferences
    # (The one with most preference)
    
    population <- product_choise(population, preferences, products)
    
    # Update Sales
    for(p in 1:Np){
      clients_p <- which(V(population)$product == products[p])
      sales_matrix[t,p] <- length(clients_p)
    }
    
    # Update prices
    for(p in 1:Np){
      prices[p] <- prices[p] + k_prices * (sales_matrix[t-1]*prices_matrix[t-1] - sales_matrix[t]*prices_matrix[t]) / (sales_matrix[t]*prices_matrix[t])
    }
    if(t < time_max){
      prices_matrix[t+1,] <- prices
    }
    #message("Prices: ")
    #print(prices)
    
  }
  
  coul  <- brewer.pal(Np, "Paired")
  
  # Prices
  plot(1:time_max, prices_matrix[,1], col = coul[1], type = "l", ylim = c(min(prices_matrix)-1, max(prices_matrix)+1), xlim = c(1, time_max))
  for(p in 2:Np){
    lines(1:time_max, prices_matrix[,p], col = coul[p])
  }
  
  # Sales
  plot(1:time_max, sales_matrix[,1], col = coul[1], type = "l", ylim = c(min(sales_matrix)-1, max(sales_matrix)+1), xlim = c(1, time_max))
  for(p in 2:Np){
    lines(1:time_max, sales_matrix[,p], col = coul[p])
  }
  
  # Incomes
  income <- matrix(0L, time_max, Np)
  for(p in 1:Np){
    income[,p] = sales_matrix[,p] * prices_matrix[,p]
  }
  # print(income)
  # print(length(income))
  # print(1:time_max)
  
  plot(1:time_max, income[,1], col = coul[1], type = "l", ylim = c(min(income)-1, max(income)+1), xlim = c(1, time_max))
  for(p in 2:Np){
    lines(1:time_max, income[,p], col = coul[p])
  }
  
  
  return(preferences)
}

product_choise <- function(population, preferences, products){
  
  consumers_ids <- which(V(population)$class == "C")
  
  for(c in consumers_ids){
    p = runif(1)
    
    for(i in 1:Np){
      if(p <= sum(preferences[1:i,c])){
        V(population)[c]$product <- products[i]
        break
      }
    }
  }
  
  return(population)
}

update_preferences <- function(population, Np, Na, preferences, prices, producers_list, products){
  adjacent_list <- as_adj_list(population)
  consumers <- which(V(population)$class == "C")
  
  distance_coefficient <- 1;
  
  k_price <- 1;
  k_voisins <- 1;
  
  for(a in consumers){
    
    local_price <- rep(0L, Np)

    for(p in 1:Np){
      d <- distances(population, v = a)[1,unlist(producers[[p]])]
      local_price[p] <- prices[p] + d*distance_coefficient
    }
    
    Nv <- length(adjacent_list[[a]])
    
    for(p in 1:Np){
      #voisins_p <- V(population)[V(population)[adjacent_list[[a]]]$product == products[p] ]
      sum_aux <- 0
      for(v in adjacent_list[[a]]){
        if (V(population)[v]$product == products[p]){
          sum_aux <- sum_aux + 1
        }
      }
      
      sum_lp = sum(local_price)
      preferences[p,a] <- preferences[p,a] + k_price * (sum_lp - local_price[p])/sum_lp + k_voisins * sum_aux/Nv
      
    }
    
    # normalisation
    preferences[,a] <-  preferences[,a] / sum(preferences[,a])
  }
  
  
  return(preferences)
}

add_links <- function(population, products, metric = 2, n_links = 10){
  if(metric == 0){
    property <- degree(population)# degree
  }
  else if(metric == 1){
    property <- transitivity(population, type="local") # clustering coefficient
  }
  else if(metric == 2){
    property <- closeness(population, normalized = TRUE) # closeness centrality
  }
  else{
    return(population)
  }
  
  Na <- length(V(population))
  adj_list <- as_adj_list(population)
  
  # Discart the producers
  producers_ids <- which(V(population)$class == "P")
  property[producers_ids] <- -1
  
  
  new_links <- 0
  
  for(i in 1:Na){
    
    id_max <- which.max(property)
    
    if(!is.element(id_max, adj_list[producers_ids])){
      population <- add.edges(population, c(id_max,producers_ids[1]))
      
      new_links <- new_links + 1
      property[id_max] <- -1
      
      if(new_links >= n_links){break}
    }
  }
  
  return(population)
  
}

add_producers <- function(population, products, metric = 2, n_producers = 1){
  if(metric == 0){
    property <- degree(population)# degree
  }
  else if(metric == 1){
    property <- transitivity(population, type="local") # clustering coefficient
  }
  else if(metric == 2){
    property <- closeness(population, normalized = TRUE) # closeness centrality
  }
  else{
    return(population)
  }
  
  Na <- length(V(population))
  adj_list <- as_adj_list(population)
  
  # Discart the producers
  producers_ids <- which(V(population)$class == "P")
  property[producers_ids] <- -1
  
  
  new_producers <- 0
  
  for(i in 1:Na){
    
    id_max <- which.max(property)
    
    V(population)[id_max]$class == "P"
    V(population)[id_max]$product == products[1]
    
    new_producers <- new_producers + 1
    property[id_max] <- -1
    
    if(new_producers >= n_producers){break}
    
  }
  
  return(population)
  
}


# Function that plot the population graph according by products bought
plot_population <- function(population, products, layout_P){
  
  coul  <- brewer.pal(length(products), "Paired")
  my_color <- coul[as.numeric(as.factor(V(population)$product))]
  my_shape <-  ifelse(V(population)$class == "C", "circle", "square")

  plot(population, vertex.shape = my_shape, vertex.color=my_color, layout = layout_P)
  legend("bottomleft", legend=levels(as.factor(products))  , col = coul , bty = "n", pch=20 , pt.cex = 3, cex = 1.5, text.col=coul , horiz = FALSE, inset = c(-0.1, -0.1))
  
}