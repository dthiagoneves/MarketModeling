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
producers_list <- function(population, products){
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



# Function that plot the population graph according by products bought
plot_population <- function(population, products, layout_P){
  
  coul  <- brewer.pal(length(products), "Set1")
  my_color <- coul[as.numeric(as.factor(V(population)$product))]
  my_shape <-  ifelse(V(population)$class == "C", "circle", "square")

  plot(population, vertex.shape = my_shape, vertex.color=my_color, layout = layout_P)
  legend("bottomleft", legend=levels(as.factor(products))  , col = coul , bty = "n", pch=20 , pt.cex = 3, cex = 1.5, text.col=coul , horiz = FALSE, inset = c(-0.1, -0.1))
  
}