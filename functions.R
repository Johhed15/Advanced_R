

# Assignment 1 
# Euc

euclidean <- function(x,y){
  # checking the input 
  stopifnot(is.integer(x) || is.numeric(x) && length(x) == 1 ,is.integer(y) || is.numeric(y) && length(y) == 1)
  
  while (y != 0) { # While y differ from 0
    t  <- y
    y <- x %% y  # X modulus y
    x <- t
  }
  return(abs(x)) # returning x which now is the GCD
}

euclidean(100, 1000)

euclidean(23141234, 13892347912)




# assignment 2
# This function have gotten 
# Code inspiration from https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm 



dijkstra <- function(graph, init_node){
  # checking the input
  stopifnot(is.data.frame(graph) && colnames(graph) %in% c('v1','v2','w'),
            is.numeric(init_node) && length(init_node) == 1,
            init_node %in% graph$v1 ||init_node %in% graph$v2 )
  
  nodes <- unique(graph$v1) # taking out each node
  dist <- rep(Inf,  length(nodes)) # Creating the dist and prev vectors
  prev <- rep(NA,  length(nodes))
  
  dist[init_node] <- 0 # The distance to itself is 0
  
  Q <- nodes # Creating variable Q
  
  while (length(Q) > 0) { # While the Q variable is longer than 0
    u <- Q[which.min(dist)] # Takes the node with the shortest distance
    Q <- setdiff(Q,u) # Remove that node from Q
    
      # Indice all elements 
   neighbors <- c(graph$v2[graph$v1 == u])
   
   for (v in neighbors){ # looping over all neighbors
       # Distance u + distance for all neighbors of u and v
      alt <-  dist[u] + graph$w[which((graph$v1 == u & graph$v2 == v) )]
      if (alt < dist[v]){ # updating the distance if its shorter than to the previous neighbor
        dist[v] <- alt
        prev[v] <- u
      }
     }
    
   }
  return(dist)   
}
  

wiki_graph <-
  data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
             v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
             w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))

dijkstra(wiki_graph, 1)


dijkstra(wiki_graph, 3)



