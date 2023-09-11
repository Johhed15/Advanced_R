dijkstra <-
function(graph, init_node){
  # checking the input
  stopifnot(is.data.frame(graph), colnames(graph) %in% c('v1','v2','w'),
            is.numeric(init_node) && length(init_node) == 1,
            init_node %in% graph$v1 ||init_node %in% graph$v2 )
  
  nodes <- unique(graph$v1) # taking out each node
  dist <- rep(Inf,  length(nodes)) # Creating the dist and prev vectors
  prev <- rep(NA,  length(nodes))
  
  dist[init_node] <- 0 # The distance to itself is 0
  
  Q <- nodes # Creating variable Q
  
  while(length(Q) > 0) { # While the Q variable is longer than 0
    u <- Q[which.min(dist[Q])] # Takes the node with the shortest distance
  
    Q <- setdiff(Q,u) # Remove that node from Q
    
      # Indice all elements in v1 for the rows in v1 that are the value of u
   neighbors <- c(graph$v2[graph$v1 == u])
   
   for (v in neighbors){ # looping over all neighbors
       # Distance u + distance for all neighbors of u and v
      alt <-  dist[u] + graph$w[which((graph$v1 == u & graph$v2 == v ))]
      if (alt < dist[v]){ # updating the distance if its shorter than to the previous neighbor
        dist[v] <- alt
        prev[v] <- u
      }
     }
    
   }
  return(dist)   
}
