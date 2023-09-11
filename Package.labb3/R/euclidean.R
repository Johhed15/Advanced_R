euclidean <-
function(x,y){
  # checking the input 
  stopifnot(is.integer(x) || is.numeric(x) && length(x) == 1 ,is.integer(y) || is.numeric(y) && length(y) == 1)
  
  while (y != 0) { # While y differ from 0
    t  <- y
    y <- x %% y  # X modulus y
    x <- t
  }
  return(abs(x)) # returning x which now is the GCD
}
