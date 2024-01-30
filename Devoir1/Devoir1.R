#fonction ti_discrete page 5/44 de 02-GenerationLoisConnus
ti_discrete <- function(n, x, p){
  
  if(length(x) != length(p)) stop("Dimensions de x et p incompatibles")
  
  valeurs <- rep(0, n)
  for(i in 1:n){
    
    u <- runif(1)
    l <- 1
    while(u > sum(p[1:l])) l <- l+1
    valeurs[i] <- x[l]
    
  }
  
  return(valeurs)
  
}

ti_generale <- function(n, F_repartition_inv){
  
  u <- runif(n)
  valeurs <- sapply(u, F_repartition_inv)
  return(valeurs)
  
}