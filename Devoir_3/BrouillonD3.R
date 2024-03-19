#a) apparement k = 10.

#c) theta_n+1 chapeau= signaux_theta log f(...) où (...) est la matrice hessienne
#d) exp(a)/exp(b) = exp(a-b)
X <- readRDS("Devoir3_data.RDS")
View(X)

dim(X)
# Sortie: 10 110

#a)

lpost <- function(theta, sig2, nu, tau2){
  #k <- length(theta)
  k <- 10 #nombre de lignes
  ni <- 110 #nombre de colonnes
  N <- sum(ni, na.rm = TRUE)
  XiMoyen <- rowMeans(X, na.rm = T)
  Xij2 <- sum(X^2, na.rm = T)
  
  logf1 <- (N+8)/2 * log(sig2) - k/2 * log(tau2) - 1/sig2 - nu^2/2 - tau2
  logf2 <- - 1/(2*tau2) * sum((theta - nu)^2, na.rm = T)
  logf3 <- - 1/(2 * sig2) * (Xij2 - sum(2*ni*XiMoyen*theta, na.rm = T) + sum(ni*theta^2, na.rm = T))
  
  return(logf1+logf2+logf3)
}


#y <- matrix(c(10,2,3,4,5,0), nrow = 2, ncol = 3, byrow = T)
#y
#sum(y)


#b)

