set.seed(2024)
X <- readRDS("Devoir3_data.RDS")
#View(X)
dim(X)
# Sortie: 10 110

#a)

k <- 10 #nombre de lignes
ni <- 110 #nombre de colonnes
N <- k*ni
lpost <- function(theta, sig2, nu, tau2){
  XiMoyen <- rowMeans(X, na.rm = T)
  Xij2 <- sum(X^2, na.rm = T)
  
  #print(paste("log(sig2):", log(sig2))) # Check the log value of sig2
  #print(paste("log(tau2):", log(tau2)))
  #print(paste("sum_Xij2:", Xij2))
  
  logf1 <- -(N+8)/2 * log(sig2) - k/2 * log(tau2) - 1/sig2 - nu^2/2 - tau2
  logf2 <- - 1/(2*tau2) * sum((theta - nu)^2, na.rm = T)
  logf3 <- - 1/(2 * sig2)* (sum(X^2, na.rm = TRUE) - 2*sum(ni*XiMoyen*theta, na.rm = TRUE) + sum(ni*theta^2, na.rm = TRUE))
  
  return(logf1+logf2+logf3)
}


#b)

lpost_optim <- function(param){
  theta <- param[1:k]
  sig2 <- param[k+1]
  nu <- param[k+2]
  tau2 <- param[k+3]
  return(-lpost(theta, sig2, nu, tau2))
}


init_param <- c(rep(0, k), 1, 0, 1)

optim_results <- optim(init_param, lpost_optim, method = "BFGS", control = list(fnscale = -1), hessian = TRUE)

#Bloque ici


print(optim_results$par)


print(optim_results$value) 



#c)









