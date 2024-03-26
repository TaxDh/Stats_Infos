#a) apparement k = 10.

#c) theta_n+1 chapeau= signaux_theta log f(...) où (...) est la matrice hessienne
#d) exp(a)/exp(b) = exp(a-b)
set.seed(2024)
X <- readRDS("Devoir3_data.RDS")
View(X)
#write.csv(X, "D:/Documents/Trimestre Actuel/STT3010 - Statistique Informatique/Stats_Infos/Devoir_3/X.csv")
dim(X)
# Sortie: 10 110

#a)

k <- 10 #nombre de lignes
ni <- 110 #nombre de colonnes

lpost <- function(theta, sig2, nu, tau2){
  N <- k*ni
  XiMoyen <- rowMeans(X, na.rm = T)
  Xij2 <- sum(X^2, na.rm = T)
  
  #print(paste("log(sig2):", log(sig2))) # Check the log value of sig2
  print(paste("log(tau2):", log(tau2)))
  #print(paste("sum_Xij2:", Xij2))
  
  logf1 <- (N+8)/2 * log(sig2) - k/2 * log(tau2) - 1/sig2 - nu^2/2 - tau2
  logf2 <- - 1/(2*tau2) * sum((theta - nu)^2, na.rm = T)
  logf3 <- - 1/(2 * sig2) * (Xij2 - 2*sum(ni*XiMoyen*theta, na.rm = T) + sum(ni*theta^2, na.rm = T))
  
  return(logf1+logf2+logf3)
}


#y <- matrix(c(10,2,3,4,5,0), nrow = 2, ncol = 3, byrow = T)
#y
#sum(y)


#b)

lpost_optim <- function(param){
  theta <- param[1:k]
  sig2 <- param[k+1]
  nu <- param[k+2]
  tau2 <- param[k+3]
  return(-lpost(theta, sig2, nu, tau2))
}




lower_bounds <- c(rep(-Inf, k), log(.Machine$double.eps), -Inf, log(.Machine$double.eps))
upper_bounds <- c(rep(Inf, k), Inf, Inf, Inf)

optim_results <- optim(init_param, lpost_optim, method = "L-BFGS-B", lower = lower_bounds, upper = upper_bounds)


init_param <- c(rep(0, k), 1, 0, 1)

optim_results <- optim(init_param, lpost_optim, method = "L-BFGS-B")

print(optim_results$par)


print(optim_results$value) 











