set.seed(2024)
X <- readRDS("Devoir3_data.RDS")

k <- 10 #nombre de lignes
ni <- 110 #nombre de colonnes


N <- k*ni
lpost <- function(theta, sig2, nu, tau2){
  # Assurez-vous que 'X' est défini dans l'environnement global.
  Xi_bar <- rowMeans(X, na.rm = TRUE)
  sum_Xij2 <- sum(X^2, na.rm = TRUE)
  
  # Calcul de la log densité a posteriori sans la constante K
  log_f <- (-N/2) * log(sig2) - (k/2) * log(tau2) - (1/(2*sig2)) * sum_Xij2 +
    sum((ni * Xi_bar^2)/(2*sig2), na.rm = TRUE) - sum((ni * theta^2)/(2*sig2), na.rm = TRUE) +
    sum((2 * Xi_bar * theta)/sig2, na.rm = TRUE) - sum((theta - nu)^2)/(2*tau2) -
    (nu^2)/(2*tau2) - (1/2)*log(sig2) - (1/2)*log(tau2) - (1/(2*sig2)) - (nu^2)/(2*tau2) - (1/tau2)
  
  return(log_f)
}

lpost_optim <- function(param) {
  theta <- param[1:k]
  sig2 <- param[k+1]
  nu <- param[k+2]
  tau2 <- param[k+3]
  return(-lpost(theta, sig2, nu, tau2)) 
}



initial_values <- c(rep(0, k), 1, 0, 1) # Replace with your actual initial values
optim_results <- optim(par = initial_values, fn = lpost_optim)



init_param <- c(rep(0, k), 1, 0, 1) 


lower_bounds <- c(rep(-Inf, k), 1e-6, -Inf, 1e-6)
upper_bounds <- rep(Inf, k+3)
opt <- optim(init_param, lpost_optim, method = "L-BFGS-B")


print(lpost(init_param[1:k], init_param[k+1], init_param[k+2], init_param[k+3]))

