set.seed(2024)
X <- readRDS("Devoir3_data.RDS")


#a)

k <- dim(X)[1]  # nombre de lignes (strates)
ni <- colSums(!is.na(X))  # nombre d'observations non-NA par colonne (ceci est un vecteur)
N <- sum(ni)  # nombre total d'observations non-NA

lpost <- function(theta,log_sig2, nu, log_tau2){
  # Calcul de la moyenne pour chaque strate, en ignorant les NA
  
  sig2 <- exp(log_sig2) + 1e-10#c'est la seule tentative qui a donné une valeur
  tau2 <- exp(log_tau2) + 1e-10#meme que la ligne plus haut
  
  
  
  XiMoyen <- rowMeans(X, na.rm = TRUE)
  # Somme des carrés de Xij, en ignorant les NA
  Xij2 <- sum(X^2, na.rm = TRUE)
  
  # Les composants de la log-densité a posteriori
  logf1 <- -(N+8)/2 * log(sig2) - k/2 * log(tau2) - 1/sig2 - nu^2/2 - tau2
  logf2 <- - 1/(2*tau2) * sum((theta - nu)^2)
  logf3 <- - 1/(2 * sig2) * (Xij2 - 2*sum(XiMoyen*ni*theta, na.rm = TRUE) + sum(ni*theta^2, na.rm = TRUE))
  
  return(logf1+logf2+logf3)
}


lpost_optim <- function(param){
  theta <- param[1:k]
  sig2 <- param[k+1]
  nu <- param[k+2]
  tau2 <- param[k+3]
  return(-lpost(theta, sig2, nu, tau2))
}


init_param <- c(rep(0, k),log(1), 0, log(1))

optim_results <- optim(init_param, lpost_optim, method = "BFGS", control = list(fnscale = -1), hessian = TRUE)

optim_results$par


optim_results$value
