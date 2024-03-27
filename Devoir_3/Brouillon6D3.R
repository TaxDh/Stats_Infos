set.seed(2024)
X <- readRDS("Devoir3_data.RDS")



#a)

lpost <- function(theta, sig2, nu, tau2) {
  k <- 10 
  N <- sum(!is.na(X))
  
  X_bar <- rowMeans(X, na.rm = TRUE)
  ni <- rowSums(!is.na(X))
  
  # Calcul de la log-densité a posteriori
  logf <- - (N + 8) / 2 * log(sig2) - k / 2 * log(tau2) - 1/sig2 - nu^2/2 - tau2 -
    1/(2*tau2) * sum((theta - nu)^2) -
    1/(2*sig2) * sum(sapply(1:k, function(i) {
      sum(X[i, ]^2, na.rm = TRUE) - 2 * ni[i] * X_bar[i] * theta[i] + ni[i] * theta[i]^2
    }))
  return(logf)
}

#b)
lpost_optim <- function(param) {
  theta <- param[1:10]
  sig2 <- param[11]
  nu <- param[12]
  tau2 <- param[13]
  lpost(theta, sig2, nu, tau2)
}

# Paramètres initiaux
init_param <- c(rep(0, 10), 1, 0, 1)  # theta, sig2, nu, tau2

# Optimisation
opt <- optim(par = init_param, fn = lpost_optim, method = "BFGS",control=list(fnscale=-1), hessian = TRUE)

# Afficher les résultats
opt$value

#c)

hess <- opt$hessian

#on trouve la matrice de covariance

cov <- solve(hess)#H^{-1}

theta_MAP <- opt$par

#on trouve les écarts-types
std <- sqrt(diag(cov))

#on estime chaque theta_i avec l'approximation de laplace (proposition page 10)
#ensuite on prend la moyenne
#on construit l'interval de crédibilité comme un interval de confiance autour de ce theta (centré)

det_MoinsH <- det(-cov)

exponentielle <- -t(theta- theta_MAP)%.%(-cov)%.%(theta-theta_MAP)

theta_i <- 1/((2*Pi)^(d/2)*det_MoinsH) * exponentielle

#on fait maintenant la moyenne des theta_i

#....................#

#on fait maintenant l'interval de crédibilité

#......................#

#d)