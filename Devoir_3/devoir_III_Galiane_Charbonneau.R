#-------------------------------------------------------------------------------
# Titre: Devoir III - Statistique informatique
# Auteur: Galiane Charbonneau
# Date: 27 mars 2024
#-------------------------------------------------------------------------------
rm(list=ls())
set.seed(3010)
setwd("~/H24/STT3010/Devoirs/Devoir III")

# ---- Données ----
X <- readRDS("Devoir3_data.RDS")

# ---- Question a) ----
# Calcul des quantités intervenant dans la définition de la fonction log(f)
N <- sum(!is.na(X))
k <- nrow(X)
n_i <- apply(X, 1L, function(row) sum(!is.na(row)))
X_i.barre <- rowMeans(X, na.rm = T)

# Définition de la log-densité a posteriori
lpost <- function(theta, sig2, nu, tau2){
  -(N+8)/2 * log(sig2) - k/2 * log(tau2) - 1/sig2 - nu**2/2 - tau2 - 
    1/(2*tau2) * sum((theta - nu)**2) - 
    1/(2*sig2) * ( sum(X**2, na.rm = T) - 2 * crossprod(X_i.barre * n_i, theta) + crossprod(n_i, theta**2) )
}

# ---- Question b) ----
# Version vectorielle de lpost 
lpost_optim <- function(param){
  if ( (param[k+1] <= 0) | (param[k+3] <= 0) ) return(NA)
  lpost(param[1:k],
        param[k+1],
        param[k+2],
        param[k+3])
}

# Optimisation de cette fonction
opt <- optim(par = c(rnorm(k), rexp(1L, 1L), rnorm(1L), rexp(1L, 1L)), 
             fn = lpost_optim, 
             method = "BFGS", 
             control = list(fnscale = -1L), 
             hessian = TRUE)

# Identification du mode a posteriori (MAP)
(MAP <- opt$par)
H <- opt[["hessian"]]

# Matrice de covariances associée
mat.cov <- solve(-H)

# ---- Question c) ----
# Le théorème d'approximation de Laplace stipule que la loi a posteriori est
# approximativement normale (multivariée) centrée en MAP et de matrice de 
# covariances -H^-1. Une des propriété propre à la loi normale est que toute 
# combinaison linéaire des entrées suit une loi normale (univariée). En 
# particulier, chaque entrée du vecteur suit une loi normale. Ainsi, par ce 
# théorème, chaque variable aléatoire theta_i est (approximativement) normalement 
# distribuée. Identifions les paramètres de ces k lois: 

# Espérances
esperances <- MAP[1:k]

# Variances
variances <- diag(mat.cov)[1:k]

# Bien sûr, il existe une infinité d'intervalles de crédibilité de niveau 95%
# pour ces k variables aléatoires. Choisissons (arbitrairement) de les centrer 
# en l'espérance de chaque variable.
IC <- cbind(
  esperances - qnorm(0.975) * sqrt(variances), 
  esperances + qnorm(0.975) * sqrt(variances)
  )
colnames(IC) <- c("borne_inf", "born_sup")
rownames(IC) <- paste0("theta_", 1:10)
print(IC)

# ---- Question d) ----
# Définition de la fonction param |--> log(h(param)) + lpost(param)
loghf <- function(param) {
  sum(param[1:k]) - log(1 + exp(sum(param[1:k]))) + lpost_optim(param)
}

# Optimisation de cette fonction
opth <- optim(par = c(rnorm(k), rexp(1L, 1L), rnorm(1L), rexp(1L, 1L)), 
              fn = loghf, 
              method = "BFGS", 
              control = list(fnscale = -1L), 
              hessian = TRUE)

V <- opth[["hessian"]]

# Estimation de l'espérance d'intérêt
(h.hat <- sqrt(det(H)/det(V)) * exp(opth$value - opt$value))

# Par curiosité, estimons également cette espérance par Monte Carlo en utilisant
# l'approximation de Laplace standard
n.simul <- 1e4L
L <- t(chol(mat.cov))
Z <- apply(L%*%matrix(rnorm(13 * n.simul), nrow = 13L, ncol = n.simul), 
           2L, 
           function(col) col + MAP)
mean(apply(Z, 2L, function(col) exp(sum(col[1:k])) / (1 + exp(sum(col[1:k]))) ))