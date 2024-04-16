#Brouillon Devoir 4 #2

# Fonction pour calculer la somme des carrés, ignorera les NA
SumSquares <- function(x) {
  sum(x^2, na.rm=TRUE)
}

# Fonction log-densité a posteriori
lf <- function(theta, sig2, nu, tau2, X, n, N, k) {
  # Utiliser X, n, N, k comme défini précédemment
  Xbar <- rowMeans(X, na.rm=TRUE)
  tau2_inv <- 1 / (2 * tau2)
  sig2_inv <- 1 / (2 * sig2)
  
  # Calculer la log-densité a posteriori
  sum_squares <- SumSquares(X)
  part1 <- - (N + 8)/2 * log(sig2) - k/2 * log(tau2)
  part2 <- -1/sig2 - nu^2/2 - tau2_inv * sum((theta - nu)^2)
  part3 <- -sig2_inv * sum_squares
  part4 <- sig2_inv * sum(n * Xbar * theta)
  part5 <- -sig2_inv * sum(n * theta^2)
  return(part1 + part2 + part3 + part4 + part5)
}

# Initialisation des paramètres
theta <- rep(0, k) # Exemple d'initialisation
sig2 <- 1          # Exemple d'initialisation
nu <- 0            # Exemple d'initialisation
tau2 <- 1          # Exemple d'initialisation

# Paramètres de l'algorithme Metropolis-Hastings
m <- 10^6                      # Longueur de la chaîne désirée
I <- diag(13)                  # Matrice identité pour les 13 paramètres (theta, sig2, nu, tau2)
epsilon <- 0.1                 # Variance de la proposition (commencer avec 0.1)
acceptance_rate <- 0           # Taux d'acceptation des sauts
accepted <- 0                  # Compteur de sauts acceptés

# Boucle Metropolis-Hastings
for (i in 1:m) {
  # Proposition de nouveaux paramètres basés sur une distribution normale multivariée
  new_theta <- rnorm(k, mean=theta, sd=sqrt(epsilon))
  new_sig2 <- rnorm(1, mean=sig2, sd=sqrt(epsilon))
  new_nu <- rnorm(1, mean=nu, sd=sqrt(epsilon))
  new_tau2 <- rnorm(1, mean=tau2, sd=sqrt(epsilon))
  
  # Calcul de la log-densité a posteriori pour les nouveaux paramètres
  new_lf <- lf(new_theta, new_sig2, new_nu, new_tau2, X, n, N, k)
  
  # Calcul de la log-densité a posteriori pour les anciens paramètres
  old_lf <- lf(theta, sig2, nu, tau2, X, n, N, k)
  
  # Calcul du ratio de Hastings
  r <- exp(new_lf - old_lf)
  
  # Accepter ou rejeter les nouveaux paramètres basés sur le ratio de Hastings
  if (runif(1) < r) {
    theta <- new_theta
    sig2 <- new_sig2
    nu <- new_nu
    tau2 <- new_tau2
    accepted <- accepted + 1
  }
  
  # Mettre à jour le taux d'acceptation
  acceptance_rate <- accepted / i
}

# Résultats
cat("Taux d'acceptation final:", acceptance_rate, "\n")








#############################


# Fonction pour calculer la log-densité a posteriori donnée dans l'énoncé de la question
lf <- function(theta, sig2, nu, tau2) {
  SumSquares <- sum(X^2, na.rm=TRUE)  # Somme des carrés des observations
  n <- rowSums(!is.na(X))  # Nombre d'observations par strate
  N <- sum(n)  # Nombre total d'observations
  k <- nrow(X)  # Nombre de strates
  Xbar <- rowMeans(X, na.rm=TRUE)  # Moyennes par strate
  
  # Calcul de la log-densité a posteriori
  - (N + 8)/2 * log(sig2) - k/2 * log(tau2) - 1/sig2 - nu^2/2 - 
    tau2 - sum((theta - nu)^2)/(2*tau2) - SumSquares/(2*sig2) +
    sum(n*Xbar*theta)/sig2 - sum(n*theta^2)/(2*sig2)
}

# Le reste du code pour l'algorithme de Metropolis-Hastings reste principalement le même.
# Assurez-vous simplement de définir correctement X, n, N, k, et Xbar avant d'appeler lf dans la boucle.
# ...
