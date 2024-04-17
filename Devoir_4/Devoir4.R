set.seed(2024)
X <- readRDS("Devoir3_data.RDS")


k <- nrow(X)
n <- rowSums(!is.na(X))
N <- sum(n)
Xbar <- rowMeans(X, na.rm = T)

# Question 1
#Algorithme de Gibbs

# Étape 1
# On initialise les hyperparamètres
nu <- 0
tau2 <- 1
alpha <- 3
beta <- 1

theta <- Xbar  # On initialise theta pour la moyenne de chaque strate
sigma2 <- var(X, na.rm = TRUE)  # Initialise sigma2 et la variance de X


# Étape 2
m <- 1e6 # Nombre d'itération
#m <- 1000 # Pour les tests


# Étape 3
for(t in 1:m){
  
  # On initialise et met à jour tau2 (l'indice)
  accept <- FALSE
  while(!accept) {
    y <- 1/rgamma(1, shape=k/2 - 1, rate=sum((theta - nu)^2)/2)
    u <- runif(1)
    accept <- (log(u) <= -y)
  }
  tau2 <- y
  
  # On met à jour nu
  nu_mean <- sum(theta) / k
  nu_var <- tau2 / k
  nu <- rnorm(1, mean=nu_mean, sd=sqrt(nu_var))
  
  
  # On met à jour sig2
  alpha_post <- (N + 6) / 2
  beta_post <- beta + 0.5 * sum(sapply(1:k, function(i) sum((X[i, ] - theta[i])^2, na.rm = TRUE)))
  sigma2 <- 1 / rgamma(1, shape=alpha_post, rate=beta_post)
  
  
  # Pour chaque i, on calcul theta[i]
  for(i in 1:k){
    ni_i <- n[i]  # Nombre d'observations dans la ième strate
    Xbar_i <- Xbar[i]  # Moyenne des observations dans la ième strate
    tau2_inv <- 1 / tau2
    sigma2_inv <- 1 / sigma2
    theta_mean <- (nu * tau2_inv + ni_i * Xbar_i * sigma2_inv) / (tau2_inv + ni_i * sigma2_inv)
    theta_var <- 1 / (tau2_inv + ni_i * sigma2_inv)
    theta[i] <- rnorm(1, mean=theta_mean, sd=sqrt(theta_var))
  }
  print(t)#Pour s'assurer que la boucle n'est pas infinie
}

# Notre résultat
theta


# Question 2

# Étape 1

# On recopie le code fournit pour la log de f à postériori
SumSquares <- sum(X^2, na.rm=TRUE)
lf <- function(theta, sig2, nu, tau2){
  - (N + 8)/2 * log(sig2) - k/2 * log(tau2) - 1/sig2 - nu^2/2 -
    tau2 - sum((theta - nu)^2)/(2*tau2) - SumSquares/(2*sig2) +
    sum(n*Xbar*theta)/sig2 - sum(n*theta^2)/(2*sig2)
}


# Initialisation des paramètres
theta <- rep(0, k)
sig2 <- 1
nu <- 0
tau2 <- 1

m <- 1e3 # Longueur de la chaine

# initialisation de l'échantillon et point de départ
ech <- matrix(0, nrow=m+1, ncol=k+3)
ech[1,] <- c(theta, sig2, nu, tau2, NA)

# Variance instrumentale pour la proposition
l2 <- 0.1
l <- sqrt(l2)

#Je n'ai pas terminé le numéro