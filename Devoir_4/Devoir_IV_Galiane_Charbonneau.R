#-------------------------------------------------------------------------------
# Titre: Devoir IV - Statistique informatique
# Auteur: Galiane Charbonneau
# Date: 16 avril 2024
#-------------------------------------------------------------------------------
rm(list=ls())
set.seed(3010)
setwd("~/Devoir IV")

# ---- Données -----
X <- readRDS("Devoir3_data.RDS")

# ---- Question 1 ----
# Définition de quelques objets utiles pour la suite
k <- nrow(X)
n <- rowSums(!is.na(X))
N <- sum(n)
Xbar <- rowMeans(X, na.rm = TRUE)

# Définition de fonctions permettant de générer des observations des lois
# conditionnelles
simul_theta_i <- function(params, i){
  # Cette fonction permet de générer de la loi conditionnelle de theta_i sachant
  # les autres paramètres du modèle. 
  
  # param params: vecteur de taille 13 représentant l'état de la chaîne de 
  #               Markov à un moment donné
  # param i: l'indice du paramètre theta dont on veut générer
  
  # return: une observation de la loi de conditionnelle de theta_i sachant les
  #         autres paramètres du modèle.
  
  if ( !(i %in% 1:10) ) return(NA)
  
  # Identification des quantités intervenant dans l'espérance ou la variance
  # de la loi normale dont on veut générer
  sigma2 <- params[k+1]
  nu <- params[k+2]
  tau2 <- params[k+3]
  n_i <- n[i]
  X_i_barre <- Xbar[i]
  
  # Espérance de la loi normale
  moyenne <- (nu/tau2 + n_i*X_i_barre/sigma2)/
    (1/tau2 + n_i/sigma2)
  
  variance <- (1/tau2 + n_i/sigma2) ** -1
  
  return(rnorm(n = 1, mean = moyenne, sd = sqrt(variance)))
}

simul_sigma2 <- function(params){
  # Cette fonction permet de générer de la loi conditionnelle de sigma_2 sachant
  # les autres paramètres du modèle. 
  
  # param params: vecteur de taille 13 représentant l'état de la chaîne de 
  #               Markov à un moment donné
  
  # return: une observation de la loi de conditionnelle de sigma2 sachant les
  #         autres paramètres du modèle.
  
  # Vecteur theta
  theta <- params[1:k]
  
  # Paramètres de la loi inverse gamma dont on veut générer
  alpha <- (N+6)/2
  beta <- 1 + 0.5 * sum((X - theta)**2, na.rm = T)
  
  return(rgamma(n = 1, shape = alpha, rate = beta) ** -1)
  
}

simul_nu <- function(params){
  # Cette fonction permet de générer de la loi conditionnelle de nu sachant
  # les autres paramètres du modèle. 
  
  # param params: vecteur de taille 13 représentant l'état de la chaîne de 
  #               Markov à un moment donné
  
  # return: une observation de la loi de conditionnelle de nu sachant les
  #         autres paramètres du modèle.
  
  # Identification des quantités d'intérêt
  theta <- params[1:k]  
  tau2 <- params[k+3]
  
  # Paramètres de la loi normale dont on veut générer
  moyenne <- sum(theta)/(tau2 + k)
  variance <- (1 + k/tau2) ** -1
  
  return(rnorm(n = 1, mean = moyenne, sd = sqrt(variance)))
}

simul_tau2 <- function(params){
  # Cette fonction permet de générer de la loi conditionnelle de tau2 sachant
  # les autres paramètres du modèle. 
  
  # param params: vecteur de taille 13 représentant l'état de la chaîne de 
  #               Markov à un moment donné
  
  # return: une observation de la loi de conditionnelle de tau2 sachant les
  #         autres paramètres du modèle.
  
  # Identification des quantités d'intérêt
  theta <- params[1:k]
  nu <- params[k+2]
  
  # Le code suivant est emprunté au professeur. Il est basé sur la méthode du rejet
  # en utilisant une certaine loi gamma inverse comme loi instrumentale. Le ratio
  # f(y)/g(y) est proportionnel à exp(-y). La constante de proportionnalité correspond
  # au M optimal. Ainsi, la probabilité d'acceptation devient simplement exp(-y). 
  accept <- FALSE
  while(!accept){
    y <- 1/rgamma(1, shape = k/2-1, rate = sum((theta-nu)^2)/2)
    u <- runif(1)
    accept <- (log(u) <= -y)
  }
  
  return(y)
}

nouv_obs <- function(params){
  # Cette fonction simule la (j+1)-ième observation de la chaîne de Markov
  # lorsque la j-ième est donnée par l'argument params
  
  # param params: état de la chaîne de Markov à un temps donné
  
  # return: l'état de la chaîne au temps suivant
  
  # Randomisons l'ordre des simulations
  ordre <- sample(1:(k+3))
  
  for (l in 1:(k+3)){
    i <- ordre[l]
    
    # Nouvelle réalisation du i-ième paramètre
    real <- dplyr::case_when(
      i == (k + 1)  ~ simul_sigma2(params),
      i == (k + 2)  ~ simul_nu(params),
      i == (k + 3)  ~ simul_tau2(params),
      TRUE ~ simul_theta_i(params, i)
    )
    params[i] <- real
  }
  
  return(params)
}

# Génération des 10^6 premières observations d'une chaîne de Markov dont la loi 
# stationnaire est la loi a posteriori du modèle d'intérêt à l'aide de l'algorithme
# de Gibbs.

# Longueur de la chaîne
m <- 1e6

# Initialisation de l'algorithme
chaineMarkov <- matrix(NA, nrow = k + 3, ncol = m + 1) # La i-ème colonne de cette matrice représente l'état de la chaîne au temps i
chaineMarkov[, 1] <- c(rnorm(k), rexp(1L, 1L), rnorm(1L), rexp(1L, 1L))

for (j in 1:m){
  chaineMarkov[, j+1] <- nouv_obs(chaineMarkov[, j])
}
#save(chaineMarkov, file = "gibbs.RData")
load("gibbs.RData")

# Trace de la chaîne (graphiques univariés)
for (i in 1:13){
  plot(chaineMarkov[i, ], pch = 20, type = "l")
}

# Estimons l'espérance de la question 1 d) du devoir III afin de vérifier la  
# réponse obtenue précédemment. Les graphiques produits aux lignes 164-166
# suggèrent que la convergence de la distribution des observations vers la 
# loi stationnaire est très rapide. Ignorons simplement les 100 premières 
# observations.
ech <- apply(chaineMarkov[, -(1:100)], 2, function(col) exp(sum(col[1:k]))/(1 + exp(sum(col[1:k]))))
mean(ech) # Très semblable à l'estimation obtenue dans le devoir III

# ---- Question 2 ----
SumSquares <- sum(X^2, na.rm=TRUE)
lf <- function(theta, sig2, nu, tau2){
  if ( (sig2 <= 0) | (tau2 <= 0) ) return(-Inf)
  
  - (N + 8)/2 * log(sig2) - k/2 * log(tau2) - 1/sig2 - nu^2/2 -
    tau2 - sum((theta - nu)^2)/(2*tau2) - SumSquares/(2*sig2) +
    sum(n*Xbar*theta)/sig2 - sum(n*theta^2)/(2*sig2)
}

# Définition du noyau
g <- function(params, l2){
  # Cette fonction génère un candidat pour l'observation au temps t lorsque 
  # la chaîne est à l'état params au temps t-1.
  rnorm(n = length(params), mean = params, sd = sqrt(l2))
}

nouv_obs_MH <- function(params, l2){
  # Cette fonction simule la (j+1)-ième observation de la chaîne de Markov
  # lorsque la j-ième est donnée par l'argument params
  
  # param params: état de la chaîne de Markov à un temps donné
  # param l2: la matrice de covariance de la loi cible est donnée par l2 * Id
  #           où Id est la matrice identité 13x13.
  
  # return: l'état de la chaîne au temps suivant
  
  # Simulation d'un candidat
  theta_prime <- g(params, l2)
  
  U <- runif(1)
  ind <- log(U) <= lf(theta_prime[1:k], theta_prime[k+1], theta_prime[k+2], theta_prime[k+3]) - 
    lf(params[1:k], params[k+1], params[k+2], params[k+3])
  
  if (ind) return(c(theta_prime, 1))
  
  return(c(params, 0))
}

# Estimons le taux d'acceptation
taux_acc_fn <- function(params.init, l2, n = 5000){
  # Cette fonction estime le taux d'acceptation des sauts pour une valeur donnée
  # de l2. L'estimation est basée sur une chaîne de longueur 5000 (par défaut) 
  # et les 1000 première observations de la chaîne sont ignorées.
  
  result <- matrix(NA, ncol = n+1, nrow = length(params.init) + 1)
  result[, 1] <- c(params.init, NA)
  
  for (i in 1:n){
    result[, i+1] <- nouv_obs_MH(result[-(k+4), i], l2 = l2)
  }
  
  #tx <- 1 - mean(sapply(1001:(n+1), function(j) all.equal(result[, j-1], result[, j]) == T))
  return(mean(result[k+4, -1]))
}

# Estimons les taux d'acceptation des sauts pour 1000 valeurs de l2 comprises
# entre 0.0001 et 0.1
taux_acc <- data.frame("l2" = seq(0.1, 0.00001, length = 500),
                       "taux" = NA)
params.init <- c(rnorm(k), rexp(1L, 1L), rnorm(1L), rexp(1L, 1L))
taux_acc[["taux"]] <- sapply(taux_acc[["l2"]], function(l2) taux_acc_fn(params.init, l2))
#save(taux_acc, file = "taux_acc.RData")
load("taux_acc.RData")

# Identifications des valeurs de l2 ayant donné lieu à des taux compris entre
# 20% et 25%
dplyr::filter(taux_acc, (taux >= 0.2) & (taux <= 0.25))

# Choix de l2
l2 <- 0.000004

# Simulation de la chaîne
m <- 1e6
chaineMarkov_MH <- matrix(NA, ncol = m+1, nrow = k + 4)
chaineMarkov_MH[, 1] <- c(rnorm(k), rexp(1L, 1L), rnorm(1L), rexp(1L, 1L), NA)

for (i in 1:m){
  chaineMarkov_MH[, i+1] <- nouv_obs_MH(chaineMarkov_MH[-(k+4), i], l2 = l2) 
}
#save(chaineMarkov_MH, file = "MH0.00062.0.RData")
load("MH0.0006.RData")

# Trace de la chaîne (graphiques univariés)
for (i in 1:13){
  plot(chaineMarkov_MH[i, ], pch = 20, type = "l")
}

# Taux d'acceptation des sauts de la chaîne simulée
mean(chaineMarkov_MH[k+4, ], na.rm = T)

# Estimation de l'espérance du devoir III. Comme avec l'algorithme de Gibbs,
# la distribution des observations semble avoir convergé très rapidement. Ignorons
# encore une fois seulement 100 observations.
echMH <- apply(chaineMarkov_MH[, -(1:100)], 2, function(col) exp(sum(col[1:k]))/(1 + exp(sum(col[1:k]))))
mean(echMH) # Très semblable à l'estimation obtenue dans le devoir III (et à celle obtenue précédemment par Gibbs)