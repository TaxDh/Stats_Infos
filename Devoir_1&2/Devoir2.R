# Devoir2
# Author: René Picard
# Date: 2024-02-13

# le seed
set.seed(20240220)

# Question 1

# On génère la matrice de covariance Sigma
V = matrix(nrow = 100, ncol = 100, byrow = T) #car 100 fois 100

for(i in 1:100){
  for(j in 1:100){
    V[i,j] <- 5 * 0.8^(abs(i-j))
  }
}

# (a)

# On écrit une fonction ma_chol
ma_chol <- function(cov_mat) {
  n <- nrow(cov_mat) #pour connaitre la dimension de la matrice
  L <- matrix(0, n, n) #on initialise la matrice
  
  #ici on calcul les colonne en dernier et les lignes en premier
  for (k in 1:n) {
    for (j in 1:n) {
      #ici on calcul l'élément L_kk du théorème
      if(j == k){
        L[k,k] <- sqrt(cov_mat[k,k] - sum(L[k, 1:(k-1)]^2))
      }
      
      #calcul de l'élément L_jk du théorème
      if (j > k) {
        L[j,k] <- (1/L[k,k]) * (cov_mat[j,k] - sum(L[j, 1:(k-1)] * L[k, 1:(k-1)]))
      }
    }
  }
  return(L)
}

# (b)

# On utilise notre fonction ma_chol avec la matrice V créé plus juste après la question 1
L_chol <- ma_chol(V)

# (c)

# On prend le code de la fonction de Box-Muller fournit dans les notes de cours pour générer des valeurs qui suivent la loi normale N(0,1)
box_muller <- function(n){
  
  m <- 2 * ceiling(n/2) # le vrai nombre (pair) de réalisations uniformes nécessaire
  u <- runif(m)
  valeurs <- rep(0, m)
  for(i in 1:(m/2)){
    
    r <- sqrt(-2 * log(u[2 * (i-1) + 1]))
    t <- 2 * pi * u[2 * (i-1) + 2]
    
    valeurs[2 * (i-1) + 1] <- r * cos(t)
    valeurs[2 * (i-1) + 2] <- r * sin(t)
    
  }
  
  return(valeurs[1:n])
  
}

# Ensuite on utilise la fonction suivante pour généré des valeurs qui suivent une loi normale N(0,1), et on en génère 5000.
n <- 5000
d <- dim(L_chol)[1]
z <- matrix(nrow = n, ncol = d)
for(i in 1:n){
  z[i,] <- box_muller(d)  
}
x <- z %*% L_chol

# (d)

# Tout d'abord on estime theta par la méthode de Monte Carlo.
theta_hat <- mean(apply(x, 1, max))

# Maintenant on estime sigma
sigma_hat <- sd(exp(apply(x, 1, max)))

# Pour un alpha de 5%, on trouve z_alpha/2
z_alpha_2 <- qnorm(1 - 0.05/2)

# Et avec ceci on calcul l'intervalle de confiance pour notre theta estimé.
interval <- c(theta_hat - z_alpha_2 * sigma_hat / sqrt(n), theta_hat + z_alpha_2 * sigma_hat / sqrt(n))

#Et donc voici l'interval de confiance:
print(interval)

# Question 2

# (a)

# Initialisation de l'aiguille, lattes, etc.
aiguille <- 0.2
nlattes <- 6
n <- 0
n_succ <- 0

# On lance 100000 fois l'aiguille
for(i in 1:10^5){
  
  x <- nlattes * runif(2)
  theta <- pi * runif(1)
  
  # calcul des deux extrémités de l'aiguille avec la nouvelle longueur 
  tete <- x - c(cos(theta), sin(theta)) * aiguille / 2
  pointe <- x + c(cos(theta), sin(theta)) * aiguille / 2
  
  n <- n + 1
  
  # vérification si l'aiguille traverse une ligne
  if(floor(tete[2]) != floor(pointe[2])){
    n_succ <- n_succ + 1
  }
  
}

# Calcul de la proportion d'aiguille qui traversent une séparation entre deux lattes
proportion <- n_succ / n
print(proportion)

# (b)

# On créé 2 vecteurs, un l qui représentent les longueurs d'aiguilles et un estimateurs qui représentent les proportions qui traversent une séparation entre 2 lattes
l = c(0.2, 0.4, 0.6, 0.8, 1)
estimateurs <- numeric(length(l))

# On utilise le code plus haut et on le met dans une boucle qui va créé estimateurs avec les valeurs.
for(k in l){
  #initialisation
  aiguille <- l[k]
  nlattes <- 6
  n <- 0
  n_succ <- 0
  
  for(i in 1:10^5){
    
    x <- nlattes * runif(2)
    theta <- pi * runif(1)
    
    # calcul des deux extrémités de l'aiguille avec la nouvelle longueur 
    tete <- x - c(cos(theta), sin(theta)) * aiguille / 2
    pointe <- x + c(cos(theta), sin(theta)) * aiguille / 2
    
    n <- n + 1
    
    # Vérification si l'aiguille traverse une ligne
    if(floor(tete[2]) != floor(pointe[2])){
      n_succ <- n_succ + 1
    }
  }
  estimateurs[k] <- n_succ / n
}
