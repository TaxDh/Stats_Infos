#-------------------------------------------------------------------------------
# Devoir II - Statistique informatique
# Auteur: Galiane Charbonneau
#-------------------------------------------------------------------------------
rm(list=ls())
set.seed(2024)

# ---- Question 1 ----
# a)
ma_chol <- function(sigma){
  # ----------
  # Cette fonction retourne la matrice triangulaire inférieure L apparaissant
  # dans la décomposition de Cholesky de la matrice sigma
  
  # param sigma: matrice symétrique définie positive
  
  # return: matrice triangulaire inférieure L telle que LL^t = sigma
  # ----------
  
  # Taille de la matrice de covariance
  d <- nrow(sigma)
  
  # Matrice retournée par la fonction
  L <- matrix(0, nrow = d, ncol = d)
  L[1,1] <- sqrt(sigma[1,1])
  
  for (j in 2:d){       # boucle sur les lignes de L
    for (k in 1:j){     # boucle sur les colonnes de L
      if (j > k){
        L[j,k] <- 1/L[k,k] * (sigma[j,k] - (k>1)*sum(L[j, 1:(k-1)]*L[k, 1:(k-1)])) 
      } else {
        L[j,k] <- sqrt(sigma[k,k] - sum(L[k, 1:(k-1)]**2))
      }
    }
  }
  
  return(L)
}

# b)
exposants <- abs(1:100 %*% t(rep(1, 100)) - rep(1, 100) %*% t(1:100))
Sig <- 5 * .8^exposants
L <- ma_chol(Sig)

# Vérification qu'il s'agit bien d'une racine carrée de Sig
all.equal(L%*%t(L), Sig)
any(diag(L) < 0)

# c)
# Simulation 
n <- 5000L 
d <- nrow(L)
ech <- L%*%matrix(rnorm(d*n), nrow = d, ncol = n)

# d)
theta.hat <- mean(apply(ech, 2, function(X) exp(max(X))))
sd.hat <- sd(apply(ech, 2, function(X) exp(max(X))))
L.bound <- theta.hat-qnorm(0.975)*sd.hat/sqrt(n)
U.bound <- theta.hat+qnorm(0.975)*sd.hat/sqrt(n)

cat("Intervalle de confiance à 95% pour l'espérance d'intérêt:", 
    paste0("[", format(round(L.bound, 2L), nsmall = 2L), " : ", 
           format(round(U.bound, 2L), nsmall = 2L), "]"), 
    sep = "\n")

# ---- Question 2 ----
buffon.fn <- function(n, l){
  # ----------
  # Cette fonction implémente l'expérience de l'aiguille de Buffon afin 
  # d'estimer la probabilité qu'une aiguille de longueur l traverse une 
  # séparation. Elle reprend essentiellement le code du programme intitulé
  # «Code R pour l'exemple de l'aiguille de Buffon» écrit par le professeur.
  
  # param n: nombre de simulations (nombre de lancers d'une aiguille sur le 
  #          plancher)
  # param l: longueur de l'aiguille
  
  # return: une estimation de la probabilité qu'une aiguille de longueur l
  #         traverse une séparation.
  # ----------
  n_succ <- 0
  nlattes <- 6 # le nombre de lattes est arbitraire
  
  for(i in 1:n){
    
    # génération de la position du centre de l'aiguille et de son angle
    x <- nlattes*runif(2)
    theta <- pi*runif(1)
    
    # calcul des deux extrémités de l'aiguille
    tete <- x - l*c(cos(theta), sin(theta)) / 2
    pointe <- x + l*c(cos(theta), sin(theta)) / 2
    
    if( floor(tete[2]) != floor(pointe[2]) ) n_succ <- n_succ + 1
    
  }
  
  return(n_succ/n)
}

# a)
l <- 0.2
n <- 1e5L
p_0.2 <- buffon.fn(n, l)
cat(paste0(paste0(format(round(100*p_0.2, 2L), nsmall = 2), "%"),
       " des aiguilles de longueur 0.2 ont traversé une séparation"))

# b)
estimateurs <- p_0.2
for (l in seq(0.4, 1, by = 0.2)){
  estimateurs <- c(estimateurs, buffon.fn(n, l))
}

# c)
par(mfrow = c(1,1), mar = c(5,5.5,3,3))
plot(seq(0.2,1,by=0.2), estimateurs, xlim=c(0, 1), ylim=c(0, 1),
     pch = 20, cex = 1.3, xlab = "longueur de l'aiguille",
     ylab = "proportion des aiguilles qui traversent\nune séparation",
     main = paste("Proportion des aiguilles qui traversent une séparation",
                   "en fonction de la longueur de l'aiguille",
                  sep = "\n"),
     cex.main = 0.9)

# d)
abline(a = 0, b = 2/pi, col = "red", lwd = 1.5)

# On peut constater que la droite d'équation y = 2x/pi s'ajuste presque 
# parfaitement aux proportions observées. Il est donc raisonnable de penser
# que l'on pourrait démontrer que la probabilité qu'une aiguille de longueur 
# l \in (0, 1] traverse une séparation est égale à 2l/pi.