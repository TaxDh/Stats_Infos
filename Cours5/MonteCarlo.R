# exemple: estimation Monte Carlo d'une intégrale sur (0, 1)
th <- sqrt(pi)/2
n <- 100
ech <- runif(n) # création des xi selon la loi uniforme
th_hat <- mean(sqrt(-log(ech))) #la formule dans l'ex page 5
th_hat

# vérification de la nortmalité asymptotique
z <- replicate(1000, {
  
  ech <- runif(n)
  th_hat <- mean(sqrt(-log(ech)))
  sig_hat <- sd(sqrt(-log(ech)))
  return(sqrt(n) * (th_hat - th)/sig_hat)
  
})
plot(density(z))

# vérification de la couverture de l'intervalle de confiance
alpha <- .1
couv <- replicate(1000, {
  
  ech <- runif(n)
  th_hat <- mean(sqrt(-log(ech)))
  sig_hat <- sd(sqrt(-log(ech)))
  return(
    (th_hat - qnorm(alpha/2, lower.tail=FALSE) * sig_hat / sqrt(n) <= th) && (th_hat + qnorm(alpha/2, lower.tail=FALSE) * sig_hat / sqrt(n) >= th)
  )
  
})
# couv
mean(couv)



# exemple: estimation Mont Carlo d'une somme
# estimation numérique de theta
test <- sapply(1:100, function(n) sum( abs(cos(0:n))/factorial(0:n) ))
plot(test) # on voit que la somme partielle est bien stabilisée après avoir sommé les dix premiers termes
plot(test[2:100] - test[1:99], log="y")
th <- test[100]

n <- 1000
ech <- rpois(n, 1)
th_hat <- exp(1) * mean(abs(cos(ech)))
th_hat

# vérification de la nortmalité asymptotique
z <- replicate(1000, {
  
  ech <- rpois(n, 1)
  th_hat <- exp(1) * mean(abs(cos(ech)))
  sig_hat <- exp(1) * sd(abs(cos(ech)))
  return(sqrt(n) * (th_hat - th)/sig_hat)
  
})
plot(density(z))

# vérification de la couverture de l'intervalle de confiance
alpha <- .05
couv <- replicate(1000, {
  
  ech <- rpois(n, 1)
  th_hat <- exp(1) * mean(abs(cos(ech)))
  sig_hat <- exp(1) * sd(abs(cos(ech)))
  return(
    (th_hat - qnorm(alpha/2, lower.tail=FALSE) * sig_hat / sqrt(n) <= th) && (th_hat + qnorm(alpha/2, lower.tail=FALSE) * sig_hat / sqrt(n) >= th)
  )
  
})
# couv
mean(couv)



# exemple: estimation Mont Carlo d'une espérance par rapport à une loi normale multivariée
# algorithme de génération de normales multivariées
# entrées: taille échantillonnale n, vecteur de moyennes mu, matrice de covariance Sig
normalemulti <- function(n, mu, Sig){
  
  if(length(mu) != nrow(Sig)) stop("Les dimensions de mu et Sig ne sont pas compatibles")
  
  d <- length(mu)
  L <- t(chol(Sig))
  
  z <- matrix(rnorm(n*d), nrow=n)
  return(t(apply(z, 1, function(x) mu + L %*% x)))
  
}

# exemple
mu <- rep(0, 2)
Sig <- matrix(c(1, -.8, -.8, 1), 2, 2)
ech <- normalemulti(1000, mu, Sig)
plot(ech[,1], ech[,2], xlim=c(-3, 3), ylim=c(-3, 3))

# simulation
th <- ???
n <- 100
mu <- rep(0, 2)
Sig <- matrix(c(1, .5, .5, 1), 2, 2)
ech <- normalemulti(n, mu, Sig)
th_hat <- mean(apply(ech, 1, function(x) x[1] * cos(x[2])))
th_hat

# vérification de la nortmalité asymptotique
z <- replicate(1000, {
  
  ech <- normalemulti(n, mu, Sig)
  th_hat <- mean(apply(ech, 1, function(x) x[1] * cos(x[2])))
  sig_hat <- sd(apply(ech, 1, function(x) x[1] * cos(x[2])))
  return(sqrt(n) * (th_hat - th)/sig_hat)
  
})
plot(density(z))

# vérification de la couverture de l'intervalle de confiance
alpha <- .02
couv <- replicate(1000, {
  
  ech <- normalemulti(n, mu, Sig)
  th_hat <- mean(apply(ech, 1, function(x) x[1] * cos(x[2])))
  sig_hat <- sd(apply(ech, 1, function(x) x[1] * cos(x[2])))
  return(
    (th_hat - qnorm(alpha/2, lower.tail=FALSE) * sig_hat / sqrt(n) <= th) && (th_hat + qnorm(alpha/2, lower.tail=FALSE) * sig_hat / sqrt(n) >= th)
  )
  
})
# couv
mean(couv)



# exercice: dans l'exemple normal multivarié, corriger l'estimateur Monte Carlo par la méthode de variable de contrôle,
# en utilisant la fonction auxiliaire m_tilde(X1, X2) = X1

th <- ???
n <- 100
mu <- rep(0, 2)
Sig <- matrix(c(1, .5, .5, 1), 2, 2)
ech <- normalemulti(n, mu, Sig)
th_hat <- mean(apply(ech, 1, function(x) x[1] * cos(x[2])))
th_contr <- th_hat # + ???

abs(th_hat - th)
abs(th_contr - th)



