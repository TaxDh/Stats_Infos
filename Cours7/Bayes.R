# exemple: Poisson-Gamma
# hyperparamètres et observation
a <- 3
b <- 3
x <- 3

# densité a posteriori exacte
post <- function(lambda) dgamma(lambda, shape=a+x, rate=b+1)
plot(post, 0, 5, xlab="lambda", ylab="f(lambda | X)")

# approximation de Laplace (échelle originale)
post_Lap <- function(lambda) dnorm(lambda, mean=(a+x-1)/(b+1), sd=sqrt(a+x-1)/(b+1))
ptp <- seq(0, 5, length.out=100)
lines(ptp, post_Lap(ptp), col=2)

# approximation de Laplace (échelle logarithmique)
post_Laplog <- function(lambda) (1/lambda) * dnorm(log(lambda), mean=log((a+x)/(b+1)), sd=1/sqrt(a+x))
ptp <- seq(0, 5, length.out=100)
lines(ptp, post_Laplog(ptp), col=4)



# exemple: modèle de mélange
# dimension 1, sigma_1 = sigma_2 = 1, nu_1 = nu_2 = 0, lambda_1 = lambda_2 = 10
prior_1 <- function(u1) dnorm(u1, mean=0, sd=10)
prior_2 <- function(u2) dnorm(u2, mean=0, sd=10)
prior_p <- function(p) dunif(p, min=0, max=.5)
densite <- function(x, u1, u2, p) p*dnorm(x, mean=u1, sd=1) + (1-p)*dnorm(x, mean=u2, sd=1)

# génération de l'échantillon
n <- 300 # il faut que le n soit divisible par 3, car le code produit 1/3 ...
data <- c(rnorm(n/3, mean=-1, sd=1), rnorm(2*n/3, mean=1, sd=1))

# calcul de la densité a posteriori (non-normalisée)
post <- function(u1, u2, p) prior_1(u1) * prior_2(u2) * prior_p(p) *
  prod(sapply(data, function(x) densite(x, u1, u2, p)))

# densité a posteriori conditionnelle d'un paramètre avec les deux autres fixés à des valeurs plausibles
ptp <- seq(-2, 2, length.out=1000)
plot(ptp, sapply(ptp, function(u1) post(u1, 1, .33)), type="l", xlab="u1", ylab="densité a posteriori")
plot(ptp, sapply(ptp, function(u2) post(-1, u2, .33)), type="l", xlab="u2", ylab="densité a posteriori")
ptp <- seq(0, .5, length.out=100)
plot(ptp, sapply(ptp, function(p) post(-1, 1, p)), type="l", xlab="p", ylab="densité a posteriori")

# définition de la fonction à optimiser (log densité a posteriori)
# on note l'argument en forme vectorielle, pour compatibilité avec optim
logpost <- function(th) log(post(th[1], th[2], th[3]))
# calcul du maximum a posteriori (MAP) et de la matrice Hessienne de log f( |X) au MAP (H)
opt <- optim(par=c(0, 0, .25), fn=logpost, method="BFGS", control=list(fnscale=-1), hessian = TRUE)
MAP <- opt$par
H <- opt$hessian

# calcul de l'approximation de Laplace de la densité
post_Lap <- function(u1, u2, p) (2*pi)^(-3/2) * det(-H)^(1/2) *
  exp(-(1/2) * t(c(u1, u2, p) - MAP) %*% (-H) %*% (c(u1, u2, p) - MAP))

# visualisation de l'approximation des conditionnelles au MAP
ptp <- seq(-2, 2, length.out=1000)
plot(ptp, sapply(ptp, function(u1) post(u1, MAP[2], MAP[3])) / post(MAP[1], MAP[2], MAP[3]),
     type="l", xlab="u1", ylab="densité a posteriori")
lines(ptp, sapply(ptp, function(u1) post_Lap(u1, MAP[2], MAP[3])) / post_Lap(MAP[1], MAP[2], MAP[3]), col=2)

ptp <- seq(-2, 2, length.out=1000)
plot(ptp, sapply(ptp, function(u2) post(MAP[1], u2, MAP[3])) / post(MAP[1], MAP[2], MAP[3]),
     type="l", xlab="u2", ylab="densité a posteriori")
lines(ptp, sapply(ptp, function(u2) post_Lap(MAP[1], u2, MAP[3])) / post_Lap(MAP[1], MAP[2], MAP[3]), col=2)

ptp <- seq(0, .5, length.out=100)
plot(ptp, sapply(ptp, function(p) post(MAP[1], MAP[2], p)) / post(MAP[1], MAP[2], MAP[3]),
     type="l", xlab="p", ylab="densité a posteriori")
lines(ptp, sapply(ptp, function(p) post_Lap(MAP[1], MAP[2], p)) / post_Lap(MAP[1], MAP[2], MAP[3]), col=2)

# visualisation de l'approximation relative des conditionnelles
ptp <- seq(-1.5, -.5, length.out=1000)
plot(ptp, sapply(ptp, function(u1) post_Lap(u1, MAP[2], MAP[3])/post(u1, MAP[2], MAP[3])) *
       post(MAP[1], MAP[2], MAP[3])/post_Lap(MAP[1], MAP[2], MAP[3]),
     type="l", xlab="u1", ylab="ratio de l'approximation de Laplace a la vraie densité a posteriori")

ptp <- seq(.5, 1.5, length.out=1000)
plot(ptp, sapply(ptp, function(u2) post_Lap(MAP[1], u2, MAP[3])/post(MAP[1], u2, MAP[3])) *
       post(MAP[1], MAP[2], MAP[3])/post_Lap(MAP[1], MAP[2], MAP[3]),
     type="l", xlab="u2", ylab="ratio de l'approximation de Laplace a la vraie densité a posteriori")

ptp <- seq(.3, .4, length.out=100)
plot(ptp, sapply(ptp, function(p) post_Lap(MAP[1], MAP[2], p)/post(MAP[1], MAP[2], p)) *
       post(MAP[1], MAP[2], MAP[3])/post_Lap(MAP[1], MAP[2], MAP[3]),
     type="l", xlab="p", ylab="ratio de l'approximation de Laplace a la vraie densité a posteriori")

# approximation de l'espérance d'une fonction des paramètres avec l'approximation de Laplace (exponentielle)
# n'importe quelle fonction h positive
h <- function(u1, u2, p) ???
loghpost <- function(th) log(h(th[1], th[2], th[3])) + log(post(th[1], th[2], th[3]))
opth <- optim(par=c(0, 0, .25), fn=loghpost, method="BFGS", control=list(fnscale=-1), hessian = TRUE)
MAPh <- opth$par
Hh <- opth$hessian
estimateur <- sqrt(det(-H)/det(-Hh)) * exp(loghpost(MAPh) - logpost(MAP))
estimateur



# exercice: dans l'exemple du modèle de mélange, utilisez la méthode de Monte Carlo
# par échantillonnage préférentiel, apprise en cours, pour estimer l'espérance de h(u1, u2, p),
# avec la(les) fonction(s) h de votre choix, puis comparez à l'approximation de Laplace
# indice: n'oubliez pas qu'il y a deux intégrales à estimer par Monte Carlo, une au numérateur
# et une au dénominateur



