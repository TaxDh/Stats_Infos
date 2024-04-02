#### exemples de chaînes de Markov continues: marche aléatoire (MA) et modèle autorégressif (AR(1)) ####

# MA manuelle
set.seed(55555)
m <- 0
# x <- 0
x <- rnorm(1)
plot(m, x, xlim=c(0, 100), ylim=c(-6, 6))
# bloc à exécuter pour visualiser l'évolution de la chaîne
m <- m+1
x <- c(x, rnorm(1, mean=x[m], sd=1))
points(m, x[m+1])
lines((m-1):m, x[m:(m+1)])

# MA automatisée
set.seed(55555)
M <- 1e4
m <- 0
# x <- 0
x <- rnorm(1)
plot(m, x, xlim=c(0, M), ylim = sqrt(2*M*log(log(M)))*c(-1, 1))
for(t in 1:M){
  
  m <- m+1
  x <- c(x, rnorm(1, mean=x[m], sd=1))
  # points(m, x[m+1])
  lines((m-1):m, x[m:(m+1)])
  
}

# AR(1) automatisé
set.seed(55555)
M <- 1e4
m <- 0
r <- 0.9
# x <- 0
x <- rnorm(1)
plot(m, x, xlim=c(0, M), ylim = sqrt(2*M*log(log(M)))*c(-1, 1))
for(t in 1:M){
  
  m <- m+1
  x <- c(x, rnorm(1, mean=r*x[m], sd=1))
  # points(m, x[m+1])
  lines((m-1):m, x[m:(m+1)])
  
}



#### exemple(s) Gibbs ####
#je crois autour page 22

# exemple normal (moyenne et variance inconnues)
# hyperparamètres
nu <- 0
tau2 <- 1
alpha <- 3
beta <- 1
# observation X
X <- rnorm(1, mean=rnorm(1, nu, tau2), sd=sqrt(1/rgamma(1, alpha, beta)))#genere une obs X normale

# taille échantillonnale désirée
m <- 100

# initialisation de l'échantillon, et choix du point de départ
ech <- matrix(0, nrow=m+1, ncol=2)
ech[1,] <- c(0, 1)# mu = 0, sig2 = 1

# algorithme
for(t in 1:m){
  
  # génération de mu conditionnelle à la valeur de sig2 précédente
  var.tmp <- 1 / (1/tau2 + 1/ech[t,2])
  moy.tmp <- (nu/tau2 + X/ech[t,2]) * var.tmp
  mu <- rnorm(1, mean=moy.tmp, sd=sqrt(var.tmp))
  
  # génération de sig2 conditionnelle à la valeur de mu juste obtenue
  sig2 <- 1 / rgamma(1, shape=alpha+1/2, rate=beta+(mu-X)^2/2)
  
  ech[t+1,] <- c(mu, sig2)
  
}

plot(ech)#meilleur avec m <- 10000

#meilleur avec m <- 100
plot(ech[,1], type="l")#toutes les observations des mu
plot(ech[,2], type="l")#sigma2

#les 2 plots en haut, nous montrent qu'on doit éliminer les prermières observations de la chaine

plot(density(ech[,1]))
plot(density(ech[,2]))


#algo où l'algo de gibs ne fonctionne pas
# exemple uniforme sur deux domaines à peine (ou pas du tout) alignés: (0, 1) x (0, 1) et (1.5, 2.5) x (a, a+1)
# représentation graphique du domaine
a <- .99
plot(NULL, NULL, xlim=c(0, 3), ylim=c(0, 2.5), xlab="x", ylab="y")
ptp <- cbind(c(0, 1, 1, 0), c(0, 0, 1, 1))
polygon(ptp[,1], ptp[,2], col = "grey")
ptp <- cbind(1.5 + c(0, 1, 1, 0), a + c(0, 0, 1, 1))
polygon(ptp[,1], ptp[,2], col = "grey")

# taille échantillonnale désirée
m <- 1000


# initialisation de l'échantillon, et choix du point de départ
ech <- matrix(0, nrow=m+1, ncol=2)
ech[1,] <- c(.5, .5)

# algorithme
for(t in 1:m){
  
  # génération de x conditionnelle à la valeur de y précédente
  y.tmp <- ech[t,2]
  if(y.tmp < a) x <- runif(1, min=0, max=1)
  else if(y.tmp > 1) x <- runif(1, min=1.5, max=2.5)
  else{
    u <- runif(1)
    if(u <= .5) x <- runif(1, min=0, max=1)
    else x <- runif(1, min=1.5, max=2.5)
  }
  
  # génération de y conditionnelle à la valeur de x juste obtenue
  if(x <= 1) y <- runif(1, min=0, max=1)
  else y <- runif(1, min=a, max=a+1)
  
  ech[t+1,] <- c(x, y)
  
}

plot(ech, xlim=c(0, 3), ylim=c(0, 2.5))

plot(ech[,1], type="l")
plot(ech[,2], type="l")



#### exemples Metropolis-Hasting ####

# exemple normal (moyenne inconnue)
# hyperparamètres
nu <- 0
tau2 <- 1
# observation X
X <- rnorm(1, mean=rnorm(1, nu, tau2), sd=1)

# taille échantillonnale désirée
m <- 1000
# variance instrumentale
l <- 2.38*sqrt(1/2)

# initialisation de l'échantillon, et choix du point de départ
# l'échantillon a deux colonnes pour pouvoir sauvegarder les valeurs de mu et les acceptations/rejets
ech <- matrix(0, nrow=m+1, ncol=2)
ech[1,] <- c(25, NA)

# algorithme
for(t in 1:m){
  
  # état précédent de la chaîne
  mu.old <- ech[t]
  
  # génération du candidat mu.new et d'une variable U(0, 1)
  mu.new <- rnorm(1, mean=mu.old, sd=l)
  u <- runif(1)
  
  # test pour déterminer si le saut vers mu.new est accepté
  accept <- (log(u) <= - (X - mu.new)^2/2 - (mu.new - nu)^2/(2*tau2) + (X - mu.old)^2/2 + (mu.old - nu)^2/(2*tau2))
  ech[t+1,2] <- accept
  if(accept) ech[t+1,1] <- mu.new
  else ech[t+1,1] <- mu.old
  
}

plot(ech[,1], type="l")
mean(ech[,2], na.rm=TRUE)

plot(density(ech[,1]))
plot(density(ech[100:(m+1),1]))



# exemple uniforme sur un domaine non connexe: (0, 1) x (0, 1) et (2, 3) x (2, 3)
# représentation graphique du domaine
a <- 2
plot(NULL, NULL, xlim=c(0, 3), ylim=c(0, 3), xlab="x", ylab="y")
ptp <- cbind(c(0, 1, 1, 0), c(0, 0, 1, 1))
polygon(ptp[,1], ptp[,2], col = "grey")
ptp <- cbind(2 + c(0, 1, 1, 0), 2 + c(0, 0, 1, 1))
polygon(ptp[,1], ptp[,2], col = "grey")

# taille échantillonnale désirée
m <- 1000
# variance instrumentale
l <- .5

# initialisation de l'échantillon, et choix du point de départ
# l'échantillon a trois colonnes pour pouvoir sauvegarder les valeurs de (x, y) et les acceptations/rejets
ech <- matrix(0, nrow=m+1, ncol=3)
ech[1,] <- c(.5, .5, NA)

# algorithme
for(t in 1:m){
  
  # état précédent de la chaîne
  x.old <- ech[t,1]
  y.old <- ech[t,2]
  
  # génération du candidat (x.new, y.new)
  x.new <- rnorm(1, mean=x.old, sd=l)
  y.new <- rnorm(1, mean=y.old, sd=l)
  
  # test pour déterminer si le saut vers (x.new, y.new) est accepté, autrement dit si (x.new, y.new) est dans le support de la loi cible
  accept <- (x.new > 0 && x.new < 1 && y.new > 0 && y.new < 1) || (x.new > 2 && x.new < 3 && y.new > 2 && y.new < 3)
  ech[t+1,3] <- accept
  if(accept) ech[t+1,1:2] <- c(x.new, y.new)
  else ech[t+1,1:2] <- c(x.old, y.old)
  
}

plot(ech, xlim=c(0, 3), ylim=c(0, 3), xlab="x", ylab="y")

plot(ech[,1], type="l")
mean(ech[,3], na.rm=TRUE)



