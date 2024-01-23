# transformation inverse pour loi discrète
# entrées: taille échantillonnale n, vecteur de valeurs possible x et vecteur de probabilités p
ti_discrete <- function(n, x, p){
  
  if(length(x) != length(p)) stop("Dimensions de x et p incompatibles")
  
  valeurs <- rep(0, n)
  for(i in 1:n){
    
    u <- runif(1)
    l <- 1
    while(u > sum(p[1:l])) l <- l+1
    valeurs[i] <- x[l]
    
  }
  
  return(valeurs)
  
}

# exemple Bin(4, 0.5)
x <- 0:4
p <- dbinom(0:4, size=4, prob=.5)
ech <- ti_discrete(1000, x, p)
hist(ech, probability = TRUE, breaks=c(x-0.5, max(x)+0.5))
lines(seq(min(ech), max(ech), length.out=1000),
      sapply(seq(min(ech), max(ech), length.out=1000), function(x) dbinom(floor(x+.5), size=4, prob=.5)),
      col=2)

# exemple Bin(1000, 0.9)
x <- 0:1000
p <- dbinom(0:1000, size=1000, prob=.9)
ech <- ti_discrete(1000, x, p)
hist(ech, probability = TRUE, breaks=c(x-0.5, max(x)+0.5))
lines(seq(min(ech), max(ech), length.out=1000),
      sapply(seq(min(ech), max(ech), length.out=1000), function(x) dbinom(floor(x+.5), size=1000, prob=.9)),
      col=2)



# transformation inverse pour loi exponentielle
# entrées: taille échantillonnale n et paramètre lambda
ti_exp <- function(n, lambda){
  
  valeurs <- -log(1 - runif(n))/lambda
  return(valeurs)
  
}

# exemple Exp(2)
lambda <- 2
ech <- ti_exp(1000, lambda)
hist(ech, probability = TRUE)
lines(seq(min(ech), max(ech), length.out=1000),
      sapply(seq(min(ech), max(ech), length.out=1000), function(x) dexp(x, rate=lambda)),
      col=2)



# transformation inverse pour loi de Pareto
# entrées: taille échantillonnale n et paramètre alpha
ti_pareto <- function(n, alpha){
  
  valeurs <- (1 - runif(n))^(-1/alpha)
  return(valeurs)
  
}

# exemple Pareto(2)
alpha <- 2
ech <- ti_pareto(1000, alpha)
hist(ech, probability = TRUE)
lines(seq(min(ech), max(ech), length.out=1000),
      sapply(seq(min(ech), max(ech), length.out=1000), function(x) alpha*x^(-alpha-1)),
      col=2)

# exemple Pareto(10)
alpha <- 10
ech <- ti_pareto(1000, alpha)
hist(ech, probability = TRUE)
lines(seq(min(ech), max(ech), length.out=1000),
      sapply(seq(min(ech), max(ech), length.out=1000), function(x) alpha*x^(-alpha-1)),
      col=2)



# transformation inverse générale
# entrées: taille échantillonnale n et inverse (généralisée) de la fonction de répartition F_repartition_inv
ti_generale <- function(n, F_repartition_inv){
  
  u <- runif(n)
  valeurs <- sapply(u, F_repartition_inv)
  return(valeurs)
  
}

# exemple de fonction de répartition non continue
F_repartition <- function(x){
  
  if(x < 0) return(0)
  else if(x < 1) return(sqrt(x)/3)
  else if(x < 2) return(1/2)
  else if(x < 3) return(2/3 + (x-2)^2/3)
  else return (1)
  
}

# graphe de la fonction de répartition
plot(seq(0, .999, .001), sapply(seq(0, .999, .001), F_repartition),
     type="l", lwd=2, col=2,
     xlim=c(0, 3), ylim=c(0, 1),
     xlab="x", ylab="F(x)")
lines(seq(1, 1.999, .001), sapply(seq(1, 1.999, .001), F_repartition),
      lwd=2, col=2)
lines(seq(2, 3, .001), sapply(seq(2, 3, .001), F_repartition),
      lwd=2, col=2)
points(c(1, 2), c(1/2, 2/3), pch=16, lwd=2, col=2)

# inverse de la fonction de répartition
F_repartition_inv <- function(u){
  
  if(u <= 1/3) return((3*u)^2)
  else if(u <= 1/2) return(1)
  else if(u <= 2/3) return(2)
  else return(2 + sqrt(3*(u - 2/3)))
  
}

# graphe de l'inverse
plot(seq(0, .5, .001), sapply(seq(0, .5, .001), F_repartition_inv),
     type="l", lwd=2, col=4,
     xlim=c(0, 1), ylim=c(0, 3))
lines(seq(.501, 1, .001), sapply(seq(.501, 1, .001), F_repartition_inv),
      type="l", lwd=2, col=4)
points(.5, 1, pch=16, lwd=2, col=4)

# génération selon cette loi
ech <- ti_generale(1000, F_repartition_inv)
plot(ecdf(ech), xlim=c(0, 3))



# transformation de Box-Müller
# entrées: taille échantillonnale n
box_muller <- function(n){
  
  m <- 2*ceiling(n/2) # le vrai nombre (pair) de réalisations uniformes nécessaire
  u <- runif(m)
  valeurs <- rep(0, m)
  for(i in 1:(m/2)){
    
    r <- sqrt(-2*log(u[2*(i-1)+1]))
    t <- 2*pi*u[2*(i-1)+2]
    
    valeurs[2*(i-1)+1] <- r*cos(t)
    valeurs[2*(i-1)+2] <- r*sin(t)
    
  }
  
  return(valeurs[1:n])
  
}

# exemple
ech <- box_muller(1000)
hist(ech, probability = TRUE)
lines(seq(min(ech), max(ech), length.out=1000),
      sapply(seq(min(ech), max(ech), length.out=1000), function(x) dnorm(x)),
      col=2)



# génération de la loi géométrique
# entrées: taille échantillonnale n et paramètre p
r_geo_bern <- function(n, p){
  
  valeurs <- rep(0, n)
  for(i in 1:n){
    
    j <- 1
    y <- runif(1) <= p
    while(y == 0){
      
      j <- j+1
      y <- runif(1) <= p
      
    }
    
    valeurs[i] <- j
    
  }
  
  return(valeurs)
  
}

# exemple
p <- .2
ech <- r_geo_bern(1000, p)
hist(ech, probability = TRUE, breaks = 0:max(ech)+.5)
lines(seq(min(ech), max(ech), length.out=1000),
      sapply(seq(min(ech), max(ech), length.out=1000), function(x) dgeom(floor(x+.5)-1, prob=p)),
      col=2)



# génération de la loi de Poisson
# entrées: taille échantillonnale n et paramètre lambda
r_poisson_exp <- function(n, lambda){
  
  valeurs <- rep(0, n)
  for(i in 1:n){
    
    l <- 1
    s <- ti_exp(1, lambda)
    while(s <= 1){
      
      l <- l+1
      s <- s + ti_exp(1, lambda)
      
    }
    
    valeurs[i] <- l-1
    
  }
  
  return(valeurs)
  
}

# exemple
lambda <- 5
ech <- r_poisson_exp(1000, lambda)
hist(ech, probability = TRUE, breaks = (-1):max(ech)+.5)
lines(seq(min(ech), max(ech), length.out=1000),
      sapply(seq(min(ech), max(ech), length.out=1000), function(x) lambda^floor(x+.5) * exp(-lambda) / factorial(floor(x+.5))),
      col=2)

# 'sanity check'
lambda <- 1e4
ech <- r_poisson_exp(1000, lambda)
ech_norm <- (ech - lambda)/sqrt(lambda)
hist(ech_norm, probability = TRUE)
lines(seq(min(ech_norm), max(ech_norm), length.out=1000),
      sapply(seq(min(ech_norm), max(ech_norm), length.out=1000), function(x) dnorm(x)),
      col=2)



