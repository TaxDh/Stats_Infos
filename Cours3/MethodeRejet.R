# algorithme général pour la méthode du rejet
# entrées: taille échantillonnale n, densité cible f, densité instrumentale g, borne M sur le ratio f/g, fonction 'algo_instru' qui génère une observation de g
rejet <- function(n, f, g, M, algo_instru){
  
  valeurs <- rep(0, n)
  for(i in 1:n){
    
    accept <- 0#variable binaire
    while(!accept){
      
      y <- algo_instru()
      u <- runif(1)#genere une uniforme
      if(u <= f(y)/(M*g(y)))
        accept <- 1
      
    }
    
    valeurs[i] <- y
    
  }
  
  return(valeurs)
  
}

# exemple: loi avec support compact
f <- function(x) 3 * x^2 / 26 * (x > 1 && x < 3)
g <- function(x) .5 * (x > 1 && x < 3)
M <- 27/13
algo_instru <- function() 1 + 2*runif(1)
ech <- rejet(1e4, f, g, M, algo_instru)
hist(ech, probability = TRUE)
points_to_plot <- seq(min(ech), max(ech), length.out=1000)
lines(points_to_plot, sapply(points_to_plot, f), col=2, lwd=3)



# parfois, il est plus efficace de programmer la méthode du rejet au cas par cas
# algorithme pour la méthode du rejet spécifiquement pour notre exemple
# entrées: taille échantillonnale n, borne M sur le ratio f/g
f <- function(x) 3 * x^2 / 26 * (x > 1 && x < 3)
g <- function(x) .5 * (x > 1 && x < 3)
#c'est un algorithme qui marche surtout pour cet exemple
rejet_exemple <- function(n, M){
  
  valeurs <- NULL
  for(i in 1:n){
    
    accept <- 0
    while(!accept){
      
      y <- 1 + 2*runif(1)
      u <- runif(1)
      if(u <= f(y)/(M*g(y))) accept <- 1
      valeurs <- cbind(valeurs, c(y, u, accept))
      
    }
    
  }
  
  return(valeurs)
  
}

# exemple
M <- 27/13 # M optimal
# M <- 4 # rejet f(y)/2
data <- rejet_exemple(500, M)
ech <- data[1,which(data[3,] == 1)]
hist(ech, probability = TRUE)
points_to_plot <- seq(min(ech), max(ech), length.out=1000)
lines(points_to_plot, sapply(points_to_plot, f), col=2, lwd=3)

# visualisation des rejets
#noir sont les rejets, verts sont les "accepts"
plot(data[1,], data[2,], col = 1*(1-data[3,]) + 3*data[3,], xlab="Candidats Y", ylab="U")
lines(points_to_plot, sapply(points_to_plot, f), col=2, lwd=3)


# Pas dans les notes de cours
# algorithme pour la méthode du rejet pour la même loi cible, mais différente loi instrumentale
# entrées: taille échantillonnale n, borne M sur le ratio f/g
f <- function(x) 3 * x^2 / 26 * (x > 1 && x < 3)
g <- function(x) 3 * (4-x)^2 / 26 * (x > 1 && x < 3)
rejet_exemple_2 <- function(n, M){
  
  valeurs <- NULL
  for(i in 1:n){
    
    accept <- 0
    while(!accept){
      
      y <- 4 - (27 - 26*runif(1))^(1/3)
      u <- runif(1)
      if(u <= f(y)/(M*g(y))) accept <- 1
      valeurs <- cbind(valeurs, c(y, u, accept))
      
    }
    
  }
  
  return(valeurs)
  
}

# exemple
M <- 9 # M optimal
data <- rejet_exemple_2(500, M)
ech <- data[1,which(data[3,] == 1)]
hist(ech, probability = TRUE)
points_to_plot <- seq(min(ech), max(ech), length.out=1000)
lines(points_to_plot, sapply(points_to_plot, f), col=2, lwd=3)

# visualisation des rejets
plot(data[1,], data[2,], col = 1*(1-data[3,]) + 3*data[3,], xlab="Candidats Y", ylab="U")
lines(points_to_plot, sapply(points_to_plot, f), col=2, lwd=3)
lines(points_to_plot, sapply(points_to_plot, g), col=4, lwd=3)
lines(points_to_plot, sapply(points_to_plot, f)/(M*sapply(points_to_plot, g)), col=6, lwd=3)



# algorithme pour la méthode du rejet pour la loi de Student
# entrées: taille échantillonnale n, degré de liberté k, borne M sur le ratio f/g
rejet_t <- function(n, k){
  
  valeurs <- NULL
  for(i in 1:n){
    
    accept <- 0
    while(!accept){
      
      y <- rt(1, df=1)
      u <- runif(1)
      if(u <= dt(y, df=k)/(M*dt(y, df=1))) accept <- 1
      valeurs <- cbind(valeurs, c(y, u, accept))
      
    }
    
  }
  
  return(valeurs)
  
}

# quel est le m optimal? on sait que si f et g son des densités de Student avec degrés de liberté k et 1,
# respectivement, le ratio f(y)/g(y) est maximisé à 1, peu importe k
K <- 1:100
plot(K, sapply(K, function(k) dt(1, df=k)/dt(1, df=1)))
# semble vouloir converger, donc on peut remplacer f par sa limite quand k -> infini: la densité N(0, 1)
dnorm(1)/dt(1, df=1) # 1.52, probablement le M optimal

# exemple
M <- 8*sqrt(pi) # M naïf
# M <- 1.6 # proche de ce qui semble être le M optimal
k <- 10
data <- rejet_t(500, k)
ech <- data[1,which(data[3,] == 1)]

f <- function(x) dt(x, df=k)
g <- function(x) dt(x, df=1)
hist(ech, probability = TRUE)
points_to_plot <- seq(min(ech), max(ech), length.out=1000)
lines(points_to_plot, sapply(points_to_plot, f), col=2, lwd=3)

# visualisation des rejets
plot(data[1,], data[2,], col = 1*(1-data[3,]) + 3*data[3,], xlab="Candidats Y", ylab="U",
     xlim=c(-5, 5))
lines(points_to_plot, sapply(points_to_plot, f), col=2, lwd=3)
lines(points_to_plot, sapply(points_to_plot, g), col=4, lwd=3)
lines(points_to_plot, sapply(points_to_plot, f)/(M*sapply(points_to_plot, g)), col=6, lwd=3)


# algorithme page 60
# exercice: écrire une fonction qui utilise la méthode polaire pour générer des observations N(0, 1)
# (indice: consulter le fichier R de la semaine passée pour le code de la méthode de Box-Müler)
# entrée: taille échantillonnale n, 
rnorm_polaire <- function(n){
  
  ???
  
}



