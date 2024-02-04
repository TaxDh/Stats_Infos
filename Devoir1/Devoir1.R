#fonction ti_discrete page 5/44 de 02-GenerationLoisConnus
t_inverse <- function(n, x, p){
  
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

# exemple Bin(1000, 0.9) avec chronométrage et différentes méthodes d'indexation
x <- 0:1000
p <- dbinom(x, size=1000, prob=.9)

# Initialisation des vecteurs pour les temps d'exécution
times_croissante <- numeric(10)
times_decroissante <- numeric(10)
times_optimale <- numeric(10)

# Méthode croissante
for (m in 1:10) {
  start_time <- Sys.time()
  ech <- t_inverse(5000, x, p)
  end_time <- Sys.time()
  times_croissante[m] <- end_time - start_time
}

# Méthode décroissante
for (m in 1:10) {
  start_time <- Sys.time()
  ech <- t_inverse(5000, x, p)
  end_time <- Sys.time()
  times_decroissante[m] <- end_time - start_time
}

# Méthode optimale
for (m in 1:10) {
  start_time <- Sys.time()
  ech <- t_inverse(5000, x, p)
  end_time <- Sys.time()
  times_optimale[m] <- end_time - start_time
}

# Faites de même pour les méthodes décroissante et optimale

# Puis créez des histogrammes pour chaque set de temps
# Vous pouvez utiliser la fonction par() pour afficher plusieurs graphiques sur la même fenêtre graphique si vous le souhaitez
hist(times_croissante, probability = TRUE, main="Méthode Croissante")
hist(times_decroissante, probability = TRUE, main="Méthode Décroissante")
hist(times_optimale, probability = TRUE, main="Méthode Optimale")
