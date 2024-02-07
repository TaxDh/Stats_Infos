set.seed(10)

#fonction ti_discrete page 5/44 qui a été modifié en t_inverse (seul le nom a été changé...)
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
x_decroissante <- rep(0, length(x))
p_decroissante <- rep(0, length(p))

for(i in 1:length(x)){
  x_decroissante[i] <- x[length(x) - i + 1]
  p_decroissante[i] <- p[length(p) - i + 1]
  i <- i+1
}


for (m in 1:10) {
  start_time <- Sys.time()
  ech <- t_inverse(5000, x_decroissante, p)
  end_time <- Sys.time()
  times_decroissante[m] <- end_time - start_time
}

# Méthode optimale
x_optimale <- x
p_optimale <- p

for (i in 1:(length(p)-1)) {
  max_idx <- i
  for (j in (i+1):length(p)) {
    if (p_optimale[j] > p_optimale[max_idx]) {
      max_idx <- j
    }
  }
  
  # Si l'élément maximal n'est pas déjà à sa place, échanger les éléments
  if (max_idx != i) {
    # Échanger les p
    temp_p <- p_optimale[i]
    p_optimale[i] <- p_optimale[max_idx]
    p_optimale[max_idx] <- temp_p
    
    # Échanger les x
    temp_x <- x_optimale[i]
    x_optimale[i] <- x_optimale[max_idx]
    x_optimale[max_idx] <- temp_x
  }
}



for (m in 1:10) {
  start_time <- Sys.time()
  ech <- t_inverse(5000, x, p)
  end_time <- Sys.time()
  times_optimale[m] <- end_time - start_time
}

#voici les 3 histogrammes pour comparé les 3 méthodes
hist(times_croissante, probability = TRUE, main="Méthode Croissante")
hist(times_decroissante, probability = TRUE, main="Méthode Décroissante")
hist(times_optimale, probability = TRUE, main="Méthode Optimale")
