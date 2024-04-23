X <- readRDS("Examenfinal_data.RDS")

set.seed(2024)

# Question 1

## a)

# data_obs c'est notre X et m est la taille de la chaine
ABC <- function(m, data_obs, epsilon){
  
  # création de notre échantillon vide
  ech <- rep(NA, m)
  
  # boucle pour générer les m observations à simuler
  for(i in 1:m){
    
    # On initialise la boucle à faux, pour l'obliger à faire un moins une fois la boucle while
    accept <- FALSE
    
    # génération de chaque élément sig2, et savoir si notre sig2 généré respecte la condistion 3.
    while(!accept){
  
      # On génère un sigma^2 qui suit la loi exponentielle avec lambda = 1
      sig2 <- rexp(1, rate = 1)
    
      # génération d'un nouvel échantillon (x_1, ..., x_n) suivant une loi normale de loi N(0, sig2)
      sample_size <- length(data_obs)
      data_simul <- rnorm(sample_size, mean = 0, sd = sig2)
    
      # comparaison de l'échantillon simulé avec l'échantillon observé avec la contrainte
      # que la valeur absolue de la différence des variances soit inférieur à epsilon
      if(abs(var(sort(data_simul))) - (var(sort(X))) <= epsilon) accept <- TRUE
    }
    
    # chaque sig2 qui respecte la condition 3 est ajouté dans ech
    ech[i] <- sig2
    
  }
  
  return(ech)
}


## b)

# On initialise nos paramètres
m <- 1000
epsilon <- 0.01

# On créé notre échantillon avec l'algorithme ABC du numéro a)
echantillon <- ABC(m, X, epsilon)

# On peut observer la répartition des données dans l'histogramme
hist(echantillon, xlim=c(0, 5))
hist(echantillon, xlim=c(0, 2))


## c)

# echantillon est un vecteur formé de mes sigma^2 généré par ma fonction ABC
# donc pour estimé la probabilité a posteriori que sigma^2 > 1,
# nous allons convertir ce vecteur en vecteur avec composante binaire (0 ou 1) 
# faire une somme sur ce nouveau vecteur et le diviser par m.

# on le transforme en variable booleenne
sig2PlusGrandQueUn <- echantillon > 1

# On convertit en numérique
sig2PlusGrandQueUn <- as.numeric(sig2PlusGrandQueUn)

# On fait la somme et on le divise par m
proba <- (sum(sig2PlusGrandQueUn)/m)

proba


# Donc avec un m = 1000, la probabilité de nos sigma^2 soient plus grand que 1 est de 0.003.

###################################################################################################

# Vu que l'algorithme était très rapide, on peut le refaire avec un m = 10000 et voir si l'on obtient le même résultat

m <- 10000
#m <- 100000 #je l'ai mis en commentaire pour éviter un 3e copier-coller
echantillon <- ABC(m, X, epsilon)
hist(echantillon, xlim=c(0, 5))
hist(echantillon, xlim=c(0, 2))

# on le transforme en variable booleenne
sig2PlusGrandQueUn <- echantillon > 1

# On convertit en numérique
sig2PlusGrandQueUn <- as.numeric(sig2PlusGrandQueUn)

# On fait la somme et on le divise par m
proba <- (sum(sig2PlusGrandQueUn)/m)

proba

# On obtient un résultat encore plus faible de 0.0023
# Avec m = 100 000, on semble avoir une probabilité qui converge, étant donné que la probabilité est de 0.00242.