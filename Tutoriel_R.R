###############################################
#### Tutoriel R/RStudio (en développement) ####
###############################################

# Le symbole '#' indique que tout ce qui suit sur cette ligne est un commentaire, et sera ignoré par R

# Ici se trouve l'éditeur de texte. Vous pouvez modifier votre code ici
# avant de l'exécuter (en bloc ou une ligne à la fois) dans la console  qui se trouve ci-bas



# Une variable est initialisée à l'aide du symbole '<-'
x <- 2
# Toute expression arithmétique ou variable peut être évaluée
x
x+1
3*x
x^2

# Les opérations arithmétiques standard: +, -, *, /, ^, && (qui représente le modulo)
# Attention! R respecte l'ordre des opérations, donc n'oubliez pas les parenthèses
1+2*3
(1+2)*3



# Un vecteur est créé par la fonction c()
vect <- c(2, 3, 5)
vect

# Par défaut, les opérations arithmétiques sur un vecteur sont effectuées sur chaque élément
vect + 1
3*vect
vect^2
# On peut aussi additionner (et multiplier, soustraire, diviser) deux vecteurs un élément à la fois
vect + 1:3

# Les éléments d'un vecteur sont accédés individuellement à l'aide de crochets []
vect[1]
# On peut également avoir accès à un sous-vecteur
vect[c(1, 3)]



# Une matrice est initialisée à l'aide de la fonction matrix(), qui accepte trois arguments:
# un long vecteur qui contient toutes les entrées listées par colonne, le nombre de ligne puis le nombre de colonnes
mat <- matrix(1:9, nrow=3, ncol=3)
mat

# Ses éléments sont indexés par deux indices (plutôt qu'un comme les vecteurs)
mat[1,1]
mat[2,3]

# Une ligne (resp. colonne) peut être extraite sous forme de vecteur,
# en ne spécifiant pas le numéro de colonne (resp. ligne)
mat[2,]
mat[,2]

# Les matrices (et vecteurs) sont transposé à l'aide de la fonction t()
t(vect)
t(mat)

# Le produit matriciel est dénoté %*%
mat %*% vect
mat %*% t(mat)



# Un tableau (ou tenseur) généralise les matrices en permettant un nombre arbitraire d'indices
tabl <- array(1:180, dim=c(3, 4, 3, 5))
tabl



# À COMPLÉTER: listes, data frames



# Une boucle for exécute séquentiellement un code dépendant d'une variable, pour plusieurs valeurs de cette variable
for(i in c(2, 3, 5)){
  
  print(i + 1)
  
}

x <- 0
for(i in 1:10){
  
  x <- x+i
  
}
x



# Ce qui suit if(condition) est exécuté seulement si la condition est satisfaite
# La condition est typiquement une comparaison de type x (<, <=, ==, >=, >) y
x <- 0
if(x < 2) x <- x + 1
x
if(x < 2) x <- x + 1
x
if(x < 2) x <- x + 1
x



# Une fonction est un object qui peut être assigné à une variable, au même titre qu'une variable/vecteur
fn <- function(x) 2*x^3
fn
fn(2)

# Une fonction peut être appliquée à tous les éléments d'un vecteur (ou d'une matrice)
# Attention à fournir en entrée un vecteur, et non plusieurs nombres!
fn(1, 3)
fn(c(1, 3))

# La définition d'une fonction peut s'étaler sur plusieurs lignes, comporter de nouvelles définitions de variables,
# retourner autre chose qu'un scalaire, etc.
fonction_mystere <- function(n){
  
  for(i in 2:(n-1)){
    
    if(floor(n/i) == n/i) stop("Non")
    
  }
  
  return("Oui")
  
}

fonction_mystere(3)
fonction_mystere(12)
fonction_mystere(17)
fonction_mystere(27)



# Le générateur de nombres pseudoaléatoires le plus important en R est runif(), qui génère un nombre (à spécifier)
# de réalisations U(0, 1) indépendantes
runif(1)
runif(10)

# Le germe est réinitialisé avec la fonction set.seed(), qui accepte un germe entier comme argument
set.seed(40138648)
runif(1)
# set.seed(40138648)
runif(1)

# Une seule initialisation du germe au début de votre programme est suffisant pour reproduire toute l'expérience
# (en autant que vous n'éteigniez pas la session R entre temps)



# À COMPLÉTER: graphiques



