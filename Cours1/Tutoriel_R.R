###############################################
#### Tutoriel R/RStudio (en d?veloppement) ####
###############################################

# Le symbole '#' indique que tout ce qui suit sur cette ligne est un commentaire, et sera ignor? par R

# Ici se trouve l'?diteur de texte. Vous pouvez modifier votre code ici
# avant de l'ex?cuter (en bloc ou une ligne ? la fois) dans la console  qui se trouve ci-bas



# Une variable est initialis?e ? l'aide du symbole '<-'
x <- 2
# Toute expression arithm?tique ou variable peut ?tre ?valu?e
x
x+1
3*x
x^2

# Les op?rations arithm?tiques standard: +, -, *, /, ^, && (qui repr?sente le modulo)
# Attention! R respecte l'ordre des op?rations, donc n'oubliez pas les parenth?ses
1+2*3
(1+2)*3



# Un vecteur est cr?? par la fonction c()
vect <- c(2, 3, 5)
vect

# Par d?faut, les op?rations arithm?tiques sur un vecteur sont effectu?es sur chaque ?l?ment
vect + 1
3*vect
vect^2
# On peut aussi additionner (et multiplier, soustraire, diviser) deux vecteurs un ?l?ment ? la fois
vect + 1:3

# Les ?l?ments d'un vecteur sont acc?d?s individuellement ? l'aide de crochets []
vect[1]
# On peut ?galement avoir acc?s ? un sous-vecteur
vect[c(1, 3)]



# Une matrice est initialis?e ? l'aide de la fonction matrix(), qui accepte trois arguments:
# un long vecteur qui contient toutes les entr?es list?es par colonne, le nombre de ligne puis le nombre de colonnes
mat <- matrix(1:9, nrow=3, ncol=3)
mat

# Ses ?l?ments sont index?s par deux indices (plut?t qu'un comme les vecteurs)
mat[1,1]
mat[2,3]

# Une ligne (resp. colonne) peut ?tre extraite sous forme de vecteur,
# en ne sp?cifiant pas le num?ro de colonne (resp. ligne)
mat[2,]
mat[,2]

# Les matrices (et vecteurs) sont transpos? ? l'aide de la fonction t()
t(vect)
t(mat)

# Le produit matriciel est d?not? %*%
mat %*% vect
mat %*% t(mat)



# Un tableau (ou tenseur) g?n?ralise les matrices en permettant un nombre arbitraire d'indices
tabl <- array(1:180, dim=c(3, 4, 3, 5))
tabl



# ? COMPL?TER: listes, data frames



# Une boucle for ex?cute s?quentiellement un code d?pendant d'une variable, pour plusieurs valeurs de cette variable
for(i in c(2, 3, 5)){
  
  print(i + 1)
  
}

x <- 0
for(i in 1:10){
  
  x <- x+i
  
}
x



# Ce qui suit if(condition) est ex?cut? seulement si la condition est satisfaite
# La condition est typiquement une comparaison de type x (<, <=, ==, >=, >) y
x <- 0
if(x < 2) x <- x + 1
x
if(x < 2) x <- x + 1
x
if(x < 2) x <- x + 1
x



# Une fonction est un object qui peut ?tre assign? ? une variable, au m?me titre qu'une variable/vecteur
fn <- function(x) 2*x^3
fn
fn(2)

# Une fonction peut ?tre appliqu?e ? tous les ?l?ments d'un vecteur (ou d'une matrice)
# Attention ? fournir en entr?e un vecteur, et non plusieurs nombres!
fn(1, 3)
fn(c(1, 3))

# La d?finition d'une fonction peut s'?taler sur plusieurs lignes, comporter de nouvelles d?finitions de variables,
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



# Le g?n?rateur de nombres pseudoal?atoires le plus important en R est runif(), qui g?n?re un nombre (? sp?cifier)
# de r?alisations U(0, 1) ind?pendantes
runif(1)
runif(10)

# Le germe est r?initialis? avec la fonction set.seed(), qui accepte un germe entier comme argument
set.seed(40138648)
runif(1)
set.seed(40138648)
runif(1)

# Une seule initialisation du germe au d?but de votre programme est suffisant pour reproduire toute l'exp?rience
# (en autant que vous n'?teigniez pas la session R entre temps)



# ? COMPL?TER: graphiques



