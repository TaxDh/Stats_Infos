# fonction générale qui accepte en entrée paramétres a, b, m, et un germe x0,
# et produit un nombre pseudoaléatoire selon un générateur congruentiel linéaire
gcl <- function(a, b, m, x0) (a*x0 + b) %% m #symbole modulo => %%

# taille de l'échantillon à produire
n <- 100



# bon choix de paramètres
m <- 2^32
a <- 69069
b <- 23606797

# choix du germe
x0 <- 11
# initialisation d'un vecteur vide qui contiendra l'échantillon obtenu
good_sample <- rep(0, n+1)
good_sample[1] <- x0
# exécution du générateur
for(i in 1:n){
  
  good_sample[i+1] <- gcl(a, b, m, good_sample[i])
  
}
# représentation graphique
plot(good_sample/m)
hist(good_sample/m)



# mauvais choix de paramétres (courte période -> répétition)
m <- 2^6
a <- 69069
b <- 23606797

# choix du germe
x0 <- 11
# initialisation d'un vecteur vide qui contiendra l'échantillon obtenu
bad_sample <- rep(0, n+1)
bad_sample[1] <- x0
# exécution du générateur
for(i in 1:n){
  
  bad_sample[i+1] <- gcl(a, b, m, bad_sample[i])
  
}
# représentation graphique
plot(bad_sample/m)



# mauvais choix de paramétres (a trop petit -> corrélation)
m <- 2^32
a <- 3
b <- 23606797

# choix du germe
x0 <- 11
# initialisation d'un vecteur vide qui contiendra l'échantillon obtenu
bad_sample <- rep(0, n+1)
bad_sample[1] <- x0
# exécution du générateur
for(i in 1:n){
  
  bad_sample[i+1] <- gcl(a, b, m, bad_sample[i])
  
}
# représentation graphique
plot(bad_sample/m)
plot(bad_sample/m, type="l")
plot(good_sample, type="l")



# trés mauvais choix de paramétres (a et b petits -> trés long avant de "mixer" é partir d'un petit germe)
m <- 2^32
a <- 3
b <- 1

# choix du germe
x0 <- 11
# initialisation d'un vecteur vide qui contiendra l'échantillon obtenu
bad_sample <- rep(0, n+1)
bad_sample[1] <- x0
# exécution du générateur
for(i in 1:n){
  
  bad_sample[i+1] <- gcl(a, b, m, bad_sample[i])
  
}
# représentation graphique
plot(bad_sample/m)



# trés mauvais choix de paramétres (a=1 -> suite affine)
m <- 2^32
a <- 1
b <- 1

# choix du germe
x0 <- 11
# initialisation d'un vecteur vide qui contiendra l'échantillon obtenu
bad_sample <- rep(0, n+1)
bad_sample[1] <- x0
# exécution du générateur
for(i in 1:n){
  
  bad_sample[i+1] <- gcl(a, b, m, bad_sample[i])
  
}
# représentation graphique
plot(bad_sample/m)



# trés mauvais choix de paramétres (état absorbant)
m <- 2^32
a <- 2
b <- 23606797

# choix du germe
x0 <- 11
# initialisation d'un vecteur vide qui contiendra l'échantillon obtenu
bad_sample <- rep(0, n+1)
bad_sample[1] <- x0
# exécution du générateur
for(i in 1:n){
  
  bad_sample[i+1] <- gcl(a, b, m, bad_sample[i])
  
}
# représentation graphique
plot(bad_sample/m)



# choix idéal (utilise MT19937)
set.seed(11111)
great_sample <- runif(n)
plot(great_sample)

# quelques test
hist(great_sample)
plot(ecdf(great_sample))
lines(c(0, 1), c(0, 1), col=2)
ks.test(great_sample, punif)



# exercice: écrivez (dans un nouve fichier .R) une fonction qui accepte en entrée les valeurs a, b, m,
# et qui calcule la période du GCL de paramétres a, b, m



period <- function(a,b,m){
  nombre <- rep(0, m+1)
  nombre[1] <- 0
  compte <- 1
  i <- 1
  while(compte <= m ){
    #print(compte)
    nombre[i+1] <- gcl(a, b, m, nombre[i])
    i <- i+1
    if(nombre[i]==0) {
      print(compte)
      stop("Non")
    }
    compte <- compte + 1
  }
  print(compte)
}
period(1,2,16)
period(5,1,16)
period(1,1,10)
