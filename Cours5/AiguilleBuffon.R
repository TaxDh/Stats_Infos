# Implémentation du problème de l'aiguille de Buffon

# représentation graphique du plancher et initialisation
nlattes <- 6
plot(NULL, xlim=c(0, nlattes), ylim=c(0, nlattes), xlab="", ylab="")
for(i in 0:nlattes){
  lines(c(-nlattes, nlattes+1), rep(i, 2), col=4, lw=3)
}
n <- 0
n_succ <- 0

# génération de la position du centre de l'aiguille et de son angle
x <- nlattes*runif(2)
theta <- pi*runif(1)
# calcul des deux extrémités de l'aiguille
tete <- x - c(cos(theta), sin(theta)) / 2
pointe <- x + c(cos(theta), sin(theta)) / 2
# incrémentation
n <- n+1
# code de couleur: rouge si l'aiguille traverse une ligne, noir sinon
coul <- "black"
if( floor(tete[2]) != floor(pointe[2]) ){
  
  n_succ <- n_succ+1
  coul <- "red"
  
}
# représentation graphique de l'aiguille
lines(c(x[1] - cos(theta)/2, x[1] + cos(theta)/2),
      c(x[2] - sin(theta)/2, x[2] + sin(theta)/2),
      col=coul, lw=2)
# estimation de pi mise à jour
2*n/n_succ

# automatisation
for(i in 1:1000){
  
  # génération de la position du centre de l'aiguille et de son angle
  x <- nlattes*runif(2)
  theta <- pi*runif(1)
  # calcul des deux extrémités de l'aiguille
  tete <- x - c(cos(theta), sin(theta)) / 2
  pointe <- x + c(cos(theta), sin(theta)) / 2
  # incrémentation
  n <- n+1
  # code de couleur: rouge si l'aiguille traverse une ligne, noir sinon
  coul <- "black"
  if( floor(tete[2]) != floor(pointe[2]) ){
    
    n_succ <- n_succ+1
    coul <- "red"
    
  }
  # représentation graphique de l'aiguille
  lines(c(x[1] - cos(theta)/2, x[1] + cos(theta)/2),
        c(x[2] - sin(theta)/2, x[2] + sin(theta)/2),
        col=coul, lw=2)
  
}
# estimation de pi mise à jour
2*n/n_succ



