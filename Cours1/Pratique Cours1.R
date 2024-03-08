# fonction générale qui accepte en entrée paramétres a, b, m, et un germe x0,
# et produit un nombre pseudoaléatoire selon un générateur congruentiel linéaire
generateur_CL <- function(a,b,x0,m){
  y <- (a*x0 +b) %% m
  return(y)
}

#exemple page 16
a <- 1; b=2; m = 16; x0 = 0

x <- rep(NA, 10)
x[1] <- x0
for(i in 1:m){
  x[i+1]  <- generateur_CL(a,b,x[i],m)
}
x

#exemple page 17
a <- 5; b=1; m = 16; x0 = 0

x <- rep(NA, 16)
x[1] <- x0
for(i in 1:m){
  x[i+1] <- generateur_CL(a,b,x[i],m)
}
x

