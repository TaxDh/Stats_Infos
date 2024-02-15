
V = matrix(nrow = 100, ncol = 100, byrow = T)#car 100 fois 100
V

for(i in 1:100){
  for(j in 1:100){
    V[i,j] <- 5 * 0.8^(abs(i-j))
  }
}
V

# Fonction pour la décomposition de Cholesky
ma_chol <- function(cov_matrix) {
  n <- nrow(cov_matrix)
  L <- matrix(0, n, n)
  
  for (i in 1:n) {
    for (j in 1:i) {
      somme <- 0
      if (j > 1) {
        somme <- sum(L[i, 1:(j-1)] * L[j, 1:(j-1)])
      }
      if (i == j) {
        L[i, j] <- sqrt(pmax(cov_matrix[i, i] - somme, 0))
      } else {
        L[i, j] <- (1 / L[j, j]) * (cov_matrix[i, j] - somme)
      }
    }
  }
  return(L)
}

ma_chol2 <- function(cov_mat) {
  n <- nrow(cov_mat)
  L <- matrix(0, n, n)
  
  for (k in 1:n) {
    for (j in 1:n) {
      if(j == k){
        L[k,k] <- sqrt(cov_mat[k,k]- sum(L[k, 1:(k-1)]^2))
      }
      
      
      if (j > k) {
        L[j,k] <- (1/L[k,k])*(cov_mat[j,k] - sum(L[j, 1:(k-1)]*L[k, 1:(k-1)]))
      }
    }
  }
  return(L)
}

ma_chol(V)
ma_chol(matrix(c(4,12,-16,12,37,-43,-16,-43,98), nrow=3, ncol = 3, byrow = T))

ma_chol2(matrix(c(4,12,-16,12,37,-43,-16,-43,98), nrow=3, ncol = 3, byrow = T))



#################################################
#(c)
######################

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
set.seed(1)
box_muller(9)


