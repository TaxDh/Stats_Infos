#-------------------------------------------------------------------------------
# Devoir I - partie informatique
# Auteur: Galiane Charbonneau
# Dernière modification: 1 février 2024
#-------------------------------------------------------------------------------
rm(list = ls())
library(ggplot2)
library(dplyr)
library(stats)
set.seed(2024)
#setwd("~/H24/STT3010/Devoirs/Devoir I")
#-------------------------------------------------------------------------------
# Question 1 c)
#-------------------------------------------------------------------------------
# Définition d'une fonction permettant de simuler des réalisations
# d'une loi discrète

simul_loi_discrete <- function(n, support, proba){
  # param n: nombre de réalisations 
  # param support: un vecteur de nombres (correspond au support de la loi de
  #                laquelle on souhaite simuler)
  # param proba: un vecteur de probabilités (l'entrée i de ce vecteur doit 
  #              correspondre à la probabilité d'observer l'entrée i du 
  #              vecteur support
  #
  # return: cette fonction retourne n valeurs simulées selon la loi donnée
  #         en entrée
  
  # Validation des paramètres 
  if(all.equal(sum(proba), 1) != TRUE) stop("Les probabilités doivent sommer à 1")
  if(length(support) != length(proba)) stop("Les dimensions des vecteurs support et proba sont incompatibles")
  
  # Probabilités cumulatives
  cum_proba <- cumsum(proba)
  
  # Vecteur qui contiendra les valeurs simulées (objet retourné par la fonction)
  real <- rep(NA, n)
  
  # Simulation des n observations
  for (i in 1:n){
    U <- runif(1)
    l <- 1
    
    while(TRUE){
      if (U <= cum_proba[l]){
        real[i] <- support[l]
        break
      }
      l <- l + 1
    }
  }
  return(real)
}
# Vérification (graphique) que la fonction est bien codée
ech_test <- simul_loi_discrete(
  n = 100000, 
  support = 0:5, 
  proba = dbinom(0:5, 5, 0.5)
  )
plot(0:5, table(ech_test)/sum(table(ech_test)), type = "h", lwd = 3, 
     xlab = "", ylab = "probabilité", 
     main = "Comparaison des distributions théorique et empirique", 
     ylim = c(0,0.4),
     cex.main = 0.9)
points(0:5+0.1, dbinom(0:5, 5, 0.5), type = "h", col = "red", lwd = 3)
legend(3.5, 0.4, legend = c("Empirique", "Théorique"), cex = 0.7,
       col = c("black", "red"), lty = c(1,1))

# ---- Simulations ---- 
# (NB) Le but ici étant de calculer le temps requis par l'ordinateur pour 
#      simuler 10 échantillons en utilisant trois stratégies, seul ce temps
#      sera enregistré. Les échantillons eux-même ne seront pas gardés en
#      mémoire
m <- 10    # Nombre d'échantillons
n <- 5000  # Taille de chaque échantillon

# ---- Méthode 1 ----
temps.1 <- c()
for (i in 1:m){
  time_obj <- system.time(
    {
      ech <- simul_loi_discrete(n, 
                                support = 0:1000, 
                                dbinom(0:1000, 1000, 0.9))
    }
  )
  temps.1 <- c(temps.1, time_obj[3])
}
# Vérification que la simulation a fonctionné adéquatement (en utilisant
# le dernier échantillon simulé)
hist(ech, probability = T, col = rgb(0.1, 0.5, 0.9),
     xlab = "", ylab = "Densité", 
     main = "Comparaison des distributions théorique\net empirique - Méthode 1")

# Utilisons une approximation normale (le premier paramètre de la loi binomiale
# étant 1000, il est raisonnable d'invoquer le théorème central limite)
lines(seq(870, 930, by = 0.1),
      dnorm(seq(870, 930, by = 0.1), mean = 1000*0.9, sd = sqrt(1000*0.9*0.1)),
      lwd = 2,
      col = rgb(0.9, 0.1, 0.1))

# ---- Méthode 2 ----
temps.2 <- c()
for (i in 1:m){
  time_obj <- system.time(
    {
      ech <- simul_loi_discrete(n, 
                                support = 1000:0, 
                                dbinom(1000:0, 1000, 0.9))
    }
  )
  temps.2 <- c(temps.2, time_obj[3])
}
# Vérification que la simulation a fonctionné adéquatement (en utilisant
# le dernier échantillon simulé)
hist(ech, probability = T, col = rgb(0.1, 0.5, 0.9),
     xlab = "", ylab = "Densité", 
     main = "Comparaison des distributions théorique\net empirique - Méthode 2")

lines(seq(870, 930, by = 0.1),
      dnorm(seq(870, 930, by = 0.1), mean = 1000*0.9, sd = sqrt(1000*0.9*0.1)),
      lwd = 2,
      col = rgb(0.9, 0.1, 0.1))

# ---- Méthode 3 ----
temps.3 <- c()

loi <- matrix(c(0:1000, dbinom(0:1000, 1000, 0.9)), nrow = 2, byrow = T)
loi <- loi[, order(loi[2,], decreasing = T)] # Ordre optimal
support <- loi[1,]
proba <- loi[2,]

for (i in 1:m){
  time_obj <- system.time(
    {
      ech <- simul_loi_discrete(n, support, proba)
    }
  )
  temps.3 <- c(temps.3, time_obj[3])
}
# Vérification que la simulation a fonctionné adéquatement (en utilisant
# le dernier échantillon simulé)
hist(ech, probability = T, col = rgb(0.1, 0.5, 0.9),
     xlab = "", ylab = "Densité", 
     main = "Comparaison des distributions théorique\net empirique - Méthode 3")

lines(seq(870, 930, by = 0.1),
      dnorm(seq(870, 930, by = 0.1), mean = 1000*0.9, sd = sqrt(1000*0.9*0.1)),
      lwd = 2,
      col = rgb(0.9, 0.1, 0.1))

# ---- Graphiques ----
# Histogramme 
df <- data.frame("temps" = c(temps.1, temps.2, temps.3),
                 "Méthode" = c(rep(1, m), rep(2, m), rep(3, m)))
df[["Méthode"]] <- as.factor(df[["Méthode"]])

gg_hist <- ggplot(df, aes(x = temps, fill = Méthode)) +
  geom_histogram(position = "identity", alpha = 0.5, color = "black") +
  labs(title = "Comparaison des distributions du temps d'exécution (en secondes)\nselon la méthode",
       x = "Temps (secondes)",
       y = "Fréquence")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Boîtes à moustaches
gg_bp <- ggplot(df, aes(x = Méthode, y = temps, fill = Méthode)) +
  geom_boxplot() +
  labs(title = "Comparaison des distributions du temps d'exécution (en secondes)\nselon la méthode", 
       x = "Méthode", 
       y = "Temps (secondes)") +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Affichage des graphiques
print(gg_hist)
print(gg_bp)

# Sauvegarde des graphiques
#ggsave("hist.png", plot = gg_hist, width = 8, height = 4, units = "in")
#ggsave("bp.png", plot = gg_bp, width = 8, height = 4, units = "in")

# ---- Test d'hypothèses ----
# Statistiques descriptives
df %>% 
  dplyr::group_by(Méthode) %>% 
  dplyr::summarise(Moyenne = mean(temps), Ecart_type = sd(temps)) 

# Test de Kruskal Wallis 
kruskal.test(temps ~ Méthode, data = df)

#-------------------------------------------------------------------------------
# Question 2 d)
#-------------------------------------------------------------------------------
rm(list=ls())

# Quelques objets nécessaires à l'expérience
# Densité cible
f <- function(x) dbeta(x, 2, 5)

# Densité instrumentale
g <- function(x) dbeta(x, 1, 3)

# Constante "enveloppante"
M <- 1000

# Nombre d'échantillons
m <- 100

# Nombre de couples par échantillon
n <- 1e6

# Fonction permettant de générer des couples (Y,A)
simul_couple_Y_A <- function(n){
  Y <- rbeta(n, 1, 3)
  U <- runif(n)
  A <- 1*(U <= f(Y)/(M*g(Y)))
  
  return(cbind(Y, A))
}

# Fonction theta_a
theta_a <- function(Y, A) sum(Y*A)/sum(A)

# Fonction theta_r
theta_r <- function(Y, A) 0.25*M - (M-1)*sum(Y*(1-A))/sum(1-A)

# Expérience
theta <- matrix(rep(NA, 2*m), ncol = 2)
for (i in 1:m){
  ech <- simul_couple_Y_A(n)
  
  # On s'assure qu'il y a au moins une acceptation et 
  # au moins un rejet
  
  while(sum(ech[, "A"]) %in% c(0, nrow(ech))){
    ech <- simul_couple_Y_A(n)
  }
  
  # Réalisations des estimateurs
  theta[i, 1] <- theta_a(ech[, "Y"], ech[, "A"])
  theta[i, 2] <- theta_r(ech[, "Y"], ech[, "A"])
}

# Estimation du biais de chaque estimateur
# L'espérance de X (le theta à estimer) est 2/7
print(paste("Le biais estimé de theta_a est", mean(theta[,1]) - 2/7))
print(paste("Le biais estimé de theta_a est", mean(theta[,2]) - 2/7))

# Estimation de la variance de chaque estimateur
print(paste("La variance estimée de theta_a est", var(theta[,1])))
print(paste("La variance estimée de theta_a est", var(theta[,2])))

# Rapport des variances
print(paste("Le rapport des variances est", var(theta[,1])/var(theta[,2])))