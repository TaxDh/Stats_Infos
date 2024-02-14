x<-3
#on veut le classer de facon decroissante
sort(x, decreasing = TRUE, index.return = TRUE)
#tu vas obptenir test$x, test$ix
#pour sorter mes probabilité
#### ex du prof
x <- 1:100
p <- dpois(1:100, lambda = 50)
plot(p)
#je veux les ordonner en ordre decroissante
sort(p, decreasing = TRUE, index.return = TRUE)
#le x associé au plus grand p était 49, le 2e 50, le 3e 51, ...