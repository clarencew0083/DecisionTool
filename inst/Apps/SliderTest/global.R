library(shiny)
library(shinythemes)      # Bootswatch color themes for shiny
library(choroplethr)      # Creating Choropleth Maps in R
library(choroplethrMaps)  # Maps used by the choroplethr package
library(reshape2)
library(dplyr)
library(ggplot2)
library(plotly)


doSum  <-
  function(value1, value2, value3)
  {
      test <- value1 + value2 + value3
      return(test)

  }

prinEigenVector <-  function(df){

  #norm.df <- sweep(df, 2, colSums(df), FUN="/")
  eig <- eigen(df)

  prinEigvector <- base::Re(eig[["vectors"]][,which.max(eig[["values"]])])
  norm.prinEigvector <- prinEigvector / sum(prinEigvector)

  return(norm.prinEigvector)

}


# Values
my_object = c('a'=1,'b'=2,'c'=3)

criteria <- c("Quality", "Price", "Delivery")

main.attr1 <- c(1, 1/2, 1/3)
main.attr2 <- c(2, 1, 1/2)
main.attr3 <- c(3, 2, 1)


main.criteria <- data.frame(main.attr1, main.attr2, main.attr3)
row.names(main.criteria) <- criteria
colnames(main.criteria) <- criteria

main.weights <- data.frame(prinEigenVector(main.criteria))
row.names(main.weights) <- criteria
main.weights[1,1] <- 0.54
main.weights[2,1] <- 0.186
main.weights[3,1] <- 0.163



#scale(m, center=FALSE, scale=colSums(m)

#Alternatives in respect to quality
alternatives <- c("Supplier A", "Supplier B", "Supplier C")

q.attr1 <- c(1, 1, 1)
q.attr2 <- c(1, 1, 1)
q.attr3 <- c(1, 1, 1)



quality  <- data.frame(q.attr1, q.attr2, q.attr3)
row.names(quality) <- alternatives
colnames(quality) <- alternatives

quality.weights <- as.data.frame(prinEigenVector(quality))
row.names(quality.weights) <- alternatives

#Alternatives in respect to price
p.attr1 <- c(1, 1/3, 1/2)
p.attr2 <- c(3, 1, 2)
p.attr3 <- c(2, 1/2, 1)



price  <- data.frame(p.attr1, p.attr2, p.attr3)
row.names(price) <- alternatives
colnames(price) <- alternatives

price.weights <- as.data.frame(prinEigenVector(price))
row.names(price.weights) <- alternatives


#Alternatives in respect to delivery
d.attr1 <- c(1, 9, 8)
d.attr2 <- c(1/9, 1, 1)
d.attr3 <- c(1/8, 1, 1)



delivery  <- data.frame(d.attr1, d.attr2, d.attr3)
row.names(delivery) <- alternatives
colnames(delivery) <- alternatives

delivery.weights <- as.data.frame(prinEigenVector(delivery))
row.names(delivery.weights) <- alternatives

ahpMatrix <- data.frame(quality.weights, price.weights, delivery.weights)

ahpGlobalWeight <- as.data.frame(as.matrix(ahpMatrix) %*% as.matrix(main.weights))
row.names(ahpGlobalWeight) <- alternatives

scoreMatrix <- as.data.frame(as.matrix(ahpMatrix[1,]) * t(as.matrix(main.weights)))
score2<- rbind(scoreMatrix, as.data.frame(as.matrix(ahpMatrix[2,]) * t(as.matrix(main.weights))))
score3 <- rbind(score2, as.data.frame(as.matrix(ahpMatrix[3,]) * t(as.matrix(main.weights))))

row.names(score3) <- alternatives
