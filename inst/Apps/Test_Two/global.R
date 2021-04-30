library(shiny)
library(shinythemes)      # Bootswatch color themes for shiny
library(choroplethr)      # Creating Choropleth Maps in R
library(choroplethrMaps)  # Maps used by the choroplethr package
library(reshape2)
library(dplyr)
library(ggplot2)
library(plotly)


prinEigenVector <-  function(df){

  #norm.df <- sweep(df, 2, colSums(df), FUN="/")
  eig <- eigen(df)

  prinEigvector <- base::Re(eig[["vectors"]][,which.max(eig[["values"]])])
  norm.prinEigvector <- prinEigvector / sum(prinEigvector)

  return(norm.prinEigvector)

}


criteria <- c("Criteria1", "Criteria2")

main.attr1 <- c(1, 1/2)
main.attr2 <- c(2, 1)


main.criteria <- data.frame(main.attr1, main.attr2)
row.names(main.criteria) <- criteria
colnames(main.criteria) <- criteria

main.weights <- data.frame(prinEigenVector(main.criteria))
row.names(main.weights) <- criteria
#main.weights[1,1] <- 0.54
#main.weights[2,1] <- 0.186




#scale(m, center=FALSE, scale=colSums(m)

#SubCriteria1 Weighting
subCriteria1.attributes <- c("subCriteria1 Attr 1", "subCriteria1 Attr 2", "subCriteria1 Attr 3", "subCriteria1 Attr 4")

subcriteria1.attr1  <- c(1, 1, 1, 1)
subcriteria1.attr2  <- c(1, 1, 1, 1)
subcriteria1.attr3  <- c(1, 1, 1, 1)
subcriteria1.attr4  <- c(1, 1, 1, 1)



subcriteria1  <- data.frame(subcriteria1.attr1, subcriteria1.attr2, subcriteria1.attr3, subcriteria1.attr4)
row.names(subcriteria1) <- subCriteria1.attributes
colnames(subcriteria1) <- subCriteria1.attributes

subcriteria1.weights <- as.data.frame(prinEigenVector(subcriteria1))
row.names(subcriteria1.weights) <- subCriteria1.attributes

#Alternatives in respect to SubCriteria1
alternatives <- c("Alt 1", "Alt 2", "Alt 3", "Alt 4", "Alt 5", "Alt 6")

subcriteria1.alt1  <- c(1, 1, 1, 1, 1, 1)
subcriteria1.alt2  <- c(1, 1, 1, 1, 1, 1)
subcriteria1.alt3  <- c(1, 1, 1, 1, 1, 1)
subcriteria1.alt4  <- c(1, 1, 1, 1, 1, 1)
subcriteria1.alt5  <- c(1, 1, 1, 1, 1, 1)
subcriteria1.alt6  <- c(1, 1, 1, 1, 1, 1)


subcriteria1.altScoring  <- data.frame(subcriteria1.alt1, subcriteria1.alt2, subcriteria1.alt3, subcriteria1.alt4, subcriteria1.alt5, subcriteria1.alt6)
row.names(subcriteria1.altScoring) <- alternatives
colnames(subcriteria1.altScoring) <- alternatives

subcriteria1.altScore <- as.data.frame(prinEigenVector(subcriteria1.altScoring))
row.names(subcriteria1.altScore) <- alternatives




#SubCriteria2 weighting
subCriteria2.attributes <- c("subCriteria2 Attr 1", "subCriteria2 Attr 2", "subCriteria2 Attr 3", "subCriteria2 Attr 4",
                             "subCriteria2 Attr 5", "subCriteria2 Attr 6", "subCriteria2 Attr 7")

subCriteria2.attr1  <- c(1, 1, 1, 1, 1, 1, 1)
subCriteria2.attr2  <- c(1, 1, 1, 1, 1, 1, 1)
subCriteria2.attr3  <- c(1, 1, 1, 1, 1, 1, 1)
subCriteria2.attr4  <- c(1, 1, 1, 1, 1, 1, 1)
subCriteria2.attr5  <- c(1, 1, 1, 1, 1, 1, 1)
subCriteria2.attr6  <- c(1, 1, 1, 1, 1, 1, 1)
subCriteria2.attr7  <- c(1, 1, 1, 1, 1, 1, 1)


subCriteria2  <- data.frame(subCriteria2.attr1, subCriteria2.attr2, subCriteria2.attr3, subCriteria2.attr4, subCriteria2.attr5, subCriteria2.attr6
                            , subCriteria2.attr7)
row.names(subCriteria2) <- subCriteria2.attributes
colnames(subCriteria2) <- subCriteria2.attributes

subCriteria2.weights <- as.data.frame(prinEigenVector(subCriteria2))
row.names(subCriteria2.weights) <- subCriteria2.attributes

#Alternatives in respect to SubCriteria2


subCriteria2.alt1  <- c(1, 1, 1, 1, 1, 1)
subCriteria2.alt2  <- c(1, 1, 1, 1, 1, 1)
subCriteria2.alt3  <- c(1, 1, 1, 1, 1, 1)
subCriteria2.alt4  <- c(1, 1, 1, 1, 1, 1)
subCriteria2.alt5  <- c(1, 1, 1, 1, 1, 1)
subCriteria2.alt6  <- c(1, 1, 1, 1, 1, 1)


subCriteria2.altScoring  <- data.frame(subCriteria2.alt1, subCriteria2.alt2, subCriteria2.alt3, subCriteria2.alt4, subCriteria2.alt5, subCriteria2.alt6)
row.names(subCriteria2.altScoring) <- alternatives
colnames(subCriteria2.altScoring) <- alternatives

subCriteria2.altScore <- as.data.frame(prinEigenVector(subCriteria2.altScoring))
row.names(subCriteria2.altScore) <- alternatives

#as.matrix(subcriteria1.altScore) %*% t(as.matrix(subcriteria1.weights))

score1 <- as.data.frame((as.matrix(subcriteria1.altScore) %*% t(as.matrix(subcriteria1.weights))) * main.weights[1,1])
score2 <- as.data.frame((as.matrix(subCriteria2.altScore) %*% t(as.matrix(subCriteria2.weights))) * main.weights[2,1])
scoreMatrix <- cbind(score1, score2)
row.names(scoreMatrix) <- alternatives
