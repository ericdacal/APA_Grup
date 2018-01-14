rm(list = ls())
library(class)
library(MASS)
set.seed (23)

source("functions.R", local = TRUE)

## Llegim les dades dels fitxers
training <- readTraining()
test <- readTest()

load("knn.model")

if (!exists("errors")) {
  neighbours <- c(1:sqrt(nrow(training[1:36])))
  errors <- matrix (nrow=length(neighbours), ncol=2)
  colnames(errors) <- c("k","error")
  ## Paral·lelitzem el for
  
  for (k in neighbours) {
    myknn <- knn(training[1:36], test[1:36], training$V37, k = k, prob=TRUE)
    
    #Omplim el número de veins i  el LOO validation error
    errors[k, "k"] <- neighbours[k]
    errors[k, "error"] <- errorValue(test$V37, myknn)
  }
  
  save(errors, file = "knn.model")
}

## Mirem quina k té el menor error
errors[which.min(errors[,2]),]

## Sembla que k=3 es el millor valor
print("Error de test: ")
print(errors[which.min(errors[,2]),2])

matplot(errors[,1], errors[,2], type = c("b"),pch=1,col = 1, xlab = "k", ylab = "error_rate") #plot
