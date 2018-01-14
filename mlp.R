rm(list = ls())
library(MASS)
library(nnet)
set.seed (23)

source("functions.R", local = TRUE)

# Llegim les dades dels fitxers
training <- readTraining()
test <- readTest()

load("nnet.model")

if (!exists("ex")) {
  
  #Escalamos las variables para evitar 'stagnation' (premature convergence)
  for (v in 1:(length(training) - 1)) {
    training[v] <- scale(training[v])
    test[v] <- scale(test[v])
  }
  
  ex <- data.frame(size=integer(), decay=double(), error_training=double(), error_test=double(),stringsAsFactors=FALSE)
  sizes <- seq(1,30,by=2)+1
  decays <- 10^seq(-2,0,by=1)
  times <- 10
  
  for (s in sizes) {
    for (d in decays) {
      for (t in 1:times) {
        cat("S: ", s, ", D: ", d, ", T: ", t, "\n")
        model.nnet <- nnet(V37 ~., data = training, size=s, maxit=100000, decay=d, MaxNWts = 100000, trace = FALSE)
        
        p1 <- predict (model.nnet, type="class")
        error_rate.learn <- errorValue(training$V37, p1)
        
        p2 <- predict (model.nnet, newdata=test, type="class")
        error_rate.learn <- errorValue(test$V37, p2)
        if (t == 1) {
          ex[nrow(ex) + 1,] <- list(s, d, error_rate.learn, error_rate.test)
        } else {
          ex[nrow(ex),] <- list(ex[nrow(ex),1], ex[nrow(ex),2], ex[nrow(ex),3] + error_rate.learn, ex[nrow(ex),4] + error_rate.test)
        }
      }
      ex[nrow(ex),3] <- ex[nrow(ex),3]/times
      ex[nrow(ex),4] <- ex[nrow(ex),4]/times
    }
  }
  
  save(ex, file = "nnet.model")
}

## Mirem quin size i decay té el menor error
ex[which.min(ex[,4]),]

## Sembla que size=26 i decay=1 es el millor valor
print("Error de test: ")
print(ex[which.min(ex[,4]),4])

matplot(ex[ex$decay == 0.01,1], data.frame(ex[ex$decay == 1,4],ex[ex$decay == 0.1,4], ex[ex$decay == 0.01,4]), type = c("b"),pch=1,col = 1:3, xlab = "size", ylab = "error_rate") #plot
legend("topright", legend = c("decay: 1","decay: 0.1","decay: 0.01" ), col=1:3, pch=1) # optional legend
