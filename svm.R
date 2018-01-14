rm(list = ls())
library(MASS)
library(kernlab)
library(caret)
set.seed (23)

source("functions.R", local = TRUE)

## Llegim les dades dels fitxers
training <- readTraining()
test <- readTest()
load("svm.model")

if (!exists("svm.model")) {
  
  #Els valors final per al nostre model són sigma = 0.1 i C = 15
  svm.model <- train(V37 ~ ., data = training, method="svmRadial", maxit = 200,
                     tuneGrid = expand.grid(C=c(15, 16, 17), sigma=c(0.01, 0.05, 0.1, 0.5)))
  
  save(svm.model, file = "svm.model")
}

svm.model$bestTune

pred <- predict(svm.model)
print("Error de training: ")
print(errorValue(training$V37, pred))

pred <- predict(svm.model, newdata = test)
print("Error de test: ")
print(errorValue(test$V37, pred))

matplot(svm.model$results[svm.model$results$sigma == 0.01,1], 
        data.frame(svm.model$results[svm.model$results$sigma == 0.01,3],
                   svm.model$results[svm.model$results$sigma == 0.05,3], 
                   svm.model$results[svm.model$results$sigma == 0.1,3],
                   svm.model$results[svm.model$results$sigma == 0.5,3]),type = c("b"),pch=1,col = 1:4, xlab = "C", ylab = "error_rate")
legend("right", legend = c("sigma: 0.01","sigma: 0.05","sigma: 0.1", "sigma: 0.5" ), col=1:4, pch=1) # optional legend
