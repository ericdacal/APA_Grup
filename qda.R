rm(list = ls())
library(MASS)
set.seed (23)

source("functions.R", local = TRUE)

# Llegim les dades dels fitxers
training <- readTraining()
test <- readTest()

load("qda.model")
if (!exists("qda.model")) {

    # Generem el model
  qda.model <- qda (training$V37 ~ ., training)
  
  save(qda.model, file = "qda.model")
}

##Generem el model
qda.model <- qda (training$V37 ~ ., training)

# Visualitzacio de l'error de training
pred.train = predict(qda.model)
print("Error de training: ")
print(errorValue(training$V37, pred.train$class))

# Visualitzacio de l'error de testing
pred.valid = predict(qda.model, newdata = test)
print("Error de test: ")
print(errorValue(test$V37, pred.valid$class))
