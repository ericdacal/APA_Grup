rm(list = ls())
library(MASS)
set.seed (23)

source("functions.R", local = TRUE)

# Llegim les dades dels fitxers
training <- readTraining()
test <- readTest()

load("lda.model")
if (!exists("lda.model")) {

  # Generem el model
  lda.model <- lda (training$V37 ~ ., training)
  
  save(lda.model, file = "lda.model")
}

# Visualitzacio de l'error de training
pred.train = predict(lda.model)
print("Error de training: ")
print(errorValue(training$V37, pred.train$class))

# Visualitzacio de l'error de testing
pred.valid = predict(lda.model, newdata = test)
print("Error de test: ")
print(errorValue(test$V37, pred.valid$class))
