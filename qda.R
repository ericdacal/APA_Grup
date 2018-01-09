library(MASS)



checkError = function(predicted, real, type) {
  tab = table(factor(predicted, levels = levels(real)), real)
  print(tab)
  print(paste("Error", type))
  error = 100 - (sum(diag(tab)))/length(predicted) * 100
  print(error)
}

##Leemos los datos de los ficheros
training <- read.table("sat.trn", header = FALSE, sep = " ")
test <- read.table("sat.tst", header = FALSE, sep = " ")


##Generem el model
qda.model <- qda (training$V37 ~ ., training)

qda.model

training$V37 <- factor(training$V37)
test$V37 <- factor(test$V37)

# Visualitzacio de l'error de training
pred.train = predict(qda.model)
checkError(pred.train$class, training$V37, "training")

#Error de cross validation
pred.cv <- update(qda.model,CV=TRUE)
checkError(pred.cv$class, training$V37, "cross")

# Visualitzacio de l'error de testing
pred.valid = predict(qda.model, newdata = test)
checkError(pred.valid$class, test$V37, "validacio")
