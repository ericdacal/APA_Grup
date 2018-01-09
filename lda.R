
library(MASS)

##Leemos los datos de los ficheros
training <- read.table("sat.trn", header = FALSE, sep = " ")
test <- read.table("sat.tst", header = FALSE, sep = " ")

# Generem el model
lda.model <- lda (training$V37 ~ ., training)
lda.model

training$V37 <- factor(training$V37)
test$V37 <- factor(test$V37)

# Visualitzacio de l'error de training
pred.train = predict(lda.model)
checkError(pred.train$class, training$V37, "training")

# Visualitzacio de l'error de testing
pred.valid = predict(lda.model, newdata = test)
checkError(pred.valid$class, test$V37, "validacio")