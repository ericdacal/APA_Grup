readTraining = function() {
  data <- read.table("sat.trn", header = FALSE, sep = " ")
  data$V37 <- factor(data$V37)
  return(data)
}

readTest = function() {
  data <- read.table("sat.tst", header = FALSE, sep = " ")
  data$V37 <- factor(data$V37)
  return(data)
}

errorValue = function(real, predicted) {
  t <- table(factor(predicted, levels = levels(real)), real)
  error <- 100 - (sum(diag(t)))/length(predicted) * 100
  return(error)
}