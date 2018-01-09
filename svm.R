library(MASS)
library(kernlab)
library(caret)
library(doParallel)


training <- read.table("sat.trn", header = FALSE, sep = " ")
test <- read.table("sat.tst", header = FALSE, sep = " ")

trc <- trainControl(method="cv", number = 4, repeats = 2)

training$V37 <- factor(training$V37)
test$V37 <- factor(test$V37)

cl <- makeCluster(detectCores())
registerDoParallel(cl)

svm.model = train(V37 ~ ., data = training, method="svmRadial", maxit = 200, trControl = trc,
                  tuneGrid = expand.grid(C=c(15, 16, 17), sigma=c(0.01, 0.05, 0.1)))

stopCluster(cl)
svm.pred <- predict(svm.model)
table(training$V37, factor(svm.pred, levels = levels(training$V37)))

svm.pred <- predict(svm.model, newdata = test)
table(test$V37, factor(svm.pred, levels = levels(test$V37)))


      