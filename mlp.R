library(MASS)
library(nnet)
set.seed (4567)

##Leemos los datos de los ficheros
training <- read.table("sat.trn", header = FALSE, sep = " ")
test <- read.table("sat.tst", header = FALSE, sep = " ")

ntraining <- nrow(training)
ntest <- nrow(test)

#Escalamos las variables para evitar 'stagnation' (premature convergence)
for (v in 1:(length(training) - 1)) {
  training[v] <- scale(training[v])
  test[v] <- scale(test[v])
}

training$V37 <- factor(training$V37)
test$V37 <- factor(test$V37)

model.nnet <- nnet(V37 ~., data = training, size=20, maxit=2000, decay=0.01)

## Take your time to understand the output
model.nnet 

model.nnet$wts

## I think this way is clearer:

summary(model.nnet)

# Now let's compute the training error

p1 <- as.factor(predict (model.nnet, type="class"))

t1 <- table(p1,training$V37)
error_rate.learn <- 100*(1-sum(diag(t1))/ntraining)
error_rate.learn

# And the corresponding test error

p2 <- as.factor(predict (model.nnet, newdata=test, type="class"))

t2 <- table(p2,test$V37)
error_rate.test <- 100*(1-sum(diag(t2))/ntest)
error_rate.test

ex <- data.frame(size=integer(), decay=double(), error_training=double(), error_test=double(),stringsAsFactors=FALSE)
sizes <- seq(1,10,by=2)+1
decays <- 10^seq(-2,0,by=1)
times <- 10

for (s in sizes) {
  for (d in decays) {
    for (t in 1:times) {
      cat("S: ", s, ", D: ", d, ", T: ", t, "\n")
      model.nnet <- nnet(V37 ~., data = training, size=s, maxit=100000, decay=d, MaxNWts = 100000, trace = FALSE)
      
      p1 <- as.factor(predict (model.nnet, type="class"))
      
      t1 <- table(p1,training$V37)
      error_rate.learn <- 100*(1-sum(diag(t1))/ntraining)
      error_rate.learn
      
      p2 <- as.factor(predict (model.nnet, newdata=test, type="class"))
      
      t2 <- table(p2,test$V37)
      error_rate.test <- 100*(1-sum(diag(t2))/ntest)
      error_rate.test
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

save(ex, file = "results_error_v2")

matplot(ex[ex$decay == 0.01,1], data.frame(ex[ex$decay == 1,4],ex[ex$decay == 0.1,4], ex[ex$decay == 0.01,4]), type = c("b"),pch=1,col = 1:3, xlab = "size", ylab = "error_rate") #plot
legend("topright", legend = c("decay: 1","decay: 0.1","decay: 0.01" ), col=1:3, pch=1) # optional legend
