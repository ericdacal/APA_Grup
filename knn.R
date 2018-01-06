####################################################################
# Example 3: The kNN classifier
####################################################################

##Leemos los datos de los ficheros
training <- read.table("sat.trn", header = FALSE, sep = " ")
test <- read.table("sat.tst", header = FALSE, sep = " ")



## first we split a separate test set of relative size 30%
learn.inputs   <- rbind(iris3[1:35,,1], iris3[1:35,,2], iris3[1:35,,3])
learn.classes <- factor(c(rep("s",35), rep("c",35), rep("v",35)))

test.inputs  <- rbind(iris3[36:50,,1], iris3[36:50,,2], iris3[36:50,,3])
test.classes <- factor(c(rep("s",15), rep("c",15), rep("v",15)))

## setup a kNN model with 3 neighbours
## Notice there is no "learning" ... the data is the model (just test!)

myknn <- knn (training[1:36], test[1:36], training$V37, k = 3, prob=TRUE) 



## rows are predictions, columns are true test targets

## one can use the function 'knn1()' when k=1 (just one neighbour)

## How do we optimize k? One way is by using LOOCV

myknn.cv <- knn.cv (training[1:36], training$V37, k = 3)

tab <- table(myknn.cv, training$V37) 
1 - sum(tab[row(tab)==col(tab)])/sum(tab)

## aha! now you see that previous training error (0%) was a little bit optimistic

## Let's loop over k
set.seed (23)

neighbours <- c(1:sqrt(nrow(training[1:36])))
errors <- matrix (nrow=length(neighbours), ncol=2)
colnames(errors) <- c("k","LOOCV error")

for (k in neighbours)
{
  myknn.cv <- knn.cv (training[1:36], training$V37, k = neighbours[k])
  
  # fill in no. of neighbours and LOO validation error
  errors[k, "k"] <- neighbours[k]
  
  tab <- table(myknn.cv, training$V37)
  errors[k, "LOOCV error"] <- 1 - sum(tab[row(tab)==col(tab)])/sum(tab)
}

errors

## It seems that k=6 is the best value
## Now "refit" with k=6 and predict the test set

myknn <- knn (training[1:36], test[1:36], training$V37, k = 6, prob=TRUE) 

tab
tab <- table(myknn, test$V37) 
1 - sum(tab[row(tab)==col(tab)])/sum(tab)

## so our error is 2.2%
