#this will compute a simple knn
#train is the training data,
#test is the test data,
#k is the number of nearest neighbors
#cl is the classifier
library(class)
simple_knn <- function(train,test,k,cl)
{
    res <- knn(train,test,c1,k,prob=TRUE) #TRUE attains the winning class
    return (res) 
}

#note this would be setup as follows (see book for example 7 chap2 ISL book)
#train=rbind(c(0,3,0),c(2,0,0),c(0,1,3),c(0,1,2),c(-1,0,1),c(1,1,1))
#test <- matrix(c(0,0,0),1,3)
#c1 <- factor(c("Red","Red","Red","Grren","Green","Red"))
#k <- 1   # use 3 also

