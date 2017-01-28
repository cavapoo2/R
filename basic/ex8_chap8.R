set.seed(5)
library(ISLR)
library(MASS)
library(tree)
train = sample(1:nrow(Carseats),nrow(Carseats)/2)
test = -train
tree.carseats = tree(Sales~.-Sales,Carseats[train,])
pred = predict(tree.carseats,newdata=Carseats[test,])
plot(pred,Carseats[test,"Sales"])
abline(0,1)
mse = mean((pred-Carseats[test,"Sales"])^2)

#use cross validation
cv.car = cv.tree(tree.carseats)
plot(cv.car$size,cv.car$dev,type='b')

#from graph seems like 10 is best
prune.car = prune.tree(tree.carseats,best=10)

#now get improved mse
pred_pruned = predict(prune.car,newdata=Carseats[test,])
plot(pred_pruned,Carseats[test,"Sales"])
abline(0,1)
mse_pruned = mean((pred_pruned - Carseats[test,"Sales"])^2)

#note pruning the tree did not improve mse

#now use bagging approach n = p
library(randomForest)
bag.carseats = randomForest(Sales~.-Sales,data=Carseats,subset=train,mtry=ncol(Carseats)-1,importance=TRUE)
pred.bag = predict(bag.carseats,newdata=Carseats[test,])
plot(pred.bag,Carseats[test,"Sales"])
abline(0,1)
mse_bag = mean((pred.bag - Carseats[test,"Sales"])^2)
imp = importance(bag.carseats)

#now try a random forest
ps = (ncol(Carseats)-1)/3

rf.carseats = randomForest(Sales~.,data=Carseats,subset=train,mtry=ps,importance=TRUE)
rf.pred = predict(rf.carseats,newdata=Carseats[test,])
mse_rf = mean((rf.pred - Carseats[test,"Sales"])^2) 

