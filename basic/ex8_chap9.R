set.seed(66)
library(ISLR)
library(e1071)
nt=800
train = sample(nrow(OJ),nt)
test = -train
train_dat = OJ[train,]
test_dat =OJ[test,]

lin.svm = svm(Purchase~.,data=train_dat,kernel="linear",cost=0.01)
#train error
train.pred = predict(lin.svm,train_dat)
t = table(train_dat$Purchase,train.pred)
err_train = 1 - ((t[1,1] + t[2,2]) / sum(t)) 

#test error
test.pred = predict(lin.svm,test_dat)
t1 = table(test_dat$Purchase,test.pred)
err_test = 1- ((t1[1,1] + t1[2,2])/sum(t1))

#use tune function to get optimal
tune.lin = tune(svm,Purchase~.,data=train_dat,kernel="linear",ranges=list(cost=c(0.01,0.1,1,10)))

sm_lin = summary(tune.lin)
sm_lin_best = tune.lin$best.model

#best cost above is 1

lin.svm.best = svm(Purchase~.,data=train_dat,kernel="linear",cost=1)

train.pred.best = predict(lin.svm.best,train_dat)
tb = table(train_dat$Purchase,train.pred.best)
err_train_best = 1 - ((tb[1,1] + tb[2,2])/sum(tb))

test.pred.best = predict(lin.svm.best,test_dat)
tbt = table(test_dat$Purchase,test.pred.best)
err_test_best = 1 - ((tbt[1,1] + tbt[2,2])/sum(tbt))
