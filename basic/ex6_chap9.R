#generate  2 classes p=2 so classes are
#just barely separable
set.seed(4)
n=1000
x = matrix(rnorm(n*2), ncol=2)
y = c(rep(-1,n/2),rep(1,n/2))
x[y==1,] = x[y==1,] +  3  
dat = data.frame(y,x)
plot(x[y==-1],col='blue',ylim=c(-5,10))
points(x[y==1],col='red')
library(e1071)
tune.out = tune(svm,as.factor(y)~.,data=dat,kernel="linear",ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100)))
sm = summary(tune.out) # seems like 0.1 is best
smbest = tune.out$best.model

res = data.frame(cost = tune.out$performances$cost, misclass = tune.out$performances$error * 100)   
#try training test

#svm.fit1 = svm(as.factor(y)~.,data=dat,kernel="linear",cost=0.01,scale=FALSE)
#plot(svm.fit1,dat)
costs=c(0.001,0.01,0.1,1,5,10,100)
#create some test data
xt = matrix(rnorm(n*2),ncol=2)
yt = c(rep(-1,n/2),rep(1,n/2))
xt[y==1,] = x[y==1,] + 3
dat_test = data.frame(yt,xt)
errs = rep(NA,length(costs))
errs_test = rep(NA,length(costs))
i=1
for(c in costs)
{
    svm.fit = svm(as.factor(y)~.,data=dat,kernel="linear",cost=c,scale=FALSE)
    pred = predict(svm.fit,dat)
    pred_test = predict(svm.fit,dat_test)
   # print(pred)
    #tabr = table(y,pred)
    errs[i] = sum(pred != dat$y)
    errs_test[i] = sum(pred_test != dat_test$yt)
    i= i + 1
}
