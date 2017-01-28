set.seed(421)
n=500
p=2
x1 = runif(500) - 0.5 # mean 0
x2 = runif(500) - 0.5 # mean 0
y = 1*(((x1^2) - (x2^2)) > 0)

#plot the results
plot(x1[y==0],x2[y==0],col='blue',main="initial")
points(x1[y==1],x2[y==1],col='red')

#fit a logisitc regression
dat = data.frame(y,x1,x2)
glm.fit = glm(y~x1+x2,data=dat,family=binomial)
lm.prob = predict(glm.fit,dat,type="response") # here we use training data to predict
lm.pred = ifelse(lm.prob > 0.53,1,0)  #note we just took mean of lm.prob to get 0.53
data.pos = dat[lm.pred==1,]
data.neg = dat[lm.pred==0,]

plot(data.pos$x1,data.pos$x2,col='blue',main="linear")
points(data.neg$x1,data.neg$x2,col='red')

#if we stuck with 0.5 then all would have been classed same value

#now fit logistic regression using non linear functions

glm.fit.nl = glm(y~poly(x1,2)+I(x2),data=dat,family=binomial)
lm.prob.nl = predict(glm.fit.nl,dat,type="response")
lm.pred.nl = ifelse(lm.prob.nl > 0.5,1,0)
data.pos.nl = dat[lm.pred.nl==1,]
data.neg.nl = dat[lm.pred.nl==0,]

plot(data.pos.nl$x1,data.pos.nl$x2,col='blue',main="non linear")
points(data.neg.nl$x1,data.neg.nl$x2,col='red')

#fit a support vector svm
library(e1071)
svm.radial = svm(as.factor(y)~x1+x2,data=dat,kernel="radial",gamma=1,cost=10,scale=FALSE)
plot(svm.linear,dat,main="svm radial")
svm.pred = predict(svm.radial,dat)
data.pos.svm = dat[svm.pred ==1,]
data.neg.svm = dat[svm.pred ==0,]
plot(data.pos.svm$x1,data.pos.svm$x2,col='blue',main="new svm radial")
points(data.neg.svm$x1,data.neg.svm$x2,col='red')
