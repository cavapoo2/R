set.seed(1)
rm(list=ls())
#create quadratic
x = rnorm(100)
y = 2*x^2 + 3 + rnorm(100)
type1 = sample(100,50)
type2 = -type1
y[type1] = y[type1] + 3
y[type2] = y[type2] - 3
plot(x[type1],y[type1], col='red', ylim=c(-2,19) )
points(x[type2],y[type2],col='blue')

z = rep(0,100)
z[type1] = 1 # this class is 1
#take 25 from each type to make train and test data
train = c(sample(type1,25), sample(setdiff(1:100,type1),25))
data.train = data.frame(x=x[train],y=y[train],z=as.factor(z[train]))
data.test = data.frame(x=x[-train],y=y[-train],z=as.factor(z[-train]))

library(e1071)
#use a linear support vector classifier
svm.linear = svm(z~.,data=data.train, kernel="linear",cost=10,scale=FALSE)
plot(svm.linear,data.train)

#support vectors are plotted as crosses
#svm.linear$index

pred_train = predict(svm.linear,data.train)
tb = table(z[train],pred_train)

#now try a polynomial kernel
    
svm.poly = svm(z~.,data=data.train, kernel="polynomial",cost=10,scale=FALSE)

plot(svm.poly,data.train)

pred_poly_train = predict(svm.poly,data.train)
tb_poly = table(z[train], pred_poly_train)# no training errors this time

#now try a radial 
svm.rad = svm(z~.,data=data.train, kernel="radial",gamma=1,cost=10,scale=FALSE)

plot(svm.rad,data.train)

pred_rad_train = predict(svm.rad,data.train)
tb_rad = table(z[train], pred_rad_train)# no training errors this time

#now look at the test errors
plot(svm.linear,data.test)
test_lin = table(z[-train],predict(svm.linear,data.test))

test_poly = table(z[-train],predict(svm.poly,data.test))

test_rad = table(z[-train],predict(svm.rad,data.test))



