library(leaps)
set.seed(3)
n=100
x = rnorm(n,0,1)  
e = rnorm(n,0,0.01)
B0 = 0.3
B1 = 1.2
B2 = -0.2
B3 = 0.5
y = B0 + B1*(x) + B2* (x^2) + B3* (x^3) + e
b1x = B1*x
b2x = B2*(x^2)
b3x = B3*(x^3)
par(mfrow=c(1,4))
#plot(x,y)

#fr = data.frame(y,b1x,b2x,b3x)
fr = data.frame(y,x,x^2,x^3,x^4,x^5,x^6,x^7,x^8,x^9,x^10)

#regfit.full = regsubsets(y~.,fr)
#res = summary(regfit.full)

#seems like using 3 variables is most accurate
y.fit = regsubsets(y~poly(x,10,raw=T),data=fr,nvmax=10)
res = summary(y.fit)
c = coef(y.fit,10)

#yres = c[1] + c[2]*(x) + c[3]*(x^2) + c[4]*(x^3)
#lines(x,yres,type='p',pch=4,col='blue')

#to get best fit
a1 = which.min(res$cp)
a2 = which.min(res$bic)
a3 = which.max(res$adjr2)
#best model is 4

#y.app = c[1] + c[2]*x + c[3]*(x^2) + c[4]*(x^3) + c[5]*(x^4) +c[6]*(x^5) + c[7]*(x^6)
y.app = c[1] + c[2]*x + c[3]*(x^2) + c[4]*(x^3) 
plot(x,y)
lines(x,y.app,type='p',pch=1,col='blue')

#now try forward step selection

y.fit.fwd = regsubsets(y~poly(x,10,raw=T),data=fr,nvmax=10,method="forward")

res.fwd = summary(y.fit.fwd)
c.fwd = coef(y.fit.fwd,10)

a1.fwd = which.min(res.fwd$cp)
a2.fwd = which.min(res.fwd$bic)
a3.fwd = which.max(res.fwd$adjr2)


y.app.fwd = c.fwd[1] + c.fwd[2]*x + c.fwd[3]*(x^2) + c.fwd[4]*(x^3) 

plot(x,y)
lines(x,y.app.fwd,type='p',pch=1,col='red')

#now try backward

y.fit.bck = regsubsets(y~poly(x,10,raw=T),data=fr,nvmax=10,method="backward")

res.bck = summary(y.fit.bck)
c.bck = coef(y.fit.bck,10)

a1.bck = which.min(res.bck$cp)
a2.bck = which.min(res.bck$bic)
a3.bck = which.max(res.bck$adjr2)


y.app.bck = c.bck[1] + c.bck[2]*x + c.bck[3]*(x^2) + c.bck[4]*(x^3) 

plot(x,y)
lines(x,y.app.bck,type='p',pch=1,col="salmon3")


#now fit a lasso
library(glmnet)
train = sample(1:length(x), length(x)/2)
test =(-train)
y.test=y[test]
xdat = model.matrix(y~.,fr)[,-1]
#xdat = fr[,-1]
grid = 10^seq(10,-2,length = n)
lasso.mod= glmnet(xdat[train,],y[train],alpha=1,lambda=grid)

#now use cross validation
set.seed(4)
cv.out = cv.glmnet(xdat[train,],y[train],alpha=1)

par(mfrow=c(1,1))
plot(cv.out)
bestlam = cv.out$lambda.min
lasso.pred = predict(lasso.mod,s=bestlam,newx = xdat[test,])
mean.lasso = mean((lasso.pred-y.test)^2)

out.lasso = glmnet(xdat,y,alpha=1,lambda=grid)
lasso.coef = predict(out.lasso,type="coefficients",s=bestlam)[1:11,]

#now generate a response for y = B0 + B7*X^7 + e
B7 = 0.85
y7 = B0 + B7*(x^7) + e
#use best subset
fr7 = data.frame(y7,fr[,-1])
par(mfrow=c(1,1))
plot(x,y7)
#seems like using 3 variables is most accurate
y.fit7 = regsubsets(y7~poly(x,10,raw=T),data=fr7,nvmax=10)
res7 = summary(y.fit7)
c7 = coef(y.fit7,10)
#get best fit
#to get best fit
a17 = which.min(res7$cp)
a27 = which.min(res7$bic)
a37 = which.max(res7$adjr2)
#
y7_approx = c7[1] + c7[8]*x^7

lines(x,y7_approx,type='p',pch=10,col='blue')
#test mean sqr error (note data should be split into train and test really)
mean.7 = mean((y7_approx - y7)^2)

#now use lasso
xdat7 = model.matrix(y7~.,fr7)[,-1]
lasso.mod7 = glmnet(xdat7,y7,alpha=1,lambda=grid)
cv.out7 = cv.glmnet(xdat7,y7,alpha=1)
bestlam7 = cv.out7$lambda.min
lasso7.coef = predict(lasso.mod7,type="coefficients",s=bestlam7)[1:11,]
y7_lasso_approx = lasso7.coef[1] + lasso7.coef[8]*x^7

mean.lasso.7 = mean((y7_lasso_approx - y7)^2)

lines(x,y7_lasso_approx,type='p',pch=0,col='red')




