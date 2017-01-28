library(ISLR)
set.seed(4)
n=100
x1 = rnorm(n)
x2 = rnorm(n)
e = rnorm(n,0,0.1)
B0 = -0.5
B1 = 0.25
B2= 0.3
y = B0 + B1*x1 + B2*x2 + e
#plot(y)

#fit the model Y - B1X1 = B0 + B2X2 + e
#keeping B1 fixed fit the model to get beta1
a = y - B1*x1
a.fit = lm(a~x2)
beta1 = a.fit$coef[2] #

#keeping B2 fixed fit the model to get beta2
b = y - B2*x2
b.fit = lm(b~x1)
beta2 = b.fit$coef[2]

B0v = rep(NA,1000)
B1v = rep(NA,1000)
B2v = rep(NA,1000)
B1v[1]=2;
for(i in 1:1000)
{
    a = y - B1v[i]*x1
    B2v[i] = lm(a~x2)$coef[2]
    b = y - B2v[i]*x2
    if (i < 1000)
    {
        fit = lm(b~x1)
        B1v[i+1] = fit$coef[2]
    }
    B0v[i] = fit$coef[1] 
}

plot(1:1000,B0v,type="l",xlab="iteration",ylab="betas",ylim =c(-2.2,2),col="green")
lines(1:1000,B1v,col="red")
lines(1:1000,B2v,col="blue")
legend("center",c("B0v","B1v","B2v"),lty=1,col = c("green","red","blue"))


#now for 100 predictors
p=100;
n=1000
x = matrix(ncol=p,nrow=n)
coefi = rep(NA,p)
for (i in 1:p)
{
    x[,i] = rnorm(n)
    coefi[i] = rnorm(1) * 100
}

y1 = x %*% coefi + rnorm(n)

beta = rep(0,p)
max_its = 1000
errors = rep(NA,max_its + 1)
iter =2 
errors[1]=Inf
errors[2] = sum((y - x %*% beta)^2)
threshold = 1e-06
while (iter < max_its && ((errors[iter-1] - errors[iter]) > threshold))
#while (iter < max_its )
{
    for (i in 1:p)
    {
        a = y - x %*% beta + beta[i] * x[,i]
        beta[i] = lm(a~x[,i])$coef[2]
    }
    iter = iter +1
    errors[iter] = sum((y- x %*% beta)^2)
    print(c(iter-2,errors[iter-1],errors[iter]))
}
print(coefi)
print(beta)

