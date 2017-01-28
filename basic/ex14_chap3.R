set.seed(1)
x1 = runif(100)
x2 = 0.5 * x1 + rnorm(100) / 10
y = 2 + 2 * x1 + 0.3 * x2 + rnorm(100)

#from above the regression coeffs are 2,2,0.3

#check out relationship between x1 and x2
#plot(x1,x2)

#use least squares regression to fit this

lm.fit = lm(y~x1+x2)

#now try using only x1

lm.fit2 = lm(y~x1)

#now try x2

lm.fit3 = lm(y~x2)

#with more observations
x1 = c(x1,0.1)
x2 = c(x2,0.8)
y = c(y,6)

#now refit the models
lm.fit4 = lm(y~x1+x2)

lm.fit5 = lm(y~x1)


lm.fit6 = lm(y~x2)

plot(lm.fit4)
