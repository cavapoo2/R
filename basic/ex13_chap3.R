set.seed(1)
x = rnorm(100,0,1)
eps = rnorm(100,0,sqrt(0.25)) #variance of 0.25
y = -1 + (0.5*x) + eps
par(mfrow=c(3,1))
plot(x,y)

#fit model
lm.fit = lm(y~x)

#yr = lm.fit$coefficients[1] + lm.fit$coefficients[2]*x
abline(lm.fit,lwd=3,col=2)
abline(-1,0.5,  lwd=3,col=3)
legend(-1, legend = c("model fit", "pop. regression"), col=2:3, lwd=3)

#now fit x^2 term

lm.fit2 = lm(y~x+I(x^2))

#now repeat but with less noise

eps_low = rnorm(100,0,sqrt(0.02)) #variance 0.02
ylow = -1 +(0.5*x) + eps_low

plot(x,ylow)

lm.fit3 = lm(ylow~x)
abline(lm.fit3,lwd=3,col=2)
abline(-1,0.5,lwd=3,col=3)
legend(-1, legend = c("model fit", "pop. regression"), col=2:3, lwd=3)

lm.fit4 = lm(ylow~+x+I(x^2))

#same again but with more noise
eps_high = rnorm(100,0,sqrt(0.75))
yhigh = -1 +(0.5*x) + eps_high

plot(x,yhigh)

lm.fit5 = lm(yhigh~x)
abline(lm.fit5,lwd=3,col=2)
abline(-1,0.5,lwd=3,col=3)
legend(-1, legend = c("model fit", "pop. regression"), col=2:3, lwd=3)

lm.fit6 = lm(yhigh~+x+I(x^2))





