
#load the Auto data
Auto = read.csv("Auto.csv",header=T,na.strings="?")
attach(Auto)
#use mpg and response and 
lm.fit = lm(horsepower~mpg)
res <- summary(lm.fit)
#plot fitted line
yf <- res$coefficients[1] + res$coefficients[2]* Auto$mpg
#par(mfrow=c(1,2))
#plot(mpg,horsepower)
#lines(mpg,yf,type='p',pch=4,col='blue')
#if horsepower is 98, then mpg is
#98 = res$coefficients[1] + res$coefficients[2] *Auto$mpg
mpg_val <- (98 - res$coefficients[1]) / res$coefficients[2]

#to get the 95% confidence intervals for b
#newdata <- data.frame(mpg=30)
newdata <- data.frame(mpg=30)
pr <- predict(lm.fit,newdata,interval="confidence")
pred <- predict(lm.fit,newdata,interval="prediction")


#to get the confidence interval for horsepower=98 then
#we need to refit like this
lm.fit2 = lm(mpg~horsepower)
newdata <- data.frame(horsepower=98)
pr2 <- predict(lm.fit2,newdata,interval="confidence")
pred2 <- predict(lm.fit2,newdata,interval="prediction")

#use abline to plot the lm data
#plot(c(0,50),c(0,200))
plot(lm.fit) #note this shows a lot more
#abline(lm.fit)


detach(Auto)


