set.seed(5)
dat = data.frame(X1 =c(3,2,4,1,2,4,4),X2=c(4,2,4,4,1,3,1),Y=c("Red","Red","Red","Red","Blue","Blue","Blue"))

plot(x=c(3,2,4,1),y=c(4,2,4,4),type='p',col='red',ylim=c(0,5),xlim=c(0,5))
lines(x=c(2,4,4),y=c(1,3,1),type='p',col='blue')
abline(-0.5,1)
abline(0,1)

