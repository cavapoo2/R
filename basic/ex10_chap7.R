library(ISLR)
library(leaps)
set.seed(5)

rows = nrow(College)
train = sample(rows,rows/2)
test = -train

data_train = College[train,]
data_test = College[test,]

#perform best subset selection
y.fit.fwd = regsubsets(Outstate~.,data=data_train,nvmax=17,method="forward")
sm = summary(y.fit.fwd)
par(mfrow=c(1,3))

min.cp = min(sm$cp)
sd.cp = sd(sm$cp)
plot(sm$cp,xlab = "num of vars",ylab="Cp",type="l")
abline(h = min.cp + 0.2*sd.cp,col="blue",lty=2)
abline(h = min.cp - 0.2*sd.cp,col="red",lty=2)

plot(sm$bic,xlab="num of vars",ylab="BIC",type="l")
min.bic = min(sm$bic)
sd.bic = sd(sm$bic)
abline(h = min.bic + 0.2*sd.bic,col="blue",lty=2)
abline(h = min.bic - 0.2*sd.bic,col="red",lty=2)

plot(sm$adjr2,xlab="num of vars", ylab="adjr2",type="l")
min.adjr2 = min(sm$adjr2)
sd.adjr2 = sd(sm$adjr2)
abline(h = min.adjr2 + 0.2*sd.adjr2,col="blue",lty=2)
abline(h = min.adjr2 - 0.2*sd.adjr2,col="red",lty=2)

#looks like 6 predictors is enough
coefi = coef(y.fit.fwd,id=6)

#fit a gam
library(gam)
gam.fit = gam(Outstate~ Private + s(Room.Board,df=2)+s(PhD,df=2)+s(perc.alumni,df=2)+s(Expend,df=5)+s(Grad.Rate,df=2),data=data_train)

par(mfrow=c(2,3))
plot(gam.fit,se=T,col="blue")

#get test R squared
gam.pred = predict(gam.fit,data_test)
gam.err = mean((data_test$Outstate - gam.pred)^2)

gam.tss = mean((data_test$Outstate - mean(data_test$Outstate))^2)
test.rss = 1 - gam.err/gam.tss









