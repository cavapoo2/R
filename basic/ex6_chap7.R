library(ISLR)
set.seed(3)
p=10
cv.error = rep(NA,10)
for (i in 1:p)
{
    glm.fit = glm(wage~poly(age,i), data= Wage)
    cv.error[i] = cv.glm(Wage,glm.fit,K=p)$delta[2]
}

plot(1:p,cv.error,xlab="degree",ylab="CV error",type="l",pch=20,lwd=2,ylim=c(1590,1700))
min.point = min(cv.error)
sd.points = sd(cv.error)
abline(h=min.point+0.2 * sd.points, col="red",lty="dashed")
abline(h=min.point-0.2 * sd.points, col="blue",lty="dashed")

#compare this with anova
lm.fit1 = lm(wage~poly(age,1),data = Wage)
lm.fit2 = lm(wage~poly(age,2),data = Wage)
lm.fit3 = lm(wage~poly(age,3),data = Wage)
lm.fit4 = lm(wage~poly(age,4),data = Wage)
lm.fit5 = lm(wage~poly(age,5),data = Wage)
lm.fit6 = lm(wage~poly(age,6),data = Wage)
lm.fit7 = lm(wage~poly(age,7),data = Wage)
lm.fit8 = lm(wage~poly(age,8),data = Wage)
lm.fit9 = lm(wage~poly(age,9),data = Wage)
lm.fit10 = lm(wage~poly(age,10),data = Wage)

an.res = anova(lm.fit1,lm.fit2,lm.fit3,lm.fit4,lm.fit5,lm.fit6,lm.fit7,lm.fit8,lm.fit9,lm.fit10)

#anova shows that anything above 3 is worse according to p values
#plot the prediction

par(mfrow=c(1,2),mar=c(4.5,4.5,1,1),oma=c(0,0,4,0))
agelims = range(Wage$age)
age.grid = seq(from=agelims[1],to=agelims[2])
lm.fit = lm(wage~poly(age,3),data=Wage)
preds = predict(lm.fit,newdata=list(age=age.grid),se=TRUE)
se.bands = cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)
par(mfrow=c(1,1),mar=c(4.5,4.5,1,1),oma=c(0,0,4,0))
plot(Wage$age,Wage$wage,xlim=agelims,cex=0.5,col="darkgrey")
title("degree 3 poly",outer=T)
lines(age.grid,preds$fit,lwd=2,col="blue")
matlines(age.grid,se.bands,lwd=1,col="red",lty=3)


#predict wage using age > 250  
fit.bool = glm(I(wage > 250)~poly(age,3),data=Wage,family=binomial)
preds.bool = predict(fit.bool,newdata=list(age=age.grid),se=T)
pfit = exp(preds.bool$fit) / (1 + exp(preds.bool$fit))
se.bands.logit = cbind(preds.bool$fit+2*preds.bool$se.fit, preds.bool$fit-2*preds.bool$se.fit)
se.bands = exp(se.bands.logit)/(1+ exp(se.bands.logit))

plot(Wage$age,I(Wage$wage > 250), xlim=agelims,type="n", ylim=c(0,.2))
points(jitter(Wage$age), I((Wage$wage > 250) / 5), cex= 0.5,pch="|", col="darkgrey")
lines(age.grid,pfit,lwd=2,col="blue")
matlines(age.grid,se.bands,lwd=1,col="red",lty=3)


#use a step function use k fold cross validation to get best number of cut points
error.steps = rep(NA,9)
for (i in 2:10)
{
    Wage$age.cut = cut(Wage$age,i)
    lm.fit = glm(wage~age.cut,data=Wage)
    error.steps[i-1] = cv.glm(Wage,lm.fit,K=10)$delta[2]
}
plot(2:10,error.steps,xlab="Number of cuts",ylab="CV error",type="l",pch=20,lwd=2)

min_step = which.min(error.steps) + 1
lm.step = glm(wage~cut(age,min_step),data=Wage)
lm.step.pred = predict(lm.step,data.frame(age=age.grid))
plot(wage~age,data=Wage,col="darkgrey")
lines(age.grid,lm.step.pred,col="red",lwd=2)

#use GAMs with smoothing spline
library(gam)
gam1 = gam(wage~s(year,4)+s(age,5)+maritl,data=Wage)
gam2 = gam(wage~s(year,4)+s(age,5)+education,data=Wage)
gam3 = gam(wage~s(year,4)+s(age,5)+jobclass,data=Wage)
gam4 = gam(wage~s(year,4)+s(age,5)+jobclass + maritl + education,data=Wage)

gam5 = gam(wage~s(year,2)+s(age,5)+jobclass + maritl + education,data=Wage)
gam6 = gam(wage~year+s(age,5) +  education,data=Wage)

par(mfrow=c(1,3))
plot(gam6,se=TRUE,col = "blue")

#test F
ares = anova(gam1,gam2,gam3,gam4,gam5,gam6)



