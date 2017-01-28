set.seed(3)
library(ISLR)
#par(mfrow=c(1,2))
#plot(Wage$maritl,Wage$wage)
#plot(Wage$jobclass,Wage$wage)
#fit a lm
fit.lm.maritl = lm(wage~maritl,data=Wage)
fit.lm.job = lm(wage~jobclass,data=Wage)
fit.lm.job.mat = lm(wage~jobclass+maritl,data=Wage)
dm = deviance(fit.lm.maritl)
dj = deviance(fit.lm.job)
djm = deviance(fit.lm.job.mat)
an = anova(fit.lm.maritl,fit.lm.job,fit.lm.job.mat) 

#try some GAMs
library(gam)
gam1 = gam(wage~s(year,4)+s(age,5)+maritl,data=Wage)
gam0 = gam(wage~age,data=Wage)
gam2 = gam(wage~s(year,4)+s(age,5)+education,data=Wage)
gam3 = gam(wage~s(year,4)+s(age,5)+jobclass,data=Wage)
gam4 = gam(wage~s(year,4)+s(age,5)+jobclass + maritl + education,data=Wage)

gam5 = gam(wage~s(year,2)+s(age,5)+jobclass + maritl + education,data=Wage)
gam6 = gam(wage~year+s(age,5) +  education,data=Wage)

par(mfrow=c(1,3))
plot(gam6,se=TRUE,col = "blue")

#test F
ares = anova(gam1,gam2,gam3,gam4,gam5,gam6)

#try predict
#using preds on the test data
preds = predict(gam6,newdata=Wage)
npreds = as.numeric(preds)
yt = Wage$wage
mse = mean((npreds - yt)^2)

plot(1:length(yt),yt,type='p',col="blue")
lines(1:length(yt),npreds,type='p',col="red")


