
set.seed(5)
library(MASS)
plot(Boston$dis,Boston$nox)

lm.fit = lm(nox~poly(dis,3),data=Boston)
dislim = range(Boston$dis)
dis.grid = seq(from = dislim[1], to = dislim[2],by = 0.1)
lm.pred = predict(lm.fit,list(dis=dis.grid))
plot(nox~dis,data=Boston,col="darkgrey")
lines(dis.grid,lm.pred,col = "red",lwd=2)
#residuals sum of squares
all.rss = rep(NA,10)
for(i in 1:10)
{
    lm.fit = lm(nox~poly(dis,i), data= Boston)
    all.rss[i] = sum((lm.fit$residuals)^2)
}
plot(all.rss)

#cross validation for optimal degree of polynomial
library(boot)
cv.opt = rep(NA,10)
for (i in 1:10)
{
    lm.fit = glm(nox~poly(dis,i),data=Boston)
    cv.opt[i] = cv.glm(Boston,lm.fit,K=10)$delta[2]
}
plot(1:10,cv.opt)

#use bs spline
library(splines)
#sp.fit = lm(nox ~ bs(dis,df=4,knots = c(3,7,11)), data = Boston)
sp.fit = lm(nox ~ bs(dis,knots = c(3,4,7,11)), data = Boston)
sp.pred = predict(sp.fit, list(dis=dis.grid))
plot(nox~dis,data=Boston,col="darkgrey")
lines(dis.grid,sp.pred,col="red",lwd=2)

#fit regression spline for range of degrees of freedom. report RSS
spl.rss = rep(NA,13)

for(i in 3:15)
{
    spl.fit = lm(nox ~ bs(dis,df=i),data=Boston)
    spl.rss[i-2] = sum((spl.fit$residuals)^2)
}
plot(spl.rss)
best_df = which.min(spl.rss) + 2

#same again but with cross validation
cv.spl = rep(NA,13)
for(i in 3:15)
{
    spl.fit = glm(nox ~ bs(dis,df=i),data=Boston)
    cv.spl[i-2] = cv.glm(Boston,spl.fit,K=10)$delta[2]
}
plot(cv.spl)
best_df.cv = which.min(cv.spl) + 2
