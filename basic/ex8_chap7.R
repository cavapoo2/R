library(ISLR)
set.seed(3)
library(boot)
#test mpg vs displacement
its=15
deltas = rep(NA,its)
for (i in 1:its)
{
    fit = glm(mpg~poly(displacement,i), data = Auto)
    deltas[i] = cv.glm(Auto,fit,K=10)$delta[2]

}
plot(deltas)
mn = which.min(deltas)

#step functions
deltas.step = rep(NA,its)

for (i in 2:its)
{
    Auto$dis.cut = cut(Auto$displacement,i)
    step.fit = glm(mpg~dis.cut,data=Auto)
    deltas.step[i-1] = cv.glm(Auto,step.fit,K=10)$delta[2]
}
plot(deltas.step)

#splines
library(splines)
deltas.spline = rep(NA,its)
for( i in 2:its)
{
    fit = glm(mpg~ns(displacement,df=i),data=Auto)
    deltas.spline[i-1] = cv.glm(Auto,fit,K=10)$delta[2]

}
plot(deltas.spline)

#GAMs
library(gam)

fit = gam(mpg ~ s(displacement, 4) + s(horsepower, 4), data = Auto)
summary(fit)

