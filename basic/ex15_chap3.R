rm(list=ls())
library(MASS) # for Boston
crim = Boston$crim
dataw = Boston[,-1] #all columns except the first 
lm.fit = lm(crim~.,data=dataw) #fit all

#fit zn
lm.fit.zn = lm(crim~Boston$zn)

#fit indus
lm.fit.indus = lm(crim~Boston$indus)

Boston$chas = factor(Boston$chas,labels = c("N","Y"))

lm.fit.chas = lm(crim~Boston$chas)

lm.fit.nox = lm(crim~Boston$nox)

lm.fit.rm = lm(crim~Boston$rm)
lm.fit.age = lm(crim~Boston$age)
lm.fit.dis = lm(crim~Boston$dis)
lm.fit.rad = lm(crim~Boston$rad)
lm.fit.tax = lm(crim~Boston$tax)
lm.fit.ptratio = lm(crim~Boston$ptratio)
lm.fit.black = lm(crim~Boston$black)
lm.fit.lstat = lm(crim~Boston$lstat)
lm.fit.medv = lm(crim~Boston$medv)

#plot the multiple versus single regression coefficients
x = c(lm.fit.zn$coefficients[2],
      lm.fit.indus$coefficients[2],
      lm.fit.chas$coefficients[2],
      lm.fit.nox$coefficients[2],
      lm.fit.rm$coefficients[2],
      lm.fit.age$coefficients[2],
      lm.fit.dis$coefficients[2],
      lm.fit.rad$coefficients[2],
      lm.fit.tax$coefficients[2],
      lm.fit.ptratio$coefficients[2],
      lm.fit.black$coefficients[2],
      lm.fit.lstat$coefficients[2],
      lm.fit.medv$coefficients[2])
y = lm.fit$coefficients[2:length(lm.fit$coefficients)]

plot(x,y)


