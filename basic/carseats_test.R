#question 10 from chap 3 of ISLR book
library(ISLR)
#fit linear regression model to predict Sales using Price
#Urban and US
lm.fit = lm(Carseats$Sales ~ Carseats$Price + Carseats$Urban + Carseats$US, data = Carseats)

#fit a smaller model

lm.fit2 = lm(Carseats$Sales ~ Carseats$Price + Carseats$US, data = Carseats)

#confidence interval

pr1 <- confint(lm.fit2)

#check for outliers
#the leverage plot that has values > 2 and < -2 shows
#some outliers
plot(lm.fit2)

#plot(predict(lm.fit2),rstudent(lm.fit2))   
