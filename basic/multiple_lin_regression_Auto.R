#load the Car_Data data
Car_Data = read.csv("Auto.csv",header=T,na.strings="?")
#attach(Car_Data)
cols = dim(Car_Data)[2]
#plot scatter plot
#pairs(Car_Data[,1:(cols-1)])

dat = Car_Data[,1:(cols-1)]
#get correlation matrix
cor_res = cor(dat)

#perform multiple linear regression on all except name column
lm.fit = lm(Car_Data$mpg~.-Car_Data$name,data=Car_Data)

#plot(lm.fit)

#try interaction term mpg and weight
lm.fit2 = lm(Car_Data$mpg~Car_Data$horsepower*Car_Data$acceleration,data=Car_Data)

#try without intercation term
lm.fit3 = lm(Car_Data$mpg~Car_Data$horsepower+Car_Data$acceleration,data=Car_Data)

#plot the results

yah <- lm.fit3$coefficients[1] + lm.fit3$coefficients[2]*Car_Data$horsepower + lm.fit3$coefficients[3] * Car_Data$acceleration

yah[is.na(yah)] <- 0
#persp(horsepower,acceleration,yah,col='blue')
#plot(lm.fit2)
cdhp = Car_Data$horsepower
cdhp[is.na(cdhp)] <- 0
cdac = Car_Data$acceleration
cdac[is.na(cdhp)] <- 0
#qplot(cdac,cdhp,yah,geom='bind2d')
plot(lm.fit2)

