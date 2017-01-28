set.seed(54)
#library(ISLR)
library(e1071)
#read csv
Auto = read.csv("Auto.csv",na.strings=c("?"))

na.rows = apply(Auto,1,function(x){any(is.na(x))})
Auto = Auto[!na.rows,]

#clean the data from ? in rows
#ix = rowSums(Auto == "?", na.rm= TRUE) > 0L
#Auto = droplevels(Auto[!ix,]) # remove and ? in levels

y = ifelse(Auto$mpg > median(Auto$mpg),1,0)
Auto$mpg_level = as.factor(y)



plot(Auto$mpg[y==1],col="red",ylim=c(0,45))
points(Auto$mpg[y==0],col="blue")
#fit a support vector
tune.lin = tune(svm,mpg_level~. ,data=Auto,kernel="linear",ranges=list(cost=c(0.01,0.1,1,5,10,100)))

sm_lin = summary(tune.lin)
sm_lin_best = tune.lin$best.model

#same again but with polynomial
tune.poly = tune(svm,mpg_level~. ,data=Auto,kernel="polynomial",ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100)),degree=c(2,3,4))

sm_poly = summary(tune.poly)
sm_poly_best = tune.poly$best.model

#same again but with radial 
tune.rad = tune(svm,mpg_level~. ,data=Auto,kernel="radial",ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100)),gamma=c(0.01,0.1,1,5,10,100))

sm_rad = summary(tune.rad)
sm_rad_best = tune.rad$best.model

#plot the linear results
best.lin = svm(mpg_level~. ,data=Auto,kernel = "linear",cost=10)
plotpairs = function(fit,type) {
    for(name in names(Auto)[!(names(Auto) %in% c("mpg","mpg_level","name"))]){
        plot(fit,Auto,as.formula(paste("mpg~",name,sep="")),main=type)
    }
}
#plot(best.lin,Auto,as.formula(paste("mpg~","cylinders",sep="")))
#plot(best.lin,Auto,as.formula(paste("mpg~","displacement",sep="")))
#plot(best.lin,Auto,as.formula(paste("mpg~","horsepower",sep="")))


plotpairs(best.lin,"linear")

#now try polynomial
best.poly = svm(mpg_level~.,data=Auto,kernel="polynomial",cost=100,degree=3)
plotpairs(best.poly,"polynomial")
#now try radial
best.rad = svm(mpg_level~.,data=Auto,kernel="radial",cost=100,gamma=0.01)

plotpairs(best.rad,"radial")



