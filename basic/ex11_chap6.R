library(MASS)
library(leaps)
set.seed(4)
#split data into test and training
rows = dim(Boston)[1]
k=10
folds = sample(rep(1:k,length=nrow(Boston)))
p = ncol(Boston) -1
#perform best subset on Boston data
#

predict.regsubsets = function(object, newdata, id, ...) {
    form = as.formula(object$call[[2]])
    mat = model.matrix(form, newdata)
    coefi = coef(object, id = id)
#    print(coefi)
#this is the returned value
   # print(dim(mat[,names(coefi)] %*% coefi))
#note mat here can be 50 by n. coefi is n by 1 (can be written as 1 by n)
#result is 50 by 1
    mat[, names(coefi)] %*% coefi
}
cv.errors = matrix(NA,k,p)
for (i in 1:k)
{
    fit_sub = regsubsets(crim~.,data = Boston[folds != i,],nvmax=p)
    for (j in 1:p)
    {
        pred = predict(fit_sub,Boston[folds == i,],id = j)
       # print(dim(pred))

        cv.errors[i,j]  = mean((Boston$crim[folds == i] - pred)^2)     
    }

}
rmse.cv = min(sqrt(apply(cv.errors,2,mean))) # mean of columns

plot(rmse.cv,pch=19,type="b")

#perform the rmse with the lasso
library(glmnet)
x = model.matrix(crim~.-1,data=Boston)
y = Boston$crim
cv.lasso = cv.glmnet(x,y,type.measure = "mse", alpha=1)
#plot(cv.lasso)
#get rmse
rmse.lasso = sqrt(cv.lasso$cvm[cv.lasso$lambda == cv.lasso$lambda.1se])
 
#now perform the ridge regression
cv.ridge = cv.glmnet(x,y,type.measure = "mse", alpha=0)
#plot(cv.ridge)
#get rmse
rmse.ridge = sqrt(cv.ridge$cvm[cv.ridge$lambda == cv.ridge$lambda.1se])

#pcr
library(pls)
pcr.fit = pcr(crim~.,data=Boston,scale=TRUE,validation="CV")
rmse.pcr = min(RMSEP(pcr.fit)$val)
 
