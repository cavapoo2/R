set.seed(2)
#create some data
n=1000
p=20
x = rnorm(n*p,0,1)
e = rnorm(n*p,0,0.01)
xe = x+e
xem = matrix(xe,n,p) # 1 by 1000
B = rnorm(p) #20 by 1 
B[4] = 0
B[11] = 0
B[14]=0
B[19]=0

y = xem %*% B  # matrix multiply

#split data set 100 training , 900 test
rows = dim(y)[1]
train = sample(rows,100)
test = -train 

train_set_x = xem[train,]
test_set_x = xem[test,]
train_set_y = y[train]
test_set_y = y[test]
#perform best subset selection
library(leaps)
df = data.frame(train_set_y,train_set_x)
#fit.sub = regsubsets(y~x,data = data.frame(y=train_set_y,x=train_set_x),nvmax=20)
fit.sub = regsubsets(train_set_y~.,data = df,nvmax=p)
bs_summary = summary(fit.sub)
c_bs = coef(fit.sub,p)
a1 = which.min(bs_summary$cp)
a2 = which.min(bs_summary$bic)
a3 = which.max(bs_summary$adjr2)
val.errors =rep(NA,p)
#train set MSE
x_cols = colnames(xem,do.NULL = FALSE,prefix = "X")
for(i in 1:p)
{
    coefi = coef(fit.sub,id=i)
  #  pred = as.matrix(train_set_x[,x_cols %in% names(coefi)])
    pred = as.matrix(train_set_x[,x_cols %in% names(coefi)]) %*% coefi[names(coefi) %in% x_cols]
    val.errors[i] = mean((train_set_y - pred)^2)
}
plot(val.errors,ylab= "Training MSE", pch=19,type="b")

val.errors1 =rep(NA,p)
#test set MSE
for(i in 1:p)
{
    coefi = coef(fit.sub,id=i)
    print(coefi)
  #  pred = as.matrix(train_set_x[,x_cols %in% names(coefi)])
    pred = as.matrix(test_set_x[,x_cols %in% names(coefi)]) %*% coefi[names(coefi) %in% x_cols]
    val.errors1[i] = mean((test_set_y - pred)^2)
}

plot(val.errors1,ylab= "Test Set MSE", pch=19,type="b")

#get norm of the predicted coefficients
val.diffs = rep(NA,p)
val = 0
for(i in 1:p)
{
    coefi = coef(fit.sub,id=i)
    val.diffs[i] = sqrt(sum ((B[ x_cols %in% names(coefi)] - coefi)^2))
     
}

plot(val.diffs,ylab= "coeffs norms", pch=19,type="b")
