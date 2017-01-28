#load the csv 
set.seed(5)
college = read.csv("College.csv")
rws = dim(college)[1]
cols = dim(college)[2]
#split data
train = sample(rws,rws/2)
test = -train
college_train = college[train,]
college_test = college[test,]

#fit a linear model
dft = college_train[,-1]
lm.fit = lm(Apps~.,data=dft)

pred = predict(lm.fit,college_test[,-1])
#get mean sq error
mean_sq_lm = mean((pred - college_test$Apps)^2)

#now the ridge regression
library(glmnet)
x_train = model.matrix(Apps~.,data=college_train[,-1])
y_train = college_train$Apps
x_test = model.matrix(Apps~.,data=college_test[,-1])
y_test = college_test$Apps
grid = 10^seq(10,-2,length = 100)
ridge.fit = cv.glmnet(x_train,y_train,alpha=0,lambda=grid,thresh=1e-12)
bestlam = ridge.fit$lambda.min
ridge.pred = predict(ridge.fit,s=bestlam,newx = x_test)
mean_sq_ridge = mean((ridge.pred-y_test)^2)

#now try the lasso
lasso.fit = cv.glmnet(x_train,y_train,alpha=1,lambda=grid,thresh=1e-12)
bestlam1 = lasso.fit$lambda.min
lasso.pred = predict(lasso.fit,s=bestlam1,newx = x_test)
mean_sq_lasso = mean((lasso.pred-y_test)^2)
x_full = model.matrix(Apps~.,data=college[,-1])
y_full = college$Apps
lasso.full.fit = glmnet(x_full,y_full,alpha=0,lambda=grid,thresh=1e-12)
lasso.coef = predict(lasso.full.fit,type="coefficients",s=bestlam1)

#now try the PCR
library(pls)
pcr.fit = pcr(Apps~.,data=college[,-1],subset=train,scale=TRUE,validation="CV")
validationplot(pcr.fit,val.type="MSEP") # this gives  about 10 good choice
pcr.pred = predict(pcr.fit,college_test[,-1],ncomp=17)
mean_sq_pcr = mean((pcr.pred - y_test)^2)

#now try the PLS
pls.fit = plsr(Apps~.,data=college[,-1],subset=train,scale=TRUE,validation="CV")
validationplot(pls.fit,val.type="MSEP") # this gives  about 10 good choice
pls.pred = predict(pls.fit,college_test[,-1],ncomp=12)
mean_sq_pls = mean((pls.pred - y_test)^2)


#compare results
mean_apps = mean(college_test$Apps)
mean2_apps =  mean((college_test$Apps - mean_apps)^2)
R2_lm = mean_sq_lm / mean2_apps
R2_ridge = mean_sq_ridge / mean2_apps
R2_lasso = mean_sq_lasso / mean2_apps
R2_PCR = mean_sq_pcr / mean2_apps
R2_PLS = mean_sq_pls / mean2_apps

barplot(c(R2_lm,R2_ridge,R2_lasso,R2_PCR,R2_PLS),col="blue",names.arg=c("R2_lm","R2_ridge","R2_lasso","R2_PCR","R2_PLS"), main="Test R sq")
