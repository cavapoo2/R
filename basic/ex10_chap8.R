set.seed(3)
library(ISLR)
na.rows = apply(Hitters,1,function(x){any(is.na(x))})

Hitters_new = Hitters[!na.rows,]
Hitters_na = Hitters[na.rows,]
log_sal = log(Hitters_new$Salary)
Hitter_log = data.frame(Hitters_new[,! names(Hitters) %in% "Salary"],log_sal)
ntrain = 200
ntest = nrow(Hitter_log) - ntrain
train = sample(1:nrow(Hitter_log),ntrain)
test = -train

#perform boosting
library(gbm)
lam = seq(0,0.1,0.01)
i=1;
err_test = rep(NA,length(lam))
err_train = rep(NA,length(lam))
for (sh in lam)
{
    boost.boston = gbm(log_sal~.,data=Hitter_log[train,],distribution="gaussian",n.trees=1000,interaction.depth=4,shrinkage=sh,verbose=F)
pred.train = predict(boost.boston,newdata=Hitter_log[train,],n.trees=1000)
pred.test = predict(boost.boston,newdata=Hitter_log[test,],n.trees=1000)
    err_test[i] = mean((pred.test - Hitter_log[test,"log_sal"])^2)
    err_train[i] = mean((pred.train - Hitter_log[train,"log_sal"])^2)
    i = i+1
    print(i)
}

plot(lam,err_train,col="red")
lines(lam,err_test,col="blue")

#compare results to lm
lm.boston = lm(log_sal~.,data=Hitter_log[train,])
lm.pred = predict(lm.boston,Hitter_log[test,]) 
error_lm = mean((lm.pred - Hitter_log[test,"log_sal"])^2)

#now use glmnet - generalised linear model with lasso
library(glmnet)
x = model.matrix(log_sal~.,data = Hitter_log[train,])
y = Hitter_log[train,"log_sal"]
x_test = model.matrix(log_sal~., data=Hitter_log[test,])
lasso.fit = glmnet(x,y,alpha=1)
lasso.pred = predict(lasso.fit, s = 0.01,newx = x_test)
error_lasso = mean((lasso.pred - Hitter_log[test,"log_sal"])^2)
#see which variables are 
boost.final = gbm(log_sal~.,data=Hitter_log[train,],distribution="gaussian",n.trees=1000,interaction.depth=4,shrinkage=err_test[which.min(err_test)],verbose=F)
suma = summary(boost.final)
#now use bagging
library(randomForest)
rf.fit = randomForest(log_sal~.,data=Hitter_log,subset=train,mtry=ncol(Hitter_log)-1,importance=TRUE)
pred.rf = predict(rf.fit,newdata=Hitter_log[test,])
err_rf = mean((pred.rf - Hitter_log[test,"log_sal"])^2)
