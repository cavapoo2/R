library(randomForest)
library(MASS)
set.seed(3)
train = sample(1:nrow(Boston),nrow(Boston)/2)
test = -train
trees = 3:500
p = 1:ncol(Boston)
#err = matrix(nrow=498,ncol = ncol(Boston) )
boston.test = Boston[-train,"medv"]
#note this takes long time
if (0)
{
    for (i in trees)
    {
        for (j in p)
        {
            #print(j)
            rf.boston = randomForest(medv~.,data=Boston,subset=train, mtry=j,ntree=i)
            pred = predict(rf.boston,newdata=Boston[-train,])
            #       print(pred)
            err[i,j] = mean((pred- boston.test)^2)
            #print( mean((pred- boston.test)^2))
        }
        # print(i)
    }
}
p1 = ncol(Boston) -1
p2 = p1/2
p3 = sqrt(p1)
pt = c(p1,p2,p3)
m=1
err = rep(NA,3)
for (j in pt)
{
    print(j)
    rf.boston = randomForest(medv~.,data=Boston,subset=train, mtry=j,ntree=500)
    pred = predict(rf.boston,newdata=Boston[-train,])
    err[m] = mean((pred- boston.test)^2)
    m = m + 1
}



