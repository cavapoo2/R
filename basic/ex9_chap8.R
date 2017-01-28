set.seed(7)
library(ISLR)
library(tree)
ntrain = 800;
ntest = nrow(OJ) - ntrain
train = sample(1:nrow(OJ), ntrain)
test = -train

OJ_train = OJ[train,]
OJ_test = OJ[test,]

#fit tree to OJ with Purchase as the response
tree.OJ = tree(Purchase~.,OJ_train)
plot(tree.OJ)
text(tree.OJ,pretty=0)

pred.OJ = predict(tree.OJ,OJ_test,type="class")
tab.OJ = table(pred.OJ,OJ_test$Purchase)

err.OJ =  (tab.OJ[1,2] + tab.OJ[2,1])  / sum(tab.OJ)
cv.OJ = cv.tree(tree.OJ)
plot(cv.OJ$size,cv.OJ$dev,type='b') # 5 is min
#now produce pruned tree
prune.OJ = prune.tree(tree.OJ,best=5)

#compare test error rates
pred.OJ.pruned = predict(prune.OJ,OJ_test, type="class")
tab.OJ.pruned = table(pred.OJ.pruned,OJ_test$Purchase)
err.OJ.pruned = (tab.OJ.pruned[1,2] + tab.OJ.pruned[2,1]) / sum(tab.OJ.pruned)

