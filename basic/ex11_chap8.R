set.seed(6)
library(ISLR)
rm(list=ls())
Purchase = ifelse(Caravan$Purchase == "Yes",1,0)
Caravan$Purchase = Purchase
Cara_train = Caravan[1:1000,]
nr = nrow(Caravan)
Cara_test = Caravan[1001:nr,]

#perform boosting
library(gbm)
boost.cara = gbm(Purchase~.-(PVRAAUT+AVRAAUT),data=Cara_train,distribution="bernoulli",n.trees=1000,shrinkage=0.01)
#predict
pred.test = predict(boost.cara,newdata=Cara_test,n.trees=1000,type="response")
pyes = pred.test > 0.2
cm = table(pyes,Cara_test$Purchase==1)

#fraction predicted true
tv = cm[2,2]/(cm[2,2]+cm[1,2])

#now try knn
library(class)
knn.pred = knn(Cara_train,Cara_test,Cara_train$Purchase,k=3)
knn_cm = table(knn.pred,Cara_test$Purchase)
tkv = knn_cm[2,2]/(knn_cm[2,2]+knn_cm[2,1])

#now try logisitc regression
glm.fit = glm(Purchase~.,data=Cara_train,family=binomial)

glm.pred = predict(glm.fit,Cara_test,type="response")
glm_20 = glm.pred > 0.2
glm_cm = table(glm_20,Cara_test$Purchase==1)
tglm = glm_cm[2,2]/(glm_cm[2,2]+glm_cm[1,2])

