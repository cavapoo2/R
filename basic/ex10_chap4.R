library(ISLR)
library(MASS)#for lda
library(class) # for knn
train = (Weekly$Year < 2009)
Weekly.0910 = Weekly[!train, ]
glm.fit = glm(Direction ~ Lag2, data = Weekly, family = binomial, subset = train)
glm.probs = predict.glm(glm.fit,newdata= Weekly.0910, type = "response")
glm.pred = rep("Down", length(glm.probs))
glm.pred[glm.probs > 0.5] = "Up"
Direction.0910 = Weekly$Direction[!train]
conf_mat = table(glm.pred, Direction.0910)
#correct predictions
correct = ((conf_mat[1,1] + conf_mat[2,2])/sum(conf_mat)) * 100
error_rate = 100 - correct

#now using the lda

lda.fit = lda(Direction ~ Lag2, data = Weekly, family = binomial, subset = train)
lda.pred = predict(lda.fit,Weekly.0910)
lda.class = lda.pred$class
conf_mat_lda = table(lda.class,Direction.0910)
correct_lda = (conf_mat_lda[1,1] + conf_mat_lda[2,2])/sum(conf_mat_lda) * 100
error_rate_lda = 100 - correct_lda

#now using qda

qda.fit = qda(Direction ~ Lag2, data = Weekly, family = binomial, subset = train)
qda.pred = predict(qda.fit,Weekly.0910)
qda.class = qda.pred$class
conf_mat_qda = table(qda.class,Direction.0910)
correct_qda = (conf_mat_qda[1,1] + conf_mat_qda[2,2])/sum(conf_mat_qda) * 100
error_rate_qda = 100 - correct_qda

#now using knn

lag2_train = as.matrix(Weekly$Lag2[train])
lag2_test = as.matrix(Weekly$Lag2[!train])
dir_train = Weekly$Direction[train]
dir_test = Weekly$Direction[!train]
knn.pred = knn(lag2_train,lag2_test,dir_train,k=1)
conf_mat_knn = table(knn.pred,dir_test)
correct_knn = (conf_mat_knn[1,1] + conf_mat_knn[2,2]) / sum(conf_mat_knn) * 100
error_rate_knn = 100 - correct_knn
err2 = mean(knn.pred == dir_test)





