library(ISLR) # for auto
library(MASS) # for lda
library(class) # for knn
mpg01 = rep(0,length(Auto$mpg))
mpg01[Auto$mpg > median(Auto$mpg)]  = 1
Auto_new = data.frame(Auto,mpg01)

#use cor to get correclations, then use boxplot to see
#the relation between mpg01 and those ith high corr

#split the data into training and test

train = (Auto_new$year %% 2 == 0)
Auto_test = Auto_new[!train,]
Auto_train = Auto_new[train,]
mpg01_test = Auto_new$mpg01[!train]


#use cylinders,displacement,horsepower,weight as predictors
lda.fit = lda(mpg01~cylinders+displacement+horsepower+weight,data=Auto_new,  subset = train)
lda.pred = predict(lda.fit,Auto_test)
lda.class = lda.pred$class
conf_mat_lda = table(lda.class,mpg01_test)
correct_lda = mean(lda.class == mpg01_test)
error_lda = (1 - correct_lda) * 100

#now try qda
qda.fit = qda(mpg01~cylinders+displacement+horsepower+weight,data=Auto_new,  subset = train)
qda.pred = predict(qda.fit,Auto_test)
qda.class = qda.pred$class
conf_mat_qda = table(qda.class,mpg01_test)
correct_qda = mean(qda.class == mpg01_test)
error_qda = (1 - correct_qda) * 100

#now try logistic regression

glm.fit = glm(mpg01~cylinders+displacement+horsepower+weight,data=Auto_new,  subset = train)
glm.probs = predict.glm(glm.fit,Auto_test)
glm.pred = rep(0,length(glm.probs))
glm.pred[glm.probs > 0.5] = 1
conf_mat_glm = table(glm.pred,mpg01_test)
correct_glm = mean(glm.pred == mpg01_test)
error_glm = (1 - correct_glm) * 100

#now try knn
knn_train = as.matrix(cbind(Auto_new$cylinders[train],Auto_new$displacement[train],Auto_new$horsepower[train],Auto_new$weight[train]))
knn_test =  as.matrix(cbind(Auto_new$cylinders[!train],Auto_new$displacement[!train],Auto_new$horsepower[!train],Auto_new$weight[!train]))

mpg01_train = Auto_new$mpg01[train]
knn.pred = knn(knn_train,knn_test,mpg01_train,k=3)
conf_mat_knn = table(knn.pred,mpg01_test)
correct_knn = mean(knn.pred == mpg01_test)
error_knn = (1 - correct_knn) * 100






