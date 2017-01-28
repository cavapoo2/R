library(ISLR)
#using student data
set.seed(1)
glm.fit = glm(default~income+balance,data = Default,family = binomial)
len = dim(Default)[1]
train = sample(len,len/2) # split data into training
glm.fit.train = glm(default~income+balance,data=Default,family=binomial,subset = train)
glm.probs = predict(glm.fit.train,newdata=Default[-train,],type="response")
glm.pred = rep("No",length(glm.probs))
glm.pred[glm.probs > 0.5] = "Yes"
conf_mat = table(glm.pred,Default$default[-train])

val_error = mean(glm.pred != Default[-train,]$default) * 100

train1= sample(len,len/2) # split data into training
train2= sample(len,len/2) # split data into training
train3= sample(len,len/2) # split data into training

glm.fit.train1 = glm(default~income+balance,data=Default,family=binomial,subset = train1)
glm.probs1 = predict(glm.fit.train1,newdata=Default[-train1,],type="response")
glm.pred1 = rep("No",length(glm.probs1))
glm.pred1[glm.probs1 > 0.5] = "Yes"

val_error1 = mean(glm.pred1 != Default[-train1,]$default) * 100

glm.fit.train2 = glm(default~income+balance,data=Default,family=binomial,subset = train2)
glm.probs2 = predict(glm.fit.train2,newdata=Default[-train2,],type="response")
glm.pred2 = rep("No",length(glm.probs2))
glm.pred2[glm.probs2 > 0.5] = "Yes"

val_error2 = mean(glm.pred2 != Default[-train2,]$default) * 100

glm.fit.train3 = glm(default~income+balance,data=Default,family=binomial,subset = train3)
glm.probs3 = predict(glm.fit.train3,newdata=Default[-train3,],type="response")
glm.pred3 = rep("No",length(glm.probs3))
glm.pred3[glm.probs3 > 0.5] = "Yes"

val_error3 = mean(glm.pred3 != Default[-train3,]$default) * 100


#now use student as dummy variable
train4 = sample(len,len/2)
glm.fit.train4 = glm(default~income+balance+student,data=Default,family=binomial,subset = train4)
glm.probs4 = predict(glm.fit.train4,newdata=Default[-train4,],type="response")
glm.pred4 = rep("No",length(glm.probs4))
glm.pred4[glm.probs4 > 0.5] = "Yes"

val_error4 = mean(glm.pred4 != Default[-train4,]$default) * 100









