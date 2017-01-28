library(ISLR)

glm.fit = glm(Direction~Lag1+Lag2,data=Weekly,family=binomial)
#all but the first observation

glm.fit2 = glm(Direction~Lag1+Lag2,data=Weekly[-1,],family=binomial)

#first row
frow = Weekly[1,]

glm.probs = predict(glm.fit2,newdata=frow,type="response")

glm.pred = rep("Down",length(glm.probs))
glm.pred[glm.probs > 0.5] = "Up"
#hence above glm.pred is classified as up, but true observation is down

rows = dim(Weekly)[1]
result = rep(0,rows)
for (i in 1:rows)
{
    glm.fit_i = glm(Direction~Lag1+Lag2,data=Weekly[-i,],family=binomial)
    i_row = Weekly[i,]
    glm.probs_i = predict(glm.fit_i,newdata=i_row,type="response")
    glm.pred_i=rep("Down",length(glm.probs_i))
    glm.pred_i[glm.probs_i > 0.5] = "Up"
    if (i_row$Direction != glm.pred_i)
        result[i]=1
}
error_rate = mean(result) * 100
