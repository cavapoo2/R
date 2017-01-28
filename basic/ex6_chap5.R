library(ISLR)
library(boot)
set.seed(2)

boot.function = function(data,index)
{
    glm.fit = glm(default~income+balance,data=data,family = binomial,subset=index)
    return (coef(glm.fit))
}
boot.res = boot(Default,boot.function,1000)
len = dim(Default)[1]
test = sample(len,len/2)

glm.fit = glm(default~income+balance,data=Default,family = binomial)
s = summary(glm.fit)


