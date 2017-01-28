set.seed(1)
y = rnorm(100)
x = rnorm(100)
y = x - (2 * (x^2)) + rnorm(100)

#n is 100, p is 2
plot(x,y)
library(boot)

#first order
df1 = data.frame(y,x)   

res = rep(0,4)
for(i in 1:4)
{
    glm.fiti = glm(y~poly(x,i),data=df1)
    res[i] = cv.glm(df1,glm.fiti)$delta[1]
}

#using another random seed
set.seed(100)
y = rnorm(100)
x = rnorm(100)
y = x - (2 * (x^2)) + rnorm(100)

df1 = data.frame(y,x)
res2 = rep(0,4)
for(i in 1:4)
{
    glm.fiti = glm(y~poly(x,i),data=df1)
    res2[i] = cv.glm(df1,glm.fiti)$delta[1]
}

#note glm uses least squares to it the data. 




