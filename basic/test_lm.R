#create some random data
x <- runif(100,-1,1)
#chose an intercept and slope
slope <- 0.5
intercept <- 2

y <- intercept + slope*x

#plot this
plot(x,y,type='p',col='red')

#create a noise term
noise <- runif(100,-0.2,0.2)
#add this to the linear output
yn <- y + noise
cl <- rainbow(1)
lines(x,yn,type ='p',col='blue')

#now use lm to fit the noisy data to estimate intercept and coefficient
lm.fit = lm(yn~x)

#now try a quadtratic regression fit
lm.fit2 = lm(yn~x+I(x^2))

#also a cubic
lm.fit3 = lm(yn~x+I(x^3))
#check results
r1 <- summary(lm.fit)
r2 <- summary(lm.fit2)
r3 <- summary(lm.fit3)

#plot new line with the fitted values
yf1 <- r1$coefficients[1] + r1$coefficients[2]*x 
yf1 <- yf1 + noise
#this will plot with cross hairs
lines(x,yf1,type='p',pch=4,col='black')

yf3 <- r3$coefficients[1] + r3$coefficients[2]*x +r3$coefficients[3]*x
yf3 <- yf3 + noise

lines(x,yf3,type='p',pch=2,col='purple')

#create some test data and use our original slope and intercept

xt <- runif(100,-1,1)
yt <- intercept + slope*xt
yt <- yt + noise

yt1 <- r1$coefficients[1] + r1$coefficients[2] * xt
yt1 <- yt1 + noise
#0 is square
lines(xt,yt1,type='p',pch=0,col='red')

yt3 <- r3$coefficients[1] + r3$coefficients[2] * xt  + r3$coefficients[3]*x
yt3 <- yt3 + noise

#8 is star
lines(xt,yt3,type='p',pch=8,col='black')

#compute the RSS explicitly
#in this case we can see the cubic has better RSS with the training data
#but not so with the test data
res1 = sum((yn - yf1)^2)
res2 = sum((yn - yf3)^2)
res3 = sum((yt - yt1)^2)
res4 = sum((yt - yt3)^2)





