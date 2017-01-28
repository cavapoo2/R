#create some test data
x <- runif(100,-1,1)

slope <- 0.6
intercept <- 3
cube_term <- 0.7

y <- intercept + slope*x + cube_term *(x^3)
plot(x,y,type='p',col='red')

#create some noise
noise <- runif(100,-0.2,0.2)

yn <- y + noise

#try a linear fit
lm.fit <- lm(yn~x)
#also the cubic fit
lm.fit3 <- lm(yn~x+I(x^3))

r1 <- summary(lm.fit)
r3 <- summary(lm.fit3)

y1_train <- r1$coefficients[1] + r1$coefficients[2]*x + noise
y3_train <- r3$coefficients[1] + r3$coefficients[2]*x + r3$coefficients[3]*(x^3) + noise

#with the training data the cubic RSS is lower
rss_train1 <- sum((yn-y1_train)^2)
rss_train3 <- sum((yn-y3_train)^2)

lines(x,y1_train,type='p',pch=4,col='blue')
lines(x,y3_train,type='p',pch=2,col='green')

#create some test data
xt <- runif(100,-1,1)
yt <- intercept + slope*xt + cube_term *(xt^3) + noise
#mow use the predicted coeffs on the test data
y1_test <- r1$coefficients[1] + r1$coefficients[2]*xt + noise
y3_test <- r3$coefficients[1] + r3$coefficients[2]*xt + r3$coefficients[3]*(xt^3) + noise

#now check the RSS
rss_test1 <- sum((yt-y1_test)^2)
rss_test3 <- sum((yt-y3_test)^2)







