#this is to test the least squares line always passes
#through the mean of x and mean of y

#create some data
x <- runif(100,-1,1)
slope <- 0.6
intercept <- 3

y <- intercept + slope*x
plot(x,y,type='p',col='red')

#create some noise
noise <- runif(100,-0.2,0.2)

yn <- y + noise

#mean
my = mean(yn)
mx = mean(x)

lines(mx,my,type='p',pch=4,cex=4,col='black')

#try linear fit
lm.fit <- lm(yn~x)

r1 <- summary(lm.fit)

#plot the result
yr <- r1$coefficients[1] + r1$coefficients[2]*x

lines(x,yr,pch=4,col='purple')


