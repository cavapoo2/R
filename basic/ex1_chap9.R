#sketch the hyperplane
set.seed(9)

X1 = seq(1,100,1)
X2 = 1+ 3* X1
plot(X1,X2,ylim=c(  -300,300))

#some uniform random numbers
x = runif(100,0,100)
y = runif(100,-300,300)

dat = 1 + 3*x - y

#lines(x[1:2],y[1:2],col='blue',type='p')
#lines(x[5],y[5],col='red',type='p')
lines(x,y,col='red',type='p')

#another hyperplane
X3 = seq(1,100,1)
X4 = -(-2 + X3)/2
lines(X3,X4)

#anything left of the lines < 0 , anything right of the line is > 0 


