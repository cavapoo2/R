Power <- function()
{
    p = 2^3
    print(p)
}

Power2 <- function(x,a)
{
    print(x^a)
}

Power3 <- function(x,a)
{
    result = x^a
    return (result)
}

PlotPower <- function(x,a)
{
    y = Power3(x,a)
    plot(x,y)
}

#x = c(1:10)
#y = Power3(x,2)
#par(mfrow=c(2,1))
#plot(x,y,xlab="x",ylab="x^2",main="plot of x^2")
#plot(log(x),log(y),xlab="log(x)",ylab="log(x^2)",main="plot of x^2")
