#this will generate a linear plot
#i is the intercept
#b is the coefficient
#x is the data vector
#noi is the noise vector to be added to the linear result
#we could pack this into a data.frame like df = data.frame(y,x)
#then call a linear regression like lm(y~x)
gen_linear <- function(i,b,x,noi)
{
    y <- i + b*x 
    y <- y + noi
}
