library(MASS)
library(boot)
mu = mean(Boston$medv)
ste = sd(Boston$medv) / sqrt(dim(Boston)[1]) 
#now estimate ste (standard error ) using bootstrap

boot.function = function(data,index)
{
    mu = mean(data[index])
    return (mu)

}

boot.median = function(data,index)
{
    mi = median(data[index])
    return (mi)
}

boot.mean = boot(Boston$medv,boot.function,1000)

#confidence interval
cia = t.test(Boston$medv)
#another way for confidence interval
cib = c(mu - 2*ste,mu + 2 * ste)

#median value
mi = median(Boston$medv)

mi.boot = boot(Boston$medv,boot.median,1000)

#quantile

q1 = quantile(Boston$medv,c(0.1))

boot.quant = function(data,index)
{
    q = quantile(data[index],0.1)
    return (q)
}

q.boot = boot(Boston$medv,boot.quant,1000)
