set.seed(1)
x = rnorm(100)
y = 2*x+rnorm(100)
#perform regression without an intercept
lm.fit = lm(y~x+0)

#now the other way
lm.fit2 = lm(x~y+0)
