set.seed(1)
Control <- matrix(rnorm(50 * 1000), ncol = 50)
Treatment <- matrix(rnorm(50 * 1000), ncol = 50)
X <- cbind(Control, Treatment)
X[1, ] <- seq(-18, 18 - .36, .36) # linear trend in one dimension
pr.out <- prcomp(scale(X))
i1 = summary(pr.out)$importance[, 1]
#above gives about 10% var in first principal component
#now if i add another row
X = rbind(X,c(rep(10,50),rep(0,50)))
pr.out2 = prcomp(scale(X))
i2 = summary(pr.out2)$importance[,1]
#can see above just with this extra row added the var increase in firt principal component


