set.seed(5)
X = cbind(c(1,1,0,5,6,4),c(4,3,4,1,2,0))

labels = sample(2,nrow(X),replace=T)

c1 = c(mean(X[labels==1,1]),mean(X[labels==1,2]))
c2 = c(mean(X[labels==2,1]),mean(X[labels==2,2]))


euclid = function(a,b) {
    return (sqrt(c(a[1]-b[1])^2 + (a[2]-b[2])^2)) 
}
labs = function(x,c_1,c_2)
{
    c1 = c_1
    c2= c_2
    print(c1)
    print(c2)
    labs = rep(NA,nrow(x)) 
    for (j in 1:10)
    {
            for(i in 1:nrow(x))
            {
                if( euclid(x[i,],c1) < euclid(x[i,],c2))
                {
                    labs[i] = 1
                }
                else
                {
                    labs[i] = 2
                }
            }

        #update centroids
        c1 = c(mean(X[labs==1,1]),mean(X[labs==1,2]))
        c2 = c(mean(X[labs==2,1]),mean(X[labs==2,2]))
        print(labs)
        print(c1)
        print(c2)    
    }

    
    return (labs)
}

labels_n = labs(X,c1,c2)

plot(X[,1],X[,2], col=(labels_n+1),pch=4,cex=2)
points(c1[1],c1[2],col="blue",pch=3)
points(c2[1],c2[2],col="black",pch=3)


