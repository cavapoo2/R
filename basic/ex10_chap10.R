set.seed(6)

dat = matrix(rnorm(20*3*50,mean=0,sd=0.001),ncol=50)

dat[1:20,2] =0.5 
dat[21:40,1]=1
dat[21:40,2]=1
dat[41:60,1] =0.5 

labs = c(rep(1,20),rep(2,20),rep(3,20)) 

Cols = function(vec)
{
    cols = rainbow(length(vec))
    return (cols[as.numeric(as.factor(vec))])
}

pr.out = prcomp(dat)
plot(pr.out$x[,1:2],col=1:3,pch=19)

#kmeans cluster
km.out = kmeans(dat,3,nstart=20)
tb = table(km.out$cluster,labs)

#try k =2
km.out2 = kmeans(dat,2,nstart=20)
tb2 = table(km.out2$cluster,labs)

#try k =4
km.out4 = kmeans(dat,4,nstart=20)
tb4 = table(km.out4$cluster,labs)

#use the pr out data

km.pr = kmeans(pr.out$x[,1:2],3,nstart=20)

tbpr = table(km.pr$cluster,labs)

#note the labels are defined randomly here. The kmeans
#decides which label, so maybe have to adapt to that
#e.g instead of 1,2,3 might need to do 3,2,1 or 1,3,2.
#hence we should create label after keans result. 
#we know our principal component has identified 3 
#regions here.
