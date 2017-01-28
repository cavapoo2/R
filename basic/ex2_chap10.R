set.seed(5)
par(mfrow=c(1,3))
m = matrix(c(0,0.3,0.4,0.7,
             0.3,0,0.5,0.8,
             0.4,0.5,0,0.45,
             0.7,0.8,0.45,0,
             0.1,0.2,0,0.3,
             0.5,0.6,0.7,0),nrow=6)
d = dist(m)
hc.complete = hclust(d,method="complete")
plot(hc.complete,main="complete")
#now try single linkage
hc.single = hclust(d,method="single")
plot(hc.single,main="single")

#change position of dendrogram
#plot(hc.complete,labels=c(2,1,4,3),main="complete flip")    

