set.seed(1)
library(ISLR)

hr.complete = hclust(dist(USArrests),method="complete")
plot(hr.complete)
c = cutree(hr.complete,3) # cut into 3 clusters
t1 = table(c)

hr.complete_sc = hclust(dist(scale(USArrests)),method="complete")
c1 = cutree(hr.complete_sc,3)
t2 = table(c1)

#quite different results from scaling. Data should be scaled,
#since measured data has different units
