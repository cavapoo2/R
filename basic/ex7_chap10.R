set.seed(8)
library(ISLR)
dat = scale(USArrests)
c = cor(dat) # correlation
e = 1 - c # supposed proportional to squared euclidena distance
dfr = data.frame(dat)

efr = data.frame(e)
sqr_euclid = function(v1,v2)
{
    return (sum((v1 - v2)^2))
}

total_sqr_euclid = function(data)
{

    cols_to_iterate = ncol(data)-1
    s = matrix(NA,nrow=cols_to_iterate,ncol = cols_to_iterate)
    for(i in 1:cols_to_iterate)
    {
        for (j in (i+1):(cols_to_iterate+1))
        {
           d = sqr_euclid(data[,i],data[,j])
           s[i,j-1]= d

        }
    }
    return (s)
        
}

s = total_sqr_euclid(dfr) / 100
#compare s with e and you can see there is a proportionality

#now sdev

pr.out = prcomp(USArrests, scale=TRUE)
pve = pr.out$sdev^2 / sum(pr.out$sdev^2)

row = nrow(dat)
ns = 0;
ds = 0;
a1=0
a2=0
a3=0
a4=0
loads = pr.out$rotation
for (i in 1:row)
{
    a1 = a1 + sum(dat[i,] * loads[,1])^2
    a2 = a2 + sum(dat[i,] * loads[,2])^2
    a3 = a3 + sum(dat[i,] * loads[,3])^2
    a4 = a4 + sum(dat[i,] * loads[,4])^2

    #ns = ns + sum(dat[i,] * pr.out$x[i,])^2
    ds = ds + sum(dat[i,]^2)
    
}
#another way
res = rep(NA,4)
for (i in 1:4)
{
    t1 = t(dat)*loads[,i]
    t1 = apply(t(t1),1,sum) # sum along cols 
    t1 = sum(t1^2) # sqr each row and sum
    res[i] = t1
}
t1 = t(dat)*loads[,1]
t1 = t(t1)
t2 = apply(t1,1,sum)
t3 = sum(t2^2)

ns = c(a1,a2,a3,a4)
pve2 =ns/ds 

dmean = apply(USArrests,2,mean)
dsdev = sqrt(apply(USArrests,2,var))
dsc = sweep(USArrests,MARGIN=2,dmean,"-")
dsc = sweep(dsc,MARGIN=2,dsdev,"/")


