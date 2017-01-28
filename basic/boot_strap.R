#boot_strap <- function()
#par(mfrow=c(1,2))
#{
#    n = 1:100000 
#    prob_not_in  = (1 - (1/n))^n
#    prob_in = 1 - prob_not_in
#    plot(n,prob_in)
#}

#pr = function(n) return(1 - (1 - 1/n)^n)
#x = 1:1e+05
#plot(x, pr(x))

store=rep(NA,10000)
for(i in 1:10000)
    store[i]=sum(sample(1:100,rep=TRUE) == 99) > 0
m = mean(store)



