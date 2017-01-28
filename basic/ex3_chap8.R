#plot gini and cross entropy

p = seq(0,1,0.01)
p1 = p
p2 = 1 - p
#gini
G = p1*(1-p1) + p2*(1-p2)
plot(p,G,ylim=c(0,1))

#cross entropy
D = -p1*log(p1) - p2*log(p2)
E = 1 - pmax(p1,p2)
lines(p,D,col="red")
lines(p,E,col="blue")
