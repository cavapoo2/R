set.seed(6)
#investigate a non linear boundary - this is circle
#(x - h)^2  + (y - k)^2  = r^2
r = 2;
c1 = -1
c2 = 2
p = 100
pis = seq(0,2*pi,pi/p)

x = r* cos(pis) +c1
y = r * sin(pis) + c2

plot(x,y,xlim=c(-5,2),ylim=c(-1,5))

xp = c(-1, 0, 1) 
yp = c(-1,0,1)

pts = (1 + (xp))^2 + (2 - (yp))^2

lines(xp,yp,col='red',type='p')  # outside > 4

xpi = c(-1,0,-2)
ypi = c(1,2,3)


pts1 = (1 + (xpi))^2 + (2 - (ypi))^2

lines(xpi,ypi,col='blue',type='p') # inside < 4

#plot circle
radius = 2
plot(NA, NA, type = "n", xlim = c(-4, 2), ylim = c(-1, 5), asp = 1, xlab = "X1", 
    ylab = "X2")
symbols(c(-1), c(2), circles = c(radius), add = TRUE, inches = FALSE)
text(c(-1), c(2), "< 4")
text(c(-4), c(2), "> 4")



