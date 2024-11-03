u = ARMAacf(ar=c(0,-.9), lag.max=25)
 
plot(0:25, u, type="h", xlab="Lag", ylab="ACF") 
points(0:25, u)
abline(h=0)