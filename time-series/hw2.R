# remove.packages("astsa")
# install.packages("astsa", repos = "https://cran.csie.ntu.edu.tw/")
library(astsa)
# library(astsa)
trend = time(jj) - 1970 # helps to ‘center’ time
Q = factor(rep(1:4,21)) # make (Q)uarter factors
reg = lm(log(jj)~0 + trend + Q, na.action=NULL) # without intercept
summary(reg)
reg2 = lm(log(jj)~ trend + Q, na.action=NULL)
summary(reg2)
# with intercept
plot(log(jj), type="o")
lines(fitted(reg), col=2)
dev.new()
plot.ts(resid(reg))
dev.new()
acf(resid(reg), 20)

# varv1 = varve[1:317]
# varv2 = varve[318:634]
# var(varv1) # = 133.4574 
# var(varv2) # = 594.4904
# a = varve[1:633]
# b = varve[2:634]
# var(log(b / a))
#  var(log(varv1)) # = 0.2707217 
#  var(log(varv2)) # = 0.451371 
#  par(mfrow=c(1,2))
# hist(varve)
# hist(log(varve))
# plot(log(varve)) # for part (b) 
# acf(log(varve)) # for part (c) 
# plot(diff(log(varve))) # for part (d) 
# acf(diff(log(varve))) # for part (d)

# plot((gtemp_both))
# lines(lowess(gtemp_both, f = 0.5),col=2) # red
# lines(smooth.spline(gtemp_both, spar=1),col=4)