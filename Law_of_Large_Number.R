#################################################
# Description: investigate Law of Large Number
#
# Inputs:
#
#   M5 = random sample of size n=5
#   M12 = random sample of size n=12
#   M25 = random sample of size n=25
#   mn = population mean
#   var = population variance
#   Y = random sample from population
#
# Outputs:
# 
#   Four graphs that go to show the approximation of normal distribution as sample size goes up
#
# 8/21/2016 by Chao Xu
#
#################################################

par(mfrow=c(2,2))


plot4graphs = function(M5, M12, M25, mn, var, Y)
{
   v = seq(mn-2*sqrt(var), mn+2*sqrt(var), by = 0.01)

   r = hist(M5, breaks=20, freq=F, xlim=c(mn-2*sqrt(var), mn+2*sqrt(var)))
   lines(v, dnorm(v, mn, sqrt(var/5)))

   r = hist(M12, breaks=20, freq=F, xlim=c(mn-2*sqrt(var)/1.5, mn+2*sqrt(var)/1.5))
   lines(v, dnorm(v, 2, sqrt(4/12)))

   r = hist(M25, breaks=20, freq=F, xlim=c(mn-2*sqrt(var)/2, mn+2*sqrt(var)/2))
   lines(v, dnorm(v, mn, sqrt(var/25)))

   qqnorm(Y)
   qqline(Y)
}


exp_5 = matrix(rexp(1000*5, 0.5), 1000, 5)
exp_12 = matrix(rexp(1000*12, 0.5), 1000, 12)
exp_25 = matrix(rexp(1000*25, 0.5), 1000, 25)

M5 = apply(exp_5, 1, mean)
M12 = apply(exp_12, 1, mean)
M25 = apply(exp_25, 1, mean)

Y = rexp(1000, 0.5)
