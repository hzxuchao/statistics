################################################################################
# Description: rejection sampling from posterior assuming univariate normal with known variance
#
# Inputs:
#
#   y      = random sample
#   theta  = random draw from support distribution (in this case from random sample Y)
#  
# Model:
#
#   Likelihood: y     ~ N(theta,sigma)
#   Prior:      theta ~ Cauchy (student's t with 1 degree of freedom)
#
# Outputs:
# 
#   Plots of the envelope, and posterior
#
# Note:
#   Large sample size results in high rejection rate
#
# 8/15/2016 by Chao Xu
#
#################################################

#define posterior
posterior = function(theta, y, sigma)
{
   pdf = dt(theta, 1)
   for (i in 1:length(y))
   {
      pdf = pdf*dnorm(y[i], theta, sigma)
   }
   pdf
}

#define envelope distribution
envelope = function(theta, y.bar, sd) {dnorm(theta, y.bar, sd)}

#generate a random sample from Y
set.seed(2012)
y = rnorm(5, 2, 1)       

#calculate the constant M
grid = seq(-2, 5, 0.01)
y.bar = mean(y)
sd = sd(y)
env.density = dnorm(grid, y.bar, sd)
post.density = posterior(grid, y, 1)
M = max(post.density/env.density)
env = M*env.density

#draw samples
Nsim = 10^4
theta = rep(0, Nsim)
count = 0
attempts = 0
while(count<Nsim)
{
   attempts = attempts+1
   can = rnorm(1, y.bar, sd)
   p = posterior(can, y, 1)/(m*envelope(can, y.bar, sd))
   if (runif(1)<p)
   {
      count = count+1
      theta[count] = can
   }
}

#plot results
rej_rate = (attempts-Nsim)/attempts
h = hist(theta, breaks=50, xlab=expression(theta), main="histogram of posterior samples")
fudge = max(h$counts)/max(post.density)
lines(grid, fudge*env, lty=1)
lines(grid, fudge*post.density, lty=2)
legend("topleft", c("envelope","posterior"), lty=c(1,2), inset=0.05)

#rejection rate
rej_rate
