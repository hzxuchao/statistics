#################################################
# Description: binomial model
#
# Inputs:
#
#   y = observation of binomial model
#   n = total number of observations of binomial model
#   a = first parameter of prior beta
#   b = second parameter of prior beta
#
# Model:
#
#   Likelihood: y     ~ binom(y, n, theta)
#   Prior:      theta ~ beta(a, b)
#   posterior:  theta ~ beta(a+y, b+n-y)
#
# Outputs:
# 
#   Plots of the likelihood, prior, and posterior
#
# 8/15/2016 by Chao Xu
#
#################################################

post_binom = function(n, y, a, b, main=""){
  theta = seq(0.001, 0.999, 0.001)
  
  prior = dbeta(theta, a, b)
  likelihood = dbinom(rep(y, length(theta)), n, theta)
  posterior = dbeta(theta, a+y, b+n-y)
  
  #normalize
  prior = prior/sum(prior)
  likelihood = likelihood/sum(likelihood)
  posterior = posterior/sum(posterior)
  
  #plot
  ylim = c(0, max(c(prior, likelihood, posterior)))
  plot(theta, likelihood, lty=2, type="l", xlab="theta", ylab="", main=main, ylim=ylim)
  lines(theta, prior, lty=1, lwd=2)
  lines(theta, posterior, lty=3)
  legend("topright", c("likelihood","prior","posterior"), lty=c(1,2,3), inset=0.05, cex=1)
}
