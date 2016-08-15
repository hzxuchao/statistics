###############################################################################
# Description: univariate normal model with unknown mean and known variance
#
# Inputs:
#
#   y      = obervation of normal distribution
#   sigma  = measurement error sd
#   pri_mn = prior mean 
#   pri_sd = prior sd 
#   p = theta value that posterior probability can be found   
#
# Model:
#
#   Likelihood: y     ~ N(theta,sigma)
#   Prior:      theta ~ N(pri_mn,pri_sd)
#   posterior:  theta ~ N(post_mn, post_sd)
#
# Outputs:
# 
#   Plots of the likelihood, prior, and posterior
#
# 8/15/2016 by Chao Xu
#
#################################################

post.norm = function(y, sigma, pri_mn, pri_sd, p){
  theta = seq(0.00, 0.40, 0.001)
  post_mn = ((pri_mn/(pri_sd^2))+(y/(sigma^2)))/(1/(pri_sd^2)+1/(sigma^2))
  post_sd = sqrt(1/(1/(pri_sd^2)+1/(sigma^2)))
  
  prior = dnorm(theta, pri_mn, pri_sd)
  likelihood = dnorm(y, theta, sigma)
  posterior = dnorm(theta, post_mn, post_sd)
  
  #normalize
  prior = prior/sum(prior)
  likelihood = likelihood/sum(likelihood)
  posterior = posterior/sum(posterior)
  
  #posterior density that is larger than p
  post_den = round(sum(posterior[theta>p]), 3)
  
  #plot
  ylim = c(0, max(c(prior, likelihood, posterior)))
  plot(theta, prior, lty=2, type="l", xlim=c(0, 0.2), ylim=ylim, 
       xlab=expression(theta), ylab="density", main=paste("p(theta>", p, "/y)=", post_den))
  lines(theta, likelihood, lty=1, lwd=2)
  lines(theta, posterior, lty=3)
  legend("topright", c("prior", "likelihood", "posterior"), lty=c(2,1,3), inset=0.05, cex=0.8)
  abline(v=0.08, lty=2)
}


