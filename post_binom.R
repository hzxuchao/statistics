post_binom = function(n, y, a, b, main=""){
  #prior ~ beta(a, b)
  #likelihood ~ bin(n, theta)
  #posterio r~ beta(a+y, b+n-y)
  theta = seq(0.001, 0.999, 0.001)
  prior = dbeta(theta, a, b)
  likelihood = dbinom(rep(y, length(theta)), n, theta)
  posterior = dbeta(theta, a+y, b+n-y)
  #standardize
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
