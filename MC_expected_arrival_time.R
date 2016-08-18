###############################################################################
# 
# Description: simulate a Markov chain process with three states
#
# Inputs:
#
#   n    = number of random subjects
#   p.i  = probability that state goes from 1 to 2
#   p.d  = probability that state goes from 2 to 3
#
# Outputs:
# 
#   Plots of the likelihood, prior, and posterior
#
# 8/17/2016 by Chao Xu
#
################################################

rproc = function(n, p.i, p.d){
  #the transition matrix
  P = matrix(0, 3, 3)
  P[1,] = c(1-p.i, p.i, 0)
  P[2,] = c((1-p.d)/2, (1-p.d)/2, p.d)
  P[3,] = c(0, 0, 1)
  
  #storage of each subject's successive states
  X = list()
  
  for(i in 1:n){
    x = c(1)
    k = 1
    
    #while the subject is not in the state 3 -- the self-absorbing state, simulation continues
    while(x[k] != 3){
      
      #rondam draws to determine transition probability
      u = runif(1)
      
      #the transition probabilities corresponding to their current state
      p = P[x[k],]
      
      #probabilty that state goes to 1
      if(u<p[1]){
        x = c(x, 1)
      
      #probability that state goes to 2
      } else if(u<sum(p[1:2])){
        x = c(x, 2)
        
      #probability that state goes to 3
      } else{
        x = c(x, 3)
      }
     k = k+1 
    }
    
    #store x in list X
    X[[i]] = x
  }
  return(X)
}

#estimate the probability that a random subject enter the stationary state by time T, and the mean active time
char.proc = function(Q){
  n = Q[1]; p.i = Q[2]; p.d = Q[3]; T = Q[4]
  Y = rproc(n, p.i, p.d)
  L = rep(0, n)
  
  for(i in 1:n){
    L[i] = length(Y[[i]])
  }
  
  return(c(mean(L<=T), mean(L)))
}

#apply for fixe sample size n=200 and arrival time T=5 
#for p.i and p.d ranging from 0.1 to 0.9 with a mesh size of 0.2

#set up the data frame G which will contain seq(0.1, 0.9, 0.2) across seq(0.1, 0.9, 0.2)
G = c()
pi = seq(0.1, 0.9, 0.2)
pd = seq(0.1, 0.9, 0.2)
for(i in 1:5) G = c(G, rep(pi[i], 5))
G = cbind(G, pd)
G = cbind(rep(200, 25), G, rep(5, 25))

OUT = apply(G, 1, char.proc)


#plot probability against pi for each value of of pd on the same plot

#indices of the pi values for each pd
ii1 <- seq(1, 25, by=5) 
ii2 <- seq(2, 25, by=5)
ii3 <- seq(3, 25, by=5)
ii4 <- seq(4, 25, by=5)
ii5 <- seq(5, 25, by=5)

#plot prob vs. pi for pd=0.1
plot( G[ii1,2], OUT[1,ii1], col=2, ylim=c(0,1), xlab="p.i", 
      ylab="Probability of arrival by time 5", axes=F, 
      main="Prob. of arrival by time 5 vs. p.i for each p.d")
axis(1, seq(.1, .9, length=5), seq(.1, .9, length=5))
axis(2, seq(0, 1, length=6), seq(0, 1, length=6))
box()
lines( G[ii1,2], OUT[1,ii1], col=2) 

#plot prob vs. pi for pd=0.3
lines( G[ii2,2], OUT[1,ii2], col=3)
points( G[ii2,2], OUT[1,ii2], col=3)

#plot prob vs. pi for pd=0.5
points( G[ii3,2], OUT[1,ii3], col=4)
lines( G[ii3,2], OUT[1,ii3], col=4)

#plot prob vs. pi for pd=0.7
points( G[ii4,2], OUT[1,ii4], col=5)
lines( G[ii4,2], OUT[1,ii4], col=5)

#plot prob vs. pi for pd=0.9
points( G[ii5,2], OUT[1,ii5], col=6) 
lines( G[ii5,2], OUT[1,ii5], col=6) 


#plot mean arrival time against pi for each value of pd on the same plot
plot( G[ii1,2], OUT[2,ii1], col=2, ylim=c(0,80), xlab="p.i", 
      ylab="Expected time until arrival", axes=F, 
      main="Expected time until arrival vs. p.i for each p.d")
axis(1, seq(.1, .9, length=5), seq(.1, .9, length=5))
axis(2, seq(0, 80, length=5), seq(0, 80, length=5))
box()
lines( G[ii1,2], OUT[2,ii1], col=2) 
lines( G[ii2,2], OUT[2,ii2], col=3)
points( G[ii2,2], OUT[2,ii2], col=3)
points( G[ii3,2], OUT[2,ii3], col=4)
lines( G[ii3,2], OUT[2,ii3], col=4)
points( G[ii4,2], OUT[2,ii4], col=5)
lines( G[ii4,2], OUT[2,ii4], col=5)
points( G[ii5,2], OUT[2,ii5], col=6) 
lines( G[ii5,2], OUT[2,ii5], col=6) 