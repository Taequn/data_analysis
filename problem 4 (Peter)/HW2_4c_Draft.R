uneven.10 <- rbinom(n=10,        #number of observations
                    size=1,        #number of trials (size=1 for a Bernoulli distribution)
                    prob=.7)       #probability of success

second.moment <- function(data, x.bar){  #function to find the second moment
  temp = 0
  for(i in data){
    temp = temp + (i - x.bar)^2
  }
  return (temp/length(data))
}

MOM.bernoulli <- function(data){          #function to find the MOM Estimator for p in a Bernoulli distribution
  mu.hat <- mean(data)
  var.hat <- second.moment(data,mu.hat)
  p.hat <- (mu.hat-var.hat)/mu.hat
  print(paste("Method of Moments Estimator for Parameter p: ",p.hat))
}

MOM.bernoulli(uneven.10)

# Negative of the Likelihood function
bernoulliLogLik.neg <- function(par, data){
  -sum(dbinom(x=data, prob=par, size=1, log=TRUE))} #Sum is negative, as we are looking for a min with optim

MLE.bernoulli <- function(data){
  MLE <- optim(par = .5,               # best guess for the parameter
               fn = bernoulliLogLik.neg,   # function to minimize
               data = data,       # data (an argument to fn)
               method = "Brent",       # required for univariate optimization
               lower = 0,              # lowest possible lambda
               upper = 1)              # reasonable upper bound
  
  # Note that our mle is hat(lambda) = xbar 
  print(paste("Maximum Likelihood Estimator for Parameter p: ",MLE$par))
}

MLE.bernoulli(uneven.10)

