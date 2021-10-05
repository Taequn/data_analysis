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
  return(p.hat)
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
  return(MLE$par)
}

MLE.bernoulli(uneven.10)

library(ggplot2)

bernoulli.plot <- function(data){
  
  dfData <- data.frame(matrix(nrow = 4, ncol = 3))
  colnames(dfData) <- c("Prop","Status","Value")
  rownames(dfData) <- c("Actual 0s","Actual 1s","Estimated 0s","Estimated 1s")
  
  dfActual <- as.data.frame(data) %>% group_by(data) %>%
    summarize(count = n()) # Calculate the counts
  
  dfData[1,1] <- prop.table(dfActual$count)[1]
  dfData[2,1] <- prop.table(dfActual$count)[2]
  dfData[1,2] <- "Actual"
  dfData[2,2] <- "Actual"
  dfData[1,3] <- 0
  dfData[2,3] <- 1
  
  dfData[3,1] <- 1-MOM.bernoulli(data)
  dfData[4,1] <- MOM.bernoulli(data)
  dfData[3,2] <- "MOM"
  dfData[4,2] <- "MOM"
  dfData[3,3] <- 0
  dfData[4,3] <- 1
  
  
  p1 <- ggplot(dfData, aes(fill=Status, y=Prop, x=Value)) +
    geom_bar(position="dodge", stat="identity") +
    ylab("Probability") + xlab("Observations")
  
  dfData <- data.frame(matrix(nrow = 4, ncol = 3))
  colnames(dfData) <- c("Prop","Status","Value")
  rownames(dfData) <- c("Actual 0s","Actual 1s","Estimated 0s","Estimated 1s")
  
  dfActual <- as.data.frame(data) %>% group_by(data) %>%
    summarize(count = n()) # Calculate the counts
  
  dfData[1,1] <- prop.table(dfActual$count)[1]
  dfData[2,1] <- prop.table(dfActual$count)[2]
  dfData[1,2] <- "Actual"
  dfData[2,2] <- "Actual"
  dfData[1,3] <- 0
  dfData[2,3] <- 1
  
  dfData[3,1] <- 1-MLE.bernoulli(data)
  dfData[4,1] <- MLE.bernoulli(data)
  dfData[3,2] <- "MLE"
  dfData[4,2] <- "MLE"
  dfData[3,3] <- 0
  dfData[4,3] <- 1
  
  
  p2 <- ggplot(dfData, aes(fill=Status, y=Prop, x=Value)) +
    geom_bar(position="dodge", stat="identity") +
    ylab("Probability") + xlab("Observations")
  
  return(p1+p2)
  
  
  
}

bernoulli.plot(uneven.10)

