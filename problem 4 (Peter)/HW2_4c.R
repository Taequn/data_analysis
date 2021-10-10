library(ggplot2)

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

bernoulli.plot <- function(data){
  
  dfActual <- data.frame(matrix(nrow = 2, ncol = 2))
  colnames(dfActual) <- c("Prop","Value")
  rownames(dfActual) <- c("0","1")
  
  ActualTab <- as.data.frame(data) %>% group_by(data) %>%
    summarize(count = n()) # Calculate the counts
  
  dfActual[1,1] <- prop.table(ActualTab$count)[1]
  dfActual[2,1] <- prop.table(ActualTab$count)[2]
  dfActual[1,2] <- 0
  dfActual[2,2] <- 1
  
  
  dfEstimate <- data.frame(matrix(nrow = 2, ncol = 2))
  colnames(dfEstimate) <- c("Prop","Value")
  rownames(dfEstimate) <- c("0","1")
  
  EstimateTab <- as.data.frame(data) %>% group_by(data) %>%
    summarize(count = n()) # Calculate the counts
  
  dfEstimate[1,1] <- 1-MOM.bernoulli(data)
  dfEstimate[2,1] <- MOM.bernoulli(data)
  dfEstimate[1,2] <- 0
  dfEstimate[2,2] <- 1
  
  
  p1 <- ggplot(mapping = aes(y=Prop, x=Value)) +
    geom_col(data = dfActual, position = "dodge", fill = "lightblue") +
    geom_col(data = dfEstimate, aes(group = Value),
             fill = "black", width = 0.01, position = position_dodge(width = 0.9))
  
  dfActual <- data.frame(matrix(nrow = 2, ncol = 2))
  colnames(dfActual) <- c("Prop","Value")
  rownames(dfActual) <- c("0","1")
  
  ActualTab <- as.data.frame(data) %>% group_by(data) %>%
    summarize(count = n()) # Calculate the counts
  
  dfActual[1,1] <- prop.table(ActualTab$count)[1]
  dfActual[2,1] <- prop.table(ActualTab$count)[2]
  dfActual[1,2] <- 0
  dfActual[2,2] <- 1
  
  
  dfEstimate <- data.frame(matrix(nrow = 2, ncol = 2))
  colnames(dfEstimate) <- c("Prop","Value")
  rownames(dfEstimate) <- c("0","1")
  
  EstimateTab <- as.data.frame(data) %>% group_by(data) %>%
    summarize(count = n()) # Calculate the counts
  
  dfEstimate[1,1] <- 1-MLE.bernoulli(data)
  dfEstimate[2,1] <- MLE.bernoulli(data)
  dfEstimate[1,2] <- 0
  dfEstimate[2,2] <- 1
  
  
  p2 <- ggplot(mapping = aes(y=Prop, x=Value)) +
    geom_col(data = dfActual, position = "dodge", fill = "lightblue") +
    geom_col(data = dfEstimate, aes(group = Value),
             fill = "black", width = 0.01, position = position_dodge(width = 0.9))
  
  return(p1+p2)
  
  
  
}


even.10 <- rbinom(n=10,        #number of observations
                    size=1,        #number of trials (size=1 for a Bernoulli distribution)
                    prob=.5)       #probability of success

MOM.bernoulli(even.10)

MLE.bernoulli(even.10)

bernoulli.plot(even.10)



uneven.10 <- rbinom(n=10,        #number of observations
                    size=1,        #number of trials (size=1 for a Bernoulli distribution)
                    prob=.7)       #probability of success

MOM.bernoulli(uneven.10)

MLE.bernoulli(uneven.10)

bernoulli.plot(uneven.10)


