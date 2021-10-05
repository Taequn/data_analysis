library(e1071)

even.10 <- rbinom(n=10,          #number of observations
              size=1,            #number of trials (size=1 for a Bernoulli distribution)
              prob=.5)           #probability of success

mean(even.10)                    #mean
sd(even.10)                      #standard deviation
skewness(even.10)                #skewness
kurtosis(even.10)                #kurtosis



even.25  <- rbinom(n=25,         #number of observations
              size=1,            #number of trials (size=1 for a Bernoulli distribution)
              prob=.5)           #probability of success

mean(even.25)                    #mean
sd(even.25)                      #standard deviation
skewness(even.25)                #skewness
kurtosis(even.25)                #kurtosis



even.100 <- rbinom(n=100,        #number of observations
              size=1,            #number of trials (size=1 for a Bernoulli distribution)
              prob=.5)           #probability of success

mean(even.100)                   #mean
sd(even.100)                     #standard deviation
skewness(even.100)               #skewness
kurtosis(even.100)               #kurtosis



even.1000 <- rbinom(n=1000,      #number of observations
              size=1,            #number of trials (size=1 for a Bernoulli distribution)
              prob=.5)           #probability of success

mean(even.1000)                  #mean
sd(even.1000)                    #standard deviation
skewness(even.1000)              #skewness
kurtosis(even.1000)              #kurtosis



uneven.10 <- rbinom(n=10,        #number of observations
                  size=1,        #number of trials (size=1 for a Bernoulli distribution)
                  prob=.7)       #probability of success

mean(uneven.10)                  #mean
sd(uneven.10)                    #standard deviation
skewness(uneven.10)              #skewness
kurtosis(uneven.10)              #kurtosis



uneven.25  <- rbinom(n=25,       #number of observations
                   size=1,       #number of trials (size=1 for a Bernoulli distribution)
                   prob=.7)      #probability of success

mean(uneven.25)                  #mean
sd(uneven.25)                    #standard deviation
skewness(uneven.25)              #skewness
kurtosis(uneven.25)              #kurtosis



uneven.100 <- rbinom(n=100,      #number of observations
                   size=1,       #number of trials (size=1 for a Bernoulli distribution)
                   prob=.7)      #probability of success

mean(uneven.100)                 #mean
sd(uneven.100)                   #standard deviation
skewness(uneven.100)             #skewness
kurtosis(uneven.100)             #kurtosis



uneven.1000 <- rbinom(n=1000,    #number of observations
                    size=1,      #number of trials (size=1 for a Bernoulli distribution)
                    prob=.7)     #probability of success

mean(uneven.1000)                #mean
sd(uneven.1000)                  #standard deviation
skewness(uneven.1000)            #skewness
kurtosis(uneven.1000)            #kurtosis
