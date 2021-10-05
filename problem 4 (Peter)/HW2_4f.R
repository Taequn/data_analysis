even.1000 <- rbinom(n=1000,        #number of observations
                  size=1,        #number of trials (size=1 for a Bernoulli distribution)
                  prob=.5)       #probability of success

MOM.bernoulli(even.1000)

MLE.bernoulli(even.1000)

bernoulli.plot(even.1000)



uneven.1000 <- rbinom(n=1000,        #number of observations
                    size=1,        #number of trials (size=1 for a Bernoulli distribution)
                    prob=.7)       #probability of success

MOM.bernoulli(uneven.1000)

MLE.bernoulli(uneven.1000)

bernoulli.plot(uneven.1000)
