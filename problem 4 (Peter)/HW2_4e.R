even.100 <- rbinom(n=100,        #number of observations
                  size=1,        #number of trials (size=1 for a Bernoulli distribution)
                  prob=.5)       #probability of success

MOM.bernoulli(even.100)

MLE.bernoulli(even.100)

bernoulli.plot(even.100)



uneven.100 <- rbinom(n=100,        #number of observations
                    size=1,        #number of trials (size=1 for a Bernoulli distribution)
                    prob=.7)       #probability of success

MOM.bernoulli(uneven.100)

MLE.bernoulli(uneven.100)

bernoulli.plot(uneven.100)
