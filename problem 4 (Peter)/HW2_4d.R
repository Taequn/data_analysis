even.25 <- rbinom(n=25,        #number of observations
                  size=1,        #number of trials (size=1 for a Bernoulli distribution)
                  prob=.5)       #probability of success

MOM.bernoulli(even.25)

MLE.bernoulli(even.25)

bernoulli.plot(even.25)



uneven.25 <- rbinom(n=25,        #number of observations
                    size=1,        #number of trials (size=1 for a Bernoulli distribution)
                    prob=.7)       #probability of success

MOM.bernoulli(uneven.25)

MLE.bernoulli(uneven.25)

bernoulli.plot(uneven.25)
