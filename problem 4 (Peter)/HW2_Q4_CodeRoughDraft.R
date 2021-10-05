library(ggplot2)
x.1 <- rbinom(10,10,.7)
x.2 <- rbinom(25,10,.7)
x.3 <- rbinom(100,10,.7)
x.4 <- rbinom(1000,10,.7)


second.moment <- function(data, x.bar){
  temp = 0
  for(i in data){
    temp = temp + (i - x.bar)^2
  }
  return (temp/length(data))
}

MOM.binomial <- function(data){
  mu.hat <- mean(data)
  var.hat <- second.moment(data,mu.hat)
  p.hat <- (mu.hat-var.hat)/mu.hat
  n.hat <- mu.hat^2/((mu.hat - var.hat))
  print(paste("Estimated Parameter 1 from MOM",p.hat))
  print(paste("Estimated Parameter 2 from MOM",n.hat))
}
MOM.binomial(x.1)
imax <- NULL
LL.binomial <- function(p.1,n.1,data){
  LL.bin <- sum(dbinom(x=data,prob=p.1,size=n.1,log=T))
  return(LL.bin)
}
LL.binomial(.7,10,x.1)
vector.1 <- seq(0,1,by = .01)
log.lik <- sapply(vector.1, FUN = LL.binomial,n.1=10,data=x.1)
imax <- which.max(log.lik)
p.MLE <- vector.1[imax]

hist(p.MLE,breaks=10)
log.lik
qplot(log.lik,
  geom="histogram",
      binwidth=.01,
  xlim=c(0,1))

par(las=1,cex.lab=1.2)
plot(vector.1,log.lik,type="l",xlab="p",ylab="log-lik")
points(p.MLE,max(log.lik),pch=19,col="red")
abline(v=p.MLE,col="red")
x <- 0:10
N <- 10
sample.size <- 10
data.factor <- factor(x.1,levels=0:N)
prop.data <- table(data.factor)/sample.size
par(las=1,cex.lab=1.2)
xbars <- barplot(prop.data,xlab="No. of Successes",
                 ylab="Proportion",ylim=c(0,.7))
points <- (xbars,dbinom(x,prob=.7,size=10),type="b",col="blue",
                pch=19,lty=2)
points <- (xbars,dbinom(x,prob=p.MLE,size=10),type="b",col="red",
                pch=19,lty=2)

par(las = 1, cex.lab = 1.2)
xbars <- barplot(prop.data, xlab = "No. of successes",
                 ylab = "Proportion", ylim = c(0, 0.4))
points(xbars, dbinom(x, prob = 0.7, size = 10), type = "b", col = "blue",
       pch = 19, lty = 2)
points(xbars, dbinom(x, prob = p.MLE, size = 10), type = "b", col = "red",
       pch = 19, lty = 2)
