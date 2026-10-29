
#PRACTICAL 1-Probability and Probability Distributions

# Consider 3 binomial distributions with n = 10 and p = 0.8, 0.5 and 0.2. Plot the spike plots to
# represent the pmfs of the three distributions.

x <- 0:10
p1 <- dbinom(x, 10, 0.8)
p2 <- dbinom(x, 10, 0.5)
p3 <- dbinom(x, 10, 0.2)

par(mfrow=c(1,3))
plot(x,p1,"h",main = "Binomial pmf for p=0.8")
points(x,p1,pch=16)
plot(x,p2,"h",main = "Binomial pmf for p=0.5")
points(x,p2,pch=16)
plot(x,p3,"h",main = "Binomial pmf for p=0.2")
points(x,p3,pch=16)

#First graph is negatively skewed.
#Second graph is symmetric.
#Third graph is positively skewed.

########################################################

##Compute PMF and CDF of Binomial (5,0.7) and plot both.


par(mfrow=c(1,2))

x1 = 0:5
bpmf1 = dbinom(x1,5,0.7)

plot(x1,bpmf1,type = "h", main = "PMF of B(5,0.7)",xlab = "x",
     ylab="PMF",col = "red")

bcdf = pbinom(x1,5,0.7)
bcdf1 = pbinom(x1,5,0.7,lower.tail = F)

plot(x1,bcdf,type = "s", main = "CDF of B(5,0.7)",
     xlab = "x", ylab="CDF",col="red")

plot(x1,bcdf1,type = "s", main = "CDF of B(5,0.7)",
     xlab = "x", ylab="CDF",col="red")

########################################################

#3. Take n = 20, p = 0.2 and compute P(X=x) for x = 0,1,……..,n, using both binomial and Poisson

########################################################

#4. Display graphically how Binomial tends to Poisson distribution.

m=seq(10,60,10)

par(mfrow=c(2,3))

for (i in 1:length(m)) {
  x=0:m[i]
  pmf.bino=dbinom(x,m[i],0.1)
  lambda1=0.1*m[i]
  pmf.pois=dpois(x,lambda1)
  plot(x,pmf.bino,type="l",col="red",xlab="x",ylab="PMF",
       main=paste("n=",m[i]))
  lines(x,pmf.pois)
}

#As n increases, Binomial approaches Poisson when p is small.

########################################################

#5. For Poisson distribution having lambda = 3,5,9,13,22.
#Plot the pmf and comment.

x=0:30

plot(x,dpois(x,3),type="l",col="red",
     main="Comparison Poisson Probabilities",xlab="x",ylab="PMF")
lines(x,dpois(x,5),col="green")
lines(x,dpois(x,9),col="blue")
lines(x,dpois(x,13),col="orange")
lines(x,dpois(x,22),col="black")

legend("topright",lty=1,col=c("red","green","blue","orange","black"),
       c("lambda=3","lambda=5","lambda=9","lambda=13","lambda=22"))

#As lambda increases, the distribution shifts to the right.

########################################################

#6. For Binomial distribution, check the property that Binomial tends to Normal.
#Find the pmf of Binomial(n,p=0.5) where n=10,100,200.
#Plot the three pmfs and comment.

par(mfrow=c(1,3))

p=0.5

for (n in c(10,100,200)) {
  x=0:n
  plot(x,dbinom(x,n,p),type="h",
       main=paste("PMF of B(",n,",",p,")"),
       xlab="x",ylab="PMF")
}

#As n increases, Binomial approaches Normal.
#For p=0.5, the distribution is symmetric.

########################################################

#7. For Normal(mu,sigma_square) distribution plot the PDF and CDF and comment.

b=seq(-50,50,0.1)

par(mfrow=c(1,2))

#i.)

plot(b,pnorm(b,mean=0,sd=4),type="l",col="red",
     main="CDF of Normal",xlab="x",ylab="CDF")
lines(b,pnorm(b,mean=0,sd=6),col="green")
lines(b,pnorm(b,mean=0,sd=10),col="blue")
lines(b,pnorm(b,mean=0,sd=20),col="black")

legend("topleft",lty=1,col=c("red","green","blue","black"),
       c("sd=4","sd=6","sd=10","sd=20"))

plot(b,dnorm(b,mean=0,sd=4),type="l",col="red",
     main="PDF of Normal",xlab="x",ylab="PDF")
lines(b,dnorm(b,mean=0,sd=6),col="green")
lines(b,dnorm(b,mean=0,sd=10),col="blue")
lines(b,dnorm(b,mean=0,sd=20),col="black")

legend("topleft",lty=1,col=c("red","green","blue","black"),
       c("sd=4","sd=6","sd=10","sd=20"))

#As standard deviation increases, the PDF becomes wider and flatter.

#ii.)

plot(b,pnorm(b,mean=-10,sd=sqrt(2)),type="l",col="red",
     main="CDF of Normal",xlab="x",ylab="CDF")
lines(b,pnorm(b,mean=-5,sd=sqrt(2)),col="green")
lines(b,pnorm(b,mean=0,sd=sqrt(2)),col="blue")
lines(b,pnorm(b,mean=2,sd=sqrt(2)),col="black")
lines(b,pnorm(b,mean=8,sd=sqrt(2)),col="orange")

legend("topleft",lty=1,col=c("red","green","blue","black","orange"),
       c("mu=-10","mu=-5","mu=0","mu=2","mu=8"))

plot(b,dnorm(b,mean=-10,sd=sqrt(2)),type="l",col="red",
     main="PDF of Normal",xlab="x",ylab="PDF")
lines(b,dnorm(b,mean=-5,sd=sqrt(2)),col="green")
lines(b,dnorm(b,mean=0,sd=sqrt(2)),col="blue")
lines(b,dnorm(b,mean=2,sd=sqrt(2)),col="black")
lines(b,dnorm(b,mean=8,sd=sqrt(2)),col="orange")

legend("topleft",lty=1,col=c("red","green","blue","black","orange"),
       c("mu=-10","mu=-5","mu=0","mu=2","mu=8"))

#As mean increases, the curves shift to the right.

########################################################

#8. For the question 1 above, draw random samples of size 100 from
#each of the three distributions and plot spike plots for relative
#frequency distributions.

#a

x <- 0:10
p1 <- dbinom(x,10,0.8)

par(mfrow=c(1,2))

plot(x,p1,"h",main="Binomial pmf for p=0.8")
points(x,p1,pch=16)

set.seed(100)
d <- rbinom(n=100,size=10,prob=0.8)

plot(table(factor(d,levels=x))/100,type="h")

#b

p2 <- dbinom(x,10,0.5)

par(mfrow=c(1,2))

plot(x,p2,"h",main="Binomial pmf for p=0.5")
points(x,p2,pch=16)

e <- rbinom(n=100,size=10,prob=0.5)

plot(table(factor(e,levels=x))/100,type="h")

#c

p3 <- dbinom(x,10,0.2)

par(mfrow=c(1,2))

plot(x,p3,"h",main="Binomial pmf for p=0.2")
points(x,p3,pch=16)

f <- rbinom(n=100,size=10,prob=0.2)

plot(table(factor(f,levels=x))/100,type="h")

########################################################

#9. Obtain Probability Distribution of X,
#Where X is the number on the upper face of the die when a six-sided
#symmetric die is rolled. Simulate random samples of sizes 100, 200 and 500 respectively
#and verify the frequency interpretation of probability.

set.seed(10)

a <- sample(1:6,100,replace=T)
table(a)
table(a)/100

b <- sample(1:6,200,replace=T)
table(b)
table(b)/200

c <- sample(1:6,500,replace=T)
table(c)
table(c)/500

