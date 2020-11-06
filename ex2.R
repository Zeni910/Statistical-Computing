#ex2 computing
#1
#a)
## p - success probability
bern <- function(p) {
  u <- runif(1, 0, 1)
  if(u <= 1 - p)
    x <- 0
  else
    x <- 1
  x
}
bern(0.8)

v <- replicate(100, bern(0.3))
sum(v) / 100 # hopefully, close to 0.3

bern1 <- function(p) {
  u <- runif(1, 0, 1)
  if(u <= 1 - p)
    0
  else
    1
}
bern1(0.3)
## [1] 1
v <- replicate(100, bern1(0.3))
sum(v) / 100 # hopefully, close to 0.3

#b
symbern <- function(n, p) {
  u <- runif(n, 0, 1)
  x <- numeric(n)
  for(i in 1:n) {
    if(u[i] <= 1 - p)
      x[i] <- 0
    else
      x[i] <- 1
  }
  x
}

simbernA <- function(n, p) {
  u <- runif(n, 0, 1)
  x <- numeric(n) # create a vector of zeroes
  x[u > 1 - p] <- 1 # set to 1 each x[i] for which u[i] > 1 - p
  x
} #clearer version

simbin <- function(n, m, p) {
  x <- numeric(n)
  for(i in 1:n) {
    y <- simbernA(m, p)
    x[i] <- sum(y)
  }
  x
}
simbin(10, 5, 0.4) # generate 10 values from Bin (5, 0.4)

#c
rbinom(10, 1, 0.25)
rbinom(10, 1, 0.5)
rbinom(10, 1, 0.75)





#2 inverse cdf method for continuous cdf
simexp <- function(n, lambda) {
  u <- runif(n) # or more verbosly: runif(n, 0, 1) 
  x <- -log(1 - u) / lambda
  x # n iid values from Exp(lambda) distribution
}
sdata<-simexp(5000,0.2)
hist(sdata,freq = FALSE)
xexpo <- seq(0, 30, length.out = 100)
lines(xexpo, dexp(xexpo, rate = 0.2 ), col = "blue")


#3
#a,b
poiss<-rpois(5000,2)
table(poiss)

#c
5000*dpois(0,2) #exp
5000*dpois(1,2)
5000*dpois(2,2)
5000 * dpois(0:2, 2) #simplified

table(poiss)

M=max(poiss)

poiexp<-{}
for (i in 0:M+1) {
   poiexp[i]<-5000*dpois(i,2)
}
poiexp
poiobs<-as.vector(table(poiss))
expected <- 5000 * dpois(0:M, 2) #simplified

observed <- numeric(M + 1)
for(i in 0:M) {
  observed[i + 1] <- sum(x == i)
}

a18<-(poiobs-poiexp)/sqrt(poiexp)

std_resid <- (observed - expected) / sqrt(expected)
tbl <- cbind(count = 0:M, observed = observed,
             expected = expected, StdRes = std_resid)
tbl

    
  
  
  
  
  
}
