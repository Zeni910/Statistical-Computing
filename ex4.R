
#1,Ratio of uniforms method
#a)h(x) = exp(−x2/2)
sim_normal <- function(n) {
  n_accepted <- 0
  x <- numeric(n)
  while(n_accepted < n) {
    u <- runif(1, 0, 1)
    v <- runif(1, -2/exp(1), 2/exp(1))
    
    if(4*log(u)*(u^2) + v^2 < 0) {
      n_accepted <- n_accepted + 1
      x[n_accepted] <- v / u
    }
  }
 
  x
}


v <- sim_normal(10000)
ks.test(v,'pnorm')
plot(density(v), main = "", col = "blue")
g <- function(x) dnorm(x, 0, 1)
curve(g, from = -5, to = 5, add = TRUE)
hist(v, freq = FALSE, main = "Simulated N(0, 1) data",
     xlab = "x", ylim = c(0, 0.45)) # Black line on the plot
curve(dnorm, add = TRUE, col = "red")


#b)h(x)=x2(1−x)2 
sim_beta <- function(n) {
  n_accepted <- 0
  x <- numeric(n)
  count<-0
  while(n_accepted < n) {
    count<-count+1
    u <- runif(1, 0, 1/4)
    v <- runif(1, 0, 1/4)
    if(u^3<v*(u-v)) {
      n_accepted <- n_accepted + 1
      x[n_accepted] <- v / u
    }
  }
  print(n_accepted / count)
  x
}

bv <- sim_beta(10000)
plot(density(bv), main = "", col = "blue")
f <- function(x) dbeta(x, 3, 3)
curve(f, from = 0, to = 1, add = TRUE)



#2.Monte Carlo Integration
n=100000
dat<-replicate(10,rnorm(n))

m_c_int<-function(m){
  n=100000
  dat<-replicate(10,rnorm(n))
  prob<- dat > m
  vec<-colSums(prob)/n
  avg<-mean(vec)
  exact=1-pnorm(m)
  p<-c(vec,avg,exact)
  return(p)
}

p1<-m_c_int(1)#P(X > 1)vector,mean value and exact value
p2<-m_c_int(2)
p3<-m_c_int(3)
p4<-m_c_int(4)
p<-cbind(p1,p2,p3,p4)
p





