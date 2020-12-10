#ex5
#1. Importance sampling
#1a
impnorm <- function(n) {
  x <- y <- psi <- numeric(n)
  for(i in 1:n) {
    x[i] <- 2 / runif(1)
    psi[i] <- x[i]^2 * exp(-x[i]^2 / 2) / sqrt(8 * pi)
    y[i] <- mean(psi[1:i])
  }
  y
    
}



#1b
impnorm2 <- function(n) {
  x <- y <- psi <- numeric(n)
  for(i in 1:n) {
    x[i] <- 2 + rexp(1)
    psi[i] <- exp(-x[i]^2 / 2) / (sqrt(2 * pi) * exp(2 - x[i]))
    y[i] <- mean(psi[1:i])
  }
  y
  
}


n <- 1000
x <- impnorm(n)
y <- impnorm2(n)
z <- rnorm(n)
w <- numeric(n)
for(i in 1:n)
  w[i] <- sum(z[1:i] > 2) / i
u <- rep(pnorm(2, lower.tail = FALSE), n) # or: rep(1 - pnorm(2), n)
plot(w, xlab = "Iterations", ylab = "Probability estimate", main = "P(Z>2)",
     ylim = c(0, 0.05), type = "l")
lines(x, lty = 2, col = "red")
lines(y, lty = 3, col = "green")
lines(u)


#2
thetac <- function(n) {
  alpha <- exp(-1/2) / sqrt(2 * pi)
  beta <- c(-alpha, 0, alpha / 3, -alpha / 12)
  eu <- c(0, 1/3, 0, 1/5)
  ew <- 2 * sum(beta * eu)
  u <- runif(n, 0, 2)
  f <- exp(-u^2 / 2) / sqrt(2 * pi)
  z <- 0
  for(i in 1:n) {
    w <- 0
    for(k in 1:4)
      w <- w + beta[k] * (u[i] - 1)^k
    z <- z + (2 / n) * (f[i] - w)
  }
  theta <- 0.5 - z - ew
  theta
}


thetac(10000)
