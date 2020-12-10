#1
simexp <- function(n, lambda) { # DIY Exponential simulation u <- runif(n)
  x <- -log(1 - u) / lambda
  x 
}
gammaint <- function(n, k, beta) {
  x <- numeric(n)
  for(i in 1:n) {
    y <- simexp(k, beta)
    x[i] <- sum(y)
  }
  x 
}
z <- gammaint(10000, 3, 1)
plot(density(z), main = "", xlab = "z", col = "blue")
f <- function(x) dgamma(x, 3, 1)
curve(f, from = 0, to = 30, add = TRUE)

#2 rejection sampling
simgamma <- function(n, alpha, beta, k, lambda) {
  zstar <- (alpha - k) / (beta - lambda) # Point of maximum 
  Kopt <- zstar^(alpha - k) * exp((lambda - beta) * zstar) 
  t <- 0 # Counts number of accepted values
  res <- numeric(n)
  while(t < n) {
    z <- gammaint(1, k, lambda) # Proposal from Gamma (k, lambda)
    u <- runif(1, 0, Kopt * z^(k - 1) * exp(-lambda * z))
    f <- z^(alpha - 1) * exp(-beta * z)
    if(u < f) {
      t <- t+1
      res[t] <- z }
  }
  res 
}

z <- simgamma(10000, 3.5, 2, 3, 1)
plot(density(z), main = "", xlab = "z", col = "blue")
f <- function(x) dgamma(x, 3.5, 2)
curve(f, add = TRUE)

