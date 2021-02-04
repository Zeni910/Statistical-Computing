trout <- read.table("troutpcb.txt", header = TRUE)
pcb <- log(trout$pcb)
age <- trout$age
beta3 <- 0
sse <- numeric(10)
beta <- matrix(0, ncol = 3, nrow = 10)
for(i in 1:10) {
  beta3[i] <- 0.1 * i
  ls <- lsfit(age ^ beta3[i], pcb)
  sse[i] <- sum(residuals(ls) ^ 2)
  beta[i, ] <- c(coef(ls)[1], coef(ls)[2], beta3[i])
}

plot(beta3, sse)
beta3[which.min(sse)]
fit1 <- nls(pcb ~ b1 + b2 * age ^ b3,
            start = list(b1 = -4.77, b2 = 4.61, b3 = 0.2), trace = TRUE)
fit1


chwirut2 <- read.table("chwirut2.txt", header = TRUE) 
x2 <- chwirut2$x
y2 <- chwirut2$y
plot(y2, x2) 

efn <- function(x,  beta1,  beta2,  beta3) {
  exp(-beta1 * x) / (beta2 + beta3 * x)
}
#Plot the data with different guesses at f.
plot(y2 ~ x2, xlab = "Metal distance", ylab = "Ultrasonic response",
     ylim = c(0, 100))
curve(efn(x, beta1 = 0.1, beta2 = 0.01, beta3 = 1  ), add = TRUE, lty = 2)
curve(efn(x, beta1 = 0.1, beta2 = 0.01, beta3 = 0.1 ), add = TRUE, lty = 3)
curve(efn(x, beta1 = 0.1, beta2 = 0.01, beta3 = 0.01), add = TRUE, lty = 4)
curve(efn(x, beta1 = 0.2, beta2 = 0.01, beta3 = 0.01), add = TRUE, col = "red")
fit2 <- nls(y2 ~ efn(x2, beta1, beta2, beta3),
            start = list(beta1 = 0.2, beta2 = 0.01, beta3 = 0.01),
            trace = TRUE)

summary(fit2)

oxydata <- read.table("oxygen.txt", header = TRUE)
oxygen <- oxydata$y
days <- oxydata$x
sse <- beta2 <- numeric(10)
beta <- matrix(rep(0, 10 * 2), nrow = 10, ncol = 2)
for(i in 1:10) {
  beta2[i] <- 0.1*i
  pred <- (1 - exp(-beta2[i] * days))
  ls <- lsfit(pred, oxygen, intercept = F)
  sse[i] <- sum(residuals(ls) ^ 2)
  beta[i, ] <- c(coef(ls)[1], beta2[i])
}
cbind(beta = beta, sse = sse)

plot(beta2, sse)

F3 <- function(x, b) {
  res <- matrix(0, ncol = 2, nrow = length(x))
  for(i in 1:length(x))
    res[i, ] <- c(1 - exp(-b[2] * x[i]), b[1] * x[i] * exp(-b[2] * x[i]))
  res
}
gaussnewton3 <- function(y, x, initial, tol, maxiter = 100) {
  b <- initial
  count <- 0
  eps <- y - (b[1] * (1 - exp(-b[2] * x)))
  SS <- sum(eps ^ 2)
  diff <- 1
  while(tol<diff) {
    count <- count + 1
    SSold <- SS
    ff <- F3(x, b)
    b <- c(b + solve(t(ff) %*% ff) %*% t(ff) %*% eps)
    eps <- y - (b[1] * (1 - exp(-b[2] * x)))
    SS <- sum(eps ^ 2)
    diff <- abs(SS - SSold)
    if(count == maxiter)
      break
    print(b) 
  }
  b 
}


gaussnewton3(oxygen, days, c(218, 0.5), 0.00001)
fit <- nls(oxygen ~ b1 * (1 - exp(-b2 * days)), start = list(b1 = 218, b2 = 0.5),
          trace = TRUE)
