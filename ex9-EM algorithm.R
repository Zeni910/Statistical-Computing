em_censored_exp <- function(y, cens, beta0 = 1 / mean(y), prec = 1e-6,
                            itermax = 100) {
  ## beta0 - initial value, if missing set to 1 / mean(y)
  ## prec - tolerance for convergence criterion
  ## itermax - max number of EM iterations
  n <- length(y)
  y_cens <- y[ cens == 0 ]
  y_not_cens <- y[ cens != 0 ]
  diff <- Inf
  niter <- 0
  res <- beta <- beta0
  while(diff > prec && niter < itermax) {
    niter <- niter + 1
    x_cens <- 1/beta + y_cens
    betaold <- beta
    beta <- n / sum(y_not_cens, x_cens) # 1 / xbar
    res <- c(res, beta)
    diff <- abs(beta - betaold)
  }
  names(res) <- paste("iter", 0:(length(res) - 1), sep = "_")
  yhat <- y
  yhat[cens == 0] <- 1 / beta + y_cens # estimates of the censored values
  data <- data.frame(y = y, cens = cens, yhat = yhat)
  list(betahat = beta, data = data,
       betaall = res)
}
surv <- read.table("survtimes.txt", header = TRUE)
y1 <- surv$y1
cens <- surv$cens
surv_result <- em_censored_exp(y1, cens)
names(surv_result) 
surv_result$betaall 

head(surv_result$data)

xx <- seq(from = 0, to  =  120, length.out = 600)
dexx <- dexp(xx, rate = surv_result$betahat)
hist(surv$y1[surv$cens == 1], freq = FALSE, breaks = 5,
     xlab = "units of 30 days",
     xlim = c(0, 140), ylim = c(0, 0.025),
     main = "Histogram of the uncensored survival times",
     sub =  "blue line : Exp(beta = 0.02023) pdf")
lines(xx, dexx, type = "l", col = "blue" )

hist(surv$y1, freq = FALSE, breaks = 5, xlab = "units of 30 days",
     xlim = c(0, 140), ylim = c(0, 0.025),
     main = "Histogram of all survival times (uncensored and censored)",
     sub = "blue line : Exp(beta = 0.02023) pdf")
lines(xx, dexx, type = "l", col = "blue")

txt <- paste("Histogram of estimated true survival times")
hist(surv_result$data$yhat, freq = FALSE, breaks = 5,
     xlab = "units of 30 days",
     xlim = c(0, 160), ylim = c(0, 0.025),
     main = txt, sub = "blue line: Exp(beta = 0.02023) pdf")
lines(xx, dexx, type = "l", col = "blue")

sim <- rexp(30, rate  =  surv_result$betahat)
hist(sim, freq = FALSE, breaks = 5, ylim = c(0, 0.025))
xmax <- max(sim) + 10
xx <- seq(from = 0, to = xmax, length.out = 600)
dexx <- dexp(xx, rate = surv_result$betahat)
lines(xx, dexx, type = "l", col = "blue")



