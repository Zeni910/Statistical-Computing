#  bootstrap for the simple linear regression   

#method 1
ebs_eps<-function(y,x,B){
  fit <- lm(y ~ x)
  e <- residuals(fit)
  y_fitted <- fitted(fit) # or : beta[1] + beta[2] * 
  x <- wood$density # for convenience
  bs_resid <- matrix(0, nrow = B, ncol = 2)
  n<-length(y)
  for(i in 1:B){
    e_star <- sample(e, n, replace = TRUE)
    y_star <- y_fitted + e_star
    fit_star <- lm(y_star ~ x)
    bs_resid[i, ] <- coef(fit_star)
  }
  bs_resid
  
}

wood <- read.table("wood.txt", header = TRUE)
bs_beta <-ebs_eps(y = wood$hardness, x = wood$density, 1000)

fit_wood <- lm(hardness ~ density, data = wood)
beta <- coef(fit_wood)

plot(wood$density, wood$hardness, main = "",  xlab = "density", ylab = "hardness")
yreg <- beta[1] + beta[2] * wood$density
lines(wood$density, yreg, col = "red")
for(i in 1:10) {
    yest <- bs_beta[i, 1] + bs_beta[i, 2] * wood$density
    lines(wood$density, yest)
}

hist(bs_beta[, 1], xlab = "Estimate of alpha", freq = FALSE)
hist(bs_beta[ , 2], xlab = "Estimate of beta", freq = FALSE)


#method 2
bs_cases<-function(y,x,B){
  n <- length(y)
  ind_orig <- 1:n
  betastar <- matrix(0, nrow = B, ncol = 2)
  colnames(betastar) <- c("beta_0", "beta_1")
  for(i in 1:B){
    ind <- sample(ind_orig, n, replace = TRUE)  #index
    ystar <- y[ind]
    xstar <- x[ind]
    fit_star <- lm(ystar ~ xstar)
    betastar[i, ] <- coef(fit_star)
  }
  betastar
  
}

betastar_cases <- bs_cases(y = wood$hardness, x = wood$density, 1000)
hist(betastar_cases[, 1], xlab = "Estimate of alpha", freq = FALSE)
hist(betastar_cases[ , 2], xlab = "Estimate of beta", freq = FALSE)




