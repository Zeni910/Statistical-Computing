#1
bslogc<-function(x,B){
  n<-length(x)
  thetastar<-replicate(B,{xstar<-sample(x,n,replace = T)
                       median(xstar)} )
  thetastar
}
 
x <- diabetes$logCpeptide   # in package"bootstrap"
hist(x, freq = FALSE, main = "Histogram of diabetes data")
median(x)
bscpep<-bslogc(x,1000)
hist(bscpep,freq = FALSE, breaks = 7,main = "Histogram of bootstrap diabetes medians")
var(bscpep)
bias<-mean(bscpep)-median(x)

#2
#estimates correlation with SAT & GPA
samplecorr <- function(data, B) {
  n <- nrow(data)
  res <- numeric(B)
  for(i in 1:B) {
    ind <- sample(n, n, replace = TRUE)
    bs_data <-  data[ind, ]
    res[i] <- cor(bs_data[ , 1], bs_data[ , 2])
  }
  res 
}
cor(law)
cor(law[ , 1], law[ , 2])
bs_law <- samplecorr(law, 10000)
hist(bs_law,  freq = FALSE,
     main = "Histogram of law data bootstrap corellation coefficients")
bias.law <- mean(bs_law) - cor.law[1, 2]
bias.law
var(bs_law)

cor(law82)
bs_law82 <- samplecorr(law82[ , 2:3], 10000)
hist(bs_law82,  freq = FALSE,
     main = "Histogram of law82 data bootstrap corellation coefficients")
bias.law82 <- mean(bs_law82) - cor.law82[1, 2]
bias.law82
var(bs_law82)



#3 t-c
#mean survival time after surgery of mice in the treatment and control groups
mean(mouse.t)-mean(mouse.c)
bsmice<-function(x,y,B){
  n1<-length(x)
  n2<-length(y)
  thet1<-replicate(B,{xstar<-sample(x,n1,replace = T)
                       mean(xstar)})
  thet2<-replicate(B,{ystar<-sample(y,n2,replace = T)
                       mean(ystar)})
  d=thet1-thet2
  
}

df1=bsmice(mouse.t,mouse.c,1000)
head(df1)
pihat <- sum(df1 > 10) / length(df1) #probability that survival time difference > 10
pihat

