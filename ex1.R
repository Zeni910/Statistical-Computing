#1.Create a vector, say x, containing the even numbers from 2 to 200.
x=seq(2,200,2)
x2<-2*1:100

x1={}
for (i in 2:198){
  x1[1]=2
  x1[i]=i+2
}

x1


x3<-numeric(100)
for (i in 1:100){
  x3[i]<-2*i
}

tmp <- 2:200 # take all integers in [2, 200]
x3 <- tmp[ tmp %% 2 == 0 ] # extract only the even numbers

x4 <- numeric(0) # a vector with no elements for the result
for(i in 2:200) {
  if(i %% 2 == 0)
    x4 <- c(x4, i)
}

all(x2 == x, x3 == x, x4 == x)


#2
n <- length(x)
s <- numeric(n)
#original
for(i in 1:n) {
  val <- 0
  for(j in 1:i) {
    val <- val + x[j]
   }
  s[i] <- val 
}
s

n <- length(x)
s <- numeric(n)
#improved
for (i in 2:n){
  s[1]=x[1]
  s[i]=x[i]+sum(s[i-1])
}
s



#3
u=runif(10)
b={}
for (i in 1:length(u)){
    if (u[i]<=0.4){
       b[i]=TRUE
       b
    }
    else b[i]=FALSE
}   
b
{is.logical(b)} 
sum(b==TRUE)

txt={}
for (i in 1:length(u)){
  if (u[i]<=0.4){
    txt[i]="Head"
    
  }
  else txt[i]="Tail"
}   
txt
sum(txt== "Tail")


#4
smallernum<-function(vec,num){
  c=sum(vec<num)
  c
}

aa<-c(2,1,4)
n1<-3
smallernum(aa,n1)
v <- rnorm(5)
smallernum(v,0)
