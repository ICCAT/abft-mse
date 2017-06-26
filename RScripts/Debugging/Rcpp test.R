
library(ABTMSE)
sfInit(parallel=TRUE, cpus=detectCores())
OM<-new('OM',OMd="C:/ABT-MSE/Objects/OMs/1/",nsim=16,proyears=30,seed=1)


load("C:/ABT-MSE/Objects/OMs/1/OM")



install.packages('Rcpp')
library(Rcpp)


cppFunction('NumericVector vmult(NumericVector x, NumericVector y) {
  int n = y.size();
  NumericVector out(n);

  for(int i = 0; i < n; ++i) {
    out[i] = y[i] * x[i];
  }
  return out;
  }')
n<-100000000
x<-runif(n)
y<-runif(n)

system.time(x*y)
system.time(vmult(x,y))







iALK<-OM@iALK[1,1,1,,]
na<-OM@nages
nl<-OM@nlen

sel<-OM@sel[1,,]


dim(iALK)

selbyfleet<-array()

FMref<-OM@FM[1,1,,1,]


dim(OM@FM)
dim(OM@iALK)









