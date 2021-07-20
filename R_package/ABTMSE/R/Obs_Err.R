rnorm_T95<-function(n=1,mean=0,sd=1){
  ntrial=n*ceiling(48*(1/n)^0.5+2)  #prevents over sampling at high n; 0.05^50 = 1E-65 chance of not sampling in the interval given n=1
  trial<-rnorm(ntrial,mean,sd)
  bound<-sd*1.959964
  if(sd==0){
    return(rep(mean,n)) # exception for sd=0 (Deterministic == T)
  }else{
    return(trial[trial>(mean-bound) & trial < (mean+bound)][1:n])
  }
}


solveforR1<-function(v2,y2){

  # gamma1 = [  -1 + SQRT{ 1 + 8 * max(gamma2, -1/8) }  ] / 2 # From Carmen and Doug
  y1 = (-1+(1 + 8 * max(y2, -1/8) )^0.5)/2
  v1 = 2*v2/(1+y1)
  return(c(v1=v1,y1=y1))
}

solveforR1_old<-function(v2,y2){

  prop<-function(par,v2,y2){
    v1<-exp(par[1])
    y1<-exp(par[2])
    v2p=v1*(1+y1)/2
    y2p=y1*(1+y1)/2
    (v2p-v2)^2+(y2p-y2)^2
  }

  opt<-optim(c(0,0),prop,v2=v2,y2=y2)
  return(c(v1=exp(opt$par[1]),y1=exp(opt$par[2])))

}

testsolveR2<-function(){

  inventR2<-function(v1,y1) c(v2=v1*(1+y1)/2, y2=y1*(1+y1)/2)
  n<-10
  v1s<-runif(n)
  y1s<-runif(n)
  roundy<-3

  for(i in 1:n){

    R2s<-inventR2(v1s[i],y1s[i])
    calc1s<-solveforR1(v2=R2s[1],y2=R2s[2])
    print(paste("Simulated:",round(v1s[i],roundy),"and",round(y1s[i],roundy),"  Predicted:",round(calc1s[1],roundy), "and", round(calc1s[2],roundy)))

  }

}

lnormdev<-function(ny,STD,AC1){
  procmu <- -0.5*STD^2 #*(1 - AC1)/sqrt(1 - AC1^2) # adjusted log normal mean
  err<-rnorm_T95(ny,procmu, STD)
  for (y in 2:ny) err[y]<-AC1*err[y-1]+err[y]*(1-AC1*AC1)^0.5
  exp(err)
}


lndev<-function(ny,STD){
  procmu <- -0.5*STD^2 #*(1 - AC1)/sqrt(1 - AC1^2) # adjusted log normal mean
  err<-rnorm_T95(ny,procmu, STD)
  err
}

apply_AC1<-function(err,AC1){ # for matrices
  for (y in 2:ncol(err)) err[,y]<-AC1*err[,y-1]+err[,y]*(1-AC1*AC1)^0.5
  err
}

apply_AC1_vec<-function(err,AC1){ # for vectors
  for (y in 2:length(err)) err[y]<-AC1*err[y-1]+err[y]*(1-AC1*AC1)^0.5
  err
}
