






dnormal<-function(age,a1,sl,sr){
    cond<-age<=a1
    sel<-rep(NA,length(age))
    sel[cond]<-2.0^-((age[cond]-a1)/sl*(age[cond]-a1)/sl)
    sel[!cond]<-2.0^-((age[!cond]-a1)/sr*(age[!cond]-a1)/sr)
    sel
}


# log(s,2)=-((a-a1)/sr)^2
# (a-a1)/(log(s,2)^0.5)=sr
sl<-3
a<-40
a1<-15
s<-0.1

sr<-(a-a1)/((-log(s,2))^0.5)

plot(dnormal(1:a,a1,sl,sr))

log(64,4)



height=1
maxage<-40
a1=5+runif(1)*10
sl<-a1
sr<-(maxage-a1)/height

plot(dnormal(1:30,a1,sl,sr))


par1<-3
par2<--3
par3<-3

mulen<-OMI@mulen
maxl<-max(OMI@mulen)
lens<-seq(0,maxl,length.out=200)
a1<-maxl*(0.05+exp(par1)/(1+exp(par1))*0.95)
sl<-a1*(-0.02+exp(par2)/(1+exp(par2))*2)
sr<-maxl*exp(par3)

dnormalFn(lens,a1,sl,sr)
plot(dnormalFn(lens,a1,sl,sr),ylim=c(0,1))



  sapply(age,func,params["a1"],params["sl"],params["sr"])}







dnormalFn<-function(age,params){

  a1=FLQuant(1,dimnames=dimnames(age))%*%params["a1"]
  s =FLQuant(1,dimnames=dimnames(age))%*%params["sl"]
  sr=FLQuant(1,dimnames=dimnames(age))%*%params["sr"]

  if (dims(age)$iter==1 &  dims(a1)$iter>1)
    age=propagate(age,dims(a1)$iter)

  s[age>=a1]=sr[age>=a1]

  res=2.0^(-((age%-%a1)%/%s%*%(age%-%a1)%/%s))}

#' @title Double normal ogive
#'
#' @description
#' Double normal ogive
#'
#' @param age FLQuant or FLCohort
#' @param params \code{FLPar} with parameters \code{a1} age at maximum, \code{sl} SD for lefthand limb and \code{sr} SD for righthand limb.
#' @param ... any other arguments
#'
#' @aliases dnormal dnormal-method dnormal,FLQuant,FLPar-method dnormal,FLPar,FLPar-method dnormal,numeric,numeric-method dnormal,FLQuant,numeric-method
#'
#' @return Returns an object of same class as \code{age} e.g. \code{FLQuant}
#'
#' @export
#' @docType methods
#' @rdname dnormal
#'
#' @seealso \code{\link{sigmoid}}, \code{\link{dnormal}}, \code{\link{logistic}}
#'
#' @examples
#' \dontrun{
#' params=FLPar(a1=4,sl=2,sr=5000)
#' dnormal(FLQuant(1:10,dimnames=list(age=1:10)),params)
#' }
setMethod("dnormal", signature(age="FLQuant",params="FLPar"),
function(age,params,...){
  res=dnormalFn(age,params)
  res@units=""
  res})
setMethod("dnormal", signature(age="FLPar",params="FLPar"),
          function(age,params,...){
            res=dnormalFn(age,params)
            res@units=""
            res})
setMethod("dnormal", signature(age="numeric",params="numeric"),
          function(age,params,...)
            dnormalFn(age,params))
setMethod("dnormal", signature(age="FLQuant",params="numeric"),
          function(age,params,...) {
            res=dnormalFn(FLPar(params),age)
            res@units=""
            res})
