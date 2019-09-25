#' Remove process error from an operating model projections (no recruitment deviations)
#'
#' @param OM an operating model object of class OM.
#' @return an object of class OM without process errors \code{x, dset}.
#' @export
#' @examples
#' OM_1d<-Deterministic(OM_1)
Deterministic<-function(OM){
  OM@Reccv<-array(1E-10,dim(OM@AC)) # nsim x nSR
  OM@Recdevs<-array(rep(apply(OM@Recdevs,2:3,mean),each=OM@nsim),dim(OM@Recdevs))
  OM
}


#' Check data dimenstions of an OMI object
#'
#' @param OMI an operating model input object of class OMI.
#' @return a printout / diagonstic of OMI data dimensions
#' @export
CheckOMI<-function(OMI){

  nf<-OMI@nf
  ny<-OMI@ny
  nHy<-OMI@nHy
  na<-OMI@na
  nma<-OMI@nma
  nr<-OMI@nr
  ns<-OMI@ns
  nsel<-OMI@nsel
  nl<-OMI@nl
  np<-OMI@np
  nCPUEq<-OMI@nCPUEq

  Year<-ny
  Quarter<-ns
  Subyear<-ns
  Stock<-np
  Area<-nr
  Fleet<-nf
  Length_category<-nl

  print("--- ALK ---")
  print(dim(OMI@iALK))
  print(c(np,ny,na,nl))
  print("")

  print("--- Cobs ---")
  print(apply(OMI@Cobs,2,max)[1:4])
  print(apply(OMI@Cobs,2,min)[1:4])
  print(data.frame(Year,Quarter,Area,Fleet))
  print("")

  print("--- Eobs - partial F's ---")
  print(as.integer(apply(OMI@Eobs,2,max)[1:4]))
  print(as.integer(apply(OMI@Eobs,2,min)[1:4]))
  print(c(ny,ns,nr,nf))
  print("")

  print("--- CPUEobs ---")
  print(as.integer(apply(OMI@CPUEobs,2,max)[1:5]))
  print(as.integer(apply(OMI@CPUEobs,2,min)[1:5]))
  print(c(ny,ns,nr,nCPUEq,nf))
  print("")

  print("--- CLobs ---")
  print(apply(OMI@CLobs,2,max)[1:5])
  print(apply(OMI@CLobs,2,min)[1:5])
  print(data.frame(Year,Subyear,Area,Fleet,Length_category))
  print("")

  print("--- HCobs ---")
  print(dim(OMI@HCobs))
  print(c(nHy,ns,na,nr))
  print("")

  print("--- Iobs ---")
  print(apply(OMI@Iobs,2,max)[1:4])
  print(apply(OMI@Iobs,2,min)[1:4])
  print(data.frame(Year,Subyear, Area, Stock))
  print("")

  print("--- PSAT ---")
  print(apply(OMI@PSAT,2,max)[1:6])
  print(apply(OMI@PSAT,2,min)[1:6])
  print(data.frame(p=np,a=nma,s=ns,t=2,fr=nr,tr=nr))
  print("")

  print("--- SOOobs ---")
  print(as.integer(apply(OMI@SOOobs,2,max)[1:4]))
  print(as.integer(apply(OMI@SOOobs,2,min)[1:4]))
  print(c(nma,ny,ns,nr))
  print("")

}
