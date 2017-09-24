

Const_Cur_TAC = function(x, dset){            # Calculate TAC from simulated data dset for simulation x

  dset$TAC[x, 1]                              # TAC is set to the first ever (current, 2016) TAC level

}

class(Const_Cur_TAC) = "MP"                   # Assign Const_Cur_TAC a class 'MP'




MeanCat <- function(x, dset){                 # Calculate TAC from simulated data dset for simulation x

  mean(dset$Cobs[x, ])                        # TAC is set to the mean historical observed catches

}

class(MeanCat) = "MP"                         # Assign MeanCat a class 'MP'




EMP1 = function(x, dset){                     # Calculate TAC from simulated data dset for simulation x

  Jtarg = 4.8                                 # Index target level
  ny = dim(dset$Iobs)[3]                      # Last year of index observations

  Jmu = mean(dset$Iobs[x, 1, (-4:0)+ny])      # Mean of index 1 (JPN_LL_NEAtl2) over last five years
  Jratio = Jmu/Jtarg                          # Ratio of current mean index / target

  cury = dim(dset$TAC)[2]                     # Last year of past TAC recommendations
  previousTAC =  dset$TAC[x, cury]            # Get previous TAC for simulation x

  if(Jratio > 0.6 & Jratio < 1.4)){           # If Jratio is greater than 0.6 and less than 1.4

    TAC = previousTAC                         # No change in TAC

  }else if(Jratio < 0.6){                     # If Jratio is less than 0.6

    TAC = previousTAC * 0.9                   # New TAC is 10% lower than previous TAC

  }else{                                      # If Jratio is greater than 1.4

    TAC = previousTAC * 1.1                   # New TAC is 10% greater than previous TAC

  }

  TAC                                         # Last line of MP is the TAC recommendation

}

class(EMP1) = "MP"                            # Assign EMP1 a class 'MP'




EMP2 <- function(x, dset,                     # Calculate TAC from simulated data dset for simulation x
                 IndexNo = 11, Jtarg = 0.6,   # Index is #11, (GOM_LAR_SUV), target index level is 0.6
                 lup = 0.05, ldown = 0.15,    # TAC change fraction of slope in index
                 pup = 0.05, pdown = 0.15){   # TAC change fraction of ratio of recent index to Jtarg

  ny = dim(dset$Iobs)[3]                       # Last year of index observations
  Ind = dset$Iobs[x,1,(-5:0)+ny]               # Last six years of index observations

  linmod = lm(y~x,data=data.frame(y = log(Ind), x = 1:6))
  slp = linmod$coefficients[2] # log-linear slope in index

  Jratio = mean(dset$Iobs[x, IndexNo, (-4:0)+ny]) / Jtarg  # Ratio of recent Index / Jtarg

  cury = dim(dset$TAC)[2]                      # Last year of past TAC recommendations
  previousTAC = dset$TAC[x, cury]              # Get previous TAC for simulation x

  if(slp > 0){                                 # If index slope is positive
    smod = lup*slp
  }else{                                       # If index slope is negative
    smod = ldown*slp
  }

  if(Jratio > 1){                              # If recent mean Index is greater than Jtarg
    Jmod = pup*(Jratio-1)
  }else{                                       # If recent mean Index is less than Jtarg
    Jmod = pdown*(Jratio-1)
  }

  Tmod<-Jmod+smod

  if(Tmod>0.15)Tmod=0.15
  if(Tmod<(-0.15))Tmod=-0.15

  oldTAC*(1+Tmod)

}
class(EMP2e)<-"MP"
















