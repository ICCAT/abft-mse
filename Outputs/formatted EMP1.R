

Const_Cur_TAC = function(x, dset){

  dset$TAC[x, 1]

}

class(Const_Cur_TAC) = "MP"




MeanCat <- function(x, dset){

  mean(dset$Cobs[x, ])

}

class(MeanCat) = "MP"




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

class(EMP1) = "MP"                             # Assign EMP1 a class 'MP'
