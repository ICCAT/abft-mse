L1=33.0
L2=270.6
p=−0.12
A1=0
A2=34
K=0.22

age=seq(0,40,1)

#mean size at age
L_ageW = (L1^p+(L2^p-L1^p)*(1-exp(-K*(age-A1)))/(1-exp(-K*(A2-A1))))^(1/p);

#std deviations of size at age
L_var = 5.84 + 0.06*L_ageW; L_var

L_ageE =318.85*(1-exp(-.093*(age + .97)))

plot(L_ageE)
lines(L_ageW,col='red')


maxl<-max(OMI@mulen)
mulen<-OMI@mulen

selpar2<-3
selpar1<--3

spar2=maxl*(0.05+0.85*exp(selpar2)/(1+exp(selpar2)))#;        // Inflection point (2) as a fraction of largest length I(0.1|0.8)
spar1=maxl*(0.001+0.12*(exp(selpar1)/(1+exp(selpar1))))#;    // Logistic slope (1) as fraction of inflection point (2) I(0.01|0.5)

msel=1/(1+exp((spar2-mulen)/spar1));
plot(msel)


#

load("C:/Users/tcar_/Documents/abft-mse/R_package/ABTMSE/inst/ts.Rdata")







