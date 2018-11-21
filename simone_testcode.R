#Carichiamo funzione
source('Split_Conformal.R')
load('test_data.rdata')

formula=co2_mat ~ individual + interac

#SSP1 - Vanilla 

set.seed(27081991)
newdata1=rep(1,11)

pred1=predictSplit(co2_mat ~ individual + interac,newdata1, alpha=.05,rho=.5)


#half of GDP
set.seed(27081991)
newdata2=c(1,0,0,.5,0,0,0,0,0,0,0)
pred2=predictSplit(co2_mat ~ individual + interac,newdata2, alpha=.05, rho=.75)


#n is small - high variablity in size and shape: repeat and combine splits (further work)
set.seed(27081991)
newdata3=c(1,0,0,1,0,0,0,0,0,0,0)
pred3=predictSplit(co2_mat ~ individual + interac,newdata2, alpha=.05,rho=.75)


