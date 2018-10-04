load('Data/bau_data_smoothed.rdata')

library(tidyverse)
library(fda)
library(stringr)
library(mefa)




#create evaluations for functional data: we start from 2020 (before, just calibration)
#yearly data: we have a statistical downscaling time-wise

year_eval=seq(2020,2090, by=1)
eval_range=15:(15+length(year_eval)-1)
bau_fd_eval=eval.fd(year_eval,bau_fd)

#create the joined dataset
temp=data.frame(t(bau_fd_eval))
colnames(temp)=as.character(year_eval)

bau_covariates_eval=cbind(bau_covariates, temp)
bau_covariates_eval[is.na(bau_covariates_eval$Var_num),]$Var_num=0


#######SSP2-1 transition#######
regr_data=filter(bau_covariates_eval, Variable=='Emissions|CO2|Fossil Fuels and Industry',SSP!='SSP3',Var_num!='3')



co2=select(regr_data,starts_with('2'))
co2_mat=as.matrix(co2)


matplot(t(co2_mat),type='l')

#let's play with data...
fbplot(t(co2_mat))




Y=model_SSP21$data.eval
fit=model_SSP21$fitted.eval

matplot(t(fit),type='l')
model_SSP21$coeff.regr.eval
matplot(t(Y-fit),type='l')
