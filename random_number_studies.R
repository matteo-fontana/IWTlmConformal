####simulated data generation####

####easy case####
library(mefa)
library(roahd)
library(tidyverse)
library(gplots)

t=seq(0,1,by=0.01)

x=sin(3*pi*t)+cos(5*t*pi)


plot(x)

rand=runif(20000,min = 1,max=2)

x_temp=(rep(1,20000) %*% t(x))

x_data=data.frame(x_temp * rand)
matplot(t(x_data)[,sample(1:20000,100)],type='l')

#very well: now, build functional coefficient
#start with simple and dumb one: linear trend from 0 to 1

beta_f=rep(3,101)
plot(beta_f)
beta_f_one=rep(data.frame(t(beta_f)),10000)

beta_f_zero=rep(data.frame(t(rep(1,101))),10000)

beta_f_mat=rbind(beta_f_one,beta_f_zero)

#now, generate error term

cov_fun=(exp_cov_function(t,5,.5))


eps=data.frame(generate_gauss_fdata(20000,rep(0,101),Cov=cov_fun))


#now, create the data

y_data= beta_f_mat * x_data + eps





matplot(t(y_data)[,sample(1:20000,100)],type='l')

x_regr1=rep(1,10000)
x_regr_temp=rep(0,10000)
x_regr=c(x_regr1,x_regr_temp)#plot the whole thing


source('Split_Conformal.R')

#split in training and test sets
i1=sample(1:20000,10000)
i2=(1:20000)[-i1]

y_data_split=data.frame(y_data[i1,])
x_regr_split=data.frame(x_regr[i1])

formula=y_data_mat ~ x_reg_mat

y_data_mat=as.matrix(y_data_split)
x_reg_mat=as.matrix(x_regr_split)



#test
y_data_mat=as.matrix(y_data)
x_reg_mat=as.matrix(x_regr)

formula=y_data_mat ~ x_reg_mat

set.seed(27081991)
newdata1=c(1,0)

pred1=predictSplit(y_data_mat ~ x_reg_mat ,newdata1, alpha=.10,rho=.5)



#select 100 '1' observations

nobs=10

test_data=y_data[i2,]
x_test=x_reg_mat[i2]

ones=test_data[x_test==0,]
selected=ones[sample(1:length(ones),nobs),]



#plot the whole thing


pred1_df=data.frame(t(pred1))
pred1_df$x=as.vector(seq(1:101))


data_dt=melt(data.table(t(selected), x=seq(1:101)),id.vars='x')



ggplot(pred1_df,aes(x=x,y=lvl))+
  geom_line(colour='orange',size=1.25) +
  geom_ribbon(aes(ymin=lwr,ymax=upr),alpha=.4,fill='orange')+
  geom_line(inherit.aes = F,data = data_dt, mapping = aes(x=x,y=value,group=variable), colour='black',size=.5)+
  theme_bw()+
  labs(title='Conformal Prediction Bands')




#harder case##############

####simulated data generation####

library(mefa)
library(roahd)
library(tidyverse)
library(gplots)

t=seq(0,1,by=0.01)

x=sin(3*pi*t)+cos(5*t*pi)
#x=t*10

plot(x)

rand=rnorm(20000)

x_temp=(rep(1,20000) %*% t(x))

x_data=data.frame(x_temp * rand1)
#matplot(t(x_data_t),type='l')

#very well: now, build functional coefficient
#start with simple and dumb one: linear trend from 0 to 1

beta_f=rep(3,101)
plot(beta_f)
beta_f_one=rep(data.frame(t(beta_f)),10000)

beta_f_zero=rep(data.frame(t(rep(1,101))),10000)

beta_f_mat=rbind(beta_f_one,beta_f_zero)

#now, generate error term

cov_fun=(exp_cov_function(t,5,5))


eps=data.frame(generate_gauss_fdata(20000,rep(0,101),Cov=cov_fun))


#now, create the data

y_data= beta_f_mat * x_data + eps


matplot(t(y_data)[,sample(1:20000,100)],type='l')

x_regr1=rep(1,10000)
x_regr_temp=rep(0,10000)
x_regr=c(x_regr1,x_regr_temp)#plot the whole thing


source('Split_Conformal.R')

#split in training and test sets
i1=sample(1:20000,10000)
i2=(1:20000)[-i1]

y_data_split=data.frame(y_data[i1,])
x_regr_split=data.frame(x_regr[i1])

formula=y_data_mat ~ x_reg_mat

y_data_mat=as.matrix(y_data_split)
x_reg_mat=as.matrix(x_regr_split)



#test
y_data_mat=as.matrix(y_data)
x_reg_mat=as.matrix(x_regr)

formula=y_data_mat ~ x_reg_mat

set.seed(27081991)
newdata1=c(1,0)

pred1=predictSplit(y_data_mat ~ x_reg_mat ,newdata1, alpha=.10,rho=.5)



#select 100 '1' observations

nobs=10

test_data=y_data[i2,]
x_test=x_reg_mat[i2]

ones=test_data[x_test==0,]
selected=ones[sample(1:length(ones),nobs),]



#plot the whole thing


pred1_df=data.frame(t(pred1))
pred1_df$x=as.vector(seq(1:101))


data_dt=melt(data.table(t(selected), x=seq(1:101)),id.vars='x')



ggplot(pred1_df,aes(x=x,y=lvl))+
  geom_line(colour='orange',size=1.25) +
  geom_ribbon(aes(ymin=lwr,ymax=upr),alpha=.4,fill='orange')+
  geom_line(inherit.aes = F,data = data_dt, mapping = aes(x=x,y=value,group=variable), colour='black',size=.1)+
  theme_bw()+
  labs(title='Conformal Prediction Bands')


