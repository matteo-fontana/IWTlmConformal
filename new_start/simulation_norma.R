#first step: simulate from unif(0.5,2)
library(mefa)
library(roahd)
library(tidyverse)
library(gplots)

t=seq(0,1,by=0.01)

x=2*sin(pi*t)


plot(x)
set.seed(27081991)

#rand=rnorm(20000,mean=1,sd=(1/4))
rand=runif(20000,0.5,2)

x_temp=(rep(1,20000) %*% t(x))

x_data=data.frame(x_temp * rand)
xdatat=t(x_data)
matplot(t,xdatat[,sample(1:20000,100)],type='l',col='goldenrod2',xlab = 't',ylab = 'y(t)', main='Simulated Data')



#compute sample mean
x_data_mean=rowMeans(xdatat)
lines(t,x_data_mean, col='black',lwd=2)

svg('Vovk_Presentation_Simulations/data.svg')
matplot(t,xdatat[,sample(1:20000,100)],type='l',col='goldenrod2',xlab = 't',ylab = 'y(t)', main='Simulated Data')
lines(t,x_data_mean, col='black',lwd=2)
dev.off()

#output svg


#let's start with Conformal Prediction

rho=0.5
alpha=.5

n <- dim(x_data)[1]
J <- dim(x_data)[2]



#Split Dataset:

i1 = sample(1:n,floor(n*rho))
i2 = (1:n)[-i1]
n1 = length(i1)
n2 = length(i2)


#compute sample mean
muhat=colMeans(x_data[i1,])
plot(muhat)

#compute residuals
muhat_mat=rep(data.frame(t(muhat)),n2)
residuals=x_data[i2,]-muhat_mat

matplot(t(residuals)[,sample(1:10000,100)],type='l')

depth=MBD(residuals)

dp_s = sort(depth, decreasing = TRUE)
index = order(depth, decreasing = TRUE)

k = ceiling(n/2 * (1-alpha))
center = residuals[index[1:k],]
inf = apply(center, 2, min)
sup = apply(center, 2, max)

df_pred=data.frame(lvl=muhat,lwr=(muhat+inf),upr=(muhat + sup), x=seq(0,1,by=0.01))


##add some newly generated data

rand1=runif(20,0.5,2)

x_temp1=(rep(1,20) %*% t(x))

x_data1=data.frame(x_temp1 * rand1)

data_dt=melt(data.table(t(x_data1), x=seq(0,1,by=0.01)),id.vars='x')





g1=ggplot(df_pred,aes(x=x,y=lvl))+
  geom_ribbon(aes(ymin=lwr,ymax=upr),alpha=.4,fill='goldenrod2')+
  geom_line(colour='black',size=1.25) +
  ylim(0,4) + 
  geom_line(inherit.aes = F,data = data_dt, mapping = aes(x=x,y=value,group=variable), colour='steelblue4',size=.1)+
  theme_bw() + 
  labs(title='Conformal Prediction Bands - alpha=.5', x='t',y='y(t)')

g1

ggsave('Vovk_Presentation_Simulations/unif_5.svg')




####let's complicate the things some more!


rm(list=ls())

#first step: simulate from unif(0.5,2)

t=seq(0,1,by=0.01)

#more complex x

x= 2*sin(pi*t)


plot(x)
set.seed(27081991)

#rand=rnorm(20000,mean=1,sd=(1/4))
rand=rnorm(20000,mean = 1, sd=.25)

x_temp=(rep(1,20000) %*% t(x))

x_data=data.frame(x_temp * rand)
xdatat=t(x_data)
matplot(t,xdatat[,sample(1:20000,100)],type='l',col='goldenrod2',xlab = 't',ylab = 'y(t)', main='Simulated Data')



#compute sample mean
x_data_mean=rowMeans(xdatat)
lines(t,x_data_mean, col='black',lwd=2)

svg('Vovk_Presentation_Simulations/data_norm.svg')
matplot(t,xdatat[,sample(1:20000,100)],type='l',col='goldenrod2',xlab = 't',ylab = 'y(t)', main='Simulated Data')
lines(t,x_data_mean, col='black',lwd=2)
dev.off()

#output svg


#let's start with Conformal Prediction

rho=0.5
alpha=.05

n <- dim(x_data)[1]
J <- dim(x_data)[2]



#Split Dataset:

i1 = sample(1:n,floor(n*rho))
i2 = (1:n)[-i1]
n1 = length(i1)
n2 = length(i2)


#compute sample mean
muhat=colMeans(x_data[i1,])
plot(muhat)

#compute residuals
muhat_mat=rep(data.frame(t(muhat)),n2)
residuals=x_data[i2,]-muhat_mat

matplot(t(residuals)[,sample(1:10000,100)],type='l')

depth=MBD(residuals)

dp_s = sort(depth, decreasing = TRUE)
index = order(depth, decreasing = TRUE)

k = ceiling(n/2 * (1-alpha))
center = residuals[index[1:k],]
inf = apply(center, 2, min)
sup = apply(center, 2, max)

df_pred=data.frame(lvl=muhat,lwr=(muhat+inf),upr=(muhat + sup), x=seq(0,1,by=0.01))


##add some newly generated data

rand1=rnorm(20,mean = 1, sd=.25)

x_temp1=(rep(1,20) %*% t(x))

x_data1=data.frame(x_temp1 * rand1)

data_dt=melt(data.table(t(x_data1), x=seq(0,1,by=0.01)),id.vars='x')





g1=ggplot(df_pred,aes(x=x,y=lvl))+
  geom_ribbon(aes(ymin=lwr,ymax=upr),alpha=.4,fill='goldenrod2')+
  geom_line(colour='black',size=1.25) +
  ylim(0,4) + 
  geom_line(inherit.aes = F,data = data_dt, mapping = aes(x=x,y=value,group=variable), colour='steelblue4',size=.1)+
  theme_bw() + 
  labs(title='Conformal Prediction Bands - alpha=.05', x='t',y='y(t)')

g1

ggsave('Vovk_Presentation_Simulations/norm_05.svg')







