library(data.table)
library(roahd)
library(mvtnorm)


##generate bivariate data points
syndata=rmvnorm(1000, sigma=matrix(c(1,.3,.3,1),nrow = 2,ncol=2, byrow = T))
plot(syndata)


#compute depth
dpt=MBD(syndata,manage_ties = T)

df=data.frame(x=syndata[,1],y=syndata[,2],dpt=evenbins(dpt,15))

p=ggplot(df,aes(x=x,y=y,color=dpt)) + 
    geom_point() + 
    scale_fill_continuous() + 
    labs(x='X',y='X',title='MBD - Normal Distribution, .3 corr')

ggsave('bivariate experiments/MBD_normal.pdf',width = 10,height = 10)

#let's see it on a grid

dpt=BD(syndata)

df=data.frame(x=syndata[,1],y=syndata[,2],dpt=evenbins(dpt,15))

ggplot(df,aes(x=x,y=y,color=dpt)) + 
  geom_point() + 
  scale_fill_continuous() + 
  labs(x='X',y='X',title='BD - Normal Distribution, .3 corr')

ggsave('bivariate experiments/BD_normal.pdf',width = 10,height = 10)

#hall-tajvidi 
dpt=syndata[,1]^2 + syndata[,2]^2

df=data.frame(x=syndata[,1],y=syndata[,2],dpt=evenbins(dpt,15))

ggplot(df,aes(x=x,y=y,color=dpt)) + 
  geom_point() + 
  scale_fill_continuous() + 
  labs(x='X',y='X',title='Hall-Tajvidi - Normal Distribution, .3 corr')

ggsave('bivariate experiments/Hall-Tajvidi_normal.pdf',width = 10,height = 10)

#mahalanobis
dpt=mahalanobis(syndata,c(0,0),cov = matrix(c(1,.3,.3,1),nrow = 2,ncol=2, byrow = T))

df=data.frame(x=syndata[,1],y=syndata[,2],dpt=evenbins(dpt,15))

ggplot(df,aes(x=x,y=y,color=dpt)) + 
  geom_point() + 
  scale_fill_continuous() + 
  labs(x='X',y='X',title='mahalanobis - Normal Distribution, .3 corr')

ggsave('bivariate experiments/mahalanobis_normal.pdf',width = 10,height = 10)


#manhattan
dpt=syndata[,1]+syndata[,2]

df=data.frame(x=syndata[,1],y=syndata[,2],dpt=evenbins(dpt,15))

ggplot(df,aes(x=x,y=y,color=dpt)) + 
  geom_point() + 
  scale_fill_continuous() + 
  labs(x='X',y='X',title='manhattan - Normal Distribution, .3 corr')

ggsave('bivariate experiments/manhattan_normal.pdf',width = 10,height = 10)










#####################################
evenbins <- function(x, bin.count=10, order=T) {
  bin.size <- rep(length(x) %/% bin.count, bin.count)
  bin.size <- bin.size + ifelse(1:bin.count <= length(x) %% bin.count, 1, 0)
  bin <- rep(1:bin.count, bin.size)
  if(order) {    
    bin <- bin[rank(x,ties.method="random")]
  }
  return(factor(bin, levels=1:bin.count, ordered=order))
}
                 

