#Carichiamo funzione
source('Split_Conformal.R')
load('test_data.rdata')

formula=co2_mat ~ individual + interac

#SSP1 - Vanilla 

set.seed(27081991)
newdata1=rep(1,11)

pred1=predictSplit(co2_mat ~ individual + interac,newdata1, alpha=.05,rho=.5)

set.seed(27081991)
newdata1=rep(1,11)

pred2=predictSplit(co2_mat ~ individual + interac,newdata1, alpha=.01,rho=.5)

pred2=pred2[-1,]
rownames(pred2)=c('lwr_01','upr_01')
rownames(pred2)

rownames(pred1)=c('lvl','lwr_05','upr_05')

pred=rbind(pred1,pred2)

pred=data.frame(t(pred))
pred$year=as.numeric(rownames(pred))

ggplot(pred,aes(x=year,y=lvl))+
      geom_line(colour='orange',size=1.25) +
      geom_ribbon(aes(ymin=lwr_05,ymax=upr_05),alpha=.4,fill='orange')+
      geom_ribbon(aes(ymin=lwr_01,ymax=upr_01),alpha=.3,fill='orange')+
      theme(text = element_text(size=20))+
      labs(title='Conformal Prediction Bands',x='Years',y='CO2 Emissions (GT/yr)')
    