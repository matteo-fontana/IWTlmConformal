model=IWTlmFoF_notest(formula)


model$coeff.regr.eval

newdata1=c(1,1,0,0,0,0,0,0,0,0,0)
newdata2=c(1,0,1,0,0,0,0,0,0,0,0)
newdata3=c(1,0,0,1,0,0,0,0,0,0,0)

test=rbind(newdata1,newdata2,newdata3)

forecast=test %*% model$coeff.regr.eval

matplot(t(forecast),type='l')


predict(model,test)


