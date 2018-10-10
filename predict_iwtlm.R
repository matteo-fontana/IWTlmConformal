predict.IWTlm=function(IWTobj,new_data){
  forecast= new_data %*% IWTobj$coeff.regr.eval
  return(forecast)
}
