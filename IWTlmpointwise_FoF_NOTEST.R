# uso:
# IWTlmpointwise(formula)
# formula: oggetto di tipo formula. 
# Esempio con due covariate: IWTlmFoF(y ~ x1 + x2)
# dove y è un dataset funzionale (matrice n*p)
# x1 e x2 possono essere funzionali (matrici n*p) o scalari (vettori n)
# il test dell'interazione tra covariate scalari e funzionali (tipo y ~ x1*x2) non funziona, per ora va fatto a mano costruendo le covariate dummy
IWTlmFoF_notest <- function(formula,B=1000,method='residuals'){
  
  extract.residuals = function(regr){
    return(regr$residuals)
  }
  extract.fitted = function(regr){
    return(regr$fitted)
  }
  extract.coeff = function(regr){
    return(regr$coefficients)
  }
  env <- environment(formula)
  cl <- match.call()
  design.matrix.temp = model.matrix(formula)
  mf = model.frame(formula)
  data = model.response(mf)
  coeff <- data
  
  #nvar = dim(design.matrix.temp)[2] - 1
  variables = attr(terms(formula),"term.labels") #all.vars(formula)[-1] #colnames(design.matrix.temp)
  y.name = all.vars(formula)[1]
  n <- dim(data)[1]
  J <- dim(data)[2]
  
  assign <- attr(design.matrix.temp,'assign')
  contrast <- attr(design.matrix.temp,'contrast')
  
  length.vars <- numeric(length(variables)+1)
  for(var in 0:(length(variables))){
    length.vars[var+1] <- sum(assign==var)
  }
  
  nvar <- sum(length.vars==J) + sum(length.vars[which(length.vars!=J)]) - 1 
  var.functional <- which(length.vars==J) - 1
  var.scalar <- which(length.vars!=J) - 1
  index.scalar <- NULL
  for(ii in var.scalar){
    index.scalar <- c(index.scalar,which(assign==ii))
  }
  #index.functional <- matrix(data=(1:dim(design.matrix.temp)[2])[-index.scalar], nrow=length(var.functional),ncol=J,byrow=TRUE)
  
  if(length(index.scalar)>0){
    index.functional <- matrix(data=(1:dim(design.matrix.temp)[2])[-index.scalar], nrow=length(var.functional),ncol=J,byrow=TRUE)
  }else{
    index.functional <- matrix(data=(1:dim(design.matrix.temp)[2]), nrow=length(var.functional),ncol=J,byrow=TRUE)
  }
  
  var.names.scalar = colnames(design.matrix.temp)[index.scalar]
  var.names.functional <- variables[var.functional]
  var.names <- c(var.names.scalar,var.names.functional)
  design.matrix <- list(J)
  for(jj in 1:J){
    design.matrix.scalar <- design.matrix.temp[,index.scalar]
    design.matrix.functional <- design.matrix.temp[,index.functional[,jj]]
    design.matrix[[jj]] <- list()
    design.matrix[[jj]]$design.matrix <- cbind(design.matrix.scalar,design.matrix.functional)
    design.matrix[[jj]]$y <- coeff[,jj]
  }
  
  ############################################################
  
  print('First step: basis expansion')
  #splines coefficients:
  coeff <- eval <- data
  p <- dim(coeff)[2]
  
  data.eval <- coeff
  

  #univariate permutations
  #regr0old = lm.fit(design.matrix[[1]],coeff)
  lm.fit.mod <- function(X,var.keep){
    x=X$design.matrix[,var.keep,drop = F]
    return(lm.fit(x,y=X$y))
  }
  regr0 <- lapply(design.matrix,lm.fit.mod,var.keep=1:(nvar+1))
  
 
  
  #calculate residuals
  if(method=='residuals'){
    #n residuals for each coefficient of basis expansion (1:p) 
    #and for each partial test + global test (nvar+1) 
    #saved in array of dim (nvar+1,n,p)
    #formula.const <- deparse(formula[[3]],width.cutoff = 500L) #extracting the part after ~ on formula. this will not work if the formula is longer than 500 char
    #design.matrix.names2 = design.matrix
    var.names2 = var.names
    
    residui = array(dim=c(nvar+1,n,p))
    fitted_part = array(dim=c(nvar+1,n,p)) 
    regr0_part = vector('list',nvar+1)
    #coeff.perm_part = array(dim=c(nvar+1,n,p))
    for(ii in 1:(nvar+1)){ #the first one is the intercept. treated as special case after loop
      var.ii = var.names2[ii]
      variables.reduced = var.names2[-c(1,which(var.names2==var.ii))] 
      index.keep <- (1:(nvar+1))[-ii]
      
      regr0_part[[ii]] = lapply(design.matrix,lm.fit.mod,var.keep=index.keep)
      
      residui[ii,,] = simplify2array(lapply(regr0_part[[ii]],extract.residuals))
      fitted_part[ii,,] = simplify2array(lapply(regr0_part[[ii]],extract.fitted))
    }
  }
  
  
  
  
  
  coeff.regr = sapply(regr0,extract.coeff)
  coeff.t <- ((coeff.regr))
  
  fitted.regr = sapply(regr0,extract.fitted)
  fitted.t <- (fitted.regr)
  

  rownames(coeff.t) = var.names
  rownames(coeff.regr) = var.names

  
  residuals.t = data.eval - fitted.t
  ybar.t = colMeans(data.eval)
  npt <- J
  R2.t = colSums((fitted.t - matrix(data=ybar.t,nrow=n,ncol=npt,byrow=TRUE))^2)/colSums((data.eval - matrix(data=ybar.t,nrow=n,ncol=npt,byrow=TRUE))^2)

  ITPresult <- list(call=cl,basis='B-spline',
                    data.eval=data.eval,
                    coeff.regr.eval=coeff.t,
                    fitted.eval=fitted.t,
                    residuals.eval=residuals.t,
                    R2.eval=R2.t)

  class(ITPresult) = 'IWTlm'
  return(ITPresult)
}




