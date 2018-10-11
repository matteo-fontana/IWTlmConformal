
load('test_data.rdata')

require(roahd)
formula = co2_mat ~ individual + interac
alpha=.05


####Here the function code starts

#Define functions for vectorizing operations

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

n <- dim(data)[1]
J <- dim(data)[2]

#Split Dataset:

i1 = sample(1:n,floor(n/2))
i2 = (1:n)[-i1]
n1 = length(i1)
n2 = length(i2)

design.matrix.temp_i1=design.matrix.temp[i1,]




variables = attr(terms(formula),"term.labels") #all.vars(formula)[-1] #colnames(design.matrix.temp)
y.name = all.vars(formula)[1]

assign <- attr(design.matrix.temp,'assign')
contrast <- attr(design.matrix.temp,'contrast')
length.vars <- numeric(length(variables)+1)

for(var in 0:(length(variables))){
  length.vars[var+1] <- sum(assign==var)
}

nvar <- sum(length.vars==J) + sum(length.vars[which(length.vars!=J)]) - 1 

#Risky.
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


#setting up the model is complete.  

############################################################

#splines coefficients:
coeff <- eval <- data.eval <- data
p <- dim(coeff)[2]
#Model fitting
#regr0old = lm.fit(design.matrix[[1]],coeff)
lm.fit.mod <- function(X,var.keep){
  x=X$design.matrix[,var.keep,drop = F]
  return(.lm.fit(x,y=X$y))
}


regr0 <- lapply(design.matrix,lm.fit.mod,var.keep=1:(nvar+1))

#calculate residuals


coeff.regr = sapply(regr0,extract.coeff)
coeff.t <- ((coeff.regr))


####Second part: forecast, then compute abs residuals

i2_data=data[i2,]
i2_design_matrix=design.matrix.temp[i2,]

i2_forecast=i2_design_matrix %*% coeff.t
i2_residuals=i2_data-i2_forecast

abs_i2_residuals=abs(i2_residuals)

#matplot(t(abs_i2_residuals),type='l')

     
fdepth=MBD(abs_i2_residuals)

val=sort(fdepth)[ceiling((n/2)*(1-alpha))]

dt=abs_i2_residuals[which(fdepth==val),]

forecast=newdata %*% coeff.t
lwr=forecast-dt
upr=forecast+dt

prova=rbind(forecast,lwr,upr)
matplot(t(prova),type='l')

