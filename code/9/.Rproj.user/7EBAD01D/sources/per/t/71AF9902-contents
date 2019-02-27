#' Title
#'
#' @param model 
#' @param knots 
#' @param data 
#' @param start 
#' @param noInt 
#' @param degree 
#' @param speed 
#'
#' @return
#' @export
#'
#' @examples


sireg = function(model,
                 speed,
                 knots,
                 group,
                 data=NULL,
                 start,
                 noInt=FALSE,
                 degree=3
) {
  # Get the response and time variable names
  tt = terms(model)
  vars = as.character(attr(tt, "variables"))[-1]
  yVarName = vars[attr(tt, "response")] 
  xVarName = setdiff(all.vars(model),yVarName)
  
  if (length(xVarName)!=1) stop('You must specify one time variable in the model')
  if (length(yVarName)!=1) stop('You must specify one response variable in the model')
  
  if (!missing(data)) {
    if (!xVarName %in% names(data)) stop(xVarName,' is not in the dataset')
    if (!yVarName %in% names(data)) stop(yVarName,' is not in the dataset')
    x = data[[xVarName]]
    y = data[[yVarName]]
  }
  else {
    if (!exists(yVarName)) stop(yVarName,' not found')
    if (!exists(xVarName)) stop(xVarName,' not found')
    
    #time = get(xVarName)
    #y = get(yVarName)
    
  }
  
  # Get the group variable name
  groupVar = eval(substitute(group),envir=data)
 
  # Build the design matrix for speed
  D = model.matrix(speed,data=data)
  
  # beta: covariate coefficients
  betas = paste0('beta',(1:(ncol(D)-1)))
  
  # gamma: spline coefficients
 # gammas = paste0('gamma',0:(degree+(length(knots))))
  gammas = paste0('gamma',0:5)
  
  # Model function
  
  # knot1 = 7.3
  # knot2 = 14.6
  # 
  
  # build nlme model 
  f = function(...,time,a0,b0,g0,knot1=7.3,knot2=14.6,gamma0,gamma1,gamma2,gamma3,gamma4,gamma5) {
    
   
    
    beta = c(...)
    
    speed = beta %*% D[,-1] + g0
    
    #speed= beta1*D[,2]+beta2*D[,3]+beta3*D[,4]+beta4*D[,5]+beta5*D[,6]
    
    trans.t = exp(speed)*time - a0
    
    # m = length(trans.t)
    # k = length(knots) 
    
     yhat = b0 + gamma0 + gamma1*trans.t + gamma2*trans.t^2 + gamma3*trans.t^3 +
       gamma4*ifelse(trans.t-knot1>0,(trans.t-knot1)^3,0) + gamma5*ifelse(trans.t-knot2>0,(trans.t-knot2)^3,0)
    
    return(yhat)
    
  }
  
  
 
  nlme.model  = paste0('outcome ~ f (',paste(betas,collapse=', '),',age, a0,b0,g0,gamma0,gamma1,gamma2,gamma3,gamma4,gamma5)')
  fixed.model = paste0(paste(c(betas,gammas),collapse=' + '),'~1')
  
 
  #return(as.formula(nlme.model))
   #return(D[,-1])
  
  start.l = c(gamma0 = 19, gamma1 = 8, gamma2 = 9,gamma3 = 0.8,gamma4 = 0.3,gamma5 = -0.9, 
                        beta0 = 0.5, beta1= 0.4,  beta2 = -0.5, beta3 = -0.9, beta4 = 0.3)
  
  # run nlme  
 fitNlme =  nlme(as.formula(nlme.model),
                 data = data,
                 fixed = as.formula(fixed.model),
                 random = b0 + a0 + g0 ~ 1 |id,
                 #group = groupVar,
                 start = start.l,
                 
                 control = nlmeControl(pnlsTol = 0.01, # not using default
                                       msMaxIter =50,
                                       msVerbose = TRUE),
                 
                 method = "ML", 
                 
                 na.action = na.omit)

 
 return(summary(fitNlme))
}


sireg(outcome ~ age,
      speed = ~ var1 + var2 + var3+ var4+var5,
      knots =c(7.3,14.6),
      group = id,
      data = sim.dt
      
)

 
