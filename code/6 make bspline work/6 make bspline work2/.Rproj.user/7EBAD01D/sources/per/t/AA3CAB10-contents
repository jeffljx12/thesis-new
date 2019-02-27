library(splines)
# function to draw spline using polynomial

#' Title
#'
#' @param t 
#' @param intercept.x 
#' @param intercept.y 
#' @param speed 
#' @param tau 
#' @param degree 
#' @param beta 
#' @param gamma 
#' @param errorTerm 
#'
#' @return
#' @export
#'
#' @examples
splineCurve = function(t,intercept.x = 0,intercept.y=0,speed=0,tau,degree=3,beta,gamma, sd=1,errorTerm =F){
  
  #
  m = length(t)
  k = length(tau) 
  
  # check coefficients length  
  # try stopifnot! 
  if ((degree != length(beta) -1) | (length(tau) != length(gamma)))
  {
    print('check coeffients length')
    
  } else {
    
    # individual t
    t_i = (t-intercept.x)/exp(speed)
    
    # spline
    design.m1 = matrix(rep(1,m),nrow=m)
    for (i in 1:degree)
    {
      design.m1=cbind(design.m1,t_i^i)
    }
    
    design.m2 = matrix(rep(1,m),nrow=m)
    for (i in 1:k)
    {
      design.m2=cbind(design.m2, ((t_i-tau[i])>0)*(t_i-tau[i])^degree)
    }
    design.m2 = design.m2[,-1]
    
    out = intercept.y + design.m1  %*% beta + design.m2  %*% gamma + rnorm(n=length(t_i),mean=0,sd = sd)*errorTerm
    
    return(out)
  }
}

sim.d3k2 = splineCurve(1:20,intercept.x = 0,intercept.y=0,speed=0,tau=c(7.3,14.6),degree=3,beta=c(20,10,7,0.7),gamma=c(0.5,-1),errorTerm=F)
fit.bs=lm(sim.d3k2~bs(1:20,degree=3,knots=c(7.3,14.6),intercept=F))
coefficients(fit.bs)[-1]

# function to draw spline using bsplie

bsplineCurve = function(t,intercept.x = 0,intercept.y=0,speed=0,tau,boundary,degree=3,beta,errorTerm =F){
  
  #
  m = length(t)
  k = length(tau) 
  
  # check coefficients length
  if (length(beta) != degree + length(tau))
  {
    print('check coeffients length')
    
  } else {
    
    # individual t
    t_i = (t-intercept.x)/exp(speed)
    
    # bspline
    out = intercept.y + bs(t_i,degree=degree,knots=tau,
                           Boundary.knots = boundary,
                           intercept=F)%*%beta  +
                           rnorm(n=length(t_i),mean=0,sd = 1)*errorTerm
    
    # 
    # out = intercept.y + bs(t_i,degree=3,df=length(beta),intercept=F)%*%beta  + 
    #   rnorm(n=length(t_i),mean=0,sd = 1)*errorTerm
    
    return(out)
  }
}

#bs(1:20,degree=3,knots=c(7.3,14.6),intercept=T) %*% c(100:105)
#bsplineCurve(1:20,intercept.x = 0,intercept.y=0,speed=0,tau=c(7.3,14.6),degree=3,beta=c(100:105),errorTerm=F)
