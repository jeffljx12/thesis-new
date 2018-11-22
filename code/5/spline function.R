
# function to draw spline

simCurve = function(t,intercept.x = 0,intercept.y=0,speed=0,tau,degree=3,beta,gamma, errorTerm =F){
  
  #
  m = length(t)
  k = length(tau) 
  
  # check coefficients length
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
  
  out = intercept.y + design.m1  %*% beta + design.m2  %*% gamma + rnorm(n=length(t_i),mean=0,sd = 1)*errorTerm

  return(out)
  }
}

sim.d3k2 = simCurve(1:20,intercept.x = 0,intercept.y=0,speed=0,tau=c(7.3,14.6),degree=3,beta=c(20,10,7,0.7),gamma=c(0.5,-1),errorTerm=F)

# test bs
?bs
library(splines)
lm(sim.d3k2 ~ bs(1:20,intercept=T, knots=c(7.3,14.6),degree=3))

bs(1:20,intercept=T, knots=c(7.3,14.6),degree=3)

