library(splines)
# function to draw spline using polynomial


splineCurve = function(t,tau,degree,spline.coeff){
  
  #
  m = length(t)
  k = length(tau) 
  
  # spline
  design.m = matrix(rep(1,m),nrow=m)
  
  for (i in 1:degree)
  {
    design.m=cbind(design.m,t^i)
  }
  
  
  for (i in 1:k)
  {
    design.m=cbind(design.m, ((t-tau[i])>0)*(t-tau[i])^degree)
  }
  
    
    out = design.m  %*% spline.coeff
    
    return(out)
  
}

#sim.d3k2 = splineCurve(1:20,tau=c(7.3,14.6),degree=3,spline.coeff=c(20,10,7,0.7,0.5,-1))


# function to draw spline using bsplie

# bsplineCurve = function(t,intercept.x = 0,intercept.y=0,speed=0,tau,boundary,degree=3,beta,errorTerm =F){
#   
#   #
#   m = length(t)
#   k = length(tau) 
#   
#   # check coefficients length
#   if (length(beta) != degree + length(tau))
#   {
#     print('check coeffients length')
#     
#   } else {
#     
#     # individual t
#     t_i = (t-intercept.x)/exp(speed)
#     
#     # bspline
#     out = intercept.y + bs(t_i,degree=degree,knots=tau,
#                            Boundary.knots = boundary,
#                            intercept=F)%*%beta  +
#                            rnorm(n=length(t_i),mean=0,sd = 1)*errorTerm
#     
#     # 
#     # out = intercept.y + bs(t_i,degree=3,df=length(beta),intercept=F)%*%beta  + 
#     #   rnorm(n=length(t_i),mean=0,sd = 1)*errorTerm
#     
#     return(out)
#   }
# }
# 
# #bs(1:20,degree=3,knots=c(7.3,14.6),intercept=T) %*% c(100:105)
# #bsplineCurve(1:20,intercept.x = 0,intercept.y=0,speed=0,tau=c(7.3,14.6),degree=3,beta=c(100:105),errorTerm=F)
