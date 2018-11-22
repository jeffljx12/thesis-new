 
library(splines)
library(tidyverse)
library(MASS)
library(nlme)
library(lme4)
 
 
#----------------------- n=30 ----------------------------------#
# sample size
n=500
 
# time points for subject i
common_t=seq(1,30,length.out = 6)
 
# knots of the spline
knots <- quantile(common_t, c(.33, .66)) 
 
 
# fixed efects: coefficients for basis functions, degree of freedom = 5
set.seed(123)
coeff = runif(5)
 
 
sim.dt =data.frame()
 
for (i in 1:n){
  
  # random effects
  # alpha0_i = 0
  # beta0_i = 1.5
  # beta1_i = 1
  
  # random effects alpha0, beta0, beta1
  Sigma = matrix(c(1,0,0,
                     0,1,0,
                     0,0,1),
                      3,byrow=T)
 
   random_effects_i = mvrnorm(n = 1, mu = rep(0, 3), Sigma = Sigma)
 
   alpha0_i = random_effects_i[1]
   beta0_i = random_effects_i[2]
   beta1_i = random_effects_i[3]
 
  # transformed time
  time_i = (common_t-beta0_i)/beta1_i  
  
  # create bspline: 2 interior knots, degree=3, no intercept. Question: knots out of the transformed time range?
  basis= bs(time_i, degree=3,knots=knots ,intercept=F)
  
  # calculate y
  y.spline <- basis %*% coeff +rnorm(n=length(time_i),mean=0,sd=0.1)
  
  # make data
  dt_i=data.frame(id=i,
                  time=common_t,
                  y=y.spline 
  )
  
  sim.dt= rbind(sim.dt,dt_i)
}
 
 
#-------------------------------------------------------------#
 
# this dones not work?
# input.spline=function(time,b1,b2,b3,b4,b5,alpha0,beta0,beta1){
#   
#   spline.coeff = c(b1,b2,b3,b4,b5)
#  
#   basis= bs((time-beta0)/beta1, degree=3,knots=knots ,intercept=F)
#   
#    alpha0 + as.matrix(basis %*% spline.coeff)
#   
#  
# }
 
input.spline=function(time,b1,b2,b3,b4,b5,beta0,beta1){
  
  spline.coeff = c(b1,b2,b3,b4,b5)
  
  basis= bs((time-beta0)/beta1, degree=3,knots=knots ,intercept=F)
  
  t(matrix(rep(1,5),ncol=5) %*% t(spline.coeff*as.matrix(basis)))
  
  
}
 
 
                                                
#Error in chol.default((value + t(value))/2) : 
#the leading minor of order 1 is not positive definite
 
fitNlme = nlme(y~input.spline(time,b1,b2,b3,b4,b5,beta0,beta1),
               data = sim.dt,
               fixed = b1 + b2 + b3 + b4 + b5  ~1,
               random = beta0+beta1 ~1,
               groups = ~ id,
               start = c(coeff) - 0.001
               )
 
 
#--------------------------------------------------------------------#
 
 
 
