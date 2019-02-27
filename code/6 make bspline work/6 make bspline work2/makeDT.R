library(splines)
library(tidyverse)
library(MASS)
library(nlme)
library(lme4)

source('curve function.r')
# -------------------- make data ----------------------#
makeDataset = function(n=50,m=20,seed=123,tau=c(7.3,14.6)) {
  set.seed(seed)
  
  # common time
  time = seq(1,20,length.out = m)
  
  # knots
  #tau1 = time[ceiling(m/3)]
  #tau2 = time[ceiling(m/3*2)]
  
  # fixed effects
  b0 = 20
  b1 = 10
  b2 = 7
  b3 = 0.7
  b4= 0.5
  b5= -1
  
  sim.dt =data.frame()
  commom_corr = 0.4
  
  Sigma = .1*matrix(c(1,commom_corr,commom_corr,
                      commom_corr,1,commom_corr,
                      commom_corr,commom_corr,1),
                    3,byrow=T)
  
  random_effects = mvrnorm(n = n, mu = rep(0, 3), Sigma = Sigma, empirical = TRUE)
  
  for (i in 1:n){
    
    # random effects alpha0, beta0, beta1
    r1_i = random_effects[i,1]
    r2_i = random_effects[i,2]
    r3_i = random_effects[i,3]
    
    # transformed time
    #time_i = (time-r2_i)/exp(r3_i)
    
    # calculate outcome:
    
   
    # degree = 3, 2 knot2
    spline_d3k2 = splineCurve(time,intercept.x = r2_i,intercept.y=r1_i,speed=r3_i,
                              tau=tau,degree=3,beta=c(b0,b1,b2,b3),gamma=c(b4,b5),errorTerm=T)

    # construct same curve using bspline 
    bspline_d3k2 =  bsplineCurve(time,intercept.x = r2_i,intercept.y=r1_i,speed=r3_i,
                                 tau=tau,boundary=c(-20,40),degree=3,beta=c(10,20,100,1000,3000),errorTerm=T)
    
    # make data
    dt_i=data.frame(id=i,
                    #tau = tau1*exp(r3_i)+r2_i,
                    age=time ,
                    transformedT = (time-r2_i)/exp(r3_i),
                    y.spline_d3k2 = spline_d3k2,
                    y.bspline_d3k2 = bspline_d3k2
                    
    )
    
    sim.dt = rbind(sim.dt,dt_i)
  }
  return(sim.dt)
  
}

sim.dt = makeDataset()