
library(splines)
library(tidyverse)
library(MASS)
library(nlme)
library(lme4)

# -------------------- make data ----------------------#
makeDataset = function(n=50,m=20,seed=123,tau1=7.3,tau2=14.6) {
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
  commom_corr = 0.3
  
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
    time_i = (time-r2_i)/exp(r3_i)
    
    # calculate outcome:
    
    # degree = 1, 1 knot
    spline_d1k1 = b0+b1*time_i+b2*ifelse(time_i-tau1>0,time_i-tau1,0) + rnorm(n=length(time_i),mean=0,sd = 1)
    
    bspline_d1k1 =  bs(time_i,knots = tau1,degree=1, intercept=T) %*% c(b0,b1,b2) + rnorm(n=length(time_i),mean=0,sd = 1)
    
    # degree = 2, 1 knot
    spline_d2k1 = r1_i*500+b0+b1*time_i+b2*time_i^2+b3*ifelse(time_i-tau1>0,(time_i-tau1)^2,0) + rnorm(n=length(time_i),mean=0,sd = 1)
    
    bspline_d2k1 =  bs(time_i,knots = tau1,degree=2, intercept=T) %*% c(b0,b1,b2,b3)*100 + rnorm(n=length(time_i),mean=0,sd = 1)
    
    # degree = 3, 2 knot2
    spline_d3k2 =  r1_i*500+b0 + b1*time_i + b2*time_i^2 + b3*time_i^3 +
      b4*ifelse(time_i-tau1>0,(time_i-tau1)^3,0) + 
      b5*ifelse(time_i-tau2>0,(time_i-tau2)^3,0) +
      rnorm(n=length(time_i),mean=0,sd = 1)
    
    bspline_d3k2 =  bs(time_i,knots = c(tau1,tau2),degree=3, intercept=T) %*% c(b0,b1,b2,b3,b4,b5)+ rnorm(n=length(time_i),mean=0,sd = 1)
    
    # make data
    dt_i=data.frame(id=i,
                    tau = tau1*exp(r3_i)+r2_i,
                    age=time ,
                    transformedT = time_i,
                    y.spline_d1k1 = spline_d1k1,
                    y.bspline_d1k1= bspline_d1k1,
                    y.spline_d2k1 = spline_d2k1,
                    y.bspline_d2k1 = bspline_d2k1,
                    y.spline_d3k2 = spline_d3k2,
                    y.bspline_d3k2 = bspline_d3k2
                    
    )
    
    sim.dt = rbind(sim.dt,dt_i)
  }
  return(sim.dt)
  
}

sim.dt = makeDataset()