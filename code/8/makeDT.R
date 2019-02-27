library(splines)
library(tidyverse)
library(MASS)
library(nlme)
library(lme4)

source('curve function.r')

# -------------------- make data ----------------------#
makeDataset = function(n=50,m=40,seed=123,tau=c(7.3,14.6)) {
  
  set.seed(seed)
  sim.dt =data.frame()
  
  # common time
  time = seq(1,20,length.out = m)
  
  # add time-indep covariates for all n subjects 
  continuous.cov1 = runif(n)
  continuous.cov2 = runif(n)
  
  # knots
  #tau1 = time[ceiling(m/3)]
  #tau2 = time[ceiling(m/3*2)]
  
  # fixed effects: spline coeff
  b0 = 20
  b1 = 10
  b2 = 7
  b3 = 0.7
  b4= 0.5
  b5= -1
  
  # fixed effects: covariate coeff (in order to maintain small changes to time, these coeffs should not be big?)
  cov.coeff.f1=5
  cov.coeff.f2=0.1
  
 
  # # random effects: random intercept alpha0, beta0, beta1
  commom_corr = 0.3
  
  Sigma = 0.1*matrix(c(1,commom_corr,commom_corr,
                      commom_corr,1,commom_corr,
                      commom_corr,commom_corr,1),
                    3,byrow=T)
  
  random_effects = mvrnorm(n = n, mu = rep(0, 3), Sigma = Sigma, empirical = TRUE)
  
  # random effects: random slope for covariates
  commom_corr2 = 0.4
  
  Sigma2 = 0.1*matrix(c(1,commom_corr2,
                      commom_corr2,1),
                      2,byrow=T)
  
  random_effects2 = mvrnorm(n = n, mu = rep(0, 2), Sigma = Sigma2, empirical = TRUE)
  
  for (i in 1:n){
    
    # random effects: random intercept alpha0, beta0, beta1
    r1_i = random_effects[i,1]*500
    r2_i = random_effects[i,2]
    r3_i = random_effects[i,3]
    
    # random effects: random slope for covariates
    cov.coeff.r1 = random_effects2[i,1] 
    cov.coeff.r2 = random_effects2[i,2]
    
    # parameters for transformed time: alpha/beta = random.intercept + fix*cov+random.slope*cov
    intercept.y = r1_i
    intercept.x = r2_i + continuous.cov1[i] * cov.coeff.f1 #+ continuous.cov1[i] * cov.coeff.r1
    speed = r3_i + continuous.cov2[i] * cov.coeff.f2 #+ continuous.cov2[i] * cov.coeff.r2
   
    # calculate outcome:
    
    # degree = 3, 2 knot2
    spline_d3k2 = splineCurve(time,intercept.x = intercept.x,intercept.y=intercept.y,speed=speed,
                              tau=tau,degree=3,beta=c(b0,b1,b2,b3),gamma=c(b4,b5),errorTerm=T)

    
    # make data
    dt_i=data.frame(id=i,
                    #tau = tau1*exp(r3_i)+r2_i,
                    age=time ,
                    var1 = continuous.cov1[i],
                    var2 = continuous.cov2[i],
                    transformedT = (time-intercept.x)/exp(speed),
                    y.spline_d3k2 = spline_d3k2
                    
                    
    )
    
    sim.dt = rbind(sim.dt,dt_i)
  }
  return(sim.dt)
  
}

sim.dt = makeDataset()

