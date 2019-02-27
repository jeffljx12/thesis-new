library(splines)
library(tidyverse)
library(MASS)
library(nlme)
library(lme4)

source('curve function.r')

# fixed effects coeff
sp.coeff1 = c(20,10,7,0.7,0.5,-1)
cov.coeff1 = c(0.7,0.5,-0.4,-1.2,0.4)  # transformed time should not be too different from the original in terms of scale

# -------------------- make data ----------------------#
makeDataset = function(n=400,m=40,seed=12534,tau=c(7.3,14.6),sp.coeff,cov.coeff) {
  
  set.seed(seed)
  sim.dt =data.frame()
  
  # common time
  time = seq(1,20,length.out = m)
  
  # add time-indep covariates for all n subjects 
  covar1 = runif(n)
  covar2 = runif(n)
  covar3 = runif(n)
  covar4 = runif(n)
  covar5 = sample(0:1,size=n, replace=T)
  
  # knots
  #tau1 = time[ceiling(m/3)]
  #tau2 = time[ceiling(m/3*2)]
  
  # random effects: random intercept alpha0, beta0, beta1
  commom_corr = 0.3
  
  Sigma = 0.1*matrix(c(1,commom_corr,commom_corr,
                      commom_corr,1,commom_corr,
                      commom_corr,commom_corr,1),
                    3,byrow=T)
  
  random_effects = mvrnorm(n = n, mu = rep(0, 3), Sigma = Sigma, empirical = TRUE)
  
  # # random effects: random slope for covariates
  # commom_corr2 = 0.4
  # 
  # Sigma2 = 0.1*matrix(c(1,commom_corr2,
  #                     commom_corr2,1),
  #                     2,byrow=T)
  # 
  # random_effects2 = mvrnorm(n = n, mu = rep(0, 2), Sigma = Sigma2, empirical = TRUE)
  
  for (i in 1:n){
    
    # random effects: random intercept alpha0, beta0, beta1
    r1_i = random_effects[i,1]*500
    r2_i = random_effects[i,2]
    r3_i = random_effects[i,3]
    
    # random effects: random slope for covariates
    # cov.coeff.r1 = random_effects2[i,1] 
    # cov.coeff.r2 = random_effects2[i,2]
    
    # transformed time: speed = random.intercept + fix*cov
    intercept.y = r1_i
    intercept.x = r2_i
    speed = r3_i + cov.coeff[1] * covar1[i] + 
                   cov.coeff[2] * covar2[i] +
                   cov.coeff[3] * covar3[i] +
                   cov.coeff[4] * covar4[i] +
                   cov.coeff[5] * covar5[i] 
   
    # calculate outcome:
    
    # degree = 3, 2 knot2
    spline_d3k2 = splineCurve(time,intercept.x = intercept.x,intercept.y=intercept.y,speed=speed,
                              tau=tau,degree=3,beta=c(sp.coeff[1],sp.coeff[2],sp.coeff[3],sp.coeff[4]),
                              gamma=c(sp.coeff[5],sp.coeff[6]),errorTerm=T)

    
    # make data
    dt_i=data.frame(id=i,
                    age=time ,
                    var1 = covar1[i],
                    var2 = covar2[i],
                    var3 = covar3[i],
                    var4 = covar4[i],
                    var5 = covar5[i],
                    r1_i = r1_i,
                    r2_i = r2_i,
                    r3_i = r3_i,
                    transformedT = time*exp(speed)-intercept.x,
                    outcome = spline_d3k2
                    
                    
    )
    
    sim.dt = rbind(sim.dt,dt_i)
  }
  return(sim.dt)
  
}

sim.dt = makeDataset(sp.coeff=sp.coeff1,cov.coeff=cov.coeff1)

