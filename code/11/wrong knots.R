library(splines)
library(tidyverse)
library(MASS)
library(nlme)
library(lme4)

source('curve function.r')

# fixed effects coeff
sp.coeff1 = c(10,5,3,0.7,0.5)
cov.coeff1 = c(0.7,0.5,-0.4,-1.2,0.4)  # transformed time should not be too different from the original in terms of scale

# -------------------- make data ----------------------#
makeDataset = function(n=400,m=8,seed=533621,tau=c(7.3,14.6),sp.coeff,cov.coeff) {
  
  set.seed(seed)
  sim.dt =data.frame()
  
  # common time
  time = (seq(1,20,length.out = m))
  #time= time/sd(time)
  
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
    r1_i = random_effects[i,1]*300
    r2_i = random_effects[i,2]
    r3_i = random_effects[i,3]
    
    # random effects: random slope for covariates
    # cov.coeff.r1 = random_effects2[i,1] 
    # cov.coeff.r2 = random_effects2[i,2]
    
    # transformed time: speed = random.intercept + fix*cov
    intercept.y = r1_i
    #intercept.y =  0
    intercept.x = r2_i
    speed = r3_i + cov.coeff[1] * covar1[i] + 
      cov.coeff[2] * covar2[i] +
      cov.coeff[3] * covar3[i] +
      cov.coeff[4] * covar4[i] +
      cov.coeff[5] * covar5[i] 
    
    transformedT = time*exp(speed) - intercept.x
    
    # calculate outcome:
    
    # degree = 3, 2 knot2
    spline_d2k2 = splineCurve(transformedT,tau=tau,degree=2,spline.coeff = sp.coeff)
    
    outcome = spline_d2k2 + intercept.y + rnorm(n=length(time),mean=0,sd=1)
    
    # make data
    dt_i=data.frame(id=i,
                    age=time ,
                    speed=speed,
                    var1 = covar1[i],
                    var2 = covar2[i],
                    var3 = covar3[i],
                    var4 = covar4[i],
                    var5 = covar5[i],
                    r1_i = r1_i,
                    r2_i = r2_i,
                    r3_i = r3_i,
                    transformedT =  transformedT,
                    spline = spline_d2k2,
                    outcome = outcome
                    
                    
    )
    
    sim.dt = rbind(sim.dt,dt_i)
  }
  return(sim.dt)
  
}

sim.dt = makeDataset(sp.coeff=sp.coeff1,cov.coeff=cov.coeff1)



#----------------------------------- run nlme -----------------------------------------#

# build model
d2k2_model = function(time,beta0,beta1,beta2,beta3,beta4,
                      ra1_i,ra2_i,ra3_i,tau1,tau2,
                      covar1,covar2,covar3,covar4,covar5,
                      cov.coef1,cov.coef2,cov.coef3,cov.coef4,cov.coef5
){
  
  
  intercept.x = ra2_i
  
  speed = ra3_i + cov.coef1 *covar1 +
    cov.coef2 *covar2 +
    cov.coef3 *covar3 +
    cov.coef4 *covar4 +
    cov.coef5 *covar5
  
  time_i = time*exp(speed) - intercept.x
  
  return( ra1_i + beta0 + beta1*time_i + beta2*time_i^2 +
            beta3*ifelse(time_i-tau1>0,(time_i-tau1)^2,0) + beta4*ifelse(time_i-tau2>0,(time_i-tau2)^2,0)
  )
  
}


# run nlme
rm(nlme_d2k2)


# almost true value
start.true =c(beta0 = 9, beta1 =4,beta2 = 2,beta3 = 0.8,beta4 = 0.4,
              cov.coef1 = 0.6,cov.coef2 = 0.4, cov.coef3 = -0.5,cov.coef4 = -1,cov.coef5 = 0.3)

nlme_d2k2 = nlme(outcome ~ d2k2_model(time = age,
                                      beta0,beta1,beta2,beta3,beta4,
                                      ra1_i, ra2_i,ra3_i,
                                      tau1=10,tau2=17,
                                      covar1 = var1,covar2 = var2,covar3 = var3,covar4 = var4,covar5 = var5,
                                      cov.coef1, cov.coef2, cov.coef3, cov.coef4, cov.coef5
),

data = sim.dt,

fixed = beta0 + beta1 + beta2 + beta3 + beta4  + 
  cov.coef1 + cov.coef2 + cov.coef3 + cov.coef4 + cov.coef5 ~1,

random = ra1_i + ra2_i + ra3_i  ~ 1 , 
group = ~id,

start = start.true,

control = nlmeControl(pnlsTol = 0.01, # not using default
                      msMaxIter =50,
                      msVerbose = TRUE),



na.action = na.omit)

summary(nlme_d2k2)

# compare with true fixed effect
nlme_d2k2$coefficients$fixed
sp.coeff1
cov.coeff1