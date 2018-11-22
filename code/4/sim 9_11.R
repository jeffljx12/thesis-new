
library(splines)
library(tidyverse)
library(MASS)
library(nlme)

# -------------------- make data ----------------------#
set.seed(123)
# number of subjects
n = 50

# number of visits
m = 20

# common time
time = seq(1,20,length.out = m)

# knots
tau1 = time[ceiling(m/3)]
tau2 = time[ceiling(m/3*2)]

# fixed effects
b0 = 20
b1 = 10
b2 = 5
b3 = 1

sim.dt =data.frame()
commom_corr = 0.3 #  corr increase may cause failure

Sigma = matrix(c(1,commom_corr,commom_corr,
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
  spline_d2k1 = b0+b1*time_i+b2*time_i^2+b3*ifelse(time_i-tau1>0,(time_i-tau1)^2,0) + rnorm(n=length(time_i),mean=0,sd = 1)
  
  bspline_d2k1 =  bs(time_i,knots = tau1,degree=2, intercept=T) %*% c(b0,b1,b2,b3) + rnorm(n=length(time_i),mean=0,sd = 1)
  
  
  # make data
  dt_i=data.frame(id=i,
                  age=time ,
                  transformedT = time_i,
                  y.spline_d1k1 = spline_d1k1,
                  y.bspline_d1k1= bspline_d1k1,
                  y.spline_d2k1 = spline_d2k1,
                  y.bspline_d2k1 = bspline_d2k1
                  
  )
  
  sim.dt = rbind(sim.dt,dt_i)
}


#---------------------------------- nlme (assume knots are known) --------------------------_#
# has to specify the value of tau1, cannot use 'tau1' directly?

rm(nlme_d1k1)
tau1
nlme_d1k1 <- nlme(y.spline_d1k1 ~ beta0+
                                beta1*((age-ra2_i)/exp(ra3_i))+
                                beta2*ifelse((age-ra2_i)/exp(ra3_i)-7 > 0,((age-ra2_i)/exp(ra3_i)-7),0),
                  
                  data = sim.dt,
                  fixed = beta0 + beta1 + beta2 ~1,
                  random = ra2_i+ra3_i~1,
                  groups = ~ id,
                  start = c(18, 8,3),
                  na.action = na.omit,control = nlmeControl(pnlsTol = 0.001,msVerbose = T))

summary(nlme_d1k1)

plot(sim.dt$y.spline_d1k1 ~fitted(nlme_d1k1) )

rm(nlme_d2k1)
nlme_d2k1 <- nlme(y.spline_d2k1 ~ ra1_i + beta0+
                    beta1*((age-ra2_i)/exp(ra3_i))+
                    beta2*((age-ra2_i)/exp(ra3_i))^2+
                    beta3*ifelse((age-ra2_i)/exp(ra3_i)-7.3 > 0,((age-ra2_i)/exp(ra3_i)-7.3)^2,0),
                  
                  data = sim.dt,
                  fixed = beta0 + beta1 + beta2 +beta3~1,
                  random = ra1_i + ra2_i+ra3_i~1,
                  groups = ~ id,
                  start = c(18, 8,3,0.8),
                  na.action = na.omit)

summary(nlme_d2k1)
plot(sim.dt$y.spline_d2k1 ~ fitted(nlme_d2k1))

# estimates for fixed effect are alright, corr for random effects are not.
# however the fitted values look good
# corr =0 d1k1 error, corr=0.3, both work
#----------------------------------------------------------------------#

  # try b-spline b1k1,  error
input.spline_d1k1=function(time,beta0,beta1,beta2,ra2_i,ra3_i){
  
  spline.coeff = c(beta0,beta1,beta2)
  
  basis= bs((time-ra2_i)/exp(ra3_i), degree=1,knots=7.3 ,intercept=T)
  
  t(matrix(rep(1,3),ncol=3) %*% t(spline.coeff*as.matrix(basis)))
  
  
}



nlme_bsd1k1 = nlme(y.bspline_d1k1~input.spline_d1k1(age,beta0,beta1,beta2,ra2_i,ra3_i),
               data = sim.dt,
               fixed = beta0 + beta1 + beta2  ~1,
               random = ra2_i+ra3_i ~1,
               groups = ~ id,
               start = c(18, 8,3)
)


# try b-spline b2k1,  error
input.spline_d2k1=function(time,beta0,beta1,beta2,beta3,ra2_i,ra3_i){
  
  spline.coeff = c(beta0,beta1,beta2,beta3)
  
  basis= bs((time-ra2_i)/exp(ra3_i), degree=2,knots=7.3 ,intercept=T)
  
  t(matrix(rep(1,4),ncol=4) %*% t(spline.coeff*as.matrix(basis)))
  
  
}



nlme_bsd2k1 = nlme(y.bspline_d2k1~input.spline_d2k1(age,beta0,beta1,beta2,beta3,ra2_i,ra3_i),
                   data = sim.dt,
                   fixed = beta0 + beta1 + beta2+beta3  ~1,
                   random = ra2_i+ra3_i ~1,
                   groups = ~ id,
                   start = c(18, 8,3,0.9)
)



