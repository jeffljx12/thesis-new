
library(splines)
library(tidyverse)
library(MASS)
library(nlme)


# use Jenss-Bayley model instead of spline


# -------------------- make data ----------------------#
n=50

time = seq(1,20,length.out = 6)

sim.dt =data.frame()

for (i in 1:n){
  
  # random effects alpha0, beta0, beta1
  Sigma = matrix(c(1,0,0,
                   0,1,0,
                   0,0,1),
                 3,byrow=T)
  
  random_effects_i = mvrnorm(n = 1, mu = rep(0, 3), Sigma = Sigma)
  
  r1_i = random_effects_i[1]
  r2_i = random_effects_i[2]
  r3_i = random_effects_i[3]
  
  # fixed effects
  b1 = 52
  b2 = 12
  b3 = -20
  gamma = -1.5
  
  # fixed + random
  b_1i = b1+r1_i
  b_2i = b2+r2_i
  b_3i = b3+r3_i
  
  # calculate y (Jenss-Bayley model)
  y= b_1i+b_2i*(time/12)+b_3i*(exp(gamma*(time/12))-1) + rnorm(n=length(time),mean=0,sd=0.5)
  

  # make data
  dt_i=data.frame(id=i,
                  age=time ,
                  y=y
  )
  
  sim.dt = rbind(sim.dt,dt_i)
}


# -------------------- run nlme ----------------------#

test.nlme <- nlme(y~b_1i+b_2i*(age/12)+b_3i*(exp(gamma*(age/12))-1),
                     data = sim.dt,
                     fixed = b_1i+b_2i+b_3i+gamma~1,
                     random = b_1i+b_2i+b_3i~1,
                     groups = ~ id,
                     start = c(50, 10, ???18, ???2),
                     na.action = na.omit)

summary(test.nlme)
