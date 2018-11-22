
library(splines)
library(tidyverse)
library(MASS)
library(nlme)



# -------------------- make data ----------------------#
n=1000

set.seed(12345)


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
  
  # transformed time
  time_i = (time-r2_i)/exp(r3_i)
  
  # calculate y
  y=  r1_i+b1+b2*(time_i/12)+b3*(exp(gamma*(time_i/12))-1) + rnorm(n=length(time_i),mean=0,sd= 0.5)
  
  
  # make data
  dt_i=data.frame(id=i,
                  age=time ,
                  y=y
  )
  
  sim.dt = rbind(sim.dt,dt_i)
}


# -------------------- run nlme ----------------------#
# nls for initial value
startValue = list(b1=50,b2=11,b3=-18,gamma=-2)
nls(y ~ b1+b2*(age/12)+b3*(exp(gamma*age/12)-1),
    data = sim.dt,
    start = startValue)
#b2,b3?

# nlme without first random effect
test.nlme2 <- nlme(y ~ b1+b2*((age-r2_i)/exp(r3_i)/12)+b3*(exp(gamma*(age-r2_i)/exp(r3_i)/12)-1),
                   data = sim.dt,
                   fixed = b1 + b2 + b3 + gamma~1,
                   random = r2_i+r3_i~1,
                   groups = ~ id,
                   start = c(51.66, 19.12, -12.2, -2.8),
                   na.action = na.omit)

test.nlme2


# nlme with all random effects
test.nlme3<- nlme(y ~ r1_i+b1+b2*((age-r2_i)/exp(r3_i)/12)+b3*(exp(gamma*(age-r2_i)/exp(r3_i)/12)-1),
                   data = sim.dt,
                   fixed = b1 + b2 + b3 + gamma~1,
                   random = r1_i + r2_i+r3_i~1,
                   groups = ~ id,
                   start = c(51.66, 19.12, -12.2, -2.8),
                   na.action = na.omit)

test.nlme3

# r1 sd?