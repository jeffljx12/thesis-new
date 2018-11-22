
# test nlme
#-------------------------------- two fixed effect one random effect -------------------#
n=50

# time points for subject i
common_t=seq(1,20,length.out = 6)

sim.dt2 =data.frame()

for (i in 1:n){
  
 
  
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
  time_i = (common_t-beta0_i)
  #time_i = (common_t-beta0_i)/beta1
  
  
  
  # calculate y
  fixed.b1 =0.3
  fixed.b2= 0.5
  y.spline <-  fixed.b1 *(time_i)+fixed.b1^2 *(time_i)+fixed.b2  + rnorm(n=length(common_t),mean=0,sd=0.1) #+fixed.b2* time_i^2
  
  # make data
  dt_i=data.frame(id=i,
                  time=common_t,
                  y=y.spline 
  )
  
  sim.dt2= rbind(sim.dt2,dt_i)
}

# define model
 model.spline2=function(time,fixed.b1,fixed.b2,beta0){
   
 time_i= time-beta0
 fixed.b1 *(time_i)+fixed.b1^2 *(time_i)+fixed.b2

}

 #fit nlme
 
fitNlme2 = nlme(y ~ model.spline2(time,fixed.b1,fixed.b2,beta0),  #fixed.b1 *(time-beta0)/beta1 ,
                
               data = sim.dt2,
               fixed = fixed.b1 + fixed.b2 ~1,
               random = beta0  ~1,
               groups = ~ id,
               start = c(0.2,0.3),
               verbose=F
               
)

summary(fitNlme2)


# fit lme4::nlmer
# model.spline2.lmer = deriv( ~ fixed.b1 *(time-beta0)+fixed.b1^2 *(time-beta0) + fixed.b2,
#                             namevec=c('fixed.b1','fixed.b2','beta0'),
#                             function.arg=c('time','fixed.b1','fixed.b2','beta0')
# )
# 
# 
# fitLmer2 = nlmer(y ~ model.spline2.lmer(time,fixed.b1,fixed.b2,beta0) ~ beta0|id,
#           data = sim.dt2,
#       start = c(fixed.b1=0.29,fixed.b2=0.49,beta0= 0.0011)
#       )


#-------------------------------------- add anotherrandom effect: beta1 (not working)------------------------#

n=50

# time points for subject i
common_t=seq(1,20,length.out = 6)

sim.dt3 =data.frame()

for (i in 1:n){
  
  
  
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
  #time_i = (common_t-beta0_i)
  time_i = (common_t-beta0_i)/beta1_i
  
  
  
  # calculate y
  fixed.b1 =0.3
  fixed.b2= 0.5
  y.spline <-  fixed.b1 *(time_i)+fixed.b1^2 *(time_i) + fixed.b2 + rnorm(n=length(common_t),mean=0,sd=0.1)
  
  # make data
  dt_i=data.frame(id=i,
                  time=common_t,
                  y=y.spline 
  )
  
  sim.dt3= rbind(sim.dt3,dt_i)
}

# define model
model.spline3=function(time,fixed.b1,fixed.b2,beta0,beta1){
  
  time_i= (time-beta0)/beta1
  fixed.b1 *(time_i)+fixed.b1^2 *(time_i)+ fixed.b2
  
}

# fit nlme
fitNlme3 = nlme(y ~ model.spline3(time,fixed.b1,fixed.b2,beta0,beta1),
                
                data = sim.dt3,
                fixed = fixed.b1 + fixed.b2 ~1,
                random = beta0+beta1  ~1,
                groups = ~ id,
                start = c(0.29,0.49),
                verbose=F
                
)

summary(fitNlme3)

#--------------- test lme4::nlmer  -------------------------------#
library(lme4)
# SSfol: Dose * exp(lKe+lKa-lCl) * (exp(-exp(lKe)*input) - exp(-exp(lKa)*input))/ (exp(lKa) - exp(lKe))

data(Theoph)
Th.start <- c( lKe = -2.5 , lKa = 0.5 , lCl = -3)
test1 =nlmer ( conc ~ SSfol ( Dose ,Time ,lKe ,lKa ,lCl ) ~ lKa + lKe+lCl +(0+ lKa | Subject )+(0+ lCl | Subject ),
               nAGQ =0 ,data=Theoph,
               start = Th.start,verbose = TRUE )


SSfol2 = deriv( ~ Dose * exp(lKe+lKa-lCl) * (exp(-exp(lKe)*Time) - exp(-exp(lKa)*Time))/ (exp(lKa) - exp(lKe)),
                namevec=c('lKe','lKa', 'lCl'),
                function.arg=c('Dose','Time','lKe','lKa', 'lCl'))

test2=nlmer ( conc ~ SSfol2( Dose ,Time ,lKe ,lKa ,lCl ) ~ lKa + lKe+lCl + (0+ lKa | Subject )+(0+ lCl | Subject ),
              nAGQ =0 ,
              data=Theoph,
              start = Th.start,
              verbose = TRUE )


?nlmer
