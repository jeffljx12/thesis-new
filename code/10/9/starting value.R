library(tidyverse)

source('makeDT.R')

#----------------------------------- loop1 -----------------------------------------#
tau1=7.3
tau2=14.6

# step 1: get spline coefficients and y0hat
sim.dt = mutate(sim.dt,
                age2= age^2,
                age3= age^3,
                age3k1= ifelse(age-tau1>0,(age-tau1)^3,0),
                age3k2= ifelse(age-tau2>0,(age-tau2)^3,0))



fit1 = lm(outcome ~ age + age2 + age3 + age3k1 + age3k2, data=sim.dt)

summary(fit1)

sim.dt$y0hat = predict(fit1)

#------------------------------------------------------------------------------#
# sim.dt2 = mutate(sim.dt,
#                   age1 = transformedT,
#                   age2= transformedT^2,
#                   age3= transformedT^3,
#                   age3k1= ifelse(transformedT-tau[1]>0,(transformedT-tau[1])^3,0),
#                   age3k2= ifelse(transformedT-tau2>0,(transformedT-tau2)^3,0),
#                   outcome2 = r1_i+ spline
#                   
#  )
#  
#  
#  
# testFit1 = lm(spline ~ age1 + age2 + age3 + age3k1 + age3k2, data=sim.dt2)  # true
# testFit2 = lm(outcome2 ~ age1 + age2 + age3 + age3k1 + age3k2, data=sim.dt2)# add random intercept
# testFit3 = lm(outcome ~ age1 + age2 + age3 + age3k1 + age3k2, data=sim.dt2) # add random intercept and error term
# 
# summary(testFit1)
# summary(testFit2)
# summary(testFit3)
#--------------------------------------------------------------------------------#


# step2: get lambda
sim.dt = mutate(sim.dt,
                
                # derivative of f(t)
                ft.d = fit1$coefficients[2]+
                       2*fit1$coefficients[3]*age +
                       3*fit1$coefficients[4]*age^2 +
                       3*fit1$coefficients[5]*ifelse(age-tau1>0,(age-tau1)^2,0)+
                       3*fit1$coefficients[6]*ifelse(age-tau2>0,(age-tau2)^2,0),

                lambda = (outcome -y0hat)/(age*ft.d),
                
                log_lambda_p1 = log(lambda+1)
)

# step 3: get covariate coefficients
fit2 = lm(log_lambda_p1~0+var1+var2+var3+var4+var5,data=sim.dt)
summary(fit2)

# #----------------------------------- loop2 -----------------------------------------#
# # step 1: get spline coefficients and y0hat
# sim.dt2 = filter(sim.dt,!is.na(log_lambda_p1))
# 
# sim.dt2 = mutate(sim.dt2,
#                  age = age*exp(predict(fit2)),
#                  age2= age^2,
#                  age3= age^3,
#                  age3k1= ifelse(age-tau1>0,(age-tau1)^3,0),
#                  age3k2= ifelse(age-tau2>0,(age-tau2)^3,0))
# 
# 
# 
# fit2.1 = lm(outcome ~ age + age2 +age3+age3k1+age3k2,
#             data=sim.dt2)
# 
# summary(fit2.1)
# fit2.1$coefficients  # very large
# 
# sim.dt2$y0hat = predict(fit2.1)
# 
# # step2: get lambda
# sim.dt2 = mutate(sim.dt2,
#                 
#                 # derivative of f(t)
#                 ft.d = fit2.1$coefficients[2]+
#                   2*fit2.1$coefficients[3]*age +
#                   3*fit2.1$coefficients[4]*age^2 +
#                   3*fit2.1$coefficients[5]*ifelse(age-tau1>0,(age-tau1)^2,0)+
#                   3*fit2.1$coefficients[6]*ifelse(age-tau2>0,(age-tau2)^2,0),
#                 
#                 lambda = (outcome -y0hat)/(age*ft.d),
#                 
#                 log_lambda_p1 = log(lambda+1)
# )
# 
# 
# # step 3: get covariate coefficients
# fit2.2 = lm(log_lambda_p1~0+var1+var2+var3+var4+var5,data=sim.dt2)
# summary(fit2.2)  # getting worse
# 
