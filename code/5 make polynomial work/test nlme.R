

#---------------------------------- nlme (assume knots are known) --------------------------_#
source('makeDs.r')

# d1k1
rm(nlme_d1k1)
nlme_d1k1 <- nlme(y.spline_d1k1 ~  beta0+beta1*(age-ra2_i)/exp(ra3_i)+
                    beta2*ifelse((age-ra2_i)/exp(ra3_i)-7.3>0,(age-ra2_i)/exp(ra3_i)-7.3,0),
                  data = sim.dt,
                  fixed = beta0 + beta1 + beta2 ~1,
                  random = ra2_i+ra3_i~1,
                  groups = ~ id,
                  start = c(18, 8,9),
                  na.action = na.omit)

summary(nlme_d1k1)

plot(sim.dt$y.spline_d1k1 ~fitted(nlme_d1k1) )

# d2k1
rm(nlme_d2k1)

d2k1_model = function(time,beta0,beta1,beta2,beta3,ra2_i,ra3_i,tau1){
  time_i = (time - ra2_i)/exp(ra3_i)
  return(beta0+beta1*time_i+beta2*time_i^2+beta3*ifelse(time_i-tau1>0,(time_i-tau1)^2,0))
  
}

nlme_d2k1 <-nlme(y.spline_d2k1 ~ d2k1_model(age,beta0,beta1,beta2,beta3,ra2_i,ra3_i,tau1=7.3),
                 data = sim.dt,
                 fixed = beta0 + beta1 + beta2 +beta3~1,
                 #random =  ra2_i+ra3_i~1,
                 random =   beta0 +ra2_i+ra3_i~1,
                 groups = ~ id,
                 start = c(18, 8,9,0.8),
                 control = nlmeControl(pnlsTol = .01, msVerbose = TRUE),
                 na.action = na.omit)


# nlme_d2k1 <- nlme(y.spline_d2k1 ~ beta0+
#                     beta1*((age-ra2_i)/exp(ra3_i))+
#                     beta2*((age-ra2_i)/exp(ra3_i))^2+
#                     beta3*ifelse((age-ra2_i)/exp(ra3_i)-7.3 > 0,((age-ra2_i)/exp(ra3_i)-7.3)^2,0),
#                   
#                   data = sim.dt,
#                   fixed = beta0 + beta1 + beta2 +beta3~1,
#                   random =  ra2_i+ra3_i~1,
#                   groups = ~ id,
#                   start = c(18, 8,9,0.8),
#                   control = nlmeControl(pnlsTol = .01, msVerbose = TRUE),
#                   na.action = na.omit)

summary(nlme_d2k1)
plot(sim.dt$y.spline_d2k1 ~ fitted(nlme_d2k1))


# d3k2
rm(nlme_d3k2)

d3k2_model = function(time,beta0,beta1,beta2,beta3,beta4,beta5,ra2_i,ra3_i,tau1,tau2){
  
  time_i = (time - ra2_i)/exp(ra3_i)
  
  return(beta0 + beta1*time_i + beta2*time_i^2 + beta3*time_i^3 +
           beta4*ifelse(time_i-tau1>0,(time_i-tau1)^3,0) + beta5*ifelse(time_i-tau2>0,(time_i-tau2)^3,0)
  )
  
}

nlme_d3k2 = nlme(y.spline_d3k2 ~ d3k2_model(age,beta0,beta1,beta2,beta3,beta4,beta5,ra2_i,ra3_i,tau1=7.3,tau2=14.6),
                 data = sim.dt,
                 fixed = beta0 + beta1 + beta2 +beta3+beta4+beta5~1,
                 #random = ra2_i +ra3_i~1,
                 random = beta0 +ra2_i +ra3_i~1,  #random intercept works when ra1_i is sufficiently large
                 groups = ~ id,
                 start = c(18, 8,9,0.8,0.3,-0.9),
                 control = nlmeControl(pnlsTol = .01, msVerbose = TRUE),
                 na.action = na.omit)

summary(nlme_d3k2)
plot(sim.dt$y.spline_d3k2 ~ fitted(nlme_d3k2))
