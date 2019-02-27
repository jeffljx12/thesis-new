
# 1. one covariate as fixed effects in ra2_i and ra3_i
d3k2_model = function(time,beta0,beta1,beta2,beta3,beta4,beta5,ra2_i,ra3_i,tau1,tau2,
                      covariate,cov.coeff1,cov.coeff2
                      ){
  
  
  ra2_i.c = ra2_i+ covariate*cov.coeff1
  #ra2_i.c = ra2_i
  ra3_i.c = ra3_i+ covariate*cov.coeff2 
  #ra3_i.c = ra3_i
  time_i = (time - ra2_i.c)/exp(ra3_i.c)
  
  return(beta0 + beta1*time_i + beta2*time_i^2 + beta3*time_i^3 +
           beta4*ifelse(time_i-tau1>0,(time_i-tau1)^3,0) + beta5*ifelse(time_i-tau2>0,(time_i-tau2)^3,0)
  )
  
}

nlme_d3k2 = nlme(y.spline_d3k2 ~ d3k2_model(age,beta0,beta1,beta2,beta3,beta4,beta5,
                                             ra2_i,ra3_i,tau1=7.3,tau2=14.6,
                                             var1,
                                            cov.coeff1,cov.coeff2 
                                            ),
                 data = sim.dt,
                 fixed = beta0 + beta1 + beta2 +beta3+beta4+beta5+cov.coeff1+cov.coeff2 ~1,
                
                 random = beta0 +ra2_i +ra3_i~1,  #random intercept works when ra1_i is sufficiently large
                 groups = ~ id,
                 start = c(beta0=18, beta1=8,beta2=9,beta3=0.8,beta4=0.3,beta5=-0.9,
                           cov.coeff1=0.2,cov.coeff2 =0.3),
                 
                 na.action = na.omit)

summary(nlme_d3k2)

