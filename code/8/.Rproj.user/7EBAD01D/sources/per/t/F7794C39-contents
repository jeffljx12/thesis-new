

# 1. random intercepts only

d3k2_model = function(time,beta0,beta1,beta2,beta3,beta4,beta5,
                      ra1_i,ra2_i,ra3_i,tau1,tau2,
                      covariate1,covariate2,cov.coef.f1,cov.coef.f2
                 
                      ){
  
  
  intercept.x = ra2_i + covariate1 * cov.coef.f1 

  speed = ra3_i+ covariate2 * cov.coef.f2 

  time_i = (time - intercept.x)/exp(speed)
  
  return( ra1_i + beta0 + beta1*time_i + beta2*time_i^2 + beta3*time_i^3 +
           beta4*ifelse(time_i-tau1>0,(time_i-tau1)^3,0) + beta5*ifelse(time_i-tau2>0,(time_i-tau2)^3,0)
  )
  
}

rm(nlme_d3k2)

nlme_d3k2 = nlme(y.spline_d3k2 ~ d3k2_model(time = age,beta0,beta1,beta2,beta3,beta4,beta5,
                                            ra1_i, ra2_i,ra3_i,tau1=7.3,tau2=14.6,
                                            covariate1 = var1, covariate2 = var2,
                                            cov.coef.f1,cov.coef.f2
                                            ),
                 data = sim.dt,
                 
                 fixed = beta0 + beta1 + beta2 + beta3 + beta4 + beta5 +
                         cov.coef.f1 + cov.coef.f2 ~1,
                
                 random = ra1_i + ra2_i + ra3_i  ~ 1 |id, 
                            
              
                 
                 start = c(beta0 = 18, beta1 = 8,beta2 = 9,beta3 = 0.8,beta4 = 0.3,beta5 = -0.9,
                           cov.coef.f1 = 4,cov.coef.f2 = 0.2),
                 
                 control = nlmeControl(pnlsTol = 0.001,
                                       msMaxIter =50,
                                       msVerbose = TRUE),
                 
                 na.action = na.omit)

summary(nlme_d3k2)


# 2. random intercepts + one random slope


d3k2_model2 = function(time,beta0,beta1,beta2,beta3,beta4,beta5,
                      ra1_i,ra2_i,ra3_i,tau1,tau2,
                      covariate1,covariate2,cov.coef.f1,cov.coef.f2,
                      cov.coef.r1
){
  
  
  intercept.x = ra2_i + covariate1 * cov.coef.f1 + covariate1 * cov.coef.r1
  
  speed = ra3_i+ covariate2 * cov.coef.f2 
  
  time_i = (time - intercept.x)/exp(speed)
  
  return(ra1_i + beta0 + beta1*time_i + beta2*time_i^2 + beta3*time_i^3 +
           beta4*ifelse(time_i-tau1>0,(time_i-tau1)^3,0) + beta5*ifelse(time_i-tau2>0,(time_i-tau2)^3,0)
  )
  
}

rm(nlme_d3k22)

nlme_d3k22 = nlme(y.spline_d3k2 ~ d3k2_model2(time = age,beta0,beta1,beta2,beta3,beta4,beta5,
                                            ra1_i,ra2_i,ra3_i,tau1=7.3,tau2=14.6,
                                            covariate1 = var1, covariate2 = var2,
                                            cov.coef.f1,cov.coef.f2,
                                            cov.coef.r1
                                            
                                            ),
              data = sim.dt,

              fixed = beta0 + beta1 + beta2 + beta3 + beta4 + beta5 +
                      cov.coef.f1 + cov.coef.f2 ~ 1,
 
             random = ra1_i + ra2_i + ra3_i  + cov.coef.r1 ~ 1 | id, 
  
           

             start = c(beta0 = 18, beta1 = 8,beta2 = 9,beta3 = 0.8,beta4 = 0.3,beta5 = -0.9,
                      cov.coef.f1 = 4,cov.coef.f2 = 0.2),

             control = nlmeControl(pnlsTol = 0.01,
                                   maxIter = 100,
                                   msMaxIter =100,
                                   msVerbose = TRUE),

              na.action = na.omit)

summary(nlme_d3k22)

