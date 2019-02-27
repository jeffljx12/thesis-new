library(nlme)


bigF= function(){
# build model
d3k2_model = function(time,beta0,beta1,beta2,beta3,beta4,beta5,
                      ra1_i,ra2_i,ra3_i,tau1,tau2,
                      covar1,covar2,covar3,covar4,covar5,
                      cov.coef1,cov.coef2,cov.coef3,cov.coef4,cov.coef5,env = parent.frame()
){
  
  
  intercept.x = ra2_i
  
  speed = ra3_i + cov.coef1 *covar1 +
    cov.coef2 *covar2 +
    cov.coef3 *covar3 +
    cov.coef4 *covar4 +
    cov.coef5 *covar5
  
  time_i = time*exp(speed) - intercept.x
  
  return( ra1_i + beta0 + beta1*time_i + beta2*time_i^2 + beta3*time_i^3 +
            beta4*ifelse(time_i-tau1>0,(time_i-tau1)^3,0) + beta5*ifelse(time_i-tau2>0,(time_i-tau2)^3,0)
  )
  
}

#return(environment(d3k2_model))

nlme_d3k2 = nlme(outcome ~ d3k2_model(time = age,
                                      beta0,beta1,beta2,beta3,beta4,beta5,
                                      ra1_i, ra2_i,ra3_i,
                                      tau1=7.3,tau2=14.6,
                                      covar1 = var1,covar2 = var2,covar3 = var3,covar4 = var4,covar5 = var5,
                                      cov.coef1, cov.coef2, cov.coef3, cov.coef4, cov.coef5
),

data = sim.dt,

fixed = beta0 + beta1 + beta2 + beta3 + beta4 + beta5 + 
  cov.coef1 + cov.coef2 + cov.coef3 + cov.coef4 + cov.coef5 ~1,

random = ra1_i + ra2_i + ra3_i  ~ 1 |id, 

start = c(beta0 = 19, beta1 = 8,beta2 = 9,beta3 = 0.8,beta4 = 0.3,beta5 = -0.9, 
          cov.coef1 = 0.5,cov.coef2 = 0.4, cov.coef3 = -0.5,cov.coef4 = -0.9,cov.coef5 = 0.3),

control = nlmeControl(pnlsTol = 0.01, # not using default
                      msMaxIter =50,
                      msVerbose = TRUE),

method = "ML", 

na.action = na.omit)

return(summary(nlme_d3k2))

}

bigF()

# rm(d3k2_model)
# rm(nlme_d3k2)