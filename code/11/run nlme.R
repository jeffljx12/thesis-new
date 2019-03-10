

#----------------------------------- run nlme -----------------------------------------#

# build model
d3k2_model = function(time,beta0,beta1,beta2,beta3,beta4,beta5,
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
  
  return( ra1_i + beta0 + beta1*time_i + beta2*time_i^2 + beta3*time_i^3 +
            beta4*ifelse(time_i-tau1>0,(time_i-tau1)^3,0) + beta5*ifelse(time_i-tau2>0,(time_i-tau2)^3,0)
  )
  
}


# run nlme
rm(nlme_d3k2)
source('starting value.R')

# estimated starting value
start.est = c(beta0=fit1$coefficients[1],beta1=fit1$coefficients[2],beta2=fit1$coefficients[3],
              beta3=fit1$coefficients[4],beta4=fit1$coefficients[5],beta5=fit1$coefficients[6],
              cov.coef1 = fit2$coefficients[1],cov.coef2 = fit2$coefficients[2],
              cov.coef3 = fit2$coefficients[3],cov.coef4 = fit2$coefficients[4],
              cov.coef5 = fit2$coefficients[5])


# almost true value
start.true =c(beta0 = 9, beta1 =4,beta2 = 2,beta3 = 0.8,beta4 = 0.4,beta5 = -1, 
  cov.coef1 = 0.6,cov.coef2 = 0.4, cov.coef3 = -0.5,cov.coef4 = -1,cov.coef5 = 0.3)

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

                 random = ra1_i + ra2_i + ra3_i  ~ 1 , 
                 group = ~id,

                 start = start.true,

                 control = nlmeControl(pnlsTol = 0.01, # not using default
                           msMaxIter =50,
                           msVerbose = TRUE),
                 
                 method = "ML", 

                 na.action = na.omit)

summary(nlme_d3k2)

# compare with true fixed effect
nlme_d3k2$coefficients$fixed
sp.coeff1
cov.coeff1


#------------------------------------ fit without covariates ------------------------------#
# build model
d3k2_model = function(time,beta0,beta1,beta2,beta3,beta4,beta5,
                      ra1_i,ra2_i,ra3_i,tau1,tau2
                      
){
  
  
  intercept.x = ra2_i
  
  speed = ra3_i 
  time_i = time*exp(speed) - intercept.x
  
  return( ra1_i + beta0 + beta1*time_i + beta2*time_i^2 + beta3*time_i^3 +
            beta4*ifelse(time_i-tau1>0,(time_i-tau1)^3,0) + beta5*ifelse(time_i-tau2>0,(time_i-tau2)^3,0)
  )
  
}


# run nlme
rm(nlme_d3k2)

# estimated starting value


# almost true value
start.true =c(beta0 = 9, beta1 =4,beta2 = 2,beta3 = 0.8,beta4 = 0.4,beta5 = -1)


table(cut(sim.dt$age,c(0,7.3,14.6,Inf)))
nlme_d3k2 = nlme(outcome ~ d3k2_model(time = age,
                                      beta0,beta1,beta2,beta3,beta4,beta5,
                                      ra1_i, ra2_i,ra3_i,
                                      tau1=7.3,tau2=14.6
                                      
),

data = sim.dt,

fixed = beta0 + beta1 + beta2 + beta3 + beta4 + beta5~1,

random = ra1_i + ra2_i  +ra3_i ~ 1 , 
group = ~id,

start = start.true,

control = nlmeControl(pnlsTol = 0.01, # not using default
                      msMaxIter =50,
                      msVerbose = TRUE),

na.action = na.omit)

summary(nlme_d3k2) 
nlme_d3k2$coefficients$fixed
sp.coeff1
cov.coeff1

