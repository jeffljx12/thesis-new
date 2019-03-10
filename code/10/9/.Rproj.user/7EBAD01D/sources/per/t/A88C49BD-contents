
library(rio)
library(nlme)
library(tidyverse)

# import data
testDt=import('out.sas7bdat')

# some plots
par(mfrow = c(2,2))
# with(testDt,scatter.smooth(diabyr, HBA1,pch=19,col=gray(0.2,0.2),main='Pooled'))
# with(testDt[testDt$ASSIGN=='Placebo',],scatter.smooth(diabyr, HBA1,pch=19,col=gray(0.2,0.2),main='Placebo'))
# with(testDt[testDt$ASSIGN=='Metformin',],scatter.smooth(diabyr, HBA1,pch=19,col=gray(0.2,0.2),main='Metformin'))
# with(testDt[testDt$ASSIGN=='Lifestyle',],scatter.smooth(diabyr, HBA1,pch=19,col=gray(0.2,0.2),main='Lifestyle'))


#-----hba1 ------#
with(testDt,list(
     plot(diabyr, HBA1,pch=19,col=gray(0.2,0.2),main='Pooled'),
     lines(loess.smooth(diabyr, HBA1),col='red')
))

with(testDt[testDt$ASSIGN=='Placebo',],list(
  plot(diabyr, HBA1,pch=19,col=gray(0.2,0.2),main='Placebo'),
  lines(loess.smooth(diabyr, HBA1),col='red')
))

with(testDt[testDt$ASSIGN=='Metformin',],list(
  plot(diabyr, HBA1,pch=19,col=gray(0.2,0.2),main='Metformin'),
  lines(loess.smooth(diabyr, HBA1),col='red')
))

with(testDt[testDt$ASSIGN=='Lifestyle',],list(
  plot(diabyr, HBA1,pch=19,col=gray(0.2,0.2),main='Lifestyle'),
  lines(loess.smooth(diabyr, HBA1),col='red')
))

#----- I000 ------#
with(testDt,list(
  plot(diabyr, I000,pch=19,col=gray(0.2,0.2),main='Pooled'),
  lines(loess.smooth(diabyr, I000),col='red')
))

with(testDt[testDt$ASSIGN=='Placebo',],list(
  plot(diabyr, I000,pch=19,col=gray(0.2,0.2),main='Placebo'),
  lines(loess.smooth(diabyr, I000),col='red')
))

with(testDt[testDt$ASSIGN=='Metformin',],list(
  plot(diabyr, I000,pch=19,col=gray(0.2,0.2),main='Metformin'),
  lines(loess.smooth(diabyr, I000),col='red')
))

with(testDt[testDt$ASSIGN=='Lifestyle',],list(
  plot(diabyr, I000,pch=19,col=gray(0.2,0.2),main='Lifestyle'),
  lines(loess.smooth(diabyr, I000),col='red')
))


#----- homa_ir ------#
with(testDt,list(
  plot(diabyr, homa_ir,pch=19,col=gray(0.2,0.2),main='Pooled'),
  lines(loess.smooth(diabyr, homa_ir),col='red')
))

with(testDt[testDt$ASSIGN=='Placebo',],list(
  plot(diabyr, homa_ir,pch=19,col=gray(0.2,0.2),main='Placebo'),
  lines(loess.smooth(diabyr, homa_ir),col='red')
))

with(testDt[testDt$ASSIGN=='Metformin',],list(
  plot(diabyr, homa_ir,pch=19,col=gray(0.2,0.2),main='Metformin'),
  lines(loess.smooth(diabyr, homa_ir),col='red')
))

with(testDt[testDt$ASSIGN=='Lifestyle',],list(
  plot(diabyr, homa_ir,pch=19,col=gray(0.2,0.2),main='Lifestyle'),
  lines(loess.smooth(diabyr, homa_ir),col='red')
))

# age cat #
par(mfrow = c(2,2))
with(testDt,list(
  plot(diabyr, homa_ir,pch=19,col=gray(0.2,0.2),main='Pooled'),
  lines(loess.smooth(diabyr, homa_ir),col='red')
))

with(testDt[testDt$agerand<60,],list(
  plot(diabyr, homa_ir,pch=19,col=gray(0.2,0.2),main='age<60'),
  lines(loess.smooth(diabyr, homa_ir),col='red')
))

with(testDt[testDt$agerand<75 & testDt$agerand>60,],list(
  plot(diabyr, homa_ir,pch=19,col=gray(0.2,0.2),main='age<75'),
  lines(loess.smooth(diabyr, homa_ir),col='red')
))

with(testDt[testDt$agerand>75,],list(
  plot(diabyr, homa_ir,pch=19,col=gray(0.2,0.2),main='age>75'),
  lines(loess.smooth(diabyr, homa_ir),col='red')
))


# female
par(mfrow = c(1,2))
with(testDt[testDt$female==1,],list(
  plot(diabyr, homa_ir,pch=19,col=gray(0.2,0.2),main='female'),
  lines(loess.smooth(diabyr, homa_ir),col='red')
))

with(testDt[testDt$female==0,],list(
  plot(diabyr, homa_ir,pch=19,col=gray(0.2,0.2),main='male'),
  lines(loess.smooth(diabyr, homa_ir),col='red')
))


# race6
par(mfrow = c(1,2))


with(testDt[testDt$female==1,],list(
  plot(diabyr, homa_ir,pch=19,col=gray(0.2,0.2),main='female'),
  lines(loess.smooth(diabyr, homa_ir),col='red')
))

with(testDt[testDt$female==0,],list(
  plot(diabyr, homa_ir,pch=19,col=gray(0.2,0.2),main='male'),
  lines(loess.smooth(diabyr, homa_ir),col='red')
))


#---------------------------------------------------------------#

# include diabyr <=0
testDt = filter(testDt,diabyr<=0)

# include only met
#testDt = filter(testDt,ASSIGN=='Placebo')
testDt = filter(testDt,ASSIGN=='Metformin')

testDt = filter(testDt,!is.na(homa_ir))

# sort 
testDt = arrange(testDt,patid,diabyr)

# include those have more than 6 visits\
testDt = testDt %>% add_count(patid)
par(mfrow = c(1, 1))
hist(testDt$n)

testDt = filter(testDt,n>=6)

# make time to positive 
testDt$time = -testDt$diabyr +0.1


# some plot
# ranSample = sample(unique(testDt$patid),50)
# 
# par(mfrow = c(5, 5))

# plot hba1c vs time
# for (i in 1:50){
#   with(filter(testDt, patid==ranSample[i]),
#        plot(diabyr, HBA1,type="b",col="red", lwd=2, pch=19)
#   )
# 
# }
# n_pat = length(unique(testDt$patid))
# for (i in 1:50){
#   with(filter(testDt, patid==ranSample[i]),
#        plot(diabyr, G120,type="b",col="red", lwd=2, pch=19)
#   )
#   
# }
# 
# for (i in 1:n_pat){
#   with(filter(testDt, patid==sort(unique(testDt$patid))[i]),
#        plot(diabyr, homa_ir,type="b",col="red", lwd=2, pch=19,main=sort(unique(testDt$patid))[i])
#   )
#   
# }


#---------- run nlme with no covariate----------# 
#testDt$time = (testDt$diabyr+16)/(sd((testDt$diabyr+16)))



#------- starting value  ------#
par(mfrow = c(1, 1))
#with(testDt,scatter.smooth(time, I000,pch=19,col=gray(0.2,0.2)))
with(testDt,scatter.smooth(time, homa_ir,pch=19,col=gray(0.2,0.2)))

tau1=5

# step1
testDt = mutate(testDt,
                time2= time^2,
                time3 = time^3,
                time3k1= ifelse(time-tau1>0,(time-tau1)^3,0))



fit1 = lm(homa_ir ~ time + time2  + time3 + time3k1, data=testDt)
summary(fit1)



d3k1_model = function(time,beta0,beta1,beta2,beta3,beta4,
                      ra1_i,ra2_i,ra3_i,tau1
                     ){
  
  
  intercept.x = ra2_i
  
  speed = ra3_i 
    
  time_i = time*exp(speed) - intercept.x
  
  return( ra1_i + beta0 + beta1*time_i + beta2*time_i^2 + beta3*time_i^3+ 
          beta4*ifelse(time_i-tau1>0,(time_i-tau1)^3,0) 
  )
  
}

# run nlme
rm(nlme_d3k1)


start.l = c(beta0 = fit1$coefficients[1], beta1 =fit1$coefficients[2],beta2 = fit1$coefficients[3],
            beta3 = fit2$coefficients[4], beta4 = fit1$coefficients[5])


nlme_d3k1 = nlme(homa_ir ~ d3k1_model(time = time,
                                      beta0,beta1,beta2,beta3,beta4,
                                      ra1_i, ra2_i,ra3_i,
                                      tau1 = 5
                                  ),

                 data =testDt,

                 fixed = beta0 + beta1 + beta2 + beta3 + beta4  ~1,
 
                 random = ra1_i + ra2_i + ra3_i  ~ 1 , 

                 group = ~patid,

                 start = start.l,

                 control = nlmeControl(pnlsTol = 0.01, # not using default
                                       msMaxIter = 50,
                                       msVerbose = TRUE),


                 na.action = na.omit)

summary(nlme_d3k1)


#---------------------------------------- nlme with age as covariate ---------------------------#
tau1=5

# starting value

# step1
testDt = mutate(testDt,
                time2= time^2,
                time3= time^3,
                time3k1= ifelse(time-tau1>0,(time-tau1)^3,0)
)

fit1 = lm(homa_ir ~ time + time2  + time3+ time3k1 , data=testDt)
summary(fit1)

testDt$y0hat = predict(fit1)

# step 2
testDt = mutate(testDt,
                
                # derivative of f(t)
                ft.d = fit1$coefficients[2]+
                  2*fit1$coefficients[3]*time  +
                  3*fit1$coefficients[4]*time^2  +
                  3*fit1$coefficients[5]*ifelse(time-tau1>0,(time-tau1)^2,0),
                
                lambda = (homa_ir -y0hat)/(time*ft.d),
                
                log_lambda_p1 = log(lambda+1)
)

# step 3: get covariate coefficients
fit2 = lm(log_lambda_p1 ~ 0+ agerand,data=testDt)
summary(fit2)
# very minor increase in R2 if include race6 and female, from 0.4725 to 0.4738. An indicator for var selection?

# run nlme #
d3k1_model = function(time,beta0,beta1,beta2,beta3,beta4,
                      ra1_i,ra2_i,ra3_i,tau1,
                      covar1,
                      cov.coef1
){
  
  
  intercept.x = ra2_i
  
  speed = ra3_i + cov.coef1 *covar1
  
  
  time_i = time*exp(speed) - intercept.x
  
  return( ra1_i + beta0 + beta1*time_i + beta2*time_i^2 + beta3*time_i^3+
            beta4*ifelse(time_i-tau1>0,(time_i-tau1)^3,0)
  )
  
}

rm(nlme_d3k1)

start.l = c(beta0=fit1$coefficients[1],beta1=fit1$coefficients[2],beta2=fit1$coefficients[3],
            beta3=fit1$coefficients[4],beta4=fit1$coefficients[5],
            cov.coef1 = fit2$coefficients[1])



nlme_d3k1 = nlme(homa_ir ~ d3k1_model(time = time,
                                      beta0,beta1,beta2,beta3,beta4,
                                      ra1_i, ra2_i,ra3_i,
                                      tau1 = 5,
                                      covar1 = agerand,
                                      cov.coef1
),

data = testDt,

fixed = beta0 + beta1 + beta2 + beta3 + beta4+ cov.coef1  ~1,

random = ra1_i + ra2_i + ra3_i  ~ 1 , 

group = ~patid,

start = start.l,

control = nlmeControl(pnlsTol = 0.01, # not using default
                      msMaxIter = 50,
                      msVerbose = TRUE),


na.action = na.omit)

summary(nlme_d3k1)
