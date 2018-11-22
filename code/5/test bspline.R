

source('makeDs.r')

# b-spline d2k1
rm(nlme_b_d2k1)

b_d2k1 <- function(time,beta0,beta1,beta2,beta3,ra2_i,ra3_i){
  
  splinecoefs <- as.matrix(cbind(beta0,beta1,beta2,beta3))
  as.vector(t(matrix(rep(1,4),ncol=4) %*%
                        t(splinecoefs*as.matrix(bs((time-ra2_i)/exp(ra3_i),degree=2,
                                                   knots=7.3,intercept=T)))))
}


nlme_b_d2k1 <-
  nlme(y.bspline_d2k1 ~ b_d2k1(age,beta0,beta1,beta2,beta3,ra2_i,ra3_i),
       data=sim.dt,
       fixed = beta0 + beta1 + beta2 + beta3 ~ 1,
       random = ra2_i+ ra3_i ~ 1 | id,
       start = c(18, 8,9,0.5)    # this initial value works , however c(18, 8,9,0.5)*100 doesnot work? neither increasing m
)

summary(nlme_b_d2k1)
plot(sim.dt$y.bspline_d2k1 ~ fitted(nlme_b_d2k1),xlab='fitted value',ylab='observed value')  #this plot seems wrong
 
 # try b-spline d3k2
rm(nlme_b_d3k2)

b_d3k2 = function(time,beta0,beta1,beta2,beta3,beta4,beta5,ra2_i,ra3_i){
  
  splinecoefs <- as.matrix(cbind(beta0,beta1,beta2,beta3,beta4,beta5))
  
  as.vector(t(matrix(rep(1,6),ncol=6) %*%
                t(splinecoefs*as.matrix(bs((time-ra2_i)/exp(ra3_i),degree=3,
                                           knots=c(7.3,14.6),intercept=T)))))
  
}
  


nlme_b_d3k2 = nlme(y.bspline_d3k2~b_d3k2(age,beta0,beta1,beta2,beta3,beta4,beta5,ra2_i,ra3_i),
                   data = sim.dt,
                   fixed = beta0 + beta1 + beta2 +beta3+beta4+beta5 ~1,
                   random = ra2_i+ra3_i ~1,
                   groups = ~ id,
                   start = c(18,8,9,0.5,0.3,-0.7)
)


