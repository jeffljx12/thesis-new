#Boundary.knots, intercept =F in bs function.


source('makeDT.R')

rm(nlme_b_d3k2)

b_d3k2 = function(time,beta0,beta1,beta2,beta3,beta4,ra1_i,ra2_i,ra3_i){
  
  splinecoefs <- as.matrix(cbind(beta0,beta1,beta2,beta3,beta4))
  
  ra1_i+as.vector(t(matrix(rep(1,5),ncol=5) %*%
                      t(splinecoefs*as.matrix(bs((time-ra2_i)/exp(ra3_i),degree=3,knots=c(7.3,14.6),
                                                 Boundary.knots = c(-20,40),
                                                 intercept=F)))))
  
}



nlme_b_d3k2 = nlme(y.bspline_d3k2~b_d3k2(age,beta0,beta1,beta2,beta3,beta4,ra1_i,ra2_i,ra3_i),
                   data = sim.dt,
                   fixed = beta0 + beta1 + beta2 +beta3+beta4 ~1,
                   random = ra1_i+ ra2_i+ra3_i ~1,
                   groups = ~ id,
                   start =c(10,20,100,1000,3000)-1,
                   
                   # increase # interations, reduce tolerance
                   #(https://stat.ethz.ch/R-manual/R-devel/library/nlme/html/nlmeControl.html)
                   control=nlmeControl(MaxIter = 200, 
                                       pnlsMaxIter = 50,
                                       msMaxIter = 100,
                                       minScale=0.001,
                                       niterEM = 100,
                                       tolerance=10^(-8),
                                       pnlsTol= 10^(-3),
                                       msTol = 10^(-9)) 
)

summary(nlme_b_d3k2)


# corr(r1,r2) is not right !

#the above code works fine even if true ra1_i = 0.