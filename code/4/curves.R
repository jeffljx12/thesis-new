# curves
library(splines)
library(tidyverse)
library(MASS)
library(nlme)
 
# -------------------- make data ----------------------#
makeDataset = function(n=3,m=20,seed=123,tau1=7) {
  set.seed(seed)
  # common time
  time = seq(1,20,length.out = m)
  
  # knots
  #tau1 = time[ceiling(m/3)]
  #tau2 = time[ceiling(m/3*2)]
  
  # fixed effects
  b0 = 20
  b1 = 10
  b2 = 10
  b3 = 1
  
  sim.dt =data.frame()
  commom_corr = 0.3 #  corr increase may cause failure
  
  Sigma = .1*matrix(c(1,commom_corr,commom_corr,
                      commom_corr,1,commom_corr,
                      commom_corr,commom_corr,1),
                    3,byrow=T)
  
  random_effects = mvrnorm(n = n, mu = rep(0, 3), Sigma = Sigma, empirical = TRUE)
  
  for (i in 1:n){
    
    # random effects alpha0, beta0, beta1
    r1_i = 50*random_effects[i,1]
    r2_i = random_effects[i,2]
    r3_i = random_effects[i,3]
    # transformed time
    time_i = (time-r2_i)/exp(r3_i)
    
    # calculate outcome:
    
    # degree = 1, 1 knot
    spline_d1k1 = r1_i+b0+b1*time_i+b2*ifelse(time_i-tau1>0,time_i-tau1,0) + rnorm(n=length(time_i),mean=0,sd = 1)
    
    bspline_d1k1 =  bs(time_i,knots = tau1,degree=1, intercept=T) %*% c(b0,b1,b2) + rnorm(n=length(time_i),mean=0,sd = 1)
    
    # degree = 2, 1 knot
    spline_d2k1 = b0+b1*time_i+b2*time_i^2+b3*ifelse(time_i-tau1>0,(time_i-tau1)^2,0) + rnorm(n=length(time_i),mean=0,sd = 1)
    
    bspline_d2k1 =  bs(time_i,knots = tau1,degree=2, intercept=T) %*% c(b0,b1,b2,b3) + rnorm(n=length(time_i),mean=0,sd = 1)
    
    
    # make data
    dt_i=data.frame(id=i,
                    tau = tau1*exp(r3_i)+r2_i,
                    age=time ,
                    transformedT = time_i,
                    y.spline_d1k1 = spline_d1k1,
                    y.bspline_d1k1= bspline_d1k1,
                    y.spline_d2k1 = spline_d2k1,
                    y.bspline_d2k1 = bspline_d2k1
                    
    )
    
    sim.dt = rbind(sim.dt,dt_i)
  }
  return(sim.dt)
  
}
 
ds = makeDataset()
 
plotCurves = function(ds,yName) {
  
  y = ds[[yName]]
  xAll = list(ds$age,ds$transformedT)
  
  par(mfrow=c(1,2))
  for (i in 1:2) {
    x = xAll[[i]]
    plot(range(x),range(y),type='n',
         xlab='Age')
    
    for (id in unique(ds$id)) {
      thisId = ds$id==id
      lines(y[thisId]~x[thisId],col=gray(0.7,0.5))
      points(y[thisId]~x[thisId],pch=19)
      
      if (i==1) knot.x = ds$tau[thisId][1]
      else knot.x=7
      knotPos = approx(x[thisId],y[thisId],knot.x)
      points(knotPos$x,knotPos$y,pch=19,col='red',cex=1.2)
    }
    
    
  }
  
}
 
plotCurves(ds,'y.spline_d1k1')
