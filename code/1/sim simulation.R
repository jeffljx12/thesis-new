


library(splines)
library(tidyverse)
library(MASS)

# n=100
# id=1:n
# 
# 
# 
# # ----------------------- simple model: no covariate ---------------------------#
# 
# # assume common time for now
# common_t=seq(2,30,length.out = 100)
# 
# # knots of the spline
# knots <- common_t[c(30,70)]
# 
# # coefficients for the basis functions, 2 knots , degree=3
# coeff_spline = c(1.3, 3, 2.7, 4.0, 4.9,2)
# 
# 
# dt = data.frame(id=rep(id,each=length(common_t)),
#                  time=rep(common_t,n)
# )
# 
# # random effects alpha0, beta0, beta1
# Sigma = matrix(c(1,0,0,
#                   0,1,0,
#                   0,0,1),
#                    3,byrow=T)
# #set.seed(123)
# random_effects_i = mvrnorm(n = 1, mu = rep(0, 3), Sigma = Sigma)
# 
# alpha0_i = random_effects_i[1]
# beta0_i = random_effects_i[2]
# beta1_i = random_effects_i[3]
# 
# alpha0_i = 1
#  beta0_i = 0
#  beta1_i = 0
# 
# ####  for each subject #####
# id_i = 1
# time_i = filter(dt, id==id_i)$time
# time_i_2=(time_i-beta0_i)/exp(beta1_i)
# 
# 
# 
# # common basis functions for spline. Boundary slightly larger than range of time 
# basis_f_i = bs(time_i_2, knots=knots, degree =3, intercept = TRUE)
# 
# 
# # y-value, without random error 
# y_i= exp(alpha0_i + basis_f_i %*% coeff_spline)  + rnorm(length(time_i),mean=0,sd=4)
# 
# # create dataset for i
# dt_i=as.data.frame(cbind(y_i,basis_f_i,time_i,time_i_2,alpha0_i,beta0_i,beta1_i))
# dt_i$id=id_i
# 
# 
# plot(y_i~time_i,data=dt_i)
# 
# 
# # test nls coeff_spline = c(1.3, 3, 2.7, 4.0, 4.9,2)
# colnames(dt_i)[1:7]=c('y_i','s1','s2','s3','s4','s5','s6')
# 
# nls(y_i~ exp(alpha0+b1*s1+b2*s2+b3*s3+b4*s4+b5*s5+b6*s6),data=dt_i,
#     start=list(alpha0=1,b1=26,b2=3,b3=4,b4=4,b5=4,b6=2))
# 
# 
# #------------------- test nls function -------------------#
# a1=runif(1,0,1)
# a2=runif(1,0,1)
# a3=runif(1,0,1)
# a4=runif(1,0,1)
# a5=runif(1,0,1)
# 
# 
# 
# x1=rnorm(50)
# x2=rnorm(50)
# x3=rnorm(50)
# x4=rnorm(50)
# x5=rnorm(50)
# 
# 
# # work
# y_2=exp(a1*x1+a2*x2)+rnorm(50,0,0.5)
# nls(y_2~exp(c1*x1+c2*x2),
#     start=list(c1=0.5,c2=0.5))
# 
# # work
# y_3=exp(a1*x1+a2*x2+a3*x3)+rnorm(50,0,0.5)
# nls(y_3~exp(c1*x1+c2*x2+c3*x3),
#     start=list(c1=0.5,c2=0.5,c3=0.5))
# 
# # work
# y_4=exp(a1*x1+a2*x2+a3*x3+a4*x4)+rnorm(50,0,0.5)
# nls(y_4~exp(c1*x1+c2*x2+c3*x3+c4*x4),
#     start=list(c1=0.5,c2=0.5,c3=0.5,c4=0.5))
# 
# # not work
# # y_5=exp(a1*x1+a2*x2+a3*x3+a4*x4+a5*x5)+rnorm(50,0,0.5)
# # nls(y_5~exp(c1*x1+c2*x2+c3*x3+c4*x4+a5*x5),
# #     start=list(c1=0.5,c2=0.5,c3=0.5,c4=0.5,c5=0.5))

#----------------------------------------------------------------------------------------------#

# time points for subject i
common_t=seq(1,30,length.out = 30)

# fixed effects 
alpha0=0
beta0=1.5
beta1=2

# transformed time 
time_i = (common_t-beta0)/beta1

# knots of the spline
knots <- common_t[c(3,10)]

# create spline: constant before knot1, 2*x in [knot1,knot2], x^2 afterwards
sp <- function(x,knot1,knot2,intercept) {
  y=ifelse((x-knot2)>=0,
           (x-knot2)^2+(knot2-knot1)*2+intercept,
           ifelse((x-knot1)>=0,2*(x-knot1)+intercept,intercept)
  )
  return(y)
}

y=sp(time_i,knot1=knots[1],knot2=knots[2],intercept=0)

# test ns to fit spline
fit1=lm(y ~ bs(time_i,knots=knots,intercept=F))
cor(predict(fit1),y)
basis=bs(time_i,knots=knots,intercept=F)

# test nls functon
x=rnorm(100)
y.nls= 3+2*exp(x)+rnorm(100,0,0.5)
nls(y.nls ~ a+b*exp(x),start=list(a=1,b=1))

# combine bs and nls to estimate alpha, beta , How?

#---------------------------------------
#try cubic spline basis functions given in Hastie, Tibshirani and Friedman
#(2001, "The Elements of Statistical Learning: Data Mining, Inference, and Pre-
#   diction").



h1=1
h2=time_i
h3=time_i^2
h4=time_i^3
h5= ifelse((time_i-knots[1])^3>=0,(time_i-knots[1])^3,0)
h6=ifelse((time_i-knots[2])^3>=0,(time_i-knots[2])^3,0)


fit2= lm(y ~ h2+h3+h4+h5+h6)
cor(predict(fit2),y)
#coef(fit2)

#nls to estimate beta0 and beta1 
b0.est=c()
b1.est=c()

for (i in 1:100)  {
  
y2=y+rnorm(length(y),0,0.5) #nls function need to have noise in y

nlsFit1 =nls(y2 ~ coef(fit2)[1] + coef(fit2)[2]*(common_t-b0)/b1 + coef(fit2)[3]* ((common_t-b0)/b1)^2 +
       coef(fit2)[4]* ((common_t-b0)/b1)^3 +  
       coef(fit2)[5]*  ifelse((((common_t-b0)/b1)-knots[1])^3>=0,(((common_t-b0)/b1)-knots[1])^3,0)+
       coef(fit2)[6]* ifelse((((common_t-b0)/b1)-knots[2])^3>=0,(((common_t-b0)/b1)-knots[2])^3,0), 
       start=list(b0=1,b1=1)
)

b0.est[i]=coef(nlsFit1)[1]
b1.est[i]=coef(nlsFit1)[2]
}

hist(b0.est,breaks=20)y
hist(b1.est,breaks=20)


?nls
?optim
