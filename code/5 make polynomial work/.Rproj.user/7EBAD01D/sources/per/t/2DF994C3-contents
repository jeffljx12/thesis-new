

source('makeDs.r')

#------------------------------- test nlmer ------------------------------#
# specifiy model
# d2k1_model = function(time,beta0,beta1,beta2,beta3,ra2i,ra3i)
#   beta0+
#   beta1*((age-ra2i)/exp(ra3i))+
#   beta2*((age-ra2i)/exp(ra3i))^2+
#   beta3*((age-ra2i)/exp(ra3i)-7.3 > 0)*((age-ra2i)/exp(ra3i)-7.3)^2

# d2k1_model_a = function(time,beta0,beta1,beta2,beta3,ra2i,ra3i)
#   beta0+
#   beta1*((age-ra2i)/exp(ra3i))+
#   beta2*((age-ra2i)/exp(ra3i))^2+
#   beta3*((age-ra2i)/exp(ra3i)-7.3)^2

# cannot do gradient with condition ?

# if (age-ra2i)/exp(ra3i)-7.3) > 0
d2k1_model_a = ~beta0+beta1*((age-ra2i)/exp(ra3i))+beta2*((age-ra2i)/exp(ra3i))^2+beta3*((age-ra2i)/exp(ra3i)-7.3)^2
d2k1_model_g_a = deriv(d2k1_model_a, 
                     namevec =c("beta0","beta1","beta2","beta3","ra2i","ra3i"),
                     function.arg = c('age',"beta0","beta1","beta2","beta3","ra2i","ra3i")
)

# if (age-ra2i)/exp(ra3i)-7.3) <= 0
d2k1_model_b = ~beta0+beta1*((age-ra2i)/exp(ra3i))+beta2*((age-ra2i)/exp(ra3i))^2
d2k1_model_g_b = deriv(d2k1_model_b, 
                       namevec =c("beta0","beta1","beta2","beta3","ra2i","ra3i"),
                       function.arg = c('age',"beta0","beta1","beta2","beta3","ra2i","ra3i")
)

# run nlmer
nlmer(y.spline_d2k1 ~ ((age-ra2i)/exp(ra3i)-7.3 > 0)*d2k1_model_g_a(age,beta0,beta1,beta2,beta3,ra2i,ra3i)+
                      d2k1_model_g_b(age,beta0,beta1,beta2,beta3,ra2i,ra3i) ~ (ra2i+ra3i|id),
      data= sim.dt,
      start = c(beta0=19, beta1= 9,beta2=6 ,beta3=0.5 ,ra2i=0,ra3i=0)
)


?nlmer
#------------------------- brms (slow: 1 hr to run) ------------------------#
#install.packages('brms')
library(brms)
f1 <- y.spline_d2k1 ~ beta0+
  beta1*((age-ra2i)/exp(ra3i))+
  beta2*((age-ra2i)/exp(ra3i))^2+
  beta3*((age-ra2i)/exp(ra3i)-7.3 > 0)*((age-ra2i)/exp(ra3i)-7.3)^2

prior_1 <- c(set_prior('normal(20, 0.1)',nlpar = 'beta0'),
             set_prior('normal(10, 0.1)',nlpar = 'beta1'),
             set_prior('normal(7, 0.1)',nlpar = 'beta2'),
             set_prior('normal(1, 0.1)',nlpar = 'beta3')
)

form = bf(f1,nl = TRUE)+list(beta0~1,beta1~1,beta2~1,beta3~1,ra2i~(1|2|id),ra3i~(1|2|id))

n1_b <- brm(form,data = sim.dt,prior = prior_1)

summary(n1_b)


#----------------------------- saemix ---------------------------------#


