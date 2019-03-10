

main_function <- function(x){
  library(nlme)
  
  nonlinear_function <- function(x){
    log(x)
  } 
  
  result <- nlme(height ~ SSasymp(age, Asym, R0, lrc) +
                   nonlinear_function(age),
                 data = Loblolly,
                 fixed = Asym + R0 + lrc ~ 1,
                 random = Asym ~ 1,
                 start = c(Asym = 103, R0 = -8.5, lrc = -3.3))
  result
}


summary(main_function())




main_function2 <- function(x){
  
  
  nonlinear_function <- function(x){
    log(x)
  } 
  
  result <- lm(mpg ~ nonlinear_function (disp),data=mtcars)
  result
}

summary(main_function2())

