library(shiny)
library(splines)
library(tidyverse)
library(MASS)
library(plotly)

ui <- fluidPage(
  
  titlePanel('Test Shape Invariant Model'),
  
  sidebarLayout(
    sidebarPanel(
      
      textInput(
        'alpha0',
        label='First set:  Alpha 0',
        value=0
      ),
      
      textInput(
        'beta0',
        label='First set:Beta 0',
        value=0
      ),
      
      textInput(
        'beta1',
        label='First set:Beta 1',
        value=0
      ),
      
      
      textInput(
        'alpha0_2',
        label='Second set: Alpha 0',
        value=1
      ),
      
      textInput(
        'beta0_2',
        label='Second set: Beta 0',
        value=0
      ),
      
     textInput(
        'beta1_2',
        label='Second set: Beta 1',
        value=0
      )
      
    ),
    
    mainPanel( 
               
               plotlyOutput('plot'),
               
               
               plotlyOutput('plot_2')
    )
    
    )
)
      
 #-------------------------------------------------------------#     
      
      server <- function(input, output) {
        
       
        common_t=seq(1,30,length.out = 30)
        
        # knots of the spline
        knots <- common_t[c(3,15)]
        
        # # coefficients for the basis functions, 2 knots , degree=3
        # coeff_spline = c(1.3, 3, 2.7, 4.0, 4.9,2)
        
        
        # function to plot time vs y
        simPlot = function(alpha0_i,beta0_i, beta1_i){
          
          time_i=(common_t-beta0_i)/exp(beta1_i)
          # 
          # basis_f_i = bs(time_i, knots=knots, degree =3,
          #                intercept = TRUE)
          # 
          # 
          # y_i= alpha0_i + basis_f_i %*% coeff_spline  + rnorm(length(time_i),mean=0,sd=0)
          
          
          # constant before knot1, 2*x in [knot1,knot2], x^2 afterwards
          sp <- function(x,knot1,knot2,intercept) {
            y=ifelse((x-knot2)>=0,
                     (x-knot2)^2+(knot2-knot1)*2+intercept,
                     ifelse((x-knot1)>=0,2*(x-knot1)+intercept,intercept)
            )
            return(y)
          }
          
          y_i=alpha0_i+sp(time_i,knots[1],knots[2],intercept=5)
          
          temp=data.frame(y=y_i,time=common_t)
          
          p=plot_ly(temp,x=~time,y=~y)
          
          return(p)
          
        }
      
       
        output$plot= renderPlotly({
          
       
          simPlot(eval(parse(text=input$alpha0)),
                  eval(parse(text=input$beta0)),
                  eval(parse(text=input$beta1)))
         
        
          })
        
        
        output$plot_2= renderPlotly({
          
          simPlot(eval(parse(text=input$alpha0_2)),
                  eval(parse(text=input$beta0_2)),
                  eval(parse(text=input$beta1_2)))
          
        })
       
        
        
        
      }
      shinyApp(ui, server)
      