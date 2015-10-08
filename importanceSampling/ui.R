
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Tarea 3: ImportanceSampling"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      numericInput('N', 'Número de simulaciones',
                  value = 500),
      sliderInput('m', 'm',
                  value = 2.5, min = 0.1, max = 5, step = .1),
      sliderInput('l', 'lambda',
                  value = 3, min = 0.1, max = 15, step = .1)
    ),
    
    # Show a plot of the generated distribution 
    mainPanel(
        plotOutput('crudo'),
        plotOutput('exponencial'),
        plotOutput('beta'),
        plotOutput('lam')
      )
    
  )
  )
)
