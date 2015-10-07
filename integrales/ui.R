# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
  
  # Application title
  headerPanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins
  
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("distPlot"),
    plotOutput("distPlot2"),
    plotOutput("distPlot3")
    
  )
  
))