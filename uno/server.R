
library(shiny)

LCG <- function(nsim,M=(2**32),a=1664525,b=1013904223,x0=1){
  X = c(x0,numeric(nsim-1))
  for(i in 1:(nsim-1)){
    X[i+1]<-(a*X[i]+b) %% M
  }
  return (X/M)
}

shinyServer(function(input, output) {
  
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- LCG(input$nsim)
    l<-input$l
    x<-sapply(x,function(x){
      log(1-x)/-l
    })
    #plot(x[-input$nsim],x[-1])
    hist(x)
  })
  
})