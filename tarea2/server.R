
library(shiny)

LCG <- function(nsim, M = 2^32, a = 22695477, c = 1, seed){
  X = c(seed, numeric(nsim-1)) # Preallocate space
  for(i in 1:(nsim-1)) X[i+1] <- ((a*X[i] + c)%% M) # Apply LCG rule
  return(X/M) # Apply transform
}

shinyServer(function(input, output) {
  
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- LCG(input$nsim,seed = 110104)
    y   <- LCG(input$nsim,seed=1505)
    #plot(x[-input$nsim],x[-1])
    #x <- runif(100,0,2*pi)
      a<-x
      b<-y
     x<-(sqrt(-2*log(b))*cos(2*pi*a))
     y<- (sqrt(-2*log(b))*sin(2*pi*a))
     hist(x)
    
     output$distPlot2<-renderPlot({hist(y)})
    
  })
  
  
})