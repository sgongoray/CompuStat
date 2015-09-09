
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
    
   # n<-seq(10,10000,by=10)
    
    trapecio<-function(N,fun,a,b){
      x<-runif(n,0,2)
      pnts<-seq(from=a,to=b,by=((b-a)/N))
      integral<-0
      for(i in 1:N){
        integral<-integral+((fun(pnts[i])+fun(pnts[i+1]))*(b-a)/2*N)
      }
      return (integral)
    }
    n<-seq(10,10000,by=100)
    
    sal<-trapecio(10000,function(x){4*sqrt(1-x**2)},0,1)
    
    plot(sal)
  
  

  
  })
  
 
})
