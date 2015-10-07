library(shiny)
library(plyr)
library(dplyr)
library(ggplot2)
library(parallel)

crudo<-function(x){
  runif(x,0,2)
}

mc <- function (Phi, N, X.dens=runif, alpha=0.05){
  result.list <- lapply(N,function(nsim){
    X<-runif(N,0,1)
    PhiX   <- sapply(X,Phi)
    estim <- mean(PhiX)
    S2 <- var(PhiX)
    quant <- qnorm(alpha/2,lower.tail=FALSE)
    int.upper <- estim + sqrt(S2/nsim)*quant
    int.lower <- estim - sqrt(S2/nsim)*quant
    return(data.frame(N = nsim, Estimate = estim, LI = int.lower, UI = int.upper))
  })
  results.table <- ldply(result.list) %>% mutate(i = row_number())
  return (results.table)
}

mc.importanceSampling <- function (Phi, N, X.dens=runif,g=dunif, alpha=0.05){
  result.list <- lapply(N,function(nsim){
    X<- X.dens(N)
    w <- sapply(X,g)
    PhiX   <- sapply(X,Phi)
    estim <- mean(PhiX/w)
    S2 <- var(PhiX)
    quant <- qnorm(alpha/2,lower.tail=FALSE)
    int.upper <- estim + sqrt(S2/nsim)*quant
    int.lower <- estim - sqrt(S2/nsim)*quant
    return(data.frame(N = nsim, Estimate = estim, LI = int.lower, UI = int.upper))
  })
  results.table <- ldply(result.list)%>% mutate(i = row_number())
  return (results.table)
}

shinyServer(function(input, output) {
  N <- reactive(seq(100,input$N,by=10))
  crudo.data<-reactive({
    phi <- function(x){
      input$m*exp(-(input$m)*x)
    } 
    mc(phi,N())   
  })
  is.exp<-reactive({
    phi <- function(x){
      input$m*exp(-(input$m)*x)
    }
    exp.is<-function(x){
      U <- runif(x, 0, 1) 
      X <- -(1/input$lambda)*log(1 - (1 - exp(-2*(input$lambda)))*U)
    }
    g=function(x){
      dexp(x,rate=input$lambda)/(1-exp(-2*(input$lambda)))
    }
    mc.importanceSampling(phi,N(),X.dens=exp.is,g=g)
    
  })
  
  is.beta<-reactive({
    phi <- function(x){
      input$m*exp(-(input$m)*x)
    }
    
    beta.is<-function(x){
      2*rbeta(x,1,3)
    }
    
    g=function(x){
      (.5)*dbeta(.5*x,1,3)
    }
    mc.importanceSampling(phi,N(),X.dens=beta.is,g=g)
  })
  
  lambda<- reactive({
    phi <- function(x){
      input$m*exp(-(input$m)*x)
    }
    exp.is<-function(x,lambda){
      U <- runif(x, 0, 1) 
      X <- -(1/lambda)*log(1 - (1 - exp(-2*(lambda)))*U)
    }
    g=function(x,lambda){
      dexp(x,rate=lambda)/(1-exp(-2*(lambda)))
    }
    lambda.v<-seq(.1,5*input$m,by=0.1)
    result<-lapply(lambda.v,function(y){
      X<- exp.is(input$N,y)
      w <- g(X,y)
      PhiX   <- sapply(X,phi)
      estim <- mean(PhiX/w)
      return(estim)
    })
    results.table <- ldply(result) %>% mutate(l = lambda.v,Estim=V1)
  })
  
  output$crudo <- renderPlot({
    dat <- crudo.data() %>% mutate(i = row_number(), method='crudo')
    ggplot(dat) +
      geom_ribbon(aes(i,ymin=LI,ymax=UI), alpha=0.5,,colour="blue") +
      geom_line(aes(i,Estimate)) +
      facet_wrap(~method, nrow = 2) + geom_hline(yintercept=1-exp(-2*input$m))
  })
  
  output$exponencial <- renderPlot({
    dat <- is.exp() %>% mutate(i = row_number(), method='exponencial')
    ggplot(dat) +
      geom_ribbon(aes(i,ymin=LI,ymax=UI), alpha=0.5,,colour="blue") +
      geom_line(aes(i,Estimate)) +
      facet_wrap(~method, nrow = 2) + geom_hline(yintercept=1-exp(-2*input$m))
  })
  
  output$beta <- renderPlot({
    dat <- is.beta() %>% mutate(i = row_number(), method='beta')
    ggplot(dat) +
     geom_ribbon(aes(i,ymin=LI,ymax=UI), alpha=0.5,colour="blue") +
      geom_line(aes(i,Estimate)) +
      facet_wrap(~method, nrow = 2) + geom_hline(yintercept=1-exp(-2*input$m))
  })
  
})