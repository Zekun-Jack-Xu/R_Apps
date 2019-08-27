library(shiny)

source("./go24.R")
source("./cal24.R")

shinyServer(function(input, output) {
  
  card<-reactive({sample(1:input$maximum+0*input$animation,4,replace=T)})  #everything in this area has to be reactive
  #when either input value changes, the output would be invalidated correspondingly  
  
  output$question <- renderPlot({
    if(input$goButton == 0)      return()
    
    card <- isolate(card())  #do not refresh screen until button clicked
    
    par(mar=c(1,1,1,1))
    plot(0,axes=F,type="n",xlab="",ylab="",xlim=c(0.5,4.5),ylim=c(0.5,1.5))
    text(1:4,1,labels=as.character(card),font=2,cex=6)
    
  })
  
  
  output$answer <- renderPlot({
    if(input$goButton == 0)      return()
    
    card <- isolate(card())
    
    x<-go(card[1],card[2],card[3],card[4])
    
    par(mar=c(1,1,1,1))
    plot(0,axes=F,type="n",xlab="",ylab="",xlim=c(0.5,3.5),ylim=c(0.5,1.5))
    text(2,1,labels=as.character(x),font=2,cex=5)
    
  })
})