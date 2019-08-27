library(shiny)
library(gsDesign)
options(shiny.maxRequestSize=30*1024^2) #maximum upload 30MB;
# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {
  
  output$summary <- renderPrint({
    #dependency on button to produce summary
    if(input$goButton==0) return()
    #isolate p1 from button
    p1 = isolate({input$refprop})
    alpha = input$alpha
    sided = ifelse(input$test=="Non-inferiority(1-sided)",1,2)
    beta = input$beta
    effectsize = input$effectsize
    ratio = input$ratio
    delta0 = input$nulldiff
    p2input = reactive({
      switch(input$scaleeffect,
             "Relative Difference" = p1 + effectsize,
             "Relative Risk" = p1 * effectsize,
             "Odds Ratio" = effectsize*p1/(1+effectsize*p1-p1))
      })
    p2 = p2input()
   round(nBinomial(p1=p1, p2=p2, beta=beta, alpha=alpha,ratio=ratio,
                   sided=sided,outtype=3,delta0=delta0),2) 

  })
  
   output$PowerPlot <- renderPlot({
     if(input$goButton==0) return()
     p1 = input$refprop
     alpha = input$alpha
     sided = ifelse(input$test=="Non-inferiority(1-sided)",1,2)
     effectsize = input$effectsize
     ratio = input$ratio
     delta0 = input$nulldiff
     p2input = reactive({
       switch(input$scaleeffect,
              "Relative Difference" = p1 + effectsize,
              "Relative Risk" = p1 * effectsize,
              "Odds Ratio" = effectsize*p1/(1+effectsize*p1-p1))
     })
     p2 = p2input()
     power = seq (0.05,0.95,by=0.05)
     n = round(nBinomial(p1=p1, p2=p2, beta=1-power, alpha=alpha,ratio=ratio,
                sided=sided,delta0=delta0),2) 
     plot(n,power,type="b",pch=15,ylim=c(0,1),
          xlab="Total,Sample Size",ylab="Power",bty="n")
     grid(NA,9,lwd=2)
  })
  
})