#lottery program:
#assuming equal probability after large number of trials
#so pick the numbers that currently have the least probability
library(shiny)


#set.seed(13)
shinyServer(function(input,output){
   
  output$plot <- renderPlot({
  weekday<-substring(date(),1,3)
  if(weekday %in% c("Tue","Thu","Sun"))add=0
  if(weekday %in% c("Mon","Wed","Sat"))add=60*60*24
  if(weekday %in% c("Fri"))add=120*60*24
  
  date_ch<-format(Sys.time(),"%Y-%m-%d")
  date_nu<-strptime(date_ch,"%Y-%m-%d")+add
  title<-format(date_nu,"%Y-%m-%d")
  
  seed<-as.numeric(as.Date(date_nu))
  set.seed(seed) 
   red<-sort(sample(1:33,6))
   set.seed(as.numeric(seed)) 
   blue=sample(1:16,1)
    
    number<-c(red,blue)
    plotdata<-data.frame(cbind(x=number,
                               y=1,
                               color=(rep(c(2,4),c(6,1)))))
    #plot
    par(mar=c(1,1,1,1))
    plot(0,axes=FALSE,type="n",xlab="",ylab="",
         xlim=c(0.5,7.5),ylim=c(0.7,1.3))
    with(plotdata,
         text(1:7,y,col=color,labels=as.character(x),font=4,cex=3))
    with(plotdata,points(1:7,y,col=color,cex=10))
    polygon(x=c(0.51,7.49,7.49,0.51),y=c(0.85,0.85,1.15,1.15))
    title(main=title)
    
  })
  
})


