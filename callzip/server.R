if(!require(plyr))install.packages("plyr")
if(!require(ggplot2))install.packages("ggplot2")
if(!require(gridExtra))install.packages("gridExtra")
if(!require(shiny))install.packages("shiny")
library(plyr)
library(ggplot2)
library(gridExtra)
library(shiny)

load("./map.RData")
base_city=aggregate(population~state+name+county+city,data=base_all,sum)
base_county=aggregate(population~state+name+county,data=base_all,sum)
base_state=aggregate(population~state+name,data=base_all,sum)

#merge and sort
county$subregion=toupper(county$subregion)
county1=merge(county,base_county,by.x=c("region","subregion"),by.y=c("name","county"),all.x=T)
county1=arrange(county1,group,order)
state1=merge(state,base_state,by.x="region",by.y="name",all.x=T)
state1=arrange(state1,group,order)


# Define server logic for random distribution application
shinyServer(function(input, output) {
  
    output$summary <- renderPrint({
      zip=as.character(input$zip)
      if(zip %in% base_all$zipcode==FALSE){
      cat(" ",zip," does not exist in US continent!",
          "\n","Please enter a valid 5-digit zip code.\n")
      }else{
        info=subset(base_all,zipcode==zip)
        cat(" Information for ZIP",zip,"(2010 Census): \n",
            "Population:",info$population,"  Median household income: $",info$median_income,
            " City:",info$city," County",info$county," State:",toupper(info$name))
      }
    })
    
    
      output$plot <- renderPlot({
        zip=as.character(input$zip)
        if(zip %in% base_all$zipcode==FALSE){
        
        p1 <-  ggplot(state1, aes(x = long, y = lat,group = group,fill=population))
        p1 <- p1 + geom_polygon(colour="black") # fill areas
        p1 <- p1 + labs(title = "USA")+
          scale_y_continuous("Latitude")+scale_x_continuous("Longitude")
        p1
        }else{
          info=subset(base_all,zipcode==zip)
          option = reactive({
            switch(input$option,
                   "maps" = 0,
                   "stats" = 1)
          }) #this is a reactive function
          
          option=option()
          
          if(option==0){
            p1 <-  ggplot(state1, aes(x = long, y = lat,group = group,fill=population))
            p1 <- p1 + geom_polygon(colour="black") # fill areas
            p1 <- p1+geom_point(data=info,mapping=aes(x=long,y=lat,group=NULL),
                                ,size=3,colour="red")
            p1 <- p1 + labs(title = "USA")+
              scale_y_continuous("Latitude")+scale_x_continuous("Longitude")
            
            thisstate=subset(county1,region==info$name)
            p2 <- ggplot(thisstate, aes(x = long, y = lat, group = group,fill=population))
            p2 <- p2 + geom_polygon(colour="black") # fill areas
            p2 <- p2+geom_point(data=info,mapping=aes(x=long,y=lat,group=NULL),
                                ,size=5,colour="red")
            p2 <- p2 + labs(title = paste("State:",info$name,collapse=" "))+
              scale_y_continuous("Latitude")+scale_x_continuous("Longitude")
            grid.arrange(p1, p2,  ncol=2, main="The zip code area falls on the red point.")
          } else{
            par(mfrow=c(2,2))
            with(subset(base_all,city==info$city)
                 ,plot(population,median_income,pch=16,
                       xlab="Population",ylab="Median household income($)",main=info$city))
            with(info,points(population,median_income,pch=16,col="red",cex=1.3))
            legend("topright",pch=16,col="red",legend="Your zip")
            grid()
            
            with(subset(base_all,county==info$county)
                 ,plot(population,median_income,pch=16,
                       xlab="Population",ylab="Median household income($)",
                       main=paste(info$county,"County",collapse=" ")))
            with(info,points(population,median_income,pch=16,col="red",cex=1.3))
            legend("topright",pch=16,col="red",legend="Your zip")
            grid()
            
            with(subset(base_all,name==info$name)
                 ,plot(population,median_income,pch=16,
                       xlab="Population",ylab="Median household income($)",main=toupper(info$name)))
            with(info,points(population,median_income,pch=16,col="red",cex=1.3))
            legend("topright",pch=16,col="red",legend="Your zip")
            grid()
            
            with(base_all
                 ,plot(population,median_income,pch=16,
                       xlab="Population",ylab="Median household income($)",main="USA"))
            with(info,points(population,median_income,pch=16,col="red",cex=1.3))
            legend("topright",pch=16,col="red",legend="Your zip")
            grid()
            
            par(mfrow=c(1,1))
          }
          
        }
    })
})

