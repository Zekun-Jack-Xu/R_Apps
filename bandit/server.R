library(shiny)
library(ggplot2)


source("utility.R")
options(shiny.maxRequestSize=30*1024^2) #maximum upload 30MB;
# Define server logic required to summarize and view the selected dataset


shinyServer(function(input, output) {
  
  
   output$PowerPlot <- renderPlot({
     if(input$goButton==0) return()
     
     #read in the data only when the button is pressed
     n_act <- isolate(input$n_arm)
     nmax <- isolate(input$n_max)
     target_error <- isolate(input$target)
     initial <- isolate(as.numeric(strsplit(input$init,",")[[1]]))
     eps <- isolate(input$eps)
     stepsize <- isolate(input$stepsize)
     ucbparm <- isolate(input$ucbparm)
     type <- isolate(input$type)
     
     #run bandit by type of data
     if(type=="Gaussian"){
        mu_true <- isolate(as.numeric(strsplit(input$mu_true,",")[[1]]))
        sd_true <- isolate(as.numeric(strsplit(input$sd_true,",")[[1]]))
        
        best <- which.max(mu_true)

        res <- normal_sim_bandit(n_act, initial, mu_true, sd_true, 
                  target_error, eps, ucbparm,
                  stepsize, FALSE, nmax, FALSE)
        
        tot1 <- n_norm(sd_true[best], target_error, n_act)

     }else if(type=="Binary"){
        prob_true <- isolate(as.numeric(strsplit(input$prob_true,",")[[1]]))
        
        best <- which.max(prob_true)

        res <- binary_sim_bandit(n_act, initial, prob_true, 
                  target_error, eps, ucbparm,
                  stepsize, FALSE, nmax, FALSE)

        tot1 <- n_bin(prob_true[best], target_error, n_act)
     }

    
      
     #gather the data for plotly
     type <- c(rep(0,n_act),rep(1,n_act))
     Count <- c(rep(ceiling(tot1 / n_act), n_act ), res$act_count)
     Arm <- c(1:n_act,1:n_act)

     data <- data.frame(cbind(type=type,Count=Count,Arm=Arm))
     data$Method <- ifelse(data$type == 0, "Classic", "Bandit")
     data$Arm <- factor(data$Arm)

     txt <- paste("The total number of trials is", ceiling(tot1), "/", sum(res$act_count),
      "for classic / multi-arm bandit design.")

      data$txt <- rep(txt,nrow(data))

     pp <- ggplot(data, aes(x=Arm, y=Count, fill=Method, label=Count)) + 
     geom_bar(stat='identity',position=position_dodge()) +
     scale_x_discrete(breaks=c(1,2,3),
     labels=c("Arm 1", "Arm 2", "Arm 3")) +
     xlab("") + theme_ggplot() + 
     ggtitle("Sample size in each arm") + 
     scale_fill_manual(values=c("#FF4D4D","#72A0E5")) +
     labs(subtitle=txt) +
     geom_text(size=5,position=position_dodge(width=1))

     pp

  })
  
})
