library(shiny)

# Define UI for dataset viewer application
shinyUI(pageWithSidebar(
  
  # Application title.
  headerPanel("Sample Size Comparison between Classic and Multi-Arm Bandit Design"),
    
  sidebarPanel(
     
    
    sliderInput("n_arm", "Number of arms:", min = 2, max = 10, step = 1, value = 3),
    
    helpText("The smaller the target standard error, the larger the sample size. But make sure the target standard error is small enough relative to the true difference between arms."),
    
    numericInput("target", "Target standard error:", 0.05),
    
    helpText("The maximum number of trials is the upper bound for sample size in the multi-arm bandit design. It should be set to at least 10 times the number of arms."),
   
    numericInput("n_max", "Maximum number of trials", 1000),

    helpText("Initialize the values for all arms in the multi-arm bandit design."),
    
    textInput("init", "Initial values (separate by comma):", value = "0.4,0.4,0.4"),
    
    helpText("The eps value is parameter for epsilon-greedy algorithm in the multi-arm bandit design. It should fall between 0 (greedy) and 1 (random)."),
    
    numericInput("eps", "eps:", 0.1),
    
    helpText("The alpha value is the step size for updating values in each arm in the multi-arm bandit design. It should be greater than or equal to 0. If alpha = 0, then update by average reward instead."),
    
    numericInput("stepsize", "alpha:", 0.1),
    
    helpText("The ucbparm value is the parameter for the UCB algorithm in the multi-arm bandit design. It should be greater than or equal to 0. If ucbparm = 0, then use the epsilon-greedy algorithm."),
    
    numericInput("ucbparm", "ucbparm:", 0),
    
    selectInput("type", "Data type:", choices=c("Binary", "Gaussian")),
    
    conditionalPanel(
          condition="input.type=='Gaussian'",
          textInput("mu_true", "True means (separate by comma):", "40,50,60"),
          textInput("sd_true", "True standard deviations (separate by comma):", "5,7,9")
    ),

    conditionalPanel(
          condition="input.type=='Binary'",
          textInput("prob_true", "True probabilities (separate by comma):", "0.3,0.4,0.5") 
    ),

    #submitButton("Submit"),
    
    actionButton("goButton","Go!")
  ),
  

  mainPanel(
     
    helpText("The minimum sample size is calculated to ensure that the standard error of the arm with the highest mean reward is smaller than the target standard error. In the classic design, all arms have the same number of trials. In the multi-arm bandit design, there is a balance between exploration and exploitation so that the arm with the highest mean reward is assigned more trials adaptively."),
    #h4("Sample Size Summary"),   
    #verbatimTextOutput("summary"),
    #h4("Power Plot"),
    plotOutput("PowerPlot")
    
  )
))