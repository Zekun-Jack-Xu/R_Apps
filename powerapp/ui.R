library(shiny)

# Define UI for dataset viewer application
shinyUI(pageWithSidebar(
  
  # Application title.
  headerPanel("Sample Size Calculation For Binomial Data"),
    
  sidebarPanel(
    
    
    helpText("Note: Equivalence test is basically","Two One-Sided Test"),
    
    #hyperlink conditional on checkbox input;
    checkboxInput("email","Email"),
    conditionalPanel(
      condition="input.email==true",
    helpText( a("Click Here to email the author",href="http://www.126.com"))
    ),
    
    selectInput("test", "Type of Testing:", 
                choices = c("Superiority(2-sided)", "Non-inferiority(1-sided)")),
    numericInput("refprop", "Reference Incidence Proportion:", 0.3),
    selectInput("scaleeffect", "Type of Effect Size:", 
                choices = c("Relative Difference", "Relative Risk", "Odds Ratio")),
    numericInput("effectsize", "Expected Effect Size:", 0.3),
    numericInput("alpha", "Type I Error:", 0.05),
    numericInput("beta", "Type II Error:", 0.1),
    numericInput("ratio", "N2 / N1:", 1),
    numericInput("nulldiff", "Null Difference in Proportion:", 0),
    
    #submitButton("Submit"),
    
    actionButton("goButton","Go!")
  ),
  

  mainPanel(
    h4("Sample Size Summary"),  #additional header
    verbatimTextOutput("summary"),
    h4("Power Plot")
    ,plotOutput("PowerPlot")
  )
))