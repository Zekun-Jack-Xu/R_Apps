library(shiny)

# Define UI for dataset viewer application
shinyUI(pageWithSidebar(
  
  headerPanel("Get 24!"),
  
  sidebarPanel(
    helpText("How to play:"),
    helpText("1.Please select the upper bound for the cards:10 - 20"),
    helpText("2.Scroll the horizontal bar at the bottom of the page to pick a question from a dynamic bank:1 - 100000"),
    helpText("3.Click Refresh Button"),
    
    sliderInput("maximum", "Upper Bound:", min = 10, max = 20, step = 1, value = 10),
    br(),
    actionButton("goButton","Refresh")
  ),
  
  mainPanel(
    
    tabsetPanel(
      tabPanel("Question", plotOutput("question")), 
      tabPanel("Answer", plotOutput("answer"))
    ),
    
    br(),br(),
    
    sliderInput("animation", "Pick a question", 1, 100000, 1, step = 1) 
    #animate=animationOptions(interval=3000, loop=T)  
  )
))
