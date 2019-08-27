library(shiny)

# Define UI for dataset viewer application
shinyUI(pageWithSidebar(
  
  # Application title.
  headerPanel("Abstract"),
  
  sidebarPanel(
    
  ),
  
  mainPanel(
    numericInput("row_min", "Begin from row:", 150),
    numericInput("row_max", "End at row:", 488),
    downloadButton('downloadData', 'Download')
      )
))


