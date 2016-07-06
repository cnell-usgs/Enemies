library(shiny)

# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Tree Diversity Effects"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    selectInput('bird_metric',label='Bird Community Metric',
                choices=c('abundance','30'))
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("distPlot")
  )
))