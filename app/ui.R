library(shiny)

# Define the overall UI
shinyUI(
  
  # Use a fluid Bootstrap layout
  fluidPage(    
    
    # Give the page a title
    titlePanel("Tree Diversity Effects"),
    
    # Generate a row with a sidebar
    sidebarLayout(      
      
      # Define the sidebar with one input
      sidebarPanel(
        radioButtons("guild", 
                           label=h3("Birds:"),
                           choices=list("All Birds" = div.guild, "Insectivorous Birds" = ins.guild),selected = "All Birds"),
        hr(),
        helpText("Insectivorous birds can be gleaning (IN), flycatchers (FL), or omnivores (OM)")
      ),
      
      # Create a spot for the barplot
      mainPanel(
        plotOutput("guildplot",width="600px",height="500px")  
      )
      
    )
  )
)