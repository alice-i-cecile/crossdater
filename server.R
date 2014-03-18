
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

# Libraries ####
library(shiny)
library(dendrotoolkit)

# Logic ####

# IO ####
shinyServer(function(input, output) {
  
  # Cross-dating plot
  output$crossdate_plot <- renderPlot({
    #print(ggplot2)
  })
  
  
})