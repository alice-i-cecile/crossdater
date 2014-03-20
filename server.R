
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
  
  
  # Processing input ####
  {
    # Reading in and processing data
    
    # Reading in standardization options
  }
  
  # Standardization ####
  {
    # Standardizing the data
    
    # Selecting plot
    
    # Model fit table
  }
  
  # Series ####
  {
    # Grabbing list of series
    
    # Hierarchical series plot
    output$crossdate_plot <- renderPlot({
      #print(ggplot2)
    }) 
    
    # Series summary table 
  }
  
  # Cross-dating ####
  {
    # Crossdating plot
    
    # Automatic shifting
    
    # Shift series
    
    # Split ring
    
    # Merge rings
    
    # Change list
  }
  
  
  # Saving output ####
  {
    # New dataset
    
    # Change list
    
    # Final standardization
  
    # Hierarchical series plot
  }
  
})