
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

# Libraries ####
library(shiny)


# UI ####
shinyUI(pageWithSidebar(
  
  # Heading
  headerPanel("crossdateR"),
  
  # Tab 1
  
  # Sidebar
  sidebarPanel(
    # Standardization options
    
    # Model (check)
    wellPanel(
      
      h4("Model"),
      
      checkboxInput(inputId = "modelTree",
                    label = "Tree",
                    value = FALSE),
      checkboxInput(inputId = "modelTime",
                    label = "Time",
                    value = TRUE),      
      checkboxInput(inputId = "modelAge",
                     label = "Age",
                     value = TRUE)
      
    ),
    
    #Split (check)
    wellPanel(
      
      h4("Split"),
      
      checkboxInput(inputId = "splitTree",
                    label = "Tree",
                    value = FALSE),
      checkboxInput(inputId = "splitTime",
                    label = "Time",
                    value = FALSE),      
      checkboxInput(inputId = "splitAge",
                    label = "Age",
                    value = TRUE)
      
    ),
    
    # optim(drop)
    selectInput(inputId = "optim",
                label = h4("Optimizer"),
                choices = c("Sequential" = "sequential",
                            "Alternate" = "alternate",
                            "GLM" = "glm",
                            "GAM" = "gam"),
                selected = "Alternate"
    ),
    
    
    # IO
    wellPanel(
      h4("Load and save"),
      # File input
      fileInput("files", "Upload raw tree ring data", multiple=TRUE),
      
      # Download results
      # TRA, change list, final standardization w/ data
      downloadButton("new_tra", "Updated dataset"),
      
      downloadButton("change_list", "Download change list"),
      
      downloadButton("last_standardization", "Download last standardization")
      
      ),
    
    # Standardize! (button)
    actionButton("standardize", "Standardize my data")
    
  ),
  
  
  mainPanel(
    # Tab 1
    # All standardization graphs
    
    # Model fit stats
    # R^2, adj. R^2, sigma, AIC, BIC
    
    # Tab 2
    # List of series (selectable, maybe scrollable)
    # Beside them average residuals etc.
    # Check box for "fixed" vs. "floating"
    
    # Tab 3
    # Graph of residuals by time for selected series
    # Can click on a point to select
    
    # Shift year +/-
    numericInput("offset", strong("Shift series"), value=0),
    
    # Merge rings
    actionButton("merge", "Merge ring with next"),
    
    # Split ring
    actionButton("split", "Split ring into two")
    
  )
  
  
  
  
  
))