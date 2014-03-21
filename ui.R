
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
    
  # Sidebar
  sidebarPanel(
    # Standardization options
    wellPanel(
      h3("Standardization options"),
      
      # Dependent variable to use
      # Generated automatically from tra data
      # Only show if more than one choice
      selectInput(inputId = "dep_var",
                  label = h4("Dependent variable"),
                  choices = "Growth",
                  selected = "Growth"
      ),
      
      # Standardization model
      checkboxGroupInput("model", h4("Model effects"),
                         c("Tree", "Time", "Age"),
                         selected=c("Time", "Age")),
      
      # Link function
      selectInput(inputId = "link",
                  label = h4("Link"),
                  choices = c("Identity" = "identity",
                              "Log" = "log"),
                  selected = "log"
      ),
      
      # Model fitting optimizer
      selectInput(inputId = "optim",
                  label = h4("Optimizer"),
                  choices = c("Sequential" = "sequential",
                              "Alternate" = "alternate",
                              "GLM" = "glm",
                              "GAM" = "gam"),
                  selected = "alternate"
      ),
      
      # Clustering
      h3("Clustering"),
      
      # Split
      # Only appears if age in model
      checkboxInput("split_age", label="Use multiple split age effects"),
      
      # Automatic clustering
      # Only appears if split age
      checkboxInput("auto_cluster", label="Identify clusters automatically"),
      
      # Select number of clusters
      # Only displays when automatic clustering is enabled 
      # Max is number of series
      # 0 means automatic number of clusters
      numericInput("num_clusters", strong("Number of clusters"), min=0, value=2)
      
    ),
    
    # IO
    wellPanel(
      h3("Data"),
      # File input
      h4("Load data"),
      fileInput("files", "Upload raw tree ring data", multiple=TRUE),
      
      # Download results
      # TRA, change list, final standardization w/ data
      h4("Save data"),
      downloadButton("new_tra", "Download updated dataset"),
      downloadButton("change_list", "Download change list"),
      downloadButton("last_standardization", "Download last standardization"),
      downloadButton("hclust_plot", "Download clustering plot")
    
    ),
    
    # Standardize! (button)
    actionButton("standardize", "Standardize my data")
    
  ),
  
  
  mainPanel(
    tabsetPanel(
      
      # Tab 1
      tabPanel(strong("Standardization"), 
        # All standardization plot
        # Select plot using dropdown menu
        # Should change options given model
        selectInput("std_plot_display", label=strong("Plot displayed"),
                    choices=c("Sample Depth by Time",
                              "Sample Depth by Age",
                              "Mean series length",
                              "Tree effect", 
                              "Time effect", 
                              "Age effect",
                              "Residuals density"),
                    selected="Time effect"  
        ),
        
        plotOutput("standardization_plot"),
       
        # Model fit stats
        # R^2, adj. R^2, sigma, AIC, BIC
        wellPanel(
          h3("Model fit"),
          tableOutput("model_fit")
        )
      ),
      
      # Tab 2
      tabPanel(strong("Series"),

        # observe() + updateCheckBoxGroupInput() to populate list of series               
        
        # Hierarchical cluster plot of series
        # ggdendro library
        plotOutput("hclust_series_plot"),
        
        # Summary for series
        # Sortable table: http://glimmer.rstudio.com/szakacs/FooTableDemo/
        # Name
        # Start year, end year
        # Tree effect (if applicable)
        # Standard deviation of residuals
        tableOutput("series_summary"),
        
        # List of series to include in chronology
        # Should be merged into summary table
        # All selected initially, unless specified by tra
        # https://groups.google.com/forum/#!topic/shiny-discuss/38Edf85wl_g
        checkboxGroupInput("inc_series", h4("Series to include in chronology"),
                           c("T1",
                             "T2")
        )
      ),
      
      # Tab 3
      tabPanel(strong("Crossdating"), 
        
        # Select series to crossdate
        selectInput("crossdate_series", label=strong("Series to crossdate"),
                    choices=c("T1", "T2")),
        selectInput("crossdate_plot", label=strong("Crossdating plot"),
                    choices=c("Standardized series and chronology"="series_chron_cd_plot",
                              "Residuals"="residual_cd_plot"),
                    selected="series_chron_cd_plot"
        ),
        
        # Graph of residuals by time for selected series
        # Can click on a point to select
        # https://gist.github.com/trestletech/5929598
        plotOutput("crossdate_plot"),
        
        # Show standard deviation of residuals
        textOutput(outputId="sd_series_resid"),
        
        # Shift year +/-
        numericInput("offset", strong("Shift series"), value=0),
       
        # Automatic shifting
        # Pressing it again moves to next best position
        actionButton("auto_offset", "Shift series automatically"),
       
        # Select year
        numericInput("selectedYear", strong("Select Year"), value=2000),
        
        # Merge rings
        actionButton("merge", "Merge ring with next"),
       
        # Split ring
        actionButton("split", "Split ring into two"),
       
        # Reset series
        actionButton("reset", "Reset series")
      )
    )
  )
))