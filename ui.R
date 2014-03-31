# Libraries ####
library(shiny)

# UI ####
shinyUI(pageWithSidebar(
  
  # Heading
  headerPanel("crossdateR"),
    
  # Sidebar
  sidebarPanel(
    wellPanel(
      h3("Standardization options"),
      
      # Dependent variable to use
      # Generated automatically from tra data
      # Only show if more than one choice
      uiOutput("dep_vars"),
      
      # Standardization model
      checkboxGroupInput("model", h4("Model effects"),
                         c("Tree", "Time", "Age"),
                         selected=c("Time", "Age")),
      
      # Split effects
      # Named e_split to avoid namespace conflicts
      selectInput(inputId = "e_split",
                  label = h4("Split effects"),
                  choices = c(None=NA, Tree="Tree", Time="Time", Age="Age"),
                  selected = "None"
      ),
      
      # Clustering
      conditionalPanel(
        condition = "input.e_split == 'Tree' || input.e_split == 'Time' || input.e_split == 'Age'",
        
        h4("Clustering"),
        
        # Automatic clustering
        # Only appears if split is selected
        checkboxInput("auto_cluster", label="Identify clusters automatically"),
        
        conditionalPanel(
          # Select number of clusters
          # Only displays when automatic clustering is enabled 
          # Max is number of series
          # 0 means automatic number of clusters
          condition = "input.auto_cluster",
          
          numericInput("n_clusters", strong("Number of clusters"), min=0, value=2)
        )
      ),
      
      # Link function
      selectInput(inputId = "link",
                  label = h4("Link"),
                  choices = c(Identity = "identity",
                              Log = "log"),
                  selected = "log"
      ),
      
      # Model fitting optimizer
      selectInput(inputId = "optim",
                  label = h4("Optimizer"),
                  choices = c(Sequential = "sequential",
                              Alternate = "alternate",
                              GLM = "glm",
                              GAM = "gam"),
                  selected = "alternate"
      ),
      
      
      
      # Standardize! (button)
      actionButton("standardize", "Standardize my data")
    ),
    
    # IO
    wellPanel(
      h3("Data"),
      # File input
      h4("Load data"),
      fileInput("tra_upload", "Upload tree ring data", multiple=FALSE),
      
      # Download results
      # TRA, change list, final standardization w/ data
      h4("Save data"),
      downloadButton("new_tra", "Download updated dataset"),
      downloadButton("change_list", "Download change list"),
      downloadButton("last_standardization", "Download last standardization")
      
    )
  ),
  
  mainPanel(
    tabsetPanel(
      
      # Tab 1
      tabPanel(strong("Standardization"), 
        
        # All standardization plots
        # Select plot using dropdown menu
        # Should change options given model
        uiOutput("standardization_plot_options"),
        
        # Selected plot
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
  
        # Summary for series
        # Sortable table: http://glimmer.rstudio.com/szakacs/FooTableDemo/
        # Name
        # Start year, end year
        # Tree effect (if applicable)
        # Standard deviation of residuals
        dataTableOutput("series_summary"),
        
        # List of series to include in chronology
        # Should be merged into summary table
        # All selected initially, unless specified by tra
        # https://groups.google.com/forum/#!topic/shiny-discuss/38Edf85wl_g
        uiOutput("series_checklist")      

      ),
      
      # Tab 3
      tabPanel(strong("Crossdating"), 
        
        # Select series to crossdate
        uiOutput("crossdate_series_list"),
        selectInput("crossdate_plot_choice", label=strong("Crossdating plot"),
                    choices=c("Std. series and chronology"="series_chron_cd_plot",
                              "Residuals"="residual_cd_plot"),
                    selected="series_chron_cd_plot"
        ),
        
        # Graph of residuals by time for selected series
        # Can click on a point to select
        # https://gist.github.com/trestletech/5929598
        plotOutput("crossdate_plot"),
        
        # Show standard deviation of residuals
        strong("Sigma Sq."),
        textOutput(outputId="sd_series_resid"),
        
        # Shift year +/-
        numericInput("offset", strong("Shift series"), value=0),
       
        # Show outcomes of shifting
        dataTableOutput("shift_checks"),
       
        # Select year
        numericInput("selected_year", strong("Select Year"), value=2000),
        
        # Merge rings
        actionButton("merge", "Merge ring with next"),
       
        # Split ring
        actionButton("split", "Split ring into two"),
       
        # Reset series
        actionButton("reset", "Reset series")
      ),
      
      # Tab 4
      tabPanel(strong("Hierarchical cluster"),
               # Hierarchical cluster plot of series
               # ggdendro library
               plotOutput("hclust_series_plot")
      ),
      
      # Tab 5
      tabPanel(strong("Changes"),
              tableOutput("changes")
      ),
      
      # Tab 6
      tabPanel(strong("About"),
        
               h1("Theory"),
        "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.",
        
        h1("Help"),
        "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.",
        
        h1("Author"),
        "Alice I. Cecile, 2014"
      )       
    )
  )
))