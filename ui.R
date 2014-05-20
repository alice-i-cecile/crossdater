# Libraries ####
library(shiny)

# UI ####
shinyUI(pageWithSidebar(
  
  # Heading
  headerPanel("crossdateR"),
    
  # Sidebar
  sidebarPanel(
    
    # Standardization options
    {wellPanel(
    
      h3("Standardization options"),
      
      # Dependent variable to use
      # Generated automatically from tra data
      # Only show if more than one choice
      uiOutput("dep_vars"),
      
      # Standardization model
      checkboxGroupInput("model", h4("Model effects"),
                         c("Age", "Tree", "Time"),
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
    )},
    
    # IO
    {wellPanel(
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
      
    )}
  ),
  
  # Main
  mainPanel(
    tabsetPanel(
      
      # Tab 1: Standardization
      {tabPanel(strong("Standardization"), 
        
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
      )},
      
      # Tab 2: Series
      {tabPanel(strong("Series"),              
  
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

      )},
      
      # Tab 3: Crossdating
      {tabPanel(strong("Crossdating"), 
        
        # Select series to crossdate
        uiOutput("crossdate_series_list"),
        selectInput("crossdate_plot_choice", label=strong("Crossdating plot"),
                    choices=c("Standardized series and chronology"="series_chron_cd_plot",
                              "Residuals"="residual_cd_plot",
                              "Changepoints"="changepoint_plot"),
                    selected="series_chron_cd_plot"
        ),
        
        # Graph of residuals by time for selected series
        # Can click on a point to select
        # https://gist.github.com/trestletech/5929598
        plotOutput("crossdate_plot"),
        
        # Show standard deviation of residuals
        strong("Sigma Sq."),
        textOutput(outputId="sd_series_resid"),
        
        # Show standard deviation of residuals
        strong("Current shift"),
        textOutput(outputId="current_shift"),
        
        # Shift year +/-
        numericInput("offset", strong("Shift series"), value=0),
       
        # Select year
        numericInput("selected_year", strong("Select Year"), value=2000),
        
        # Merge rings
        actionButton("merge", "Merge ring with next"),
       
        # Split ring
        actionButton("split", "Split ring into two"),
       
        # Reset series
        actionButton("reset", "Reset series"),
        
        # Show outcomes of shifting
        dataTableOutput("shift_checks")
      )},
      
      # Tab 4: Hierarchical cluster
      {tabPanel(strong("Hierarchical cluster"),
               # Hierarchical cluster plot of series
               # ggdendro library
               plotOutput("hclust_series_plot")
      )},
      
      # Tab 5: Changes
      {tabPanel(strong("Changes"),
              tableOutput("changes")
      )},
      
      # Tab 6: About
      {tabPanel(strong("About"),
        
        h3("Philosophy"),
        "Crossdating tree rings is always going to be hard. But it doesn't have to be painful. CrossdateR combines a rigorous and innovative model-based perspective with a modern, easy to learn graphical user interface to allow you  explore your data at an unprecedented level of detail.", br(), br(),
        
        "Unlike other programs, crossdateR fully integrates crossdating and tree-ring standardization. The reason is simple: the same secondary effects that interfere with the construction of the final chronology also interferes with determining the common signals needed for crossdating. Most traditional dendrochronological statistics are deliberately absent as they have been largely replaced by simpler, more unified model-based alternatives.", br(), br(),
        
        "You should be able to focus on your science, not your software. With a simple integrated workflow, interactive changes and a clean visual exploration of your series crossdateR allows you to let your intuition guide you.",
        
        h3("Help"),
        
        strong("Uploading:"), br(),br(),
        
        strong("Downloading:"), br(),br(),
        
        strong("More help:"),
        
        h3("Theory"),
        "crossdateR uses a model-based perspective on tree ring data to power its standardization and crossdating tools. Instead of using the traditional assortment of algorithms we allow the user to make explicit decisions about the patterns in their data and then use powerful model-fitting tools to do the heavy lifting.", br(), br(),
        
        "Picking out the appropriate standardization technique is one of the most challenging and subjective tasks in tree ring analysis. By conceptualizing various standardization algorithms as models that explain the tree ring data selected, we can use model selection tools to help us decide. When working with real data, researchers can never know the true time signal that they seek. As a result, models must be evaluated on the basis of their ability to parsimoniously explain the data available. If the model can reliably capture the key features of the data, it has a better chance of correctly seperating the desired time signal from the time. CrossdateR provides a number of model fit statistics for descriptive and model selection purposes. Adjusted R-squared, AICc and BIC are the most appropriate metrics for model selection as they balance model fit and complexity.", br(), br(),
        
        "Although they are distinct, there exist rough analogues between many traditional standardization techniques and various standardization models. The table below lists these:", br(), br(),
        
        tableOutput("standardization_analogues"),
        
        "The Link option determines the form of the model: Identity means that the effects are additive, Log means that the effects are multiplicative. This roughly corresponds to the use of differencing vs. ratios when extracting the standardized series. In general, a log link function is more appropriate for ring width / basal area increment data but this can be checked by examining the residuals of the model.", br(), br(),
        
        "The choice of Optimizer determines the technique used to estimate the chosen model. Alternate is effectively the traditional algorithmic technique in which the Age effect is estimated before the Time effect. It is strictly inferior to the other optimizers available, suffers from segment-length curse and should only be used to explore the effects of changing the estimation technique on your chronology. Alternate and GLM optimizers perform very well and typically produce very similar results. The alternate technique is based on signal-free standardization however while the GLM technique uses generalized linear models to standardize the chronology. The GAM optimizer uses generalized additive models to fit nonparametric smooth functions to the Age effect in the model. This is usually a more realistic option than assuming a completely discontinuous Age effect. This can dramatically improve model power by reducing the degrees of freedom used for estimating the Age effect and result in a more reliable estimation of the time signal, especially when the Age effect is split.", br(), br(),
        
        "CROSSDATING STATISTICS",
        
        h3("Papers"),
        
        
        h3("Author and version information"),
          strong("Version:"), "0.1 (May 20, 2014)", br(),

          strong("Citation:"), "", br(),

          strong("Webpage:"), "https://github.com/alice-i-cecile/crossdateR", br(),          
  
          strong("Author:"), "Alice I. Cecile <alice.i.cecile@gmail.com>"
      )}       
    )
  )
))