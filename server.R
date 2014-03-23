
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

# Libraries ####
library(shiny)
library(dendrotoolkit)
library(ggplot2)
library(stringr)
library(igraph)

# Utility ####

# Find columns that could serve as a dependent variable
find_dep_vars <- function(tra)
{
  
  valid_dep_var <- function(cname){
    x <- tra[[cname]]
    x <- x[!is.na(x)]
    
    # Must be numeric in value
    if (any(is.na(as.numeric(x)))){
      return(FALSE)
    }
    
    # Must contain unique values, not replicates
    # Each value can be replicated at most 2 times
    if (length(unique(x)) < (length(x)/2)){
      return(FALSE)
    }
    
    return(TRUE)
  }

  dep_vars <- names(tra)[sapply(names(tra), valid_dep_var)]
  
  return(dep_vars)
}


# Summarize series
series_summary_table <- function(tra)
{
  sst <- data.frame(Series=unique(tra$Tree))
  sst$Start <- sapply(sst$Series, function(x){min(as.numeric(tra[tra$Tree==x,"Time"]))})
  sst$End <- sapply(sst$Series, function(x){max(as.numeric(tra[tra$Tree==x,"Time"]))})
  
  return(sst)
}

find_sigma_series <- function(series, resids, link="log", dep_var="Growth"){
  x <- resids[resids$Tree==series, dep_var]
  
  if (link=="log"){
    x <- log(x)
  }
  
  return(sd(x, na.rm=T))
}


# Change list ####

# Checking if a series is included in the chronology
check_included <- function(series, tra){
  head(tra[tra$Tree==series, "Include"], n=1)
}

# Apply changes to tree-ring array
apply_changes_tra <- function(tra, change_list)
{
  return(tra)
}


# Plotting ####

make_series_resid_plot <- function(series, resids, link="log", dep_var="Growth"){
  dat <- resids[resids$Tree==series, ]
  dat$Time <- as.numeric(as.character(dat$Time))
  names(dat)[names(dat)==dep_var] <- "y"
  
  first_year <- min(as.numeric(as.character(resids$Time)))
  last_year <- max(as.numeric(as.character(resids$Time)))
  
  my_plot <- ggplot(dat, aes(x=Time, y=y)) + geom_line() + xlab("Year") + ylab("Residuals") + theme_bw() + xlim(c(first_year, last_year))
  
  if (link=="log"){
    my_plot <- my_plot + geom_hline(y=1)
  } else {
    my_plot <- my_plot + geom_hline(y=0)
  }
  return(my_plot)
}

make_std_series_chron_plot <- function(series, resids, effects, split=NA, link="log", dep_var="Growth"){
  series_df <- resids[resids$Tree==series, ]  
  
  if("Time" %in% split){
    group <- series_df$Time_Split[1]
    chron <- effects$Time[[group]]
  } else{
    chron <- effects$Time
  }
  
  # Need to add time effect back to obtain "standardized series"
  # Thus invert time effect then remove
  if(link=="log"){
    series_df <- remove_effect(series_df, 1/chron, id="Time", link="log", dep_var=dep_var)
  } else {
    series_df <- remove_effect(series_df, -chron, id="Time", link="identity", dep_var=dep_var)
  }

  # Add chronology info
  chron_df <- data.frame(Time=names(chron), y=chron, id="Chronology")
  
  # Clean and combine
  series_df$id <- "Standardized series" 
  names(series_df)[names(series_df)==dep_var] <- "y"
  series_df <- series_df[, c("Time", "y", "id")]
  
  my_df <- rbind(series_df, chron_df)
  my_df$Time <- as.numeric(as.character(my_df$Time))
  
  # Plot
  my_plot <- ggplot(my_df, aes(x=Time, y=y, colour=id)) + geom_line() + xlab("Year") + ylab("Time effect") + theme_bw() + scale_colour_manual(values=c("red", "blue")) + theme(legend.position="top", legend.title=element_blank())
  
  if (link=="log"){
    my_plot <- my_plot + geom_hline(y=1)
  } else {
    my_plot <- my_plot + geom_hline(y=0)
  }
  
  return(my_plot)
}

# IO ####
shinyServer(function(input, output) { 
  
  # Processing input ####
  {
    # Reading in and processing data
    # Currently only .csv + tra
    original_tra <- reactive({
      # User has not uploaded a file yet
      if (is.null(input$tra_upload)) {
        return(NULL)
      }
      
      inFile <- input$tra_upload
      my_csv <- read.csv(inFile$datapath, header=T)
      rownames(my_csv) <- my_csv[[1]]
      my_csv <- my_csv[,-1]
      
      tra <- my_csv
      
      
      # Include all series by default
      if (!("Include" %in% names(tra))){
        tra$Include <- TRUE
      }
      
      return(tra)
      })
    
    # Identifying potential dependent variables
    output$dep_vars <- renderUI({
      if(is.null(original_tra())){return(NULL)}
      
      selectInput("dep_var", strong("Dependent variable"),
                  choices=find_dep_vars(original_tra())
      )      
    })
  }
  
  # Updating tree ring dataset ####
  {
    
    # Change list  
    output$change_list <- reactive({
            
      if (is.null(original_tra())){return(NULL)}
      
      # Including / excluding
      include_df <- data.frame(Value=input$inc_series)
      
      return(include_df)
    })
    
    # New data set
    new_tra <- reactive({
      if (is.null(original_tra())){return(NULL)}
      # Refresh based on change list
      return(apply_changes_tra(original_tra(), change_list()))     
    })
  }
    
  # Standardization ####
  {
    # Standardizing the data
    standardization <- reactive({
      # Only standardize when button is pressed      
      if(input$standardize==0){return(NULL)}
      
      return(isolate({
        
        # Data needs to be loaded
        if (is.null(original_tra())){return(NULL)}
                
        # Standardize
        return(
          standardize_tra(original_tra(), 
          model=input$model, split=input$e_split, 
          link=input$link, dep_var=input$dep_var, 
          optim=input$optim, 
          auto_cluster=input$auto_cluster, n_clusters=input$n_clusters, 
          show_plots=F, return_data=T)
        )
      }))

    })
    
    
    # Selecting plot
    
    # Making plot
    # Log transform y axis
    output$standardization_plot <- renderPlot({
        
        # Requires standardization to be complete        
        if(is.null(standardization())){return(NULL)}
        
        print(standardization()$plots[[input$std_plot_display]])
    })
    # Model fit table
    output$model_fit <- renderTable({
      # Requires standardization to be complete        
      if(is.null(standardization())){return(NULL)}
      
      return(data.frame(standardization()$fit))
    })
    
  }
  
  # Series ####
  {
    # Grabbing list of series
    output$series_checklist <- renderUI({
      
      if(is.null(original_tra())){return(NULL)}
      
      all_trees <- unique(original_tra()$Tree)
      
      # Check off boxes for the trees that are included 
      inc_trees <- all_trees[sapply(all_trees, check_included, tra=original_tra())]
      
      checkboxGroupInput("inc_series", h4("Series to include in chronology"),
      choices=all_trees,
      selected=inc_trees)
    })

    # Hierarchical series plot
    
    
    # Series summary table
    output$series_summary <- renderDataTable(
    {
      # Only shows when data is loaded
      if(is.null(original_tra())){return(NULL)} 
      else {
        sst <- series_summary_table(original_tra())
        
        # Add information on 
        if (!is.null(standardization())){
          sst$sigma <- sapply(sst$Series, find_sigma_series, resids=standardization()$dat$residuals, link=input$link, dep_var=input$dep_var)
        }
        
        return(sst)
      }
    }
    )
  }
  
  # Cross-dating ####
  {
    # Series to crossdate
    output$crossdate_series_list <- renderUI({
      if (is.null(original_tra())){return(NULL)}
      selectInput("crossdate_series", label=strong("Series to crossdate"),
                  choices=unique(original_tra()$Tree)
      )
    })


    output$crossdate_plot <- renderPlot({
      if (is.null(standardization())){return(NULL)}
      
      # Trigger when series selected changes
      input$crossdate_series
      
      if (input$crossdate_plot_choice=="series_chron_cd_plot"){
        # Residual crossdating plot
        # Transform y
        # Dotted line shows limit of existing chronology
        # If no predicted values exist, compare to base level
        isolate(print(make_std_series_chron_plot(input$crossdate_series, standardization()$dat$residuals, standardization()$effects, input$split, input$link, input$dep_var)))
      } else if (input$crossdate_plot_choice=="residual_cd_plot"){
        # Residual crossdating plot
        # Transform y
        # Dotted line shows limit of existing chronology
        # If no predicted values exist, compare to base level
        isolate(print(make_series_resid_plot(input$crossdate_series, standardization()$dat$residuals, input$link, input$dep_var)))
      }
    }) 
    # Standardized series plus chronology crossdating plot
    # Transform y
    
    # Standard deviation of series residuals
    output$sd_series_resid <- renderText({
      if (is.null(standardization())){return(NULL)}
      
      input$crossdate_series
      
      return(isolate(
        find_sigma_series(input$crossdate_series, resids=standardization()$dat$residuals, link=input$link, dep_var=input$dep_var)
        ))
    })
    # Automatic shifting
    
    # Shift series
    
    # Split ring
    
    # Merge rings
    
  }
  
  
  # Saving output ####
  {
    # New dataset
    output$new_tra <- reactive(new_tra())
    
    # Final standardization
    output$standardization <- reactive(standardization())
      
  }
  
})
