# Libraries ####
library(shiny)
library(dendrotoolkit)
library(ggplot2)
library(stringr)
library(igraph)
library(cluster)
library(ggdendro)

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

# Returns sigma^2 of residuals
find_sigma_series <- function(series, resids, link="log", dep_var="Growth"){
  x <- resids[resids$Tree==series, dep_var]
  
  if (link=="log"){
    x <- log(x)
  }
  
  return(sd(x, na.rm=T))
}

# Changes ####

# Checking if a series is included in the chronology
check_included <- function(series, tra){
  head(tra[tra$Tree==series, "Include"], n=1)
}

# Apply changes to tree-ring array
apply_changes_tra <- function(tra, changes)
{
  
  changer <- function(series, action, value){
    
    # Avoid side effects
    new_tra <- tra
      
    # Including / excluding
    if (action=="Include"){
      new_tra[tra$Tree==series, "Include"] <- value
    }
    
    if (action=="Shift"){
      new_tra[tra$Tree==series, "Time"] <- as.numeric(as.character(new_tra[tra$Tree==series, "Time"])) + as.numeric(value)
    }
    
    return(new_tra)
  }
  
  # Only make changes if they exist
  if (nrow(changes)>0){
    
    # Apply changes one a time
    for (r in 1:nrow(changes)){
      tra <- changer(changes[r, "Series"], changes[r, "Action"], changes[r, "Value"])
    }
    
  }
 
  
  return(tra)
}

# Pseudo-residuals ####
pseudo_residuals_tra <- function(series, tra, effects, model, split, link, dep_var){
  
  # Repair effects if needed
  # Due to missing coefficients
  skele <- make_skeleton_effects(tra, model, split, link)
  
  effects <- synchronize_effects(effects, skele, split)

  # Only generate for new series
  short_tra <- tra[tra$Tree==series, ]
  
  predicted <- predicted_tra(effects, short_tra, model, split, link, dep_var)
    
  resids <- residuals_tra(short_tra, predicted, link, dep_var)

  return (resids)
}

# Shifting / merging / splitting ####
check_shifts <- function(series, tra, effects, model, split, link, dep_var){
  
  # Extract data
  series_tra <- tra[tra$Tree==series,]
    
  if ("Time" %in% split){
    group <- series_tra$Time_Split[1]
    
    chron_start <- min(as.numeric(names(effects$Time[[group]])))
    chron_end <- max(as.numeric(names(effects$Time[[group]])))  
  } else {
    chron_start <- min(as.numeric(names(effects$Time)))
    chron_end <- max(as.numeric(names(effects$Time)))
  }
    
  series_start <- min(as.numeric(series_tra$Time))
  series_end <- max(as.numeric(series_tra$Time))
                                          
  series_length <- nrow(series_tra)
  
  
  # Require at least one year overlap
  min_shift <- chron_start - series_end 
  max_shift <- chron_end - series_start
  
  # Find pseudo-residuals given shifts
  shift_pseudo_residuals <- function(shift){
    shifted_series_tra <- tra
    shifted_series_tra[tra$Tree==series,"Time"] <- as.numeric(series_tra$Time) + shift
    
    shifted_pr <- pseudo_residuals_tra(series=series, tra=shifted_series_tra, effects=effects, model=model, split=split, link=link, dep_var=dep_var)
    
    return(shifted_pr)
  }
  
  # Compute pseudo-residuals
  pseudo_resids <- lapply(min_shift:max_shift, shift_pseudo_residuals)
    
  # Find sd of residuals at each position
  shift_sd <- sapply(pseudo_resids, function(x){sd(x[[dep_var]], na.rm=T)})
  
  # Format as data.frame for pretty display
  shift_df <- data.frame(Shift=min_shift:max_shift, sigma=shift_sd)
  
  return(shift_df)
  
}

# Plotting ####
make_series_resid_plot <- function(series, resids, sigma_chron, link="log", dep_var="Growth"){  
  dat <- resids[resids$Tree==series, ]
  dat$Time <- as.numeric(as.character(dat$Time))
  names(dat)[names(dat)==dep_var] <- "y"
  
  first_year <- min(as.numeric(as.character(resids$Time)))
  last_year <- max(as.numeric(as.character(resids$Time)))
  
  my_plot <- ggplot(dat, aes(x=Time, y=y)) + geom_line() + xlab("Year") + ylab("Residuals") + theme_bw() + xlim(c(first_year, last_year))
  
  
  # Adding lines showing expected value plus sd of chronology residuals
  if (link=="log"){
    sigma_u <- exp(0 + sigma_chron)
    sigma_l <- exp(0 - sigma_chron)
    my_plot <- my_plot + geom_hline(y=1) + geom_hline(y=sigma_u, linetype="dashed") + geom_hline(y=sigma_l, linetype="dashed")
  } else {
    sigma_u <- 0 + sigma_chron
    sigma_l <- 0 - sigma_chron
    my_plot <- my_plot + geom_hline(y=0) + geom_hline(y=sigma_u, linetype="dashed") + geom_hline(y=sigma_l, linetype="dashed")  }
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

make_hclust_plot <- function(resids, link="log", dep_var="Growth"){
  dist_matrix <- find_dist_tra(resids, group_by="Time", link=link, distance="euclidean", dep_var=dep_var)
  
  dummy_d <- dist(dist_matrix)
  dummy_d[] <- dist_matrix[lower.tri(dist_matrix)]
  
  htree <- hclust(dummy_d)
    
  hplot <- ggdendrogram(dendro_data(htree, type="triangle"), rotate=TRUE)
  
  return(hplot)
}

# IO ####
shinyServer(function(input, output, session) { 
  
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
    changes <- reactive({
      
      # Data must be loaded
      if (is.null(original_tra())){return(NULL)}
      
      # Change list prototype
      change_df <- data.frame(Series=NA, Action=NA, Value=NA)[0,]
      
      # Including / excluding series
      if (!is.null(input$inc_series)){
      
        all_trees <- unique(original_tra()$Tree)
        
        orig_inc_trees <- unique(original_tra()[original_tra()$Include==TRUE, "Tree"])
        orig_exc_trees <- unique(original_tra()[original_tra()$Include==FALSE, "Tree"])
        
        inc_trees <- input$inc_series
        exc_trees <- setdiff(all_trees, inc_trees)
        
        new_inc_trees <- inc_trees[!(inc_trees %in% orig_inc_trees)]
        new_exc_trees <- exc_trees[!(exc_trees %in% orig_exc_trees)]
        
        # New trees to include
        if (length(new_inc_trees > 0)){
          inc_df <- data.frame(Series=new_inc_trees, Action="Include", Value=TRUE)
          change_df <- rbind(change_df, inc_df)
        }
        
        # New trees to exclude
        if (length(new_exc_trees > 0)){
          exc_df <- data.frame(Series=new_exc_trees, Action="Include", Value=FALSE)
          change_df <- rbind(change_df, exc_df)
        }
      }
      
      # Shifting series
      if(!is.null(all_shifts())){
          shift_df <- all_shifts()
          change_df <- rbind(change_df, shift_df)
      }
      
      # Return changes if any exist
      if (nrow(change_df) > 0){

        # Make sure columns are not factors
        change_df <- data.frame(lapply(change_df, as.character), stringsAsFactors=FALSE)
        
        return(change_df)
      } else {
        return(NULL)
      }
    })
    
    # Keep track of all shifts that occur      
    all_shifts <- reactive({
      if(is.null(original_tra())){return(NULL)}
      
      # Create storage of old shifts if it doesn't exist
      isolate(
      if (!exists("old_shifts", envir=.GlobalEnv)){
        assign("old_shifts", data.frame(Series=NA, Action=NA, Value=NA)[0,], envir=.GlobalEnv)
      })
      
      # Load in old shifts
      isolate(shifts <- old_shifts)
      
      # Concatenate new shifts
      if(!is.null(input$offset)){
        # Don't copy settings over to new series
        isolate({
          if (input$offset!=0){
        
              new_shift <- data.frame(Series=input$crossdate_series, Action="Shift", Value=input$offset)          
              shifts <- rbind(shifts, new_shift)
              
              # Use only most recent shift for the new series
              shifts_i <- which(shifts$Series==input$crossdate_series)
              redundant_shifts <- shifts_i[shifts_i < nrow(shifts)]
              if (length(redundant_shifts)>0){
                shifts <- shifts[-redundant_shifts,]
              }
              
          } else {
            # Remove all shift changes when shift set to 0 years
            shifts <- shifts[-which(shifts$Series==input$crossdate_series),] 
          }
        })
      }            
      
      # Save updated shifts
      isolate(assign("old_shifts", shifts, envir=.GlobalEnv))
      
      
      
      return(shifts)
    })
    
    # Change dataframe to display
    output$changes <- renderTable({
      changes()
    })
    
    # New data set
    new_tra <- reactive({
      if (is.null(original_tra())){return(NULL)}
      
      # Return original if no changes have been made
      if (is.null(changes())){
        return(original_tra())
      }
      
      # Refresh based on change list
      return(apply_changes_tra(original_tra(), changes()))     
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
        
        # Only use included series
        inc_tra <- new_tra()
        inc_tra <- inc_tra[inc_tra$Include==TRUE,]
        
        # Standardize
        std <- standardize_tra(inc_tra, 
                        model=input$model, split=input$e_split, 
                        link=input$link, dep_var=input$dep_var, 
                        optim=input$optim, 
                        auto_cluster=input$auto_cluster, n_clusters=input$n_clusters, 
                        show_plots=F, return_data=T)
        
        # Add in pseudoresiduals
        # For excluded but relevant series
        exc_series <- unique(original_tra()[new_tra()$Include==F, "Tree"])
        
        if (length(exc_series)>0){
          pseudo_resids <- pseudo_residuals_tra(exc_series, new_tra(), std$effects, model=input$model, split=input$split, link=input$link, dep_var=input$dep_var)
          
          std$dat$residuals <- rbind(std$dat$residuals, pseudo_resids)
        }
        
        return(std)
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

    # Crossdating plots
    output$crossdate_plot <- renderPlot({
      if (is.null(standardization())){return(NULL)}
      if (is.null(input$crossdate_series)){return(NULL)}
      
      # Trigger when series selected changes
      # Or when data is modified
      input$crossdate_series
      new_tra()
      
      # Generate updated residuals
      isolate(new_residuals <- pseudo_residuals_tra(input$crossdate_series, new_tra(), standardization()$effects, input$model, input$split, input$link, input$dep_var))
      
      if (input$crossdate_plot_choice=="series_chron_cd_plot"){
        # Residual crossdating plot
        # Transform y
        # Dotted line shows limit of existing chronology
        # If no predicted values exist, compare to base level
        my_plot <- isolate(make_std_series_chron_plot(input$crossdate_series, new_residuals, standardization()$effects, input$split, input$link, input$dep_var))
      } else if (input$crossdate_plot_choice=="residual_cd_plot"){
        # Standardized series plus chronology crossdating plot
        # Transform y
        # Dotted line shows limit of existing chronology
        # If no predicted values exist, compare to base level
        my_plot <- isolate(make_series_resid_plot(input$crossdate_series, new_residuals, standardization()$fit$sigma_sq, input$link, input$dep_var))
      }
      return(print(my_plot))
    })
    
    # Standard deviation of series residuals
    output$sd_series_resid <- renderText({
      if (is.null(standardization())){return(NULL)}
      
      # Triggers
      input$crossdate_series
      new_tra()
      
      # Updated residuals
      isolate(new_residuals <- pseudo_residuals_tra(input$crossdate_series, new_tra(), standardization()$effects, input$model, input$split, input$link, input$dep_var))
      
      return(isolate(
        find_sigma_series(input$crossdate_series, resids=new_residuals, link=input$link, dep_var=input$dep_var)
        ))
    })
    
    # Update shift series control
    # Currently bugged
    # Updating input occurs before other reactives realize the series has changed
    # Changing shifts to observer may fix things w/ priority
#     observe({
#       if (is.null(input$crossdate_series)){return(NULL)}
#       isolate(if(is.null(all_shifts())){return(NULL)})
#       
#       # Only refresh when series changes
#       input$crossdate_series
#       
#         # Set value in control to previous shift for series
#         isolate({
#         if (input$crossdate_series %in% all_shifts()$Series){
#           current_shift <- all_shifts()[all_shifts()$Series == input$crossdate_series, "Value"]
#         } else {
#           current_shift <- 0
#         }
#                       
#         updateNumericInput(session, "offset", value=current_shift)
#         print(paste(input$crossdate_series, current_shift))
#         
#         })
#     })
    
    # Checking all shifts
    output$shift_checks <- renderDataTable({
      
      if(is.null(input$crossdate_series)){return(NULL)}
      
      input$crossdate_series;standardization()
      
      isolate({shift_checks <- check_shifts(input$crossdate_series, new_tra(), standardization()$effects, input$model, input$split, input$link, input$dep_var)})
      
      return(shift_checks)
    }, options=list(aaSorting=list(c(1, "asc")))) 
    
    # Split ring
    
    # Merge rings
    
  }
  
  # Hierarchical series plot ####
  {
    output$hclust_series_plot <- renderPlot({
      if (is.null(standardization())){return(NULL)}
      
      return(isolate(
        print(make_hclust_plot(standardization()$dat$residuals, link=input$link, dep_var=input$dep_var))
      ))
      
    })
  }  
    
  # Saving output ####
  {
    # New dataset
    output$new_tra <- reactive(new_tra())
    
    # Final standardization
    output$standardization <- reactive(standardization())
      
  }
  
})
