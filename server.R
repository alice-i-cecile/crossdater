# Libraries ####
library(shiny)
library(dendrotoolkit)
library(ggplot2)
library(stringr)
library(igraph)
library(cluster)
library(ggdendro)
library(reshape2)
library(tools)
library(dplR)
library(plyr)

# Utility ####

# Find columns that could serve as a dependent variable
find_dep_vars <- function(tra)
{
  
  valid_dep_var <- function(cname){
    x <- tra[[cname]]
    x <- x[!is.na(x)]
    
    # Must be numeric in value
    if (!all(is.numeric(x))){
      return(FALSE)
    }
    
    # Must contain unique values, not replicates
    # Each value can be replicated at most 2 times
    if (cname == "Time" | cname =="Age"){
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

# Returns (corrected) root-mean square of residuals
find_rms_series <- function(series, resids, link="log", dep_var="Growth"){
  x <- resids[resids$Tree==series, dep_var]
  
  if (link=="log"){
    x <- log(x)
  }
  
  rms <- sqrt(sum(x^2, na.rm=TRUE)/(length(x[!is.na(x)])-1))
  
  return(rms)
}

# Changes ####

# Checking if a series is included in the chronology
check_included <- function(series, tra){
  head(tra[tra$Tree==series, "Include"], n=1)
}

# Apply changes to tree-ring array
apply_changes_tra <- function(tra, changes, dep_var="Growth")
{
  
  changer <- function(series, action, value){
        
    # Avoid side effects
    new_tra <- tra
#     new_tra$Time <- as.numeric(as.character(new_tra$Time))
    
    # Including / excluding
    if (action=="Include"){
      new_tra[tra$Tree==series, "Include"] <- value
    }
    
    if (action=="Shift"){
      new_tra[tra$Tree==series, "Time"] <- as.numeric(as.character(new_tra[tra$Tree==series, "Time"])) + as.numeric(value)
    }
    
    if (action=="Merge"){    
      
      # Find rows to merge
      i1 <- which(tra$Tree==series & tra$Time==value)
      i2 <- which(tra$Tree==series & tra$Time==as.character(as.numeric(value)+1))
      
      r1 <- tra[i1, ]
      r2 <- tra[i2, ]
      
      # Sum together ring widths
      new_width <- r1[[dep_var]] + r2[[dep_var]]
      
      # Assign new value to first ring
      new_tra[i1, dep_var] <- new_width  
      
      # Delete second ring
      new_tra <- new_tra[-i2, ]
      
      # Shift all following years      
      new_tra[new_tra$Tree==series & new_tra$Time > as.numeric(value), "Time"] <- new_tra[new_tra$Tree==series & new_tra$Time > value, "Time"] - 1
      
      new_tra[new_tra$Tree==series & new_tra$Time > as.numeric(value), "Age"] <- new_tra[new_tra$Tree==series & new_tra$Time > value, "Age"] - 1
      
    }
  
    if (action=="Split"){    
      
      # Find row to split
      i1 <- which(tra$Tree==series & tra$Time==value)
      
      r1 <- tra[i1, ]
      
      # Split width in half
      new_width <- r1[[dep_var]] / 2
      
      # Assign new value to first ring
      new_tra[i1, dep_var] <- new_width  
      
      # Insert second ring
      r2 <- r1
      r2[[dep_var]] <- new_width
      r2$Time <- r2$Time +1
      r2$Age <- r2$Age + 1
      
      new_tra <- rbind(new_tra, r2)
      
      # Shift all following years      
      new_tra[new_tra$Tree==series & new_tra$Time > as.numeric(value), "Time"] <- new_tra[new_tra$Tree==series & new_tra$Time > value, "Time"] + 1
      
      new_tra[new_tra$Tree==series & new_tra$Time > as.numeric(value), "Age"] <- new_tra[new_tra$Tree==series & new_tra$Time > value, "Age"] + 1
            
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
check_shifts <- function(series, tra, effects, model=c("Time", "Age"), split=NA, link="log", dep_var="Growth"){
  
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
    
  # Find rms of residuals at each position
  shift_rms <- sapply(pseudo_resids, function(x){
    y <- x[[dep_var]]
    
    if (link == "log"){
      y <- log(y)
    }
    
    sqrt(sum(y^2, na.rm=TRUE)/(length(y[!is.na(y)])-1))
  })
  
  # Format as data.frame for pretty display
  shift_df <- data.frame(shift=min_shift:max_shift, rms=shift_rms)
    
  return(shift_df)
  
}

# Modified from changepoint::single.mean.norm.calc
# Probe for changepoints in mean
# Calculates likelihood ratios between null and alternate hypotheses
# Better scores, better split
cp_mean_lhr <- function(x) {
  n = length(x)
  y = c(0, cumsum(x))
  y2 = c(0, cumsum(x^2))
  null = y2[n + 1] - y[n + 1]^2/n
  taustar = 1:(n - 1)
  alt = y2[taustar + 1] - y[taustar + 1]^2/taustar + (y2[n + 1] - y2[taustar + 1]) - ((y[n + 1] - y[taustar + 1])^2)/(n - taustar)
  
  likelihood_ratios <- null / alt
  
  return(likelihood_ratios)
  
}

# Modified from changepoint::single.var.norm.calc
# Probe for changepoints in variance
# Calculates likelihood ratios between null and alternate hypotheses
# Better scores, better split
cp_var_lhr <- function(x) {
  n = length(x)
  mu = mean(x)
  
  y = c(0, cumsum((x - mu)^2))
  null = n * log(y[n + 1]/n)
  taustar = 1:(n - 1)
  sigma1 = y[taustar + 1]/taustar
  neg = sigma1 <= 0
  sigma1[neg == TRUE] = NA
  sigman = (y[n + 1] - y[taustar + 1])/(n - taustar)
  neg = sigman <= 0
  sigman[neg == TRUE] = NA
  
  alt = taustar * log(sigma1) + (n - taustar) * log(sigman)
  
  likelihood_ratios <- null / alt
  
  return(likelihood_ratios)
}

# Find the likelihood ratio for changepoints of variance and mean
find_cp_lhr <- function(series, residuals, link="log", dep_var="Growth"){
  
  # Extract series
  series_resids <- residuals[residuals$Tree==series,]
  
  x <- series_resids[[dep_var]]
  
  # Transform data by link
  if (link=="log"){
    x <- log(x)
  }
  
  # Find log-likelihood ratios for a single mean and variance shifts
  cp_mean <- cp_mean_lhr(x)
  cp_var <- cp_var_lhr(x)
    
  # Values correspond to a breakpoint after that year
  year_names <- series_resids$Time[1:(nrow(series_resids)-1)]
  
  # Shape results into a data.frame
  cp_df <- data.frame(Year=year_names, Mean=cp_mean, Variance=cp_var)
  
  return(cp_df)
  
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
    
  hplot <- ggdendrogram(dendro_data(htree), rotate=TRUE)
  
  return(hplot)
}

# Server ####
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
            
      # Determine data type
      file_extension <- file_ext(input$tra_upload$name)
      
      # Load and convert files
      if (file_extension == "csv"){
        tra <- read.csv(input$tra_upload$datapath, header=T)
      } else if (tolower(file_extension) == "rdata") {
        tra <- load(input$tra_upload$datapath)       
      } else if (file_extension %in% c("rwl", "tridas", "txt", "fh")) {
        rwl <- read.rwl(input$tra_upload$datapath)
        tra <- rwl_to_tra(rwl)    
      } else {
        print("File extension not recognized.")
      }
      
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
      
      # Merging rings
      if(!is.null(all_merges())){
        merge_df <- all_merges()
        change_df <- rbind(change_df, merge_df)
      }
      
      # Splitting rings
      if(!is.null(all_splits())){
        split_df <- all_splits()
        change_df <- rbind(change_df, split_df)
      }
      
      # Clean up change list
      if (nrow(change_df) > 0){
        
        # Merges and splits cancel out
        r <- 1
        while (r <= nrow(change_df)){
          
          change_r <- change_df[r,]
          opp <- NA
          
          # Get rid of first opposite that each value hits
          if (change_r$Action=="Merge"){
                        
            opp <- which(change_df$Series == change_r$Series & change_df$Value == change_r$Value & change_df$Action=="Split")[1]
                        
            # Cancel both changes out
            if (!is.na(opp)){
              change_df <- change_df[-c(opp, r),]
            }
          }
          
          # Get rid of first opposite that each value hits
          if (change_r$Action=="Split"){
            
            opp <- which(change_df$Series == change_r$Series & change_df$Value == change_r$Value & change_df$Action=="Merge")[1]
            
            # Cancel both changes out
            if (!is.na(opp)){
              change_df <- change_df[-c(opp, r),]
            }          
          }
          
          # Stay at the same row if opposite was found
          # Previous entry in that place was deleted
          if (is.na(opp)){
            r <- r + 1
          }
        }        
      }  
      

      
      # Return changes if any exist
      if (nrow(change_df) > 0){

        # Add a column for notes
        change_df$Notes <- NA
        
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
    
    # Keep track of all merges that occur      
    all_merges <- reactive({
      
      # Only run when merge button is pressed
      input$merge
      
      isolate({
        
        if(is.null(original_tra())){return(NULL)}
        
        # Create storage of old merges if it doesn't exist
        if (!exists("old_merge", envir=.GlobalEnv)){
          assign("old_merge", data.frame(Series=NA, Action=NA, Value=NA)[0,], envir=.GlobalEnv)
        }
                
      # Load in old merges
      merges <- old_merge      
      
      # Concatenate new merge
      if(!is.null(input$crossdate_series) & !is.null(input$selected_year)){
        new_merge <- data.frame(Series=input$crossdate_series, Action="Merge", Value=input$selected_year)
        merges <- rbind(merges, new_merge)
      }
      
      # Save updated merges
      assign("old_merge", merges, envir=.GlobalEnv)
      
      })
      
      return(merges)

    })
    
    # Keep track of all splits that occur      
    all_splits <- reactive({
      
      # Only run when split button is pressed
      input$split
      
      isolate({
        
        if(is.null(original_tra())){return(NULL)}
        
        # Create storage of old splits if it doesn't exist
        if (!exists("old_split", envir=.GlobalEnv)){
          assign("old_split", data.frame(Series=NA, Action=NA, Value=NA)[0,], envir=.GlobalEnv)
        }
        
        # Load in old splits
        splits <- old_split
        
        # Concatenate new split
        if(!is.null(input$crossdate_series) & !is.null(input$selected_year)){
          new_split <- data.frame(Series=input$crossdate_series, Action="Split", Value=input$selected_year)
          splits <- rbind(splits, new_split)           
        }  
        
        # Save updated splits
        assign("old_split", splits, envir=.GlobalEnv)
        
      })
      
      return(splits)
      
    })
    
    # Undo all changes to a series
    reset_series <- observe({
      
      # Proc on reset button
      input$reset
      
      isolate({
        
        # Undo shifts
        if(exists("old_shifts")){
          rev_shifts <- old_shifts[-which(as.character(old_shifts$Series) == input$crossdate_series),]
          
          if (any(is.na(rev_shifts))){
            rev_shifts <- rev_shifts[0,]
          }          
          
          rm("old_shifts")
          assign("old_shifts", rev_shifts, envir=.GlobalEnv)
        }
        
        # Undo merges
        if(exists("old_merge")){
          rev_merge <- old_merge[-which(as.character(old_shifts$Series) == input$crossdate_series),]
          
          if (any(is.na(rev_merge))){
            rev_merge <- rev_merge[0,]
          }
          
          rm("old_merge")
          assign("old_merge", rev_merge, envir=.GlobalEnv) 
        }
          
        
        # Undo splits
        if (exists("old_split")){
          rev_split <- old_split[-which(as.character(old_shifts$Series) == input$crossdate_series),]
          
          if (any(is.na(rev_split))){
            rev_split <- rev_split[0,]
          }
          
          rm("old_split")
          assign("old_split", rev_split, envir=.GlobalEnv)   
        }
      })
      
      return(NULL)
      
    })
    
    # Change dataframe to display
    output$changes <- renderTable({
      changes()
    })
    
    # Create updated dataset
    new_tra <- reactive({
      if (is.null(original_tra())){return(NULL)}
      
      # Return original if no changes have been made
      if (is.null(changes())){
        return(original_tra())
      }
      
      # Refresh based on change list
      return(apply_changes_tra(original_tra(), changes(), input$dep_var))     
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
        
        # Only use included data and series
        inc_tra <- new_tra()
        inc_tra <- inc_tra[inc_tra$Include==TRUE,]
        
        # Clustering options
        
        e_split <- ifelse(input$e_split == "None", NA, input$e_split)
        
        auto_cluster <- input$cluster_type == "Automatic" & !is.na(e_split)
        
        if (input$cluster_type == "Complete"){
          inc_tra[[paste(input$e_split, "Split", sep="_")]] <- inc_tra$Tree
        }
        
        # Standardize
        std <- standardize_tra(inc_tra, 
                        model=input$model, split=e_split, 
                        link=input$link, dep_var=input$dep_var, 
                        optim=input$optim, 
                        auto_cluster=auto_cluster, n_clusters=input$n_clusters, 
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
    # All standardization plots
    # Select plot using dropdown menu
    # Should change options given model
    output$standardization_plot_options <- renderUI({
      
      # Update every time standardization is complete
      standardization()
      
      # Common plots
      plot_choices <- c("Residuals density"="residual_density_plot",
                        "Sample depth by time"="sample_depth_time_plot",
                        "Sample depth by age"="sample_depth_age_plot",
                        "Mean series length"="series_length_plot")
      
      # Plots depending on effects
      isolate({
        
        if("Tree" %in% input$model){
          plot_choices <- c(plot_choices,
                            "Tree effect"="tree_effect_plot",
                            "Tree effect density"="tree_effect_density_plot",
                            "Tree effect vs. age at sampling"="tree_effect_age_plot", 
                            "Tree effect vs. year of birth"="tree_effect_year_plot"
                            )
        }
        
        if("Time" %in% input$model){
          plot_choices <- c(plot_choices,
                            "Time effect"="time_effect_plot",
                            "Time effect density"="time_effect_density_plot"
          )
        }
        
        if("Age" %in% input$model){
          plot_choices <- c(plot_choices,
                            "Age effect"="age_effect_plot",
                            "Age effect density"="age_effect_density_plot"
          )
        }
        
      })
      
      # Use time effect as default if available
      selected_plot <- isolate({ifelse("Time" %in% input$model, "time_effect_plot", "sample_depth_time_plot")})
        
      
      # Building input UI
      selectInput("std_plot_display", label=strong("Standardization plot"),
                  choices=plot_choices,
                  selected=selected_plot  
      )
    })    
    
    # Making plot
    output$standardization_plot <- renderPlot({
        
        # Requires standardization to be complete        
        if(is.null(standardization())){return(NULL)}
        
        print(standardization()$plots[[input$std_plot_display]])
    })
    # Model fit table
    output$model_fit <- renderTable({
      # Requires standardization to be complete        
      if(is.null(standardization())){return(NULL)}
      
      model_fit <- data.frame(standardization()$fit)
      
      names(model_fit) <- c("Number of data points", "Number of parameters estimated", "Standard deviation of residuals", "Total sum of squares", "Residual sum of squares", "Log-likelihood", "R^2", "Adjusted R^2", "AIC", "AICc", "BIC")
      
      return(model_fit)
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
          sst$rms <- sapply(sst$Series, find_rms_series, resids=standardization()$dat$residuals, link=input$link, dep_var=input$dep_var)
          
          names(sst)[names(sst)=="rms"] <- "Root mean square of series residuals"
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
      # Plot to show changes
      # Standardization is rerun
      # Or when data is modified
      input$crossdate_series;standardization();new_tra();input$crossdate_plot_choice; input$selected_year
      
      # Generate updated residuals
      isolate({
        
        new_residuals <- pseudo_residuals_tra(input$crossdate_series, new_tra(), standardization()$effects, input$model, input$split, input$link, input$dep_var)
      
      if (input$crossdate_plot_choice=="series_chron_cd_plot"){
        # Residual crossdating plot
        # Transform y
        # Dotted line shows limit of existing chronology
        # If no predicted values exist, compare to base level
        my_plot <- make_std_series_chron_plot(input$crossdate_series, new_residuals, standardization()$effects, input$split, input$link, input$dep_var) + geom_vline(x=input$selected_year)
      } else if (input$crossdate_plot_choice=="residual_cd_plot"){
        # Standardized series plus chronology crossdating plot
        # Transform y
        # Dotted line shows limit of existing chronology
        # If no predicted values exist, compare to base level
        my_plot <- make_series_resid_plot(input$crossdate_series, new_residuals, standardization()$fit$sigma_sq, input$link, input$dep_var) + geom_vline(x=input$selected_year)
      } else if (input$crossdate_plot_choice=="changepoint_plot"){
        # Changepoint graph for mean and variance
          
          # Find changepoint scores
          cp_df <- find_cp_lhr(input$crossdate_series, new_residuals, input$link, input$dep_var)        
          cp_melt <- melt(cp_df, id.var="Year")
          
          # Plot variance and mean changepoint likelihood ratios on top of each other
          my_plot <- ggplot(cp_melt, aes(x=Year, y=value, colour=variable)) + geom_line() + theme_bw() + ylab("Likelihood ratio (break at year / no break") + scale_colour_discrete("Type of change") + geom_hline(y=1) + geom_vline(x=input$selected_year)
      }
    
      })
      return(print(my_plot))
    })
    
    # Root mean square of series residuals
    output$sd_series_rms <- renderText({
      if (is.null(standardization())){return(NULL)}
      
      if (is.null(input$crossdate_series)){return(NULL)}
      
      # Triggers
      new_tra()
      
      # Updated residuals
      isolate(new_residuals <- pseudo_residuals_tra(input$crossdate_series, new_tra(), standardization()$effects, input$model, input$split, input$link, input$dep_var))
      
      return(isolate(
        find_rms_series(input$crossdate_series, resids=new_residuals, link=input$link, dep_var=input$dep_var)
        ))
    })
    
    # Display current shift
    output$current_shift <- renderText({
      if (is.null(input$crossdate_series)){return(NULL)}
      
      if (is.null(standardization())){return(NULL)}
      
      
      current_shift <- all_shifts()[all_shifts()$Series == input$crossdate_series, "Value"]
            
      if (length(current_shift)==0){
        return ("0")
      }
      
      return(current_shift)
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
      
      if(is.null(standardization())){return(NULL)}
      
      
      input$crossdate_series;standardization()
      
      isolate({shift_checks <- check_shifts(input$crossdate_series, new_tra(), standardization()$effects, input$model, input$split, input$link, input$dep_var)})
      
      names(shift_checks) <- c("Shift", "Standard deviation of series residuals")
      
      return(shift_checks)
    }, options=list(aaSorting=list(c(1, "asc")), iDisplayLength=5))

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
  
  # Change notes ####
  {
    # Change to make note on
    output$change_notes_choices <- renderUI({
      if (is.null(original_tra())){return(NULL)}
      selectInput("change_note_choice", label=strong("Make note on"),
                  choices=unique(original_tra()$Tree)
      )
    })  
  }
    
  # Saving output ####
  {
    # New dataset
    
    output$new_tra <- downloadHandler(
      filename = "new_tra.csv",
      
      content = function(file){
        write.csv(new_tra(), file)
      }
    )
    
    # Change list
    output$change_list <- downloadHandler(
      filename = "change_list.csv",
      
      content = function(file){
        write.csv(changes(), file)
      }
    )
    
    # Final standardization
    output$last_standardization <- downloadHandler(
      filename = "standardization.RData",
      
      content = function(file) {
        
        last_standardization <- standardization()
        
        save(last_standardization, file = file)
      }
    )
      
  }

  # Help details ####
  {
  output$standardization_analogues <- renderTable({
    
    standardization_analogues_df <- data.frame(
    "Traditional technique"=c(
      "Regional curve standardization",
      "Multiple regional curves",
      "Flat detrending",
      "Individual series spline detrending"
    ), 
    "Analagous model"= c(
      "Time and Age effects",
      "Time and Age effects, split Age effect",
      "Tree and Time effects",
      "Time and Age effects, split Age effect, complete clustering, GAM optimization"
    ),
    "Notes"=c(
      "Add a Tree effect to address modern sample bias",
      "Use manual clustering to address ecologically distinct growth patterns",
      "Model-based alternative does not suffer from segment-length curse",
      "Model-based alternative does not suffer from segment-length curse"
    ))
    
    # Ensure correct use of spaces
    names(standardization_analogues_df) <- c("Traditional technique", "Analagous model", "Notes")
    
    return(standardization_analogues_df)
  })
  }

})
