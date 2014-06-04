# Parameters ####

set.seed(42)

n_trees <- 100

series_length <- 50

start_year <- 1600
end_year <- 2000

low_freq_amplitude <- 0.5
low_freq_period <- 100
low_freq_signal <- low_freq_amplitude*sin(2*pi*(start_year:end_year)/low_freq_period)

hi_freq_amplitude <- 0.4
hi_freq_period <- 10
hi_freq_signal <- hi_freq_amplitude*sin(2*pi*(start_year:end_year)/hi_freq_period)

annual_noise_sd <- 0.2
annual_signal <- rnorm(end_year-start_year+1, 0, annual_noise_sd)

noise_sd <- 0.2

tree_effect_sd <- 0.2

log_transform <- TRUE

# Creating dummy data ####

dummy_series <- function(series_name){
  series <- data.frame(x=start_year:end_year)
  
  names(series) <- series_name
  rownames(series) <- series[[1]]
  series[1] <- NA
  
  first_year <- sample(start_year:end_year, 1)
  
  last_year <- ifelse(first_year+series_length<=end_year, first_year+series_length, end_year)
  
  filled_years <- which(start_year:end_year > first_year & start_year:end_year < last_year)
  
  series[filled_years, 1] <- 0
  
  return(series)
}

slc_rwl <- Reduce(cbind, lapply(paste("T", 1:n_trees, sep=""), dummy_series))

slc_tra <- rwl_to_tra(slc_rwl)

slc_tra$Growth <- 0

# Creating effects ####
time_effect <- low_freq_signal + hi_freq_signal + annual_signal
names(time_effect) <- start_year:end_year

tree_effect <- rnorm(n_trees, 0, tree_effect_sd)
names(tree_effect) <- names(slc_rwl)

# Adding effects ####

# Adding time
slc_tra <- remove_effect(slc_tra, -time_effect, "Time", link="identity")

# Adding tree
slc_tra <- remove_effect(slc_tra, -tree_effect, "Tree", link="identity")

# Transforming and scaling ####

slc_effects <- list(Tree=tree_effect, Time=time_effect)

if (log_transform){
  slc_effects <- lapply(slc_effects, exp)
  
  slc_tra$Growth <- exp(slc_tra$Growth)
  
}

slc_effects <- rescale_effects(slc_effects, link=ifelse(log_transform, "log", "identity"))

# Saving ####

save(slc_effects, file="./Demos/slc_effects.RData")

write.csv(slc_tra, "./Demos/slc.cav")
