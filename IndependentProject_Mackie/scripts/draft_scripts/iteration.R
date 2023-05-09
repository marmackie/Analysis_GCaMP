#### Info ####
# This script is purely for the For-loops used to iterate between
# stimulant folder --> jnl folder --> worm folder --> log file

#### Load Libraries ####-----------------------------------------------
library(tidyverse)
library(here)
library(tidytext)

#### Iteration through stim, jnl, and worm folders ####----------------

# points to worm strain + stimulant folder on the computer
stim_path <- here("IndependentProject_Mackie", "data", "GCaMP_data")
# lists all folders in that path
stim_folders <- dir(path = stim_path)
stim_folders

# Iterates through each stimulant folder
for (i in 1:length(stim_folders)){
  
  # points to JNL folder on the computer
  jnl_path <- here("IndependentProject_Mackie", "data", "GCaMP_data", stim_folders[i])
  # lists all JNL folders in that path
  jnl_folders <- dir(path = jnl_path)
  jnl_folders
  
  # Iterates through each JNL folder within the current stimulant folder
  for (j in 1:length(jnl_folders)){
    # points to worm folder on the computer
    worm_path <- here("IndependentProject_Mackie", "data", "GCaMP_data", stim_folders[i], jnl_folders[j])
    # lists all worm folders in that path
    worm_folders <- dir(path = worm_path)
    worm_folders
    
    # Iterates through each worm folder within the current JNL folder
    for (k in 1:length(worm_folders)){
      
      # points to the location of tracked worm data (logfiles) on the computer
      logfile_path <- here("IndependentProject_Mackie", "data", "GCaMP_data", stim_folders[i], jnl_folders[j], worm_folders[k])
      # lists all logfiles in that path (contains ".log")
      logfiles <- dir(path = logfile_path, pattern = ".log")
      logfiles
      
    } # ends iteration through worm folders
    
  } # ends iteration through JNL folders
  
} # ends iteration through stim_folders