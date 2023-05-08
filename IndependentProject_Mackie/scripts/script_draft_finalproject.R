#### General Info ####-------------------------------------------------
# 
# This script will serve as the "drawing board" and draft of all the functions, for loops,
# and other code necessary for my final project.
# This is NOT the final draft to turn in for the Final Project.
#
# created: 2023-05-04
# created by: Marisa Mackie
# edited: 2023-05-07

#### Load Libraries ####-----------------------------------------------
library(tidyverse)
library(here)
library(tidytext)

#### Part 1 - Iterate through folders, grab data, convert to clean .csv ####----------

### Iteration through stim, jnl, and worm folders ####-----

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
      
      # reads in current logfile into rawdata
      rawdata <- read_log(here("IndependentProject_Mackie","data", "GCaMP_data", stim_folders[i],jnl_folders[j],worm_folders[k],logfiles))
      glimpse(rawdata)
      
      wormdata <- rawdata %>% 
        # adds columns for stimulant and JNL type
        mutate(strain_stim = stim_folders[i],
               jnl = jnl_folders[j],
               # renames default column names (Xn) to specified variable names
               frame = rawdata$X1,
               time_ms = rawdata$X2,
               fluor = rawdata$X3,
               # only keeps mutated columns
               .keep = "used")
      
      # For fluor column, removes commas and converts values from sci notation (chr) to expanded form (dbl)
      wormdata <- wormdata %>% 
        mutate(fluor = str_replace_all(fluor, "\\,",""),
               fluor = e_to_num(fluor)) %>% 
        # adds column for calculating dff
        mutate(dff = dff(fluor))
      glimpse(wormdata)
      
      # converts wormdata to csv and saves to clean_csv folder within data folder, naming it based on stimulant, JNL, and worm ID
      write_csv(wormdata, here("IndependentProject_Mackie", "data", "clean_csv", paste(stim_folders[i],jnl_folders[j],logfiles, sep = "_")))
      
    } # ends iteration through worm folders
    
  } # ends iteration through JNL folders
  
} # ends iteration through stim_folders


#### Part 2 - Calculate dff & plot for individual worms ####---------------

# points to location of csv file on the computer
csv_path <- here("IndependentProject_Mackie", "data", "clean_csv")
# lists all .csv files in that path
csvfiles <- dir(path = csv_path)
csvfiles

# pre-allocates dataframe to hold all dff values
dff_all <- data.frame(matrix(ncol = 4, nrow = length(csvfiles)))
# column names
colnames(dff_all) <- c("strain_Stim","jnl","worm","dff")
                      
# Iterates over each csv file
for (i in 1:length(csvfiles)){
  
  # reads in csv files
  current_data <- read_csv(csvfiles[i])
  
  # gets the
  
  # gets the filename based on the current csv file
  dff_all$filename[i] <- csvfiles[i]
  
  # calculates dff using fluorescence column
  dff_all$dff <- dff(current_data$fluor[i])
  
}