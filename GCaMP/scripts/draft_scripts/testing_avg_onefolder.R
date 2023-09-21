### Trouble-shooting
# This script is to try to create avg dff plot for each stim_jnl combo folder at a time,
# using the csv files sorted into folders (See csv_for_avg)

# created: 2023-09-17; by Marisa Mackie
# edited: 2023-09-17

#### I CANT GET THIS STUPID FUCKING BULLSHIT TO WORK

# First must specify what i is
# i = current folder in csv_for_avg

#### Load Libraries ####-----------------------------------------------
library(tidyverse)
library(here)
library(tidytext)
library(gplots)
library(RColorBrewer)
library(wrMisc)
library(dplyr)


#### Analysis ####-----------------------------------------------

# points to location of csv file on the computer
csvfolder_path <- here("GCaMP", "data", "csv_for_avg")
# lists all .csv files in that path
csvfolders <- dir(path = csvfolder_path)

# points to location of csv file on the computer
csvfile_path <- here("GCaMP", "data", "csv_for_avg", csvfolders[i])
# lists all .csv files in that path
csvfiles <- dir(path = csvfile_path)

# the maximum number of frames (rows); starts at zero
max_frames <- 0

# Iterates through each csv file
# Purpose: determine max number of frames (rows) among data (required for dff_all later)
for (l in 1:length(csvfiles)){
  
  # reads in csv files
  csv_data <- read_csv(here::here("GCaMP", "data", "csv_for_avg", csvfiles[l]))
  
  # counts current number of rows (frames) in the current csv file
  frames <- nrow(csv_data)
  
  # If current frames (rows) is greater than the max, makes that value the new max. Otherwise, max is unchanged.
  if(frames > max_frames){
    max_frames <- frames}
  
} # ends iteration through csvfiles

## Part 2b - Put dff in dff_all ----
# pre-allocates a dataframe to store dff values for each csv folder
dff_all <- data.frame(matrix(nrow = max_frames, ncol = length(csvfiles)))
# adds column names as dff_worm{n}
colnames(dff_all) <- c(paste("dff_worm",(1:length(csvfiles)),sep = ""))

# Iterates through each csv file (i.e., for each worm)
# Purpose: To grab dff data and store it into dff_all
for (m in 1:length(csvfiles)){
  
  # reads in csv files
  csv_data <- read_csv(here("GCaMP", "data", "csv_for_avg", csvfiles[m]))
  
  # counts current number of rows (frames) in the current csv file
  frames <- nrow(csv_data)
  
  # stores dff column into dff_all
  dff_all[1:frames,length(csv_files)] <- csv_data$dff
  
} # ends iteration through each csv file

"Part 2 complete!
Gathered dff values into dff_all."