#### General Info ####-------------------------------------------------
# 
# Trouble-shooting
# This script is to run plot individual dff of one SPECIFIC FOLDER at a time
#
# created: 2023-09-17; by Marisa Mackie
# edited: 2023-09-17

# First, one must indicate what i, j are.
# i = stim folder number
# j = JNL number within stim folder

#### Load Libraries ####-----------------------------------------------
library(tidyverse)
library(here)
library(tidytext)
library(gplots)
library(RColorBrewer)
library(wrMisc)
library(dplyr)

#### Functions ####----------------------------------------------------
# globally removes sci notation
options(scipen = 999)

### Function to convert strings of scientific notation containing "e" (i.e., 1.234e+005) to numeric in expanded form (double)
e_to_num <- function(string) {
  # finds first instance of "e" in the string, subsets everything before it & converts it to a numeric (double)
  value <- as.numeric(substring(string, 1, unlist(gregexpr("e", string))[1]))
  # finds first instance of "+" in the string, subsets everything after it & converts it to a numeric (double)
  # note: this number is the exponent, so we will set 10^exponent
  exponent <- 10^(as.numeric(str_sub(string, unlist(gregexpr("\\+", string))[1]+1, str_length(string))))
  # multiplies value by the 10^n exponent
  expanded_num <- value*exponent
  # returns numeric (double) of original string in expanded form
  return(expanded_num)
}

# Note: Problem here. as.numeric() works differently now, such that numbers in scientific
# notation with 3 digits or less (i.e. "1.61e+008") stay in scientific notation when 
# mathematically operated upon. The result stays in scientific notation after e_to_num(),
# and results in an NA value when trying to calculate aything with it. When doing dff with
# an NA value present, it turns the whole dff column into NAs, resulting in an empty plot.
# Current solution (temporary): drop NAs between using e_to_num() and dff()

# Function for calculating df/f (% change in fluorescence)
dff <- function(data) {
  # sets baseline fluorescence
  f0 <- mean(data[10:40]) # averages fluorescence data from frame 10-40 (1-4s)
  # calculates df/f as a percentage
  dff <- ((data - f0)/f0)*100
  # returns dff
  return(dff)
}


#### Analysis----------------------------------------------------

# worm_num is the current worm being analyzed overall
worm_num = 0

# points to worm strain + stimulant folder on the computer
stim_path <- here("GCaMP", "data", "GCaMP_data")
# lists all folders in that path
stim_folders <- dir(path = stim_path)

# points to JNL folder on the computer
jnl_path <- here("GCaMP", "data", "GCaMP_data", stim_folders[i])
# lists all JNL folders in that path
jnl_folders <- dir(path = jnl_path)

# points to worm folder on the computer
logfile_path <- here("GCaMP", "data", "GCaMP_data", stim_folders[i], jnl_folders[j])
# lists all worm folders in that path
logfiles <- dir(path = logfile_path, pattern = ".log")

# Iterates through each logfile within the current JNL folder
for (k in 1:length(logfiles)){
  
  # adds +1 to worm count
  worm_num = worm_num + 1
  
  ### Step 1b - Read in data & convert to csv----
  
  # reads in current logfile into rawdata
  rawdata <- read_log(here("GCaMP","data", "GCaMP_data", stim_folders[i],jnl_folders[j],logfiles[k]))
  
  wormdata <- rawdata %>% 
    # adds column for stimulant and JNL type
    mutate(stim_jnl = paste(stim_folders[i],jnl_folders[j], sep = "_"),
           # renames default column names (Xn) to specified variable names
           frame = rawdata$X1,
           time_s = (rawdata$X2)/1000, # converts time in ms to s
           fluor = rawdata$X3,
           .keep = "used") # only keeps mutated columns
  
  # For fluor column, removes commas and converts values from sci notation (chr) to expanded form (dbl)
  wormdata <- wormdata %>% 
    mutate(fluor = str_replace_all(fluor, "\\,",""),
           fluor = e_to_num(fluor)) %>%
    # drops rows with NA values
    drop_na(fluor) %>%
    # adds column for calculating dff
    mutate(dff = dff(fluor))
  
  # converts wormdata to csv and saves to clean_csv folder within data folder, naming it based on stimulant, JNL, and worm ID
  write_csv(wormdata, here("GCaMP", "data", "clean_csv", paste(stim_folders[i],jnl_folders[j],logfiles[k], sep = "_")))
  
  
  ### Step 1b - Generate individual dff plots----
  
  if(str_detect(wormdata$stim_jnl[k], "JNL_10off_120on_50off") == TRUE){
    
    # generates plot of % df/f over time in s
    ggplot()+
      
      # sets x-axis to match time of JNL & sets y-axis from -100 to 100
      coord_cartesian(xlim = c(0,180), ylim = c(-100,100))+
      
      
      # adds red vertical line to notable timepoints for stimulant ON & OFF
      geom_vline(xintercept = 10, color = "red")+
      geom_vline(xintercept = 130, color = "red")+
      
      # specifies line plot, & size/color of line
      geom_line(data = wormdata,
                mapping = aes(x = time_s, y = dff),
                color = "black", linewidth = 1)+
      
      # specifies plot labels
      labs(x = "Time (s)",
           y = "% dF/F",
           title = "Change in fluorescence over time",
           subtitle = paste(str_replace_all(stim_folders[i], "_", " "), jnl_folders[j]),
           caption = logfiles[k])+
      
      
      # specifies color and size of plot elements
      theme(plot.title = element_text(size = 12, face = "bold"),
            plot.subtitle = element_text(size = 10, color = "saddlebrown"),
            plot.caption = element_text(size = 8, color = "gray30"),
            axis.title = element_text(size = 10))
  }
  
  if(str_detect(wormdata$stim_jnl[k], "JNL_10off_20on_20off") == TRUE){
    
    # generates plot of % df/f over time in s
    ggplot()+
      
      # sets x-axis to match time of JNL & sets y-axis from -100 to 100
      coord_cartesian(xlim = c(0,60), ylim = c(-100,100))+
      
      # adds red vertical line to notable timepoints for stimulant ON & OFF
      geom_vline(xintercept = 10, color = "red")+
      geom_vline(xintercept = 30, color = "red")+
      
      # specifies line plot, & size/color of line
      geom_line(data = wormdata,
                mapping = aes(x = time_s, y = dff),
                color = "black", linewidth = 1)+
      
      # specifies plot labels
      labs(x = "Time (s)",
           y = "% dF/F",
           title = "Change in fluorescence over time",
           subtitle = paste(str_replace_all(stim_folders[i], "_", " "), jnl_folders[j]),
           caption = logfiles[k])+
      
      
      # specifies color and size of plot elements
      theme(plot.title = element_text(size = 12, face = "bold"),
            plot.subtitle = element_text(size = 10, color = "saddlebrown"),
            plot.caption = element_text(size = 8, color = "gray30"),
            axis.title = element_text(size = 10))
  }
  
  if((str_detect(wormdata$stim_jnl[k], "JNL_10off_120on_50off") == FALSE &
      str_detect(wormdata$stim_jnl[k], "JNL_10off_20on_20off") == FALSE) |
     str_detect(wormdata$stim_jnl[k], "lightbasal") == TRUE){
    # generates plot of % df/f over time in s
    ggplot()+
      
      # sets x-axis to match time of JNL & sets y-axis from -100 to 100
      coord_cartesian(xlim = c(0,60), ylim = c(-100,100))+
      
      # specifies line plot, & size/color of line
      geom_line(data = wormdata,
                mapping = aes(x = time_s, y = dff),
                color = "black", linewidth = 1)+
      
      # specifies plot labels
      labs(x = "Time (s)",
           y = "% dF/F",
           title = "Change in fluorescence over time",
           subtitle = paste(str_replace_all(stim_folders[i], "_", " "), jnl_folders[j]),
           caption = logfiles[k])+
      
      
      # specifies color and size of plot elements
      theme(plot.title = element_text(size = 12, face = "bold"),
            plot.subtitle = element_text(size = 10, color = "saddlebrown"),
            plot.caption = element_text(size = 8, color = "gray30"),
            axis.title = element_text(size = 10))
  }
  
  # saves plot to output folder, and names it by worm folder name
  ggsave(here("GCaMP","output","individual_dff",paste("plot",stim_folders[i],jnl_folders[j],logfiles[k],".png", sep = "-")),
         width = 6, height = 6)
  
### COMMENTED OUT FOR NOW----
#  # minimum number of frames is the number of rows within wormdata
  
#  # if 1st worm in the journal then minframes = numrows
#  # else not 1st worm of JNL, compare minframes and numrows
#  if (k == 1){
#    minframes <- nrow(wormdata)
#  } else {
#    # if minimum frames > current worm's frames, then update the minimum to the current frames
#    if (minframes > nrow(wormdata)) {
#      minframes <- nrow(wormdata)
#    }
#  }
  
  
  # display which worm it is in the console
  paste("CSV created for worm #", worm_num)
  
  
} # ends iteration through logfiles

"done"
