#### General Info ####-----------------------------------------------
# Revamping the previously-working code for GCaMP analysis to accommodate
# 1. new data collected
# 2. new research questions (Single neuron responses vs both)
# 3. changes in file naming

# created: 2023-09-02 by Marisa Mackie
# edited: 2023-09-03

#### Libraries ####-----------------------------------------------
library(tidyverse)
library(here)
library(tidytext)
library(gplots)
library(RColorBrewer)
library(wrMisc)

#### Functions ####-----------------------------------------------
# turns off sci notation (global)
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

# **** Note: Problem here. When e_to_num() is being used, some logfiles with shorter
# numbers in the fluor column (i.e. "1.61e+008") remain in scientific notation even 
# after the function. This results in an NA value when converting to numeric, and
# when dff is calculated, turns the whole dff col into NAs, resulting in no plot.
# Problem seems to be in line 28 & 30
# Current solution: dropping NAs after e_to_sum() but before dff() acts on data

# Function for calculating df/f (% change in fluorescence)
dff <- function(data) {
  # sets baseline fluorescence
  f0 <- mean(data[10:40]) # averages fluorescence data from frame 10-40 (1-4s)
  # calculates df/f as a percentage
  dff <- ((data - f0)/f0)*100
  # returns dff
  return(dff)
}

#### Analysis ####-----------------------------------------------------

### Step 1 - Get logfile data----

# pre-allocates a dataframe to hold information for convenience later
# 5 columns, but we will append more rows later
size_mat <- data.frame(matrix(ncol = 5))

# worm_num is the current worm being analyzed overall
worm_num = 0

# points to worm strain + stimulant folder on the computer
stim_path <- here("GCaMP", "data", "GCaMP_data")
# lists all folders in that path
stim_folders <- dir(path = stim_path)

# Purpose: Iterate through worm-stimulant and JNL folders to get to logfiles
# Iterates through each worm-stimulant folder
for (i in 1:length(stim_folders)){
  
  # points to JNL folder on the computer
  jnl_path <- here("GCaMP", "data", "GCaMP_data", stim_folders[i])
  # lists all JNL folders in that path
  jnl_folders <- dir(path = jnl_path)
  
  # Iterates through each JNL folder within the current stimulant folder
  for (j in 1:length(jnl_folders)){
    # points to the location of tracked worm data (logfiles) on the computer
    logfile_path <- here("GCaMP", "data", "GCaMP_data", stim_folders[i], jnl_folders[j])
    # lists all .log files in that path
    logfiles <- dir(path = logfile_path, pattern = ".log")
    
    # Iterates through each logfile within the current JNL folder
    for (k in 1:length(logfiles)){
      
      # adds +1 to worm count
      worm_num = worm_num + 1
      
      ### Step 1a - Load raw data & convert to csv----
      # reads in current logfile into rawdata
      rawdata <- read_log(here("GCaMP","data", "GCaMP_data", stim_folders[i],jnl_folders[j],logfiles[k]))
      
      wormdata <- rawdata %>% 
        # adds 1 column for stimulant + JNL
        mutate(stim_jnl = paste(stim_folders[i],jnl_folders[j], sep = "_"),
               # renames default col names (Xn) to specific variable names
               frame = rawdata$X1,
               time_s = (rawdata$X2)/1000, # converts ms to s
               fluor = rawdata$X3,
               .keep = "used") # only keeps mutated columns
      
      # For "fluor" col, converts values from sci notation (class chr) to expanded form (class dbl)
      wormdata <- wormdata %>% 
        mutate(fluor = str_replace_all(fluor, "\\,",""), # removes commas
               fluor = e_to_num(fluor)) %>% 
        # drops NA values
        drop_na() %>% 
        # adds column to calculate dff
        mutate(dff = dff(fluor))
      
      # converts wormdata to csv and saves to clean_csv folder within data folder, naming it based on stimulant, JNL, and worm ID
      write_csv(wormdata, here("GCaMP", "data", "clean_csv", paste(stim_folders[i],jnl_folders[j],logfiles[k], sep = "_")))
      
      
      ### Step 1b - Plot individual worm dff----
      
      if(str_detect(wormdata$stim_jnl[k], "10off_120on_50off") == TRUE){
        
        # generates plot of % df/f over time in s
        ggplot()+
          
          # axis scales by 10
          scale_x_continuous(breaks = seq(from = 0, to = 180, by=10))+
          
          # scales y by 10
          scale_y_continuous(breaks = seq(from = -100, to = 100, by=10))+
          
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
      
      if(str_detect(wormdata$stim_jnl[k], "10off_20on_20off") == TRUE){
        
        # generates plot of % df/f over time in s
        ggplot()+
          
          # axis scales by 10
          scale_x_continuous(breaks = seq(from = 0, to = 180, by=10))+
          
          # scales y by 10
          scale_y_continuous(breaks = seq(from = -100, to = 100, by=10))+
          
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
      
      if((str_detect(wormdata$stim_jnl[k], "10off_120on_50off") == FALSE &
          str_detect(wormdata$stim_jnl[k], "10off_20on_20off") == FALSE) |
         str_detect(wormdata$stim_jnl[k], "lightbasal") == TRUE){
        # generates plot of % df/f over time in s
        ggplot()+
          
          # axis scales by 10
          scale_x_continuous(breaks = seq(from = 0, to = 60, by=10))+
          
          # scales y by 10
          scale_y_continuous(breaks = seq(from = -100, to = 100, by=10))+
          
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
      
      # saves plot to output folder, and names it by logfile name
      ggsave(here("GCaMP","output","individual_dff",paste("plot",stim_folders[i],jnl_folders[j],logfiles[k],".png", sep = "-")),
             width = 6, height = 6)
      
      
      # min number of frames = number of rows within wormdata
      
      # if 1st worm of jnl, then minframes = numrows
      # else: not 1st worm of JNL, compare minframes and numrows
      if (k == 1){
        minframes <- nrow(wormdata)
      } else {
        # if min frames > current worm's frames, then update the minimum -> current frames
        if (minframes > nrow(wormdata)) {
          minframes <- nrow(wormdata)
        }
      }
      
      
      # display which worm it is in the console
      paste("CSV created for worm #", worm_num)
      
      
    } # ends iteration through logfiles
    
    # adds column names to size_mat
    colnames(size_mat) <- c("minframes","stim_jnl","sample_size","worm_num","jnl_start")
    # appends the following rows to size_mat:
    size_mat[nrow(size_mat) + 1,] = c(minframes,
                                      paste(stim_folders[i],jnl_folders[j],sep = "_"), # stimulant + jnl
                                      k, # final worm of each journal (= worms per journal)
                                      worm_num, # worm count (= total worms up to that journal)
                                      worm_num - k + 1) # starting worm for each jnl
    
  } # ends iteration through JNL folders
  
} # ends iteration through stim_folders

# drop first row (Nas) from size_mat
size_mat <- drop_na(size_mat)

"Part 1 complete - csv files & individual dff plots generated"

