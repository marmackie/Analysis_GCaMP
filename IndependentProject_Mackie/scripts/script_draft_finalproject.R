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

#### Functions ####----------------------------------------------------

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
#### Part 1 - Get data, convert to clean csv, calculate dff, plot individual dff ####----------

### 1a - Iterate through stimulant, JNL, and worm folders ###-----

# pre-allocates a dataframe to hold information for convenience later
# 4 columns, but we will append more rows later
size_mat <- data.frame(matrix(ncol = 4))

# worm_num is the current worm being analyzed overall
worm_num = 0

# points to worm strain + stimulant folder on the computer
stim_path <- here("IndependentProject_Mackie", "data", "GCaMP_data")
# lists all folders in that path
stim_folders <- dir(path = stim_path)

# Iterates through each stimulant folder
for (i in 1:length(stim_folders)){
  
  # points to JNL folder on the computer
  jnl_path <- here("IndependentProject_Mackie", "data", "GCaMP_data", stim_folders[i])
  # lists all JNL folders in that path
  jnl_folders <- dir(path = jnl_path)
  
  # Iterates through each JNL folder within the current stimulant folder
  for (j in 1:length(jnl_folders)){
    # points to worm folder on the computer
    worm_path <- here("IndependentProject_Mackie", "data", "GCaMP_data", stim_folders[i], jnl_folders[j])
    # lists all worm folders in that path
    worm_folders <- dir(path = worm_path)
    
    # Iterates through each worm folder within the current JNL folder
    for (k in 1:length(worm_folders)){
      
      # adds +1 to worm count
      worm_num = worm_num + 1
      
      # points to the location of tracked worm data (logfiles) on the computer
      logfile_path <- here("IndependentProject_Mackie", "data", "GCaMP_data", stim_folders[i], jnl_folders[j], worm_folders[k])
      # lists all logfiles in that path (contains ".log")
      logfiles <- dir(path = logfile_path, pattern = ".log")
      
      ### 1b - Read in data & calculate dff -----
      
      # reads in current logfile into rawdata
      rawdata <- read_log(here("IndependentProject_Mackie","data", "GCaMP_data", stim_folders[i],jnl_folders[j],worm_folders[k],logfiles))
      
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
        # adds column for calculating dff
        mutate(dff = dff(fluor))
      
      # converts wormdata to csv and saves to clean_csv folder within data folder, naming it based on stimulant, JNL, and worm ID
      write_csv(wormdata, here("IndependentProject_Mackie", "data", "clean_csv", paste(stim_folders[i],jnl_folders[j],logfiles, sep = "_")))
      
      
      ### 1c - Plotting dff of individual worms -----
      
      # generates plot of % df/f over time in s
      ggplot()+
        
        # axis scales by 10
        scale_x_continuous(breaks = seq(from = 0, to = 180, by=10))+
        
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
             caption = worm_folders[k])+
        
        
        # specifies color and size of plot elements
        theme(plot.title = element_text(size = 12, face = "bold"),
              plot.subtitle = element_text(size = 10, color = "saddlebrown"),
              plot.caption = element_text(size = 8, color = "gray30"),
              axis.title = element_text(size = 10))
      
      ##### possibly puts gray box where stimulant on/off is (note, youll need a case_when() to do the different ones)
      #panel.first = rect(c(10,130), usr[3] col = "gray60", border = NA)
      
      # saves plot to output folder, and names it by worm folder name
      ggsave(here("IndependentProject_Mackie","output","individual_dff",paste("plot",stim_folders[i],jnl_folders[j],worm_folders[k],".png", sep = "-")),
             width = 6, height = 6)
      
      # number of frames is the number of rows within wormdata
      frames <- nrow(wormdata)
      
      # dataframe to store time information (to use for plotting later)
      time_data <- wormdata$time_s
      
    } # ends iteration through worm folders
    
    # adds column names to size_mat
    colnames(size_mat) <- c("frames","stim_jnl","worms_jnl","jnl_start")
    #
    size_mat[nrow(size_mat) + 1,] = c(frames,
                                      paste(stim_folders[i],jnl_folders[j],sep = "_"), # stimulant + jnl
                                      k, # final worm of each journal (= worms per journal)
                                      worm_num) # worm count (= total worms up to that journal)
    
  } # ends iteration through JNL folders
  
} # ends iteration through stim_folders

# drop first row (Nas) from size_mat
size_mat <- drop_na(size_mat)

"Part 1 complete!"
"clean csv files generated (see data)"
"individual dff plots generated (see outputs)"

#### Part 2 - Gather dff values into dff_all ####-------------

# points to location of csv file on the computer
csv_path <- here("IndependentProject_Mackie", "data", "clean_csv")
# lists all .csv files in that path
csvfiles <- dir(path = csv_path)

# pre-allocates a dataframe to store all dff values
dff_all <- data.frame(matrix(nrow = frames, ncol = length(csvfiles)))
# adds column names as dff{n}
colnames(dff_all) <- c(paste("dff_worm",(1:length(csvfiles)),sep = ""))

# Iterates through each csv file (i.e., for each worm)
for (l in 1:length(csvfiles)){
  
  # reads in csv files
  csv_data <- read_csv(here("IndependentProject_Mackie", "data", "clean_csv", csvfiles[l]))
  
  # stores dff column into dff_all
  dff_all[l] <- csv_data$dff
}
glimpse(dff_all)

"Part 2 complete!"
"Gathered all dff values into dff_all."


#### Part 3 - Average dff values by Stimulant + Journal & Plot them! ####-------------

# pre-allocates a dataframe to store all average dff values (averaged by journal)
# 2 columns because there are 2 unique stim + jnl combinations
avg_all <- data.frame(matrix(nrow = frames, ncol = nrow(size_mat)))
# adds column names as stim_jnl{n}
colnames(avg_all) <- c(paste("stim_jnl",(1:nrow(size_mat)),sep = ""))

# iterates over each row in size_mat (i.e., m = the current stim_jnl)
for (m in 1:nrow(size_mat)){
  
  # if first stim_jnl, starts with 1st worm
  ifelse(m == 1, start_worm <-  1,
         # else, starts with 1st worm of the current journal
         start_worm <- size_mat$jnl_start[m - 1])
  
  # ending worm = final worm of current stim_jnl
  end_worm <- size_mat$jnl_start[m]
  
  # calculates averages across rows, only for worms that correspond to the current stim_jnl (removes NAs)
  # stores this in column of avg_all corresponding to current stim_jnl
  avg_all[m] <- rowMeans(dff_all[,start_worm:end_worm], na.rm = TRUE)
  
} # ends iteration for each jnl (row in size_mat)

"Part 3 complete!"
"Average dff calculated and stored in avg_all"
  
    

### Part 4 - Plotting Average dff -----
  
    # generates plot of average % df/f over time in s
    ggplot(avg_data)+
      
    # axis scales by 10
    #scale_x_discrete(breaks = seq(from = 0, to = 180, by = 10))+
    
    # adds red vertical line to notable timepoints for stimulant ON & OFF
    geom_vline(xintercept = 10, color = "red")+
    geom_vline(xintercept = 130, color = "red")+
    
    # specifies line plot, & size/color of line
    geom_line(mapping = aes(x = time_s,
                            y = avg_all),
              color = "blue", linewidth = 2)+
    
    # specifies plot labels
    labs(x = "Time (s)",
         y = "% dF/F",
         title = "Average Change in fluorescence over time",
         subtitle = size_mat$stim_jnl[m])+ # current stimulant + jnl
    
    
    # specifies color and size of plot elements
    theme(plot.title = element_text(size = 12, face = "bold"),
          plot.subtitle = element_text(size = 10, color = "saddlebrown"),
          plot.caption = element_text(size = 8, color = "gray30"),
          axis.title = element_text(size = 10))
  
  # saves plot to output folder, and names it by worm folder name
  ggsave(here("IndependentProject_Mackie","output","average_dff",paste("plot",size_mat$stim_jnl[m],".png", sep = "-")),
         width = 6, height = 6)


### Notes to self:
# you need to fix max_frames so that it is actually max_frames & so you can later do padding
# you need to fix ggplot of individual & average plots so that red lines are in different spots based on the stim_jnl
# possibly change the red lines to gray shaded area like in publications
# rename size_mat to something that makes more sense ("conditions"?)