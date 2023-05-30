#### General Info ####-------------------------------------------------
# 
# This script will serve as the working script for Calcium Imaging data analysis.
# Based on script created for Final Independent project.
#
# created: 2023-05-17
# created by: Marisa Mackie
# edited: 2023-05-17

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

# pre-allocates a dataframe to hold information for convenience later
# 4 columns, but we will append more rows later
size_mat <- data.frame(matrix(ncol = 5))

# worm_num is the current worm being analyzed overall
worm_num <- 0

# current journal being analyzed; starts at 0
jnl_num <- 0

# max number of frames (rows) in each dataset; starts at -1
max_frames <- -1

# pre-allocates a dataframe to store all dff values
# 1799 rows for max frames, will append columns later
dff_all <- data.frame(matrix(nrow = 1799, ncol = 1))

# points to worm strain + stimulant folder on the computer
stim_path <- here("GCaMP", "data", "GCaMP_data")
# lists all folders in that path
stim_folders <- dir(path = stim_path)

# Iterates through each stimulant folder
for (i in 1:length(stim_folders)){
  
  # points to JNL folder on the computer
  jnl_path <- here("GCaMP", "data", "GCaMP_data", stim_folders[i])
  # lists all JNL folders in that path
  jnl_folders <- dir(path = jnl_path)
  
  # Iterates through each JNL folder within the current stimulant folder
  for (j in 1:length(jnl_folders)){
    # points to worm folder on the computer
    worm_path <- here("GCaMP", "data", "GCaMP_data", stim_folders[i], jnl_folders[j])
    # lists all worm folders in that path
    worm_folders <- dir(path = worm_path)
    
    jnl_num <- jnl_num + 1
    
    # Iterates through each worm folder within the current JNL folder
    for (k in 1:length(worm_folders)){
      
      # adds +1 to worm count
      worm_num = worm_num + 1
      
      # points to the location of tracked worm data (logfiles) on the computer
      logfile_path <- here("GCaMP", "data", "GCaMP_data", stim_folders[i], jnl_folders[j], worm_folders[k])
      # lists all logfiles in that path (contains ".log")
      logfiles <- dir(path = logfile_path, pattern = ".log")
      
      ### 1b - Read in data & calculate dff -----
      
      # reads in current logfile into rawdata
      rawdata <- read_log(here("GCaMP","data", "GCaMP_data", stim_folders[i],jnl_folders[j],worm_folders[k],logfiles))
      
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
      
      # saves plot to output folder, and names it by worm folder name
      ggsave(here("GCaMP","output","individual_dff",paste("plot",stim_folders[i],jnl_folders[j],worm_folders[k],".png", sep = "-")),
             width = 6, height = 6)
      
      ### Store dff into dff_all ###-----------
      
      # number of frames is the number of rows within wormdata (for the current worm)
      frames <- nrow(wormdata)
        
      # "result" is the frames for the current worm minus the max number of frames
      result <- frames - max_frames
        
        # If it is the 1st jnl OR current frames = max frames
        ifelse(jnl_num == 1 | result == 0,
               # then stores dff column into dff_all as is
               mutate(dff_all, dff_worm = wormdata$dff),
               
               # if it is not the 1st jnl and current frames > max frames
               ifelse(result > 0,
                      # then current frames becomes the new max, and pads all previous columns with -1 up to new max
                      max_frames <- nrow(wormdata),)
               # padding here?
               # if it is not the 1st jnl and current frames < max frames, pads column with -1
               # paddng here?
        )
      
      glimpse(dff_all)
      
    } # ends iteration through worm folders
  } # ends iteration through jnl folders
} # ends iteration through stim folders
    

"Part 1 complete!
Individual dff plots generated (see outputs)"


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
         start_worm <- size_mat$jnl_start[m])
  
  # ending worm = final worm of current stim_jnl
  end_worm <- size_mat$worm_num[m]
  
  # calculates averages across rows, only for worms that correspond to the current stim_jnl (removes NAs)
  # stores this in column of avg_all corresponding to current stim_jnl
  avg_all[m] <- rowMeans(dff_all[,start_worm:end_worm])
  
} # ends iteration for each jnl (row in size_mat)

"Part 3 complete!
Average dff calculated and stored in avg_all"


### Part 4 - Plotting Average dff -----

# pre-allocates dataframe to hold only the x & y values for plotting
plot_data <- data.frame(matrix(nrow = frames, ncol = 2))
# column names
colnames(plot_data) <- c("x","y")

# iterates for each journal (# of rows in size_mat)
for (n in 1:nrow(size_mat)){
  
  # updates plot_data with appropriate data
  plot_data <- plot_data %>% 
    mutate(x = time_data, # assigns time data to x column
           y = avg_all[,n]) # assigns current column of avg_all to y column
  
  # generates plot of average % df/f over time in s
  ggplot(plot_data)+
    
    # axis scales by 10
    scale_x_discrete(breaks = seq(from = 0, to = 180, by = 10))+
    
    # adds red vertical line to notable timepoints for stimulant ON & OFF
    geom_vline(xintercept = 10, color = "red")+
    geom_vline(xintercept = 130, color = "red")+
    
    # specifies line plot, & size/color of line
    geom_line(mapping = aes(x = x,
                            y = y),
              color = "blue", linewidth = 2)+
    
    # specifies plot labels
    labs(x = "Time (s)",
         y = "% dF/F",
         title = "Average Change in fluorescence over time",
         subtitle = size_mat$stim_jnl[n])+ # current stimulant + jnl
    
    
    # specifies color and size of plot elements
    theme(plot.title = element_text(size = 12, face = "bold"),
          plot.subtitle = element_text(size = 10, color = "saddlebrown"),
          plot.caption = element_text(size = 8, color = "gray30"),
          axis.title = element_text(size = 10))
  
  # saves plot to output folder, and names it by worm folder name
  ggsave(here("GCaMP","output","average_dff",paste("plot",size_mat$stim_jnl[n],".png", sep = "-")),
         width = 6, height = 6)
  
} # ends iteration

"Part 4 complete!
Average dff plots generated! (see outputs)"

"Done!"

### Notes to self:
# you need to fix max_frames so that it is actually max_frames & so you can later do padding
# you need to fix ggplot of individual & average plots so that red lines are in different spots based on the stim_jnl
# possibly change the red lines to gray shaded area like in publications
# rename size_mat to something that makes more sense ("conditions"?)
# add the individual worm lines on the plots