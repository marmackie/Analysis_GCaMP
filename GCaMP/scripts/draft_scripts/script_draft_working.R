#### General Info ####-------------------------------------------------
# 
# This script will serve as the "drawing board" and draft of all the functions, for loops,
# and other code necessary for analyzing GCaMP data
#
# created: 2023-05-04
# created by: Marisa Mackie
# edited: 2023-05-28

#### Load Libraries ####-----------------------------------------------
library(tidyverse)
library(here)
library(tidytext)
library(gplots)
library(RColorBrewer)
library(wrMisc)

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
worm_num = 0

# points to worm strain + stimulant folder on the computer
stim_path <- here("GCaMP", "data", "GCaMP_data")
# lists all folders in that path
stim_folders <- dir(path = stim_path)

# Purpose: Iterate through Stimulant, JNL, and worm folders to get to logfiles
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
    
    # Iterates through each worm folder within the current JNL folder
    for (k in 1:length(worm_folders)){
      
      # adds +1 to worm count
      worm_num = worm_num + 1
      
      # points to the location of tracked worm data (logfiles) on the computer
      logfile_path <- here("GCaMP", "data", "GCaMP_data", stim_folders[i], jnl_folders[j], worm_folders[k])
      # lists all logfiles in that path (contains ".log")
      logfiles <- dir(path = logfile_path, pattern = ".log")
      
      ## Part 1a - Read in raw data & convert to csv files ----
      
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
      
      # converts wormdata to csv and saves to clean_csv folder within data folder, naming it based on stimulant, JNL, and worm ID
      write_csv(wormdata, here("GCaMP", "data", "clean_csv", paste(stim_folders[i],jnl_folders[j],logfiles, sep = "_")))
      
      
      ## Part 1b - Plot individual worm dff ----
      
      if(str_detect(wormdata$stim_jnl[k], "JNL_10off_120on_50off") == TRUE){
        
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
             caption = worm_folders[k])+
        
        
        # specifies color and size of plot elements
        theme(plot.title = element_text(size = 12, face = "bold"),
              plot.subtitle = element_text(size = 10, color = "saddlebrown"),
              plot.caption = element_text(size = 8, color = "gray30"),
              axis.title = element_text(size = 10))
      }
      
      if(str_detect(wormdata$stim_jnl[k], "JNL_10off_20on_20off") == TRUE){
        
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
               caption = worm_folders[k])+
          
          
          # specifies color and size of plot elements
          theme(plot.title = element_text(size = 12, face = "bold"),
                plot.subtitle = element_text(size = 10, color = "saddlebrown"),
                plot.caption = element_text(size = 8, color = "gray30"),
                axis.title = element_text(size = 10))
      }
      
      if((str_detect(wormdata$stim_jnl[k], "JNL_10off_120on_50off") == FALSE &
         str_detect(wormdata$stim_jnl[k], "JNL_10off_20on_20off") == FALSE) |
         str_detect(wormdata$stim_jnl[k], "bluelightbasal") == TRUE){
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
               caption = worm_folders[k])+
          
          
          # specifies color and size of plot elements
          theme(plot.title = element_text(size = 12, face = "bold"),
                plot.subtitle = element_text(size = 10, color = "saddlebrown"),
                plot.caption = element_text(size = 8, color = "gray30"),
                axis.title = element_text(size = 10))
      }
      
      # saves plot to output folder, and names it by worm folder name
      ggsave(here("GCaMP","output","individual_dff",paste("plot",stim_folders[i],jnl_folders[j],worm_folders[k],".png", sep = "-")),
             width = 6, height = 6)
      
      
      # minimum number of frames is the number of rows within wormdata
      
      # if 1st worm in the journal then minframes = numrows
      # else not 1st worm of JNL, compare minframes and numrows
      if (k == 1){
        minframes <- nrow(wormdata)
      } else {
        # if minimum frames > current worm's frames, then update the minimum to the current frames
        if (minframes > nrow(wormdata)) {
          minframes <- nrow(wormdata)
        }
      }
      
      
      # display which worm it is in the console
      paste("CSV created for worm #", worm_num)
      
      
    } # ends iteration through worm folders
    
    # adds column names to size_mat
    colnames(size_mat) <- c("minframes","stim_jnl","sample_size","worm_num","jnl_start")
    # appends rows to size_mat
    size_mat[nrow(size_mat) + 1,] = c(minframes,
                                      paste(stim_folders[i],jnl_folders[j],sep = "_"), # stimulant + jnl
                                      k, # final worm of each journal (= worms per journal)
                                      worm_num, # worm count (= total worms up to that journal)
                                      worm_num - k + 1) # starting worm for each jnl
    
  } # ends iteration through JNL folders
  
} # ends iteration through stim_folders

# drop first row (Nas) from size_mat
size_mat <- drop_na(size_mat)

"Part 1 complete!
Clean csv files generated (see data)
Individual dff plots generated (see outputs)"



#### Part 2 - Gather dff values into dff_all ####-------------

  ## Part 2a - Determine Max Frames ----
# points to location of csv file on the computer
csv_path <- here("GCaMP", "data", "clean_csv")
# lists all .csv files in that path
csvfiles <- dir(path = csv_path)

# the maximum number of frames (rows); starts at zero
max_frames <- 0

# Iterates through each csv file
# Purpose: determine max number of frames (rows) among all our data (required for dff_all later)
for (l in 1:length(csvfiles)){
  
  # reads in csv files
  csv_data <- read_csv(here("GCaMP", "data", "clean_csv", csvfiles[l]))
  
  # counts current number of rows (frames) in the current csv file
  frames <- nrow(csv_data)
  
  # If current frames (rows) is greater than the max, makes that value the new max. Otherwise, max is unchanged.
  if(frames > max_frames){
         max_frames <- frames}
  
} # ends iteration through csvfiles

  ## Part 2b - Put dff in dff_all ----

# pre-allocates a dataframe to store all dff values
dff_all <- data.frame(matrix(nrow = max_frames, ncol = length(csvfiles)))
# adds column names as dff_worm{n}
colnames(dff_all) <- c(paste("dff_worm",(1:length(csvfiles)),sep = ""))

# Iterates through each csv file (i.e., for each worm)
# Purpose: To grab dff data and store it into dff_all
for (m in 1:length(csvfiles)){
  
  # reads in csv files
  csv_data <- read_csv(here("GCaMP", "data", "clean_csv", csvfiles[m]))
    
  # counts current number of rows (frames) in the current csv file
  frames <- nrow(csv_data)
    
  # stores dff column into dff_all
  dff_all[1:frames,m] <- csv_data$dff
  
  } # ends iteration through each csv file

"Part 2 complete!
Gathered all dff values into dff_all."


#### Part 3 - Average dff values by Stimulant + Journal ####-------------

# pre-allocates a dataframe to store all average dff values (averaged by journal)
avg_all <- data.frame(matrix(nrow = max_frames, ncol = nrow(size_mat)))
# adds column names as stim_jnl{n}
colnames(avg_all) <- c(paste("stim_jnl",(1:nrow(size_mat)),sep = ""))

# iterates over each row in size_mat (i.e., m = the current stim_jnl)
for (n in 1:nrow(size_mat)){
  
  # if first stim_jnl, starts with 1st worm
  ifelse(n == 1, start_worm <-  1,
         # else, starts with 1st worm of the current journal
         start_worm <- size_mat$jnl_start[n])
  
  # ending worm = final worm of current stim_jnl
  end_worm <- size_mat$worm_num[n]
  
  # calculates averages across rows for worms belonging to the same stim_jnl (excludes NAs) & stores in avg_all
  ifelse(start_worm == end_worm,
         # if only 1 worm in current stim_jnl, maintains value
         avg_all[n] <- dff_all[,start_worm:end_worm],
         # else calculate row means from start worm to end worm
         avg_all[1:size_mat$minframes[n],n] <- rowMeans(dff_all[1:size_mat$minframes[n],start_worm:end_worm], na.rm = TRUE))
  
} # ends iteration for each jnl (row in size_mat)

"Part 3 complete!
Average dff calculated and stored in avg_all"
    
### Part 4 - Calculating SEM -----
# Calculates standard error of the mean

# SEM_cols will hold the number of columns we should use for SEM_all
SEM_cols <- 0
# SEM_names will hold the names of the journals that have >1 worm
SEM_names <- c()
SEM_index <- c()
# Finds value of SEM_cols by summing the number of journals that have >1 worm
for (n in 1:nrow(size_mat)){
  if(size_mat$sample_size[n] > 1){
    # adds one to our count of SEM_cols
    SEM_cols <- SEM_cols + 1
    # appends new row to SEM_names with next JNL number
    SEM_names[SEM_cols] = size_mat$stim_jnl[n]
    SEM_index[SEM_cols] = n
  }
}

# pre-allocates a dataframe to store all SEM values (by journal)
SEM_all <- data.frame(matrix(nrow = max_frames, ncol = SEM_cols))
# adds column names as stim_jnl{n}
colnames(SEM_all) <- SEM_names

# iterates over each row in size_mat (i.e., n = the current stim_jnl)
for (n in 1:length(SEM_names)){
  
  # SEM_index[n] == index of journal
  # determines starting and ending worm
  
  start_worm <- size_mat$jnl_start[SEM_index[n]]
  
  end_worm <- size_mat$worm_num[SEM_index[n]]
  
  # calculate row SEMs from start worm to end worm for each stim_jnl
  SEM_all[1:size_mat$minframes[SEM_index[n]],n] <- rowSEMs(dff_all[1:size_mat$minframes[SEM_index[n]],start_worm:end_worm])
  
} # ends iteration for each jnl (row in size_mat)

"Part 4 complete!
SEM calculated"

### Part 5 - Plotting Average dff -----
# Will plot average dff with SEM for all JNLs with >1 worm

# dataframe to store time information (to use for plotting later)
time_data <- data.frame(time = wormdata$time_s)

# iterates for each journal (# of rows in size_mat)
for (o in 1:length(SEM_all)){
  
    # Generates avg plot for 3 min JNL
    if(str_detect(size_mat$stim_jnl[SEM_index[o]], "JNL_10off_120on_50off") == TRUE){
      
      # creates dataframe plot_data with time & dff data
      plot_data <- data.frame(x = time_data[1:size_mat$minframes[SEM_index[o]],],
                              y = avg_all[1:size_mat$minframes[SEM_index[o]],SEM_index[o]],
                              pSEM = avg_all[1:size_mat$minframes[SEM_index[o]],SEM_index[o]] + SEM_all[1:size_mat$minframes[SEM_index[o]],o],
                              nSEM = avg_all[1:size_mat$minframes[SEM_index[o]],SEM_index[o]] - SEM_all[1:size_mat$minframes[SEM_index[o]],o])
      
      # make plot
      ggplot(plot_data,
             mapping = aes(x = x,
                           y = y,
                           ymax = pSEM,
                           ymin = nSEM))+
      
        # x axis scales by 10, from 0-180s
        scale_x_continuous(breaks = seq(from = 0, to = 180, by = 10))+
        
        # sets y-axis  
        coord_cartesian(ylim = c(-100,60))+
        
        # y axis scales y by 10
        scale_y_continuous(breaks = seq(from = -100, to = 100, by=10))+
        
        # adds red vertical line to notable timepoints for stimulant ON & OFF
        geom_vline(xintercept = 10, color = "red")+
        geom_vline(xintercept = 130, color = "red")+
        
        # specifies line plot, & size/color of line
        geom_line(color = "blue", linewidth = 0.6)+
        
        geom_ribbon(alpha = 0.5)+
        
        # specifies plot labels
        labs(x = "Time (s)",
             y = "% dF/F",
             title = "Average Change in fluorescence over time",
             subtitle = size_mat$stim_jnl[SEM_index[o]])+ # current stimulant + jnl
        
        # specifies color and size of plot elements
        theme(plot.title = element_text(size = 12, face = "bold"),
              plot.subtitle = element_text(size = 10, color = "saddlebrown"),
              plot.caption = element_text(size = 8, color = "gray30"),
              axis.title = element_text(size = 10))
    }
    
    # Generates avg plot for 1 min JNL
    if(str_detect(size_mat$stim_jnl[SEM_index[o]], "JNL_10off_20on_20off") == TRUE){
      
      # creates dataframe plot_data with time & dff data
      plot_data <- data.frame(x = time_data[1:size_mat$minframes[SEM_index[o]],],
                              y = avg_all[1:size_mat$minframes[SEM_index[o]],SEM_index[o]],
                              pSEM = avg_all[1:size_mat$minframes[SEM_index[o]],SEM_index[o]] + SEM_all[1:size_mat$minframes[SEM_index[o]],o],
                              nSEM = avg_all[1:size_mat$minframes[SEM_index[o]],SEM_index[o]] - SEM_all[1:size_mat$minframes[SEM_index[o]],o])
      
      # make plot
      ggplot(plot_data,
             mapping = aes(x = x,
                           y = y,
                           ymax = pSEM,
                           ymin = nSEM))+
        
        # x axis scales by 10, from 0-60s
        scale_x_continuous(breaks = seq(from = 0, to = 60, by = 10))+
        
        # sets y-axis  
        coord_cartesian(ylim = c(-100,60))+
        
        # y axis scales y by 10
        scale_y_continuous(breaks = seq(from = -100, to = 100, by=10))+
        
        # adds red vertical line to notable timepoints for stimulant ON & OFF
        geom_vline(xintercept = 10, color = "red")+
        geom_vline(xintercept = 30, color = "red")+
        
        # specifies line plot, & size/color of line
        geom_line(color = "blue", linewidth = 0.6)+
        
        geom_ribbon(alpha = 0.5)+
        
        # specifies plot labels
        labs(x = "Time (s)",
             y = "% dF/F",
             title = "Average Change in fluorescence over time",
             subtitle = size_mat$stim_jnl[SEM_index[o]])+ # current stimulant + jnl
        
        # specifies color and size of plot elements
        theme(plot.title = element_text(size = 12, face = "bold"),
              plot.subtitle = element_text(size = 10, color = "saddlebrown"),
              plot.caption = element_text(size = 8, color = "gray30"),
              axis.title = element_text(size = 10))
    }
    
    # Generates avg plot for all other JNLs (no red lines)
    if(str_detect(size_mat$stim_jnl[SEM_index[o]], "JNL_10off_120on_50off") == FALSE &
       str_detect(size_mat$stim_jnl[SEM_index[o]], "JNL_10off_20on_20off") == FALSE){
    
      # creates dataframe plot_data with time & dff data
      plot_data <- data.frame(x = time_data[1:size_mat$minframes[SEM_index[o]],],
                              y = avg_all[1:size_mat$minframes[SEM_index[o]],SEM_index[o]],
                              pSEM = avg_all[1:size_mat$minframes[SEM_index[o]],SEM_index[o]] + SEM_all[1:size_mat$minframes[SEM_index[o]],o],
                              nSEM = avg_all[1:size_mat$minframes[SEM_index[o]],SEM_index[o]] - SEM_all[1:size_mat$minframes[SEM_index[o]],o])
      
      # make plot
      ggplot(plot_data,
             mapping = aes(x = x,
                           y = y,
                           ymax = pSEM,
                           ymin = nSEM))+
      
      # x axis scales by 10, from 0-60s
      scale_x_continuous(breaks = seq(from = 0, to = 180, by = 10))+
        
      # sets y-axis  
      coord_cartesian(ylim = c(-100,60))+
        
      # y axis scales y by 10
      scale_y_continuous(breaks = seq(from = -100, to = 100, by=10))+
      
      # specifies line plot, & size/color of line
      geom_line(color = "blue", linewidth = 0.6)+
      
      geom_ribbon(alpha = 0.5)+
      
      # specifies plot labels
      labs(x = "Time (s)",
           y = "% dF/F",
           title = "Average Change in fluorescence over time",
           subtitle = size_mat$stim_jnl[SEM_index[o]])+ # current stimulant + jnl
      
      # specifies color and size of plot elements
      theme(plot.title = element_text(size = 12, face = "bold"),
            plot.subtitle = element_text(size = 10, color = "saddlebrown"),
            plot.caption = element_text(size = 8, color = "gray30"),
            axis.title = element_text(size = 10))
  }
    
  
  # saves plot to output folder, and names it by worm folder name
  ggsave(here("GCaMP","output","average_dff",paste("plot",size_mat$stim_jnl[SEM_index[o]],".png", sep = "-")),
         width = 6, height = 6)
  
} # ends iteration

"Part 5 complete!
Average dff plots generated! (see outputs)"


#### Part 6 - Generate Heatmap ####----------

### For heatmap (using heatmap() function)

# Color palette for heatmap
colors <- colorRampPalette(c("blue","lavenderblush4", "darkorange"))

# converts NA to 0
dff_zeros <- data.frame(dff_all)
dff_zeros[is.na(dff_zeros)] = 0

# converts dff_all to a numeric matrix called dff_mat
dff_mat <- as.matrix(dff_zeros)

# iterates over each row in size_mat (i.e., m = the current stim_jnl)
for (n in 1:nrow(size_mat)){
  
  # if first stim_jnl, starts with 1st worm
  ifelse(n == 1, start_worm <-  1,
         # else, starts with 1st worm of the current journal
         start_worm <- size_mat$jnl_start[n])
  
  # ending worm = final worm of current stim_jnl
  end_worm <- size_mat$worm_num[n]
  
  # makes heatmap for JNLs with >1 worm
  if(start_worm != end_worm){
    temp_mat <- as.matrix(dff_mat[1:size_mat$minframes[n],start_worm:end_worm])
    
    # saves heatmap as png file to heatmaps folder
    png(file = here("GCaMP", "output", "heatmaps", paste("heatmap",size_mat$stim_jnl[n],'.png')),
        width = 6, height = 6, units = "in", res = 1000, par(pin = c(0,0)))
    
    # generates heatmap
    dff_heatmap <- heatmap.2(t(temp_mat), 
                             # specifies heatmap labels
                             xlab = "Time (ms)", ylab = 'Animals', main = "Change in fluorescence over Time 
for each Animal", key.title = '% dF/F', 
                             # removes row names & lines
                             labRow = FALSE, trace ='none',
                             # removes dendrogram stuff
                             Colv= FALSE, Rowv = FALSE, dendrogram = 'none', 
                             # specifies colors
                             col = colors, na.color = "white")
    
    dev.off()
  } # ends if
  
} # ends iteration

"Part 6 complete! Heatmaps generated (See outputs)"


"Done!"