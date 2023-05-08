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

#### Part 1 - Get data, convert to clean csv, calculate dff, plot individual dff ####----------

### 1a - Iterate through stimulant, JNL, and worm folders ###-----

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
      
      wormdata <- rawdata %>% 
        # adds columns for stimulant and JNL type
        mutate(strain_stim = stim_folders[i],
               jnl = jnl_folders[j],
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
      
      
      ### 1b - Plotting dff of individual worms --------
      
      # generates plot of % df/f over time in s
      ggplot()+
        
        # axis scales by 10
        scale_x_continuous(breaks = seq(from = 10, to = 180, by=10))+
        
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
      
    } # ends iteration through worm folders
    
  } # ends iteration through JNL folders
  
} # ends iteration through stim_folders

"Part 1 complete!"
"clean csv files generated (see data)"
"individual dff plots generated (see outputs)"

#### Part 2 - Calculate & Plot Average dff by Stimulant & JNL ####-------------

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

"Part 2 complete!"
"average dff plots generated (see outputs)"