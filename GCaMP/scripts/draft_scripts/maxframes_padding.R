#### Info ####
# This script is for figuring out how to get max_frames & how to do padding

#### Load Libraries ####-----------------------------------------------
library(tidyverse)
library(here)
library(tidytext)

#### Iterate thru jnls ####--------------------------------------------


# Iterates through each JNL folder within the current stimulant folder
# for (j in 1:length(jnl_folders)){

# jnl_num <- jnl_num + 1

#### max frames ####---------------------------------------------------



# "result" is the number of frames for the current worm minus the max number of frames for each data set
result <- nrow(data) - max_frames

# current jnl (row in sizemat)
jnl_num <- jnl_num + 1 

# If it is the 1st jnl OR current frames = max frames
ifelse(jnl_num == 1 | result == 0,
       # then stores dff column into dff_all as is
       dff_all[l] <- csv_data$dff,
       
       # if it is not the 1st jnl and current frames > max frames
       ifelse(result > 0),
       # then current frames becomes the new max, and pads all previous columns with -1 up to new max
       max_frames <- nrow(data),
       # padding here?
       # if it is not the 1st jnl and current frames < max frames, pads column with -1
       # paddng here?
       )
  
# If the above conditions are not true, we must pad

# If the current frames > max frames:
# The number of current frames becomes the new maximum & previous data will be padded with -1

# "n" is the value at the last row, last column in size_mat (i.e., the last worm from the previous JNL)

#"sm_row" is the row we are looking at in size_mat


# "m" goes from the very first worm until the final worm of the previous journal. 
# This is the worm that will currently be padded with -1s.

# Checks to see if the previous worm was the final worm of the previous journal.
# If yes, then go to the next row in size_mat (i.e., the next journal).

# Pads the current worm that needs to be padded 

#### padding with -1 ####----------------------------------------------

# pad w NAs

# try this?

# "result" is the number of frames for the current worm minus the max number of frames for each data set
##result <- frames - max_frames

# If it is the 1st jnl OR current frames = max frames
##ifelse(jnl_num == 1 | result == 0,
# then stores dff column into dff_all as is
##dff_all[l] <- csv_data$dff,

# if it is not the 1st jnl and current frames > max frames
##ifelse(result > 0,
# then current frames becomes the new max, and pads all previous columns with NAs up to new max
##mutate(dff_all = lapply(dff_all,"length<-", max(lengths(dff_all)))) %>% 
##max_frames <- nrow(csv_data),
# if it is not the 1st jnl and current frames < max frames, pads column with NAs up to max
##mutate(dff_all[l] <- lapply(dff_all[l],"length<-", max(lengths(dff_all)))))
##) # ends outer ifelse
dff_cols <- names(dff_all)
for (col in dff_cols){
  ifelse(length(dff_all[[p]]) > max_frames,
         max_frames <- length(dff_all[[p]]),
         
         # pad each column
         for(col in dff_cols){
           dff_all[[p]] <- c(dff_all[[p]], rep("", max_frames - length(dff_all[[p]])))
         })
}

test <- lapply(wormdata,"length<-", max(lengths(wormdata)))
view(test)

# try this?
# max column length
max_frames <- 0
p <- names(dff_all)
for (p in dff_all){
  if (length(dff_all[[p]]) > max_frames)
    max_frames <- length(dff_all[[p]])
  
  # pad each column
  for(p in dff_all){
    dff_all[[p]] <- c(dff_all[[p]], rep(NA, max_frames - length(dff_all[[p]])))
  }
}

