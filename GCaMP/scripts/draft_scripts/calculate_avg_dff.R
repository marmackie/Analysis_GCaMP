#### Info ####
# This script is purely for the function to calculate average dff

#### Load Libraries ####-----------------------------------------------
library(tidyverse)
library(here)
library(tidytext)

# Not used
# Function to calculate averages of dff values, grouped by by stim_jnl
avg_dff <- function(data, rows, columns){
  
  sums <- sum(data[rows,columns])
  n <- nrows(data[rows,columns], na.rm = TRUE)
  avg <- sums/n
  
  return(avg)
  
}



####------------------------------------------------------------------------
#### Makin sure rowMeans does what I actually want it to
# It does :)
testrowmeans <- data.frame(a = c(10.5713340,35.4631698,3,4,5), 
                           b = c(37.7726544,10.2465323,3,4,5))
view(testrowmeans)

df_rm <- rowMeans(testrowmeans)
df_rm
df_cm <- colMeans(testrowmeans)

df_rm2 <- rowMeans(testrowmeans[,1:2])
df_rm2

df_rm3 <- rowMeans(testrowmeans[,2:3])
df_rm3

#########
### Bad because ifelse() should go OUTSIDE of assignment statement, not inside

# calculates averages across rows for worms belonging to the same stim_jnl (excludes NAs) & stores in avg_all
avg_all[n] <- ifelse(start_worm == end_worm,
                     # if only 1 worm in current stim_jnl, maintains value
                     dff_all[,start_worm:end_worm],
                     # else calculate row means from start worm to end worm
                     (rowMeans(dff_all[,start_worm:end_worm], na.rm = TRUE)))






# calculates averages across rows for worms belonging to the same stim_jnl (excludes NAs) & stores in avg_all
avg_all <- avg_all[n] %>% 
  mutate(ifelse(start_worm == end_worm,
                # if only 1 worm in current stim_jnl, maintains value
                dff_all[,start_worm:end_worm],
                # else calculate row means from start worm to end worm
                rowMeans(dff_all[,start_worm:end_worm], na.rm = TRUE)))






