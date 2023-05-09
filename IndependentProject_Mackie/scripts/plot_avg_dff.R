#### Info ####

# This script is purely for plotting AVERAGE df/f for each stimulant + JNL combo

#### Load Libraries ####-----------------------------------------------
library(tidyverse)
library(here)
library(tidytext)

## Plotting averages

# generates plot of average % df/f over time in ms
ggplot(DATAHERE)+
  
  geom_line(aes(x = time_ms,
                y = dff),
            color = "gray30", size = 1)+
  
  # specifies line plot, & size/color of line
  geom_line(aes(x = time_ms,
                y = dff_avg),
            color = "blue", size = 2)+
  
  # specifies plot labels
  labs(x = "Time (ms)",
       y = "% dF/F",
       title = paste("Average", str_replace_all(stim_folders[i], "_", " "), jnl_folders[j]))+
  
  ###### insert thing to do the gray box here
  
# saves plot to output folder, and names it by worm folder name
ggsave(here("IndependentProject_Mackie","output",paste("plot",str_replace_all(stim_folders[i], "_", " "), jnl_folders[j],".png", sep = "")))

