#### Info ####

# This script is purely for plotting df/f for each individual worm

#### Load Libraries ####-----------------------------------------------
library(tidyverse)
library(here)
library(tidytext)

## Plotting individual worms

# generates plot of % df/f over time in ms
ggplot(DATAHERE,
       aes(x = time_ms,
           y = dff))+
  
  # specifies line plot, & size/color of line
  geom_line(color = "black", size = 2)+
  
  # specifies plot labels
  labs(x = "Time (ms)",
       y = "% dF/F",
       title = paste(str_replace_all(stim_folders[i], "_", " "), jnl_folders[j]),
       caption = worm_folders[k])+
  
  
  ##### possibly puts gray box where stimulant on/off is (note, youll need a case_when() to do the different ones)
  panel.first = rect(c(10,130), -1e6, 1e6, col = "gray85", border = NA)

# saves plot to output folder, and names it by worm folder name
ggsave(here("IndependentProject_Mackie","output",paste("plot",worm_folders[k],".png", sep = "")))

