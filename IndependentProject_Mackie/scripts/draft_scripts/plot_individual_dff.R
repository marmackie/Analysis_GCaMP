#### Info ####

# This script is purely for plotting df/f for each individual worm

#### Load Libraries ####-----------------------------------------------
library(tidyverse)
library(here)
library(tidytext)

## Plotting individual worms

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

