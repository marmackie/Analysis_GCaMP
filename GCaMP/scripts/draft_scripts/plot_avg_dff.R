#### Info ####

# This script is purely for plotting AVERAGE df/f for each stimulant + JNL combo

#### Load Libraries ####-----------------------------------------------
library(tidyverse)
library(here)
library(tidytext)

## Plotting averages------

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

##############################--------------

# iterates over each column in avg_all
# n is the current column (i.e. each stim_jnl)
for (n in 1:ncol(avg_all)){
  
  # assigns each avg_all column to avg_data
  avg_data <- avg_all[n]
  
  plot_data <- avg_data %>% 
    mutate(x = time_data,
           y = avg_data[n])
  
  # generates plot of average % df/f over time in s
  ggplot(data = avg_data)+
    
    # axis scales by 10
    scale_x_discrete(breaks = seq(from = 0, to = 180, by = 10))+
    
    # adds red vertical line to notable timepoints for stimulant ON & OFF
    geom_vline(xintercept = 10, color = "red")+
    geom_vline(xintercept = 130, color = "red")+
    
    # specifies line plot, & size/color of line
    geom_line(mapping = aes(x = time_data,
                            y = paste("stim_jnl",n,sep="")),
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
  
} # ends iteration through avg_all
