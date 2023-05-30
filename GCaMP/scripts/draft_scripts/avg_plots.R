### Part 4 - Plotting Average dff -----

# pre-allocates dataframe to hold only the x & y values for plotting
plot_data <- data.frame(matrix(nrow = max_frames, ncol = 2))
# column names
colnames(plot_data) <- c("x","y")

# iterates for each journal (# of rows in size_mat)
for (o in 1:nrow(size_mat)){
  
  # Generates avg plot for 3 min JNL
  if(str_detect(size_mat$stim_jnl[o], "JNL_10off_120on_50off") == TRUE){
    
    # updates plot_data with appropriate data
    plot_data <- plot_data %>% 
      mutate(x = time_data, # assigns time data to x column
             y = avg_all[,o]) # assigns current column of avg_all to y column
    
    # make plot
    ggplot(plot_data)+
      
      # axis scales by 10, from 0-180s
      scale_x_continuous(breaks = seq(from = 0, to = 180, by = 10))+
      
      # adds red vertical line to notable timepoints for stimulant ON & OFF
      geom_vline(xintercept = 10, color = "red")+
      geom_vline(xintercept = 130, color = "red")+
      
      # specifies line plot, & size/color of line
      geom_line(mapping = aes(x = x,
                              y = y),
                color = "blue", linewidth = 0.6)+
      
      # specifies plot labels
      labs(x = "Time (s)",
           y = "% dF/F",
           title = "Average Change in fluorescence over time",
           subtitle = size_mat$stim_jnl[o])+ # current stimulant + jnl
      
      # specifies color and size of plot elements
      theme(plot.title = element_text(size = 12, face = "bold"),
            plot.subtitle = element_text(size = 10, color = "saddlebrown"),
            plot.caption = element_text(size = 8, color = "gray30"),
            axis.title = element_text(size = 10))
  }
  
  # Generates avg plot for 1 min JNL
  if(str_detect(size_mat$stim_jnl[o], "JNL_10off_20on_20off") == TRUE){
    
    # updates plot data with appropriate data
    plot_data <- plot_data %>%
      mutate(x = time_data, # assigns time data to x column
             y = avg_all[,o]) # assigns current column of avg_all to y column
    drop_na(plot_data) # drops NA values
    
    # make plot
    ggplot(plot_data)+
      
      # axis scales by 10, from 0-60s
      scale_x_continuous(breaks = seq(from = 0, to = 60, by = 10))+
      
      # adds red vertical line to notable timepoints for stimulant ON & OFF
      geom_vline(xintercept = 10, color = "red")+
      geom_vline(xintercept = 30, color = "red")+
      
      # specifies line plot, & size/color of line
      geom_line(mapping = aes(x = x,
                              y = y),
                color = "blue", linewidth = 0.6)+
      
      # specifies plot labels
      labs(x = "Time (s)",
           y = "% dF/F",
           title = "Average Change in fluorescence over time",
           subtitle = size_mat$stim_jnl[o])+ # current stimulant + jnl
      
      # specifies color and size of plot elements
      theme(plot.title = element_text(size = 12, face = "bold"),
            plot.subtitle = element_text(size = 10, color = "saddlebrown"),
            plot.caption = element_text(size = 8, color = "gray30"),
            axis.title = element_text(size = 10))
  }
  
  # Generates avg plot for all other JNLs (no red lines)
  if(str_detect(size_mat$stim_jnl[o], "JNL_10off_120on_50off") == FALSE &
     str_detect(size_mat$stim_jnl[o], "JNL_10off_20on_20off") == FALSE){
    
    # updates plot_data with appropriate data
    plot_data <- plot_data %>%
      mutate(x = time_data, # assigns time data to x column
             y = avg_all[,o]) # assigns current column of avg_all to y column
    drop_na(plot_data) # drops NA values
    
    # make plot
    ggplot(plot_data)+
      
      # axis scales by 10, from 0-60s
      scale_x_continuous(breaks = seq(from = 0, to = 60, by = 10))+
      
      # specifies line plot, & size/color of line
      geom_line(mapping = aes(x = x,
                              y = y),
                color = "blue", linewidth = 0.6)+
      
      # specifies plot labels
      labs(x = "Time (s)",
           y = "% dF/F",
           title = "Average Change in fluorescence over time",
           subtitle = size_mat$stim_jnl[o])+ # current stimulant + jnl
      
      # specifies color and size of plot elements
      theme(plot.title = element_text(size = 12, face = "bold"),
            plot.subtitle = element_text(size = 10, color = "saddlebrown"),
            plot.caption = element_text(size = 8, color = "gray30"),
            axis.title = element_text(size = 10))
  }
  
  
  # saves plot to output folder, and names it by worm folder name
  ggsave(here("GCaMP","output","average_dff",paste("plot",size_mat$stim_jnl[o],".png", sep = "-")),
         width = 6, height = 6)
  
} # ends iteration

"Part 4 complete!
Average dff plots generated! (see outputs)"