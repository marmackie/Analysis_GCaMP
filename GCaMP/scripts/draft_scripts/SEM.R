####------------------------------------------------------

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


ggplot(avg_all)+
  geom_ribbon(mapping=aes(x=, ymin = SEM_all[1], ymax = SEM_all[1]))




##### TESTING PLOT

# iterates for each journal (# of rows in size_mat)
for (o in 1:length(SEM_all)){
  
  # Generates avg plot for 3 min JNL
  if(str_detect(size_mat$stim_jnl[o], "JNL_10off_120on_50off") == TRUE){
    
    # creates dataframe plot_data with time & dff data
    plot_data <- data.frame(x = time_data[1:size_mat$minframes[3],],
                            y = avg_all[1:size_mat$minframes[3],3],
                            pSEM = SEM_all[1:size_mat$minframes[3],3],
                            nSEM = -SEM_all[1:size_mat$minframes[3],3])
    
    # make plot
plot <- ggplot(plot_data,
               mapping = aes(x = x,
                             y = y,
                             ymax = pSEM,
                             ymin = nSEM))+
      
      # axis scales by 10, from 0-180s
      scale_x_continuous(breaks = seq(from = 0, to = 180, by = 10))+
      
      # adds red vertical line to notable timepoints for stimulant ON & OFF
      geom_vline(xintercept = 10, color = "red")+
      geom_vline(xintercept = 130, color = "red")+
      
      # specifies line plot, & size/color of line
      geom_line(color = "blue", linewidth = 0.6)+
      
      geom_ribbon(alpha = -0.5)+
      
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
  
}
plot



####-------------------------
test_df <- data.frame(x = time_data[1:6,],
                      y = avg_all[1:6,3],
                      ymax = avg_all[1:6,3] + SEM_all[1:6,3],
                      ymin = avg_all[1:6,3] - SEM_all[1:6,3])

testing <- ggplot(test_df,
                  aes(x = x,
                      y = y,
                      ymax = ymax,
                      ymin = ymin))+
  geom_line()+
  geom_ribbon(alpha = 0.5)

dev.off()

testing

