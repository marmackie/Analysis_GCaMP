#### Info ####
# This script is for generating other plots, i.e. heatmaps


#####------------------------------------------------------
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
                             xlab = "Time (ms)", ylab = 'Animals', main = "Change in fluorescence over Time for each Animal", key.title = '% dF/F', 
                             # removes row names & lines
                             labRow = FALSE, trace ='none',
                             # removes dendrogram stuff
                             Colv= FALSE, Rowv = FALSE, dendrogram = 'none', 
                             # specifies colors
                             col = colors, na.color = "white")
    
    dev.off()
  } # ends if
  
} # ends iteration












#### NOT WORKING ####
#####------------------------------------------------------
# iterates over each row in size_mat (i.e., n = the current stim_jnl)
for (n in 1:nrow(size_mat)){
  
  # if first stim_jnl, starts with 1st worm
  ifelse(n == 1, start_worm <-  1,
         # else, starts with 1st worm of the current journal
         start_worm <- size_mat$jnl_start[n])
  
  # ending worm = final worm of current stim_jnl
  end_worm <- size_mat$worm_num[n]
  
  # for JNLs with >1 worm
  if(start_worm != end_worm){
    
    # pre-allocates dataframe to hold only the x & y values for heatmap
    hmap_data <- data.frame(matrix(nrow = as.numeric(size_mat$frames[n]), ncol = 2))
    # column names
    colnames(hmap_data) <- c("x","y")
    
    hmap_dff <- dff_all[1:size_mat$frames[n],start_worm:end_worm]
    hmap_time <- time_data[1:size_mat$frames[n],1]
    
    hmap_data <- hmap_data %>% 
      mutate(x = hmap_time,
             y = hmap_dff[,n])
    
    ggplot(hmap_data,
           mapping = aes(x = x,
                         y = y[,n]))+
      
      geom_tile()
    
    # saves heatmap to output folder, and names it by stim_jnl name
    ggsave(here("GCaMP","output","heatmaps",paste("heatmap",size_mat$stim_jnl[n],".png", sep = "-")),
           width = 10, height = 10)
    
  }
}