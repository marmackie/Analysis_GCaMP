#### Part 3 - Average dff values by Stimulant + Journal ####-------------

# pre-allocates a dataframe to store all average dff values (averaged by journal)
avg_all <- data.frame(matrix(nrow = max_frames, ncol = nrow(size_mat)))
# adds column names as stim_jnl{n}
colnames(avg_all) <- c(paste("stim_jnl",(1:nrow(size_mat)),sep = ""))

# iterates over each row in size_mat (i.e., m = the current stim_jnl)
for (n in 1:nrow(size_mat)){
  
  # if first stim_jnl, starts with 1st worm
  ifelse(n == 1, start_worm <-  1,
         # else, starts with 1st worm of the current journal
         start_worm <- size_mat$jnl_start[n])
  
  # ending worm = final worm of current stim_jnl
  end_worm <- size_mat$worm_num[n]
  
  # calculates averages across rows for worms belonging to the same stim_jnl (excludes NAs) & stores in avg_all
  ifelse(start_worm == end_worm,
         # if only 1 worm in current stim_jnl, maintains value
         avg_all[n] <- dff_all[,start_worm:end_worm],
         # else calculate row means from start worm to end worm
         avg_all[1:size_mat$minframes[n],n] <- rowMeans(dff_all[1:size_mat$minframes[n],start_worm:end_worm], na.rm = TRUE))
  
} # ends iteration for each jnl (row in size_mat)

"Part 3 complete!
Average dff calculated and stored in avg_all"