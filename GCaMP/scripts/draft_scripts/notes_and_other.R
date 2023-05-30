### for later

# creates dataframe of all dff values for the current stim_jnl combination
dff_jnl <- csv_data %>% 
  
  # selects only for specified columns
  select(stim_jnl, time_s, dff) %>% 
  
  # if it is NOT the 1st csv file 
  # OR
  # the current csv file's stim_jnl matches that of the previous csv file...
  ifelse(l != 1 | csv_data$stim_jnl == stim_jnl,
         # then add dff to the current dataframe
         mutate(csv_data$dff))