## Reproducible example:

# libraries
library(tidyverse)

# creating dataframes to use for the example
data1 <- data.frame(matrix(nrow = 5, ncol = 1))
colnames(data1) <- "dff"
data1$dff <- c(5, 5, 5, 5, 5)
                    
data2 <- data.frame(matrix(nrow = 10, ncol = 1)) 
colnames(data2) <- "dff"
data2$dff <- c(10, 10, 10, 10, 10, 10, 10, 10, 10, 10)

## my code:

# pre-allocates a dataframe to store all dff values
dff_all <- data.frame(matrix(nrow = rows, ncol = length(dflist)))
# adds column names as dff{n}
colnames(dff_all) <- c(paste("dff",(1:length(dflist)),sep = "_"))

# Iterates through each thing in the list
for (i in 1:length(dflist)){

# current frames
frames <- nrow(dflist[i])

# "result" is the number of rows for the current sample minus the max rows for each data set
result <- rows - max_rows

# If it is the 1st one OR current rows = max rows
ifelse(i == 1 | result == 0,
# then stores dff column into dff_all as is
dff_all[i] <- dflist[i]$dff,

# if it is not the 1st one and current rows > max rows
ifelse(result > 0,
# then current frames becomes the new max, and pads all previous columns with NAs up to new max
mutate(dff_all = lapply(dff_all,"length<-", max(lengths(dff_all)))) %>% 
max_frames <- nrow(csv_data),
# if it is not the 1st jnl and current frames < max frames, pads column with NAs up to max
mutate(dff_all[l] <- lapply(dff_all[l],"length<-", max(lengths(dff_all)))))
) # ends outer ifelse

} # ends iteration