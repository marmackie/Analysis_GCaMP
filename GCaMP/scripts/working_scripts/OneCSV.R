### Introduction ###--------------------------------------------------
# Purpose of this script is to combine .log files from calcium imaging
# data into one .csv file for each neuron imaged. This file will be used
# for generating a computational model.
#
# created: 2024-02-01 by Marisa Mackie
# edited: 2024-02-01

### Libraries ###-----------------------------------------------------
library(tidyverse)
library(here)
library(tidytext)

### Script ###--------------------------------------------------------
# Purpose of this section is to iterate through each .logfile in the folder,
# read it, 

# WAIT. I don't know if it needs to be the processed files (what we plot in 
# Matlab) or the raw dff in the logfile. Figure this out first before writing
# this code.