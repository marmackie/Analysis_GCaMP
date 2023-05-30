#### Info ####
# This script is purely for the function to calculate df/f

#### Load Libraries ####-----------------------------------------------
library(tidyverse)
library(here)
library(tidytext)

# Function for calculating df/f (% change in fluorescence)
dff <- function(data) {
  # sets baseline fluorescence
  f0 <- mean(data[10:40]) # averages fluorescence data from frame 10-40 (1-4s)
  # calculates df/f as a percentage
  dff <- ((data - f0)/f0)*100
  # returns dff
  return(dff)
}