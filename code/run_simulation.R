library(tidyverse)
library(lubridate)
library(reshape2) # we will use the 'melt' function
library(cumstats)
library(ggpubr)

# import the function and the dataframe
source('code/montecarlo_simulation.R')
distf <- read_csv('data/test_eg.csv')

#run the code..
montecarlo_simulation(pollutant_col_id = 2, distf = distf)
