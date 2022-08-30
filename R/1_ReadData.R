## 1. reading in the files

library(readxl)
library(dplyr)
source('./R/Functions.R')  ## load functions

files <- list.files(path = ('./data/'), full.names = TRUE)

## test the function
ch1 <- read_exl('./data/002.xlsx')

## next we have to apply the function to all the filenames in the "files". I stop here
