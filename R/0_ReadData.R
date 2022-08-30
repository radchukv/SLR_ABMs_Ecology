## 1. reading in the files

library(readxl)
library(dplyr)
source('./R/Functions.R')  ## load functions

files <- list.files(path = ('./data/Seb'), full.names = TRUE)

## test the function
ch1 <- read_exl('./data/Seb/002.xlsx')


#bind all data frames of FIRST data frame ofread_exl() together
answers_together <- data.frame()
all_data_together <- list()

for (file in files) {
  tmp_exl <- read_exl(file)
  answers_together <- bind_rows(answers_together, tmp_exl[[1]])
}
row.names(answers_together) <- files

