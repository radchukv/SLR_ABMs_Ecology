## 1. reading in the files for analysis with tables and plots

library(readxl)
library(dplyr)
library(flextable)
source('./R/Functions.R')  ## load functions

files_ecology <- list.files(path = ('./data/ecology/Aligned'), full.names = TRUE)
files_social <- list.files(path = ('./data/social/Aligned'), full.names = TRUE)
files <- append(files_ecology, files_social)


# bind all data frames of FIRST data frame of read_exl() from ecology together
answers_ecology <- data.frame()

for (file in files_ecology) {
  tmp_exl <- read_exl(file)
  answers_ecology <- bind_rows(answers_ecology, tmp_exl[[1]])
}

answers_ecology$category <- rep(c("ecology"),times=nrow(answers_ecology))


# bind all data frames of FIRST data frame of read_exl() from social together
answers_social <- data.frame()

for (file in files_social) {
  tmp_exl <- read_exl(file)
  answers_social <- bind_rows(answers_social, tmp_exl[[1]])
}

answers_social$category <- rep(c("social"),times=nrow(answers_social))


# combine ecology and social dataframe together
answers_together <- rbind(answers_ecology, answers_social)

## Get a dataframe with codebook details (questions and answer options)
codebook_template <- read_xlsx(path = ('./data/Codebook_template.xlsx'),  sheet = "Coding_tool", range = cell_limits(ul = c(7, 3), lr = c(97, 16)),
                                col_names = c('Q_ID', 'Drop1', 'Drop2', 'Question', 'Drop3', 'AnswerType', 'Explan', 'Source', 'Drop4', 'Answer_CoderA', 'Comments_CoderA','Drop5', 'Answer_CoderB', 'Comments_CoderB'))

codebook_template <- codebook_template %>% 
  select(!starts_with('Drop')) %>%                                               ## drop the columns we do not need
  filter(!row_number() %in% c(2, 5, 6, 13, 17, 30, 31, 45, 46, 56, 60, 86))      ## drop the rows we do not need

