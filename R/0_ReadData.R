## 1. reading in the files for analysis with tables and plots

library(readxl)
library(dplyr)
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

## Get a dataframe with all questions
questions_codebook <- tmp_exl[[2]][,1:3]
# Clean up
questions_codebook <- questions_codebook %>% mutate(AnswerType=ifelse(Q_ID=="Q2", "yes/no/no research question", AnswerType),
                                                    AnswerType=ifelse(Q_ID=="Q3", "Multiple choice", AnswerType))

