## calculating the inter-rater reliability, using Cohen's Kappa
library(readxl)
library(dplyr)
library(irr)
source('./R/Functions.R')

## read in the data coded by the first coder
files_seb <- list.files(path = ('./data/Seb'), full.names = TRUE)

#bind all data frames of FIRST data frame of read_exl() together
answers_Seb <- data.frame()

for (file in files_seb) {
  tmp_exl <- read_exl(file)
  answers_Seb <- bind_rows(answers_Seb, tmp_exl[[1]])
}
row.names(answers_Seb) <- files_seb


## read in the data coded by the second coder
files_mel <- list.files(path = ('./data/Mel'), full.names = TRUE)

#bind all data frames of FIRST data frame of read_exl() together
answers_Mel <- data.frame()

for (file in files_mel) {
  tmp_exl <- read_exl(file)
  answers_Mel <- bind_rows(answers_Mel, tmp_exl[[1]])
}
row.names(answers_Mel) <- files_mel

## for now it seems like 123 study was not entered, so dropping it here (later on check with the coders)
answers_Mel <- answers_Mel[!is.na(answers_Mel$Q0), ]
answers_Seb <- answers_Seb[!is.na(answers_Seb$Q0), ]


Q0 <- bind_cols(answers_Seb$Q0, answers_Mel$Q0)
kappa2(Q0, weight = 'unweighted')

kappa2(bind_cols(answers_Seb$Q1, answers_Mel$Q1))
kappa2(bind_cols(answers_Seb$Q2, answers_Mel$Q2))
kappa2(bind_cols(answers_Seb$Q3, answers_Mel$Q3))
## this metric is only possible to calculate for cases with mutually-exclusive categories
## OR: one would really go for each subquestion (liek Q3.1, Q3.2 etc)
kappa2(bind_cols(answers_Seb$Q3.1, answers_Mel$Q3.1))
