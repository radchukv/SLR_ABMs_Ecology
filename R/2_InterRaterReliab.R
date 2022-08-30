## clauclating the inter-rater reliability
source('./R/0_ReadData.R')

## for now it seems like 123 study was not entered, so dropping it here (later on check with the coders)
answers_together <- answers_together[!is.na(answers_together$Q0), ]
k <- length(unique(answers_together$Q0))
N <- length(answers_together$Q0)

## basically we will have to have Q0 two times - one for Seb and one for Mel
## so, update the function to read the files to differentiate different coders (name of coders as another column);
## and then rbind them all
table(answers_together$Q0)

## have to run now