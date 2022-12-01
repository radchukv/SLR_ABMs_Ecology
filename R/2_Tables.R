## saving each question as a table

library(readxl)
library(openxlsx)


## read in the data
source('./R/0_ReadData.R')


## we exclude the 123 paper because it was not coded, the coders decided it does not belong to the sample actually

#sum(is.na(answers_together$Q0))
#is.na(answers_together$Q0)
#answers_together$PaperID[is.na(answers_together$Q0)]

## exclude NA sheets
answers_together <- answers_together[!is.na(answers_together$Q0),]

## testing the function

save_table(Col_Q = "Q0",
           fold_path = './output/',
           data= answers_together)

save_table(Col_Q = "Q1",
           fold_path = './output/',
           data= answers_together) 

for(i in colnames(answers_together[-c(length(colnames(answers_together))-1, length(colnames(answers_together)))])){
  save_table(Col_Q = i,
             fold_path = './output/',
             data= answers_together)
}
