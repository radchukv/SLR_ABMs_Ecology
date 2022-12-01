## saving each question as a table

library(flextable)
library(dplyr)




## read in the data
source('./R/0_ReadData.R')

question_id <- list("Q0", "Q2","Q3.1")
question_id <- question_id[[3]]

test <- as.data.frame(t(answers_together %>% group_by(category) %>% count(Q3.1)))
test <-cbind(filter(questions_codebook, Q_ID == "Q3.1")[2],test)

#test <- test[3,]

#header setting
ft <- flextable(test) %>% set_header_labels(V1 = "No", V2 = "Yes", V3 = "No", V4 = "Yes")
ft <- add_header_row(ft, colwidths = c(1,2,2), values = c("Question","Ecology","Social"))
ft <- align(ft, i = 1, part = "header", align = "center")
ft <- merge_at(ft, i=1:2, j=1, part = "header")
ft <- align(ft, i=1, j=1, part = "header", align = "left")

#footer settings
ft <- add_footer_lines(ft, values = "This is the footnote for this table")
ft <- color(ft, part = "footer", color = "#666666")
ft <- italic(ft, part = "footer")
ft
  

rep(c("ecology"),times=nrow(answers_ecology))