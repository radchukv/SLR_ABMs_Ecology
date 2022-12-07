## saving each question as a table

library(flextable)
library(dplyr)




## read in the data
source('./R/0_ReadData.R')

#
#Questions where we have a problem: Q20
#

### Table for single questions -------------------------------------------------------------------------------

# settings for table
questions <- list("Q0","Q7","Q8","Q9","Q12","Q13","Q19","Q28","Q30","Q31","Q38")

for (question in questions)  {
caption_text <- paste("Table_", question[[1]], sep = "")
footer_text <- c("This is the footnote for this table")
path_save <- paste("./output/", caption_text,".docx", sep = "")

output <- count_question_result(question[[1]], answers_together, questions_codebook)

ft_input <- output[3,]

## Settings for table layout
 #header setting
ft <- flextable(ft_input) %>% set_header_labels(V2 = output[2,2], 
                                              V3 = output[2,3], 
                                              V4 = output[2,4], 
                                              V5 = output[2,5])
ft <- add_header_row(ft, colwidths = c(1,2,2), values = c("Question","Ecology","Social"))
ft <- align(ft, i = 1, part = "header", align = "center")
ft <- merge_at(ft, i=1:2, j=1, part = "header")
ft <- align(ft, i=1, j=1, part = "header", align = "left")
 #caption setting
ft <- set_caption(ft, caption_text) 
 #footer settings
ft <- add_footer_lines(ft, values = footer_text) 
ft <- color(ft, part = "footer", color = "#666666")
ft <- italic(ft, part = "footer")
 #format settings
ft <- autofit(ft)
ft <- width(ft, j=1, width=8, unit="cm")
 #save table in word file
save_as_docx(ft, path = path_save)

}

### Table for question collection -------------------------------------------------------------------------------
  
# settings for table
question_list <- list(list("Q14","Q15","Q16","Q17","Q18"), list("Q21","Q22","Q23")) #Q14-Q18, Q21-Q23 

for (questions in question_list) {
  
  # merge questions ob sublist in one dataframe
  for (question in questions)  { #check if we are in first loop-iteration
    if (question == questions[1]) {
      output <- count_question_result(question[[1]], answers_together, questions_codebook)
    }
    else {
      output <- rbind(output, count_question_result(question[[1]], answers_together, questions_codebook)[3,])
    }
  }
  
    caption_text <- paste("Table_", questions[[1]], "-", questions[[length(questions)]], sep = "")
    footer_text <- c("This is the footnote for this table")
    path_save <- paste("./output/", caption_text,".docx", sep = "")
    ft_input <- output[3:nrow(output),]
    
    ## Settings for table layout
    #header setting
    ft <- flextable(ft_input) %>% set_header_labels(V2 = output[2,2], 
                                                    V3 = output[2,3], 
                                                    V4 = output[2,4], 
                                                    V5 = output[2,5])
    ft <- add_header_row(ft, colwidths = c(1,2,2), values = c("Question","Ecology","Social"))
    ft <- align(ft, i = 1, part = "header", align = "center")
    ft <- merge_at(ft, i=1:2, j=1, part = "header")
    ft <- align(ft, i=1, j=1, part = "header", align = "left")
    #caption setting
    ft <- set_caption(ft, caption_text) 
    #footer settings
    ft <- add_footer_lines(ft, values = footer_text) 
    ft <- color(ft, part = "footer", color = "#666666")
    ft <- italic(ft, part = "footer")
    #format settings
    ft <- autofit(ft)
    ft <- width(ft, j=1, width=8, unit="cm")
    #save table in word file
    save_as_docx(ft, path = path_save)
  
}

### Table for multiple-answer questions -------------------------------------------------------------------------------

# settings for table
question_list <- list(list("Q3.1","Q3.2","Q3.3","Q3.4","Q3.5"), 
                      list("Q33.1","Q33.2","Q33.3","Q33.4","Q33.5","Q33.6","Q33.7")
                      )
                      #list that do not work yet
                      #list("Q11.1","Q11.2","Q11.3","Q11.4","Q11.5"),
                      #list("Q27.1","Q27.2","Q27.3","Q27.4","Q27.5","Q27.6"),
                      #list("Q34.1","Q34.2","Q34.3"),
                      #list("Q36.1","Q36.2","Q36.3","Q36.4","Q36.5") 

for (questions in question_list) {
  
  # merge questions ob sublist in one dataframe
  for (question in questions)  { #check if we are in first loop-iteration
    if (question == questions[1]) {
      output <- count_question_result(question[[1]], answers_together, questions_codebook)
    }
    else {
      output <- rbind(output, count_question_result(question[[1]], answers_together, questions_codebook)[3,])
    }
  }
  
  caption_text <- paste("Table_", questions[[1]], "-", questions[[length(questions)]], sep = "")
  footer_text <- c("This is the footnote for this table")
  path_save <- paste("./output/", caption_text,".docx", sep = "")
  ft_input <- output[3:nrow(output),]
  
  ## Settings for table layout
  #header setting
  ft <- flextable(ft_input) %>% set_header_labels(V2 = output[2,2], 
                                                  V3 = output[2,3], 
                                                  V4 = output[2,4], 
                                                  V5 = output[2,5])
  ft <- add_header_row(ft, colwidths = c(1,2,2), values = c("Question","Ecology","Social"))
  ft <- align(ft, i = 1, part = "header", align = "center")
  ft <- merge_at(ft, i=1:2, j=1, part = "header")
  ft <- align(ft, i=1, j=1, part = "header", align = "left")
  #caption setting
  ft <- set_caption(ft, caption_text) 
  #footer settings
  ft <- add_footer_lines(ft, values = footer_text) 
  ft <- color(ft, part = "footer", color = "#666666")
  ft <- italic(ft, part = "footer")
  #format settings
  ft <- autofit(ft)
  ft <- width(ft, j=1, width=8, unit="cm")
  #save table in word file
  save_as_docx(ft, path = path_save)
  
}





