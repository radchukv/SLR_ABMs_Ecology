## saving each question as a table

## read in the data
source('./R/0_ReadData.R')

#
#Questions where we have a problem: Q20
#

### Table for single questions with 2 answer options -------------------------------------------------------------------------------

# settings for table
questions <- list("Q0","Q7","Q8","Q9","Q12","Q13","Q19","Q20","Q28","Q30","Q31","Q38")
#questions <- list("Q20")

for (question in questions)  {
caption_text <- paste("Table_", question[[1]], sep = "")
footer_text <- c("This is the footnote for this table")
path_save <- paste("./output/", caption_text,".docx", sep = "")

output <- count_question_result(question[[1]], answers_together, codebook_template)

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
ft <- align(ft, part = "header", align = "center")
 #caption setting
ft <- set_caption(ft, caption_text) 
 #footer settings
ft <- add_footer_lines(ft, values = footer_text) 
ft <- color(ft, part = "footer", color = "#666666")
ft <- italic(ft, part = "footer")
ft <- fontsize(ft, size = 9, part = "footer")
 #format settings
ft <- font(ft, fontname = "Calibri", part = "all")
ft <- align(ft, align = "center", part = "body")
ft <- autofit(ft)
ft <- width(ft, j=1, width=8, unit="cm")
 #save table in word file
save_as_docx(ft, path = path_save)

}

### Table for question collection with 2 answer options -------------------------------------------------------------------------------
  
# settings for table
question_list <- list(list("Q14","Q15","Q16","Q17","Q18"), list("Q21","Q22","Q23")) 

for (questions in question_list) {
  
  # merge questions ob sublist in one dataframe
  for (question in questions)  { #check if we are in first loop-iteration
    if (question == questions[1]) {
      output <- count_question_result(question[[1]], answers_together, codebook_template)
    }
    else {
      output <- rbind(output, count_question_result(question[[1]], answers_together, codebook_template)[3,])
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
    ft <- align(ft, part = "header", align = "center")
    #caption setting
    ft <- set_caption(ft, caption_text) 
    #footer settings
    ft <- add_footer_lines(ft, values = footer_text) 
    ft <- color(ft, part = "footer", color = "#666666")
    ft <- italic(ft, part = "footer")
    ft <- fontsize(ft, size = 9, part = "footer")
    #format settings
    ft <- font(ft, fontname = "Calibri", part = "all")
    ft <- align(ft, align = "center", part = "body")
    ft <- autofit(ft)
    ft <- width(ft, j=1, width=8, unit="cm")
    #save table in word file
    save_as_docx(ft, path = path_save)
  
}

### Table for multiple-answer questions with 2 answer options -------------------------------------------------------------------------------

# settings for table
question_list <- list(list("Q3.1","Q3.2","Q3.3","Q3.4","Q3.5"),
                      list("Q11.1","Q11.2","Q11.3","Q11.4","Q11.5"),
                      list("Q27.1","Q27.2","Q27.3","Q27.4","Q27.5","Q27.6"),
                      list("Q33.1","Q33.2","Q33.3","Q33.4","Q33.5","Q33.6","Q33.7"),
                      list("Q34.1","Q34.2","Q34.3"),
                      list("Q36.1","Q36.2","Q36.3","Q36.4","Q36.5") 
                      )


for (questions in question_list) {
  
  # merge questions ob sublist in one dataframe
  for (question in questions)  { #check if we are in first loop-iteration
    if (question == questions[1]) {
      output <- count_question_result(question[[1]], answers_together, codebook_template)
    }
    else {
      output <- rbind(output, count_question_result(question[[1]], answers_together, codebook_template)[3,])
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
  ft <- add_header_row(ft, colwidths = c(1,2,2), values = c(filter(codebook_template, Q_ID == substr(questions[[1]],1,nchar(questions[[1]])-3))[1,2],
                                                            "Ecology",
                                                            "Social")) #Instead of "question" write the actual question as first element in the heading
  ft <- align(ft, i = 1, part = "header", align = "center")
  ft <- merge_at(ft, i=1:2, j=1, part = "header")
  ft <- align(ft, part = "header", align = "center")
  
  #ft <- set_header_labels(ft, Question = filter(codebook_template, Q_ID == substr(questions[[1]],1,nchar(questions[[1]])-3))[1,2])
  
  #caption setting
  ft <- set_caption(ft, caption_text) 
  #footer settings
  ft <- add_footer_lines(ft, values = footer_text)
  ft <- color(ft, part = "footer", color = "#666666")
  ft <- italic(ft, part = "footer")
  ft <- fontsize(ft, size = 9, part = "footer")
  #format settings
  ft <- font(ft, fontname = "Calibri", part = "all")
  ft <- align(ft, align = "center", part = "body")
  ft <- autofit(ft)
  ft <- width(ft, j=1, width=8, unit="cm")
  #save table in word file
  save_as_docx(ft, path = path_save)
  
}


### Table for single questions with 3 answer options-------------------------------------------------------------------------------

# settings for table
questions <- list("Q1","Q2","Q4","Q5","Q6","Q29") #"Q26" has 4 answer possibilities

for (question in questions)  {
  caption_text <- paste("Table_", question[[1]], sep = "")
  footer_text <- c("This is the footnote for this table")
  path_save <- paste("./output/", caption_text,".docx", sep = "")
  
  output <- count_question_result(question[[1]], answers_together, codebook_template)
  
  ft_input <- output[3,]
  
  ## Settings for table layout
  #header setting
  ft <- flextable(ft_input) %>% set_header_labels(V2 = output[2,2], 
                                                  V3 = output[2,3], 
                                                  V4 = output[2,4], 
                                                  V5 = output[2,5],
                                                  V6 = output[2,6],
                                                  V7 = output[2,7])
  ft <- add_header_row(ft, colwidths = c(1,3,3), values = c("Question","Ecology","Social"))
  ft <- align(ft, i = 1, part = "header", align = "center")
  ft <- merge_at(ft, i=1:2, j=1, part = "header")
  ft <- align(ft, part = "header", align = "center")
  #caption setting
  ft <- set_caption(ft, caption_text) 
  #footer settings
  ft <- add_footer_lines(ft, values = footer_text) 
  ft <- color(ft, part = "footer", color = "#666666")
  ft <- italic(ft, part = "footer")
  ft <- fontsize(ft, size = 9, part = "footer")
  #format settings
  ft <- font(ft, fontname = "Calibri", part = "all")
  ft <- align(ft, align = "center", part = "body")
  ft <- autofit(ft)
  ft <- width(ft, j=1, width=4, unit="cm")
  ft <- width(ft, j=2:7, width=2.5, unit="cm")
  #save table in word file
  save_as_docx(ft, path = path_save)
  
}

### Table for single questions with 4 answer options !! something is not yet working here-------------------------------------------------------------------------------

# settings for table
questions <- list("Q26") 

for (question in questions)  {
  caption_text <- paste("Table_", question[[1]], sep = "")
  footer_text <- c("This is the footnote for this table")
  path_save <- paste("./output/", caption_text,".docx", sep = "")
  
  output <- count_question_result(question[[1]], answers_together, codebook_template)
  
  ft_input <- output[3,]
  
  ## Settings for table layout
  #header setting
  ft <- flextable(ft_input) %>% set_header_labels(V2 = output[2,2], 
                                                  V3 = output[2,3], 
                                                  V4 = output[2,4], 
                                                  V5 = output[2,5],
                                                  V6 = output[2,6],
                                                  V7 = output[2,6],
                                                  V8 = output[2,6],
                                                  V9 = output[2,7])
  ft <- add_header_row(ft, colwidths = c(1,4,4), values = c("Question","Ecology","Social"))
  ft <- align(ft, i = 1, part = "header", align = "center")
  ft <- merge_at(ft, i=1:2, j=1, part = "header")
  ft <- align(ft, part = "header", align = "center")
  #caption setting
  ft <- set_caption(ft, caption = caption_text, style="Table Caption") 
  #footer settings
  ft <- add_footer_lines(ft, values = footer_text) 
  ft <- color(ft, part = "footer", color = "#666666")
  ft <- italic(ft, part = "footer")
  ft <- fontsize(ft, size = 9, part = "footer")
  #format settings
  ft <- font(ft, fontname = "Calibri", part = "all")
  ft <- align(ft, align = "center", part = "body")
  ft <- autofit(ft)
  ft <- width(ft, j=1, width=4, unit="cm")
  ft <- width(ft, j=2:7, width=2, unit="cm")
  #save table in word file
  save_as_docx(ft, path = path_save)
  
}
