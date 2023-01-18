## functions used int he scripts

## 1. Function to read the Excel sheets

#  1. Function checks whether (1) in folder with "aligned" files or (2) in folder with "comparison" files
#  2. For (1), num_coder = 1 that for-loop runs only one iteration to read the aligned coding 
#      --> output are two dataframes nested in a list: raw data and cleaned aligned coding 
#      --> Used for analysis with plots and tables 
#  3. For (2), num_coder = 2 that for-loop runs two iterations to read the coding of both coders 
#      --> output are three dataframes nested in a list: raw data and cleaned coding for both coder 
#      --> Used for IRR calculation



read_exl <- function(filename) {
  if(grepl('Aligned', filename)){
    dat <- read_xlsx(path = filename,  sheet = "Coding_agreement", range = cell_limits(ul = c(7, 3), lr = c(97, 13)),
                     col_names = c('Q_ID', 'Drop1', 'Drop2', 'Question', 'Drop3', 'AnswerType', 'Explan', 'Source', 'Drop4', 'Answer_CoderAligned', 'Comments_CoderAligned'))
    
    num_coder <- list('Answer_CoderAligned') ##set number of coder in file - Here one for the aligned coding
 
  }
  else{
    dat <- read_xlsx(path = filename,  sheet = "Comparison", range = cell_limits(ul = c(7, 3), lr = c(97, 16)),
                     col_names = c('Q_ID', 'Drop1', 'Drop2', 'Question', 'Drop3', 'AnswerType', 'Explan', 'Source', 'Drop4', 'Answer_CoderA', 'Comments_CoderA','Drop5', 'Answer_CoderB', 'Comments_CoderB'))
    
    num_coder <-  list('Answer_CoderA', 'Answer_CoderB') ##set number of coder in file - Here two for comparing between two coder
  }
  
  ## one dataset with all relevant info
  dat1 <- dat %>% 
    select(!starts_with('Drop')) %>%                                               ## drop the columns we do not need
    filter(!row_number() %in% c(2, 5, 6, 13, 17, 30, 31, 45, 46, 56, 60, 86))      ## drop the rows we do not need
  
  output_list <- list(dat1) ## start output list for return function
  
  for(coder in num_coder){
    
    ## second dataset only with the data on answers + Q number
    dat2 <- dat %>% 
      select(Q_ID, all_of(coder)) %>%               ## keep the columns we do need - column Q_ID and coder from for-loop
      filter(!row_number() %in% c(2, 5, 6, 13, 17, 30, 31, 45, 46, 56, 60, 86)) 
    
    dat2_trs <- as.data.frame(t(dat2))
    colnames(dat2_trs) <- dat2$Q_ID
    dat2_trs_cl <- dat2_trs %>% 
      filter(row_number() %in% 2) %>% 
      mutate(PaperID = strsplit(strsplit(filename, split = '.xlsx')[[1]], split = '/')[[1]][5],
             coder = strsplit(coder, split = '_')[[1]][2])
    
    ##replacing manually added NA values from df to real NAs
    dat2_trs_cl[dat2_trs_cl == "na"] <- NA
    dat2_trs_cl[dat2_trs_cl == "n/a"] <- NA
    dat2_trs_cl[dat2_trs_cl == "N/A"] <- NA
    
    ## set explicit answer for Q3 
    if(is.na(dat2_trs_cl$Q3)){
      for (i in 5:9) {
        if(is.na(dat2_trs_cl[i])) {next}
        if(dat2_trs_cl[i] == "Yes") {
          dat2_trs_cl$Q3 = colnames(dat2_trs_cl)[i]
        }
      }
    }
    
    output_list <- append(list(dat2_trs_cl), output_list)
    
  }
  
  return(output_list)
  
}


## 2. Function to save the results in tabular format

save_table <- function(Col_Q, data,
                       fold_path){
Q_table <- table(data[, Col_Q])
Q_table <- cbind(Q_table,prop.table(Q_table))
table <- t(Q_table)
table_basename <- './output/Quest1'
wb <- openxlsx::createWorkbook("My name here")
openxlsx::addWorksheet(wb, 'Sheet 1', gridLines = FALSE)
openxlsx::writeData(wb, sheet = 1, table)
hs <- openxlsx::createStyle(border = "TopBottom",
                            textDecoration = "Bold",
                            halign = "center")
openxlsx::addStyle(wb, sheet = 1, hs, rows = 1, cols= 1:ncol(table))

openxlsx::saveWorkbook(wb, paste0(fold_path, Col_Q, ".xlsx"), overwrite = TRUE)
print(paste("The table has been created and is saved at location:",
            normalizePath(paste0(fold_path, Col_Q, ".xlsx"))))
}

## 3. Function to count results per question and save them as data frame
 # Format adjusted for use with flextable-package
 # Q_ID needs to be a single char-variable

count_question_result <- function(Q_ID, data, codebook_details) {
  
  
  
  Q = Q_ID
  
  # Count results to be stored in df_results
  result_question = as.data.frame(data %>% group_by(category) %>% count(data[Q]))
  
  # Create data frame in right format to store count-results and afterwards use for flextable-layout
  
  answer_row <- rep(strsplit(filter(codebook_details, Q_ID == Q)[1,3] %>% pull(AnswerType), split = "/")[[1]], times=2) %>% append(NA, after=0)
  category_row <- rep(c("ecology", "social"), each = length(unique(answer_row))-1) %>% append(NA, after=0)
  question <- filter(codebook_details, Q_ID == Q)[1,2] %>% pull(Question)
  question_row <- rep(NA, times = length(answer_row)-1) %>% append(question, after=0)
  df_result <- as.data.frame(rbind(category_row,answer_row,question_row))
  
  for(i in 2:ncol(df_result)) {       # for-loop over columns
    var1 <- df_result[1,i]
    var2 <- df_result[2,i]
    #print(var1)
    #print(var2)
    result_count <- ifelse(identical(filter(result_question, category == var1 & result_question[Q] == var2)$n, integer(0)),
                           0,
                           filter(result_question, category == var1 & result_question[Q] == var2)$n)
    #print(result_count)
    df_result[3,i] <- result_count
  }
  #print(df_result)
  
  return(df_result)

}
