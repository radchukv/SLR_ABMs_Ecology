## functions used int he scripts

## 1. Function to read the Excel sheets

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
      select(Q_ID, coder) %>%               ## keep the columns we do need - column Q_ID and coder from for-loop
      filter(!row_number() %in% c(2, 5, 6, 13, 17, 30, 31, 45, 46, 56, 60, 86)) 
    
    dat2_trs <- as.data.frame(t(dat2))
    colnames(dat2_trs) <- dat2$Q_ID
    dat2_trs_cl <- dat2_trs %>% 
      filter(row_number() %in% 2) %>% 
      mutate(PaperID = strsplit(strsplit(filename, split = '.xlsx')[[1]], split = '/')[[1]][4],
             coder = strsplit(strsplit(filename, split = '.xlsx')[[1]], split = '/')[[1]][3])
    
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
