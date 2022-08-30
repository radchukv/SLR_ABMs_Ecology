## functions used int he scripts

## 1. Function to read the Excel sheets

read_exl <- function(filename) {
  dat <- read_xlsx(path = filename,  sheet = "Coding_tool", range = cell_limits(ul = c(7, 3), lr = c(97, 13)),
                   col_names = c('Q_ID', 'Drop1', 'Drop2', 'Question', 'Drop3', 'AnswerType', 'Explan', 'Source', 'Drop4', 'Answer', 'Comments'))
  
  ## one dataset with all relevant info
  dat1 <- dat %>% 
    select(!starts_with('Drop')) %>%                                               ## drop the columns we do not need
    filter(!row_number() %in% c(2, 5, 6, 13, 17, 30, 31, 45, 46, 56, 60, 86))      ## drop the rows we do not need
  
  
  ## sencod dataset only with the data on answers + Q number
  dat2 <- dat %>% 
    select(c('Q_ID', 'Answer')) %>%                              ## keep the columns we do not need
    filter(!row_number() %in% c(2, 5, 6, 13, 17, 30, 31, 45, 46, 56, 60, 86)) 
  
  dat2_trs <- as.data.frame(t(dat2))
  colnames(dat2_trs) <- dat2$Q_ID
  dat2_trs_cl <- dat2_trs %>% 
    filter(row_number() %in% 2) %>% 
    mutate(PaperID = strsplit(strsplit(filename, split = '.xlsx')[[1]], split = 'a/')[[1]][2])
  
  ##replacing manually added NA values from df to real NAs
  dat2_trs_cl[dat2_trs_cl == "na"] <- NA
  dat2_trs_cl[dat2_trs_cl == "n/a"] <- NA
  dat2_trs_cl[dat2_trs_cl == "N/A"] <- NA
  
  return(list(dat2_trs_cl, dat1))
}
