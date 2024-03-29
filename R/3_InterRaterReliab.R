## calculating the inter-rater reliability, using Cohen's Kappa
library(readxl)
library(dplyr)
library(stringr)
library(irr)
library(irrCAC)
source('./R/Functions.R')

## read in the data coded by the first coder
files <- list.files(path = ('./data/ecology/Comparison_IRR'), full.names = TRUE)

#bind all data frames of FIRST data frame of read_exl() together
answers_CoderA <- data.frame()
answers_CoderB <- data.frame()

for (file in files) {
  tmp_exl <- read_exl(file)
  answers_CoderA <- bind_rows(answers_CoderA, tmp_exl[[1]])
  answers_CoderB <- bind_rows(answers_CoderB, tmp_exl[[2]])
}
row.names(answers_CoderA) <- files
row.names(answers_CoderB) <- files


## Study [123] was excluded from sample set, so dropping it here by searching for NA entry of Q0
answers_CoderA <- answers_CoderA[!is.na(answers_CoderA$Q0), ]
answers_CoderB <- answers_CoderB[!is.na(answers_CoderB$Q0), ]

## Replace NA with 0 for easier operations
answers_CoderA[is.na(answers_CoderA)] <- 0
answers_CoderB[is.na(answers_CoderB)] <- 0

### Start: testing calculations for Gwet's AC1, Cohen's kappa and raw (percentage) agreement---------------

## Calculate raw (percentage) agreement 
tmp_raw_agree <- sum(ifelse(answers_CoderA$Q18==answers_CoderB$Q18, 1, 0))/nrow(answers_CoderA)

## Calculate Gwet's AC1 and Kappa with the irrCAC-package including benchmarking
tmp_contigency_table <-  as.matrix(table(answers_CoderA$Q19, answers_CoderB$Q19)) #create a contingency table

#create support matrix since not always nxn matrix (on coder did not use on of the answer options)
un1 <- unique(sort(c(colnames(tmp_contigency_table), rownames(tmp_contigency_table))))
m2 <- matrix(NA, length(un1), length(un1), dimnames = list(un1, un1))
cols <- colnames(m2)[colnames(m2) %in% colnames(tmp_contigency_table)]
rows <- rownames(m2)[rownames(m2) %in% rownames(tmp_contigency_table)]
m2[rows, cols] <- tmp_contigency_table[rows, cols]
m2[is.na(m2)] <- 0

tmp_contigency_table <- m2

tmp_gwet <- gwet.ac1.table(tmp_contigency_table)
#tmp_gwet_bm <- landis.koch.bf(tmp_gwet$coeff.val,tmp_gwet$coeff.se) 
tmp_kappa <- kappa2.table(tmp_contigency_table)
#tmp_kappa_bm <- landis.koch.bf(tmp_kappa$coeff.val,tmp_kappa$coeff.se) 



### End: testing calculations for kappa and raw agreement------------------------------------------------


#### calculate kappa and raw (percentage) agreement for all questions in one dataframe
int_rel_per_quest <- data.frame()

for (i in 1:(length(answers_CoderB)-2)) {
  if(str_detect(colnames(answers_CoderB)[i+1], "\\.1")){
    next
  }
 
  #Gwet's AC1 and Kappa
  tmp_contigency_table <- as.data.frame.matrix(table(answers_CoderA[i][,1], answers_CoderB[i][,1])) #create a contingency table
  
  if((nrow(tmp_contigency_table)==1) && (ncol(tmp_contigency_table)==1)){
    tmp_gwet = 1 #If bother rater only used one answer the agreement always equals 1 (see e.g. Q20)
    tmp_kappa2 = 1
  } else if(nrow(tmp_contigency_table) == ncol(tmp_contigency_table)){
      tmp_gwet <- gwet.ac1.table(tmp_contigency_table)$coeff.val #only if both rater used all answer option row and column number are the same, which is the needed format for calculating gwet
      tmp_kappa2 <- kappa2.table(tmp_contigency_table)$coeff.val
    }else{
      # create a new contingency table in the correct nxn format
      tmp_contigency_table <-  as.matrix(table(answers_CoderA[i][,1], answers_CoderB[i][,1])) #create a contingency table
      # create support matrix since not always nxn matrix (on coder did not use on of the answer options)
      un1 <- unique(sort(c(colnames(tmp_contigency_table), rownames(tmp_contigency_table))))
      m2 <- matrix(NA, length(un1), length(un1), dimnames = list(un1, un1))
      cols <- colnames(m2)[colnames(m2) %in% colnames(tmp_contigency_table)]
      rows <- rownames(m2)[rownames(m2) %in% rownames(tmp_contigency_table)]
      m2[rows, cols] <- tmp_contigency_table[rows, cols]
      m2[is.na(m2)] <- 0
      # over-write the tmp_contingency_table with the new and correct table
      tmp_contigency_table <- m2
      
      # calculate AC1 and Kappa
      tmp_gwet <- gwet.ac1.table(tmp_contigency_table)$coeff.val
      tmp_kappa2 <- kappa2.table(tmp_contigency_table)$coeff.val
    }
  
  #Raw (percentage) agreement
  tmp_raw_agree <- sum(ifelse(answers_CoderA[i]==answers_CoderB[i], 1, 0))/nrow(answers_CoderA)
  
  # bind all results together in one dataframe
  int_rel_per_quest <- rbind(int_rel_per_quest, c(colnames(answers_CoderB)[i], tmp_kappa2, tmp_gwet, tmp_raw_agree))
}
  
colnames(int_rel_per_quest)[1] <- "Question"
colnames(int_rel_per_quest)[2] <- "Kappa"
colnames(int_rel_per_quest)[3] <- "AC1"
colnames(int_rel_per_quest)[4] <- "Raw_percentage"

# Create a plot of the Kappa results
int_rel_per_quest$Kappa <- as.numeric(int_rel_per_quest$Kappa)
pdf('./plots/hist_CohensKappa.pdf')
hist(int_rel_per_quest$Kappa, main ='', xlab = 'Kappa')
abline(v = median(int_rel_per_quest$Kappa, na.rm = T), col = 'blue', lwd = 2)
dev.off()

# Print all results to csv file
write.csv(int_rel_per_quest, file = './output/Cohenkappa_calc.csv')


### Start: Work in progress--------------------------------------------------------------------------------------------------------------------

quantile(int_rel_per_quest$Kappa, probs = c(0.05, 0.1, 0.25, 0.7, 0.8), na.rm = T)
#### kappa with levels from codebook, not actual data (work in progress)

# retrieve levels from codebook

kappa2(bind_cols(factor(answers_CoderA$Q34.3, levels = c("Yes", "No")), factor(answers_CoderB$Q34.3, levels = c("Yes", "No"))))

library(vcd)
library(caret)


fact_Q0_seb <- factor(answers_CoderA$Q0, levels = c("Yes", "No"))
fact_Q0_mel <- factor(answers_CoderB$Q0, levels = c("Yes", "No"))

test <- confusionMatrix(factor(answers_CoderA$Q11.1, levels = c("Yes", "No", NA)), factor(answers_CoderB$Q11.1, levels = c("Yes", "No", NA)))

## calculate P_e

sum <- 0
for (i in 1:2) {
  sum = sum #+ ()
}

(5*3+11*13)*(1/16^2)
(16*16+0*0)*(1/16)

### End: Work in progress-------------------------------------------------------------------------------------------------------------------
