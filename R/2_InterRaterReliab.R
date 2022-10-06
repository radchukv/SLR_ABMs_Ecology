## calculating the inter-rater reliability, using Cohen's Kappa
library(readxl)
library(dplyr)
library(stringr)
library(irr)
source('./R/Functions.R')

## read in the data coded by the first coder
files <- list.files(path = ('./data/Comparison_IRR'), full.names = TRUE)

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


## for now it seems like 123 study was not entered, so dropping it here (later on check with the coders)
answers_CoderA <- answers_CoderA[!is.na(answers_CoderA$Q0), ]
answers_CoderB <- answers_CoderB[!is.na(answers_CoderB$Q0), ]

'''
##### Start: testing calculations for kappa and raw agreement
'''
Q0 <- bind_cols(answers_CoderA$Q0, answers_CoderB$Q0)
kappa2(Q0, weight = 'unweighted')

kappa2(bind_cols(answers_CoderA$Q1, answers_CoderB$Q1))
kappa2(bind_cols(answers_CoderA$Q2, answers_CoderB$Q2))
kappa2(bind_cols(answers_CoderA$Q3, answers_CoderB$Q3))
## this metric is only possible to calculate for cases with mutually-exclusive categories
## OR: one would really go for each sub-question (like Q3.1, Q3.2 etc)
kappa2(bind_cols(answers_CoderA$Q3.1, answers_CoderB$Q3.1))

tmp_raw_agree 
## tmp_raw_agree <- as.data.frame(cbind(anwers_CoderA,ifelse(answers_CoderA$Q0==anwer_CoderB$Q0, 1, 0)))
tmp_raw_agree <- sum(ifelse(answers_CoderA$Q1==answers_CoderB$Q1, 1, 0))/nrow(answers_CoderA)


'''
##### End: testing calculations for kappa and raw agreement
'''

#### kappa for all questions in one df
int_rel_per_quest <- data.frame()

for (i in 1:(length(answers_CoderB)-2)) {
  if(str_detect(colnames(answers_CoderB)[i+1], "\\.1")){
    next
  }
  tmp_kappa <- kappa2(bind_cols(answers_CoderA[i], answers_CoderB[i]))$value
  if(tmp_kappa == "NaN") { #problem with calculation, if only one variable expression exists
    if((nrow(unique(answers_CoderB[i])) == 1 && nrow(unique(answers_CoderA[i])) == 1) ||
       (nrow(unique(answers_CoderB[i])) == 2 && sum(is.na(unique(answers_CoderA[i]))) == 1) ||
       (nrow(unique(answers_CoderA[i])) == 2 && sum(is.na(unique(answers_CoderB[i]))) == 1)) {
    tmp_kappa = 1
    
    }
  }
  
  tmp_raw_agree <- sum(ifelse(answers_CoderA[i]==answers_CoderB[i], 1, 0))/nrow(answers_CoderA)
  
  int_rel_per_quest <- rbind(int_rel_per_quest, c(colnames(answers_CoderB)[i], tmp_kappa, tmp_raw_agree))
}

colnames(int_rel_per_quest)[1] <- "Question"
colnames(int_rel_per_quest)[2] <- "kappa"
colnames(int_rel_per_quest)[3] <- "raw agreement"


int_rel_per_quest$kappa[int_rel_per_quest$Question == 'Q10'] <- NA
int_rel_per_quest$kappa[int_rel_per_quest$Question == 'Q11.5'] <- NA
int_rel_per_quest$kappa[int_rel_per_quest$Question == 'Q24'] <- NA
int_rel_per_quest$kappa[int_rel_per_quest$Question == 'Q25'] <- NA
int_rel_per_quest$kappa[int_rel_per_quest$Question == 'Q32'] <- NA
int_rel_per_quest$kappa[int_rel_per_quest$Question == 'Q34.4'] <- NA
int_rel_per_quest$kappa[int_rel_per_quest$Question == 'Q36.6'] <- NA
int_rel_per_quest$kappa[int_rel_per_quest$Question == 'Q37'] <- NA
int_rel_per_quest$kappa[int_rel_per_quest$Question == 'Q39'] <- NA
int_rel_per_quest$kappa[int_rel_per_quest$Question == 'Q40'] <- NA
int_rel_per_quest$kappa[int_rel_per_quest$Question == 'Q41'] <- NA
int_rel_per_quest$kappa[int_rel_per_quest$Question == 'Q42'] <- NA
int_rel_per_quest$kappa[int_rel_per_quest$Question == 'Q43'] <- NA
int_rel_per_quest$kappa[int_rel_per_quest$Question == 'Q44'] <- NA

int_rel_per_quest$kappa <- as.numeric(int_rel_per_quest$kappa)
pdf('./plots/hist_CohensKappa.pdf')
hist(int_rel_per_quest$kappa, main ='', xlab = 'kappa')
abline(v = median(int_rel_per_quest$kappa, na.rm = T), col = 'blue', lwd = 2)
dev.off()

write.csv(int_rel_per_quest, file = './output/Cohenkappa_calc.csv')
quantile(int_rel_per_quest$kappa, probs = c(0.05, 0.1, 0.25, 0.7, 0.8), na.rm = T)
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
