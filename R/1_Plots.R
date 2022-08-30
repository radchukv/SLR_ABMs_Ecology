## 1. reading in the files

library(readxl)
library(dplyr)
library(ggplot2)
library(gridExtra)
source('./R/0_ReadData.R')


## vik: general comment ot Johannes. We should check the data for presence of NAs prior to plotting. There should be NAs in such 
# questions as Q0. So, if there is NA it measn ppl did not code the paper properly. And then we should check it with the coders

## Jo: is this necessary for each Question? And if yes, do I need to report this in someway (e.g. a variable containing all na's?)

sum(is.na(answers_together$Q0))
is.na(answers_together$Q0)
answers_together$PaperID[is.na(answers_together$Q0)]

## 2. plots

#Q0
#barplot
ggplot() +
  geom_bar(data = answers_together, aes(Q0))
#simple table with proportions
Q0_table <- table(answers_together$Q0)
Q0_table <- cbind(Q0_table,prop.table(Q0_table))

##Question formulation phase
#Q1
ggplot() +
  geom_bar(data = answers_together, aes(Q1)) +
  scale_x_discrete(labels = c("No", "Not as questions, \nbut explicit aim or objective", "Yes", "NA"))

##RQ1.2
##sampling phase
#Q3
ggplot() +
  geom_bar(data = answers_together, aes(Q3))

#Q4
# comment J: description says only 3 bars? so no NA?
ggplot() +
  geom_bar(data = answers_together, aes(Q4)) +
  scale_x_discrete(labels = c("No", "Not keyword \nsearch applied", "Yes", "NA"))

#Q10
answers_together$Q10 <- as.numeric(answers_together$Q10)
summary(answers_together$Q10)

#Q11
Q11_sum <- c()
names_Q11 <- c("Q11.1", "Q11.2", "Q11.3", "Q11.4", "Q11.5")
for (i in 18:21) {
  Q11_sum <- append(Q11_sum, sum((answers_together[i] == "Yes"), na.rm = T))
}
Q11_sum <- append(Q11_sum, sum((answers_together$Q11.5 != "No"), na.rm = T))
names(Q11_sum) <- names_Q11

barplot(Q11_sum)

#Q14
ggplot() +
  geom_bar(data = answers_together, aes(Q14))

#Q15-Q20 for supplement
pQ15 <- ggplot() +
  geom_bar(data = answers_together, aes(Q15))
pQ16 <- ggplot() +
  geom_bar(data = answers_together, aes(Q16))
pQ17 <- ggplot() +
  geom_bar(data = answers_together, aes(Q17))
pQ18 <- ggplot() +
  geom_bar(data = answers_together, aes(Q18))
pQ19 <- ggplot() +
  geom_bar(data = answers_together, aes(Q19))
pQ20 <- ggplot() +
  geom_bar(data = answers_together, aes(Q20))

grid.arrange(pQ15, pQ16, pQ17, pQ18, pQ19, pQ20)

#Q21
ggplot() +
  geom_bar(data = answers_together, aes(Q21))

#Q22 for supplement
ggplot() +
  geom_bar(data = answers_together, aes(Q22))

#Q27
Q27_sum <- c()
names_Q27 <- c("Q27.1", "Q27.2", "Q27.3", "Q27.4", "Q27.5", "Q27.6")
for (i in 39:44) {
  Q27_sum <- append(Q27_sum, sum((answers_together[i] == "Yes"), na.rm = T))
}
names(Q27_sum) <- names_Q27


barplot(Q27_sum, ylim = c(0,10))

#Q30
ggplot() +
  geom_bar(data = answers_together, aes(Q30))

#Q32
answers_together$Q32 <- as.numeric(answers_together$Q32)
summary(answers_together$Q32)


## Analysis phase
#Q36 Alluvial plot

##RQ3.1
#Q2
#barplot
ggplot() +
  geom_bar(data = answers_together, aes(Q2)) +
  scale_y_continuous(limits=c(0,10), breaks = c(0,2,4,6,8,10)) #needs to be adapted to final counts!

#simple table with proportions
Q2_table <- table(answers_together$Q2)
Q2_table <- cbind(Q2_table,prop.table(Q2_table))

#Q33.7
ggplot() +
  geom_bar(data = answers_together, aes(Q33.7)) +
  scale_y_continuous(breaks = c(2,4,6,8,10,12,14,16)) #needs to be adapted to final counts!

##RQ3.1
#Q33
Q33_sum <- c()
names_Q33 <- c("Q33.1", "Q33.2", "Q33.3", "Q33.4", "Q33.5", "Q33.6", "Q33.7", "Q33.8")
for (i in 51:57) {
  Q33_sum <- append(Q33_sum, sum((answers_together[i] == "Yes"), na.rm = T))
}
Q33_sum <- append(Q33_sum, sum((answers_together$Q33.8 != "No"), na.rm = T))
names(Q33_sum) <- names_Q33

barplot(Q33_sum)

#Q34
Q34_sum <- c()
names_Q34 <- c("Q34.1", "Q34.2", "Q34.3", "Q34.4")
for (i in 60:62) {
  Q34_sum <- append(Q34_sum, sum((answers_together[i] == "Yes"), na.rm = T))
}
Q34_sum <- append(Q34_sum, sum((answers_together$Q34.4 != "No"), na.rm = T))
names(Q34_sum) <- names_Q34

barplot(Q34_sum)

