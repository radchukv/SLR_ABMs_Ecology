## 1. reading in the files

library(readxl)
library(dplyr)
library(ggplot2)
library(gridExtra)
source('./R/Functions.R')  ## load functions

files <- list.files(path = ('./data/'), full.names = TRUE)

## test the function
ch1 <- read_exl('./data/002.xlsx')


#bind all data frames of FIRST data frame ofread_exl() together
answers_together <- data.frame()
all_data_together <- list()

for (file in files) {
  tmp_exl <- read_exl(file)
  answers_together <- bind_rows(answers_together, tmp_exl[[1]])
}
row.names(answers_together) <- files



## 2. plots

#Q0
ggplot() +
  geom_bar(data = answers_together, aes(Q0))

#Q1
ggplot() +
  geom_bar(data = answers_together, aes(Q1)) +
  scale_x_discrete(labels = c("No", "Not as questions, \nbut explicit aim or objective", "Yes", "NA"))

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