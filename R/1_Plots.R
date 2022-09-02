## 1. reading in the files

library(readxl)
library(dplyr)
library(ggplot2)
library(ggalluvial)
library(gridExtra)
source('./R/0_ReadData.R')


## vik: general comment ot Johannes. We should check the data for presence of NAs prior to plotting. There should be NAs in such 
# questions as Q0. So, if there is NA it measn ppl did not code the paper properly. And then we should check it with the coders

## Jo: is this necessary for each Question? And if yes, do I need to report this in someway (e.g. a variable containing all na's?)

sum(is.na(answers_together$Q0))
is.na(answers_together$Q0)
answers_together$PaperID[is.na(answers_together$Q0)]

## exclude NA sheets
answers_together <- answers_together[!is.na(answers_together$Q0),]

## 2. plots

color_chosen <- "green"
edge_rec <- 0.5

#Q0
#barplot
#max(sum(answers_together$Q0 == "Yes"), sum(answers_together$Q0 == "No"))
y_max <- max(table(answers_together$Q0))
  
ggplot() +
  geom_bar(data = answers_together, aes(Q0)) +
  geom_rect(aes(xmin = 1.5, xmax = 2.5,
                ymin = -edge_rec, ymax = y_max+  edge_rec), 
            col = color_chosen, alpha = 0) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

ggsave("./plots/Q0.pdf")

#simple table with proportions
Q0_table <- table(answers_together$Q0)
Q0_table <- cbind(Q0_table,prop.table(Q0_table))

##Question formulation phase
#Q1
y_max <- max(table(answers_together$Q1))
ggplot() +
  geom_bar(data = answers_together, aes(Q1)) +
  geom_rect(aes(xmin = 1.5, xmax = 3.5,
                ymin = -edge_rec, ymax = y_max+edge_rec), 
            col = color_chosen, alpha = 0) +
  scale_x_discrete(labels = c("No", "Not as questions, \nbut explicit aim or objective", "Yes", "NA")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

ggsave("./plots/Q1.pdf")

##RQ1.2
##sampling phase
#Q3
#max(c(sum(answers_together$Q3 == "Q3.1"), sum(answers_together$Q3 == "Q3.2"),sum(answers_together$Q3 == "Q3.3"), sum(answers_together$Q3 == "Q3.4")))
x_max = length(unique(answers_together$Q3))-0.5
y_max = max(table(answers_together$Q3))
# add labels of real answer
for (i in 1:length(answers_together$Q3)) {
  switch (answers_together$Q3[i],
    "Q3.1" = answers_together$Q3[i] <- "Journal-Driven",
    "Q3.2" = answers_together$Q3[i] <- "(public) Database-Driven",
    "Q3.3" = answers_together$Q3[i] <- "Seminal-Work-Driven",
    "Q3.4" = answers_together$Q3[i] <- "Others",
    "Q3.5" = answers_together$Q3[i] <- "Approach unclear",
  )
}
answers_together$Q3 <- factor(answers_together$Q3, levels = c("Combined", "Journal-Driven", "(public) Database-Driven", "Seminal-Work-Driven", "Others", "Approach unclear"))

ggplot() +
  geom_bar(data = answers_together, aes(Q3)) +
  geom_rect(aes(xmin = 0.5, xmax = x_max,
                ymin = -edge_rec, ymax = y_max + edge_rec), 
            col = color_chosen, alpha = 0) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("./plots/Q3.pdf")

#Q4
#max(sum(answers_together$Q4 == "Yes"),sum(answers_together$Q4 == "No keyword search applied"))
y_max <- max(table(answers_together$Q4))
ggplot() +
  geom_bar(data = answers_together, aes(Q4)) +
  scale_x_discrete(labels = c("No", "Not keyword \nsearch applied", "Yes")) +
  geom_rect(aes(xmin = 1.5, xmax = 3.5,
                ymin = -edge_rec, ymax = y_max + edge_rec), 
            col = color_chosen, alpha = 0) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

ggsave("./plots/Q4.pdf")

#Q10
answers_together$Q10 <- as.numeric(answers_together$Q10)
summary(answers_together$Q10)

#Q11
Q11_sum <- c()
#names_Q11 <- c("Q11.1", "Q11.2", "Q11.3", "Q11.4", "Q11.5")
names_Q11 <- c("not mentioned", "Scopus", "Web of Science", "Google Scholar", "Others")

for (i in 18:21) {
  Q11_sum <- append(Q11_sum, sum((answers_together[i] == "Yes"), na.rm = T))
}
Q11_sum <- append(Q11_sum, sum((answers_together$Q11.5 != "No"), na.rm = T))
names(Q11_sum) <- names_Q11

y_max = max(Q11_sum)

ggplot(data = data.frame(Q11_sum),aes(seq_along(Q11_sum),Q11_sum)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(name = "Q11", breaks = c(1,2,3,4,5), labels = names_Q11) +
  ylab("count") +
  geom_rect(aes(xmin = 1.5, xmax = 5.5,
                ymin = -edge_rec, ymax = y_max + edge_rec), 
            col = color_chosen, alpha = 0) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("./plots/Q11.pdf")

#Q14
y_max <- max(table(answers_together$Q14))
ggplot() +
  geom_bar(data = answers_together, aes(Q14)) +
  geom_rect(aes(xmin = 1.5, xmax = 2.5,
                ymin = -edge_rec, ymax = y_max + edge_rec), 
            col = color_chosen, alpha = 0) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

ggsave("./plots/Q14.pdf")

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

p <- arrangeGrob(pQ15, pQ16, pQ17, pQ18, pQ19, pQ20)

ggsave("./plots/Q15-Q20.pdf", p)

#Q21
y_max <- max(table(answers_together$Q21))
ggplot() +
  geom_bar(data = answers_together, aes(Q21)) +
  geom_rect(aes(xmin = 1.5, xmax = 2.5,
                ymin = -0.5, ymax = y_max + 0.5), 
            col = color_chosen, alpha = 0) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

ggsave("./plots/Q21.pdf")

#Q22 for supplement
ggplot() +
  geom_bar(data = answers_together, aes(Q22)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

ggsave("./plots/Q22.pdf")

#Q27
Q27_sum <- c()
#names_Q27 <- c("Q27.1", "Q27.2", "Q27.3", "Q27.4", "Q27.6", "Q27.5")
names_Q27 <- c("Main text\n(method section)", "Footnote", "Appendix", "Supplementary\nmaterial", "Other", "Not at all")
for (i in 39:44) {
  Q27_sum <- append(Q27_sum, sum((answers_together[i] == "Yes"), na.rm = T))
}
# exchange Q27.5 and Q27.6 for rectangel representation
tmp_Q27.6 <- Q27_sum[6]
Q27_sum[6] <- Q27_sum[5]
Q27_sum[5] <- tmp_Q27.6

names(Q27_sum) <- names_Q27

y_max = max(Q27_sum)

ggplot(data = data.frame(Q27_sum),aes(seq_along(Q27_sum),Q27_sum)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(name = "Q27", breaks = c(1,2,3,4,5,6), labels = names_Q27) +
  ylab("count") +
  geom_rect(aes(xmin = 0.5, xmax = 5.5,
                ymin = -edge_rec, ymax = y_max + edge_rec), 
            col = color_chosen, alpha = 0) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("./plots/Q27.pdf")

#Q30
y_max <- max(table(answers_together$Q30))
ggplot() +
  geom_bar(data = answers_together, aes(Q30)) +
  geom_rect(aes(xmin = 1.5, xmax = 2.5,
                ymin = -edge_rec, ymax = y_max + edge_rec), 
            col = color_chosen, alpha = 0) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

ggsave("./plots/Q30.pdf")

#Q32
answers_together$Q32 <- as.numeric(answers_together$Q32)
summary(answers_together$Q32)


## Analysis phase
#Q36
Q36_sum <- c()
#names_Q11 <- c("Q11.1", "Q11.2", "Q11.3", "Q11.4", "Q11.5")
names_Q36 <- c("Bibliometric\nanalysis", "Scientific\nmapping", "Meta-analysis", "Structured\ncomparison", "Unstructured\ncomparison", "Others")

for (i in 66:71) {
  Q36_sum <- append(Q36_sum, sum((answers_together[i] == "Yes"), na.rm = T))
}
names(Q36_sum) <- names_Q36

ggplot(data = data.frame(Q36_sum),aes(seq_along(Q36_sum),Q36_sum)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(name = "Q36", breaks = c(1,2,3,4,5,6), labels = names_Q36) +
  ylab("count") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("./plots/Q36.pdf")

# Alluvial plot for the overlap between the phases
#Q1
answers_together$practise_Q1 <- NA
answers_together$practise_Q1[which(answers_together$Q1 != "No")] <- "Yes"
answers_together$practise_Q1[which(answers_together$Q1 == "No")] <- "No"
#Q3
answers_together$practise_Q3 <- NA
answers_together$practise_Q3[which(answers_together$Q3 != "Approach unclear")] <- "Yes"
answers_together$practise_Q3[which(answers_together$Q3 == "Approach unclear")] <- "No"
#Q4
answers_together$practise_Q4 <- NA
answers_together$practise_Q4[which(answers_together$Q4 != "No")] <- "Yes"
answers_together$practise_Q4[which(answers_together$Q4 == "No")] <- "No"
#Q11
answers_together$practise_Q11 <- NA
answers_together$practise_Q11[which(answers_together$Q11.1 != "Yes")] <- "Yes"
answers_together$practise_Q11[which(answers_together$Q11.1 == "Yes")] <- "No"
#Q14
answers_together$practise_Q14 <- NA
answers_together$practise_Q14[which(answers_together$Q14 != "No")] <- "Yes"
answers_together$practise_Q14[which(answers_together$Q14 == "No")] <- "No"
#Q21
answers_together$practise_Q21 <- NA
answers_together$practise_Q21[which(answers_together$Q21 != "No")] <- "Yes"
answers_together$practise_Q21[which(answers_together$Q21 == "No")] <- "No"
#Q27
answers_together$practise_Q27 <- NA
answers_together$practise_Q27[which(answers_together$Q27.5 != "Yes")] <- "Yes"
answers_together$practise_Q27[which(answers_together$Q27.5 == "Yes")] <- "No"
#Q30
answers_together$practise_Q30 <- NA
answers_together$practise_Q30[which(answers_together$Q30 != "No")] <- "Yes"
answers_together$practise_Q30[which(answers_together$Q30 == "No")] <- "No"

#Q36?
## set explicit answer for Q36 (assumed, that it's mutual exclusive)
#for (i in 1:length(answers_together$Q36)) {
#  if(is.na(answers_together$Q36[i])){
#    for (j in 66:71) {
#      if(is.na(answers_together[i,j])) {next}
#      if(answers_together[i,j] == "Yes") {
#        answers_together$Q36[i] <- colnames(answers_together)[j]
#      }
#    }
#  }
#}

#actual alluvial plot: full version
ggplot(data = answers_together, aes(axis1 = practise_Q1, axis2 = practise_Q3, 
                                    axis3 = practise_Q4, axis4 = practise_Q11, 
                                    axis5 = practise_Q14, axis6 = practise_Q21, 
                                    axis7 = practise_Q27, axis8 = practise_Q30,
                                    #axis9 = Q36,
                                    fill = practise_Q1)) +
  geom_alluvium(width = 0) +
  geom_stratum(width = 1/3, fill = "white", color = "grey") +
  scale_x_discrete(limits = c("Q1", "Q3", "Q4", "Q11", "Q14", "Q21", "Q27", "Q30"), expand = c(.05, .05)) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  ylab("count") +
  guides(fill=guide_legend(title="Is a research question\nexplicitly formulated?"))

ggsave("./plots/alluvial_big.pdf")

# shorter version
ggplot(data = answers_together, aes(axis1 = practise_Q1, axis2 = practise_Q4, 
                                    axis3 = practise_Q11, axis4 = practise_Q14, 
                                    axis5 = practise_Q27, 
                                    #axis8 = Q36,
                                    fill = practise_Q1)) +
  geom_alluvium(width = 0) +
  geom_stratum(width = 1/3, fill = "white", color = "grey") +
  scale_x_discrete(limits = c("Q1", "Q4", "Q11", "Q14", "Q27"), expand = c(.05, .05)) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  ylab("count") +
  guides(fill=guide_legend(title="Is a research question\nexplicitly formulated?"))

ggsave("./plots/alluvial_small.pdf")

##RQ3.1
#Q2
#barplot
ggplot() +
  geom_bar(data = answers_together, aes(Q2)) +
  #scale_y_continuous(limits=c(0,10), breaks = c(0,2,4,6,8,10)) + #needs to be adapted to final counts!
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("./plots/Q2.pdf")

#simple table with proportions
Q2_table <- table(answers_together$Q2)
Q2_table <- cbind(Q2_table,prop.table(Q2_table))

#Q33.7
ggplot() +
  geom_bar(data = answers_together, aes(Q33.7)) +
  #scale_y_continuous(breaks = c(2,4,6,8,10,12,14,16)) + #needs to be adapted to final counts!
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("./plots/Q33.7.pdf")

##RQ3.1
#Q33
Q33_sum <- c()
names_Q33 <- c("Description of or new\ninsights about mechanisms", "Behavioral model comparisons",
               "Generalization derived\nfrom the review", "Identify gaps and\nresearch avenues", 
               "Discussion of alternative\nformalizations", "Discussion of theories",
               "Theory development is\nan explicit focus", "Others")

for (i in 51:57) {
  Q33_sum <- append(Q33_sum, sum((answers_together[i] == "Yes"), na.rm = T))
}
Q33_sum <- append(Q33_sum, sum((answers_together$Q33.8 != "No"), na.rm = T))
names(Q33_sum) <- names_Q33

ggplot(data = data.frame(Q33_sum),aes(seq_along(Q33_sum),Q33_sum)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(name = "Q27", breaks = c(1,2,3,4,5,6,7,8), labels = names_Q33) +
  ylab("count") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("./plots/Q33.pdf")

#Q34
Q34_sum <- c()
names_Q34 <- c("Design perspective", "Insight perspective", "Effect perspective", "Other")

for (i in 60:62) {
  Q34_sum <- append(Q34_sum, sum((answers_together[i] == "Yes"), na.rm = T))
}
Q34_sum <- append(Q34_sum, sum((answers_together$Q34.4 != "No"), na.rm = T))
names(Q34_sum) <- names_Q34

ggplot(data = data.frame(Q34_sum),aes(seq_along(Q34_sum),Q34_sum)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(name = "Q27", breaks = c(1,2,3,4), labels = names_Q34) +
  ylab("count") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("./plots/Q34.pdf")

