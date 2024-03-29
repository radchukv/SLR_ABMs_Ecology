## 1. reading in the files

library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggalluvial)
library(gridExtra)
library(scales)
library(patchwork)
library(reshape2)
library(cowplot)
library(svglite)
source('./R/0_ReadData.R')


### Now exluded from folder from the beginning
## we exclude the 123 paper because it was not coded, the coders decided it does not belong to the sample actually

#sum(is.na(answers_together$Q0))
#is.na(answers_together$Q0)
#answers_together$PaperID[is.na(answers_together$Q0)]


## exclude NA sheets
answers_together <- answers_together[!is.na(answers_together$Q0),]


### Pre-analysis------------------------------------------------------------------------------------------------


## Settings

color_chosen <- "green"
edge_rec <- 0.5

##RQ phase

#Q0
#barplot
#max(sum(answers_together$Q0 == "Yes"), sum(answers_together$Q0 == "No"))
y_max <- max(table(answers_together$Q0))
ggplot(data = answers_together, aes(x = Q0)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) +
  scale_y_continuous(breaks= pretty_breaks()) +
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
ggplot(data = answers_together, aes(x = Q1)) +
  geom_bar() +
  geom_rect(aes(xmin = 1.5, xmax = 3.5,
                ymin = -edge_rec, ymax = y_max+edge_rec), 
            col = color_chosen, alpha = 0) +
  scale_x_discrete(labels = c("No", "Not as questions, \nbut explicit aim or objective", "Yes", "NA")) +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) +
  scale_y_continuous(breaks= pretty_breaks()) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

ggsave("./plots/Q1.pdf")


#Q2
#barplot
ggplot(data = answers_together, aes(x = Q2)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) +
  scale_y_continuous(breaks= pretty_breaks()) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("./plots/Q2.pdf")

#simple table with proportions
Q2_table <- table(answers_together$Q2)
Q2_table <- cbind(Q2_table,prop.table(Q2_table))


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

ggplot(data = answers_together, aes(x = Q3)) +
  geom_bar() +
  geom_rect(aes(xmin = 0.5, xmax = x_max,
                ymin = -edge_rec, ymax = y_max + edge_rec), 
            col = color_chosen, alpha = 0) +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) +
  scale_y_continuous(breaks= pretty_breaks()) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("./plots/Q3.pdf")

#Q4
#max(sum(answers_together$Q4 == "Yes"),sum(answers_together$Q4 == "No keyword search applied"))
y_max <- max(table(answers_together$Q4))
ggplot(data = answers_together, aes(x = Q4)) +
  geom_bar() +
  scale_x_discrete(labels = c("No", "Not keyword \nsearch applied", "Yes")) +
  geom_rect(aes(xmin = 1.5, xmax = 3.5,
                ymin = -edge_rec, ymax = y_max + edge_rec +0.5), 
            col = color_chosen, alpha = 0) +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) +
  scale_y_continuous(breaks= pretty_breaks()) +
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
  geom_text(aes(label = Q11_sum), vjust=-0.5) +
  scale_y_continuous(breaks= pretty_breaks()) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust=1))

ggsave("./plots/Q11.pdf")

#Q14
y_max <- max(table(answers_together$Q14))
ggplot(data = answers_together, aes(x = Q14)) +
  geom_bar() +
  geom_rect(aes(xmin = 1.5, xmax = 2.5,
                ymin = -edge_rec, ymax = y_max + edge_rec), 
            col = color_chosen, alpha = 0) +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) +
  scale_y_continuous(breaks= pretty_breaks()) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

ggsave("./plots/Q14.pdf")

#Q15-Q20 for supplement
pQ15 <- ggplot(data = answers_together, aes(x = Q15)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) +
  scale_y_continuous(limits = c(0,max(table(answers_together$Q20))+3), breaks = pretty_breaks()) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 
pQ16 <- ggplot(data = answers_together, aes(x = Q16)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) +
  scale_y_continuous(limits = c(0,max(table(answers_together$Q20))+3), breaks = pretty_breaks()) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 
pQ17 <- ggplot(data = answers_together, aes(x = Q17)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) +
  scale_y_continuous(limits = c(0,max(table(answers_together$Q20))+3), breaks = pretty_breaks()) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 
pQ18 <- ggplot(data = answers_together, aes(x = Q18)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) +
  scale_y_continuous(limits = c(0,max(table(answers_together$Q20))+3), breaks = pretty_breaks()) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 
pQ19 <- ggplot(data = answers_together, aes(x = Q19)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) +
  scale_y_continuous(limits = c(0,max(table(answers_together$Q20))+3), breaks = pretty_breaks()) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 
pQ20 <- ggplot(data = answers_together, aes(x = Q20)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) +
  scale_y_continuous(limits = c(0,max(table(answers_together$Q20))+3), breaks = pretty_breaks()) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

p <- arrangeGrob(pQ15, pQ16, pQ17, pQ18, pQ19, pQ20)

ggsave("./plots/Q15-Q20.pdf", p)

#Q21
y_max <- max(table(answers_together$Q21))
ggplot(data = answers_together, aes(x = Q21)) +
  geom_bar() +
  geom_rect(aes(xmin = 1.5, xmax = 2.5,
                ymin = -0.5, ymax = y_max + 0.5), 
            col = color_chosen, alpha = 0) +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) +
  scale_y_continuous(breaks= pretty_breaks()) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

ggsave("./plots/Q21.pdf")

#Q22 for supplement
ggplot(data = answers_together, aes(x = Q22)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) +
  scale_y_continuous(breaks= pretty_breaks()) +
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
  geom_text(aes(label = Q27_sum), vjust=-0.5) +
  scale_y_continuous(breaks= pretty_breaks()) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust=1))

ggsave("./plots/Q27.pdf")

#Q30
y_max <- max(table(answers_together$Q30))
ggplot(data = answers_together, aes(x = Q30)) +
  geom_bar() +
  geom_rect(aes(xmin = 1.5, xmax = 2.5,
                ymin = -edge_rec, ymax = y_max + edge_rec), 
            col = color_chosen, alpha = 0) +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) +
  scale_y_continuous(breaks= pretty_breaks()) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

ggsave("./plots/Q30.pdf")

#Q32
answers_together$Q32 <- as.numeric(answers_together$Q32)
summary(answers_together$Q32)

#Q32 update according to fig. 4.4 paper
answers_ecology$Q32 <- as.numeric(answers_ecology$Q32)
answers_social$Q32 <- as.numeric(answers_social$Q32)

p1 <- ggplot(data = answers_ecology[!is.na(answers_ecology$Q32),], 
       aes(reorder(PaperID, -Q32), Q32)) +
  geom_bar(stat="identity") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  ylab("Sample size in review article") +
  xlab("Article ID") +
  ggtitle("ecology")


p2 <- ggplot(data = answers_social[!is.na(answers_social$Q32),], 
       aes(reorder(PaperID, -Q32), Q32)) +
  geom_bar(stat="identity") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y=element_blank()) +
  ylab("Sample size in review article") +
  xlab("Article ID") +
  ggtitle("social")

p1 + p2
ggsave("./plots/Q32_fig4.4.pdf")


#Q36
Q36_sum <- c()
#names_Q11 <- c("Q11.1", "Q11.2", "Q11.3", "Q11.4", "Q11.5")

for (i in 66:71) {
  Q36_sum <- append(Q36_sum, sum((answers_together[i] == "Yes"), na.rm = T))
}

names_Q36 <- c("Bibliometric\nanalysis", "Scientific\nmapping", "Meta-analysis", "Structured\ncomparison", "Others", "Unstructured\ncomparison")
names(Q36_sum) <- names_Q36

# exchange order of unstructured comparison and others for plotting good practise box
tmp <- Q36_sum[6]
Q36_sum[6] <- Q36_sum[5]
Q36_sum[5] <- tmp

y_max <- max(Q36_sum)

ggplot(data = data.frame(Q36_sum),aes(seq_along(Q36_sum),Q36_sum)) +
  geom_bar(stat = "identity") +
  geom_rect(aes(xmin = 0.5, xmax = 5.5,
                ymin = -edge_rec, ymax = y_max + edge_rec), 
            col = color_chosen, alpha = 0) +
  geom_text(aes(label = Q36_sum), vjust=-0.5) +
  scale_x_continuous(name = "Q36", breaks = c(1,2,3,4,5,6), labels = names_Q36) +
  scale_y_continuous(breaks= pretty_breaks()) +
  ylab("count") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust=1))

ggsave("./plots/Q36.pdf")


#Q33.7
ggplot(data = answers_together, aes(x = Q33.7)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) +
  scale_y_continuous(breaks= pretty_breaks()) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("./plots/Q33.7.pdf")

#Q34 (bar chart)
Q34_sum <- c()
names_Q34 <- c("Design perspective", "Insight perspective", "Effect perspective", "Other")

for (i in 60:62) {
  Q34_sum <- append(Q34_sum, sum((answers_together[i] == "Yes"), na.rm = T))
}
Q34_sum <- append(Q34_sum, sum((answers_together$Q34.4 != "No"), na.rm = T))
names(Q34_sum) <- names_Q34

ggplot(data = data.frame(Q34_sum),aes(seq_along(Q34_sum),Q34_sum)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Q34_sum), vjust=-0.5) +
  scale_x_continuous(name = "Q34", breaks = c(1,2,3,4), labels = names_Q34) +
  scale_y_continuous(breaks= pretty_breaks()) +
  ylab("count") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust=1))

ggsave("./plots/Q34.pdf")


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
  geom_text(aes(label = Q33_sum), vjust=-0.5) +
  scale_x_continuous(name = "Q33", breaks = c(1,2,3,4,5,6,7,8), labels = names_Q33) +
  scale_y_continuous(breaks= pretty_breaks()) +
  ylab("count") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust=1))

ggsave("./plots/Q33.pdf")

### Analysis plotting found in article -----------------------------------------------------------------------

# Define criteria for good practice based on selected questions
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
#Q36
answers_together$practise_Q36 <- NA
answers_together$practise_Q36[which(answers_together$Q36.5 != "Yes")] <- "Yes"
answers_together$practise_Q36[which(answers_together$Q36.5 == "Yes")] <- "No"



# Updated version for separated alluvial plots

# add good practice for phase 2: sampling phase
answers_together$practise_phase_2 <- NA
for (j in 1:length(answers_together$Q0)) {
  for (i in 83:89) {
    if(answers_together[j,i] == "No") {
      answers_together$practise_phase_2[j] = "No"
      break
    }
  }
}
answers_together$practise_phase_2[is.na(answers_together$practise_phase_2)] <- "Yes"

all <- rep("", 42)
for (i in 1:length(answers_together$practise_Q1)) {
  ifelse(answers_together$practise_Q1[i] == "Yes" && answers_together$practise_phase_2[i] == "Yes" && answers_together$practise_Q36[i] == "Yes", all[i] <- "Yes", all[i] <- "No") 
}

answers <- c(answers_together$practise_Q1, answers_together$practise_phase_2, answers_together$practise_Q36, all)
studies <- rep(1:42, 4)
category <- rep(answers_together$category, 4)
phase <- c(rep("Quest_form", 42), rep("Samp_phase", 42),rep("Ana_phase", 42), rep("all", 42))
alluvial_data <- data.frame(studies, phase, answers)
alluvial_data$phase <- factor(alluvial_data$phase, levels = c("Quest_form", "Samp_phase", "Ana_phase", "all"))
alluvial_data$category <- category

p1 <- ggplot(alluvial_data[alluvial_data$category == "ecology",], 
       aes(x = phase, stratum = answers, alluvium = studies,
           fill = answers, label = answers)) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  geom_flow(stat = "alluvium", lode.guidance = "frontback",
            color = "darkgray") +
  geom_stratum(fill = "white", color = "grey") +
  scale_x_discrete(labels = c("Question\nformulation", "Sampling\nstep", "Analysis\nstep", "Overall"), expand = c(.05, .05)) +
  geom_text(stat = "stratum", size = 3) +
  guides(fill="none") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), 
        axis.title = element_text(size = 8), plot.title = element_text(size = 8),
        legend.title = element_text(size=8),axis.text.x = element_text(size =8, color = "black"),
        axis.text.y = element_text(size =8, color = "black")) +
  ylab("Number of articles") +
  xlab("SLR steps") +
  #guides(fill=guide_legend(title="Does the paper comply with the 'good practice?'")) +
  ggtitle("Ecology")

p2 <- ggplot(alluvial_data[alluvial_data$category == "social",], 
       aes(x = phase, stratum = answers, alluvium = studies,
           fill = answers, label = answers)) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  geom_flow(stat = "alluvium", lode.guidance = "frontback",
            color = "darkgray") +
  geom_stratum(fill = "white", color = "grey") +
  scale_x_discrete(labels = c("Question\nformulation", "Sampling\nstep", "Analysis\nstep", "Overall"), expand = c(.05, .05)) +
  geom_text(stat = "stratum", size = 3) +
  theme(legend.position = "bottom") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), 
        axis.title = element_text(size = 8), legend.position = 'bottom',plot.title = element_text(size = 8),
        axis.text.x = element_text(size =8, color = "black"), axis.text.y = element_text(size =8, color = "black"),
        legend.title = element_text(size=8)) +
  ylab("Number of articles") +
  xlab("SLR steps") +
  guides(fill=guide_legend(title="Does the paper comply with 'good practice?' ")) +
  ggtitle("Social Sciences")

p1 / p2

ggsave("./plots/alluvial_separated_update.png", dpi=300)




## VennDiagram alternative

devtools::install_github("gaospecial/ggVennDiagram")
library("ggVennDiagram")


# bring data in right dataframe format
ven_ecology_data <- answers_together[answers_together$category =="ecology", 
                                     c("PaperID", "practise_Q1", "practise_phase_2", "practise_Q36")]
ven_ecology_data$practise_Q1 <- ifelse(ven_ecology_data$practise_Q1 == "Yes", 1, 0)
ven_ecology_data$practise_phase_2 <- ifelse(ven_ecology_data$practise_phase_2 == "Yes", 1, 0)
ven_ecology_data$practise_Q36 <- ifelse(ven_ecology_data$practise_Q36 == "Yes", 1, 0)

ven_social_data <- answers_together[answers_together$category =="social", 
                                     c("PaperID", "practise_Q1", "practise_phase_2", "practise_Q36")]
ven_social_data$practise_Q1 <- ifelse(ven_social_data$practise_Q1 == "Yes", 1, 0)
ven_social_data$practise_phase_2 <- ifelse(ven_social_data$practise_phase_2 == "Yes", 1, 0)
ven_social_data$practise_Q36 <- ifelse(ven_social_data$practise_Q36 == "Yes", 1, 0)

# change to list
ven_ecology_list <- list(
  Question = unlist(as.list(ven_ecology_data[ven_ecology_data$practise_Q1 == 1,"PaperID"])), 
  Sampling = unlist(as.list(ven_ecology_data[ven_ecology_data$practise_phase_2 == 1,"PaperID"])), 
  Analysis = unlist(as.list(ven_ecology_data[ven_ecology_data$practise_Q36 == 1,"PaperID"])))

ven_social_list <- list(
  Question = unlist(as.list(ven_social_data[ven_social_data$practise_Q1 == 1,"PaperID"])), 
  Sampling = unlist(as.list(ven_social_data[ven_social_data$practise_phase_2 == 1,"PaperID"])), 
  Analysis = unlist(as.list(ven_social_data[ven_social_data$practise_Q36 == 1,"PaperID"])))

# plot venn diagram for ecology
ggVennDiagram(ven_ecology_list, label_alpha = 0.6, edge_lty = 0, set_size = 3, 
              category.names = c("Question formulation","Sampling step","Analysis step")) +
  ggplot2::scale_fill_gradient(low="white",high = "aquamarine3") +
  ggplot2::ggtitle("Does the review follows 'good practice'?", 
                   subtitle = "Ecology papers: n=29") +
  ggplot2::theme(legend.position='none')


# plot venn diagram for social
ggVennDiagram(ven_social_list, label_alpha = 0.6, edge_lty = 0, set_size = 4, 
                    category.names = c("Question formulation","Sampling step","Analysis step")) +
  ggplot2::scale_x_continuous(expand = expansion(mult = .3)) +
  ggplot2::scale_fill_gradient(low="white",high = "grey") +
  ggplot2::ggtitle("Does the review follows 'good practice'?", subtitle = "Social Science papers: n=13") +
  ggplot2::theme(legend.position='bottom', 
                 legend.direction='horizontal')


#Q33 new Figure 5.3

#theory <- read_excel("fig53.xlsx")
Q33_df <- select(answers_together, PaperID, category, Q33.2, Q33.1, Q33.6, Q33.5, Q33.3, Q33.4, Q33.7)
Q33_df <- Q33_df %>% rename("Dim-1" = "Q33.2",
                            "Dim-2" = "Q33.1",
                            "Dim-3" = "Q33.6",
                            "Dim-4" = "Q33.5",
                            "Dim-5" = "Q33.3",
                            "Dim-6" = "Q33.4",
                            "Dim-7" = "Q33.7")

Q33_df_long <- gather(Q33_df, Dimension, Discussed, `Dim-1`:`Dim-7`, factor_key=TRUE)

ecology <- Q33_df_long %>% filter(category == "ecology")
social <- Q33_df_long %>% filter(category == "social")

pQ33_ecol <- ggplot(data = ecology, aes(x=PaperID, y=Dimension, fill=Discussed)) + 
  geom_tile(colour="white") +
  coord_fixed(ratio=1) +
  scale_fill_manual(values=c("grey90", "#3EB489")) + 
  labs(x = "Article ID", y = "Theory\nDimensions") + 
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5)) 
pQ33_social <- ggplot(data = social, aes(x=PaperID, y=Dimension, fill=Discussed)) + 
  geom_tile(colour="white") + 
  coord_fixed(ratio=1) +
  scale_fill_manual(values=c("grey90", "#3EB489")) + 
  labs(x = "Article ID", y = "Theory\nDimensions") + 
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))

# Arrange the plots in one graph with cowplots
# Here we use library("cowplot")
ggdraw() +
  draw_plot(pQ33_ecol, x = 0, y = .55, width = 1, height = .3) +
  draw_plot(pQ33_social, x = 0, y = .1, width = 1, height = .4) +
  draw_plot_label(label = c("Ecology", "Social Sciences"), size = 10,
                  x = c(.28, .39), y = c(.87, 0.52))

ggsave("./plots/fig53.svg", width = 1000, height=600, limitsize = FALSE)




### Old code ---------------------------------------------------------------------------------------------------

#Q33 old version of Figure 5.3
Q33_df <- data.frame(
  c(rep("Q33.1", 42), rep("Q33.2", 42), rep("Q33.3", 42), rep("Q33.4", 42),
    rep("Q33.5", 42), rep("Q33.6", 42), rep("Q33.7", 42), rep("Q33.8", 42)),
  c(answers_together$Q33.1, answers_together$Q33.2, answers_together$Q33.3, 
    answers_together$Q33.4, answers_together$Q33.5, answers_together$Q33.6,
    answers_together$Q33.7, answers_together$Q33.8),
  rep(answers_together$PaperID, 8),
  rep(answers_together$category, 8)
)
names(Q33_df) <- c("subquestion", "answer", "Paper_ID", "category")
Q33_df$answer_num <- 0
for (i in 1:length(Q33_df$answer)) {
  if (Q33_df$answer[i] == "Yes") {
    Q33_df$answer_num[i] <- 1
  }
}


p1 <- ggplot() +
  geom_bar(data = Q33_df[Q33_df$category == "ecology",], 
           aes(fill = subquestion, y=answer_num, x=reorder(Paper_ID, -answer_num)), 
           position="stack", stat="identity", width=0.8) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = 'none', axis.title.x = element_blank()) +
  ylab("count") +
  ylim(0, 6) +
  ggtitle("ecology")

p2 <- ggplot() +
  geom_bar(data = Q33_df[Q33_df$category == "social",], 
           aes(fill = subquestion, y=answer_num, x=reorder(Paper_ID, -answer_num)), 
           position="stack", stat="identity", width=0.4) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x = element_blank()) +
  ylab("count") +
  ggtitle("social")


p1 + p2

ggsave("./plots/Q33_fig5.3.pdf")

