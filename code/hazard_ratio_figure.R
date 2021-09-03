# This script plots the Hazard Ratios from the Cox model for the tabular + topics after backward elimination

library(ggplot2)
library(stringr)
library(dplyr)
library(reshape2)
library(survival)
library(survminer)

setwd("C:/Users/Hanna/Desktop/LU/Chapman/Research_2/Survival-and-Topic-Modeling")


load("Intermediate Datasets/tabular_topic_30_sig.RData")


names(data) <- c("Insurance", "Admission type", "duration","event",
                 "Congestive heart failure", "Peripheral vascular disease",
                 "Renal disease","Cancer", "Moderate/severe liver disease", 
                 "Metastatic solid tumour", "Body temperature", 
                  "Age","Hospital LOS (in days)", 
                  "Topic 1","Topic 9","Topic 12","Topic 18",
                 "Topic 19", "Topic 24", "Topic 25", "Topic 27")


data_2 <- data[, c(12,2,1,13,11,10,8,7,5,6,9,21,14,17,16,15,19,20,18,3,4)]

coxph_tabular_topic_sig <- coxph(Surv(duration,event) ~ ., data = data_2)

summary(coxph_tabular_topic_sig)

tiff(file="Model Output/plot_hazard_ratio_redo_2.tiff",
     width=10, height=15, units="in", res=250)
ggforest(coxph_tabular_topic_sig, data = data_2, main=NULL, fontsize = 0.8)
dev.off()

svg(file="Model Output/plot_hazard_ratio_redo.svg")
ggforest(coxph_tabular_topic_sig, data = data_2, main=NULL, fontsize = 0.8)
dev.off()


###############################################################################
############################## BINARY TOPICS ###############################
###############################################################################
load("Intermediate Datasets/tabular_binary_topics_sig.RData")

data <- within(data, {
  chf <- factor(chf, levels = c("0", "1"), labels = c("No", "Yes"))
  pvd <- factor(pvd, levels = c("0", "1"), labels = c("No", "Yes"))
  rend <- factor(rend, levels = c("0", "1"), labels = c("No", "Yes"))
  canc <- factor(canc, levels = c("0", "1"), labels = c("No", "Yes"))
  msld <- factor(msld, levels = c("0", "1"), labels = c("No", "Yes"))
  metacanc <- factor(metacanc, levels = c("0", "1"), labels = c("No", "Yes"))
  had_surgery <- factor(had_surgery, levels = c("0", "1"), labels = c("No", "Yes"))
  topic_1 <- factor(topic_1, levels = c("0", "1"), labels = c("No", "Yes"))
  topic_9 <- factor(topic_9, levels = c("0", "1"), labels = c("No", "Yes"))
  topic_12 <- factor(topic_12, levels = c("0", "1"), labels = c("No", "Yes"))
  topic_20 <- factor(topic_20, levels = c("0", "1"), labels = c("No", "Yes"))
  topic_27 <- factor(topic_27, levels = c("0", "1"), labels = c("No", "Yes"))
})
             


names(data) <- c("Insurance", "Admission type", "duration","event",
                 "Congestive heart failure", "Peripheral vascular disease",
                 "Renal disease","Cancer", "Moderate/severe liver disease", 
                 "Metastatic solid tumor", "Body temperature", 
                 "Age","Hospital LOS (in days)", "Had surgery",
                 "Topic 1","Topic 9","Topic 12", "Topic 20", "Topic 27")



data_2 <- data[, c(12,2,1,13,11,14,10,8,7,5,6,9,15,19,17,18,16,3,4)]


coxph_tabular_topic_sig <- coxph(Surv(duration,event) ~ ., data = data_2)

summary(coxph_tabular_topic_sig)

tiff(file="Model Output/plot_HR_redo_binary_topic_4.tiff",
     width=10, height=15, units="in", res=250)
ggforest(coxph_tabular_topic_sig, data = data_2, main=NULL, fontsize = 0.8)
dev.off()

svg(file="Model Output/plot_HR_redo_binary_topic_4.svg")
ggforest(coxph_tabular_topic_sig, data = data_2, main=NULL, fontsize = 0.8)
dev.off()
