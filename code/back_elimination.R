# This script does back elimination on 4 models.
# 1. Topics only
# 2. Demographics and topics
# 3. Structured only
# 4. Structured and topics

list.of.packages <- c("ggplot2", "stringr", "dplyr","reshape2", "survival", "survminer")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(ggplot2)
library(stringr)
library(dplyr)
library(reshape2)
library(survival)
library(survminer)

setwd("C:/Users/Hanna/Desktop/LU/Chapman/Research_2/Survival-and-Topic-Modeling")



for_topic_mod_30 <- read.csv('Intermediate Datasets/for_topic_mod_30_discharge_May2.csv')
for_topic_mod_wide <- dcast(for_topic_mod_30, document ~ topic, value.var="gamma")
names(for_topic_mod_wide) = c("hadm_id", paste0("topic_",names(for_topic_mod_wide)[2:31]))

load("Intermediate Datasets/all_structured_May2.RData")
names(all_surg) <- tolower(names(all_surg))

d_topic <- merge(all_surg, for_topic_mod_wide, by="hadm_id")


load("Intermediate Datasets/tabular_topic_30_May2.RData")

# the following dataset has been uploaded to one-drive.
save(d_topic, file = "Intermediate Datasets/tabular_topic_30_May2.RData")



################################################################################
################################## TOPICS ONLY #################################
################################################################################
# No need to redo Topics Only part
# Select only the first 29 topics
data <- select(d_topic, c(duration, event, names(for_topic_mod_wide)[2:30]))

# Run full model with 29 topics only:
coxph_topic_30 <- coxph(Surv(duration,event) ~ ., data = data)
sink(file = 'Results/back_elimination_redo/topics_only_30.txt')
summary(coxph_topic_30)
sink(file=NULL)

############################## Back Elimination ##################################
# Remove those with p-value > 0.75
data <- select(data, -c(topic_16, topic_26))

coxph_topic_30 <- coxph(Surv(duration,event) ~ ., data = data)
sink(file = 'Results/back_elimination_redo/topics_only_30_back_1.txt')
summary(coxph_topic_30)
sink(file=NULL)


# Remove those with p-value > 0.7
data <- select(data, -c(topic_13, topic_28))

coxph_topic_30 <- coxph(Surv(duration,event) ~ ., data = data)
sink(file = 'Results/back_elimination_redo/topics_only_30_back_2.txt')
summary(coxph_topic_30)
sink(file=NULL)


# Remove those with p-value > 0.6
data <- select(data, -c(topic_11, topic_21))

coxph_topic_30 <- coxph(Surv(duration,event) ~ ., data = data)
sink(file = 'Results/back_elimination_redo/topics_only_30_back_3.txt')
summary(coxph_topic_30)
sink(file=NULL)


# Remove those with p-value > 0.4
data <- select(data, -c(topic_6))

coxph_topic_30 <- coxph(Surv(duration,event) ~ ., data = data)
sink(file = 'Results/back_elimination_redo/topics_only_30_back_4.txt')
summary(coxph_topic_30)
sink(file=NULL)


# Remove those with p-value > 0.3
data <- select(data, -c(topic_8, topic_20))

coxph_topic_30 <- coxph(Surv(duration,event) ~ ., data = data)
sink(file = 'Results/back_elimination_redo/topics_only_30_back_5.txt')
summary(coxph_topic_30)
sink(file=NULL)


# Remove those with p-value > 0.3
data <- select(data, -c(topic_15))

coxph_topic_30 <- coxph(Surv(duration,event) ~ ., data = data)
sink(file = 'Results/back_elimination_redo/topics_only_30_back_6.txt')
summary(coxph_topic_30)
sink(file=NULL)


# Remove those with p-value > 0.3
data <- select(data, -c(topic_10))

coxph_topic_30 <- coxph(Surv(duration,event) ~ ., data = data)
sink(file = 'Results/back_elimination_redo/topics_only_30_back_7.txt')
summary(coxph_topic_30)
sink(file=NULL)


# Remove those with p-value >= 0.15
data <- select(data, -c(topic_7))

coxph_topic_30 <- coxph(Surv(duration,event) ~ ., data = data)
sink(file = 'Results/back_elimination_redo/topics_only_30_back_8.txt')
summary(coxph_topic_30)
sink(file=NULL)


# Remove those with p-value > 0.15 one by one
data <- select(data, -c(topic_3))

coxph_topic_30 <- coxph(Surv(duration,event) ~ ., data = data)
sink(file = 'Results/back_elimination_redo/topics_only_30_back_9.txt')
summary(coxph_topic_30)
sink(file=NULL)

# Remove those with p-value > 0.15 one by one
data <- select(data, -c(topic_17))

coxph_topic_30 <- coxph(Surv(duration,event) ~ ., data = data)
sink(file = 'Results/back_elimination_redo/topics_only_30_back_10.txt')
summary(coxph_topic_30)
sink(file=NULL)


# Remove those with p-value > 0.09
data <- select(data, -c(topic_29))

coxph_topic_30 <- coxph(Surv(duration,event) ~ ., data = data)
sink(file = 'Results/back_elimination_redo/topics_only_30_back_11.txt')
summadata <- select(data, -c(topic_14))

coxph_topic_30 <- coxph(Surv(duration,event) ~ ., data = data)
sink(file = 'Results/back_elimination_redo/topics_only_30_back_12.txt')
summary(coxph_topic_30)
sink(file=NULL)ry(coxph_topic_30)
sink(file=NULL)



# Remove those with p-value > 0.05 one by one
data <- select(data, -c(topic_5))

coxph_topic_30_sig <- coxph(Surv(duration,event) ~ ., data = data)
sink(file = 'Results/back_elimination_redo/topics_only_30_back_13.txt')
summary(coxph_topic_30_sig)
sink(file=NULL)



# Remove those with p-value > 0.05 one by one
data <- select(data, -c(topic_22))

coxph_topic_30_sig <- coxph(Surv(duration,event) ~ ., data = data)
sink(file = 'Results/back_elimination_redo/topics_only_30_back_14.txt')
summary(coxph_topic_30_sig)
sink(file=NULL)

################################################################################
############################## STRUCTURED ONLY #################################
################################################################################

# Take the same HADM_ID's as in the topic data so that the models are comparable

d <- select(d_topic, -c(starts_with("topic")))


x <- select(d, -c(hadm_id))
names(x) <- tolower(names(x))


coxph_str_only <- coxph(Surv(duration,event) ~ ., data = x)

#### did not save the version without ortho
sink(file = 'Results/back_elimination_redo/Structured_only.txt')
summary(coxph_str_only)
sink(file=NULL)

################################################################################
# BACK ELIMINATION

# Remove those with p-value > 0.6
x <- select(x, -c(cevd, diab))
coxph <- coxph(Surv(duration,event) ~ ., data = x)

sink(file = 'Results/back_elimination_redo/Structured_only_back_1.txt')
summary(coxph)
sink(file=NULL)

# Remove those with p-value > 0.6
x <- select(x, -c(pud))
coxph <- coxph(Surv(duration,event) ~ ., data = x)

sink(file = 'Results/back_elimination_redo/Structured_only_back_2.txt')
summary(coxph)
sink(file=NULL)

# Remove those with p-value > 0.45
x <- select(x, -c(ami, aids, diastolic_cat))
coxph <- coxph(Surv(duration,event) ~ ., data = x)

sink(file = 'Results/back_elimination_redo/Structured_only_back_3.txt')
summary(coxph)
sink(file=NULL)

# Remove those with p-value > 0.39
x <- select(x, -c(rheumd))
coxph <- coxph(Surv(duration,event) ~ ., data = x)

sink(file = 'Results/back_elimination_redo/Structured_only_back_4.txt')
summary(coxph)
sink(file=NULL)



# Remove those with p-value > 0.35
x <- select(x, -c(diabwc, heart_cat))
coxph <- coxph(Surv(duration,event) ~ ., data = x)

sink(file = 'Results/back_elimination_redo/Structured_only_back_5.txt')
summary(coxph)
sink(file=NULL)



# Remove those with p-value > 0.32
x <- select(x, -c(copd))
coxph <- coxph(Surv(duration,event) ~ ., data = x)

sink(file = 'Results/back_elimination_redo/Structured_only_back_6.txt')
summary(coxph)
sink(file=NULL)


# Remove those with p-value > 0.3
x <- select(x, -c(dementia))
coxph <- coxph(Surv(duration,event) ~ ., data = x)

sink(file = 'Results/back_elimination_redo/Structured_only_back_7.txt')
summary(coxph)
sink(file=NULL)


# Remove those with p-value > 0.24
x <- select(x, -c(mld, ventilation))
coxph <- coxph(Surv(duration,event) ~ ., data = x)

sink(file = 'Results/back_elimination_redo/Structured_only_back_8.txt')
summary(coxph)
sink(file=NULL)


# Remove those with p-value > 0.2 
x <- select(x, -c(last_icu))
coxph <- coxph(Surv(duration,event) ~ ., data = x)

sink(file = 'Results/back_elimination_redo/Structured_only_back_9.txt')
summary(coxph)
sink(file=NULL)

# Remove those with p-value > 0.18 
x <- select(x, -c(systolic_cat, hp, ethnicity_cat))
coxph <- coxph(Surv(duration,event) ~ ., data = x)

sink(file = 'Results/back_elimination_redo/Structured_only_back_10.txt')
summary(coxph)
sink(file=NULL)

# Remove those with p-value > 0.15 one by one 
x <- select(x, -c(oxygen_cat))
coxph <- coxph(Surv(duration,event) ~ ., data = x)

sink(file = 'Results/back_elimination_redo/Structured_only_back_11.txt')
summary(coxph)
sink(file=NULL)


x <- select(x, -c(resp_cat))
coxph <- coxph(Surv(duration,event) ~ ., data = x)

sink(file = 'Results/back_elimination_redo/Structured_only_back_12.txt')
summary(coxph)
sink(file=NULL)



x <- select(x, -c(marital_status_cat))
coxph <- coxph(Surv(duration,event) ~ ., data = x)

sink(file = 'Results/back_elimination_redo/Structured_only_back_13.txt')
summary(coxph)
sink(file=NULL)

# for model comparison
coxph_str_only_sig <- coxph(Surv(duration,event) ~ ., data = x)


################################################################################
################################ TABULAR + TOPICS ##############################
################################################################################

data <- select(d_topic, -c(hadm_id, topic_30))
coxph_tabular_topic_full <- coxph(Surv(duration,event) ~ ., data = data)

sink(file = 'Results/back_elimination_redo/structured_topics.txt')
summary(coxph_tabular_topic_full)
sink(file=NULL)

############################## Back Elimination ##################################
# Remove those with p-value > 0.8
data <- select(data, -c(topic_28, topic_26, topic_14, topic_21, topic_16, topic_8))

coxph_tabular_topic <- coxph(Surv(duration,event) ~ ., data = data)
sink(file = 'Results/back_elimination_redo/structured_topics_back_1.txt')
summary(coxph_tabular_topic)
sink(file=NULL)



# Remove those with p-value > 0.7
data <- select(data, -c(diab, topic_20))

coxph_tabular_topic <- coxph(Surv(duration,event) ~ ., data = data)
sink(file = 'Results/back_elimination_redo/structured_topics_back_2.txt')
summary(coxph_tabular_topic)
sink(file=NULL)



# Remove those with p-value > 0.6
data <- select(data, -c(mld, topic_15))

coxph_tabular_topic <- coxph(Surv(duration,event) ~ ., data = data)
sink(file = 'Results/back_elimination_redo/structured_topics_back_3.txt')
summary(coxph_tabular_topic)
sink(file=NULL)



# Remove those with p-value > 0.55
data <- select(data, -c(aids, topic_3, topic_6, topic_10))

coxph_tabular_topic <- coxph(Surv(duration,event) ~ ., data = data)
sink(file = 'Results/back_elimination_redo/structured_topics_back_4.txt')
summary(coxph_tabular_topic)
sink(file=NULL)



# Remove those with p-value > 0.65
data <- select(data, -c(systolic_cat, topic_17, cevd, rheumd))

coxph_tabular_topic <- coxph(Surv(duration,event) ~ ., data = data)
sink(file = 'Results/back_elimination_redo/structured_topics_back_5.txt')
summary(coxph_tabular_topic)
sink(file=NULL)



# Remove those with p-value > 0.3
data <- select(data, -c(topic_29, copd, topic_13))

coxph_tabular_topic <- coxph(Surv(duration,event) ~ ., data = data)
sink(file = 'Results/back_elimination_redo/structured_topics_back_6.txt')
summary(coxph_tabular_topic)
sink(file=NULL)



# Remove those with p-value > 0.3
data <- select(data, -c(ami))

coxph_tabular_topic <- coxph(Surv(duration,event) ~ ., data = data)
sink(file = 'Results/back_elimination_redo/structured_topics_back_7.txt')
summary(coxph_tabular_topic)
sink(file=NULL)



# Remove those with p-value > 0.3
data <- select(data, -c(topic_7))

coxph_tabular_topic <- coxph(Surv(duration,event) ~ ., data = data)
sink(file = 'Results/back_elimination_redo/structured_topics_back_8.txt')
summary(coxph_tabular_topic)
sink(file=NULL)



# Remove those with p-value > 0.3
data <- select(data, -c(heart_cat, topic_4))

coxph_tabular_topic <- coxph(Surv(duration,event) ~ ., data = data)
sink(file = 'Results/back_elimination_redo/structured_topics_back_9.txt')
summary(coxph_tabular_topic)
sink(file=NULL)



# Remove those with p-value > 0.25
data <- select(data, -c(diabwc, dementia, had_surgery))

coxph_tabular_topic <- coxph(Surv(duration,event) ~ ., data = data)
sink(file = 'Results/back_elimination_redo/structured_topics_back_10.txt')
summary(coxph_tabular_topic)
sink(file=NULL)



# Remove those with p-value > 0.15
data <- select(data, -c(pud, ethnicity_cat, topic_11))

coxph_tabular_topic <- coxph(Surv(duration,event) ~ ., data = data)
sink(file = 'Results/back_elimination_redo/structured_topics_back_11.txt')
summary(coxph_tabular_topic)
sink(file=NULL)



# Remove those with p-value > 0.15
data <- select(data, -c(hp, ventilation))

coxph_tabular_topic <- coxph(Surv(duration,event) ~ ., data = data)
sink(file = 'Results/back_elimination_redo/structured_topics_back_12.txt')
summary(coxph_tabular_topic)
sink(file=NULL)




# Remove those with p-value > 0.1
data <- select(data, -c(diastolic_cat, topic_22))

coxph_tabular_topic <- coxph(Surv(duration,event) ~ ., data = data)
sink(file = 'Results/back_elimination_redo/structured_topics_back_13.txt')
summary(coxph_tabular_topic)
sink(file=NULL)




# Remove those with p-value > 0.1
data <- select(data, -c(resp_cat))

coxph_tabular_topic <- coxph(Surv(duration,event) ~ ., data = data)
sink(file = 'Results/back_elimination_redo/structured_topics_back_14.txt')
summary(coxph_tabular_topic)
sink(file=NULL)




# Remove those with p-value > 0.1 one by one
data <- select(data, -c(last_icu))

coxph_tabular_topic <- coxph(Surv(duration,event) ~ ., data = data)
sink(file = 'Results/back_elimination_redo/structured_topics_back_15.txt')
summary(coxph_tabular_topic)
sink(file=NULL)




# Remove those with p-value > 0.1 one by one
data <- select(data, -c(oxygen_cat))

coxph_tabular_topic <- coxph(Surv(duration,event) ~ ., data = data)
sink(file = 'Results/back_elimination_redo/structured_topics_back_16.txt')
summary(coxph_tabular_topic)
sink(file=NULL)




# Remove those with p-value > 0.05 one by one
data <- select(data, -c(topic_23))

coxph_tabular_topic <- coxph(Surv(duration,event) ~ ., data = data)
sink(file = 'Results/back_elimination_redo/structured_topics_back_17.txt')
summary(coxph_tabular_topic)
sink(file=NULL)



# Remove those with p-value > 0.05 one by one
data <- select(data, -c(topic_5))

coxph_tabular_topic <- coxph(Surv(duration,event) ~ ., data = data)
sink(file = 'Results/back_elimination_redo/structured_topics_back_18.txt')
summary(coxph_tabular_topic)
sink(file=NULL)



# Remove those with p-value > 0.05 one by one
data <- select(data, -c(topic_2))

coxph_tabular_topic <- coxph(Surv(duration,event) ~ ., data = data)
sink(file = 'Results/back_elimination_redo/structured_topics_back_19.txt')
summary(coxph_tabular_topic)
sink(file=NULL)



# Remove those with p-value > 0.05 one by one
data <- select(data, -c(gender))

coxph_tabular_topic <- coxph(Surv(duration,event) ~ ., data = data)
sink(file = 'Results/back_elimination_redo/structured_topics_back_20.txt')
summary(coxph_tabular_topic)
sink(file=NULL)



# Remove those with p-value > 0.05 one by one
data <- select(data, -c(marital_status_cat))

coxph_tabular_topic <- coxph(Surv(duration,event) ~ ., data = data)
sink(file = 'Results/back_elimination_redo/structured_topics_back_21.txt')
summary(coxph_tabular_topic)
sink(file=NULL)

# for model comparison
coxph_tabular_topic_sig <- coxph(Surv(duration,event) ~ ., data = data)

save(data, file = "Intermediate Datasets/tabular_topic_30_sig.RData")

###############################################################################
############################## MODEL COMPARISON ###############################
###############################################################################

anova(coxph_tabular_topic_full,coxph_str_only)
anova(coxph_tabular_topic_full,coxph_topic_30)


anova(coxph_tabular_topic_full,coxph_tabular_topic_sig)
anova(coxph_tabular_topic_full,coxph_str_only_sig)
anova(coxph_tabular_topic_full,coxph_topic_30_sig)
