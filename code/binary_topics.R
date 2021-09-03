library(ggplot2)
library(stringr)
library(dplyr)
library(reshape2)
library(survival)
library(survminer)

setwd("C:/Users/Hanna/Desktop/LU/Chapman/Research_2/Survival-and-Topic-Modeling")

load("Intermediate Datasets/tabular_topic_30_May2.RData")

to_binary <- function(x){
  ifelse(x>=1/30, 1, 0)
}

topics <- select(d_topic, starts_with("topic"))
topic_binary <- sapply(topics, to_binary)

d_topic_binary <- select(d_topic, -starts_with("topic"))
d_topic_binary <- cbind(d_topic_binary, topic_binary)

save(d_topic_binary, file = "Intermediate Datasets/tabular_topic_binary.RData")




back_elimination <- function(df){
  flag <- TRUE
  while(flag){
    model <- coxph(Surv(duration,event) ~ ., data = df)
    if(max(coef(summary(model))[,5])>0.05){
      for(i in 1:(ncol(df)-2)){
        if(coef(summary(model))[i,5] == max(coef(summary(model))[,5])){
          df <- select(df, -rownames(coef(summary(model)))[i])
          break
        }
      } 
    } else {
      flag <- FALSE
      break
    }
  }
  return(model)
}

#########################################################################
# interactions on topics only
load("Intermediate Datasets/tabular_topic_binary.RData")

data1 <- select(d_topic_binary, c("duration","event", paste0("topic_",seq(1,30))))
topic_binary <- coxph(Surv(duration,event) ~ .^2, data = data1)
sink(file = 'Results/back_elimination_binary/topics_binary_interaction.txt')
summary(topic_binary)

rownames(coef(summary(topic_binary))[coef(summary(topic_binary))[,5]<0.1,])

##################################################################################
# 30 binary topics only - full model:
data2 <- select(d_topic_binary, c("duration","event", paste0("topic_",seq(1,30))))
topic_binary <- coxph(Surv(duration,event) ~ ., data = data2)
sink(file = 'Results/back_elimination_binary/topics_binary.txt')
summary(topic_binary)
sink(file=NULL)

# 30 Binary Topics only - after back elimination:
topic_binary_sig <- back_elimination(data2)
sink(file = 'Results/back_elimination_binary/topics_binary_sig.txt')
summary(topic_binary_sig)
sink(file=NULL)

##################################################################################
# Run full model with tabular and 30 binary topics:
data <- select(d_topic_binary, -c(hadm_id))
tabular_topic_binary <- coxph(Surv(duration,event) ~ ., data = data)
sink(file = 'Results/back_elimination_binary/tabular_topics_binary.txt')
summary(tabular_topic_binary)
sink(file=NULL)


# Tabular + 30 Binary Topics:
# Remove those with p-value > 0.9
data <- select(data, -c(ami, cevd, topic_7, topic_14))

tabular_topic_binary_sig <- coxph(Surv(duration,event) ~ ., data = data)
sink(file = 'Results/back_elimination_binary/tabular_topics_binary_back_1.txt')
summary(tabular_topic_binary_sig)
sink(file=NULL)


# Remove those with p-value > 0.7
data <- select(data, -c(topic_16, topic_8))

tabular_topic_binary_sig <- coxph(Surv(duration,event) ~ ., data = data)
sink(file = 'Results/back_elimination_binary/tabular_topics_binary_back_2.txt')
summary(tabular_topic_binary_sig)
sink(file=NULL)

# Remove those with p-value > 0.6
data <- select(data, -c(topic_6, topic_13, topic_23))

tabular_topic_binary_sig <- coxph(Surv(duration,event) ~ ., data = data)
sink(file = 'Results/back_elimination_binary/tabular_topics_binary_back_3.txt')
summary(tabular_topic_binary_sig)
sink(file=NULL)


# Remove those with p-value > 0.5
data <- select(data, -c(diab, diabwc, topic_4, topic_17, topic_22, topic_29, systolic_cat))

tabular_topic_binary_sig <- coxph(Surv(duration,event) ~ ., data = data)
sink(file = 'Results/back_elimination_binary/tabular_topics_binary_back_4.txt')
summary(tabular_topic_binary_sig)
sink(file=NULL)


# Remove those with p-value > 0.5
data <- select(data, -c(copd, mld, topic_10))

tabular_topic_binary_sig <- coxph(Surv(duration,event) ~ ., data = data)
sink(file = 'Results/back_elimination_binary/tabular_topics_binary_back_5.txt')
summary(tabular_topic_binary_sig)
sink(file=NULL)


# Remove those with p-value > 0.4
data <- select(data, -c(last_icu, rheumd, aids, topic_25))

tabular_topic_binary_sig <- coxph(Surv(duration,event) ~ ., data = data)
sink(file = 'Results/back_elimination_binary/tabular_topics_binary_back_6.txt')
summary(tabular_topic_binary_sig)
sink(file=NULL)



# Remove those with p-value > 0.3
data <- select(data, -c(topic_5, topic_26))

tabular_topic_binary_sig <- coxph(Surv(duration,event) ~ ., data = data)
sink(file = 'Results/back_elimination_binary/tabular_topics_binary_back_7.txt')
summary(tabular_topic_binary_sig)
sink(file=NULL)



# Remove those with p-value > 0.25
data <- select(data, -c(ethnicity_cat, dementia, topic_3))

tabular_topic_binary_sig <- coxph(Surv(duration,event) ~ ., data = data)
sink(file = 'Results/back_elimination_binary/tabular_topics_binary_back_8.txt')
summary(tabular_topic_binary_sig)
sink(file=NULL)


# Remove those with p-value > 0.2
data <- select(data, -c(ventilation, pud, hp, heart_cat))

tabular_topic_binary_sig <- coxph(Surv(duration,event) ~ ., data = data)
sink(file = 'Results/back_elimination_binary/tabular_topics_binary_back_9.txt')
summary(tabular_topic_binary_sig)
sink(file=NULL)



# Remove those with p-value > 0.1 one by one
data <- select(data, -c(topic_2))

tabular_topic_binary_sig <- coxph(Surv(duration,event) ~ ., data = data)
sink(file = 'Results/back_elimination_binary/tabular_topics_binary_back_10.txt')
summary(tabular_topic_binary_sig)
sink(file=NULL)



# Remove those with p-value > 0.1 one by one
data <- select(data, -c(resp_cat))

tabular_topic_binary_sig <- coxph(Surv(duration,event) ~ ., data = data)
sink(file = 'Results/back_elimination_binary/tabular_topics_binary_back_11.txt')
summary(tabular_topic_binary_sig)
sink(file=NULL)



# Remove those with p-value > 0.1 one by one
data <- select(data, -c(marital_status_cat))

tabular_topic_binary_sig <- coxph(Surv(duration,event) ~ ., data = data)
sink(file = 'Results/back_elimination_binary/tabular_topics_binary_back_12.txt')
summary(tabular_topic_binary_sig)
sink(file=NULL)



# Remove those with p-value > 0.1 one by one
data <- select(data, -c(diastolic_cat))

tabular_topic_binary_sig <- coxph(Surv(duration,event) ~ ., data = data)
sink(file = 'Results/back_elimination_binary/tabular_topics_binary_back_13.txt')
summary(tabular_topic_binary_sig)
sink(file=NULL)


# Remove those with p-value > 0.05 one by one
data <- select(data, -c(topic_30 ))

tabular_topic_binary_sig <- coxph(Surv(duration,event) ~ ., data = data)
sink(file = 'Results/back_elimination_binary/tabular_topics_binary_back_14.txt')
summary(tabular_topic_binary_sig)
sink(file=NULL)



# Remove those with p-value > 0.05 one by one
data <- select(data, -c(topic_15))

tabular_topic_binary_sig <- coxph(Surv(duration,event) ~ ., data = data)
sink(file = 'Results/back_elimination_binary/tabular_topics_binary_back_15.txt')
summary(tabular_topic_binary_sig)
sink(file=NULL)



# Remove those with p-value > 0.05 one by one
data <- select(data, -c(topic_21))

tabular_topic_binary_sig <- coxph(Surv(duration,event) ~ ., data = data)
sink(file = 'Results/back_elimination_binary/tabular_topics_binary_back_16.txt')
summary(tabular_topic_binary_sig)
sink(file=NULL)



# Remove those with p-value > 0.05 one by one
data <- select(data, -c(topic_28))

tabular_topic_binary_sig <- coxph(Surv(duration,event) ~ ., data = data)
sink(file = 'Results/back_elimination_binary/tabular_topics_binary_back_17.txt')
summary(tabular_topic_binary_sig)
sink(file=NULL)




# Remove those with p-value > 0.05 one by one
data <- select(data, -c(gender))

tabular_topic_binary_sig <- coxph(Surv(duration,event) ~ ., data = data)
sink(file = 'Results/back_elimination_binary/tabular_topics_binary_back_18.txt')
summary(tabular_topic_binary_sig)
sink(file=NULL)




# Remove those with p-value > 0.05 one by one
data <- select(data, -c(topic_18))

tabular_topic_binary_sig <- coxph(Surv(duration,event) ~ ., data = data)
sink(file = 'Results/back_elimination_binary/tabular_topics_binary_back_19.txt')
summary(tabular_topic_binary_sig)
sink(file=NULL)



# Remove those with p-value > 0.05 one by one
data <- select(data, -c(topic_19))

tabular_topic_binary_sig <- coxph(Surv(duration,event) ~ ., data = data)
sink(file = 'Results/back_elimination_binary/tabular_topics_binary_back_20.txt')
summary(tabular_topic_binary_sig)
sink(file=NULL)



# Remove those with p-value > 0.05 one by one
data <- select(data, -c(oxygen_cat))

tabular_topic_binary_sig <- coxph(Surv(duration,event) ~ ., data = data)
sink(file = 'Results/back_elimination_binary/tabular_topics_binary_back_21.txt')
summary(tabular_topic_binary_sig)
sink(file=NULL)



# Remove those with p-value > 0.05 one by one
data <- select(data, -c(topic_11))

tabular_topic_binary_sig <- coxph(Surv(duration,event) ~ ., data = data)
sink(file = 'Results/back_elimination_binary/tabular_topics_binary_back_22.txt')
summary(tabular_topic_binary_sig)
sink(file=NULL)



# Remove those with p-value > 0.05 one by one
data <- select(data, -c(topic_24))

tabular_topic_binary_sig <- coxph(Surv(duration,event) ~ ., data = data)
sink(file = 'Results/back_elimination_binary/tabular_topics_binary_back_23.txt')
summary(tabular_topic_binary_sig)
sink(file=NULL)



# Remove those with p-value > 0.05 one by one
data <- select(data, -c(topic_24))

tabular_topic_binary_sig <- coxph(Surv(duration,event) ~ ., data = data)
sink(file = 'Results/back_elimination_binary/tabular_topics_binary_back_23.txt')
summary(tabular_topic_binary_sig)
sink(file=NULL)

save(data, file = "Intermediate Datasets/tabular_binary_topics_sig.RData")

###############################################################################
############################## MODEL COMPARISON ###############################
###############################################################################


anova(tabular_topic_binary,coxph_str_only) # structure only full model
anova(tabular_topic_binary,topic_binary) # binary topics only (full model)


anova(tabular_topic_binary,tabular_topic_binary_sig) # tabular + binary topics (after backward elimination)
anova(tabular_topic_binary,coxph_str_only_sig) # tabular only (after backward elimination)
anova(tabular_topic_binary,topic_binary_sig) # binary topics only (after backward elimination)
