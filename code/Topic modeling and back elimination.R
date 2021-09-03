# This script runs topic modeling on the notes data from the NOTEEVENTS table, and extract a specified number of topics from the notes.

library(dplyr)
library(stringr)
library(tidyverse)
library(tidytext) 
library(topicmodels) # for LDA topic modelling 
library(tm) # general text mining functions, making document term matrixes
library(SnowballC) # for stemming
library(reshape2)
library(ggplot2)

setwd("C:/Users/Hanna/Desktop/LU/Chapman/Research_2/Survival-and-Topic-Modeling")

# Import NOTEEVENTS dataset
filename <- gzfile('Data/NOTEEVENTS.csv.gz','rt')
note <- read.csv(filename)
note_2 <- select(note, c(HADM_ID, CATEGORY, TEXT))


# Take Discharge Summary notes only
note_3 <- note_2 %>%
  filter(!is.na(note_2$HADM_ID)) %>%
  filter(CATEGORY =="Discharge summary")
  # filter(CATEGORY %in% c("Discharge summary", "Nursing"))

rm(note)
rm(note_2)

# Import main data set with structured data
load("Intermediate Datasets/all_structured_May2.RData")

# Join the two data sets so that the topic modeling is only on the selected cohort
names(note_3) <- tolower(names(note_3))
note_home <- note_3 %>%
  merge(all_surg, by="hadm_id") %>%
  select(hadm_id, text) 

# Function that removes special characters and numbers
rm_characters <- function(x){
  x = str_replace_all(x, "[\n]", " ")
  x = str_replace_all(x, "[0-9]", " ")
  x = str_replace_all(x, "[^[:alnum:]]", " ")
  return(x)
}
note_home$text <- sapply(note_home$text, rm_characters)

# Combine all notes for each hadm_id (each hadm_id can have multiple notes)
note_unique <- note_home %>%
  group_by(hadm_id) %>%
  mutate(Note = paste(text, collapse = " ")) %>%
  select(hadm_id, Note) %>%
  distinct()


###############################################################################
############################ REMOVE STOP WORDS ################################
###############################################################################

# load stopwords
# english_stopwords <- readLines("https://slcladal.github.io/resources/stopwords_en.txt", encoding = "UTF-8")

# Split into words and remove standard stop words
words <- note_unique %>%
  unnest_tokens(input = Note, output = word, format = "text", 
                token = "words", drop = T, to_lower = T) %>%
  anti_join(stop_words, by = "word") %>%
  count(hadm_id, word, sort = TRUE) %>%
  ungroup()

# Define customized stop words
custom_stop_words <- c('admission', 'date','birth', 'sex','discharge','insurance', 'update',
                       'patient', 'primary','reviewer','hospital','year','old', 'first',
                       'last', 'name','pattern', 'job', 'number', 'service','addendum',
                       'male','female', 'fservice','mservice','yesterday',"namepattern", 
                       "provider", "doctor", "nameis", "phone","week", "weeks","hours", 
                       "week","month","months", "day","days", "fax", "telephone",
                       "pain", 'medic', 'medical', 'call', 'hour', 'post', 'home', 'histori',
                       'history', 'instruct', 'instruction', 'find', 'finding', 'findings', 
                       'studies', 'studied','study', 'left', 'note', 'identified', 'identify', 
                       'report',  
                       'sinus', 'prior', 'air', 'daily', 'diseases', 'disease', 'neg',
                       'negative', 'sig', 'normal', 'disp', 'refill', 'unit', 'type', 'scale',
                       'follow', 'time', 'start', 'stable', 'stabl', 'ward', 'depart', 
                       'department', 'park', 'lab', 'laboratory', 'medication', 'continuous',
                       'continued', 'capsules', 'capsule', 'outpati', 'outpatient', 'left', 'stitl',
                       'check', 'dose', 'day', 'rate', 'upper', 'lower', 'appoint', 'appointment',
                       'post', 'sust', 'final', 'posit', 'pend', 'pending', 'mouth', 'exam',
                       'examination', 'location', 'status', 'deny', 'symptom', 'symptoms','ago',
                       'family', 'remain', 'clinic', 'count', 'care', 'reveal', 'time',
                       "cell", 'examin', 'continu', 'continuous', "every", "for", "show", "showed",
                       "tablet", "while", "when", "take", "need", "followup", "per", 
                       "with", "appear", "appeared","chief", "complaint", "attending", "drug", 
                       "reaction", "surgery","allergies", "identifier", "daily","more", 
                       "Street", "Address","up", "in", "on", "if", "md","final", "new",
                       "blood","mg","anymore",
                       'cta', 'bid', 'prn', 'tid', 'bedtime', 'tablet', 'diet', 'deny', 'campus',
                       'park', 'parked', 'parking', 'slide', 'slides', 'sliding', 'refill', 
                       'refills', 'refilled', 'pcp', 'evidence', 'evid', 'impression', 
                       'impressive', 'impressed', 'impress',
                       "aa", "aaa", "aad","aado", "aaf","aam", "aao","aaox",
                       "aast","aax","ab","zoo","zone","zones", "stitle",
                       "daytime", "nighttime", "suppertime", "dinnertime","lifetime",
                       "mealtime","meantime","times","timing","tip","tire","toilet","toileting")

# Remove customized stop words, words less than 2 characters, and words appear only once.  
words_clean <- words %>%
  filter(nchar(word)>=3) %>% 
  filter(!(word %in% custom_stop_words)) %>%
  filter(!(word=="")) %>%
  filter(n>=2)# remove words appear only once(mostly neighboring words without space in between)

###############################################################################
###############################################################################
# write.csv(words_clean, 'Intermediate Datasets/words_clean_discharge_May2.csv')



# Perform word stemming so that words with the same root will be combined as one
words_clean_2 <- words_clean %>%
  mutate(word = stemDocument(word)) # stemming


# Convert the dataframe to a Document Term Matrix for topic modeling
words_dtm <- cast_dtm(data = words_clean_2, document = hadm_id, term = word, value = n)


# 30 topics
start_time <- Sys.time()
print(start_time)

mod <- LDA(words_dtm, k=30, method = "Gibbs")
for_topic_mod_30 <- tidy(mod, matrix = "gamma")
write.csv(for_topic_mod_30, 'Intermediate Datasets/for_topic_mod_30_discharge_May2.csv')

end_time <- Sys.time()
print(end_time-start_time)
#Time difference of 37.17434 mins

###############################################################################
################################# IDENTIFY TOPICS #############################
###############################################################################
# Save the top 20 words in each topic for topic concept identification
topic_top <- tidy(mod, matrix = "beta")

top_terms <- topic_top %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

write.csv(top_terms, "Intermediate Datasets/top_terms_discharge_30topics_May2.csv")



###############################################################################
############################# TABULAR + 30 TOPICS #############################
###############################################################################

# For the 30 topics with a long list of stop words removed
# Reshape for_topic_mod from long to wide and then merge with tabular data:
for_topic_mod_wide <- dcast(for_topic_mod_30, document ~ topic, value.var="gamma")
names(for_topic_mod_wide) = c("hadm_id", paste0("topic_",names(for_topic_mod_wide)[2:31]))
d_topic <- merge(all_surg, for_topic_mod_wide, by="hadm_id")


time <- d_topic$Duration
event <- d_topic$EVENT

# Remove Id column and the last topic from modeling
data <- select(d_topic, -c(hadm_id, Duration, EVENT, topic_30))
names(data) <- tolower(names(data))

# Run full model (tabular + 30 topics):
coxph_topic <- coxph(Surv(time,event) ~ ., data = data)
sink(file = 'Results/tabular_30_topics_full.txt')
summary(coxph_topic)
sink(file=NULL)

########################################################################################
# For the 30 topics with a short list of stop words removed (the very first 30 topics)
for_topic_mod_30_2 <- read.csv("Intermediate Datasets/for_topic_mod_30_2.csv")
for_topic_mod_wide_2 <- dcast(for_topic_mod_30_2, document ~ topic, value.var="gamma")
names(for_topic_mod_wide_2) = c("hadm_id", paste0("topic_",names(for_topic_mod_wide_2)[2:31]))
d_topic_2 <- merge(all_surg, for_topic_mod_wide_2, by="hadm_id")


time <- d_topic_2$Duration
event <- d_topic_2$EVENT

# Remove Id column and the last topic from modeling
data <- select(d_topic_2, -c(hadm_id, Duration, EVENT, topic_30))
names(data) <- tolower(names(data))

# Run full model (tabular + 30 topics):
coxph_topic <- coxph(Surv(time,event) ~ ., data = data)
sink(file = 'Results/tabular_30_original_topics_full.txt')
summary(coxph_topic)
sink(file=NULL)


###############################################################################
############################ 30 TOPICS ONLY ###################################
###############################################################################

# Plot the distribution of the gammas for each topic
tiff(file="Model Output/topic_30_customized_stop_density.tiff",res = 200)
topic_density <- ggplot(for_topic_mod_30) +
  geom_density(aes(gamma),color="blue")+
  facet_wrap(~topic)+
  xlim(c(0,0.1)) +
  theme(
    strip.text.x = element_text(
      size = 5, color = "black"
    ),
    strip.text.y = element_text(
      size = 5, color = "black"
    ),
    axis.text.x = element_text(size=5),
    axis.text.y = element_text(size=5)
  )
print(topic_density)
dev.off()


##***********************************************************************
##***********************************************************************
##***********************************************************************


# Remove the ID variable and the last topic (to eliminate multicoliearity)
# Keep only the first 20 topics
data <- select(d_topic, names(for_topic_mod_wide)[2:30])

# Run full model with 30 topics only:
coxph_topic_30 <- coxph(Surv(time,event) ~ ., data = data)
sink(file = 'Results/topics_only_30.txt')
summary(coxph_topic_30)
sink(file=NULL)

# Run full model with other variables too:
data_full <- select(d_topic, -c(X,hadm_id, Duration, EVENT, topic_30))
coxph_topic_30_full <- coxph(Surv(time,event) ~ ., data = data_full)
sink(file = 'Results/home_all_vars_topic_30_full.txt')
summary(coxph_topic_30_full)
sink(file=NULL)

###############################################################################
##################### 30 TOPICS BACKWARD SELECTION ############################
###############################################################################
# Run full model with 30 topics only (remove P-value>0.3):
data_sig <- select(data, -c(topic_18, topic_26, topic_10))
coxph_topic_30_sig_1 <- coxph(Surv(time,event) ~ ., data = data_sig)
sink(file = 'Results/home_topic_30_sig_1.txt')
summary(coxph_topic_30_sig_1)
sink(file=NULL)

# Run full model with 30 topics only (remove P-value>0.2):
data_sig$topic_19 <- NULL
coxph_topic_30_sig_2 <- coxph(Surv(time,event) ~ ., data = data_sig)
sink(file = 'Results/home_topic_30_sig_2.txt')
summary(coxph_topic_30_sig_2)
sink(file=NULL)

# Run full model with 30 topics only (remove P-value>0.2):
data_sig$topic_8 <- NULL
coxph_topic_30_sig_3 <- coxph(Surv(time,event) ~ ., data = data_sig)
sink(file = 'Results/home_topic_30_sig_3.txt')
summary(coxph_topic_30_sig_3)
sink(file=NULL)

# Run full model with 30 topics only (remove P-value>0.2):
data_sig$topic_27 <- NULL
coxph_topic_30_sig_4 <- coxph(Surv(time,event) ~ ., data = data_sig)
sink(file = 'Results/home_topic_30_sig_4.txt')
summary(coxph_topic_30_sig_4)
sink(file=NULL)

# Run full model with 30 topics only (remove P-value>0.2):
data_sig <- select(data_sig, -c(topic_6, topic_28))
coxph_topic_30_sig_5 <- coxph(Surv(time,event) ~ ., data = data_sig)
sink(file = 'Results/home_topic_30_sig_5.txt')
summary(coxph_topic_30_sig_5)
sink(file=NULL)

# Run full model with 30 topics only (remove P-value>0.2):
data_sig <- select(data_sig, -c(topic_25, topic_29, topic_13))
coxph_topic_30_sig_6 <- coxph(Surv(time,event) ~ ., data = data_sig)
sink(file = 'Results/home_topic_30_sig_6.txt')
summary(coxph_topic_30_sig_6)
sink(file=NULL)

# Run full model with 30 topics only (remove P-value>0.2):
data_sig <- select(data_sig, -c(topic_16, topic_3))
coxph_topic_30_sig_7 <- coxph(Surv(time,event) ~ ., data = data_sig)
sink(file = 'Results/home_topic_30_sig_7.txt')
summary(coxph_topic_30_sig_7)
sink(file=NULL)

# concordance score = 0.783 with 16 topics (largest p-value=0.144974 )

# Run full model with 30 topics only (remove P-value>0.1):
data_sig$topic_9 <- NULL

coxph_topic_30_sig_8 <- coxph(Surv(time,event) ~ ., data = data_sig)
sink(file = 'Results/home_topic_30_sig_8.txt')
summary(coxph_topic_30_sig_8)

# Run full model with 30 topics only (remove P-value>0.1):
data_sig$topic_14 <- NULL

coxph_topic_30_sig_9 <- coxph(Surv(time,event) ~ ., data = data_sig)
sink(file = 'Results/home_topic_30_sig_9.txt')
summary(coxph_topic_30_sig_9)

# concordance score = 0.782 with 14 topics (p-value<0.05 )
sink(file=NULL)

###############################################################################
################ ALL VARS + 30 TOPICS BACKWARD SELECTION ######################
###############################################################################
# Run full model with other variables too (remove p_value>0.5):
data_full <- select(data_full, -c(cevd, copd, rheumd, mld, diab, diabwc, ETHNICITY_CAT,
                                  NUM_REPORTS_CAT, heart_cat, oxygen_cat,resp_cat,
                                  diastolic_cat, systolic_cat, topic_3,
                                  topic_8, topic_18, topic_25, topic_26, topic_27))

coxph_topic_30_full_1 <- coxph(Surv(time,event) ~ ., data = data_full)
sink(file = 'Results/home_all_vars_topic_30_full_1.txt')
summary(coxph_topic_30_full_1)
sink(file=NULL)

# Run full model with other variables too (remove p_value>0.4):
data_full <- select(data_full, -c(LAST_ICU,ami,aids))

coxph_topic_30_full_2 <- coxph(Surv(time,event) ~ ., data = data_full)
sink(file = 'Results/home_all_vars_topic_30_full_2.txt')
summary(coxph_topic_30_full_2)
sink(file=NULL)

# Run full model with other variables too (remove p_value>0.4):
data_full$pud <- NULL 

coxph_topic_30_full_3 <- coxph(Surv(time,event) ~ ., data = data_full)
sink(file = 'Results/home_all_vars_topic_30_full_3.txt')
summary(coxph_topic_30_full_3)
sink(file=NULL)

# Run full model with other variables too (remove p_value>0.3):
data_full$topic_10 <- NULL 

coxph_topic_30_full_4 <- coxph(Surv(time,event) ~ ., data = data_full)
sink(file = 'Results/home_all_vars_topic_30_full_4.txt')
summary(coxph_topic_30_full_4)
sink(file=NULL)

# Run full model with other variables too (remove p_value>0.3):
data_full$topic_9 <- NULL 
data_full$topic_13 <- NULL 

coxph_topic_30_full_5 <- coxph(Surv(time,event) ~ ., data = data_full)
sink(file = 'Results/home_all_vars_topic_30_full_5.txt')
summary(coxph_topic_30_full_5)
sink(file=NULL)

# Run full model with other variables too (remove p_value>0.3):
data_full <- select(data_full, -c(topic_16, topic_19, topic_20))

coxph_topic_30_full_6 <- coxph(Surv(time,event) ~ ., data = data_full)
sink(file = 'Results/home_all_vars_topic_30_full_6.txt')
summary(coxph_topic_30_full_6)
sink(file=NULL)

# Run full model with other variables too (remove p_value>0.3):
data_full <- select(data_full, -c(msld, topic_1, topic_17, topic_21, topic_28))

coxph_topic_30_full_7 <- coxph(Surv(time,event) ~ ., data = data_full)
sink(file = 'Results/home_all_vars_topic_30_full_7.txt')
summary(coxph_topic_30_full_7)
sink(file=NULL)

# Run full model with other variables too (remove p_value>0.1):
data_full <- select(data_full, -c(dementia, topic_4, topic_23, topic_24))

coxph_topic_30_full_8 <- coxph(Surv(time,event) ~ ., data = data_full)
sink(file = 'Results/home_all_vars_topic_30_full_8.txt')
summary(coxph_topic_30_full_8)
sink(file=NULL)

# Run full model with other variables too (remove p_value>0.1):
data_full <- select(data_full, -c(hp, topic_6))

coxph_topic_30_full_9 <- coxph(Surv(time,event) ~ ., data = data_full)
sink(file = 'Results/home_all_vars_topic_30_full_9.txt')
summary(coxph_topic_30_full_9)
sink(file=NULL)

# Run full model with other variables too (remove p_value>0.1):
data_full$topic_7 <- NULL

coxph_topic_30_full_10 <- coxph(Surv(time,event) ~ ., data = data_full)
sink(file = 'Results/home_all_vars_topic_30_full_10.txt')
summary(coxph_topic_30_full_10)
sink(file=NULL)

# Run full model with other variables too (remove p_value>0.1):
data_full$topic_14 <- NULL

coxph_topic_30_full_11 <- coxph(Surv(time,event) ~ ., data = data_full)
sink(file = 'Results/home_all_vars_topic_30_full_11.txt')
summary(coxph_topic_30_full_11)
sink(file=NULL)

# Concordance score = 0.842 with all P_value < 0.1

###############################################################################
################ DEMOGRAPHIC + 30 TOPICS BACKWARD SELECTION ###################
###############################################################################
# Run full model:
topic_vars <- paste0("topic_", seq(1:29))
data_demo <- select(d_topic, c(GENDER,MARITAL_STATUS_CAT,ETHNICITY_CAT,AGE_CAT,
                               topic_vars))

coxph_topic_30_demo_1 <- coxph(Surv(time,event) ~ ., data = data_demo)
sink(file = 'Results/coxph_topic_30_demo_1.txt')
summary(coxph_topic_30_demo_1)
sink(file=NULL)

# Remove p-value > 0.5:
data_demo <- select(data_demo, -c(ETHNICITY_CAT,topic_18))

coxph_topic_30_demo_2 <- coxph(Surv(time,event) ~ ., data = data_demo)
sink(file = 'Results/coxph_topic_30_demo_2.txt')
summary(coxph_topic_30_demo_2)
sink(file=NULL)

# Remove p-value > 0.4:
data_demo$topic_10 <- NULL

coxph_topic_30_demo_3 <- coxph(Surv(time,event) ~ ., data = data_demo)
sink(file = 'Results/coxph_topic_30_demo_3.txt')
summary(coxph_topic_30_demo_3)
sink(file=NULL)

# Remove p-value > 0.4:
data_demo$topic_28 <- NULL

coxph_topic_30_demo_4 <- coxph(Surv(time,event) ~ ., data = data_demo)
sink(file = 'Results/coxph_topic_30_demo_4.txt')
summary(coxph_topic_30_demo_4)
sink(file=NULL)

# Remove p-value > 0.4:
data_demo$topic_8 <- NULL
data_demo$topic_27 <- NULL

coxph_topic_30_demo_5 <- coxph(Surv(time,event) ~ ., data = data_demo)
sink(file = 'Results/coxph_topic_30_demo_5.txt')
summary(coxph_topic_30_demo_5)

# Remove p-value > 0.3:
data_demo$topic_13 <- NULL
data_demo$topic_19 <- NULL

coxph_topic_30_demo_6 <- coxph(Surv(time,event) ~ ., data = data_demo)
sink(file = 'Results/coxph_topic_30_demo_6.txt')
summary(coxph_topic_30_demo_6)

# Remove p-value > 0.3:
data_demo$topic_25 <- NULL

coxph_topic_30_demo_7 <- coxph(Surv(time,event) ~ ., data = data_demo)
sink(file = 'Results/coxph_topic_30_demo_7.txt')
summary(coxph_topic_30_demo_7)
sink(file=NULL)

# Remove p-value > 0.3:
data_demo$topic_3 <- NULL

coxph_topic_30_demo_8 <- coxph(Surv(time,event) ~ ., data = data_demo)
sink(file = 'Results/coxph_topic_30_demo_8.txt')
summary(coxph_topic_30_demo_8)
sink(file=NULL)

# Remove p-value > 0.15:
data_demo$topic_16 <- NULL
data_demo$topic_21 <- NULL
data_demo$topic_29 <- NULL

coxph_topic_30_demo_9 <- coxph(Surv(time,event) ~ ., data = data_demo)
sink(file = 'Results/coxph_topic_30_demo_9.txt')
summary(coxph_topic_30_demo_9)
sink(file=NULL)

# Remove p-value > 0.1:
data_demo$topic_9 <- NULL

coxph_topic_30_demo_10 <- coxph(Surv(time,event) ~ ., data = data_demo)
sink(file = 'Results/coxph_topic_30_demo_10.txt')
summary(coxph_topic_30_demo_10)
sink(file=NULL)

# Remove p-value > 0.1:
data_demo$topic_6 <- NULL

coxph_topic_30_demo_11 <- coxph(Surv(time,event) ~ ., data = data_demo)
sink(file = 'Results/coxph_topic_30_demo_11.txt')
summary(coxph_topic_30_demo_11)
sink(file=NULL)

# Remove p-value > 0.08:
data_demo$topic_24 <- NULL

coxph_topic_30_demo_12 <- coxph(Surv(time,event) ~ ., data = data_demo)
sink(file = 'Results/coxph_topic_30_demo_12.txt')
summary(coxph_topic_30_demo_12)
sink(file=NULL)

# Remove p-value > 0.08:
data_demo$topic_14 <- NULL

coxph_topic_30_demo_13 <- coxph(Surv(time,event) ~ ., data = data_demo)
sink(file = 'Results/coxph_topic_30_demo_13.txt')
summary(coxph_topic_30_demo_13)
sink(file=NULL)

