## ----setup, echo=FALSE-----------------------------------------------------------------
setwd("C:/Users/Hanna/Desktop/LU/Chapman/Research_2/Survival-and-Topic-Modeling")

list.of.packages <- c("dplyr","stringr", "reshape2", "comorbidity","ggplot2",
                      "visdat", "naniar","survival","survminer","grid","tiff","pROC")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(new.packages)

## ----message=FALSE, warning=FALSE------------------------------------------------------

library(dplyr)
library(stringr)
library(reshape2)
library(comorbidity)
library(ggplot2)
library(visdat)
library(naniar)
library(survival)
library(survminer)
library(grid)
library(tiff)


# For XGboost
library(caret)
library(xgboost)
library(tidymodels) # for xgboost model
library(doParallel)
library(pROC)


## --------------------------------------------------------------------------------------
# Removed Number of Reports

## --------------------------------------------------------------------------------------
filename <- gzfile('Data/ADMISSIONS.csv.gz','rt')
admissions <- read.csv(filename)

admissions <- filter(admissions,DISCHARGE_LOCATION =='HOME')



## ----warning=FALSE---------------------------------------------------------------------
filename <- gzfile('Data/ICUSTAYS.csv.gz','rt')
icu <- read.csv(filename)


# Create two columns for the last ICU unit and total Length of Stay:
icu_last <- icu %>%
  select(HADM_ID, LAST_CAREUNIT, LOS) %>%
  group_by(HADM_ID) %>%
  mutate(LAST_ICU = last(LAST_CAREUNIT), LOS_cum = sum(LOS)) %>%
  filter(LOS_cum >= 4/24) %>% # Remove ICU LOS less than 4 hours
  select(HADM_ID, LAST_ICU, LOS_cum) %>%
  distinct()

# Merge ICU Unit data with admission data (inner join):
adm_icu <- merge(admissions, icu_last, by = 'HADM_ID')


rm(admissions)
rm(icu)

## --------------------------------------------------------------------------------------
filename <- gzfile('Data/PATIENTS.csv.gz','rt')
patients <- read.csv(filename)

# Merge patients and adm_icu:
df_patient_icu <- merge(adm_icu, patients, by='SUBJECT_ID', all.x = T)

rm(patients)

## ----warning=FALSE, message=FALSE------------------------------------------------------
filename <- gzfile('Data/DIAGNOSES_ICD.csv.gz','rt')
diagnoses <- read.csv(filename)

diagnoses <- diagnoses[,c("HADM_ID","ICD9_CODE")]


# Prepare for calculating comorbidity
df_com <- merge(df_patient_icu, diagnoses, by = 'HADM_ID', all.x = T)

rm(diagnoses)

## ----warning=FALSE, message=FALSE------------------------------------------------------
# Charlson comorbidity scores based on ICD-10 diagnostic codes:
df_comorbidity <- comorbidity(x = df_com, id = "HADM_ID", code = "ICD9_CODE", 
                              score = "charlson", assign0 = FALSE, icd = 'icd9')

# Remove the last four items from the comorbidity output
df_comorbidity <- df_comorbidity[,!names(df_comorbidity) %in%
                                   c("score", "index","wscore","windex")]

# Remove the icd code column from original data frame:
com_no_icd <- df_com[,names(df_com) != 'ICD9_CODE']

# Remove duplicates from original data frame:
unique_com <- unique(com_no_icd)

# Merge the main dataset and the comorbidity:
com_all <- merge(unique_com, df_comorbidity, by="HADM_ID")

# Remove "DIAGNOSIS":
com_all <- select(com_all, -c(ROW_ID.x,DIAGNOSIS,ROW_ID.y))



################################################################################
# Convert DOD (Date of Death) to date format:
com_all$DOD <- as.Date(com_all$DOD, format = '%Y-%m-%d')

df_all <- com_all %>%
  # If no DOD recorded, assign 300 days to Duration, otherwise assign the duration
  # between discharge and DOD
  mutate(Duration = ifelse(is.na(DOD), 300, 
                           (DOD - as.Date(DISCHTIME, format = '%Y-%m-%d %H:%M:%S')))) %>%
  # Create EVENT (death within 90 days of discharge as 1, otherwise 0)
  mutate(EVENT = ifelse(Duration <= 90, 1, 0)) %>%
  # Remove patients who died before discharge or within one day of discharge
  # Remove newborns
  # Remove patients who died in hospital
  filter(Duration >= 1 & ADMISSION_TYPE!='NEWBORN' & HOSPITAL_EXPIRE_FLAG == 0)



## --------------------------------------------------------------------------------------
df_all <- df_all %>%
  # Create a column for hospital length of stay
  mutate(HOSPITAL_LOS = (as.Date(DISCHTIME, format = '%Y-%m-%d %H:%M:%S') - 
                           as.Date(ADMITTIME, format = '%Y-%m-%d %H:%M:%S'))) %>%
  # Remove those whose hospital LOS is less than 0 (organ doners have zero or less LOS)
  filter(HOSPITAL_LOS > 0) %>%
  # Create Age at Discharge column from DOB and Discharge time columns
  mutate(age = (as.Date(DISCHTIME, format = '%Y-%m-%d %H:%M:%S') -
                  as.Date(DOB, format = '%Y-%m-%d %H:%M:%S'))/365) %>%
  # Remove those who are under the age of 18:
  filter(age >= 18) 


# If age is greater than 100, code it to 90
# In the MIMIC database, patients older than 89 were coded as 300 years old
df_all$age <- ifelse(df_all$age > 300, df_all$age - 211, df_all$age)



################################################################################
## --------------------------------------------------------------------------------------

# Regroup Marital Status:
df_all <- df_all %>% 
  mutate(MARITAL_STATUS_CAT = case_when(
    MARITAL_STATUS == 'SINGLE'                       ~ 'SINGLE',
    MARITAL_STATUS == 'WIDOWED'                      ~ 'WIDOWED',
    MARITAL_STATUS %in% c('MARRIED', 'LIFE PARTNER') ~ 'MARRIED',
    MARITAL_STATUS %in% c('DIVORCED','SEPARATED')    ~ 'DIVORCED',
    TRUE                                             ~ 'UNKNOWN'))

df_all$MARITAL_STATUS_CAT <- factor(df_all$MARITAL_STATUS_CAT,
                                    levels = c("SINGLE", "MARRIED","DIVORCED",
                                               "WIDOWED", "UNKNOWN"))

# Regroup Ethnicity:
df_all <- df_all %>%
  mutate(ETHNICITY_CAT = case_when(
    str_detect(ETHNICITY, 'WHITE')                               ~ 'WHITE',
    str_detect(ETHNICITY, 'BLACK')                               ~ 'BLACK',
    str_detect(ETHNICITY, 'HISPANIC')                            ~ 'HISPANIC',
    str_detect(ETHNICITY, 'ASIAN')                               ~ 'ASIAN',
    ETHNICITY %in% c('UNKNOWN/NOT SPECIFIED','UNABLE TO OBTAIN',
                     'PATIENT DECLINED TO ANSWER')               ~ 'UNKNOWN',
    TRUE                                                         ~ 'OTHER'))

df_all$ETHNICITY_CAT <- factor(df_all$ETHNICITY_CAT, 
                               levels = c("WHITE","BLACK","HISPANIC","ASIAN",
                                          "OTHER","UNKNOWN"))

########################################################################

# Regroup Insurance:
df_all$INSURANCE_CAT <- ifelse(df_all$INSURANCE %in% c("Government", "Medicaid", "Medicare"),
                               "Governmental", "Non-Governmental")


df_all$INSURANCE_CAT <- factor(df_all$INSURANCE_CAT, 
                               levels = c("Non-Governmental", "Governmental"))

# Regroup addmission type:
df_all$ADMISSION_CAT <- ifelse(df_all$ADMISSION_TYPE == "EMERGENCY", "Emergency", "Non-Emergency")


df_all$ADMISSION_CAT <- factor(df_all$ADMISSION_CAT, 
                               levels = c("Non-Emergency","Emergency"))


#############################################################################
############################# ADD VENTILATION DATA ##########################
#############################################################################
# Ventilation data
filename <- gzfile('Data/CPTEVENTS.csv.gz','rt')
vent <- read.csv(filename)

# Code invasive ventilation as 1, and otherwise 0
vent$VENT <- as.numeric(str_detect(vent$DESCRIPTION,"INVASIVE"))
vent <- vent %>%
  group_by(HADM_ID) %>%
  summarise(ventilation = max(VENT)) %>%
  select(c(HADM_ID, ventilation)) %>%
  distinct()

df_all_2 <- merge(df_all, vent, by = "HADM_ID", all.x = T)
df_all_2$ventilation[is.na(df_all_2$ventilation)] = 0
## --------------------------------------------------------------------------------------


# Select columns that will be used in the model:
all <- df_all_2 %>%
  select("HADM_ID","LAST_ICU", "GENDER", "INSURANCE_CAT","ETHNICITY_CAT", "ADMISSION_CAT",
         "Duration", "EVENT","MARITAL_STATUS_CAT", "age", "ventilation", "HOSPITAL_LOS",
         "ami","chf","pvd","cevd","dementia","copd","rheumd","pud","mld",          
         "diab","diabwc" ,"hp", "rend","canc","msld","metacanc","aids")

# write.csv(all, "Intermediate Datasets/all_Feb26.csv")
save(all, file = "Intermediate Datasets/before_vital_May2.RData")

################################################################################
############################ VITAL SIGNS #######################################
################################################################################
# TEMERATURE
temperature <- read.csv("Data/Vital_Sign_Data/temperatureFixed.csv")
temperature <- select(temperature, c("HADM_ID","TEMPERATURE_LAST"))
all_vital <- merge(all, temperature, by="HADM_ID", all.x =TRUE)


# HEART RATE
heart <- read.csv("Data/Vital_Sign_Data/heartRateFixed.csv")
heart <- select(heart, c("HADM_ID","HeartRate_LAST"))
all_vital <- merge(all_vital, heart, by="HADM_ID", all.x =TRUE)


# OXYGEN SATURATION
oxygen <- read.csv("Data/Vital_Sign_Data/spo2Fixed.csv")
oxygen <- select(oxygen, c("HADM_ID","SpO2_LAST"))
all_vital <- merge(all_vital, oxygen, by="HADM_ID", all.x =TRUE)


# RESPIROTORY RATE
resp <- read.csv("Data/Vital_Sign_Data/respRateFixed.csv")
resp <- select(resp, c("HADM_ID","RespRate_LAST"))
all_vital <- merge(all_vital, resp, by="HADM_ID", all.x =TRUE)


# DIASTOLIC BLOOD PRESSURE
diastolic <- read.csv("Data/Vital_Sign_Data/diastolicBPfixed.csv")
diastolic <- select(diastolic, c("HADM_ID","DIASTOLIC_BP_LAST"))
all_vital <- merge(all_vital, diastolic, by="HADM_ID", all.x =TRUE)


# SYSTOLIC BLOOD PRESSURE
systolic <- read.csv("Data/Vital_Sign_Data/systolicBPfixed.csv")
systolic <- select(systolic, c("HADM_ID","SYSTOLIC_BP_LAST"))
all_vital <- merge(all_vital, systolic, by="HADM_ID", all.x =TRUE)


names(all_vital)[30:35] = c("temperature","heart_rate","oxygen","resp_rate",   
                            "diastolic_bp", "systolic_bp" )

save(all_vital, file = "Intermediate Datasets/all_vital_May2.RData")
# load("Intermediate Datasets/all_vital_May2.RData")

# Group vital sign data into categories
all_vital_clean <- all_vital %>% 
  mutate(temperature_cat = case_when(
    is.na(temperature) == TRUE   ~ 'Missing',
    temperature <= 0             ~ 'Missing',
    temperature < 36.5           ~ 'Low',
    temperature <= 37.5          ~ 'Normal',
    TRUE                              ~ 'High'),
    heart_cat = case_when(
      is.na(heart_rate) == TRUE   ~ 'Missing',
      heart_rate <= 0             ~ 'Missing',
      heart_rate < 60             ~ 'Low',
      heart_rate <= 100           ~ 'Normal',
      heart_rate <= 300           ~ 'High',
      TRUE                        ~ 'Missing'),
    oxygen_cat = case_when(
      is.na(oxygen) == TRUE   ~ 'Missing',
      oxygen <= 0             ~ 'Missing',
      oxygen < 90             ~ 'VeryLow',
      oxygen <= 94            ~ 'Low',
      oxygen <= 100           ~ 'Normal',
      TRUE                    ~ 'Missing'),
    resp_cat = case_when(
      is.na(resp_rate) == TRUE   ~ 'Missing',
      resp_rate <= 0             ~ 'Missing',
      resp_rate < 12             ~ 'Low',
      resp_rate <= 25            ~ 'Normal',
      TRUE                       ~ 'High'),
    diastolic_cat = case_when(
      is.na(diastolic_bp) == TRUE   ~ 'Missing',
      diastolic_bp <= 0             ~ 'Missing',
      diastolic_bp < 60             ~ 'Low',
      diastolic_bp <= 80            ~ 'Normal',
      TRUE                          ~ 'High'),
    systolic_cat = case_when(
      is.na(systolic_bp) == TRUE   ~ 'Missing',
      systolic_bp <= 0             ~ 'Missing',
      systolic_bp < 90             ~ 'Low',
      systolic_bp <= 120           ~ 'Normal',
      TRUE                         ~ 'High')
  )


#######################################################################
# Reorder the levels of the new categorical variables:
all_vital_clean <- within(all_vital_clean, {
  temperature_cat = factor(temperature_cat, levels = c("Normal", "Low", "High", "Missing"))
  heart_cat = factor(heart_cat, levels = c("Normal", "Low", "High", "Missing"))
  oxygen_cat = factor(oxygen_cat, levels = c("Normal", "VeryLow", "Low", "Missing"))
  resp_cat = factor(resp_cat, levels = c("Normal", "Low", "High", "Missing"))
  diastolic_cat = factor(diastolic_cat, levels = c("Normal", "Low", "High", "Missing"))
  systolic_cat = factor(systolic_cat, levels = c("Normal", "Low", "High", "Missing"))
  temperature = NULL
  heart_rate = NULL
  oxygen = NULL
  resp_rate = NULL
  diastolic_bp = NULL
  systolic_bp = NULL
  ADMISSION_CAT = factor(ADMISSION_CAT)
  LAST_ICU = factor(LAST_ICU, levels = c("MICU", "CCU", "CSRU", "SICU", "TSICU"))
  GENDER = factor(GENDER)
  ventilation = factor(ventilation)
  ami = factor(ami)
  chf = factor(chf)
  pvd = factor(pvd)
  cevd = factor(cevd)
  dementia = factor(dementia)
  copd = factor(copd)
  rheumd = factor(rheumd)
  pud = factor(pud)
  mld = factor(mld)
  diab = factor(diab)
  diabwc = factor(diabwc)
  hp = factor(hp)
  rend = factor(rend)
  canc = factor(canc)
  msld = factor(msld)
  metacanc = factor(metacanc)
  aids = factor(aids)
})

 

################################################################################
############################ ADDED SURGERY INFO ################################
################################################################################

# Load Surgery data from services table:
filename <- gzfile('Data/SERVICES.csv.gz','rt')
services <- read.csv(filename)


# Reshape service dataframe into wide form
surgeries <- services %>%
  select(c(HADM_ID, CURR_SERVICE)) %>%
  dcast(HADM_ID~CURR_SERVICE) %>%
  mutate(num_surg = CSURG+NSURG+ORTHO+SURG+TRAUM+TSURG+VSURG) %>%
  select(c(HADM_ID, num_surg))


# Combine the surgery data with the cleaned data with vital signs
all_surg <- left_join(all_vital_clean, surgeries, by = "HADM_ID")
all_surg[is.na(all_surg)] <- 0 # assign 0 to missing values (only 1 row for surg data)




all_surg <- all_surg %>%
  # Bin Age according to MeSH:
  mutate(
    age_cat = cut(round(age), breaks=c(-Inf,40,55,70, Inf),
                  labels = c("<=40","41-55", "56-70", ">70")),
    # Bin Hospital Length of Stay:
    hospital_los_cat = cut(as.numeric(HOSPITAL_LOS), breaks=c(-Inf,3,7,Inf),
                           labels = c("<=3","4-7",">7")),
    had_surgery = factor(ifelse(num_surg==0, 0, 1))
  )

all_surg <- select(all_surg, -c(age, num_surg, HOSPITAL_LOS))
names(all_surg) <- tolower(names(all_surg))

save(all_surg, file = "Intermediate Datasets/all_structured_May2.RData")

# Next: Run Code for Survival Analysis models 