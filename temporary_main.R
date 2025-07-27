#------------------------INSTALL AND LOAD THE PACKAGES--------------------------
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("gtsummary")
install.packages("lubridate")
install.packages("mice")
install.packages("lattice")
install.packages("corrplot")
install.packages("caret")
install.packages("parallel")
install.packages("doParallel")
install.packages("recipes")
install.packages("data.table")
install.packages("mltools")
install.packages("glmnet")
install.packages("xgboost")
install.packages("DiagrammeR")
install.packages("car")
install.packages("randomForest")
install.packages("forcats")
install.packages("tweedie")
install.packages("statmod")
install.packages("yardstick")
install.packages("Metrics")
library(Metrics)
library(yardstick)
library(statmod)
library(tweedie)
library(forcats)
library(randomForest)
library(car)
library(DiagrammeR)
library(xgboost)
library(glmnet)
library(data.table)
library(mltools)
library(recipes)
library(doParallel)
library(parallel)
library(caret)
library(corrplot)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gtsummary)
library(lubridate)
library(mice)
library(lattice)

#-----------------------------READ IN THE DATASETS------------------------------

#Read in the lung cancer MIMIC-IV ICU dataset
icustays <- read.csv("icustays_lungcancer.csv") %>%
  arrange(hadm_id, intime) %>%
  group_by(hadm_id, icd_code) %>%
  mutate(time_since_discharge = 
           as.numeric(difftime(intime, lag(outtime), units = "hours")),
         episode = 
           cumsum(if_else(is.na(time_since_discharge)| time_since_discharge > 24,
                          1, 0)),
         #time_since_discharge = if_else(is.na(time_since_discharge, 0,
                                              #time_since_discharge))
         ) %>%
  ungroup() #%>%
  #group_by(hadm_id, icd_code, episode, stay_id) %>%
  #mutate(discharge_time = if_else(time_since_discharge < 24, time_since_discharge, 0),
         #discharge_time_total = sum(discharge_time)) %>%
#ungroup()
 
icustays2 <- icustays %>%
  select(hadm_id, subject_id, icd_code) %>%
  unique()

earliest_stay_ids <- icustays %>%
  group_by(hadm_id, episode) %>%
  arrange(intime) %>%
  slice(1) %>%
  select(hadm_id, episode, stay_id_earliest = stay_id)
  
icu_episodes <- icustays %>%
  left_join(earliest_stay_ids, by = c("hadm_id", "episode")) %>%
  mutate(stay_id = stay_id_earliest) %>%
  select(-stay_id_earliest) %>%
  group_by(hadm_id, stay_id, episode) %>%
  summarise(
    episode_intime = min(intime),
    episode_outtime = max(outtime),
    total_los = sum(los, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  inner_join(icustays2, by = "hadm_id")


time_to_icu <- 24

#Read in the datasets for the covariates
temperature <- read.csv("temperature_icu_data.csv") %>%
  full_join(read.csv("temperature_icu.csv"), by = c("hadm_id", "stay_id")) %>%
  mutate(charttime.x = if_else(is.na(charttime.x), ymd_hms("2001-01-01 00:00:00"),
                               ymd_hms(charttime.x)),
         charttime.y = if_else(is.na(charttime.y), ymd_hms("2001-01-01 00:00:00"),
                               ymd_hms(charttime.y)),
         charttime = if_else(charttime.x < charttime.y, charttime.y, charttime.x),
         intime = if_else(charttime == charttime.y, intime.y, intime.x)) %>%
  mutate(temperature.y = if_else(is.na(temperature.y) | temperature.y <= 47,
                                 temperature.y, round((temperature.y-32)*(5/9), 1)),
         temperature = if_else(charttime.x < charttime.y, temperature.y,
                               temperature.x)) %>%
  mutate(timediff = as.numeric(ymd_hms(intime) - charttime, 'hours')) %>%
  select(c(hadm_id, stay_id, temperature, timediff)) %>%
  filter(temperature >= 25 & temperature < 47) %>%
  filter(timediff <= time_to_icu)

platelets <- read.csv("platelets_icu_data.csv") %>%
  full_join(read.csv("platelets_icu.csv"), by = c("hadm_id", "stay_id")) %>%
  mutate(charttime.x = if_else(is.na(charttime.x), ymd_hms("2001-01-01 00:00:00"),
                               ymd_hms(charttime.x)),
         charttime.y = if_else(is.na(charttime.y), ymd_hms("2001-01-01 00:00:00"),
                               ymd_hms(charttime.y)),
         charttime = if_else(charttime.x < charttime.y, charttime.y, charttime.x),
         intime = if_else(charttime == charttime.y, intime.y, intime.x)) %>%
  mutate(platelets = if_else(charttime.x < charttime.y, platelets.y, platelets.x)) %>%
  mutate(timediff = as.numeric(ymd_hms(intime) - charttime, 'hours')) %>%
  select(c(hadm_id, stay_id, platelets, timediff)) %>%
  filter(platelets < 1100) %>%
  filter(timediff <= time_to_icu)

adm_type <- read.csv("admission_type.csv")

glucose <- read.csv("glucose_icu_data.csv") %>%
  full_join(read.csv("glucose_icu.csv"), by = c("hadm_id", "stay_id")) %>%
  mutate(charttime.x = if_else(is.na(charttime.x), ymd_hms("2001-01-01 00:00:00"),
                               ymd_hms(charttime.x)),
         charttime.y = if_else(is.na(charttime.y), ymd_hms("2001-01-01 00:00:00"),
                               ymd_hms(charttime.y)),
         charttime = if_else(charttime.x < charttime.y, charttime.y, charttime.x),
         intime = if_else(charttime == charttime.y, intime.y, intime.x)) %>%
  mutate(glucose = if_else(charttime.x < charttime.y, glucose.y, glucose.x)) %>%
  mutate(timediff = as.numeric(ymd_hms(intime) - charttime, 'hours')) %>%
  select(c(hadm_id, stay_id, glucose, timediff)) %>%
  filter(timediff <= time_to_icu)
  
chloride <- read.csv("chloride_icu_data.csv") %>%
  full_join(read.csv("chloride_icu.csv"), by = c("hadm_id", "stay_id")) %>%
  mutate(charttime.x = if_else(is.na(charttime.x), ymd_hms("2001-01-01 00:00:00"),
                               ymd_hms(charttime.x)),
         charttime.y = if_else(is.na(charttime.y), ymd_hms("2001-01-01 00:00:00"),
                               ymd_hms(charttime.y)),
         charttime = if_else(charttime.x < charttime.y, charttime.y, charttime.x),
         intime = if_else(charttime == charttime.y, intime.y, intime.x)) %>%
  mutate(chloride = if_else(charttime.x < charttime.y, chloride.y, chloride.x)) %>%
  mutate(timediff = as.numeric(ymd_hms(intime) - charttime, 'hours')) %>%
  select(c(hadm_id, stay_id, chloride, timediff)) %>%
  filter(timediff <= time_to_icu)

potassium <- read.csv("potassium_icu_data.csv") %>%
  full_join(read.csv("potassium_icu.csv"), by = c("hadm_id", "stay_id")) %>%
  mutate(charttime.x = if_else(is.na(charttime.x), ymd_hms("2001-01-01 00:00:00"),
                               ymd_hms(charttime.x)),
         charttime.y = if_else(is.na(charttime.y), ymd_hms("2001-01-01 00:00:00"),
                               ymd_hms(charttime.y)),
         charttime = if_else(charttime.x < charttime.y, charttime.y, charttime.x),
         intime = if_else(charttime == charttime.y, intime.y, intime.x)) %>%
  mutate(potassium = if_else(charttime.x < charttime.y, potassium.y, potassium.x)) %>%
  mutate(timediff = as.numeric(ymd_hms(intime) - charttime, 'hours')) %>%
  select(c(hadm_id, stay_id, potassium, timediff)) %>%
  filter(timediff <= time_to_icu)

ptt <- read.csv("ptt_icu_data.csv") %>%
  full_join(read.csv("PTT_icu.csv"), by = c("hadm_id", "stay_id")) %>%
  mutate(charttime.x = if_else(is.na(charttime.x), ymd_hms("2001-01-01 00:00:00"),
                               ymd_hms(charttime.x)),
         charttime.y = if_else(is.na(charttime.y), ymd_hms("2001-01-01 00:00:00"),
                               ymd_hms(charttime.y)),
         charttime = if_else(charttime.x < charttime.y, charttime.y, charttime.x),
         intime = if_else(charttime == charttime.y, intime.y, intime.x)) %>%
  mutate(ptt = if_else(charttime.x < charttime.y, ptt.y, ptt.x)) %>%
  mutate(timediff = as.numeric(ymd_hms(intime) - charttime, 'hours')) %>%
  select(c(hadm_id, stay_id, ptt, timediff)) %>%
  filter(timediff <= time_to_icu)

haemoglobin <- read.csv("haemoglobin_icu_data.csv") %>%
  full_join(read.csv("haemoglobin_icu.csv"), by = c("hadm_id", "stay_id")) %>%
  mutate(charttime.x = if_else(is.na(charttime.x), ymd_hms("2001-01-01 00:00:00"),
                               ymd_hms(charttime.x)),
         charttime.y = if_else(is.na(charttime.y), ymd_hms("2001-01-01 00:00:00"),
                               ymd_hms(charttime.y)),
         charttime = if_else(charttime.x < charttime.y, charttime.y, charttime.x),
         intime = if_else(charttime == charttime.y, intime.y, intime.x)) %>%
  mutate(haemoglobin = if_else(charttime.x < charttime.y, haemoglobin.y, haemoglobin.x)) %>%
  mutate(timediff = as.numeric(ymd_hms(intime) - charttime, 'hours')) %>%
  select(c(hadm_id, stay_id, haemoglobin, timediff)) %>%
  filter(haemoglobin < 19) %>%
  filter(timediff <= time_to_icu)

troponin <- read.csv("troponin_icu_data.csv") %>%
  full_join(read.csv("troponin_icu.csv"), by = c("hadm_id", "stay_id")) %>%
  mutate(charttime.x = if_else(is.na(charttime.x), ymd_hms("2001-01-01 00:00:00"),
                               ymd_hms(charttime.x)),
         charttime.y = if_else(is.na(charttime.y), ymd_hms("2001-01-01 00:00:00"),
                               ymd_hms(charttime.y)),
         charttime = if_else(charttime.x < charttime.y, charttime.y, charttime.x),
         intime = if_else(charttime == charttime.y, intime.y, intime.x)) %>%
  mutate(troponin = if_else(charttime.x < charttime.y, troponin.y,
                            troponin.x)) %>%
  mutate(timediff = as.numeric(ymd_hms(intime) - charttime, 'hours')) %>%
  select(c(hadm_id, stay_id, troponin, timediff)) %>%
  filter(timediff <= time_to_icu)

haematocrit <- read.csv("haematocrit_icu_data.csv") %>%
  full_join(read.csv("haematocrit_icu.csv"), by = c("hadm_id", "stay_id")) %>%
  mutate(charttime.x = if_else(is.na(charttime.x), ymd_hms("2001-01-01 00:00:00"),
                               ymd_hms(charttime.x)),
         charttime.y = if_else(is.na(charttime.y), ymd_hms("2001-01-01 00:00:00"),
                               ymd_hms(charttime.y)),
         charttime = if_else(charttime.x < charttime.y, charttime.y, charttime.x),
         intime = if_else(charttime == charttime.y, intime.y, intime.x)) %>%
  mutate(haematocrit = if_else(charttime.x < charttime.y, haematocrit.y,
                               haematocrit.x)) %>%
  mutate(timediff = as.numeric(ymd_hms(intime) - charttime, 'hours')) %>%
  select(c(hadm_id, stay_id, haematocrit, timediff)) %>%
  filter(haematocrit < 60) %>%
  filter(timediff <= time_to_icu)

INR <- read.csv("inr_icu_data.csv") %>%
  full_join(read.csv("INR_icu.csv"), by = c("hadm_id", "stay_id")) %>%
  mutate(charttime.x = if_else(is.na(charttime.x), ymd_hms("2001-01-01 00:00:00"),
                               ymd_hms(charttime.x)),
         charttime.y = if_else(is.na(charttime.y), ymd_hms("2001-01-01 00:00:00"),
                               ymd_hms(charttime.y)),
         charttime = if_else(charttime.x < charttime.y, charttime.y, charttime.x),
         intime = if_else(charttime == charttime.y, intime.y, intime.x)) %>%
  mutate(inr = if_else(charttime.x < charttime.y, inr.y, inr.x)) %>%
  mutate(timediff = as.numeric(ymd_hms(intime) - charttime, 'hours')) %>%
  select(c(hadm_id, stay_id, inr, timediff)) %>%
  filter(timediff <= time_to_icu)

sodium <- read.csv("sodium_icu_data.csv") %>%
  full_join(read.csv("sodium_icu.csv"), by = c("hadm_id", "stay_id")) %>%
  mutate(charttime.x = if_else(is.na(charttime.x), ymd_hms("2001-01-01 00:00:00"),
                               ymd_hms(charttime.x)),
         charttime.y = if_else(is.na(charttime.y), ymd_hms("2001-01-01 00:00:00"),
                               ymd_hms(charttime.y)),
         charttime = if_else(charttime.x < charttime.y, charttime.y, charttime.x),
         intime = if_else(charttime == charttime.y, intime.y, intime.x)) %>%
  mutate(sodium = if_else(charttime.x < charttime.y, sodium.y, sodium.x)) %>%
  mutate(timediff = as.numeric(ymd_hms(intime) - charttime, 'hours')) %>%
  select(c(hadm_id, stay_id, sodium, timediff)) %>%
  filter(timediff <= time_to_icu)

demographics <- read.csv("demographics_icu_data.csv")

drugs <- read.csv("drug_icu_data.csv") %>%
  mutate(ondansetron = if_else(drug %in% c("Ondansetron", "Ondansetron ODT"),
                               1, 0),
         calcium_gluconate = if_else(drug %in% c("Calcium Gluconate",
                                               "Calcium Gluconate (Premix)",
                                               "Calcium Gluconate Replacement
                                               (Oncology)",
                                               "Calcium Gluconate sliding scale
                                               (Critical Care-Ionized calcium)"),
                                     1, 0)) %>%
  select(-c(drug, stoptime, starttime)) %>%
  unique()

tab <- table(drugs$hadm_id)
ond_calcgluc <- names(tab[tab != 1])

drugs <- drugs %>%
  mutate(ondansetron = if_else(hadm_id %in% ond_calcgluc, 1, ondansetron),
         calcium_gluconate = if_else(hadm_id %in% ond_calcgluc, 1,
                                     calcium_gluconate)) %>%
  unique()


#--------------------------------JOIN THE DATASETS------------------------------
main <- icu_episodes %>%
  left_join(temperature, by = c("hadm_id", "stay_id")) %>%
  left_join(platelets, by = c("hadm_id", "stay_id")) %>%
  left_join(adm_type, by = c("hadm_id", "stay_id")) %>%
  left_join(glucose, by = c("hadm_id", "stay_id")) %>%
  left_join(chloride, by = c("hadm_id", "stay_id")) %>%
  left_join(potassium, by = c("hadm_id", "stay_id")) %>%
  left_join(ptt, by = c("hadm_id", "stay_id")) %>%
  left_join(haemoglobin, by = c("hadm_id", "stay_id")) %>%
  left_join(troponin, by = c("hadm_id", "stay_id")) %>%
  left_join(haematocrit, by = c("hadm_id", "stay_id")) %>%
  left_join(INR, by = c("hadm_id", "stay_id")) %>%
  left_join(sodium, by = c("hadm_id", "stay_id")) %>%
  left_join(demographics, by = c("hadm_id", "stay_id")) %>%
  left_join(drugs, by = c("hadm_id", "stay_id")) %>%
  select(c("subject_id.x", "hadm_id", "stay_id", "episode", "icd_code", 
           "age", "gender", "race", "insurance", "temperature", "platelets",
           "glucose", "chloride", "potassium", "ptt", "haemoglobin", "troponin",
           "haematocrit", "inr", "sodium", "ondansetron", "calcium_gluconate",
           "admission_type", "hospital_expire_flag", "total_los")) %>%
  rename(subject_id = subject_id.x,
         sex = gender) %>%
  unique() %>%
  mutate(ondansetron = if_else(is.na(ondansetron), 0, ondansetron),
         calcium_gluconate = if_else(is.na(calcium_gluconate), 0,
                                     calcium_gluconate),
         insurance = if_else(insurance == "NULL", "Unknown", insurance),
         previous_stay = episode - 1)

#Create a dataset for the unique records
main_unique <- main %>%
  select(-c(episode,stay_id,total_los,icd_code)) %>%
  unique()

#---------------------------MISSINGNESS PROPORTION------------------------------

#Histogram for the proportion of missingness
main %>%
  select(c("temperature", "platelets", "glucose", "chloride", "potassium", "ptt",
           "haemoglobin", "troponin", "haematocrit", "inr", "sodium")) %>%
  rename(Temperature = temperature,
         Platelets = platelets,
         Glucose = glucose,
         Chloride = chloride,
         Potassium = potassium,
         PTT = ptt,
         Haemoglobin = haemoglobin,
         Troponin = troponin,
         Haematocrit = haematocrit,
         INR = inr,
         Sodium = sodium) %>%
  summarise_all(~mean(is.na(.))*100) %>%
  pivot_longer(everything(), names_to = "column", values_to = "missing") %>%
  ggplot(aes(x = reorder(column, missing), y = missing, fill = column)) +
  ylab("Missing (%)") +
  xlab("Variable") +
  geom_col(width = 0.5) +
  scale_fill_manual(values = c("Temperature"= "firebrick2",
                               "Troponin" = "firebrick2",
                               "INR" = "firebrick2" ,
                               "PTT" = "firebrick2",
                               "Chloride" = "darkolivegreen3",
                               "Glucose" = "darkolivegreen3",
                               "Haematocrit" = "darkolivegreen3",
                               "Haemoglobin" = "darkolivegreen3",
                               "Platelets" = "darkolivegreen3",
                               "Potassium" = "darkolivegreen3",
                               "Sodium" = "darkolivegreen3")) +
  theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust = 0.5)) +
  guides(fill = "none")

#---------------------------------SUMMARY TABLE---------------------------------

#Summary table for the variables
main_summary <- main %>%
  mutate(age_cat = cut(age, breaks = c(17, 40, 50, 60, 70, 80, 90, Inf), 
                        labels = c("18-40", "41-50", "51-60", "61-70", "71-80",
                                   "81-90", "91+")),
         ethnicity = case_when(
           race %in% c("PORTUGUESE", "WHITE", "WHITE - BRAZILIAN",
                       "WHITE - EASTERN EUROPEAN", "WHITE - OTHER EUROPEAN",
                       "WHITE - RUSSIAN") ~ "White",
           race %in% c("BLACK/AFRICAN", "BLACK/AFRICAN AMERICAN",
                       "BLACK/CAPE VERDEAN", "BLACK/CARIBBEAN ISLAND") ~ "Black",
           race %in% c("ASIAN", "ASIAN - ASIAN INDIAN", "ASIAN - CHINESE",
                       "ASIAN - KOREAN", "ASIAN - SOUTH EAST ASIAN") ~ "Asian",
           race %in% c("HISPANIC OR LATINO", "HISPANIC/LATINO - CENTRAL AMERICAN",
                       "HISPANIC/LATINO - CUBAN", "HISPANIC/LATINO - DOMINICAN", 
                       "HISPANIC/LATINO - MEXICAN",
                       "HISPANIC/LATINO - PUERTO RICAN",
                       "HISPANIC/LATINO - SALVADORAN") ~ "Hispanic/Latino",
           race %in% c("AMERICAN INDIAN/ALASKA NATIVE",
                       "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER", "OTHER", 
                       "SOUTH AMERICAN") ~ "Other",
           race %in% c("PATIENT DECLINED TO ANSWER", "UNABLE TO OBTAIN",
                       "UNKNOWN") ~ "Missing"),
        insurance = factor(insurance, levels = c("Medicaid", "Medicare",
                                                    "Other", "Private", "Unknown"),
                              labels = c("Medicaid", "Medicare", "Other",
                                         "Private", "Missing")),
        admission_type = case_when(
          admission_type %in% c("DIRECT EMER.", "EW EMER.") ~ "Emergency",
          admission_type %in% c("DIRECT OBSERVATION", "EU OBSERVATION",
                                "OBSERVATION ADMIT") ~ "Observation",
          admission_type %in% c("ELECTIVE", "SURGICAL SAME DAY ADMISSION") ~ 
            "Elective",
          admission_type %in% c("URGENT") ~ "Urgent")
        )

labels <- list(previous_stay ~ "Number of previous ICU stays in this admission",
               age_cat ~ "Age (years)",
               sex ~ "Sex",
               ethnicity ~ "Ethnicity",
               insurance ~ "Insurance",
               temperature ~ "Temperature (°C)",
               platelets ~ "Platelets (k/µL)",
               glucose ~ "Glucose (mg/dL)",
               chloride ~ "Chloride (mEq/L)",
               potassium ~ "Potassium (mEq/L)",
               ptt ~ "Partial Thromboplastin Time (seconds)",
               haemoglobin ~ "Haemoglobin (g/dL)",
               troponin ~ "Troponin (ng/mL)",
               haematocrit ~ "Haematocrit (%)",
               inr ~ "International Normalised Ratio",
               sodium ~ "Sodium (mEq/L)",
               ondansetron ~ "Ondansetron",
               calcium_gluconate ~ "Calcium Gluconate",
               admission_type ~ "Admission Type",
               hospital_expire_flag ~ "Death in ICU",
               total_los ~ "Length of stay (days)")

main_summary %>%
  select(c("previous_stay", "age_cat", "sex", "ethnicity", "insurance",
           "temperature", "platelets", "glucose", "chloride", "potassium", "ptt",
           "haemoglobin", "troponin", "haematocrit", "inr", "sodium", 
           "ondansetron", "calcium_gluconate", "admission_type", 
           "hospital_expire_flag", "total_los")) %>%
  gtsummary::tbl_summary(label = labels,
                         #missing_text = "Missing",
                         type = all_continuous() ~ "continuous2",
                         statistic = list(all_categorical() ~ "{n} ({p}%)",
                                          all_continuous() ~ c("{mean} ({sd})",
                                                               "{N_miss},
                                                               ({p_miss})")))

#------------------------------TIME DIFFERENCE PLOTS----------------------------

ggplot(temperature, aes(x = timediff)) +
  geom_histogram(binwidth = 1) +
  theme_minimal()

ggplot(platelets, aes(x = timediff)) +
  geom_histogram(binwidth = 1) +
  theme_minimal()

ggplot(glucose, aes(x = timediff)) +
  geom_histogram(binwidth = 1) +
  theme_minimal()

ggplot(chloride, aes(x = timediff)) +
  geom_histogram(binwidth = 1) +
  theme_minimal()

ggplot(potassium, aes(x = timediff)) +
  geom_histogram(binwidth = 1) +
  theme_minimal()

ggplot(ptt, aes(x = timediff)) +
  geom_histogram(binwidth = 1) +
  theme_minimal()

ggplot(haemoglobin, aes(x = timediff)) +
  geom_histogram(binwidth = 1) +
  theme_minimal()

ggplot(troponin, aes(x = timediff)) +
  geom_histogram(binwidth = 1) +
  theme_minimal()

ggplot(haematocrit, aes(x = timediff)) +
  geom_histogram(binwidth = 1) +
  theme_minimal()

ggplot(INR, aes(x = timediff)) +
  geom_histogram(binwidth = 1) +
  theme_minimal()

ggplot(sodium, aes(x = timediff)) +
  geom_histogram(binwidth = 1) +
  theme_minimal()

ggplot(main_summary, aes(x = age_cat)) +
  geom_bar() +
  theme_minimal()

#----------------------------MISSING DATA IMPUTATION----------------------------

main_impute <- main_summary %>%
  select("age", "sex","admission_type", "ethnicity", "insurance", "previous_stay",
         "platelets", "glucose", "chloride", "potassium", "haemoglobin",
         "haematocrit", "sodium", "ondansetron", "calcium_gluconate", 
         "hospital_expire_flag", "total_los") %>%
  mutate(previous_stay = factor(previous_stay),
         ondansetron = factor(ondansetron),
         calcium_gluconate = factor(calcium_gluconate),
         hospital_expire_flag = factor(hospital_expire_flag),
         insurance = na_if(insurance, "Missing"),
         insurance = droplevels(insurance),
         ethnicity = na_if(ethnicity, "Missing"),
         ethnicity = factor(ethnicity),
         ethnicity = droplevels(ethnicity))
  

main_impute %>% 
  select("potassium", "haematocrit", "sodium", "glucose", "haemoglobin",
         "chloride", "platelets", "insurance", "ethnicity") %>%
  
  md.pattern(rotate.names = TRUE)

main_impute$sex <- factor(main_impute$sex)
main_impute$admission_type <- factor(main_impute$admission_type)

imp <- mice(main_impute, method = "mean", m = 5, maxit = 5)

meth <- imp$method


meth["ethnicity"] <- "pmm"
meth["insurance"] <- "pmm"
meth["admission_type"] <- ""
meth["sex"] <- ""
meth["age"] <- "norm"
meth["ondansetron"] <- "logreg"
meth["calcium_gluconate"] <- "logreg"
meth["hospital_expire_flag"] <- "logreg"
meth["total_los"] <- ""
meth["previous_stay"] <- "polyreg"
meth["glucose"] <- "pmm"
meth["sodium"] <- "pmm"
meth["chloride"] <- "pmm"
meth["platelets"] <- "pmm"
meth["potassium"] <- "pmm"
meth["haemoglobin"] <- "pmm"
meth["haematocrit"] <- "pmm"

imp <- mice(main_impute, method = meth, m = 55, maxit = 20)

plot(imp)

completed_data <- complete(imp, 2)

hist(completed_data$sodium)

main_impute.cor = main_impute %>% 
  select(c("glucose","sodium","chloride", "platelets", "potassium",
           "haemoglobin", "haematocrit")) %>%
  cor(use = "complete.obs")

corrplot(main_impute.cor)

predictor_matrix <- imp$predictorMatrix

#plot imp
plot(imp)

#change predictor matrix for haematocrit and haemoglobin to 0
predictor_matrix["haematocrit","haemoglobin"] <- 0
predictor_matrix["haemoglobin","haematocrit"] <- 0

#make imp2 with new predictor matrix
imp2 <- mice(main_impute, method = meth, predictorMatrix = predictor_matrix,
            m = 55, maxit = 20)

#plot imp2
plot(imp2)

#-------------------------------PREDICTION MODELS-------------------------------

main_impute_NA <- main_impute %>%
  mutate(ethnicity = ethnicity %>% 
           addNA() %>%
           fct_explicit_na(na_level = "NA") %>%
           relevel(ref = "NA"),
         insurance = insurance %>%
           addNA() %>%
           fct_explicit_na(na_level = "NA") %>%
           relevel(ref = "NA"),
         )

main_dummy_data <- recipe(total_los ~ ., data = main_impute_NA) %>%
  step_dummy(all_nominal(), one_hot = FALSE) %>%
  prep() %>%
  bake(new_data = NULL)

imp_dummy <- mice(main_dummy_data, m = 5, maxit = 5)

imp_dummy$method

View(imp_dummy$predictorMatrix)

predictor_matrix_dummy <- imp_dummy$predictorMatrix

predictor_matrix_dummy["haematocrit","haemoglobin"] <- 0
predictor_matrix_dummy["haemoglobin","haematocrit"] <- 0

meth_dummy <- imp_dummy$method

variables <- paste(names(main_dummy_data[,-9]), collapse = " + ")

full_model <- paste(c("total_los", variables), collapse = " ~ ")

full_model_log <- paste(c("log(total_los)", variables), collapse = " ~ ")

#create cross validation folds
folds <- createFolds(main_dummy_data$total_los, 5)

#number of rounds of imputations
m <- 5

lam_ridge_rmse <- list()

lam_ridge_mae <- list()

lam_lasso_rmse <- list()

lam_lasso_mae <- list()



#add the cores
number_of_cores <- detectCores() - 1
clust <- makeCluster(number_of_cores)
registerDoParallel(clust)

clusterEvalQ(clust, library(mice))
clusterEvalQ(clust, library(dplyr))
clusterEvalQ(clust, library(mltools))
clusterEvalQ(clust, library(data.table))
clusterEvalQ(clust, library(glmnet))
clusterEvalQ(clust, library(xgboost))
clusterEvalQ(clust, library(randomForest))
clusterEvalQ(clust, library(Metrics))
             
#make for loop including first define train and test data, impute the train data,
#install recipes package ready for imputing test with different methods. The next
#steps after that would be to train the model and evaluate but I haven't got to 
#that yet
#---------------------------------FOR LOOP--------------------------------------

results <- foreach(x = seq_along(folds)) %dopar% {
  #Define train and test data
  test_id <- folds[[x]]
  train_data <- main_dummy_data[-test_id,]
  test_data <- main_dummy_data[test_id,]
  
  #Impute the train data
  imp_train <- mice(train_data, method = meth_dummy,
                    predictorMatrix = predictor_matrix_dummy, m = m, maxit = 5)
  
  lm_models <- with(imp_train, glm(formula = as.formula(full_model),
                                   family = gamma))
  
  pooled_lm <- pool(lm_models)
  
  #impute test set
  imp_test <- mice(test_data, method = meth_dummy,
                   predictorMatrix = predictor_matrix_dummy, m = m, maxit = 5)
  
  #get predictions for each test set on the pooled model
  imputed_test_data <- complete(imp_test, "all")
  
  fitted_models <- lm_models$analyses
  
  prediction_matrix <- matrix(, nrow = nrow(test_data), ncol = 0)
  
  for (x in imputed_test_data) {
    for (y in fitted_models){
      los_pred <- predict(y, x)#, #type = "response")
      prediction_matrix <- cbind(prediction_matrix, los_pred)
    }
  }
  
  #average the predictions and average the actuals
  predictions <- rowMeans(prediction_matrix)
  
  predictions_df <- as.data.frame(predictions) %>%
    rename(total_los = predictions) %>%
    mutate(total_los = ifelse(total_los <= 0.25, 0.25, total_los))
  
  #work out the performance metric - output
  lm_rmse <- mltools::rmse(preds = predictions_df$total_los,
                  imputed_test_data[[1]]$total_los)
  
  lm_mae <- Metrics::mae(actual = imputed_test_data[[1]]$total_los,
                predicted = predictions_df$total_los)
  
  ##ridge
  
  #after training models average coefficients to get pooled model
  
  #do the same as for linear regression
  
  #test for different lambda
  
  ##LASSO
  
  #combine imputed train sets and run LASSO
  
  #combine imputed test sets and predict on the trained LASSO model
  
  #Get the performance metric
  
  #test for different lambda
  
  
  
  
  #onehot_list <- list()
  
  #for (y in 1:m){
  #train_onehot <- complete(imp_train, y) %>%
  #  as.data.table() %>%
  #  one_hot() %>%
  #  as.data.frame()
  
  #onehot_list[[y]] <- train_onehot
  #}
  
 # lm_models <- lapply(onehot_list,
   #                   function(train_onehot)
    #                  lm(total_los ~ ., data = train_onehot)) %>%
  #  as.mira()
  
  #attr(lm_models, "call") <- quote(with.mids(data = imp_train,
  #                                           expr = lm(total_los ~ .)))
    
  #pooled_lm <- pool(lm_models)
  
  
  train <- complete(imp_train, 1)
  
  y_train = train$total_los
  
  x_train = train[, -9]
  
  #Train the model
  #Linear regression
  #model <- lm(total_los ~ ., train)
  
  #model2 <- lm(log(total_los) ~ ., train)
  
  #model3 <- lm(formula = log(total_los) ~ age + admission_type + insurance + 
       #previous_stay + platelets + chloride + calcium_gluconate + 
       #hospital_expire_flag, data = train)
  
  #One-hot
  #model_onehot <- lm(total_los ~ ., train_onehot)
  
  #LASSO
  
  imputed_datasets <- complete(imp_train, "all")
  
 # model_lasso <- glmnet(x_train, y_train, alpha = 1)
  
  #lambdagrid <- c(0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1, 1.5, 2, 2.5, 3, 3.5)
  
  lasso_list_rmse <- list()
  
  lasso_list_mae <- list()
  
  lasso_rmse_list <- list()
  
  lasso_mae_list <- list()
  
  #for(lam in lambdagrid ) {
    #for (i in seq_along(imputed_datasets)){
      #y_train <- imputed_datasets[[i]]$total_los
      #x_train <- imputed_datasets[[i]][, -9]
      
      #model_lasso <- glmnet(x_train, y_train, alpha = 1, lambda = lam)
      
     # lasso_list[[i]] <- model_lasso
    #}
    
    #prediction_matrix <- matrix(, nrow = nrow(test_data), ncol = 0)
    
    #for (x in imputed_test_data) {
     # x_variables <- x[,-9]
      #for (y in seq_along(lasso_list)){
      #  los_pred <- predict(lasso_list[[y]], as.matrix(x_variables))#, #type = "response")
     #   prediction_matrix <- cbind(prediction_matrix, los_pred)
    #  }
   # }
    
    #predictions <- rowMeans(prediction_matrix)
    
    #predictions_df <- as.data.frame(predictions) %>%
      #rename(total_los = predictions) %>%
     # mutate(total_los = ifelse(total_los <= 0.25, 0.25, total_los))
    
    #lasso_rmse <- mltools::rmse(preds = predictions_df$total_los,
     #               imputed_test_data[[1]]$total_los)
    
    #lasso_mae <- mae(actual = imputed_test_data[[1]]$total_los,
     #                predicted = predictions_df$total_los)
    
    #lasso_rmse_list <- append(lasso_rmse_list, lasso_rmse)
    
   # lasso_mae_list <- append(lasso_mae_list, lasso_mae)
  #}
  
  #lasso_rmse_df <- as.data.frame(lasso_rmse_list)
  
  #lasso_mae_df <- as.data.frame(lasso_mae_list)
  
  #colnames(lasso_rmse_df) <- c("0.001", "0.005", "0.01", "0.05", "0.1", "0.5",
   #                            "1", "1.5", "2", "2.5", "3", "3.5")
  
 # colnames(lasso_mae_df) <- c("0.001", "0.005", "0.01", "0.05", "0.1", "0.5",
  #                             "1", "1.5", "2", "2.5", "3", "3.5")
  
  #for (i in seq_along(imputed_datasets)){
   # y_train <- imputed_datasets[[i]]$total_los
  #  x_train <- imputed_datasets[[i]][, -9]
    
    #cv_lasso <- cv.glmnet(as.matrix(x_train), y_train, alpha = 1, nfolds = 5)
    
    #model_lasso <- glmnet(x_train, y_train, alpha = 1, lambda = lam)
    
    #opt_lam <- cv_lasso$lambda.min
    
    #model_lasso <- glmnet(as.matrix(x_train), y_train, alpha = 1,
     #                     lambda = opt_lam)
    
   # lasso_list[[i]] <- model_lasso
  #}
  
  #prediction_matrix <- matrix(, nrow = nrow(test_data), ncol = 0)
  
  #for (x in imputed_test_data) {
   # x_variables <- x[,-9]
    #for (y in seq_along(lasso_list)){
     # los_pred <- predict(lasso_list[[y]], as.matrix(x_variables))#, #type = "response")
      #prediction_matrix <- cbind(prediction_matrix, los_pred)
    #}
    #}
    
    #predictions <- rowMeans(prediction_matrix)
    
   # predictions_df <- as.data.frame(predictions) %>%
    #  rename(total_los = predictions) %>%
     # mutate(total_los = ifelse(total_los <= 0.25, 0.25, total_los))
    
  #  lasso_rmse <- mltools::rmse(preds = predictions_df$total_los,
     #                           imputed_test_data[[1]]$total_los)
    
    #lasso_mae <- Metrics::mae(actual = imputed_test_data[[1]]$total_los,
   #                           predicted = predictions_df$total_los)
    
  #  lasso_rmse_list <- append(lasso_rmse_list, lasso_rmse)
    
   # lasso_mae_list <- append(lasso_mae_list, lasso_mae)
  #}
  
  #lasso_rmse_df <- as.data.frame(lasso_rmse_list)
  
  #lasso_mae_df <- as.data.frame(lasso_mae_list)
  
  
  opt_lam_lasso_rmse <- list()
  
  opt_lam_lasso_mae <-list()
  
  rmse_cv_list <- list()
  
  mae_cv_list <- list()
  
  for (i in seq_along(imputed_datasets)){
    y_train <- imputed_datasets[[i]]$total_los
    x_train <- imputed_datasets[[i]][, -9]
    
    cv_lasso <- cv.glmnet(as.matrix(x_train), y_train, alpha = 1, nfolds = 5)
    
    cv_lasso_mae <- cv.glmnet(as.matrix(x_train), y_train, alpha = 1,
                              nfolds = 5, type.measure = "mae")
    
    #model_lasso <- glmnet(x_train, y_train, alpha = 1, lambda = lam)
    
    opt_lam <- cv_lasso$lambda.min
    
    rmse_cv <- sqrt(cv_lasso$cvm)
    
    opt_lam_lasso_rmse <- append(opt_lam_lasso_rmse, opt_lam)
    
    rmse_cv_list <- append(rmse_cv_list, rmse_cv)
    
    opt_lam_mae <- cv_lasso_mae$lambda.min
    
    mae_cv <- cv_lasso_mae$cvm
    
    opt_lam_lasso_mae <- append(opt_lam_lasso_mae, opt_lam_mae)
    
    mae_cv_list <- append(mae_cv_list, mae_cv)
    
    #model_ridge_rmse <- glmnet(as.matrix(x_train), y_train, alpha = 0,
    #lambda = opt_lam)
    
    #model_ridge_mae <- glmnet(as.matrix(x_train), y_train, alpha = 0,
    #lambda = opt_lam_mae)
    
    #ridge_list_rmse[[i]] <- model_ridge_rmse
    
    #ridge_list_mae[[i]] <- model_ridge_mae
  }
  
  lasso_lam_rmse_df <- data.frame(
    lambda = unlist(opt_lam_lasso),
    rmse = unlist(rmse_cv_list)
  )
  
  opt_ind_lasso_rmse <- which.min(lasso_lam_rmse_df$rmse)
  lasso_tuned_lambda_rmse <- lasso_lam_rmse_df[opt_ind_lasso_rmse]
  
  lam_lasso_rmse <- append(lam_lasso_rmse, lasso_tuned_lambda_rmse)
  
  lasso_lam_mae_df <- data.frame(
    lambda = unlist(opt_lam_lasso),
    mae = unlist(mae_cv_list)
  )
  
  opt_ind_lasso_mae <- which.min(lasso_lam_mae_df$mae)
  lasso_tuned_lambda_mae <- lasso_lam_mae_df[opt_ind_lasso_mae]
  
  lam_lasso_mae <- append(lam_lasso_mae, lasso_tuned_lambda_mae)
  
  for (i in seq_along(imputed_datasets)){
    y_train <- imputed_datasets[[i]]$total_los
    x_train <- imputed_datasets[[i]][, -9]
    
    model_lasso_rmse <- glmnet(as.matrix(x_train), y_train, alpha = 1,
                               lambda = lasso_tuned_lambda_rmse)
    
    model_lasso_mae <- glmnet(as.matrix(x_train), y_train, alpha = 1,
                              lambda = lasso_tuned_lambda_mae)
    
    lasso_list_rmse[[i]] <- model_lasso_rmse
    
    lasso_list_mae[[i]] <- model_lasso_mae
  }
  
  prediction_matrix_rmse <- matrix(, nrow = nrow(test_data), ncol = 0)
  
  prediction_matrix_mae <- matrix(, nrow = nrow(test_data), ncol = 0)
  
  for (x in imputed_test_data) {
    x_variables <- x[,-9]
    for (y in seq_along(lasso_list_rmse)){
      los_pred_rmse <- predict(lasso_list_rmse[[y]], as.matrix(x_variables))#, #type = "response")
      prediction_matrix_rmse <- cbind(prediction_matrix_rmse, los_pred_rmse)
    }
    #}
    
    #for (x in imputed_test_data) {
    #x_variables <- x[,-9]
    for (y in seq_along(lasso_list_mae)){
      los_pred_mae <- predict(lasso_list_mae[[y]], as.matrix(x_variables))#, #type = "response")
      prediction_matrix_mae <- cbind(prediction_matrix_mae, los_pred_mae)
    }
    
    predictions_rmse <- rowMeans(prediction_matrix_rmse)
    
    predictions_mae <- rowMeans(prediction_matrix_mae)
    
    predictions_df_rmse <- as.data.frame(predictions_rmse) %>%
      rename(total_los = predictions_rmse) %>%
      mutate(total_los = ifelse(total_los <= 0.25, 0.25, total_los))
    
    predictions_df_mae <- as.data.frame(predictions_mae) %>%
      rename(total_los = predictions_mae) %>%
      mutate(total_los = ifelse(total_los <= 0.25, 0.25, total_los))
    
    lasso_rmse <- mltools::rmse(preds = predictions_df_rmse$total_los,
                                imputed_test_data[[1]]$total_los)
    
    lasso_mae <- Metrics::mae(actual = imputed_test_data[[1]]$total_los,
                              predicted = predictions_df_mae$total_los)
    
    lasso_rmse_list <- append(lasso_rmse_list, lasso_rmse)
    
    lasso_mae_list <- append(lasso_mae_list, lasso_mae)
  }
  
  lasso_rmse_df <- as.data.frame(lasso_rmse_list)
  
  lasso_mae_df <- as.data.frame(lasso_mae_list)
  
  
  #Ridge
  ridge_list_rmse <- list()
  
  ridge_list_mae <- list()
    
  ridge_rmse_list <- list()
  
  ridge_mae_list <- list()
    
  #for(lam in lambdagrid ) {
    #for (i in seq_along(imputed_datasets)){
     # y_train <- imputed_datasets[[i]]$total_los
    #  x_train <- imputed_datasets[[i]][, -9]
        
     # model_ridge <- glmnet(x_train, y_train, alpha = 0, lambda = lam)
        
    #  ridge_list[[i]] <- model_ridge
    #}
      
    #prediction_matrix <- matrix(, nrow = nrow(test_data), ncol = 0)
      
    #for (x in imputed_test_data) {
     # x_variables <- x[,-9]
    #  for (y in seq_along(ridge_list)){
       # los_pred <- predict(ridge_list[[y]], as.matrix(x_variables))#, #type = "response")
      #  prediction_matrix <- cbind(prediction_matrix, los_pred)
     # }
    #}
      
    #predictions <- rowMeans(prediction_matrix)
      
    #predictions_df <- as.data.frame(predictions) %>%
      #rename(total_los = predictions) %>%
     # mutate(total_los = ifelse(total_los <= 0.25, 0.25, total_los))
      
    #ridge_rmse <- mltools::rmse(preds = predictions_df$total_los,
     #                    imputed_test_data[[1]]$total_los)
    
    #ridge_mae <- mae(actual = imputed_test_data[[1]]$total_los,
     #                predicted = predictions_df$total_los)
      
    #ridge_rmse_list <- append(ridge_rmse_list, ridge_rmse)
    
   # ridge_mae_list <- append(ridge_mae_list, ridge_mae)
  #}
    
  #ridge_rmse_df <- as.data.frame(ridge_rmse_list)
  
  #ridge_mae_df <- as.data.frame(ridge_mae_list)
    
  #colnames(ridge_rmse_df) <- c("0.001", "0.005", "0.01", "0.05", "0.1", "0.5",
  #                             "1", "1.5", "2", "2.5", "3", "3.5")
  
  #colnames(ridge_mae_df) <- c("0.001", "0.005", "0.01", "0.05", "0.1", "0.5",
                               #"1", "1.5", "2", "2.5", "3", "3.5")
  
  
  opt_lam_ridge_rmse <- list()
  
  opt_lam_ridge_mae <-list()
  
  rmse_cv_list <- list()
  
  mae_cv_list <- list()
  
  for (i in seq_along(imputed_datasets)){
    y_train <- imputed_datasets[[i]]$total_los
    x_train <- imputed_datasets[[i]][, -9]
    
    cv_ridge <- cv.glmnet(as.matrix(x_train), y_train, alpha = 0, nfolds = 5)
    
    cv_ridge_mae <- cv.glmnet(as.matrix(x_train), y_train, alpha = 0,
                              nfolds = 5, type.measure = "mae")
    
    #model_lasso <- glmnet(x_train, y_train, alpha = 1, lambda = lam)
    
    opt_lam <- cv_ridge$lambda.min
    
    rmse_cv <- sqrt(cv_ridge$cvm)
    
    opt_lam_ridge_rmse <- append(opt_lam_ridge_rmse, opt_lam)
    
    rmse_cv_list <- append(rmse_cv_list, rmse_cv)
    
    opt_lam_mae <- cv_ridge_mae$lambda.min
    
    mae_cv <- cv_ridge_mae$cvm
    
    opt_lam_ridge_mae <- append(opt_lam_ridge_mae, opt_lam_mae)
    
    mae_cv_list <- append(mae_cv_list, mae_cv)
    
    #model_ridge_rmse <- glmnet(as.matrix(x_train), y_train, alpha = 0,
                          #lambda = opt_lam)
    
    #model_ridge_mae <- glmnet(as.matrix(x_train), y_train, alpha = 0,
                               #lambda = opt_lam_mae)
    
    #ridge_list_rmse[[i]] <- model_ridge_rmse
    
    #ridge_list_mae[[i]] <- model_ridge_mae
  }
  
  ridge_lam_rmse_df <- data.frame(
    lambda = unlist(opt_lam_ridge),
    rmse = unlist(rmse_cv_list)
  )
  
  opt_ind_ridge_rmse <- which.min(ridge_lam_rmse_df$rmse)
  ridge_tuned_lambda_rmse <- ridge_lam_rmse_df$lambda[opt_ind_ridge_rmse]
  
  lam_ridge_rmse <- append(lam_ridge_rmse, ridge_tuned_lambda_rmse)
  
  ridge_lam_mae_df <- data.frame(
    lambda = unlist(opt_lam_ridge),
    mae = unlist(mae_cv_list)
  )
  
  opt_ind_ridge_mae <- which.min(ridge_lam_mae_df$mae)
  ridge_tuned_lambda_mae <- ridge_lam_mae_df$lambda[opt_ind_ridge_mae]
  
  lam_ridge_mae <- append(lam_ridge_mae, ridge_tuned_lambda_mae)
  
  for (i in seq_along(imputed_datasets)){
    y_train <- imputed_datasets[[i]]$total_los
    x_train <- imputed_datasets[[i]][, -9]
    
    model_ridge_rmse <- glmnet(as.matrix(x_train), y_train, alpha = 0,
                               lambda = ridge_tuned_lambda_rmse)
    
    model_ridge_mae <- glmnet(as.matrix(x_train), y_train, alpha = 0,
                              lambda = ridge_tuned_lambda_mae)
    
    ridge_list_rmse[[i]] <- model_ridge_rmse
    
    ridge_list_mae[[i]] <- model_ridge_mae
  }
  
  prediction_matrix_rmse <- matrix(, nrow = nrow(test_data), ncol = 0)
  
  prediction_matrix_mae <- matrix(, nrow = nrow(test_data), ncol = 0)
  
  for (x in imputed_test_data) {
    x_variables <- x[,-9]
    for (y in seq_along(ridge_list_rmse)){
      los_pred_rmse <- predict(ridge_list_rmse[[y]], as.matrix(x_variables))#, #type = "response")
      prediction_matrix_rmse <- cbind(prediction_matrix_rmse, los_pred_rmse)
    }
    #}
    
    #for (x in imputed_test_data) {
      #x_variables <- x[,-9]
    for (y in seq_along(ridge_list_mae)){
      los_pred_mae <- predict(ridge_list_mae[[y]], as.matrix(x_variables))#, #type = "response")
      prediction_matrix_mae <- cbind(prediction_matrix_mae, los_pred_mae)
    }
    
    predictions_rmse <- rowMeans(prediction_matrix_rmse)
    
    predictions_mae <- rowMeans(prediction_matrix_mae)
    
    predictions_df_rmse <- as.data.frame(predictions_rmse) %>%
      rename(total_los = predictions_rmse) %>%
      mutate(total_los = ifelse(total_los <= 0.25, 0.25, total_los))
    
    predictions_df_mae <- as.data.frame(predictions_mae) %>%
      rename(total_los = predictions_mae) %>%
      mutate(total_los = ifelse(total_los <= 0.25, 0.25, total_los))
    
    ridge_rmse <- mltools::rmse(preds = predictions_df_rmse$total_los,
                                imputed_test_data[[1]]$total_los)
    
    ridge_mae <- Metrics::mae(actual = imputed_test_data[[1]]$total_los,
                              predicted = predictions_df_mae$total_los)
    
    ridge_rmse_list <- append(ridge_rmse_list, ridge_rmse)
    
    ridge_mae_list <- append(ridge_mae_list, ridge_mae)
  }
  
  ridge_rmse_df <- as.data.frame(ridge_rmse_list)
  
  ridge_mae_df <- as.data.frame(ridge_mae_list)
    
  #model_ridge <- glmnet(x_train, y_train, alpha = 0)
  
  #for(lam in lambdagrid ) {
   # model_ridge2 <- glmnet(x_train, y_train, alpha = 0, lambda = lam)
    
    #predict on train
    #predict on test
    
    #confusion matricies
  #}
  
  #Random Forest
  
  mtry_range <- c(2, 3, 4, 5, 6, 10)
  
  rfm_list <- list()
  
  rfm_rmse_list <- list()
  
  rfm_mae_list <- list()
  
  #for (try in seq_along(mtry_range)) {
    for (i in seq_along(imputed_datasets)){
      
      #y_train <- imputed_datasets[[i]]$total_los
      #x_train <- imputed_datasets[[i]][, -9]
      
      rfm <- randomForest( total_los ~ .,
                           mtry = mtry_range[try],
                           ntree = 200,
                           data = imputed_datasets[[i]])
      
      rfm_list[[i]] <- rfm
    }
    
    prediction_matrix <- matrix(, nrow = nrow(test_data), ncol = 0)
    
    for (x in imputed_test_data) {
      for (y in seq_along(rfm_list)){
        los_pred <- predict(rfm_list[[y]], as.matrix(x))
        prediction_matrix <- cbind(prediction_matrix, los_pred)
      }
    }
    
    predictions <- rowMeans(prediction_matrix)
    
    predictions_df <- as.data.frame(predictions) %>%
      rename(total_los = predictions) %>%
      mutate(total_los = ifelse(total_los <= 0.25, 0.25, total_los))
    
    rfm_rmse <- mltools::rmse(preds = predictions_df$total_los,
                       imputed_test_data[[1]]$total_los)
    
    rfm_mae <- Metrics::mae(actual = imputed_test_data[[1]]$total_los,
                   predicted = predictions_df$total_los)
    
    rfm_rmse_list <- append(rfm_rmse_list, rfm_rmse)
    
    rfm_mae_list <- append(rfm_mae_list, rfm_mae)
  }
  
  rfm_rmse_df <- as.data.frame(rfm_rmse_list)
  
  rfm_mae_df <- as.data.frame(rfm_mae_list)
  
  colnames(rfm_rmse_df) <- c("2", "3", "4", "5", "6", "10")
  
  colnames(rfm_mae_df) <- c("2", "3", "4", "5", "6", "10")
  
  
  cv_method <- trainControl(method = "cv", number = 5)
  
  #rmse
  for (i in seq_along(imputed_datasets)){
    model_rmse <- caret::train(total_los ~ .,
                          method = "rf",
                          trControl = cv_method,
                          data = imputed_datasets[[i]],
                          tuneGrid = expand.grid(mtry_range),
                          metric = "RMSE")
    
    model_mae <- caret::train(total_los ~ .,
                               method = "rf",
                               trControl = cv_method,
                               data = imputed_datasets[[i]],
                               tuneGrid = expand.grid(mtry_range),
                               metric = "MAE")
    
    opt_mtry_rmse <- model_rmse$finalModel$mtry
    
    opt_rmse <- model_rmse$results$RMSE[model_rmse$results$mtry 
                                        == model_rmse$bestTune$mtry]
    
    opt_mtry_mae <- model_mae$finalModel$mtry
    
    opt_mae <- model_mae$results$MAE[model_mae$results$mtry 
                                        == model_mae$bestTune$mtry]
    
    
    opt_mtry_list_rmse <- append(opt_mtry_list_rmse, opt_mtry_rmse)
    
    opt_mtry_list_mae <- append(opt_mtry_list_mae, opt_mtry_mae)
    
    opt_rmse_list <- append(opt_rmse_list, opt_rmse)
    
    opt_mae_list <- append(opt_mae_list, opt_mae)
  }
  
  rfm_mtry_rmse_df <- data.frame(
    mtry = unlist(opt_mtry_list_rmse),
    rmse = unlist(opt_rmse_list)
  )
  
  opt_ind_rfm_rmse <- which.min(rfm_mtry_rmse_df$rmse)
  rfm_tuned_mtry_rmse <- rfm_mtry_rmse_df$mtry[opt_ind_rfm_rmse]
  
  mtry_rfm_rmse <- append(mtry_rfm_rmse, rfm_tuned_mtry_rmse)
  
  rfm_mtry_mae_df <- data.frame(
    mtry = unlist(opt_mtry_list_mae),
    mae = unlist(opt_mae_list)
  )
  
  opt_ind_rfm_mae <- which.min(rfm_mtry_mae_df$mae)
  rfm_tuned_mtry_mae <- rfm_mtry_mae_df$lambda[opt_ind_mtry_mae]
  
  mtry_rfm_mae <- append(mtry_rfm_mae, rfm_tuned_mtry_mae)
  
  for (i in seq_along(imputed_datasets)){
    
    #y_train <- imputed_datasets[[i]]$total_los
    #x_train <- imputed_datasets[[i]][, -9]
    
    rfm_rmse <- randomForest( total_los ~ .,
                         mtry = rfm_tuned_mtry_rmse,
                         ntree = 200,
                         data = imputed_datasets[[i]])
    
    rfm_list_rmse[[i]] <- rfm_rmse
    
    rfm_mae <- randomForest( total_los ~ .,
                              mtry = rfm_tuned_mtry_mae,
                              ntree = 200,
                              data = imputed_datasets[[i]])
    
    rfm_list_mae[[i]] <- rfm_mae
  }
  
  prediction_matrix_rmse <- matrix(, nrow = nrow(test_data), ncol = 0)
  
  for (x in imputed_test_data) {
    for (y in seq_along(rfm_list_rmse)){
      los_pred_rmse <- predict(rfm_list_rmse[[y]], as.matrix(x))
      prediction_matrix_rmse <- cbind(prediction_matrix_rmse, los_pred_rmse)
    }
  }
  
  predictions_rmse <- rowMeans(prediction_matrix_rmse)
  
  predictions_rmse_df <- as.data.frame(predictions_rmse) %>%
    rename(total_los = predictions_rmse) %>%
    mutate(total_los = ifelse(total_los <= 0.25, 0.25, total_los))
  
  prediction_matrix_mae <- matrix(, nrow = nrow(test_data), ncol = 0)
  
  for (x in imputed_test_data) {
    for (y in seq_along(rfm_list_mae)){
      los_pred_mae <- predict(rfm_list_mae[[y]], as.matrix(x))
      prediction_matrix_mae <- cbind(prediction_matrix_mae, los_pred_mae)
    }
  }

  predictions_mae <- rowMeans(prediction_matrix_mae)

  predictions_mae_df <- as.data.frame(predictions_mae) %>%
    rename(total_los = predictions_mae) %>%
    mutate(total_los = ifelse(total_los <= 0.25, 0.25, total_los))
  
  rfm_rmse <- mltools::rmse(preds = predictions_rmse_df$total_los,
                            imputed_test_data[[1]]$total_los)
  
  rfm_mae <- Metrics::mae(actual = imputed_test_data[[1]]$total_los,
                          predicted = predictions_mae_df$total_los)
  
  rfm_rmse_list <- append(rfm_rmse_list, rfm_rmse)
  
  rfm_mae_list <- append(rfm_mae_list, rfm_mae)

  rfm_rmse_df <- as.data.frame(rfm_rmse_list)
  
  rfm_mae_df <- as.data.frame(rfm_mae_list)

#colnames(rfm_rmse_df) <- c("2", "3", "4", "5", "6", "10")

#colnames(rfm_mae_df) <- c("2", "3", "4", "5", "6", "10")
  
  
  
  
  
 # rm_2 <- tuneRF(
      #  x = x_train,
       # y = y_train,
        #ntreeTry = 50, 
      #  mtryStart = 5,
       # stepFactor = 0.5,
        #improve = 0.01, 
        #trace = FALSE)
  
 # rmse_rm_1 <- sqrt(mean(rm_1$mse))
  
  #xgboost
  #x_onehot <- complete(imp_train, 1) %>%
  #  as.data.table() %>%
  #  one_hot() %>%
  #  as.data.frame()
  
  #x_test <- test_data[, -c(length(test_data), length(test_data)-1)]
  
  #xgdata <- xgb.DMatrix(data = as.matrix(x_onehot), label = y_train)
  
  #single_tree <- xgb.train(data = xgdata, nrounds = 1)
  
  #xg_test <- xgb.DMatrix(data = as.matrix())
  
 # xgboost_multi <- xgb.train(data = xgdata,
                           #enable_categorical = TRUE,
   #                        tree_method = "hist",
   #                        objective = "reg:squarederror",
   #                        nrounds = 100)
  
  list(lm_rmse = lm_rmse,
       lm_mae = lm_mae,
       lasso_rmse = lasso_rmse_df,
       lasso_mae = lasso_mae_df,
       ridge_rmse = ridge_rmse_df,
       ridge_mae = ridge_mae_df,
       random_forest_rmse = rfm_rmse_df,
       random_forest_mae = rfm_mae_df,
       imputed_datasets = imputed_datasets,
       imputed_test_data = imputed_test_data
    #   xgboost_single = single_tree,
    #   xgboost = xgboost_multi
  )
  
#}

stopCluster(clust)


#ridge_lam_rmse_df <- data.frame(
 # lambda = unlist(opt_lam_ridge),
 # rmse = unlist(rmse_cv_list)
  #)

#opt_ind_ridge_rmse <- which.min(ridge_lam_rmse_df$rmse)
#ridge_tuned_lambda_rmse <- ridge_lam_rmse_df[opt_ind_ridge_rmse]

#ridge_lam_mae_df <- data.frame(
#  lambda = unlist(opt_lam_ridge),
#  mae = unlist(mae_cv_list)
#)

#opt_ind_ridge_mae <- which.min(ridge_lam_mae_df$mae)
#ridge_tuned_lambda_mae <- ridge_lam_mae_df[opt_ind_ridge_mae]

lasso_rmse <- bind_rows(
  results[[1]]$lasso_rmse,
  results[[2]]$lasso_rmse,
  results[[3]]$lasso_rmse,
  results[[4]]$lasso_rmse,
  results[[5]]$lasso_rmse) %>%
  t() %>%
  as.data.frame()#%>%
  #t() %>%
  #as.data.frame() %>%
 # rowwise() %>%
  #mutate(rmse_mean = mean(c_across(everything()))) %>%
  #ungroup()

lasso_rmse$rmse_mean <- rowMeans(lasso_rmse, na.rm = TRUE)

lasso_rmse$lambda <- lam_lasso_rmse

colnames(lasso_rmse) <- c("1", "2", "3", "4", "5", "rmse_mean", "lambda")

rownames(lasso_rmse) <- c(1:nrow(lasso_rmse))

final_id_lasso_rmse <- which.min(lasso_rmse$rmse_mean)

final_lambda_lasso_rmse <- lasso_rmse$lambda[final_id_lasso_rmse]

lasso_mae <- bind_rows(
  results[[1]]$lasso_mae,
  results[[2]]$lasso_mae,
  results[[3]]$lasso_mae,
  results[[4]]$lasso_mae,
  results[[5]]$lasso_mae) %>%
  t() %>%
  as.data.frame()

lasso_mae$mae_mean <- rowMeans(lasso_mae, na.rm = TRUE)

lasso_mae$lambda <- lam_lasso_mae

colnames(lasso_mae) <- c("1", "2", "3", "4", "5", "mae_mean", "lambda")

rownames(lasso_mae) <- c(1:nrow(lasso_mae))

final_id_lasso_mae <- which.min(lasso_mae$mae_mean)

final_lambda_lasso_mae <- lasso_mae$lambda[final_id_lasso_mae]

ridge_rmse <- bind_rows(
  results[[1]]$ridge_rmse,
  results[[2]]$ridge_rmse,
  results[[3]]$ridge_rmse,
  results[[4]]$ridge_rmse,
  results[[5]]$ridge_rmse) %>%
  t() %>%
  as.data.frame()

ridge_rmse$rmse_mean <- rowMeans(ridge_rmse, na.rm = TRUE)

ridge_rmse$lambda <- lam_ridge_rmse

colnames(ridge_rmse) <- c("1", "2", "3", "4", "5", "rmse_mean", "lambda")

rownames(ridge_rmse) <- c(1:nrow(ridge_rmse))

final_id_ridge_rmse <- which.min(ridge_rmse$rmse_mean)

final_lambda_ridge_rmse <- ridge_rmse$lambda[final_id_ridge_rmse]

ridge_mae <- bind_rows(
  results[[1]]$ridge_mae,
  results[[2]]$ridge_mae,
  results[[3]]$ridge_mae,
  results[[4]]$ridge_mae,
  results[[5]]$ridge_mae) %>%
  t() %>%
  as.data.frame()
  #mutate(rmse_mean <- rowMeans(across(everything())))

ridge_mae$mae_mean <- rowMeans(ridge_mae, na.rm = TRUE)

ridge_mae$lambda <- lam_ridge_mae

colnames(ridge_mae) <- c("1", "2", "3", "4", "5", "mae_mean", "lambda")

rownames(ridge_mae) <- c(1:nrow(ridge_mae))

final_id_ridge_mae <- which.min(ridge_mae$mae_mean)

final_lambda_ridge_mae <- ridge_mae$lambda[final_id_ridge_mae]


rfm_rmse <- bind_rows(
  results[[1]]$random_forest_rmse,
  results[[2]]$random_forest_rmse, 
  results[[3]]$random_forest_rmse,
  results[[4]]$random_forest_rmse, 
  results[[5]]$random_forest_rmse) %>%
  t() %>%
  as.data.frame()
  #mutate(rmse_mean <- rowMeans(across(everything())))

rfm_rmse$rmse_mean <- rowMeans(rfm_rmse, na.rm = TRUE)

rfm_rmse$mtry <- row.names(rfm_rmse)

colnames(rfm_rmse) <- c("1", "2", "3", "4", "5", "rmse_mean", "mtry")

rownames(rfm_rmse) <- c(1:nrow(rfm_rmse))

rfm_mae <- bind_rows(
  results[[1]]$random_forest_mae,
  results[[2]]$random_forest_mae, 
  results[[3]]$random_forest_mae,
  results[[4]]$random_forest_mae, 
  results[[5]]$random_forest_mae) %>%
  t() %>%
  as.data.frame()
#mutate(rmse_mean <- rowMeans(across(everything())))

rfm_mae$mae_mean <- rowMeans(rfm_mae, na.rm = TRUE)

rfm_mae$mtry <- 

colnames(rfm_mae) <- c("1", "2", "3", "4", "5", "mae_mean", "mtry")

rownames(rfm_mae) <- c(1:nrow(rfm_mae))

lm_rmse_mean <- (results[[1]]$lm_rmse + 
                   results[[2]]$lm_rmse +
                   results[[3]]$lm_rmse + 
                   results[[4]]$lm_rmse + 
                   results[[5]]$lm_rmse)/5

lm_mae_mean <- (results[[1]]$lm_mae + 
                   results[[2]]$lm_mae +
                   results[[3]]$lm_mae + 
                   results[[4]]$lm_mae + 
                   results[[5]]$lm_mae)/5

#xgb.plot.tree(model = results[[1]]$xgboost)

#for(lam in lambdagrid ) {
  

#colnames(lasso_rmse_df) <- c("0.001", "0.005", "0.01", "0.05", "0.1", "0.5",
                            # "1", "1.5", "2", "2.5", "3", "3.5")

#colnames(lasso_mae_df) <- c("0.001", "0.005", "0.01", "0.05", "0.1", "0.5",
                           # "1", "1.5", "2", "2.5", "3", "3.5")

#cv_method <- trainControl(method = "cv", number = 5)

#model <- train(total_los, )

#inside for each

cv_method <- trainControl(method = "cv", number = 5)

#rmse
for (i in seq_along(imputed_datasets)){
  model <- caret::train(total_los ~ .,
                      method = "rf",
                      trControl = cv_method,
                      data = imputed_datasets[[i]],
                      #tuneGrid = expand.grid(mtry_range),
                      metric = "RMSE")
  
  opt_mtry <- model$finalModel$mtry
  
  opt_mtry_list_rmse <- append(opt_mtry_list, opt_mtry)
}

opt_mtry_df_rmse <- as.data.frame(opt_mtry_list_rmse)

colnames(opt_mtry_df_rmse) <- c("mtry")

mode_mtry_rmse <- mode(opt_mtry_df$mtry)

#Find the mode

#make data frame of optimal mtry values 

#predict(model$finalModel, as.matrix(imputed_datasets[[i]]))

#metrics using the test datasets

#outside for each

#will need to make the row a column and rename the column

save the mode within the foreach loop 

make list of the mtry modes

mtry_mode_list <- list(results[[1]]$mode_mtry,
                       results[[2]]$mode_mtry,
                       results[[3]]$mode_mtry,
                       results[[4]]$mode_mtry,
                       results[[5]]$mode_mtry)

opt_mtry_mode <- mode(mtry_mode_list)

#mode_mtry from optimal mtry values data frame

imputed_datasets and imputed_test_data lists need to made

imputed_datasets_list <- list(results[[1]]$imputed_datasets,
                              results[[2]]$imputed_datasets,
                              results[[3]]$imputed_datasets,
                              results[[4]]$imputed_datasets,
                              results[[5]]$imputed_datasets)

imputed_test_data_list <- list(results[[1]]$imputed_test_data,
                              results[[2]]$imputed_test_data,
                              results[[3]]$imputed_test_data,
                              results[[4]]$imputed_test_data,
                              results[[5]]$imputed_test_data)

for(i in seq_along(imputed_datasets_list)) {
rfm <- randomForest( total_los ~ .,
                     mtry = opt_mtry_mode,
                     ntree = 500,
                     data = imputed_datasets_list[[i]])


rfm_list[[i]] <- rfm

}

prediction_matrix <- matrix(, nrow = nrow(test_data), ncol = 0)

for (x in imputed_test_data_list) {
  for (y in seq_along(rfm_list)){
    los_pred <- predict(rfm_list[[y]], as.matrix(x))
    prediction_matrix <- cbind(prediction_matrix, los_pred)
  }
}

predictions <- rowMeans(prediction_matrix)

predictions_df <- as.data.frame(predictions) %>%
  rename(total_los = predictions) %>%
  mutate(total_los = ifelse(total_los <= 0.25, 0.25, total_los))

rfm_rmse <- mltools::rmse(preds = predictions_df$total_los,
                          imputed_test_data[[1]]$total_los)

rfm_mae <- Metrics::mae(actual = imputed_test_data[[1]]$total_los,
                        predicted = predictions_df$total_los)

rfm_rmse_list <- append(rfm_rmse_list, rfm_rmse)

rfm_mae_list <- append(rfm_mae_list, rfm_mae)


rfm_rmse_df <- as.data.frame(rfm_rmse_list)

rfm_mae_df <- as.data.frame(rfm_mae_list)

many to many predictions and evaluations then averages of performance metrics



save the imputed datasets and test_sets from each fold to use i/n  the rfm final loop




#(lm_mae_mean/mean(imputed_test_data[[1]]$total_los))*100

#(lm_mae_mean/median(imputed_test_data[[1]]$total_los))*100




