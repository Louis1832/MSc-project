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
install.packages("ggmice")
install.packages("psych")
install.packages("gridGraphics")
install.packages("sjPlot")
library(sjPlot)
library(gridGraphics)
library(psych)
library(ggmice)
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
#library(gtsummary)
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
         previous_stay = episode - 1) %>%
  filter(episode == 1)

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
  ggtitle("Missing Data") +
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
  theme(plot.title = element_text(hjust = 0.5)) +
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

ggplot(main, aes(x = total_los)) +
  geom_histogram(binwidth = 1) +
  theme_minimal()

#----------------------------MISSING DATA IMPUTATION----------------------------

main_impute <- main_summary %>%
  select("age", "sex","admission_type", "ethnicity", "insurance", #"previous_stay",
         "platelets", "glucose", "chloride", "potassium", "haemoglobin",
         "haematocrit", "sodium", "ondansetron", "calcium_gluconate", 
         "hospital_expire_flag", "total_los") %>%
  mutate(#previous_stay = factor(previous_stay),
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
meth["admission_type"] <- "polyreg"
meth["sex"] <- "logreg"
meth["age"] <- "norm"
meth["ondansetron"] <- "logreg"
meth["calcium_gluconate"] <- "logreg"
meth["hospital_expire_flag"] <- "logreg"
meth["total_los"] <- ""
#meth["previous_stay"] <- "polyreg"
meth["glucose"] <- "pmm"
meth["sodium"] <- "pmm"
meth["chloride"] <- "pmm"
meth["platelets"] <- "pmm"
meth["potassium"] <- "pmm"
meth["haemoglobin"] <- "pmm"
meth["haematocrit"] <- "pmm"

imp <- mice(main_impute, method = meth, m = 15, maxit = 15)

plot(imp, vars = "haemoglobin")

completed_data <- complete(imp, 2)

hist(completed_data$sodium)

main_impute.cor = main_impute %>% 
  select(c("glucose","sodium","chloride", "platelets", "potassium",
           "haemoglobin", "haematocrit")) %>%
  cor(use = "complete.obs")

corrplot(main_impute.cor, tl.col = "black")

predictor_matrix <- imp$predictorMatrix

#plot imp
plot(imp)
ggmice::plot_trace(imp, "haemoglobin", legend = FALSE)
ggmice::plot_trace(imp, "haematocrit", legend = FALSE)

#change predictor matrix for haematocrit and haemoglobin to 0
predictor_matrix["haematocrit","haemoglobin"] <- 0
predictor_matrix["haemoglobin","haematocrit"] <- 0

#make imp2 with new predictor matrix
imp2 <- mice(main_impute, method = meth, predictorMatrix = predictor_matrix,
            m = 15, maxit = 15)

#plot imp2
plot(imp2)
ggmice::plot_trace(imp2, "haemoglobin", legend = FALSE)
ggmice::plot_trace(imp2, "haematocrit", legend = FALSE)

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

full_model_formula <- as.formula(full_model)

#create cross validation folds
set.seed(123)

folds <- createFolds(main_dummy_data$total_los, 5)

#number of rounds of imputations
m <- 5

lam_ridge_rmse <- list()

lam_ridge_mae <- list()

lam_lasso_rmse <- list()

lam_lasso_mae <- list()

mtry_rfm_rmse <- list()

mtry_rfm_mae <- list()

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
clusterEvalQ(clust, library(caret))
clusterEvalQ(clust, library(psych))
clusterEvalQ(clust, library(gridGraphics))
             
#make for loop including first define train and test data, impute the train data,
#install recipes package ready for imputing test with different methods. The next
#steps after that would be to train the model and evaluate but I haven't got to 
#that yet
#---------------------------------FOR LOOP--------------------------------------

results <- foreach(x = seq_along(folds),
                   .export = c("full_model", "meth_dummy",
                               "predictor_matrix_dummy", "m",
                               "main_dummy_data")) %dopar% {
  #Define train and test data
  test_id <- folds[[x]]
  train_data <- main_dummy_data[-test_id,]
  test_data <- main_dummy_data[test_id,]
  
  #Impute the train data
  imp_train <- mice(train_data, method = meth_dummy,
                    predictorMatrix = predictor_matrix_dummy, m = m, maxit = 5)
  
  lm_models <- with(imp_train, glm(formula = as.formula(full_model),
                                   family = Gamma(link = "log")))
  
  pooled_lm <- pool(lm_models)
  
  #impute test set
  imp_test <- mice(test_data, method = meth_dummy,
                   predictorMatrix = predictor_matrix_dummy, m = m, maxit = 5)
  
  #get predictions for each test set on the pooled model
  imputed_test_data <- complete(imp_test, "all")
  
  fitted_models <- lm_models$analyses
  
  fitted_models <- list()
  
  #pairs_plots <- list()
  
  imp_train_data <- complete(imp_train, "all")
  
  for (i in 1:m) {
  completed_data <- complete(imp_train, i)
   #pdf(NULL)  # invisible device
   #pairs.panels(completed_data)  # draw the plot
   #pairs_plots[[i]] <- grid.echo()  # capture it
   #dev.off()
   #pairs_plots[[i]] <- recordPlot(pairs.panels(completed_data))
    model <- glm(formula = full_model_formula,
                 data = completed_data,
                 family = Gamma(link = "log"))
    fitted_models[[i]] <- model
  }
  
 # variable_importance_lm <- NULL
  
 # for (i in seq_along(fitted_models)){
  #  imp <- varImp(fitted_models[[i]])$importance
    
    #imp_vec <- imp[,1]
    
  #  variable_importance_lm <- cbind(variable_importance_lm, imp)
  #}
  
 # for (i in 1:m) {
  #  completed_data <- complete(imp_train, i)
 #   model <- lm(formula = as.formula(full_model),
   #              data = completed_data#,
                 #family = Gamma(link = "log"))
  #  )
  #  fitted_models[[i]] <- model
  #}
  
  prediction_matrix <- matrix(, nrow = nrow(test_data), ncol = 0)
  
  for (x in imputed_test_data) {
    for (y in fitted_models){
      los_pred <- as.numeric(predict(y, x))#, type = "response")
      prediction_matrix <- cbind(prediction_matrix, los_pred)
    }
  }
  
  #print(paste("Fold:", x))
  #print(paste("nrow test_data:", nrow(test_data)))
  #print(sapply(imputed_test_data, nrow))
  
  #predictions <- rep(0, nrow(test_data))
  #count <- 0
  
  #for (j in imputed_test_data) {
   # for (y in fitted_models){
    #  los_pred <- as.numeric(predict(y, j))#, #type = "response")
     # if(length(los_pred) == nrow(test_data)){
      # predictions <- predictions + los_pred
       # count <- count + 1
      #}
  # }
  #}
  
  #average the predictions and average the actuals
  predictions <- rowMeans(prediction_matrix)
  
  #predictions <- predictions/count
  
  predictions_df <- as.data.frame(predictions) %>%
   rename(total_los = predictions) %>%
   mutate(total_los = ifelse(total_los <= 0.25, 0.25, total_los))
  
  #work out the performance metric - output
  lm_rmse <- mltools::rmse(preds = predictions_df$total_los,
                  imputed_test_data[[1]]$total_los)
  
  lm_mae <- Metrics::mae(actual = imputed_test_data[[1]]$total_los,
                predicted = predictions_df$total_los)
  
  SSres <- sum((imputed_test_data[[1]]$total_los - predictions_df$total_los)^2)
  
  SStot <- sum((imputed_test_data[[1]]$total_los - mean(imputed_test_data[[1]]$total_los))^2)
  
  lm_R2 <- 1 - SSres/SStot
  
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
  
  
  #train <- complete(imp_train, 1)
  
  #y_train = train$total_los
  
  #x_train = train[, -9]
  
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
  
  lasso_list_R2 <- list()
  
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
    
    #rmse_cv <- sqrt(cv_lasso$cvm)
    
    opt_lam_lasso_rmse <- append(opt_lam_lasso_rmse, opt_lam)
    
    #rmse_cv_list <- append(rmse_cv_list, rmse_cv)
    
    opt_lam_mae <- cv_lasso_mae$lambda.min
    
    #mae_cv <- cv_lasso_mae$cvm
    
    opt_lam_lasso_mae <- append(opt_lam_lasso_mae, opt_lam_mae)
    
    #mae_cv_list <- append(mae_cv_list, mae_cv)
    
    #model_ridge_rmse <- glmnet(as.matrix(x_train), y_train, alpha = 0,
    #lambda = opt_lam)
    
    #model_ridge_mae <- glmnet(as.matrix(x_train), y_train, alpha = 0,
    #lambda = opt_lam_mae)
    
    #ridge_list_rmse[[i]] <- model_ridge_rmse
    
    #ridge_list_mae[[i]] <- model_ridge_mae
  }
  
  #lasso_lam_rmse_df <- data.frame(
   # lambda = unlist(opt_lam_lasso_rmse),
   # rmse = unlist(rmse_cv_list)
  #)
  
  #opt_ind_lasso_rmse <- which.min(lasso_lam_rmse_df$rmse)
  #lasso_tuned_lambda_rmse <- lasso_lam_rmse_df[opt_ind_lasso_rmse]
  
  #lam_lasso_rmse <- append(lam_lasso_rmse, median(unlist(opt_lam_lasso_rmse)))
  
  #lasso_lam_mae_df <- data.frame(
   # lambda = unlist(opt_lam_lasso_mae),
    #mae = unlist(mae_cv_list)
  #)
  
  #opt_ind_lasso_mae <- which.min(lasso_lam_mae_df$mae)
  #lasso_tuned_lambda_mae <- lasso_lam_mae_df[opt_ind_lasso_mae]
  
  #lam_lasso_mae <- append(lam_lasso_mae, median(unlist(opt_lam_lasso_mae)))
  
  for (i in seq_along(imputed_datasets)){
    y_train <- imputed_datasets[[i]]$total_los
    x_train <- imputed_datasets[[i]][, -9]
    
    model_lasso_rmse <- glmnet(as.matrix(x_train), y_train, alpha = 1,
                               lambda = opt_lam_lasso_rmse[[i]])
    
    model_lasso_mae <- glmnet(as.matrix(x_train), y_train, alpha = 1,
                              lambda = opt_lam_lasso_mae[[i]])
    
    lasso_list_rmse[[i]] <- model_lasso_rmse
    
    lasso_list_mae[[i]] <- model_lasso_mae
    
  }
  
  prediction_matrix_rmse <- matrix(, nrow = nrow(test_data), ncol = 0)
  
  prediction_matrix_mae <- matrix(, nrow = nrow(test_data), ncol = 0)
  
  #prediction_matrix_ssres <- matrix(, nrow = nrow(test_data), ncol = 0)
  
  #prediction_matrix_sstot <- matrix(, nrow = nrow(test_data), ncol = 0)
  
  for (j in imputed_test_data) {
    x_variables <- j[,-9]
    for (y in seq_along(lasso_list_rmse)){
      los_pred_rmse <- predict(lasso_list_rmse[[y]], as.matrix(x_variables))#, #type = "response")
      prediction_matrix_rmse <- cbind(prediction_matrix_rmse, los_pred_rmse)
      
      #lasso_ssres <- sum((imputed_test_data[[1]]$total_los - los_pred_rmse)^2)
      #lasso_sstot <- sum((imputed_test_data[[1]]$total_los - mean(imputed_test_data[[1]]))^2)
      #prediction_matrix_ssres <- cbind(prediction_matrix_ssres, lasso_ssres)
      #prediction_matrix_sstot <- cbind(prediction_matrix_sstot, lasso_sstot)
    }
  }
    
  for (j in imputed_test_data) {
    x_variables <- j[,-9]
    for (y in seq_along(lasso_list_mae)){
      los_pred_mae <- predict(lasso_list_mae[[y]], as.matrix(x_variables))#, #type = "response")
      prediction_matrix_mae <- cbind(prediction_matrix_mae, los_pred_mae)
    }
  }
  
  lasso_ssres <- colSums((imputed_test_data[[1]]$total_los - prediction_matrix_rmse)^2)
  
  lasso_sstot <- sum((imputed_test_data[[1]]$total_los - mean(imputed_test_data[[1]]$total_los))^2)
    
  predictions_rmse <- rowMeans(prediction_matrix_rmse)
    
  predictions_mae <- rowMeans(prediction_matrix_mae)
  
  #predictions_ssres <- rowMeans(prediction_matrix_ssres)
  
  #predictions_sstot <- rowMeans(prediction_matrix_sstot)
    
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
  
  lasso_R2_values <- 1 - lasso_ssres/lasso_sstot
  
  lasso_R2 <- mean(lasso_R2_values)
    
  #lasso_rmse_list <- append(lasso_rmse_list, lasso_rmse)
    
  #lasso_mae_list <- append(lasso_mae_list, lasso_mae)
  
  #lasso_rmse_df <- as.data.frame(lasso_rmse_list)
  
  #lasso_mae_df <- as.data.frame(lasso_mae_list)
  
  
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
    
    #rmse_cv <- sqrt(cv_ridge$cvm)
    
    opt_lam_ridge_rmse <- append(opt_lam_ridge_rmse, opt_lam)
    
    #rmse_cv_list <- append(rmse_cv_list, rmse_cv)
    
    opt_lam_mae <- cv_ridge_mae$lambda.min
    
    #mae_cv <- cv_ridge_mae$cvm
    
    opt_lam_ridge_mae <- append(opt_lam_ridge_mae, opt_lam_mae)
    
    #mae_cv_list <- append(mae_cv_list, mae_cv)
    
    #model_ridge_rmse <- glmnet(as.matrix(x_train), y_train, alpha = 0,
                          #lambda = opt_lam)
    
    #model_ridge_mae <- glmnet(as.matrix(x_train), y_train, alpha = 0,
                               #lambda = opt_lam_mae)
    
    #ridge_list_rmse[[i]] <- model_ridge_rmse
    
    #ridge_list_mae[[i]] <- model_ridge_mae
  }
  
  #ridge_lam_rmse_df <- data.frame(
    #lambda = unlist(opt_lam_ridge_rmse),
   # rmse = unlist(rmse_cv_list)
  #)
  
  #opt_ind_ridge_rmse <- which.min(ridge_lam_rmse_df$rmse)
  #ridge_tuned_lambda_rmse <- ridge_lam_rmse_df$lambda[opt_ind_ridge_rmse]
  
  #lam_ridge_rmse <- append(lam_ridge_rmse, median(unlist(opt_lam_ridge_rmse)))
  
  #ridge_lam_mae_df <- data.frame(
    #lambda = unlist(opt_lam_ridge_mae),
   # mae = unlist(mae_cv_list)
  #)
  
  #opt_ind_ridge_mae <- which.min(ridge_lam_mae_df$mae)
  #ridge_tuned_lambda_mae <- ridge_lam_mae_df$lambda[opt_ind_ridge_mae]
  
  #lam_ridge_mae <- append(lam_ridge_mae, median(unlist(opt_lam_ridge_mae)))
  
  for (i in seq_along(imputed_datasets)){
    y_train <- imputed_datasets[[i]]$total_los
    x_train <- imputed_datasets[[i]][, -9]
    
    model_ridge_rmse <- glmnet(as.matrix(x_train), y_train, alpha = 0,
                               lambda = opt_lam_ridge_rmse[[i]])
    
    model_ridge_mae <- glmnet(as.matrix(x_train), y_train, alpha = 0,
                              lambda = opt_lam_ridge_mae[[i]])
    
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
  }
    
  for (x in imputed_test_data) {
    x_variables <- x[,-9]
    for (y in seq_along(ridge_list_mae)){
      los_pred_mae <- predict(ridge_list_mae[[y]], as.matrix(x_variables))#, #type = "response")
      prediction_matrix_mae <- cbind(prediction_matrix_mae, los_pred_mae)
    }
  }
    
  ridge_ssres <- colSums((imputed_test_data[[1]]$total_los - prediction_matrix_rmse)^2)
  
  ridge_sstot <- sum((imputed_test_data[[1]]$total_los - mean(imputed_test_data[[1]]$total_los))^2)
  
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
  
  ridge_R2_values <- 1 - ridge_ssres/ridge_sstot
  
  ridge_R2 <- mean(ridge_R2_values)
    
  #ridge_rmse_list <- append(ridge_rmse_list, ridge_rmse)
    
  #ridge_mae_list <- append(ridge_mae_list, ridge_mae)
  
  #ridge_rmse_df <- as.data.frame(ridge_rmse_list)
  
  #ridge_mae_df <- as.data.frame(ridge_mae_list)
    
  #model_ridge <- glmnet(x_train, y_train, alpha = 0)
  
  #for(lam in lambdagrid ) {
   # model_ridge2 <- glmnet(x_train, y_train, alpha = 0, lambda = lam)
    
    #predict on train
    #predict on test
    
    #confusion matricies
  #}
  
  #Random Forest
  
  mtry_range <- c(2, 3, 4, 5, 6, 10)
  
  rfm_list_rmse <- list()
  
  rfm_list_mae <- list()
  
  rfm_rmse_list <- list()
  
  rfm_mae_list <- list()
  
  #for (try in seq_along(mtry_range)) {
    #for (i in seq_along(imputed_datasets)){
      
      #y_train <- imputed_datasets[[i]]$total_los
      #x_train <- imputed_datasets[[i]][, -9]
      
      #rfm <- randomForest( total_los ~ .,
       #                    mtry = mtry_range[try],
      #                     ntree = 200,
      #                     data = imputed_datasets[[i]])
      
     # rfm_list[[i]] <- rfm
    #}
    
   # prediction_matrix <- matrix(, nrow = nrow(test_data), ncol = 0)
    
  #  for (x in imputed_test_data) {
      #for (y in seq_along(rfm_list)){
       # los_pred <- predict(rfm_list[[y]], as.matrix(x))
      #  prediction_matrix <- cbind(prediction_matrix, los_pred)
     # }
    #}
    
   # predictions <- rowMeans(prediction_matrix)
    
  #  predictions_df <- as.data.frame(predictions) %>%
      #rename(total_los = predictions) %>%
     # mutate(total_los = ifelse(total_los <= 0.25, 0.25, total_los))
    
    #rfm_rmse <- mltools::rmse(preds = predictions_df$total_los,
      #                 imputed_test_data[[1]]$total_los)
    
    #rfm_mae <- Metrics::mae(actual = imputed_test_data[[1]]$total_los,
     #              predicted = predictions_df$total_los)
    
    #rfm_rmse_list <- append(rfm_rmse_list, rfm_rmse)
    
    #rfm_mae_list <- append(rfm_mae_list, rfm_mae)
  #}
  
  #rfm_rmse_df <- as.data.frame(rfm_rmse_list)
  
  #rfm_mae_df <- as.data.frame(rfm_mae_list)
  
  #colnames(rfm_rmse_df) <- c("2", "3", "4", "5", "6", "10")
  
  #colnames(rfm_mae_df) <- c("2", "3", "4", "5", "6", "10")
  
  opt_mtry_list_rmse <- list()
  
  opt_mtry_list_mae <- list()
  
  opt_rmse_list <- list()
  
  opt_mae_list <- list()
  
  
  cv_method <- trainControl(method = "cv", number = 5)
  
  #rmse
  for (i in seq_along(imputed_datasets)){
    model_rmse <- caret::train(total_los ~ .,
                          method = "rf",
                          trControl = cv_method,
                          data = imputed_datasets[[i]],
                          tuneGrid = expand.grid(mtry = mtry_range),
                          metric = "RMSE")
    
    model_mae <- caret::train(total_los ~ .,
                               method = "rf",
                               trControl = cv_method,
                               data = imputed_datasets[[i]],
                               tuneGrid = expand.grid(mtry = mtry_range),
                               metric = "MAE")
    
    opt_mtry_rmse <- model_rmse$finalModel$mtry
    
    #opt_rmse <- model_rmse$results$RMSE[model_rmse$results$mtry 
                                       # == model_rmse$bestTune$mtry]
    
    opt_mtry_mae <- model_mae$finalModel$mtry
    
    #opt_mae <- model_mae$results$MAE[model_mae$results$mtry 
                                       # == model_mae$bestTune$mtry]
    
    
    opt_mtry_list_rmse <- append(opt_mtry_list_rmse, opt_mtry_rmse)
    
    opt_mtry_list_mae <- append(opt_mtry_list_mae, opt_mtry_mae)
    
    #opt_rmse_list <- append(opt_rmse_list, opt_rmse)
    
    #opt_mae_list <- append(opt_mae_list, opt_mae)
  }
  
  #rfm_mtry_rmse_df <- data.frame(
   # mtry = unlist(opt_mtry_list_rmse),
    #rmse = unlist(opt_rmse_list)
  #)
  
  #opt_ind_rfm_rmse <- which.min(rfm_mtry_rmse_df$rmse)
  #rfm_tuned_mtry_rmse <- rfm_mtry_rmse_df$mtry[opt_ind_rfm_rmse]
  
  #mtry_rfm_rmse <- append(mtry_rfm_rmse, floor(median(unlist(opt_mtry_list_rmse))))
  
  #rfm_mtry_mae_df <- data.frame(
   # mtry = unlist(opt_mtry_list_mae),
   # mae = unlist(opt_mae_list)
 # )
  
 # opt_ind_rfm_mae <- which.min(rfm_mtry_mae_df$mae)
  #rfm_tuned_mtry_mae <- rfm_mtry_mae_df$lambda[opt_ind_mtry_mae]
  
  #mtry_rfm_mae <- append(mtry_rfm_mae, floor(median(unlist(opt_mtry_list_rmse))))
  
  for (i in seq_along(imputed_datasets)){
    
    #y_train <- imputed_datasets[[i]]$total_los
    #x_train <- imputed_datasets[[i]][, -9]
    
    rfm_rmse <- randomForest( total_los ~ .,
                         mtry = opt_mtry_list_rmse[[i]],
                         ntree = 200,
                         data = imputed_datasets[[i]],
                         importance = TRUE)
    
    rfm_list_rmse[[i]] <- rfm_rmse
    
    rfm_mae <- randomForest( total_los ~ .,
                              mtry = opt_mtry_list_mae[[i]],
                              ntree = 200,
                              data = imputed_datasets[[i]],
                             importance = TRUE)
    
    rfm_list_mae[[i]] <- rfm_mae
  }
  
  prediction_matrix_rmse <- matrix(, nrow = nrow(test_data), ncol = 0)
  
  #variable_importance <- sapply(rfm_rmse_list, function(m) {
   # importance(m)[, "IncNodePurity"]   # or "%IncMSE" if you prefer
  #})
  
  #variable_importance <- sapply(rfm_list_rmse, function(model) {
   # varImp(model)$importance[,1]  # take the first column (Overall)
  #})
  
  variable_importance <- NULL
  
  for (i in seq_along(rfm_list_rmse)){
    imp <- importance(rfm_list_rmse[[i]])[,"%IncMSE"]
    
    variable_importance <- cbind(variable_importance, imp)
  }
  
  for (x in imputed_test_data) {
    for (y in seq_along(rfm_list_rmse)){
      los_pred_rmse <- predict(rfm_list_rmse[[y]], as.matrix(x))
      prediction_matrix_rmse <- cbind(prediction_matrix_rmse, los_pred_rmse)
    }
  }
  
  rfm_ssres <- colSums((imputed_test_data[[1]]$total_los - prediction_matrix_rmse)^2)
  
  rfm_sstot <- sum((imputed_test_data[[1]]$total_los - mean(imputed_test_data[[1]]$total_los))^2)
  
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
  
  rfm_R2_values <- 1 - rfm_ssres/rfm_sstot
  
  rfm_R2 <- mean(rfm_R2_values)
  
  #rfm_rmse_list <- append(rfm_rmse_list, rfm_rmse)
  
  #rfm_mae_list <- append(rfm_mae_list, rfm_mae)

  #rfm_rmse_df <- as.data.frame(rfm_rmse_list)
  
  #rfm_mae_df <- as.data.frame(rfm_mae_list)

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
       lm_R2 = lm_R2,
       lasso_rmse = lasso_rmse,
       lasso_mae = lasso_mae,
       lasso_R2 = lasso_R2,
       ridge_rmse = ridge_rmse,
       ridge_mae = ridge_mae,
       ridge_R2 = ridge_R2,
       random_forest_rmse = rfm_rmse,
       random_forest_mae = rfm_mae,
       random_forest_R2 = rfm_R2,
       variable_importance = variable_importance,
       #variable_importance_lm = variable_importance_lm,
       train_data = imp_train_data#,
       #imputed_datasets = imputed_datasets,
       #imputed_test_data = imputed_test_data
    #   xgboost_single = single_tree,
    #   xgboost = xgboost_multi
  )
  
}

stopCluster(clust)

var_imp_2 <- NULL

for(i in seq_along(results)) {
  
  # Extract the variable importance matrix/vector
  imp <- results[[i]]$variable_importance  # should be a matrixx or data frame
  
  # Compute row means if it's multi-column, otherwise keep as is
  if(is.matrix(imp) || ncol(imp) > 1){
    imp_vec <- rowMeans(as.matrix(imp))
  } else {
    imp_vec <- as.numeric(imp)
  }
  
  # Make sure it's a column matrix and keep row names
  imp_df <- data.frame(imp_vec)
  rownames(imp_df) <- rownames(imp)
  
  # Bind as new column
  var_imp_2 <- cbind(var_imp_2, imp_df)
}


#for(i in seq_along(results)){
 # imp_df <- results[[i]]$variable_importance %>%
  #as.data.frame() %>%
  #rowMeans() %>%
  #as.data.frame()
  
  #var_imp_2 <- cbind(var_imp_2, imp_df)
#}



var_imp <- results[[1]]$variable_importance %>%
  as.data.frame %>%
  rowMeans() %>%
  as.data.frame() #%>%
#  tibble::rownames_to_column("var") %>%
 # rename("importance" = ".") %>%
  #arrange(importance)

for(i in seq_along(results)){
  if(i==1){
    var_imp <- var_imp
  }
  else {
    var_imp_1 <- results[[i]]$variable_importance %>%
      as.data.frame %>%
      rowMeans() %>%
      as.data.frame()
    
    var_imp <- cbind(var_imp, var_imp_1$.)
  }
}

var_imp <- var_imp %>%
  rowMeans() %>%
  as.data.frame() %>%
  tibble::rownames_to_column("var") %>%
  rename("importance" = ".") %>%
  arrange(importance)

var_imp$var<- var_imp$var %>% as.factor()
var_imp$var <- fct_inorder(var_imp$var)

imp_bar <- ggplot(data = var_imp) + 
  geom_bar(
    stat = "identity",#it leaves the data without count and bin
    mapping = aes(x = var, y=importance, fill = var), 
    show.legend = FALSE,
    width = 1
  ) + 
  labs(x = NULL, y = NULL)
imp_bar + coord_polar() + theme_minimal()
imp_bar + coord_flip() + theme_minimal()


pairs_plots <- vector("list", 25)
i <- 1

for(x in seq_along(results)) {
  for(y in seq_along(results[[x]]$train_data)){
    comp_data <- results[[x]]$train_data[[y]][, 
      sapply(results[[x]]$train_data[[y]], function(x) sd(x, na.rm = TRUE) > 0)
    ]
    #pdf(NULL)  # invisible device
    #pairs.panels(comp_data)  # draw the plot
    #pairs_plots[[i]] <- recordPlot()  # capture it
    #dev.off()
    #pairs_plots[[i]] <- recordPlot(pairs.panels(completed_data)) 
    file <- paste0("pairs_plot_", x, "_", y, ".pdf")
    
    pdf(file)
    pairs.panels(comp_data)
    dev.off()
    
    pairs_plots[[i]] <- file
    
    i <- i + 1
  }
}


#----------------------------PREDICTION MODELS PART 2--------------------------


main_impute_2 <- main_summary %>%
  select("age", "sex","admission_type", "ethnicity", "insurance", #"previous_stay",
         "platelets", "glucose", "chloride", "potassium", "haemoglobin",
         "sodium", "ondansetron", "calcium_gluconate", 
         "hospital_expire_flag", "total_los") %>%
  mutate(#previous_stay = factor(previous_stay),
         ondansetron = factor(ondansetron),
         calcium_gluconate = factor(calcium_gluconate),
         hospital_expire_flag = factor(hospital_expire_flag),
         insurance = na_if(insurance, "Missing"),
         insurance = droplevels(insurance),
         ethnicity = na_if(ethnicity, "Missing"),
         ethnicity = factor(ethnicity),
         ethnicity = droplevels(ethnicity))


main_impute_2 %>% 
  select("potassium", "sodium", "glucose", "haemoglobin",
         "chloride", "platelets", "insurance", "ethnicity") %>%
  
  md.pattern(rotate.names = TRUE)

main_impute_2$sex <- factor(main_impute_2$sex)
main_impute_2$admission_type <- factor(main_impute_2$admission_type)

imp_2 <- mice(main_impute_2, method = "mean", m = 5, maxit = 5)

meth_2 <- imp_2$method


meth_2["ethnicity"] <- "pmm"
meth_2["insurance"] <- "pmm"
meth_2["admission_type"] <- ""
meth_2["sex"] <- ""
meth_2["age"] <- "norm"
meth_2["ondansetron"] <- "logreg"
meth_2["calcium_gluconate"] <- "logreg"
meth_2["hospital_expire_flag"] <- "logreg"
meth_2["total_los"] <- ""
#meth_2["previous_stay"] <- "polyreg"
meth_2["glucose"] <- "pmm"
meth_2["sodium"] <- "pmm"
meth_2["chloride"] <- "pmm"
meth_2["platelets"] <- "pmm"
meth_2["potassium"] <- "pmm"
meth_2["haemoglobin"] <- "pmm"
#meth["haematocrit"] <- "pmm"

imp_2 <- mice(main_impute_2, method = meth_2, m = 15, maxit = 15)

plot(imp_2, vars = "haemoglobin")

completed_data_2 <- complete(imp_2, 2)

hist(completed_data_2$sodium)

main_impute.cor_2 = main_impute_2 %>% 
  select(c("glucose","sodium","chloride", "platelets", "potassium",
           "haemoglobin")) %>%
  cor(use = "complete.obs")

corrplot(main_impute.cor_2, tl.col = "black")

predictor_matrix_2 <- imp_2$predictorMatrix

#plot imp
plot(imp_2)
#ggmice::plot_trace(imp, "haemoglobin", legend = FALSE)
#ggmice::plot_trace(imp, "haematocrit", legend = FALSE)

#change predictor matrix for haematocrit and haemoglobin to 0
#predictor_matrix["haematocrit","haemoglobin"] <- 0
#predictor_matrix["haemoglobin","haematocrit"] <- 0

#make imp2 with new predictor matrix
#imp2 <- mice(main_impute, method = meth, predictorMatrix = predictor_matrix,
 #            m = 15, maxit = 15)

#plot imp2
#plot(imp2)
#ggmice::plot_trace(imp2, "haemoglobin", legend = FALSE)
#ggmice::plot_trace(imp2, "haematocrit", legend = FALSE)

main_impute_NA_2 <- main_impute_2 %>%
  mutate(ethnicity = ethnicity %>% 
           addNA() %>%
           fct_explicit_na(na_level = "NA") %>%
           relevel(ref = "NA"),
         insurance = insurance %>%
           addNA() %>%
           fct_explicit_na(na_level = "NA") %>%
           relevel(ref = "NA"),
  )

main_dummy_data_2 <- recipe(total_los ~ ., data = main_impute_NA_2) %>%
  step_dummy(all_nominal(), one_hot = FALSE) %>%
  prep() %>%
  bake(new_data = NULL)

imp_dummy_2 <- mice(main_dummy_data_2, m = 5, maxit = 5)

imp_main_2 <- mice(main_impute_NA_2, m = 5, maxit = 5)

imp_main_2$method

View(imp_dummy_2$predictorMatrix)

predictor_matrix_main_2 <- imp_main_2$predictorMatrix

#predictor_matrix_dummy["haematocrit","haemoglobin"] <- 0
#predictor_matrix_dummy["haemoglobin","haematocrit"] <- 0

meth_main_2 <- imp_main_2$method

variables_2 <- paste(names(main_impute_NA_2[,-15]), collapse = " + ")

full_model_2 <- paste(c("total_los", variables_2), collapse = " ~ ")

full_model_log_2 <- paste(c("log(total_los)", variables_2), collapse = " ~ ")

full_model_formula_2 <- as.formula(full_model_2)

#create cross validation folds
set.seed(123)

folds <- createFolds(main_impute_NA_2$total_los, 5)

#number of rounds of imputations
m <- 5

lam_ridge_rmse_2 <- list()

lam_ridge_mae_2 <- list()

lam_lasso_rmse_2 <- list()

lam_lasso_mae_2 <- list()

mtry_rfm_rmse_2 <- list()

mtry_rfm_mae_2 <- list()

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
clusterEvalQ(clust, library(caret))
clusterEvalQ(clust, library(psych))
clusterEvalQ(clust, library(gridGraphics))

#make for loop including first define train and test data, impute the train data,
#install recipes package ready for imputing test with different methods. The next
#steps after that would be to train the model and evaluate but I haven't got to 
#that yet
#---------------------------------FOR LOOP PART 2--------------------------------------

results_2 <- foreach(x = seq_along(folds),
                   .export = c("full_model_2", "meth_main_2",
                               "predictor_matrix_main_2", "m",
                               "main_impute_NA_2")) %dopar% {
                                 #Define train and test data
                                 test_id <- folds[[x]]
                                 train_data <- main_impute_NA_2[-test_id,]
                                 test_data <- main_impute_NA_2[test_id,]
                                 
                                 #Impute the train data
                                 imp_train <- mice(train_data, method = meth_main_2,
                                                   predictorMatrix = 
                                                     predictor_matrix_main_2, 
                                                   m = m, maxit = 5)
                                 
                                 lm_models <- with(imp_train, glm(formula = as.formula(full_model_2),
                                                                  family = Gamma(link = "log")))
                                 
                                 pooled_lm <- pool(lm_models)
                                 
                                 #impute test set
                                 imp_test <- mice(test_data, method = meth_main_2,
                                                  predictorMatrix = predictor_matrix_main_2, m = m, maxit = 5)
                                 
                                 #get predictions for each test set on the pooled model
                                 imputed_test_data <- complete(imp_test, "all")
                                 imputed_train_data <- complete(imp_train, "all")
                                 
                                 train_template <- imputed_train_data[[1]]
                                 train_cols <- names(train_template)
                                 
                                 imputed_train_data <- lapply(imputed_train_data, function(df) {
                                   df <- df[, train_cols]
                                   
                                   for (col in train_cols) {
                                     if (is.factor(train_template[[col]])) {
                                       df[[col]] <- factor(df[[col]], levels = levels(train_template[[col]]))
                                     }
                                   }
                                   
                                   df
                                 })
                                 
                                 imputed_test_data <- lapply(imputed_test_data, function(df) {
                                   df <- df[, train_cols]
                                   
                                   for (col in train_cols) {
                                     if (is.factor(train_template[[col]])) {
                                       df[[col]] <- factor(df[[col]], levels = levels(train_template[[col]]))
                                     }
                                   }
                                   
                                   df
                                 })
                                 
                                 fitted_models <- lm_models$analyses
                                 
                                 fitted_models <- list()
                                 
                                 #pairs_plots <- list()
                                 
                                 imp_train_data <- complete(imp_train, "all")
                                 
                                 scaled_data_list <- list()
                                 
                                 #clean_for_glm <- function(df, cols_to_scale = NULL) {
                                 #  df[] <- lapply(df, function(col) {
                                     # unlist anything wrapped as list
                                  #   if (is.list(col)) col <- unlist(col)
                                     
                                     # numeric conversion only for columns to scale
                                   #  if (!is.null(cols_to_scale) && (names(df)[which(df==col)] %in% cols_to_scale)) {
                                    #   col <- as.numeric(col)
                                     #}
                                     
                                     # keep factor as factor
                                     #if (is.character(col)) col <- as.factor(col)
                                     
                                     #col
                                  # })
                                #   df
                                 #}
                                 
                                 #cols_to_scale <- c("glucose", "sodium", "chloride", "platelets", "potassium",
                                 #                   "haemoglobin")
                                 
                                 for (i in 1:m) {
                                   completed_data <- complete(imp_train, i)
                                   numeric_cols <- names(completed_data)[sapply(completed_data, is.numeric)]
                                   
                                   # remove encoded/binary columns (0/1)
                                   cols_to_scale <- numeric_cols[
                                     sapply(completed_data[numeric_cols], function(col) length(unique(col)) > 2)
                                   ]
                                   cols_to_scale <- setdiff(cols_to_scale, "total_los")
                                   train_scaled_cont <- scale(completed_data[cols_to_scale])
                                   other_cols <- setdiff(names(completed_data), cols_to_scale)
                                   other_data <- completed_data[other_cols]
                                   train_scaled <- cbind(train_scaled_cont, other_data)
                                   
                                   
                                   #completed_data <- clean_for_glm(completed_data, cols_to_scale)
                                   #scaled_train <- completed_data
                                   #scaled_train[cols_to_scale] <- scale(completed_data[cols_to_scale])
                                   #scaled_data_list[[i]] <- scaled_train
                                   #pdf(NULL)  # invisible device
                                   #pairs.panels(completed_data)  # draw the plot
                                   #pairs_plots[[i]] <- grid.echo()  # capture it
                                   #dev.off()
                                   #pairs_plots[[i]] <- recordPlot(pairs.panels(completed_data))
                                   model <- glm(formula = full_model_formula_2,
                                                data = train_scaled,
                                                family = Gamma(link = "log"))
                                   fitted_models[[i]] <- model
                                 }
                                 
                                 # variable_importance_lm <- NULL
                                 
                                 # for (i in seq_along(fitted_models)){
                                 #  imp <- varImp(fitted_models[[i]])$importance
                                 
                                 #imp_vec <- imp[,1]
                                 
                                 #  variable_importance_lm <- cbind(variable_importance_lm, imp)
                                 #}
                                 
                                 # for (i in 1:m) {
                                 #  completed_data <- complete(imp_train, i)
                                 #   model <- lm(formula = as.formula(full_model),
                                 #              data = completed_data#,
                                 #family = Gamma(link = "log"))
                                 #  )
                                 #  fitted_models[[i]] <- model
                                 #}
                                 
                                 prediction_matrix <- matrix(, nrow = nrow(test_data), ncol = 0)
                                 
                                 #j <- 1
                                 
                                 #for (x in imputed_test_data) {
                                   #for (y in fitted_models){
                                     #centers <- as.numeric(sapply(scaled_data_list[[j]][cols_to_scale], attr, "scaled:center"))
                                     #scales  <- as.numeric(sapply(scaled_data_list[[j]][cols_to_scale], attr, "scaled:scale"))
                                    # test_scaled <- x
                                     #test_scaled[cols_to_scale] <- scale(
                                      # as.matrix(x[cols_to_scale]),
                                       #center = centers,
                                       #scale = scales
                                     #)
                                     
                                     #x[] <- lapply(x, function(col) {
                                      # if (is.list(col)) col <- unlist(col)
                                       #return(col)
                                     #})
                                     
                                     #x[cols_to_scale] <- lapply(x[cols_to_scale], as.numeric)
                                     
                                     #test_scaled[cols_to_scale] <- scale(
                                       #as.matrix(x[cols_to_scale]),
                                      # center = as.numeric(sapply(scaled_data_list[[j]][cols_to_scale], attr, "scaled:center")),
                                      # scale  = as.numeric(sapply(scaled_data_list[[j]][cols_to_scale], attr, "scaled:scale"))
                                     #)
                                     
                                     #test_scaled <- x
                                     #for (col in cols_to_scale) {
                                      # test_scaled[[col]] <- scale(
                                       #  x[[col]],
                                        # center = attr(scaled_data_list[[j]][[col]], "scaled:center"),
                                         #scale = attr(scaled_data_list[[j]][[col]], "scaled:scale")
                                      # )
                                     
                                     #test_scaled <- scale(x, center=attr(scaled_data_list[[j]], "scaled:center"), scale=attr(scaled_data_list[[j]], "scaled:scale"))
                                     #los_pred <- as.numeric(predict(y, test_scaled))#, type = "response")
                                     #prediction_matrix <- cbind(prediction_matrix, los_pred)
                                     #if(j == 5){
                                      # j <- 1
                                     #}
                                     #else{
                                      # j <- j + 1
                                     #}
                                   #}
                                 #}
                                 
                                 for (j in seq_along(imputed_test_data)) {
                                   # get the imputed dataset
                                   x <- imputed_test_data[[j]]
                                   
                                   test_scaled_cont <- scale(
                                     x[cols_to_scale],
                                     center = attr(train_scaled_cont, "scaled:center"),
                                     scale  = attr(train_scaled_cont, "scaled:scale")
                                   )
                                   
                                   test_scaled <- cbind(x[other_cols], test_scaled_cont)
                                   
                                   # clean all columns (unlist lists, fix factors, numeric)
                                   #x <- clean_for_glm(x, cols_to_scale)
                                   
                                   #  scale only the continuous numeric columns
                                   #x[cols_to_scale] <- scale(
                                    # as.matrix(x[cols_to_scale]),
                                     #center = as.numeric(sapply(scaled_data_list[[j]][cols_to_scale], attr, "scaled:center")),
                                    # scale  = as.numeric(sapply(scaled_data_list[[j]][cols_to_scale], attr, "scaled:scale"))
                                   #)
                                   
                                   #  loop over fitted models and predict
                                   for (y in fitted_models) {
                                     los_pred <- as.numeric(predict(y, x))  # now safe
                                     prediction_matrix <- cbind(prediction_matrix, los_pred)
                                   }
                                 }
                                 
                                 
                                 #print(paste("Fold:", x))
                                 #print(paste("nrow test_data:", nrow(test_data)))
                                 #print(sapply(imputed_test_data, nrow))
                                 
                                 #predictions <- rep(0, nrow(test_data))
                                 #count <- 0
                                 
                                 #for (j in imputed_test_data) {
                                 # for (y in fitted_models){
                                 #  los_pred <- as.numeric(predict(y, j))#, #type = "response")
                                 # if(length(los_pred) == nrow(test_data)){
                                 # predictions <- predictions + los_pred
                                 # count <- count + 1
                                 #}
                                 # }
                                 #}
                                 
                                 #average the predictions and average the actuals
                                 predictions <- rowMeans(prediction_matrix)
                                 
                                 #predictions <- predictions/count
                                 
                                 predictions_df <- as.data.frame(predictions) %>%
                                   rename(total_los = predictions) %>%
                                   mutate(total_los = ifelse(total_los <= 0.25, 0.25, total_los))
                                 
                                 #work out the performance metric - output
                                 lm_rmse <- mltools::rmse(preds = predictions_df$total_los,
                                                          imputed_test_data[[1]]$total_los)
                                 
                                 lm_mae <- Metrics::mae(actual = imputed_test_data[[1]]$total_los,
                                                        predicted = predictions_df$total_los)
                                 
                                 SSres <- sum((imputed_test_data[[1]]$total_los - predictions_df$total_los)^2)
                                 
                                 SStot <- sum((imputed_test_data[[1]]$total_los - mean(imputed_test_data[[1]]$total_los))^2)
                                 
                                 lm_R2 <- 1 - SSres/SStot
                                 
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
                                 
                                 
                                 #train <- complete(imp_train, 1)
                                 
                                 #y_train = train$total_los
                                 
                                 #x_train = train[, -9]
                                 
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
                                 
                                 lasso_list_R2 <- list()
                                 
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
                                   
                                   #rmse_cv <- sqrt(cv_lasso$cvm)
                                   
                                   opt_lam_lasso_rmse <- append(opt_lam_lasso_rmse, opt_lam)
                                   
                                   #rmse_cv_list <- append(rmse_cv_list, rmse_cv)
                                   
                                   opt_lam_mae <- cv_lasso_mae$lambda.min
                                   
                                   #mae_cv <- cv_lasso_mae$cvm
                                   
                                   opt_lam_lasso_mae <- append(opt_lam_lasso_mae, opt_lam_mae)
                                   
                                   #mae_cv_list <- append(mae_cv_list, mae_cv)
                                   
                                   #model_ridge_rmse <- glmnet(as.matrix(x_train), y_train, alpha = 0,
                                   #lambda = opt_lam)
                                   
                                   #model_ridge_mae <- glmnet(as.matrix(x_train), y_train, alpha = 0,
                                   #lambda = opt_lam_mae)
                                   
                                   #ridge_list_rmse[[i]] <- model_ridge_rmse
                                   
                                   #ridge_list_mae[[i]] <- model_ridge_mae
                                 }
                                 
                                 #lasso_lam_rmse_df <- data.frame(
                                 # lambda = unlist(opt_lam_lasso_rmse),
                                 # rmse = unlist(rmse_cv_list)
                                 #)
                                 
                                 #opt_ind_lasso_rmse <- which.min(lasso_lam_rmse_df$rmse)
                                 #lasso_tuned_lambda_rmse <- lasso_lam_rmse_df[opt_ind_lasso_rmse]
                                 
                                 #lam_lasso_rmse <- append(lam_lasso_rmse, median(unlist(opt_lam_lasso_rmse)))
                                 
                                 #lasso_lam_mae_df <- data.frame(
                                 # lambda = unlist(opt_lam_lasso_mae),
                                 #mae = unlist(mae_cv_list)
                                 #)
                                 
                                 #opt_ind_lasso_mae <- which.min(lasso_lam_mae_df$mae)
                                 #lasso_tuned_lambda_mae <- lasso_lam_mae_df[opt_ind_lasso_mae]
                                 
                                 #lam_lasso_mae <- append(lam_lasso_mae, median(unlist(opt_lam_lasso_mae)))
                                 
                                 for (i in seq_along(imputed_datasets)){
                                   y_train <- imputed_datasets[[i]]$total_los
                                   x_train <- imputed_datasets[[i]][, -9]
                                   
                                   model_lasso_rmse <- glmnet(as.matrix(x_train), y_train, alpha = 1,
                                                              lambda = opt_lam_lasso_rmse[[i]])
                                   
                                   model_lasso_mae <- glmnet(as.matrix(x_train), y_train, alpha = 1,
                                                             lambda = opt_lam_lasso_mae[[i]])
                                   
                                   lasso_list_rmse[[i]] <- model_lasso_rmse
                                   
                                   lasso_list_mae[[i]] <- model_lasso_mae
                                   
                                 }
                                 
                                 prediction_matrix_rmse <- matrix(, nrow = nrow(test_data), ncol = 0)
                                 
                                 prediction_matrix_mae <- matrix(, nrow = nrow(test_data), ncol = 0)
                                 
                                 #prediction_matrix_ssres <- matrix(, nrow = nrow(test_data), ncol = 0)
                                 
                                 #prediction_matrix_sstot <- matrix(, nrow = nrow(test_data), ncol = 0)
                                 
                                 for (j in imputed_test_data) {
                                   x_variables <- j[,-9]
                                   for (y in seq_along(lasso_list_rmse)){
                                     los_pred_rmse <- predict(lasso_list_rmse[[y]], as.matrix(x_variables))#, #type = "response")
                                     prediction_matrix_rmse <- cbind(prediction_matrix_rmse, los_pred_rmse)
                                     
                                     #lasso_ssres <- sum((imputed_test_data[[1]]$total_los - los_pred_rmse)^2)
                                     #lasso_sstot <- sum((imputed_test_data[[1]]$total_los - mean(imputed_test_data[[1]]))^2)
                                     #prediction_matrix_ssres <- cbind(prediction_matrix_ssres, lasso_ssres)
                                     #prediction_matrix_sstot <- cbind(prediction_matrix_sstot, lasso_sstot)
                                   }
                                 }
                                 
                                 for (j in imputed_test_data) {
                                   x_variables <- j[,-9]
                                   for (y in seq_along(lasso_list_mae)){
                                     los_pred_mae <- predict(lasso_list_mae[[y]], as.matrix(x_variables))#, #type = "response")
                                     prediction_matrix_mae <- cbind(prediction_matrix_mae, los_pred_mae)
                                   }
                                 }
                                 
                                 lasso_ssres <- colSums((imputed_test_data[[1]]$total_los - prediction_matrix_rmse)^2)
                                 
                                 lasso_sstot <- sum((imputed_test_data[[1]]$total_los - mean(imputed_test_data[[1]]$total_los))^2)
                                 
                                 predictions_rmse <- rowMeans(prediction_matrix_rmse)
                                 
                                 predictions_mae <- rowMeans(prediction_matrix_mae)
                                 
                                 #predictions_ssres <- rowMeans(prediction_matrix_ssres)
                                 
                                 #predictions_sstot <- rowMeans(prediction_matrix_sstot)
                                 
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
                                 
                                 lasso_R2_values <- 1 - lasso_ssres/lasso_sstot
                                 
                                 lasso_R2 <- mean(lasso_R2_values)
                                 
                                 #lasso_rmse_list <- append(lasso_rmse_list, lasso_rmse)
                                 
                                 #lasso_mae_list <- append(lasso_mae_list, lasso_mae)
                                 
                                 #lasso_rmse_df <- as.data.frame(lasso_rmse_list)
                                 
                                 #lasso_mae_df <- as.data.frame(lasso_mae_list)
                                 
                                 
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
                                   
                                   #rmse_cv <- sqrt(cv_ridge$cvm)
                                   
                                   opt_lam_ridge_rmse <- append(opt_lam_ridge_rmse, opt_lam)
                                   
                                   #rmse_cv_list <- append(rmse_cv_list, rmse_cv)
                                   
                                   opt_lam_mae <- cv_ridge_mae$lambda.min
                                   
                                   #mae_cv <- cv_ridge_mae$cvm
                                   
                                   opt_lam_ridge_mae <- append(opt_lam_ridge_mae, opt_lam_mae)
                                   
                                   #mae_cv_list <- append(mae_cv_list, mae_cv)
                                   
                                   #model_ridge_rmse <- glmnet(as.matrix(x_train), y_train, alpha = 0,
                                   #lambda = opt_lam)
                                   
                                   #model_ridge_mae <- glmnet(as.matrix(x_train), y_train, alpha = 0,
                                   #lambda = opt_lam_mae)
                                   
                                   #ridge_list_rmse[[i]] <- model_ridge_rmse
                                   
                                   #ridge_list_mae[[i]] <- model_ridge_mae
                                 }
                                 
                                 #ridge_lam_rmse_df <- data.frame(
                                 #lambda = unlist(opt_lam_ridge_rmse),
                                 # rmse = unlist(rmse_cv_list)
                                 #)
                                 
                                 #opt_ind_ridge_rmse <- which.min(ridge_lam_rmse_df$rmse)
                                 #ridge_tuned_lambda_rmse <- ridge_lam_rmse_df$lambda[opt_ind_ridge_rmse]
                                 
                                 #lam_ridge_rmse <- append(lam_ridge_rmse, median(unlist(opt_lam_ridge_rmse)))
                                 
                                 #ridge_lam_mae_df <- data.frame(
                                 #lambda = unlist(opt_lam_ridge_mae),
                                 # mae = unlist(mae_cv_list)
                                 #)
                                 
                                 #opt_ind_ridge_mae <- which.min(ridge_lam_mae_df$mae)
                                 #ridge_tuned_lambda_mae <- ridge_lam_mae_df$lambda[opt_ind_ridge_mae]
                                 
                                 #lam_ridge_mae <- append(lam_ridge_mae, median(unlist(opt_lam_ridge_mae)))
                                 
                                 for (i in seq_along(imputed_datasets)){
                                   y_train <- imputed_datasets[[i]]$total_los
                                   x_train <- imputed_datasets[[i]][, -9]
                                   
                                   model_ridge_rmse <- glmnet(as.matrix(x_train), y_train, alpha = 0,
                                                              lambda = opt_lam_ridge_rmse[[i]])
                                   
                                   model_ridge_mae <- glmnet(as.matrix(x_train), y_train, alpha = 0,
                                                             lambda = opt_lam_ridge_mae[[i]])
                                   
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
                                 }
                                 
                                 for (x in imputed_test_data) {
                                   x_variables <- x[,-9]
                                   for (y in seq_along(ridge_list_mae)){
                                     los_pred_mae <- predict(ridge_list_mae[[y]], as.matrix(x_variables))#, #type = "response")
                                     prediction_matrix_mae <- cbind(prediction_matrix_mae, los_pred_mae)
                                   }
                                 }
                                 
                                 ridge_ssres <- colSums((imputed_test_data[[1]]$total_los - prediction_matrix_rmse)^2)
                                 
                                 ridge_sstot <- sum((imputed_test_data[[1]]$total_los - mean(imputed_test_data[[1]]$total_los))^2)
                                 
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
                                 
                                 ridge_R2_values <- 1 - ridge_ssres/ridge_sstot
                                 
                                 ridge_R2 <- mean(ridge_R2_values)
                                 
                                 #ridge_rmse_list <- append(ridge_rmse_list, ridge_rmse)
                                 
                                 #ridge_mae_list <- append(ridge_mae_list, ridge_mae)
                                 
                                 #ridge_rmse_df <- as.data.frame(ridge_rmse_list)
                                 
                                 #ridge_mae_df <- as.data.frame(ridge_mae_list)
                                 
                                 #model_ridge <- glmnet(x_train, y_train, alpha = 0)
                                 
                                 #for(lam in lambdagrid ) {
                                 # model_ridge2 <- glmnet(x_train, y_train, alpha = 0, lambda = lam)
                                 
                                 #predict on train
                                 #predict on test
                                 
                                 #confusion matricies
                                 #}
                                 
                                 #Random Forest
                                 
                                 mtry_range <- c(2, 3, 4, 5, 6, 10)
                                 
                                 rfm_list_rmse <- list()
                                 
                                 rfm_list_mae <- list()
                                 
                                 rfm_rmse_list <- list()
                                 
                                 rfm_mae_list <- list()
                                 
                                 #for (try in seq_along(mtry_range)) {
                                 #for (i in seq_along(imputed_datasets)){
                                 
                                 #y_train <- imputed_datasets[[i]]$total_los
                                 #x_train <- imputed_datasets[[i]][, -9]
                                 
                                 #rfm <- randomForest( total_los ~ .,
                                 #                    mtry = mtry_range[try],
                                 #                     ntree = 200,
                                 #                     data = imputed_datasets[[i]])
                                 
                                 # rfm_list[[i]] <- rfm
                                 #}
                                 
                                 # prediction_matrix <- matrix(, nrow = nrow(test_data), ncol = 0)
                                 
                                 #  for (x in imputed_test_data) {
                                 #for (y in seq_along(rfm_list)){
                                 # los_pred <- predict(rfm_list[[y]], as.matrix(x))
                                 #  prediction_matrix <- cbind(prediction_matrix, los_pred)
                                 # }
                                 #}
                                 
                                 # predictions <- rowMeans(prediction_matrix)
                                 
                                 #  predictions_df <- as.data.frame(predictions) %>%
                                 #rename(total_los = predictions) %>%
                                 # mutate(total_los = ifelse(total_los <= 0.25, 0.25, total_los))
                                 
                                 #rfm_rmse <- mltools::rmse(preds = predictions_df$total_los,
                                 #                 imputed_test_data[[1]]$total_los)
                                 
                                 #rfm_mae <- Metrics::mae(actual = imputed_test_data[[1]]$total_los,
                                 #              predicted = predictions_df$total_los)
                                 
                                 #rfm_rmse_list <- append(rfm_rmse_list, rfm_rmse)
                                 
                                 #rfm_mae_list <- append(rfm_mae_list, rfm_mae)
                                 #}
                                 
                                 #rfm_rmse_df <- as.data.frame(rfm_rmse_list)
                                 
                                 #rfm_mae_df <- as.data.frame(rfm_mae_list)
                                 
                                 #colnames(rfm_rmse_df) <- c("2", "3", "4", "5", "6", "10")
                                 
                                 #colnames(rfm_mae_df) <- c("2", "3", "4", "5", "6", "10")
                                 
                                 opt_mtry_list_rmse <- list()
                                 
                                 opt_mtry_list_mae <- list()
                                 
                                 opt_rmse_list <- list()
                                 
                                 opt_mae_list <- list()
                                 
                                 
                                 cv_method <- trainControl(method = "cv", number = 5)
                                 
                                 #rmse
                                 for (i in seq_along(imputed_datasets)){
                                   model_rmse <- caret::train(total_los ~ .,
                                                              method = "rf",
                                                              trControl = cv_method,
                                                              data = imputed_datasets[[i]],
                                                              tuneGrid = expand.grid(mtry = mtry_range),
                                                              metric = "RMSE")
                                   
                                   model_mae <- caret::train(total_los ~ .,
                                                             method = "rf",
                                                             trControl = cv_method,
                                                             data = imputed_datasets[[i]],
                                                             tuneGrid = expand.grid(mtry = mtry_range),
                                                             metric = "MAE")
                                   
                                   #opt_mtry_rmse <- model_rmse$finalModel$mtry
                                   
                                   #opt_rmse <- model_rmse$results$RMSE[model_rmse$results$mtry 
                                   # == model_rmse$bestTune$mtry]
                                   
                                   #opt_mtry_mae <- model_mae$finalModel$mtry
                                   
                                   #opt_mae <- model_mae$results$MAE[model_mae$results$mtry 
                                   # == model_mae$bestTune$mtry]
                                   
                                   
                                   #opt_mtry_list_rmse <- append(opt_mtry_list_rmse, opt_mtry_rmse)
                                   
                                   #opt_mtry_list_mae <- append(opt_mtry_list_mae, opt_mtry_mae)
                                   
                                   opt_mtry_list_rmse[[i]] <- model_rmse$bestTune$mtry
                                   
                                   opt_mtry_list_mae[[i]] <- model_mae$bestTune$mtry
                                   
                                   #opt_rmse_list <- append(opt_rmse_list, opt_rmse)
                                   
                                   #opt_mae_list <- append(opt_mae_list, opt_mae)
                                 }
                                 
                                 #rfm_mtry_rmse_df <- data.frame(
                                 # mtry = unlist(opt_mtry_list_rmse),
                                 #rmse = unlist(opt_rmse_list)
                                 #)
                                 
                                 #opt_ind_rfm_rmse <- which.min(rfm_mtry_rmse_df$rmse)
                                 #rfm_tuned_mtry_rmse <- rfm_mtry_rmse_df$mtry[opt_ind_rfm_rmse]
                                 
                                 #mtry_rfm_rmse <- append(mtry_rfm_rmse, floor(median(unlist(opt_mtry_list_rmse))))
                                 
                                 #rfm_mtry_mae_df <- data.frame(
                                 # mtry = unlist(opt_mtry_list_mae),
                                 # mae = unlist(opt_mae_list)
                                 # )
                                 
                                 # opt_ind_rfm_mae <- which.min(rfm_mtry_mae_df$mae)
                                 #rfm_tuned_mtry_mae <- rfm_mtry_mae_df$lambda[opt_ind_mtry_mae]
                                 
                                 #mtry_rfm_mae <- append(mtry_rfm_mae, floor(median(unlist(opt_mtry_list_rmse))))
                                 
                                 for (i in seq_along(imputed_datasets)){
                                   
                                   #y_train <- imputed_datasets[[i]]$total_los
                                   #x_train <- imputed_datasets[[i]][, -9]
                                   
                                   rfm_rmse <- randomForest( total_los ~ .,
                                                             mtry = opt_mtry_list_rmse[[i]],
                                                             ntree = 200,
                                                             data = imputed_datasets[[i]],
                                                             importance = TRUE)
                                   
                                   rfm_list_rmse[[i]] <- rfm_rmse
                                   
                                   rfm_mae <- randomForest( total_los ~ .,
                                                            mtry = opt_mtry_list_mae[[i]],
                                                            ntree = 200,
                                                            data = imputed_datasets[[i]],
                                                            importance = TRUE)
                                   
                                   rfm_list_mae[[i]] <- rfm_mae
                                 }
                                 
                                 prediction_matrix_rmse <- matrix(, nrow = nrow(test_data), ncol = 0)
                                 
                                 #variable_importance <- sapply(rfm_rmse_list, function(m) {
                                 # importance(m)[, "IncNodePurity"]   # or "%IncMSE" if you prefer
                                 #})
                                 
                                 #variable_importance <- sapply(rfm_list_rmse, function(model) {
                                 # varImp(model)$importance[,1]  # take the first column (Overall)
                                 #})
                                 
                                 variable_importance <- NULL
                                 
                                 for (i in seq_along(rfm_list_rmse)){
                                   imp <- importance(rfm_list_rmse[[i]])[,"%IncMSE"]
                                   
                                   variable_importance <- cbind(variable_importance, imp)
                                 }
                                 
                                 for (x in imputed_test_data) {
                                   for (y in seq_along(rfm_list_rmse)){
                                     #los_pred_rmse <- predict(rfm_list_rmse[[y]], as.matrix(x))
                                     los_pred_rmse <- predict(rfm_list_rmse[[y]], x)
                                     prediction_matrix_rmse <- cbind(prediction_matrix_rmse, los_pred_rmse)
                                   }
                                 }
                                 
                                 rfm_ssres <- colSums((imputed_test_data[[1]]$total_los - prediction_matrix_rmse)^2)
                                 
                                 rfm_sstot <- sum((imputed_test_data[[1]]$total_los - mean(imputed_test_data[[1]]$total_los))^2)
                                 
                                 predictions_rmse <- rowMeans(prediction_matrix_rmse)
                                 
                                 predictions_rmse_df <- as.data.frame(predictions_rmse) %>%
                                   rename(total_los = predictions_rmse) %>%
                                   mutate(total_los = ifelse(total_los <= 0.25, 0.25, total_los))
                                 
                                 prediction_matrix_mae <- matrix(, nrow = nrow(test_data), ncol = 0)
                                 
                                 for (x in imputed_test_data) {
                                   for (y in seq_along(rfm_list_mae)){
                                     #los_pred_mae <- predict(rfm_list_mae[[y]], as.matrix(x))
                                     los_pred_mae <- predict(rfm_list_mae[[y]], x)
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
                                 
                                 rfm_R2_values <- 1 - rfm_ssres/rfm_sstot
                                 
                                 rfm_R2 <- mean(rfm_R2_values)
                                 
                                 #rfm_rmse_list <- append(rfm_rmse_list, rfm_rmse)
                                 
                                 #rfm_mae_list <- append(rfm_mae_list, rfm_mae)
                                 
                                 #rfm_rmse_df <- as.data.frame(rfm_rmse_list)
                                 
                                 #rfm_mae_df <- as.data.frame(rfm_mae_list)
                                 
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
                                      lm_R2 = lm_R2,
                                      lasso_rmse = lasso_rmse,
                                      lasso_mae = lasso_mae,
                                      lasso_R2 = lasso_R2,
                                      ridge_rmse = ridge_rmse,
                                      ridge_mae = ridge_mae,
                                      ridge_R2 = ridge_R2,
                                      random_forest_rmse = rfm_rmse,
                                      random_forest_mae = rfm_mae,
                                      random_forest_R2 = rfm_R2,
                                      variable_importance = variable_importance,
                                      #variable_importance_lm = variable_importance_lm,
                                      train_data = imp_train_data,
                                      lm_models = fitted_models#,
                                      #imputed_datasets = imputed_datasets,
                                      #imputed_test_data = imputed_test_data
                                      #   xgboost_single = single_tree,
                                      #   xgboost = xgboost_multi
                                 )
                                 
                               }

stopCluster(clust)

plot_model(results_2[[1]]$lm_models[[1]], show.values = TRUE, value.offset = .3)

var_imp_2 <- NULL

for(i in seq_along(results_2)) {
  
  # Extract the variable importance matrix/vector
  imp <- results[[i]]$variable_importance  # should be a matrixx or data frame
  
  # Compute row means if it's multi-column, otherwise keep as is
  if(is.matrix(imp) || ncol(imp) > 1){
    imp_vec <- rowMeans(as.matrix(imp))
  } else {
    imp_vec <- as.numeric(imp)
  }
  
  # Make sure it's a column matrix and keep row names
  imp_df <- data.frame(imp_vec)
  rownames(imp_df) <- rownames(imp)
  
  # Bind as new column
  var_imp_2 <- cbind(var_imp_2, imp_df)
}


#for(i in seq_along(results)){
# imp_df <- results[[i]]$variable_importance %>%
#as.data.frame() %>%
#rowMeans() %>%
#as.data.frame()

#var_imp_2 <- cbind(var_imp_2, imp_df)
#}



var_imp <- results_2[[1]]$variable_importance %>%
  as.data.frame %>%
  rowMeans() %>%
  as.data.frame() #%>%
#  tibble::rownames_to_column("var") %>%
# rename("importance" = ".") %>%
#arrange(importance)

for(i in seq_along(results)){
  if(i==1){
    var_imp <- var_imp
  }
  else {
    var_imp_1 <- results[[i]]$variable_importance %>%
      as.data.frame %>%
      rowMeans() %>%
      as.data.frame()
    
    var_imp <- cbind(var_imp, var_imp_1$.)
  }
}

var_imp <- var_imp %>%
  rowMeans() %>%
  as.data.frame() %>%
  tibble::rownames_to_column("var") %>%
  rename("importance" = ".") %>%
  arrange(importance)

var_imp$var<- var_imp$var %>% as.factor()
var_imp$var <- fct_inorder(var_imp$var)

imp_bar <- ggplot(data = var_imp) + 
  geom_bar(
    stat = "identity",#it leaves the data without count and bin
    mapping = aes(x = var, y=importance, fill = var), 
    show.legend = FALSE,
    width = 1
  ) + 
  labs(x = NULL, y = NULL)
imp_bar + coord_polar() + theme_minimal()
imp_bar + coord_flip() + theme_minimal()


pairs_plots <- vector("list", 25)
i <- 1

for(x in seq_along(results)) {
  for(y in seq_along(results[[x]]$train_data)){
    comp_data <- results[[x]]$train_data[[y]][, 
                                              sapply(results[[x]]$train_data[[y]], function(x) sd(x, na.rm = TRUE) > 0)
    ]
    #pdf(NULL)  # invisible device
    #pairs.panels(comp_data)  # draw the plot
    #pairs_plots[[i]] <- recordPlot()  # capture it
    #dev.off()
    #pairs_plots[[i]] <- recordPlot(pairs.panels(completed_data)) 
    file <- paste0("pairs_plot_", x, "_", y, ".pdf")
    
    pdf(file)
    pairs.panels(comp_data)
    dev.off()
    
    pairs_plots[[i]] <- file
    
    i <- i + 1
  }
}


#----------------------------PREDICTION MODELS PART 3--------------------------


main_impute_3 <- main_summary %>%
  select("age", "sex","admission_type", "ethnicity", "insurance", #"previous_stay",
         "platelets", "glucose", "chloride", "potassium", "haemoglobin",
         "sodium", "ondansetron", "calcium_gluconate", 
         #"hospital_expire_flag", 
         "total_los") %>%
  mutate(#previous_stay = factor(previous_stay),
    ondansetron = factor(ondansetron),
    calcium_gluconate = factor(calcium_gluconate),
    #hospital_expire_flag = factor(hospital_expire_flag),
    insurance = na_if(insurance, "Missing"),
    insurance = droplevels(insurance),
    ethnicity = na_if(ethnicity, "Missing"),
    ethnicity = factor(ethnicity),
    ethnicity = droplevels(ethnicity))


main_impute_3 %>% 
  select("potassium", "sodium", "glucose", "haemoglobin",
         "chloride", "platelets", "insurance", "ethnicity") %>%
  
  md.pattern(rotate.names = TRUE)

main_impute_3$sex <- factor(main_impute_3$sex)
main_impute_3$admission_type <- factor(main_impute_3$admission_type)

imp_3 <- mice(main_impute_3, method = "mean", m = 5, maxit = 5)

meth_3 <- imp_3$method


meth_3["ethnicity"] <- "pmm"
meth_3["insurance"] <- "pmm"
meth_3["admission_type"] <- ""
meth_3["sex"] <- ""
meth_3["age"] <- "norm"
meth_3["ondansetron"] <- "logreg"
meth_3["calcium_gluconate"] <- "logreg"
#meth_2["hospital_expire_flag"] <- "logreg"
meth_3["total_los"] <- ""
#meth_2["previous_stay"] <- "polyreg"
meth_3["glucose"] <- "pmm"
meth_3["sodium"] <- "pmm"
meth_3["chloride"] <- "pmm"
meth_3["platelets"] <- "pmm"
meth_3["potassium"] <- "pmm"
meth_3["haemoglobin"] <- "pmm"
#meth["haematocrit"] <- "pmm"

imp_3 <- mice(main_impute_3, method = meth_3, m = 15, maxit = 15)

plot(imp_3, vars = "haemoglobin")

completed_data_3 <- complete(imp_3, 2)

hist(completed_data_3$sodium)

main_impute.cor_3 = main_impute_3 %>% 
  select(c("glucose","sodium","chloride", "platelets", "potassium",
           "haemoglobin")) %>%
  cor(use = "complete.obs")

corrplot(main_impute.cor_3, tl.col = "black")

predictor_matrix_3 <- imp_3$predictorMatrix

#plot imp
plot(imp_3)
#ggmice::plot_trace(imp, "haemoglobin", legend = FALSE)
#ggmice::plot_trace(imp, "haematocrit", legend = FALSE)

#change predictor matrix for haematocrit and haemoglobin to 0
#predictor_matrix["haematocrit","haemoglobin"] <- 0
#predictor_matrix["haemoglobin","haematocrit"] <- 0

#make imp2 with new predictor matrix
#imp2 <- mice(main_impute, method = meth, predictorMatrix = predictor_matrix,
#            m = 15, maxit = 15)

#plot imp2
#plot(imp2)
#ggmice::plot_trace(imp2, "haemoglobin", legend = FALSE)
#ggmice::plot_trace(imp2, "haematocrit", legend = FALSE)

main_impute_NA_3 <- main_impute_3 %>%
  mutate(ethnicity = ethnicity %>% 
           addNA() %>%
           fct_explicit_na(na_level = "NA") %>%
           relevel(ref = "NA"),
         insurance = insurance %>%
           addNA() %>%
           fct_explicit_na(na_level = "NA") %>%
           relevel(ref = "NA"),
  )

#main_dummy_data_2 <- recipe(total_los ~ ., data = main_impute_NA_2) %>%
 # step_dummy(all_nominal(), one_hot = FALSE) %>%
 # prep() %>%
#  bake(new_data = NULL)

#imp_dummy_2 <- mice(main_dummy_data_2, m = 5, maxit = 5)

imp_main_3 <- mice(main_impute_NA_3, m = 5, maxit = 5)

imp_main_3$method

#View(imp_dummy_3$predictorMatrix)

predictor_matrix_main_3 <- imp_main_3$predictorMatrix

#predictor_matrix_dummy["haematocrit","haemoglobin"] <- 0
#predictor_matrix_dummy["haemoglobin","haematocrit"] <- 0

meth_main_3 <- imp_main_3$method

variables_3 <- paste(names(main_impute_NA_3[,-14]), collapse = " + ")

full_model_3 <- paste(c("total_los", variables_3), collapse = " ~ ")

full_model_log_3 <- paste(c("log(total_los)", variables_3), collapse = " ~ ")

full_model_formula_3 <- as.formula(full_model_3)

#create cross validation folds
set.seed(123)

folds <- createFolds(main_impute_NA_3$total_los, 5)

#number of rounds of imputations
m <- 5

lam_ridge_rmse_3 <- list()

lam_ridge_mae_3 <- list()

lam_lasso_rmse_3 <- list()

lam_lasso_mae_3 <- list()

mtry_rfm_rmse_3 <- list()

mtry_rfm_mae_3 <- list()

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
clusterEvalQ(clust, library(caret))
clusterEvalQ(clust, library(psych))
clusterEvalQ(clust, library(gridGraphics))

#make for loop including first define train and test data, impute the train data,
#install recipes package ready for imputing test with different methods. The next
#steps after that would be to train the model and evaluate but I haven't got to 
#that yet
#---------------------------------FOR LOOP PART 3--------------------------------------

results_3 <- foreach(x = seq_along(folds),
                     .export = c("full_model_3", "meth_main_3",
                                 "predictor_matrix_main_3", "m",
                                 "main_impute_NA_3")) %dopar% {
                                   #Define train and test data
                                   test_id <- folds[[x]]
                                   train_data <- main_impute_NA_3[-test_id,]
                                   test_data <- main_impute_NA_3[test_id,]
                                   
                                   #Impute the train data
                                   imp_train <- mice(train_data, method = meth_main_3,
                                                     predictorMatrix = 
                                                       predictor_matrix_main_3, 
                                                     m = m, maxit = 5)
                                   
                                   lm_models <- with(imp_train, glm(formula = as.formula(full_model_3),
                                                                    family = Gamma(link = "log")))
                                   
                                   pooled_lm <- pool(lm_models)
                                   
                                   #impute test set
                                   #imp_test <- mice(test_data, method = meth_main_3,
                                                    #predictorMatrix = predictor_matrix_main_3, m = m, maxit = 5)
                                   
                                   imp_test  <- mice.mids(imp_train, newdata = test_data)
                                   
                                   #get predictions for each test set on the pooled model
                                   imputed_test_data <- complete(imp_test, "all")
                                   imputed_train_data <- complete(imp_train, "all")
                                   
                                   train_template <- imputed_train_data[[1]]
                                   train_cols <- names(train_template)
                                   
                                   imputed_train_data <- lapply(imputed_train_data, function(df) {
                                     df <- df[, train_cols]
                                     
                                     for (col in train_cols) {
                                       if (is.factor(train_template[[col]])) {
                                         df[[col]] <- factor(df[[col]], levels = levels(train_template[[col]]))
                                       }
                                     }
                                     
                                     df
                                   })
                                   
                                   imputed_test_data <- lapply(imputed_test_data, function(df) {
                                     df <- df[, train_cols]
                                     
                                     for (col in train_cols) {
                                       if (is.factor(train_template[[col]])) {
                                         df[[col]] <- factor(df[[col]], levels = levels(train_template[[col]]))
                                       }
                                     }
                                     
                                     df
                                   })
                                   
                                   fitted_models <- lm_models$analyses
                                   
                                   fitted_models <- list()
                                   
                                   #pairs_plots <- list()
                                   
                                   imp_train_data <- complete(imp_train, "all")
                                   
                                   scaled_data_list <- list()
                                   
                                   #clean_for_glm <- function(df, cols_to_scale = NULL) {
                                   #  df[] <- lapply(df, function(col) {
                                   # unlist anything wrapped as list
                                   #   if (is.list(col)) col <- unlist(col)
                                   
                                   # numeric conversion only for columns to scale
                                   #  if (!is.null(cols_to_scale) && (names(df)[which(df==col)] %in% cols_to_scale)) {
                                   #   col <- as.numeric(col)
                                   #}
                                   
                                   # keep factor as factor
                                   #if (is.character(col)) col <- as.factor(col)
                                   
                                   #col
                                   # })
                                   #   df
                                   #}
                                   
                                   #cols_to_scale <- c("glucose", "sodium", "chloride", "platelets", "potassium",
                                   #                   "haemoglobin")
                                   
                                   for (i in 1:m) {
                                     completed_data <- complete(imp_train, i)
                                     numeric_cols <- names(completed_data)[sapply(completed_data, is.numeric)]
                                     
                                     # remove encoded/binary columns (0/1)
                                     cols_to_scale <- numeric_cols[
                                       sapply(completed_data[numeric_cols], function(col) length(unique(col)) > 2)
                                     ]
                                     cols_to_scale <- setdiff(cols_to_scale, "total_los")
                                     train_scaled_cont <- scale(completed_data[cols_to_scale])
                                     other_cols <- setdiff(names(completed_data), cols_to_scale)
                                     other_data <- completed_data[other_cols]
                                     train_scaled <- cbind(train_scaled_cont, other_data)
                                     
                                     
                                     #completed_data <- clean_for_glm(completed_data, cols_to_scale)
                                     #scaled_train <- completed_data
                                     #scaled_train[cols_to_scale] <- scale(completed_data[cols_to_scale])
                                     #scaled_data_list[[i]] <- scaled_train
                                     #pdf(NULL)  # invisible device
                                     #pairs.panels(completed_data)  # draw the plot
                                     #pairs_plots[[i]] <- grid.echo()  # capture it
                                     #dev.off()
                                     #pairs_plots[[i]] <- recordPlot(pairs.panels(completed_data))
                                     model <- glm(formula = full_model_formula_3,
                                                  data = train_scaled,
                                                  family = Gamma(link = "log"))
                                     fitted_models[[i]] <- model
                                   }
                                   
                                   # variable_importance_lm <- NULL
                                   
                                   # for (i in seq_along(fitted_models)){
                                   #  imp <- varImp(fitted_models[[i]])$importance
                                   
                                   #imp_vec <- imp[,1]
                                   
                                   #  variable_importance_lm <- cbind(variable_importance_lm, imp)
                                   #}
                                   
                                   # for (i in 1:m) {
                                   #  completed_data <- complete(imp_train, i)
                                   #   model <- lm(formula = as.formula(full_model),
                                   #              data = completed_data#,
                                   #family = Gamma(link = "log"))
                                   #  )
                                   #  fitted_models[[i]] <- model
                                   #}
                                   
                                   prediction_matrix <- matrix(, nrow = nrow(test_data), ncol = 0)
                                   
                                   #j <- 1
                                   
                                   #for (x in imputed_test_data) {
                                   #for (y in fitted_models){
                                   #centers <- as.numeric(sapply(scaled_data_list[[j]][cols_to_scale], attr, "scaled:center"))
                                   #scales  <- as.numeric(sapply(scaled_data_list[[j]][cols_to_scale], attr, "scaled:scale"))
                                   # test_scaled <- x
                                   #test_scaled[cols_to_scale] <- scale(
                                   # as.matrix(x[cols_to_scale]),
                                   #center = centers,
                                   #scale = scales
                                   #)
                                   
                                   #x[] <- lapply(x, function(col) {
                                   # if (is.list(col)) col <- unlist(col)
                                   #return(col)
                                   #})
                                   
                                   #x[cols_to_scale] <- lapply(x[cols_to_scale], as.numeric)
                                   
                                   #test_scaled[cols_to_scale] <- scale(
                                   #as.matrix(x[cols_to_scale]),
                                   # center = as.numeric(sapply(scaled_data_list[[j]][cols_to_scale], attr, "scaled:center")),
                                   # scale  = as.numeric(sapply(scaled_data_list[[j]][cols_to_scale], attr, "scaled:scale"))
                                   #)
                                   
                                   #test_scaled <- x
                                   #for (col in cols_to_scale) {
                                   # test_scaled[[col]] <- scale(
                                   #  x[[col]],
                                   # center = attr(scaled_data_list[[j]][[col]], "scaled:center"),
                                   #scale = attr(scaled_data_list[[j]][[col]], "scaled:scale")
                                   # )
                                   
                                   #test_scaled <- scale(x, center=attr(scaled_data_list[[j]], "scaled:center"), scale=attr(scaled_data_list[[j]], "scaled:scale"))
                                   #los_pred <- as.numeric(predict(y, test_scaled))#, type = "response")
                                   #prediction_matrix <- cbind(prediction_matrix, los_pred)
                                   #if(j == 5){
                                   # j <- 1
                                   #}
                                   #else{
                                   # j <- j + 1
                                   #}
                                   #}
                                   #}
                                   
                                   #for (j in seq_along(imputed_test_data)) {
                                     # get the imputed dataset
                                    # x <- imputed_test_data[[j]]
                                     
                                     #test_scaled_cont <- scale(
                                      # x[cols_to_scale],
                                       #center = attr(train_scaled_cont, "scaled:center"),
                                       #scale  = attr(train_scaled_cont, "scaled:scale")
                                     #)
                                     
                                     #test_scaled <- cbind(x[other_cols], test_scaled_cont)
                                     
                                     # clean all columns (unlist lists, fix factors, numeric)
                                     #x <- clean_for_glm(x, cols_to_scale)
                                     
                                     #  scale only the continuous numeric columns
                                     #x[cols_to_scale] <- scale(
                                     # as.matrix(x[cols_to_scale]),
                                     #center = as.numeric(sapply(scaled_data_list[[j]][cols_to_scale], attr, "scaled:center")),
                                     # scale  = as.numeric(sapply(scaled_data_list[[j]][cols_to_scale], attr, "scaled:scale"))
                                     #)
                                     
                                     #  loop over fitted models and predict
                                     #for (y in fitted_models) {
                                      # los_pred <- as.numeric(predict(y, x))  # now safe
                                     #  prediction_matrix <- cbind(prediction_matrix, los_pred)
                                    # }
                                   #}
                                   
                                   prediction_list_3 <- list()
                                   
                                   for (j in seq_along(imputed_test_data)) {
                                     
                                     # get test data
                                     x <- imputed_test_data[[j]]
                                     
                                     # scale correctly
                                     test_scaled_cont <- scale(
                                       x[cols_to_scale],
                                       center = attr(train_scaled_cont, "scaled:center"),
                                       scale  = attr(train_scaled_cont, "scaled:scale")
                                     )
                                     
                                     test_scaled <- cbind(x[other_cols], test_scaled_cont)
                                     
                                     # matched model
                                     model <- fitted_models[[j]]
                                     
                                     # predict (IMPORTANT: use test_scaled)
                                     los_pred <- as.numeric(predict(model, test_scaled))
                                     
                                     prediction_list_3[[j]] <- los_pred
                                   }
                                   
                                   # combine predictions
                                   predictions <- Reduce("+", prediction_list_3) / length(prediction_list_3)
                                   
                                   #print(paste("Fold:", x))
                                   #print(paste("nrow test_data:", nrow(test_data)))
                                   #print(sapply(imputed_test_data, nrow))
                                   
                                   #predictions <- rep(0, nrow(test_data))
                                   #count <- 0
                                   
                                   #for (j in imputed_test_data) {
                                   # for (y in fitted_models){
                                   #  los_pred <- as.numeric(predict(y, j))#, #type = "response")
                                   # if(length(los_pred) == nrow(test_data)){
                                   # predictions <- predictions + los_pred
                                   # count <- count + 1
                                   #}
                                   # }
                                   #}
                                   
                                   #average the predictions and average the actuals
                                   #predictions <- rowMeans(prediction_matrix)
                                   
                                   #predictions <- predictions/count
                                   
                                   predictions_df <- as.data.frame(predictions) %>%
                                     rename(total_los = predictions) %>%
                                     mutate(total_los = ifelse(total_los <= 0.25, 0.25, total_los))
                                   
                                   #work out the performance metric - output
                                   #lm_rmse <- mltools::rmse(preds = predictions_df$total_los,
                                                            #imputed_test_data[[1]]$total_los)
                                   
                                   rmse_list_lm <- numeric(length(prediction_list_3))
                                   
                                   for (i in seq_along(prediction_list_3)) {
                                     rmse_list_lm[i] <- mltools::rmse(
                                       preds = prediction_list_3[[i]],
                                       actuals = imputed_test_data[[i]]$total_los
                                     )
                                   }
                                   
                                   lm_rmse <- mean(rmse_list_lm)
                                   
                                   #lm_mae <- Metrics::mae(actual = imputed_test_data[[1]]$total_los,
                                                          #predicted = predictions_df$total_los)
                                   
                                   mae_list_lm <- numeric(length(prediction_list_3))
                                   
                                   for (i in seq_along(prediction_list_3)) {
                                     mae_list_lm[i] <- Metrics::mae(
                                       predicted = prediction_list_3[[i]],
                                       actual = imputed_test_data[[i]]$total_los
                                     )
                                   }
                                   
                                   lm_mae <- mean(mae_list_lm)
                                   
                                   #SSres <- sum((imputed_test_data[[1]]$total_los - predictions_df$total_los)^2)
                                   
                                   #SStot <- sum((imputed_test_data[[1]]$total_los - mean(imputed_test_data[[1]]$total_los))^2)
                                   
                                   #lm_R2 <- 1 - SSres/SStot
                                   
                                   r2_list_lm <- sapply(seq_along(prediction_list_3), function(i) {
                                     
                                     y_true <- imputed_test_data[[i]]$total_los
                                     y_pred <- prediction_list_3[[i]]
                                     
                                     ss_res <- sum((y_true - y_pred)^2)
                                     ss_tot <- sum((y_true - mean(y_true))^2)
                                     
                                     1 - ss_res / ss_tot
                                   })
                                   
                                   lm_R2 <- mean(r2_list_lm)
                                   
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
                                   
                                   
                                   #train <- complete(imp_train, 1)
                                   
                                   #y_train = train$total_los
                                   
                                   #x_train = train[, -9]
                                   
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
                                   
                                   lasso_list_R2 <- list()
                                   
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
                                     
                                     #rmse_cv <- sqrt(cv_lasso$cvm)
                                     
                                     opt_lam_lasso_rmse <- append(opt_lam_lasso_rmse, opt_lam)
                                     
                                     #rmse_cv_list <- append(rmse_cv_list, rmse_cv)
                                     
                                     opt_lam_mae <- cv_lasso_mae$lambda.min
                                     
                                     #mae_cv <- cv_lasso_mae$cvm
                                     
                                     opt_lam_lasso_mae <- append(opt_lam_lasso_mae, opt_lam_mae)
                                     
                                     #mae_cv_list <- append(mae_cv_list, mae_cv)
                                     
                                     #model_ridge_rmse <- glmnet(as.matrix(x_train), y_train, alpha = 0,
                                     #lambda = opt_lam)
                                     
                                     #model_ridge_mae <- glmnet(as.matrix(x_train), y_train, alpha = 0,
                                     #lambda = opt_lam_mae)
                                     
                                     #ridge_list_rmse[[i]] <- model_ridge_rmse
                                     
                                     #ridge_list_mae[[i]] <- model_ridge_mae
                                   }
                                   
                                   #lasso_lam_rmse_df <- data.frame(
                                   # lambda = unlist(opt_lam_lasso_rmse),
                                   # rmse = unlist(rmse_cv_list)
                                   #)
                                   
                                   #opt_ind_lasso_rmse <- which.min(lasso_lam_rmse_df$rmse)
                                   #lasso_tuned_lambda_rmse <- lasso_lam_rmse_df[opt_ind_lasso_rmse]
                                   
                                   #lam_lasso_rmse <- append(lam_lasso_rmse, median(unlist(opt_lam_lasso_rmse)))
                                   
                                   #lasso_lam_mae_df <- data.frame(
                                   # lambda = unlist(opt_lam_lasso_mae),
                                   #mae = unlist(mae_cv_list)
                                   #)
                                   
                                   #opt_ind_lasso_mae <- which.min(lasso_lam_mae_df$mae)
                                   #lasso_tuned_lambda_mae <- lasso_lam_mae_df[opt_ind_lasso_mae]
                                   
                                   #lam_lasso_mae <- append(lam_lasso_mae, median(unlist(opt_lam_lasso_mae)))
                                   
                                   for (i in seq_along(imputed_datasets)){
                                     y_train <- imputed_datasets[[i]]$total_los
                                     x_train <- imputed_datasets[[i]][, -9]
                                     
                                     model_lasso_rmse <- glmnet(as.matrix(x_train), y_train, alpha = 1,
                                                                lambda = opt_lam_lasso_rmse[[i]])
                                     
                                     model_lasso_mae <- glmnet(as.matrix(x_train), y_train, alpha = 1,
                                                               lambda = opt_lam_lasso_mae[[i]])
                                     
                                     lasso_list_rmse[[i]] <- model_lasso_rmse
                                     
                                     lasso_list_mae[[i]] <- model_lasso_mae
                                     
                                   }
                                   
                                   prediction_matrix_rmse <- matrix(, nrow = nrow(test_data), ncol = 0)
                                   
                                   prediction_matrix_mae <- matrix(, nrow = nrow(test_data), ncol = 0)
                                   
                                   #prediction_matrix_ssres <- matrix(, nrow = nrow(test_data), ncol = 0)
                                   
                                   #prediction_matrix_sstot <- matrix(, nrow = nrow(test_data), ncol = 0)
                                   
                                   for (j in imputed_test_data) {
                                     x_variables <- j[,-9]
                                     for (y in seq_along(lasso_list_rmse)){
                                       los_pred_rmse <- predict(lasso_list_rmse[[y]], as.matrix(x_variables))#, #type = "response")
                                       prediction_matrix_rmse <- cbind(prediction_matrix_rmse, los_pred_rmse)
                                       
                                       #lasso_ssres <- sum((imputed_test_data[[1]]$total_los - los_pred_rmse)^2)
                                       #lasso_sstot <- sum((imputed_test_data[[1]]$total_los - mean(imputed_test_data[[1]]))^2)
                                       #prediction_matrix_ssres <- cbind(prediction_matrix_ssres, lasso_ssres)
                                       #prediction_matrix_sstot <- cbind(prediction_matrix_sstot, lasso_sstot)
                                     }
                                   }
                                   
                                   for (j in imputed_test_data) {
                                     x_variables <- j[,-9]
                                     for (y in seq_along(lasso_list_mae)){
                                       los_pred_mae <- predict(lasso_list_mae[[y]], as.matrix(x_variables))#, #type = "response")
                                       prediction_matrix_mae <- cbind(prediction_matrix_mae, los_pred_mae)
                                     }
                                   }
                                   
                                   lasso_ssres <- colSums((imputed_test_data[[1]]$total_los - prediction_matrix_rmse)^2)
                                   
                                   lasso_sstot <- sum((imputed_test_data[[1]]$total_los - mean(imputed_test_data[[1]]$total_los))^2)
                                   
                                   predictions_rmse <- rowMeans(prediction_matrix_rmse)
                                   
                                   predictions_mae <- rowMeans(prediction_matrix_mae)
                                   
                                   #predictions_ssres <- rowMeans(prediction_matrix_ssres)
                                   
                                   #predictions_sstot <- rowMeans(prediction_matrix_sstot)
                                   
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
                                   
                                   lasso_R2_values <- 1 - lasso_ssres/lasso_sstot
                                   
                                   lasso_R2 <- mean(lasso_R2_values)
                                   
                                   #lasso_rmse_list <- append(lasso_rmse_list, lasso_rmse)
                                   
                                   #lasso_mae_list <- append(lasso_mae_list, lasso_mae)
                                   
                                   #lasso_rmse_df <- as.data.frame(lasso_rmse_list)
                                   
                                   #lasso_mae_df <- as.data.frame(lasso_mae_list)
                                   
                                   
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
                                     
                                     #rmse_cv <- sqrt(cv_ridge$cvm)
                                     
                                     opt_lam_ridge_rmse <- append(opt_lam_ridge_rmse, opt_lam)
                                     
                                     #rmse_cv_list <- append(rmse_cv_list, rmse_cv)
                                     
                                     opt_lam_mae <- cv_ridge_mae$lambda.min
                                     
                                     #mae_cv <- cv_ridge_mae$cvm
                                     
                                     opt_lam_ridge_mae <- append(opt_lam_ridge_mae, opt_lam_mae)
                                     
                                     #mae_cv_list <- append(mae_cv_list, mae_cv)
                                     
                                     #model_ridge_rmse <- glmnet(as.matrix(x_train), y_train, alpha = 0,
                                     #lambda = opt_lam)
                                     
                                     #model_ridge_mae <- glmnet(as.matrix(x_train), y_train, alpha = 0,
                                     #lambda = opt_lam_mae)
                                     
                                     #ridge_list_rmse[[i]] <- model_ridge_rmse
                                     
                                     #ridge_list_mae[[i]] <- model_ridge_mae
                                   }
                                   
                                   #ridge_lam_rmse_df <- data.frame(
                                   #lambda = unlist(opt_lam_ridge_rmse),
                                   # rmse = unlist(rmse_cv_list)
                                   #)
                                   
                                   #opt_ind_ridge_rmse <- which.min(ridge_lam_rmse_df$rmse)
                                   #ridge_tuned_lambda_rmse <- ridge_lam_rmse_df$lambda[opt_ind_ridge_rmse]
                                   
                                   #lam_ridge_rmse <- append(lam_ridge_rmse, median(unlist(opt_lam_ridge_rmse)))
                                   
                                   #ridge_lam_mae_df <- data.frame(
                                   #lambda = unlist(opt_lam_ridge_mae),
                                   # mae = unlist(mae_cv_list)
                                   #)
                                   
                                   #opt_ind_ridge_mae <- which.min(ridge_lam_mae_df$mae)
                                   #ridge_tuned_lambda_mae <- ridge_lam_mae_df$lambda[opt_ind_ridge_mae]
                                   
                                   #lam_ridge_mae <- append(lam_ridge_mae, median(unlist(opt_lam_ridge_mae)))
                                   
                                   for (i in seq_along(imputed_datasets)){
                                     y_train <- imputed_datasets[[i]]$total_los
                                     x_train <- imputed_datasets[[i]][, -9]
                                     
                                     model_ridge_rmse <- glmnet(as.matrix(x_train), y_train, alpha = 0,
                                                                lambda = opt_lam_ridge_rmse[[i]])
                                     
                                     model_ridge_mae <- glmnet(as.matrix(x_train), y_train, alpha = 0,
                                                               lambda = opt_lam_ridge_mae[[i]])
                                     
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
                                   }
                                   
                                   for (x in imputed_test_data) {
                                     x_variables <- x[,-9]
                                     for (y in seq_along(ridge_list_mae)){
                                       los_pred_mae <- predict(ridge_list_mae[[y]], as.matrix(x_variables))#, #type = "response")
                                       prediction_matrix_mae <- cbind(prediction_matrix_mae, los_pred_mae)
                                     }
                                   }
                                   
                                   ridge_ssres <- colSums((imputed_test_data[[1]]$total_los - prediction_matrix_rmse)^2)
                                   
                                   ridge_sstot <- sum((imputed_test_data[[1]]$total_los - mean(imputed_test_data[[1]]$total_los))^2)
                                   
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
                                   
                                   ridge_R2_values <- 1 - ridge_ssres/ridge_sstot
                                   
                                   ridge_R2 <- mean(ridge_R2_values)
                                   
                                   #ridge_rmse_list <- append(ridge_rmse_list, ridge_rmse)
                                   
                                   #ridge_mae_list <- append(ridge_mae_list, ridge_mae)
                                   
                                   #ridge_rmse_df <- as.data.frame(ridge_rmse_list)
                                   
                                   #ridge_mae_df <- as.data.frame(ridge_mae_list)
                                   
                                   #model_ridge <- glmnet(x_train, y_train, alpha = 0)
                                   
                                   #for(lam in lambdagrid ) {
                                   # model_ridge2 <- glmnet(x_train, y_train, alpha = 0, lambda = lam)
                                   
                                   #predict on train
                                   #predict on test
                                   
                                   #confusion matricies
                                   #}
                                   
                                   #Random Forest
                                   
                                   mtry_range <- c(2, 3, 4, 5, 6, 10)
                                   
                                   rfm_list_rmse <- list()
                                   
                                   rfm_list_mae <- list()
                                   
                                   rfm_rmse_list <- list()
                                   
                                   rfm_mae_list <- list()
                                   
                                   #for (try in seq_along(mtry_range)) {
                                   #for (i in seq_along(imputed_datasets)){
                                   
                                   #y_train <- imputed_datasets[[i]]$total_los
                                   #x_train <- imputed_datasets[[i]][, -9]
                                   
                                   #rfm <- randomForest( total_los ~ .,
                                   #                    mtry = mtry_range[try],
                                   #                     ntree = 200,
                                   #                     data = imputed_datasets[[i]])
                                   
                                   # rfm_list[[i]] <- rfm
                                   #}
                                   
                                   # prediction_matrix <- matrix(, nrow = nrow(test_data), ncol = 0)
                                   
                                   #  for (x in imputed_test_data) {
                                   #for (y in seq_along(rfm_list)){
                                   # los_pred <- predict(rfm_list[[y]], as.matrix(x))
                                   #  prediction_matrix <- cbind(prediction_matrix, los_pred)
                                   # }
                                   #}
                                   
                                   # predictions <- rowMeans(prediction_matrix)
                                   
                                   #  predictions_df <- as.data.frame(predictions) %>%
                                   #rename(total_los = predictions) %>%
                                   # mutate(total_los = ifelse(total_los <= 0.25, 0.25, total_los))
                                   
                                   #rfm_rmse <- mltools::rmse(preds = predictions_df$total_los,
                                   #                 imputed_test_data[[1]]$total_los)
                                   
                                   #rfm_mae <- Metrics::mae(actual = imputed_test_data[[1]]$total_los,
                                   #              predicted = predictions_df$total_los)
                                   
                                   #rfm_rmse_list <- append(rfm_rmse_list, rfm_rmse)
                                   
                                   #rfm_mae_list <- append(rfm_mae_list, rfm_mae)
                                   #}
                                   
                                   #rfm_rmse_df <- as.data.frame(rfm_rmse_list)
                                   
                                   #rfm_mae_df <- as.data.frame(rfm_mae_list)
                                   
                                   #colnames(rfm_rmse_df) <- c("2", "3", "4", "5", "6", "10")
                                   
                                   #colnames(rfm_mae_df) <- c("2", "3", "4", "5", "6", "10")
                                   
                                   opt_mtry_list_rmse <- list()
                                   
                                   opt_mtry_list_mae <- list()
                                   
                                   opt_rmse_list <- list()
                                   
                                   opt_mae_list <- list()
                                   
                                   
                                   cv_method <- trainControl(method = "cv", number = 5)
                                   
                                   #rmse
                                   for (i in seq_along(imputed_datasets)){
                                     model_rmse <- caret::train(total_los ~ .,
                                                                method = "rf",
                                                                trControl = cv_method,
                                                                data = imputed_datasets[[i]],
                                                                tuneGrid = expand.grid(mtry = mtry_range),
                                                                metric = "RMSE")
                                     
                                     model_mae <- caret::train(total_los ~ .,
                                                               method = "rf",
                                                               trControl = cv_method,
                                                               data = imputed_datasets[[i]],
                                                               tuneGrid = expand.grid(mtry = mtry_range),
                                                               metric = "MAE")
                                     
                                     #opt_mtry_rmse <- model_rmse$finalModel$mtry
                                     
                                     #opt_rmse <- model_rmse$results$RMSE[model_rmse$results$mtry 
                                     # == model_rmse$bestTune$mtry]
                                     
                                     #opt_mtry_mae <- model_mae$finalModel$mtry
                                     
                                     #opt_mae <- model_mae$results$MAE[model_mae$results$mtry 
                                     # == model_mae$bestTune$mtry]
                                     
                                     
                                     #opt_mtry_list_rmse <- append(opt_mtry_list_rmse, opt_mtry_rmse)
                                     
                                     #opt_mtry_list_mae <- append(opt_mtry_list_mae, opt_mtry_mae)
                                     
                                     opt_mtry_list_rmse[[i]] <- model_rmse$bestTune$mtry
                                     
                                     opt_mtry_list_mae[[i]] <- model_mae$bestTune$mtry
                                     
                                     #opt_rmse_list <- append(opt_rmse_list, opt_rmse)
                                     
                                     #opt_mae_list <- append(opt_mae_list, opt_mae)
                                   }
                                   
                                   #rfm_mtry_rmse_df <- data.frame(
                                   # mtry = unlist(opt_mtry_list_rmse),
                                   #rmse = unlist(opt_rmse_list)
                                   #)
                                   
                                   #opt_ind_rfm_rmse <- which.min(rfm_mtry_rmse_df$rmse)
                                   #rfm_tuned_mtry_rmse <- rfm_mtry_rmse_df$mtry[opt_ind_rfm_rmse]
                                   
                                   #mtry_rfm_rmse <- append(mtry_rfm_rmse, floor(median(unlist(opt_mtry_list_rmse))))
                                   
                                   #rfm_mtry_mae_df <- data.frame(
                                   # mtry = unlist(opt_mtry_list_mae),
                                   # mae = unlist(opt_mae_list)
                                   # )
                                   
                                   # opt_ind_rfm_mae <- which.min(rfm_mtry_mae_df$mae)
                                   #rfm_tuned_mtry_mae <- rfm_mtry_mae_df$lambda[opt_ind_mtry_mae]
                                   
                                   #mtry_rfm_mae <- append(mtry_rfm_mae, floor(median(unlist(opt_mtry_list_rmse))))
                                   
                                   for (i in seq_along(imputed_datasets)){
                                     
                                     #y_train <- imputed_datasets[[i]]$total_los
                                     #x_train <- imputed_datasets[[i]][, -9]
                                     
                                     rfm_rmse <- randomForest( total_los ~ .,
                                                               mtry = opt_mtry_list_rmse[[i]],
                                                               ntree = 200,
                                                               data = imputed_datasets[[i]],
                                                               importance = TRUE)
                                     
                                     rfm_list_rmse[[i]] <- rfm_rmse
                                     
                                     rfm_mae <- randomForest( total_los ~ .,
                                                              mtry = opt_mtry_list_mae[[i]],
                                                              ntree = 200,
                                                              data = imputed_datasets[[i]],
                                                              importance = TRUE)
                                     
                                     rfm_list_mae[[i]] <- rfm_mae
                                   }
                                   
                                   #prediction_matrix_rmse <- matrix(, nrow = nrow(test_data), ncol = 0)
                                   
                                   #variable_importance <- sapply(rfm_rmse_list, function(m) {
                                   # importance(m)[, "IncNodePurity"]   # or "%IncMSE" if you prefer
                                   #})
                                   
                                   #variable_importance <- sapply(rfm_list_rmse, function(model) {
                                   # varImp(model)$importance[,1]  # take the first column (Overall)
                                   #})
                                   
                                   variable_importance <- NULL
                                   
                                   for (i in seq_along(rfm_list_rmse)){
                                     imp <- importance(rfm_list_rmse[[i]])[,"%IncMSE"]
                                     
                                     variable_importance <- cbind(variable_importance, imp)
                                   }
                                   
                                   prediction_list <- list()
                                   
                                   for (i in seq_along(rfm_list_rmse)) {
                                     
                                     model <- rfm_list_rmse[[i]]
                                     x <- imputed_test_data[[i]]
                                     
                                     pred <- predict(model, x)
                                     
                                     prediction_list[[i]] <- as.numeric(pred)
                                   }
                                   
                                   predictions_rmse <- Reduce("+", prediction_list) / length(prediction_list)
                                   
                                   #for (x in imputed_test_data) {
                                    # for (y in seq_along(rfm_list_rmse)){
                                     #  los_pred_rmse <- predict(rfm_list_rmse[[y]], as.matrix(x))
                                      # los_pred_rmse <- predict(rfm_list_rmse[[y]], x)
                                       #prediction_matrix_rmse <- cbind(prediction_matrix_rmse, los_pred_rmse)
                                    # }
                                   
                    
                                   #}
                                   
                                   r2_list <- numeric(length(prediction_list))
                                   
                                   for (i in seq_along(prediction_list)) {
                                     
                                     y_true <- imputed_test_data[[i]]$total_los
                                     y_pred <- prediction_list[[i]]
                                     
                                     ss_res <- sum((y_true - y_pred)^2)
                                     ss_tot <- sum((y_true - mean(y_true))^2)
                                     
                                     r2_list[i] <- 1 - (ss_res / ss_tot)
                                   }
                                   
                                   rfm_R2 <- mean(r2_list)
                                   
                                   #rfm_ssres <- colSums((imputed_test_data[[1]]$total_los - prediction_matrix_rmse)^2)
                                   
                                   #rfm_sstot <- sum((imputed_test_data[[1]]$total_los - mean(imputed_test_data[[1]]$total_los))^2)
                                   
                                   #predictions_rmse <- rowMeans(prediction_matrix_rmse)
                                   
                                   predictions_rmse_df <- as.data.frame(predictions_rmse) %>%
                                     rename(total_los = predictions_rmse) %>%
                                     mutate(total_los = ifelse(total_los <= 0.25, 0.25, total_los))
                                   
                                   #prediction_matrix_mae <- matrix(, nrow = nrow(test_data), ncol = 0)
                                   
                                   #for (x in imputed_test_data) {
                                    # for (y in seq_along(rfm_list_mae)){
                                       #los_pred_mae <- predict(rfm_list_mae[[y]], as.matrix(x))
                                     #  los_pred_mae <- predict(rfm_list_mae[[y]], x)
                                      # prediction_matrix_mae <- cbind(prediction_matrix_mae, los_pred_mae)
                                     #}
                                   #}
                                   
                                   prediction_list_2 <- list()
                                   
                                   for (i in seq_along(rfm_list_mae)) {
                                     
                                     model <- rfm_list_mae[[i]]
                                     x <- imputed_test_data[[i]]
                                     
                                     pred_2 <- predict(model, x)
                                     
                                     prediction_list_2[[i]] <- as.numeric(pred_2)
                                   }
                                   
                                   predictions_mae <- Reduce("+", prediction_list_2) / length(prediction_list_2)
                                   
                                   
                                   #predictions_mae <- rowMeans(prediction_matrix_mae)
                                   
                                   predictions_mae_df <- as.data.frame(predictions_mae) %>%
                                     rename(total_los = predictions_mae) %>%
                                     mutate(total_los = ifelse(total_los <= 0.25, 0.25, total_los))
                                   
                                   rmse_list <- numeric(length(prediction_list))
                                   
                                   for (i in seq_along(prediction_list)) {
                                     rmse_list[i] <- mltools::rmse(
                                       preds = prediction_list[[i]],
                                       actuals = imputed_test_data[[i]]$total_los
                                     )
                                   }
                                   
                                   mae_list <- numeric(length(prediction_list_2))
                                   
                                   for (i in seq_along(prediction_list_2)) {
                                     mae_list[i] <- Metrics::mae(
                                       predicted = prediction_list_2[[i]],
                                       actual = imputed_test_data[[i]]$total_los
                                     )
                                   }
                                   
                                   rfm_rmse <- mean(rmse_list)
                                   
                                   rfm_mae <- mean(mae_list)
                                   
                                   #rfm_rmse <- mltools::rmse(preds = predictions_rmse_df$total_los,
                                                             #imputed_test_data[[1]]$total_los)
                                   
                                   #rfm_mae <- Metrics::mae(actual = imputed_test_data[[1]]$total_los,
                                                           #predicted = predictions_mae_df$total_los)
                                   
                                   #rfm_R2_values <- 1 - rfm_ssres/rfm_sstot
                                   
                                   #rfm_R2 <- mean(rfm_R2_values)
                                   
                                   #rfm_rmse_list <- append(rfm_rmse_list, rfm_rmse)
                                   
                                   #rfm_mae_list <- append(rfm_mae_list, rfm_mae)
                                   
                                   #rfm_rmse_df <- as.data.frame(rfm_rmse_list)
                                   
                                   #rfm_mae_df <- as.data.frame(rfm_mae_list)
                                   
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
                                        lm_R2 = lm_R2,
                                        lasso_rmse = lasso_rmse,
                                        lasso_mae = lasso_mae,
                                        lasso_R2 = lasso_R2,
                                        ridge_rmse = ridge_rmse,
                                        ridge_mae = ridge_mae,
                                        ridge_R2 = ridge_R2,
                                        random_forest_rmse = rfm_rmse,
                                        random_forest_mae = rfm_mae,
                                        random_forest_R2 = rfm_R2,
                                        variable_importance = variable_importance,
                                        #variable_importance_lm = variable_importance_lm,
                                        train_data = imp_train_data,
                                        lm_models = fitted_models#,
                                        #imputed_datasets = imputed_datasets,
                                        #imputed_test_data = imputed_test_data
                                        #   xgboost_single = single_tree,
                                        #   xgboost = xgboost_multi
                                   )
                                   
                                 }

stopCluster(clust)

plot_model(results_3[[1]]$lm_models[[1]], show.values = TRUE, value.offset = .3)

var_imp_3 <- NULL

for(i in seq_along(results_3)) {
  
  # Extract the variable importance matrix/vector
  imp_3 <- results_3[[i]]$variable_importance  # should be a matrixx or data frame
  
  # Compute row means if it's multi-column, otherwise keep as is
  if(is.matrix(imp_3) || ncol(imp_3) > 1){
    imp_vec <- rowMeans(as.matrix(imp_3))
  } else {
    imp_vec <- as.numeric(imp_3)
  }
  
  # Make sure it's a column matrix and keep row names
  imp_df_3 <- data.frame(imp_vec)
  rownames(imp_df_3) <- rownames(imp_3)
  
  # Bind as new column
  var_imp_3 <- cbind(var_imp_3, imp_df_3)
}


#for(i in seq_along(results)){
# imp_df <- results[[i]]$variable_importance %>%
#as.data.frame() %>%
#rowMeans() %>%
#as.data.frame()

#var_imp_2 <- cbind(var_imp_2, imp_df)
#}



var_imp_3 <- results_3[[1]]$variable_importance %>%
  as.data.frame %>%
  rowMeans() %>%
  as.data.frame() #%>%
#  tibble::rownames_to_column("var") %>%
# rename("importance" = ".") %>%
#arrange(importance)

for(i in seq_along(results_3)){
  if(i==1){
    var_imp_3 <- var_imp_3
  }
  else {
    var_imp_3 <- results_3[[i]]$variable_importance %>%
      as.data.frame %>%
      rowMeans() %>%
      as.data.frame()
    
    var_imp_3 <- cbind(var_imp_3, var_imp_3$.)
  }
}

var_imp_3 <- var_imp_3 %>%
  rowMeans() %>%
  as.data.frame() %>%
  tibble::rownames_to_column("var") %>%
  rename("importance" = ".") %>%
  arrange(importance)

var_imp_3$var<- var_imp_3$var %>% as.factor()
var_imp_3$var <- fct_inorder(var_imp_3$var)

imp_bar_3 <- ggplot(data = var_imp_3) + 
  geom_bar(
    stat = "identity",#it leaves the data without count and bin
    mapping = aes(x = var, y=importance, fill = var), 
    show.legend = FALSE,
    width = 1
  ) + 
  labs(x = NULL, y = NULL)

imp_bar_3 + coord_polar() + theme_minimal()
imp_bar_3 + coord_flip() + theme_minimal()


pairs_plots_3 <- vector("list", 25)
i <- 1

for(x in seq_along(results_3)) {
  for(y in seq_along(results_3[[x]]$train_data)){
    comp_data <- results_3[[x]]$train_data[[y]][, 
                                              sapply(results_3[[x]]$train_data[[y]], function(x) sd(x, na.rm = TRUE) > 0)
    ]
    #pdf(NULL)  # invisible device
    #pairs.panels(comp_data)  # draw the plot
    #pairs_plots[[i]] <- recordPlot()  # capture it
    #dev.off()
    #pairs_plots[[i]] <- recordPlot(pairs.panels(completed_data)) 
    file_3 <- paste0("pairs_plot_", x, "_", y, ".pdf")
    
    pdf(file)
    pairs.panels(comp_data)
    dev.off()
    
    pairs_plots_3[[i]] <- file_3
    
    i <- i + 1
  }
}


#---------------------------JUNK------------------------------------------------
final_lm_rmse_tot <- 0
final_lm_mae_tot <- 0
final_lm_R2_tot <- 0
final_lasso_rmse_tot <- 0
final_lasso_mae_tot <- 0
final_lasso_R2_tot <- 0
final_ridge_rmse_tot <- 0
final_ridge_mae_tot <- 0
final_ridge_R2_tot <- 0
final_rfm_rmse_tot <- 0
final_rfm_mae_tot <- 0
final_rfm_R2_tot <- 0

for (i in seq_along(results)){
  final_lm_rmse_tot <- final_lm_rmse_tot + results[[i]]$lm_rmse
  final_lm_mae_tot <- final_lm_mae_tot + results[[i]]$lm_mae
  final_lm_R2_tot <- final_lm_R2_tot + results[[i]]$lm_R2
  final_lasso_rmse_tot <- final_lasso_rmse_tot + results[[i]]$lasso_rmse
  final_lasso_mae_tot <- final_lasso_mae_tot + results[[i]]$lasso_mae
  final_lasso_R2_tot <- final_lasso_R2_tot + results[[i]]$lasso_R2
  final_ridge_rmse_tot <- final_ridge_rmse_tot + results[[i]]$ridge_rmse
  final_ridge_mae_tot <- final_ridge_mae_tot + results[[i]]$ridge_mae
  final_ridge_R2_tot <- final_ridge_R2_tot + results[[i]]$ridge_R2
  final_rfm_rmse_tot <- final_rfm_rmse_tot + results[[i]]$random_forest_rmse
  final_rfm_mae_tot <- final_rfm_mae_tot + results[[i]]$random_forest_mae
  final_rfm_R2_tot <- final_rfm_R2_tot + results[[i]]$random_forest_R2
} 

final_lm_rmse <- final_lm_rmse_tot/5
final_lm_mae <- final_lm_mae_tot/5
final_lm_R2 <- final_lm_R2_tot/5
final_lasso_rmse <- final_lasso_rmse_tot/5
final_lasso_mae <- final_lasso_mae_tot/5
final_lasso_R2 <- final_lasso_R2_tot/5
final_ridge_rmse <- final_ridge_rmse_tot/5
final_ridge_mae <- final_ridge_mae_tot/5
final_ridge_R2 <- final_ridge_R2_tot/5
final_rfm_rmse <- final_rfm_rmse_tot/5
final_rfm_mae <- final_rfm_mae_tot/5
final_rfm_R2 <- final_rfm_R2_tot/5

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
  results[[5]]$lasso_rmse) #%>%
  #t() %>%
  #as.data.frame()#%>%
  #t() %>%
  #as.data.frame() %>%
 # rowwise() %>%
  #mutate(rmse_mean = mean(c_across(everything()))) %>%
  #ungroup()

#lasso_rmse$rmse_mean <- rowMeans(lasso_rmse, na.rm = TRUE)

lasso_rmse$lambda <- lam_lasso_rmse

colnames(lasso_rmse) <- c("rmse",# "2", "3", "4", "5", "rmse_mean",
                          "lambda")

rownames(lasso_rmse) <- c(1:nrow(lasso_rmse))

final_id_lasso_rmse <- which.min(lasso_rmse$rmse)

final_lambda_lasso_rmse <- lasso_rmse$lambda[final_id_lasso_rmse]

lasso_mae <- bind_rows(
  results[[1]]$lasso_mae,
  results[[2]]$lasso_mae,
  results[[3]]$lasso_mae,
  results[[4]]$lasso_mae,
  results[[5]]$lasso_mae) #%>%
  #t() %>%
  #as.data.frame()

#lasso_mae$mae_mean <- rowMeans(lasso_mae, na.rm = TRUE)

lasso_mae$lambda <- lam_lasso_mae

colnames(lasso_mae) <- c("mae",#"2", "3", "4", "5", "mae_mean",
                         "lambda")

rownames(lasso_mae) <- c(1:nrow(lasso_mae))

final_id_lasso_mae <- which.min(lasso_mae$mae)

final_lambda_lasso_mae <- lasso_mae$lambda[final_id_lasso_mae]

ridge_rmse <- bind_rows(
  results[[1]]$ridge_rmse,
  results[[2]]$ridge_rmse,
  results[[3]]$ridge_rmse,
  results[[4]]$ridge_rmse,
  results[[5]]$ridge_rmse) #%>%
  #t() %>%
  #as.data.frame()

#ridge_rmse$rmse_mean <- rowMeans(ridge_rmse, na.rm = TRUE)

ridge_rmse$lambda <- lam_ridge_rmse

colnames(ridge_rmse) <- c("rmse",# "2", "3", "4", "5", "rmse_mean",
                          "lambda")

rownames(ridge_rmse) <- c(1:nrow(ridge_rmse))

final_id_ridge_rmse <- which.min(ridge_rmse$rmse)

final_lambda_ridge_rmse <- ridge_rmse$lambda[final_id_ridge_rmse]

ridge_mae <- bind_rows(
  results[[1]]$ridge_mae,
  results[[2]]$ridge_mae,
  results[[3]]$ridge_mae,
  results[[4]]$ridge_mae,
  results[[5]]$ridge_mae) #%>%
  #t() %>%
  #as.data.frame()
  #mutate(rmse_mean <- rowMeans(across(everything())))

#ridge_mae$mae_mean <- rowMeans(ridge_mae, na.rm = TRUE)

ridge_mae$lambda <- lam_ridge_mae

colnames(ridge_mae) <- c("mae",# "2", "3", "4", "5", "mae_mean",
                         "lambda")

rownames(ridge_mae) <- c(1:nrow(ridge_mae))

final_id_ridge_mae <- which.min(ridge_mae$mae)

final_lambda_ridge_mae <- ridge_mae$lambda[final_id_ridge_mae]


rfm_rmse <- bind_rows(
  results[[1]]$random_forest_rmse,
  results[[2]]$random_forest_rmse, 
  results[[3]]$random_forest_rmse,
  results[[4]]$random_forest_rmse, 
  results[[5]]$random_forest_rmse) #%>%
  #t() %>%
  #as.data.frame()
  #mutate(rmse_mean <- rowMeans(across(everything())))

#rfm_rmse$rmse_mean <- rowMeans(rfm_rmse, na.rm = TRUE)

rfm_rmse$mtry <- mtry_rfm_rmse

colnames(rfm_rmse) <- c("rmse",# "2", "3", "4", "5", "rmse_mean",
                        "mtry")

rownames(rfm_rmse) <- c(1:nrow(rfm_rmse))

final_id_rfm_rmse <- which.min(rfm_rmse$rmse)

final_mtry_rfm_rmse <- rfm_rmse$mtry[final_id_rfm_rmse]

rfm_mae <- bind_rows(
  results[[1]]$random_forest_mae,
  results[[2]]$random_forest_mae, 
  results[[3]]$random_forest_mae,
  results[[4]]$random_forest_mae, 
  results[[5]]$random_forest_mae) #%>%
  #t() %>%
  #as.data.frame()
#mutate(rmse_mean <- rowMeans(across(everything())))

#rfm_mae$mae_mean <- rowMeans(rfm_mae, na.rm = TRUE)

rfm_mae$mtry <- mtry_rfm_mae

colnames(rfm_mae) <- c("mae",# "2", "3", "4", "5", "mae_mean",
                       "mtry")

rownames(rfm_mae) <- c(1:nrow(rfm_mae))

final_id_rfm_mae <- which.min(rfm_mae$mae)

final_mtry_rfm_mae <- rfm_mae$mtry[final_id_rfm_mae]

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

#cv_method <- trainControl(method = "cv", number = 5)

#rmse
#for (i in seq_along(imputed_datasets)){
 # model <- caret::train(total_los ~ .,
  #                    method = "rf",
    #                  trControl = cv_method,
   #                   data = imputed_datasets[[i]],
                      #tuneGrid = expand.grid(mtry_range),
     #                 metric = "RMSE")
  
  #opt_mtry <- model$finalModel$mtry
  
  #opt_mtry_list_rmse <- append(opt_mtry_list, opt_mtry)
#}

#opt_mtry_df_rmse <- as.data.frame(opt_mtry_list_rmse)

#colnames(opt_mtry_df_rmse) <- c("mtry")

#mode_mtry_rmse <- mode(opt_mtry_df$mtry)

#Find the mode

#make data frame of optimal mtry values 

#predict(model$finalModel, as.matrix(imputed_datasets[[i]]))

#metrics using the test datasets

#outside for each

#will need to make the row a column and rename the column

#save the mode within the foreach loop
#make list of the mtry modes

#mtry_mode_list <- list(results[[1]]$mode_mtry,
 #                      results[[2]]$mode_mtry,
  #                     results[[3]]$mode_mtry,
    #                   results[[4]]$mode_mtry,
   #                    results[[5]]$mode_mtry)

#opt_mtry_mode <- mode(mtry_mode_list)

#mode_mtry from optimal mtry values data frame

#imputed_datasets and imputed_test_data lists need to made

#imputed_datasets_list <- list(results[[1]]$imputed_datasets,
 #                             results[[2]]$imputed_datasets,
  #                            results[[3]]$imputed_datasets,
   #                           results[[4]]$imputed_datasets,
    #                          results[[5]]$imputed_datasets)

#imputed_test_data_list <- list(results[[1]]$imputed_test_data,
 #                             results[[2]]$imputed_test_data,
  #                            results[[3]]$imputed_test_data,
   #                           results[[4]]$imputed_test_data,
    #                          results[[5]]$imputed_test_data)

#for(i in seq_along(imputed_datasets_list)) {
#rfm <- randomForest( total_los ~ .,
 #                    mtry = opt_mtry_mode,
  #                   ntree = 500,
   #                  data = imputed_datasets_list[[i]])


#rfm_list[[i]] <- rfm

#}

#prediction_matrix <- matrix(, nrow = nrow(test_data), ncol = 0)

#for (x in imputed_test_data_list) {
 # for (y in seq_along(rfm_list)){
  #  los_pred <- predict(rfm_list[[y]], as.matrix(x))
   # prediction_matrix <- cbind(prediction_matrix, los_pred)
  #}
#}

#predictions <- rowMeans(prediction_matrix)

#predictions_df <- as.data.frame(predictions) %>%
 # rename(total_los = predictions) %>%
  #mutate(total_los = ifelse(total_los <= 0.25, 0.25, total_los))

#rfm_rmse <- mltools::rmse(preds = predictions_df$total_los,
 #                         imputed_test_data[[1]]$total_los)

#rfm_mae <- Metrics::mae(actual = imputed_test_data[[1]]$total_los,
 #                       predicted = predictions_df$total_los)

#rfm_rmse_list <- append(rfm_rmse_list, rfm_rmse)

#rfm_mae_list <- append(rfm_mae_list, rfm_mae)


#rfm_rmse_df <- as.data.frame(rfm_rmse_list)

#rfm_mae_df <- as.data.frame(rfm_mae_list)

#many to many predictions and evaluations then averages of performance metrics



#save the imputed datasets and test_sets from each fold to use i/n  the rfm final loop




#(lm_mae_mean/mean(imputed_test_data[[1]]$total_los))*100

#(lm_mae_mean/median(imputed_test_data[[1]]$total_los))*100

lisr <- list()

lism <- list()

for(x in seq_along(folds)){
test_id <- folds[[x]]
train_data <- main_dummy_data[-test_id, ]
test_data <- main_dummy_data[test_id, ]

# imputation
imp_train <- mice(train_data, method = meth_dummy, predictorMatrix = predictor_matrix_dummy, 
                  m = m, maxit = 5)
imp_test <- mice(test_data, method = meth_dummy, predictorMatrix = predictor_matrix_dummy, 
                 m = m, maxit = 5)

# manually complete and build models
fitted_models <- list()
for (i in 1:m) {
  completed_data <- complete(imp_train, i)
  model <- glm(formula = as.formula(full_model), data = completed_data, 
               family = Gamma(link = "log"))
  fitted_models[[i]] <- model
}

# predictions
imputed_test_data <- complete(imp_test, "all")
print(sapply(imputed_test_data, nrow))  # check row counts

predictions <- rep(0, nrow(test_data))
count <- 0

for (x in imputed_test_data) {
  for (y in fitted_models) {
    los_pred <- as.numeric(predict(y, x))
    if (length(los_pred) != nrow(test_data)) {
      print(paste("Mismatch: prediction length =", length(los_pred), 
                  "test rows =", nrow(test_data)))
    } else {
      predictions <- predictions + los_pred
      count <- count + 1
    }
  }
}

predictions <- predictions / count

predictions_df <- as.data.frame(predictions) %>%
  rename(total_los = predictions) %>%
  mutate(total_los = ifelse(total_los <= 0.25, 0.25, total_los))

lm_rmse <- mltools::rmse(preds = predictions_df$total_los,
                         imputed_test_data[[1]]$total_los)

lm_mae <- Metrics::mae(actual = imputed_test_data[[1]]$total_los,
                       predicted = predictions_df$total_los)

lisr <- append(lisr, lm_rmse)

lism <- append(lism, lm_mae)

}

llisr <- list()

llism <- list()

for(x in seq_along(folds)){
  test_id <- folds[[x]]
  train_data <- main_dummy_data[-test_id, ]
  test_data <- main_dummy_data[test_id, ]
  
  # imputation
  imp_train <- mice(train_data, method = meth_dummy, predictorMatrix = predictor_matrix_dummy, 
                    m = m, maxit = 5)
  imp_test <- mice(test_data, method = meth_dummy, predictorMatrix = predictor_matrix_dummy, 
                   m = m, maxit = 5)
  imputed_test_data <- complete(imp_test, "all")
  
  imputed_datasets <- complete(imp_train, "all")
  
  # model_lasso <- glmnet(x_train, y_train, alpha = 1)
  
  #lambdagrid <- c(0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1, 1.5, 2, 2.5, 3, 3.5)
  
  lasso_list_rmse <- list()
  
  lasso_list_mae <- list()
  
  lasso_rmse_list <- list()
  
  lasso_mae_list <- list()
  
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
    
    #rmse_cv <- sqrt(cv_lasso$cvm)
    
    
    opt_lam_lasso_rmse <- append(opt_lam_lasso_rmse, opt_lam)
    
    #rmse_cv_list <- append(rmse_cv_list, rmse_cv)
    
    opt_lam_mae <- cv_lasso_mae$lambda.min
    
    #mae_cv <- cv_lasso_mae$cvm
    
    opt_lam_lasso_mae <- append(opt_lam_lasso_mae, opt_lam_mae)
    
    #mae_cv_list <- append(mae_cv_list, mae_cv)
    
    #model_ridge_rmse <- glmnet(as.matrix(x_train), y_train, alpha = 0,
    #lambda = opt_lam)
    
    #model_ridge_mae <- glmnet(as.matrix(x_train), y_train, alpha = 0,
    #lambda = opt_lam_mae)
    
    #ridge_list_rmse[[i]] <- model_ridge_rmse
    
    #ridge_list_mae[[i]] <- model_ridge_mae
  }
  
  #lasso_lam_rmse_df <- data.frame(
   # lambda = unlist(opt_lam_lasso_rmse),
    #rmse = unlist(rmse_cv_list)
  #)
  
  #opt_ind_lasso_rmse <- which.min(lasso_lam_rmse_df$rmse)
  #lasso_tuned_lambda_rmse <- lasso_lam_rmse_df[opt_ind_lasso_rmse]
  
  lam_lasso_rmse <- append(lam_lasso_rmse, median(unlist(opt_lam_lasso_rmse)))
  
  #lasso_lam_mae_df <- data.frame(
   # lambda = unlist(opt_lam_lasso_mae),
    #mae = unlist(mae_cv_list)
  #)
  
  #opt_ind_lasso_mae <- which.min(lasso_lam_mae_df$mae)
  #lasso_tuned_lambda_mae <- lasso_lam_mae_df[opt_ind_lasso_mae]
  
  lam_lasso_mae <- append(lam_lasso_mae, median(unlist(opt_lam_lasso_mae)))
  
  for (i in seq_along(imputed_datasets)){
    y_train <- imputed_datasets[[i]]$total_los
    x_train <- imputed_datasets[[i]][, -9]
    
    model_lasso_rmse <- glmnet(as.matrix(x_train), y_train, alpha = 1,
                               lambda = as.numeric(lam_lasso_rmse[[x]]))
    
    model_lasso_mae <- glmnet(as.matrix(x_train), y_train, alpha = 1,
                              lambda = as.numeric(lam_lasso_mae[[x]]))
    
    lasso_list_rmse[[i]] <- model_lasso_rmse
    
    lasso_list_mae[[i]] <- model_lasso_mae
  }
  
  prediction_matrix_rmse <- matrix(, nrow = nrow(test_data), ncol = 0)
  
  prediction_matrix_mae <- matrix(, nrow = nrow(test_data), ncol = 0)
  
  for (j in imputed_test_data) {
    x_variables <- j[,-9]
    for (y in seq_along(lasso_list_rmse)){
      los_pred_rmse <- predict(lasso_list_rmse[[y]], as.matrix(x_variables))#, #type = "response")
      prediction_matrix_rmse <- cbind(prediction_matrix_rmse, los_pred_rmse)
    }
  }
  
  for (j in imputed_test_data) {
    x_variables <- j[,-9]
    for (y in seq_along(lasso_list_mae)){
      los_pred_mae <- predict(lasso_list_mae[[y]], as.matrix(x_variables))#, #type = "response")
      prediction_matrix_mae <- cbind(prediction_matrix_mae, los_pred_mae)
    }
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
  
  lasso_rmse_df <- as.data.frame(lasso_rmse_list)
  
  lasso_mae_df <- as.data.frame(lasso_mae_list)
  
  llisr <- append(llisr, lasso_rmse_df)
  
  llism <- append(llism, lasso_mae_df)
  
}





#Random Forest

mtry_rfm_rmse <- list()

mtry_rfm_mae <- list()

lrfist <- list()

lrfistm <- list()

rfm_rmse_list <- list()

rfm_mae_list <- list()

mtry_range <- c(2, 3, 4, 5, 6, 10)

for(x in seq_along(folds)){
  test_id <- folds[[x]]
  train_data <- main_dummy_data[-test_id, ]
  test_data <- main_dummy_data[test_id, ]
  
  # imputation
  imp_train <- mice(train_data, method = meth_dummy, predictorMatrix = predictor_matrix_dummy, 
                    m = m, maxit = 5)
  imp_test <- mice(test_data, method = meth_dummy, predictorMatrix = predictor_matrix_dummy, 
                   m = m, maxit = 5)
  imputed_test_data <- complete(imp_test, "all")
  
  imputed_datasets <- complete(imp_train, "all")



rfm_list_rmse <- list()

rfm_list_mae <- list()





opt_mtry_list_rmse <- list()

opt_mtry_list_mae <- list()

opt_rmse_list <- list()

opt_mae_list <- list()




cv_method <- trainControl(method = "cv", number = 5)

#rmse
for (i in seq_along(imputed_datasets)){
  model_rmse <- caret::train(total_los ~ .,
                             method = "rf",
                             trControl = cv_method,
                             data = imputed_datasets[[i]],
                             tuneGrid = expand.grid(mtry = mtry_range),
                             metric = "RMSE")
  
  model_mae <- caret::train(total_los ~ .,
                            method = "rf",
                            trControl = cv_method,
                            data = imputed_datasets[[i]],
                            tuneGrid = expand.grid(mtry = mtry_range),
                            metric = "MAE")
  
  opt_mtry_rmse <- model_rmse$finalModel$mtry
  
  #opt_rmse <- model_rmse$results$RMSE[model_rmse$results$mtry 
  # == model_rmse$bestTune$mtry]
  
  opt_mtry_mae <- model_mae$finalModel$mtry
  
  #opt_mae <- model_mae$results$MAE[model_mae$results$mtry 
  # == model_mae$bestTune$mtry]
  
  
  opt_mtry_list_rmse <- append(opt_mtry_list_rmse, opt_mtry_rmse)
  
  opt_mtry_list_mae <- append(opt_mtry_list_mae, opt_mtry_mae)
  
  #opt_rmse_list <- append(opt_rmse_list, opt_rmse)
  
  #opt_mae_list <- append(opt_mae_list, opt_mae)
}

#rfm_mtry_rmse_df <- data.frame(
# mtry = unlist(opt_mtry_list_rmse),
#rmse = unlist(opt_rmse_list)
#)

#opt_ind_rfm_rmse <- which.min(rfm_mtry_rmse_df$rmse)
#rfm_tuned_mtry_rmse <- rfm_mtry_rmse_df$mtry[opt_ind_rfm_rmse]

#mtry_rfm_rmse <- append(mtry_rfm_rmse, floor(median(unlist(opt_mtry_list_rmse))))

#rfm_mtry_mae_df <- data.frame(
# mtry = unlist(opt_mtry_list_mae),
# mae = unlist(opt_mae_list)
# )

# opt_ind_rfm_mae <- which.min(rfm_mtry_mae_df$mae)
#rfm_tuned_mtry_mae <- rfm_mtry_mae_df$lambda[opt_ind_mtry_mae]

#mtry_rfm_mae <- append(mtry_rfm_mae, floor(median(unlist(opt_mtry_list_rmse))))

for (i in seq_along(imputed_datasets)){
  
  #y_train <- imputed_datasets[[i]]$total_los
  #x_train <- imputed_datasets[[i]][, -9]
  
  rfm_rmse <- randomForest( total_los ~ .,
                            mtry = opt_mtry_list_rmse[[i]],
                            ntree = 200,
                            data = imputed_datasets[[i]])
  
  rfm_list_rmse[[i]] <- rfm_rmse
  
  rfm_mae <- randomForest( total_los ~ .,
                           mtry = opt_mtry_list_mae[[i]],
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

}
