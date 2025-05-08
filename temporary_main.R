#Install and load the packages
install.packages("dplyr")
library(dplyr)

#Read in the lung cancer MIMIC-IV ICU dataset
icustays <- read.csv("icustays_lungcancer.csv") %>%
  arrange(hadm_id, intime) %>%
  group_by(hadm_id) %>%
  mutate(time_since_discharge = 
           as.numeric(difftime(intime, lag(outtime), units = "hours")),
         episode = 
           cumsum(if_else(is.na(time_since_discharge)| time_since_discharge > 24, 1, 0)),
         episode_id = paste0(hadm_id, "_", episode)) %>%
  ungroup()
 
icustays2 <- icustays %>%
  select(hadm_id, subject_id, icd_code) %>%
  unique()
  
icu_episodes <- icustays %>%
  group_by(hadm_id, episode_id, episode) %>%
  summarise(
    episode_intime = min(intime),
    episode_outtime = max(outtime),
    total_los = sum(los, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  inner_join(icustays2, by = "hadm_id")


#Read in the datasets for the covariates
temperature <- read.csv("temperature_icu_data.csv") %>%
  full_join(read.csv("temperature_icu.csv"), by = "hadm_id")

platelets <- read.csv("platelets_icu_data.csv")

adm_type <- read.csv("admission_type.csv")

glucose <- read.csv("glucose_icu_data.csv")

chloride <- read.csv("chloride_icu_data.csv")

potassium <- read.csv("potassium_icu_data.csv")

ptt <- read.csv("ptt_icu_data.csv")

haemoglobin <- read.csv("haemoglobin_icu_data.csv")

troponin <- read.csv("troponin_icu_data.csv")

haematocrit <- read.csv("haematocrit_icu_data.csv")

INR <- read.csv("inr_icu_data.csv")

sodium <- read.csv("sodium_icu_data.csv")

demographics <- read.csv("demographics_icu_data.csv")

drugs <- read.csv("drug_icu_data.csv") %>%
  mutate(ondansetron = if_else(drug %in% c("Ondansetron", "Ondansetron ODT"), 1, 0),
         calcium_gluconate = if_else(drug %in% c("Calcium Gluconate",
                                               "Calcium Gluconate (Premix)",
                                               "Calcium Gluconate Replacement (Oncology)",
                                               "Calcium Gluconate sliding scale (Critical Care-Ionized calcium)"),
                                     1, 0)) %>%
  select(-c(drug, stoptime, starttime)) %>%
  unique()

tab <- table(drugs$hadm_id)
ond_calcgluc <- names(tab[tab != 1])

drugs <- drugs %>%
  mutate(ondansetron = if_else(hadm_id %in% ond_calcgluc, 1, ondansetron),
         calcium_gluconate = if_else(hadm_id %in% ond_calcgluc, 1, calcium_gluconate)) %>%
  unique()

temperature_icu <- read.csv("temperature_icu.csv")


#Join the datasets
main <- icu_episodes %>%
  left_join(temperature, by = "hadm_id") %>%
  left_join(platelets, by = "hadm_id") %>%
  left_join(adm_type, by = "hadm_id") %>%
  left_join(glucose, by = "hadm_id") %>%
  left_join(chloride, by = "hadm_id") %>%
  left_join(potassium, by = "hadm_id") %>%
  left_join(ptt, by = "hadm_id") %>%
  left_join(haemoglobin, by = "hadm_id") %>%
  left_join(troponin, by = "hadm_id") %>%
  left_join(haematocrit, by = "hadm_id") %>%
  left_join(INR, by = "hadm_id") %>%
  left_join(sodium, by = "hadm_id") %>%
  left_join(demographics, by = "hadm_id") %>%
  left_join(drugs, by = "hadm_id") %>%
  select(c("subject_id.x", "hadm_id", "episode_id", "episode", "icd_code", 
           "age", "gender", "race", "insurance", "temperature", "platelets",
           "glucose", "chloride", "potassium", "ptt", "haemoglobin", "troponin",
           "haematocrit", "inr", "sodium", "ondansetron", "calcium_gluconate",
           "admission_type", "hospital_expire_flag", "total_los")) %>%
  rename(subject_id = subject_id.x,
         sex = gender) %>%
  unique() %>%
  rename(icustay_id = episode_id)

#Create a dataset for the unique records
main_unique <- main %>%
  select(-c(episode,icustay_id,total_los,icd_code)) %>%
  unique()



