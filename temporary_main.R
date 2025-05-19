#Install and load the packages
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("gtsummary")
install.packages("lubridate")
library(dplyr)
library(tidyr)
library(ggplot2)
library(gtsummary)
library(lubridate)

#Read in the lung cancer MIMIC-IV ICU dataset
icustays <- read.csv("icustays_lungcancer.csv") %>%
  arrange(hadm_id, intime) %>%
  group_by(hadm_id, icd_code) %>%
  mutate(time_since_discharge = 
           as.numeric(difftime(intime, lag(outtime), units = "hours")),
         episode = 
           cumsum(if_else(is.na(time_since_discharge)| time_since_discharge > 24, 1, 0))) %>%
  ungroup()
 
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
         temperature = if_else(charttime.x < charttime.y, temperature.y, temperature.x)) %>%
  mutate(timediff = as.numeric(ymd_hms(intime) - charttime, 'hours')) %>%
  select(c(hadm_id, stay_id, temperature, timediff)) %>%
  filter(temperature >= 25 & temperature < 47) %>%
  filter(timediff <= 24)

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
  filter(timediff <= time_to_icu)

troponin <- read.csv("troponin_icu_data.csv") %>%
  full_join(read.csv("troponin_icu.csv"), by = c("hadm_id", "stay_id")) %>%
  mutate(charttime.x = if_else(is.na(charttime.x), ymd_hms("2001-01-01 00:00:00"),
                               ymd_hms(charttime.x)),
         charttime.y = if_else(is.na(charttime.y), ymd_hms("2001-01-01 00:00:00"),
                               ymd_hms(charttime.y)),
         charttime = if_else(charttime.x < charttime.y, charttime.y, charttime.x),
         intime = if_else(charttime == charttime.y, intime.y, intime.x)) %>%
  mutate(troponin = if_else(charttime.x < charttime.y, troponin.y, troponin.x)) %>%
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
  mutate(haematocrit = if_else(charttime.x < charttime.y, haematocrit.y, haematocrit.x)) %>%
  mutate(timediff = as.numeric(ymd_hms(intime) - charttime, 'hours')) %>%
  select(c(hadm_id, stay_id, haematocrit, timediff)) %>%
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


#Join the datasets
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
         calcium_gluconate = if_else(is.na(calcium_gluconate), 0, calcium_gluconate),
         insurance = if_else(insurance == "NULL", "Unknown", insurance),
         previous_stay = episode - 1)

#Create a dataset for the unique records
main_unique <- main %>%
  select(-c(episode,icustay_id,total_los,icd_code)) %>%
  unique()

#Histogram for the proportion of missingness
main %>%
  select(c("temperature", "platelets", "glucose", "chloride", "potassium", "ptt",
           "haemoglobin", "troponin", "haematocrit", "inr", "sodium")) %>%
  summarise_all(~mean(is.na(.))*100) %>%
  pivot_longer(everything(), names_to = "column", values_to = "missing") %>%
  ggplot(aes(x = column, y = missing)) +
  geom_col(fill = "blue") +
  theme_minimal()

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
                              labels = c("Medicaid", "Medicare", "Other", "Private",
                                         "Missing")),
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
  select(c("previous_stay", "age_cat", "sex", "ethnicity", "insurance", "temperature", 
           "platelets", "glucose", "chloride", "potassium", "ptt", "haemoglobin",
           "troponin", "haematocrit", "inr", "sodium", "ondansetron", 
           "calcium_gluconate", "admission_type", "hospital_expire_flag",
           "total_los")) %>%
  gtsummary::tbl_summary(label = labels,
                         missing_text = "Missing",
                         statistic = list(all_categorical() ~ "{n} ({p}%)",
                                          all_continuous() ~ "{mean} ({sd})"))

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




  
