install.packages("pmsampsize")
install.packages("dplyr")
library(dplyr)
library(pmsampsize)

#Read in the lung cancer MIMIC-IV dataset
icustays <- read.csv("icustays_lungcancer.csv")

#Takes only one record of the length of stay data for each ICU stay
icustays_unique <- icustays %>%
  dplyr::select(c(stay_id, los)) %>%
  dplyr::distinct()

#Standard deviation and mean of the length of stay variable
los_sd <- sd(icustays_unique$los)
los_mean <- mean(icustays_unique$los)

#Minimum sample size calculation using the length of stay standard deviation and
#mean and other parameters to be adjusted
pmsampsize::pmsampsize(
  type = "c",                # continuous outcome
  rsquared = 0.95,            # anticipated R²
  parameters = 10,           # number of candidate predictors
  sd = los_sd,
  intercept = los_mean
)
