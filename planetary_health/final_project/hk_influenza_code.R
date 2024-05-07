
################################################################################################################
## This project hopes to evaluate the rates of influenza in HK over many years among different age groups. Compare to pollution levels 
## of those years. 
################################################################################################################

library(tidyverse)
library(dplyr)
library(ggplot2)
library(here)


# lets explore the data first 

hk_flu <- read.csv("hk_influenza_weekly.csv")
head(hk_flu)

hk_flu_cleaned <- hk_flu %>%  
  rename(ILI.consultation.rate.per.1.000.consultations.SentinelGOPC = ILI.consultation.rate..per.1.000.consultations.) %>% 
  rename(ILI.consultation.rate.per.1.000.consultations.SentinelPMP = X) %>% 
  rename(lab_surv_no_pos_detection_A.H1 = Laboratory.surveillance) %>% 
  rename(lab_surv_no_pos_detection_A.H3 = X.1) %>% 
  rename(lab_surv_no_pos_detection_B = X.2) %>% 
  rename(lab_surv_no_pos_detection_all.A.B = X.3) %>% 
  rename(lab_surv_percent_pos_A.H1 = X.4) %>% 
  rename(lab_surv_percent_pos_A.H3 = X.5) %>% 
  rename(lab_surv_percent_pos_B = X.6) %>% 
  rename(lab_surv_percent_pos_all.A.B = X.7) %>% 
  rename(No.ILI.outbreaks.in.schools = No..of.ILI.outbreaks.in.schools.institutions) %>% 
  rename(No.ILI.outbbreaks.in.institutions = X.8) %>% 
  rename(hospitalization_rate_per_10k_0to5yrs = Admission.rates.in.public.hospitals.with.principal.diagnosis.of.influenza..per.10.000.people.in.the.age.group.) %>% 
  rename(hospitalization_rate_per_10k_6to11yrs = X.9) %>% 
  rename(hospitalization_rate_per_10k_12to17yrs = X.10) %>% 
  rename(hospitalization_rate_per_10k_18to49yrs = X.11) %>% 
  rename(hospitalization_rate_per_10k_50to64yrs = X.12) %>% 
  rename(hospitalization_rate_per_10k_over65yrs = X.13) %>%
  rename(hospitalization_rate_per_10k_all.ages = X.14) %>% 
  rename(weekly_no_severe_case_0to17years =  Weekly.number.of.severe.influenza.cases.by.age.groups..) %>% 
  rename(weekly_no_severe_case_18to49years = X.15) %>% 
  rename(weekly_no_severe_case_50to64years = X.16) %>% 
  rename(weekly_no_severe_case_over65yrs = X.17)
