## Survival - DC 
#####

library(janitor)
library(tidyverse)
library(here)
library(dplyr)
library(ggplot2)
#install.packages("survival")
library(survival)


quagga <- read.csv(here("Portfolio 5 - survival", "Data", "Davis_quagga_mussels.csv")) 

# 1. Use Quagga Mussel Data to run Cox PH Model and plot it on a Kaplan Meier Curve

# Fit a Cox Proportional Hazards model
cox_quagga <- coxph(Surv(day, status) ~ treatment, data = quagga)
# Display the summary of the model
summary(cox_quagga)

# Calculate Kaplan-Meier survival curves
# For the overall study population
km_fit_total<-survfit(Surv(day,status)~1, data=quagga)
# Check it
summary(km_fit_total)

## R can't find "ggsurvplot" even though ggplot2 is loaded in the libary 
surv_plot <- ggsurvplot(fit = survfit(Surv(day, status) ~ treatment, data = quagga), conf.int = TRUE, xlab = "Days", ylab = "Overall survival probability")

surv_plot
# 2. How does the information each method produces differ? How would you apply these outputs? 
# 3. Come up with another population ecology specific example for where to use this package
