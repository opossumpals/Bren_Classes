## Survival - DC 
#####

library(janitor)
library(tidyverse)
library(here)
library(dplyr)
library(ggplot2)
#install.packages("survival")
library(survival)
library(survminer)


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

## plot cox over km 
surv_plot <- ggsurvplot(fit = survfit(Surv(day, status) ~ treatment, data = quagga), 
                        conf.int = TRUE, 
                        xlab = "Days", 
                        ylab = "Overall survival probability")
print(surv_plot)


# 2. How does the information each method produces differ? How would you apply these outputs? 
#### The Cox Proportional Hazards model compares the days the the mussels are alive or dead for the population across treatment
#### groups. This helps us understand the conditions that favor or disadvantage a species' survival. 
#### Meanwhile, the Kaplan-Meier curve visualizes the survival probability over time across treatment groups. This is helpful 
#### for informing management decisions in how to improve or decrease survivability. 

# 3. Come up with another population ecology specific example for where to use this package
#### This could be used for sea stars like the Morning sun star (Solaster dawsoni) in understanding and managing sea star wasting disease. 
#### Labs have been attempting to grow sea stars with varying success, and the survival package could be helpful in determining 
#### when trials are successful and how to manage for survival in the wild as well. 
