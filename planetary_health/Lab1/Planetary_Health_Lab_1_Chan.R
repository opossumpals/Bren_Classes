#Planetary health labs -- Peru dengue data:

rm(list=ls())
library(tidyverse)
library(lubridate)
library(doBy)
library(mosaic)
library(car)
library(ggplot2)
library(dplyr)

###########################
#Lab 1: quantifying health
###########################

#read in dengue case data:
Den <- read.csv("Dengue_Confirmed_Peru.csv", header=T)
head(Den)
lapply(Den, class)

#make Date a date object
Den$Date <- as.Date(Den$Week_Yr)

#floor dates for later merging with environment data:
#assigns a new date to the date column (first day of the week)
head(Den)
Den$Date_floored <- floor_date(Den$Date, "week")
head(Den)

Den_plot <- ggplot(Den, aes(x=Date_floored, y=Cases, col=as.factor(District))) +
  geom_point() + theme(legend.position="none")
Den_plot

#Explore the data:
# 1) what time of year do dengue cases tend to peak?
        # Dengue cases tend to peak in April and May, especially in 2017. By adjusting out threshold of minimum cases to evaluate, we get 
        # a wider range of dates, but as we increase the threshold of minimum cases, we see a higher concentration of cases in April and 
        # May. 2017 was an intense El Nino year for Peru, thus creating conditions that exacerbate Dengue concentrations. 
# filter by > 200, 500, and 1000 cases and look at dates 
den_peak <- Den %>% group_by(Date_floored) %>% filter(Cases > 500, na.rm=TRUE)


# 2a) what district has the highest number of cases? 
        # Piura has the highest number of cases. 
# sum across entire data set, group by area code
den_area <- Den %>% group_by(District) %>% 
  filter(Cases > 500, na.rm=TRUE) %>% 
  summarize(total_cases = sum(Cases))


# 2b) Where is this district? Is it heavily populated? What biome is it in?
        # Piura is located in northwestern Peru. It is low in elevation and is a subtropical desert climate -- as such it can be dry and
        # hot. Piura gets most of its precipitation from January to April, making April and May the times when mosquitoes are most active. 
        # Piura has a population of 484,475 in 2017 -- this is not one of the largest cities in Peru. 

# 3) Find another health dataset and repeat the above (could be any health outcome and in any place)
        # This dataset does not contain times of the year when asthma hospitalizations spike, but in 2020, asthma hospitalizations were
        # much higher. Los Angeles County has the highest number of hospitalizations with 14,086 from 2015-2020. This is a highly populated
        # urban area that is characterized by heavy industrial and urban pollution. This is not surprising as heavy polluted areas have 
        # poor air quality which can result in higher rates of asthma. 

# read in asthma data 
ca_asthma <- read.csv("asthma-hospitalization-rates-by-county-2015_2020.csv")
head(ca_asthma)
lapply(ca_asthma, class)

# make number of hospitalizations numeric
ca_asthma$Number_hospitalization <- as.numeric(ca_asthma$NUMBER.OF.HOSPITALIZATIONS)

ca_asthma_cleaned <- na.omit(ca_asthma)

# plot for # of asthma hospitalizations from 2015-2020 
ca_asthma_plot <- ggplot(ca_asthma_cleaned, aes(x=YEAR, y=Number_hospitalization, col=as.factor(COUNTY))) +
  geom_point() + 
  theme(legend.position="none")
ca_asthma_plot 

# group and filter to find county with highest number of asthma hospitalization 
# Using > 500 because prior to 2020, the highest number of asthma hospitalization was around 1000. 
ca_asthma_area <- ca_asthma_cleaned %>% group_by(COUNTY) %>% 
  filter(Number_hospitalization > 500, na.rm=TRUE) %>% 
  summarize(total_county_hospitalization = sum(Number_hospitalization))



# https://libguides.tulane.edu/c.php?g=182563&p=1354703
# CDC WONDER: https://wonder.cdc.gov/controller/datarequest/D140
# and Cancer: https://wonder.cdc.gov/cancer.html
# NCHS: https://www.cdc.gov/nchs/nchs_for_you/researchers.htm
# flu: https://gis.cdc.gov/GRASP/Fluview/FluHospRates.html
# asthma: https://www.cdc.gov/asthma/asthmadata.htm
# calenviroscreen data: https://oehha.ca.gov/calenviroscreen/report/calenviroscreen-40
# Brazil: https://datasus.saude.gov.br/informacoes-de-saude-tabnet/
# and https://datasus.saude.gov.br/acesso-a-informacao/doencas-e-agravos-de-notificacao-2001-a-2006-sinan/
# and https://datasus.saude.gov.br/acesso-a-informacao/doencas-e-agravos-de-notificacao-de-2007-em-diante-sinan/
# MANY OTHERS...

