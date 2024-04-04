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
        # May. 
    # filter by > 200, 500, and 1000 cases and look at dates 
den_peak <- Den %>% group_by(Date_floored) %>% filter(Cases > 500, na.rm=TRUE)
  
# 2a) what district has the highest number of cases? 
        # Piura has the highest number of cases. 
    # sum across entire data set, group by area code
den_area <- Den %>% group_by(District) %>% 
  filter(Cases > 500, na.rm=TRUE) %>% 
  summarize(total_cases = sum(Cases))




# 2b) Where is this district? Is it heavily populated? What biome is it in?
    # do some googling 
# 3) Find another health dataset and repeat the above (could be any health outcome and in any place)

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

