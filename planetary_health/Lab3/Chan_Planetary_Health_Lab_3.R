#Planetary health labs -- Peru dengue data:

rm(list=ls())
library(tidyverse)
library(lubridate)
library(doBy)
library(mosaic)
library(car)
library(here)

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
head(Den)
Den$Date_floored <- floor_date(Den$Date, "week")
head(Den)

Den_plot <- ggplot(Den, aes(x=Date_floored, y=Cases, col=as.factor(District))) +
  geom_point() + theme(legend.position="none")
Den_plot

#Explore the data:
# 1) what time of year do dengue cases tend to peak?
# 2a) what district has the highest number of cases? 
# 2b) Where is this district? Is it heavily populated? What biome is it in?
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


#################################################
#Lab 3: Modeling environment-health relationships
#################################################

# read in temperature data from GEE:
MDD_temp <- read.csv("MDD_Temperature.csv", header=T)
head(MDD_temp)

#convert system.index to date:
MDD_temp$Date <- substr(MDD_temp$system.index, 1, 8)
head(MDD_temp)
MDD_temp$Date <- gsub('(.{4})(.{2})(.*)', '\\1-\\2-\\3', MDD_temp$Date)
head(MDD_temp)
MDD_temp <- MDD_temp[,c(2:4)]
MDD_temp$Date <- as.Date(MDD_temp$Date)
head(MDD_temp)

# floor dates to week to summarize:
MDD_temp$Date_floored <- floor_date(MDD_temp$Date, "week")
head(MDD_temp)
length(unique(MDD_temp$Date))
length(unique(MDD_temp$Date_floored))

# summarize temperature by district code and date floored
MDD_temp_weekly <- summaryBy(mean ~ IDDIST + Date_floored, data=MDD_temp, FUN=mean)
head(MDD_temp_weekly)
colnames(MDD_temp_weekly)[1] <- "Dist_Code"
colnames(MDD_temp_weekly)[3] <- "Mean_Temp_C"

### merge dengue and temperature data by floored dates:
Dengue_Temp <- merge(Den, MDD_temp_weekly, by=c("Dist_Code", "Date_floored"), all.x=F, all.y=F)
head(Dengue_Temp)
unique(Dengue_Temp$Year)

#standardize variables:
Dengue_Temp$Mean_Temp_C_std <- zscore(Dengue_Temp$Mean_Temp_C, na.rm=T) # centers around mean of 0 
hist(Dengue_Temp$Mean_Temp_C, n=50)
hist(Dengue_Temp$Mean_Temp_C_std, n=50)

Dengue_Temp$Cases_std <- zscore(Dengue_Temp$Cases, na.rm=T)
hist(Dengue_Temp$Cases, n=50)
hist(Dengue_Temp$Cases_std, n=50)

Dengue_Temp$log.Mean_Temp_C <- log(Dengue_Temp$Mean_Temp_C)
hist(Dengue_Temp$Mean_Temp_C, n=50)
hist(Dengue_Temp$log.Mean_Temp_C, n=50)

Dengue_Temp$log.Cases <- log(Dengue_Temp$Cases + 1)
hist(Dengue_Temp$Cases, n=50)
hist(Dengue_Temp$log.Cases, n=50)

#model dengue cases by temperature:
head(Dengue_Temp)

Dengue_lm <- lm(
  log.Cases ~
    log.Mean_Temp_C
  + I(log.Mean_Temp_C^2)
  + factor(Year)
  + factor(Dist_Code)
  ,
  data=Dengue_Temp,
  na.action=na.exclude
)
summary(Dengue_lm)
crPlots(Dengue_lm)

#find at what temperature dengue peaks:
vertex <- -26.75177/(2*-4.17434)
vertex
plot(Dengue_Temp$Mean_Temp_C, Dengue_Temp$log.Mean_Temp_C)
abline(h=3.2)
abline(v=24.6)
#looks like ~25 C, close to predicted thermal optimum for dengue transmitted by Aedes albopictus (25.4-27.6 C) 

#exploratory plots
Den_plot2 <- ggplot(Dengue_Temp, aes(x=Mean_Temp_C, y=Cases, col=as.factor(District))) +
  geom_point() + theme(legend.position="none")
Den_plot2

Den_plot3 <- ggplot(Dengue_Temp, aes(x=Date_floored, y=Cases, col=as.factor(District))) +
  geom_point() + theme(legend.position="none")
Den_plot3

Den_plot4 <- ggplot(Dengue_Temp, aes(x=Date_floored, y=Mean_Temp_C, col=as.factor(District))) +
  geom_line() #+ theme(legend.position="none")
Den_plot4


##############################
# repeat the above either by adding in the additional Peru environment data from lab 2
# or using your health data and associated environment data from labs 1 and 2
# depending on what you did in earlier labs
