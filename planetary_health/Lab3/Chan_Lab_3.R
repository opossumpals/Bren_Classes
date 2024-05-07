#################################################
#Lab 3: Modeling environment-health relationships
#################################################


# read in temp data 
ca_temp <- read.csv("CAcounties_Temperature_2015-2020.csv")
head(ca_temp)

# create column for year 
ca_temp$Year <- as.integer(substr(ca_temp$system.index, 1, 4))
head(ca_temp)

# clean up temp data. Average temp in each county per year 
county_avg <- ca_temp %>%
  group_by(COUNTY_NAM, Year) %>%
  summarise(average_temp = mean(mean, na.rm = TRUE)) %>%
  ungroup()
head(county_avg)

county_avg <- county_avg %>% 
  rename(COUNTY = COUNTY_NAM) 

county_avg <- county_avg %>% 
  rename(YEAR = Year)


# read in asthma data and clean
asthma <- read.csv("asthma-hospitalization-rates-by-county-2015_2020.csv")
asthma_clean <- asthma %>%  
  filter(STRATA == "Total population")
head(asthma_clean)

# merge asthma and temperature data by year:
asthma_temp <- merge(asthma_clean, county_avg, by=c("COUNTY", "YEAR"), all.x=F, all.y=F)
head(asthma_temp)
unique(asthma_temp$Year)

# reclass number of hospitalization to numeric 
asthma_temp$NUMBER.OF.HOSPITALIZATIONS <- as.numeric(asthma_temp$NUMBER.OF.HOSPITALIZATIONS)
class(asthma_temp$NUMBER.OF.HOSPITALIZATIONS)

# standardize variables:
asthma_temp$average_temp_std <- zscore(asthma_temp$average_temp, na.rm=T) # centers around mean of 0 
hist(asthma_temp$average_temp, n=50)
hist(asthma_temp$average_temp_std, n=50)

asthma_temp$NUMBER.OF.HOSPITALIZATIONS_std <- zscore(asthma_temp$NUMBER.OF.HOSPITALIZATIONS, na.rm=T)
hist(asthma_temp$NUMBER.OF.HOSPITALIZATIONS, n=50)
hist(asthma_temp$NUMBER.OF.HOSPITALIZATIONS_std, n=50)

asthma_temp$log.avg_temp <- log(asthma_temp$average_temp)
hist(asthma_temp$average_temp, n=50)
hist(asthma_temp$log.avg_temp, n=50)

asthma_temp$log.hospitalization <- log(asthma_temp$NUMBER.OF.HOSPITALIZATIONS + 1)
hist(asthma_temp$NUMBER.OF.HOSPITALIZATIONS, n=50)
hist(asthma_temp$log.hospitalization, n=50)


# model asthma cases by temperature:
head(asthma_temp)

asthma_lm <- lm(
  log.hospitalization ~
    log.avg_temp
  + I(log.avg_temp^2)
  + factor(YEAR)
  + factor(COUNTY)
  ,
  data=asthma_temp,
  na.action=na.exclude
)
summary(asthma_lm)
crPlots(asthma_lm)


# find at what temperature asthma peaks:
vertex <- -26.75177/(2*-4.17434)
vertex

plot(asthma_temp$average_temp, asthma_temp$log.avg_temp)
abline(h=3.2)
abline(v=24.6)

# close to 24 degrees C

#exploratory plots
asthma_plot2 <- ggplot(asthma_temp, aes(x=average_temp, y=NUMBER.OF.HOSPITALIZATIONS, col=as.factor(COUNTY))) +
  geom_point() + theme(legend.position="none")
asthma_plot2

asthma_plot3 <- ggplot(asthma_temp, aes(x=YEAR, y=NUMBER.OF.HOSPITALIZATIONS, col=as.factor(COUNTY))) +
  geom_point() + theme(legend.position="none")
asthma_plot3

asthma_plot4 <- ggplot(asthma_temp, aes(x=YEAR, y=average_temp, col=as.factor(COUNTY))) +
  geom_line() #+ theme(legend.position="none")
asthma_plot4

# seems to be a dip in asthma cases after 2018. relatively stable cases from 2015-2018. Hotter counties have higher hospitalizations from asthma. 