## FSA - DC
####################

# packages 
library(FSA)
library(FSAdata)
library(tidyverse)
library(stringr)
library(janitor)

# we are going to use data of estimated catch-at-age for Gulf Menhaden from 1964-2004
data<-FSAdata::Menhaden1 %>% 
  #pivot the data to get it in the required format
  pivot_longer(cols = age0:age6, names_to = "age", values_to = "ct") %>% 
  #then use group by and summarize to get one total catch value by age class
  group_by(age) %>% summarise(catch=sum(ct))

#use string manipulations to remove the leading age_ from the values in the age column
data$age<-str_replace_all(data$age, "age", "")

#convert columns to numerics
data$age<-as.numeric(data$age)

#take the log of catch
data$logct <- log(data$catch)

#examine the structure of the dataframe
str(data)
plot(logct~age,data=data,ylab="log(Catch)",pch=19)

## 1. Compare and contrast the initial population sizes between the three methods.
#### Leslie method has an N0 of 1065, Delury has an N0 of 1077, and k-pass has an N0 of 985. 
snapper <- FSAdata::Pathfinder
snapper_pzonatus <-snapper %>% clean_names() 

#### Leslie Method 
#use the depletion() to calculate N0 and q
leslie <- with(snapper_pzonatus, depletion(pzonatus,effort,method="Leslie",Ricker.mod = TRUE)) #the ricker.mod is a modification that is used within the equation
summary(leslie) 
#remember that N0 = initial population size and q = catchability coefficient or the fraction of the population that is removed by one unit of fishing effort

#find the confidence intervals of this data
confint(leslie)

#plot it
plot(leslie)

#### DeLury Method 
delury <- with(snapper_pzonatus, depletion(pzonatus, effort, method="Delury", Ricker.mod = TRUE))
summary(delury)

confint(delury)

plot(delury)

#### K-pass Method 
#we only need catch data for this method, we will still get N0 but will also get p which is probability of capture
k_pass <- with(snapper_pzonatus, removal(pzonatus)) 
summary(k_pass)

confint(k_pass)



## 2. Why are the Leslie and DeLury methods not valid at estimating N0? 
#### The Leslie and DeLury methods both operate on a host of assumptions, like being a closed system and constant catchability.
#### This is unrealistic in a realistic fisheries model. 

## 3. What values can you look at to determine the reliability of the data? 
  #### Hint: Think back to what we learned about statistics in 206) 
#### We want a low standard error for determinet hat our data is reliable. 

## 4. How do the estimates of Z and A change between the weighted and non-weighted regressions? How does the confidence interval change? 
### The Z estimate does not change much between weighted (2.59) and non-weighted (2.29) regressions. 
### A estimate changes a bit more between weighted (92.55) and non-weighted (9.84) regressions. 
### There is a greater difference between the confidence intervals of the two regressions. The weighted results have higher confidence intervals than non-weighted. 

# Non-Weighted Regression 

#we will use years 1-6
non_weighted_results <- FSA::catchCurve(catch~age,data=data,ages2use=1:6)

#show the estimated values of Z and A (and std. deviation)
#Z is the instantaneous mortality rate (think slope of line)
#A is the annual mortality rate
summary(non_weighted_results)

#show the confidence intervals associated with these values
confint(non_weighted_results)

#plot the points with the results for Z and A
plot(non_weighted_results)

# Weighted Regression 

weighted_results <- FSA::catchCurve(catch~age,data=data,
                                    ages2use=2:6,use.weights=TRUE)

summary(weighted_results)

confint(weighted_results)

plot(weighted_results)