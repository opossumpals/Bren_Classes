# Logistic growth of Human Population & Bison
# Example
# Christopher L Jerde
# Notes:Data from https://ourworldindata.org/population-growth#introduction
########################################

#Clear the R environment
rm(list = ls())

#Libraries needed
library(janitor) #cleans data and names
library(here) #allows for localized file directory
library(tidyverse) #makes R work nicely
library(growthcurver) # older package, for logistic growth

#get the data
w_pop_data<-read_csv(here("ESM_211", "InClass_01162024", "Data","population-and-demography.csv"))

#clean the data for only the global human population
w_pop_data<- w_pop_data |> clean_names() |>
  filter(country_name=="World") |> 
  select(year, population) |> drop_na()

#transform year to time, must be done to use growthcurver. just subtracts first year to make it year 0
w_pop_data <- w_pop_data |> mutate(time = year - year[1])

#using the growthcurver package
#here: https://cran.r-project.org/web/packages/growthcurver/vignettes/Growthcurver-vignette.html

#recall: logistic growth involves a limitation of resources (there is a carrying capacity)
human_fit<-SummarizeGrowth(w_pop_data$time,w_pop_data$population)
human_fit
plot(human_fit) #what is funny with this graph?  N at 0 is what?  
w_pop_data$population[1] # need to add this to the y population values because the previous figure has a 0 population at time 0.  
est_K_human<-human_fit$vals$k + w_pop_data$population[1] #estimated K for the data = 8.5 billion 

#bison data
bison_data<-read_csv(here("ESM_211", "InClass_01162024", "Data", "bison.csv"))

#transform year to time
bison_data <- bison_data |> mutate(time = year - year[1])

bison_fit<-SummarizeGrowth(bison_data$time,bison_data$bison)
bison_fit
plot(bison_fit)
est_K_bison<-bison_fit$vals$k + bison_data$bison[1] #estimated K for the data





#Example test: Japan
#clean the data for only the global human population
japan_pop_data<- w_pop_data |> clean_names() |>
  filter(country_name=="Japan") |> 
  select(year, population) |> drop_na()

#transform year to time, must be done to use growthcurver. just subtracts first year to make it year 0
japan_pop_data <- japan_pop_data |> mutate(time = year - year[1])

#using the growthcurver package
#here: https://cran.r-project.org/web/packages/growthcurver/vignettes/Growthcurver-vignette.html

#recall: logistic growth involves a limitation of resources (there is a carrying capacity)
human_fit<-SummarizeGrowth(japan_pop_data$time,japan_pop_data$population)
human_fit
plot(human_fit) #what is funny with this graph?  N at 0 is what?  
japan_pop_data$population[1] # need to add this to the y population values because the previous figure has a 0 population at time 0.  
est_K_human<-human_fit$vals$k + cr_pop_data$population[1] #estimated K for the data = 44 million








