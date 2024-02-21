## growthrates - DC
####################


# Install and load packages
# Install.packages("growthrates")
library(growthrates)
library(tidyverse)
library(ggplot2)
library(here)

## Call in data 
bison <- read.csv(here("Portfolio 1 - growthrates", "Data", "bison.csv"))
summary(bison)

bison_gg <- ggplot(bison, aes(x=year)) + 
  geom_point(col="black",aes(y=bison),size=3) +
  labs(x="Year", y="Bison Population (Counts)") +
  ylim(0,5000) + xlim(1969,2000)+
  theme_bw()
bison_gg

## 1. find r and K 

## 1a. find exponential 

bison_fit <- fit_easylinear(bison$year, bison$bison)
coef(bison_fit)
# r is 1.338e-01

## 1b. logisitc 

p <- c(y0 = 400, mumax = 1.338e-01, K = 5000)

fit1 <- fit_growthmodel(FUN = grow_logistic, 
                        p = p, 
                        bison$year, 
                        bison$bison)

coef(fit1)

plot(fit1, cex.lab = 2, cex.axis = 2)

## 1c. spline 

bison_spline <- fit_spline(bison$year, bison$bison)
coef(bison_spline)

## 2. do the different models agree on parameter values? 
# No the models do not agree on parameter values. 
