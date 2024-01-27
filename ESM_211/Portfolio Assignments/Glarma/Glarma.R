install.packages("glarma")
library(glarma)
library(here)
library(dplyr)
library(tidyverse)

## Read in data and add intercept
bison <- read.csv(here("ESM_211", "Portfolio Assignments", "Glarma", "Data","bison.csv"))

bison$intercept<-as.integer(1)

## Create column for explanatory variables
bison$major_events <- as.integer(0)

bison$major_events[bison$year==1995] <- 1 # reintroduction of wolves to yellowstone NP

bison$major_events[bison$year==1988] <- 1 # 1988 Yellowstone Fire
  

bison_gg <- ggplot(bison, aes(x=year)) + 
  geom_point(col="black",aes(y=bison),size=3) +
  labs(x="Year", y="Bison Population (Counts)") +
  ylim(0,5000) + xlim(1969,2000)+
  theme_bw()
bison_gg



## Identify variables
y <- bison$bison # y value is bison counts
x0 <- bison %>% select(intercept) # x0 is the null
x0 <- as.matrix(x0) # turns x0 into a matrix 
x1 <- bison %>% select(intercept, major_events) # x1 explanatory variables, presence of major events
x1 <- as.matrix(x1) #turns x1 into a matrix



## Null bison model with phiLag=1 
null_bison_mod_lag1 <- glarma(y, x0, phiLags = c(1), type = "Poi", method = "FS",
                    residuals = "Pearson", maxit = 100, grad = 1e-6)
null_bison_mod_lag1

summary(null_bison_mod_lag1)

#plot.glarma(null_bison_mod_lag1)


## Bison model with explanatory variables and phiLag=1
exp_bison_mod_lag1 <- glarma(y, x1, phiLags = c(1), type = "Poi", method = "FS",
                              residuals = "Pearson", maxit = 100, grad = 1e-6)
exp_bison_mod_lag1

summary(exp_bison_mod_lag1)

#plot.glarma(exp_bison_mod_lag1)



## Null bison glarma model with phiLag=2
null_bison_mod_lag2 <- glarma(y, x0, phiLags = c(2), type = "Poi", method = "FS",
                         residuals = "Pearson", maxit = 100, grad = 1e-6)
null_bison_mod_lag2

summary(null_bison_mod_lag2)

#plot.glarma(null_bison_mod_lag2)


## Bison model with explanatory variables and phiLag=2
exp_bison_mod_lag2 <- glarma(y, x1, phiLags = c(2), type = "Poi", method = "FS",
                             residuals = "Pearson", maxit = 100, grad = 1e-6)
exp_bison_mod_lag2

summary(exp_bison_mod_lag2)

#plot.glarma(exp_bison_mod_lag2)



## Null bison glarma model with phiLag=7
null_bison_mod_lag7 <- glarma(y, x0, phiLags = c(7), type = "Poi", method = "FS",
                         residuals = "Pearson", maxit = 100, grad = 1e-6)
null_bison_mod_lag7

summary(null_bison_mod_lag7)

#plot.glarma(null_bison_mod_lag7)


## Bison model with explanatory variables and phiLag=7
exp_bison_mod_lag7 <- glarma(y, x1, phiLags = c(7), type = "Poi", method = "FS",
                             residuals = "Pearson", maxit = 100, grad = 1e-6)
exp_bison_mod_lag7

summary(exp_bison_mod_lag7)

#plot.glarma(exp_bison_mod_lag7)



## Modified bison plot with lag=1
bison_mod_1 <- bison 

bison_mod_1$est <- exp_bison_mod_lag1$fitted.values #adds fitted values

#Plots the model (black) and the bison count data (gray)
bison_plot_mod_1<-ggplot(bison_mod_1, aes(x=year)) + 
  geom_point(col="gray",aes(y=bison),size=3) + 
  geom_point(col="black",aes(y=est),size=3) +
  geom_line(col="black",aes(y=est)) +
  labs(x="Year", y="Bison Population") +
  ylim(0,4000) + xlim(1970,1997)+
  theme_bw()

#Adds two red horizontal lines for the occurence of fire and reintroduction of wolves
bison_plot_mod_1 <-bison_plot_mod_1 + 
  annotate("rect", xmin = 1988, xmax =1988.1, ymin = 0, ymax = 4000, alpha = .75,fill = "red") + 
  annotate("rect", xmin = 1995, xmax =1995.1, ymin = 0, ymax = 4000, alpha = .75,fill = "red")

ggsave(here("plots","bison_plot_mod_1.jpg"), bison_plot_mod_1, dpi=500, width=6, height=3, unit="in")

bison_plot_mod_1 
#ERROR MESSAGE: Removed 1 rows containing missing values (`geom_point()`). -- Might just ignore this, I don't know what was removed. (DC)



## Modified bison plot with lag=2
bison_mod_2 <- bison 

bison_mod_2$est <- exp_bison_mod_lag2$fitted.values #adds fitted values

#Plots the model (black) and the bison count data (gray)
bison_plot_mod_2<-ggplot(bison_mod_2, aes(x=year)) + 
  geom_point(col="gray",aes(y=bison),size=3) + 
  geom_point(col="black",aes(y=est),size=3) +
  geom_line(col="black",aes(y=est)) +
  labs(x="Year", y="Bison Population") +
  ylim(0,4000) + xlim(1970,1997)+
  theme_bw()

#Adds two red horizontal lines for the occurence of fire and reintroduction of wolves
bison_plot_mod_2 <-bison_plot_mod_2 + 
  annotate("rect", xmin = 1988, xmax =1988.1, ymin = 0, ymax = 4000, alpha = .75,fill = "red") + 
  annotate("rect", xmin = 1995, xmax =1995.1, ymin = 0, ymax = 4000, alpha = .75,fill = "red")

ggsave(here("plots","bison_plot_mod_2.jpg"), bison_plot_mod_2, dpi=500, width=6, height=3, unit="in")

bison_plot_mod_2
#ERROR MESSAGE: Removed 1 rows containing missing values (`geom_point()`). -- Might just ignore this, I don't know what was removed. (DC)



## Modified bison plot with lag=7
bison_mod_7 <- bison 

bison_mod_7$est <- exp_bison_mod_lag7$fitted.values #adds fitted values

#Plots the model (black) and the bison count data (gray)
bison_plot_mod_7<-ggplot(bison_mod_7, aes(x=year)) + 
  geom_point(col="gray",aes(y=bison),size=3) + 
  geom_point(col="black",aes(y=est),size=3) +
  geom_line(col="black",aes(y=est)) +
  labs(x="Year", y="Bison Population") +
  ylim(0,4000) + xlim(1970,1997)+
  theme_bw()

#Adds two red horizontal lines for the occurence of fire and reintroduction of wolves
bison_plot_mod_7 <-bison_plot_mod_7 + 
  annotate("rect", xmin = 1988, xmax =1988.1, ymin = 0, ymax = 4000, alpha = .75,fill = "red") + 
  annotate("rect", xmin = 1995, xmax =1995.1, ymin = 0, ymax = 4000, alpha = .75,fill = "red")

ggsave(here("plots","bison_plot_mod_7.jpg"), bison_plot_mod_7, dpi=500, width=6, height=3, unit="in")

bison_plot_mod_7 
#ERROR MESSAGE: Removed 1 rows containing missing values (`geom_point()`). -- Might just ignore this, I don't know what was removed. (DC)



