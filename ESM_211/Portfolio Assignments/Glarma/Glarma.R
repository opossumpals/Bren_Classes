install.packages("glarma")
library(glarma)
library(here)
library(dplyr)
library(tidyverse)

data(Asthma)

y <- Asthma[, 1]
X <- as.matrix(Asthma[, 2:16])
glarmamod <- glarma(y, X, thetaLags = 7, type = "NegBin", method = "NR", residuals = "Pearson", alphaInit = 0, maxit = 100, grad = 1e-6)

glarmamod


bison <- read.csv(here("ESM_211", "Portfolio Assignments", "Glarma", "Data","bison.csv"))

bison_ed <- mutate(bison, intercept=1)

y <- bison_ed$bison
x <- as.matrix(bison_ed$year)

bison_mod <- glarma(y, x, phiLags = c(1), type = "Poi", method = "FS",
                    residuals = "Pearson", maxit = 100, grad = 1e-6)
bison_mod

summary(bison_mod)

#plot.glarma(bison_mod)


species_richness <- read.csv(here("ESM_211", "Portfolio Assignments", "Glarma", "Data","SR_df.csv"))