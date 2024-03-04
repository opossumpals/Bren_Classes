## recapr - DC 
################

install.packages("recapr")

library(tidyverse)
library(here)
library(recapr)


## Silver carp data from Sass et al. (2010) Silver carp study
n1_sc <- 4540
n2_sc <- 2239 #approximated from the paper (not reported)
m2_sc <- 30


# 1. Run the Petersen Estimator for the silver carp parameters 
NPetersen(n1_sc, n2_sc, m2_sc) # population size estimate = 338835.3
ciPetersen(n1_sc, n2_sc, m2_sc) # CI with normal approx = 218401.9 459268.7, CI with bootstrapping = 247928.3 508253.0


# 2. What is the difference between the Petersen and Chapman estimates and the confidence intervals 
## The Petersen estimates and confidence intervals are greater than the Chapman estimates and confidence intervals The Petersen
## is better for smaller sample sizes, while Chapman is better suited for large sample sizes. Even though both assume sampling
## without replacement in the second sampling event, the Chapman estimates are better suited for the Silver Carp data given the 
## sample size.

## Chapman estimates
NChapman(n1_sc, n2_sc, m2_sc) # population size estimate = 328122.9
ciChapman(n1_sc, n2_sc, m2_sc) # CI with normal approx = 215611.3 440634.5, CI with bootstrapping = 242185.7 484372.3



# 3. How many M&Ms are in this bag? 

#First capture
n1_mm<-75 # number of individuals marked in the first sampling
n2_mm<-59 # number of individuals captured in the second sampling effort
m2_mm<- 7 # number of individuals in the second sampling effort that were marked.

# Petersen model
NPetersen(n1_mm, n2_mm, m2_mm) # population size estimate = 632.1429
ciPetersen(n1_mm,n2_mm,m2_mm)  # CI with normal approx = 192.5102 1071.7755, CI with bootstrapping = 368.75 1475.00

#second capture
n3_mm<-118
m3_mm<-14
NPetersen(n1_mm, n3_mm, m3_mm) # population size estimate = 632.1429
ciPetersen(n1_mm,n3_mm,m3_mm) # CI with normal approx = 321.2756 943.0101, CI with bootstrapping = 421.4286 1106.2500

#third capture
n4_mm<- 176
m4_mm<-19
NPetersen(n1_mm, n4_mm, m4_mm) # population size estimate = 694.7368
ciPetersen(n1_mm,n4_mm,m4_mm) # CI with normal approx = 399.6940 989.7797, CI with bootstrapping = 488.8889 1200.0000

#whole bag = 1009
176+142+93+111+128+121+114+124

# Sample size recommendation
n2RR(N=8000, n1=75)
plotn2sim(N=8000, n1=75)
#total: 1171 m&ms in the bag 
  