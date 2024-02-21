#script for simple mark recapture
#Feb 12 2024
#CL Jerde
##################################

library(here)
library(tidyverse)
library(recapr)


# 'recapr' package https://cran.r-project.org/web/packages/recapr/recapr.pdf

n1<-75 # number of individuals marked in the first sampling
n2<-100 # number of individuals captured in the second sampling effort
m2<- 4 # number of individuals in the second sampling effort that were marked.
  
# Runs the model, get the estimate of N and calculates a confidence interval
NPetersen(n1, n2, m2) # estimates the population size
ciPetersen(n1,n2,m2)  # 95% CIs using normal approximation ($ciNorm) and bootstrapping ($ciBoot)

# Sample size recommendation
n2RR(N=8000, n1=75)
plotn2sim(N=8000, n1=75)


#Sass et al. (2010) Silver carp study
n1_sc <- 4540
n2_sc <- 2239 #approximated from the paper (not reported)
m2_sc <- 30

NChapman(n1_sc, n2_sc, m2_sc)
ciChapman(n1_sc, n2_sc, m2_sc)

# Sample size recommendation
n2RR(N=328192, n1=4540)
plotn2sim(N=328192, n1=4540)




#M&Ms 

#total: 1171 m&ms in the bag 

