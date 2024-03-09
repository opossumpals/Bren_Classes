## popdemo - DC 
#######

# install.packages("popdemo")
library(popdemo)
library(tidyverse)
library(here)
library(ggplot2)

# polar bear data from paper (Steven)
Pbear # from package
polar_vector <- c(0.106, 0.068, 0.106, 0.461, 0.151, 0.108) # from paper

# Task 1:
# Define a new matrix to feed into Aseq that takes the previous year’s sea ice status into account.
# Make good years more likely if the previous year was good (use 20% likelihood), and conversely
# make bad ice years more likely if the previous year was bad (use 80% likelihood).

p4 <- 0.2 # probability next year is bad given that this year was good
q4 <- 0.8 # probability next year is bad given that this year was bad 

# probability of transitioning from one year to the next 
polar_prob <- matrix(c(rep(c((1-p4)/3, (1-p4)/3, (1-p4)/3, p4/2, p4/2), 3),
                               rep(c((1-q4)/3, (1-q4)/3, (1-q4)/3, q4/2, q4/2), 2)), 5, 5)

polar_proj <- project(Pbear, polar_vector, Aseq = polar_prob, time = 50)
## projects polar bear population over time given the probability of good and bad ice years 
plot(polar_proj)


# Task 2:
# Define 2 new parameters which describe the probability next year is a bad year
# given that this year was a good  (use 50% likelihood), and (b) describe the
# probability next year is a bad year given that this year was a bad year (use
# 80% likelihood again).

p5 <- 0.5 # probability next year is bad given that this year was good 
q5 <- 0.8 # probability next year is bad given that this year was bad

# probability of transitioning from one year to the next 
polar_prob_2 <- matrix(c(rep(c((1-p5)/3, (1-p5)/3, (1-p5)/3, p5/2, p5/2), 3),
                               rep(c((1-q5)/3, (1-q5)/3, (1-q5)/3, q5/2, q5/2), 2)), 5, 5)

polar_proj_2 <- project(Pbear, polar_vector, Aseq=polar_prob_2, time = 50)
## projects polar bear population over time given the probability of good and bad ice years 
plot(polar_proj_2)

# When the probability that next year's ice will be bad given that this year will be good, results in a steep decline and 
# eventually a very low population size. When there is a lower probability that next year's ice will be bad given that 
# this year will be good, there is a steady decline over 20 years before crashing. Both scenarios end with very low polar
# bear population sizes and densities. 

