## demography - Chan
#######################

library(here)
library(tidyverse)
library(ggplot2)
## not actually using demogaphy here

bison <- read.csv(here("Portfolio 6 - demography", "Data", "bison_lh.csv"))

# 1. Run the bison example 

#Creating new columns of variables for calculation
life_table_bison <- bison %>%
  mutate("lx*mx"=lx*mx,
         "x*lx*mx"=age*lx*mx,
         "Lx"=(lx+lead(lx))/2,
         "Lx"=replace(Lx, length(lx), 0),
         "ex"=rev(cumsum(rev(Lx)))/lx,
         "R0"=sum(lx*mx),
         "G"=sum(age*lx*mx)/R0,
         "approx.r"=log(R0)/G
  )

life_table_bison #check your work

# 2. Interpret the equations in the life table for yourself (what is the code doing?)
## It's using survivorship at certain ages to calculate life expectancy. By knowing life expectancy, we can estimate the number of
## years an individual will likely survive to given that it has survived to x age. 

# 3. Plot the life expectancy as a function age?
bison_surv_plot <- ggplot(data=life_table_bison, aes(x=age)) + 
  geom_point(aes(y=ex), col="blue", size=3) + 
  labs(x="Bison age", y="Life expectancy")
  
bison_surv_plot

# 4. Why is age 0 life expectancy less than age 1? 
## This has to do with the fact the bison periodically give birth. Age 0 is for any bison born that year (Young of Year).
## As a result, there is a sudden population jump with calving, which corresponds with theie life expectancy. 

# 5. Why are R0, G, and r the same for each age? 
## R0 is the same for each age because it sums the product of lx (survivorship) and mx (fecundity). G sums the product of 
## R0 with age and divides it all by R0, giving a the same value of each age. Finally, r is the log of R0 and G, again resulting
## in the same for the ages. 