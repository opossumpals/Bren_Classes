# CL Jerde
# Demography Example
# 8 Feb 2024
#######################

#Clear the R environment
rm(list = ls())


#libraries
library(here)
library(tidyverse)


# Sheep example
sheep_data<-read.csv(here("data","bh_sheep.csv"))
sheep_lx_table<-sheep_data |> mutate(lx=count/count[1]) |> select(age,lx)

#Creat new columns of variables for calculation
life_table_sheep <- sheep_lx_table %>%
  mutate("Lx"=(lx+lead(lx))/2,
         "Lx"=replace(Lx, length(lx), 0),
         "ex"=rev(cumsum(rev(Lx)))/lx
  )

life_table_sheep #output for sheep


###########################
# From bison research
bison_data<-read.csv(here("data","bison_lh.csv"))

#Creat new columns of variables for calculation
life_table_bison <- bison_data %>%
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



