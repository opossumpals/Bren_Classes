## metafolio - DC 
########## 

install.packages("metafolio")
library(metafolio)
library(tidyverse)
library(here)
library(ggplot2)

# 1. We evaluated 10 individual populations as our baseline meta population. Change the number of populations in the 
# baseline scenario with the meta_sim() function and plot the results. How does this affect the base simulation plot 
# (plot_sim_ts)?


## When we increase the number of populations in the baseline scenario, the plots become a lot busier. The range of returns
## catch, escapement, and strays leaving increases. There are also more intense spikes in the returns and catch. It makes
## sense that these would increase since there are now an additional 20 populations of fish - adding exponentially more to
## the simulation. I suspect that if we decreased the number of populations, we would see the opposite happen in the plots. 

############################################ begin copied from class ############################################## 

arma_env_params <- list(mean_value = 16, # mean
                        ar = 0.1, # auto regressive parameter
                        sigma_env = 2, # std deviation of env signal
                        ma = 0) # moving average parameter


# INITIAL from class code 
base1 <- meta_sim(n_pop = 10, # simulation with 10 populations
                  env_params = arma_env_params, # input environmental parameters that were created above
                  env_type = "arma", # use an arima model; run '?arima' in the console to learn more about arima models
                  assess_freq = 5) ## re-assess the fishery every five years.

## This is generating a predictive time series of the status of the simulated portfolio based on previous data 
## (Which is why the fishery is being reassessed periodically).  

plot_sim_ts(base1, years_to_show = 70, burn = 30) 
## Plotting the time series of the simulated metapopulations and their predictive environmental parameters over 70 years

############################################ end copied from class ############################################## 

# NOW we change number of populations 

base1mod <- meta_sim(n_pop = 30, # simulation with 10 populations
                  env_params = arma_env_params, # input environmental parameters that were created above
                  env_type = "arma", # use an arima model; run '?arima' in the console to learn more about arima models
                  assess_freq = 5) ## re-assess the fishery every five years.

## This is generating a predictive time series of the status of the simulated portfolio based on previous data 
## (Which is why the fishery is being reassessed periodically).  

plot_sim_ts(base1mod, years_to_show = 70, burn = 30) 






# 2. Change the weight allocation for the "balanced" conservation strategy so that all 10 populations receive a maximum 
# investment (i.e. 1000) and plot the results. How does the variance and mean of the growth rate shift? Does it shift in 
# the efficiency frontier? Of the two conservation plans, which portfolio is optimal? Why? 



############################################ begin copied from class ############################################## 
w_plans <- list() ## Making this a list to run a for loop over all our portfolios 

## We are going to manipulate the investment weights in each stream by changing the b_i parameter in the ricker model. Since b_i is the carrying capacity of each population, we will use the maximum value 1000.
w_plans[["balanced"]] <- c(5, 1000, 5, 1000, 5, 5, 1000, 5,1000, 5) ## Here we have 10 populations. 1000 means we want to conserve that population. 5 means we do not want to conserve the population. We are then choosing to conserve a balance metapopulation
## (Hint: Play around with these numbers to see how the portfolios change in the end)

w_plans[["one_half"]] <- c(rep(1000, 4), rep(5, 6)) ## Here we are conserving the first 4 populations. We are not conserving the other 6 populations.

w <- list() ## Making a list of stream capacities 
for(i in 1:2) { # loop over plans
  w[[i]] <- list()
  for(j in 1:80) { # loop over iterations
    w[[i]][[j]] <- matrix(w_plans[[i]], nrow = 1)
  }
}

set.seed(1)
arma_sp <- run_cons_plans(w, env_type = "arma", env_params = arma_env_params) ## Running the simulated portfolios using the different prioritization weights from above


plot_cons_plans(arma_sp$plans_mv,
                plans_name = c("Balanced", "One half"),
                cols = c("#E41A1C", "#377EB8"), xlab = "Variance of growth rate",
                ylab = "Mean growth rate") ## Plotting the simulated the portfolios


############################################ end copied from class ############################################## 



w.plans <- list()

w.plans[["balanced"]] <- c(1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000,1000, 10000) # all changed to 1000 

w2 <- list() ## Making a list of stream capacities 
for(i in 1:1) { # loop over plans
  w2[[i]] <- list()
  for(j in 1:80) { # loop over iterations
    w2[[i]][[j]] <- matrix(w.plans[[i]], nrow = 1)
  }
}
set.seed(1)
arma2_sp <- run_cons_plans(w2, env_type = "arma", env_params = arma_env_params) ## Running the simulated portfolios using the different prioritization weights from above

plot_cons_plans(arma2_sp$plans_mv,
                plans_name = c("Balanced"),
                cols = c("#E41A1C"), xlab = "Variance of growth rate",
                ylab = "Mean growth rate") ## Plotting the simulated the portfolios

# 3. What are some other examples of environmental conditions for salmonids that would be beneficial to model using this 
# package? 






