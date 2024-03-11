#logistic growth - Chan
###########################


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
