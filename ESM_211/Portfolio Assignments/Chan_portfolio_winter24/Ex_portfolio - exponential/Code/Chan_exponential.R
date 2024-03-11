#exponential growth - Chan
###########################



#Example with Japan 

#clean the data for only Japan's population
japan_pop_data<- w_pop_data |> clean_names() |>
  filter(country_name=="Japan") |> 
  select(year, population) |> drop_na()

#On the observed scale
japan_ts<-ggplot(japan_pop_data, aes(x=year, y=population))+
  geom_point()+
  xlab("Year")+
  ylab("Human count (N)")+
  xlim(1949, 2023)+
  ggtitle("Japan's Population on Earth (1950-2022)")+
  theme_bw()
japan_ts

#on the transformed log scale
japan_ts_log<-ggplot(japan_pop_data, aes(x=year, y=population))+
  geom_point()+
  xlab("Year")+
  ylab("Human count (N)")+
  scale_y_continuous(trans="log")+ # log scale for linear transformation
  xlim(1949, 2023)+
  ggtitle("Japan's Population on Earth (1950-2022)")+
  theme_bw()
japan_ts_log

japan.lm_fit<- lm(log(population)~year,data=japan_pop_data)
summary(japan.lm_fit) #how well does it fit? 

NO<-exp(japan.lm_fit$coefficients[1]) #N0=e^B0
r<-japan.lm_fit$coefficients[2]

# On the transformed scale
japan_ts_model_trans<-ggplot(japan_pop_data, aes(x=year, y=population))+
  geom_point()+
  xlab("Year")+
  ylab("Human count (N)")+
  scale_y_continuous(trans="log")+
  geom_smooth(method="lm",color="blue")+
  xlim(1949, 2023)+
  ggtitle("Japan's Population on Earth (1950-2022)")+
  theme_bw()
japan_ts_model_trans


# On the observed scale

# Predicted population values from the model
predicted_df <- data.frame(pop_pred = predict(japan.lm_fit, japan_pop_data), year=japan_pop_data$year)
predicted_df<- predicted_df |> mutate(N_est=exp(pop_pred)) #transformed back to observed population estimates


japan_ts_model<-ggplot(japan_pop_data, aes(x=year, y=population))+
  geom_point(color="black")+
  geom_line(color='red',data = predicted_df, aes(x=year, y=N_est))+
  xlab("Year")+
  ylab("Human count (N)")+
  xlim(1949, 2023)+
  ggtitle("Japan's Population on Earth (1950-2022)")+
  theme_bw()
japan_ts_model
