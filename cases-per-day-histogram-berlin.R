# case increase per day graphs
number_of_days <- 21

# Loading
library(reshape2)
library(ggplot2)
library(dplyr)

df<-read.csv("data\\daily.csv", header = TRUE)
df <- mutate(df, berlin_increase = berlin_cases - lag(berlin_cases))
df <- mutate(df, date = as.Date(date, '%Y-%m-%d'))

# Berlin
ggplot(subset(df,date > Sys.Date() - number_of_days), aes(x=date)) +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "3 day")+
  theme(axis.text.x = element_text(angle = -90, hjust = 1))+
  geom_col(aes(y=berlin_increase, group=2),
           position = "dodge", fill = "grey50", colour = "black")+
  scale_y_continuous("New Cases per day") +
  ggtitle("Berlin New Cases per Day",
          subtitle = paste("in the last",number_of_days, "days"))
  

