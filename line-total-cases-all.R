# Loading
library(reshape2)
library(ggplot2)
library(dplyr)

df<-read.csv("data\\daily.csv", header = TRUE)
df <- mutate(df, date = as.Date(date, '%Y-%m-%d'))
head(df)

# linear total cases
ggplot(subset(df,date > Sys.Date() - 20), aes(x=date)) +
  scale_x_date(NULL, date_labels = "%Y-%m-%d", date_breaks = "5 day")+
  theme(axis.text.x = element_text(angle = -90, hjust = 1))+
  geom_line(aes(y=berlin_cases, group=1), color="red")+
  geom_line(aes(y=germany_cases, group=1))+
  geom_line(aes(y=italy_cases, group=1), color="green")+
  scale_y_continuous("Total Cases")

# log10 total cases
ggplot(subset(df,date > Sys.Date() - 20), aes(x=date)) +
  scale_x_date(NULL, date_labels = "%Y-%m-%d", date_breaks = "5 day")+
  theme(axis.text.x = element_text(angle = -90, hjust = 1))+
  geom_line(aes(y=berlin_cases, group=1), color="red")+
  geom_line(aes(y=germany_cases, group=1))+
  geom_line(aes(y=italy_cases, group=1), color="green")+
  scale_y_log10("Total Cases (log 10)")


