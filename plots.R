# Installation
#install.packages('ggplot2)
#install.packages('dplyr')
#install.packages('reshape2')

# Loading
library(reshape2)
library(ggplot2)
library(dplyr)

df<-read.csv("data\\daily.csv", header = TRUE)
df <- mutate(df, berlin_increase = berlin_cases - lag(berlin_cases))
df <- mutate(df, germany_increase = germany_cases - lag(germany_cases))
df <- mutate(df, berlin_cases_log10 = log10(berlin_cases))
df <- mutate(df, date = as.Date(date, '%Y-%m-%d'))
head(df)

# support bar graphs
mdf <- melt(df, id.vars = "date",measure.vars = c("germany_cases","berlin_cases"))
head(mdf)

# new cases per day, log scale
ggplot(data = mdf, aes(x = date, y = value, fill = variable)) + 
  # `geom_col()` uses `stat_identity()`: it leaves the data as is.
  geom_col(position = 'dodge')+
  scale_y_log10("New Cases per day")
  
# new cases per day
ggplot(data=df, aes(x=date)) +
  scale_x_date("Date", date_labels = "%Y-%m-%d", date_breaks = "1 day")+
  theme(axis.text.x = element_text(angle = -90, hjust = 1))+
  geom_col(aes(y=germany_increase, group=1),
           position = "dodge", fill = "yellow", colour = "black")+
  geom_col(aes(y=berlin_increase, group=2),
           position = "dodge", fill = "grey50", colour = "black")+
  scale_y_log10("New Cases per day")

# log10 total cases
ggplot(data=df, aes(x=date)) +
  scale_x_date("Date", date_labels = "%Y-%m-%d", date_breaks = "1 day")+
  theme(axis.text.x = element_text(angle = -90, hjust = 1))+
  geom_line(aes(y=berlin_cases, group=1), color="red")+
  geom_point(aes(y=berlin_cases, group=1), color="red")+
  geom_line(aes(y=germany_cases, group=1))+
  geom_point(aes(y=germany_cases, group=1))+
  scale_y_log10("New Cases per day")

