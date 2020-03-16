# case increase per day graphs

# Loading
library(reshape2)
library(ggplot2)
library(dplyr)

df<-read.csv("data\\daily.csv", header = TRUE)
df <- mutate(df, berlin_increase = berlin_cases - lag(berlin_cases))
df <- mutate(df, germany_increase = germany_cases - lag(germany_cases))
df <- mutate(df, italy_increase = italy_cases - lag(italy_cases))
df <- mutate(df, s_korea_increase = s_korea_cases - lag(s_korea_cases))
df <- mutate(df, china_increase = china_cases - lag(china_cases))
df <- mutate(df, date = as.Date(date, '%Y-%m-%d'))
head(df)

# cases per day, overlapping, germany + berlin
ggplot(data=df, aes(x=date)) +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 day")+
  theme(axis.text.x = element_text(angle = -90, hjust = 1))+
  geom_col(aes(y=germany_increase, group=1),
           position = "dodge", fill = "yellow", colour = "black")+
  geom_col(aes(y=berlin_increase, group=2),
           position = "dodge", fill = "grey50", colour = "black")+
  scale_y_log10("New Cases per day")



# support multi-bar graphs of total cases
cases_melted_df <- melt(df, id.vars = "date",measure.vars = c("germany_cases","berlin_cases","italy_cases","china_cases","s_korea_cases"))
head(cases_melted_df)

# total cases, log scale
ggplot(data = cases_melted_df, aes(x = date, y = value, fill = variable)) + 
  # `geom_col()` uses `stat_identity()`: it leaves the data as is.
  geom_col(position = 'dodge')+
  scale_y_log10("Total Cases")


# new cases per day
# support multi-bar graphs of cases per day
increase_melted_df <- melt(df, id.vars = "date",
                        measure.vars = c("germany_increase",
                                         "berlin_increase",
                                         "italy_increase",
                                         "china_increase",
                                         "s_korea_increase"))
head(increase_melted_df)
# new cases, log scale
ggplot(data = increase_melted_df, aes(x = date, y = value, fill = variable)) + 
  # `geom_col()` uses `stat_identity()`: it leaves the data as is.
  geom_col(position = 'dodge')+
  scale_y_log10("New cases per day")

# new cases, linear scale, italy, china
test <- subset(increase_melted_df,
               variable %in% c("italy_increase","china_increase"))
head(test)
ggplot(data = test, 
       aes(x = date, y = value, fill = variable)) + 
  # `geom_col()` uses `stat_identity()`: it leaves the data as is.
  geom_col(position = 'dodge')+
  scale_y_log10("New cases per day")

# linear
ggplot(data=df, aes(x=date)) +
  scale_x_date(NULL, date_labels = "%Y-%m-%d", date_breaks = "5 day")+
  theme(axis.text.x = element_text(angle = -90, hjust = 1))+
  geom_line(aes(y=berlin_cases, group=1), color="red")+
  geom_point(aes(y=berlin_cases, group=1), color="red")+
  geom_line(aes(y=germany_cases, group=1))+
  geom_point(aes(y=germany_cases, group=1))+
  geom_line(aes(y=italy_cases, group=1), color="green")+
  geom_point(aes(y=italy_cases, group=1), color="green")+
  geom_line(aes(y=s_korea_cases, group=1), color="blue")+
  geom_point(aes(y=s_korea_cases, group=1), color="blue")+
  scale_y_continuous("New Cases per day")
