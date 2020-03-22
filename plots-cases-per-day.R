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
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "3 day")+
  theme(axis.text.x = element_text(angle = -90, hjust = 1))+
  geom_col(aes(y=germany_increase, group=1),
           position = "dodge", fill = "yellow", colour = "black")+
  geom_col(aes(y=berlin_increase, group=2),
           position = "dodge", fill = "grey50", colour = "black")+
  scale_y_continuous("New Cases per day")

# Berlin
ggplot(subset(df,date > as.Date("2020-02-15")), aes(x=date)) +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "3 day")+
  theme(axis.text.x = element_text(angle = -90, hjust = 1))+
  geom_col(aes(y=berlin_increase, group=2),
           position = "dodge", fill = "grey50", colour = "black")+
  scale_y_continuous("New Cases per day")

# berlin last 20 days
ggplot(subset(df,date > Sys.Date() - 20), aes(x=date)) +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "3 day")+
  theme(axis.text.x = element_text(angle = -90, hjust = 1))+
  geom_col(aes(y=berlin_increase, group=2),
           position = "dodge", fill = "grey50", colour = "black")+
  scale_y_continuous("New Cases per day")

# support multi-bar graphs of total cases
cases_melted_df <- melt(df, 
                        id.vars = "date",
                        measure.vars = c("germany_cases",
                                         "berlin_cases",
                                         "italy_cases",
                                         "china_cases",
                                         "s_korea_cases"))
head(cases_melted_df)

# bar graph total cases, log scale
ggplot(subset(cases_melted_df,date > Sys.Date() - 20),
       aes(x = date, y = value, fill = variable)) + 
  geom_col(position = 'dodge')+
  scale_y_log10("Total Cases")

# line graph total cases, log scale
ggplot(subset(cases_melted_df,date > Sys.Date() - 20),
       aes(x = date, y = value, fill = variable, col = variable)) + 
  scale_x_date(NULL, date_labels = "%Y-%m-%d", date_breaks = "5 day")+
  theme(axis.text.x = element_text(angle = -90, hjust = 1))+
  geom_line()+
  scale_y_log10("Total Cases log scale")

# line graph total cases, linear scale
ggplot(subset(cases_melted_df,date > Sys.Date() - 20),
       aes(x = date, y = value, fill = variable, col = variable)) + 
  scale_x_date(NULL, date_labels = "%Y-%m-%d", date_breaks = "5 day")+
  theme(axis.text.x = element_text(angle = -90, hjust = 1))+
  geom_line()+
  scale_y_continuous("Total Cases")


# new cases per day log10 all countries
# support multi-bar graphs of cases per day
increase_melted_df <- melt(df, id.vars = "date",
                        measure.vars = c("germany_increase",
                                         "berlin_increase",
                                         "italy_increase",
                                         "china_increase",
                                         "s_korea_increase"))
ggplot(data = increase_melted_df, aes(x = date, y = value, fill = variable)) + 
  geom_col(position = 'dodge')+
  scale_y_log10("New cases per day")

# new cases per day germany
# support multi-bar graphs of cases per day
increase_melted_df <- melt(df, id.vars = "date",
                           measure.vars = c("germany_increase",
                                            "berlin_increase"))
ggplot(data = increase_melted_df, aes(x = date, y = value, fill = variable)) + 
  geom_col(position = 'dodge')+
  scale_y_continuous("New cases per day")

ggplot(data = increase_melted_df, aes(x = date, y = value, fill = variable)) + 
  geom_col(position = 'dodge')+
  scale_y_log10("New cases per day Log Scale")


# new cases, linear scale, italy, china
test <- subset(increase_melted_df,
               variable %in% c("italy_increase","china_increase"))

head(test)
ggplot(data = test, 
       aes(x = date, y = value, fill = variable)) + 
  geom_col(position = 'dodge')+
  scale_y_log10("New cases per day")

