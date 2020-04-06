# Age distribution of cases over time in Berlin

# sorting tool
age_group_sort <- c(c("0-4","5-9","10-14","15-19","20-24","25-29","30-39","40-49","50-59","60-69","70-79","80+","unk"))
dates <- c("2020-03-11","2020-03-18","2020-03-25","2020-04-02")

# Loading
library(reshape2)
library(ggplot2)
library(dplyr)
library(broom)

df<-read.csv("data\\age-distribution.csv", header = TRUE)
df <- filter(df,date %in% dates)
# get date as actual date object, and weekend calcs
df <- mutate(df, 
             date = as.Date(date, '%Y-%m-%d'), 
             wday = as.POSIXlt(date)$wday, 
             wkday = ifelse(wday == 0 | wday == 6,0.9,1))
latest_data_date = max(df$date)

# get increase by place and age group (assumes sorted by date?)
df <- df %>%
  group_by(place,ageGroup) %>%
  mutate(increase = value - lag(value, default=0))

# get percentage distribution on total
df <- df %>%
  group_by(date,place) %>%
  mutate(valuePct = value / sum(value) * 100)

# get percentage distribution of new cases
df <- df %>%
  group_by(date,place) %>%
  mutate(increasePct = increase / sum(increase) * 100)

df[order(df$date,factor(df$ageGroup,levels=age_group_sort)),]

ggplot(df[order(df$date),],
       aes(x = ageGroup,
           y = increasePct,
           fill = date)) +
  geom_bar(stat = 'identity', colour = "black", 
           position="dodge2",
           show.legend = TRUE) + #position_dodge(width = NULL, preserve = "total")) +
  scale_y_continuous("Percent of New Cases since last time") +
  scale_x_discrete(limits=age_group_sort) +
  labs(title = "Berlin age distribution over time",
       subtitle = paste("Data from",min(df$date),"to",max(df$date)),
       caption = "Source: berlin.de (Berlin)")
