# case increase per day graphs
number_of_days <- 40
regression_days <- 10

# Loading
library(reshape2)
library(ggplot2)
library(dplyr)
library(broom)
library(ggpmisc)

df<-read.csv("data\\daily.csv", header = TRUE)
df <- mutate(df, berlin_increase = cases - lag(cases))
df <- mutate(df, germany_increase = germany_cases - lag(germany_cases))
df <- mutate(df, italy_increase = italy_cases - lag(italy_cases))
# get date as actual date object, and weekend calcs
df <- mutate(df, 
             date = as.Date(date, '%Y-%m-%d'), 
             wday = as.POSIXlt(date)$wday, 
             wkday = ifelse(wday == 0 | wday == 6,0.9,1))

dflong <- melt(df, id.vars = c("date","wday","wkday"),
                   measure.vars = c("berlin_increase"))

variable_names <- list(
  "berlin_increase" = "Berlin"
)

variable_labeller <- function(variable,value){
  return(variable_names[value])
}

latest_data_date <- max(dflong$date)
regressions <- dflong %>%
  filter(date > latest_data_date - regression_days) %>%
  group_by(variable) %>%
  do(model = lm(value ~ date, data = .)) %>%
  tidy(model)

labels <- dflong %>%
  group_by(variable) %>%
  top_n(1,value)

labels <- regressions %>%
  filter(term == 'date') %>%
  inner_join(labels)
labels <- mutate(labels,
                 labelText = paste('p =',
                                   format(`p.value`, 
                                          digits=2, 
                                          nsmall = 1)))

ggplot(subset(dflong, date > Sys.Date() - number_of_days),
       aes(x = date,
           y = value,
           fill = variable,
           alpha = wkday)) +
  geom_col(colour = "black") +
  geom_smooth(method='lm',formula=y ~ x,
              data = subset(dflong, 
                            (date > latest_data_date - regression_days))) +
  geom_text_npc(data=labels,aes(npcx = 1, npcy = 1, label = labelText))+
  scale_x_date(date_labels = "%m.%d",
               breaks = "3 days",
               minor_breaks = "1 day",
               name = "Date"
               ) +
  scale_alpha(range = c(0.4, 1)) +
  theme(axis.text.x = element_text(angle = -90, vjust = 0.3),
        legend.position = "none") +
  scale_y_continuous("New Cases per day") +
  scale_fill_brewer(palette = "Dark2") +
  labs(title = "Berlin new cases per Day",
       subtitle = paste("Data as of",latest_data_date,".p values for last",regression_days,"day linear regression"),
       caption = "Sources: berlin.de")
