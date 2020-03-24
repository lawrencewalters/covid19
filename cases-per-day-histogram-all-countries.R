# case increase per day graphs
number_of_days <- 19

# Loading
library(reshape2)
library(ggplot2)
library(dplyr)

df<-read.csv("data\\daily.csv", header = TRUE)
df <- mutate(df, berlin_increase = berlin_cases - lag(berlin_cases))
df <- mutate(df, germany_increase = germany_cases - lag(germany_cases))
df <- mutate(df, italy_increase = italy_cases - lag(italy_cases))
df <- mutate(df, date = as.Date(date, '%Y-%m-%d'))


dflong <- melt(df, id.vars = "date",
                   measure.vars = c("berlin_increase", 
                                    "germany_increase",
                                    "italy_increase"))
variable_names <- list(
  "berlin_increase" = "Berlin" ,
  "germany_increase" = "Germany",
  "italy_increase" = "Italy"
)

variable_labeller <- function(variable,value){
  return(variable_names[value])
}

weekend_labeller <- function(variable, value) {
  return("hello")
}

x_end <- dflong %>% 
  group_by(date) %>% 
  top_n(1, date) %>%
  pull(date)


ggplot(subset(dflong, date > Sys.Date() - number_of_days),
       aes(x = date,
           y = value,
           fill = variable)) +
  scale_x_date(date_labels = "%m.%d",
               breaks = "3 days",
               minor_breaks = "1 day",
               name = "Date",
               #labels = weekend_labeller
               #limits = c(Sys.Date() - number_of_days,Sys.Date()),
               #expand=c(0,0)
               ) +
  theme(axis.text.x = element_text(angle = -90, vjust = 0.3),
        legend.position = "none") +
  geom_col(colour = "black") +
  scale_y_continuous("New Cases per day") +
  facet_wrap(~ variable,
             scales = "free_y",
             labeller = variable_labeller) +
  scale_fill_brewer(palette = "Dark2") +
  ggtitle(paste("New Cases per Day",Sys.Date()),
          subtitle = paste("Note different scales! In the last", number_of_days, "days"))
