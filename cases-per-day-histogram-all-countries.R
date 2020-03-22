# case increase per day graphs

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

# Berlin
ggplot(subset(dflong,date > Sys.Date() - 20), 
       aes(x=date,
           y=value,
           fill=variable)) +
  scale_x_date(date_labels = "%Y-%m-%d",
               date_breaks = "7 day",
               name = "Date")+
  theme(axis.text.x = element_text(angle = -90, hjust = 1),
        legend.position="none")+
  geom_col(colour="black")+
  scale_y_continuous("New Cases per day")+
  facet_wrap(~variable, 
             scales="free_y", 
             labeller= variable_labeller)+
  scale_fill_brewer(palette="Dark2")

