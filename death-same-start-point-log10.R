# compare slopes starting at same deaths for all countries
min_start_value <- 5
# Loading
library(reshape2)
library(ggplot2)
library(dplyr)

df<-read.csv("data\\daily.csv", header = TRUE)
df <- mutate(df, date = as.Date(date, '%Y-%m-%d'))

# find the day that had > min_start_value deaths per country
df[df$berlin_deaths > min_start_value, 1][1]
df <- mutate(df, berlin_index = as.integer(date - df[df$berlin_deaths > min_start_value, 1][1]))
df[df$germany_deaths > min_start_value, 1][1]
df <- mutate(df, germany_index = as.integer(date - df[df$germany_deaths > min_start_value, 1][1]))
df[df$italy_deaths > min_start_value, 1][1]
df <- mutate(df, italy_index = as.integer(date - df[df$italy_deaths > min_start_value, 1][1]))

# get these from a wide format to a long format
dflong <- data.frame(day=integer(),
                     variable=character(),
                     value=integer(),
                     stringsAsFactors=FALSE)

tmplong <- melt(subset(df,berlin_index >= 0), 
               id.vars = "berlin_index",
               measure.vars = c("berlin_deaths"))
names(tmplong)[1] <- "day"
dflong <- rbind(dflong,tmplong)

tmplong <- melt(subset(df,germany_index >= 0), 
               id.vars = "germany_index",
               measure.vars = c("germany_deaths"))

names(tmplong)[1] <- "day"
dflong <- rbind(dflong,tmplong)

tmplong <- melt(subset(df,italy_index >= 0), 
                id.vars = "italy_index",
                measure.vars = c("italy_deaths"))

names(tmplong)[1] <- "day"
dflong <- rbind(dflong,tmplong)


variable_names <- list(
  "berlin_increase" = "Berlin" ,
  "germany_increase" = "Germany",
  "italy_increase" = "Italy"
)

variable_labeller <- function(variable,value){
  return(variable_names[value])
}

y_ends <- dflong %>% 
  group_by(variable) %>% 
  top_n(1, value) %>% 
  pull(value)

ggplot(dflong, 
       aes(x=day,
           y=value,
           fill=variable,
           color=variable)) +
  #theme(axis.text.x = element_text(angle = -90, hjust = 1))+
  geom_line(size = 0.1)+
  ggtitle("Total deaths over time",
          subtitle = paste("Synchronized with day '0' as when the country had",min_start_value, "deaths")) +
  scale_y_continuous("Total deaths (log scale)", 
                     trans="log10",
                     sec.axis = sec_axis(~ ., breaks = y_ends))+
  expand_limits(x=0, y=50)
  #geom_smooth(method='lm',se=FALSE)

