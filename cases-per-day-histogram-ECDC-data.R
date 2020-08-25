# cases per day histogram ECDC data
number_of_days <- 45
countries <- c('DE','US', 'IT', 'ES','FR')
Sys.setenv(TZ="Europe/Berlin")

library(httr)
library(reshape2)
library(ggplot2)
library(dplyr)

source("load_data.R")
source("utilities.R")

ecdc_list <- EcdcData(countries)
ecdc <- ecdc_list$data
latest_data_date <- ecdc_list$latest_data_date
retrieved_date <- ecdc_list$retrieved_date

# case increase per day graphs
ecdc <- mutate(ecdc, date = as.Date(dateRep, '%Y-%m-%d'),
               wday = as.POSIXlt(date)$wday,
               wkday = ifelse(wday == 0 | wday == 6,0.9,1))

yrng <- range(ecdc$cases)

ggplot(subset(ecdc, (date > Sys.Date() - number_of_days) & (geoId %in% countries)),
       aes(x = date,
           y = cases,
           fill = countriesAndTerritories, 
           alpha=wkday
           )) +
  scale_x_date(date_labels = "%m.%d",
               breaks = "3 days",
               minor_breaks = "1 day",
               name = "Date. Weekends shaded lighter"
  ) +
  theme(axis.text.x = element_text(angle = -90, vjust = 0.3),
        legend.position = "none") +
  geom_col(colour = "black") +
  scale_y_continuous("New cases per day") +
  facet_wrap(~ countriesAndTerritories,
             scales = "free_y") +
  scale_fill_brewer(palette = "Dark2") +
  scale_alpha(range = c(0.4, 1)) +
  ggtitle(paste("New cases per Day",Sys.Date()),
          subtitle = paste("In the last", number_of_days, "days. Data from ECDC data set",latest_data_date,"retrieved",retrieved_date))
