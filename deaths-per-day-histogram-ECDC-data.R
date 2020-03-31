# deaths per day histogram ECDC data
number_of_days <- 21
countries <- c('DE','US', 'IT', 'ES','FR','UK','AT')

#install.packages("readxl")
#install.packages("httr")

library(readxl)
library(httr)
library(reshape2)
library(ggplot2)
library(dplyr)

# get the most recent data.... note that URL includes date
retrieved_date <- Sys.time()
latest_data_date <- Sys.Date() + 1
resp <- list()
resp[["status_code"]] <- 404
while(resp[["status_code"]] == 404)
{
  latest_data_date <- latest_data_date - 1
  url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",format(latest_data_date, "%Y-%m-%d"), ".xlsx", sep = "")
  resp <- GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))
  retrieved_date <- resp[["date"]]
}

#read the Dataset sheet into “R”
ecdc <- read_excel(tf)
ecdc <- filter(ecdc, geoId %in% countries)

# death increase per day graphs
ecdc <- mutate(ecdc, date = as.Date(dateRep, '%Y-%m-%d'),
               wday = as.POSIXlt(date)$wday,
               wkday = ifelse(wday == 0 | wday == 6,0.9,1))

yrng <- range(ecdc$deaths)

ggplot(subset(ecdc, (date > Sys.Date() - number_of_days) & (geoId %in% countries)),
       aes(x = date,
           y = deaths,
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
  scale_y_continuous("New deaths per day") +
  facet_wrap(~ countriesAndTerritories,
             scales = "free_y") +
  scale_fill_brewer(palette = "Dark2") +
  scale_alpha(range = c(0.4, 1)) +
  ggtitle(paste("New deaths per Day",Sys.Date()),
          subtitle = paste("In the last", number_of_days, "days. Data from ECDC data set",latest_data_date,"retrieved",retrieved_date))

