# cases per day histogram ECDC data
number_of_days <- 30

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

# case increase per day graphs
ecdc <- mutate(ecdc, date = as.Date(dateRep, '%Y-%m-%d'))

ggplot(subset(ecdc, (date > Sys.Date() - number_of_days) & (geoId %in% c('DE','IT','FR','ES','US'))),
       aes(x = date,
           y = cases, # / (Pop_Data.2018 / 100000),
           fill = countriesAndTerritories)) +
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
  scale_y_continuous("New cases per day") +
  facet_wrap(~ countriesAndTerritories,
             scales = "free_y") +
  scale_fill_brewer(palette = "Dark2") +
  ggtitle(paste("New cases per Day",Sys.Date()),
          subtitle = paste("In the last", number_of_days, "days. Data from ECDC data set",latest_data_date,"retrieved",retrieved_date))
