# Cases per day histogram ECDC data

#install.packages("readxl")
#install.packages("httr")

library(readxl)
library(httr)
library(reshape2)
library(ggplot2)
library(dplyr)

#create the URL where the dataset is stored with automatic updates every day

url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",format(Sys.time(), "%Y-%m-%d"), ".xlsx", sep = "")

#download the dataset from the website to a local temporary file

GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))

#read the Dataset sheet into “R”
ecdc <- read_excel(tf)

# case increase per day graphs
number_of_days <- 20

ecdc <- mutate(ecdc, date = as.Date(DateRep, '%Y-%m-%d'))

ggplot(subset(ecdc, (date > Sys.Date() - number_of_days) & (GeoId %in% c('DE','IT','FR','ES','US'))),
       aes(x = date,
           y = Cases, # / (Pop_Data.2018 / 100000),
           fill = `Countries and territories`)) +
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
  scale_y_continuous("New Cases per day per 100k people") +
  facet_wrap(~ `Countries and territories`,
             scales = "free_y") +
  scale_fill_brewer(palette = "Dark2") +
  ggtitle(paste("New Cases per Day normalized by Population",Sys.Date()),
          subtitle = paste("Note different scales! In the last", number_of_days, "days"))
