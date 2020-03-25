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
number_of_days <- 19

ecdc <- mutate(ecdc, date = as.Date(DateRep, '%Y-%m-%d'))

x_end <- ecdc %>% 
  group_by(date) %>% 
  top_n(1, date) %>%
  pull(date)

ggplot(subset(ecdc, (date > Sys.Date() - number_of_days) & GeoId == 'DE'),
       aes(x = date,
           y = Cases,
           fill = "Countries and territories")) +
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
  facet_wrap(~ GeoId,
             scales = "free_y") +
  scale_fill_brewer(palette = "Dark2") +
  ggtitle(paste("New Cases per Day",Sys.Date()),
          subtitle = paste("Note different scales! In the last", number_of_days, "days"))
