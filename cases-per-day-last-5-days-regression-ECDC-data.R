# cases per day histogram ECDC data
number_of_days <- 40
regression_days <- 10
countries <- c('DE','US', 'IT', 'ES','FR','UK','AT','CA')
#countries <- c('AT','IT')
Sys.setenv(TZ="Europe/Berlin")

#install.packages("readxl")
#install.packages("httr")
#install.packages("tidyverse")
library(readxl)
library(httr)
library(reshape2)
library(ggplot2)
library(dplyr)
library(broom)
library(scales)
library(ggpmisc)

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

# case increase per day graphs
ecdc <- mutate(ecdc, 
               date = as.Date(dateRep, '%Y-%m-%d',tz='Europe/Berlin'),
               wday = as.POSIXlt(date, tz='Europe/Berlin')$wday,
               wkday = ifelse(wday == 0 | wday == 6,0.9,1))

ecdc <- filter(ecdc, date > latest_data_date - number_of_days)

yrng <- range(ecdc$cases)

regressions <- ecdc %>%
  filter(date > latest_data_date - regression_days) %>%
  group_by(geoId) %>%
  do(model = lm(cases ~ date, data = .)) %>%
  tidy(model)

labels <- ecdc[,c('geoId','countriesAndTerritories','cases')] %>%
  group_by(geoId) %>%
  top_n(1,cases)

# get just one row from regressions (date is arbitrary)
labels <- regressions %>%
  filter(term=="date") %>%
  inner_join(labels)

labels <- mutate(labels,
                 labelText = paste('p =',
                                   format(`p.value`, 
                                          digits=2, 
                                          nsmall = 1)))

ggplot(subset(ecdc, 
              (date > latest_data_date - number_of_days) & 
                (geoId %in% countries)),
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
  geom_smooth(method='lm',formula=y ~ x,
              data = subset(ecdc, 
                            (date > latest_data_date - regression_days))) +
  geom_text_npc(data=labels,aes(npcx = 1, npcy = 1, label = labelText))+
  scale_y_continuous("New cases per day", label = comma) +
  facet_wrap(~ countriesAndTerritories,
             scales = "free_y") +
  scale_fill_brewer(palette = "Dark2") +
  scale_alpha(range = c(0.4, 1)) +
  ggtitle(paste("New cases per day for last", number_of_days,"days as of",format.Date(latest_data_date, "%D")),
          subtitle = paste("p value from linear regression on most recent",regression_days,"days. ECDC data",
                           latest_data_date,
                           "retrieved",
                           format.Date(retrieved_date, "%D %H:%M:%S %Z")))
