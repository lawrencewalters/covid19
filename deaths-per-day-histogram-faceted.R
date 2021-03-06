# deaths per day histogram ECDC data and berlin.de data for Berlin
number_of_days <- 180
regression_days <- 20
countries <- c('DE','US', 'IT', 'ES','FR','UK','AT','CA','BER')
Sys.setenv(TZ="Europe/Berlin")

library(readxl)
library(httr)
library(reshape2)
library(ggplot2)
library(dplyr)
library(broom)
library(scales)
library(ggpmisc)

source("load_data.R")
source("utilities.R")

ecdc_list <- EcdcData(countries)
ecdc <- ecdc_list$data
latest_data_date <- ecdc_list$latest_data_date
retrieved_date <- ecdc_list$retrieved_date

#berlin <- BerlinData("data\\daily.csv")
berlin <- BerlinData("https://raw.githubusercontent.com/jakubvalenta/covid-berlin-data/master/covid_berlin_data_incl_hospitalized.csv")

ecdc <- rbind(berlin, ecdc)

# case increase per day graphs
ecdc <- mutate(ecdc, 
               date = as.Date(dateRep, '%Y-%m-%d',tz='Europe/Berlin'),
               wday = as.POSIXlt(date, tz='Europe/Berlin')$wday,
               wkday = ifelse(wday == 0 | wday == 6,0.9,1))

ecdc <- filter(ecdc, date > latest_data_date - number_of_days)

yrng <- range(ecdc$deaths)

regressions <- ecdc %>%
  filter(date > latest_data_date - regression_days) %>%
  group_by(geoId) %>%
  do(model = lm(deaths ~ date, data = .)) %>%
  tidy(model)

labels <- ecdc[,c('geoId','countriesAndTerritories','deaths')] %>%
  group_by(geoId) %>%
  top_n(1,deaths)

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
           y = deaths,
           fill = countriesAndTerritories, 
           alpha=wkday
           )) +
  scale_x_date(date_labels = "%m.%d",
               breaks = "14 days",
               minor_breaks = "7 days",
               name = "Date. Weekends shaded lighter"
  ) +
  theme(axis.text.x = element_text(angle = -90, vjust = 0.3),
        legend.position = "none") +
  geom_col(position="dodge2") +
  geom_smooth(method='lm',formula=y ~ x,
              data = subset(ecdc, 
                            (date > latest_data_date - regression_days))) +
  geom_text_npc(data=labels,aes(npcx = 1, npcy = 1, label = labelText))+
  scale_y_continuous("Deaths per day", label = comma) +
  facet_wrap(~ geoId,
             scales = "free_y",
             labeller = LabellerForGeoIDs()) +
  scale_fill_brewer(palette = "Dark2") +
  scale_alpha(range = c(0.4, 1)) +
  ggtitle(paste("New deaths per day for last", number_of_days,"days as of",format.Date(latest_data_date, "%D")),
          subtitle = paste("p value from linear regression on most recent",regression_days,"days. ECDC data",
                           latest_data_date,
                           "retrieved",
                           format.Date(retrieved_date, "%D %H:%M:%S %Z")))
