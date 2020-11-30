# Deaths per day histogram ECDC data
number_of_days <- 20
regression_days <- 10
#countries <- c('DE-BER','DE')
#countries <- c('DE','IT','UK','SE')
countries <- c('US','CA','MX')
#countries <- c('NORAM')
#countries <- c('DE-BER', 'DE','US','CA','MX','IT','ES','UK')
#countries <- c('ES')

Sys.setenv(TZ="Europe/Berlin")

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
ecdc <- filter(ecdc, geoId %in% countries)

# deaths per day graphs
ecdc <- mutate(ecdc, 
               date = as.Date(dateRep, '%Y-%m-%d',tz='Europe/Berlin'),
               wday = as.POSIXlt(date, tz='Europe/Berlin')$wday,
               wkday = ifelse(wday == 0 | wday == 6,0.9,1))

ecdc <- filter(ecdc, date > latest_data_date - number_of_days)
dflong <- melt(ecdc, id.vars = c("dateRep","day","month","year","countriesAndTerritories","geoId","countryterritoryCode","popData2019","date","wday","wkday"),
               measure.vars = c("cases","deaths"))

# calculate linear regression so that we can display p-value on graphs as labels
regressions <- dflong %>%
  filter(date > latest_data_date - regression_days) %>%
  group_by(geoId,variable) %>%
  do(model = lm(value ~ date, data = .)) %>%
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

fixed_scales <- data.frame(variable=c("cases","cases","deaths","deaths"), x = c(latest_data_date - number_of_days, latest_data_date), y = c(0,max(subset(dflong, variable == "cases")$value),0,max(subset(dflong, variable == "deaths")$value)))

ggplot(dflong,
       aes(x = date,
           y = value,
           fill = variable, 
           alpha=wkday
           )) +
  geom_col(position="dodge2") +
  # linear regression line, only for specific recent timeframe
  geom_smooth(method='lm',formula=y ~ x, se = TRUE,
              data = subset(dflong, 
                            (date > latest_data_date - regression_days))) +
  # x-axis as dates
  scale_x_date(date_labels = "%m.%d",
               breaks = "60 days",
               minor_breaks = "30 day",
               name = "Date. Weekends shaded lighter"
  ) +
  # x-axis written vertically
  theme(axis.text.x = element_text(angle = -90, vjust = 0.3),
        legend.position = "none") +
  # show the p-value in the upper right of each graph
  geom_text_npc(data=labels,aes(npcx = 1, npcy = 1, label = labelText)) +
  # graph per country, for both cases and deaths, let y axis be dynamic
  facet_wrap(vars(variable, geoId),
             scales = "free_y",
             ncol = length(countries),
             labeller = LabellerForGeoIDs()) +
  # zero bound y scale (regressions sometimes go negative), use commas in y scale labels
  scale_y_continuous(NULL, label=comma, limits = c(0,NA)) + 
  # fixed y-axis per facet (cases, deaths)
  geom_blank(data = fixed_scales, aes(x = x, y = y, alpha=1)) +
  scale_fill_brewer(palette = "Dark2") +
  # weekdays shaded lighter
  scale_alpha(range = c(0.4, 1)) +
  # legend
  labs(title = paste("Trends by country in cases and deaths per day over time as of"
                ,latest_data_date),
          subtitle = paste("p value from linear regression on most recent",
                           regression_days,"days."),
          caption= paste("Source: Berlin - berlin.de, all others ECDC data",
                           latest_data_date,
                           "retrieved",
                           format.Date(retrieved_date, "%Y-%m-%d %H:%M:%S %Z")))
