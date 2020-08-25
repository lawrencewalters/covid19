# compare slopes starting at same deaths for all countries
min_start_value <- 1000
countries <- c('DE','US', 'IT', 'AT','BER')

#install.packages("readxl")
#install.packages("httr")

library(readxl)
library(httr)
library(reshape2)
library(ggplot2)
library(dplyr)
library(scales)

source("load_data.R")
source("utilities.R")

ecdc_list <- EcdcData(countries)
ecdc <- ecdc_list$data
latest_data_date <- ecdc_list$latest_data_date
retrieved_date <- ecdc_list$retrieved_date

#berlin <- BerlinData("data\\daily.csv")
berlin <- BerlinData("https://raw.githubusercontent.com/jakubvalenta/covid-berlin-data/master/covid_berlin_data_incl_hospitalized.csv")

ecdc <- rbind(berlin, ecdc)
ecdc <- mutate(ecdc, date = as.Date(dateRep, '%Y-%m-%d'))

# calculate the running sum... ecdc data has new cases, deaths per day
ecdc <- ecdc %>%
  group_by(geoId) %>%
  arrange(date) %>%
  mutate(deathsTot = cumsum(deaths))

# find the day that had > min_start_value deaths per country
starts <- filter(ecdc, deathsTot > min_start_value) %>%
  group_by(geoId) %>%
  summarise(startDate = min(date),deathsAdj = min(deathsTot) - min_start_value)

# get that into an "index" field that can be used for graphing
ecdc <- filter(ecdc, geoId %in% starts$geoId) %>%
  inner_join(starts,by = c('geoId','geoId')) %>%
  group_by(geoId) %>%
  mutate(deathsIndexDate = as.integer(date - startDate), 
         adjDeathsTot = deathsTot - deathsAdj)

y_ends <- ecdc %>% 
  group_by(geoId) %>% 
  top_n(1, deathsTot) %>% 
  pull(deathsTot)

labels <- ecdc %>%
  group_by(geoId) %>%
  top_n(1, deathsIndexDate)

ggplot(subset(ecdc,deathsIndexDate >= 0), 
       aes(x=deathsIndexDate,
           y=adjDeathsTot,
           fill=geoId,
           color=geoId)) +
  # slopes are log(2) / doubling days: these are 4,3,2 days
  geom_abline(intercept = log10(min_start_value), slope = (log10(2) / 2), linetype="dashed", color="gray", ) +
  geom_abline(intercept = log10(min_start_value), slope = (log10(2) / 3), linetype="dashed", color="gray") +
  geom_abline(intercept = log10(min_start_value), slope = (log10(2) / 4), linetype="dashed", color="gray") +
  geom_line(size = 0.1)+
  geom_text(data=labels, 
            aes(label = geoId, 
                colour = geoId, 
                x = deathsIndexDate, 
                y = deathsTot), 
            hjust = -0.5, 
            vjust = 0.5) +
  ggtitle("Total deaths over time",
          subtitle = paste("Starting at",min_start_value, "deaths. ECDC data ",latest_data_date,"retrieved",retrieved_date)) +
  theme(legend.position = 'none') +
  scale_y_continuous("Total deaths", 
                     trans="log10",
                     sec.axis = sec_axis(~ ., 
                                         breaks = y_ends,
                                         label = comma),
                     expand = expansion(mult = c(0, .1)),
                     label = comma) +
  scale_x_continuous(paste("Days since",min_start_value,"deaths"),
                     expand = expansion(mult = c(0, .1)))

