# compare slopes starting at same cases for all countries
min_start_value <- 1000
countries <- c('DE','US', 'IT', 'ES','FR','UK','KR','AT','CA','BER')

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

# calculate the running sum... ecdc data has new cases, cases per day
ecdc <- ecdc %>%
  group_by(geoId) %>%
  arrange(date) %>%
  mutate(casesTot = cumsum(cases))

# find the day that had > min_start_value cases per country
start_dates <- filter(ecdc, casesTot > min_start_value) %>%
  group_by(geoId) %>%
  summarise(start_date = min(date))

# get that into an "index" field that can be used for graphing
ecdc <- filter(ecdc, geoId %in% start_dates$geoId) %>%
  inner_join(start_dates,by = c('geoId','geoId')) %>%
  group_by(geoId) %>%
  mutate(casesIndexDate = as.integer(date - start_date))

y_ends <- ecdc %>% 
  group_by(geoId) %>% 
  top_n(1, casesTot) %>% 
  pull(casesTot)

labels <- ecdc %>%
  group_by(geoId) %>%
  top_n(1, casesIndexDate)

ggplot(subset(ecdc,casesIndexDate >= 0), 
       aes(x=casesIndexDate,
           y=casesTot,
           fill=geoId,
           color=geoId)) +
  geom_line(size = 0.1)+
  # slopes are log(2) / doubling days: these are 4,3,2 days
  geom_abline(intercept = log10(min_start_value), slope = (log10(2) / 2), linetype="dashed", color="gray", ) +
  geom_abline(intercept = log10(min_start_value), slope = (log10(2) / 3), linetype="dashed", color="gray", ) +
  geom_abline(intercept = log10(min_start_value), slope = (log10(2) / 4), linetype="dashed", color="gray") +
  geom_abline(intercept = log10(min_start_value), slope = (log10(2) / 5), linetype="dashed", color="gray") +
  geom_text(data=labels, 
            aes(label = geoId, 
                colour = geoId, 
                x = casesIndexDate, 
                y = casesTot), 
            hjust = -0.5, 
            vjust = 0.5) +
  ggtitle("Total cases over time",
          subtitle = paste("Synchronized with day '0' as when the country had",min_start_value, "cases.")) +
  labs(caption=paste("Data from ECDC data set",latest_data_date,"retrieved",retrieved_date)) +
  theme(legend.position = "none") +
  scale_y_continuous("Total cases",
                     trans="log10",
                     label=comma,
                     sec.axis = sec_axis(~ .,
                                         breaks = y_ends,
                                         label = comma),
                     expand = expansion(mult = c(0, 0.1))
                     )+
  scale_x_continuous(paste("Days since",min_start_value,"cases"),
                     expand = expansion(mult = c(0, .1))) 
