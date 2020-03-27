# compare slopes starting at same deaths for all countries
min_start_value <- 10
countries <- c('DE','US', 'IT', 'ES','FR')

#install.packages("readxl")
#install.packages("httr")

library(readxl)
library(httr)
library(reshape2)
library(ggplot2)
library(dplyr)

# get the most recent data.... note that URL includes date
retrieved_date <- Sys.time()
latest_data_date <- Sys.Date()
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
ecdc <- filter(ecdc, GeoId %in% countries)
ecdc <- mutate(ecdc, date = as.Date(DateRep, '%Y-%m-%d'))

# calculate the running sum... ecdc data has new cases, deaths per day
ecdc <- ecdc %>%
  group_by(GeoId) %>%
  arrange(date) %>%
  mutate(DeathsTot = cumsum(Deaths))

# find the day that had > min_start_value deaths per country
start_dates <- filter(ecdc, DeathsTot > min_start_value) %>%
  group_by(GeoId) %>%
  summarise(start_date = min(date))

# get that into an "index" field that can be used for graphing
ecdc <- filter(ecdc, GeoId %in% start_dates$GeoId) %>%
  inner_join(start_dates,by = c('GeoId','GeoId')) %>%
  group_by(GeoId) %>%
  mutate(DeathsIndexDate = as.integer(date - start_date))

# data check
head(filter(ecdc, GeoId == 'DE')[1,10:13])

for(country in countries) {
  # do something
}

y_ends <- ecdc %>% 
  group_by(GeoId) %>% 
  top_n(1, DeathsTot) %>% 
  pull(DeathsTot)

#x_labels <-
  
labels <- ecdc %>%
  group_by(GeoId) %>%
  top_n(1, DeathsIndexDate)

ggplot(subset(ecdc,DeathsIndexDate >= 0), 
       aes(x=DeathsIndexDate,
           y=DeathsTot,
           fill=GeoId,
           color=GeoId)) +
  #theme(axis.text.x = element_text(angle = -90, hjust = 1))+
  geom_line(size = 0.1)+
  geom_point(aes(shape = GeoId))+
  geom_text(data=labels, 
            aes(label = GeoId, 
                colour = GeoId, 
                x = DeathsIndexDate, 
                y = DeathsTot), 
            hjust = -0.5, 
            vjust = 0.5) +
  ggtitle("Total deaths over time",
          subtitle = paste("Synchronized with day '0' as when the country had",min_start_value, "deaths. Data from ECDC data set",latest_data_date,"retrieved",retrieved_date)) +
  scale_y_continuous("Total deaths (log scale)", 
                     trans="log10",
                     sec.axis = sec_axis(~ ., breaks = y_ends))+
  scale_x_continuous(paste("Days since",min_start_value,"deaths"))
  expand_limits(x=0, y=50)
  #geom_smooth(method='lm',se=FALSE)

