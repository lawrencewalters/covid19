# total cases per capita starting at same cases for all countries
min_start_value <- 50
countries <- c('DE','BER','UK','US','CA','FR','AT','ES','IT')
capita <- 100000
capita_word <- format(capita, scientific=FALSE,big.mark=",")
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


# get my hand managed data
df<-read.csv("data\\daily.csv", header = TRUE)
df <- mutate(df, cases = cases - lag(cases, default = 0))
df <- mutate(df, deaths = berlin_deaths - lag(berlin_deaths, default = 0))
# get date as actual date object, and weekend calcs
df <- mutate(df, 
             dateRep = as.Date(date, '%Y-%m-%d'), 
             day = as.double(format.Date(dateRep, "%e")),
             month = as.double(format.Date(dateRep, "%m")),
             year = as.double(format.Date(dateRep, "%Y")),
             countriesAndTerritories = 'Berlin',
             geoId = "BER",
             countryterritoryCode = "BER",
             popData2018 = 3769495)
df <- subset(df, select = c(dateRep, day, month, year, cases, deaths, countriesAndTerritories, geoId, countryterritoryCode, popData2018))

ecdc <- rbind(df, ecdc)


ecdc <- mutate(ecdc, date = as.Date(dateRep, '%Y-%m-%d'))

# calculate the running sum... ecdc data has new cases, cases per day
ecdc <- ecdc %>%
  group_by(geoId) %>%
  arrange(date) %>%
  mutate(casesTot = cumsum(cases))

# find the day that had > min_start_value cases per country
start_dates <- filter(ecdc, casesTot / (popData2018 / capita) > min_start_value) %>%
  group_by(geoId) %>%
  summarise(start_date = min(date))

# get that into an "index" field that can be used for graphing
ecdc <- filter(ecdc, geoId %in% start_dates$geoId) %>%
  inner_join(start_dates,by = c('geoId','geoId')) %>%
  group_by(geoId) %>%
  mutate(casesIndexDate = as.integer(date - start_date))

# data check
head(filter(ecdc, geoId == 'DE')[1,10:13])

for(country in countries) {
  # do something
}

y_ends <- ecdc %>% 
  group_by(geoId) %>% 
  top_n(1, casesTot) %>%
  mutate(casesPerCap = casesTot / (popData2018 / capita)) %>%
  pull(casesPerCap)

labels <- ecdc %>%
  group_by(geoId) %>%
  top_n(1, casesIndexDate)

ggplot(subset(ecdc,casesIndexDate >= 0), 
       aes(x=casesIndexDate,
           y=casesTot/(popData2018 / capita),
           fill=geoId,
           color=geoId)) +
  geom_line(size = 0.1)+
  # slopes are log(2) / doubling days: these are 4,3,2 days
  geom_abline(intercept = log10(min_start_value), slope = (log10(2) / 9), linetype="dashed", color="gray", ) +
  geom_abline(intercept = log10(min_start_value), slope = (log10(2) / 7), linetype="dashed", color="gray") +
  geom_abline(intercept = log10(min_start_value), slope = (log10(2) / 5), linetype="dashed", color="gray") +
  geom_text(data=labels, 
            aes(label = geoId, 
                colour = geoId, 
                x = casesIndexDate, 
                y = casesTot/(popData2018 / capita)),
            hjust = -0.5, 
            vjust = 0.5) +
  ggtitle(paste("Total cases per",capita_word,"people over time"),
          subtitle = paste("Synchronized with day '0' as when the country had",min_start_value, "cases. Data from ECDC data set",latest_data_date,"retrieved",retrieved_date)) +
  scale_y_continuous(paste("Total cases per", capita_word, "people"),
                     trans="log10",
                     sec.axis = sec_axis(~ ., breaks = y_ends),
                     expand = expansion(mult = c(0, 0.1))
                     )+
  scale_x_continuous(paste("Days since",min_start_value,"cases per",capita_word),
                     expand = expansion(mult = c(0, .1)))
  