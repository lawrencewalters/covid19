# Deaths per day histogram ECDC data
number_of_days <- 180
regression_days <- 30
countries <- c('DE','BER')
#countries <- c('DE','US','CA','MX','IT','ES','UK')
#countries <- c('UK')
Sys.setenv(TZ="Europe/Berlin")

library(readxl)
library(httr)
library(reshape2)
library(ggplot2)
library(dplyr)
library(broom)
library(scales)
library(ggpmisc)

# get ECDC data from their website for specified countries
EcdcData <- function(countries) {
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
  full_ecdc <- read_excel(tf)
  return(list("data" = filter(full_ecdc, geoId %in% countries), "retrieved_date" = retrieved_date, "latest_data_date" = latest_data_date))
}


# get berlin data from URL or local file CSV
BerlinData <- function(filename) {
  df<-read.csv(filename, header = TRUE)
  # calculate numbers per day - file has cumulative numbers per day
  df <- mutate(df, cases = cases - lag(cases))
  df <- mutate(df, deaths = deaths - lag(deaths))
  
  # get date as actual date object, and weekend calcs - these fields match ECDC fields
  df <- mutate(df, 
               dateRep = as.Date(date, '%Y-%m-%d'), 
               day = as.double(format.Date(dateRep, "%e")),
               month = as.double(format.Date(dateRep, "%m")),
               year = as.double(format.Date(dateRep, "%Y")),
               countriesAndTerritories = 'Berlin',
               geoId = "BER",
               countryterritoryCode = "BER",
               popData2019 = 3769495,
               continentExp = 'Europe',
               "Cumulative_number_for_14_days_of_COVID-19_cases_per_100000" = 1.0
               )
  return(subset(df, select = c(dateRep, day, month, year, cases, deaths, countriesAndTerritories, geoId, countryterritoryCode, popData2019,continentExp,get("Cumulative_number_for_14_days_of_COVID-19_cases_per_100000"))))
}

ecdc_list <- EcdcData(countries)
ecdc <- ecdc_list$data
latest_data_date <- ecdc_list$latest_data_date
retrieved_date <- ecdc_list$retrieved_date

#berlin <- BerlinData("data\\daily.csv")
berlin <- BerlinData("https://raw.githubusercontent.com/jakubvalenta/covid-berlin-data/master/covid_berlin_data_incl_hospitalized.csv")

colnames(berlin)

ecdc <- rbind(berlin, ecdc)
ecdc <- filter(ecdc, geoId %in% countries)

# deaths per day graphs
ecdc <- mutate(ecdc, 
               date = as.Date(dateRep, '%Y-%m-%d',tz='Europe/Berlin'),
               wday = as.POSIXlt(date, tz='Europe/Berlin')$wday,
               wkday = ifelse(wday == 0 | wday == 6,0.9,1))

ecdc <- filter(ecdc, date > latest_data_date - number_of_days)
ecdc <- filter(ecdc, date > latest_data_date - number_of_days)
dflong <- melt(ecdc, id.vars = c("dateRep","day","month","year","countriesAndTerritories","geoId","countryterritoryCode","popData2019","date","wday","wkday"),
               measure.vars = c("cases","deaths"))

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

facet_labels <- as_labeller(c("cases" = "Cases","deaths" = "Deaths",
                              "US" = "USA","BER" = "Berlin","DE" = "Germany",
                              "IT" = "Italy", "AT" = "Austria", "ES" = "Spain",
                              "CA" = "Canada","FR" = "France","DK" = "Denmark", "UK" = "UK",
                              "MX" = "Mexico"), default = label_value, multi_line = FALSE)

ggplot(subset(dflong, 
              (date > latest_data_date - number_of_days) & 
                (geoId %in% countries)),
       aes(x = date,
           y = value,
           fill = variable, 
           alpha=wkday
           )) +
  scale_x_date(date_labels = "%m.%d",
               breaks = "14 days",
               minor_breaks = "7 day",
               name = "Date. Weekends shaded lighter"
  ) +
  theme(axis.text.x = element_text(angle = -90, vjust = 0.3),
        legend.position = "none") +
  geom_col(position="dodge2") +
  geom_smooth(method='lm',formula=y ~ x,
              data = subset(dflong, 
                            (date > latest_data_date - regression_days))) +
  geom_text_npc(data=labels,aes(npcx = 1, npcy = 1, label = labelText))+
  scale_y_continuous(NULL, label = comma) +
  facet_wrap(vars(variable, geoId),
             scales = "free_y",
             ncol=length(countries),
             labeller = facet_labels) +
  scale_fill_brewer(palette = "Dark2") +
  scale_alpha(range = c(0.4, 1)) +
  labs(title = paste("Trends by country in cases and deaths per day over time as of"
                ,latest_data_date),
          subtitle = paste("p value from linear regression on most recent",
                           regression_days,"days."),
          caption= paste("Source: Berlin - berlin.de, all others ECDC data",
                           latest_data_date,
                           "retrieved",
                           format.Date(retrieved_date, "%Y-%m-%d %H:%M:%S %Z")))

