# load data from external sources

# get ECDC data from their website for specified countries
EcdcData <- function(countries) {
  library(readxl)
  library(httr)
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
