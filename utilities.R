# utility functions like labels

# take a 2 letter geo id and provide a labeller that will show a short name for that GeoID (US -> USA, or DE -> Germany)
LabellerForGeoIDs <- function() {
  return(as_labeller(c("cases" = "Cases","deaths" = "Deaths",
                              "US" = "USA","DE-BER" = "Berlin","DE" = "Germany",
                              "IT" = "Italy", "AT" = "Austria", "ES" = "Spain",
                              "CA" = "Canada","FR" = "France","DK" = "Denmark", "UK" = "UK",
                              "MX" = "Mexico", "US-SKO" = "Skokie"), default = label_value, multi_line = FALSE))
}
