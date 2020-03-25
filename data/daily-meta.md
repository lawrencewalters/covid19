# Daily.csv Metadata


Originally, I used RKI data (see below). More often now, I'm using zeit.de data. https://www.zeit.de/wissen/gesundheit/2020-03/coronavirus-deutschland-infektionen-faelle-verbreitung-epidemie-karte

JSON format for reference: 
https://interactive.zeit.de/cronjobs/2020/corona/kreise-chronology.json?time=1585129716
lookup the ags codes: https://interactive.zeit.de/2020/starterkit/static/kreise-data.topo.64ee3a8c9d3db9e582427f9600f3a1e6.cached.json
  (berlin is 11000)

They are also the most open about their source methodology - apparently contacting the german federal states. Their berlin numbers pretty much match the press releases from the Berlin Senat that you can find on berlin.de.

For non german numbers, I typically use John's Hopkins numbers in their github, which seem to track WHO, https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series

previous sources: Robert Koch Institute in Germany, generally speaking. Berlin and German cases are taken from their site at https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/nCoV.html

Counts - generally "official" at 3pm daily, actually posted online later in the day - https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Fallzahlen.html

Situation reports for historical data - https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Situationsberichte/Gesamt.html?nn=13490888

