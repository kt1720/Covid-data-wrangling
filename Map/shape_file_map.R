library(tidyverse)
library(plotly)
library(zoo)
library(lubridate)
library(sf)

# Import JHU data
confirmedraw <- read_csv("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv") %>%
  filter(`Country/Region` == "Canada") %>%
  # & `Province/State` != "Repatriated Travellers"
  # & `Province/State` != "Diamond Princess" & `Province/State` != "Grand Princess") %>%
  rename(Province = `Province/State`) %>%
  select(!`Country/Region`)
deathsraw <- read_csv("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv") %>%
  filter(`Country/Region` == "Canada") %>% 
  # & `Province/State` != "Repatriated Travellers"
  # & `Province/State` != "Diamond Princess" & `Province/State` != "Grand Princess") %>%
  rename(Province = `Province/State`) %>%
  select(!`Country/Region`)

# DATA CLEANING: To create provincial level data in Canada
# Convert each data set from wide to long AND aggregate at provincial level  
confirmed <- confirmedraw %>%
  gather(key="date", value="confirmed", -c(Province, Lat, Long)) %>%
  mutate(date = mdy(date)) 
deaths <- deathsraw %>%
  gather(key="date", value="deaths", -c(Province, Lat, Long)) %>%
  mutate(date = mdy(date)) 

# Final data: combine all two
CA_provinces <- left_join(confirmed, deaths, by = c("Province", "date", "Lat", "Long"))

# Create new variable: daily reported numbers of cases, deaths and recovered cases, 
# 7 days rolling average case numbers, number of days
# Drop observations of Repatriated Travellers and the two cruise ships from the provincial data
CA_provinces <- CA_provinces %>% 
  group_by(Province) %>% 
  mutate(daily.confirmed = c(confirmed[1], diff(confirmed)),
         rolling.confirmed = round(rollmean(daily.confirmed, k = 7, fill = NA, align = "right"), 3),
         daily.deaths = c(deaths[1], diff(deaths)),
         rolling.deaths = round(rollmean(daily.deaths, k = 7, fill = NA, align = "right"), 3),
         days = date - first(date) + 1) %>% 
  filter(Province != "Repatriated Travellers" & Province != "Diamond Princess" &
           Province != "Grand Princess")


# Read in the shape file to draw the Canadian map
province <- st_read("province.shp", quiet = T) %>%
   left_join(CA_provinces, by = "Province") %>%
   filter(days >= max(days) - 7 & days <= max(days)) %>%
   mutate(text = paste("Province:", Province, "\nConfirmed cases:", daily.confirmed),
          text2 = paste("Province:", Province, "\nDeath counts:", daily.deaths)) %>%
   rename(`Total cases` = confirmed, `Total death counts` = deaths)

# First map that shows the total cases
map <- province %>%
  ggplot() +
    geom_sf(aes(fill = `Total cases`, text = paste("Province: ", Province)),
            color = "gray40") +
    geom_sf_text(aes(label = PREABBR), size = 2) +
    scale_fill_gradient("Total COVID cases", labels = scales::number,
                        low = "#fee8c8", high = "#e34a33") +
    theme_void() +
    theme(panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_text(hjust=0.5,color="gray40", size=14),
          plot.subtitle = element_text(color="gray40", size = 10),
          legend.title	= element_text(color="grey35", size=9),
          legend.text	= element_text(color="grey35", size=9)) # +
    # labs(title = "Total Covid cases in Canadian provinces",
    #      subtitle = paste("As of", max(province$date), '%d %B, %Y')))
ggplotly(map, tooptip = "text") %>%
  layout(title = list(text = paste("Total Covid-19 cases in Canadian provinces<br>",
                                   "<sup>As of", max(province$date), '</sup>')))

# Second map that displays cases for the last 7 days
# The Animation map using the shape file has some weird behavior on the first animation run, 
# but subsequent run seems fine.
province %>%
  plot_ly(stroke = I("black"), split = ~Province, color = ~daily.confirmed, colors = viridis::inferno(99),
          text = ~text, showlegend = FALSE, hoveron = "fills", frame = ~date) %>%
  colorbar(title = "Daily confirmed cases") %>%
  style(hoverlabel = list(bgcolor = "white")) %>%
  animation_slider(currentvalue = list(prefix="Date: ", font = list(color = "red"))) %>%
  layout(title = list(text = paste("New Covid-19 cases in Canadian provinces for the past 7 days<br>",
                                   "<sup>As of", max(province$date), '</sup>')))
