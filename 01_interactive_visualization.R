# Interactive Visualization

#LEARNING OBJECTIVES
  #How to use interactive visualization in R
  #Workflows to go from data to visualization
  #Understand a visualization objective

library(tidygeocoder) # geocode our addresses
library(tidyverse)    # wrangle data
library(janitor)      # clean column names
library(glue)         # modern paste() function
library(sf)           # make spatial data
library(mapview)      # interactive maps!
library(janitor)
mapviewOptions(fgb = FALSE)


# Using the url for the Form data 
form_data <- paste0("https://docs.google.com/spreadsheets/d/e/",
                    "2PACX-1vSODxBm_z5Gu8a42C6ZFEa3S5iTbYV-",
                    "qucCGvasGS6c0qFUAml5vSMEgbvI9PYo1HJ20Y_WY62aTAb-",
                    "/pub?gid=1462593645&single=true&output=csv")

# read in url and clean
dat <- read_csv(form_data) %>% 
  clean_names() %>% 
  rename( dining_name = 3, dining_address = 4)

# geocode using Open Street Map (osm) API because it's free
dat_geo <- dat %>%
  geocode(dining_address, method = 'osm', lat = latitude , long = longitude)

# make into sf object so we can map
dat_geo <- dat_geo %>% 
  filter(!is.na(latitude) & !is.na(longitude)) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

# map
mapview(
  dat_geo, 
  zcol = "comfort_using_r", 
  layer.name = "R comfort level", 
  cex = 6.5
)

# Use leaflet
library(leafpm)
library(leaflet)
library(leaflet.extras)
library(htmltools)

# set up our map
m <- leaflet() %>%
  # add tiles or the "basemaps"
  addTiles(group = "OSM") %>% # defaults to Open Street Maps
  addProviderTiles(providers$CartoDB.Positron, group = "Positron") %>% 
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
  addCircleMarkers(
    lng = -121.4944, lat = 38.5816, fillColor = "red", color = "black",
    popup = "Sacramento!", group = "Home",
  ) %>% 
  addCircleMarkers(
    data = dat_geo, group = "Food & Drink",
    label = ~htmlEscape(first_name),
    popup = glue(
      "<b>Name:</b> {dat_geo$first_name}<br>
      <b>Food_Name:</b> {dat_geo$dining_name}<br>
      <b>Food_Address:</b> {dat_geo$dining_address}<br>
      <b>R comfort (1-10):</b> {dat_geo$comfort_using_r}"
    )
  )  %>% 
  addLayersControl(
    baseGroups = c("Toner Lite", "Positron", "OSM"),
    overlayGroups = c("Home", "Food & Drink"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  addMeasure()

# Print the map
m


# A Network with D3
library(igraph)
library(networkD3)

# create a dataset:
data <- tibble(
  from = c(
    "Dam","Dam","Dam", "Dam",
    "River","River","River", "River","River",
    "Canal", "Canal", 
    "Diversion","Diversion", 
    "Reservoir", "Reservoir","Reservoir",
    "Lake","Lake","Lake", "Lake", 
    "Road","Road","Road",
    "Culvert", "Culvert",
    "Fish", "Fish","Fish",
    "Frog","Frog","Frog",
    "MacroInvertebrates","MacroInvertebrates"
  ),
  to = c(
    "River","Reservoir","Canal","Diversion",
    "Lake","Reservoir","Frog","Fish","MacroInvertebrates",
    "Diversion", "Reservoir",
    "Dam", "River",
    "River","Dam","Fish",
    "Fish","Dam","Frog","MacroInvertebrates",
    "Fish","Dam", "Canal",
    "Road", "Dam",
    "Frog", "River","MacroInvertebrates",
    "Fish", "River", "Lake",
    "River", "Lake"
  )
)

# Plot
p <- simpleNetwork(data, height = "600px", width = "600px", 
                   fontSize = 12, fontFamily = "sans-serif",
                   nodeColour = "darkblue", linkColour = "steelblue",
                   opacity = 0.8, zoom = FALSE, charge = -500)
p


# Plotly
library(plotly)
library(cowplot)

# load CES data for Sacramento county
ces3_sac <- readRDS("C:/Users/mepra/OneDrive/Documents/R/R4WRDS Intermediate Course/data/ces3_sac.rds")
mapview(ces3_sac, zcol = "CIscoreP")

# plot of Groundwater threats vs. CES score
ces_plot <- ces3_sac %>% 
  ggplot(aes(x = gwthreatsP, y = CIscoreP, label = tract)) + 
  geom_point() +
  geom_smooth(method = "gam") +
  cowplot::theme_half_open(font_size = 12) +
  labs(
    title = "CES Score vs. Groundwater Threats in Sacramento County",
    subtitle = "Higher CI Score indicates higher threat/impact",
    x = "Groundwater Threats (percentile)", 
    y = "CI Score (percentile)"
  )
ces_plot

#ggplot
ggplotly(ces_plot)
