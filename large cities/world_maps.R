library(rgdal)
library(raster)
library(ggplot2)
library(rgeos)
library(mapview)
library(leaflet)
library(broom) # if you plot with ggplot and need to turn sp data into dataframes
library(GADMTools) ## World Administrative Layers
library(sp)
library(tmap)
library(tmaptools)
library(readxl)
library(dplyr)
library(ggthemes)
options(stringsAsFactors = FALSE)


## largest_cities
largest_cities <- read_xlsx("largest_cities.xlsx")
largest_cities <- largest_cities %>% filter(year %in% c("1950", "1970", "1990", "2000", "2020", "2035"))



# download the shapefile data 
download.file("https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_admin_0_countries.zip", 
              destfile = 'countries.zip')

# unzip the file
unzip(zipfile = "countries.zip", 
      exdir = 'ne-countries-10m')

# load the data 
countries <- readOGR("ne-countries-10m/ne_10m_admin_0_countries.shp")
st_crs(countries)

countries <- fortify(countries)
large_cities_gg <- fortify(largest_cities)


ggplot() +
  geom_polygon(data = countries, aes(x = long, y = lat, group = group), fill = "#adb5bd") + 
  geom_point(data = large_cities_gg, aes(x = long, y = lat, size = population), shape = 16, alpha = 0.4, color = 'red') + 
  labs(title = "World's Top 30 Largest Cities (1950-2035)", subtitle = "(in millions)", caption = "Data: World Urbanization Prospects-2018 Revision") +
  scale_size_continuous(range = c(0.5, 4)) + 
  coord_fixed() +
  facet_wrap(~year, ncol = 2) + theme_fivethirtyeight() +
  theme(strip.text.x = element_text(size = 13, face = 'bold'),
        axis.text.x = element_text(size = 8, face = 'bold'),
        axis.text.y = element_text(size = 8, face = 'bold'),
        axis.title.x = element_text(size = 12, margin = margin(t = 10), face = 'bold'),
        axis.title.y = element_text(size = 12, margin = margin(r = 10), face = 'bold'),
        plot.title = element_text(size = 20, face = 'bold'),
        plot.caption.position = "plot", plot.caption = element_text(hjust = 0.5),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 10, face = 'bold'),
        legend.position = 'bottom')


ggsave("largest_cities.png", height = 9, width = 8, dpi = 300)







