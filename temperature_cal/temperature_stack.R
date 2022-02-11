## Load the libraries
install.packages('sf')
install.packages("raster")
install.packages("rgdal")
install.packages("reshape2")

library(reshape2)
library(sf)
library(raster)
library(rgdal)
library(ggplot2)
library(RColorBrewer)
library(tidyverse)
library(ggthemes)
library(gtools)
#library(stars)

getwd()


col_day <- list.files('temperature_day', full.names = T, pattern = "\\.tif$")

## Stack the collection
col_day1 <- stack(col_day)
col_day2 <- col_day1*0.1

## Create a monthly mean of the stack
temp_day <- stackApply(col_day2, 1:12, mean)

#temp_march <- stackApply(col_stack2, 3:3, mean)

## plot the stack
plot(temp_day)


## Rename the files as months
names(temp_day) <- month.name

## clip the files to the site of Delhi NCR
## Load the shapefile
delhi <- st_read("site/Total_area_43N_1.shp")
district <- readOGR(dsn = "all_districts/.", layer = "all_districts_site_43N",  stringsAsFactors = F)
district <- fortify(district)


## Reproject raster data of the stack
temp_day <- projectRaster(temp_day, crs = crs(delhi))
crs(temp_day)

## Crop and mask the images
temp_night_crop <- crop(temp_day, delhi)
temp_night_mask <- mask(temp_night_crop, delhi)
plot(temp_night_mask)


### plot the raster stack with GGPLOT

## converting the images to dataframe - ggplot need x and y values

col_df <- as.data.frame(temp_night_mask, xy = TRUE) %>% melt(id.vars = c('x', 'y'))
col_df_na <- col_df[complete.cases(col_df),]


## Plot the data
ggplot() + geom_raster(data=col_df_na, aes(x=x/1e4, y=y/1e5, fill = value)) + 
  geom_polygon(data = district, aes(x = long/1e4, y = lat/1e5, group = group), lwd = 0.6, colour = "black", fill = NA) +
  facet_wrap(~variable, ncol = 4) +
  labs(x = "Longitude", y = "Latitude", caption = "Based on MODIS Data  |  Maps by Author") + 
  ggtitle("Long Term Mean Monthly Variation in Day Temperature (2003-2020)") +
  scale_fill_gradientn(name = "Temperature\n(°C)", colours = rev(hcl.colors(9, "RdYlBu")), breaks = seq(0, 50, 4)) +
  theme_fivethirtyeight() + 
  theme(panel.spacing.x = unit(1, "lines"),
        strip.text.x = element_text(size = 30,  face = 'bold'),
        axis.text.x = element_text(size = 23, face = 'bold'),
        axis.text.y = element_text(size = 23, face = 'bold'),
        #axis.text.y.left = element_text(colour = "#08589e"),
        axis.title.x =  element_text(margin = margin(t = 20), size = 30, face = 'bold'),
        axis.title.y = element_text(margin = margin(r = 20), size = 30, face = 'bold'),
        plot.title = element_text(size = 40, face = 'bold', color = "black"),
        legend.key.size = unit(0.8, 'in'), 
        legend.title = element_text(size = 30, face = "bold", angle = 90),
        legend.text = element_text(size = 25, angle = 0, vjust = 0.5),
        legend.direction = "vertical",
        legend.position = "right",
        plot.caption = element_text(size = 26))

ggsave("output/temperature_mean_month_day.png", height = 18, width = 30, units = 'in', dpi= 200)



##=====================================================================================================
## Mean Night Time Temperature
##=====================================================================================================

## Load the night raster images
col_night <- list.files('temperature_night', full.names = T, pattern = "\\.tif$")
col_night <- mixedsort(col_night) ## From gtools package this arranges the files in order

## Stack the collection
col_night1 <- stack(col_night)
col_night2 <- col_night1*0.1

## Create a monthly mean of the stack
temp_night <- stackApply(col_night2, 1:12, mean)

#temp_march <- stackApply(col_stack2, 3:3, mean)

## plot the stack
plot(temp_night)


## Rename the files as months
names(temp_night) <- month.name

## clip the files to the site of Delhi NCR
## Load the shapefile (optional in this case as already read in first)
# delhi <- st_read("site/Total_area_43N_1.shp")
# district <- readOGR(dsn = "all_districts/.", layer = "all_districts_site_43N",  stringsAsFactors = F)
# district <- fortify(district)


## Reproject raster data of the stack
temp_night <- projectRaster(temp_night, crs = crs(delhi))
crs(temp_night)

## Crop and mask the images
temp_crop <- crop(temp_night, delhi)
temp_mask <- mask(temp_crop, delhi)
plot(temp_mask)


### plot the raster stack with GGPLOT

## converting the images to dataframe - ggplot need x and y values

col_df_night <- as.data.frame(temp_mask, xy = TRUE) %>% melt(id.vars = c('x', 'y'))
col_df_night <- col_df_night[complete.cases(col_df_night),]


## Plot the data
ggplot() + geom_raster(data=col_df_night, aes(x=x/1e4, y=y/1e5, fill = value)) + 
  geom_polygon(data = district, aes(x = long/1e4, y = lat/1e5, group = group), lwd = 0.6, colour = "black", fill = NA) +
  facet_wrap(~variable, ncol = 4) +
  labs(x = "Longitude", y = "Latitude", caption = "Based on MODIS Data  |  Maps by Author") + 
  ggtitle("Long Term Mean Monthly Variation in Night Temperature (2003-2020)") +
  scale_fill_gradientn(name = "Temperature\n(°C)", colours = rev(hcl.colors(9, "RdYlBu")), breaks = seq(0, 30, 2)) +
  theme_fivethirtyeight() + 
  theme(panel.spacing.x = unit(1, "lines"),
        strip.text.x = element_text(size = 30,  face = 'bold'),
        axis.text.x = element_text(size = 23, face = 'bold'),
        axis.text.y = element_text(size = 23, face = 'bold'),
        #axis.text.y.left = element_text(colour = "#08589e"),
        axis.title.x =  element_text(margin = margin(t = 20), size = 30, face = 'bold'),
        axis.title.y = element_text(margin = margin(r = 20), size = 30, face = 'bold'),
        plot.title = element_text(size = 40, face = 'bold', color = "black"),
        legend.key.size = unit(0.8, 'in'), 
        legend.title = element_text(size = 30, face = "bold", angle = 90),
        legend.text = element_text(size = 25, angle = 0, vjust = 0.5),
        legend.direction = "vertical",
        legend.position = "right",
        plot.caption = element_text(size = 26))

ggsave("output/temperature_mean_month_night.png", height = 18, width = 30, units = 'in', dpi= 200)






