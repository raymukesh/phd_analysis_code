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
library(ggspatial)
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
district_2 <- readOGR(dsn = "all_districts/.", layer = "all_districts_site_43N",  stringsAsFactors = F)
district <- fortify(district)

plot(district)

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

###############################################################################
## Potential Future Paper - Heat Stress Distribution in Delhi NCR



## Import districts as polygons
all_districts <- st_read("all_districts/all_districts_site_43N.shp")
st_crs(temp_june_night) == st_crs(all_districts)

## Mean night temp overs districts
all_districts_mean <- raster::extract(temp_night, all_districts, fun = mean, df = TRUE) 
all_districts_mean <- all_districts %>% st_set_geometry(., NULL) %>% mutate(ID = 1:18) %>% left_join(all_districts_mean, by = "ID") %>% as_tibble()
write.csv(all_districts_mean, file = "mean_night_temp.csv")


## Mean day temp overs districts
all_districts_mean_day <- raster::extract(temp_day, all_districts, fun = mean, df = TRUE) 
all_districts_mean_day <- all_districts %>% st_set_geometry(., NULL) %>% mutate(ID = 1:18) %>% left_join(all_districts_mean_day, by = "ID") %>% as_tibble()

write.csv(all_districts_mean_day, file = "mean_day_temp.csv")



####################################################################################
### YEARLY MEAN
####################################################################################
## Create a monthly mean of the stack
yearly_day_mean <- stackApply(col_day2, indices = c(rep(1:18, each = 12)), mean)
names(yearly_day_mean) <- rep(2003:2020)

## Reproject raster data of the stack
yearly_day_mean <- projectRaster(yearly_day_mean, crs = crs(delhi))

## Crop and mask the images
day_y_crop <- crop(yearly_day_mean, delhi)
day_y_mask <- mask(day_y_crop, delhi)


### plot the raster stack with GGPLOT
## converting the images to dataframe - ggplot need x and y values
yearly_df_day <- as.data.frame(day_y_mask, xy = TRUE) %>% melt(id.vars = c('x', 'y'))
yearly_df_day <- yearly_df_day[complete.cases(yearly_df_day),]


df <- yearly_df_day %>% group_by(variable) %>% summarise(mean = mean(value)) %>% as_tibble()


## Highest Change in Temp
year_2003_day <- subset(day_y_mask, 1)
year_2020_day <- subset(day_y_mask, 18)

diff_day <- year_2020_day - year_2003_day

## Convert to dataframe and remove NA
yearly_diff_day <- as.data.frame(diff_day, xy = TRUE) %>% melt(id.vars = c('x', 'y'))
yearly_diff_day <- yearly_diff_day[complete.cases(yearly_diff_day),]


yearly_diff_day %>% filter(value > 0) %>% mutate(cat = (ifelse(value <= 1, 1,
                                                          ifelse(value <= 2, 2,
                                                            ifelse(value <= 3, 3,
                                                              ifelse(value <= 4, 4)))))) %>% 
  ggplot() + 
  geom_raster(aes(x=x/1e4, y=y/1e5, fill = factor(cat))) + 
  geom_polygon(data = district, aes(x = long/1e4, y = lat/1e5, group = group), lwd = 0.6, colour = "black", fill = NA) +
  labs(x = "Longitude", y = "Latitude", caption = "\nBased on MODIS (MOD11A2.006) Terra | Maps by Author") + 
  ggtitle("Areas Experiencing Temperature Gain (2003-2020)") +
  scale_fill_manual(name = "Temperature\nRise (°C)", values= c('#fee5d9','#fcae91','#fb6a4a','#cb181d')) +
  coord_fixed(10/1) + annotation_north_arrow(which_north = "grid") +
    theme_fivethirtyeight() + 
  theme(panel.spacing.x = unit(1, "lines"),
        strip.text.x = element_text(size = 30,  face = 'bold'),
        axis.text.x = element_text(size = 15, face = 'bold'),
        axis.text.y = element_text(size = 15, face = 'bold'),
        #axis.text.y.left = element_text(colour = "#08589e"),
        axis.title.x =  element_text(margin = margin(t = 20), size = 20, face = 'bold'),
        axis.title.y = element_text(margin = margin(r = 20), size = 20, face = 'bold'),
        plot.title = element_text(size = 30, face = 'bold', color = "black"),
        legend.key.size = unit(0.5, 'in'), 
        legend.title = element_text(size = 18, face = "bold", angle = 90),
        legend.text = element_text(size = 18, angle = 0, vjust = 0.5),
        legend.direction = "vertical",
        legend.position = "right",
        plot.caption = element_text(size = 18, hjust = 0.5))

ggsave("output/tem_gain_2003_2020.png", height = 10, width = 14, units = 'in', dpi= 200)


## Mean night temp overs districts
all_diff_mean <- raster::extract(diff_day, all_districts, fun = mean, df = TRUE) 
all_diff_mean <- all_districts %>% st_set_geometry(., NULL) %>% mutate(ID = 1:18) %>% left_join(all_diff_mean, by = "ID") %>% as_tibble()
write.csv(all_diff_mean, file = "mean_diff_district_day.csv")

anno