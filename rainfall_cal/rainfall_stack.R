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

col <- list.files('collection2', full.names = T, pattern = ".*tif")

## Stack the collection
col_stack <- stack(col)

## Create a monthly mean of the stack
rain_month <- stackApply(col_stack, 1:12, mean)

## plot the stack
plot(rain_month)


## Rename the files as months
names(rain_month) <- month.name

## clip the files to the site of Delhi NCR
## Load the shapefile
delhi <- st_read("site/Total_area_43N_1.shp")
district <- readOGR(dsn = "all_districts/.", layer = "all_districts_site_43N",  stringsAsFactors = F)
district <- fortify(district)

## Reproject raster data of the stack
rain_month <- projectRaster(rain_month, crs = crs(delhi))
crs(rain_month)

## Crop and mask the images
rain_crop <- crop(rain_month, delhi)
rain_mask <- mask(rain_crop, delhi)
plot(rain_mask)


### plot the raster stack with GGPLOT

## converting the images to dataframe - ggplot need x and y values

col_df <- as.data.frame(rain_mask, xy = TRUE) %>% melt(id.vars = c('x', 'y'))
col_df_na <- col_df[complete.cases(col_df),]


## Plot the data
ggplot() + geom_raster(data=col_df_na, aes(x=x/1e4, y=y/1e5, fill = value)) +
  geom_polygon(data = district, aes(x = long/1e4, y = lat/1e5, group = group), lwd = 0.7, colour = "black", fill = NA) +
  coord_fixed(ratio = 10) +
  facet_wrap(~variable, ncol = 4) +
  labs(x = "Longitude", y = "Latitude", caption = "Based on CHIRPS Dataset  |  Maps by Author") + 
  ggtitle("Mean Monthly Rainfall (2001 - 2020)") +
  scale_fill_gradientn(name = "Rainfall\n(mm)", colours = brewer.pal(9, 'RdYlBu'), breaks = seq(0, 500, 50)) +
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
        legend.title = element_text(size = 30, face = "bold"),
        legend.text = element_text(size = 25, angle = 0, vjust = 0.5),
        legend.direction = "vertical",
        legend.position = "right",
        plot.caption = element_text(size = 26))

ggsave("output/rainfal_mean_month_v2.png", height = 18, width = 30, units = 'in', dpi= 200)

