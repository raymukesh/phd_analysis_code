library(reshape2)
library(sf)
library(raster)
library(rgdal)
library(ggplot2)
library(RColorBrewer)
library(tidyverse)
library(ggthemes)
library(gtools)
library(ggsn)

getwd()

## Speed up raster process
rasterOptions(tmpdir="D:/Scratch Folder", tmptime = 24, progress="text", timer=TRUE,
            overwrite = T, chunksize=10e8, maxmemory=8e8)


## Load the ndvi raster images
## Use ndvi_stack for daily NDVI and ndvi_stack16 folder for 16 day composite.
col_ndvi <- list.files('ndvi_stack16_terra/', full.names = T, pattern = "\\.tif$")
col_ndvi <- mixedsort(col_ndvi) ## From gtools package this arranges the files in order

length(col_ndvi)

## Stack the collection
col_stack <- stack(col_ndvi)
col_stack2 <- col_stack*0.0001



## Create a monthly mean of the stack
mean_ndvi <- stackApply(col_stack2, 1:12, mean)



## plot the stack
plot(mean_ndvi)


## Rename the files as months
names(mean_ndvi) <- month.name

## clip the files to the site of site NCR
## Load the shapefile (optional in this case as already read in first)
site <- st_read("site/Total_area_43N_1.shp")
district <- readOGR(dsn = "all_districts/.", layer = "all_districts_site_43N",  stringsAsFactors = F)
district <- fortify(district)


## Reproject raster data of the stack
mean_ndvi <- projectRaster(mean_ndvi, crs = crs(site))
crs(mean_ndvi)

## Crop and mask the images
ndvi_crop <- crop(mean_ndvi, site)
ndvi_mask <- mask(ndvi_crop, site)
plot(ndvi_mask)
cellStats(ndvi_mask, 'mean')


### plot the raster stack with GGPLOT
## converting the images to dataframe - ggplot need x and y values
col_df_ndvi <- as.data.frame(ndvi_mask, xy = TRUE) %>% melt(id.vars = c('x', 'y'))
col_df_ndvi <- col_df_ndvi[complete.cases(col_df_ndvi),]


## Plot the data
ggplot() + geom_raster(data=col_df_ndvi, aes(x=x/1e4, y=y/1e5, fill = value)) + 
  geom_polygon(data = district, aes(x = long/1e4, y = lat/1e5, group = group), lwd = 0.6, colour = "black", fill = NA) +
  facet_wrap(~variable, ncol = 4) +
  labs(x = "Longitude", y = "Latitude", caption = "\nBased on MODIS (MOD13Q1.006) Terra | Maps by Author") + 
  ggtitle("Long Term Mean Monthly Variation in NDVI (2001-2020)") +
  scale_fill_gradientn(name = "NDVI", colours = hcl.colors(9, "RdYlGn"), breaks = seq(-1, 1, 0.1)) +
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
        legend.title = element_text(size = 30, face = "bold", angle = 0),
        legend.text = element_text(size = 25, angle = 0, vjust = 0.5),
        legend.direction = "vertical",
        legend.position = "right",
        plot.caption = element_text(size = 26))

ggsave("output/ndvi_mean_month_16_terra.png", height = 18, width = 30, units = 'in', dpi= 200)



## Import districts as polygons
all_districts <- st_read("all_districts/all_districts_site_43N.shp")
st_crs(mean_ndvi) == st_crs(all_districts)

## Mean night temp overs districts
all_districts_mean <- raster::extract(mean_ndvi, all_districts, fun = mean, df = TRUE) 
all_districts_mean <- all_districts %>% st_set_geometry(., NULL) %>% mutate(ID = 1:18) %>% left_join(all_districts_mean, by = "ID") %>% as_tibble()
write.csv(all_districts_mean, file = "mean_ndvi_district_16_terra.csv")


###########################################################################################
## Seasonal NDVI Maps
###########################################################################################
mean_season_ndvi <- stackApply(mean_ndvi, indices = c(1,1,1,2,2,2,3,3,3,4,4,4), fun = mean)


## Crop and mask the images
ndvi_s_crop <- crop(mean_season_ndvi, site)
ndvi_s_mask <- mask(ndvi_s_crop, site)
plot(ndvi_s_mask)

cellStats(ndvi_s_mask, 'mean')


### plot the raster stack with GGPLOT
## converting the images to dataframe - ggplot need x and y values
seas_df_ndvi <- as.data.frame(ndvi_s_mask, xy = TRUE) %>% melt(id.vars = c('x', 'y'))
seas_df_ndvi <- seas_df_ndvi[complete.cases(seas_df_ndvi),]

seas_df_ndvi <- seas_df_ndvi %>% mutate(variable = recode(variable, 'index_1' = "Jan-Feb-Mar",
                                                                    'index_2' = "Apr-May-Jun",
                                                                    'index_3' = "Jul-Aug-Sep",
                                                                    'index_4' = "Oct-Nov-Dec"))


## Plot the data
ggplot() + geom_raster(data=seas_df_ndvi, aes(x=x/1e4, y=y/1e5, fill = value)) + 
  geom_polygon(data = district, aes(x = long/1e4, y = lat/1e5, group = group), lwd = 1, colour = "black", fill = NA) +
  facet_wrap(~variable, ncol = 2) +
  labs(x = "Longitude", y = "Latitude", caption = "\nBased on MODIS (MOD13Q1.006) Terra | Maps by Author") + 
  ggtitle("Long Term Mean Seasonal Variation in NDVI (2001-2020)") +
  scale_fill_gradientn(name = "NDVI", colours = hcl.colors(9, "RdYlGn"), breaks = seq(-1, 1, 0.1)) +
  coord_fixed(10/1) + 
  theme_fivethirtyeight() + 
  theme(panel.spacing.x = unit(1, "lines"),
        strip.text.x = element_text(size = 33,  face = 'bold'),
        axis.text.x = element_text(size = 23, face = 'bold'),
        axis.text.y = element_text(size = 23, face = 'bold'),
        #axis.text.y.left = element_text(colour = "#08589e"),
        axis.title.x =  element_text(margin = margin(t = 20), size = 30, face = 'bold'),
        axis.title.y = element_text(margin = margin(r = 20), size = 30, face = 'bold'),
        plot.title = element_text(size = 40, face = 'bold', color = "black"),
        legend.key.size = unit(0.8, 'in'), 
        legend.title = element_text(size = 30, face = "bold", angle = 0),
        legend.text = element_text(size = 25, angle = 0, vjust = 0.5),
        legend.direction = "vertical",
        legend.position = "right",
        plot.caption = element_text(size = 26))

ggsave("output/ndvi_mean_season_16_terra.png", height = 18, width = 25, units = 'in', dpi= 200)




####################################################################################
### YEARLY MEAN
####################################################################################
## Create a monthly mean of the stack
yearly_ndvi_mean <- stackApply(col_stack2, indices = c(rep(1:20, each = 12)), mean)
names(yearly_ndvi_mean) <- rep(2001:2020)
cellStats(yearly_ndvi_mean, sum)/1000

## Reproject raster data of the stack
yearly_ndvi_mean <- projectRaster(yearly_ndvi_mean, crs = crs(site))

## Crop and mask the images
ndvi_y_crop <- crop(yearly_ndvi_mean, site)
ndvi_y_mask <- mask(ndvi_y_crop, site)


### plot the raster stack with GGPLOT
## converting the images to dataframe - ggplot need x and y values
yearly_df_ndvi <- as.data.frame(ndvi_y_mask, xy = TRUE) %>% melt(id.vars = c('x', 'y'))
yearly_df_ndvi <- yearly_df_ndvi[complete.cases(yearly_df_ndvi),]

df <- yearly_df_ndvi %>% group_by(variable) %>% summarise(mean = mean(value)) %>% as_tibble()

write.csv(df, "mean_yearly_values.csv")

## Plot the data
ggplot() + geom_raster(data=yearly_df_ndvi, aes(x=x/1e4, y=y/1e5, fill = value)) + 
  geom_polygon(data = district, aes(x = long/1e4, y = lat/1e5, group = group), lwd = 0.6, colour = "black", fill = NA) +
  facet_wrap(~variable, ncol = 4) +
  labs(x = "Longitude", y = "Latitude", caption = "\nBased on MODIS (MOD13Q1.006) Terra | Maps by Author") + 
  ggtitle("Long Term Mean Monthly Variation in NDVI (2001-2020)") +
  scale_fill_gradientn(name = "NDVI", colours = hcl.colors(9, "RdYlGn"), breaks = seq(-1, 1, 0.1)) +
  coord_fixed(10/1) +
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
        legend.title = element_text(size = 30, face = "bold", angle = 0),
        legend.text = element_text(size = 25, angle = 0, vjust = 0.5),
        legend.direction = "vertical",
        legend.position = "right",
        plot.caption = element_text(size = 26))

ggsave("output/ndvi_mean_yearly_16_terra.png", height = 30, width = 20, units = 'in', dpi= 150)


## Highest Change in NDVI
year_2001_ndvi <- subset(ndvi_y_mask, 1)
year_2020_ndvi <- subset(ndvi_y_mask, 20)

diff_ndvi <- year_2020_ndvi - year_2001_ndvi

yearly_diff_ndvi <- as.data.frame(diff_ndvi, xy = TRUE) %>% melt(id.vars = c('x', 'y'))
yearly_diff_ndvi <- yearly_diff_ndvi[complete.cases(yearly_diff_ndvi),]

yearly_diff_ndvi %>% filter(value <=0) %>% 
ggplot() + 
  geom_raster(aes(x=x/1e4, y=y/1e5, fill = value)) + 
  geom_polygon(data = district, aes(x = long/1e4, y = lat/1e5, group = group), lwd = 0.6, colour = "black", fill = NA) +
  labs(x = "Longitude", y = "Latitude", caption = "\nBased on MODIS (MOD13Q1.006) Terra | Maps by Author") + 
  ggtitle("NDVI Loss Between 2001-2020") +
  scale_fill_gradientn(name = "NDVI", colours = "red", breaks = seq(-0.5, 0.5, 0.1)) +
  coord_fixed(10/1) +
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
        legend.title = element_text(size = 20, face = "bold", angle = 0),
        legend.text = element_text(size = 20, angle = 0, vjust = 0.5),
        legend.direction = "vertical",
        legend.position = "none",
        plot.caption = element_text(size = 18, hjust = 0.5))

ggsave("output/ndvi_loss_2001_2020_high.png", height = 10, width = 14, units = 'in', dpi= 200)


