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



## Load the evap raster images
## Use evap_stack for daily evap and evap_stack16 folder for 16 day composite.
col_evap <- list.files('evapotrans_terra8_sum/', full.names = T, pattern = "\\.tif$")
col_evap <- mixedsort(col_evap) ## From gtools package this arranges the files in order

length(col_evap)

## Stack the collection
col_stack <- stack(col_evap)
col_stack2 <- col_stack*0.1



## Create a monthly mean of the stack
mean_evap <- stackApply(col_stack2, 1:12, mean)


#temp_march <- stackApply(col_stack2, 3:3, mean)

## plot the stack
plot(mean_evap)


## Rename the files as months
names(mean_evap) <- month.name

## clip the files to the site of site NCR
## Load the shapefile (optional in this case as already read in first)
site <- st_read("site/Total_area_43N_1.shp")
district <- readOGR(dsn = "all_districts/.", layer = "all_districts_site_43N",  stringsAsFactors = F)
district <- fortify(district)


## Reproject raster data of the stack
mean_evap <- projectRaster(mean_evap, crs = crs(site))
crs(mean_evap)

## Crop and mask the images
evap_crop <- crop(mean_evap, site)
evap_mask <- mask(evap_crop, site)
plot(evap_mask)
cellStats(evap_mask, 'max')
cellStats(evap_mask, 'min')

### plot the raster stack with GGPLOT
## converting the images to dataframe - ggplot need x and y values
col_df_evap <- as.data.frame(evap_mask, xy = TRUE) %>% melt(id.vars = c('x', 'y'))
col_df_evap <- col_df_evap[complete.cases(col_df_evap),]

## Plot the data
ggplot() + geom_raster(data=col_df_evap, aes(x=x/1e4, y=y/1e5, fill = value)) + 
  geom_polygon(data = district, aes(x = long/1e4, y = lat/1e5, group = group), lwd = 0.6, colour = "black", fill = NA) +
  facet_wrap(~variable, ncol = 4) +
  labs(x = "Longitude", y = "Latitude", caption = "\nBased on MODIS (MOD16A2.006) Terra | Maps by Author") + 
  ggtitle("Long Term Mean Monthly Variation in Evapotranspiration (2001-2020)") +
  scale_fill_gradientn(name = "Evapotranspiration\n(mm)", colours = hcl.colors(9, "RdYlBu"), breaks = seq(0, 110, 10)) +
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

ggsave("output/evap_mean_month_8_sum.png", height = 18, width = 30, units = 'in', dpi= 200)



## Import districts as polygons
all_districts <- st_read("all_districts/all_districts_site_43N.shp")
st_crs(mean_evap) == st_crs(all_districts)

## Mean night temp overs districts
all_districts_mean <- raster::extract(mean_evap, all_districts, fun = mean, df = TRUE) 
all_districts_mean <- all_districts %>% st_set_geometry(., NULL) %>% mutate(ID = 1:18) %>% left_join(all_districts_mean, by = "ID") %>% as_tibble()
write.csv(all_districts_mean, file = "mean_evap_district_8_sum.csv")


###########################################################################################
## Seasonal Evapotranspiration Maps
###########################################################################################
mean_season_evap <- stackApply(mean_evap, indices = c(1,1,1,2,2,2,3,3,3,4,4,4), fun = mean)


## Crop and mask the images
evap_s_crop <- crop(mean_season_evap, site)
evap_s_mask <- mask(evap_s_crop, site)
plot(evap_s_mask)

cellStats(evap_s_mask, 'mean')


### plot the raster stack with GGPLOT
## converting the images to dataframe - ggplot need x and y values
seas_df_evap <- as.data.frame(evap_s_mask, xy = TRUE) %>% melt(id.vars = c('x', 'y'))
seas_df_evap <- seas_df_evap[complete.cases(seas_df_evap),]

seas_df_evap <- seas_df_evap %>% mutate(variable = recode(variable, 'index_1' = "Jan-Feb-Mar",
                                                          'index_2' = "Apr-May-Jun",
                                                          'index_3' = "Jul-Aug-Sep",
                                                          'index_4' = "Oct-Nov-Dec"))


## Plot the data
ggplot() + geom_raster(data=seas_df_evap, aes(x=x/1e4, y=y/1e5, fill = value)) + 
  geom_polygon(data = district, aes(x = long/1e4, y = lat/1e5, group = group), lwd = 1, colour = "black", fill = NA) +
  facet_wrap(~variable, ncol = 2) +
  labs(x = "Longitude", y = "Latitude", caption = "\nBased on MODIS (MOD16A2.006) Terra | Maps by Author") + 
  ggtitle("Long Term Mean Seasonal Variation in Evapotranspiration (2001-2020)") +
  scale_fill_gradientn(name = "Evapotranspiration\n(mm)", colours = hcl.colors(9, "RdYlGn"), breaks = seq(0, 1110, 10)) +
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
        legend.title = element_text(size = 30, face = "bold", angle = 90),
        legend.text = element_text(size = 25, angle = 0, vjust = 0.5),
        legend.direction = "vertical",
        legend.position = "right",
        plot.caption = element_text(size = 26))

ggsave("output/evap_mean_season_8_sum.png", height = 18, width = 25, units = 'in', dpi= 200)



### YEARLY MEAN

## Create a monthly mean of the stack
yearly_evap_mean <- stackApply(col_stack2, indices = c(rep(1:20, each = 12)), mean)

names(yearly_evap_mean) <- rep(2001:2020)

yearly_evap_mean <- projectRaster(yearly_evap_mean, crs = crs(site))


year_et_crop <- crop(yearly_evap_mean, site)
year_et_mask <- mask(year_et_crop, site)



cellStats(year_et_mask, mean)
plot(cellStats(year_et_mask, "mean"))

df_eva_y <- as.data.frame(year_et_mask, xy=TRUE) %>% melt(id.vars = c('x', 'y'))
df_eva_y <- df_eva_y[complete.cases(df_eva_y),]

df_eva_y %>% group_by(variable) %>% summarise(mean = mean(value)) %>% 
  ggplot(aes(x=variable, y=mean, group = 1)) + geom_line()
