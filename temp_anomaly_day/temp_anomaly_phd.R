#######################################################################################
#################### THIS CODE CALCULATES THE DAY TEMPERATURE ANOMALY #################


# LOADING THE PACKAGES
library(terra)
library(RCurl)
library(glue)
library(tidyverse)
library(sf)
library(showtext)
library(raster)
library(reshape2)
library(ggthemes)
library(mapview)
library(units)  ## For changing the units of area of polygons from sqm to sqkm
showtext_auto()



## Listing the files
raw_raster <- list.files(path = "temperature_day", pattern = "*.tif", full.names = TRUE)

## Stacking the raster files
raw_raster <- raster::stack(raw_raster)

## Calculating the yearly mean from the raster images (output is 18 raster files)
raw_raster2 <- stackApply(raw_raster, indices = c(rep(1:18, each=12)), fun = 'mean', na.rm = TRUE)

years <- 2003L:2020L
names(raw_raster2) <- years

## Loading the shapefile
site <- st_read("all_districts/all_districts_site_43N.shp")

site %>% mutate(area2 = st_area(site))
site$area <- set_units(site$area, "km^2")

## Project the raster to the site crs
raw_raster2 <- projectRaster(raw_raster2, crs = crs(site))

## crop and mask the raster to the site
raw_crop <- raster::crop(raw_raster2, site)
raw_mask <- raster::mask(raw_crop, site)

st_crs(site) == st_crs(raw_raster2)

##scale the raster
raw_mask <- raw_mask*0.1

raw_mean <- calc(raw_mask, mean)


########################################
####### Yearly Anomaly Calculation #####
#######################################

## Subtract the mean raster image from the yearly raster image
raw_anam <- raw_mask - raw_mean

# Name the years
names(raw_anam) <- years

mapview(raw_anam[[c(1,18)]], alpha.regions = 1)

?mapview

# Convert to dataframe, remove the NA values
raw_df <- as.data.frame(raw_anam, xy = TRUE) %>% drop_na() %>% 
  melt(id.vars = c('x', 'y')) %>% rename('year' = 'variable') %>% mutate(year = gsub('X', '', year))



## Break the data into various categories
raw_df_cat <- raw_df %>% mutate(temp_cat = cut(value, breaks = 5))


temp_classes <- classInt::classIntervals(raw_df$value, 
                                        n=9, 
                                        style="fixed",
                                        fixedBreaks=c(-4, -3, -2, -1, 0, 1, 2, 3, 4))


raw_df$classes <- cut(raw_df$value, temp_classes$brks) 

dev.off()

## Plot the anomaly data of all the years
ggplot() +
  geom_raster(data = raw_df, aes(x = x, y = y, fill = classes)) +
  geom_sf(data = site, lwd = 0.6, colour = "black", fill = NA)  +
  scale_fill_manual(labels = legend_labels, name = "Temperature (°C)", values = rev(hcl.colors(7, "RdYlBu"))) +
  ggtitle("Temperature Anomaly - variation from mean (2003-2020)") +
  guides(fill = guide_legend(keyheight = unit(4, units = "mm"),
                             keywidth = unit(8, units = "mm"),
                             direction = "horizontal",
                             nrow = 1,
                             ticks.colour = "white",
                             label.position = "bottom",
                             title.position = "top",
                             title.hjust = 0.5)) +
  
  facet_wrap(~year, ncol = 3) + theme_fivethirtyeight() +
  
  theme(panel.spacing.x = unit(1, "lines"),
        strip.background =element_rect(fill= alpha('#cd5c5c', 0.05), colour = NA),
        strip.text.x = element_text(size = 50,  face = 'bold'),
        axis.text.x = element_text(size = 35, face = 'bold', color = '#636363'),
        axis.text.y = element_text(size = 35, face = 'bold', color = '#636363'),
        #axis.text.y.left = element_text(colour = "#08589e"),
        axis.title.x =  element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 80, face = 'bold', color = "black"),
        legend.key.size = unit(0.8, 'in'), 
        legend.title = element_text(size = 55, face = "bold", angle = 0),
        legend.text = element_text(size = 45, angle = 0, vjust = 0.5),
        legend.direction="horizontal",
        legend.position="bottom", 
        legend.box = "vertical",
        plot.caption = element_text(size = 35))

ggsave("output/temp_variation_mean_day3.pdf", height = 30, width = 22, units = 'in', dpi= 200)

legend_labels <- c("-4 to -3", "-3 to -2", "-2 to -1", "-1 to 0", "0 to 1", "1 to 2", "2 to 3")

ggplot() +
  geom_sf(data = site)


## Areas which gained temperature > 0 C over the mean
raw_df %>% filter(value > 0) %>% 
ggplot() +
  geom_raster(aes(x = x, y = y, fill = classes)) +
  geom_sf(data = site, lwd = 0.6, colour = "black", fill = NA)  +
  scale_fill_discrete() +
  facet_wrap(~year, ncol = 3) # theme_void()


## Mean anomaly in the districts
raw_dist_mean <- extract(raw_anam, site, mean, na.rm = TRUE, df = TRUE)

dist_mean_anam <- site %>% st_set_geometry(., NULL) %>% 
  mutate(ID = 1:18) %>% left_join(raw_dist_mean, by = "ID") %>% as_tibble() %>%  dplyr::select(ID, 1:2, 8:25)


## Convert to long
dist_mean_long <- pivot_longer(dist_mean_anam, !c(1:3), names_to = 'year', values_to = 'value') %>% 
  mutate(year = gsub('X', '', year)) %>% mutate(var = ifelse(value <= 0, 'blue', 'red'))

col_pal <- if_else(dist_mean_long$value <= 0, "blue", "red")

ggplot(dist_mean_long, aes(x = year, y = value, group = district)) +
  geom_bar(stat = 'identity', aes(fill = col_pal), show.legend = F) +
  #geom_bar(stat = 'identity', fill = col_pal, show.legend = FALSE) + 
  scale_fill_manual(values = c("blue", "red")) +
  facet_wrap(~district, ncol = 3) +
  theme_base() +
  theme(panel.spacing.x = unit(1, "lines"),
        strip.background =element_rect(fill= alpha('#cd5c5c', 0.05), colour = NA),
        strip.text.x = element_text(size = 50,  face = 'bold'),
        axis.text.x = element_text(size = 45, color = '#636363', angle = 90),
        axis.text.y = element_text(size = 45, face = 'bold', color = '#636363'),
        #axis.text.y.left = element_text(colour = "#08589e"),
        axis.title.x =  element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 80, face = 'bold', color = "black"),
        legend.key.size = unit(0.8, 'in'), 
        legend.title = element_text(size = 55, face = "bold", angle = 90),
        legend.text = element_text(size = 45, angle = 0, vjust = 0.5),
        legend.direction="horizontal",
        legend.position="bottom", 
        legend.box = "vertical",
        plot.caption = element_text(size = 35))


ggsave("output/temp_anomaly_dist.png", height = 30, width = 20, units = 'in', dpi= 200)




raw_df %>% group_by(year) %>% summarise(mean = mean(value)) %>% 
  ggplot(aes(x = year, y = mean)) + geom_bar(stat = 'identity', fill = 5) +
  scale_fill_manual(values = c('red', 'blue'))


## Changes between 2003 and 2020

raw_diff_03_20 <- raw_anam[[18]] - raw_anam[[1]]
mapview(raw_diff_03_20)



ggplot() +
  geom_sf(data = site, fill = NA) +
  facet_wrap(~district)
