library(raster)
library(terra)
library(tidyverse)
library(reshape2)
library(sf)
library(plyr)
library(ggthemes)
library(ggrepel)
library(rgdal)


#Read the raster files

images <- list.files("raster/", pattern = "\\.tif$", full.names = TRUE)
rasStack <- stack(images)
year2020 <- rast("raster/final_2020_v4.tif")
year2040 <- rast("raster/final_2040_v7.tif")

##load the site
site <- st_read("site/Total_area_43N_1.shp")
districts <- st_read("all_districts/all_districts_site_43N.shp")
st_crs(site) == st_crs(rasStack)
plot(districts)

district_plot <- readOGR(dsn = "all_districts/.", layer = "all_districts_site_43N",  stringsAsFactors = F)
district_plot <- fortify(district_plot)


##crop and mask
ras_crop <- raster::crop(rasStack, site)
ras_mask <- raster::mask(ras_crop, site)

plot(ras_mask)

##Change the name of the raster layers
names(ras_mask) <- c("Y2020", "Y2025", "Y2030", "Y2035", "Y2040")

##Convert rasterStack to dataframe
rast_df <- as.data.frame(ras_mask, xy = TRUE) %>% melt(id.vars =c('x','y'))
rast_df <- rast_df[complete.cases(rast_df),]

##set new colors
newCols <- c("#1080ff", "#049a01", "red", "#ede731", "#ca994a") ##colors for maps
newCols_2 <- c("#2b8cbe", "#31a354", "red", "#fecc5c", "#fc8d59") ##colors for graphs

newNames <- c("Water", "Vegetation", "Built-up", "Agriculture", "Barren")
distNames <- c(unique(districts$district))



##plot the future maps
ggplot() + geom_raster(data=rast_df %>% mutate(variable = gsub("Y","", variable)), aes(x=x/1e4, y=y/1e5, fill = as.character(value))) +
  geom_polygon(data = district_plot, aes(x = long/1e4, y = lat/1e5, group = group), lwd = 0.6, colour = "black", fill = NA) +
  scale_fill_manual(name = "Land Cover", values = newCols, labels = newNames, na.translate = FALSE) +
  labs(color = "Land Use") + xlab("Longitude") + ylab("Latitude") +
  ggtitle("Future Land Use 2020-2040") +
  coord_fixed(10/1) +
  facet_wrap(~variable, ncol = 2) +
  theme_fivethirtyeight() +
  theme(strip.text.x = element_text(size = 20, face = 'bold'),
        axis.text.x = element_text(size = 14, face = 'bold', angle = 0),
        axis.text.y = element_text(size = 14, face = 'bold'),
        axis.title.x = element_text(size = 20, margin = margin(t = 20), face = 'bold'),
        axis.title.y = element_text(size = 20, margin = margin(r = 10), face = 'bold'),
        plot.title = element_text(size = 28, face = 'bold', hjust = 0.5),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20, face = 'bold'),
        legend.position = "bottom",
        legend.direction = "horizontal")

ggsave("output/future_maps_2.png", height = 17, width = 14, dpi = 250)


## Extract the total of each class based on area
freq_df <- freq(ras_mask, usenames=TRUE)
freq_df <- ldply(freq_df, data.frame) %>% drop_na() %>% dplyr::rename('year' = '.id')


##Convert pixels to square kilometers and remove "Y" from the year column
lulc_chg <- freq_df %>%
  mutate(km2 = round((count * 3600 / 1000000),2)) %>% mutate(year = gsub("Y", "", year))


## Plot the chart of five yearly growth of each class
lulc_chg %>% ggplot() + 
  geom_line(aes(x=year, y=km2, group = value, color = as.factor(value)), lwd =1.5) +
  geom_point(aes(x=year, y=km2, color = as.factor(value)), size = 4) +
  scale_color_manual( values = newCols_2, labels = newNames) +
  
  labs(color = "Land Use") + xlab("Year") + ylab(expression(Area~(km^2))) +
  ggtitle("Future Land Use Change 2020 - 2040") +
  theme_fivethirtyeight() +
  theme(strip.text.x = element_text(size = 18, face = 'bold'),
        axis.text.x = element_text(size = 14, face = 'bold'),
        axis.text.y = element_text(size = 14, face = 'bold'),
        axis.title.x = element_text(size = 16, margin = margin(t = 10), face = 'bold'),
        axis.title.y = element_text(size = 16, margin = margin(r = 10), face = 'bold'),
        plot.title = element_text(size = 20, face = 'bold'),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15, face = 'bold'))

ggsave("output/future_maps.png", height = 20, width = 14, dpi = 250)

head(lulc_chg)

##Extract with respect to districts
change_dist <- terra::extract(ras_mask, districts, df = T)

change_dist <- change_dist %>% pivot_longer(2:6, names_to = "Year", values_to = "LC") %>% 
  drop_na() %>% mutate(ID = as.factor(ID), Year = as.factor(Year))

change_dist2 <- districts %>% st_set_geometry(NULL) %>% select(1:2) %>% mutate(ID = as.factor(1:18)) %>% 
                 as_tibble() %>% select(ID, district, state) %>% 
                 left_join(change_dist, by = "ID") %>% group_by(district, state, Year, LC) %>% count() %>% 
                 summarise(km2 = (n*3600)/1e6) %>% mutate(Year = gsub("Y", "", Year))




##Plot all the districts with years
change_dist2 %>% ggplot() + 
  geom_line(aes(x=Year, y=km2, group = LC, color = as.factor(LC)), lwd =1.3) +
  geom_point(aes(x=Year, y=km2, color = as.factor(LC)), size =4) +
  scale_color_manual(values = newCols_2, labels = newNames) +
  labs(color = "Land Use") + xlab("Year") + ylab(expression(Area~(km^2))) +
  ggtitle("Future Land Use Change of the Districts (2020 - 2040)") +
  facet_wrap(~district, ncol = 3, scales = 'free') +
  theme_fivethirtyeight() +
  theme(strip.text.x = element_text(size = 20, face = 'bold'),
        axis.text.x = element_text(size = 14, face = 'bold', angle = 45),
        axis.text.y = element_text(size = 14, face = 'bold'),
        axis.title.x = element_text(size = 20, margin = margin(t = 20), face = 'bold'),
        axis.title.y = element_text(size = 20, margin = margin(r = 10), face = 'bold'),
        plot.title = element_text(size = 28, face = 'bold', hjust = 0.5),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20, face = 'bold'))

ggsave("output/districts_future_change.png", height = 20, width = 14, dpi = 250)

  
## Plot by States
change_dist2 %>% group_by(state, Year, LC) %>% dplyr::summarise(sum = sum(km2)) %>% 
  ggplot() +
  geom_line(aes(x=Year, y=sum, group = LC, color = as.factor(LC)), lwd =1.2) +
  geom_point(aes(x=Year, y=sum, color = as.factor(LC)), size = 3) +
  
  scale_color_manual(values = newCols_2, labels = newNames) +
  labs(color = "Land Use") + xlab("Year") + ylab(expression(Area~(km^2))) +
  ggtitle("Future Land Use Change of the States (2020 - 2040)") +
  facet_wrap(~state, ncol = 3, scales = 'free') +
  theme_fivethirtyeight() +
  theme(strip.text.x = element_text(size = 22, face = 'bold'),
        axis.text.x = element_text(size = 12, face = 'bold'),
        axis.text.y = element_text(size = 12, face = 'bold'),
        axis.title.x = element_text(size = 16, margin = margin(t = 20), face = 'bold'),
        axis.title.y = element_text(size = 16, margin = margin(r = 10), face = 'bold'),
        plot.title = element_text(size = 20, face = 'bold'),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15, face = 'bold'))


ggsave("output/state_future_change.png", height = 6, width = 12, dpi = 250)

change_dist2 %>% group_by(Year, LC) %>% filter(Year %in% c("2020", "2040")) %>% summarise(km = sum(km2))

## Statewise Table LULC
state_lulc_future <- change_dist2 %>% group_by(state, Year, LC) %>% dplyr::summarise(sum = sum(km2)) %>%
  pivot_wider(names_from = Year, values_from = sum)


write.csv(state_lulc_future, file = "state_lulc_2040.csv", row.names = F)



#######################################
## Overall change of land use
#######################################
change_dist2 %>% group_by(Year, LC) %>% dplyr::summarise(sum = sum(km2)) %>% 
  ggplot() +
  geom_line(aes(x=Year, y=sum, group = LC, color = as.factor(LC)), lwd =1.5) +
  geom_point(aes(x=Year, y=sum, color = as.factor(LC)), size = 5) +
  scale_color_manual(values = newCols_2, labels = newNames) +
  labs(color = "Land Use") + xlab("Year") + ylab(expression(Area~(km^2))) +
  ggtitle("Future Land Use Change (2020 - 2040)") +
  theme_fivethirtyeight() +
  theme(strip.text.x = element_text(size = 22, face = 'bold'),
        axis.text.x = element_text(size = 12, face = 'bold'),
        axis.text.y = element_text(size = 12, face = 'bold'),
        axis.title.x = element_text(size = 16, margin = margin(t = 20), face = 'bold'),
        axis.title.y = element_text(size = 16, margin = margin(r = 10), face = 'bold'),
        plot.title = element_text(size = 20, face = 'bold'),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15, face = 'bold'))


ggsave("output/overall_LC_future_change.png", height = 6, width = 9, dpi = 250)


##Data in Wide Format
future_LC_df <-   change_dist2 %>% group_by(district, Year, LC) %>% dplyr::summarise(sum = sum(km2)) %>% 
  pivot_wider(names_from = Year, values_from = sum)

write.csv(future_LC_df, file = "future_LC_table.csv", row.names = F)

##wide format of Districts - only built-up ## This values are already in the report
district_built_long <- change_dist2 %>% filter(LC == 3) %>% group_by(district, state, Year, LC) %>% 
  dplyr::summarise(sum = sum(km2)) %>% 
  pivot_wider(names_from = Year, values_from = sum)

write.csv(district_built_long, file = "districts_future_change.csv", row.names = F)



########################################
## Cross tabulation of 2020 and 2040
########################################
changeAreas <- c(year2020, year2040)
names(changeAreas) <- c("Y2020", "Y2040")

changedf <- crosstab(changeAreas) ##crosstab (terra) works with "rast" and not "stack" from raster package

changedf <- as.data.frame(changedf)
class(changedf)

changedf2 <- changedf %>% mutate(Y2020 = factor(Y2020, 
                                                levels = 1:5, 
                                                labels = newNames),
                                 Y2040 = factor(Y2040,
                                                levels = 1:5,
                                                labels = newNames),
                                 Freq = round((Freq*3600)/1e6, 2)) %>% group_by(Y2020) %>% 
                          mutate(TOT2020 = sum(Freq), perc = round(100*Freq/TOT2020,2))


##Change of areas from 2020 to 2040
changemat <- matrix(changedf2$Freq, nrow = 5, ncol = 5)

#changemat <- round(changemat)
rownames(changemat) <- newNames
colnames(changemat) <- newNames

write.table(changemat, file = "matrix_20_40.csv", row.names = T, col.names = T)

##Percentage change from 2020 to 2040
changePerc <- matrix(changedf2$perc, nrow = 5, ncol = 5)
rownames(changePerc) <- newNames
colnames(changePerc) <- newNames


write.table(changePerc, file = "matrix_perc_20_40.csv", row.names = T, col.names = T)



## Plot the perc change from matrix
ggplot(data = changedf2, aes(x = Y2020, y = perc, group = Y2040, fill = Y2040)) +
  geom_bar(color = "black", position = "dodge", stat = "identity") + ylim(c(0,100)) +
  geom_text(aes(label = round(perc,1)), size = 4, vjust =0.5, hjust = -0.2, angle = 90, position = position_dodge(width = 0.8)) +
  scale_fill_manual(name = "2040\nLand Cover", values = newCols) +
  labs(x = "2020 Land Cover", y = "(%)") +
  ggtitle("Percentage Land Use Change (2020 - 2040)\n") +
  theme_fivethirtyeight() +
  theme(strip.text.x = element_text(size = 22, face = 'bold'),
        axis.text.x = element_text(size = 15, face = 'bold'),
        axis.text.y = element_text(size = 12, face = 'bold'),
        axis.title.x = element_text(size = 16, margin = margin(t = 20), face = 'bold'),
        axis.title.y = element_text(size = 16, margin = margin(r = 10), face = 'bold'),
        plot.title = element_text(size = 20, face = 'bold'),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15, face = 'bold'),
        legend.position = "right",
        legend.direction = "vertical")

ggsave("output/20_40_change_perc.png", height = 6, width = 12, dpi = 250)


## Plot the area change from matrix
ggplot(data = changedf2, aes(x = Y2020, y = Freq, group = Y2040, fill = Y2040)) +
  geom_bar(color = "black", position = "dodge", stat = "identity") + ylim(c(0,12000)) +
  geom_text(aes(label = round(Freq,1)), size = 4, vjust =0.5, hjust = -0.2, angle = 90, position = position_dodge(width = 0.8)) +
  scale_fill_manual(name = "2040\nLand Cover", values = newCols) +
  labs(x = "2020 Land Cover", y = expression(Area~(km^2))) +
  ggtitle("Land Use Change (2020 - 2040)\n") +
  theme_fivethirtyeight() +
  theme(strip.text.x = element_text(size = 22, face = 'bold'),
        axis.text.x = element_text(size = 15, face = 'bold'),
        axis.text.y = element_text(size = 12, face = 'bold'),
        axis.title.x = element_text(size = 16, margin = margin(t = 20), face = 'bold'),
        axis.title.y = element_text(size = 16, margin = margin(r = 10), face = 'bold'),
        plot.title = element_text(size = 20, face = 'bold'),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15, face = 'bold'),
        legend.position = "right",
        legend.direction = "vertical")

ggsave("output/20_40_change_area.png", height = 6, width = 12, dpi = 250)



dev.off()
