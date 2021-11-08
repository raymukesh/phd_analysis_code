setwd("D:/OneDrive - UTS/PhD_UTS/Stage 3/Master Data Sheets/R Calculations/phd_analysis_code/Patch Metrics")


## Read all the classified raster files
year1990 <- raster("D:/OneDrive - UTS/PhD_UTS/Stage 3/Analysis/Prediction/ForTerrset60_v4/final_1990_v4.tif")
year1995 <- raster("D:/OneDrive - UTS/PhD_UTS/Stage 3/Analysis/Prediction/ForTerrset60_v4/final_1995_v4.tif")
year2000 <- raster("D:/OneDrive - UTS/PhD_UTS/Stage 3/Analysis/Prediction/ForTerrset60_v4/final_2000_v4.tif")
year2005 <- raster("D:/OneDrive - UTS/PhD_UTS/Stage 3/Analysis/Prediction/ForTerrset60_v4/final_2005_v4.tif")
year2010 <- raster("D:/OneDrive - UTS/PhD_UTS/Stage 3/Analysis/Prediction/ForTerrset60_v4/final_2010_v4.tif")
year2015 <- raster("D:/OneDrive - UTS/PhD_UTS/Stage 3/Analysis/Prediction/ForTerrset60_v4/final_2015_v4.tif")
year2020 <- raster("D:/OneDrive - UTS/PhD_UTS/Stage 3/Analysis/Prediction/ForTerrset60_v4/final_2020_v4.tif")



library(rgdal)
library(sp)
library(sf)
library(raster)
library(dplyr)
library(tmap)
library(landscapemetrics)
library(landscapetools)
library(ggplot2)

# Plot landscape
plot(year2020)

# Plot landscape + landscape with labeled patches
show_patches(year1990) 


# show patches of all classes
show_patches(year1990, class = "all", labels = FALSE)


## Check the metric names available in R
metric_names <- lsm_abbreviations_names
tail(metric_names)
View(metric_names)


## Caluclate the total area
metrics_total <- calculate_lsm(year2020, what = c("lsm_c_ai",
                                                  "lsm_c_core_cv",
                                                  "lsm_l_division",
                                                  "lsm_l_pd") )


## Number of Patches by Class
number_patches <- calculate_lsm(all_years, what = c("lsm_c_np"), progress = T)

number_patches[c(1,3)]  <- lapply(number_patches[c(1,3)], as.factor)

number_patches <-  number_patches %>% mutate(layer = recode(layer, "1" = "1990", "2" = "1995", "3" = "2000",
                                                "4" = "2005", "5" = "2010", "6" = "2015", "7" = "2020",)) %>% 
  
                                      mutate(class= recode(class, "1" = "Water", "2" = "Vegetation", 
                                                           "3" = "Builtup", "4" = "Agriculture", "5" = "Barren" ))


number_patches %>% group_by(layer, class) %>% summarise(sum = sum(value)) %>% 
  ggplot(aes(x = layer, y = sum, fill = class)) + geom_bar(stat = 'identity') + 
  xlab("\n Year") + ylab("Number of Patches \n") + labs(fill = "Legend") +
  scale_fill_manual(values = c("#00bbf9", "#38b000", "#f94144", "#ffd819", "#f8961e")) +
  facet_wrap(~class) + theme_linedraw() +
  theme(panel.spacing.x = unit(1, "lines"),
        panel.spacing.y = unit(1, "lines"),
        strip.text.x = element_text(size = 17, face = 'bold'),
        axis.title.y = element_text(size = 17, face = "bold" ),
        axis.title.x = element_text(size=17, face = "bold"),
        axis.text.y = element_text(size = 15),
        axis.text.x = element_text(angle=45, size = 15, hjust = 1),
        panel.grid = element_line(color = '#343a40'),
        plot.title = element_text(size=22, face = "bold"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 13),
        legend.position = 'none')
  
ggsave("num_of_patches_v4.png", height =8, width = 12, dpi= 250)
dev.off()



## Mean Patch Area
mean_patch_area <- calculate_lsm(all_years, what = c("lsm_c_area_mn"), progress = T)

mean_patch_area[c(1,3)]  <- lapply(mean_patch_area[c(1,3)], as.factor)

mean_patch_area <-  mean_patch_area %>% mutate(layer = recode(layer, "1" = "1990", "2" = "1995", "3" = "2000",
                                                            "4" = "2005", "5" = "2010", "6" = "2015", "7" = "2020",)) %>% 
  
                                        mutate(class= recode(class, "1" = "Water", "2" = "Vegetation", 
                                                            "3" = "Builtup", "4" = "Agriculture", "5" = "Barren" ))




mean_patch_area %>% group_by(layer, class) %>% summarise(sum = sum(value)) %>% 
  ggplot(aes(x = layer, y = sum, fill = class)) + geom_bar(stat = 'identity') + 
  xlab("\n Year") + ylab("Mean Patch Area \n") + labs(fill = "Legend") +
  scale_fill_manual(values = c("#00bbf9", "#38b000", "#f94144", "#ffd819", "#f8961e")) +
  facet_wrap(~class, scales = "free") + theme_linedraw() +
  theme(panel.spacing.x = unit(1, "lines"),
        panel.spacing.y = unit(1, "lines"),
        strip.text.x = element_text(size = 17, face = 'bold'),
        axis.title.y = element_text(size = 17, family = "Arial", face = "bold" ),
        axis.title.x = element_text(size=17, family = "Arial", face = "bold"),
        axis.text.y = element_text(size = 15),
        axis.text.x = element_text(angle=45, size = 15, hjust = 1),
        panel.grid = element_line(color = '#343a40'),
        plot.title = element_text(size=22, face = "bold"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 13),
        legend.position = 'none')

ggsave("mean_patch_area_v4.png", height =8, width = 12, units = c("in"), dpi= 250)
dev.off()



## Coefficient of variation of patch area

coff_variation <- calculate_lsm(all_years, what = c("lsm_c_area_cv"), progress = T)

coff_variation[c(1,3)]  <- lapply(coff_variation[c(1,3)], as.factor)

coff_variation <-  coff_variation %>% mutate(layer = recode(layer, "1" = "1990", "2" = "1995", "3" = "2000",
                                                              "4" = "2005", "5" = "2010", "6" = "2015", "7" = "2020",)) %>% 
  
                                      mutate(class= recode(class, "1" = "Water", "2" = "Vegetation", 
                                                              "3" = "Builtup", "4" = "Agriculture", "5" = "Barren" ))

coff_variation %>% group_by(layer, class) %>% summarise(sum = sum(value)) %>% 
  ggplot(aes(x = layer, y = sum, fill = class)) + geom_bar(stat = 'identity') + 
  xlab("\n Year") + ylab("Mean Patch Area \n") + labs(fill = "Legend") +
  scale_fill_manual(values = c("#00bbf9", "#38b000", "#f94144", "#ffd819", "#f8961e")) +
  facet_wrap(~class, scales = "free") + theme_linedraw() +
  theme(panel.spacing.x = unit(1, "lines"),
        panel.spacing.y = unit(1, "lines"),
        strip.text.x = element_text(size = 17, face = 'bold'),
        axis.title.y = element_text(size = 17, family = "Arial", face = "bold" ),
        axis.title.x = element_text(size=17, family = "Arial", face = "bold"),
        axis.text.y = element_text(size = 15),
        axis.text.x = element_text(angle=45, size = 15, hjust = 1),
        panel.grid = element_line(color = '#343a40'),
        plot.title = element_text(size=22, face = "bold"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 13),
        legend.position = 'none')














## Calculating Shannon Diversity Index
shannon_diversity <-  lsm_l_shdi(year2020)


## Calculate Shannon Entropy - Marginal Entropy on 4km Grid and Plot
my_raster = year2015


my_grid_geom <- st_make_grid(st_as_sfc(st_bbox(my_raster)), cellsize = 4000)
my_grid <- st_sf(geom = my_grid_geom)



plot(my_raster)
plot(my_grid, add= T)


my_metric <- sample_lsm(my_raster, my_grid, level = "landscape", metric = "ed")
my_grid <- bind_cols(my_grid, my_metric)



plot(my_grid["value"])


metrics <- calculate_lsm(year2020, what = "patch")

show_correlation(metrics, method = "pearson")
