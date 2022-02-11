## CALCULATING THE PATCH METRICS OF THE SITE
##============================================
install.packages("ggthemes")
install.packages("rasterVis")

##load the libraries
library(landscapemetrics)
library(raster)
library(sf)
library(hrbrthemes)
library(rasterVis)
library(plyr)
library(hrbrthemes)
library(ggthemes)
library(tidyverse)
library(kableExtra)


rm(list=ls())


## list all the functions of the raster
func <- list_lsm()

## Loading the year 2020 data set
y1990 <- raster("data/final_1990_v4.tif")
y1995 <- raster("data/final_1995_v4.tif")
y2000 <- raster("data/final_2000_v4.tif")
y2005 <- raster("data/final_2005_v4.tif")
y2010 <- raster("data/final_2010_v4.tif")
y2015 <- raster("data/final_2015_v4.tif")
y2020 <- raster("data/final_2020_v4.tif")

all_years <- raster::stack(list.files("data/", full.names = TRUE))

nlayers(all_years)

## Check the raster data
check_landscape(all_years)



## NUMP - Number of Patches for all the years
num_patch <- lsm_p_area(all_years) %>% mutate(layer = factor(layer), class = factor(class))
num_patch$layer <- plyr::revalue(num_patch$layer, c("1" = "1990",
                                                    "2" = "1995",
                                                    "3" = "2000",
                                                    "4" = "2005",
                                                    "5" = "2010",
                                                    "6" = "2015",
                                                    "7" = "2020"))

num_patch$class <- plyr::revalue(num_patch$class, c("1" = "Water",
                                                    "2" = "Vegetation",
                                                    "3" = "BuiltUp",
                                                    "4" = "Agriculture",
                                                    "5" = "Barren"))


num_patch %>% select(layer, class) %>% group_by(layer, class) %>% count() %>% 
  ggplot(aes(x = layer, y = freq, fill = class)) + geom_bar(stat = 'identity', width = 0.6) + ylim(c(0, 11000)) +
  scale_x_discrete(limits = rev(levels(num_patch$layer))) + 
  
  geom_text(aes(label = freq), hjust = -0.1, size = 5) +
  coord_flip() +
  facet_wrap(~class) +
  scale_fill_manual(values = c("#2b8cbe", "#31a354", "red", "#fecc5c", "#fc8d59")) +
  ##geom_line(size = 1, alpha = 0.8) + geom_point(aes(y = freq, color = class), size = 3) + 
  labs(color = "Land Use") + xlab("Year") + ylab("Number of Patches") +
  ggtitle("Variation in Number of Patches") + labs(fill = "Land Use") +
  scale_color_manual(values = c("#2b8cbe", "#31a354", "red", "#fecc5c", "#fc8d59")) +
  theme_fivethirtyeight() +
  theme(strip.text.x = element_text(size = 18, face = 'bold'),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 14, face = 'bold'),
        axis.title.x = element_text(size = 16, margin = margin(t = 10), face = 'bold'),
        axis.title.y = element_text(size = 16, margin = margin(r = 10), face = 'bold'),
        plot.title = element_text(size = 20, face = 'bold'),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15, face = 'bold')
  )

ggsave("output/num_patches.png", height = 8, width = 12, dpi = 250)


## All Patch Summary
num_patch %>% group_by(layer, class) %>% 
  dplyr::summarise(min = min(value),
                   q1 = quantile(value, 0.25),
                   median = median(value),
                   mean = mean(value),
                   q3 = quantile(value, 0.75),
                   max = max(value)) %>% write.csv("summary.csv", append = FALSE, row.names = FALSE)

lsm_p_area(y1990) %>% filter(class == '1') %>% summary()


aggregate(num_patch$value)


## Edge Density
edge_density <- lsm_c_ed(all_years) %>% mutate(layer = factor(layer), class = factor(class))

edge_density$layer <- plyr::revalue(edge_density$layer, c("1" = "1990",
                                                          "2" = "1995",
                                                          "3" = "2000",
                                                          "4" = "2005",
                                                          "5" = "2010",
                                                          "6" = "2015",
                                                          "7" = "2020"))

edge_density$class <- plyr::revalue(edge_density$class, c("1" = "Water",
                                                          "2" = "Vegetation",
                                                          "3" = "BuiltUp",
                                                          "4" = "Agriculture",
                                                          "5" = "Barren"))


edge_density %>% mutate(class = factor(class)) %>% group_by(layer, class) %>% 
  ggplot(aes(x= layer, y=value, group = class, color = class)) + 
  geom_line(size = 1.8, alpha = 0.8) + geom_point(aes(y = value, color = class), size = 7) + 
  labs(color = "Land Use") + xlab("Year") + ylab("Edge Density (m/ha)") +
  ggtitle("Variation in Edge Density") +
  scale_color_manual(values = c("#2b8cbe", "#31a354", "red", "#fecc5c", "#fc8d59")) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(size = 16, face = 'bold'),
        axis.text.y = element_text(size = 16, face = 'bold'),
        axis.title.x = element_text(size = 17, margin = margin(t = 10), face = 'bold'),
        axis.title.y = element_text(size = 17, margin = margin(r = 10), face = 'bold'),
        plot.title = element_text(size = 20, face = 'bold'),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 14, face = 'bold'))
  
ggsave("output/edge_density.png", height = 8, width = 12, dpi = 250)


## Patch Density
patch_density <- lsm_c_pd(all_years) %>% mutate(layer = factor(layer), class = factor(class))

patch_density$layer <- plyr::revalue(patch_density$layer, c("1" = "1990",
                                                          "2" = "1995",
                                                          "3" = "2000",
                                                          "4" = "2005",
                                                          "5" = "2010",
                                                          "6" = "2015",
                                                          "7" = "2020"))

patch_density$class <- plyr::revalue(patch_density$class, c("1" = "Water",
                                                          "2" = "Vegetation",
                                                          "3" = "BuiltUp",
                                                          "4" = "Agriculture",
                                                          "5" = "Barren"))

lsm_l_ta(y1990)

patch_density %>% mutate(class = factor(class)) %>% group_by(layer, class) %>% 
  ggplot(aes(x= layer, y=value, group = class, color = class)) + 
  geom_line(size = 1.8, alpha = 0.8) + geom_point(aes(y = value, color = class), size = 7) + 
  labs(color = "Land Use") + xlab("Year") + ylab("Number Per 100 Hectares") +
  ggtitle("Variation in Patch Density") +
  scale_color_manual(values = c("#2b8cbe", "#31a354", "red", "#fecc5c", "#fc8d59")) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(size = 16, face = 'bold'),
        axis.text.y = element_text(size = 16, face = 'bold'),
        axis.title.x = element_text(size = 17, margin = margin(t = 10), face = 'bold'),
        axis.title.y = element_text(size = 17, margin = margin(r = 10), face = 'bold'),
        plot.title = element_text(size = 20, face = 'bold'),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 14, face = 'bold'))


ggsave("output/patch_density.png", height = 8, width = 12, dpi = 250)


## Mean Patch Size
num_patch %>% select(layer, class, value) %>% group_by(layer, class) %>% 
  dplyr::summarise(mean = mean(value),sd = sd(value), median = median(value)) %>% write.csv("output/patch_distribution.csv")


mean_patch %>% ggplot(aes(x=layer, y=sd, group = class, color = class)) +
  geom_line(size = 1.8, alpha = 0.8) + geom_point(aes(y = sd, color = class), size = 5) + 
  facet_wrap(~class, scales = "free", nrow = 1) +
  labs(color = "Land Use") + xlab("Year") + ylab("Area (Ha)") +
  ggtitle("Variation in Mean Patch Size") +
  scale_color_manual(values = c("#2b8cbe", "#31a354", "red", "#fecc5c", "#fc8d59")) +
  theme_fivethirtyeight() +
  theme(panel.spacing.x = unit(2, "lines"),
        strip.text.x = element_text(size = 18, face = 'bold'),
        axis.text.x = element_text(size = 14, face = 'bold', angle = 45, vjust = 1),
        axis.text.y = element_text(size = 14, face = 'bold'),
        axis.title.x = element_text(size = 17, margin = margin(t = 10), face = 'bold'),
        axis.title.y = element_text(size = 17, margin = margin(r = 10), face = 'bold'),
        plot.title = element_text(size = 22, face = 'bold'),
        legend.position = "none",
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18, face = 'bold'))


ggsave("output/mean_patch.png", height = 4, width = 20, dpi = 250)



## Area Weighted Mean Patch

                                                                                          
## Largest Patch Index
## Class level
lpi <- lsm_c_lpi(all_years) %>% mutate(layer = factor(layer), class = factor(class))

lpi$layer <- plyr::revalue(lpi$layer, c("1" = "1990",
                                        "2" = "1995",
                                        "3" = "2000",
                                        "4" = "2005",
                                        "5" = "2010",
                                        "6" = "2015",
                                        "7" = "2020"))

lpi$class <- plyr::revalue(lpi$class, c("1" = "Water",
                                        "2" = "Vegetation",
                                        "3" = "BuiltUp",
                                        "4" = "Agriculture",
                                        "5" = "Barren"))



lpi %>% mutate(class = factor(class)) %>% group_by(layer, class) %>% 
  ggplot(aes(x= layer, y=value, group = class, color = class)) + 
  geom_line(size = 1.8, alpha = 0.8) + geom_point(aes(y = value, color = class), size = 7) + 
  labs(color = "Land Use") + xlab("Year") + ylab("(%)") +
  ggtitle("Variation in Largest Patch Index") +
  scale_color_manual(values = c("#2b8cbe", "#31a354", "red", "#fecc5c", "#fc8d59")) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(size = 16, face = 'bold'),
        axis.text.y = element_text(size = 16, face = 'bold'),
        axis.title.x = element_text(size = 17, margin = margin(t = 10), face = 'bold'),
        axis.title.y = element_text(size = 17, margin = margin(r = 10), face = 'bold'),
        plot.title = element_text(size = 20, face = 'bold'),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 14, face = 'bold'))


ggsave("output/largest_patch_index.png", height = 8, width = 12, dpi = 250)


## Shannon's Diversity Index
lsm_l_shdi(all_years)


## Core Area Density


## 

bind_rows(
  lsm_l_cohesion(y1990),
  lsm_l_cohesion(y2000),
  lsm_l_cohesion(y2010),
  lsm_l_cohesion(y2020),
)

## Aggregation Index - Class Level
agg_index <- lsm_c_ai(all_years) %>% mutate(layer = factor(layer), class = factor(class))


agg_index$layer <- plyr::revalue(agg_index$layer, c("1" = "1990",
                                                    "2" = "1995",
                                                    "3" = "2000",
                                                    "4" = "2005",
                                                    "5" = "2010",
                                                    "6" = "2015",
                                                    "7" = "2020"))

agg_index$class <- plyr::revalue(agg_index$class, c("1" = "Water",
                                                    "2" = "Vegetation",
                                                    "3" = "BuiltUp",
                                                    "4" = "Agriculture",
                                                    "5" = "Barren"))


agg_index %>% mutate(class = factor(class)) %>% group_by(layer, class) %>% 
  ggplot(aes(x= layer, y=value, group = class, color = class)) + 
  geom_line(size = 1.8, alpha = 0.8) + geom_point(aes(y = value, color = class), size = 7) + 
  labs(color = "Land Use") + xlab("Year") + ylab("Aggregation Index (%)") +
  ggtitle("Variation in Aggregation Index") +
  scale_color_manual(values = c("#2b8cbe", "#31a354", "red", "#fecc5c", "#fc8d59")) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(size = 16, face = 'bold'),
        axis.text.y = element_text(size = 16, face = 'bold'),
        axis.title.x = element_text(size = 17, margin = margin(t = 10), face = 'bold'),
        axis.title.y = element_text(size = 17, margin = margin(r = 10), face = 'bold'),
        plot.title = element_text(size = 20, face = 'bold'),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 14, face = 'bold')
  )


ggsave("output/agg_index.png", height = 8, width = 12, dpi = 250)





## Show Patches
show_lsm(y2020, class = 3, labels = F, what = "lsm_p_area")


num_patch %>% filter(layer == "1990" & !class %in% c("Agriculture","BuiltUp")) %>% 
  ggplot(aes(value)) + geom_histogram()



## Diversity Metrics
calculate_lsm(landscape = y2020, level = "landscape", type = "diversity metric", classes_max = 5)



## Calculate the K - Means Clusters
## Install factoextra package
install.packages("factoextra")
library(factoextra)

functions <- list_lsm()

## Creating the data table for clustering
metrics <- dplyr::bind_rows(
  lsm_p_area(y2000),
  lsm_p_perim(y2000),
  lsm_p_cai(y2000)
)

metrics2 <- metrics %>% select(!layer) %>% filter(class == 3) %>% 
  pivot_wider(names_from = metric, values_from = value) %>% select(4:6) %>% filter(area < 100)


## Scale the data
metrics_scaled <- scale(metrics2)
dist(metrics_scaled)

## Calculate the number of clusters needed
fviz_nbclust(metrics_scaled, kmeans, method = "wss") + 
  labs(subtitle = "Elbow Method")


## Kmeans cluster
k.out <- kmeans(metrics_scaled, centers = 4, nstart = 100)

## Visualise the cluster
km.cluster <- k.out$cluster
fviz_cluster(k.out, data = metrics_scaled,
             main = "Partitioning Cluster Points")


