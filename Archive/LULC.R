library(readr)
library(ggplot2)
library(tidyverse)
library(patchwork)


## 1.0 Data ----
lulc <- read_csv("master_all_districtwise.csv", )

## Converting the data to long format
lulc2 <- pivot_longer(lulc, 8:12, names_to = "land_use", values_to = "area")


## Converting non-numeric columns to factors
lulc2$year <- as.factor(as.character(lulc2$year))
lulc2$district <- as.factor(as.character(lulc2$district))
lulc2$state <- as.factor(as.character(lulc2$state))
lulc2$st_cen_code <- as.factor(as.character(lulc2$st_cen_code))
lulc2$dt_cen_code <- as.factor(as.character(lulc2$dt_cen_code))
lulc2$censuscode <- as.factor(as.character(lulc2$censuscode))


## Land use change of all the district year wise
lulc2 %>% filter(!lulc2$district == "Delhi") %>% 
ggplot(aes(x=year, y=area, group = land_use, color = land_use)) +
  geom_line() + facet_wrap(~district, scale = 'free_y', ncol = 3)

## Top districts with Agriculture
lulc2 %>% filter(land_use %in% c('builtup') & !lulc2$district == "Delhi") %>% group_by(year, district) %>% 
  dplyr::summarise(max_agri = max(area)) %>% arrange(desc(max_agri)) %>% 
  ggplot(aes(x = reorder(district, max_agri), y = max_agri, fill = district)) + geom_col() + 
  geom_text(aes(label = max_agri), size = 2, hjust = 1) +
  coord_flip() + facet_wrap(~year, scale = 'free_x', ncol = 4) + theme_bw() +
  theme(plot.title = element_text(size=20, family = "Roboto Condensed"),
        axis.title.x = element_text(color="#993333", size=14, family = "Roboto Condensed"),
        axis.title.y = element_text(color="#993333", size=14, family = "Roboto Condensed"),
        axis.text.x = element_text(angle=45, hjust = 1),
        legend.position = "none")


## Use it to calculate any landuse for any year by changing the values
sum(lulc2$area[lulc2$year == '1990' & lulc2$land_use == 'builtup' & lulc2$district != 'Delhi'], na.rm = T)



##
meerut <- lulc2 %>% filter(land_use %in% c('agriculture', 'builtup', 'barren', 'vegetation', 'water') & lulc2$district == "Meerut") %>% 
  group_by(year, district, land_use) %>% 
  dplyr::summarise(max_agri = max(area)) %>% 
  ggplot(aes(x = year, y = max_agri, fill = land_use)) + 
  geom_col() + ylab('') + xlab('Meerut') + ggtitle('Meerut') + coord_flip() +
  facet_wrap(~land_use, scale = 'free_x', ncol = 5) +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle=45, hjust = 1))


gurgaon <- lulc2 %>% filter(land_use %in% c('agriculture', 'builtup', 'barren', 'vegetation', 'water') & lulc2$district == "Gurgaon") %>% 
  group_by(year, district, land_use) %>% 
  summarise(max_agri = max(area)) %>% 
  ggplot(aes(x = year, y = max_agri, fill = land_use)) + 
  geom_col() + ylab('')  + xlab('Gurgaon') + coord_flip() +
  facet_wrap(~land_use, scale = 'free_x', ncol = 5) + 
  theme(legend.position = 'none',
        axis.text.x = element_text(angle=45, hjust = 1))


ghaziabad <- lulc2 %>% filter(land_use %in% c('agriculture', 'builtup', 'barren', 'vegetation', 'water') & lulc2$district == "Ghaziabad") %>% 
  group_by(year, district, land_use) %>% 
  summarise(max_agri = max(area)) %>% 
  ggplot(aes(x = year, y = max_agri, fill = land_use)) + 
  geom_col() + ylab('') + xlab('Ghaziabad') + coord_flip() +
  facet_wrap(~land_use, scale = 'free_x', ncol = 5) +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle=45, hjust = 1))




meerut/gurgaon/ghaziabad




lulc2 %>% filter(land_use %in% c('builtup') & lulc2$district == "Delhi") %>% group_by(year, district) %>% 
  summarise(max_agri = max(area))

## 2.0 Agriculture All Districts ----
lulc2 %>% filter(land_use %in% c('agriculture')) %>% group_by(year, district) %>% #& !lulc2$district == "Delhi")
  dplyr::summarise(max_agri = max(area)) %>% 
  ggplot(aes(x = year, y = max_agri, fill = district)) + geom_col() + 
  geom_text(aes(label = max_agri), size = 3, hjust = 1) + ylab('Agriculture') +
  coord_flip() + facet_wrap(~district, scale = 'free_x', ncol = 3) + 
  theme(plot.title = element_text(size=20, family = "Roboto Condensed"),
        axis.title.x = element_text(color="#993333", size=14, family = "Roboto Condensed"),
        axis.title.y = element_text(color="#993333", size=14, family = "Roboto Condensed"),
        axis.text.x = element_text(angle=45, hjust = 1),
        legend.position = "none")


## 3.0 Builtup All Districts ----
lulc2 %>% filter(land_use %in% c('builtup')) %>% group_by(year, district) %>% 
  dplyr::summarise(builtup = max(area)) %>% 
  ggplot(aes(x = year, y = builtup, fill = district)) + geom_col() + 
  geom_text(aes(label = builtup), size = 3, hjust = 1) + ylab('Built') +
  coord_flip() + facet_wrap(~district, scale = 'free_x', ncol = 3) + 
  theme(plot.title = element_text(size=20, family = "Roboto Condensed"),
        axis.title.x = element_text(color="#993333", size=14, family = "Roboto Condensed"),
        axis.title.y = element_text(color="#993333", size=14, family = "Roboto Condensed"),
        axis.text.x = element_text(angle=45, hjust = 1),
        legend.position = "none")



## 4.0 Barren All Districts ----
lulc2 %>% filter(land_use %in% c('barren')) %>% group_by(year, district) %>% #& !lulc2$district == "Delhi")
  summarise(max_agri = max(area)) %>% 
  ggplot(aes(x = year, y = max_agri, fill = district)) + geom_col() + 
  geom_text(aes(label = max_agri), size = 3, hjust = 1) +
  coord_flip() + facet_wrap(~district, scale = 'free_x', ncol = 3) + 
  theme(plot.title = element_text(size=20, family = "Roboto Condensed"),
        axis.title.x = element_text(color="#993333", size=14, family = "Roboto Condensed"),
        axis.title.y = element_text(color="#993333", size=14, family = "Roboto Condensed"),
        axis.text.x = element_text(angle=45, hjust = 1),
        legend.position = "none")













                                              