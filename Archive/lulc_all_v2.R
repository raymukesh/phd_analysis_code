library(dplyr)
library(data.table)
library(readr)
library(tidyverse)
library(tidyr)
library(plyr)
library(viridis)
library(hrbrthemes)
library(kableExtra)
library(patchwork)
library(cowplot)
library(ggthemes)



getwd()


## 1.0 Data Preprocessing ----
## Data Loading
raw_data <- list.files(path = getwd(), full.names = T) %>% lapply(read_csv) %>% bind_rows()

## check for NA values in rows
which(!complete.cases(raw_data))


## Remove NA values
raw_data <- na.omit(raw_data)
View(raw_data)

# Add new columns of water, vegetation, builtup, agriculture, barren
raw_data$water <- format(round((raw_data$HISTO_1*3600)/1e6, 2), nsmall = 2)
raw_data$vegetation <- format(round((raw_data$HISTO_2*3600)/1e6, 2), nsmall = 2)
raw_data$builtup <- format(round((raw_data$HISTO_3*3600)/1e6, 2), nsmall = 2)
raw_data$agriculture <- format(round((raw_data$HISTO_4*3600)/1e6, 2), nsmall = 2)
raw_data$barren <- format(round((raw_data$HISTO_5*3600)/1e6, 2), nsmall = 2)

## Remove HISTO columns from 8 to 12
raw_data <- raw_data[, -c(8:12)]

## Converting to factors from colum 1 to 6
raw_data[1:6] <- lapply(raw_data[1:6], as.factor)

## Converting to longer format
dist_lu_v3 <- raw_data %>% pivot_longer(cols = 8:12, names_to = 'landuse', values_to = 'area')
dist_lu_v3$area <- as.numeric(round(dist_lu_v3$area,1))


## Rename district names in R - Since its a level, normal renaming won't work
## Either use conventional method or 'revalue' function from plyr package
levels(dist_lu_v3$district)
levels(dist_lu_v3$district)[levels(dist_lu_v3$district) == 'North'] <- 'North Delhi'



## Renaming factors with Plyr package 'revalue' function
dist_lu_v3$district <-   revalue(dist_lu_v3$district, c("North East" = "North East Delhi",
                      "Central" = "Central Delhi",
                      "East" = "East Delhi",
                      "North East" = "North East Delhi",
                      "North West" = "North West Delhi",
                      "South" = "South Delhi",
                      "South West" = "South West Delhi",
                      "West" = "West Delhi"))

dist_lu_v3$state <- revalue(dist_lu_v3$state, c("NCT of Delhi" = "Delhi"))
dist_lu_v3$landuse <- revalue(dist_lu_v3$landuse, c("water" = "Water",
                                                    "vegetation"="Vegetation",
                                                    "builtup"="Builtup",
                                                    "agriculture"="Agriculture",
                                                    "barren"="Barren"))


## Write the data
write_csv(dist_lu_v3, "lulc_all_v2.csv")


## Loading the processed dataset- run from this section
dist_lu_v3 <- read_csv('lulc_all_v2.csv')


## Converting to factors from colum 1 to 6
dist_lu_v3[1:6] <- lapply(dist_lu_v3[1:6], as.factor)
dist_lu_v3[8] <- lapply(dist_lu_v3[8], as.factor)





## ANALYSIS =======================================================

## 2.0 Data Analysis ----


## 2.1 Land use composition (total area of each landuse) by State in 2020 ----
state_area <- dist_lu_v3 %>% select(year, state, district, landuse, area) %>% 
  filter(year == 2020) %>% group_by(state, landuse) %>% 
  dplyr::summarise(sum = sum(area)) %>% 
  ggplot(aes(x=landuse, y=sum, group = landuse, fill = landuse)) + geom_bar(stat = 'identity', color="black") + 
  scale_fill_manual(values=c("yellow", "#BF9000", "red","#00B050", "#56B4E9")) + 
  ylab("Area"~(km^2)) + xlab('') +
  geom_text(aes(label = sum), size = 4, hjust = -0.1) + ylim(c(-0, 7200)) + 
  coord_flip() +
  facet_wrap(~state, ncol=3) + theme_linedraw() +
  theme(panel.spacing.x = unit(1, "lines"),
        strip.text.x = element_text(size = 17),
        axis.text.x = element_text(angle=45, size = 12, hjust = 1),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 14, face = "bold" ),
        legend.position = 'none')


## 2.2 Proportion of landuse in 2020 by state ----
state_prop <- dist_lu_v3 %>% select(year, state, district, area_km, landuse, area) %>%
  filter(year == 2020 & state %in% c('Delhi', 'Haryana', 'Uttar Pradesh')) %>% group_by(state, landuse) %>% 
  dplyr::summarise(area_cat = sum(area)) %>% 
  
  ## this step creates sum of area of states and adds it into the above table
  left_join(dist_lu_v3 %>% filter(year == 2020 & state %in% c('Delhi', 'Haryana', 'Uttar Pradesh')) %>% 
              group_by(year, state) %>% 
              dplyr::summarise(statearea = sum(area)), by = c('state' = 'state')) %>% 
  
  ## Creates a new column with proportion
  mutate(prop = round((area_cat/statearea)*100, 2)) %>%
  
  ggplot(aes(x=landuse, y=prop, fill=landuse)) + geom_bar(stat = 'identity', color = 'black') + 
  scale_fill_manual(values=c("yellow", "#BF9000", "red","#00B050", "#56B4E9")) + 
  ylim(0,100) + ylab("(%)") + xlab('') +
  facet_wrap(~state, ncol = 3) + geom_text(aes(label = prop), size = 4, hjust = -0.1) + 
  coord_flip() + theme_linedraw() +
  theme(panel.spacing.x = unit(1, "lines"),
        strip.text.x = element_text(size = 17),
        axis.text.x = element_text(angle=45, size = 12, hjust = 1),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 14, face = "bold" ),
        legend.position = 'none')

## combine the plots
plot_grid(state_area, state_prop, labels = c('A', 'B'), ncol = 1)


## ---------------------------------------------------------------------
## 2.2 Land use composition by Districts in 2020 ----
delhi <- dist_lu_v3 %>% select(year, state, district, landuse, area) %>% 
  filter(year == 2020 & state %in% c("Delhi")) %>% group_by(district, landuse) %>% 
  dplyr::summarise(sum = sum(area)) %>% 
  ggplot(aes(x=landuse, y=sum, group = landuse, fill = landuse)) + 
  geom_bar(stat = 'identity', color = 'black') + scale_fill_manual(values=c("yellow", "#BF9000", "red","#00B050", "#56B4E9")) +
  ylab('') + xlab('Delhi\n') +
  geom_text(aes(label = sum), size = 4, hjust = -0.1) + ylim(c(0, 2500)) + coord_flip() +
  facet_wrap(~district, ncol=3) + theme_linedraw() +
  theme(panel.spacing.x = unit(0.5, "lines"),
        strip.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold" ),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_line(color = '#ADADAD'),
        legend.position = 'none')

haryana <- dist_lu_v3 %>% select(year, state, district, landuse, area) %>% 
  filter(year == 2020 & state %in% c("Haryana")) %>% group_by(district, landuse) %>% 
  dplyr::summarise(sum = sum(area)) %>% 
  ggplot(aes(x=landuse, y=sum, group = landuse, fill = landuse)) + 
  geom_bar(stat = 'identity', color = 'black') + scale_fill_manual(values=c("yellow", "#BF9000", "red","#00B050", "#56B4E9")) +
  ylab('') + xlab('Haryana\n') +
  geom_text(aes(label = sum), size = 4, hjust = -0.1) + ylim(c(0, 2500)) + coord_flip() +
  facet_wrap(~district, ncol=3) + theme_linedraw() +
  theme(panel.spacing.x = unit(0.5, "lines"),
        strip.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold" ),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_line(color = '#ADADAD'),
        legend.position = 'none')

up <- dist_lu_v3 %>% select(year, state, district, landuse, area) %>% 
  filter(year == 2020 & state %in% c("Uttar Pradesh")) %>% group_by(district, landuse) %>% 
  dplyr::summarise(sum = sum(area)) %>% 
  ggplot(aes(x=landuse, y=sum, group = landuse, fill = landuse)) + 
  geom_bar(stat = 'identity', color = 'black') + scale_fill_manual(values=c("yellow", "#BF9000", "red","#00B050", "#56B4E9")) +
  ylab("Area"~(km^2)) + xlab('Uttar Pradesh\n') + 
  geom_text(aes(label = sum), size = 4, hjust = -0.1) + ylim(c(0, 2500)) + coord_flip() +
  facet_wrap(~district, ncol=3) + theme_linedraw() +
  theme(panel.spacing.x = unit(0.5, "lines"),
        strip.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold" ),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        panel.grid = element_line(color = '#ADADAD'),
        legend.position = 'none')


## Stack the chart with cowplot package
plot_grid(delhi, haryana, up, ncol = 1)

## Saves the last plot
ggsave("states_area_prop_2020.png", height = 15, width = 10,  units = c('in'), dpi= 300)



## ==============================================================================

## Proportion of land use composition in Districts

## Delhi
delhi_dist <- dist_lu_v3 %>% select(year, state, district, area_km, landuse, area) %>% 
  filter(year == 2020 & state %in% c("Delhi")) %>% group_by(state, district, landuse) %>% 
  dplyr::summarise(lcArea = sum(area)) %>% 
 
  ## this step creates sum of area of states and adds it into the above table
  left_join(dist_lu_v3 %>% filter(year == 2020 & state %in% c('Delhi')) %>% 
              group_by(year, district) %>% 
              dplyr::summarise(districtArea = sum(area)), by = c('district' = 'district')) %>% 

  ## Creates a new column with proportion
  mutate(prop2 = round((lcArea/districtArea)*100, 2)) %>%
  
  ggplot(aes(x=landuse, y=prop2, fill=landuse)) + geom_bar(stat = 'identity', color = 'black') + 
  scale_fill_manual(values=c("yellow", "#BF9000", "red","#00B050", "#56B4E9")) + 
  ylim(0,100) + ylab("") + xlab('Delhi\n') +
  facet_wrap(~district, ncol = 3) + geom_text(aes(label = prop2), size = 4, hjust = -0.1) + 
  coord_flip() + theme_linedraw() +
  theme(panel.spacing.x = unit(1, "lines"),
        strip.text.x = element_text(size = 17),
        axis.text.x = element_text(angle=45, size = 12, hjust = 1),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 14, face = "bold" ),
        panel.grid = element_line(color = '#ADADAD'),
        legend.position = 'none')


## Haryana
haryana_dist <- dist_lu_v3 %>% select(year, state, district, area_km, landuse, area) %>% 
  filter(year == 2020 & state %in% c("Haryana")) %>% group_by(state, district, landuse) %>% 
  dplyr::summarise(lcArea = sum(area)) %>% 
  
  ## this step creates sum of area of states and adds it into the above table
  left_join(dist_lu_v3 %>% filter(year == 2020 & state %in% c('Haryana')) %>% 
              group_by(year, district) %>% 
              dplyr::summarise(districtArea = sum(area)), by = c('district' = 'district')) %>% 
  
  ## Creates a new column with proportion
  mutate(prop2 = round((lcArea/districtArea)*100, 2)) %>%
  
  ggplot(aes(x=landuse, y=prop2, fill=landuse)) + geom_bar(stat = 'identity', color = 'black') + 
  scale_fill_manual(values=c("yellow", "#BF9000", "red","#00B050", "#56B4E9")) + 
  ylim(0,100) + ylab("") + xlab('Haryana\n') +
  facet_wrap(~district, ncol = 3) + geom_text(aes(label = prop2), size = 4, hjust = -0.1) + 
  coord_flip() + theme_linedraw() +
  theme(panel.spacing.x = unit(1, "lines"),
        strip.text.x = element_text(size = 17),
        axis.text.x = element_text(angle=45, size = 12, hjust = 1),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 14, face = "bold" ),
        panel.grid = element_line(color = '#ADADAD'),
        legend.position = 'none')  
  
## Uttar Pradesh
up_dist <- dist_lu_v3 %>% select(year, state, district, area_km, landuse, area) %>% 
  filter(year == 2020 & state %in% c("Uttar Pradesh")) %>% group_by(state, district, landuse) %>% 
  dplyr::summarise(lcArea = sum(area)) %>% 
  
  ## this step creates sum of area of states and adds it into the above table
  left_join(dist_lu_v3 %>% filter(year == 2020 & state %in% c('Uttar Pradesh')) %>% 
              group_by(year, district) %>% 
              dplyr::summarise(districtArea = sum(area)), by = c('district' = 'district')) %>% 
  
  ## Creates a new column with proportion
  mutate(prop2 = round((lcArea/districtArea)*100, 2)) %>%
  
  ggplot(aes(x=landuse, y=prop2, fill=landuse)) + geom_bar(stat = 'identity', color = 'black') + 
  scale_fill_manual(values=c("yellow", "#BF9000", "red","#00B050", "#56B4E9")) + 
  ylim(0,100) + ylab("(%)") + xlab('Uttar Pradesh\n') +
  facet_wrap(~district, ncol = 3) + geom_text(aes(label = prop2), size = 4, hjust = -0.1) + 
  coord_flip() + theme_linedraw() +
  theme(panel.spacing.x = unit(1, "lines"),
        strip.text.x = element_text(size = 17),
        axis.text.x = element_text(angle=45, size = 12, hjust = 1),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 14, face = "bold" ),
        panel.grid = element_line(color = '#ADADAD'),
        legend.position = 'none') 

## Stack the chart with cowplot package
plot_grid(delhi_dist, haryana_dist, up_dist, ncol = 1)

## Saves the last plot
ggsave("districts_proportion.pdf", height = 15, width = 10,  units = c('in'), dpi= 300)









## Growth of land uses in all the districts yearly
dist_lu_v3 %>% filter(state %in% c('Haryana')) %>% ggplot(aes(x=year, y=area, group = landuse, color = landuse)) +
  geom_line() + facet_wrap(~district, scale = 'free_y', ncol = 3) +
  title(main = "Land Use Changes in Haryana", col.main = 'red') 



dist_lu_v3 %>% filter(landuse %in% c('barren')) %>% group_by(year, district) %>% 
  dplyr::summarise(builtup = max(area)) %>% 
  ggplot(aes(x = year, y = builtup, fill = district)) + geom_col() + 
  geom_text(aes(label = builtup), size = 3, hjust = 1) + ylab('Built') +
  coord_flip() + facet_wrap(~district, scale = 'free_x', ncol = 3) + 
  theme(plot.title = element_text(size=20, family = "Roboto Condensed"),
        axis.title.x = element_text(color="#993333", size=14, family = "Roboto Condensed"),
        axis.title.y = element_text(color="#993333", size=14, family = "Roboto Condensed"),
        axis.text.x = element_text(angle=45, hjust = 1),
        legend.position = "none")


dist_lu_v3 %>% group_by(year, district) %>% 
  dplyr::summarise(sum = sum(area)) %>% 
  ggplot(aes(x= district, y = sum, fill = district, group = 1)) + geom_col() + 
  coord_flip() +  facet_wrap(~year, ncol = 4)



dist_lu_v3 %>% filter(landuse %in% c('builtup')) %>% ggplot(aes(year, area, group = district, text= district)) + 
  geom_area(aes(fill = district)) + scale_fill_viridis(discrete = T) + facet_wrap(facets = ~reorder(district, area), scale = 'free_y') + theme_ipsum() +
  theme(legend.position = 'none')

dist_lu_v3 %>% filter(landuse %in% c('builtup')) %>% group_by(year) %>% dplyr::summarise(sum = sum(area)) %>% 
  mutate(change = ((sum - lag(sum))/lag(sum))*100) %>% 
  ggplot(aes(x=year, y=sum, group = 1)) + geom_line()


str(dist_lu_v3$year)
  
  
