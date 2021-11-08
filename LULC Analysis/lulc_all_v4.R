library(dplyr)
library(ggplot2)
library(data.table)
library(readr)
library(tidyr)
## library(plyr)
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
raw_data$Water <- format(round((raw_data$HISTO_1*3600)/1e6, 2), nsmall = 2)
raw_data$Vegetation <- format(round((raw_data$HISTO_2*3600)/1e6, 2), nsmall = 2)
raw_data$Builtup <- format(round((raw_data$HISTO_3*3600)/1e6, 2), nsmall = 2)
raw_data$Agriculture <- format(round((raw_data$HISTO_4*3600)/1e6, 2), nsmall = 2)
raw_data$Barren <- format(round((raw_data$HISTO_5*3600)/1e6, 2), nsmall = 2)

## Remove HISTO columns from 8 to 12
raw_data <- raw_data[, -c(8:12)]

## Converting to factors from colum 1 to 6
raw_data[1:6] <- lapply(raw_data[1:6], as.factor)

## Converting to longer format
dist_lu_v4 <- raw_data %>% pivot_longer(cols = 8:12, names_to = 'landuse', values_to = 'area')
dist_lu_v4$area <- round(as.numeric(dist_lu_v4$area), 1)


## Rename district names in R - Since its a level, normal renaming won't work
## Either use conventional method or 'revalue' function from plyr package
## DO NOT RUN THIS (for information only)
#levels(dist_lu_v3$district)
#levels(dist_lu_v3$district)[levels(dist_lu_v3$district) == 'North'] <- 'North Delhi'



## Renaming factors with Plyr package 'revalue' function
## Not required in this version of data file (required in v3)


dist_lu_v4$landuse <- revalue(dist_lu_v3$landuse, c("water" = "Water",
                                                    "vegetation"="Vegetation",
                                                    "builtup"="Builtup",
                                                    "agriculture"="Agriculture",
                                                    "barren"="Barren"))


dist_lu_v4$state <- revalue(dist_lu_v4$state, c("NCT of Delhi" = "Delhi"))


## Write the data
write_csv(dist_lu_v4, "lulc_all_v4.csv")


## Loading the processed dataset- run from this section
dist_lu_v4 <- read_csv('lulc_all_v4.csv')


## Converting to factors from column 1 to 6
dist_lu_v4[1:6] <- lapply(dist_lu_v3[1:6], as.factor)
dist_lu_v4[8] <- lapply(dist_lu_v3[8], as.factor)





## ANALYSIS =======================================================

## 2.0 Data Analysis ----


## 2.1 Land use composition (total area of each landuse) by State in 2020 ----
state_area <- dist_lu_v4 %>% select(year, state, district, landuse, area) %>% 
  filter(year == 2020) %>% group_by(state, landuse) %>% 
  dplyr::summarise(sum = sum(area)) %>% 
  ggplot(aes(x=landuse, y=sum, group = landuse, fill = landuse)) + geom_bar(stat = 'identity', color="black") + 
  scale_fill_manual(values=c("yellow", "#BF9000", "red","#00B050", "#56B4E9")) + 
  ylab("Area"~(km^2)) + xlab('') + labs(title = "A") +
  geom_text(aes(label = sum), size = 6, hjust = -0.1) + ylim(c(-0, 7200)) + 
  coord_flip() +
  facet_wrap(~state, ncol=3) + theme_linedraw() +
    theme(panel.spacing.x = unit(1, "lines"),
          strip.text.x = element_text(size = 22),
          axis.text.x = element_text(angle=45, size = 17, hjust = 1),
          axis.text.y = element_text(size = 17),
          axis.title = element_text(size = 22, face = "bold" ),
          plot.title = element_text(size=24, face = "bold"),
          panel.grid = element_line(color = '#ADADAD'),
          legend.position = 'none')
  


## 2.2 Proportion of landuse in 2020 by state ----
state_prop <- dist_lu_v4 %>% select(year, state, district, area_km, landuse, area) %>%
  filter(year == 2020 & state %in% c('Delhi', 'Haryana', 'Uttar Pradesh')) %>% group_by(state, landuse) %>% 
  dplyr::summarise(area_cat = sum(area)) %>% 
  
  ## this step creates sum of area of states and adds it into the above table
  left_join(dist_lu_v4 %>% filter(year == 2020 & state %in% c('Delhi', 'Haryana', 'Uttar Pradesh')) %>% 
              group_by(year, state) %>% 
              dplyr::summarise(statearea = sum(area)), by = c('state' = 'state')) %>% 
  
  ## Creates a new column with proportion
  mutate(prop = round((area_cat/statearea)*100, 2)) %>%
  
  ggplot(aes(x=landuse, y=prop, fill=landuse)) + geom_bar(stat = 'identity', color = 'black') + 
  scale_fill_manual(values=c("yellow", "#BF9000", "red","#00B050", "#56B4E9")) + 
  ylim(0,100) + ylab("(%)") + xlab('') + labs(title = "B") +
  facet_wrap(~state, ncol = 3) + geom_text(aes(label = prop), size = 6, hjust = -0.1) + 
  coord_flip() + theme_linedraw() +
  theme(panel.spacing.x = unit(1, "lines"),
        strip.text.x = element_text(size = 22),
        axis.text.x = element_text(angle=45, size = 17, hjust = 1),
        axis.text.y = element_text(size = 17),
        axis.title = element_text(size = 22, face = "bold" ),
        plot.title = element_text(size=24, face = "bold"),
        panel.grid = element_line(color = '#ADADAD'),
        legend.position = 'none')

## combine the plots
plot_grid(state_area, state_prop, labels = c('', ''), ncol = 1)

ggsave("state_area_prop_v4.png", height = 10, width = 14, dpi =300)



## ---------------------------------------------------------------------
## 2.2 Land use composition by Districts in 2020 ----

delhi <- dist_lu_v4 %>% select(year, state, district, landuse, area) %>% 
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

haryana <- dist_lu_v4 %>% select(year, state, district, landuse, area) %>% 
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

up <- dist_lu_v4 %>% select(year, state, district, landuse, area) %>% 
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
ggsave("states_area_prop_2020_v4.png", height = 15, width = 10,  units = c('in'), dpi= 300)



## ==============================================================================

## Proportion of land use composition in Districts

## Delhi
delhi_dist <- dist_lu_v4 %>% select(year, state, district, area_km, landuse, area) %>% 
  filter(year == 2020 & state %in% c("Delhi")) %>% group_by(state, district, landuse) %>% 
  dplyr::summarise(lcArea = sum(area)) %>% 
 
  ## this step creates sum of area of states and adds it into the above table
  left_join(dist_lu_v4 %>% filter(year == 2020 & state %in% c('Delhi')) %>% 
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
haryana_dist <- dist_lu_v4 %>% select(year, state, district, area_km, landuse, area) %>% 
  filter(year == 2020 & state %in% c("Haryana")) %>% group_by(state, district, landuse) %>% 
  dplyr::summarise(lcArea = sum(area)) %>% 
  
  ## this step creates sum of area of states and adds it into the above table
  left_join(dist_lu_v4 %>% filter(year == 2020 & state %in% c('Haryana')) %>% 
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
up_dist <- dist_lu_v4 %>% select(year, state, district, area_km, landuse, area) %>% 
  filter(year == 2020 & state %in% c("Uttar Pradesh")) %>% group_by(state, district, landuse) %>% 
  dplyr::summarise(lcArea = sum(area)) %>% 
  
  ## this step creates sum of area of states and adds it into the above table
  left_join(dist_lu_v4 %>% filter(year == 2020 & state %in% c('Uttar Pradesh')) %>% 
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
ggsave("districts_proportion_2.pdf", height = 15, width = 10,  units = c('in'), dpi= 300)









## Growth of land uses in all the districts yearly (change the land use to show the chart)
dist_lu_v4 %>% filter(landuse %in% c('Builtup')) %>% group_by(year, district) %>% 
  dplyr::summarise(builtup = max(area)) %>% 
  ggplot(aes(x = year, y = builtup, fill = district)) + geom_col() + 
  geom_text(aes(label = builtup), size = 3, hjust = 1) + ylab('Built') +
  coord_flip() + facet_wrap(~district, scale = 'free_x', ncol = 3) + 
  theme(plot.title = element_text(size=20, family = "Roboto Condensed"),
        axis.title.x = element_text(color="#993333", size=14, family = "Roboto Condensed"),
        axis.title.y = element_text(color="#993333", size=14, family = "Roboto Condensed"),
        axis.text.x = element_text(angle=45, hjust = 1),
        legend.position = "none")


dist_lu_v4 %>% group_by(year, district) %>% 
  dplyr::summarise(sum = sum(area)) %>% 
  ggplot(aes(x= district, y = sum, fill = district, group = 1)) + geom_col() + 
  coord_flip() +  facet_wrap(~year, ncol = 4)


yl <- expression(Area ~ (km^2))

## Total change in landuse from 1990-2000. Change the landuse value to see the result for each value.
dist_lu_v4 %>% filter(landuse %in% c('Agriculture')) %>% ggplot(aes(year, area, group = district, text= district)) + 
  geom_area(aes(fill = district), alpha = 0.8) + 
  geom_smooth(method = 'lm', se = FALSE, linetype = "dashed", color = "red", lwd = 1.3) +
  scale_fill_viridis(discrete = T) + ylab(yl) + xlab('Year') +
  facet_wrap(facets = ~reorder(district, -area), scale = 'free_y', ncol = 3) + 
  theme_linedraw() +
  theme(panel.spacing.x = unit(1, "lines"),
        strip.text.x = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(angle=45, size = 13, hjust = 1),
        axis.text.y = element_text(size = 13),
        axis.title = element_text(size = 16, face = "bold" ),
        panel.grid = element_line(color = '#ADADAD'),
        legend.position = 'none')  

## Saves the last plot
ggsave("all_districts_built.png", height = 13, width = 8,  units = c('in'), dpi= 250)



## Percent change in land use from 1990-2020. Change the landuse value to see result for each value
dist_lu_v4 %>% filter(landuse %in% c('Builtup') & district == "Meerut") %>% group_by(year, district) %>% dplyr::summarise(sum = sum(area)) %>% 
  mutate(change = ((sum - lag(sum))/lag(sum))*100) %>% 
  ggplot(aes(x=year, y=change, group = district, color = district)) + geom_line(show.legend = F) + 
  facet_wrap(~district, ncol = 3)


## Five Yearly Changes in Land Use with respect to districts -  Replace the land use for each graph
dist_lu_v4 %>% filter(landuse %in% c('Builtup')) %>% group_by(year, landuse, district) %>% 
  ggplot(aes(x = reorder(district, area), y = area, group = year, color = year)) + geom_line(size = 1) + 
  facet_wrap(~landuse, ncol = 1, scales = "free_y") +
  scale_color_viridis(discrete = T) +
  theme_linedraw() +
  theme(axis.text.x = element_text(angle=45, size = 15, hjust = 1, vjust = 1, face = "bold"),
        axis.text.y = element_text(size = 17),
        axis.title = element_text(size = 22, face = "bold" ),
        plot.title = element_text(size=24, face = "bold"),
        panel.grid = element_line(color = '#D1D1D1')
        )



## Five Yearly Changes in Land Use with respect to states and districts -  Replace the land use for each graph
dist_lu_v4 %>% filter(state %in% c("Delhi")) %>%  group_by(year, district) %>% 
  ggplot(aes(x = year, y = area, group = landuse, color = landuse)) + geom_line(size = 1) + 
  facet_wrap(~district, ncol = 3) +
  scale_color_manual(values=c("yellow", "#BF9000", "red","#00B050", "#56B4E9")) +
  theme_linedraw() +
  theme(panel.spacing.x = unit(1, "lines"),
        strip.text.x = element_text(size = 17),
        axis.text.x = element_text(angle=90, size = 12, hjust = 1),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 14, face = "bold" ),
        panel.grid = element_line(color = '#ADADAD'),
        legend.text = element_text(size = 15),
        legend.position = 'bottom') 


ggsave("yearly_change_delhi_v4.png", height = 10, width = 15, dpi= 300)

