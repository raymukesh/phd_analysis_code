library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(viridis)
library(hrbrthemes)
library(patchwork)
library(cowplot)
library(ggthemes)
library(ggridges)



getwd()


## 1.0 Data Preprocessing ----
## Data Loading
df_raw <- list.files(path = getwd(), full.names = T) %>% lapply(read_csv) %>% bind_rows()



# Add new columns of water, vegetation, builtup, agriculture, barren
df_raw$Water <- format(round((df_raw$HISTO_1*3600)/1e6, 2), nsmall = 2)
df_raw$Vegetation <- format(round((df_raw$HISTO_2*3600)/1e6, 2), nsmall = 2)
df_raw$Builtup <- format(round((df_raw$HISTO_3*3600)/1e6, 2), nsmall = 2)
df_raw$Agriculture <- format(round((df_raw$HISTO_4*3600)/1e6, 2), nsmall = 2)
df_raw$Barren <- format(round((df_raw$HISTO_5*3600)/1e6, 2), nsmall = 2)


## Remove HISTO columns from 8 to 12
df_raw <- df_raw[, -c(4:8)]

## Converting to factors from colum 1 to 6
df_raw[1:2] <- lapply(df_raw[1:2], as.factor)

## Converting to longer format
space_time_v4 <- df_raw %>% pivot_longer(cols = 4:8, names_to = 'landuse', values_to = 'area')

space_time_v4[5] <- lapply(space_time_v4[5] , as.numeric) ## convert area to numeric

## Creating an ordered factor for distane
space_time_v4$dist <- factor(space_time_v4$dist,
                levels = c("0-10", "10-20", "20-30", "30-40", "40-50", 
                           "50-60", "60-70", "70-80", "80-90", "90-100", "100-110"))


space_time_v4 <- space_time_v4 %>% dplyr::rename("lc_area" = "area", "zone_area" = "area_km")




## Write the data
write_csv(space_time_v4, 
          file = "D:/OneDrive - UTS/PhD_UTS/Stage 3/Master Data Sheets/R Calculations/phd_analysis_code/Space Time/space_time_v4.csv")



## Percent Areas of Land Use Over the Distance (Zones)
space_time_v4 %>% dplyr::mutate(percent = (lc_area/zone_area)*100) %>% 
  filter(!landuse %in% c("Water")) %>% 
  
  ggplot(aes(x=dist, y=percent, group = year, color = year)) + geom_line(size = 1, alpha = 0.8) +
  ##ggtitle("Percent Variation of Builtup Over Distance") +
  ylab('(%)') + xlab('Distance from CBD (km)') + guides(color = guide_legend(title = "Year")) +
  facet_wrap(~landuse, ncol = 2) +
  theme_linedraw() +
  theme(panel.spacing.x = unit(1, "lines"),
        panel.spacing.y = unit(1, "lines"),
        strip.text.x = element_text(size = 17, face = 'bold'),
        axis.title.y = element_text(size = 17, face = "bold" ),
        axis.title.x = element_text(size=18, face = "bold"),
        axis.text.y = element_text(size = 15),
        axis.text.x = element_text(angle=45, size = 15, hjust = 1),
        panel.grid = element_line(color = '#343a40'),
        plot.title = element_text(size=22, face = "bold"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 17, face = "bold"),
        legend.position = 'right')

ggsave("lc_dist_all_v4.png", height =10, width = 12, dpi= 300)









