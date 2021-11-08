## NDVI CALCULATION FOR DELHI AND SURROUNDING DISTRICTS FOR 2000 - 2021

## Load the packages
library(tidyverse)
library(lubridate)
library(jcolors)
library(ggthemes)

## Read the data
df <- read.csv(file = "data/modis_ndvi_dist_20_21.csv", stringsAsFactors = TRUE)

### DATA CLEANING ----
## Remove dot from district names using "gsub"
names(df) <- gsub("\\.", " ", names(df))

## Rearrange the table in long format
df <- df %>% pivot_longer(2:19, names_to = "Districts", values_to = "ndvi")


## Create the date column
df$date <- ymd(df$date) 
df$year <- as.factor(year(df$date))
df$month <- as.factor(month(df$date, label = TRUE))

## Calculating the monthly mean NDVI for the given years
df %>% filter(year %in% c("2001", "2010", "2020")) %>% 
  group_by(Districts, year, month) %>% 
  summarise(mean = mean(ndvi), n = n()) %>% 
  
  ggplot(aes(x = month, y = mean, group = year, color =  year)) + 
  geom_line(size = 0.7, alpha = 1) + 
  geom_point(aes(color = year), size = 2, alpha = 1) + scale_color_jcolors(palette = "pal3") +
  facet_wrap(~reorder(Districts, -mean), ncol = 3) + xlab("Months") + ylab("Mean NDVI") +
  ggtitle(label = "District Wise Annual Distribution of Mean NDVI Values \n(2001, 2010, 2020)") +
  labs(color = "Years:") +
  theme_fivethirtyeight() +
  theme(panel.spacing.x = unit(0.5, "lines"),
        panel.spacing.y = unit(0.5, "lines"),
        strip.text.x = element_text(size = 11, face = 'bold'),
        axis.title.y = element_text(margin = margin(r = 10), size=12, face = "bold"),
        axis.title.x = element_text(margin = margin(t = 5), size=12, face = "bold"),
        axis.text.y = element_text(size = 10, color = "black"),
        axis.text.x = element_text(angle=90, size = 10, vjust = 1, hjust = 1, color = 'black'),
       # panel.grid = element_line(color = '#343a40'),
        plot.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 13, face = "bold"),
        legend.position = 'bottom')

ggsave("output/yearly_ndvi_all_dist.png", height=11, width=8, dpi=300)

## PLOTTING THE TIME SERIES DATA





