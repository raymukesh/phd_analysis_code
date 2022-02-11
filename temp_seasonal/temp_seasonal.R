library(tidyverse)
library(lubridate)
library(zoo)
library(readxl)
library(hrbrthemes)
library(jcolors)
library(ggthemes)
library(kableExtra)


temp_day <- read_xlsx("data/temp_day_seasonal.xlsx")


temp_day$date <- as.date(temp_day$date, format = "%Y %B, %d")

temp_day$month <- as.yearmon(temp_day$date, "%b %Y") ## as.yearmon is from zoo package


##  Create the monthly sum of temp_dayfall in that particular year
temp_day_2 <- temp_day %>% group_by(month) %>% summarise(temp_day_val = mean(Day, na.rm = TRUE))
temp_day_2$temp_day_val <- round(temp_day_2$temp_day_val, 2)

## Seasonal total temp_dayfall
temp_day_3 <- temp_day %>% mutate(month_num = month(temp_day$date)) %>% 
  mutate(year = year(temp_day$date)) %>% 
  mutate(season = as.factor(ifelse(month_num <= 3, "JFM",
                                   ifelse(month_num <= 6, "AMJ",
                                          ifelse(month_num <= 9, "JAS",
                                                 ifelse(month_num <= 12, "OND"))))))

year_lable <- seq(2003, 2020, 1)

temp_day_3$season <-  factor(temp_day_3$season, levels = c("JFM", "AMJ", "JAS", "OND"), ordered = T)

## Seasonal temp_dayfall Data
temp_day_3 %>% group_by(year, season) %>% 
  summarise(mean_sea = mean(Day, na.rm = TRUE)) %>% 
  ggplot(aes(x = year, y = mean_sea, group = season, color = season)) +
  geom_line(size = 1.5, alpha = 0.8) + geom_point(aes(color = season), size = 5, alpha = 0.8) + 
  scale_x_continuous(limits = c(2003, 2020), breaks = c(2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2020)) +
  
  xlab("Year") + ylab("Temperature (°C)") +
  ggtitle(label = "Seasonal Variation in Mean Day Temperature (2003-2020)") +  labs(color = "Season:") +
  scale_color_jcolors(palette = "pal6") + 
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(size = 19, angle = 0, face = 'bold'),
        axis.text.y = element_text(size = 17, face = 'bold'),
        axis.title.x =  element_text(margin = margin(t = 10), size = 18, face = 'bold'),
        axis.title.y = element_text(margin = margin(r = 10), size = 18, face = 'bold'),
        plot.title = element_text(size = 22, face = 'bold'),
        legend.text = element_text(size = 18, face = "bold"),
        legend.title = element_text(size = 18, face = "bold"),
        legend.position = "bottom")

ggsave("output/temp_day_seasonal.png", height = 7, width = 14, units = 'in', dpi= 300)  
