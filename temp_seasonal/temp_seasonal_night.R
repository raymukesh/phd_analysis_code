library(tidyverse)
library(lubridate)
library(zoo)
library(readxl)
library(hrbrthemes)
library(jcolors)
library(ggthemes)
library(kableExtra)


temp_night <- read_xlsx("data/temp_night_seasonal.xlsx")


temp_night$date <- as.Date(temp_night$date, format = "%Y %B, %d")

temp_night$month <- as.yearmon(temp_night$date, "%b %Y") ## as.yearmon is from zoo package


##  Create the monthly sum of temp_nightfall in that particular year
temp_night_2 <- temp_night %>% group_by(month) %>% summarise(temp_night_val = mean(Night, na.rm = TRUE))
temp_night_2$temp_night_val <- round(temp_night_2$temp_night_val, 2)

## Seasonal total temp_nightfall
temp_night_3 <- temp_night %>% mutate(month_num = month(temp_night$date)) %>% 
  mutate(year = year(temp_night$date)) %>% 
  mutate(season = as.factor(ifelse(month_num <= 3, "JFM",
                                   ifelse(month_num <= 6, "AMJ",
                                          ifelse(month_num <= 9, "JAS",
                                                 ifelse(month_num <= 12, "OND"))))))

year_label <- seq(2003, 2020, 1)

temp_night_3$season <-  factor(temp_night_3$season, levels = c("JFM", "AMJ", "JAS", "OND"), ordered = T)

## Seasonal temp_nightfall Data
temp_night_3 %>% group_by(year, season) %>% 
  summarise(mean_sea = mean(Night, na.rm = TRUE)) %>% 
  ggplot(aes(x = year, y = mean_sea, group = season, color = season)) +
  geom_line(size = 1.5, alpha = 0.8) + geom_point(aes(color = season), size = 5, alpha = 0.8) + 
  scale_x_continuous(limits = c(2003, 2020), breaks = year_label) +
  
  xlab("Year") + ylab("Temperature (°C)") +
  ggtitle(label = "Seasonal Variation in Mean Night Temperature (2003-2020)") +  labs(color = "Season:") +
  scale_color_jcolors(palette = "pal6") + 
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(size = 16, angle = 45, face = 'bold'),
        axis.text.y = element_text(size = 17, face = 'bold'),
        axis.title.x =  element_text(margin = margin(t = 10), size = 18, face = 'bold'),
        axis.title.y = element_text(margin = margin(r = 10), size = 18, face = 'bold'),
        plot.title = element_text(size = 22, face = 'bold'),
        legend.text = element_text(size = 18, face = "bold"),
        legend.title = element_text(size = 18, face = "bold"),
        legend.position = "bottom")

ggsave("output/temp_night_seasonal.png", height = 7, width = 14, units = 'in', dpi= 300)  
