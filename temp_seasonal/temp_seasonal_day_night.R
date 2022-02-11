library(tidyverse)
library(lubridate)
library(zoo)
library(readxl)
library(hrbrthemes)
library(jcolors)
library(ggthemes)
library(kableExtra)



temp_night <- read_xlsx("data/day_night_lst.xlsx", sheet = 'combined')


temp_night$date <- as.Date(temp_night$date, format = "%Y %B, %d")

temp_night$month <- as.yearmon(temp_night$date, "%b %Y") ## as.yearmon is from zoo package


##  Create the monthly sum of temp_nightfall in that particular year
temp_night_2 <- temp_night %>% group_by(month) %>% summarise(night_mean = mean(Night, na.rm = TRUE),
                                                             day_mean = mean(Day, na.rm = TRUE))
temp_night_2$night_mean <- round(temp_night_2$night_mean, 2)
temp_night_2$day_mean <- round(temp_night_2$day_mean, 2)


## Seasonal total temp_nightfall
temp_night_3 <- temp_night %>% mutate(month_num = month(temp_night$date)) %>% 
  mutate(year = year(temp_night$date)) %>% 
  mutate(season = as.factor(ifelse(month_num <= 3, "Jan-Feb-Mar",
                                   ifelse(month_num <= 6, "Apr-May-Jun",
                                          ifelse(month_num <= 9, "Jul-Aug-Sep",
                                                 ifelse(month_num <= 12, "Oct-Nov-Dec"))))))

year_label <- seq(2003, 2020, 1)

temp_night_3$season <-  factor(temp_night_3$season, levels = c("Jan-Feb-Mar", "Apr-May-Jun", "Jul-Aug-Sep", "Oct-Nov-Dec"), ordered = T)

## Seasonal temp_nightfall Data
temp_night_3 %>% group_by(year, season) %>% 
  summarise(Night = mean(Night, na.rm = TRUE), Day = mean(Day, na.rm = TRUE)) %>% 
  pivot_longer(3:4, names_to = 'time', values_to = 'value') %>% 
  
  ggplot(aes(x = year, y = value, group = time, color = time)) +
  geom_line(size = 1.5) + geom_point(aes(color = time), size = 5) + 
  scale_x_continuous(limits = c(2003, 2020), breaks = year_label) + 
  facet_wrap(~season, ncol = 2) +
  
  xlab("Year") + ylab("Temperature (°C)") +
  ggtitle(label = "Seasonal Variation in Mean Day and Night Temperature (2003-2020)") +  labs(color = "Season:") +
  scale_color_jcolors(palette = "pal6") + 
  theme_fivethirtyeight() +
  theme(strip.text = element_text(size = 17, face = 'bold'),
        axis.text.x = element_text(size = 16, angle = 90, face = 'bold'),
        axis.text.y = element_text(size = 17, face = 'bold'),
        axis.title.x =  element_text(margin = margin(t = 10), size = 18, face = 'bold'),
        axis.title.y = element_text(margin = margin(r = 10), size = 18, face = 'bold'),
        plot.title = element_text(size = 22, face = 'bold'),
        legend.text = element_text(size = 18, face = "bold"),
        legend.title = element_text(size = 18, face = "bold"),
        legend.position = "bottom")

ggsave("output/temp_night_day_seasonal.png", height = 7, width = 14, units = 'in', dpi= 300)  
