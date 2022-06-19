library(tidyverse)
library(lubridate)
library(zoo)
library(readxl)
library(hrbrthemes)
library(jcolors)
library(ggthemes)
library(kableExtra)
library(ggpubr)


## Quick open files in excel function

show_in_excel <- function(.data) {
  tmp <- paste0(tempfile(), ".csv")
  write.csv(.data, tmp, row.names = FALSE)
  fs::file_show(path = tmp)
}

## Update 4 (13/02/2022 )- The Rainfall and ET Data is upto date - ET data changed to version 3 (version 2 values are way higher thus rejected)
## Direct scaled values of MODIS ET is used now.

## Read the data NDVI and Evapotranspiration (ET)
ndvi <- read_xlsx("data/NDVI_20_years.xlsx")
ET2 <- read_xlsx("data/ET_20_years_v3.xlsx") ## Use version 2 for mean of months and version 3 for sum of months
rain <- read_xlsx("data/chirps_rainfall_20_21.xlsx")

# Convert date column into date
ndvi$Date <- as.Date(ndvi$Date, format = "%B %d, %Y")
ET2$Date <- as.Date(ET2$Date, format = "%B %d, %Y")
rain$Date <- as.Date(rain$Date, format = "%B %d, %Y")


# create month column with year - e.g. Mar 2000
ndvi$month <- as.yearmon(ndvi$Date, "%b %Y")
ET2$month <- as.yearmon(ET2$Date, "%b %Y")
rain$month <- as.yearmon(rain$Date, "%b %Y") ## as.yearmon is from zoo package

# Create monthly mean of NDVI and ET
ndvi_2 <- ndvi %>% group_by(month) %>% summarise(NDVI_val = mean(NDVI))
ndvi_2$NDVI_val <- round(ndvi_2$NDVI_val, 2)


ET2_2 <- ET2 %>% group_by(month) %>% summarise(ET_val = mean(ET_val))
ET2_2$ET_val <- round(ET2_2$ET_val, 2)

##  Create the monthly sum of rainfall in that particular year
rain_2 <- rain %>% group_by(month) %>% summarise(rain_val = sum(rain_mm))
rain_2$rain_val <- round(rain_2$rain_val, 2)

# Combine ET and NDVI by month
combined <- ndvi_2 %>% left_join(ET2_2, by = "month")
combined2 <- combined %>% left_join(rain_2, by = "month")

#Convert month to date
combined2$month <- as.Date(combined2$month, format = "%B %Y")


# Extract month name using lubridate
combined2$month_name <- month(combined2$month, label = T)
combined2$year <- year(combined2$month)


combined2 %>% filter(!year %in% c("2000")) %>% group_by(month_name) %>% summarise(sum = mean(ET_val))



## Plot Annual Monthly Variability Between Evapotranspiration, NDVI and Precipitation
combined2 %>% filter(!year %in% c("2000")) %>% 
  group_by(year, month_name) %>% 
  summarise(ET_mean = mean(ET_val), NDVI_mean = mean(NDVI_val), rain_mean = (mean(rain_val))/10) %>%
  
  ggplot(aes(x=month_name)) + 
  geom_bar(aes(y = ET_mean), stat = "identity", fill = "#4eb3d3", alpha = 0.6) +
  geom_line(aes(y = NDVI_mean*150, group = 1), color = "red", size = 1) + geom_point(aes(y = NDVI_mean*150), color = "red", size =3) +
  scale_y_continuous(sec.axis = sec_axis(~./150, name = "NDVI", breaks = derive())) + ## Adds secondary axis on the right
  geom_line(aes(y = rain_mean, group = 1), color = "#08589e", size = 1) + geom_point(aes(y = rain_mean), color = "#08589e", size = 3) + 
  labs(x = "Month", y = "Evapotranspiration (mm) and Precipitation (x10 mm)") + 
  ggtitle(label = "Comparison of Mean Monthly Variation of NDVI, Evapotranspiration and Precipitation (2001-2020)") +
  #geom_line(aes(x= month_name, y = NDVI_mean, group = 1), color = "red", size= 1) +
  facet_wrap(~year) + theme_fivethirtyeight() +
  theme(panel.spacing.x = unit(1, "lines"),
        strip.text.x = element_text(size = 15,  face = 'bold'),
        axis.text.x = element_text(size = 13, angle = 90, face = 'bold'),
        axis.text.y = element_text(size = 14, face = 'bold', color = "#31a354"),
        axis.line.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(colour = "red"),
        axis.ticks.y.right = element_line(color = "red"),
        axis.line.y.left = element_line(color = "#08589e"),
        axis.text.y.left = element_text(colour = "#08589e"),
        axis.title.x =  element_text(margin = margin(t = 10), size = 18, face = 'bold'),
        axis.title.y = element_text(margin = margin(r = 10), size = 18, face = 'bold', color = "#08589e"),
        axis.title.y.right = element_text(margin = margin(r = 10), size = 18, face = 'bold', color = "red", vjust = 1),
        plot.title = element_text(size = 22, face = 'bold', color = "black"),
        legend.position = "bottom")
        
  
ggsave("output/ndvi_v_et_v3.png", height = 10, width = 18, units = 'in', dpi= 300)  

dev.off()

## Boxplot

ndvi %>% mutate(year = as.factor(year(ndvi$Date))) %>% 
  ggplot(aes(x = year, y = NDVI)) + geom_boxplot()

ET$year <- as.factor(year(ET$Date))
ET %>% ggplot(aes(x = year, y = ET)) + 
  geom_boxplot() + geom_jitter(shape=16, position=position_jitter(0.2), alpha = 0.3) +
  theme_minimal()


## Statistical Analysis
attach(combined2)      

cor.test(NDVI_val, rain_val)
plot(NDVI_val, rain_val)
summary(ET$ET)



## =================================== RAINFALL ============================================
## Seasonal total rainfall
rain_3 <- rain %>% mutate(month_num = month(rain$Date)) %>% 
  mutate(year = year(rain$Date)) %>% 
  mutate(season = as.factor(ifelse(month_num <= 3, "JFM",
                              ifelse(month_num <= 6, "AMJ",
                                ifelse(month_num <= 9, "JAS",
                                 ifelse(month_num <= 12, "OND"))))))


## Long Term Monthly Mean Rainfall in the site
combined2 %>% filter(!year %in% c("2000")) %>% #group_by(year, month_name) %>% summarise(rai_sum = sum(rain_val)) %>% 
  group_by(month_name) %>% summarise(Ymean = mean(rain_val)) %>% 
  ggplot(aes(x=month_name, y = Ymean)) + geom_bar(stat = 'identity', fill = "#3182bd") +
  geom_text(aes(label = round(Ymean)), vjust = -0.5, colour = "black", size = 4.5) + ylim(c(0,230)) +
  xlab("Month") + ylab("Rainfall (mm)") +
  ggtitle(label = "Long Term Monthly Mean Precipitation (2001-2020)") +
  theme_fivethirtyeight() +
  theme(axis.title.y = element_text(margin = margin(r = 10), size=16, face = "bold"),
        axis.title.x = element_text(margin = margin(t = 10), size=16, face = "bold"),
        axis.text.y = element_text(size = 13, color = "black", face = 'bold'),
        axis.text.x = element_text(angle = 0, size = 13, color = 'black', face = 'bold'),
        plot.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 13, face = "bold"),
        legend.position = 'none')

ggsave("output/rainfall_LT_month_mean.png", height=4, width=10, dpi=300)

year_lable <- seq(2001, 2020, 1)

## Total Rainfall by Year (2001-2020)
rain_3 %>% filter(!year %in% c("2000")) %>% group_by(year) %>% summarise(year_sum = sum(rain_mm)) %>%
  ggplot(aes(x=year, y=year_sum)) + geom_bar(stat = 'identity', fill = "#3182bd") +
  geom_text(aes(label = round(year_sum)), vjust = 1.5, colour = "white", size = 5) +
  xlab("Year") + ylab("Precipitation (mm)") +
  ggtitle(label = "Total Yearly Precipitation (2001-2020)") +
  scale_x_continuous(labels = year_lable, breaks = year_lable) +
  theme_fivethirtyeight() +
  theme(axis.title.y = element_text(margin = margin(r = 10), size=18, face = "bold"),
        axis.title.x = element_text(margin = margin(t = 10), size=18, face = "bold"),
        axis.text.y = element_text(size = 13, color = "black", face = 'bold'),
        axis.text.x = element_text(angle = 90, size = 14, color = 'black', face = 'bold', hjust = 0.5, vjust = 0.5),
        plot.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 13, face = "bold"),
        legend.position = 'bottom')

ggsave("output/rainfall_yearly.png", height=6, width=13, dpi=300)




## Time Series of Rainfall (2001-2020)
rain_3 %>% filter(!year %in% c("2000")) %>% 
  ggplot(aes(x=Date, y=rain_mm, group = 1)) + geom_line(color = "#2b8cbe") + geom_smooth(method = "loess") +
  xlab("Year") + ylab("Precipitation (mm)") +
  ggtitle(label = "Time Series of Five Day Composite Precipitation (2001-2020)") +
  scale_x_date(date_breaks = "1 year", date_minor_breaks = "6 month", date_labels = "%Y") +
  theme_fivethirtyeight() +
  theme(axis.title.y = element_text(margin = margin(r = 10), size=16, face = "bold"),
        axis.title.x = element_text(margin = margin(t = 10), size=16, face = "bold"),
        axis.text.y = element_text(size = 13, color = "black", face = 'bold'),
        axis.text.x = element_text(angle = 90, size = 13, color = 'black', face = 'bold'),
        plot.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 13, face = "bold"),
        legend.position = 'bottom')

ggsave("output/rainfall_timeseries.png", height=6, width=13, dpi=300)



rain_3$season <-  factor(rain_3$season, levels = c("JFM", "AMJ", "JAS", "OND"), ordered = T)

## Seasonal Rainfall Data
rain_3 %>% filter(!year %in% c("2000")) %>% group_by(year, season) %>% 
  summarise(sum_sea = sum(rain_mm)) %>% 
  ggplot(aes(x = year, y = sum_sea, group = season, color = season)) +
  geom_line(size = 1.5, alpha = 0.8) + geom_point(aes(color = season), size = 5, alpha = 0.8) + 
  scale_x_continuous(limits = c(2001, 2020), breaks = c(2001, 2005, 2010, 2015, 2020)) +
 
  xlab("Year") + ylab("Rainfall (mm)") +
  ggtitle(label = "Seasonal Rainfall (2001-2020)") +  labs(color = "Season:") +
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
  
ggsave("output/rain_seasonal.png", height = 7, width = 14, units = 'in', dpi= 300)  

write.csv(rain_3, file = "data/rainfall_excel.csv")

combined2 %>% group_by(year) %>% summarise(sum = sum(rain_val)) %>%  
  ggplot(aes(x = year, y = sum, group = 1)) + geom_line()


### ================================================== NDVI ==========================================
## Seasonal NDVI
ndvi_3 <- ndvi %>% mutate(month_num = month(ndvi$Date)) %>% 
  mutate(year = year(ndvi$Date)) %>% 
  mutate(season = as.factor(ifelse(month_num <= 3, "JFM",
                                   ifelse(month_num <= 6, "AMJ",
                                          ifelse(month_num <= 9, "JAS",
                                                 ifelse(month_num <= 12, "OND"))))))


ndvi_3$season <-  factor(ndvi_3$season, levels = c("JFM", "AMJ", "JAS", "OND"), ordered = T)

# The palette with black:
cbbPalette <- c("#009E73", "#0072B2", "#D55E00", "#CC79A7")

ndvi_3 %>% group_by(year, season) %>% 
  summarise(mean_sea = mean(NDVI)) %>% 
  ggplot(aes(x = year, y = mean_sea, group = season, color = season)) + 
  geom_line(size = 1.5, alpha = 0.8) + geom_point(aes(color = season), size = 3, alpha = 0.8) + 
  scale_color_jcolors(palette = "pal6") + scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0.2, 0.6)) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 13, angle = 0, face = 'bold'),
        axis.text.y = element_text(size = 14, face = 'bold'),
        axis.title.x =  element_text(margin = margin(t = 10), size = 18, face = 'bold'),
        axis.title.y = element_text(margin = margin(r = 10), size = 18, face = 'bold'),
        plot.title = element_text(size = 22, face = 'bold', color = "black"),
        legend.position = "bottom")
  
ggsave("ndvi_seasonal.png", height = 5, width = 5, units = 'in', dpi= 300)  



## Days of the year
ndvi$day = yday(ndvi$Date)
ndvi$year <- year(ndvi$Date)

ndvi %>% filter(!year %in% c("2000")) %>% group_by(day) %>% summarise(mean= mean(NDVI)) %>% show_in_excel
#%>%   ggplot(aes(x=day, y = mean, group =1)) + geom_line()



## ======================================== Evapotranspiration ==============================
## Monthly mean of 20 years
combined2 %>% filter(!year %in% c("2000")) %>% group_by(month_name) %>% summarise(mean= mean(ET_val, na.rm = T)) %>% 
  ggplot(aes(x=month_name, y = mean)) + geom_bar(stat = 'identity', fill = "#3182bd") +
  geom_text(aes(label = round(mean,1), size = 5), vjust = -0.5, color = 'black') + ylim(c(0,50)) +
  xlab("Month") + ylab("Evapotranspiration (mm)") +
  ggtitle(label = "Long Term Monthly Mean Evapotranspiration (2001-2020)") +
  theme_fivethirtyeight() +
  theme(axis.title.y = element_text(margin = margin(r = 10), size=16, face = "bold"),
        axis.title.x = element_text(margin = margin(t = 10), size=16, face = "bold"),
        axis.text.y = element_text(size = 13, color = "black", face = 'bold'),
        axis.text.x = element_text(angle = 0, size = 13, color = 'black', face = 'bold'),
        plot.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 13, face = "bold"),
        legend.position = 'none')

ggsave("output/evapotras_LT_month_mean.png", height=4, width=10, dpi=300)












## Correlation Test of ET, NDVI, Rainfall
ggscatter(combined2, x = "ET_val", y = "rain_val", add = "reg.line",
          color = "red",  size = 4, alpha = 0.5,
          conf.int = TRUE, col="black", cor.coef = TRUE,
          cor.coeff.args = list(method = "pearson", label.x.npc = 'left', label.sep = "\n"),
          add.params = list(color = "blue", fill = "lightgray"),
          cor.method = "pearson", xlab = "Evapotranspiration (mm)", ylab = "Precipitation (mm)") + 
  theme_clean() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14, margin = margin(t = 10), face = 'bold'),
        axis.title.y = element_text(size = 14, margin = margin(r = 10), face = 'bold'))

