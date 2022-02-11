library(tidyverse)
library(lubridate)
library(imputeTS)

day <- Daytime_LST_2
night <- NIGHT_LST_2

day$date <- dmy(day$date)
night$date <- dmy(night$date)
night$night_lst <- round(night$night_lst, 2)


comb <- left_join(day, night, by = "date")
comb <- na_interpolation(comb, option = 'linear')

comb %>% pivot_longer(2:3, names_to = "time", values_to = "temp") %>% 
  ggplot(aes(x = date, y = temp, group = time, color = time)) + geom_line()

which(!complete.cases(comb))
