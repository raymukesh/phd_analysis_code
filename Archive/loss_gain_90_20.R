library(dplyr)
library(data.table)
library(readr)
library(tidyr)
library(plyr)
library(viridis)
library(plotly)
library(hrbrthemes)
library(showtext)
library(ggplot2)


font_add_google("Roboto Condensed", "roboto")


##========================

loss_gain <- read.csv("D:/OneDrive - UTS/PhD_UTS/Stage 3/Master Data Sheets/R Calculations/90_20_gain_loss_all.csv", stringsAsFactors = T)

str(loss_gain)

loss_gain %>% pivot_longer(8:9, names_to = 'change', values_to = 'area') %>% group_by(state, landuse, change) %>% 
  dplyr::summarise(sum = sum(area)) %>% ggplot(aes(x=state, y=sum, fill = change)) + geom_col() + 
  geom_text(aes(label = sum), size = 3, hjust = 0) + coord_flip() + 
  labs(title = "COVID 19 - Positivity Rate of Indian States") +
  facet_wrap(~landuse, ncol = 3)


