library(tidyverse)
library(readxl)
library(ggthemes)
library(webr)
library(tidytext)
library(cowplot)


setwd("D:/OneDrive - UTS/PhD_UTS/Stage 3/Master Data Sheets/R Calculations/phd_analysis_code/demographics")

data <- read_xlsx("urban_rural_pop.xlsx")

data <- data %>% group_by(name) %>% mutate(perc = round(pop/sum(pop)*100,1)) %>% 
  mutate(name = factor(name), type = factor(type), hsize = 30)


## DELHI
  data %>% filter(state == "Delhi") %>% group_by(type) %>% 
  ggplot(aes(x = hsize, y = perc, fill = type )) + 
  geom_bar(stat = "identity", width = 20) + 
  #labs(title = "Population Distribution (%)\n", caption = "Source: Census of India, 2011 | Chart: Author", fill = "Type") + 
  geom_text(aes(label = perc), size = 9, position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#83c5be", "#e07a5f")) +
  coord_polar(theta = "y") + 
  xlim(c(0, 40)) + 
  facet_wrap(~name, ncol = 4) +
  #theme_void() +
  theme_fivethirtyeight() +
  theme(panel.spacing.x = unit(2, "lines"),
        panel.spacing.y = unit(1, "lines"),
        strip.text.x = element_text(size = 28, face = "bold"), 
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size = 30, face = 'bold', hjust = 0.5),
        plot.caption = element_text(size = 20, hjust = 0.5),
        legend.text = element_text(size = 25),
        legend.title = element_text(size = 25, face = 'bold'),
        legend.position = 'none')

  
  ggsave("del_urban_rural.png", height=15, width=17, dpi=300)
  

  ## UTTAR PRADESH
  data %>% filter(state == "Uttar Pradesh") %>% group_by(type) %>% 
  ggplot(aes(x = hsize, y = perc, fill = type )) + 
  geom_bar(stat = "identity", width = 20) + 
  #labs(title = "Population Distribution (%)\n", caption = "Source: Census of India, 2011 | Chart: Author", fill = "Type") + 
  geom_text(aes(label = perc), size = 9, position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#83c5be", "#e07a5f")) +
  coord_polar(theta = "y") + 
  xlim(c(0, 40)) + 
  facet_wrap(~name, ncol = 4) +
  #theme_void() +
  theme_fivethirtyeight() +
  theme(panel.spacing.x = unit(2, "lines"),
        panel.spacing.y = unit(1, "lines"),
        strip.text.x = element_text(size = 28, face = "bold"), 
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size = 30, face = 'bold', hjust = 0.5),
        plot.caption = element_text(size = 20, hjust = 0.5),
        legend.text = element_text(size = 25),
        legend.title = element_text(size = 25, face = 'bold'),
        legend.position = 'none')


  ggsave("UP_urban_rural.png", height=6, width=17, dpi=300)
  
  ## HARYANA
  data %>% filter(state == "Haryana") %>% group_by(type) %>% 
  ggplot(aes(x = hsize, y = perc, fill = type )) + 
  geom_bar(stat = "identity", width = 20) + 
  labs(fill = "Type") + 
  geom_text(aes(label = perc), size = 9, position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#83c5be", "#e07a5f")) +
  coord_polar(theta = "y") + 
  xlim(c(0, 40)) + 
  facet_wrap(~name, ncol = 4) +
  #theme_void() +
  theme_fivethirtyeight() +
  theme(panel.spacing.x = unit(2, "lines"),
        panel.spacing.y = unit(1, "lines"),
        strip.text.x = element_text(size = 28, face = "bold"), 
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size = 30, face = 'bold', hjust = 0.5),
        plot.caption = element_text(size = 20, hjust = 0.5),
        legend.text = element_text(size = 25),
        legend.title = element_text(size = 25, face = 'bold'),
        legend.position = 'none')

  ggsave("HAR_urban_rural.png", height=10, width=17, dpi=300)

plot_grid(delhi, up, ha, labels = "AUTO", ncol = 1)

ggsave("com_urban_rural.png", height=18, width=15, dpi=300)
  



PieDonut(data = data, aes(name, type, count = pop), title = "Chart")
