library(tidyverse)
library(readxl)
library(ggthemes)
library(ggpubr)
install.packages("ggpubr")

data <- read_xlsx("lst_ndvi_corr.xlsx")

ggplot(data = data, aes(x= LST, y=NDVI)) + 
  geom_point(size = 5, alpha = 0.4, color = 'red') + geom_smooth(method = lm, lwd = 2) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(size = 15, face = 'bold'),
        axis.text.y = element_text(size = 15, face = 'bold'),
        axis.title.x = element_text(size = 20, margin = margin(t = 10), face = 'bold'),
        axis.title.y = element_text(size = 20, margin = margin(r = 10), face = 'bold'))


ggsave("ndv_lst_profile.png", height = 10, width = 10, dpi = 300)

ggscatter(data, x = "LST", y = "NDVI", add = "reg.line",
          color = "red",  size = 4, alpha = 0.5,
          conf.int = TRUE, col="black", cor.coef = TRUE,
          cor.coeff.args = list(method = "pearson", label.x = 40, label.sep = "\n"),
          add.params = list(color = "blue", fill = "lightgray"),
          cor.method = "pearson", xlab = "LST (°C) ", ylab = "NDVI") + 
  theme_clean() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14, margin = margin(t = 10), face = 'bold'),
        axis.title.y = element_text(size = 14, margin = margin(r = 10), face = 'bold'))
  




ggsave("ndv_lst_profile.png", height = 8, width = 10, dpi = 300)
