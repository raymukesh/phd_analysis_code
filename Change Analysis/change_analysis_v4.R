## This code creates transition of land use among all the classes for all the years of data
## This utilises OpenLand package

## install.packages("OpenLand")
library(OpenLand)
library(raster)
library(tidyverse)

list.files("D:/OneDrive - UTS/PhD_UTS/Stage 3/Analysis/Prediction/ForTerrset60_v4", pattern = ".tif")

## Read all the classified raster files
year1990 <- raster("D:/OneDrive - UTS/PhD_UTS/Stage 3/Analysis/Prediction/ForTerrset60_v4/final_1990_v4.tif")
year1995 <- raster("D:/OneDrive - UTS/PhD_UTS/Stage 3/Analysis/Prediction/ForTerrset60_v4/final_1995_v4.tif")
year2000 <- raster("D:/OneDrive - UTS/PhD_UTS/Stage 3/Analysis/Prediction/ForTerrset60_v4/final_2000_v4.tif")
year2005 <- raster("D:/OneDrive - UTS/PhD_UTS/Stage 3/Analysis/Prediction/ForTerrset60_v4/final_2005_v4.tif")
year2010 <- raster("D:/OneDrive - UTS/PhD_UTS/Stage 3/Analysis/Prediction/ForTerrset60_v4/final_2010_v4.tif")
year2015 <- raster("D:/OneDrive - UTS/PhD_UTS/Stage 3/Analysis/Prediction/ForTerrset60_v4/final_2015_v4.tif")
year2020 <- raster("D:/OneDrive - UTS/PhD_UTS/Stage 3/Analysis/Prediction/ForTerrset60_v4/final_2020_v4.tif")





## Stack all the raster files as raster brick
stack_all <- stack(year1990, year1995, year2000, year2005, year2010, year2015, year2020)


## Create a contingency table - this table has all the transitions among all the years
SL_1990_2020 <- contingencyTable(input_raster = stack_all, pixelresolution = 60)

## check the table
head(SL_1990_2020)

names(SL_1990_2020)

all_years_contingency_v4 <- SL_1990_2020$lulc_Multistep



## change the legend names and colors
SL_1990_2020$tb_legend$color <- c("#00bbf9", "#38b000", "#f94144", "#ffd819", "#f8961e")
SL_1990_2020$tb_legend$categoryName <- factor(c("Water", "Vegetation", "Builtup", "Agriculture", "Barren"),
                                              levels = c("Water", "Vegetation", "Builtup", "Agriculture", "Barren"))


## Intensity Analysis - 

## Intensity of changes between builtup and agriculture
testSL <- intensityAnalysis(dataset = SL_1990_2020,
                            category_n = "Builtup", category_m = "Agriculture")


## Intensity of changes between agriculture and built
testSL_agri_built <- intensityAnalysis(dataset = SL_1990_2020,
                                       category_n = "Agriculture", category_m = "Builtup")

names(testSL_agri_built)


all_transition_v4 <- testSL$lulc_table

write.csv(all_transition_v4, file = "all_transitions_v4.csv", row.names = F)


plot(testSL$interval_lvl,
     labels = c(leftlabel = "Interval Change Area (%)",
                rightlabel = "Annual Change Area (%)"),
     marginplot = c(-8, 0), labs = c("Changes", "Uniform Rate"),
     leg_curv = c(x = 2/10, y = 3/10))


plot(testSL$category_lvlGain,
     labels = c(leftlabel = bquote("Gain Area (" ~ km^2 ~ ")"),
                rightlabel = "Intensity Gain (%)"),
     marginplot = c(.3, .3), labs = c("Categories", "Uniform Rate"),
     leg_curv = c(x = 5/10, y = 5/10))


plot(testSL$category_lvlLoss,
     labels = c(leftlabel = bquote("Loss Area (" ~ km^2 ~ ")"),
                rightlabel = "Loss Intensity (%)"),
     marginplot = c(.3, .3), labs = c("Categories", "Uniform Rate"), 
     leg_curv = c(x = 5/10, y = 5/10))




plot(testSL$transition_lvlGain_n,
     labels = c(leftlabel = bquote("Gain of Builtup (" ~ km^2 ~ ")"),
                rightlabel = "Intensity Gain of Builtup (%)"),
     marginplot = c(.3, .3), labs = c("Categories", "Uniform Rate"), 
     leg_curv = c(x = 5/10, y = 5/10))

plot(testSL$transition_lvlLoss_m,
     labels = c(leftlabel = bquote("Loss of Agriculture (" ~ km^2 ~ ")"),
                rightlabel = "Intensity Loss of Agriculture (%)"),
     marginplot = c(.3, .3), labs = c("Categories", "Uniform Rate"), 
     leg_curv = c(x = 5/10, y = 5/10))



netgrossplot(dataset = SL_1990_2020$lulc_Multistep,
             legendtable = SL_1990_2020$tb_legend,
             xlab = "LUC Category",
             ylab = bquote("Area (" ~ km^2 ~ ")"),
             changesLabel = c(GC = "Gross changes", NG = "Net Gain", NL = "Net Loss"),
             color = c(GC = "gray70", NG = "#006400", NL = "#EE2C2C"))




## Chord diagram showing transiton between land uses
chordDiagramLand(dataset = SL_1990_2020$lulc_Onestep,
                 legendtable = SL_1990_2020$tb_legend)


## Sankey diagram for all the years
sankeyLand(dataset = SL_1990_2020$lulc_Multistep,
           legendtable = SL_1990_2020$tb_legend)



## Sankey diagram for the years 1990-2020
sankeyLand(dataset = SL_1990_2020$lulc_Onestep,
           legendtable = SL_1990_2020$tb_legend)

##
barplotLand(dataset = SL_1990_2020$lulc_Multistep, 
            legendtable = SL_1990_2020$tb_legend,
            xlab = "Year",
            ylab = bquote("Area (" ~ km^2~ ")"),
            area_km2 = TRUE)





## Annual Gain and Loss Intensity And 5 Year Gain and Loss of Area of Categories
category_gain_table <- testSL$category_lvlGain@categoryData
category_loss_table <- testSL$category_lvlLoss@categoryData


loss_table <- category_loss_table %>% select(!c("From", "Interval"))


combined <- cbind(category_gain_table, category_loss_table)

combined2 <- combined %>% select(1,2,4,5,6,10, 11) %>% rename("interval" = "Period...1",
                                                              "category" = "To",
                                                              "gain_km" = "GG_km2",
                                                              "uniform_rate" = "St...6",
                                                              "gain_rate" = "Gtj",
                                                              "loss_km" = "GL_km2",
                                                              "loss_rate" = "Lti")

combined2$interval <- factor(combined2$interval, 
                                levels = c("1990-1995", "1995-2000", "2000-2005", "2005-2010", "2010-2015", "2015-2020"))


## Annual Gain and Loss Rate
combined2 %>% 
   pivot_longer(c(4,7), names_to = "gain_loss_rate", values_to = "rate") %>% 
   pivot_longer(c(3,5), names_to = "gain_loss_km", values_to = "area") %>% 
   
   ggplot(aes(x= category, y= rate, fill = gain_loss_rate)) + geom_bar(stat = 'identity', position = position_dodge(0.8)) +
   geom_text(aes(0,uniform_rate,label = paste("Uniform Rate = ", round(uniform_rate,2)), hjust = -0.4, vjust= -0.5), size = 5) +
   scale_fill_manual(name = "", labels = c("Gain Rate", "Loss Rate"), values=c("#00afb9", "#f07167")) +
   geom_hline(aes(yintercept = uniform_rate, color = uniform_rate), linetype="dashed", color = "black", size = 1) +
   xlab('Category') + ylab('Annual Change Intensity (%)') + ggtitle("Annual Gain and Loss Intensity of Categories") +
   facet_wrap(~interval, ncol = 3) + theme_linedraw() +
   theme(panel.spacing.x = unit(1, "lines"),
         panel.spacing.y = unit(1, "lines"),
         strip.text.x = element_text(size = 17, face = 'bold'),
         axis.title.y = element_text(size = 17, face = "bold" ),
         axis.title.x = element_blank(),
         axis.text.y = element_text(size = 15),
         axis.text.x = element_text(angle=45, size = 15, hjust = 1),
         panel.grid = element_line(color = '#343a40'),
         plot.title = element_text(size=22, face = "bold"),
         legend.text = element_text(size = 16),
         legend.title = element_text(size = 13),
         legend.position = 'bottom')


ggsave("gain_loss_rate_v4.png", height = 7, width = 12, dpi= 300)



## Interval Gain and Loss of Area (km2)
combined2 %>% 
   pivot_longer(c(4,7), names_to = "gain_loss_rate", values_to = "rate") %>% 
   pivot_longer(c(3,5), names_to = "gain_loss_km", values_to = "area") %>% 
   arrange(desc(interval)) %>% 
   ggplot(aes(x= category, y= area, fill = gain_loss_km)) + geom_bar(stat = 'identity', position = position_dodge(0.8)) +
   scale_fill_manual(name = "", labels = c("Gain", "Loss"), values=c("#00afb9", "#f07167")) +
   xlab('') + ylab("Area"~(km^2)) + ggtitle("Gain and Loss Area of Categories") +
   facet_wrap(~interval, ncol = 3) + theme_linedraw() +
   theme(panel.spacing.x = unit(1, "lines"),
         panel.spacing.y = unit(1, "lines"),
         strip.text.x = element_text(size = 17, face = 'bold'),
         axis.title.y = element_text(size = 17, face = "bold" ),
         axis.title.x = element_blank(),
         axis.text.y = element_text(size = 15),
         axis.text.x = element_text(angle=45, size = 15, hjust = 1),
         panel.grid = element_line(color = '#343a40'),
         plot.title = element_text(size=22, face = "bold"),
         legend.text = element_text(size = 16),
         legend.title = element_text(size = 13),
         legend.position = 'bottom')


ggsave("gain_loss_area_v4.png", height = 7, width = 12, dpi= 300)




## Transition level gain of builtup from all other categories
transition_gain_built <- testSL$transition_lvlGain_n@transitionData

transition_gain_built <- transition_gain_built %>% rename("area_km" = "T_i2n_km2",
                                                          "interval" = "Period",
                                                          "rate" = "Rtin",
                                                          "uniform_rate" = "Wtn")

transition_gain_built$interval <- factor(transition_gain_built$interval, 
                                          levels = c("1990-1995", "1995-2000", "2000-2005", 
                                                     "2005-2010", "2010-2015", "2015-2020"))


transition_gain_built %>% 
   ggplot(aes(x= From, y= rate, fill = From)) + geom_bar(stat = 'identity', position = position_dodge(0.8)) +
   geom_text(aes(0, uniform_rate,label = paste("Uniform Rate = ", round(uniform_rate,2)), hjust = -0.4, vjust= -0.5), size = 5) +
   geom_hline(aes(yintercept = uniform_rate, color = uniform_rate), linetype="dashed", color = "black", size = 1) +
   scale_fill_manual(name = "", values=c("#00bbf9", "#38b000", "#ffd819", "#f8961e")) +
   xlab('') + ylab("Annual Rate of Transition (%)") + ggtitle("Intensity of Gain of Builtup") +
   facet_wrap(~interval, ncol = 3) + theme_linedraw() +
   theme(panel.spacing.x = unit(1, "lines"),
         panel.spacing.y = unit(1, "lines"),
         strip.text.x = element_text(size = 17, face = 'bold'),
         axis.title.y = element_text(size = 17, face = "bold" ),
         axis.title.x = element_blank(),
         axis.text.y = element_text(size = 15),
         axis.text.x = element_text(angle=45, size = 15, hjust = 1),
         panel.grid = element_line(color = '#343a40'),
         plot.title = element_text(size=22, face = "bold"),
         legend.text = element_text(size = 16),
         legend.title = element_text(size = 13),
         legend.position = 'bottom')


ggsave("transition_to_built_v4.png", height = 7, width = 12, dpi= 300)



names(testSL)


## Transition loss of Agriculture
transition_loss_agri <- testSL$transition_lvlLoss_m@transitionData

transition_loss_agri <- transition_loss_agri %>% rename("area_km" = "T_m2j_km2",
                                                          "interval" = "Period",
                                                          "gap" = "Interval",
                                                          "rate" = "Qtmj",
                                                          "uniform_rate" = "Vtm")

transition_loss_agri$interval <- factor(transition_loss_agri$interval, 
                                         levels = c("1990-1995", "1995-2000", "2000-2005", 
                                                    "2005-2010", "2010-2015", "2015-2020"))



transition_loss_agri %>% 
   ggplot(aes(x= To, y= rate, fill = To)) + geom_bar(stat = 'identity', position = position_dodge(0.8)) +
   geom_text(aes(0, uniform_rate,label = paste("Uniform Rate = ", round(uniform_rate,2)), hjust = -0.4, vjust= -0.5), size = 5) +
   geom_hline(aes(yintercept = uniform_rate, color = uniform_rate), linetype="dashed", color = "black", size = 1) +
   scale_fill_manual(name = "", values=c("#00bbf9", "#38b000", "red", "#f8961e")) +
   xlab('') + ylab("Annual Rate of Transition (%)") + ggtitle("Intensity of Loss of Agriculture") +
   facet_wrap(~interval, ncol = 1) + theme_linedraw() +
   theme(panel.spacing.x = unit(1, "lines"),
         panel.spacing.y = unit(1, "lines"),
         strip.text.x = element_text(size = 17, face = 'bold'),
         axis.title.y = element_text(size = 17, face = "bold" ),
         axis.title.x = element_blank(),
         axis.text.y = element_text(size = 15),
         axis.text.x = element_text(angle=45, size = 15, hjust = 1),
         panel.grid = element_line(color = '#343a40'),
         plot.title = element_text(size=22, face = "bold"),
         legend.text = element_text(size = 16),
         legend.title = element_text(size = 13),
         legend.position = 'bottom')


ggsave("transition_loss_agri_v4.png", height = 7, width = 12, dpi= 300)





write.csv(category_loss_table, file = "category_loss_table.csv", row.names = F, sep = )


barplotLand(dataset = SL_1990_2020$lulc_Multistep, 
            legendtable = SL_1990_2020$tb_legend,
            xlab = "Year",
            ylab = bquote("Area (" ~ km^2~ ")"),
            area_km2 = TRUE)


