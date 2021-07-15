## This code creates transition of land use among all the classes for all the years of data
## This utilises OpenLand package

## install.packages("OpenLand")
library(OpenLand)
library(raster)

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

## change the legend names and colors
SL_1990_2020$tb_legend$color <- c("blue", "green", "red", "yellow", "brown")
SL_1990_2020$tb_legend$categoryName <- factor(c("Water", "Vegetation", "Builtup", "Agriculture", "Barren"),
                                              levels = c("Water", "Vegetation", "Builtup", "Agriculture", "Barren"))


## Intensity Analysis - 

## Intensity of changes between builtup and agriculture
testSL <- intensityAnalysis(dataset = SL_1990_2020,
                            category_n = "Builtup", category_m = "Agriculture")



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


