library(raster)

list.files("D:/OneDrive - UTS/PhD_UTS/Stage 3/Analysis/Prediction/ForTerrset60_v4", pattern = ".tif")


year1990 <- raster("final_1990_v4.tif")
year1995 <- raster("final_1995_v4.tif")
year2000 <- raster("final_2000_v4.tif")
year2005 <- raster("final_2005_v4.tif")
year2010 <- raster("final_2010_v4.tif")
year2015 <- raster("final_2015_v4.tif")
year2020 <- raster("final_2020_v4.tif")


landcover <- data.frame(year1990=values(year1990), year2020=values(year2020))

landcover_change <- table(landcover)

landcover_change_matrix <- (round(addmargins(landcover_change)*3600/1000000, digits = 1))


write.table(landcover_change_matrix, file = "matrix_1990_2020.csv", append = T, sep = ",", col.names = NA, 
            row.names = T, quote = F)



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
             color = c(GC = "gray70", NG = "#006400", NL = "#EE2C2C")
)

chordDiagramLand(dataset = SL_1990_2020$lulc_Onestep,
                 legendtable = SL_1990_2020$tb_legend)



sankeyLand(dataset = SL_1990_2020$lulc_Multistep,
           legendtable = SL_1990_2020$tb_legend)


sankeyLand(dataset = SL_1990_2020$lulc_Onestep,
           legendtable = SL_1990_2020$tb_legend)



## Use Openland Package for Further Analysis

stack_all <- stack(year1990, year1995, year2000, year2005, year2010, year2015, year2020)


SL_1990_2020 <- contingencyTable(input_raster = stack_all, pixelresolution = 60)

head(SL_1990_2020)

names(SL_1990_2020)

SL_1990_2020$tb_legend$color <- c("blue", "green", "red", "yellow", "brown")
SL_1990_2020$tb_legend$categoryName <- factor(c("Water", "Vegetation", "Builtup", "Crop", "Barren"),
                                              levels = c("Water", "Vegetation", "Builtup", "Crop", "Barren"))

testSL <- intensityAnalysis(dataset = SL_1990_2020,
                            category_n = "Builtup", category_m = "Crop")



plot(testSL$interval_lvl,
     labels = c(leftlabel = "Interval Change Area (%)",
                rightlabel = "Annual Change Area (%)"),
     marginplot = c(-8, 0), labs = c("Changes", "Uniform Rate"),
     leg_curv = c(x = 2/10, y = 3/10))


## Use Openland Package for Further Analysis

install.packages("OpenLand")
library(OpenLand)


# first we load the OpenLand package
library(OpenLand)

# downloading the SaoLourencoBasin multi-layer raster and make it available into R
url <- "https://zenodo.org/record/3685230/files/SaoLourencoBasin.rda?download=1"

temp <- tempfile()
download.file(url, temp, mode = "wb") # downloading the SaoLourencoBasin dataset
load(temp)



SaoLourencoBasin


SL_2002_2014 <- contingencyTable(input_raster = SaoLourencoBasin, pixelresolution = 30)




