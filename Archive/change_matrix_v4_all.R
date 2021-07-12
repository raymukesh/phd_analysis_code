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



