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


landcover <- data.frame(year2015=values(year2015), year2020=values(year2020))

landcover_change <- table(landcover)

landcover_change_matrix <- (round(addmargins(landcover_change)*3600/1000000, digits = 1))


write.table(landcover_change_matrix, file = "matrix_2015_2020.csv", append = T, sep = ",", col.names = NA, 
            row.names = T, quote = F)



## Generating percentage of land cover change table

landcoverYear1990 <- cbind(rowSums(round(table(landcover)*3600*1/1000000,digits = 1)))

landcoverYear1995 <- cbind(colSums(round(table(landcover)*3600*1/1000000,digits = 1)))

Year1990percentage <- cbind(round((landcoverYear1990/sum(landcoverYear1990)*100),digits = 1))

Year1995percentage <- cbind(round((landcoverYear1995/sum(landcoverYear1995)*100),digits = 1))

Difference <- cbind(c(landcoverYear1995)-c(landcoverYear1990))

percentageDifference <-(Difference/landcoverYear1990)*100


FinalTable <- cbind(c(landcoverYear1990),
                    c(landcoverYear1995),c(Difference),
                    c(Year1990percentage),c(Year1995percentage),
                    c(percentageDifference))

colnames(FinalTable) <- c("Year 1990","Year 1995","Difference","Year 1990 % of Total",
                          "Year 1995 % of Total","% Difference")

write.table(FinalTable ,file="percentage_landcover_1990_1995.csv")




