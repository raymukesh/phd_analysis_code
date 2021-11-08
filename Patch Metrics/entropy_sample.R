
## Install Required Packages
pkgs = c(
  "rgeopat2",          # helper functions for GeoPAT 2 in R
  "sf",                # vector data classes
  "raster",            # raster data classes
  "dplyr",             # data manipulation
  "tmap",              # spatial visualisation
  "landscapemetrics"   # contains example dataset
)
to_install = !pkgs %in% installed.packages()
if(any(to_install)) {
  install.packages(pkgs[to_install])
}

library(rgeopat2)
library(sf)
library(raster)
library(dplyr)
library(tmap)
library(landscapemetrics)


data("augusta_nlcd")

augusta_nlcd = deratify(augusta_nlcd, "NLCD.2011.Land.Cover.Class")
dir.create("data")
writeRaster(augusta_nlcd, "data/augusta_nlcd.tif", overwrite = TRUE)

nlcd_colors = c("#000000", "#00F900", "#476BA0", "#D1DDF9", "#DDC9C9", "#D89382",
                "#ED0000", "#AA0000", "#B2ADA3",  "#68AA63", "#1C6330", 
                "#B5C98E", "#dcca8f", 
                "#fde9aa", "#DBD83C", "#AA7028", "#BAD8EA", "#64b3d5")

p1 <- tm_shape(augusta_nlcd) +
  tm_raster("NLCD.2011.Land.Cover.Class", palette = nlcd_colors) + 
  tm_layout(legend.outside = TRUE)
p1


system("gpat_gridhis -i data/augusta_nlcd.tif -o data/augusta_ent.grd -z 20 -f 20 -s 'ent' -n 'none'")

system("gpat_grid2txt -i data/augusta_ent.grd -o data/augusta_ent.txt")

augusta_grid = gpat_create_grid("data/augusta_ent.grd.hdr")

getwd()





