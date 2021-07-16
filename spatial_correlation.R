# Libraries
library(raster)
library(rasterVis)
#library(mapview)
#library(mapedit)
library(sf)
library(readr)
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(RColorBrewer)
library(rgdal) 
library(maptools)
library(sp)
library(ggspatial) # add scalebar



# Importing data
fmc <- raster('D:/POstdoc/Namadgi/figs/fmc_burn/forest_smokers_fmc.tif') # Importing FMC map
rdnbr <- raster('D:/POstdoc/Namadgi/figs/fmc_burn/forest_smokers_Rdelta_NBR.tif') #Importing Relative difference Normalized Burn Ratio Map
shp <- readOGR(dsn="D:/POstdoc/Namadgi/Adam_files/shp_burn_area/Smokers.shp") # Importing shapefile

# Setting coordinate system epsg 3577 to raster maps
crs(fmc) <- CRS('+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=132 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs')
crs(rdnbr) <- CRS('+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=132 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs')

crs(shp) # checking crs for shapefile
class(shp) # checking the data format of a shapefile
shp_3577 <- spTransform(shp, crs(fmc)) # transforming a shapefile to epsg 3577


# Checking the correlaition between the two rasters
cor(values(fmc), values(rdnbr), use="pairwise.complete.obs")
# checking the linear relationship between the two rasters
lm1 <- lm(fmc_burn$fmc[,1] ~ fmc_burn$burn[,2], na.action=na.omit)
summary(lm1)

## Set up color gradient for plotting FMC map
breaks_fmc <- seq(0, 160, by = 14.9)
cols_fmc <- brewer.pal(10, "Spectral")
## Set up color gradient for plotting rdnbr map
breaks_rdnbr <- seq(-1.22, 1.8, by = 0.3)
cols_rdnbr <- rev(brewer.pal(11, "Spectral"))


## Use `at` and `col.regions` arguments to set the color scheme
# plotting
p1 <- levelplot(fmc, at = breaks_fmc, col.regions = cols_fmc, margin = FALSE) + latticeExtra::layer(sp.lines(shp_3577, lwd=1.5, col='black'))
p2 <- levelplot(rdnbr, at = breaks_rdnbr, col.regions = cols_rdnbr, margin = FALSE)+ latticeExtra::layer(sp.lines(shp_3577, lwd=1.5, col='black'))


# Stack covariates
fmc_burn <- stack(fmc, rdnbr)
names(fmc_burn) <- c("fmc", "burn")

## Calculate local correlation using focal on two rasters at the same time (source: https://statnmap.com/2018-01-27-spatial-correlation-between-rasters/)

## for each raster we calculate correlation of 5x5 focal pixels and record the correlation coefficient for the central pixel
fmc_burn_nb <- raster(fmc_burn, 1)
values(fmc_burn_nb) <- 1:ncell(fmc_burn)

matrix_fmc_burn <- values(fmc_burn) # stack as raster [MW]
extraWD <- 'D:/POstdoc/Namadgi/figs/fmc_burn/'
focal_cor <- focal(
  x = fmc_burn_nb,
  w = matrix(1, 7, 7),
  fun = function(x, y = matrix_fmc_burn){ # [MW]
    cor(y[x, 1], y[x, 2], # [MW]
        use = "na.or.complete")
  },
  filename = file.path(extraWD, "focal_cor1.tif"), # saving the correlation raster map
  overwrite = TRUE
)

# opening the correlation map
focal_cor <- raster(file.path(extraWD, "focal_cor1.tif"))


# creating color breaks and selecting color palette for plotting
breaks_cor <- seq(-1, 1, by = 0.2)
cols_cor <- brewer.pal(11, "Spectral")

## Use `at` and `col.regions` arguments to set the color scheme
p3 <- levelplot(focal_cor, at = breaks_cor, col.regions = cols_cor, margin = FALSE) + latticeExtra::layer(sp.lines(shp_3577, lwd=1.5, col='black'))

# Plotting all p1, p2, p3 figures
grid.arrange(p1, p2, p3, ncol=3)

