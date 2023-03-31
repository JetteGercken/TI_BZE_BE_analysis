# exctracting elevation of plots from DGM5

# install packages
install.packages("terra")
install.packages("sf")
install.packages("elevatr")
install.packages("inborutils")
install.packages("stars")
install.packages("data.table")
install.packages("readxl")
install.packages("usethis")
install.packages("here")
install.packages("readr")
install.packages("tidyverse")
install.packages("tibble")
install.packages("dplyr")



# Load packages
require(terra)
require(sf)
require(elevatr)
require(inborutils)
require(data.table)
require(readxl)
require(usethis)
require(here)
require(readr)
require(tidyverse)
library(tidyr)
require(tibble)
require(dplyr)
options(stringsAsFactors = FALSE)
require(stars)


# CSR of the DEM we´re working with
# CRS: EPSG:25832
# CSR of the coordinates we´re working with
# WGS84

# Specify working directory
here::here()
here()
# Provide download url --> should I provide the HPC directory here? 
#download.url <- "https://data.geobasis-bb.de/geobasis/daten/dgm/xyz/"
HPC.path <- "/home/data/raster/BKG_DGM5/tifs/"
project.CSR <- "EPSG:25832"


# Read the tiles
# --> here I have to insert the tiles from the HPC no? 
# no it should be a path to the grid of a gpkg of the grid
# whcih is not the same as the tiles it seems
# so the tiles are only donwloaded or importetn when they are needed it seems, 
# like when there is an inteersection of the grid and the point i´ll import the
# tiff to extract the slop/ height/ aspect info from the HPC path
# Nikos code: tiles.sf <- st_read("BB_1x1km_tiles.gpkg")
tiles.sf <- read_sf("C:/Users/gercken/Documents/TI_BZE_BE_analysis/data/input/dgm5_20km_tiles_grid.shx")

# Get the tile grid via WFS in QGIS and export to gpkg
# https://geobroker.geobasis-bb.de/gbss.php?MODE=GetProductInformation&PRODUCTID=d9895ec2-7039-4c0d-914c-a68f227a7069
# https://isk.geobasis-bb.de/ows/blattschnitte_wfs
st_crs(tiles.sf) <- 25832
tiles.sf.gpkg <- tiles.sf %>% sf::st_write("C:/Users/gercken/Documents/TI_BZE_BE_analysis/data/input/dgm5_tiles_momok.gpkg", append=FALSE)
st_crs(tiles.sf.gpkg) <- 25832

#import list of tiffs
# https://stackoverflow.com/questions/52746936/how-to-efficiently-import-multiple-raster-tif-files-into-r
# rastlist <- list.files(path = "/home/data/raster/BKG_DGM5/tifs", pattern='.TIF$', 
#                       all.files=TRUE, full.names=FALSE)



# coordinates
# Dataframe
# read coordinates of MoMok plot Referrenzpunkte: 
momok.df <- read.delim(file = here("data/input/MoMok/locations_MoMoK_total.csv"), sep = ";", dec = ",")  
momok.df <- momok.df %>% select(., c(1, 3, 11, 12)) %>% filter(Skizzenpunkt == "RP")
colnames(momok.df) <- c("id", "point_type", "Y", "X")
# create datafram that has just the coordinates
# https://stackoverflow.com/questions/46664164/r-how-to-transform-epsg-25832-coordinates-into-epsg-4326-ones
momok.dt = as.data.table(momok.df)
momok.dt = momok.dt[,.(as.numeric(X),as.numeric(Y))]

# Spatial vector  --> doesnt work
# create SPatVec with terra
# https://rdrr.io/cran/terra/man/vect.html
#momok.spvec <- terra::vect(momok.dt, geom=c("V1", "V2"), crs=" +proj=longlat +datum=WGS84", keepgeom=FALSE)
# reproject Point to the same CSR as the DEM with terra: 
# https://search.r-project.org/CRAN/refmans/terra/html/project.html
#momok.spvec.reproj <-  terra::project(momok.spvec, "EPSG:25832")


# Spatial object sf
# Make the dataframe a spatial object of class = "sf"  
# https://www.ecologi.st/spatial-r/raster-gis-operations-in-r-with-terra.html
# https://gis.stackexchange.com/questions/3334/difference-between-wgs84-and-epsg4326
# from Nikolai:
# moorwald.sf <- st_as_sf(moorwald.dt, coords=c("X_GK3", "Y_GK3"), remove=F, crs=31467)
# st_as_sf
momok.sf <- st_as_sf(momok.df, coords = c("X", "Y"), remove=F, crs = 4326)
# reproject: Set to the same projection as the elevation data
# this would be the original code, but as i don´t have one DEM but many I don´t know if that would work: pc <- st_transform(pc, crs(dem30))
# thus i use Nikolais approach: aoi.sf <- st_transform(aoi.sf, crs=25833) 
momok.sf.reproj <- st_transform(momok.sf, crs = 25832)
sf::st_crs(momok.sf.reproj)  <- 25832
# vectorize: one webpage said this would be necesarry to do a terra intersect but aparently for st_intersection from the 
# NOTE!!! terra doesn’t play nicely with sf objects at this stage, so you need to coerce them into terra’s own vector format using vect().
# https://www.ecologi.st/spatial-r/raster-gis-operations-in-r-with-terra.html
momok.sf.vec.reproj <- terra::vect(momok.sf.reproj)
plot(momok.sf.reproj$geometry)


# create an id column 
names(momok.sf.vec.reproj)
id.df <- data.frame(id = momok.df %>% dplyr::pull(id))
momok.sf.vec.reproj.df <- as.data.frame(momok.sf.vec.reproj)
setnames(momok.sf.vec.reproj.df, old=c("id"), new=c("id"))
names(momok.sf.vec.reproj.df)

# Maiking th repeojected coordinates the area/ points of interest 
#aoi.sf <- momok.spvec.reproj
aoi.sf <- momok.sf.reproj
momok.sf.reproj$id # this one already has an ID column

plot(tiles.sf.gpkg$geometry)
inter <- sf::st_intersection(x=tiles.sf.gpkg, y=aoi.sf)
plot(inter$geometry)


# processing loop from Nikolai
for(i in 1:nrow(aoi.sf)){   # area of interest will be the list of points i think
  # i=1
  
  # Pick AOI
  my.aoi.sf <- aoi.sf[i,]
  # plot(my.aoi.sf$geom)
  
  # Extract AOI name
  my.aoi.name <- my.aoi.sf$id
  
  # Get tiles which intersect with AOI 
  # ---> maybe I can use this to filter for those tiles where momo plot are located in?   
  # so tiles will be a lits of tiles in the HPC folder
  #https://stackoverflow.com/questions/61727820/using-gcontains-and-checking-if-the-points-is-inside-the-polygon-country-r
  intersecting.tiles.sf <- sf::st_intersection(x=tiles.sf.gpkg, y=my.aoi.sf)  #intersection(x=tiles.sf.gpkg, y=my.aoi.sf)
  plot(intersecting.tiles.sf$geometry)
  
  # Loop over tiles
  dtm.list <- list() # this creates an empty list
  for(j in 1:nrow(intersecting.tiles.sf)){
    # j=1
    
    # Pick tile
    my.tile.sf <- intersecting.tiles.sf[j,]
    
    # Construct file name
    my.kachelnummer <- gsub(my.tile.sf$kachelnummer, pattern="_", replacement="-")
    my.fn <- paste0("dgm_", my.kachelnummer)
    
    # Download the file
    # instead of downloading the file Iĺl just have to import/ access it no? 
    # --> do i have tp put the HPC directoy here? 
    # i think i can directly load the raster here
    dtm.rast <- terra::rast(paste0(HPC.path, my.fn, ".tif"), quiet=T)
    # paste0 pasts the respective name with the ending .tif, which then creates the name of the tif to download/ read
    
    #terra::rast(paste0(HPC.path, my.fn, ".tif"),
    #            destfile=paste0(my.fn, ".tif"), quiet=T) 
    # Unzip the file --> mine won´t be zipped
    #unzip(paste0(my.fn, ".zip"), overwrite=T)
    
    
    # Read the xyz data  --> mine wil aready be rasterized as they are tifs?
    # dtm.rast <- rast(paste0(my.fn, ".tif"))
    
    #xyz.dt <- fread(paste0(my.fn, ".tif"))
    # Rasterize the data
    #dtm.rast <- rast(xyz.dt, type="tif")
    plot(dtm.rast)
    
    # Assign CRS
    crs(dtm.rast) <- "EPSG:25833"
    # Collect in list
    dtm.list[[j]] <- dtm.rast
    
    # Remove zip and xyz file
    #file.remove(paste0(my.fn, ".zip"))
    #file.remove(paste0(my.fn, ".xyz"))
    #file.remove(paste0(my.fn, ".html"))
    #file.remove(paste0(my.fn, ".xml"))
  }
  
  # Create a SpatRaster collection (src) from the list
  dtm.src <- terra::src(dtm.list)
  # Make the mosaic
  dtm.mosaic.rast <- mosaic(dtm.src)
  plot(dtm.mosaic.rast)
  
  # turn the mosaic into a star object to the run the extract
 # https://r-spatial.github.io/stars/reference/st_extract.html
  # tif = system.file("tif/L7_ETMs.tif", package = "stars")
  #r = read_stars(tif)
  # help("system.file") 
  
 
  
  # Ithibnk the following I wont need because I am not creating a raster file but I am extracting point data
  # instead Iĺl have to add a code that extracts the elevation
  
  # for st_extract I need the raster or the raster Mosaic to be a star object 
  # https://r-spatial.github.io/stars/reference/st_as_stars.html
  st_as_stars(dtm.mosaic.rast, att = 1, ignore_file = FALSE)
  elevation.df <- st_extract(dtm.mosaic.rast, aoi.sf)
  
  
  # Crop and mask with the AOI
  # my.aoi.vect <- vect(my.aoi.sf)
  # dtm.mosaic.rast <- crop(dtm.mosaic.rast, my.aoi.vect)
  # dtm.mosaic.rast <- mask(dtm.mosaic.rast, my.aoi.vect)
  # 
  # # Save DTM mosaic
  # writeRaster(dtm.mosaic.rast, paste0(out.path, "dtm/dtm_", my.aoi.name, ".tif"), overwrite=T)
  # 
  # # Compute hill shade raster
  # slope.rast <- terrain(dtm.mosaic.rast, "slope", unit="radians")
  # aspect.rast <- terrain(dtm.mosaic.rast, "aspect", unit="radians")
  # hillshade.rast <- shade(slope=slope.rast, aspect=aspect.rast, angle=40, direction=270)
  # plot(hillshade.rast)
  # 
  # # Save hill shade raster
  # writeRaster(hillshade.rast, paste0(out.path, "hillshade/hillshade_", my.aoi.name, ".tif"), overwrite=T)
  
  # Progress feedback
  print(elevation.df)
}
