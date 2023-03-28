# exctracting elevation of plots from DGM5

# install packages
install.packages("terra")
install.packages("sf")
install.packages("elevatr")
install.packages("inborutils")
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
require(tibble)
require(dplyr)
options(stringsAsFactors = FALSE)




# CSR of the DEM we´re working with
  # CRS: EPSG:25832
# CSR of the coordinates we´re working with
    # WGS84

# Specify working directory
here::here()

# Provide download url --> should I provide the HPC directory here? 
#download.url <- "https://data.geobasis-bb.de/geobasis/daten/dgm/xyz/"
HPC.path <- "/home/data/raster/BKG_DGM5/tifs"

# Get the tile grid via WFS in QGIS and export to gpkg
# https://geobroker.geobasis-bb.de/gbss.php?MODE=GetProductInformation&PRODUCTID=d9895ec2-7039-4c0d-914c-a68f227a7069
# https://isk.geobasis-bb.de/ows/blattschnitte_wfs

# Read the tiles
 # --> here I have to insert the tiles from the HPC no? 
 # no it should be a path to the grid of a gpkg of the grid
 # whcih is not the same as the tiles it seems
 # so the tiles are only donwloaded or importetn when they are needed it seems, 
 # like when there is an inteersection of the grid and the point i´ll import the
 # tiff to extract the slop/ height/ aspect info from the HPC path
tiles.sf <- st_read("BB_1x1km_tiles.gpkg")
tiles.sf <- st_read("/home/data/raster/BKG_DGM5/tifs")

tiles.sf <- read_sf("/home/data/raster/BKG_DGM5/dgm5_20km_tiles_grid.shp") %>% 
st_crs(tiles.sf)
tiles.sf %>% sf::st_write("tiles.gpkg")

# https://stackoverflow.com/questions/52746936/how-to-efficiently-import-multiple-raster-tif-files-into-r
# rastlist <- list.files(path = "/home/data/raster/BKG_DGM5/tifs", pattern='.TIF$', 
#                       all.files=TRUE, full.names=FALSE)


# # this I wont need because the country doesnt matter to me 
# # Read GADM country borders
# countries.sf <- st_read("C:/Users/knapp/Rawdata/GADM/gadm36_levels_shp/gadm36_1.shp")
# head(countries.sf)
# # Subset Brandenburg
# BB.sf <- subset(countries.sf, NAME_1 == "Brandenburg")
# plot(BB.sf$geom)
project.CSR <- "EPSG:25832"


tiles_DGM5.list <- list.files(datapath("/home/data/raster/BKG_DGM5/tifs/"), full.names = TRUE)
# temp_rasters <- rast(tiflist_DGM5)


# coordinates
# Dataframe
# read coordinates of MoMok plot Referrenzpunkte: 
  momok.df <- read.delim(file = here("data/input/MoMoK/locations_MoMoK_total.csv"), sep = ";", dec = ",")  
  momok.df <- momok.df %>% select(., c(1, 3, 11, 12)) %>% filter(Skizzenpunkt == "RP")
  colnames(momok.df) <- c("id", "point_type", "Y", "X")
# create datafram that has just the coordinates
  # https://stackoverflow.com/questions/46664164/r-how-to-transform-epsg-25832-coordinates-into-epsg-4326-ones
   momok.dt = as.data.table(momok.df)
   momok.dt = momok.dt[,.(as.numeric(X),as.numeric(Y))]

# Spatial vector  
# create SPatVec with terra
  # https://rdrr.io/cran/terra/man/vect.html
  momok.spvec <- terra::vect(momok.dt, geom=c("V1", "V2"), crs=" +proj=longlat +datum=WGS84", keepgeom=FALSE)
# reproject Point to the same CSR as the DEM with terra: 
  # https://search.r-project.org/CRAN/refmans/terra/html/project.html
  momok.spvec.reproj <-  terra::project(momok.spvec, "EPSG:25832")

  
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
# vectorize
  # NOTE!!! terra doesn’t play nicely with sf objects at this stage, so you need to coerce them into terra’s own vector format using vect().
  # https://www.ecologi.st/spatial-r/raster-gis-operations-in-r-with-terra.html
  momok.sf.vec.reproj <- terra::vect(momok.sf.reproj)
  #plot(momok.sf$geometry)
  
# this is how Niko created the area of interest
 # Create 300-m circles around each point
  # moorwald.sf <- st_as_sf(moorwald.dt, coords=c("X_GK3", "Y_GK3"), remove=F, crs=31467)
  # some steps in between to create circles that I don´t need
  # Make the MoorWald circles the AOI
  # aoi.sf <- moorwald.circles.sf
  # Make the MoMoK BB squares the AOI
  # aoi.sf <- momok.BB.squares.sf
  # Reproject AOI to same CRS as BB DTM data
  # aoi.sf <- st_transform(aoi.sf, crs=25833)

# Maiking th repeojected coordinates the area/ points of interest 
  #aoi.sf <- momok.spvec.reproj
  aoi.sf <- momok.sf.vec.reproj
  
  
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
  intersecting.tiles.sf <- st_intersection(x=tiles.sf, y=my.aoi.sf)
  #plot(intersecting.tiles.sf$geom)
  
  # Loop over tiles
  dtm.list <- list() # this creates an empty list
  for(j in 1:nrow(intersecting.tiles.sf)){
    # j=1
    # Pick tile
    my.tile.sf <- intersecting.tiles.sf[j,]
    
    # Construct file name
    my.kachelnummer <- gsub(my.tile.sf$kachelnummer, pattern="_", replacement="-")
    my.fn <- paste0("dgm_", my.kachelnummer)
    
    # Download the file --> do i have tp put the HPC directoy here? 
    download.file(url=paste0(HPC.path, my.fn, ".zip"),
                  destfile=paste0(my.fn, ".zip"), quiet=T)
    # Unzip the file --> mine won´t be zipped
    unzip(paste0(my.fn, ".zip"), overwrite=T)
    
    # Read the xyz data  --> mine wil aready be rasterized as they are tifs?
    xyz.dt <- fread(paste0(my.fn, ".xyz"))
    # Rasterize the data
    dtm.rast <- rast(xyz.dt, type="xyz")
    # plot(dtm.rast)
    # Assign CRS
    crs(dtm.rast) <- "EPSG:25833"
    # Collect in list
    dtm.list[[j]] <- dtm.rast
    
    # Remove zip and xyz file
    file.remove(paste0(my.fn, ".zip"))
    file.remove(paste0(my.fn, ".xyz"))
    file.remove(paste0(my.fn, ".html"))
    file.remove(paste0(my.fn, ".xml"))
  }
  
  # Create a SpatRaster collection (src) from the list
  dtm.src <- terra::src(dtm.list)
  # Make the mosaic
  dtm.mosaic.rast <- mosaic(dtm.src)
  plot(dtm.mosaic.rast)
  
  # Crop and mask with the AOI
  my.aoi.vect <- vect(my.aoi.sf)
  dtm.mosaic.rast <- crop(dtm.mosaic.rast, my.aoi.vect)
  dtm.mosaic.rast <- mask(dtm.mosaic.rast, my.aoi.vect)
  
  # Save DTM mosaic
  writeRaster(dtm.mosaic.rast, paste0(out.path, "dtm/dtm_", my.aoi.name, ".tif"), overwrite=T)
  
  # Compute hill shade raster
  slope.rast <- terrain(dtm.mosaic.rast, "slope", unit="radians")
  aspect.rast <- terrain(dtm.mosaic.rast, "aspect", unit="radians")
  hillshade.rast <- shade(slope=slope.rast, aspect=aspect.rast, angle=40, direction=270)
  plot(hillshade.rast)
  
  # Save hill shade raster
  writeRaster(hillshade.rast, paste0(out.path, "hillshade/hillshade_", my.aoi.name, ".tif"), overwrite=T)
  
  # Progress feedback
  print(i)
}
