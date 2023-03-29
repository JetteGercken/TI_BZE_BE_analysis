#########################################################################
# Script for downloading and processing DTMs in Brandenburg at selected
# plot positions
#########################################################################

# Load packages
require(terra)
require(sf)
require(data.table)
require(readxl)


# Specify working directory
wd <- "C:/Users/knapp/Documents/Kleinprojekte/BB_DGMs/"
setwd(wd)

# Provide download url
download.url <- "https://data.geobasis-bb.de/geobasis/daten/dgm/xyz/"

# Get the tile grid via WFS in QGIS and export to gpkg
# https://geobroker.geobasis-bb.de/gbss.php?MODE=GetProductInformation&PRODUCTID=d9895ec2-7039-4c0d-914c-a68f227a7069
# https://isk.geobasis-bb.de/ows/blattschnitte_wfs

# Read the tiles
tiles.sf <- st_read("BB_1x1km_tiles.gpkg")
st_crs(tiles.sf)

# Read GADM country borders
countries.sf <- st_read("C:/Users/knapp/Rawdata/GADM/gadm36_levels_shp/gadm36_1.shp")
head(countries.sf)
# Subset Brandenburg
BB.sf <- subset(countries.sf, NAME_1 == "Brandenburg")
plot(BB.sf$geom)


# Function that creates rectangular polygons around given center points
make_rectangles <- function(ctr.x=0, ctr.y=0, sl.x=100, sl.y=100){
  require(sp)
  # Create an empty list to collect the single polygons
  ps.list <- list()
  # Loop over all unique polygons and process the coordinate info to build a polygon
  n <- length(ctr.x)
  for(i in 1:n){
    # Calculate y-coordinates of all 4 corners
    my.ll.y <- ctr.y[i] - sl.y / 2
    my.lr.y <- ctr.y[i] - sl.y / 2
    my.ul.y <- ctr.y[i] + sl.y / 2
    my.ur.y <- ctr.y[i] + sl.y / 2
    # Calculate x-coordinates of all 4 corners
    my.ll.x <- ctr.x[i] - sl.x / 2
    my.ul.x <- ctr.x[i] - sl.x / 2
    my.ur.x <- ctr.x[i] + sl.x / 2
    my.lr.x <- ctr.x[i] + sl.x / 2
    # Build a corner matrix
    corner.mx <- matrix(c(my.ll.x, my.ll.y, my.ul.x, my.ul.y,
                          my.ur.x, my.ur.y, my.lr.x, my.lr.y,
                          my.ll.x, my.ll.y), ncol=2, byrow=T)
    # Make a polygon
    p = sp::Polygon(corner.mx)
    ps = sp::Polygons(list(p), 1)
    # Add it to the Polygons list (by default IDs > 1e5 are written in scientific
    # notation; this is avoided with format function; this is needed for later
    # conversion to SpatialPolygonsDataFrame)
    ps.list[[i]] <- sp::Polygons(list(p), ID=format(i, scientific=F))
  }
  # Convert them to spatial polygons
  sps <- sp::SpatialPolygons(ps.list)
  # Convert them to a spatial polygon dataframe
  spdf <- sp::SpatialPolygonsDataFrame(sps, data.frame(ID=1:i))
  return(spdf)
}


##############
# WZE
##############

# Load polygons of area of interest (from geopackage or shape file)
aoi.sf <- st_read("WZE_16x16_INSPIRE_grid_cells_EPSG3035.gpkg")
plot(aoi.sf$geom)

# Reproject to LAEA CRS
BB.laea.sf <- st_transform(BB.sf, crs=3035)

# Clip only plots in BB as AOI
aoi.sf <- st_intersection(x=aoi.sf, y=BB.laea.sf)
plot(aoi.sf$geom)


##############
# MoorWald
##############

# Provide an outpath
out.path <- "MoorWald/"

# Read the MoorWald tract infos
moorwald.dt <- data.table(read_xlsx("BWI_Punkte_Brandenburg.xlsx"))

# Create 300-m circles around each point
moorwald.sf <- st_as_sf(moorwald.dt, coords=c("X_GK3", "Y_GK3"), remove=F, crs=31467)
moorwald.circles.sf <- st_buffer(moorwald.sf, dist=300, nQuadSegs=90)
plot(moorwald.circles.sf$geometry)

# Save vector data to files
st_write(moorwald.circles.sf, paste0(out.path, "MoorWald_circles_GK3.gpkg"), append=F)
st_write(moorwald.sf, paste0(out.path, "MoorWald_points_GK3.gpkg"), append=F)

# Create an id column
moorwald.circles.sf$id <- paste0(moorwald.circles.sf$TNR, "_", moorwald.circles.sf$ENR)


##############
# MoMoK
##############

# Provide an outpath
out.path <- "MoMoK/"

# Read the MoorWald tract infos
momok.dt <- data.table(read_xlsx("MoMoK Standorte und Koordinaten.xlsx"))

# Convert from geographic to UTM33N CRS to match the DTM rasters and
# work with meter units
momok.sf <- st_as_sf(momok.dt, coords=c("X", "Y"), remove=F, crs=4326)
plot(momok.sf$geometry)
momok.sf <- st_transform(momok.sf, crs=25833)

# Create columns with LAEA coordinates
momok.sf$X_UTM33N <- st_coordinates(momok.sf)[,1]
momok.sf$Y_UTM33N <- st_coordinates(momok.sf)[,2]

# Create 500-m squares around each point
momok.squares.spdf <- 
  make_rectangles(ctr.x=momok.sf$X_UTM33N, ctr.y=momok.sf$Y_UTM33N, 
                  sl.x=500, sl.y=500)
momok.squares.sf <- st_as_sf(momok.squares.spdf)
st_crs(momok.squares.sf) <- 25833

# Add the data from the original table
momok.squares.sf <- cbind(momok.squares.sf, momok.dt)
plot(momok.squares.sf$geometry)

# Save vector data to files
st_write(momok.squares.sf, paste0(out.path, "MoMoK_squares_UTM33N.gpkg"), append=F)
st_write(momok.sf, paste0(out.path, "MoMoK_points_UTM33N.gpkg"), append=F)

# Clip only plots in BB
BB.utm33n.sf <- st_transform(BB.sf, crs=25833)
momok.BB.squares.sf <- st_intersection(x=momok.squares.sf, y=BB.utm33n.sf)
plot(momok.BB.squares.sf$geom)

st_write(momok.BB.squares.sf, paste0(out.path, "MoMoK_BB_squares_UTM33N.gpkg"), append=F)

# Create an id column
names(momok.BB.squares.sf)
setnames(momok.BB.squares.sf, old=c("Nr...lt..BZE."), new=c("id"))
names(momok.BB.squares.sf)


###################
# Start processing
###################

# Make the MoorWald circles the AOI
aoi.sf <- moorwald.circles.sf

# Make the MoMoK BB squares the AOI
aoi.sf <- momok.BB.squares.sf

# Reproject AOI to same CRS as BB DTM data
aoi.sf <- st_transform(aoi.sf, crs=25833)

# Loop over AOIs
for(i in 1:nrow(aoi.sf)){
  # i=1
  # Pick AOI
  my.aoi.sf <- aoi.sf[i,]
  # plot(my.aoi.sf$geom)
  # Extract AOI name
  my.aoi.name <- my.aoi.sf$id

  # Get tiles which intersect with AOI
  intersecting.tiles.sf <- st_intersection(x=tiles.sf, y=my.aoi.sf)
  plot(intersecting.tiles.sf$geom)
  
  # Loop over tiles
  dtm.list <- list()
  for(j in 1:nrow(intersecting.tiles.sf)){
    # j=1
    # Pick tile
    my.tile.sf <- intersecting.tiles.sf[j,]
    
    # Construct file name
    my.kachelnummer <- gsub(my.tile.sf$kachelnummer, pattern="_", replacement="-")
    my.fn <- paste0("dgm_", my.kachelnummer)
    
    # Download the file
    download.file(url=paste0(download.url, my.fn, ".zip"),
                  destfile=paste0(my.fn, ".zip"), quiet=T)
    # Unzip the file
    unzip(paste0(my.fn, ".zip"), overwrite=T)
    
    # Read the xyz data
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



# Test hydrology
require(topmodel)

plot(dtm.mosaic.rast)

dtm.mosaic.mx <- as.matrix(dtm.mosaic.rast, wide=T)
dim(dtm.mosaic.mx)
topidx.list <- topidx(DEM=dtm.mosaic.mx, resolution=1)
class(topidx.list[[1]])
topidx.rast <- rast(topidx.list[[1]])
dim(topidx.list[[1]])
plot(topidx.rast)
area.rast <- rast(topidx.list[[2]])
plot(area.rast)

river.mx <- river(DEM=dtm.mosaic.mx, atb=topidx.list[[1]], 
                  area=topidx.list[[2]], res=1, thatb=1, tharea=2)
river.rast <- rast(river.mx)
plot(river.rast)












