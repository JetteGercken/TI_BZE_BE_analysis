# exctracting elevation of plots from DGM5

# install packages
install.packages("terra")
install.packages("sf")
install.packages("elevatr")

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
require(data.table)
require(readxl)
require(usethis)
require(here)
require(readr)
require(tidyverse)
require(tibble)
require(dplyr)


# CSR of the DEM we´re working with
  # CRS: EPSG:25832
# CSR of the coordinates we´re working with
    # WGS84

# Specify working directory
here::here()

# Provide download url --> should I provide the HPC directory here? 
#download.url <- "https://data.geobasis-bb.de/geobasis/daten/dgm/xyz/"
HPC.path <- "/home/data/raster/BKG_DGM5/tifs/"

# Get the tile grid via WFS in QGIS and export to gpkg
# https://geobroker.geobasis-bb.de/gbss.php?MODE=GetProductInformation&PRODUCTID=d9895ec2-7039-4c0d-914c-a68f227a7069
# https://isk.geobasis-bb.de/ows/blattschnitte_wfs

# Read the tiles --> here I have to insert the tiles from the HPC no? 
tiles.sf <- st_read("BB_1x1km_tiles.gpkg")
st_crs(tiles.sf)

# # this I wont need because the country doesnt matter to me 
# # Read GADM country borders
# countries.sf <- st_read("C:/Users/knapp/Rawdata/GADM/gadm36_levels_shp/gadm36_1.shp")
# head(countries.sf)
# # Subset Brandenburg
# BB.sf <- subset(countries.sf, NAME_1 == "Brandenburg")
# plot(BB.sf$geom)



tiflist_DGM5 <- list.files(datapath("/home/data/raster/BKG_DGM5/tifs/"), full.names = TRUE)
# temp_rasters <- rast(tiflist_DGM5)


# coordinates
  # Read the MoorWald tract infos
  #moorwald.dt <- data.table(read_csv("BWI_Punkte_Brandenburg.xlsx"))

# read coordinates of MoMok plot Referrenzpunkte: 
momok.dt <- read.delim(file = here("data/input/MoMoK/locations_MoMoK_total.csv"), sep = ";", dec = ",")  
momok.dt <- momok.dt %>% select(., c(1, 3, 11, 12)) %>% filter(Skizzenpunkt == "RP")
colnames(momok.dt) <- c("plot_ID", "point_type", "Y", "X")

# create datafram that has just the coordinates
  # https://stackoverflow.com/questions/46664164/r-how-to-transform-epsg-25832-coordinates-into-epsg-4326-ones
  d = as.data.table(momok.dt)
  d = d[,.(as.numeric(X),as.numeric(Y))]
  # project(d$X, d$Y)
  # proj4string(d) <- CRS("+init=epsg:25832") 
  # CRS.new <- CRS("+init=epsg:4326") # WGS 84
  # dnew <- spTransform(d, CRS.new)
  
# create SPatVec with terra
  # https://rdrr.io/cran/terra/man/vect.html
  d <- vect(d, geom=c("V1", "V2"), crs=" +proj=longlat +datum=WGS84", keepgeom=FALSE)
  
# change CSR with terra: 
  # https://search.r-project.org/CRAN/refmans/terra/html/project.html
  project(d, "EPSG:25832")


# Create 300-m circles around each point
moorwald.sf <- st_as_sf(moorwald.dt, coords=c("X_GK3", "Y_GK3"), remove=F, crs=31467)
