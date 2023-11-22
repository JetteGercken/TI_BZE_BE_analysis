# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the  national soil inventory
# forest edges processign for regeneration 

# ----- 0. SETUP ---------------------------------------------------------------

# ----- 0.1. packages and functions --------------------------------------------
source(paste0(getwd(), "/scripts/00_functions_library.R"))

# ----- 0.2. working directory -------------------------------------------------
here::here()
getwd()

out.path.BZE3 <- ("output/out_data/out_data_BZE/") 


# ----- 0.3 data import --------------------------------------------------------
# regeneration
# this dataset contains the plant specific inventory data of the regenertaion inventory of the HBI (BZE2) 
HBI_RG <- read.delim(file = here("data/input/BZE2_HBI/bejb.csv"), sep = ",", dec = ",") %>% mutate(inv_year = 2012)
# this dataset contains the position and extend of the sampling circle satelites of the regeneration inventory of the HBI (BZE2) 
HBI_RG_loc <- read.delim(file = here("data/input/BZE2_HBI/bej.csv"), sep = ",", dec = ",")%>% mutate(inv_year = 2012)
# this dataset contains the HBI forest edges info
HBI_forest_edges <- read.delim(file = here("data/input/BZE2_HBI/be_waldraender.csv"), sep = ";", dec = ",") 
colnames(HBI_forest_edges) <- c("plot_ID", "e_ID", "e_type", "e_form", 
                                "A_dist", "A_azi",  "B_dist", "B_azi", 
                                "T_dist", "T_azi") # t = turning point 

# # import coordinates of polygones along all edges iin triangle shape
all_edge_intersections_coords <- read.delim(file = here(paste0(out.path.BZE3, inv_name(HBI_RG$inv_year[1]), "_all_edges_intersection_coords.csv")), sep = ";", dec = ",")
all_rem_circles_coords <- read.delim(file = here(paste0(out.path.BZE3, inv_name(HBI_RG$inv_year[1]), "_all_rem_circles_coords.csv")), sep = ";", dec = ",")
all_edge_triangles_coords <- read.delim(file = here(paste0(out.path.BZE3, inv_name(HBI_RG$inv_year[1]), "_all_edges_triangle_coords.csv")), sep = ";", dec = ",")
all_areas_stands <- read.delim(file = here(paste0(out.path.BZE3, inv_name(HBI_RG$inv_year[1]), "_all_edges_rem_circles.csv")), sep = ";", dec = ",")


# 0.4 data prep: harmonise strings, assign columnnames etc. ---------------------------------------------------------------------
# assign column names 
                        # bund_nr     pk_nr      pk_richtung     pk_dist     pk_aufnahme      pk_maxdist
colnames(HBI_RG_loc) <- c("plot_ID", "CCS_nr", "CCS_position",  "CCS_dist", "RG_inv_status", "CCS_max_dist_cm", "inv_year")
                    #  "bund_nr"  "pk_nr"  "lfd_nr"   "bart"  "hoehe"    "grklasse"
colnames(HBI_RG) <- c("plot_ID", "CCS_no", "t_ID", "SP_code", "H_cm", "D_class_cm", "inv_year")



# 1. calculations ---------------------------------------------------------
# 1.1. assign gon according to exposition --------------------------------
HBI_RG_loc <- HBI_RG_loc %>% 
  mutate(inv = inv_name(inv_year)) %>% 
  left_join(., HBI_forest_edges %>% select(plot_ID, e_ID, e_form), by = "plot_ID", multiple = "all") %>% 
  mutate(CCS_gon = case_when(CCS_position == "n" ~ 0,
                             CCS_position == "o" ~ 100,
                             CCS_position == "s" ~ 200,
                             CCS_position == "w" ~ 300,
                             CCS_position == "nw" ~ 350,
                             CCS_position == "no" ~ 50,
                             CCS_position == "sw" ~ 250,
                             CCS_position == "so" ~ 150,
                             TRUE ~ NA), 
         # if the max distance of the last plant in the RG CCS is not measured we assume it´s 5m or 500cm
         CCS_max_dist_cm = ifelse(CCS_max_dist_cm == -9 | is.na(CCS_max_dist_cm), 500, CCS_max_dist_cm))





# 2. sorting sampling circles into stands ---------------------------------
# 2.1. plots with 1 edge: sorting sampling circles into stands ---------------------------------

# subsetting the HBI_RG_loc dataset by filtering for plots that have only one intersecting edge
HBI_RG_one_edge <- HBI_RG_loc %>% 
  # filter only for trees that are located in plots with a forest edge
  semi_join(forest_edges_HBI.man %>% filter(e_form == 1 | e_form == 2 & inter_status_AT_17 == "two I" | e_form == 2 & inter_status_BT_17 == "two I") %>% 
              select(plot_ID) %>% distinct(), by = "plot_ID") %>% 
  # filter for trees located in plots htat haev only one forest edge
  anti_join(forest_edges_HBI.man %>%
              #filter(e_form == 1 | e_form == 2 & inter_status_AT_17 == "two I" | e_form == 2 & inter_status_BT_17 == "two I")  %>% 
              select(plot_ID, e_ID) %>% group_by(plot_ID) %>% summarise(n = n()) %>% filter(n > 1) %>% select(plot_ID) %>% distinct(), by = "plot_ID") #%>% 
# remove plots that do now have a corresponding center coordiante in the HBI loc document
#semi_join(HBI_loc %>% filter(!is.na( RW_MED) & !is.na(HW_MED)) %>%  select(plot_ID)  %>% distinct(), by = "plot_ID")


# for each plot_id and regeneration circle at plots with one edge only 
RG.CCS.one.edge <- vector("list", length = nrow(unique(HBI_RG_one_edge[c("plot_ID", "CCS_nr")])))
for (i in 1:nrow(unique(HBI_RG_one_edge[c("plot_ID", "CCS_nr")]))) {
  # i = 206
  # i = which(grepl(50132, unique(HBI_RG_one_edge[c("plot_ID", "CCS_nr")][, "plot_ID"])))
  
  # assign crs
  #my.utm.epsg <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"
  
  # regerneation sampling cirlce data
  my.plot.id <- unique(HBI_RG_one_edge[c("plot_ID", "CCS_nr")])[, "plot_ID"][i]  # plot id of respecctive regereation satelite
  my.ccs.id <- unique(HBI_RG_one_edge[c("plot_ID", "CCS_nr")])[, "CCS_nr"][i]    # circle id7 number of respecctive regereation satelite 
  
  # select edge ID 
  my.e.id <- HBI_RG_one_edge$e_ID[HBI_RG_one_edge$plot_ID == my.plot.id & HBI_RG_one_edge$CCS_nr == my.ccs.id] # edge id of the respective edge, because if we filter for the plot ID it might also pull edges that are double edges but one of them does not intersect
  my.ccs.r <- (HBI_RG_one_edge$CCS_max_dist_cm[HBI_RG_one_edge$plot_ID == my.plot.id & 
                                                 HBI_RG_one_edge$CCS_nr == my.ccs.id & 
                                                 HBI_RG_one_edge$e_ID == my.e.id])/100   # max dist of last plant in the circle to create buffer

  # circle data
  # select UTM coordiantes of BZE (NSI point)
  # my.center.easting <- HBI_loc[HBI_loc$plot_ID == my.plot.id, "RW_MED"]
  # my.center.northing <- HBI_loc[HBI_loc$plot_ID == my.plot.id, "HW_MED"]
  c.x0 = 0 # + my.center.easting
  c.y0 = 0 # + my.center.northing
  c.r3 = 17.84
  
  # determine center corodiantes of the respective regeneration sampling circuit saterilte
  ccs.dist <- HBI_RG_one_edge$CCS_dist[HBI_RG_one_edge$plot_ID == my.plot.id & HBI_RG_one_edge$CCS_nr == my.ccs.id]/100
  ccs.azi <- HBI_RG_one_edge$CCS_gon[HBI_RG_one_edge$plot_ID == my.plot.id & HBI_RG_one_edge$CCS_nr == my.ccs.id]
  x_CCS_center = ccs.dist*sin(ccs.azi * pi/200)  # + my.center.easting
  y_CCS_center = ccs.dist*cos(ccs.azi* pi/200)  # + my.center.northing
  
## create polyones
  # create polygone of regeneration sampling circle RG CCS 
  my.rg.ccs.poly <- sf::st_buffer(
    sf::st_as_sf(as.data.frame(cbind("lon" = x_CCS_center, 
                                     "lat" = y_CCS_center)),
                 coords = c("lon", "lat")), # center point df
    my.ccs.r)                               # radius
  # assing CRS to points
  #sf::st_crs(tree.sf) <- my.utm.epsg
  
  # create polygone of sampling circle
  circle.17 <- sf::st_buffer(
    sf::st_as_sf(as.data.frame(cbind("lon" = c.x0, 
                                     "lat" = c.y0)),
                 coords = c("lon", "lat")), # center point df
    c.r3)                                   # radius of outer 17.84 circle
  # assing CRS to circle
  #sf::st_crs(tree.sf) <- my.utm.epsg
  circle.17$stand <- unique(all_rem_circles_coords$stand[all_rem_circles_coords$plot_ID == my.plot.id])
  circle.17$plot_ID <- my.plot.id
  circle.17$e_ID <- 0
  circle.17$e_form <- 0
  circle.17$CCS_r_m <- c.r3
  
  # create polygone of edge triangle
   edge.poly <- sfheaders::sf_polygon(obj = all_edge_intersections_coords %>% filter(plot_ID == my.plot.id & e_ID == my.e.id) 
                                      , x = "X"
                                      , y = "Y"
                                      , keep = TRUE)
   
  
 # create polygon of remaining circle
   circle.edge.inter <- sf::st_intersection(circle.17, edge.poly)
   # if there is no intersection between the edge and the cirlce, 
    if(isTRUE(nrow(circle.edge.inter) == 0)){
      # the whole circle polygon is set as the remaining circle 
     rem.circle.17 <- circle.17}else{
       # else the remaining area/ polyon after the inersection is deducted is passed on as remaining circle
       rem.circle.17 <- sf::st_difference(circle.17, st_geometry(circle.edge.inter))
     }
   
   
  ## importing the remianing circle polygone directly fro the all_rem_circles_coords.df wouldn not work so well 
   # since we´d have to find a way to first export and then mport and convert multipolygones 
   # accurately, which is to much effort given that we can just wirk with the whole cirlce and the edge-intersections
  # rem.circle.17 <- sfheaders::sf_polygon(obj = all_rem_circles_coords %>% filter(plot_ID == my.plot.id) 
  #                                 , x = "X"
  #                                 , y = "Y"
  #                                 , keep = TRUE)

 ## check for intersections
  # with edge-intersection-polygon
  intersection.with.edge <- sf::st_intersection(my.rg.ccs.poly, edge.poly)
  # with remaining circle polygon
  intersection.with.rem.circle <- sf::st_intersection(my.rg.ccs.poly, rem.circle.17)
  
## set the stand of the rg circle according to its intersections: https://www.geeksforgeeks.org/nested-if-else-statement-in-r/
    my.stand.rg <- list(c(intersection.with.rem.circle$stand, intersection.with.edge$stand))
                         
## determine area of the rg circle (stands) according to it´s intersection
   my.rg.A.m2 <- list(c(st_area(intersection.with.rem.circle), st_area(intersection.with.edge)))
   
 ## safe intersection info (stand and area) of the RG CCS into dataframe
    rg.edge.data <- as.data.frame(cbind(
      "plot_ID" = c(rep(my.plot.id, times = length(unlist(my.stand.rg)))), 
      "CCS_nr" = c(rep(my.ccs.id, times = length(unlist(my.stand.rg)))), 
      "stand" =  c(unlist(my.stand.rg)), 
      "area_m2"= c(unlist(my.rg.A.m2)) 
    ))
    
  ## assign the whole CCS area to the stand that covers 2/3rds of the RG CCS area
    # determine 2/3 of the RG CCS area 
    rg.ccs.A.0.6 <- sf::st_area(my.rg.ccs.poly)*(2/3)
    
    # select the row that includes the stand that covers 2/3 of the RG CCS area, 
    # by filtering the area fot bigger/ equal 2/3 of the total RG CCS area 
    rg.0.6.data <- rg.edge.data[rg.edge.data$area_m2 >= rg.ccs.A.0.6, ]
    # as we cannot localise the plants in the cirlce, we cannot adjust the refference area (Bezugsfläche) according to the are covered by the respective stand
    # thus the whole are of the RG CCS is allocated to the stand that covers most of it´s area, as all plants included in the respective RG CCS are also allocated to this stand
    # since we cannot sort them into stands by location as we don´t know their location
    
    if(isTRUE(nrow(rg.0.6.data)== 0)){
      rg.edge.data[1,]$stand <- NA
      rg.edge.data$area_m2  <- sf::st_area(my.rg.ccs.poly)
    }else{
      rg.edge.data <- rg.0.6.data
      rg.edge.data$area_m2  <- sf::st_area(my.rg.ccs.poly)
    }
    
  
## put dataframe in export list
  RG.CCS.one.edge[[i]] <- rg.edge.data

  #print(my.plot.id)
  
  print(ggplot() +
          geom_sf(data = ( sf::st_as_sf(as.data.frame(cbind("lon" = c.x0, 
                                                            "lat" = c.y0)),
                                        coords = c("lon", "lat"))), aes(),fill = NA)+
          geom_sf(data = ( sf::st_as_sf(as.data.frame(cbind("lon" = x_CCS_center, 
                                                           "lat" = y_CCS_center)),
                                       coords = c("lon", "lat"))), aes(),fill = NA)+
         
           geom_sf(data = rem.circle.17, aes(colour = stand),fill = NA)+
           geom_sf(data = edge.poly, aes(colour = stand), fill = NA)+
          geom_sf(data = my.rg.ccs.poly, colour = "black", fill = NA)+
          ggtitle(my.plot.id, my.ccs.id)
         )
  }
# bind areas and stands in one dataframe with plot_ID, CCS_nr to join stand & area info into HBI_RG dataset later      
RG.one.edge.stands.areas <- as.data.frame(rbindlist(RG.CCS.one.edge))




# 2.2. plots with 2 edges: sorting sampling circles into stands ---------------------------------
# subsetting data
HBI_RG_two_edges <- HBI_RG_loc %>% 
  # filter only for trees that are located in plots with a forest edge
  semi_join(forest_edges_HBI.man %>% filter(e_form == 1 | e_form == 2 & inter_status_AT_17 == "two I" | e_form == 2 & inter_status_BT_17 == "two I") %>% 
              select(plot_ID) %>% distinct(), by = "plot_ID") %>% 
  # filter for trees located in plots htat haev only one forest edge
  semi_join(forest_edges_HBI.man %>%
              filter(e_form == 1 | e_form == 2 & inter_status_AT_17 == "two I" | e_form == 2 & inter_status_BT_17 == "two I")  %>% 
              select(plot_ID, e_ID) %>% group_by(plot_ID) %>% summarise(n = n()) %>% filter(n > 1) %>% select(plot_ID) %>% distinct(), by = "plot_ID") #%>% 
# remove plots that do now have a corresponding center coordiante in the HBI loc document
# semi_join(HBI_loc %>% filter(!is.na( RW_MED) & !is.na(HW_MED)) %>%  select(plot_ID)  %>% distinct(), by = "plot_ID")


# for each plot_id and regeneration circle at plots with one edge only 
RG.CCS.two.edges <- vector("list", length = nrow(unique(HBI_RG_two_edges[c("plot_ID", "CCS_nr")])))
for (i in 1:nrow(unique(HBI_RG_two_edges[c("plot_ID", "CCS_nr")]))) {
  # i = 34
  # i = which(grepl(50132, unique(HBI_RG_two_edges[c("plot_ID", "CCS_nr")][, "plot_ID"])))
  
  # assign crs
  # my.utm.epsg <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"
  
  # regerneation sampling cirlce data
  my.plot.id <- unique(HBI_RG_two_edges[c("plot_ID", "CCS_nr")])[, "plot_ID"][i]  # plot id of respecctive regereation satelite
  my.ccs.id <- unique(HBI_RG_two_edges[c("plot_ID", "CCS_nr")])[, "CCS_nr"][i]    # circle id7 number of respecctive regereation satelite 
  
  # select edge ID 
  my.e.id.1 <- HBI_RG_two_edges$e_ID[HBI_RG_two_edges$plot_ID == my.plot.id & HBI_RG_two_edges$CCS_nr == my.ccs.id &  HBI_RG_two_edges$e_ID == 1] # edge id of the respective edge, because if we filter for the plot ID it might also pull edges that are double edges but one of them does not intersect
  my.e.id.2 <- HBI_RG_two_edges$e_ID[HBI_RG_two_edges$plot_ID == my.plot.id & HBI_RG_two_edges$CCS_nr == my.ccs.id &  HBI_RG_two_edges$e_ID == 2] # edge id of the respective edge, because if we filter for the plot ID it might also pull edges that are double edges but one of them does not intersect
  
  # circle data
  # select UTM coordiantes of BZE (NSI point)
  # my.center.easting <- HBI_loc[HBI_loc$plot_ID == my.plot.id, "RW_MED"]
  # my.center.northing <- HBI_loc[HBI_loc$plot_ID == my.plot.id, "HW_MED"]
  c.x0 = 0 # + my.center.easting
  c.y0 = 0 # + my.center.northing
  c.r3 = 17.84
  
  # spatial data of RG sampling circle
  # select regeneration sampling circuit radius 
  my.ccs.r <- (HBI_RG_two_edges$CCS_max_dist_cm[HBI_RG_two_edges$plot_ID == my.plot.id & 
                                                  HBI_RG_two_edges$CCS_nr == my.ccs.id & 
                                                  HBI_RG_two_edges$e_ID == my.e.id])/100   # max dist of last plant in the circle to create buffer
  # determine center corodiantes of the respective regeneration sampling circuit saterilte
  ccs.dist <-unique( HBI_RG_two_edges$CCS_dist[HBI_RG_two_edges$plot_ID == my.plot.id & HBI_RG_two_edges$CCS_nr == my.ccs.id]/100)
  ccs.azi <- unique(HBI_RG_two_edges$CCS_gon[HBI_RG_two_edges$plot_ID == my.plot.id & HBI_RG_two_edges$CCS_nr == my.ccs.id])
  x_CCS_center = ccs.dist*sin(ccs.azi* pi/200)  # + my.center.easting
  y_CCS_center = ccs.dist*cos(ccs.azi* pi/200)  # + my.center.northing
  
  ## create polyones
  # create polygone of regeneration sampling circle RG CCS 
  my.rg.ccs.poly <- sf::st_buffer(
    sf::st_as_sf(as.data.frame(cbind("lon" = x_CCS_center, 
                                     "lat" = y_CCS_center)),
                 coords = c("lon", "lat")), # center point df
    my.ccs.r)                               # radius
  # assing CRS to points
  #sf::st_crs(my.rg.ccs.poly) <- my.utm.epsg
  
  # create polygone of sampling circle
  circle.17 <- sf::st_buffer(
    sf::st_as_sf(as.data.frame(cbind("lon" = c.x0, 
                                     "lat" = c.y0)),
                 coords = c("lon", "lat")), # center point df
    c.r3)                                   # radius of outer 17.84 circle
  # assing CRS to circle
  #sf::st_crs(circle.17) <- my.utm.epsg
  circle.17$stand <- unique(all_rem_circles_coords$stand[all_rem_circles_coords$plot_ID == my.plot.id])
  circle.17$plot_ID <- my.plot.id
  circle.17$e_ID <- 0
  circle.17$e_form <- 0
  circle.17$CCS_r_m <- c.r3
  
  # create polygone of edge 1 triangle
  edge.poly.1 <- sfheaders::sf_polygon(obj = all_edge_intersections_coords %>% filter(plot_ID == my.plot.id & e_ID == my.e.id.1) 
                                     , x = "X"
                                     , y = "Y"
                                     , keep = TRUE)
  
  # create polygone of edge 2 triangle
  edge.poly.2 <- sfheaders::sf_polygon(obj = all_edge_intersections_coords %>% filter(plot_ID == my.plot.id & e_ID == my.e.id.2) 
                                       , x = "X"
                                       , y = "Y"
                                       , keep = TRUE)
  
  ## create polygon of remaining circle after circle-edge.1 intersection
  circle.edge.1.inter <- sf::st_intersection(circle.17, edge.poly.1)
  # if there is no intersection between the edge and the cirlce, 
  if(isTRUE(nrow(circle.edge.1.inter) == 0)){
    # the whole circle polygon is set as the remaining circle 
    rem.circle.17.1 <- circle.17}else{
      # else the remaining area/ polyon after the inersection is deducted is passed on as remaining circle
      rem.circle.17.1 <- sf::st_difference(circle.17, st_geometry(circle.edge.1.inter))
    }

  ## create polygon of remaining circle after circle-edge.2 intersection
  circle.edge.2.inter <- sf::st_intersection(rem.circle.17.1, edge.poly.2)
  # if there is no intersection between the edge and the cirlce, 
  if(isTRUE(nrow(circle.edge.2.inter) == 0)){
    # the previous remaining circle polygon is set as the remaining circle 
    rem.circle.17.2 <- rem.circle.17.1}else{
      # else the remaining area/ polyon after the inersection is deducted is passed on as remaining circle
      rem.circle.17.2 <- sf::st_difference(rem.circle.17.1, st_geometry(circle.edge.2.inter))
    }
  
  
  ## check for intersections
  # with edge-intersection-polygon
  intersection.with.edge.1 <- sf::st_intersection(my.rg.ccs.poly, edge.poly.1)
  intersection.with.edge.2 <- sf::st_intersection(my.rg.ccs.poly, edge.poly.2)
  # with remaining circle polygon
  intersection.with.rem.circle <- sf::st_intersection(my.rg.ccs.poly, rem.circle.17.2)
  
  
  ## set the stand of the rg circle according to its intersections: https://www.geeksforgeeks.org/nested-if-else-statement-in-r/
  my.stand.rg <- list(c(intersection.with.rem.circle$stand, intersection.with.edge.1$stand, intersection.with.edge.2$stand))
  ## determine area of the rg circle (stands) according to it´s intersection
  my.rg.A.m2 <-  list(c(st_area(intersection.with.rem.circle), st_area(intersection.with.edge.1), st_area(intersection.with.edge.2)))
  
  
  ## safe intersection info (stand and area) of the RG CCS into dataframe
  rg.edge.data <- as.data.frame(cbind(
    "plot_ID" = c(rep(my.plot.id, times = length(unlist(my.stand.rg)))), 
    "CCS_nr" = c(rep(my.ccs.id, times = length(unlist(my.stand.rg)))), 
    "stand" =  c(unlist(my.stand.rg)), 
    "area_m2"= c(unlist(my.rg.A.m2)) 
  ))
  
  ## assign the whole CCS area to the stand that covers 2/3rds of the RG CCS area
  # determine 2/3 of the RG CCS area 
  rg.ccs.A.0.6 <- sf::st_area(my.rg.ccs.poly)*(2/3)
  
  # select the row that includes the stand that covers 2/3 of the RG CCS area, 
  # by filtering the area fot bigger/ equal 2/3 of the total RG CCS area 
  rg.0.6.data <- (rg.edge.data[rg.edge.data$area_m2 >= rg.ccs.A.0.6, ] %>% arrange(., desc(area_m2)))[1,]
  # as we cannot localise the plants in the cirlce, we cannot adjust the refference area (Bezugsfläche) according to the are covered by the respective stand
  # thus the whole are of the RG CCS is allocated to the stand that covers most of it´s area, as all plants included in the respective RG CCS are also allocated to this stand
  # since we cannot sort them into stands by location as we don´t know their location
  
  if(isTRUE(nrow(rg.0.6.data)== 0)){
    rg.edge.data[1,]$stand <- NA
    rg.edge.data$area_m2  <- sf::st_area(my.rg.ccs.poly)
  }else{
    rg.edge.data <- rg.0.6.data
    rg.edge.data$area_m2  <- sf::st_area(my.rg.ccs.poly)
  }
  
  
  ## put dataframe in export list
  RG.CCS.two.edges[[i]] <- rg.edge.data
  
  #print(my.plot.id)
  
  print(ggplot() +
          geom_sf(data = rem.circle.17.2, aes(colour = stand),fill = NA)+
          geom_sf(data = edge.poly.1, aes(colour = stand), fill = NA)+
          geom_sf(data = edge.poly.2, aes(colour = stand), fill = NA)+
          geom_sf(data = my.rg.ccs.poly, colour = "black", fill = NA)+
          ggtitle(my.plot.id, my.ccs.id)
  )
  
}
# bind areas and stands in one dataframe with plot_ID, CCS_nr to join stand & area info into HBI_RG dataset later      
RG.two.edges.stands.areas <- as.data.frame(rbindlist(RG.CCS.one.edge))




# 3. export RG data ----------------------------------------------------------

# 3.1. preparing data for export -----------------------------------------------

## bind stand and area of plots with one ad two edges together 
RG_all_edges_stands_areas <- rbind(RG.one.edge.stands.areas, RG.two.edges.stands.areas)

## harmonizig strings with HBI_RG datasets 
RG_all_edges_stands_areas[,c(1,2)] <- lapply(RG_all_edges_stands_areas[,c(1,2)], as.integer) 

## joining stand and area info into RG datasets
  # join stand and area data into dataset with regeneration CCS locations and size 
  # details (HBI_RG_loc) as well as into regeneration individual plant info dataset (HBI_RG)
# HBI_RG_loc update
HBI_RG_loc_update_1 <- HBI_RG_loc %>% left_join(.,RG_all_edges_stands_areas, by = c("plot_ID", "CCS_nr"), multiple = "all") 
# HBI_RG update
HBI_RG_update_1 <- HBI_RG %>% left_join(., RG_all_edges_stands_areas, by = c("plot_ID", c("CCS_no" = "CCS_nr")), multiple = "all") 


# 3.2. export  ------------------------------------------------------------
write.csv2(HBI_RG_loc_update_1, paste0(out.path.BZE3, paste(unique(HBI_RG_update_1$inv)[1], "RG_loc_update_1", sep = "_"), ".csv"))
write.csv2(HBI_RG_update_1, paste0(out.path.BZE3, paste(unique(HBI_RG_update_1$inv)[1], "RG_update_1", sep = "_"), ".csv"))




# notes -------------------------------------------------------------------




