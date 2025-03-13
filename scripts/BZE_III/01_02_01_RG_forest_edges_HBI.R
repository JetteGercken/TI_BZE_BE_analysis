# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the  national soil inventory
# forest edges processign for regeneration 

# ----- 0. SETUP ---------------------------------------------------------------
# dev.off()
# ----- 0.1. packages and functions --------------------------------------------
source(paste0(getwd(), "/scripts/01_00_functions_library.R"))

# ----- 0.2. working directory -------------------------------------------------
here::here()
getwd()


out.path.BZE3 <- paste0(getwd(), "/output/out_data/out_data_BZE/") 


# ----- 0.3 data import --------------------------------------------------------
# regeneration                                                                                                   inv = inv_name(inv_year))
# this dataset contains the position and extend of the sampling circle satelites of the regeneration inventory of the BZE3 (BZE2) 
RG_loc <- read.delim(file = paste0(out.path.BZE3, "HBI_RG_loc_update_1.csv"), sep = ",", dec = ".") 

# this dataset contains the plant specific inventory data of the regenertaion inventory of the BZE3 (BZE2), including stand and area info
RG_data <- read.delim(file =  paste0(out.path.BZE3, inv_name((RG_loc$inv_year)[1]), "_RG_update_1.csv"), sep = ",", dec = ".")

# this dataset contains the BZE3 forest edges info
forest_edges <- read.delim(file = paste0(out.path, inv_name((RG_loc$inv_year)[1]), "_forest_edges_update_1.csv"), sep = ",", dec = ".")

colnames(forest_edges) <- c("plot_ID", "e_ID", "e_type", "e_form", 
                            "A_dist", "A_azi",  "B_dist", "B_azi", 
                            "T_dist", "T_azi") # t = turning point 
# HBI BE locations dataset: this dataset contains the coordinates of the center point of the tree inventory accompanying the second national soil inventory
HBI_loc <- read.delim(file = paste0(getwd(), "/data/input/BZE2_HBI/location_",  inv_name((RG_loc$inv_year)[1]), ".csv"), sep = ",", dec = ".")

# HBI locations
HBI_loc <- HBI_loc[1:3] 
colnames(HBI_loc) <- c("plot_ID",  "RW_MED", "HW_MED") 




# import coordinates of polygones along all edges iin triangle shape based on inv of RG dataset -----------------------------------------------------------------------------------------------
all_edge_intersections_coords <- read.delim(file = paste0(out.path.BZE3, inv_name(RG_loc$inv_year[1]), "_all_edges_intersection_coords.csv"), sep = ",", dec = ".")
all_rem_circles_coords <- read.delim(file = paste0(out.path.BZE3, inv_name(RG_loc$inv_year[1]), "_all_rem_circles_coords.csv"), sep = ",", dec = ".")
all_edge_triangles_coords <- read.delim(file = paste0(out.path.BZE3, inv_name(RG_loc$inv_year[1]), "_all_edges_triangle_coords.csv"), sep = ",", dec = ".")
all_areas_stands <- read.delim(file = paste0(out.path.BZE3, inv_name(RG_loc$inv_year[1]), "_all_edges_rem_circles.csv"), sep = ",", dec = ".")



# creating dataset with information about the concentric sampling circles
data_circle <- data.frame(x0 = c(0,0,0),       # x of centre point of all 3 circles is 0 
                          y0 = c(0,0,0),       # y of centre point of all 3 circles is 0 
                          r0 = c(5.64, 12.62, 17.84), # darius in m
                          rmax = c(30.00, 30.00, 30.00)) # these are the radi of the sampling circuits in m

# 0.4 data prep: harmonise strings, assign columnnames etc. ---------------------------------------------------------------------
# calcualte edge data: cooridnates of edges and intersection stati of the edge lines
forest_edges <- forest_edges %>% 
  filter(e_form %in% c("1", "2")) %>% 
  # convert distance from cm to m
  mutate(across(c("A_dist", "B_dist", "T_dist"), ~ (.x)/100)) %>% 
  # find line parameters
  # 1. calculate x and y coordinates for all edge points
  mutate(X_A = ifelse(A_azi != "-2", coord(data_circle$x0[1], data_circle$y0[1], A_dist, A_azi, coordinate = "x"), NA), # if the value is marked -2 its equal to an NA
         X_B = ifelse(B_azi != "-2", coord(data_circle$x0[1], data_circle$y0[1], B_dist, B_azi, coordinate = "x"), NA),
         X_T = ifelse(T_azi != "-2", coord(data_circle$x0[1], data_circle$y0[1], T_dist, T_azi, coordinate = "x"), NA),
         Y_A = ifelse(A_azi != "-2", coord(data_circle$x0[1], data_circle$y0[1], A_dist, A_azi, coordinate = "y"), NA), # if the value is marked -2 its equal to an NA
         Y_B = ifelse(B_azi != "-2", coord(data_circle$x0[1], data_circle$y0[1], B_dist, B_azi, coordinate = "y"), NA),
         Y_T = ifelse(T_azi != "-2", coord(data_circle$x0[1], data_circle$y0[1], T_dist, T_azi, coordinate = "y"), NA)) %>% 
  # 2. calcualte slope ß1 = (y2-y1)/(x2-x1) hight/width
  mutate(b1_AB = ifelse(e_form == "1", slope(X_A, Y_A, X_B, Y_B), NA), 
         b1_AT = ifelse(e_form == "2", slope(X_T, Y_T, X_A, Y_A), NA),
         b1_BT = ifelse(e_form == "2", slope(X_T, Y_T, X_B, Y_B), NA)) %>% 
  # 3. intercept of line with y-axis b0 : insert known point: XA YA
  # Y_A = b1_AB*X_A + b0_AB -- -b1_AB*X_A --> b0_AB =  Y_A - b1_AB*X_A
  mutate(b0_AB = ifelse(e_form == "1", intercept(X_A, Y_A,  X_B, Y_B), NA), 
         b0_AT = ifelse(e_form == "2", intercept(X_T, Y_T, X_A, Y_A), NA),
         b0_BT = ifelse(e_form == "2", intercept(X_T, Y_T, X_B, Y_B), NA)) %>% 
  ### 17m circle --> used for tree status also   
  # find x coordinate of the interception between line and 17.84m circle: insert line equation in circle equation (function: intersection_line_circle)
  # for AB line 
  mutate(X1_inter_AB_17 = intersection_line_circle(b0_AB, b1_AB,  X_A, X_B, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3], coordinate="x1"),
         X2_inter_AB_17 = intersection_line_circle(b0_AB, b1_AB, X_A, X_B, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3],  coordinate="x2"), 
         inter_status_AB_17 = intersection.status(intersection_line_circle(b0_AB, b1_AB,  X_A, X_B, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3], coordinate="x1"),
                                                  intersection_line_circle(b0_AB, b1_AB, X_A, X_B, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3],  coordinate="x2")),
         # for AT line
         X1_inter_AT_17 = intersection_line_circle(b0_AT, b1_AT, X_A, X_T, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3], coordinate="x1"),
         X2_inter_AT_17 = intersection_line_circle(b0_AT, b1_AT,X_A, X_T, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3], coordinate="x2"), 
         inter_status_AT_17 = intersection.status(intersection_line_circle(b0_AT, b1_AT,X_A, X_T, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3], coordinate="x1"), 
                                                  intersection_line_circle(b0_AT, b1_AT, X_A, X_T, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3], coordinate="x2")),
         # for BT line
         X1_inter_BT_17 = intersection_line_circle(b0_BT, b1_BT, X_B, X_T, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3],  coordinate="x1"),
         X2_inter_BT_17 = intersection_line_circle(b0_BT, b1_BT, X_B, X_T, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3],  coordinate="x2"), 
         inter_status_BT_17 = intersection.status(intersection_line_circle(b0_BT, b1_BT, X_B, X_T, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3],  coordinate="x1"), 
                                                  intersection_line_circle(b0_BT, b1_BT, X_B, X_T, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3],  coordinate="x2")))





# 1. calculations --------------------------------------------------------------------------------------------------------------------------------------------------------
# 1.1. assign gon according to exposition --------------------------------------------------------------------------------------------------------------------------------
RG_loc <- RG_loc %>% 
  # relationship = "many-to-many" because there are multiple edges per plot in forest edges but also multiple CCS in RG loc
  left_join(., forest_edges %>% select(plot_ID, e_ID, e_form), by = "plot_ID", relationship = "many-to-many") %>% 
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
## subsetting the RG_loc dataset by filtering for plots that have only one intersecting edge
# we have an issue here because RG_loc and by that RG.one.edge too, where plots do only have one relevant edge, but the filter also 
# takes the edge that´s not relevant. e.g. 140058 has 2 edges whereby one doesn´t intersect. as we pull single-edge stands 
# by the plot ID, however, we pull both edges here, also the one that´s not relevant. we have to adjust this
RG_one_edge <-   
  RG_loc %>% 
  # filter for plots that we actually have coordiantes and areas for 
  semi_join(., all_areas_stands %>%
               filter(CCS_r_m == 17.84 &  # select only outer circle
                        e_ID %in% c(1, 2) & # select only intersection polys, not remaining cirlces
                        inter_stat !=  "no intersections") %>%   # select only edges that somehow intersect the circle 
               select(plot_ID) %>% 
               distinct(), 
             by = c("plot_ID")) %>%
  # filter for plots that have only one forest edge
   anti_join(., all_areas_stands %>%
               filter(CCS_r_m == 17.84 & 
                        e_ID %in% c("1", "2")) %>% 
               select(plot_ID, e_ID) %>% 
               group_by(plot_ID) %>% 
               summarise(n = n()) %>% 
               filter(n > 1 ) %>% 
               select(plot_ID) %>% 
               distinct(), 
             by = "plot_ID") # %>% 
## remove plots that do now have a corresponding center coordiante in the BZE3 loc document
#semi_join(HBI_loc %>% filter(!is.na( RW_MED) & !is.na(HW_MED)) %>%  select(plot_ID)  %>% distinct(), by = "plot_ID")

# for each plot_id and regeneration circle at plots with one edge only 
RG.CCS.one.edge.list <- vector("list", length = nrow(unique(RG_one_edge[c("plot_ID", "CCS_nr")])))
for (i in 1:nrow(unique(RG_one_edge[c("plot_ID", "CCS_nr")]))) {
  # i = 29
  # i = which(grepl(140058, unique(RG_one_edge[c("plot_ID", "CCS_nr")][, "plot_ID"])))
  
  
  # regerneation sampling cirlce data
  my.plot.id <- unique(RG_one_edge[c("plot_ID", "CCS_nr")])[, "plot_ID"][i]  # plot id of respecctive regereation satelite
  my.ccs.id <- unique(RG_one_edge[c("plot_ID", "CCS_nr")])[, "CCS_nr"][i]    # circle id7 number of respecctive regereation satelite 
  
  # select edge ID 
  #my.e.id <-  RG_one_edge$e_ID[ RG_one_edge$plot_ID == my.plot.id &  RG_one_edge$CCS_nr == my.ccs.id] # edge id of the respective edge, because if we filter for the plot ID it might also pull edges that are double edges but one of them does not intersect
  my.ccs.r <- unique(( RG_one_edge$CCS_max_dist_cm[ RG_one_edge$plot_ID == my.plot.id & 
                                               RG_one_edge$CCS_nr == my.ccs.id
                                             #& RG_one_edge$e_ID == my.e.id
                                             ]
                ))/100   # max dist of last plant in the circle to create buffer
  
  ## select georefference data
  ## select UTM coordiantes of BZE (NSI point)
  # my.center.easting <- HBI_loc[HBI_loc$plot_ID == my.plot.id, "RW_MED"]
  # my.center.northing <- HBI_loc[HBI_loc$plot_ID == my.plot.id, "HW_MED"]
  ## assign crs
  # my.utm.epsg <-  paste0("+proj=utm +zone=", pick_utm(my.center.easting)," ", "+datum=WGS84 +units=m +no_defs +type=crs")
  
  
  # circle data
  c.x0 = 0 # + my.center.easting
  c.y0 = 0 # + my.center.northing
  c.r3 = 17.84
  
  # determine center corodiantes of the respective regeneration sampling circuit saterilte
  ccs.dist <-  unique(RG_one_edge$CCS_dist[ RG_one_edge$plot_ID == my.plot.id &  RG_one_edge$CCS_nr == my.ccs.id]/100) 
  ccs.azi <-  unique(RG_one_edge$CCS_gon[ RG_one_edge$plot_ID == my.plot.id &  RG_one_edge$CCS_nr == my.ccs.id])
  x_CCS_center = ccs.dist*sin(ccs.azi * pi/200)  # + my.center.easting
  y_CCS_center = ccs.dist*cos(ccs.azi* pi/200)   # + my.center.northing
  
  ## create polyones
  # create polygone of regeneration sampling circle RG CCS 
  my.rg.ccs.poly <- sf::st_buffer(
    sf::st_as_sf(as.data.frame(cbind("lon" = x_CCS_center, 
                                     "lat" = y_CCS_center)),
                 coords = c("lon", "lat")), # center point df
    my.ccs.r)                               # radius
  ##  assing CRS to points
  #sf::st_crs(my.rg.ccs.poly) <- my.utm.epsg
  
  # create polygone of sampling circle
  # circle.17 <- sf::st_buffer(
  #   sf::st_as_sf(as.data.frame(cbind("lon" = c.x0, 
  #                                    "lat" = c.y0)),
  #                coords = c("lon", "lat")), # center point df
  #   c.r3)                                   # radius of outer 17.84 circle
  # ## assing CRS to circle
  # #sf::st_crs(circle.17) <- my.utm.epsg
  # circle.17$stand <- unique(all_rem_circles_coords$stand[all_rem_circles_coords$plot_ID == my.plot.id])
  # circle.17$plot_ID <- my.plot.id
  # circle.17$e_ID <- 0
  # circle.17$e_form <- 0
  # circle.17$CCS_r_m <- c.r3
  
  # create polygone of edge triangle
  edge.poly <- sfheaders::sf_polygon(obj = all_edge_intersections_coords %>% filter(plot_ID == my.plot.id) 
                                     , x = "X"
                                     , y = "Y"
                                     , keep = TRUE)
  edge.poly <- sf::st_buffer(edge.poly, 0)
  
  # create polygon of remaining circle
  # circle.edge.inter <- sf::st_intersection(edge.poly, st_geometry(circle.17)) # we should set the circle to st_geometry here so we can assure that the inter polygone maintains its stand 
  # # if there is no intersection between the edge and the cirlce, 
  # if(isTRUE(nrow(circle.edge.inter) == 0) == TRUE){
  #   # the whole circle polygon is set as the remaining circle 
  #   rem.circle.17 <- circle.17}else{
  #     # else the remaining area/ polyon after the inersection is deducted is passed on as remaining circle
  #     rem.circle.17 <- sf::st_difference(circle.17, st_geometry(circle.edge.inter))
  #   }
  
  
  ## importing the remianing circle polygone directly fro the all_rem_circles_coords.df wouldn not work so well 
  # since we´d have to find a way to first export and then mport and convert multipolygones 
  # accurately, which is to much effort given that we can just wirk with the whole cirlce and the edge-intersections
  rem.circle.17 <- sfheaders::sf_polygon(obj = all_rem_circles_coords %>% filter(plot_ID == my.plot.id) 
                                         , x = "X"
                                         , y = "Y"
                                         , keep = TRUE)
  rem.circle.17 <- sf::st_buffer(rem.circle.17, 0)
  
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
  rg.0.6.data <- rg.edge.data[as.numeric(rg.edge.data$area_m2) >= rg.ccs.A.0.6, ]
  # as we cannot localise the plants in the cirlce, we cannot adjust the refference area (Bezugsfläche) according to the are covered by the respective stand
  # thus the whole are of the RG CCS is allocated to the stand that covers most of it´s area, as all plants included in the respective RG CCS are also allocated to this stand
  # since we cannot sort them into stands by location as we don´t know their location
  
  if(isTRUE(nrow(rg.0.6.data)== 0)){
    # if there is no part of the RG CCS that has min 2/3 of its area covered by 1 stand, 
    # then only select the first row of the rg dataset, assin the stand to NA and the area to the whole RG CCS area
    rg.edge.data <- rg.edge.data[1,]
    rg.edge.data[1,]$stand <- NA
    rg.edge.data[1,]$area_m2  <- sf::st_area(my.rg.ccs.poly)
  }else{
    # if there is a part of the RG CCS that haas min 2/3 of its area covered by 1 stand, 
    # then select this part and arssing the area to the whole RG CCS area
    rg.edge.data <- rg.0.6.data
    rg.edge.data$area_m2  <- sf::st_area(my.rg.ccs.poly)
  }
  
  
  ## put dataframe in export list
  RG.CCS.one.edge.list[[i]] <- rg.edge.data
  
  print(paste(i, my.plot.id, my.ccs.id, sep = " "))
  
 # try( print(ggplot() +
 #          geom_sf(data = ( sf::st_as_sf(as.data.frame(cbind("lon" = c.x0, 
 #                                                            "lat" = c.y0)),
 #                                        coords = c("lon", "lat"))), aes(),fill = NA)+
 #          geom_sf(data = ( sf::st_as_sf(as.data.frame(cbind("lon" = x_CCS_center, 
 #                                                            "lat" = y_CCS_center)),
 #                                        coords = c("lon", "lat"))), aes(),fill = NA)+
 #          geom_sf(data = rem.circle.17, aes(colour = stand),fill = NA)+
 #          geom_sf(data = edge.poly, aes(colour = stand), fill = NA)+
 #          geom_sf(data = my.rg.ccs.poly, aes(colour = rg.edge.data$stand), fill = NA)+
 #          ggtitle(my.plot.id, my.ccs.id)+ 
 #          xlim(-60,60)+
 #          ylim(-60,60), silent = T)
 # )
  
}
# bind areas and stands in one dataframe with plot_ID, CCS_nr to join stand & area info into  RG dataset later      
RG_one_edge_stands_areas <- as.data.frame(rbindlist(RG.CCS.one.edge.list))




# 2.2. plots with 2 edges: sorting sampling circles into stands ---------------------------------
# subsetting data
RG_two_edges <- RG_loc %>% 
  #filter for plots that we actually have coordiantes and areas for all edges 
  semi_join(., all_areas_stands %>%
              filter(CCS_r_m == 17.84 & 
                       e_ID %in% c(1, 2) &
                       inter_stat != "no intersections") %>% 
              select(plot_ID, e_ID) %>% 
              distinct(), 
            by = c("plot_ID")) %>% 
  # silter for plots that have only one forest edge
  semi_join(., all_areas_stands %>%
              filter(CCS_r_m == 17.84 & 
                       e_ID %in% c("1", "2")) %>% 
              select(plot_ID, e_ID) %>% 
              group_by(plot_ID) %>% 
              summarise(n = n()) %>% 
              filter(n > 1 ) %>% 
              select(plot_ID) %>% 
              distinct(), 
            by = "plot_ID")# %>% 
## remove plots that do now have a corresponding center coordiante in the HBI loc document
# semi_join(HBI_loc %>% filter(!is.na( RW_MED) & !is.na(HW_MED)) %>%  select(plot_ID)  %>% distinct(), by = "plot_ID")


# for each plot_id and regeneration circle at plots with one edge only 
RG.CCS.two.edges.list <- vector("list", length = nrow(unique(RG_two_edges[c("plot_ID", "CCS_nr")])))
for (i in 1:nrow(unique( RG_two_edges[c("plot_ID", "CCS_nr")]))) {
  # i = 164
  # i = which(grepl(50132, unique( RG_two_edges[c("plot_ID", "CCS_nr")][, "plot_ID"])))
  
  # assign crs
  # my.utm.epsg <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"
  
  # regerneation sampling cirlce data
  my.plot.id <- unique( RG_two_edges[c("plot_ID", "CCS_nr")])[, "plot_ID"][i]  # plot id of respecctive regereation satelite
  my.ccs.id <- unique( RG_two_edges[c("plot_ID", "CCS_nr")])[, "CCS_nr"][i]    # circle id7 number of respecctive regereation satelite 
  
  # select edge ID 
  my.e.id.1 <-  RG_two_edges$e_ID[ RG_two_edges$plot_ID == my.plot.id &  RG_two_edges$CCS_nr == my.ccs.id &   RG_two_edges$e_ID == 1] # edge id of the respective edge, because if we filter for the plot ID it might also pull edges that are double edges but one of them does not intersect
  my.e.id.2 <-  RG_two_edges$e_ID[ RG_two_edges$plot_ID == my.plot.id &  RG_two_edges$CCS_nr == my.ccs.id &   RG_two_edges$e_ID == 2] # edge id of the respective edge, because if we filter for the plot ID it might also pull edges that are double edges but one of them does not intersect
  
  ## select georefference data
  ## select UTM coordiantes of BZE (NSI point)
  # my.center.easting <- HBI_loc[HBI_loc$plot_ID == my.plot.id, "RW_MED"]
  # my.center.northing <- HBI_loc[HBI_loc$plot_ID == my.plot.id, "HW_MED"]
  ## assign crs
  # my.utm.epsg <-  paste0("+proj=utm +zone=", pick_utm(my.center.easting)," ", "+datum=WGS84 +units=m +no_defs +type=crs")
  
  
  # circle data
  c.x0 = 0 # + my.center.easting
  c.y0 = 0 # + my.center.northing
  c.r3 = 17.84
  
  # spatial data of RG sampling circle
  # select regeneration sampling circuit radius 
  my.ccs.r <- unique(( RG_two_edges$CCS_max_dist_cm[ RG_two_edges$plot_ID == my.plot.id & 
                                                RG_two_edges$CCS_nr == my.ccs.id])/100)   # max dist of last plant in the circle to create buffer
  # determine center corodiantes of the respective regeneration sampling circuit saterilte
  ccs.dist <- unique(  RG_two_edges$CCS_dist[ RG_two_edges$plot_ID == my.plot.id &  RG_two_edges$CCS_nr == my.ccs.id]/100)
  ccs.azi <- unique( RG_two_edges$CCS_gon[ RG_two_edges$plot_ID == my.plot.id &  RG_two_edges$CCS_nr == my.ccs.id])
  x_CCS_center = ccs.dist*sin(ccs.azi* pi/200)  # + my.center.easting
  y_CCS_center = ccs.dist*cos(ccs.azi* pi/200)  # + my.center.northing
  
  ## create polyones
  # create polygone of regeneration sampling circle RG CCS 
  my.rg.ccs.poly <- sf::st_buffer(
    sf::st_as_sf(as.data.frame(cbind("lon" = x_CCS_center, 
                                     "lat" = y_CCS_center)),
                 coords = c("lon", "lat")), # center point df
    my.ccs.r)                               # radius
  ## assign CRS to RG circle
  #sf::st_crs(my.rg.ccs.poly) <- my.utm.epsg
  
  # # create polygone of sampling circle
  # circle.17 <- sf::st_buffer(
  #   sf::st_as_sf(as.data.frame(cbind("lon" = c.x0, 
  #                                    "lat" = c.y0)),
  #                coords = c("lon", "lat")), # center point df
  #   c.r3)                                   # radius of outer 17.84 circle
  # ## assign CRS to BZE circle
  # #sf::st_crs(circle.17) <- my.utm.epsg
  # circle.17$stand <- unique(all_rem_circles_coords$stand[all_rem_circles_coords$plot_ID == my.plot.id])
  # circle.17$plot_ID <- my.plot.id
  # circle.17$e_ID <- 0
  # circle.17$e_form <- 0
  # circle.17$CCS_r_m <- c.r3
  # 
  
  # create polygone of edge 1 triangle
  edge.poly.1 <- sfheaders::sf_polygon(obj = all_edge_intersections_coords %>% filter(plot_ID == my.plot.id & e_ID == my.e.id.1) 
                                       , x = "X"
                                       , y = "Y"
                                       , keep = TRUE)
  edge.poly.1 <- sf::st_buffer(edge.poly.1, 0)
  
  
  # create polygone of edge 2 triangle
  edge.poly.2 <- sfheaders::sf_polygon(obj = all_edge_intersections_coords %>% filter(plot_ID == my.plot.id & e_ID == my.e.id.2) 
                                       , x = "X"
                                       , y = "Y"
                                       , keep = TRUE)
  edge.poly.2 <- sf::st_buffer(edge.poly.2, 0)
  
  # ## create polygon of remaining circle after circle-edge.1 intersection
  # circle.edge.1.inter <- sf::st_intersection(edge.poly.1, st_geometry(circle.17))
  # # if there is no intersection between the edge and the cirlce, 
  # if(isTRUE(nrow(circle.edge.1.inter) == 0)){
  #   # the whole circle polygon is set as the remaining circle 
  #   rem.circle.17.1 <- circle.17}else{
  #     # else the remaining area/ polyon after the inersection is deducted is passed on as remaining circle
  #     rem.circle.17.1 <- sf::st_difference(circle.17, st_geometry(circle.edge.1.inter))
  #   }
  # 
  # 
  # 
  # ## create polygon of remaining circle after circle-edge.2 intersection
  # circle.edge.2.inter <- sf::st_intersection(edge.poly.2, st_geometry(rem.circle.17.1))
  # # if there is no intersection between the edge and the cirlce, 
  # if(isTRUE(nrow(circle.edge.2.inter) == 0)){
  #   # the previous remaining circle polygon is set as the remaining circle 
  #   rem.circle.17.2 <- rem.circle.17.1}else{
  #     # else the remaining area/ polyon after the inersection is deducted is passed on as remaining circle
  #     rem.circle.17.2 <- sf::st_difference(rem.circle.17.1, st_geometry(circle.edge.2.inter))
  #   }
  
  ## importing the remianing circle polygone directly fro the all_rem_circles_coords.df wouldn not work so well 
  # since we´d have to find a way to first export and then mport and convert multipolygones 
  # accurately, which is to much effort given that we can just wirk with the whole cirlce and the edge-intersections
  rem.circle.17.2 <- sfheaders::sf_polygon(obj = all_rem_circles_coords %>% filter(plot_ID == my.plot.id) 
                                           , x = "X"
                                           , y = "Y"
                                           , keep = TRUE)
  # set buffer to avoid problems with polygones import: https://stackoverflow.com/questions/66584191/sfst-intersection-virtually-random-error-action
  rem.circle.17.2 <- sf::st_buffer(rem.circle.17.2, 0)
  
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
    "area_m2"= c(unlist((my.rg.A.m2))) 
  ))
  
  ## assign the whole CCS area to the stand that covers 2/3rds of the RG CCS area
  # determine 2/3 of the RG CCS area 
  rg.ccs.A.0.6 <- sf::st_area(my.rg.ccs.poly)*(2/3) 
  
  # select the row that includes the stand that covers 2/3 of the RG CCS area, 
  # by filtering the area fot bigger/ equal 2/3 of the total RG CCS area 
  rg.0.6.data <- (rg.edge.data[as.numeric(rg.edge.data$area_m2) >= rg.ccs.A.0.6, ] %>% arrange(., desc(area_m2))) %>% slice(1) 
  # as we cannot localise the plants in the cirlce, we cannot adjust the refference area (Bezugsfläche) according to the are covered by the respective stand
  # thus the whole are of the RG CCS is allocated to the stand that covers most of it´s area, as all plants included in the respective RG CCS are also allocated to this stand
  # since we cannot sort them into stands by location as we don´t know their location
  
  if(isTRUE(nrow(rg.0.6.data)== 0)){
    rg.edge.data <- rg.edge.data[1,]
    rg.edge.data[1,]$stand <- NA
    rg.edge.data[1,]$area_m2  <- sf::st_area(my.rg.ccs.poly)
  }else{
    rg.edge.data <- rg.0.6.data
    rg.edge.data$area_m2  <- sf::st_area(my.rg.ccs.poly)
  }
  
  
  ## put dataframe in export list
  RG.CCS.two.edges.list[[i]] <- rg.edge.data
  
  print(paste(i, my.plot.id, my.ccs.id, sep = " "))
  
  # try(print(ggplot() +
  #         geom_sf(data = ( sf::st_as_sf(as.data.frame(cbind("lon" = c.x0, 
  #                                                           "lat" = c.y0)),
  #                                       coords = c("lon", "lat"))), aes(),fill = NA)+
  #         geom_sf(data = ( sf::st_as_sf(as.data.frame(cbind("lon" = x_CCS_center, 
  #                                                           "lat" = y_CCS_center)),
  #                                       coords = c("lon", "lat"))), aes(),fill = NA)+
  #         geom_sf(data = rem.circle.17.2, aes(colour = stand),fill = NA)+
  #         geom_sf(data = edge.poly.1, aes(colour = stand), fill = NA)+
  #         geom_sf(data = edge.poly.2, aes(colour = stand), fill = NA)+
  #         geom_sf(data = my.rg.ccs.poly,aes(colour = rg.edge.data$stand), fill = NA)+
  #         ggtitle(my.plot.id, my.ccs.id)
  # ), silent = T)
  
}
# bind areas and stands in one dataframe with plot_ID, CCS_nr to join stand & area info into BZE3_RG dataset later      
RG_two_edges_stands_areas <- as.data.frame(rbindlist(RG.CCS.two.edges.list))




# 3. export RG data ----------------------------------------------------------

# 3.1. preparing data for export -----------------------------------------------

## bind stand and area of plots with one ad two edges together 
RG_all_edges_stands_areas <- rbind(RG_one_edge_stands_areas, RG_two_edges_stands_areas)

## harmonizig strings with BZE3_RG datasets 
RG_all_edges_stands_areas[,c(1,2)] <- lapply(RG_all_edges_stands_areas[,c(1,2)], as.integer) 


## joining stand and area info into RG datasets
# join stand and area data into dataset with regeneration CCS locations and size 
# details (RG_loc) as well as into regeneration individual plant info dataset (BZE3_RG)
# RG_loc update
if(exists('RG_all_edges_stands_areas') && nrow(RG_all_edges_stands_areas) != 0){
  RG_loc_update_2 <- RG_loc %>% 
    left_join(.,RG_all_edges_stands_areas, by = c("plot_ID", "CCS_nr"), multiple = "all") %>% 
    # if there is not stand and area assigned because the plot doesn´t have an edge ... 
    mutate(stand = ifelse(is.na(stand) & is.na(area_m2), "A", stand),   # ... the stand is set to A
           area_m2 = ifelse(stand == "A" & is.na(area_m2), c_A(as.numeric(CCS_max_dist_cm)/100), area_m2)) # ... the area is calculated from CCS_max_dist_cm
}else{
  RG_loc_update_2 <- RG_loc %>% 
    # if there is not stand and area assigned because the plot doesn´t have an edge ... 
    mutate(stand =  "A",   # ... the stand is set to A
           area_m2 = c_A(as.numeric(CCS_max_dist_cm)/100)) # ... the area is calculated from CCS_max_dist_cm
  
}


## join in stand and plot area data in RG_data set   
RG_data_update_2 <- RG_data %>% 
  left_join(.,RG_loc_update_2 %>% 
              select(plot_ID, CCS_nr, stand, area_m2) %>% 
              distinct(),
            by = c("plot_ID", "CCS_nr"), 
            multiple = "all") %>% 
  arrange(plot_ID, CCS_nr, tree_ID)


# 3.2. export  ------------------------------------------------------------
write.csv(RG_loc_update_2, paste0(out.path.BZE3, paste(unique(RG_loc_update_2$inv)[1], "RG_loc_update_2", sep = "_"), ".csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(RG_data_update_2, paste0(out.path.BZE3, paste(unique(RG_data_update_2$inv)[1], "RG_update_2", sep = "_"), ".csv"), row.names = FALSE, fileEncoding = "UTF-8")



stop("that´s were the notes of RG_forest_edges HBI start")
# notes -------------------------------------------------------------------
## subsetting the RG_loc dataset by filtering for plots that have only one intersecting edge
# we have an issue here because RG_loc and by that RG.one.edge too, where plots do only have one relevant edge, but the filter also 
# takes the edge that´s not relevant. e.g. 140058 has 2 edges whereby one doesn´t intersect. as we pull single-edge stands 
# by the plot ID, however, we pull both edges here, also the one that´s not relevant. we have to adjust this
RG_one_edge <- RG_loc %>%
  # filter only rg cirlces that have an edge that actually cuts the circle
  semi_join(forest_edges %>% filter(e_form == 1 | e_form == 2 & inter_status_AT_17 == "two I" | e_form == 2 & inter_status_BT_17 == "two I") %>% 
              select(plot_ID, e_ID) %>% distinct(), by = c("plot_ID", "e_ID")) %>% 
  # filter for trees located in plots htat haev only one forest edge: be removing those plots that do officially have 2 edges even according to the 
  # "active edge" filter 
  anti_join(forest_edges %>% filter(e_form == 1 | e_form == 2 & inter_status_AT_17 == "two I" | e_form == 2 & inter_status_BT_17 == "two I") %>% 
              group_by(plot_ID) %>% summarise(n = n()) %>% filter(n > 1) %>% select(plot_ID), by = "plot_ID")

