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
HBI_RG_loc <- read.delim(file = here("data/input/BZE2_HBI/bej.csv"), sep = ",", dec = ",")

# # import coordinates of polygones along all edges iin triangle shape
all_edge_intersections_coords <- read.delim(file = here(paste0(out.path.BZE3, inv_name(HBI_RG$inv_year[1]), "_all_edges_intersection_coords.csv")), sep = ";", dec = ",")
all_rem_circles_coords <- read.delim(file = here(paste0(out.path.BZE3, inv_name(HBI_RG$inv_year[1]), "_all_rem_circles_coords.csv")), sep = ";", dec = ",")
all_edge_triagles_coords <- read.delim(file = here(paste0(out.path.BZE3, inv_name(HBI_RG$inv_year[1]), "_all_edge_triangle_coords.csv")), sep = ";", dec = ",")



# 0.4 data prep: harmonise strings, assign columnnames etc. ---------------------------------------------------------------------
# assign column names 
                        # bund_nr     pk_nr      pk_richtung     pk_dist     pk_aufnahme      pk_maxdist
colnames(HBI_RG_loc) <- c("plot_ID", "CCS_nr", "CCS_position",  "CCS_dist", "RG_inv_status", "CCS_max_dist_cm")
                    #  "bund_nr"  "pk_nr"  "lfd_nr"   "bart"  "hoehe"    "grklasse"
colnames(HBI_RG) <- c("plot_ID", "CCS_no", "t_ID", "SP_code", "H_cm", "D_class_cm", "inv_year")


# 1. calculations ---------------------------------------------------------


# 1.1. assign gon according to exposition --------------------------------
HBI_RG_loc <- HBI_RG_loc %>% 
  mutate(CCS_gon = case_when(CCS_position == "n" ~ 0,
                             CCS_position == "w" ~ 100,
                             CCS_position == "s" ~ 200,
                             CCS_position == "o" ~ 300), 
         # if the max distance of the last plant in the RG CCS is not measured we assume it´s 5m or 500cm
         CCS_max_dist_cm = ifelse(CCS_max_dist_cm == -9 | is.na(CCS_max_dist_cm), 500, CCS_max_dist_cm))

 

HBI_RG_one_edge <- HBI_RG_loc %>% semi_join(., all_edge_intersections_coords %>% 
                                              select(plot_ID, e_ID) %>% 
                                              distinct() %>% 
                                              group_by(plot_ID) %>% 
                                              summarise(n_edges = n()) %>% 
                                              filter(n_edges > 1), by = "plot_ID")

# for each plot_id and regeneration circle at plots with one edge only 
RG.CCS.one.edge <- vector("list", length = nrow(unique(HBI_RG_one_edge[c("plot_ID", "CCS_nr")])))
for (i in 1:nrow(unique(HBI_RG_one_edge[c("plot_ID", "CCS_nr")]))) {
  # i = 1
  
  # regerneation sampling cirlce data
  my.plot.id <- unique(HBI_RG_one_edge[c("plot_ID", "CCS_nr")])[, "plot_ID"][i]  # plot id of respecctive regereation satelite
  my.ccs.id <- unique(HBI_RG_one_edge[c("plot_ID", "CCS_nr")])[, "CCS_nr"][i]    # circle id7 number of respecctive regereation satelite 
  my.ccs.r <- (HBI_RG_one_edge$CCS_max_dist[HBI_RG_one_edge$plot_ID == my.plot.id & HBI_RG_one_edge$CCS_nr == my.ccs.id])/100   # max dist of last plant in the circle to create buffer

  # circle data
  c.x0 = 0 
  c.y0 = 0
  c.r3 = 17.84
  
  # determine center corodiantes of the respective regeneration sampling circuit saterilte
  ccs.dist <- HBI_RG_one_edge$CCS_dist[HBI_RG_one_edge$plot_ID == my.plot.id & HBI_RG_one_edge$CCS_nr == my.ccs.id]/100
  ccs.azi <- HBI_RG_one_edge$CCS_gon[HBI_RG_one_edge$plot_ID == my.plot.id & HBI_RG_one_edge$CCS_nr == my.ccs.id]
  x_CCS_center = coord(c.x0, c.y0, ccs.dist, ccs.azi, coordinate = "x")
  y_CCS_center = coord(c.x0, c.y0, ccs.dist, ccs.azi, coordinate = "y")
  
## create polyones
  # create polygone of RG CCS 
  my.rg.ccs.poly <- sf::st_buffer(
    sf::st_as_sf(as.data.frame(cbind("lon" = x_CCS_center, "lat" = y_CCS_center)),coords = c("lon", "lat")), # center point df
    my.ccs.r) # radius
  # create polygon of edge-intersection from all.edge.intersection.coords.df
  edge.poly <- sfheaders::sf_polygon(obj = all_edge_intersections_coords %>% filter(plot_ID == my.plot.id) 
                                     , x = "X"
                                     , y = "Y"
                                     , keep = TRUE)
  # create polygon of edge-intersection from all.edge.intersection.coords.df
  rem.circle.poly <- sfheaders::sf_polygon(obj = all_rem_circles_coords %>% filter(plot_ID == my.plot.id) 
                                           , x = "X"
                                           , y = "Y"
                                           , keep = TRUE)
 ## check for intersections
  # with edge-intersection-polygon
  intersection.with.edge <- sf::st_intersection(my.rg.ccs.poly, edge.poly)
  # with remaining circle polygon
  intersection.with.rem.circle <- sf::st_intersection(rem.circle.poly, my.rg.ccs.poly)
  
## set the stand of the rg circle according to its intersections: https://www.geeksforgeeks.org/nested-if-else-statement-in-r/
  # if both polygones, intersectionw if there are intersectionons of the respective RG CCS with the edge and remaining circle polyones (nrow != 0) 
if(nrow(intersection.with.edge) != 0 & nrow(intersection.with.rem.circle) != 0){
  # the RG CCS receives two stands and areas 
  my.stand.rg <-c(intersection.with.rem.circle$stand, intersection.with.edge$stand)
  # if there are only intersectionons of the respective RG CCS with remaining circle polyone (nrow != 0) 
  if(nrow(intersection.with.edge) == 0 & nrow(intersection.with.rem.circle) != 0){
    # the RG CCS receives the stand of the remaining circle, as well as the area covered by it
    my.stand.rg <- c(intersection.with.rem.circle$stand)
    # if there are only intersectionons of the respective RG CCS with the edge-circle-intersection polyone (nrow != 0) 
    if(nrow(intersection.with.edge) != 0 & nrow(intersection.with.rem.circle) == 0){
      # the RG CCS receives the stand of the edge intersection, as well as the area covered by it
      my.stand.rg <- c(intersection.with.edge$stand)
    }}}else{my.stand.rg <- c("warning")}
  
  
## determine area of the rg circle (stands) according to it´s intersection
  if(nrow(intersection.with.edge) != 0 & nrow(intersection.with.rem.circle) != 0){
    # the RG CCS receives two stands and areas 
    my.rg.A.m2 <- c(st_area(intersection.with.rem.circle), st_area(intersection.with.edge))
    # if there are only intersectionons of the respective RG CCS with remaining circle polyone (nrow != 0) 
    if(nrow(intersection.with.edge) == 0 & nrow(intersection.with.rem.circle) != 0){
      # the RG CCS receives the stand of the remaining circle, as well as the area covered by it
      my.rg.A.m2 <- c(st_area(intersection.with.rem.circle))
      # if there are only intersectionons of the respective RG CCS with the edge-circle-intersection polyone (nrow != 0) 
      if(nrow(intersection.with.edge) != 0 & nrow(intersection.with.rem.circle) == 0){
        # the RG CCS receives the stand of the edge intersection, as well as the area covered by it
        my.rg.A.m2 <- c(st_area(intersection.with.edge))
      }}}else{my.rg.A.m2 <- c(0)}                      
  
## safe intersection info (stand and area) of the RG CCS into dataframe
   rg.edge.data <- as.data.frame(cbind(
     "plot_ID" = c(rep(my.plot.id, times = length(my.stand.rg))), 
     "CCS_nr" = c(rep(my.ccs.id, times = length(my.stand.rg))), 
     "stand" = my.stand.rg, 
     "RG_area_m2"= my.rg.A.m2
   ))
  
## put dataframe in export list
  RG.CCS.one.edge[[i]] <- rg.edge.data

  
  print(ggplot() +
           geom_sf(data = rem.circle.poly, aes(colour = stand))+
           geom_sf(data = edge.poly, aes(colour = stand))+
          geom_sf(data = my.rg.ccs.poly, colour = "black", fill = NA)+
          ggtitle(my.plot.id, my.ccs.id)
         )
  
  }
      









# 1.3. check if RG circuit lies inside remaining circle or --------------------------------------------------------------------









)