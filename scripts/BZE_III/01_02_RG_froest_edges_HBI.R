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

# # import polygones along all edges iin triangle shape
# all_triangles_poly <- read_delim(here(paste0(out.path.BZE3, inv_name(HBI_RG$inv_year[1]), "_all_edges_triangle_poly.csv")), delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)
# # import all remaining circle polygones with geometry as list
# all_rem_circles_poly <- read_delim(here(paste0(out.path.BZE3, inv_name(HBI_RG$inv_year[1]), "_all_edges_rem_circles_poly.csv")),  delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)
# all_rem_circles_poly$geometry <- as_tibble(all_rem_circles_poly$geometry)
# # import all edge-triangle-circle-intersection polygons with geometry as list
# all_edge_intersections_poly <- read_delim(here(paste0(out.path.BZE3, inv_name(HBI_RG$inv_year[1]), "_all_edges_intersection_poly.csv")),  delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)
# all_edge_intersections_poly$geometry <-as.list(all_edge_intersections_poly$geometry)

all_edge_intersections_coords <- read.delim(file = here(paste0(out.path.BZE3, inv_name(HBI_RG$inv_year[1]), "_all_edges_intersection_coords.csv")), sep = ";", dec = ",")
all_rem_circles_coords <- read.delim(file = here(paste0(out.path.BZE3, inv_name(HBI_RG$inv_year[1]), "_all_rem_circles_coords.csv")), sep = ";", dec = ",")



# 0.4 data prep: harmonise strings, assign columnnames etc. ---------------------------------------------------------------------
# assign column names 
                        # bund_nr     pk_nr      pk_richtung     pk_dist     pk_aufnahme      pk_maxdist
colnames(HBI_RG_loc) <- c("plot_ID", "CCS_nr", "CCS_position",  "CCS_dist", "RG_inv_status", "CCS_max_dist")
                    #  "bund_nr"  "pk_nr"  "lfd_nr"   "bart"  "hoehe"    "grklasse"
colnames(HBI_RG) <- c("plot_ID", "CCS_no", "t_ID", "SP_code", "H_cm", "D_class_cm", "inv_year")


# 1. calculations ---------------------------------------------------------


# 1.1. assign gon according to exposition --------------------------------
HBI_RG_loc <- HBI_RG_loc%>% 
  mutate(CCS_gon = case_when(CCS_position == "n" ~ 0,
                             CCS_position == "w" ~ 100,
                             CCS_position == "s" ~ 200,
                             CCS_position == "o" ~ 300), 
         # if the max distance of the last plant in the RG CCS is not measured we assume it´s 5m or 500cm
         CCS_max_dist = ifelse(CCS_max_dist == -9 | is.na(CCS_max_dist), 500, CCS_max_dist))

 

HBI_RG_one_edge <- HBI_RG_loc %>% semi_join(., all_edge_intersections_coords %>% 
                                              select(plot_ID, e_ID) %>% 
                                              distinct() %>% 
                                              group_by(plot_ID) %>% 
                                              summarise(n_edges = n()) %>% 
                                              filter(n_edges == 1), by = "plot_ID")

# for each plot_id and regeneration circle at plots with one edge only 
for (i in nrow(unique(HBI_RG_forest_edges[c("plot_ID", "CCS_nr")]))) {
  # i = 1
  
  # regerneation sampling cirlce data
  my.plot.id <- unique(HBI_RG_forest_edges[c("plot_ID", "CCS_nr")])[, "plot_ID"][i]  # plot id of respecctive regereation satelite
  my.ccs.id <- unique(HBI_RG_forest_edges[c("plot_ID", "CCS_nr")])[, "CCS_nr"][i]    # circle id7 number of respecctive regereation satelite 
  my.ccs.r <- (HBI_RG_forest_edges$CCS_max_dist[HBI_RG_forest_edges$plot_ID == my.plot.id & HBI_RG_forest_edges$CCS_nr == my.ccs.id])/100   # max dist of last plant in the circle to create buffer

  # circle data
  c.x0 = 0 
  c.y0 = 0
  c.r3 = 17.84
  
  # determine center corodiantes of the respective regeneration sampling circuit saterilte
  ccs.dist <- HBI_RG_forest_edges$CCS_dist[HBI_RG_forest_edges$plot_ID == my.plot.id & HBI_RG_forest_edges$CCS_nr == my.ccs.id]
  ccs.azi <- HBI_RG_forest_edges$CCS_gon[HBI_RG_forest_edges$plot_ID == my.plot.id & HBI_RG_forest_edges$CCS_nr == my.ccs.id]
  x_CCS_center = coord(c.x0, c.y0, ccs.dist, ccs.azi, coordinate = "x")
  y_CCS_center = coord(c.x0, c.y0, ccs.dist, ccs.azi, coordinate = "y")
  # create polygone around 
  my.ccs.center <- as.data.frame(cbind("lon" = x_CCS_center, "lat" = y_CCS_center))
  my.rg.ccs.poly <- sf::st_as_sf(center.df, coords = c("lon", "lat"))
  circle.17 <- sf::st_buffer(circle.pt, my.ccs.r/100)
  
  edge.poly <- sfheaders::sf_polygon(obj = all.edge.intersections.coords.df %>% filter(plot_ID == my.plot.id) 
                                     , x = "X"
                                     , y = "Y"
                                     , keep = TRUE)
  
  rem.circle.poly <- sfheaders::sf_polygon(obj = all_rem_circles_coords %>% filter(plot_ID == my.plot.id) 
                                           , x = "X"
                                           , y = "Y"
                                           , keep = TRUE)
 
  
  #### build whole 17.84 m circle
  # build polygon (circlular buffer) around center point
  center.df<- as.data.frame(cbind("lon" = c.x0, "lat" = c.y0))
  # center.df <- as.data.frame(cbind("lon" = my.center.easting, "lat" = my.center.northing))
  circle.pt <- sf::st_as_sf(center.df, coords = c("lon", "lat"))
  circle.17 <- sf::st_buffer(circle.pt, c.r3)
  
  
  print(plot(circle.17$geometry), 
        plot(edge.poly$geometry, col = "red", add = T), 
        plot(rem.circle.poly$geometry, col = "blue", add =T))
  
  }
      








# 1.3. check if RG circuit lies inside remaining circle or --------------------------------------------------------------------









)