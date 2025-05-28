# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the  national soil inventory
# visulaization plotskizzen BZE3 & HBI 





# ----- 0. SETUP ---------------------------------------------------------------

# ----- 0.1. packages and functions --------------------------------------------
source(paste0(getwd(), "/scripts/01_00_functions_library.R"))

# ----- 0.2. working directory -------------------------------------------------
here::here()
getwd()

out.path.BZE3 <- paste0(getwd(), "/output/out_data/out_data_BZE/") 

# ----- 0.3 data import --------------------------------------------------------
# BZE3
# livign trees
BZE3_trees_data <- read_csv(paste0(out.path.BZE3, "BZE3_LT_update_1.csv"))

# import coordinates of polygones along all edges iin triangle shape based  
BZE3_all_edge_intersections_coords <- read.delim(file = paste0(out.path.BZE3, inv_name(BZE3_trees_data$inv_year[1]), "_all_edges_intersection_coords.csv"), sep = ",", dec = ".")
BZE3_all_rem_circles_coords <- read.delim(file = paste0(out.path.BZE3, inv_name(BZE3_trees_data$inv_year[1]), "_all_rem_circles_coords.csv"), sep = ",", dec = ".")
BZE3_all_edge_triangles_coords <- read.delim(file = paste0(out.path.BZE3, inv_name(BZE3_trees_data$inv_year[1]), "_all_edges_triangle_coords.csv"), sep = ",", dec = ".")
BZE3_all_areas_stands <- read.delim(file = paste0(out.path.BZE3, inv_name(BZE3_trees_data$inv_year[1]), "_all_edges_rem_circles.csv"), sep = ",", dec = ".")

# HBI
# livign trees
HBI_trees_data <- read_csv(paste0(out.path.BZE3, "HBI_LT_update_1.csv"))

# import coordinates of polygones along all edges iin triangle shape based  
HBI_all_edge_intersections_coords <- read.delim(file = paste0(out.path.BZE3, inv_name(HBI_trees_data$inv_year[1]), "_all_edges_intersection_coords.csv"), sep = ",", dec = ".")
HBI_all_rem_circles_coords <- read.delim(file = paste0(out.path.BZE3, inv_name(HBI_trees_data$inv_year[1]), "_all_rem_circles_coords.csv"), sep = ",", dec = ".")
HBI_all_edge_triangles_coords <- read.delim(file = paste0(out.path.BZE3, inv_name(HBI_trees_data$inv_year[1]), "_all_edges_triangle_coords.csv"), sep = ",", dec = ".")
HBI_all_areas_stands <- read.delim(file = paste0(out.path.BZE3, inv_name(HBI_trees_data$inv_year[1]), "_all_edges_rem_circles.csv"), sep = ",", dec = ".")






# 1.1. visualisation forest edges and trees of BZE3   --------------------------------
for(i in 1:(nrow(BZE3_trees_data %>% select(plot_ID) %>% distinct()))){
  # https://ggplot2.tidyverse.org/reference/ggsf.html
  
  #i = 1
  # i = which(grepl(140043, unique(BZE3_trees_data$plot_ID)))
  my.plot.id = unique(BZE3_trees_data$plot_ID)[i]
  #print(my.plot.id)
  
  c.df <- as.data.frame(cbind("lon" = 0, "lat" = 0))
  c.pt <- sf::st_as_sf(c.df, coords = c("lon", "lat"))
  c.poly.17 <- sf::st_buffer(c.pt, 17.84)
  c.poly.12 <- sf::st_buffer(c.pt, 12.62)
  c.poly.5 <- sf::st_buffer(c.pt, 5.64)
  
  # get tree points 
  all.trees.points.df.nogeo.sp <- sf::st_as_sf(BZE3_trees_data[BZE3_trees_data$plot_ID == my.plot.id, c("plot_ID", "tree_ID", "X_tree", "Y_tree", "SP_code", "stand", "DBH_cm")], 
                                               coords = c("X_tree", "Y_tree"))

  # edge form 1 
  my.triangle.e1.poly.df.nogeo <- BZE3_all_edge_triangles_coords[BZE3_all_edge_triangles_coords$e_form == 1, ]
  try( { my.triangle.e1.poly <- sfheaders::sf_polygon(obj = my.triangle.e1.poly.df.nogeo[my.triangle.e1.poly.df.nogeo$plot_ID == my.plot.id, ]
                        , x = "lon"
                        , y = "lat"
                        , keep = TRUE)}
       ,silent = FALSE )
  # edge form 2
  my.triangle.e2.poly.df.nogeo <- BZE3_all_edge_triangles_coords[BZE3_all_edge_triangles_coords$e_form == 2, ]
try( { my.triangle.e2.poly <- sfheaders::sf_polygon(obj = my.triangle.e2.poly.df.nogeo[my.triangle.e2.poly.df.nogeo$plot_ID == my.plot.id, ]
                                               , x = "lon"
                                               , y = "lat"
                                               , keep = TRUE)}
      ,silent = FALSE )
  
  # intersection polygone 
  try( { my.edge.poly <- sfheaders::sf_polygon(obj = BZE3_all_edge_intersections_coords[BZE3_all_edge_intersections_coords$plot_ID == my.plot.id, ]
                                                     , x = "X"
                                                     , y = "Y"
                                                     , keep = TRUE)} 
       ,silent = FALSE )
  # rem circle
  try( { my.rem.circle.poly <- sfheaders::sf_polygon(obj = BZE3_all_rem_circles_coords[BZE3_all_rem_circles_coords$plot_ID == my.plot.id, ]
                        , x = "X"
                        , y = "Y"
                        , keep = TRUE)} 
       ,silent = FALSE )
  
 try( {print(
    ggplot() +
          ggtitle(my.plot.id)+
          geom_sf(data = my.triangle.e1.poly$geometry[my.triangle.e1.poly$plot_ID == my.plot.id], 
                  aes(alpha = 0
                     # , fill = na.omit(my.triangle.e1.poly$stand[my.triangle.e1.poly$plot_ID == my.plot.id])
                      ))+
           geom_sf(data = my.triangle.e2.poly$geometry[my.triangle.e2.poly$plot_ID == my.plot.id], 
                   aes(alpha = 0
                       #, fill = na.omit(my.triangle.e2.poly$stand[my.triangle.e2.poly$plot_ID == my.plot.id])
                       ))+ 
          geom_sf(data = my.rem.circle.poly$geometry[my.rem.circle.poly$plot_ID == my.plot.id], 
                  aes(alpha = 0, fill = na.omit(my.rem.circle.poly$stand[my.rem.circle.poly$plot_ID == my.plot.id])))+
      geom_sf(data = my.edge.poly$geometry[my.edge.poly$plot_ID == my.plot.id], 
              aes(alpha = 0, fill = na.omit(my.edge.poly$stand[my.edge.poly$plot_ID == my.plot.id])))+
      
      geom_sf(data = c.poly.17, aes(alpha = 0))+
          geom_sf(data = c.poly.12, aes(alpha = 0))+
          geom_sf(data = c.poly.5, aes(alpha = 0))+
          geom_sf(data = all.trees.points.df.nogeo.sp$geometry[all.trees.points.df.nogeo.sp$plot_ID == my.plot.id], 
                  aes(color = all.trees.points.df.nogeo.sp$SP_code[all.trees.points.df.nogeo.sp$plot_ID == my.plot.id], 
                      size =  all.trees.points.df.nogeo.sp$DBH_cm[all.trees.points.df.nogeo.sp$plot_ID == my.plot.id]))+
          guides(color=guide_legend(title="species"))+
          guides(size=guide_legend(title="DBH cm"))+
          guides(fill=guide_legend(title="stand"))+
          geom_sf_text(data = all.trees.points.df.nogeo.sp$geometry[all.trees.points.df.nogeo.sp$plot_ID == my.plot.id], 
                       aes(label = all.trees.points.df.nogeo.sp$tree_ID[all.trees.points.df.nogeo.sp$plot_ID == my.plot.id]))+
          xlim(-30, 30)+
          ylim(-30, 30)
        
  )} 
  , silent = F)
  
  print(paste(i, my.plot.id))
}





# 1.2. visualisation forest edges and trees of HBI   --------------------------------
for(i in 1:(nrow(HBI_trees_data %>% select(plot_ID) %>% distinct()))){
  # https://ggplot2.tidyverse.org/reference/ggsf.html
  
  #i = 1
  # i = which(grepl(140043, unique(HBI_trees_data$plot_ID)))
  my.plot.id = unique(HBI_trees_data$plot_ID)[i]
  #print(my.plot.id)
  
  c.df <- as.data.frame(cbind("lon" = 0, "lat" = 0))
  c.pt <- sf::st_as_sf(c.df, coords = c("lon", "lat"))
  c.poly.17 <- sf::st_buffer(c.pt, 17.84)
  c.poly.12 <- sf::st_buffer(c.pt, 12.62)
  c.poly.5 <- sf::st_buffer(c.pt, 5.64)
  
  # get tree points 
  all.trees.points.df.nogeo.sp <- sf::st_as_sf(HBI_trees_data[HBI_trees_data$plot_ID == my.plot.id, c("plot_ID", "tree_ID", "X_tree", "Y_tree", "SP_code", "stand", "DBH_cm")], 
                                               coords = c("X_tree", "Y_tree"))
  
  # edge form 1 
  my.triangle.e1.poly.df.nogeo <- HBI_all_edge_triangles_coords[HBI_all_edge_triangles_coords$e_form == 1, ]
  try( { my.triangle.e1.poly <- sfheaders::sf_polygon(obj = my.triangle.e1.poly.df.nogeo[my.triangle.e1.poly.df.nogeo$plot_ID == my.plot.id, ]
                                                      , x = "lon"
                                                      , y = "lat"
                                                      , keep = TRUE)}
       ,silent = FALSE )
  # edge form 2
  my.triangle.e2.poly.df.nogeo <- HBI_all_edge_triangles_coords[HBI_all_edge_triangles_coords$e_form == 2, ]
  try( { my.triangle.e2.poly <- sfheaders::sf_polygon(obj = my.triangle.e2.poly.df.nogeo[my.triangle.e2.poly.df.nogeo$plot_ID == my.plot.id, ]
                                                      , x = "lon"
                                                      , y = "lat"
                                                      , keep = TRUE)}
       ,silent = FALSE )
  
  # intersection polygone 
  try( { my.edge.poly <- sfheaders::sf_polygon(obj = HBI_all_edge_intersections_coords[HBI_all_edge_intersections_coords$plot_ID == my.plot.id, ]
                                               , x = "X"
                                               , y = "Y"
                                               , keep = TRUE)} 
       ,silent = FALSE )
  # rem circle
  try( { my.rem.circle.poly <- sfheaders::sf_polygon(obj = HBI_all_rem_circles_coords[HBI_all_rem_circles_coords$plot_ID == my.plot.id, ]
                                                     , x = "X"
                                                     , y = "Y"
                                                     , keep = TRUE)} 
       ,silent = FALSE )
  
  try( {print(
    ggplot() +
      ggtitle(my.plot.id)+
      geom_sf(data = my.triangle.e1.poly$geometry[my.triangle.e1.poly$plot_ID == my.plot.id], 
              aes(alpha = 0
                  # , fill = na.omit(my.triangle.e1.poly$stand[my.triangle.e1.poly$plot_ID == my.plot.id])
              ))+
      geom_sf(data = my.triangle.e2.poly$geometry[my.triangle.e2.poly$plot_ID == my.plot.id], 
              aes(alpha = 0
                  #, fill = na.omit(my.triangle.e2.poly$stand[my.triangle.e2.poly$plot_ID == my.plot.id])
              ))+ 
      geom_sf(data = my.rem.circle.poly$geometry[my.rem.circle.poly$plot_ID == my.plot.id], 
              aes(alpha = 0, fill = na.omit(my.rem.circle.poly$stand[my.rem.circle.poly$plot_ID == my.plot.id])))+
      geom_sf(data = my.edge.poly$geometry[my.edge.poly$plot_ID == my.plot.id], 
              aes(alpha = 0, fill = na.omit(my.edge.poly$stand[my.edge.poly$plot_ID == my.plot.id])))+
      
      geom_sf(data = c.poly.17, aes(alpha = 0))+
      geom_sf(data = c.poly.12, aes(alpha = 0))+
      geom_sf(data = c.poly.5, aes(alpha = 0))+
      geom_sf(data = all.trees.points.df.nogeo.sp$geometry[all.trees.points.df.nogeo.sp$plot_ID == my.plot.id], 
              aes(color = all.trees.points.df.nogeo.sp$SP_code[all.trees.points.df.nogeo.sp$plot_ID == my.plot.id], 
                  size =  all.trees.points.df.nogeo.sp$DBH_cm[all.trees.points.df.nogeo.sp$plot_ID == my.plot.id]))+
      guides(color=guide_legend(title="species"))+
      guides(size=guide_legend(title="DBH cm"))+
      guides(fill=guide_legend(title="stand"))+
      geom_sf_text(data = all.trees.points.df.nogeo.sp$geometry[all.trees.points.df.nogeo.sp$plot_ID == my.plot.id], 
                   aes(label = all.trees.points.df.nogeo.sp$tree_ID[all.trees.points.df.nogeo.sp$plot_ID == my.plot.id]))+
      xlim(-30, 30)+
      ylim(-30, 30)
    
  )} 
  , silent = F)
  
  print(paste(i, my.plot.id))
}


