
# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the  national soil inventory
# forest edges  

# ----- 0. SETUP ---------------------------------------------------------------

# ----- 0.1. packages and functions --------------------------------------------
source(paste0(getwd(), "/scripts/01_00_functions_library.R"))


# ----- 0.2. working directory -------------------------------------------------
here::here()
getwd()

out.path.BZE3 <- ("/output/out_data/out_data_BZE/") 

# ----- 0.3 data import --------------------------------------------------------
# LIVING TREES
# BZE3 BE dataset: this dataset contains the inventory data of the tree inventory accompanying the second national soil inventory
# here one should immport the the dataset called HBI_trees_update_01.csv which includes only trees that are already sortet according to their inventory status (Baumkennzahl)
trees_data <- read.delim(file = paste0(getwd(), out.path.BZE3, "HBI_LT_update_0.csv"), sep = ",", dec = ".")
# this dataset contains the removed trees that evolved from the inventory status sorting. 
# we import it to continuously collect removed data in one dataset
trees_removed <- read.delim(file = paste0(getwd(), out.path.BZE3, trees_data$inv[1], "_LT_removed.csv"), sep = ",", dec = ".")
# BZE3 BE locations dataset: this dataset contains the coordinates of the center point of the tree inventory accompanying the second national soil inventory
geo_loc <- read.delim(file = paste0(getwd(), "/data/input/BZE2_HBI/location_",  "HBI", ".csv"), sep = ",", dec = ",")
# BZE3 forest edges (Waldränder) info
forest_edges <- read.delim(file = paste0(getwd(), out.path.BZE3, trees_data$inv[1], "_forest_edges_update_1.csv"), sep = ",", dec = ".")


# ----- 0.6 harmonising column names & structure  -------------------------
# HBI locations
geo_loc <- geo_loc[1:3] 
colnames(geo_loc) <- c("plot_ID",  "RW_MED", "HW_MED") 



# ----- 1. joining in external info  --------------------------------------
# ----- 1.1. LIVING TREES -------------------------------------------------
# ----- 1.1.1. forest edges -----------------------------------------------
# filter for Waldrandform that imply that we have to do something about it
# Edge form: 
# 1 =	L = 	Linie
# 2 =	E	 = Eck
# filter for waldrandtyp that imply that we have to do something about it 
# Edge type: 
# 1	WA	Waldaußenrand 
# L> there shoulnd´t be trees beyond and we have to calculate the area of the cut-out to exclude from calculating the hectar values 
# 2	WI	Waldinnenrand 
# L> there shoulnd´t be trees beyond and we have to calculate the area of the cut-out to exclude from calculating the hectar values
# 3	BE	Bestandesgrenze
# L> no idea. I think it doesn´t matter because we calculate everything per hecktar
# but we can also try to split the stand by calculating the area behind and before the edge and treat them as two different plots s
# 4	sBE	sonst. Bestandesgrenze

# 1a) for waldrandfrom == 1 
# create 1 lm function for forest edge 
# through  X|Y of intersection with sampling circuit 
# 1b) for waldrandform == 2 we have a turning point in the graph, for WFR == 1 we don´t
#     --> build two lin models (1) X|Y anfang, X|Y Knickpunkt, (2) (1) X|Y ende, X|Y Knickpunkt,

# 2) calculate X|Y of each tree
# y tree = y centre + distance * (sin(Azimut between centre and point)

# 3a) filter for trees with x between x anfang and x Knickpunkt and x ende and x knickpunkt 
#     check if y at respeective x is higher then y andfang or y ende
# 3b) filter for trees with Y < Y forest edge function at given X


# geo means the dataset is georefferenced and has an attributed crs

# ----- 1.1.2.1. join in edge info to tree dataset ------------------------
# ----- 1.1.2.1.1. HBI join in forest edge info per plot -----------------------------------------------
trees_data <- trees_data %>% 
  # calculate the coordinates of every tree
  mutate(dist_m = dist_cm/100, 
         X_tree = coord(data_circle$x0[1], data_circle$y0[1], dist_m, azi_gon, coordinate = "x"), 
         Y_tree = coord(data_circle$x0[1], data_circle$y0[1], dist_m, azi_gon, coordinate = "y")) %>% 
  # join in the forest edge information per plot 
  left_join(., forest_edges %>% 
              select(plot_ID, e_ID, e_type, e_form), 
            by = "plot_ID", 
            multiple = "all", 
            relationship = "many-to-many") # this is necesarry since there are multiple edges per plot and multiple trees per plot 




# ----- 1.1.2.2. edge  point coordinates,  line parameters, intersections with circles -----------------------------------------------------------
# set up line from 2 points manually
forest_edges.man <- forest_edges %>% 
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
  mutate(X1_inter_AB_17 = intersection_line_circle(b0_AB, b1_AB, X_A, X_B, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3], coordinate="x1"),
         X2_inter_AB_17 = intersection_line_circle(b0_AB, b1_AB, X_A, X_B, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3],  coordinate="x2"), 
         inter_status_AB_17 = intersection.status(intersection_line_circle(b0_AB, b1_AB, X_A, X_B, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3], coordinate="x1"),
                                                  intersection_line_circle(b0_AB, b1_AB, X_A, X_B, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3],  coordinate="x2")),
         # for AT line
         X1_inter_AT_17 = intersection_line_circle(b0_AT, b1_AT, X_A, X_T, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3], coordinate="x1"),
         X2_inter_AT_17 = intersection_line_circle(b0_AT, b1_AT, X_A, X_T, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3], coordinate="x2"), 
         inter_status_AT_17 = intersection.status(intersection_line_circle(b0_AT, b1_AT,X_A, X_T, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3], coordinate="x1"), 
                                                  intersection_line_circle(b0_AT, b1_AT,X_A, X_T, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3], coordinate="x2")),
         # for BT line
         X1_inter_BT_17 = intersection_line_circle(b0_BT, b1_BT, X_B, X_T, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3],  coordinate="x1"),
         X2_inter_BT_17 = intersection_line_circle(b0_BT, b1_BT, X_B, X_T, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3],  coordinate="x2"), 
         inter_status_BT_17 = intersection.status(intersection_line_circle(b0_BT, b1_BT,X_B, X_T, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3],  coordinate="x1"), 
                                                  intersection_line_circle(b0_BT, b1_BT, X_B, X_T,data_circle$y0[3], data_circle$x0[3], data_circle$r0[3],  coordinate="x2"))) %>%   
  # y intersection with 17m circle: insert x of intercept with circle in equation of line
  # AB line 
  mutate(Y1_inter_AB_17 = intersection_line_circle(b0_AB, b1_AB,  X_A, X_B, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3], coordinate="y1"),
         Y2_inter_AB_17 = intersection_line_circle(b0_AB, b1_AB,  X_A, X_B, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3], coordinate="y2"), 
         # AT line 
         Y1_inter_AT_17 = intersection_line_circle(b0_AT, b1_AT, X_A, X_T, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3], coordinate="y1"), 
         Y2_inter_AT_17 = intersection_line_circle(b0_AT, b1_AT, X_A, X_T,data_circle$y0[3], data_circle$x0[3], data_circle$r0[3], coordinate="y2"),
         # BT line 
         Y1_inter_BT_17 = intersection_line_circle(b0_BT, b1_BT,  X_B, X_T, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3],  coordinate="y1"), 
         Y2_inter_BT_17 = intersection_line_circle(b0_BT, b1_BT,  X_B, X_T, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3],  coordinate="y2")) %>%
  
  # distance interception centre --> to see if points are actually placed on the rim of the circle 
  mutate(inter_1_dist = distance(X1_inter_AB_17, Y1_inter_AB_17, 0, 0)) %>%     # this is just to control if the whole thing worked and 
  # selecting intersections on the "right" side to check if point lies within triangle
  # to calculate the triangles Barycentric coordinates we need 3 points: A, B, C = centre point
  # in case T lies within the circle, we want R to select A and B from the intersection with the circle.
  # Whereby we have to use a wider radius, to make sure that trees located the halfmoon of the circle cut by the triangle (Kreisbogen) are selected too. 
  # when t lies inside the circle (so both lines reach outside) ue only intersception point where direction between inter_AT and AT is equal choose this x, we need a buffer tho  
  # the following statement says:  check if the slope of x_inter_1  or the slope of x_inter_2 is equal to the slope of AT,
  #                                choose the x which has the same slope (x_inter_1 or x_inter_2)as the second point on the line (A or B) 
  #                                but with a buffer of 60m radius, which is why it has to be newly calculated 
  # find the intercept of circle and line that prolonges the line between a and t or B and T via inter.for.triangle function
  # if azimut T to A  identical to azimut T to intercept 1 A and circle use this intercept (inter_AT_1) for the triable, if azimut T to A identical to azimute T to intercept 2 between A and  circle use this intercept (inter_AT_2),
  mutate(X_inter_AT_triangle_60 = inter.for.triangle(b0_AT, b1_AT, 0, 0, data_circle$rmax[3]*2, X_A, Y_A, X_T, Y_T, coordinate = "x"),
         X_inter_BT_triangle_60 = inter.for.triangle(b0_BT, b1_BT, 0, 0, data_circle$rmax[3]*2, X_B, Y_B, X_T, Y_T, coordinate = "x"), 
         # calcualte y to the x that lie in the same direction then the second point on the line, if turning points lies witin circle and lines "reach out"
         Y_inter_AT_triangle_60 = inter.for.triangle(b0_AT, b1_AT, 0, 0, data_circle$rmax[3]*2, X_A, Y_A, X_T, Y_T, coordinate = "y"),
         Y_inter_BT_triangle_60 = inter.for.triangle(b0_BT, b1_BT, 0, 0, data_circle$rmax[3]*2, X_B, Y_B, X_T, Y_T, coordinate = "y")) 

# there will always occur the following error as for some lines there are no intersections, so the intersection function returns NaNs
# In argument: `X_inter_AT_17_triangle = case_when(...)`.
# Caused by warning in `sqrt()`:
#   ! NaNs wurden erzeugt


# 3. georefferencing edge data ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



# 3.2. georefferencing per loop -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# creating dataframe with caartesian coordinates of all edges
# by using polar coordinates to calculate the cartesian coordinates 
# by adding polar coordiantes calcualted through functions to the 
# cartesian coordinates (RW_MED = lat and HW_MED = lon) of the center point of the plot

# https://stackoverflow.com/questions/26504736/create-a-list-of-spatial-polygon-dataframe-from-a-list-of-dataframe

# some gerenal facts about coordinate systems: 
# https://giswiki.hsr.ch/Koordinatensystem
# https://stackoverflow.com/questions/49094949/geo-coordinates-long-lat-to-meters-x-y
# northing = latitude = Y = hochwert --> goes from north to south
# easting = longitude = X = rechtswert  --> goes from east to west 



# 3.2.1. edge form 1: creating triangles based on line  --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

forest_edges.man.sub.e1.geo <-  forest_edges.man%>% filter(e_form == 1)  %>% ##georef
  semi_join(geo_loc %>% filter(!is.na( RW_MED) & !is.na(HW_MED)) %>%  select(plot_ID)  %>% distinct(), by = "plot_ID") # 62

triangle.e1.list.geo <- vector("list", length = length(forest_edges.man.sub.e1.geo$plot_ID))
triangle.e1.coords.geo <- vector("list", length = length(forest_edges.man.sub.e1.geo$plot_ID)*4)
for(i in 1:length(forest_edges.man.sub.e1.geo$plot_ID) ) {
  # i = 1
  # i = which(grepl(50086, forest_edges.man.sub.e1.geo$plot_ID))
  
  #if(nrow(forest_edges.man.sub.e1.geo) == 0){break}
  
  # select plot ID, edge form and edge_ID accordint to positioin in the list
  my.plot.id <- forest_edges.man.sub.e1.geo[i, "plot_ID"] 
  my.e.id <- forest_edges.man.sub.e1.geo[i, "e_ID"]
  my.e.form <- forest_edges.man.sub.e1.geo[i, "e_form"]
  my.inv.year <- forest_edges.man.sub.e1.geo[i, "inv_year"]
  
  ## georef
  ## select UTM corrdinates of the plot center by plot ID
  my.center.easting <-  as.numeric(geo_loc[geo_loc$plot_ID == my.plot.id, "RW_MED"])
  my.center.northing <- as.numeric(geo_loc[geo_loc$plot_ID == my.plot.id, "HW_MED"])
  
  # circle center and radius to calcualte intersections 
  c.x0 = 0 
  c.y0 = 0   
  c.r0 = 17.84
  c.rmax =  60
  
  
  # extract polar coordiantes of forest edge
  # point A 
  dist.A <-  forest_edges.man.sub.e1.geo[i, "A_dist"] 
  azi.A <- forest_edges.man.sub.e1.geo[i, "A_azi"] 
  x.A <- round(dist.A*sin(azi.A*pi/200), digits = 12)       # this is: easting, longitude, RW ##test*pi/200
  y.A <- round(dist.A*cos(azi.A*pi/200), digits = 12)       # this is: northing, latitude, HW ##test*pi/200
  # point B
  dist.B <- forest_edges.man.sub.e1.geo[i, "B_dist"] 
  azi.B <- forest_edges.man.sub.e1.geo[i, "B_azi"] 
  x.B <-  round(dist.B*sin(azi.B*pi/200), digits = 12)      # this is: easting, longitude, RW #test*pi/200
  y.B <-  round(dist.B*cos(azi.B*pi/200), digits = 12)      # this is: northing, latitude, HW #test*pi/200
  
  # calcualte slope (b1) and intercept (b0)
  b1 <- (y.B- y.A)/(x.B - x.A)
  b0 <- y.B - b1*x.B
  
  
  # calculate polar coordiantes of intersections of AB line with 
  x.1 <- intersection_line_circle(b0, b1, x.A, x.B, c.x0, c.y0,  c.rmax, coordinate = "x1") # this is: easting, longitude, RW
  y.1 <- intersection_line_circle(b0, b1, x.A, x.B, c.x0, c.y0,  c.rmax, coordinate = "y1") # this is: northing, latitude, HW
  x.2 <- intersection_line_circle(b0, b1, x.A, x.B, c.x0, c.y0,  c.rmax, coordinate = "x2") # this is: easting, longitude, RW
  y.2 <- intersection_line_circle(b0, b1, x.A, x.B,c.x0, c.y0,  c.rmax, coordinate = "y2") # this is: northing, latitude, HW
  
  
  # for edge form 1 we have to consider that the triangle has to be directed into the direction of the smaller half of the circle
  # calculate coordiantes of the middle of thie line between inter_1 and inter_2
  x_m_line = (x.1 + x.2)/2
  y_m_line = (y.1 + y.2)/2
  # calculate the parameters of the equation between the middle of the line and the centre of the circle
  # here we have to consider that if the middle between the intersections is equal to the center of the circle, 
  # we cannot calculate the slope or intercept of the line between the center of AB line and center of the circle
  # accordng to https://www.sofatutor.com/mathematik/videos/parallele-und-orthogonale-geraden#orthogonale-geraden
  # we can create the function of a orthogonal line by the function: 
  # -1 = AB_b1 * MC_b1 <=> MC_b1 = -1/(AB_b1)
  b1_MC = ortho_line(b1, c.x0, c.y0, parameter= "slope") # slope(c.x0, c.y0, x_m_line, y_m_line)
  b0_MC =  ortho_line(b1, c.x0, c.y0, parameter= "intercept") # intercept(c.x0, c.y0, x_m_line, y_m_line)
  # calcualte the x corrdiante of the interception of the line between M and the centre of the cirle and the circle at the given radio
  X1_inter_MC = intersection_line_circle(b0_MC, b1_MC, x_m_line, c.x0, c.x0, c.y0,  c.rmax,  coordinate = "x1") 
  X2_inter_MC = intersection_line_circle(b0_MC, b1_MC, x_m_line, c.x0, c.x0, c.y0,  c.rmax,  coordinate = "x2")
  # insert the intersection x corodinate in the line function to get the respective y coordinate
  y1_inter_MC = intersection_line_circle(b0_MC, b1_MC, x_m_line, c.x0, c.x0, c.y0,  c.rmax,  coordinate = "y1") 
  y2_inter_MC = intersection_line_circle(b0_MC, b1_MC, x_m_line, c.x0,  c.x0, c.y0,  c.rmax,  coordinate = "y2")
  # distance between the intersections (inter_MC_1, inter_MC_2) to M on the line 
  dist_C_inter_1_MC = distance(X1_inter_MC, y1_inter_MC, x_m_line, y_m_line)
  dist_C_inter_2_MC = distance(X2_inter_MC, y2_inter_MC, x_m_line, y_m_line) 
  # find the x and y coordinate of the intersection on the shorter side , which is the side to exlcude from the plot 
  X_inter_MC_shorter_side = ifelse(dist_C_inter_1_MC < dist_C_inter_2_MC, X1_inter_MC, X2_inter_MC) 
  Y_inter_MC_shorter_side = ifelse(dist_C_inter_1_MC < dist_C_inter_2_MC, y1_inter_MC, y2_inter_MC)
  
  # creating the polar coordiantes of a turning point of a triangle by selecting the intersection of the 
  # line from the middle of the AB.inter-ray and the circle center (MC_line) with 
  # the 60m radius at the "shorter side" so the intersection of the MC_line with a 60m radius that has le lest distance to the MC point on the AB.inter-ray
  turning.east <- X_inter_MC_shorter_side    + my.center.easting ## georef
  turning.north <- Y_inter_MC_shorter_side  + my.center.northing ## georef
  
  # UTM coordiantes of corner points 
  x1.east <- x.1 + my.center.easting 
  y1.north <- y.1 + my.center.northing 
  x2.east <- x.2  + my.center.easting 
  y2.north <- y.2 + my.center.northing 
  
  # create dataframe that holds coordinates of the intersections of the AB line with a 60m radius and the turning pint of a diagonal line through the AB line with a 60m radius circle
  triangle.e1.df <- as.data.frame(cbind("lon" = c(turning.east, x1.east, x2.east, turning.east),
                                        "lat" = c(turning.north, y1.north, y2.north,  turning.north),
                                        "plot_ID" = c(my.plot.id, my.plot.id, my.plot.id, my.plot.id),
                                        "e_ID" = c(my.e.id, my.e.id, my.e.id, my.e.id), 
                                        "inv_year" = c(my.inv.year, my.inv.year, my.inv.year, my.inv.year)))
  
  
  # creating polygones in sf: https://stackoverflow.com/questions/61215968/creating-sf-polygons-from-a-dataframe
  triangle.e1.poly <-  sfheaders::sf_polygon(obj = triangle.e1.df  
                                             , x = "lon"
                                             , y = "lat"
                                             , polygon_id = "e_ID")
  
  ## georef
  #  assign crs
  my.utm.epsg <-  paste0("+proj=utm +zone=", pick_utm(my.center.easting)," ", "+datum=WGS84 +units=m +no_defs +type=crs")
  ## assing crs
  sf::st_crs(triangle.e1.poly) <- my.utm.epsg
  
  print(plot(triangle.e1.poly, main = my.plot.id))
  c.df <- as.data.frame(cbind("lon" = my.center.easting, "lat" = my.center.northing))
  c.pt <- sf::st_as_sf(c.df, coords = c("lon", "lat"))
  sf::st_crs(c.pt) <- my.utm.epsg
  c.poly.17 <- sf::st_buffer(c.pt, 17.84)
  c.poly.12 <- sf::st_buffer(c.pt, 12.62)
  c.poly.5 <- sf::st_buffer(c.pt, 5.64)
  c.poly.60 <-  sf::st_buffer(c.pt, 60.0)
  
  # test 
  print(ggplot() +
          ggtitle(my.plot.id)+
          geom_sf(data = c.poly.60, aes(alpha = 0))+
          geom_sf(data = c.poly.17, aes(alpha = 0))+
          geom_sf(data = c.poly.12, aes(alpha = 0))+
          geom_sf(data = c.poly.5, aes(alpha = 0))+
          geom_sf(data = triangle.e1.poly, aes(alpha = 0)))
          # xlim(-80, 80)+
          # ylim(-80, 80))
  
  #save polygones in list 
  triangle.e1.list.geo[[i]] <- c("plot_ID" = my.plot.id, "inv_year" = my.inv.year, "e_form" = my.e.form, triangle.e1.poly)
  
  # save coordiantes of polygones in list
  triangle.e1.coords.geo[[i]] <- triangle.e1.df %>% mutate(e_form = my.e.form)
  
} # closing loop for square polys of edge form 1
triangle.e1.poly.df.geo <- as.data.frame(rbindlist(triangle.e1.list.geo, fill=TRUE)) 
triangle.e1.coords.df.geo <- as.data.frame(rbindlist(triangle.e1.coords.geo))




# 3.2.1.2. edge form 2: creating triangle polygons for edge form 2 ----------------------------------------------------------------------------------------------------------------------------------------------------------------

## loop to create list of polygones for edge form 1
forest_edges.man.sub.e2.geo <- forest_edges.man %>%
  filter(e_form == 2) %>%  # nrow = 21
  filter(inter_status_AT_17 == "two I" | inter_status_BT_17 == "two I") %>% 
  arrange(plot_ID) %>% ## georef
  semi_join(geo_loc %>% filter(!is.na( RW_MED) & !is.na(HW_MED)) %>%  
              select(plot_ID)  %>% distinct(), 
            by = "plot_ID")  # nrow = 21 ## georef

# preparing output dataset
triangle.e2.list.geo <- vector("list", length = length(forest_edges.man.sub.e2.geo$plot_ID) )
triangle.e2.coords.geo <- vector("list", length = length(forest_edges.man.sub.e2.geo$plot_ID))
for(i in 1:length(forest_edges.man.sub.e2.geo$plot_ID) ) {
  # i = 1
  # i = which(grepl(140058, forest_edges.man.sub.e2.geo$plot_ID))
  
  #if(nrow(forest_edges.man.sub.e2.geo) == 0){break}
  
  # select plot ID accordint to positioin in the list
  my.plot.id <- forest_edges.man.sub.e2.geo[i, "plot_ID"] 
  my.e.id <- forest_edges.man.sub.e2.geo[i, "e_ID"] 
  my.e.form <- forest_edges.man.sub.e2.geo[i, "e_form"]
  my.inv.year <- forest_edges.man.sub.e2.geo[i, "inv_year"]
  
  ## select UTM corrdinates of the plot center
  my.center.easting <- as.numeric(geo_loc[geo_loc$plot_ID == my.plot.id, "RW_MED"]) ## georef
  my.center.northing <- as.numeric(geo_loc[geo_loc$plot_ID == my.plot.id, "HW_MED"]) ## georef
  
  # circle data
  c.x0 = 0 
  c.y0 = 0 
  c.r0 = 17.84
  c.rmax = 3000
  
  # extract polar coordiantes of forest edge
  # point A 
  dist.A <- forest_edges.man.sub.e2.geo[i, "A_dist"] 
  azi.A <- forest_edges.man.sub.e2.geo[i, "A_azi"] 
  x.A <- dist.A*sin(azi.A*pi/200)   # longitude, easting, RW, X
  y.A <- dist.A*cos(azi.A*pi/200)   # latitude, northing, HW, y 
  
  # point B
  dist.B <- forest_edges.man.sub.e2.geo[i, "B_dist"] 
  azi.B <- forest_edges.man.sub.e2.geo[i, "B_azi"] 
  x.B <- dist.B*sin(azi.B*pi/200)   # longitude, easting, RW, X
  y.B <- dist.B*cos(azi.B*pi/200)   # latitude, northing, HW, y 
  
  # point T
  dist.T <- forest_edges.man.sub.e2.geo[i, "T_dist"] 
  azi.T <- forest_edges.man.sub.e2.geo[i, "T_azi"] 
  x.T <- dist.T*sin(azi.T*pi/200)   # longitude, easting, RW, X
  y.T <- dist.T*cos(azi.T*pi/200)   # latitude, northing, HW, y 
  
  b0.AT = intercept(x.T, y.T, x.A, y.A)
  b1.AT = slope(x.T, y.T, x.A, y.A)
  b0.BT = intercept(x.T, y.T, x.B, y.B)
  b1.BT = slope(x.T, y.T, x.B, y.B)
  
  # select polar coordiantes of the points of the triangle corners via "inter_for_triangle"-function
  # for AT side
  AT.x <- inter.for.triangle(b0.AT, b1.AT,c.x0, c.y0, c.rmax, x.A, y.A, x.T, y.T, coordinate = "x")                              # longitude, easting, RW, X
  AT.y <- inter.for.triangle(b0.AT, b1.AT, c.x0, c.y0, c.rmax, x.A, y.A, x.T, y.T, coordinate = "y")                              # latitude, northing, HW, y 
  # for BT side
  BT.x <- inter.for.triangle(b0.BT, b1.BT, c.x0, c.y0, c.rmax, x.B, y.B, x.T, y.T, coordinate = "x")                              # longitude, easting, RW, X
  BT.y <- inter.for.triangle(b0.BT, b1.BT, c.x0, c.y0, c.rmax, x.B, y.B, x.T, y.T, coordinate = "y")                              # latitude, northing, HW, y 
  
  #calculate UTM coordiantes of triangle corners
  T.east <- x.T         + my.center.easting    ## georef                         # longitude, easting, RW, X
  T.north <- y.T        + my.center.northing   ## georef                        # latitude, northing, HW, y 
  AT.x.east <-  AT.x    + my.center.easting     ## georef                   # longitude, easting, RW, X
  AT.y.north <- AT.y    + my.center.northing    ## georef                   # latitude, northing, HW, y 
  BT.x.east <- BT.x     + my.center.easting     ## georef                   # longitude, easting, RW, X
  BT.y.north <- BT.y    + my.center.northing    ## georef                   # latitude, northing, HW, y 
  
  # create dataframe with triangle corner UTM coordiantes
  triangle.e2.df <- as.data.frame(cbind("lon" = c(T.east, AT.x.east, BT.x.east, T.east),       # longitude, easting, RW, X
                                        "lat" = c(T.north, AT.y.north, BT.y.north, T.north),   # latitude, northing, HW, y
                                        "plot_ID" =  c(my.plot.id, my.plot.id, my.plot.id, my.plot.id), 
                                        "e_ID" = c(my.e.id, my.e.id, my.e.id, my.e.id ), 
                                        "inv_year" = c(my.inv.year,my.inv.year,my.inv.year,my.inv.year)))
  
  # createa polygone with triangle corners via sf package: https://r-spatial.github.io/sf/reference/st.html
  triangle.e2.poly <- sfheaders::sf_polygon(obj = triangle.e2.df
                                            , x = "lon"
                                            , y = "lat"
                                            , polygon_id = "e_ID")
  ## georef
  ## select crs
  my.utm.epsg <-  paste0("+proj=utm +zone=", pick_utm(my.center.easting)," ", "+datum=WGS84 +units=m +no_defs +type=crs")
  ## assing crs
  sf::st_crs(triangle.e2.poly) <- my.utm.epsg
  
  c.df <- as.data.frame(cbind("lon" = my.center.easting, "lat" = my.center.northing))
  c.pt <- sf::st_as_sf(c.df, coords = c("lon", "lat"))
  sf::st_crs(c.pt) <- my.utm.epsg
  c.poly.17 <- sf::st_buffer(c.pt, 17.84)
  c.poly.12 <- sf::st_buffer(c.pt, 12.62)
  c.poly.5 <- sf::st_buffer(c.pt, 5.64)
  c.poly.60 <-  sf::st_buffer(c.pt, 60.0)
  # test 
  print(ggplot() +
          ggtitle(my.plot.id)+
          geom_sf(data = c.poly.60, aes(alpha = 0))+
          geom_sf(data = c.poly.17, aes(alpha = 0))+
          geom_sf(data = c.poly.12, aes(alpha = 0))+
          geom_sf(data = c.poly.5, aes(alpha = 0))+
          geom_sf(data = triangle.e2.poly, aes(alpha = 0)))
  
  # print triangle
  print(plot(triangle.e2.poly$geometry, main = my.plot.id))
  
  # save polygones in list
  triangle.e2.list.geo[[i]] <- c("plot_ID" = my.plot.id, "inv_year" = my.inv.year, "e_form" = my.e.form, triangle.e2.poly)
  
  # save coordiantes of polygones in list
  triangle.e2.coords.geo[[i]] <- triangle.e2.df %>% mutate(e_form = my.e.form)
}
# list of polygones
triangle.e2.poly.df.geo <- as.data.frame(rbindlist(triangle.e2.list.geo))

#list of coordiantes of triangle.e2 polygones
triangle.e2.coords.df.geo <- as.data.frame(rbindlist(triangle.e2.coords.geo)) 



# 3.2.1.3. intersections between circles and edges -------------------------------------------------------------------------------------------------------------------------------------
# dataprep for loop
# bind polygone dataframes together
edge.poly.df.geo <- plyr::rbind.fill(triangle.e1.poly.df.geo, triangle.e2.poly.df.geo) # rows: 83

# 3.2.1.3.1. plots with 1 edge: loop for intersections for plots with only one edge  -------------------------------------------------------------------------------------------------------------------------------
# createa dataframe with plots that have only one forest edges
forest_edges.man.sub.1.edge.geo <- forest_edges.man %>% # rows:84
  # select only plots with a known edge form and for edge 2 only those that actually intersect the 17m circle
  filter(e_form == 1 | e_form == 2 & inter_status_AT_17 == "two I" | e_form == 2 & inter_status_BT_17 == "two I") %>%  # rows:81
  # remove plots that have two edges
  anti_join(forest_edges.man %>%  filter(e_form == 1 | e_form == 2 & inter_status_AT_17 == "two I" | e_form == 2 & inter_status_BT_17 == "two I") %>% 
              group_by(plot_ID) %>% summarise(n = n()) %>% filter(n > 1) %>% select(plot_ID), by = "plot_ID") %>% 
  anti_join(., forest_edges.man %>% filter(e_type %in% c(1, 2)) %>% select(plot_ID) %>% distinct(), by = "plot_ID")  %>% ## georef # 14 plots with 2 edges --> 28 rows -> 53 left
  ## remove plots that do now have a corresponding center coordiante in the HBI loc document ## georef
  semi_join(geo_loc %>% 
              filter(!is.na( RW_MED) & !is.na(HW_MED)) %>%  
              select(plot_ID)  %>% distinct(), 
            by = "plot_ID") # nrow = 52 --> there is 1 plots without corresponding

# prepare output datasets
edges.list.geo <- vector("list", length = length(unique(forest_edges.man.sub.1.edge.geo$plot_ID)))
inter.poly.list.geo <- vector("list", length = length(unique(forest_edges.man.sub.1.edge.geo$plot_ID)))
#inter.poly.NA.list <- vector("list", length = length(unique(forest_edges.man.sub.1.edge$plot_ID)))
remaining.circle.poly.list.geo <- vector("list", length = length(unique(forest_edges.man.sub.1.edge.geo$plot_ID)))
remaining.circle.multipoly.list.geo <- vector("list", length = length(unique(forest_edges.man.sub.1.edge.geo$plot_ID)))

# loop for intersection of all edge triablge polygoens woth their respective sampling cirlce for plots with one edge only
for (i in 1:length(unique(forest_edges.man.sub.1.edge.geo$plot_ID))){ 
  # i = 1
  #i = which(grepl(50131, (forest_edges.man.sub.1.edge.geo$plot_ID)))
  
  # if(nrow(forest_edges.man.sub.1.edge.geo) == 0){break}
  
  # select plot ID of the respective circle 
  my.plot.id <- forest_edges.man.sub.1.edge.geo[i, "plot_ID"]
  my.e.form <- edge.poly.df.geo$e_form[edge.poly.df.geo$plot_ID == my.plot.id]
  my.e.id <- edge.poly.df.geo$e_ID[edge.poly.df.geo$plot_ID == my.plot.id]
  my.inv.year <- forest_edges.man.sub.1.edge.geo[i, "inv_year"]
  
  ## georef
  ##  select UTM corrdinates of the plot center
  my.center.easting <- as.numeric(geo_loc[geo_loc$plot_ID == my.plot.id, "RW_MED"])
  my.center.northing <- as.numeric(geo_loc[geo_loc$plot_ID == my.plot.id, "HW_MED"])
  ## select crs ## georef
  my.utm.epsg <-  paste0("+proj=utm +zone=", pick_utm(my.center.easting)," ", "+datum=WGS84 +units=m +no_defs +type=crs")
  
  
  # circle data
  c.x0 = 0  + my.center.easting  ## georef
  c.y0 = 0 + my.center.northing  ## georef
  c.r3 = 17.84
  c.r2 = 12.62
  c.r1 = 5.64
  center.df<- as.data.frame(cbind("lon" = c.x0, "lat" = c.y0))
  
  # build polygon (circlular buffer) around center point
  circle.pt <- sf::st_as_sf(center.df, coords = c("lon", "lat"))
  ## assing crs to cirlce corodiantes
  sf::st_crs(circle.pt) <- my.utm.epsg ## georef
  circle.17 <- sf::st_buffer(circle.pt, c.r3)
  circle.12 <- sf::st_buffer(circle.pt, c.r2)
  circle.5 <- sf::st_buffer(circle.pt, c.r1)
  
  
  ## select the respective polygones the circle is intersected by
  my.poly <- sf::st_as_sf(edge.poly.df.geo %>% filter(plot_ID == my.plot.id))
  
  # print the cirlce and edge polygone
  print(c(plot(circle.17$geometry, main = paste0("plot:", " ", my.plot.id, ",", " ", "e_form:"," ", my.e.form)), 
          plot(my.poly$geometry, col = 0, add = T)))
  
  
  #### 17m circle
  my.circle = circle.17
  # calculate intersection for 17m circle 
  inter.poly.17  <- sf::st_intersection(my.circle, my.poly)
  
  inter.status.poly.17 <- ifelse(nrow(inter.poly.17) == 0, "no intersections",
                                 ifelse(my.e.form == 1 & inter.poly.17$geometry == my.circle$geometry,  "no intersections",
                                        ifelse(my.e.form == 2 & inter.poly.17$geometry == my.circle$geometry, "fully covering circle", 
                                               "partly intersecting")))
  # this is just to remove all the additional attributes from the intersection polygone
  #inter.poly  <- sf::st_intersection(my.circle, st_geometry(my.poly))
  # if the ednge covers all of the circle remaining, the inter.polygone its going to be set to 0 so we know there are no direct intersections
  inter.poly.17 <- if(isTRUE(inter.poly.17) && inter.poly.17$geometry == my.circle$geometry){inter.poly.17 <- data.frame()}else{inter.poly.17}
  # if the edge-circle intersection is equal to 0 (so there is no intersection) return the whole cirlce as remaining circle area, else calculate the remaining circle by decuctng the intersection are from the circle area
  remaining.circle.poly.17  <- if(isTRUE(nrow(inter.poly.17)==0)){my.circle}else{sf::st_difference(my.circle, inter.poly.17)}
  # plot(remaining.circle.poly.17)
  
  # calculate area
  # intersection
  inter.area.17 <- ifelse(nrow(inter.poly.17) == 0, 0, sf::st_area(inter.poly.17))
  #remaining circle
  remaining.circle.area.17 <- ifelse(nrow(remaining.circle.poly.17) == 0, 0, sf::st_area(remaining.circle.poly.17))
  # create area dataframe for areas
  inter.area.df.17 <- as.data.frame(cbind("plot_ID" = c(my.plot.id, my.plot.id), 
                                          "e_ID" = c(my.e.id,  0),
                                          "inv_year" = c(my.inv.year, my.inv.year),
                                          # "e_form" = c(my.e.form, 0),
                                          #"shape" = c("edge", "circle"),
                                          "CCS_r_m" = c(c.r3, c.r3), "inter_stat" = c(inter.status.poly.17, 0),
                                          "area_m2" = c(inter.area.17, remaining.circle.area.17)))
  ##### 12m circle
  my.circle = circle.12
  # calculate intersection for 12m circle 
  inter.poly.12  <- sf::st_intersection(my.circle, my.poly)
  inter.status.poly.12 <- ifelse(nrow(inter.poly.12) == 0, "no intersections",
                                 ifelse(my.e.form == 1 & inter.poly.12$geometry == my.circle$geometry,  "no intersections",
                                        ifelse(my.e.form == 2 & inter.poly.12$geometry == my.circle$geometry, "fully covering circle", 
                                               "partly intersecting")))
  # this is just to remove all the additional attributes from the intersection polygone
  #inter.poly  <- sf::st_intersection(circle.17, st_geometry(my.poly))
  # if the ednge covers all of the circle remaining, the inter.polygone its going to be set to 0 so we know there are no direct intersections
  inter.poly.12 <- if(isTRUE(inter.poly.12) && inter.poly.12$geometry == my.circle$geometry){inter.poly.12 <- data.frame()}else{inter.poly.12}
  # if the edge-circle intersection is equal to 0 (so there is no intersection) return the whole cirlce as remaining circle area, else calculate the remaining circle by decuctng the intersection are from the circle area
  remaining.circle.poly.12  <- if(isTRUE(nrow(inter.poly.12)==0)){my.circle}else{sf::st_difference(my.circle, inter.poly.12)}
  # plot(remaining.circle.poly.12$geometry)
  
  # calculate area
  # intersection
  inter.area.12 <- ifelse(nrow(inter.poly.12) == 0, 0, sf::st_area(inter.poly.12))
  #remaining circle
  remaining.circle.area.12 <- ifelse(nrow(remaining.circle.poly.12) == 0, 0, sf::st_area(remaining.circle.poly.12))
  # create area dataframe for areas
  inter.area.df.12 <- as.data.frame(cbind("plot_ID" = c(my.plot.id, my.plot.id), "e_ID" = c(my.e.id,  0),
                                          "inv_year" = c(my.inv.year, my.inv.year),
                                          # "e_form" = c(my.e.form, 0),
                                          #"shape" = c("edge", "circle"),
                                          "CCS_r_m" = c(c.r2, c.r2),"inter_stat" = c(inter.status.poly.12, 0),
                                          "area_m2" = c(inter.area.12, remaining.circle.area.12)))
  
  ##### 5m circle
  my.circle = circle.5
  # calculate intersection for 17m circle 
  inter.poly.5  <- sf::st_intersection(my.circle, my.poly)
  inter.status.poly.5 <- ifelse(nrow(inter.poly.5) == 0, "no intersections",
                                ifelse(my.e.form == 1 & inter.poly.5$geometry == my.circle$geometry,  "no intersections",
                                       ifelse(my.e.form == 2 & inter.poly.5$geometry == my.circle$geometry, "fully covering circle", 
                                              "partly intersecting")))
  # this is just to remove all the additional attributes from the intersection polygone
  #inter.poly  <- sf::st_intersection(circle.17, st_geometry(my.poly))
  # if the ednge covers all of the circle remaining, the inter.polygone its going to be set to 0 so we know there are no direct intersections
  inter.poly.5 <- if(isTRUE(inter.poly.5) && inter.poly.5$geometry == my.circle$geometry){inter.poly.5 <-data.frame()}else{inter.poly.5}
  # if the edge-circle intersection is equal to 0 (so there is no intersection) return the whole cirlce as remaining circle area, else calculate the remaining circle by decuctng the intersection are from the circle area
  remaining.circle.poly.5  <- if(isTRUE(nrow(inter.poly.5)==0)){my.circle}else{sf::st_difference(my.circle, inter.poly.5)}
  # calculate area
  # intersection
  inter.area.5 <- ifelse(nrow(inter.poly.5) == 0, 0, sf::st_area(inter.poly.5))
  #remaining circle
  remaining.circle.area.5 <- ifelse(nrow(remaining.circle.poly.5) == 0, 0, sf::st_area(remaining.circle.poly.5))
  # create area dataframe for areas
  inter.area.df.5 <- as.data.frame(cbind("plot_ID" = c(my.plot.id, my.plot.id), "e_ID" = c(my.e.id,  0),
                                         "inv_year" = c(my.inv.year, my.inv.year),
                                         # "e_form" = c(my.e.form, 0),
                                         #"shape" = c("edge", "circle"),
                                         "CCS_r_m" = c(c.r1, c.r1),"inter_stat" = c(inter.status.poly.5, 0),
                                         "area_m2" = c(inter.area.5, remaining.circle.area.5)))
  
  # bind area dataframes together
  inter.area.df <- rbind(inter.area.df.17, inter.area.df.12, inter.area.df.5)
  
  
  # assing stand to the edges depedning on area
  stand.df <- inter.area.df%>% 
    filter(CCS_r_m  == 17.84) %>% 
    mutate(area_m2 = as.numeric(area_m2)) %>% 
    group_by(plot_ID) %>% 
    arrange(area_m2) %>% 
    # lowest area receives stand ID C, then B, then A
    mutate(stand = case_when(
      row_number()== 1 ~ "B",
      row_number()== 2 ~ "A",
      TRUE ~ NA)) %>% 
    # make stand.df joinable by only leaving plot_ID, e_ID, no matter the diameter of the CCS
    select(- c(CCS_r_m, inter_stat, area_m2))
  
  # join in stand info based on area of the edge segment by plot and edge ID
  inter.area.df <- inter.area.df %>% left_join(., stand.df, by = c("plot_ID", "e_ID", "inv_year"))
  
  
  # list with inter and remaining circle areas areas
  edges.list.geo[[i]] <- inter.area.df
  
  # create lists with polgons of intersections if there are intersections, if there is non, save the edge triangle polygone instead. 
  inter.poly.list.geo[[i]] <- if(isTRUE(nrow(inter.poly.17)!= 0)){c(inter.poly.17)}else{c(my.poly)}
  
  # testing if corect inter was saved: 
  #  i.plot <- if(isTRUE(nrow(inter.poly.17)!= 0)){c(inter.poly.17)}else{c(my.poly)}
  # plot(i.plot$geometry)
  #  plot(circle.17, add = T)
  
  # save remaining circles polygones into list
  #plot(remaining.circle.poly.17)
  remaining.circle.poly.17$plot_ID <- my.plot.id
  remaining.circle.poly.17$e_ID <- 0
  remaining.circle.poly.17$inv_year <- my.inv.year
  remaining.circle.poly.17$e_form <- 0
  remaining.circle.poly.17$geometry <- remaining.circle.poly.17$geometry
  # remaining.circle.poly.17$stand <- stand.df$stand[stand.df$e_ID == remaining.circle.poly.17$e_ID]
  # #plot(remaining.circle.poly.17)
  # create list wit polygones of the remaining cirlce when it´s only one polygone
  remaining.circle.poly.list.geo[[i]] <- if(st_geometry_type(remaining.circle.poly.17)== "POLYGON"){c(remaining.circle.poly.17)}else{}
  # create list wit polygones of the remaining cirlce when it´s a multipoligone
  remaining.circle.multipoly.list.geo[[i]] <- if(st_geometry_type(remaining.circle.poly.17)== "MULTIPOLYGON"){c(remaining.circle.poly.17)}else{}
  
  
}
# list of areas
edges.area.df.geo <- as.data.frame( rbindlist(edges.list.geo))

# list of polygones of forest edges 
inter.poly.one.edge.df.geo <- as.data.frame(rbindlist(inter.poly.list.geo, fill=TRUE))

# list of polygones of remainign circles 
rem.circle.poly.df.geo <- as.data.frame(rbindlist(remaining.circle.poly.list.geo, fill = TRUE))#[,c(2,1,4)]  %>% distinct()
# list of multipolygones of remaining circles
rem.circle.multipoly.df.geo <- as.data.frame(rbindlist(remaining.circle.multipoly.list.geo))#[,c(2,1,4)] %>% distinct()
# binding the both circle lists back together 
rem.circle.one.edge.df.geo <- plyr::rbind.fill(rem.circle.poly.df.geo, rem.circle.multipoly.df.geo)


# 3.2.1.3.2. plots with 1 outer edge: loop for intersections for plots with once adge and edge type %in% c(1, 2) --------

# dataprep for loop
# createa dataframe with plots that have only one forest edges
forest_edges.man.sub.1.outer.edge.geo <- forest_edges.man %>% # rows:84
  # select only plots with a known edge form and for edge 2 only those that actually intersect the 17m circle
  filter(e_form == 1 | e_form == 2 & inter_status_AT_17 == "two I" | e_form == 2 & inter_status_BT_17 == "two I") %>%  # rows:81
  # remove plots that have two edges
  anti_join(forest_edges.man %>%  filter(e_form == 1 | e_form == 2 & inter_status_AT_17 == "two I" | e_form == 2 & inter_status_BT_17 == "two I") %>% 
              group_by(plot_ID) %>% summarise(n = n()) %>% filter(n > 1) %>% select(plot_ID), by = "plot_ID")  %>% # 14 plots with 2 edges --> 28 rows -> 53 left
  filter(e_type %in% c(1, 2)) %>% ## georef
  ## remove plots that do now have a corresponding center coordiante in the HBI loc document
  ## georef
  semi_join(geo_loc %>% filter(!is.na( RW_MED) & !is.na(HW_MED)) %>%  select(plot_ID)  %>% distinct(), by = "plot_ID") # nrow = 52 --> there is 1 plots without corresponding

# prepare output datasets
outer.edges.list.geo <- vector("list", length = length(unique(forest_edges.man.sub.1.outer.edge.geo$plot_ID)))

outer.inter.poly.list.geo <- vector("list", length = length(unique(forest_edges.man.sub.1.outer.edge.geo$plot_ID)))
outer.inter.multipoly.list.geo <- vector("list", length = length(unique(forest_edges.man.sub.1.outer.edge.geo$plot_ID)))
#inter.poly.NA.list <- vector("list", length = length(unique(forest_edges.man.sub.1.edge$plot_ID)))
outer.remaining.circle.poly.list.geo <- vector("list", length = length(unique(forest_edges.man.sub.1.outer.edge.geo$plot_ID)))
outer.remaining.circle.multipoly.list.geo <- vector("list", length = length(unique(forest_edges.man.sub.1.outer.edge.geo$plot_ID)))

# loop for intersection of all edge triablge polygoens woth their respective sampling cirlce for plots with one edge only
for (i in 1:length(unique(forest_edges.man.sub.1.outer.edge.geo$plot_ID))){ 
  # i = 1
  #i = which(grepl(60130    , (forest_edges.man.sub.1.outer.edge.geo$plot_ID)))
  
  
  # select plot ID of the respective circle 
  my.plot.id <- forest_edges.man.sub.1.outer.edge.geo[i, "plot_ID"]
  my.e.form <- edge.poly.df.geo$e_form[edge.poly.df.geo$plot_ID == my.plot.id]
  my.e.id <- edge.poly.df.geo$e_ID[edge.poly.df.geo$plot_ID == my.plot.id]
  my.inv.year <- forest_edges.man.sub.1.outer.edge.geo[i, "inv_year"]
  
  
  ## georef
  ##  select UTM corrdinates of the plot center
  my.center.easting <- as.numeric(geo_loc[geo_loc$plot_ID == my.plot.id, "RW_MED"])
  my.center.northing <- as.numeric(geo_loc[geo_loc$plot_ID == my.plot.id, "HW_MED"])
  ## select crs ## georef
  my.utm.epsg <-  paste0("+proj=utm +zone=", pick_utm(my.center.easting)," ", "+datum=WGS84 +units=m +no_defs +type=crs")
  
  
  # circle data
  c.x0 = 0  + my.center.easting  ## georef
  c.y0 = 0  + my.center.northing ## georef
  c.r3 = 17.84
  c.r2 = 12.62
  c.r1 = 5.64
  center.df<- as.data.frame(cbind("lon" = c.x0, "lat" = c.y0))
  
  # build polygon (circlular buffer) around center point
  circle.pt <- sf::st_as_sf(center.df, coords = c("lon", "lat"))
  ## assing crs to cirlce corodiantes
  sf::st_crs(circle.pt) <- my.utm.epsg ## georef
  circle.17 <- sf::st_buffer(circle.pt, c.r3)
  circle.12 <- sf::st_buffer(circle.pt, c.r2)
  circle.5 <- sf::st_buffer(circle.pt, c.r1)
  
  
  # tree data to identify edge without trees
  outer.trees.df <- trees_data[trees_data$plot_ID == my.plot.id, ]
  my.tree.id <- outer.trees.df["tree_ID"]
  # calcualte polar coordinates of trees
  tree.coord.df <- outer.trees.df %>% 
    mutate(dist_tree = dist_cm/100, 
           x_tree = dist_tree*sin(azi_gon*pi/200), 
           y_tree = dist_tree*cos(azi_gon*pi/200), 
           lon = x_tree + my.center.easting, ##georef
           lat =  y_tree + my.center.northing) %>%  ##georef
    select(plot_ID, tree_ID, inv_year, lon, lat) %>% distinct()
  # create sf point object from dataframe
  #https://stackoverflow.com/questions/52551016/creating-sf-points-from-multiple-lat-longs
  tree.sf <-  sf::st_as_sf(tree.coord.df, coords = c("lon", "lat"), remove = FALSE)
  ## assing CRS to points
  sf::st_crs(tree.sf) <- my.utm.epsg ##georef
  
  
  ## select the respective polygones the circle is intersected by
  my.poly <- sf::st_as_sf(edge.poly.df.geo %>% filter(plot_ID == my.plot.id)) %>% 
    left_join(forest_edges.man %>% select(plot_ID, e_ID, e_type), by = c("plot_ID", "e_ID"))
  
  # print the cirlce and edge polygone
  # print(
  #   c(plot(st_geometry(circle.17), main = paste0("plot:", " ", my.plot.id, ",", " ", "e_form:"," ", my.e.form)) ,
  #       plot(st_geometry(my.poly), col = "green", add = TRUE),
  #       plot(st_geometry(tree.sf), add = TRUE))
  # )
  
  # first we have to figure out which of the both halfs is the circle and which is the edge
  # this is done by identifying the partial cirlce intersecting with the circles center
  # if the poly holds the center of the plot we change it´s geometry to 
  # the respective other half of the circle to make sure that the edge poly is
  # actually the edge
  if(isTRUE(nrow(sf::st_intersection(my.poly, circle.pt)) != 0) == T){
    my.poly$geometry <- sf::st_difference(circle.17, my.poly)$geometry
  }
  
  
  
  #### 17m circle
  # calculate intersection for 17m circle 
  inter.poly.17  <- sf::st_intersection(circle.17, my.poly)
  
  inter.status.poly.17 <- ifelse(nrow(inter.poly.17) == 0, "no intersections",
                                 ifelse(my.e.form == 1 & inter.poly.17$geometry == circle.17$geometry,  "no intersections",
                                        ifelse(my.e.form == 2 & inter.poly.17$geometry == circle.17$geometry, "fully covering circle", 
                                               "partly intersecting")))
  # this is just to remove all the additional attributes from the intersection polygone
  #inter.poly  <- sf::st_intersection(circle.17, st_geometry(my.poly))
  # if the ednge covers all of the circle remaining, the inter.polygone its going to be set to 0 so we know there are no direct intersections
  inter.poly.17.a <- if(isTRUE(inter.poly.17) && inter.poly.17$geometry == circle.17$geometry){inter.poly.17 <- data.frame()}else{inter.poly.17}
  # if the edge-circle intersection is equal to 0 (so there is no intersection) return the whole cirlce as remaining circle area, else calculate the remaining circle by decuctng the intersection are from the circle area
  remaining.circle.poly.17.a  <- if(isTRUE(nrow(inter.poly.17)==0)){circle.17}else{sf::st_difference(circle.17, inter.poly.17)}
  # select "correct" side of the edge: in case of outer forest edges, the polygone containing the center of the plot has to be the remaining circle. 
  inter.poly.17<- if(nrow(st_intersection(inter.poly.17.a, circle.pt))==0){inter.poly.17.a}else{remaining.circle.poly.17.a}
  remaining.circle.poly.17 <- if(nrow(st_intersection(remaining.circle.poly.17.a, circle.pt))!=0){remaining.circle.poly.17.a}else{inter.poly.17.a}
  
  
  
  # calculate area
  # intersection
  inter.area.17 <- ifelse(nrow(inter.poly.17) == 0, 0, sf::st_area(inter.poly.17))
  #remaining circle
  remaining.circle.area.17 <- ifelse(nrow(remaining.circle.poly.17) == 0, 0, sf::st_area(remaining.circle.poly.17))
  # create area dataframe for areas
  inter.area.df.17 <- as.data.frame(cbind("plot_ID" = c(my.plot.id, my.plot.id), 
                                          "e_ID" = c(my.e.id,  0),
                                          "inv_year" = c(my.inv.year, my.inv.year),
                                          # "e_form" = c(my.e.form, 0),
                                          #"shape" = c("edge", "circle"),
                                          "CCS_r_m" = c(c.r3, c.r3), "inter_stat" = c(inter.status.poly.17, 0),
                                          "area_m2" = c(inter.area.17, remaining.circle.area.17)))
  ##### 12m circle
  # calculate intersection for 17m circle 
  inter.poly.12  <- sf::st_intersection(circle.12, my.poly)
  inter.status.poly.12 <- ifelse(nrow(inter.poly.12) == 0, "no intersections",
                                 ifelse(my.e.form == 1 & inter.poly.12$geometry == circle.12$geometry,  "no intersections",
                                        ifelse(my.e.form == 2 & inter.poly.12$geometry == circle.12$geometry, "fully covering circle", 
                                               "partly intersecting")))
  # this is just to remove all the additional attributes from the intersection polygone
  #inter.poly  <- sf::st_intersection(circle.17, st_geometry(my.poly))
  # if the ednge covers all of the circle remaining, the inter.polygone its going to be set to 0 so we know there are no direct intersections
  inter.poly.12.a <- if(isTRUE(inter.poly.12) && inter.poly.12$geometry == circle.12$geometry){inter.poly.12 <- data.frame()}else{inter.poly.12}
  # if the edge-circle intersection is equal to 0 (so there is no intersection) return the whole cirlce as remaining circle area, else calculate the remaining circle by decuctng the intersection are from the circle area
  remaining.circle.poly.12.a  <- if(isTRUE(nrow(inter.poly.12)==0)){circle.12}else{sf::st_difference(circle.12, inter.poly.12)}
  # select "correct" side of the edge: in case of outer forest edges, the polygone containing the center of the plot has to be the remaining circle. 
  inter.poly.12<- if(nrow(st_intersection(inter.poly.12.a, circle.pt))==0){inter.poly.12.a}else{remaining.circle.poly.12.a}
  remaining.circle.poly.12 <- if(nrow(st_intersection(remaining.circle.poly.12.a, circle.pt))!=0){remaining.circle.poly.12.a}else{inter.poly.12.a}
  
  
  # calculate area
  # intersection
  inter.area.12 <- ifelse(nrow(inter.poly.12) == 0, 0, sf::st_area(inter.poly.12))
  #remaining circle
  remaining.circle.area.12 <- ifelse(nrow(remaining.circle.poly.12) == 0, 0, sf::st_area(remaining.circle.poly.12))
  # create area dataframe for areas
  inter.area.df.12 <- as.data.frame(cbind("plot_ID" = c(my.plot.id, my.plot.id), "e_ID" = c(my.e.id,  0),
                                          "inv_year" = c(my.inv.year, my.inv.year),
                                          # "e_form" = c(my.e.form, 0),
                                          #"shape" = c("edge", "circle"),
                                          "CCS_r_m" = c(c.r2, c.r2),"inter_stat" = c(inter.status.poly.12, 0),
                                          "area_m2" = c(inter.area.12, remaining.circle.area.12)))
  
  ##### 5m circle
  # calculate intersection for 17m circle 
  inter.poly.5  <- sf::st_intersection(circle.5, my.poly)
  inter.status.poly.5 <- ifelse(nrow(inter.poly.5) == 0, "no intersections",
                                ifelse(my.e.form == 1 & inter.poly.5$geometry == circle.5$geometry,  "no intersections",
                                       ifelse(my.e.form == 2 & inter.poly.5$geometry == circle.5$geometry, "fully covering circle", 
                                              "partly intersecting")))
  # this is just to remove all the additional attributes from the intersection polygone
  #inter.poly  <- sf::st_intersection(circle.17, st_geometry(my.poly))
  # if the ednge covers all of the circle remaining, the inter.polygone its going to be set to 0 so we know there are no direct intersections
  inter.poly.5.a <- if(isTRUE(inter.poly.5) && inter.poly.5$geometry == circle.5$geometry){inter.poly.5 <-data.frame()}else{inter.poly.5}
  # if the edge-circle intersection is equal to 0 (so there is no intersection) return the whole cirlce as remaining circle area, else calculate the remaining circle by decuctng the intersection are from the circle area
  remaining.circle.poly.5.a  <- if(isTRUE(nrow(inter.poly.5)==0)){circle.5}else{sf::st_difference(circle.5, inter.poly.5)}
  # select "correct" side of the edge: in case of outer forest edges, the polygone containing the center of the plot has to be the remaining circle. 
  inter.poly.5 <- if(nrow(st_intersection(inter.poly.5.a, circle.pt))==0){inter.poly.5.a}else{remaining.circle.poly.5.a}
  
  remaining.circle.poly.5 <- if(nrow(st_intersection(remaining.circle.poly.5.a, circle.pt))!=0){remaining.circle.poly.5.a}else{inter.poly.5.a}
  
  # calculate area
  # intersection
  inter.area.5 <- ifelse(nrow(inter.poly.5) == 0, 0, sf::st_area(inter.poly.5))
  #remaining circle
  remaining.circle.area.5 <- ifelse(nrow(remaining.circle.poly.5) == 0, 0, sf::st_area(remaining.circle.poly.5))
  # create area dataframe for areas
  inter.area.df.5 <- as.data.frame(cbind("plot_ID" = c(my.plot.id, my.plot.id), "e_ID" = c(my.e.id,  0),
                                         "inv_year" = c(my.inv.year, my.inv.year),
                                         # "e_form" = c(my.e.form, 0),
                                         #"shape" = c("edge", "circle"),
                                         "CCS_r_m" = c(c.r1, c.r1),"inter_stat" = c(inter.status.poly.5, 0),
                                         "area_m2" = c(inter.area.5, remaining.circle.area.5)))
  
  # bind area dataframes together
  inter.area.df <- rbind(inter.area.df.17, inter.area.df.12, inter.area.df.5)
  
  
  
  # assing stand to the edges depedning on area
  # to assign the stand we have to check if the edge polygone (defined earlier
  # as the part of the poly that doesn´t include the center of the cirlce)
  # doesn´t contain trees
  
  # if the edge poly doesn´t have trees and doesnt have the center its a "no forest" 
  # and the remaining circle is stand A
  if(isTRUE(nrow(sf::st_intersection(my.poly, tree.sf)) == 0 & 
            nrow(sf::st_intersection(my.poly, circle.pt)) == 0) == T){
    remaining.circle.poly.17$stand <- "A"
    my.poly$stand <- "no forest"
    # if the edge poly contains trees tho its identified as edge since 
    # it doesn´t hold the center of the plot we assign the status "warning"
  }else if(isTRUE(nrow(sf::st_intersection(my.poly, tree.sf)) != 0)==T){
    my.poly$stand <- "warning" 
    remaining.circle.poly.17$stand <- "A"
  }else{
    remaining.circle.poly.17$stand <- NA
    my.poly$stand <- NA
  }
  
  
  inter.area.df <- inter.area.df%>%
    mutate(stand = case_when(
      e_ID > 0  ~ my.poly$stand, # e_ID == 1 or 2 is the edge plygone, this will apply for all circles
      e_ID == 0 ~ remaining.circle.poly.17$stand, # e_ID == 0 represents the remaoning cricle
      TRUE ~ NA)) %>% 
    select(plot_ID, e_ID, inv_year, CCS_r_m ,inter_stat, area_m2,stand)
  
  
  # print(  c(plot(circle.17$geometry, main = paste0(my.plot.id, " - ", my.e.form, " - ", my.e.form)),
  #           plot(remaining.circle.poly.17$geometry, col = "grey", add = T),
  #           plot(remaining.circle.poly.12$geometry, add = T),
  #           plot(remaining.circle.poly.5$geometry, add = T),
  #           plot(inter.poly.17$geometry, col = "green", add =TRUE),
  #           plot(circle.pt$geometry, col = "red",  add = TRUE),
  #           legend("topleft", legend=c(paste0(unique(inter.area.df$stand[inter.area.df$e_ID == my.poly$e_ID]),":",  my.poly$e_type),
  #                                      paste0(unique(inter.area.df$stand[inter.area.df$e_ID == 0]),":","rem_circle")),
  #                  col=c("green", "grey"), lty=1:2, cex=0.8),
  #           plot(st_geometry(tree.sf), add = TRUE)))
  
  
  # list with inter and remaining circle areas areas
  outer.edges.list.geo[[i]] <- inter.area.df
  
  # create lists with polgons of intersections if there are intersections, if there is non, save the edge triangle polygone instead. 
  edge.poly <- if(isTRUE(nrow(inter.poly.17)!= 0)){inter.poly.17}else{my.poly}
  # if the edge.poly doesn´t have a stand yet we have to assign it later
  edge.poly <- if(isTRUE(length(edge.poly$stand)== 0)){edge.poly %>% mutate(stand = my.poly$stand) }else{edge.poly}
  # create list wit polygones of the remaining cirlce when it´s only one polygone
  outer.inter.poly.list.geo[[i]] <- if(st_geometry_type(edge.poly)== "POLYGON"){c(edge.poly)}else{}
  # create list wit polygones of the remaining cirlce when it´s a multipoligone
  outer.inter.multipoly.list.geo[[i]] <- if(st_geometry_type(edge.poly)== "MULTIPOLYGON"){c(edge.poly)}else{}
  
  
  # testing if corect inter was saved: 
  #  i.plot <- if(isTRUE(nrow(inter.poly.17)!= 0)){c(inter.poly.17)}else{c(my.poly)}
  # plot(i.plot$geometry)
  #  plot(circle.17, add = T)
  
  # save remaining circles polygones into list
  #plot(remaining.circle.poly.17)
  remaining.circle.poly.17$plot_ID <- my.plot.id
  remaining.circle.poly.17$e_ID <- 0
  remaining.circle.poly.17$inv_year <- my.inv.year
  remaining.circle.poly.17$e_form <- 0
  remaining.circle.poly.17$geometry <- remaining.circle.poly.17$geometry
  # plot(remaining.circle.poly.17)
  # create list wit polygones of the remaining cirlce when it´s only one polygone
  outer.remaining.circle.poly.list.geo[[i]] <- if(st_geometry_type(remaining.circle.poly.17)== "POLYGON"){c(remaining.circle.poly.17)}else{}
  # create list wit polygones of the remaining cirlce when it´s a multipoligone
  outer.remaining.circle.multipoly.list.geo[[i]] <- if(st_geometry_type(remaining.circle.poly.17)== "MULTIPOLYGON"){c(remaining.circle.poly.17)}else{}
  
  
}
# list of areas
outer.edges.area.df.geo <- as.data.frame( rbindlist(outer.edges.list.geo))
# list of polygones of forest edges 
outer.inter.poly.df.geo <- as.data.frame(rbindlist(outer.inter.poly.list.geo, fill = TRUE))#[,c(2,1,4)]  %>% distinct()
# list of multipolygones of remaining circles
outer.inter.multipoly.df.geo <- as.data.frame(rbindlist(outer.inter.multipoly.list.geo))#[,c(2,1,4)] %>% distinct()
# binding the both circle lists back together 
outer.inter.poly.one.edge.df.geo <- plyr::rbind.fill(outer.inter.poly.df.geo, outer.inter.multipoly.df.geo)

# list of polygones of remainign circles 
outer.rem.circle.poly.df.geo <- as.data.frame(rbindlist(outer.remaining.circle.poly.list.geo, fill = TRUE))#[,c(2,1,4)]  %>% distinct()
# list of multipolygones of remaining circles
outer.rem.circle.multipoly.df.geo <- as.data.frame(rbindlist(outer.remaining.circle.multipoly.list.geo))#[,c(2,1,4)] %>% distinct()
# binding the both circle lists back together 
outer.rem.circle.one.edge.df.geo <- plyr::rbind.fill(outer.rem.circle.poly.df.geo, outer.rem.circle.multipoly.df.geo)








# 3.2.1.3.1. plots with 2 edges: loop for intersections for plots with two edges ----------------------------------------------------------------------------------------------------------------------------
# dataprep for loop
# createa dataframe with plots that have only one forest edges
forest_edges.man.sub.2.edges.geo <- forest_edges.man %>% # rows:84
  # select only plots with a known edge form and for edge 2 only those that actually intersect the 17m circle
  filter(e_form == 1 | 
           e_form == 2 & inter_status_AT_17 == "two I" | 
           e_form == 2 & inter_status_BT_17 == "two I") %>%  # rows:81
  #filter(inter_status_AB_17 == "two I") %>% 
  # remove plots that have two edges
  semi_join(forest_edges.man %>% filter(e_form == 1 | 
                                          e_form == 2 & inter_status_AT_17 == "two I" | 
                                          e_form == 2 & inter_status_BT_17 == "two I") %>% 
              group_by(plot_ID) %>% summarise(n = n()) %>% filter(n > 1) %>% select(plot_ID), by = "plot_ID") %>% 
  anti_join(., forest_edges.man %>% filter(e_type %in% c(1, 2)) %>% select(plot_ID) %>% distinct(), by = "plot_ID") %>% 
  arrange(plot_ID, e_ID) %>% # 14 plots iwth 2 edges --> 28 rows ##georef
# remove plots that do now have a corresponding center coordiante in the HBI loc document
  semi_join(geo_loc %>% 
              filter(!is.na( RW_MED) & !is.na(HW_MED)) %>%  
              select(plot_ID)  %>% distinct(), by = "plot_ID") # nrow = 28 


# prepare output lists
# list to save areas in
edges.list.two.edges.geo <- vector("list", length = length(unique(forest_edges.man.sub.2.edges.geo$plot_ID)))
# list to save the first intersection polygone per plot in
inter.poly.1.list.geo <- vector("list", length = length(unique(forest_edges.man.sub.2.edges.geo$plot_ID)))
inter.multipoly.1.list.geo<- vector("list", length = length(unique(forest_edges.man.sub.2.edges.geo$plot_ID)))
# list to save the second intersection polygone per plot in
inter.poly.2.list.geo <- vector("list", length = length(unique(forest_edges.man.sub.2.edges.geo$plot_ID)))
inter.multipoly.2.list.geo <- vector("list", length = length(unique(forest_edges.man.sub.2.edges.geo$plot_ID)))

# list to save the remaining circle polygones per plot in
rem.circle.poly.2.edges.list.geo <- vector("list", length = length(unique(forest_edges.man.sub.2.edges.geo$plot_ID)))
# list to save the remaining circle MULTIpolygones per plot in
rem.circle.multipoly.2.edges.list.geo <- vector("list", length = length(unique(forest_edges.man.sub.2.edges.geo$plot_ID)))
# list for plop IDs of those plots where the edge lines/ polygones intersect within the 17.84m circle
intersection.warning.edges.list.geo <- vector("list", length = length(unique(forest_edges.man.sub.2.edges.geo$plot_ID)))
if(isTRUE(nrow(forest_edges.man.sub.2.edges.geo) != 0) == T){
for (i in 1:length(unique(forest_edges.man.sub.2.edges.geo$plot_ID))){ 
  #i = 13
  # i = which(grepl(60076, unique(forest_edges.man.sub.2.edges.geo$plot_ID)))
  
  #if(nrow(forest_edges.man.sub.2.edges.geo) == 0){break}
  
  # select plot ID of the respective circle 
  my.plot.id <- unique(forest_edges.man.sub.2.edges.geo$plot_ID)[i]
  my.inv.year <- unique(forest_edges.man.sub.2.edges.geo[i, c("plot_ID", "inv_year")])[, "inv_year"]
  
  ## georef
  ## select the UTM coordiantes of the center of the cirlce corresponding with the plot ID
  my.center.easting <- as.numeric(geo_loc[geo_loc$plot_ID == my.plot.id, "RW_MED"]) ## georef
  my.center.northing <- as.numeric(geo_loc[geo_loc$plot_ID == my.plot.id, "HW_MED"]) ## georef
  ## select crs
  my.utm.epsg <-  paste0("+proj=utm +zone=", pick_utm(my.center.easting)," ", "+datum=WGS84 +units=m +no_defs +type=crs") ## georef
  
  
  #### build circle
  # circle data
  c.x0 = 0     + my.center.easting ## georef
  c.y0 = 0     + my.center.northing ## georef
  c.r3 = 17.84
  c.r2 = 12.62
  c.r1 = 5.64
  # build polygon (circlular buffer) around center point
  center.df<- as.data.frame(cbind("lon" = c.x0, "lat" = c.y0))
  circle.pt <- sf::st_as_sf(center.df, coords = c("lon", "lat"))   ## georef
  ## georef
  ## assing crs to cirlce corodiantes
  sf::st_crs(circle.pt) <- my.utm.epsg   ## georef
  circle.17 <- sf::st_buffer(circle.pt, c.r3)   ## georef
  circle.12 <- sf::st_buffer(circle.pt, c.r2)     ## georef
  circle.5 <- sf::st_buffer(circle.pt, c.r1)   ## georef
  
  #### select the  polygones the circle is intersected by
  # select the polygones with the same plot ID as the cirlce
  my.plot.polys.df <- edge.poly.df.geo %>% filter(plot_ID == my.plot.id) %>% arrange(e_ID)
  # create the polygones of the edge geometries
  my.poly.1 <- sf::st_as_sf(my.plot.polys.df[1,])
  my.poly.2 <- sf::st_as_sf(my.plot.polys.df[2,])
  # select edge ID of edge polygones
  my.e.id.1 <- my.plot.polys.df$e_ID[1]
  my.e.id.2 <- my.plot.polys.df$e_ID[2]
  # select edge form of the respective edge polygones
  my.e.form.1 <- my.plot.polys.df$e_form[1]
  my.e.form.2 <- my.plot.polys.df$e_form[2]
  
  # print edges and circle
  # print(c(plot(circle.17$geometry, col = "grey"),
  #       plot(my.poly.1$geometry, col = "blue", add = T),
  #       plot(my.poly.2$geometry, col = "red", add = T)))
  
  #### intersections between polygones and circles   
  ### 17m circle 
  my.circle = circle.17
  ## create poolygon of intersection for first polygon with circle
  inter.poly.17.1  <- st_intersection(my.circle, my.poly.1)
  inter.status.poly.17.1 <- ifelse(nrow(inter.poly.17.1) == 0, "no intersections",
                                   ifelse(my.e.form.1 == 1 & inter.poly.17.1$geometry == my.circle$geometry,  "no intersections",
                                          ifelse(my.e.form.1 == 2 & inter.poly.17.1$geometry == my.circle$geometry, "fully covering circle", 
                                                 "partly intersecting")))
  # if the first ednge covers all of the circle remaining its going to be set to 0 so we know there are no direct intersections and the circle is passed on to the next edge to calcualte the intersection
  # https://www.statology.org/r-argument-is-of-length-zero/
  inter.poly.17.1 <- if(isTRUE(inter.poly.17.1) && inter.poly.17.1$geometry == my.circle$geometry){inter.poly.17.1 <- data.frame()}else{inter.poly.17.1}
  
  ## create poolygon of remaining circle after first edge polygone is intersected
  # create poly with remaining area: https://gis.stackexchange.com/questions/353633/r-spatial-erase-one-polygon-from-another-correct-use-of-st-difference
  remaining.circle.17.1 <- if(nrow(inter.poly.17.1)==0){my.circle}else{sf::st_difference(my.circle, inter.poly.17.1)}
  #print(plot(remaining.circle.17.1$geometry, main = paste0(my.plot.id, "-", my.e.form.1,  "-", c.r3))) 
  
  ## create polygone of intersecting area of second polygone with remaining circle
  inter.poly.17.2 <- st_intersection(my.poly.2, st_geometry(remaining.circle.17.1))
  inter.status.poly.17.2 <- ifelse(nrow(inter.poly.17.2) == 0, "no intersections",
                                   ifelse(my.e.form.2== 1 & inter.poly.17.2$geometry == remaining.circle.17.1$geometry,  "no intersections",
                                          ifelse(my.e.form.2 == 2 & inter.poly.17.2$geometry == remaining.circle.17.1$geometry, "fully covering circle", 
                                                 "partly intersecting")))
  # if the second edge covers all of the circle remaining its going to be set to 0 so we know there are no direct intersections
  # https://www.statology.org/r-argument-is-of-length-zero/
  inter.poly.17.2 <- if(isTRUE(inter.poly.17.2) && inter.poly.17.2$geometry == remaining.circle.17.1$geometry){inter.poly.17.2 <- data.frame()}else{inter.poly.17.2}
  
  ## create polygone of the  remaining cricle after both intersects are decucted
  # so the area of the frst remining circle minus the area of the second remaining circle 
  remaining.circle.17.1.and.2.poly <- if(nrow(inter.poly.17.2)==0){remaining.circle.17.1}else{sf::st_difference(remaining.circle.17.1, inter.poly.17.2)}
  # as there was a problem with mutlipolygones and geometry collections being created in this step, we have to extract the collection into one poly
  remaining.circle.17.1.and.2.poly <- if(isTRUE(sf::st_geometry_type(remaining.circle.17.1.and.2.poly) == "GEOMETRYCOLLECTION") == T){
    sf::st_collection_extract(remaining.circle.17.1.and.2.poly, c("POLYGON"), warn = FALSE)} else{remaining.circle.17.1.and.2.poly}
  #print(plot(remaining.circle.17.1.and.2.poly$geometry, main = paste0(my.plot.id, "-", my.e.form.2,  "-", c.r3))) 
  
  
  ### 12m circle 
  my.circle = circle.12
  ## create poolygon of intersection for first polygon with circle
  inter.poly.12.1  <- st_intersection(my.circle, my.poly.1)
  inter.status.poly.12.1 <- ifelse(nrow(inter.poly.12.1) == 0, "no intersections",
                                   ifelse(my.e.form.1 == 1 & inter.poly.12.1$geometry == my.circle$geometry,  "no intersections",
                                          ifelse(my.e.form.1 == 2 & inter.poly.12.1$geometry == my.circle$geometry, "fully covering circle", 
                                                 "partly intersecting")))
  # if the first ednge covers all of the circle remaining its going to be set to 0 so we know there are no direct intersections and the circle is passed on to the next edge to calcualte the intersection
  # https://www.statology.org/r-argument-is-of-length-zero/
  inter.poly.12.1 <- if(isTRUE(inter.poly.12.1) && inter.poly.12.1$geometry == my.circle$geometry){inter.poly.12.1 <- data.frame()}else{inter.poly.12.1}
  
  ## create poolygon of remaining circle after first edge polygone is intersected
  # create poly with remaining area: https://gis.stackexchange.com/questions/353633/r-spatial-erase-one-polygon-from-another-correct-use-of-st-difference
  remaining.circle.12.1 <- if(nrow(inter.poly.12.1)==0){my.circle}else{sf::st_difference(my.circle, inter.poly.12.1)}
  # print(plot(remaining.circle.12.1$geometry, main = paste0(my.plot.id, "-",my.e.form.1,  "-", c.r2))) 
  
  ## create polygone of intersecting area of second polygone with remaining circle
  inter.poly.12.2 <- st_intersection(my.poly.2, st_geometry(remaining.circle.12.1))
  inter.status.poly.12.2 <- ifelse(nrow(inter.poly.12.2) == 0, "no intersections",
                                   ifelse(my.e.form.2== 1 & inter.poly.12.2$geometry == remaining.circle.12.1$geometry,  "no intersections",
                                          ifelse(my.e.form.2 == 2 & inter.poly.12.2$geometry == remaining.circle.12.1$geometry, "fully covering circle", 
                                                 "partly intersecting")))
  # if the second edge covers all of the circle remaining its going to be set to 0 so we know there are no direct intersections
  # https://www.statology.org/r-argument-is-of-length-zero/
  inter.poly.12.2 <- if(isTRUE(inter.poly.12.2) && inter.poly.12.2$geometry == remaining.circle.12.1$geometry){inter.poly.12.2 <- data.frame()}else{inter.poly.12.2}
  
  ## create polygone of the  remaining cricle after both intersects are decucted
  # so the area of the frst remining circle minus the area of the second remaining circle 
  remaining.circle.12.1.and.2.poly <- if(nrow(inter.poly.12.2)==0){remaining.circle.12.1}else{sf::st_difference(remaining.circle.12.1, inter.poly.12.2)}
  # as there was a problem with mutlipolygones and geometry collections being created in this step, we have to extract the collection into one poly
  remaining.circle.12.1.and.2.poly <- if(isTRUE(sf::st_geometry_type(remaining.circle.12.1.and.2.poly) == "GEOMETRYCOLLECTION") == T){
    sf::st_collection_extract(remaining.circle.12.1.and.2.poly, c("POLYGON"), warn = FALSE)} else{remaining.circle.12.1.and.2.poly}
  #print(plot(remaining.circle.12.1.and.2.poly$geometry, main = paste0(my.plot.id, "-", my.e.form.2,  "-", c.r2)))
  
  ### 5m circle 
  my.circle = circle.5
  ## create poolygon of intersection for first polygon with circle
  inter.poly.5.1  <- st_intersection(my.circle, my.poly.1)
  inter.status.poly.5.1 <- ifelse(nrow(inter.poly.5.1) == 0, "no intersections",
                                  ifelse(my.e.form.1 == 1 & inter.poly.5.1$geometry == my.circle$geometry,  "no intersections",
                                         ifelse(my.e.form.1 == 2 & inter.poly.5.1$geometry == my.circle$geometry, "fully covering circle", 
                                                "partly intersecting")))
  # if the first ednge covers all of the circle remaining its going to be set to 0 so we know there are no direct intersections and the circle is passed on to the next edge to calcualte the intersection
  # https://www.statology.org/r-argument-is-of-length-zero/
  inter.poly.5.1 <- if(isTRUE(inter.poly.5.1) && inter.poly.5.1$geometry == my.circle$geometry){inter.poly.5.1 <- data.frame()}else{inter.poly.5.1}
  
  ## create poolygon of remaining circle after first edge polygone is intersected
  # create poly with remaining area: https://gis.stackexchange.com/questions/353633/r-spatial-erase-one-polygon-from-another-correct-use-of-st-difference
  remaining.circle.5.1 <- if(nrow(inter.poly.5.1)==0){my.circle}else{sf::st_difference(my.circle, inter.poly.5.1)}
  # print(plot(remaining.circle.5.1$geometry, main = paste0(my.plot.id, "-",my.e.form.1,  "-", c.r1))) 
  
  ## create polygone of intersecting area of second polygone with remaining circle
  inter.poly.5.2 <- st_intersection(my.poly.2, st_geometry(remaining.circle.5.1))
  inter.status.poly.5.2 <- ifelse(nrow(inter.poly.5.2) == 0, "no intersections",
                                  ifelse(my.e.form.2== 1 & inter.poly.5.2$geometry == remaining.circle.5.1$geometry,  "no intersections",
                                         ifelse(my.e.form.2 == 2 & inter.poly.5.2$geometry == remaining.circle.5.1$geometry, "fully covering circle", 
                                                "partly intersecting")))
  # if the second edge covers all of the circle remaining its going to be set to 0 so we know there are no direct intersections
  # https://www.statology.org/r-argument-is-of-length-zero/
  inter.poly.5.2 <- if(isTRUE(inter.poly.5.2) && inter.poly.5.2$geometry == remaining.circle.5.1$geometry){inter.poly.5.2 <- data.frame()}else{inter.poly.5.2}
  
  ## create polygone of the  remaining cricle after both intersects are decucted
  # so the area of the frst remining circle minus the area of the second remaining circle 
  remaining.circle.5.1.and.2.poly <- if(nrow(inter.poly.5.2)==0){remaining.circle.5.1}else{sf::st_difference(remaining.circle.5.1, inter.poly.5.2)}
  # as there was a problem with mutlipolygones and geometry collections being created in this step, we have to extract the collection into one poly
  remaining.circle.5.1.and.2.poly <- if(isTRUE(sf::st_geometry_type(remaining.circle.5.1.and.2.poly) == "GEOMETRYCOLLECTION") == T){
    sf::st_collection_extract(remaining.circle.5.1.and.2.poly, c("POLYGON"), warn = FALSE)} else{remaining.circle.5.1.and.2.poly}
  
  
  print( c(plot(remaining.circle.17.1.and.2.poly$geometry, main = paste0(my.plot.id, " - ", my.e.form.1, " - ", my.e.form.2)),
           plot(remaining.circle.12.1.and.2.poly$geometry, add = T),
           plot(remaining.circle.5.1.and.2.poly$geometry, add = T))
  )
  
  #### calculate the area
  ## 17m cricle
  # area of the intersection 1
  inter.17.1.area <- ifelse(nrow(inter.poly.17.1) == 0, 0, sf::st_area(inter.poly.17.1))
  # area of the intersection polygone 2
  inter.17.2.area <- ifelse(nrow(inter.poly.17.2) == 0, 0, sf::st_area(inter.poly.17.2))
  #  area of the remaining circle, after both intersections are deducted
  remaining.circle.area.17 <- sf::st_area(remaining.circle.17.1.and.2.poly)
  # save area in dataframe
  inter.area.df.17 <- as.data.frame(
    cbind("plot_ID" = c(my.plot.id, my.plot.id, my.plot.id), "e_ID" = c(my.e.id.1, my.e.id.2, 0),
          "inv_year" = c(my.inv.year, my.inv.year, my.inv.year),
          #"e_form" = c(my.poly.1$e_form, my.poly.2$e_form, 0),"shape" = c("edge", "edge", "circle"),
          "CCS_r_m" = c(c.r3, c.r3, c.r3), 
          "inter_stat" = c(inter.status.poly.17.1, inter.status.poly.17.2, 0),
          "area_m2" = c(inter.17.1.area, inter.17.2.area, remaining.circle.area.17)
    ))
  
  ## 12m cricle
  # area of the intersection 1
  inter.12.1.area <- ifelse(nrow(inter.poly.12.1) == 0, 0, sf::st_area(inter.poly.12.1))
  # area of the intersection polygone 2
  inter.12.2.area <- ifelse(nrow(inter.poly.12.2) == 0, 0, sf::st_area(inter.poly.12.2))
  #  area of the remaining circle, after both intersections are deducted
  remaining.circle.area.12 <- sf::st_area(remaining.circle.12.1.and.2.poly)
  # save area in dataframe
  inter.area.df.12 <- as.data.frame(
    cbind("plot_ID" = c(my.plot.id, my.plot.id, my.plot.id), "e_ID" = c(my.e.id.1, my.e.id.2, 0),
          #"e_form" = c(my.poly.1$e_form, my.poly.2$e_form, 0),"shape" = c("edge", "edge", "circle"),
          "inv_year" = c(my.inv.year, my.inv.year, my.inv.year),
          "CCS_r_m" = c(c.r2, c.r2, c.r2), 
          "inter_stat" = c(inter.status.poly.12.1, inter.status.poly.12.2, 0),
          "area_m2" = c(inter.12.1.area, inter.12.2.area, remaining.circle.area.12)
    ))
  
  ## 5m cricle
  # area of the intersection 1
  inter.5.1.area <- ifelse(nrow(inter.poly.5.1) == 0, 0, sf::st_area(inter.poly.5.1))
  # area of the intersection polygone 2
  inter.5.2.area <- ifelse(nrow(inter.poly.5.2) == 0, 0, sf::st_area(inter.poly.5.2))
  #  area of the remaining circle, after both intersections are deducted
  remaining.circle.area.5 <- sf::st_area(remaining.circle.5.1.and.2.poly)
  # save area in dataframe
  inter.area.df.5 <- as.data.frame(
    cbind("plot_ID" = c(my.plot.id, my.plot.id, my.plot.id), 
          "e_ID" = c(my.e.id.1, my.e.id.2, 0),
          #"e_form" = c(my.poly.1$e_form, my.poly.2$e_form, 0),"shape" = c("edge", "edge", "circle"),
          "inv_year" = c(my.inv.year, my.inv.year, my.inv.year),
          "CCS_r_m" = c(c.r1, c.r1, c.r1), 
          "inter_stat" = c(inter.status.poly.5.1, inter.status.poly.5.2, 0),
          "area_m2" = c(inter.5.1.area, inter.5.2.area, remaining.circle.area.5)
    ))
  
  
  # bind area datafames of all 3 circles together
  inter.area.df <- rbind(inter.area.df.17, inter.area.df.12,inter.area.df.5 )
  
  # assing stand to the edges depedning on area
  stand.df <- inter.area.df%>% 
    filter(CCS_r_m  == 17.84) %>% 
    mutate(area_m2 = as.numeric(area_m2)) %>% 
    group_by(plot_ID, inv_year) %>% 
    arrange(area_m2) %>% 
    # lowest area receives stand ID C, then B, then A
    mutate(stand = case_when(
      row_number()== 1 ~ "C",
      row_number()== 2 ~ "B",
      row_number()== 3 ~ "A",
      TRUE ~ NA)) %>% 
    # make stand.df joinable by only leaving plot_ID, e_ID, no matter the diameter of the CCS
    select(- c(CCS_r_m, inter_stat, area_m2))
  
  # join in stand info based on area of the edge segment
  inter.area.df <- inter.area.df %>% left_join(., stand.df, 
                                               by = c("plot_ID", "e_ID", "inv_year"))
  
  # save datacframe per plot in list
  edges.list.two.edges.geo[[i]] <- inter.area.df
  
  
  # create list with those plot ID where the two edges intersect within the radius of 17.84m
  intersection.between.edges.17 <- sf::st_intersection(
    sf::st_intersection(my.poly.1, circle.17), # intersection poly 1 and cirlce 17
    sf::st_intersection(my.poly.2, circle.17)) # intersection poly 2 and cirlce 17
  intersection.warning.edges <- ifelse(nrow(intersection.between.edges.17) == 0, NA, intersection.between.edges.17$plot_ID)
  intersection.warning.edges.list.geo[[i]] <- as.data.frame(cbind("plot_ID" = c(intersection.warning.edges)))
  
  ## save intersection polygones in list
  # poly.1
  inter.poly.both.types.1 <- if(nrow(inter.poly.17.1)!= 0){inter.poly.17.1}else{my.poly.1}
  inter.poly.1.list.geo[[i]] <- if(isTRUE(st_geometry_type(inter.poly.both.types.1) == "POLYGON") == T){c(inter.poly.both.types.1)}else{}
  inter.multipoly.1.list.geo[[i]] <- if(isTRUE(st_geometry_type(inter.poly.both.types.1) == "MULTIPOLYGON") == T){c(inter.poly.both.types.1)}else{}
  
  # poly.2
  inter.poly.both.types.2 <- if(nrow(inter.poly.17.2)!= 0){inter.poly.17.2}else{my.poly.2}
  inter.poly.2.list.geo[[i]] <- if(isTRUE(st_geometry_type(inter.poly.both.types.2) == "POLYGON") == T){c(inter.poly.both.types.2)}else{}
  inter.multipoly.2.list.geo[[i]] <- if(isTRUE(st_geometry_type(inter.poly.both.types.2) == "MULTIPOLYGON") == T){c(inter.poly.both.types.2)}else{}
  
  
  ## save the reimaingf circle polygones in a list
  remaining.circle.17.1.and.2.poly$plot_ID <- my.plot.id
  remaining.circle.17.1.and.2.poly$e_ID <- 0
  remaining.circle.17.1.and.2.poly$inv_year <- my.inv.year
  remaining.circle.17.1.and.2.poly$e_form <- 0
  remaining.circle.17.1.and.2.poly$geometry <- remaining.circle.17.1.and.2.poly$geometry
  # create list wit polygones of the remaining cirlce when it´s only one polygone
  rem.circle.poly.2.edges.list.geo[[i]] <- if(isTRUE(st_geometry_type(remaining.circle.17.1.and.2.poly)== "POLYGON") == T){c(remaining.circle.17.1.and.2.poly)}else{}
  # create list wit polygones of the remaining cirlce when it´s a multipoligone
  rem.circle.multipoly.2.edges.list.geo[[i]] <- if(isTRUE(st_geometry_type(remaining.circle.17.1.and.2.poly)== "MULTIPOLYGON") ==T){c(remaining.circle.17.1.and.2.poly)}else{}
  
}
}
# save areas into dataframe
edges.area.two.edges.df.geo <- as.data.frame(rbindlist(edges.list.two.edges.geo)) 
# save plot IDs with overlappig edges within the 17.84m circle into dataframe
intersection.two.edges.warning.df.geo <- na.omit(as.data.frame( rbindlist(intersection.warning.edges.list.geo, fill=TRUE)))
if(nrow(intersection.two.edges.warning.df.geo)!=0){print("There are plots with overlapping edges within a 17.84m radius around the plot center. 
                                                           Please check dataset intersection.two.edges.warning.df.geo")}

# save intersection polygones into dataframe 
# list of polygones 1 of forest edges 
inter.poly.1.two.edges.df.geo <-  plyr::rbind.fill(as.data.frame(rbindlist(inter.poly.1.list.geo, fill=TRUE)),as.data.frame(rbindlist(inter.multipoly.1.list.geo, fill=TRUE)))
# list of polygones 2 of forest edges 
inter.poly.2.two.edges.df.geo <-  plyr::rbind.fill(as.data.frame(rbindlist(inter.poly.2.list.geo, fill=TRUE)),as.data.frame(rbindlist(inter.multipoly.2.list.geo, fill=TRUE)))


# bind the both edges per plot together
inter.poly.two.edges.df.geo <- plyr::rbind.fill(inter.poly.1.two.edges.df.geo, inter.poly.2.two.edges.df.geo)

# list of polygones of remainign circles 
rem.circle.poly.two.edges.df.geo <- if(nrow(as.data.frame(rbindlist(rem.circle.poly.2.edges.list.geo)))!= 0){
  as.data.frame(rbindlist(rem.circle.poly.2.edges.list.geo, fill = TRUE))[,c("plot_ID", "inv_year","e_ID",  "e_form","geometry")] %>% distinct()
}else{  data.frame()}
# list of multipolygones of remaining circles
rem.circle.multipoly.two.edges.df.geo <- if(nrow(as.data.frame(rbindlist(rem.circle.multipoly.2.edges.list.geo)))!= 0){
  as.data.frame(rbindlist(rem.circle.multipoly.2.edges.list.geo, fill = TRUE))[,c("plot_ID", "inv_year","e_ID",  "e_form","geometry")] %>% distinct()
}else{  data.frame()}
# binding the both circle lists back together 
rem.circle.two.edges.df.geo <- if(isTRUE(nrow(rem.circle.poly.two.edges.df.geo) != 0 && nrow(rem.circle.multipoly.two.edges.df.geo) != 0) == T){
  plyr::rbind.fill(rem.circle.poly.two.edges.df.geo, rem.circle.multipoly.two.edges.df.geo)
}else{rem.circle.poly.two.edges.df.geo}







# 3.2.1.3.2. plots with 2 (outer) edges: loop for intersection of plots with 2 edges, edge type 1, 2 ---------------------------------------------------------------
# dataprep for loop
# createa dataframe with plots that have only one forest edges
forest_edges.man.sub.2.outer.edges.geo <-
  # check for plots with two edges that have an edge labelled type 1 or 2 
  forest_edges.man %>%
  # select only plots with a known edge form and for edge 2 
  # --> select only those that actually intersect the 17m circle
  filter(e_form == 1 | 
           e_form == 2 & inter_status_AT_17 == "two I" | 
           e_form == 2 & inter_status_BT_17 == "two I") %>%  # rows:81
  # filter plots that have two edges among those that have edges at all
  semi_join(forest_edges.man %>% filter(e_form == 1 | 
                                          e_form == 2 & inter_status_AT_17 == "two I" | 
                                          e_form == 2 & inter_status_BT_17 == "two I") %>% 
              group_by(plot_ID) %>% summarise(n = n()) %>% filter(n > 1) %>% select(plot_ID), by = "plot_ID") %>% 
  # select those plots among the plots htat have two edges of which at least one intersects the circle which 
  # have at least one edge_type of 1 or 2
  semi_join(., forest_edges.man %>% filter(e_type %in% c(1, 2)) %>% select(plot_ID), 
            by = "plot_ID") %>% 
  arrange(plot_ID, e_ID) %>% ## georef  # 14 plots iwth 2 edges --> 28 rows
  # remove plots that do now have a corresponding center coordiante in the HBI loc document
  semi_join(geo_loc %>% filter(!is.na( RW_MED) & !is.na(HW_MED)) %>%  select(plot_ID)  %>% distinct(), by = "plot_ID") # nrow = 28 


# prepare output lists
# list to save areas in
outer.edges.list.two.edges.geo <- vector("list", length = length(unique(forest_edges.man.sub.2.outer.edges.geo$plot_ID)))
# list to save the first intersection polygone per plot in
outer.inter.poly.1.list.geo <- vector("list", length = length(unique(forest_edges.man.sub.2.outer.edges.geo$plot_ID)))
outer.inter.multipoly.1.list.geo  <- vector("list", length = length(unique(forest_edges.man.sub.2.outer.edges.geo$plot_ID)))
# list to save the second intersection polygone per plot in
outer.inter.poly.2.list.geo <- vector("list", length = length(unique(forest_edges.man.sub.2.outer.edges.geo$plot_ID)))
outer.inter.multipoly.2.list.geo  <- vector("list", length = length(unique(forest_edges.man.sub.2.outer.edges.geo$plot_ID)))
# list to save the remaining circle polygones per plot in
outer.rem.circle.poly.2.edges.list.geo <- vector("list", length = length(unique(forest_edges.man.sub.2.outer.edges.geo$plot_ID)))
# list to save the remaining circle MULTIpolygones per plot in
outer.rem.circle.multipoly.2.edges.list.geo <- vector("list", length = length(unique(forest_edges.man.sub.2.outer.edges.geo$plot_ID)))
# list for plop IDs of those plots where the edge lines/ polygones intersect within the 17.84m circle
outer.intersection.warning.edges.list.geo <- vector("list", length = length(unique(forest_edges.man.sub.2.outer.edges.geo$plot_ID)))

for (i in 1:length(unique(forest_edges.man.sub.2.outer.edges.geo$plot_ID))){ 
  #i = 1
  # i = which(grepl(50009, unique(forest_edges.man.sub.2.outer.edges.geo$plot_ID)))
  
  #if(nrow(forest_edges.man.sub.2.outer.edges.geo) == 0){break}
  
  # select plot ID of the respective circle 
  my.plot.id <- unique(forest_edges.man.sub.2.outer.edges.geo$plot_ID)[i]
  my.inv.year <- unique(forest_edges.man.sub.2.outer.edges.geo[i, c("plot_ID", "inv_year")])[, "inv_year"]
  
  ## select the UTM coordiantes of the center of the cirlce corresponding with the plot ID
  my.center.easting <- as.numeric(geo_loc[geo_loc$plot_ID == my.plot.id, "RW_MED"]) ## georef
  my.center.northing <- as.numeric(geo_loc[geo_loc$plot_ID == my.plot.id, "HW_MED"]) ## georef
  ## select crs
  my.utm.epsg <-  paste0("+proj=utm +zone=", pick_utm(my.center.easting)," ", "+datum=WGS84 +units=m +no_defs +type=crs")
  
  
  #### build circle
  # circle data
  c.x0 = 0     + my.center.easting ## georef
  c.y0 = 0     + my.center.northing ## georef
  c.r3 = 17.84
  c.r2 = 12.62
  c.r1 = 5.64
  # build polygon (circlular buffer) around center point
  center.df<- as.data.frame(cbind("lon" = c.x0, "lat" = c.y0))
  circle.pt <- sf::st_as_sf(center.df, coords = c("lon", "lat"))
  ## assing crs to cirlce corodiantes
  sf::st_crs(circle.pt) <- my.utm.epsg ## georef
  circle.17 <- sf::st_buffer(circle.pt, c.r3)
  circle.12 <- sf::st_buffer(circle.pt, c.r2)
  circle.5 <- sf::st_buffer(circle.pt, c.r1)
  
  
  # tree data to identify edge without trees
  outer.trees.df <- trees_data[trees_data$plot_ID == my.plot.id, ]
  my.tree.id <- outer.trees.df["tree_ID"]
  # calcualte polar coordinates of trees
  tree.coord.df <- outer.trees.df %>% 
    mutate(dist_tree = dist_cm/100, 
           x_tree = dist_tree*sin(azi_gon*pi/200), 
           y_tree = dist_tree*cos(azi_gon*pi/200), 
           lon = x_tree+ my.center.easting,   ## georef
           lat =  y_tree + my.center.northing) %>%    ## georef
    select(plot_ID, tree_ID, inv_year, lon, lat) %>% distinct()
  # create sf point object from dataframe
  #https://stackoverflow.com/questions/52551016/creating-sf-points-from-multiple-lat-longs
  tree.sf <-  sf::st_as_sf(tree.coord.df, coords = c("lon", "lat"), remove = FALSE)
  ## georef
  ## assing CRS to points
  sf::st_crs(tree.sf) <- my.utm.epsg
  
  
  #### select the  polygones the circle is intersected by
  # select the polygones with the same plot ID as the cirlce
  my.plot.polys.df <- edge.poly.df.geo %>% 
    filter(plot_ID == my.plot.id ) %>% arrange(e_ID) %>% 
    left_join(., forest_edges.man %>% select(plot_ID, e_ID, e_type), by = c("plot_ID", "e_ID"))
  #  this part is about whicht polgone to priotiise if there are two overlapping edges: 
  # usually we would select the polygone with edge_ID == 1 to be the priotiised one. now however, we will select the polygone with 
  # no trees to be the prioritized 
  # plot(my.plot.polys.df$geometry[1])
  # plot(my.plot.polys.df$geometry[2], add = T, col = "grey")
  # plot(circle.17, add = T)
  # plot(circle.pt, add =T, col = "red")
  
  
  ## create the polygones of the edge geometries
  
  ## determine intersection status poly and circle center to identify "outer" side
  # we have to think about this again. 
  # actually  we have to sort the edges first by the question "is this poly an edge or not" 
  # the criteria to decide this by is the intersections with the circle center. 
  # is the cirlce center in the poly? then the poly becomes the rem. cirlce and the rem. cirlce becomes one of the edges. 
  # so actually i first have to intersect the edges with the circle and define the "side" they are on. then i start the intersection stuff
  # so poly 1 should be the half of the circle between poly.1 and cirlce that holds the center of the circle
  # same for poly 2
  # then only we check them for the order between poly 1 and 2 
  
  # if the edge poly is an outer edge and it includes the center of the cirlce the remaining cirlce after intersecting the cirlce and the edge poly becomes
  # the new edge poly 
  # if the poly doesnt cut the circle, it could never include the center so we don´t have to worry about this stuff 
  # poly 1
  if(isTRUE(my.plot.polys.df$e_type[1] %in% c(1,2) & nrow(sf::st_intersection(sf::st_as_sf(my.plot.polys.df[1,]), circle.pt) ) != 0 ) == T){
    my.plot.polys.df$geometry[1] <-  sf::st_geometry(sf::st_difference(circle.17, sf::st_as_sf(my.plot.polys.df[1,])))
  } # if the condition is not true do nothing
  # poly 2
  if(isTRUE(my.plot.polys.df$e_type[2] %in% c(1,2) & nrow(sf::st_intersection(sf::st_as_sf(my.plot.polys.df[2,]), circle.pt) ) != 0 ) == T){
    my.plot.polys.df$geometry[2] <-  sf::st_geometry(sf::st_difference(circle.17, sf::st_as_sf(my.plot.polys.df[2,])))
  }
  
  
  # determine intersection status between poly and trees
  # select the polygone with no trees and make it poly.1
  # if the first polygone intersects with trees, but the second polygone doesn´t have intersections with trees while the edge type is 1 or 2,
  # poly.1 is changes to poly.2 
  # if that is not the case everything remains as it is in the "normal" / "not outer  edges" way
  # if edge 2 doesn´t have trees and the edge type 1 or two  edge ID 2 becomed poly.1 and edge ID 1 becomes poly.2
  # poly.1 means that this is the polygone to be prioritized while 
  # --> this means the only way to prioritize the polygonn with the ID 2 is when it is an edge is an outer edge and it really doesnt have trees
  # in any other case edge ID 1is prioritized
  # nrow(st_intersection(sf::st_as_sf(my.plot.polys.df[1,]), tree.sf)) != 0) && 
  if(isTRUE(my.plot.polys.df$e_type[2] %in% c(1,2) & nrow(st_intersection(sf::st_as_sf(my.plot.polys.df[2,]), tree.sf)) == 0 &
            !(my.plot.polys.df$e_type[1] %in% c(1,2)))){
    my.poly.1 <- sf::st_as_sf(my.plot.polys.df[2,])
    my.poly.2 <- sf::st_as_sf(my.plot.polys.df[1,])
  }else {
    # if edge ID 2 does have type 1 or 2 but intersects with trees, the poly with the ID 1 remains poly.1 if 
    # if edge ID 1 doesn´t have trees but has the edge type 1 or 2 edge 1 becomed poly.1 and edge 2 becomes poly.2
    # if both edges have trees or both edges do not have trees no matter their edge type, the poly with edge ID 1 becomes poly.1 and the edge with ID 2 becomes poly.2 
    my.poly.1 <- sf::st_as_sf(my.plot.polys.df[1,])
    my.poly.2 <- sf::st_as_sf(my.plot.polys.df[2,])
    
  }
  
  
  # select edge ID of edge polygones
  my.e.id.1 <- my.poly.1$e_ID
  my.e.id.2 <- my.poly.2$e_ID
  # select edge form of the respective edge polygones
  my.e.form.1 <- my.poly.1$e_form
  my.e.form.2 <- my.poly.2$e_form
  
  # # print edges and circle
  # print(c(plot(circle.17$geometry),
  #         plot(my.poly.1$geometry, col = "red", add = T),
  #       plot(my.poly.2$geometry, col = "blue",  add = T), 
  #       plot(st_geometry(tree.sf), add = TRUE)), 
  #       plot(circle.pt$geometry, col = "green", add = T))
  
  
  #### intersections between polygones and circles   
  ### 17m circle 
  my.circle = circle.17
  ## create poolygon of intersection for first polygon with circle
  inter.poly.17.1  <- st_intersection(my.circle, my.poly.1)
  inter.status.poly.17.1 <- ifelse(nrow(inter.poly.17.1) == 0, "no intersections",
                                   ifelse(my.e.form.1 == 1 & inter.poly.17.1$geometry == my.circle$geometry,  "no intersections",
                                          ifelse(my.e.form.1 == 2 & inter.poly.17.1$geometry == my.circle$geometry, "fully covering circle", 
                                                 "partly intersecting")))
  # if the first ednge covers all of the circle remaining its going to be set to 0 so we know there are no direct intersections and the circle is passed on to the next edge to calcualte the intersection
  # https://www.statology.org/r-argument-is-of-length-zero/
  inter.poly.17.1.a <- if(isTRUE(inter.poly.17.1$geometry == my.circle$geometry)){inter.poly.17.1 <- data.frame()}else{inter.poly.17.1}
  ## create poolygon of remaining circle after first edge polygone is intersected
  # create poly with remaining area: https://gis.stackexchange.com/questions/353633/r-spatial-erase-one-polygon-from-another-correct-use-of-st-difference
  remaining.circle.17.1.a <- if(nrow(inter.poly.17.1)==0){my.circle}else{sf::st_difference(my.circle, inter.poly.17.1)}
  
  # here we have to correct the remaining circle: the circle can only be the part that contains the center of the plot
  inter.poly.17.1 <- if(nrow(st_intersection(inter.poly.17.1.a, circle.pt))==0){inter.poly.17.1.a}else{remaining.circle.17.1.a}
  remaining.circle.17.1 <- if(nrow(st_intersection(remaining.circle.17.1.a, circle.pt))!=0){remaining.circle.17.1.a}else{inter.poly.17.1.a}
  
  # print(plot(inter.poly.17.1$geometry, col = "grey", main = paste0(my.plot.id, "-", my.e.form.1,  "-", c.r3))) 
  # print(plot(remaining.circle.17.1$geometry, col = "white", main = paste0(my.plot.id, "-", my.e.form.1,  "-", c.r3), add = T)) 
  
  
  ## create polygone of intersecting area of second polygone with remaining circle
  inter.poly.17.2 <- st_intersection(my.poly.2, st_geometry(remaining.circle.17.1))
  inter.status.poly.17.2 <- ifelse(nrow(inter.poly.17.2) == 0, "no intersections",
                                   ifelse(my.e.form.2== 1 & inter.poly.17.2$geometry == remaining.circle.17.1$geometry,  "no intersections",
                                          ifelse(my.e.form.2 == 2 & inter.poly.17.2$geometry == remaining.circle.17.1$geometry, "fully covering circle", 
                                                 "partly intersecting")))
  # if the second edge covers all of the circle remaining its going to be set to 0 so we know there are no direct intersections
  # https://www.statology.org/r-argument-is-of-length-zero/
  inter.poly.17.2 <- if(isTRUE(inter.poly.17.2$geometry == remaining.circle.17.1$geometry)){inter.poly.17.2 <- data.frame()}else{inter.poly.17.2}
  
  ## create polygone of the  remaining cricle after both intersects are decucted
  # so the area of the frst remining circle minus the area of the second remaining circle 
  remaining.circle.17.1.and.2.poly <- if(nrow(inter.poly.17.2)==0){remaining.circle.17.1}else{sf::st_difference(remaining.circle.17.1, inter.poly.17.2)}
  # as there was a problem with mutlipolygones and geometry collections being created in this step, we have to extract the collection into one poly
  remaining.circle.17.1.and.2.poly <- if(isTRUE(sf::st_geometry_type(remaining.circle.17.1.and.2.poly) == "GEOMETRYCOLLECTION") == T){
    sf::st_collection_extract(remaining.circle.17.1.and.2.poly, c("POLYGON"), warn = FALSE)} else{remaining.circle.17.1.and.2.poly}
  # print(plot(remaining.circle.17.1.and.2.poly$geometry, main = paste0(my.plot.id, "-", my.e.form.2,  "-", c.r3))) 
  
  ### 12m circle 
  my.circle = circle.12
  ## create poolygon of intersection for first polygon with circle
  inter.poly.12.1  <- st_intersection(my.circle, my.poly.1)
  inter.status.poly.12.1 <- ifelse(nrow(inter.poly.12.1) == 0, "no intersections",
                                   ifelse(my.e.form.1 == 1 & inter.poly.12.1$geometry == my.circle$geometry,  "no intersections",
                                          ifelse(my.e.form.1 == 2 & inter.poly.12.1$geometry == my.circle$geometry, "fully covering circle", 
                                                 "partly intersecting")))
  # if the first ednge covers all of the circle remaining its going to be set to 0 so we know there are no direct intersections and the circle is passed on to the next edge to calcualte the intersection
  # https://www.statology.org/r-argument-is-of-length-zero/
  inter.poly.12.1.a <- if(isTRUE(inter.poly.12.1) && inter.poly.12.1$geometry == my.circle$geometry){inter.poly.12.1 <- data.frame()}else{inter.poly.12.1}
  
  ## create poolygon of remaining circle after first edge polygone is intersected
  # create poly with remaining area: https://gis.stackexchange.com/questions/353633/r-spatial-erase-one-polygon-from-another-correct-use-of-st-difference
  remaining.circle.12.1.a <- if(nrow(inter.poly.12.1)==0){my.circle}else{sf::st_difference(my.circle, inter.poly.12.1)}
  # here we have to correct the remaining circle: the circle can only be the part that contains the center of the plot
  inter.poly.12.1 <- if(nrow(st_intersection(inter.poly.12.1.a, circle.pt))==0){inter.poly.12.1.a}else{remaining.circle.12.1.a}
  remaining.circle.12.1 <- if(nrow(st_intersection(remaining.circle.12.1.a, circle.pt))!=0){remaining.circle.12.1.a}else{inter.poly.12.1.a}
  #print(plot(remaining.circle.12.1$geometry, main = paste0(my.plot.id, "-", my.e.form.2,  "-", c.r2)))
  
  
  ## create polygone of intersecting area of second polygone with remaining circle
  inter.poly.12.2 <- st_intersection(my.poly.2, st_geometry(remaining.circle.12.1))
  inter.status.poly.12.2 <- ifelse(nrow(inter.poly.12.2) == 0, "no intersections",
                                   ifelse(my.e.form.2== 1 & inter.poly.12.2$geometry == remaining.circle.12.1$geometry,  "no intersections",
                                          ifelse(my.e.form.2 == 2 & inter.poly.12.2$geometry == remaining.circle.12.1$geometry, "fully covering circle", 
                                                 "partly intersecting")))
  # if the second edge covers all of the circle remaining its going to be set to 0 so we know there are no direct intersections
  # https://www.statology.org/r-argument-is-of-length-zero/
  inter.poly.12.2 <- if(isTRUE(inter.poly.12.2) && inter.poly.12.2$geometry == remaining.circle.12.1$geometry){inter.poly.12.2 <- data.frame()}else{inter.poly.12.2}
  
  ## create polygone of the  remaining cricle after both intersects are decucted
  # so the area of the frst remining circle minus the area of the second remaining circle 
  remaining.circle.12.1.and.2.poly <- if(nrow(inter.poly.12.2)==0){remaining.circle.12.1}else{sf::st_difference(remaining.circle.12.1, inter.poly.12.2)}
  # as there was a problem with mutlipolygones and geometry collections being created in this step, we have to extract the collection into one poly
  remaining.circle.12.1.and.2.poly <- if(isTRUE(sf::st_geometry_type(remaining.circle.12.1.and.2.poly) == "GEOMETRYCOLLECTION") == T){
    sf::st_collection_extract(remaining.circle.12.1.and.2.poly, c("POLYGON"), warn = FALSE)} else{remaining.circle.12.1.and.2.poly}
  
  
  ### 5m circle 
  my.circle = circle.5
  ## create poolygon of intersection for first polygon with circle
  inter.poly.5.1  <- st_intersection(my.circle, my.poly.1)
  inter.status.poly.5.1 <- ifelse(nrow(inter.poly.5.1) == 0, "no intersections",
                                  ifelse(my.e.form.1 == 1 & inter.poly.5.1$geometry == my.circle$geometry,  "no intersections",
                                         ifelse(my.e.form.1 == 2 & inter.poly.5.1$geometry == my.circle$geometry, "fully covering circle", 
                                                "partly intersecting")))
  # if the first ednge covers all of the circle remaining its going to be set to 0 so we know there are no direct intersections and the circle is passed on to the next edge to calcualte the intersection
  # https://www.statology.org/r-argument-is-of-length-zero/
  inter.poly.5.1.a <- if(isTRUE(inter.poly.5.1) && inter.poly.5.1$geometry == my.circle$geometry){inter.poly.5.1 <- data.frame()}else{inter.poly.5.1}
  
  ## create poolygon of remaining circle after first edge polygone is intersected
  # create poly with remaining area: https://gis.stackexchange.com/questions/353633/r-spatial-erase-one-polygon-from-another-correct-use-of-st-difference
  remaining.circle.5.1.a <- if(nrow(inter.poly.5.1)==0){my.circle}else{sf::st_difference(my.circle, inter.poly.5.1)}
  # print(plot(remaining.circle.5.1$geometry, main = paste0(my.plot.id, "-",my.e.form.1,  "-", c.r1))) 
  # here we have to correct the remaining circle: the circle can only be the part that contains the center of the plot
  inter.poly.5.1 <- if(nrow(st_intersection(inter.poly.5.1.a, circle.pt))==0){inter.poly.5.1.a}else{remaining.circle.5.1.a}
  remaining.circle.5.1 <- if(nrow(st_intersection(remaining.circle.5.1.a, circle.pt))!=0){remaining.circle.5.1.a}else{inter.poly.5.1.a}
  # print(plot(remaining.circle.12.1$geometry, main = paste0(my.plot.id, "-", my.e.form.2,  "-", c.r2)))
  
  
  ## create polygone of intersecting area of second polygone with remaining circle
  inter.poly.5.2 <- st_intersection(my.poly.2, st_geometry(remaining.circle.5.1))
  inter.status.poly.5.2 <- ifelse(nrow(inter.poly.5.2) == 0, "no intersections",
                                  ifelse(my.e.form.2== 1 & inter.poly.5.2$geometry == remaining.circle.5.1$geometry,  "no intersections",
                                         ifelse(my.e.form.2 == 2 & inter.poly.5.2$geometry == remaining.circle.5.1$geometry, "fully covering circle", 
                                                "partly intersecting")))
  # if the second edge covers all of the circle remaining its going to be set to 0 so we know there are no direct intersections
  # https://www.statology.org/r-argument-is-of-length-zero/
  inter.poly.5.2 <- if(isTRUE(inter.poly.5.2) && inter.poly.5.2$geometry == remaining.circle.5.1$geometry){inter.poly.5.2 <- data.frame()}else{inter.poly.5.2}
  
  ## create polygone of the  remaining cricle after both intersects are decucted
  # so the area of the frst remining circle minus the area of the second remaining circle 
  remaining.circle.5.1.and.2.poly <- if(nrow(inter.poly.5.2)==0){remaining.circle.5.1}else{sf::st_difference(remaining.circle.5.1, inter.poly.5.2)}
  # as there was a problem with mutlipolygones and geometry collections being created in this step, we have to extract the collection into one poly
  remaining.circle.5.1.and.2.poly <- if(isTRUE(sf::st_geometry_type(remaining.circle.5.1.and.2.poly) == "GEOMETRYCOLLECTION") == T){
    sf::st_collection_extract(remaining.circle.5.1.and.2.poly, c("POLYGON"), warn = FALSE)} else{remaining.circle.5.1.and.2.poly}
  
  #### calculate the area
  ## 17m cricle
  # area of the intersection 1
  inter.17.1.area <- ifelse(nrow(inter.poly.17.1) == 0, 0, sf::st_area(inter.poly.17.1))
  # area of the intersection polygone 2
  inter.17.2.area <- ifelse(nrow(inter.poly.17.2) == 0, 0, sf::st_area(inter.poly.17.2))
  #  area of the remaining circle, after both intersections are deducted
  remaining.circle.area.17 <- sf::st_area(remaining.circle.17.1.and.2.poly)
  # save area in dataframe
  inter.area.df.17 <- as.data.frame(
    cbind("plot_ID" = c(my.plot.id, my.plot.id, my.plot.id), "e_ID" = c(my.e.id.1, my.e.id.2, 0),
          "inv_year" = c(my.inv.year, my.inv.year, my.inv.year),
          #"e_form" = c(my.poly.1$e_form, my.poly.2$e_form, 0),"shape" = c("edge", "edge", "circle"),
          "CCS_r_m" = c(c.r3, c.r3, c.r3), 
          "inter_stat" = c(inter.status.poly.17.1, inter.status.poly.17.2, 0),
          "area_m2" = c(inter.17.1.area, inter.17.2.area, remaining.circle.area.17)
    ))
  
  ## 12m cricle
  # area of the intersection 1
  inter.12.1.area <- ifelse(nrow(inter.poly.12.1) == 0, 0, sf::st_area(inter.poly.12.1))
  # area of the intersection polygone 2
  inter.12.2.area <- ifelse(nrow(inter.poly.12.2) == 0, 0, sf::st_area(inter.poly.12.2))
  #  area of the remaining circle, after both intersections are deducted
  remaining.circle.area.12 <- sf::st_area(remaining.circle.12.1.and.2.poly)
  # save area in dataframe
  inter.area.df.12 <- as.data.frame(
    cbind("plot_ID" = c(my.plot.id, my.plot.id, my.plot.id), "e_ID" = c(my.e.id.1, my.e.id.2, 0),
          #"e_form" = c(my.poly.1$e_form, my.poly.2$e_form, 0),"shape" = c("edge", "edge", "circle"),
          "inv_year" = c(my.inv.year, my.inv.year, my.inv.year),
          "CCS_r_m" = c(c.r2, c.r2, c.r2), 
          "inter_stat" = c(inter.status.poly.12.1, inter.status.poly.12.2, 0),
          "area_m2" = c(inter.12.1.area, inter.12.2.area, remaining.circle.area.12)
    ))
  
  ## 5m cricle
  # area of the intersection 1
  inter.5.1.area <- ifelse(nrow(inter.poly.5.1) == 0, 0, sf::st_area(inter.poly.5.1))
  # area of the intersection polygone 2
  inter.5.2.area <- ifelse(nrow(inter.poly.5.2) == 0, 0, sf::st_area(inter.poly.5.2))
  #  area of the remaining circle, after both intersections are deducted
  remaining.circle.area.5 <- sf::st_area(remaining.circle.5.1.and.2.poly)
  # save area in dataframe
  inter.area.df.5 <- as.data.frame(
    cbind("plot_ID" = c(my.plot.id, my.plot.id, my.plot.id), 
          "e_ID" = c(my.e.id.1, my.e.id.2, 0),
          #"e_form" = c(my.poly.1$e_form, my.poly.2$e_form, 0),"shape" = c("edge", "edge", "circle"),
          "inv_year" = c(my.inv.year, my.inv.year, my.inv.year),
          "CCS_r_m" = c(c.r1, c.r1, c.r1), 
          "inter_stat" = c(inter.status.poly.5.1, inter.status.poly.5.2, 0),
          "area_m2" = c(inter.5.1.area, inter.5.2.area, remaining.circle.area.5)
    ))
  
  
  # bind area datafames of all 3 circles together
  inter.area.df <- rbind(inter.area.df.17, inter.area.df.12,inter.area.df.5 ) %>% 
    mutate(across(c("plot_ID", "e_ID", "inv_year", "CCS_r_m", "area_m2"), as.numeric))
  
  # assing stand to the edges depedning on area and the stand with trees
  # actually what i should do here is to identify which poly out of poly 1, poly2 and rem.circle holds the center of the 
  # plot. this 
  # all.polys.of.circle <- rbind(terra::vect(inter.poly.17.1), terra::vect(inter.poly.17.2), terra::vect(remaining.circle.17.1.and.2.poly))
  # plot(all.polys.of.circle)
  # text(all.polys.of.circle, labels = all.polys.of.circle$e_ID)
  # plot(terra::vect(circle.pt), add = T)
  #  is.related(terra::vect(circle.pt), all.polys.of.circle, "intersects") 
  # # select the polygone out of the cirlce, inter 1 and onter 2 that intsersects the  center of the cirlce becomes the new remaining cirlce 
  #  remaining.circle.17.1.and.2.poly <- all.polys.of.circle[apply(relate(terra::vect(circle.pt), all.polys.of.circle, "intersects"), 1, which),] 
  
  
  # is there a polygone that does not incluce the middle of the plot, has the type 1 or 2 (if its an edge) and doesn´t have trees 
  # then the poylgone is labelled with "no forest" and the remaining polygones are allocated into stands by area
  # first "if": both edges are edge type 1 or 2 and do not have trees and do not have the middle of the plot --> forest edge
  # if poly has the center of the plot but 
  if(isTRUE(nrow(st_intersection(inter.poly.17.1, tree.sf))!=0 & my.poly.1$e_type %in% c(1,2))){inter.17.1.tree.stat <- "trees but outer edge"}else{inter.17.1.tree.stat <- NA}
  if(isTRUE(nrow(st_intersection(inter.poly.17.2, tree.sf))!=0 & my.poly.2$e_type %in% c(1,2))){inter.17.2.tree.stat <- "trees but outer edge"}else{inter.17.2.tree.stat <- NA}
  if(isTRUE(nrow(st_intersection(inter.poly.17.1, tree.sf))==0 & nrow(st_intersection(inter.poly.17.1, circle.pt))==0 & my.poly.1$e_type %in% c(1,2))){inter.17.1.forest.stat <- "no forest"}else{inter.17.1.forest.stat <- NA}
  if(isTRUE(nrow(st_intersection(inter.poly.17.2, tree.sf))==0 & nrow(st_intersection(inter.poly.17.2, circle.pt))==0 & my.poly.2$e_type %in% c(1,2))){inter.17.2.forest.stat <- "no forest"}else{inter.17.2.forest.stat <- NA}
  my.poly.1$stand <- case_when(inter.17.1.tree.stat ==  "trees but outer edge"  ~ "warning_1",
                               inter.17.1.forest.stat == "no forest"~ "no forest", 
                               TRUE ~ NA)
  
  my.poly.2$stand <- case_when(inter.17.2.tree.stat ==  "trees but outer edge" ~ "warning_2",
                               inter.17.2.forest.stat == "no forest"~ "no forest", 
                               TRUE ~ NA)
  
  # create dataframe with stand info per edge/ remaining circle
  stand.df <- as.data.frame(cbind(
    "plot_ID" = my.plot.id,
    "e_ID" = c(my.poly.1$e_ID, my.poly.2$e_ID, 0), 
    "stand"= c(ifelse(length(my.poly.1$stand) == 0, NA, my.poly.1$stand),   
               ifelse(length(my.poly.2$stand) == 0, NA, my.poly.2$stand),
               ifelse(length(remaining.circle.17.1.and.2.poly$stand) == 0, NA, remaining.circle.17.1.and.2.poly$stand) ))) %>% 
    mutate(plot_ID = as.numeric(plot_ID), 
           e_ID = as.numeric(e_ID))
  
  stand.df <- 
    rbind(
      inter.area.df%>% 
        filter(CCS_r_m  == 17.84) %>% 
        mutate(area_m2 = as.numeric(area_m2)) %>% 
        left_join(., stand.df, by = c("plot_ID", "e_ID")) %>% 
        group_by(plot_ID, inv_year) %>% 
        arrange(desc(area_m2)) %>% 
        filter(!(stand %in% c("no forest", "warning_1", "warning_2"))) %>% 
        # assing stand according to row number by selecting the letter of the alphabet with the respective number e.g. row == 1 --> A: 
        # https://www.geeksforgeeks.org/sequence-of-alphabetical-character-letters-from-a-z-in-r/
        # lowest area receives stand ID C, then B, then A
        mutate(stand = LETTERS[row_number()]), 
      # dataset with "no forest" or "warning" stands (if there are any, if not i´ll just not bind them)
      inter.area.df%>% 
        filter(CCS_r_m  == 17.84) %>% 
        mutate(area_m2 = as.numeric(area_m2)) %>% 
        left_join(., stand.df, by = c("plot_ID", "e_ID")) %>% 
        filter(stand %in% c("no forest", "warning_1", "warning_2")) %>% 
        mutate(stand = as.character(stand))
    ) %>% 
    # make stand.df joinable by only leaving plot_ID, e_ID, no matter the diameter of the CCS
    select(- c(CCS_r_m, inter_stat, area_m2))
  
  stand.df <- stand.df %>% 
    mutate(plot_ID = as.numeric(plot_ID), 
           e_ID = as.numeric(e_ID), 
           inv_year = as.numeric(inv_year))
  
  # join in stand info based on area of the edge segment
  inter.area.df <- inter.area.df %>% left_join(., stand.df, 
                                               by = c("plot_ID", "e_ID", "inv_year"))
  
  try(print(  c(plot(circle.17$geometry, main = paste0(my.plot.id, " - ", my.e.form.1, " - ", my.e.form.2)), 
                plot(remaining.circle.17.1.and.2.poly$geometry, col = "grey", add = T),
                plot(remaining.circle.12.1.and.2.poly$geometry, add = T),
                plot(remaining.circle.5.1.and.2.poly$geometry, add = T),
                plot(inter.poly.17.1$geometry, col = "green", add =TRUE),
                plot(inter.poly.17.2$geometry, col = "blue", add =TRUE),
                plot(circle.pt$geometry, col = "red",  add = TRUE),
                legend("topleft", legend=c(paste0(unique(inter.area.df$stand[inter.area.df$e_ID == my.poly.1$e_ID]),":",  my.poly.1$e_type), 
                                           paste0(unique(inter.area.df$stand[inter.area.df$e_ID == my.poly.2$e_ID]),":",  my.poly.2$e_type), 
                                           paste0(unique(inter.area.df$stand[inter.area.df$e_ID == 0]),":","rem_circle")), 
                       col=c("green", "blue", "grey"), lty=1:2, cex=0.8),
                plot(st_geometry(tree.sf), add = TRUE))), silent = T)
  
  # save datacframe per plot in list
  outer.edges.list.two.edges.geo[[i]] <- inter.area.df
  
  
  # create list with those plot ID where the two edges intersect within the radius of 17.84m
  intersection.between.edges.17 <- sf::st_intersection(
    sf::st_intersection(my.poly.1, circle.17), # intersection poly 1 and cirlce 17
    sf::st_intersection(my.poly.2, circle.17)) # intersection poly 2 and cirlce 17
  intersection.warning.edges <- ifelse(nrow(intersection.between.edges.17) == 0, NA, intersection.between.edges.17$plot_ID)
  outer.intersection.warning.edges.list.geo[[i]] <- as.data.frame(cbind("plot_ID" = c(intersection.warning.edges)))
  
  ## save intersection polygones in list
  # poly.1
  outer.inter.poly.1.both.types <- if(nrow(inter.poly.17.1)!= 0){inter.poly.17.1}else{
    # this is in case one of the inter polys is empty but we still want to transport the stand info with the polygone
    my.poly.1[1:6]}
  # in case one of the poly is multipolygones geometry type
  outer.inter.poly.1.list.geo[[i]] <- if(isTRUE(st_geometry_type(outer.inter.poly.1.both.types) == "POLYGON") == T){c(outer.inter.poly.1.both.types)}else{}
  outer.inter.multipoly.1.list.geo[[i]] <- if(isTRUE(st_geometry_type(outer.inter.poly.1.both.types) == "MULTIPOLYGON") == T){c(outer.inter.poly.1.both.types)}else{}
  
  # poly.2
  outer.inter.poly.2.both.types <- if(nrow(inter.poly.17.2)!= 0){inter.poly.17.2}else{
    # this is in case one of the inter polys is empty but we still want to transport the stand info with the polygone
    my.poly.2[1:6]}
  # in case one of the poly is multipolygones geometry type
  outer.inter.poly.2.list.geo[[i]] <- if(isTRUE(st_geometry_type(outer.inter.poly.2.both.types) == "POLYGON") == T){c(outer.inter.poly.2.both.types)}else{}
  outer.inter.multipoly.2.list.geo[[i]] <- if(isTRUE(st_geometry_type(outer.inter.poly.2.both.types) == "MULTIPOLYGON") == T){c(outer.inter.poly.2.both.types)}else{}
  
  
  ## save the reimaingf circle polygones in a list
  remaining.circle.17.1.and.2.poly$plot_ID <- my.plot.id
  remaining.circle.17.1.and.2.poly$e_ID <- 0
  remaining.circle.17.1.and.2.poly$inv_year <- my.inv.year
  remaining.circle.17.1.and.2.poly$e_form <- 0
  remaining.circle.17.1.and.2.poly$geometry <- remaining.circle.17.1.and.2.poly$geometry
  # create list wit polygones of the remaining cirlce when it´s only one polygone
  outer.rem.circle.poly.2.edges.list.geo[[i]] <- if(st_geometry_type(remaining.circle.17.1.and.2.poly)== "POLYGON"){c(remaining.circle.17.1.and.2.poly)}else{}
  # create list wit polygones of the remaining cirlce when it´s a multipoligone
  outer.rem.circle.multipoly.2.edges.list.geo[[i]] <- if(st_geometry_type(remaining.circle.17.1.and.2.poly)== "MULTIPOLYGON"){c(remaining.circle.17.1.and.2.poly)}else{}
  
}
# save areas into dataframe
outer.edges.area.two.edges.df.geo <- as.data.frame(rbindlist(outer.edges.list.two.edges.geo))
# save plot IDs with overlappig edges within the 17.84m circle into dataframe
outer.intersection.two.edges.warning.df.geo <- na.omit(as.data.frame(rbindlist(outer.intersection.warning.edges.list.geo, fill=TRUE)))
if(nrow(outer.intersection.two.edges.warning.df.geo)!=0){print("There are plots with overlapping edges within a 17.84m radius around the plot center. 
                                                           Please check dataset intersection.two.edges.warning.df.geo")}
# save intersection polygones into dataframe 
# list of polygones 1 of forest edges 
outer.inter.poly.1.two.edges.df.geo <- plyr::rbind.fill(as.data.frame(rbindlist(outer.inter.poly.1.list.geo, fill=TRUE)), as.data.frame(rbindlist(outer.inter.multipoly.1.list.geo, fill=TRUE)))
# list of polygones 2 of forest edges 
outer.inter.poly.2.two.edges.df.geo <- plyr::rbind.fill(as.data.frame(rbindlist(outer.inter.poly.2.list.geo, fill=TRUE)), as.data.frame(rbindlist(outer.inter.multipoly.2.list.geo, fill=TRUE)))
# bind the both edges per plot together
outer.inter.poly.two.edges.df.geo <- plyr::rbind.fill(outer.inter.poly.1.two.edges.df.geo, outer.inter.poly.2.two.edges.df.geo)

# list of polygones of remainign circles 
outer.rem.circle.poly.two.edges.list.final.geo <- rbindlist(outer.rem.circle.poly.2.edges.list.geo, fill = TRUE)
outer.rem.circle.poly.two.edges.df.geo <- as.data.frame(outer.rem.circle.poly.two.edges.list.final.geo)[,c("plot_ID", "inv_year","e_ID",  "e_form","geometry")]  %>% distinct()
# list of multipolygones of remaining circles
outer.rem.circle.multipoly.two.edges.df.geo <- if(nrow(as.data.frame(rbindlist(outer.rem.circle.multipoly.2.edges.list.geo)))!= 0){
  as.data.frame(rbindlist(outer.rem.circle.multipoly.2.edges.list.geo, fill = TRUE))[,c("plot_ID", "inv_year","e_ID",  "e_form","geometry")] %>% distinct()
}else{  data.frame()}
# binding the both circle lists back together 
outer.rem.circle.two.edges.df.geo <- if(isTRUE(nrow(outer.rem.circle.poly.two.edges.df.geo) != 0 && nrow(outer.rem.circle.multipoly.two.edges.df.geo) != 0) == T){
  plyr::rbind.fill(outer.rem.circle.poly.two.edges.df.geo, outer.rem.circle.multipoly.two.edges.df.geo)
}else{outer.rem.circle.poly.two.edges.df.geo}





# 3.2.1.4. binding all egde and remaining circle areas and polys tog --------

# bind all edges area dataframes together
all.edges.area.df.geo <- plyr::rbind.fill(edges.area.df.geo,
                                             edges.area.two.edges.df.geo, 
                                            outer.edges.area.df.geo, 
                                            outer.edges.area.two.edges.df.geo) %>% mutate(area_m2 = as.numeric(area_m2)) %>% filter(!is.na(plot_ID))
inter.poly.one.edge.df.geo <- plyr::rbind.fill(inter.poly.one.edge.df.geo, outer.inter.poly.one.edge.df.geo) # %>% select(colnames(inter.poly.one.edge.df.geo)))
rem.circle.one.edge.df.geo <- plyr::rbind.fill(rem.circle.one.edge.df.geo, outer.rem.circle.one.edge.df.geo) # %>% select(colnames(rem.circle.one.edge.df.geo)))


# 3.2.2. sorting TREES into edge and remaining circle polygones ---------
# 3.2.2.1. plots with one edge: sorting trees into edge and remaining circle polygones ---------
trees.one.edge.geo <- trees_data %>%
  # filter only for trees that are located in plots with a forest edge
  semi_join(forest_edges.man %>% filter(e_form == 1 | e_form == 2 & inter_status_AT_17 == "two I" | e_form == 2 & inter_status_BT_17 == "two I") %>% 
              select(plot_ID) %>% distinct(), by = "plot_ID") %>% 
  # filter for trees located in plots htat haev only one forest edge
  anti_join(forest_edges.man %>% filter(e_form == 1 | e_form == 2 & inter_status_AT_17 == "two I" | e_form == 2 & inter_status_BT_17 == "two I") %>% 
              group_by(plot_ID) %>% summarise(n = n()) %>% filter(n > 1) %>% select(plot_ID), by = "plot_ID") %>% 
# anti_join(., forest_edges.man %>% filter(e_type %in% c(1, 2)) %>% select(plot_ID), by = "plot_ID") %>% 
## ## georef remove plots that do now have a corresponding center coordiante in the HBI loc document
 semi_join(geo_loc %>% filter(!is.na( RW_MED) & !is.na(HW_MED)) %>%  select(plot_ID)  %>% distinct(), by = "plot_ID")


tree.status.list.geo <- vector("list", length = length(trees.one.edge.geo$tree_ID))
tree.points.list.geo <- vector("list", length = length(trees.one.edge.geo$tree_ID))
for (i in 1:length(trees.one.edge.geo$tree_ID)){ 
  #i = 356 
  # i = which(grepl(140042, (trees.one.edge.geo$plot_ID)))
  
  #if(nrow(trees.one.edge.geo) == 0){break}
  
  # select plot ID accordint to positioin in the list
  my.plot.id <-trees.one.edge.geo [i, "plot_ID"] 
  my.tree.id <- trees.one.edge.geo[i, "tree_ID"]
  my.inv.year <- trees.one.edge.geo[i, "inv_year"]
  
  ## select the UTM coordiantes of the center of the cirlce corresponding with the plot ID
   my.center.easting <- as.numeric(geo_loc[geo_loc$plot_ID == my.plot.id, "RW_MED"]) ##georef
   my.center.northing <- as.numeric(geo_loc[geo_loc$plot_ID == my.plot.id, "HW_MED"])##georef
  ## select crs
   my.utm.epsg <-  paste0("+proj=utm +zone=", pick_utm(my.center.easting)," ", "+datum=WGS84 +units=m +no_defs +type=crs")##georef
  
  # select the remaining cirlce we want to intersect the tree with
  my.rem.circle <- sf::st_as_sf(rem.circle.one.edge.df.geo %>% filter(plot_ID == my.plot.id) %>% distinct())
  my.inter <- sf::st_as_sf(inter.poly.one.edge.df.geo  %>% filter(plot_ID == my.plot.id) %>% distinct())
  
  # sort area dataframe by size of cirlce fragments if it is not a : 
  # bigger polygone/ polygone with greater area is assigned to category A, smaller area polygone is assigned to B
  
  area.plot.df <- all.edges.area.df.geo %>% filter(plot_ID == my.plot.id & CCS_r_m == 17.84)
  # assign stand category to the polygones depending on which one is bigger/ smaller
  my.rem.circle$stand <- area.plot.df$stand[area.plot.df$e_ID == 0]
  my.inter$stand <- area.plot.df$stand[area.plot.df$e_ID == 1 | area.plot.df$e_ID == 2]
  
  
  # extract polar coordiantes of forest edge
  # point A 
  dist.tree <- trees.one.edge.geo[i, "dist_cm"]/100 
  azi.tree <- trees.one.edge.geo[i, "azi_gon"] 
  x.tree <- dist.tree*sin(azi.tree*pi/200)   # longitude, easting, RW, X
  y.tree <- dist.tree*cos(azi.tree*pi/200)   # latitude, northing, HW, y 
  # transform polar into cartesian coordiantes
  tree.east <- x.tree   + my.center.easting  ## georef
  tree.north <-  y.tree  + my.center.northing ## georef
  
  # save cartesian coordiantes in dataframe
  tree.coord.df <- as.data.frame(cbind(
    "plot_ID" = c(as.integer(my.plot.id)), 
    "tree_ID" = c(as.integer(my.tree.id)),
    "inv_year" = c(my.inv.year),
    "lon" = c(tree.east),
    "lat" = c(tree.north)
  ))
  
  # create sf point object from dataframe
  #https://stackoverflow.com/questions/52551016/creating-sf-points-from-multiple-lat-longs
  tree.sf <-  sf::st_as_sf(tree.coord.df, coords = c("lon", "lat"), remove = FALSE)
  ## assing CRS to points
  sf::st_crs(tree.sf) <- my.utm.epsg
  
  # print(c(plot(my.inter$geometry, main = paste0(my.plot.id)),
  #         plot(my.rem.circle$geometry, add = T),
  #         plot(tree.sf$geometry, add = T))
  # )
  
  # check if tree intersects with polygone of rem cirlce or of edge 
  inter.tree.circle <- sf::st_intersection(tree.sf, my.rem.circle)
  inter.tree.edge <- sf::st_intersection(tree.sf, my.inter)
  
  tree_status <- ifelse(nrow(inter.tree.edge)!= 0, my.inter$stand, 
                        ifelse(nrow(inter.tree.circle) != 0,  my.rem.circle$stand,
                               "warning"))
  
  tree.status.list.geo[[i]] <- as.data.frame(cbind(
    "plot_ID" = c(as.integer(my.plot.id)), 
    "tree_ID" = c(as.integer(my.tree.id)),
    "inv_year" = c(my.inv.year),
    "lon" = c(as.numeric(tree.coord.df$lon)),
    "lat" = c(as.numeric(tree.coord.df$lat)),
    "t_stat" = c(tree_status))) 
  
  # export tree points as sf
  tree.points.list.geo[[i]] <- c("t_stat" = tree_status, tree.sf)
  
  print(paste(my.plot.id, my.tree.id))
}
# save tree corodiantes and status into dataframe
tree.status.one.edge.df.geo <- as.data.frame(rbindlist(tree.status.list.geo))
# save tree sf into dataframe
tree.points.one.edge.df.geo <- as.data.frame(rbindlist(tree.points.list.geo))




# 3.2.2.2. plots with 2 edges: sorting trees into edge and remaining circle polygones ---------
# bind all edges area dataframes together
inter.poly.two.edges.df.geo <- plyr::rbind.fill(inter.poly.two.edges.df.geo,
                                                  outer.inter.poly.two.edges.df.geo)# %>% select(colnames(inter.poly.two.edges.df.geo)))%>% arrange(plot_ID)
rem.circle.two.edges.df.geo <- plyr::rbind.fill(rem.circle.two.edges.df.geo, 
                                                  outer.rem.circle.two.edges.df.geo)# %>% select(colnames(rem.circle.two.edges.df.geo)))%>% arrange(plot_ID)

# intersection of trees with 2 edges
trees.two.edges.geo <- trees_data %>%
  # filter only for trees that are located in plots with a forest edge
  semi_join(forest_edges.man %>% filter(e_form == 1 | e_form == 2) %>% 
              #& inter_status_AT_17 == "two I" | e_form == 2 & inter_status_BT_17 == "two I") %>% 
              select(plot_ID) %>% distinct(), by = "plot_ID") %>% 
  # filter for trees located in plots htat haev only one forest edge
  semi_join(forest_edges.man %>% filter(e_form == 1 | e_form == 2 & inter_status_AT_17 == "two I" | e_form == 2 & inter_status_BT_17 == "two I") %>% 
              group_by(plot_ID) %>% summarise(n = n()) %>% filter(n > 1) %>% 
              select(plot_ID), by = "plot_ID") %>% ## georef 
  ## remove plots that do now have a corresponding center coordiante in the HBI loc document
  semi_join(geo_loc %>% filter(!is.na( RW_MED) & !is.na(HW_MED)) %>%  select(plot_ID)  %>% distinct(), by = "plot_ID")

tree.status.two.edges.list.geo <- vector("list", length = length(trees.two.edges.geo$tree_ID))
tree.points.two.edges.list.geo <- vector("list", length = length(trees.two.edges.geo$tree_ID))
for (i in 1:length(trees.two.edges.geo$tree_ID)){ 
  # i = 3225
  # i = which(grepl(50122, (trees.two.edges.geo$plot_ID)))[2]
  
  #if(nrow(trees.two.edges.geo) == 0){break}
  
  # select plot ID accordint to positioin in the list
  my.plot.id <- trees.two.edges.geo[i, "plot_ID"] 
  my.tree.id <- trees.two.edges.geo[i, "tree_ID"]
  my.inv.year <- trees.two.edges.geo[i, "inv_year"]
  
  ## georef
  ## select the UTM coordiantes of the center of the cirlce corresponding with the plot ID
  my.center.easting <- as.numeric(geo_loc[geo_loc$plot_ID == my.plot.id, "RW_MED"])##georef
  my.center.northing <- as.numeric(geo_loc[geo_loc$plot_ID == my.plot.id, "HW_MED"])##georef
  ## select crs
  my.utm.epsg <-  paste0("+proj=utm +zone=", pick_utm(my.center.easting)," ", "+datum=WGS84 +units=m +no_defs +type=crs")##georef
  
  # select the remaining cirlce we want to intersect the tree with
  my.rem.circle <- sf::st_as_sf(rem.circle.two.edges.df.geo %>% filter(plot_ID == my.plot.id) %>% distinct())
  my.edges.df <- inter.poly.two.edges.df.geo %>% filter(plot_ID == my.plot.id) %>% distinct() %>% arrange(e_ID)
  my.inter.1 <- sf::st_as_sf(my.edges.df[1,])
  my.inter.2 <- sf::st_as_sf(my.edges.df[2,])
  
  # assign stand category to the polygones depending on which one is bigger/ smaller: 
  # bigger polygone/ polygone with greater area is assigned to category A, smaller area polygone is assigned to B
  area.plot.df <- all.edges.area.df.geo %>% filter(plot_ID == my.plot.id & CCS_r_m == 17.84) 
  # assign stand category to the polygones depending on which one is bigger/ smaller
  my.rem.circle$stand <- area.plot.df$stand[area.plot.df$e_ID == 0]
  my.inter.1$stand <- area.plot.df$stand[area.plot.df$e_ID == 1]
  my.inter.2$stand <- area.plot.df$stand[area.plot.df$e_ID == 2]
  
  # extract polar coordiantes of forest edge
  # point A 
  dist.tree <- trees.two.edges.geo[i, "dist_cm"]/100 
  azi.tree <- trees.two.edges.geo[i, "azi_gon"] 
  x.tree <- dist.tree*sin(azi.tree*pi/200)   # longitude, easting, RW, X
  y.tree <- dist.tree*cos(azi.tree*pi/200)   # latitude, northing, HW, y 
  
  # transform polar into cartesian coordiantes  
  tree.east <- x.tree   + my.center.easting ## georef
  tree.north <- y.tree  + my.center.northing## georef
  
  # save cartesian coordiantes in dataframe
  tree.coord.df <- as.data.frame(cbind(
    "plot_ID" = c(as.integer(my.plot.id)), 
    "tree_ID" = c(as.integer(my.tree.id)),
    "inv_year" = c(my.inv.year),
    "lon" = c(tree.east),
    "lat" = c(tree.north)
  ))
  
  # create sf point object from dataframe
  #https://stackoverflow.com/questions/52551016/creating-sf-points-from-multiple-lat-longs
  tree.sf <-  sf::st_as_sf(tree.coord.df, coords = c("lon", "lat"), remove = FALSE)
  ## assing CRS to points
  sf::st_crs(tree.sf) <- my.utm.epsg ## georef
  
  # print(c(plot(my.rem.circle$geometry, col = "red"), 
  #       plot(my.inter.2$geometry, add = T),
  #       plot(my.inter.1$geometry, add = T), 
  #       plot(tree.sf$geometry, add = T))
  #       )
  
  inter.tree.circle <- sf::st_intersection(tree.sf, my.rem.circle)
  inter.tree.edge.1 <- sf::st_intersection(tree.sf, my.inter.1)
  inter.tree.edge.2 <- sf::st_intersection(tree.sf, my.inter.2)
  
  tree_status <- ifelse(nrow(inter.tree.edge.1)!= 0 & nrow(inter.tree.edge.2)== 0 & nrow(inter.tree.circle)== 0,  my.inter.1$stand,                     # if tree is in edge 1
                        ifelse(nrow(inter.tree.edge.2)!= 0 & nrow(inter.tree.edge.1)== 0 & nrow(inter.tree.circle)== 0,  my.inter.2$stand,              # if tree is in edge 2
                               ifelse(nrow(inter.tree.circle)!= 0 & nrow(inter.tree.edge.1)== 0 & nrow(inter.tree.edge.2)== 0,  my.rem.circle$stand,    # if tree is in circle
                                      #ifelse(nrow(inter.tree.circle)== 0 & nrow(inter.tree.edge.1)!= 0 & nrow(inter.tree.edge.2)!= 0,  "warning",       # if tree is in two edges
                                      "warning")))                                                                                             # if tree is nowhere
  
  tree.status.two.edges.list.geo[[i]] <- as.data.frame(cbind(
    "plot_ID" = c(as.integer(my.plot.id)), 
    "tree_ID" = c(as.integer(my.tree.id)),
    "inv_year" = c(my.inv.year),
    "lon" = c(as.numeric(tree.coord.df$lon)),
    "lat" = c(as.numeric(tree.coord.df$lat)),
    "t_stat" = c(tree_status))) 
  
  tree.points.two.edges.list.geo[[i]] <- c("t_stat" = tree_status, tree.sf)
  
  print(paste(my.plot.id, my.tree.id))
  
}
# save tree corodiantes and status into dataframe
tree.status.two.edges.df.geo <- as.data.frame(rbindlist(tree.status.two.edges.list.geo))
# save tree sf into dataframe
tree.points.two.edges.df.geo <- as.data.frame( rbindlist(tree.points.two.edges.list.geo))


# 3.2.2.3. binding datasets together --------------------------------------
# bind the tree point datafarmes of one and two edges plots together
two.and.one.edge.trees.points.df.geo <- plyr::rbind.fill(tree.points.one.edge.df.geo,tree.points.two.edges.df.geo) 
# this step i separated in case both of the rbinded dfs are empty and the mutate wouldn´t grip
two.and.one.edge.trees.points.df.geo <- two.and.one.edge.trees.points.df.geo %>% mutate(plot_ID = as.integer(plot_ID)) 





# 3.2.2.4 plots with no edge: sorting trees into circle ---------
trees.no.edge.geo <- anti_join(trees_data, forest_edges.man %>% 
                                   # filter only for trees that are located in plots with a forest edge
                                   semi_join(forest_edges.man %>% 
                                               filter(e_form == 1 | e_form == 2 & inter_status_AT_17 == "two I" | e_form == 2 & inter_status_BT_17 == "two I") %>% 
                                               select(plot_ID) %>% distinct(), by = "plot_ID") %>% 
                                   select(plot_ID) %>% 
                                   distinct(), by = "plot_ID") %>% ##georef
# remove plots that do now have a corresponding center coordiante in the HBI loc document
 semi_join(geo_loc %>% filter(!is.na( RW_MED) & !is.na(HW_MED)) %>%  
             select(plot_ID)  %>% distinct(), by = "plot_ID") ##georef


tree.status.no.edge.list.geo <- vector("list", length = length(trees.no.edge.geo$tree_ID))
tree.points.no.edge.list.geo <- vector("list", length = length(trees.no.edge.geo$tree_ID))
for (i in 1:length(trees.no.edge.geo$tree_ID)){ 
  #i = 2173
  #i = which(grepl(50124, unique(trees.no.edge.geo$plot_ID)))
  
  #if(nrow(trees.no.edge.geo) == 0){break}
  
  # select plot ID accordint to positioin in the list
  my.plot.id <- trees.no.edge.geo[i, "plot_ID"] 
  my.tree.id <- trees.no.edge.geo[i, "tree_ID"]
  my.inv.year <- trees.no.edge.geo[i, "inv_year"]
  
  ## georeference
  ## select UTM corrdinates of the plot center
  my.center.easting <- as.numeric(geo_loc[geo_loc$plot_ID == my.plot.id, "RW_MED"]) ##georef
  my.center.northing <- as.numeric(geo_loc[geo_loc$plot_ID == my.plot.id, "HW_MED"]) ##georef
  ## select crs
  my.utm.epsg <-  paste0("+proj=utm +zone=", pick_utm(my.center.easting)," ", "+datum=WGS84 +units=m +no_defs +type=crs")##georef
  
  # extract polar coordiantes of forest edge
  # point A 
  dist.tree <- trees.no.edge.geo[i, "dist_cm"]/100 
  azi.tree <- trees.no.edge.geo[i, "azi_gon"] 
  x.tree <- dist.tree*sin(azi.tree*pi/200)   # longitude, easting, RW, X
  y.tree <- dist.tree*cos(azi.tree*pi/200)   # latitude, northing, HW, y 
  
  
  # transform polar into cartesian coordiantes
  tree.east <- x.tree   + my.center.easting  ## georef
  tree.north <- y.tree  + my.center.northing ## georef
  
  # save cartesian coordiantes in dataframe
  tree.coord.df <- as.data.frame(cbind(
    "plot_ID" = c(as.integer(my.plot.id)), 
    "tree_ID" = c(as.integer(my.tree.id)),
    "inv_year" = c(my.inv.year),
    "lon" = c(as.numeric(tree.east)),
    "lat" = c(as.numeric(tree.north))
  ))
  
  # create sf point object from dataframe
  #https://stackoverflow.com/questions/52551016/creating-sf-points-from-multiple-lat-longs
  tree.sf <-  sf::st_as_sf(tree.coord.df, coords = c("lon", "lat"), remove = FALSE)
  ## assing CRS to points
  sf::st_crs(tree.sf) <- my.utm.epsg ## georef
  
  
  #### build circle
  # circle data
  c.x0 = 0  # + my.center.easting
  c.y0 = 0  # + my.center.northing
  c.r3 = 17.84
  c.r2 = 12.62
  c.r1 = 5.64
  # build polygon (circlular buffer) around center point
  center.df<- as.data.frame(cbind("lon" = c.x0, "lat" = c.y0))
  circle.pt <- sf::st_as_sf(center.df, coords = c("lon", "lat")) ##georef
  ## assing crs to cirlce corodiantes
   sf::st_crs(circle.pt) <- my.utm.epsg ##georef
  circle.17 <- sf::st_buffer(circle.pt, c.r3)
  circle.12 <- sf::st_buffer(circle.pt, c.r2)
  circle.5 <- sf::st_buffer(circle.pt, c.r1)
  
  inter.tree.circle.17 <- sf::st_intersection(tree.sf, circle.17)
  
  # if a tree is not intersecting with the circle or its exactly at the edge of the cirlce the inter.tree.circle.17 will be empty, 
  # however, trees that are exactly 17.84 meters apart from the circle center would still be part of the plot, tho the polygones won´t detect and intersection
  # which is why trees only receive the status "warning" if they are acturally situated outside of the circle
  tree_status <- ifelse(nrow(inter.tree.circle.17) == 0 & dist.tree > 17.84,  "warning", "A")                                                                                            # if tree is nowhere
  
  tree.status.no.edge.list.geo[[i]] <- as.data.frame(cbind(
    "plot_ID" = c(as.integer(my.plot.id)), 
    "tree_ID" = c(as.integer(my.tree.id)),
    "inv_year" = c(my.inv.year),
    "lon" = c(as.numeric(tree.coord.df$lon)),
    "lat" = c(as.numeric(tree.coord.df$lat)),
    "t_stat" = c(tree_status))
  )
  
  tree.points.no.edge.list.geo[[i]] <- c("t_stat" = tree_status, tree.sf)
  
  print(paste(my.plot.id, my.tree.id))
  
}
# save tree corodiantes and status into dataframe
tree.status.no.edges.df.geo <- as.data.frame(rbindlist(tree.status.no.edge.list.geo))
# save tree sf into dataframe
tree.points.no.edges.df.geo <- as.data.frame(rbindlist(tree.points.no.edge.list.geo))


# bind all tree point.sf dataframes (with & without edges together)
all.trees.points.df.geo <- 
  plyr::rbind.fill(tree.points.one.edge.df.geo , 
                   tree.points.two.edges.df.geo,
                   tree.points.no.edges.df.geo) %>% 
  mutate(across(plot_ID:tree_ID, ~ as.integer(.x))) %>% 
  distinct() %>% 
  left_join(., trees_data %>% 
              select(plot_ID, tree_ID, inv_year, DBH_cm), 
            by = c("plot_ID", "tree_ID", "inv_year"), 
            multiple = "all")


# bind all tree status dataframes together (one edge, two edges, no edge plots)
all.trees.status.df <- 
  plyr::rbind.fill(tree.status.no.edges.df.geo, 
                   tree.status.one.edge.df.geo, 
                   tree.status.two.edges.df.geo)


# 4.prep for data export ---------------------------------------------------------------------------------------------------------
# 4.1. harmonzing strings for join --------------------------------------------------------
# harmonize strings of all.trees.status.df and   
# https://stackoverflow.com/questions/20637360/convert-all-data-frame-character-columns-to-factors
all.trees.status.df[,c(1,2,3, 4, 5)] <- lapply(all.trees.status.df[,c(1,2, 3, 4, 5)], as.numeric)
all.edges.area.df.geo[,c(1,2, 3,4, 6)] <- lapply(all.edges.area.df.geo[,c(1,2, 3, 4, 6)], as.numeric) 

# 3.3.2. calculate plot area for plots with outer edge --------------------
##noforestground
# find plots with no forest polygones
outer_edge_area_per_CCS <- all.edges.area.df.nogeo %>% 
  # filter only for plots that have a "no forest" status or edge type 1 or 2
  semi_join(., forest_edges.man %>% filter(e_type %in% c(1, 2)) %>% select(plot_ID) %>% distinct(), 
            by = "plot_ID") %>% 
  # the only select those stands that are forestred
  filter(stand != "no forest") %>% 
  group_by(plot_ID, CCS_r_m) %>%  # and sum their area up per sampling circuit to get the adequate total area of the plot 
  summarise(area_ha_reduced = sum(area_m2)/10000) # divide by 10000 to male m2 to ha 


# 4.2. join tree stand status and plot areas into trees dataset  --------------------------------------------------------
trees_update_1 <- trees_data %>%  
  # join in stand of each tree
  left_join(., all.trees.status.df %>% 
              select(plot_ID, tree_ID, inv_year, t_stat) %>% 
              distinct(),
            by = c("plot_ID", "tree_ID", "inv_year")) %>% 
  rename(stand = t_stat) 

# this if statement is in case there are no forest edges for that dataset... which is unlikely
if(exists("all.edges.area.df.geo")){
  # join in the area that belongs to the tree according to the CCS the tree was measured in/ belongs to
  trees_update_1 <- trees_update_1 %>% 
    # remove and rejoin edge info 
    select(-c(e_ID, e_form, e_type)) %>% 
    distinct() %>% 
    left_join(., all.edges.area.df.geo %>% 
                select(plot_ID, inter_stat, CCS_r_m, stand, area_m2, e_ID),
              by = c("plot_ID", "CCS_r_m", "stand"))%>% 
    left_join(., forest_edges %>% select(plot_ID, e_ID, e_form, e_type), by = c("plot_ID", "e_ID")) %>% 
    # if there was no plot area claualted due to the fact that there is no edger at the plot, 
    # we calcualte the area from the sampling circuit diameter assign under CCD_r_m
    mutate(area_m2 = ifelse(is.na(e_ID) & is.na(area_m2) |
                              # for trees alloceted to a in a cirlce without intersections wil not run throuhg the loops
                              # thus they do  have an edge ID but no calcualted areas or assigned intersection status
                              # therefore we have to calculate their area manually subsequently
                              # trees with the status "warning" will not have any stand and area from the dataset "all.edges.area.df.geo" assigned
                              # as this stand category doesn´t exist
                              # trees with the status "warning" will be excluded from the analysis
                              stand == "A" & inter_stat != "partly intersecting" & is.na(area_m2) | 
                              stand == "A" & is.na(inter_stat) & is.na(area_m2), c_A(CCS_r_m), area_m2), 
           # this column is for stand-wise analysis and contains the plot area per tree according to the stand and the sampling circuit it is located in according to its diameter
           stand_plot_A_ha = as.numeric(area_m2)/10000,# dividedd by 10 000 to transform m2 into hectar
           # this column is for not stand wise analysis and contains the plot area per ptree according to the sampling circiont it is located in according to its diameter
           plot_A_ha = c_A(CCS_r_m)/10000) %>%   # dividedd by 10 000 to transform m2 into hectar ##georef
    ##noforestground
    # if we are dealing with a plot with an outer edge that cuts part of the plot, the total plot area has to be reduced 
    # by the area of the outer edge, saved in the outer_edge_area_per_CCS dataset
    # thus we will join in the dataset and then replace the plot_A_ha where its available 
    left_join(outer_edge_area_per_CCS, by = c("plot_ID", "CCS_r_m")) %>% 
    # so if the plot is in the list of plots with outer edge that actually have an outer edge ans an interception between the both 
    # we replace the plot are based on the CCS radius witht the plot area that we calculated earlier
    mutate(plot_A_ha_new = ifelse(plot_ID %in% 
                                    c(unique(all.edges.area.df.nogeo$plot_ID[all.edges.area.df.nogeo$inter_stat == "partly intersecting" &
                                                                               all.edges.area.df.nogeo$stand == "no forest"])) , 
                                  # we select the reduced area           
                                  area_ha_reduced, 
                                  plot_A_ha )) %>%
    left_join(geo_loc %>% select(plot_ID, RW_MED, HW_MED), by = "plot_ID") %>% ##georef
   mutate(east_tree =  X_tree + as.numeric(RW_MED), ##georef
          north_tree = Y_tree + as.numeric(HW_MED)) %>%  ##georef
    # add column with epsg (if georef then respective epsg, if not georef then "polar)
    mutate(EPSG = ifelse(east_tree < 6, 32631, 
                  ifelse(east_tree >= 6 & east_tree < 12, 32632, 
                         ifelse(east_tree >= 12, 32633, NA)))
    ) 
  
  
}else{
  trees_update_1 <- trees_update_1 %>%
    mutate(# this column is for not stand wise analysis and contains the plot area per ptree according to the sampling circiont it is located in according to its diameter
      plot_A_ha = c_A(CCS_r_m)/10000,
      # if there are no edges the area is equal to the plot area in m2
      area_m2 = c_A(CCS_r_m), 
      # if there are no edges this column contains the same area s the plot and the area column this column is for stand-wise analysis and contains the plot area per tree according to the stand and the sampling circuit it is located in according to its diameter
      stand_plot_A_ha = as.numeric(area_m2)/10000,# dividedd by 10 000 to transform m2 into hectar
      inter_stat = NA)  %>% ##georef
    mutate(east_tree =  X_tree + as.numeric(RW_MED), ##georef
           north_tree = Y_tree + as.numeric(HW_MED),  ##georef
           # add column with epsg (if georef then respective epsg, if not georef then "polar)
           EPSG = ifelse(east_tree < 6, 32631, 
                         ifelse(east_tree >= 6 & east_tree < 12, 32632, 
                                ifelse(east_tree >= 12, 32633, NA)))
           ) 
  }



# 4.3. sort trees into remove and process on datasets by status "warning" --------------------------------------------------------
trees_removed <- plyr::rbind.fill(trees_removed, 
                                  trees_update_1 %>% 
                                    anti_join(., trees_removed, by = c("plot_ID", "tree_ID")) %>% 
                                    filter(stringr::str_detect(stand, "warning")) %>% 
                                    mutate(rem_reason = "LT excluded during forest edges allocation"))

trees_update_1 <- trees_update_1 %>% filter(!(stringr::str_detect(stand, "warning"))) 


 
 # 4.4.  binding datasets together ----------------------------------------------------------
all.triangle.polys.df.geo <- plyr::rbind.fill(triangle.e1.poly.df.geo, triangle.e2.poly.df.geo)
all.edge.intersections.poly  <- plyr::rbind.fill(inter.poly.one.edge.df.geo , inter.poly.two.edges.df.geo)#%>% nest("geometry" = geometry)
# add missing stands to all intersection polygones dataframe 
all.edge.intersections.poly <- all.edge.intersections.poly %>% 
  left_join(., all.edges.area.df.geo %>%
              filter(CCS_r_m == 17.84) %>%
              distinct() %>% 
              mutate(stand_of_area_df = stand, 
                     plot_ID= as.integer(plot_ID), 
                     e_ID = as.integer(e_ID) ) %>% 
  select(plot_ID, e_ID,  stand_of_area_df), 
  by = c("plot_ID", "e_ID")) %>% 
  mutate(stand = ifelse(is.na(stand), stand_of_area_df, stand)) %>% 
  select(-c(stand_of_area_df))

all.remaning.circles.poly <- plyr::rbind.fill(rem.circle.one.edge.df.geo, rem.circle.two.edges.df.geo) #%>% nest("geometry" = geometry)
# add missing stands to rem cirlces dataframe 
all.remaning.circles.poly <- all.remaning.circles.poly %>% 
left_join(., all.edges.area.df.geo %>%
            filter(CCS_r_m == 17.84) %>%
            distinct() %>% 
            mutate(stand_of_area_df = stand, 
                   plot_ID= as.integer(plot_ID), 
                   e_ID = as.integer(e_ID) ) %>% 
            select(plot_ID, e_ID,  stand_of_area_df), 
          by = c("plot_ID", "e_ID")) %>% 
  mutate(stand = ifelse(is.na(stand), stand_of_area_df, stand)) %>% 
  select(-c(stand_of_area_df))

all.triangle.coords.df.geo <- plyr::rbind.fill(triangle.e1.coords.df.geo, triangle.e2.coords.df.geo) %>% 
  # the exportet polygones only include the widest cirlce intersection at 17.84m radius
  mutate(CCS_r_m = 17.84) %>% 
  # join in the stand info by plot_ID, e_ID, CCS_r_M
  left_join(., all.edges.area.df.geo %>% select(plot_ID, e_ID, CCS_r_m, stand), 
            by = c("plot_ID", "e_ID", "CCS_r_m"))




# 5. EXPORT ---------------------------------------------------
# exporting tree and edge/ plot area data
write.csv(trees_update_1, paste0(getwd(), out.path.BZE3, paste(unique(trees_update_1$inv)[1], "LT_update_1", sep = "_"), ".csv"), row.names = FALSE)
if(nrow(trees_removed)!=0){write.csv(trees_removed, paste0(getwd(), out.path.BZE3, paste(unique(trees_update_1$inv)[1], "LT_removed", sep = "_"), ".csv"), row.names = FALSE)}

# export tree stand status of all trees nomatter if they have one, two or no forest edges at their plot
write.csv(all.trees.status.df, paste0(getwd(), out.path.BZE3, paste(unique(trees_update_1$inv)[1], "all_LT_stand_georef", sep = "_"), ".csv"), row.names = FALSE)
# export areas and stand info of all sampling circuits, edges and remaining circles
write.csv(all.edges.area.df.geo,  paste0(getwd(), out.path.BZE3, paste(unique(trees_update_1$inv)[1], "all_edges_rem_circles_georef", sep = "_"), ".csv"), row.names = FALSE)

# export list of plots where the both edge polygones intersect within the 17.84 radius
write.csv(intersection.two.edges.warning.df.geo,  paste0(getwd(),out.path.BZE3, paste(unique(trees_update_1$inv)[1], "edges_intersecting_warning_georef", sep = "_"), ".csv"), row.names = FALSE)

# exporting edge triangle polygones
write.csv(all.triangle.polys.df.geo, paste0(getwd(),out.path.BZE3, paste(unique(trees_update_1$inv)[1], "all_edges_triangle_poly_georef", sep = "_"), ".csv"), row.names = FALSE)
# exporting edge triangle coordiantes
write.csv(all.triangle.coords.df.geo, paste0(getwd(),out.path.BZE3, paste(unique(trees_update_1$inv)[1], "all_edges_triangle_coords_georef", sep = "_"), ".csv"), row.names = FALSE)


# exporting edge intersection polygones 
#write.csv2(all.edge.intersections.poly, paste0(out.path.BZE3, paste(unique(trees_update_1$inv)[1], "all_edges_intersection_poly", sep = "_"), ".csv"))
# to export the dataframes with long geometries and keep the geometries in list format for better processing later 
# thus we export them with the following function, that enables to save the whole geometry list in 1 Table
# https://stackoverflow.com/questions/48024266/save-a-data-frame-with-list-columns-as-csv-file
tibble_with_lists_to_csv(all.edge.intersections.poly %>% nest("geometry" = geometry), paste0(getwd(),out.path.BZE3, paste(unique(trees_update_1$inv)[1], "all_edges_intersection_poly_georef", sep = "_"), ".csv"))
# exporting all remaining circles polygones
tibble_with_lists_to_csv(all.remaning.circles.poly %>% nest("geometry" = geometry), paste0(getwd(),out.path.BZE3, paste(unique(trees_update_1$inv)[1], "all_edges_rem_circles_poly_georef", sep = "_"), ".csv"))


## export coordiantes of all edge-triangle-circle-intersections polygones  to  dataframes 
all.edge.intersections.coords.list <- vector("list", length = nrow(unique(all.edge.intersections.poly[, c("plot_ID", "e_ID")])))
for (i in 1:nrow(unique(all.edge.intersections.poly[, c("plot_ID", "e_ID")]))) {
  # i = 1
  all.edge.intersections.coords.list[[i]] <- as.data.frame(cbind(
    "plot_ID" = c(all.edge.intersections.poly$plot_ID[i]), 
    "e_ID" = c(all.edge.intersections.poly$e_ID[i]),  
    "e_form" = c(all.edge.intersections.poly$e_form[i]),
    "lon" = (as_tibble(st_coordinates(all.edge.intersections.poly$geometry[i])) %>% select("X", -c( "L1", "L2"))),
    "lat" = (as_tibble(st_coordinates(all.edge.intersections.poly$geometry[i])) %>% select("Y", -c( "L1", "L2")))
  ))
}
all.edge.intersections.coords.list.final <- rbindlist(all.edge.intersections.coords.list)
all.edge.intersections.coords.df <- as.data.frame(all.edge.intersections.coords.list.final) %>% 
  # the exportet polygones only include the widest cirlce intersection at 17.84m radius
  mutate(CCS_r_m = 17.84) %>% 
  # join in the stand info by plot_ID, e_ID, CCS_r_M
  left_join(., all.edges.area.df.geo %>% select(plot_ID, e_ID, CCS_r_m, stand), 
            by = c("plot_ID", "e_ID", "CCS_r_m"))
write.csv(all.edge.intersections.coords.df,  paste0(getwd(), out.path.BZE3, paste(unique(trees_update_1$inv)[1], "all_edges_intersection_coords_georef", sep = "_"), ".csv"), row.names = FALSE)



## export coordiantes of all remaining polygones  to  dataframes
all.rem.circle.coords.list <- vector("list", length = nrow(unique(all.remaning.circles.poly[, c("plot_ID", "e_ID")])))
for (i in 1:nrow(unique(all.remaning.circles.poly[, c("plot_ID", "e_ID")]))) {
  # i = 1
  all.rem.circle.coords.list[[i]] <- as.data.frame(cbind(
    "plot_ID" = c(all.remaning.circles.poly$plot_ID[i]), 
    "e_ID" = c(all.remaning.circles.poly$e_ID[i]),  
    "e_form" = c(all.remaning.circles.poly$e_form[i]),
    "lon" = (as_tibble(st_coordinates(all.remaning.circles.poly$geometry[i])) %>% select("X", -c( "L1", "L2"))),
    "lat" = (as_tibble(st_coordinates(all.remaning.circles.poly$geometry[i])) %>% select("Y", -c( "L1", "L2")))
  ))
}
all.rem.circle.coords.list.final <- rbindlist(all.rem.circle.coords.list)
all.rem.circle.coords.df <- as.data.frame(all.rem.circle.coords.list.final) %>% 
  # the exportet polygones only include the widest cirlce intersection at 17.84m radius
  mutate(CCS_r_m = 17.84) %>% 
  # join in the stand info by plot_ID, e_ID, CCS_r_M
  left_join(., all.edges.area.df.geo %>% select(plot_ID, e_ID, CCS_r_m, stand), 
            by = c("plot_ID", "e_ID", "CCS_r_m"))
write.csv(all.rem.circle.coords.df,  paste0(getwd(), out.path.BZE3, paste(unique(trees_update_1$inv)[1], "all_rem_circles_coords_georef", sep = "_"), ".csv"), row.names = FALSE)








stop("this is where forest edges georef HBI ends")

# ----- 2. VISUALISATION  -------------------------------------------------

# 2.1.. visulaizing for all plots, edges, trees -------------------------

for(i in 1:(nrow(trees_update_1 %>% select(plot_ID) %>% distinct()))){
  # https://ggplot2.tidyverse.org/reference/ggsf.html
  
  #i = 2
  # i = which(grepl( 80003 , unique(trees_update_1$plot_ID)))
  my.plot.id = unique(trees_update_1$plot_ID)[i]
  #print(my.plot.id)
  
  my.center.easting <-  as.numeric(geo_loc[geo_loc$plot_ID == my.plot.id, "RW_MED"])
  my.center.northing <- as.numeric(geo_loc[geo_loc$plot_ID == my.plot.id, "HW_MED"])
  
  c.df <- as.data.frame(cbind("lon" = my.center.easting, "lat" = my.center.northing))
  c.pt <- sf::st_as_sf(c.df, coords = c("lon", "lat"))
  sf::st_crs(c.pt) <- my.utm.epsg
  c.poly.17 <- sf::st_buffer(c.pt, 17.84)
  c.poly.12 <- sf::st_buffer(c.pt, 12.62)
  c.poly.5 <- sf::st_buffer(c.pt, 5.64)
  
  all.trees.points.df.geo.sp <- all.trees.points.df.geo %>% 
    filter(plot_ID == my.plot.id) %>% 
    left_join(trees_data %>% 
                filter(plot_ID == my.plot.id) %>%
                select(plot_ID, tree_ID, SP_code), by = c("plot_ID", "tree_ID"))
  
  print(ggplot() +
          ggtitle(my.plot.id)+
          geom_sf(data = c.poly.17, aes(alpha = 0))+
          geom_sf(data = c.poly.12, aes(alpha = 0))+
          geom_sf(data = c.poly.5, aes(alpha = 0))+
          geom_sf(data = triangle.e1.poly.df.geo$geometry[triangle.e1.poly.df.geo$plot_ID == my.plot.id], aes(alpha = 0))+
          geom_sf(data = triangle.e2.poly.df.geo$geometry[triangle.e2.poly.df.geo$plot_ID == my.plot.id], aes(alpha = 0))+ 
          geom_sf(data = all.trees.points.df.geo.sp$geometry[all.trees.points.df.geo.sp$plot_ID == my.plot.id], 
                  aes(color = all.trees.points.df.geo.sp$t_stat[all.trees.points.df.geo.sp$plot_ID == my.plot.id], 
                      size =  all.trees.points.df.geo.sp$DBH_cm[all.trees.points.df.geo.sp$plot_ID == my.plot.id]))+
          guides(color=guide_legend(title="tree status"))+
          guides(size=guide_legend(title="DBH cm"))+
          geom_sf_text(data = all.trees.points.df.geo.sp$geometry[all.trees.points.df.geo.sp$plot_ID == my.plot.id], 
                       aes(label = all.trees.points.df.geo.sp$tree_ID[all.trees.points.df.geo.sp$plot_ID == my.plot.id]))
        
  )
  
}


#----2.2. visual prep: tree-edge-status by combining tree and edge data ---------------------------------------
# next step will be to join the forest edges dataset into the trees datset, 
# via b0 and b1 and then compare the calculated tree_y with the functions result
# if the tree_y is higher then the function_y we have to do something with the tree...
# etc. assiningg another plot iD or something. 

trees_and_edges <-
  trees_data  %>% 
  # join in edges info per plot
  left_join(., forest_edges.man %>% 
              select(plot_ID, e_ID, e_type, e_form,
                     A_dist, A_azi, B_dist, B_azi, T_dist, T_azi, 
                     X_A, X_B, X_T, Y_A, Y_B, Y_T,
                     b1_AB, b1_AT, b1_BT, b0_AB, b0_AT, b0_BT, 
                     X1_inter_AB_17, X2_inter_AB_17, Y1_inter_AB_17, Y2_inter_AB_17, inter_status_AB_17, 
                     X1_inter_AT_17, X2_inter_AT_17, Y1_inter_AT_17, Y2_inter_AT_17, inter_status_AT_17, 
                     X1_inter_BT_17, X2_inter_BT_17,  Y1_inter_BT_17, Y2_inter_BT_17, inter_status_BT_17,
                     X_inter_AT_triangle_60, X_inter_BT_triangle_60, Y_inter_AT_triangle_60, Y_inter_BT_triangle_60),
            by = c("plot_ID", "e_ID", "e_type", "e_form")) %>% 
  # the following two functions work, however the processign through loops and polygoens which follows under section 3
  # works better and more efficient & precise
  mutate(t_status_AB_ABT = tree.status(e_form,
                                       0, 0, data_circle$r0[3],
                                       b0_AB, b1_AB,
                                       X_tree, Y_tree,
                                       X_A, Y_A, X_T, Y_T, b0_AT, b1_AT,
                                       data_circle$rmax[3]*2,
                                       X_B, Y_B,  b0_BT, b1_BT)) %>%   
  # ---- 1.1.2.4. assigning plot area by according to diameter class (klubschwelle)  ---------------------------------------
# this is necesarry to make the function work. why exactly remains unclear 
mutate(id_func = row_number()) %>%
  group_by(id_func) %>% 
  mutate(edge_A_method = edge.A(e_form, DBH_cm,  X_A, X_B, X_T, Y_A, Y_B, Y_T, T_dist, t_status_AB_ABT, output = "method"), 
         plot_A =  edge.A(e_form, DBH_cm,  X_A, X_B, X_T, Y_A, Y_B, Y_T, T_dist, t_status_AB_ABT, output = "area_m2")) %>% 
  ungroup()


# ecport treees and edges dataset  ----------------------------------------
write.csv2(trees_and_edges, paste0(out.path.BZE3, paste(unique(trees_update_1$inv)[1], "LT_edges", sep = "_"), ".csv"))


# ----- 2.1.   Living trees visualization forest edges -----------------------------------
# ----- 2.1.1. AB lines, edge form 1, visualization forest edges -----------------------------------
# plotting trees and interception lines divided in t_line_status
#AB line
ggplot() +  
  geom_circle(data = data_circle, aes(x0 = x0, y0 = y0, r = r0))+ # Draw ggplot2 plot with circle representing sampling circuits 
  geom_circle(data = data_circle, aes(x0 = x0, y0 = y0, r = rmax*2))+ # Draw ggplot2 plot with circle representing sampling circuits
  # AB line
  geom_point(data = trees_and_edges %>%
               filter(e_form == "1") %>% 
               inner_join(.,   forest_edges.man %>% 
                            filter(e_form == "1") %>% 
                            group_by(plot_ID) %>% 
                            summarize(n = n()) %>% 
                            filter(n <= 1), 
                          by = "plot_ID") %>% 
               select(plot_ID, X1_inter_AB_17, X2_inter_AB_17, X_A, X_B, Y1_inter_AB_17, Y2_inter_AB_17, Y_A, Y_B) %>% 
               to_long(keys = c("X_name",  "Y_name"),
                       values = c( "X_value", "Y_value"),  
                       names(.)[2:5], names(.)[6:9]), 
             aes(x= X_value, y = Y_value, colour = X_name))+
  geom_line(data = trees_and_edges %>% 
              filter(e_form == "1") %>% 
              inner_join(.,   forest_edges.man %>% 
                           filter(e_form == "1") %>% 
                           group_by(plot_ID) %>% 
                           summarize(n = n()) %>% 
                           filter(n <= 1), 
                         by = "plot_ID") %>% 
              select(plot_ID, X_A, X_B, Y_A, Y_B) %>% 
              to_long(keys = c("X_name",  "Y_name"),
                      values = c( "X_value", "Y_value"),  
                      names(.)[2:3], names(.)[4:5]), 
            aes(x= X_value, y = Y_value))+
  geom_line(data = trees_and_edges %>% 
              filter(e_form == "1") %>% 
              inner_join(.,   forest_edges.man %>% 
                           filter(e_form == "1") %>% 
                           group_by(plot_ID) %>% 
                           summarize(n = n()) %>% 
                           filter(n <= 1), 
                         by = "plot_ID") %>% 
              select(plot_ID, X1_inter_AB_17, X_A, Y1_inter_AB_17, Y_A) %>% 
              to_long(keys = c("X_name",  "Y_name"),
                      values = c( "X_value", "Y_value"),
                      names(.)[2:3], names(.)[4:5]),  
            aes(x= X_value, y = Y_value, colour = X_name))+
  geom_line(data = trees_and_edges %>% 
              filter(e_form == "1") %>% 
              inner_join(.,   forest_edges.man %>% 
                           filter(e_form == "1") %>% 
                           group_by(plot_ID) %>% 
                           summarize(n = n()) %>% 
                           filter(n <= 1), 
                         by = "plot_ID") %>% 
              select(plot_ID, X2_inter_AB_17, X_A, Y2_inter_AB_17, Y_A) %>% 
              to_long(keys = c("X_name",  "Y_name"),
                      values = c( "X_value", "Y_value"),
                      names(.)[2:3], names(.)[4:5]),  
            aes(x= X_value, y = Y_value, colour = X_name))+
  geom_line(data = trees_and_edges %>% 
              filter(e_form == "1") %>% 
              inner_join(.,   forest_edges.man %>% 
                           filter(e_form == "1") %>% 
                           group_by(plot_ID) %>% 
                           summarize(n = n()) %>% 
                           filter(n <= 1), 
                         by = "plot_ID") %>% 
              select(plot_ID, X1_inter_AB_17, X_B, Y1_inter_AB_17, Y_B) %>% 
              to_long(keys = c("X_name",  "Y_name"),
                      values = c( "X_value", "Y_value"),
                      names(.)[2:3], names(.)[4:5]),  
            aes(x= X_value, y = Y_value, colour = X_name))+
  geom_line(data = trees_and_edges %>% 
              filter(e_form == "1") %>% 
              inner_join(.,   forest_edges.man %>% 
                           filter(e_form == "1") %>% 
                           group_by(plot_ID) %>% 
                           summarize(n = n()) %>% 
                           filter(n <= 1), 
                         by = "plot_ID") %>% 
              select(plot_ID, X2_inter_AB_17, X_B, Y2_inter_AB_17, Y_B) %>% 
              to_long(keys = c("X_name",  "Y_name"),
                      values = c( "X_value", "Y_value"),
                      names(.)[2:3], names(.)[4:5]),  
            aes(x= X_value, y = Y_value, colour = X_name))+
  # trees
  geom_point(data =  trees_and_edges %>% filter(e_form == "1"), 
             # %>% 
             #   inner_join(.,   forest_edges.man %>% 
             #                filter(e_form == "1" ) %>% 
             #                group_by(plot_ID) %>% 
             #                summarize(n = n()) %>% 
             #                filter(n <= 1), 
             #              by = "plot_ID"),
             aes(X_tree, Y_tree, colour = edge_A_method))+
  theme_bw()+
  facet_wrap(~plot_ID)


# ----- 2.1.2. ABT lines, edge form 2, Visualisation forest edges -----------------------------------
# forest edge type 2 
# if the # i removed, this part allows to plot plots with forest edges with a turning point
# AT line
ggplot() +  
  geom_circle(data = data_circle, aes(x0 = x0, y0 = y0, r = r0))+ # Draw ggplot2 plot with circle representing sampling circuits 
  geom_circle(data = data_circle, aes(x0 = x0, y0 = y0, r = rmax*2))+ # Draw ggplot2 plot with circle representing sampling circuits
  geom_point(data = trees_and_edges %>% 
               filter(e_form == "2") %>% 
               select(plot_ID, X_A, X_T, Y_A, Y_T) %>% 
               to_long(keys = c("X_name",  "Y_name"),
                       values = c( "X_value", "Y_value"),  
                       names(.)[2:3], names(.)[4:5]), 
             aes(x= X_value, y = Y_value, colour = "T"))+
  geom_line(data = trees_and_edges %>% 
              filter(e_form == "2") %>% 
              select(plot_ID, X_A, X_T, Y_A, Y_T) %>% 
              to_long(keys = c("X_name",  "Y_name"),
                      values = c( "X_value", "Y_value"),  
                      names(.)[2:3], names(.)[4:5]), 
            aes(x= X_value, y = Y_value))+
  # intersections choosen to draw the triangle AT
  geom_point(data = trees_and_edges %>% 
               filter(e_form == "2") %>% 
               select(plot_ID, X_inter_AT_triangle_60, X_A, Y_inter_AT_triangle_60, Y_A) %>% 
               to_long(keys = c("X_name",  "Y_name"),
                       values = c( "X_value", "Y_value"),  
                       names(.)[2:3], names(.)[4:5]), 
             aes(x= X_value, y = Y_value, colour = "A_Intercept"))+
  geom_line(data = trees_and_edges %>% 
              filter(e_form == "2") %>% 
              select(plot_ID, X_inter_AT_triangle_60, X_A, Y_inter_AT_triangle_60, Y_A) %>% 
              to_long(keys = c("X_name",  "Y_name"),
                      values = c( "X_value", "Y_value"),  
                      names(.)[2:3], names(.)[4:5]), 
            aes(x= X_value, y = Y_value))+
  # BT line 
  geom_point(data = trees_and_edges %>%
               filter(e_form == "2") %>% 
               select(plot_ID, X_B, X_T, Y_B, Y_T) %>% 
               to_long(keys = c("X_name",  "Y_name"),
                       values = c( "X_value", "Y_value"),  
                       names(.)[2:3], names(.)[4:5]), 
             aes(x= X_value, y = Y_value, colour = "T"))+
  geom_line(data = trees_and_edges %>%
              filter(e_form == "2") %>% 
              select(plot_ID, X_B, X_T, Y_B, Y_T) %>% 
              to_long(keys = c("X_name",  "Y_name"),
                      values = c( "X_value", "Y_value"),  
                      names(.)[2:3], names(.)[4:5]), 
            aes(x= X_value, y = Y_value))+
  # intersections choosen to draw the triangle BT
  geom_point(data = trees_and_edges %>% 
               filter(e_form == "2") %>% 
               select(plot_ID, X_inter_BT_triangle_60, X_B, Y_inter_BT_triangle_60, Y_B) %>% 
               to_long(keys = c("X_name",  "Y_name"),
                       values = c( "X_value", "Y_value"),  
                       names(.)[2:3], names(.)[4:5]), 
             aes(x= X_value, y = Y_value, colour = "B_intercept"))+
  geom_line(data = trees_and_edges %>% 
              filter(e_form == "2") %>% 
              select(plot_ID, X_inter_BT_triangle_60, X_B, Y_inter_BT_triangle_60, Y_B) %>% 
              to_long(keys = c("X_name",  "Y_name"),
                      values = c( "X_value", "Y_value"),  
                      names(.)[2:3], names(.)[4:5]), 
            aes(x= X_value, y = Y_value))+
  # trees
  geom_point(data =  trees_and_edges %>% filter(e_form == "2"), 
             aes(X_tree, Y_tree, colour =  edge_A_method))+
  theme_bw()+ 
  facet_wrap(~plot_ID)  




# ----- 2.1.3. visulaliszing forest edge_form 1 and edge_form 2 together using m_s_status --------
# plotting trees and interception lines divided in t_line_status
#AB line
ggplot() +  
  geom_circle(data = data_circle, aes(x0 = x0, y0 = y0, r = r0))+ # Draw ggplot2 plot with circle representing sampling circuits 
  geom_circle(data = data_circle, aes(x0 = x0, y0 = y0, r = rmax*2))+ # Draw ggplot2 plot with circle representing sampling circuits
  ### AB line
  geom_point(data = trees_and_edges %>%
               filter(e_form == "1") %>% 
               select(plot_ID, X1_inter_AB_17, X2_inter_AB_17, X_A, X_B, Y1_inter_AB_17, Y2_inter_AB_17, Y_A, Y_B) %>% 
               to_long(keys = c("X_name",  "Y_name"),
                       values = c( "X_value", "Y_value"),  
                       names(.)[2:5], names(.)[6:9]), 
             aes(x= X_value, y = Y_value, colour = X_name))+
  geom_line(data = trees_and_edges %>% 
              filter(e_form == "1") %>% 
              select(plot_ID, X_A, X_B, Y_A, Y_B) %>% 
              to_long(keys = c("X_name",  "Y_name"),
                      values = c( "X_value", "Y_value"),  
                      names(.)[2:3], names(.)[4:5]), 
            aes(x= X_value, y = Y_value))+
  geom_line(data = trees_and_edges %>% 
              filter(e_form == "1") %>% 
              select(plot_ID, X1_inter_AB_17, X_A, Y1_inter_AB_17, Y_A) %>% 
              to_long(keys = c("X_name",  "Y_name"),
                      values = c( "X_value", "Y_value"),
                      names(.)[2:3], names(.)[4:5]),  
            aes(x= X_value, y = Y_value, colour = X_name))+
  geom_line(data = trees_and_edges %>% 
              filter(e_form == "1") %>%
              select(plot_ID, X2_inter_AB_17, X_A, Y2_inter_AB_17, Y_A) %>% 
              to_long(keys = c("X_name",  "Y_name"),
                      values = c( "X_value", "Y_value"),
                      names(.)[2:3], names(.)[4:5]),  
            aes(x= X_value, y = Y_value, colour = X_name))+
  geom_line(data = trees_and_edges %>% 
              filter(e_form == "1") %>% 
              select(plot_ID, X1_inter_AB_17, X_B, Y1_inter_AB_17, Y_B) %>% 
              to_long(keys = c("X_name",  "Y_name"),
                      values = c( "X_value", "Y_value"),
                      names(.)[2:3], names(.)[4:5]),  
            aes(x= X_value, y = Y_value, colour = X_name))+
  geom_line(data = trees_and_edges %>% 
              filter(e_form == "1") %>% 
              select(plot_ID, X2_inter_AB_17, X_B, Y2_inter_AB_17, Y_B) %>% 
              to_long(keys = c("X_name",  "Y_name"),
                      values = c( "X_value", "Y_value"),
                      names(.)[2:3], names(.)[4:5]),  
            aes(x= X_value, y = Y_value, colour = X_name))+
  # trees
  geom_point(data =  trees_and_edges %>% filter(e_form == "1"), #%>% 
             aes(X_tree, Y_tree, colour = t_status_AB_ABT))+
  #theme_bw()+
  #facet_wrap(~plot_ID)
  
  # forest edge type 2 
  # if the # i removed, this part allows to plot plots with forest edges with a turning point
  ### AT line
  # ggplot() +  
  # geom_circle(data = data_circle, aes(x0 = x0, y0 = y0, r = r0))+ # Draw ggplot2 plot with circle representing sampling circuits 
  # geom_circle(data = data_circle, aes(x0 = x0, y0 = y0, r = rmax*2))+ # Draw ggplot2 plot with circle representing sampling circuits
  geom_point(data = trees_and_edges %>% 
               filter(e_form == "2") %>% 
               select(plot_ID, X_A, X_T, Y_A, Y_T) %>% 
               to_long(keys = c("X_name",  "Y_name"),
                       values = c( "X_value", "Y_value"),  
                       names(.)[2:3], names(.)[4:5]), 
             aes(x= X_value, y = Y_value, colour = "T"))+
  geom_line(data = trees_and_edges %>% 
              filter(e_form == "2") %>% 
              select(plot_ID, X_A, X_T, Y_A, Y_T) %>% 
              to_long(keys = c("X_name",  "Y_name"),
                      values = c( "X_value", "Y_value"),  
                      names(.)[2:3], names(.)[4:5]), 
            aes(x= X_value, y = Y_value))+
  # intersections choosen to draw the triangle AT
  geom_point(data = trees_and_edges %>% 
               filter(e_form == "2") %>% 
               select(plot_ID, X_inter_AT_triangle_60, X_A, Y_inter_AT_triangle_60, Y_A) %>% 
               to_long(keys = c("X_name",  "Y_name"),
                       values = c( "X_value", "Y_value"),  
                       names(.)[2:3], names(.)[4:5]), 
             aes(x= X_value, y = Y_value, colour = "A_Intercept"))+
  geom_line(data = trees_and_edges %>% 
              filter(e_form == "2") %>% 
              select(plot_ID, X_inter_AT_triangle_60, X_A, Y_inter_AT_triangle_60, Y_A) %>% 
              to_long(keys = c("X_name",  "Y_name"),
                      values = c( "X_value", "Y_value"),  
                      names(.)[2:3], names(.)[4:5]), 
            aes(x= X_value, y = Y_value))+
  # BT line 
  geom_point(data = trees_and_edges %>%
               filter(e_form == "2") %>% 
               select(plot_ID, X_B, X_T, Y_B, Y_T) %>% 
               to_long(keys = c("X_name",  "Y_name"),
                       values = c( "X_value", "Y_value"),  
                       names(.)[2:3], names(.)[4:5]), 
             aes(x= X_value, y = Y_value, colour = "T"))+
  geom_line(data = trees_and_edges %>%
              filter(e_form == "2") %>% 
              select(plot_ID, X_B, X_T, Y_B, Y_T) %>% 
              to_long(keys = c("X_name",  "Y_name"),
                      values = c( "X_value", "Y_value"),  
                      names(.)[2:3], names(.)[4:5]), 
            aes(x= X_value, y = Y_value))+
  # intersections choosen to draw the triangle BT
  geom_point(data = trees_and_edges %>% 
               filter(e_form == "2") %>% 
               select(plot_ID, X_inter_BT_triangle_60, X_B, Y_inter_BT_triangle_60, Y_B) %>% 
               to_long(keys = c("X_name",  "Y_name"),
                       values = c( "X_value", "Y_value"),  
                       names(.)[2:3], names(.)[4:5]), 
             aes(x= X_value, y = Y_value, colour = "B_intercept"))+
  geom_line(data = trees_and_edges %>% 
              filter(e_form == "2") %>% 
              select(plot_ID, X_inter_BT_triangle_60, X_B, Y_inter_BT_triangle_60, Y_B) %>% 
              to_long(keys = c("X_name",  "Y_name"),
                      values = c( "X_value", "Y_value"),  
                      names(.)[2:3], names(.)[4:5]), 
            aes(x= X_value, y = Y_value))+
  # trees
  geom_point(data =  trees_and_edges %>% filter(e_form == "2"), 
             aes(X_tree, Y_tree, colour = t_status_AB_ABT))+
  theme_bw()+ 
  facet_wrap(~plot_ID) 




# Notes:  -----------------------------------------------------------------
