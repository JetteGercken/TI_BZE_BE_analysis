
# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the  national soil inventory
# forest edges  

# ----- 0. SETUP ---------------------------------------------------------------

# ----- 0.1. packages and functions --------------------------------------------


source(paste0(getwd(), "/scripts/00_functions_library.R"))


# ----- 0.2. working directory -------------------------------------------------
here::here()
getwd()

out.path.BZE3 <- ("output/out_data/out_data_BZE/") 

# ----- 0.3 data import --------------------------------------------------------
# LIVING TREES
# HBI BE dataset: this dataset contains the inventory data of the tree inventory accompanying the second national soil inventory
# here one should immport the the dataset called HBI_trees_update_01.csv which includes only trees that are already sortet according to their inventory status (Baumkennzahl)
HBI_trees <- read.delim(file = here("data/input/BZE2_HBI/beab.csv"), sep = ",", dec = ",")
# HBI_trees_update <- read.delim(file = here("output/out_data/out_data_BZE/HBI_trees_update_1.csv"), sep = ",", dec = ",")
# HBI BE locations dataset: this dataset contains the coordinates of the center point of the tree inventory accompanying the second national soil inventory
HBI_loc <- read.delim(file = here("data/input/BZE2_HBI/location_HBI.csv"), sep = ";", dec = ",")
# HBI point info
HBI_inv_info <- read.delim(file = here("data/input/BZE2_HBI/be.csv"), sep = ",", dec = ",", stringsAsFactors=FALSE)



#SP_names_com_ID_tapeS <- read.delim(file = here("output/out_data/x_bart_tapeS.csv"), sep = ",", dec = ",") 

forest_edges_HBI <- read.delim(file = here("data/input/BZE2_HBI/be_waldraender.csv"), sep = ";", dec = ",")

# creating dataset with information about the concentric sampling circles
data_circle <- data.frame(x0 = c(0,0,0),       # x of centre point of all 3 circles is 0 
                          y0 = c(0,0,0),       # y of centre point of all 3 circles is 0 
                          r0 = c(5.64, 12.62, 17.84), # darius in m
                          rmax = c(30.00, 30.00, 30.00)) # these are the radi of the sampling circuits in m

# ----- 0.6 harmonising column names & structure  -------------------------
# HBI trees
colnames(HBI_trees) <- c("multi_stem", "D_mm", "DBH_class", "DBH_h_cm", "H_dm",
                         "azi_gon", "SP_code", "tree_ID", "plot_ID", "tree_inventory_status", 
                         "DBH_cm", "age", "C_layer", "C_h_dm", "Kraft", "Dist_cm", "age_meth")  
HBI_trees <- HBI_trees %>% select(plot_ID,  tree_ID ,  tree_inventory_status ,  multi_stem ,
                                  Dist_cm ,  azi_gon ,age ,  age_meth ,  SP_code , DBH_class ,  Kraft ,  
                                  C_layer , H_dm ,  C_h_dm , D_mm ,   DBH_h_cm ,  DBH_cm )
# HBI locations
HBI_loc <- HBI_loc %>% select("ï..ToTraktId", "ToEckId", "K2_RW",
                              "K2_HW", "K3_RW", "K3_HW", "RW_MED",
                              "HW_MED",  "LAT_MED",  "LON_MED", 
                              "LAT_MEAN", "LON_MEAN")
colnames(HBI_loc) <- c("plot_ID", "ToEckId", "K2_RW",
                       "K2_HW", "K3_RW", "K3_HW", "RW_MED",
                       "HW_MED",  "LAT_MED",  "LON_MED", 
                       "LAT_MEAN", "LON_MEAN") 

# HBI point/ inventory info
HBI_inv_info <- HBI_inv_info %>% select(bund_nr, datum, hbi_status )
colnames(HBI_inv_info) <- c("plot_ID", "date", "plot_inventory_status")
# create column that just contains year of inventory: https://www.geeksforgeeks.org/how-to-extract-year-from-date-in-r/
HBI_inv_info$date <- as.Date(HBI_inv_info$date)
HBI_inv_info$inv_year <- as.numeric(format(HBI_inv_info$date, "%Y"))
# this line can be removed later
HBI_inv_info <- HBI_inv_info %>% mutate(inv_year = ifelse(inv_year < 2012, 2012,inv_year), 
                                        inv = inv_name(inv_year))

# Forest edges 
colnames(forest_edges_HBI) <- c("plot_ID", "e_ID", "e_type", "e_form", 
                                "A_dist", "A_azi",  "B_dist", "B_azi", 
                                "T_dist", "T_azi") # t = turning point 

# ----- 1. joining in external info  --------------------------------------
# ----- 1.1. LIVING TREES -------------------------------------------------
# ----- 1.1.1. species & inventory names ----------------------------------------------
# ----- 1.1.1.1. HBI species & inventory ----------------------------------------------
HBI_trees <- HBI_trees %>% 
  left_join(., HBI_inv_info %>% select("plot_ID", "plot_inventory_status", "inv_year", "inv"), 
            by = "plot_ID") %>% 
  mutate(inv = inv_name(inv_year),
         DBH_cm = ifelse(DBH_h_cm == 130, D_mm/10, DBH_BWI(D_mm, DBH_h_cm)))



# ----- 1.1.2. forest edges -----------------------------------------------
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


# ----- 1.1.2.1. join in edge info to tree dataset ------------------------
# ----- 1.1.2.1.1. HBI join in forest edge info per plot -----------------------------------------------
HBI_trees <- HBI_trees %>% 
  # calculate the coordinates of every tree
  mutate(dist_m = Dist_cm/100, 
         X_tree = coord(data_circle$x0[1], data_circle$y0[1], dist_m, azi_gon, coordinate = "x"), 
         Y_tree = coord(data_circle$x0[1], data_circle$y0[1], dist_m, azi_gon, coordinate = "y")) %>% 
  # join in the forest edge information per plot 
  left_join(., forest_edges_HBI %>% 
              select(plot_ID, e_ID, e_type, e_form), 
            by = "plot_ID", 
            multiple = "all") # this is necesarry since there are, apperently, multiple edges per plot 

# ----- 1.1.2.1.2. BZE3 join in forest edge info per plot -----------------------------------------------
# BZE3_trees <- BZE3_trees %>% 
#   # calculate the coordinates of every tree
#   mutate(X_tree = x_coord(Dist_cm, azi_gon), 
#          Y_tree = y_coord(Dist_cm, azi_gon)) %>% 
#   # join in the forest edge information per plot 
#   left_join(., forest_edges_HBI %>% 
#               select(plot_ID, e_ID, e_type, e_form), 
#             by = "plot_ID", 
#             multiple = "all") # this is necesarry since there are, apperently, multiple edges per plot 



# ----- 1.1.2.2. edge  point coordinates,  line parameters, intersections with circles -----------------------------------------------------------
# set up line from 2 points manually
forest_edges_HBI.man <- forest_edges_HBI %>% 
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
  mutate(X1_inter_AB_17 = intersection_line_circle(b0_AB, b1_AB,  data_circle$y0[3], data_circle$x0[3], data_circle$r0[3], coordinate="x1"),
         X2_inter_AB_17 = intersection_line_circle(b0_AB, b1_AB, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3],  coordinate="x2"), 
         inter_status_AB_17 = intersection.status(intersection_line_circle(b0_AB, b1_AB,  data_circle$y0[3], data_circle$x0[3], data_circle$r0[3], coordinate="x1"),
                                                  intersection_line_circle(b0_AB, b1_AB, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3],  coordinate="x2")),
         # for AT line
         X1_inter_AT_17 = intersection_line_circle(b0_AT, b1_AT, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3], coordinate="x1"),
         X2_inter_AT_17 = intersection_line_circle(b0_AT, b1_AT, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3], coordinate="x2"), 
         inter_status_AT_17 = intersection.status(intersection_line_circle(b0_AT, b1_AT, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3], coordinate="x1"), 
                                                  intersection_line_circle(b0_AT, b1_AT, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3], coordinate="x2")),
         # for BT line
         X1_inter_BT_17 = intersection_line_circle(b0_BT, b1_BT, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3],  coordinate="x1"),
         X2_inter_BT_17 = intersection_line_circle(b0_BT, b1_BT, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3],  coordinate="x2"), 
         inter_status_BT_17 = intersection.status(intersection_line_circle(b0_BT, b1_BT, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3],  coordinate="x1"), 
                                                  intersection_line_circle(b0_BT, b1_BT, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3],  coordinate="x2"))) %>%   
  # y intersection with 17m circle: insert x of intercept with circle in equation of line
  # AB line 
  mutate(Y1_inter_AB_17 = intersection_line_circle(b0_AB, b1_AB,  data_circle$y0[3], data_circle$x0[3], data_circle$r0[3], coordinate="y1"),
         Y2_inter_AB_17 = intersection_line_circle(b0_AB, b1_AB,  data_circle$y0[3], data_circle$x0[3], data_circle$r0[3], coordinate="y2"), 
         # AT line 
         Y1_inter_AT_17 = intersection_line_circle(b0_AT, b1_AT, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3], coordinate="y1"), 
         Y2_inter_AT_17 = intersection_line_circle(b0_AT, b1_AT, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3], coordinate="y2"),
         # BT line 
         Y1_inter_BT_17 = intersection_line_circle(b0_BT, b1_BT, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3],  coordinate="y1"), 
         Y2_inter_BT_17 = intersection_line_circle(b0_BT, b1_BT, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3],  coordinate="y2")) %>%
  
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

#----1.1.2.3. tree-edge-status by combining tree and edge data ---------------------------------------
# next step will be to join the forest edges dataset into the trees datset, 
# via b0 and b1 and then compare the calculated tree_y with the functions result
# if the tree_y is higher then the function_y we have to do something with the tree...
# etc. assiningg another plot iD or something. 

trees_and_edges <-
  HBI_trees  %>% 
  # join in edges info per plot
  left_join(., forest_edges_HBI.man %>% 
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



# ----- 2. visualization  -------------------------------------------------
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
               inner_join(.,   forest_edges_HBI.man %>% 
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
              inner_join(.,   forest_edges_HBI.man %>% 
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
              inner_join(.,   forest_edges_HBI.man %>% 
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
              inner_join(.,   forest_edges_HBI.man %>% 
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
              inner_join(.,   forest_edges_HBI.man %>% 
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
              inner_join(.,   forest_edges_HBI.man %>% 
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
             #   inner_join(.,   forest_edges_HBI.man %>% 
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





# 3.2.1. georefferencing trough separate loops  --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

forest_edges_HBI.man.sub.e1.nogeo <-  forest_edges_HBI.man%>% filter(e_form == 1)# %>% 
# semi_join(HBI_loc %>% filter(!is.na( RW_MED) & !is.na(HW_MED)) %>%  select(plot_ID)  %>% distinct(), by = "plot_ID") # 62

triangle.e1.list.nogeo <- vector("list", length = length(forest_edges_HBI.man.sub.e1.nogeo$plot_ID))
triangle.e1.coords.nogeo <- vector("list", length = length(forest_edges_HBI.man.sub.e1.nogeo$plot_ID)*4)

for(i in 1:length(forest_edges_HBI.man.sub.e1.nogeo$plot_ID) ) {
  # i = 2
  # i = which(grepl(50086, forest_edges_HBI.man.sub.e1$plot_ID))
  
  # select plot ID, edge form and edge_ID accordint to positioin in the list
  my.plot.id <- forest_edges_HBI.man.sub.e1.nogeo[i, "plot_ID"] 
  my.e.id <- forest_edges_HBI.man.sub.e1.nogeo[i, "e_ID"]
  my.e.form <- forest_edges_HBI.man.sub.e1.nogeo[i, "e_form"]
  
  ## assign crs
  #my.utm.epsg <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"
  
  # select UTM corrdinates of the plot center by plot ID
  # my.center.easting <- 0 #HBI_loc[HBI_loc$plot_ID == my.plot.id, "RW_MED"]
  # my.center.northing <- 0 # HBI_loc[HBI_loc$plot_ID == my.plot.id, "HW_MED"]
  
  # circle center and radius to calcualte intersections 
  c.x0 = 0
  c.y0 = 0
  c.r0 = 17.84
  c.rmax = 60
  
  # extract polar coordiantes of forest edge
  # point A 
  dist.A <- forest_edges_HBI.man.sub.e1.nogeo[i, "A_dist"] 
  azi.A <- forest_edges_HBI.man.sub.e1.nogeo[i, "A_azi"] 
  x.A <- dist.A*sin(azi.A)       # this is: easting, longitude, RW
  y.A <- dist.A*cos(azi.A)       # this is: northing, latitude, HW
  # point B
  dist.B <- forest_edges_HBI.man.sub.e1.nogeo[i, "B_dist"] 
  azi.B <- forest_edges_HBI.man.sub.e1.nogeo[i, "B_azi"] 
  x.B <- dist.B*sin(azi.B)      # this is: easting, longitude, RW
  y.B <- dist.B*cos(azi.B)      # this is: northing, latitude, HW
  
  # calcualte slope (b1) and intercept (b0)
  b1 <- (y.B- y.A)/(x.B - x.A)
  b0 <- y.B - b1*x.B
  
  # calculate polar coordiantes of intersections of AB line with 
  x.1 <- intersection_line_circle(b0, b1, c.x0, c.y0,  c.rmax, coordinate = "x1") # this is: easting, longitude, RW
  y.1 <- intersection_line_circle(b0, b1, c.x0, c.y0,  c.rmax, coordinate = "y1") # this is: northing, latitude, HW
  x.2 <- intersection_line_circle(b0, b1, c.x0, c.y0,  c.rmax, coordinate = "x2") # this is: easting, longitude, RW
  y.2 <- intersection_line_circle(b0, b1 ,c.x0, c.y0,  c.rmax, coordinate = "y2") # this is: northing, latitude, HW
  
  # for edge form 1 we have to consider that the square has to be directed into the direction of the smaller half of the circle
  # calculate coordiantes of the middle of thie line between 
  x_m_line = (x.1 + x.2)/2
  y_m_line = (y.1 + y.2)/2
  # calculate the parameters of the equation between the middle of the line and the centre of the circle
  b1_MC = slope(c.x0, c.y0, x_m_line, y_m_line)
  b0_MC = intercept(c.x0, c.y0, x_m_line, y_m_line)
  # calcualte the x corrdiante of the interception of the line between M and the centre of the cirle and the circle at the given radio
  X1_inter_MC = intersection_line_circle(b0_MC, b1_MC,  c.x0, c.y0,  c.rmax,  coordinate = "x1") 
  X2_inter_MC = intersection_line_circle(b0_MC, b1_MC,  c.x0, c.y0,  c.rmax,  coordinate = "x2")
  # insert the intersection x corodinate in the line function to get the respective y coordinate
  y1_inter_MC = intersection_line_circle(b0_MC, b1_MC,  c.x0, c.y0,  c.rmax,  coordinate = "y1") 
  y2_inter_MC = intersection_line_circle(b0_MC, b1_MC,  c.x0, c.y0,  c.rmax,  coordinate = "y2")
  # distance between the intersections (inter_MC_1, inter_MC_2) to M on the line 
  dist_C_inter_1_MC = distance(X1_inter_MC, y1_inter_MC, x_m_line, y_m_line)
  dist_C_inter_2_MC = distance(X2_inter_MC, y2_inter_MC, x_m_line, y_m_line) 
  # find the x and y coordinate of the intersection on the shorter side , which is the side to exlcude from the plot 
  X_inter_MC_shorter_side = ifelse(dist_C_inter_1_MC < dist_C_inter_2_MC, X1_inter_MC, X2_inter_MC) 
  Y_inter_MC_shorter_side = ifelse(dist_C_inter_1_MC < dist_C_inter_2_MC, y1_inter_MC, y2_inter_MC)
  
  # creating the polar coordiantes of a turning point of a triangle by selecting the intersection of the 
  # line from the middle of the AB.inter-ray and the circle center (MC_line) with 
  # the 60m radius at the "shorter side" so the intersection of the MC_line with a 60m radius that has le lest distance to the MC point on the AB.inter-ray
  turning.east <- X_inter_MC_shorter_side # + my.center.easting
  turning.north <-  Y_inter_MC_shorter_side #+ my.center.northing 
  
  # UTM coordiantes of corner points 
  x1.east <- x.1 # + my.center.easting 
  y1.north <- y.1  #+ my.center.northing 
  x2.east <- x.2 # + my.center.easting 
  y2.north <- y.2 #+ my.center.northing 
  
  # create dataframe that holds coordinates of the intersections of the AB line with a 60m radius and the turning pint of a diagonal line through the AB line with a 60m radius circle
  triangle.e1.df <- as.data.frame(cbind("lon" = c(turning.east, x1.east, x2.east, turning.east),
                                        "lat" = c(turning.north, y1.north, y2.north,  turning.north),
                                        "plot_ID" = c(my.plot.id, my.plot.id, my.plot.id, my.plot.id),
                                        "e_ID" = c(my.e.id, my.e.id, my.e.id, my.e.id)))
  
  # creating polygones in sf: https://stackoverflow.com/questions/61215968/creating-sf-polygons-from-a-dataframe
  triangle.e1.poly <-  sfheaders::sf_polygon(obj = triangle.e1.df  
                                             , x = "lon"
                                             , y = "lat"
                                             , polygon_id = "e_ID")
  # assing crs
  #sf::st_crs(triangle.e1.poly) <- my.utm.epsg
  
  print(plot(triangle.e1.poly, main = my.plot.id))
  
  #save polygones in list 
  triangle.e1.list.nogeo[[i]] <- c("plot_ID" = my.plot.id, triangle.e1.poly)
  
  # save coordiantes of polygones in list
  triangle.e1.coords.nogeo[[i]] <- triangle.e1.df
  
} # closing loop for square polys of edge form 1

triangle.e1.list.final.nogeo <- rbindlist(triangle.e1.list.nogeo, fill=TRUE)
triangle.e1.poly.df.nogeo <- as.data.frame(triangle.e1.list.final.nogeo) %>% mutate("e_form" = 1)

triangle.e1.coords.list.nogeo <- rbindlist(triangle.e1.coords.nogeo)
triangle.e1.coords.df.nogeo <- as.data.frame(triangle.e1.coords.list.nogeo) %>% 
  mutate("e_form" = 1)




# 3.2.1.2. nogeo creating list of triangle polygons for edge form 2 ----------------------------------------------------------------------------------------------------------------------------------------------------------------

## loop to create list of polygones for edge form 1
forest_edges_HBI.man.sub.e2.nogeo <- forest_edges_HBI.man %>%
  filter(e_form == 2) %>%  # nrow = 21
  filter(inter_status_AT_17 == "two I" | inter_status_BT_17 == "two I") #%>% 
#semi_join(HBI_loc %>% filter(!is.na( RW_MED) & !is.na(HW_MED)) %>%  select(plot_ID)  %>% distinct(), by = "plot_ID")  # nrow = 21

triangle.e2.list.nogeo <- vector("list", length = length(forest_edges_HBI.man.sub.e2.nogeo$plot_ID) )
triangle.e2.coords.nogeo <- vector("list", length = length(forest_edges_HBI.man.sub.e2.nogeo$plot_ID)*4 )

for(i in 1:length(forest_edges_HBI.man.sub.e2.nogeo$plot_ID) ) {
  # i = 1
  # i = which(grepl(50023, forest_edges_HBI.man.sub.e2.nogeo$plot_ID)
  
  # select plot ID accordint to positioin in the list
  my.plot.id <- forest_edges_HBI.man.sub.e2.nogeo[i, "plot_ID"] 
  my.e.id <- forest_edges_HBI.man.sub.e2.nogeo[i, "e_ID"] 
  my.e.form <- forest_edges_HBI.man.sub.e2.nogeo[i, "e_form"]
  #my.n.of.edges <- forest_edges_HBI.man %>% filter(plot_ID == my.plot.id) %>% group_by(plot_ID) %>% summarize(n = n()) %>% dplyr::pull(n)
  
  # assign crs
  #my.utm.epsg <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"
  
  # select UTM corrdinates of the plot center
  # my.center.easting <- HBI_loc[HBI_loc$plot_ID == my.plot.id, "RW_MED"]
  # my.center.northing <- HBI_loc[HBI_loc$plot_ID == my.plot.id, "HW_MED"]
  
  # circle data
  c.x0 = 0
  c.y0 = 0
  c.r0 = 17.84
  c.rmax = 300
  
  # extract polar coordiantes of forest edge
  # point A 
  dist.A <- forest_edges_HBI.man.sub.e2.nogeo[i, "A_dist"] 
  azi.A <- forest_edges_HBI.man.sub.e2.nogeo[i, "A_azi"] 
  x.A <- dist.A*sin(azi.A)   # longitude, easting, RW, X
  y.A <- dist.A*cos(azi.A)   # latitude, northing, HW, y 
  
  # point B
  dist.B <- forest_edges_HBI.man.sub.e2.nogeo[i, "B_dist"] 
  azi.B <- forest_edges_HBI.man.sub.e2.nogeo[i, "B_azi"] 
  x.B <- dist.B*sin(azi.B)   # longitude, easting, RW, X
  y.B <- dist.B*cos(azi.B)   # latitude, northing, HW, y 
  
  # point T
  dist.T <- forest_edges_HBI.man.sub.e2.nogeo[i, "T_dist"] 
  azi.T <- forest_edges_HBI.man.sub.e2.nogeo[i, "T_azi"] 
  x.T <- dist.T*sin(azi.T)   # longitude, easting, RW, X
  y.T <- dist.T*cos(azi.T)   # latitude, northing, HW, y 
  
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
  T.east <- x.T # + my.center.easting                             # longitude, easting, RW, X
  T.north <- y.T # + my.center.northing                           # latitude, northing, HW, y 
  AT.x.east <-  AT.x # + my.center.easting                        # longitude, easting, RW, X
  AT.y.north <- AT.y # + my.center.northing                       # latitude, northing, HW, y 
  BT.x.east <- BT.x  # + my.center.easting                        # longitude, easting, RW, X
  BT.y.north <- BT.y # + my.center.northing                       # latitude, northing, HW, y 
  
  # create dataframe with triangle corner UTM coordiantes
  triangle.e2.df <- as.data.frame(cbind("lon" = c(T.east, AT.x.east, BT.x.east, T.east),       # longitude, easting, RW, X
                                        "lat" = c(T.north, AT.y.north, BT.y.north, T.north),   # latitude, northing, HW, y
                                        "plot_ID" =  c(my.plot.id, my.plot.id, my.plot.id, my.plot.id), 
                                        "e_ID" = c(my.e.id, my.e.id, my.e.id, my.e.id )))
  
  # createa polygone with triangle corners via sf package: https://r-spatial.github.io/sf/reference/st.html
  triangle.e2.poly <- sfheaders::sf_polygon(obj = triangle.e2.df
                                            , x = "lon"
                                            , y = "lat"
                                            , polygon_id = "e_ID")
  # assing crs
  #sf::st_crs(triangle.e2.poly) <- my.utm.epsg
  
  # print triangle
  print(plot(triangle.e2.poly$geometry, main = my.plot.id))
  
  # save polygones in list
  triangle.e2.list.nogeo[[i]] <- c("plot_ID" = my.plot.id, triangle.e2.poly)
  
  # save coordiantes of polygones in list
  triangle.e2.coords.nogeo[[i]] <- triangle.e2.df
}


# list of polygones
triangle.e2.list.final.nogeo <- rbindlist(triangle.e2.list.nogeo)
triangle.e2.poly.df.nogeo <- as.data.frame(triangle.e2.list.final.nogeo) %>% mutate("e_form" = 2)

#list of coordiantes of triangle.e2 polygones
triangle.e2.coords.list.nogeo <- rbindlist(triangle.e2.coords.nogeo)
triangle.e2.coords.df.nogeo <- as.data.frame(triangle.e2.coords.list.nogeo) %>%  mutate("e_form" = 2) 


# 3.2.1.3. loop for intersections between circles and edges -------------------------------------------------------------------------------------------------------------------------------------
# 3.2.1.3.1. loop for intersections for plots with only one edge  -------------------------------------------------------------------------------------------------------------------------------

# dataprep for loop
# bind polygone dataframes together
edge.poly.df.nogeo <- rbind(triangle.e1.poly.df.nogeo, triangle.e2.poly.df.nogeo) # rows: 83
# createa dataframe with plots that have only one forest edges
forest_edges_HBI.man.sub.1.edge.nogeo <- forest_edges_HBI.man %>% # rows:84
  # select only plots with a known edge form and for edge 2 only those that actually intersect the 17m circle
  filter(e_form == 1 | e_form == 2 & inter_status_AT_17 == "two I" | e_form == 2 & inter_status_BT_17 == "two I") %>%  # rows:81
  # remove plots that have two edges
  anti_join(forest_edges_HBI.man %>%  filter(e_form == 1 | e_form == 2 & inter_status_AT_17 == "two I" | e_form == 2 & inter_status_BT_17 == "two I") %>% 
              group_by(plot_ID) %>% summarise(n = n()) %>% filter(n > 1) %>% select(plot_ID), by = "plot_ID")# %>% # 14 plots with 2 edges --> 28 rows -> 53 left
# remove plots that do now have a corresponding center coordiante in the HBI loc document
#semi_join(HBI_loc %>% filter(!is.na( RW_MED) & !is.na(HW_MED)) %>%  select(plot_ID)  %>% distinct(), by = "plot_ID") # nrow = 52 --> there is 1 plots without corresponding 

edges.list.nogeo <- vector("list", length = length(unique(forest_edges_HBI.man.sub.1.edge.nogeo$plot_ID)))
inter.poly.list.nogeo <- vector("list", length = length(unique(forest_edges_HBI.man.sub.1.edge.nogeo$plot_ID)))
#inter.poly.NA.list <- vector("list", length = length(unique(forest_edges_HBI.man.sub.1.edge$plot_ID)))
remaining.circle.poly.list.nogeo <- vector("list", length = length(unique(forest_edges_HBI.man.sub.1.edge.nogeo$plot_ID)))
remaining.circle.multipoly.list.nogeo <- vector("list", length = length(unique(forest_edges_HBI.man.sub.1.edge.nogeo$plot_ID)))

# loop for 17m radius circle
for (i in 1:length(unique(forest_edges_HBI.man.sub.1.edge.nogeo$plot_ID))){ 
  # i = 48
  #i = which(grepl(50133, (forest_edges_HBI.man.sub.1.edge.nogeo$plot_ID)))
  
  # select plot ID of the respective circle 
  my.plot.id <- forest_edges_HBI.man.sub.1.edge.nogeo[i, "plot_ID"]
  my.e.form <- edge.poly.df.nogeo$e_form[edge.poly.df.nogeo$plot_ID == my.plot.id]
  my.e.id <- edge.poly.df.nogeo$e_ID[edge.poly.df.nogeo$plot_ID == my.plot.id]
  
  # assign crs
  #my.utm.epsg <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"
  
  # select UTM corrdinates of the plot center
  # my.center.easting <- HBI_loc[HBI_loc$plot_ID == my.plot.id, "RW_MED"]
  # my.center.northing <- HBI_loc[HBI_loc$plot_ID == my.plot.id, "HW_MED"]
  
  # select the circle polygone corresponding with the plot ID
  # my.circle.17 <- sf::st_as_sf(circle.poly.df %>% filter(id == my.plot.id) %>% distinct())
  
  # circle data
  center.df<- as.data.frame(cbind("lon" = c.x0, "lat" = c.y0))
  # center.df <- as.data.frame(cbind("lon" = my.center.easting, "lat" = my.center.northing))
  c.x0 = 0 
  c.y0 = 0
  c.r3 = 17.84
  c.r2 = 12.62
  c.r1 = 5.64
  
  # build polygon (circlular buffer) around center point
  circle.pt <- sf::st_as_sf(center.df, coords = c("lon", "lat"))
  circle.17 <- sf::st_buffer(circle.pt, c.r3)
  circle.12 <- sf::st_buffer(circle.pt, c.r2)
  circle.5 <- sf::st_buffer(circle.pt, c.r1)
  
  # select the respective polygones the circle is intersected by
  my.poly <- sf::st_as_sf(edge.poly.df.nogeo %>% filter(plot_ID == my.plot.id))
  
  # print the cirlce and edge polygone
   print(plot(circle.17, main = paste0("plot:", " ", my.plot.id, ",", " ", "e_form:"," ", my.e.form)), 
         plot(my.poly, col = 0, add = T))
  
  
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
  inter.poly.17 <- if(isTRUE(inter.poly.17) && inter.poly.17$geometry == circle.17$geometry){inter.poly.17 <- data.frame()}else{inter.poly.17}
  # if the edge-circle intersection is equal to 0 (so there is no intersection) return the whole cirlce as remaining circle area, else calculate the remaining circle by decuctng the intersection are from the circle area
  remaining.circle.poly.17  <- if(isTRUE(nrow(inter.poly.17)==0)){circle.17}else{sf::st_difference(circle.17, inter.poly.17)}
 # plot(remaining.circle.poly.17)
 
   # calculate area
  # intersection
  inter.area.17 <- ifelse(nrow(inter.poly.17) == 0, 0, sf::st_area(inter.poly.17))
  #remaining circle
  remaining.circle.area.17 <- ifelse(nrow(remaining.circle.poly.17) == 0, 0, sf::st_area(remaining.circle.poly.17))
  # create area dataframe for areas
  inter.area.df.17 <- as.data.frame(cbind("plot_ID" = c(my.plot.id, my.plot.id), "e_ID" = c(my.e.id,  0),
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
  inter.poly.12 <- if(isTRUE(inter.poly.12) && inter.poly.12$geometry == circle.12$geometry){inter.poly.12 <- data.frame()}else{inter.poly.12}
  # if the edge-circle intersection is equal to 0 (so there is no intersection) return the whole cirlce as remaining circle area, else calculate the remaining circle by decuctng the intersection are from the circle area
  remaining.circle.poly.12  <- if(isTRUE(nrow(inter.poly.12)==0)){circle.12}else{sf::st_difference(circle.12, inter.poly.12)}
  # plot(remaining.circle.poly.12$geometry)
  
  # calculate area
  # intersection
  inter.area.12 <- ifelse(nrow(inter.poly.12) == 0, 0, sf::st_area(inter.poly.12))
  #remaining circle
  remaining.circle.area.12 <- ifelse(nrow(remaining.circle.poly.12) == 0, 0, sf::st_area(remaining.circle.poly.12))
  # create area dataframe for areas
  inter.area.df.12 <- as.data.frame(cbind("plot_ID" = c(my.plot.id, my.plot.id), "e_ID" = c(my.e.id,  0),
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
  inter.poly.5 <- if(isTRUE(inter.poly.5) && inter.poly.5$geometry == circle.5$geometry){inter.poly.5 <-data.frame()}else{inter.poly.5}
  # if the edge-circle intersection is equal to 0 (so there is no intersection) return the whole cirlce as remaining circle area, else calculate the remaining circle by decuctng the intersection are from the circle area
  remaining.circle.poly.5  <- if(isTRUE(nrow(inter.poly.5)==0)){circle.5}else{sf::st_difference(circle.5, inter.poly.5)}
  # calculate area
  # intersection
  inter.area.5 <- ifelse(nrow(inter.poly.5) == 0, 0, sf::st_area(inter.poly.5))
  #remaining circle
  remaining.circle.area.5 <- ifelse(nrow(remaining.circle.poly.5) == 0, 0, sf::st_area(remaining.circle.poly.5))
  # create area dataframe for areas
  inter.area.df.5 <- as.data.frame(cbind("plot_ID" = c(my.plot.id, my.plot.id), "e_ID" = c(my.e.id,  0),
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
  inter.area.df <- inter.area.df %>% left_join(., stand.df, 
                                               by = c("plot_ID", "e_ID"))
  
  
  # list with inter and remaining circle areas areas
  edges.list.nogeo[[i]] <- inter.area.df
  
  # create lists with polgons of intersections if there are intersections, if there is non, save the polygone instead. 
  inter.poly.list.nogeo[[i]] <- if(isTRUE(nrow(inter.poly.17)!= 0)){c(inter.poly.17)}else{c(my.poly)}
  
  # testing if corect inter was saved: 
  # i.plot <- if(isTRUE(nrow(inter.poly.17)!= 0)){c(inter.poly.17)}else{c(my.poly)}
  # plot(i.plot$geometry)
  # plot(circle.17, add = T)
  
  # save remaining circles polygones into list
  #plot(remaining.circle.poly.17)
  remaining.circle.poly.17$plot_ID <- my.plot.id
  remaining.circle.poly.17$e_ID <- 0
  remaining.circle.poly.17$e_form <- 0
  remaining.circle.poly.17$geometry <- remaining.circle.poly.17$geometry
  #plot(remaining.circle.poly.17)
  # create list wit polygones of the remaining cirlce when it´s only one polygone
  remaining.circle.poly.list.nogeo[[i]] <- if(st_geometry_type(remaining.circle.poly.17)== "POLYGON"){c(remaining.circle.poly.17)}else{}
  # create list wit polygones of the remaining cirlce when it´s a multipoligone
  remaining.circle.multipoly.list.nogeo[[i]] <- if(st_geometry_type(remaining.circle.poly.17)== "MULTIPOLYGON"){c(remaining.circle.poly.17)}else{}
  
  
}


# list of areas
edges.area.list.final.nogeo <- rbindlist(edges.list.nogeo)
edges.area.df.nogeo <- as.data.frame(edges.area.list.final.nogeo)

# list of polygones of forest edges 
inter.poly.list.final.nogeo <- rbindlist(inter.poly.list.nogeo, fill=TRUE)
inter.poly.one.edge.df.nogeo <- as.data.frame(inter.poly.list.final.nogeo)#[,c(2, 1, 3, 5)]%>% arrange(id, e_id)

# list of polygones of remainign circles 
rem.circle.poly.list.final.nogeo <- rbindlist(remaining.circle.poly.list.nogeo, fill = TRUE)
rem.circle.poly.df.nogeo <- as.data.frame(rem.circle.poly.list.final.nogeo)#[,c(2,1,4)]  %>% distinct()
# list of multipolygones of remaining circles
rem.circle.multipoly.list.final.nogeo <- rbindlist(remaining.circle.multipoly.list.nogeo)
rem.circle.multipoly.df.nogeo <- as.data.frame(rem.circle.multipoly.list.final.nogeo)#[,c(2,1,4)] %>% distinct()
# binding the both circle lists back together 
rem.circle.one.edge.df.nogeo <- rbind(rem.circle.poly.df.nogeo, rem.circle.multipoly.df.nogeo)





# 3.2.1.3.1. loop for intersections for plots with two edges ----------------------------------------------------------------------------------------------------------------------------
# dataprep for loop
# createa dataframe with plots that have only one forest edges
forest_edges_HBI.man.sub.2.edges.nogeo <- forest_edges_HBI.man %>% # rows:84
  # select only plots with a known edge form and for edge 2 only those that actually intersect the 17m circle
  filter(e_form == 1 | 
           e_form == 2 & inter_status_AT_17 == "two I" | 
           e_form == 2 & inter_status_BT_17 == "two I") %>%  # rows:81
  #filter(inter_status_AB_17 == "two I") %>% 
  # remove plots that have two edges
  semi_join(forest_edges_HBI.man %>% filter(e_form == 1 | 
                                              e_form == 2 & inter_status_AT_17 == "two I" | 
                                              e_form == 2 & inter_status_BT_17 == "two I") %>% 
              group_by(plot_ID) %>% summarise(n = n()) %>% filter(n > 1) %>% select(plot_ID), by = "plot_ID") #%>% # 14 plots iwth 2 edges --> 28 rows
# remove plots that do now have a corresponding center coordiante in the HBI loc document
#semi_join(HBI_loc %>% filter(!is.na( RW_MED) & !is.na(HW_MED)) %>%  select(plot_ID)  %>% distinct(), by = "plot_ID") # nrow = 28 

# prepare output lists
# list to save areas in
edges.list.two.edges.nogeo <- vector("list", length = length(unique(forest_edges_HBI.man.sub.2.edges.nogeo$plot_ID)))
# list to save the first intersection polygone per plot in
inter.poly.1.list.nogeo <- vector("list", length = length(unique(forest_edges_HBI.man.sub.2.edges.nogeo$plot_ID)))
# list to save the second intersection polygone per plot in
inter.poly.2.list.nogeo <- vector("list", length = length(unique(forest_edges_HBI.man.sub.2.edges.nogeo$plot_ID)))
# list to save the remaining circle polygones per plot in
rem.circle.poly.2.edges.list.nogeo <- vector("list", length = length(unique(forest_edges_HBI.man.sub.2.edges.nogeo$plot_ID)))
# list to save the remaining circle MULTIpolygones per plot in
rem.circle.multipoly.2.edges.list.nogeo <- vector("list", length = length(unique(forest_edges_HBI.man.sub.2.edges.nogeo$plot_ID)))
# list for plop IDs of those plots where the edge lines/ polygones intersect within the 17.84m circle
intersection.warning.edges.list.nogeo <- vector("list", length = length(unique(forest_edges_HBI.man.sub.2.edges.nogeo$plot_ID)))

for (i in 1:length(unique(forest_edges_HBI.man.sub.2.edges.nogeo$plot_ID))){ 
  #i = 6
  # i = which(grepl(50009, unique(forest_edges_HBI.man.sub.2.edges.nogeo$plot_ID)))
  
  # select plot ID of the respective circle 
  my.plot.id <- unique(forest_edges_HBI.man.sub.2.edges.nogeo$plot_ID)[i]
  
  # select the UTM coordiantes of the center of the cirlce corresponding with the plot ID
  # my.center.easting <- HBI_loc[HBI_loc$plot_ID == my.plot.id, "RW_MED"]
  # my.center.northing <- HBI_loc[HBI_loc$plot_ID == my.plot.id, "HW_MED"]
  
  #### build circle
  # circle data
  
  c.x0 = 0 
  c.y0 = 0
  c.r3 = 17.84
  c.r2 = 12.62
  c.r1 = 5.64
  # build polygon (circlular buffer) around center point
  center.df<- as.data.frame(cbind("lon" = c.x0, "lat" = c.y0))
  # center.df <- as.data.frame(cbind("lon" = my.center.easting, "lat" = my.center.northing))
  circle.pt <- sf::st_as_sf(center.df, coords = c("lon", "lat"))
  circle.17 <- sf::st_buffer(circle.pt, c.r3)
  circle.12 <- sf::st_buffer(circle.pt, c.r2)
  circle.5 <- sf::st_buffer(circle.pt, c.r1)
  
  #### select the  polygones the circle is intersected by
  # select the polygones with the same plot ID as the cirlce
  my.plot.polys.df <- edge.poly.df.nogeo %>% filter(plot_ID == my.plot.id) %>% arrange(e_ID)
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
  # print(plot(circle.17$geometry), 
  #       plot(my.poly.1$geometry,  add = T), 
  #       plot(my.poly.2$geometry, add = T))
  
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
  inter.poly.17.2 <- st_intersection(remaining.circle.17.1, my.poly.2)
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
  inter.poly.12.2 <- st_intersection(remaining.circle.12.1, my.poly.2)
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
  inter.poly.5.2 <- st_intersection(remaining.circle.5.1, my.poly.2)
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
  
  plot(remaining.circle.17.1.and.2.poly$geometry, main = paste0(my.plot.id, " - ", my.e.form.1, " - ", my.e.form.2))
  plot(remaining.circle.12.1.and.2.poly$geometry, add = T)
  plot(remaining.circle.5.1.and.2.poly$geometry, add = T)
  
  
  
  
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
          #"e_form" = c(my.poly.1$e_form, my.poly.2$e_form, 0),"shape" = c("edge", "edge", "circle"),
          "CCS_r_m" = c(c.r3, c.r3, c.r3), "inter_stat" = c(inter.status.poly.17.1, inter.status.poly.17.2, 0),
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
          "CCS_r_m" = c(c.r2, c.r2, c.r2), "inter_stat" = c(inter.status.poly.12.1, inter.status.poly.12.2, 0),
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
     group_by(plot_ID) %>% 
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
                               by = c("plot_ID", "e_ID"))
   
  # save datacframe per plot in list
  edges.list.two.edges.nogeo[[i]] <- inter.area.df
  
  
  # create list with those plot ID where the two edges intersect within the radius of 17.84m
  intersection.between.edges.17 <- sf::st_intersection(
    sf::st_intersection(my.poly.1, circle.17), # intersection poly 1 and cirlce 17
    sf::st_intersection(my.poly.2, circle.17) # intersection poly 2 and cirlce 17
  )
  intersection.warning.edges <- ifelse(nrow(intersection.between.edges.17) == 0, NA, intersection.between.edges.17$plot_ID)
  intersection.warning.edges.list.nogeo[[i]] <- as.data.frame(cbind("plot_ID" = c(intersection.warning.edges)))
  
  
  
  ## save intersection polygones in list
  # poly.1
  inter.poly.1.list.nogeo[[i]] <- if(nrow(inter.poly.17.1)!= 0){c(inter.poly.17.1)}else{c(my.poly.1)}
  # poly.2
  inter.poly.2.list.nogeo[[i]] <- if(nrow(inter.poly.17.2)!= 0){c(inter.poly.17.2)}else{c( my.poly.2)}
  
  ## save the reimaingf circle polygones in a list
  remaining.circle.17.1.and.2.poly$plot_ID <- my.plot.id
  remaining.circle.17.1.and.2.poly$e_ID <- 0
  remaining.circle.17.1.and.2.poly$e_form <- 0
  remaining.circle.17.1.and.2.poly$geometry <- remaining.circle.17.1.and.2.poly$geometry
  # create list wit polygones of the remaining cirlce when it´s only one polygone
  rem.circle.poly.2.edges.list.nogeo[[i]] <- if(st_geometry_type(remaining.circle.17.1.and.2.poly)== "POLYGON"){c(remaining.circle.17.1.and.2.poly)}else{}
  # create list wit polygones of the remaining cirlce when it´s a multipoligone
  rem.circle.multipoly.2.edges.list.nogeo[[i]] <- if(st_geometry_type(remaining.circle.17.1.and.2.poly)== "MULTIPOLYGON"){c(remaining.circle.17.1.and.2.poly)}else{}
  
}

# save areas into dataframe
edges.list.two.edges.final.nogeo <- rbindlist(edges.list.two.edges.nogeo)
edges.area.two.edges.df.nogeo <- as.data.frame(edges.list.two.edges.final.nogeo)

# save plot IDs with overlappig edges within the 17.84m circle into dataframe
intersection.two.edges.warning.final.nogeo <- rbindlist(intersection.warning.edges.list.nogeo, fill=TRUE)
intersection.two.edges.warning.df.nogeo <- na.omit(as.data.frame(intersection.two.edges.warning.final.nogeo))
if(nrow(intersection.two.edges.warning.df.nogeo)!=0){print("There are plots with overlapping edges within a 17.84m radius around the plot center. 
                                                           Please check dataset intersection.two.edges.warning.df.nogeo")}

# save intersection polygones into dataframe 
# list of polygones 1 of forest edges 
inter.poly.1.list.final.nogeo <- rbindlist(inter.poly.1.list.nogeo, fill=TRUE)
inter.poly.1.two.edges.df.nogeo <- as.data.frame(inter.poly.1.list.final.nogeo)
# list of polygones 2 of forest edges 
inter.poly.2.list.final.nogeo <- rbindlist(inter.poly.2.list.nogeo, fill=TRUE)
inter.poly.2.two.edges.df.nogeo <- as.data.frame(inter.poly.2.list.final.nogeo)[,c(1,2,3,4)]
# bind the both edges per plot together
inter.poly.two.edges.df.nogeo <- rbind(inter.poly.1.two.edges.df.nogeo, inter.poly.2.two.edges.df.nogeo) %>% arrange(plot_ID, e_ID)

# list of polygones of remainign circles 
rem.circle.poly.two.edges.list.final.nogeo <- rbindlist(rem.circle.poly.2.edges.list.nogeo, fill = TRUE)
rem.circle.poly.two.edges.df.nogeo <- as.data.frame(rem.circle.poly.two.edges.list.final.nogeo)[,c(1,2,3,4)]  %>% distinct()
# list of multipolygones of remaining circles
rem.circle.multipoly.two.edges.list.final.nogeo <- rbindlist(rem.circle.multipoly.2.edges.list.nogeo)
rem.circle.multipoly.two.edges.df.nogeo <- as.data.frame(rem.circle.multipoly.two.edges.list.final.nogeo)[,c(1,2,3,10)] %>% distinct()
# binding the both circle lists back together 
rem.circle.two.edges.df.nogeo <- if(nrow(rem.circle.poly.two.edges.df.nogeo) != 0 && nrow(rem.circle.multipoly.two.edges.list.final.nogeo) != 0){
  rbind(rem.circle.poly.two.edges.df.nogeo, rem.circle.multipoly.two.edges.df.nogeo)
}else{rem.circle.poly.two.edges.df.nogeo}



# 3.2.1.4. binding all egde and remaining circle areas and polys tog --------

# bind all edges area dataframes together
all.edges.area.df.nogeo <- rbind(edges.area.df.nogeo, edges.area.two.edges.df.nogeo) %>% mutate(area_m2 = as.numeric(area_m2))




# 3.2.2. sorting TREES into edge and remaining circle polygones ---------
# 3.2.2.1. plots with one edge: sorting trees into edge and remaining circle polygones ---------
trees.one.edge.nogeo <- HBI_trees %>%
  # filter only for trees that are located in plots with a forest edge
  semi_join(forest_edges_HBI.man %>% filter(e_form == 1 | e_form == 2 & inter_status_AT_17 == "two I" | e_form == 2 & inter_status_BT_17 == "two I") %>% 
              select(plot_ID) %>% distinct(), by = "plot_ID") %>% 
  # filter for trees located in plots htat haev only one forest edge
  anti_join(forest_edges_HBI.man %>% filter(e_form == 1 | e_form == 2 & inter_status_AT_17 == "two I" | e_form == 2 & inter_status_BT_17 == "two I") %>% group_by(plot_ID) %>% summarise(n = n()) %>% filter(n > 1) %>% select(plot_ID), by = "plot_ID") #%>% 
# remove plots that do now have a corresponding center coordiante in the HBI loc document
#semi_join(HBI_loc %>% filter(!is.na( RW_MED) & !is.na(HW_MED)) %>%  select(plot_ID)  %>% distinct(), by = "plot_ID")

tree.status.list.nogeo <- vector("list", length = length(trees.one.edge.nogeo$tree_ID))
tree.points.list.nogeo <- vector("list", length = length(trees.one.edge.nogeo$tree_ID))

for (i in 1:length(trees.one.edge.nogeo$tree_ID)){ 
  #i = 997
  
 # i = which(grepl(50133, (trees.one.edge.nogeo$plot_ID)))[7]
  
  
  # select plot ID accordint to positioin in the list
  my.plot.id <- trees.one.edge.nogeo[i, "plot_ID"] 
  my.tree.id <- trees.one.edge.nogeo[i, "tree_ID"]
  my.inv <- trees.one.edge.nogeo[i, "inv"]
  
  # select the remaining cirlce we want to intersect the tree with
  my.rem.circle <- sf::st_as_sf(rem.circle.one.edge.df.nogeo %>% filter(plot_ID == my.plot.id) %>% distinct())
  my.inter <- sf::st_as_sf(inter.poly.one.edge.df.nogeo  %>% filter(plot_ID == my.plot.id) %>% distinct())
  
  # sort area dataframe by size of cirlce fragments: 
  # bigger polygone/ polygone with greater area is assigned to category A, smaller area polygone is assigned to B
  area.plot.df <- edges.area.df.nogeo %>% filter(plot_ID == my.plot.id & CCS_r_m == 17.84) %>% 
    arrange(area_m2) %>% 
    mutate(stand = case_when(
      row_number()== 1 ~ "B",
      row_number()== 2 ~ "A",
      TRUE ~ NA))
  # assign stand category to the polygones depending on which one is bigger/ smaller
  my.rem.circle$stand <- area.plot.df$stand[area.plot.df$e_ID == 0]
  my.inter$stand <- area.plot.df$stand[area.plot.df$e_ID == 1 | area.plot.df$e_ID == 2]
  
  # # assign crs
  #my.utm.epsg <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"
  # # select UTM corrdinates of the plot center
  #my.center.easting <- HBI_loc[HBI_loc$plot_ID == my.plot.id, "RW_MED"]
  #my.center.northing <- HBI_loc[HBI_loc$plot_ID == my.plot.id, "HW_MED"]
  
  # extract polar coordiantes of forest edge
  # point A 
  dist.tree <- trees.one.edge.nogeo[i, "Dist_cm"]/100 
  azi.tree <- trees.one.edge.nogeo[i, "azi_gon"] 
  x.tree <- dist.tree*sin(azi.tree)   # longitude, easting, RW, X
  y.tree <- dist.tree*cos(azi.tree)   # latitude, northing, HW, y 
  # transform polar into cartesian coordiantes
  tree.east <- x.tree #+ my.center.easting 
  tree.north <-  y.tree # + my.center.northing
  
  # save cartesian coordiantes in dataframe
  tree.coord.df <- as.data.frame(cbind(
    "plot_ID" = c(as.integer(my.plot.id)), 
    "tree_ID" = c(as.integer(my.tree.id)),
    "inv" = c(my.inv),
    "lon" = c(tree.east),
    "lat" = c(tree.north)
  ))
  
  # create sf point object from dataframe
  #https://stackoverflow.com/questions/52551016/creating-sf-points-from-multiple-lat-longs
  tree.sf <-  sf::st_as_sf(tree.coord.df, coords = c("lon", "lat"), remove = FALSE)
  # assing CRS to points
  #sf::st_crs(tree.sf) <- my.utm.epsg
  
  # print(plot(my.inter$geometry), 
  #      plot(my.rem.circle$geometry, add = T), 
  #      plot(tree.sf$geometry, add = T)
  #     )
  
  inter.tree.circle <- sf::st_intersection(tree.sf, my.rem.circle)
  inter.tree.edge <- sf::st_intersection(tree.sf, my.inter)
  
  tree_status <- ifelse(nrow(inter.tree.edge)!= 0, my.inter$stand, 
                        ifelse(nrow(inter.tree.circle) != 0,  my.rem.circle$stand,
                               "warning"))
  
  tree.status.list.nogeo[[i]] <- as.data.frame(cbind(
    "plot_ID" = c(as.integer(my.plot.id)), 
    "tree_ID" = c(as.integer(my.tree.id)),
    "inv" = c(my.inv),
    "lon" = c(as.numeric(tree.coord.df$lon)),
    "lat" = c(as.numeric(tree.coord.df$lat)),
    "t_stat" = c(tree_status))) 
  
  # export tree points as sf
  tree.points.list.nogeo[[i]] <- c("t_stat" = tree_status, tree.sf)
  
}

# save tree corodiantes and status into dataframe
tree.status.list.one.edge.final.nogeo <- rbindlist(tree.status.list.nogeo)
tree.status.one.edge.df.nogeo <- as.data.frame(tree.status.list.one.edge.final.nogeo)
# save tree sf into dataframe
tree.points.list.one.edge.final.nogeo <- rbindlist(tree.points.list.nogeo)
tree.points.one.edge.df.nogeo <- as.data.frame(tree.points.list.one.edge.final.nogeo)




# 3.2.2.2. plots with 2 edges: sorting trees into edge and remaining circle polygones ---------
# intersection of trees with 2 edges
trees.two.edges.nogeo <- HBI_trees %>%
  # filter only for trees that are located in plots with a forest edge
  semi_join(forest_edges_HBI.man %>% filter(e_form == 1 | e_form == 2) %>% 
              #& inter_status_AT_17 == "two I" | e_form == 2 & inter_status_BT_17 == "two I") %>% 
              select(plot_ID) %>% distinct(), by = "plot_ID") %>% 
  # filter for trees located in plots htat haev only one forest edge
  semi_join(forest_edges_HBI.man %>% filter(e_form == 1 | e_form == 2 & inter_status_AT_17 == "two I" | e_form == 2 & inter_status_BT_17 == "two I") %>% 
              group_by(plot_ID) %>% summarise(n = n()) %>% filter(n > 1) %>% select(plot_ID), by = "plot_ID") #%>% 
# remove plots that do now have a corresponding center coordiante in the HBI loc document
# semi_join(HBI_loc %>% filter(!is.na( RW_MED) & !is.na(HW_MED)) %>%  select(plot_ID)  %>% distinct(), by = "plot_ID")

tree.status.two.edges.list.nogeo <- vector("list", length = length(trees.two.edges.nogeo$tree_ID))
tree.points.two.edges.list.nogeo <- vector("list", length = length(trees.two.edges.nogeo$tree_ID))

for (i in 1:length(trees.two.edges.nogeo$tree_ID)){ 
  # i = 1
  # i = which(grepl(50122, (trees.two.edges.nogeo$plot_ID)))[2]
  
  # select plot ID accordint to positioin in the list
  my.plot.id <- trees.two.edges.nogeo[i, "plot_ID"] 
  my.tree.id <- trees.two.edges.nogeo[i, "tree_ID"]
  my.inv <- trees.two.edges.nogeo[i, "inv"]
  
  # select the remaining cirlce we want to intersect the tree with
  my.rem.circle <- sf::st_as_sf(rem.circle.two.edges.df.nogeo %>% filter(plot_ID == my.plot.id) %>% distinct())
  my.edges.df <- inter.poly.two.edges.df.nogeo %>% filter(plot_ID == my.plot.id) %>% distinct() %>% arrange(e_ID)
  my.inter.1 <- sf::st_as_sf(my.edges.df[1,])
  my.inter.2 <- sf::st_as_sf(my.edges.df[2,])
  
  # assign stand category to the polygones depending on which one is bigger/ smaller: 
  # bigger polygone/ polygone with greater area is assigned to category A, smaller area polygone is assigned to B
  area.plot.df <- edges.area.two.edges.df.nogeo %>% filter(plot_ID == my.plot.id & CCS_r_m == 17.84) %>% 
    arrange(as.numeric(area_m2)) %>% 
    mutate(stand = case_when(
      row_number()== 1 ~ "C",
      row_number()== 2 ~ "B",
      row_number()== 3 ~ "A",
      TRUE ~ NA))
  # assign stand category to the polygones depending on which one is bigger/ smaller
  my.rem.circle$stand <- area.plot.df$stand[area.plot.df$e_ID == 0]
  my.inter.1$stand <- area.plot.df$stand[area.plot.df$e_ID == 1]
  my.inter.2$stand <- area.plot.df$stand[area.plot.df$e_ID == 2]
  
  # # assign crs
  # my.utm.epsg <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"
  # # select UTM corrdinates of the plot center
  # my.center.easting <- HBI_loc[HBI_loc$plot_ID == my.plot.id, "RW_MED"]
  # my.center.northing <- HBI_loc[HBI_loc$plot_ID == my.plot.id, "HW_MED"]
  
  # extract polar coordiantes of forest edge
  # point A 
  dist.tree <- trees.two.edges.nogeo[i, "Dist_cm"]/100 
  azi.tree <- trees.two.edges.nogeo[i, "azi_gon"] 
  x.tree <- dist.tree*sin(azi.tree)   # longitude, easting, RW, X
  y.tree <- dist.tree*cos(azi.tree)   # latitude, northing, HW, y 
  
  # transform polar into cartesian coordiantes
  tree.east <- x.tree  # + my.center.easting 
  tree.north <- y.tree # + my.center.northing
  
  # save cartesian coordiantes in dataframe
  tree.coord.df <- as.data.frame(cbind(
    "plot_ID" = c(as.integer(my.plot.id)), 
    "tree_ID" = c(as.integer(my.tree.id)),
    "inv" = c(my.inv),
    "lon" = c(tree.east),
    "lat" = c(tree.north)
  ))
  
  # create sf point object from dataframe
  #https://stackoverflow.com/questions/52551016/creating-sf-points-from-multiple-lat-longs
  tree.sf <-  sf::st_as_sf(tree.coord.df, coords = c("lon", "lat"), remove = FALSE)
  # assing CRS to points
  #sf::st_crs(tree.sf) <- my.utm.epsg
  
  # print(plot(my.rem.circle$geometry, col = "red"), 
  #       plot(my.inter.2$geometry, add = T),
  #       plot(my.inter.1$geometry, add = T), 
  #       plot(tree.sf$geometry, add = T)
  #       )
  
  inter.tree.circle <- sf::st_intersection(tree.sf, my.rem.circle)
  inter.tree.edge.1 <- sf::st_intersection(tree.sf, my.inter.1)
  inter.tree.edge.2 <- sf::st_intersection(tree.sf, my.inter.2)
  
  tree_status <- ifelse(nrow(inter.tree.edge.1)!= 0 & nrow(inter.tree.edge.2)== 0 & nrow(inter.tree.circle)== 0,  my.inter.1$stand,                     # if tree is in edge 1
                        ifelse(nrow(inter.tree.edge.2)!= 0 & nrow(inter.tree.edge.1)== 0 & nrow(inter.tree.circle)== 0,  my.inter.2$stand,              # if tree is in edge 2
                               ifelse(nrow(inter.tree.circle)!= 0 & nrow(inter.tree.edge.1)== 0 & nrow(inter.tree.edge.2)== 0,  my.rem.circle$stand,    # if tree is in circle
                                      #ifelse(nrow(inter.tree.circle)== 0 & nrow(inter.tree.edge.1)!= 0 & nrow(inter.tree.edge.2)!= 0,  "warning",       # if tree is in two edges
                                      "warning")))                                                                                             # if tree is nowhere
  
  tree.status.two.edges.list.nogeo[[i]] <- as.data.frame(cbind(
    "plot_ID" = c(as.integer(my.plot.id)), 
    "tree_ID" = c(as.integer(my.tree.id)),
    "inv" = c(my.inv),
    "lon" = c(as.numeric(tree.coord.df$lon)),
    "lat" = c(as.numeric(tree.coord.df$lat)),
    "t_stat" = c(tree_status))) 
  
  tree.points.two.edges.list.nogeo[[i]] <- c("t_stat" = tree_status, tree.sf)
  
  
}
# save tree corodiantes and status into dataframe
tree.status.list.two.edges.final.nogeo <- rbindlist(tree.status.two.edges.list.nogeo)
tree.status.two.edges.df.nogeo <- as.data.frame(tree.status.list.two.edges.final.nogeo)
# save tree sf into dataframe
tree.points.list.two.edges.final.nogeo <- rbindlist(tree.points.two.edges.list.nogeo)
tree.points.two.edges.df.nogeo <- as.data.frame(tree.points.list.two.edges.final.nogeo)
# bind the tree point datafarmes of one and two edges plots together
two.and.one.edge.trees.points.df.nogeo <- rbind(tree.points.one.edge.df.nogeo,tree.points.two.edges.df.nogeo) %>% mutate(plot_ID = as.integer(plot_ID)) 




# 3.2.2.3 plots with no edge edge: sorting trees into circle ---------
trees.no.edge.nogeo <- anti_join(HBI_trees, two.and.one.edge.trees.points.df.nogeo %>% select(plot_ID) %>% distinct(), by = "plot_ID")
tree.status.no.edge.list.nogeo <- vector("list", length = length(trees.no.edge.nogeo$tree_ID))
tree.points.no.edge.list.nogeo <- vector("list", length = length(trees.no.edge.nogeo$tree_ID))
for (i in 1:length(trees.no.edge.nogeo$tree_ID)){ 
  #i =1
  #i = which(grepl(50080, unique(trees.one.edge$plot_ID)))
  
  # select plot ID accordint to positioin in the list
  my.plot.id <- trees.no.edge.nogeo[i, "plot_ID"] 
  my.tree.id <- trees.no.edge.nogeo[i, "tree_ID"]
  my.inv <- trees.two.edges.nogeo[i, "inv"]
  
  # extract polar coordiantes of forest edge
  # point A 
  dist.tree <- trees.no.edge.nogeo[i, "Dist_cm"]/100 
  azi.tree <- trees.no.edge.nogeo[i, "azi_gon"] 
  x.tree <- dist.tree*sin(azi.tree)   # longitude, easting, RW, X
  y.tree <- dist.tree*cos(azi.tree)   # latitude, northing, HW, y 
  
  # transform polar into cartesian coordiantes
  tree.east <- x.tree  # + my.center.easting 
  tree.north <- y.tree # + my.center.northing
  
  # save cartesian coordiantes in dataframe
  tree.coord.df <- as.data.frame(cbind(
    "plot_ID" = c(as.integer(my.plot.id)), 
    "tree_ID" = c(as.integer(my.tree.id)),
    "inv" = c(my.inv),
    "lon" = c(as.numeric(tree.east)),
    "lat" = c(as.numeric(tree.north))
  ))
  
  # create sf point object from dataframe
  #https://stackoverflow.com/questions/52551016/creating-sf-points-from-multiple-lat-longs
  tree.sf <-  sf::st_as_sf(tree.coord.df, coords = c("lon", "lat"), remove = FALSE)
  # assing CRS to points
  #sf::st_crs(tree.sf) <- my.utm.epsg
  
  # select the UTM coordiantes of the center of the cirlce corresponding with the plot ID
  # my.center.easting <- HBI_loc[HBI_loc$plot_ID == my.plot.id, "RW_MED"]
  # my.center.northing <- HBI_loc[HBI_loc$plot_ID == my.plot.id, "HW_MED"]
  
  #### build circle
  # circle data
  c.x0 = 0 
  c.y0 = 0
  c.r3 = 17.84
  c.r2 = 12.62
  c.r1 = 5.64
  # build polygon (circlular buffer) around center point
  center.df<- as.data.frame(cbind("lon" = c.x0, "lat" = c.y0))
  # center.df <- as.data.frame(cbind("lon" = my.center.easting, "lat" = my.center.northing))
  circle.pt <- sf::st_as_sf(center.df, coords = c("lon", "lat"))
  circle.17 <- sf::st_buffer(circle.pt, c.r3)
  circle.12 <- sf::st_buffer(circle.pt, c.r2)
  circle.5 <- sf::st_buffer(circle.pt, c.r1)
  
  inter.tree.circle.17 <- sf::st_intersection(tree.sf, circle.17)
  
  # if a tree is not intersecting with the circle or its exactly at the edge of the cirlce the inter.tree.circle.17 will be empty, 
  # however, trees that are exactly 17.84 meters apart from the circle center would still be part of the plot, tho the polygones won´t detect and intersection
  # which is why trees only receive the status "warning" if they are acturally situated outside of the circle
  tree_status <- ifelse(nrow(inter.tree.circle.17) == 0 & dist.tree > 17.84,  "warning", "A")                                                                                            # if tree is nowhere
  
  tree.status.no.edge.list.nogeo[[i]] <- as.data.frame(cbind(
    "plot_ID" = c(as.integer(my.plot.id)), 
    "tree_ID" = c(as.integer(my.tree.id)),
    "inv" = c(my.inv),
    "lon" = c(as.numeric(tree.coord.df$lon)),
    "lat" = c(as.numeric(tree.coord.df$lat)),
    "t_stat" = c(tree_status))
    )
  
  tree.points.no.edge.list.nogeo[[i]] <- c("t_stat" = tree_status, tree.sf)
  
  
}

# save tree corodiantes and status into dataframe
tree.status.no.edges.final.nogeo <- rbindlist(tree.status.no.edge.list.nogeo)
tree.status.no.edges.df.nogeo <- as.data.frame(tree.status.no.edges.final.nogeo)
# save tree sf into dataframe
tree.points.list.no.edges.final.nogeo <- rbindlist(tree.points.no.edge.list.nogeo)
tree.points.no.edges.df.nogeo <- as.data.frame(tree.points.list.no.edges.final.nogeo)


# bind all tree point.sf dataframes (with & without edges together)
all.trees.points.df.nogeo <- 
  rbind(two.and.one.edge.trees.points.df.nogeo , 
        tree.points.no.edges.df.nogeo) %>% 
  mutate(across(plot_ID:tree_ID, ~ as.integer(.x))) %>% 
  left_join(., trees_and_edges %>% 
              select(plot_ID, tree_ID, DBH_cm), 
            by = c("plot_ID", "tree_ID"), 
            multiple = "all")


# bind all tree status dataframes together (one edge, two edges, no edge plots)
all.trees.status.df <- 
  rbind(tree.status.no.edges.df.nogeo, 
        tree.status.one.edge.df.nogeo, 
        tree.status.two.edges.df.nogeo)


# 3.3. data export ---------------------------------------------------------------------------------------------------------
# 3.3.1. data prep for export -----------------------------------------------------------------------------------------------
# 3.3.1.1. harmonzing strings for join --------------------------------------------------------
# harmonize strings of all.trees.status.df and   
# https://stackoverflow.com/questions/20637360/convert-all-data-frame-character-columns-to-factors
all.trees.status.df[,c(1,2, 4, 5)] <- lapply(all.trees.status.df[,c(1,2, 4, 5)], as.numeric)
all.edges.area.df.nogeo[,c(1,2, 3, 5)] <- lapply(all.edges.area.df.nogeo[,c(1,2, 3, 5)], as.numeric) 

# 3.3.1.2. join tree stand status and plot areas into trees dataset  --------------------------------------------------------
HBI_trees_update_1 <- HBI_trees %>%  
 # join in stand of each tree
  left_join(., all.trees.status.df %>% 
              select(plot_ID, tree_ID, inv, t_stat) %>% 
              distinct(),
            by = c("plot_ID", "tree_ID", "inv")) %>% 
  rename(stand = t_stat) %>% 
  # then join in plot area the tree reffers to due to it´s DBH which determines the sampling circuit it was found in 
  # asssing corect samling circle diameter according to DBH of the tree to be able to join in the right plot area
  mutate(CCS_r_m = case_when(DBH_cm >= 7  & DBH_cm < 10 ~ 5.64, 
                             DBH_cm >= 10 & DBH_cm < 30 ~ 12.62, 
                             DBH_cm >= 30 ~ 17.84, 
                             TRUE ~ NA)) %>% 
  # join in the area that belongs to the tree according to the CCS the tree was measured in/ belongs to
  left_join(., all.edges.area.df.nogeo %>% 
              select(plot_ID, inter_stat, CCS_r_m, stand, area_m2),
             by = c("plot_ID", "CCS_r_m", "stand")) %>% 
  # if there was no plot area claualted due to the fact that there is no edger at the plot, 
  # we calcualte the area from the sampling circuit diameter assign under CCD_r_m
  mutate(area_m2 = ifelse(is.na(e_ID) & is.na(area_m2) |
                            # for trees alloceted to a in a cirlce without intersections wil not run throuhg the loops
                            # thus they do  have an edge ID but no calcualted areas or assigned intersection status
                            # therefore we have to calculate their area manually subsequently
                            # trees with the status "warning" will not have any stand and area from the dataset "all.edges.area.df.nogeo" assigned
                            # as this stand category doesn´t exist
                            # trees with the status "warning" will be excluded from the analysis
                            stand == "A" & inter_stat != "partly intersecting" & is.na(area_m2) | 
                            stand == "A" & is.na(inter_stat) & is.na(area_m2), c_A(CCS_r_m), area_m2), 
         plot_A_ha = as.numeric(area_m2)/10000)  # dividedd by 10 000 to transform m2 into hectar



# 3.3.1.3. sort trees into remove and process on datasets by status "warning" --------------------------------------------------------
HBI_trees_removed_1 <- HBI_trees_update_1 %>% filter(stand == "warning")
HBI_trees_update_1 <- HBI_trees_update_1 %>% filter(stand != "warning")

# 3.3.1.4.  binding datasets together ----------------------------------------------------------
all.triangle.polys.df.nogeo <- rbind(triangle.e1.poly.df.nogeo, triangle.e2.poly.df.nogeo)
all.edge.intersections.poly  <- rbind(inter.poly.one.edge.df.nogeo, inter.poly.two.edges.df.nogeo)
all.remaning.circles.poly <- rbind(rem.circle.one.edge.df.nogeo, rem.circle.two.edges.df.nogeo)


# 3.3.2. exporting data ---------------------------------------------------

# exporting tree and edge/ plot area data
write.csv(HBI_trees_update_1, paste0(out.path.BZE3, paste(unique(HBI_trees_update_1$inv)[1], "trees_update_1", sep = "_"), ".csv"))
write.csv(HBI_trees_removed_1, paste0(out.path.BZE3, paste(unique(HBI_trees_removed_1$inv)[1], "trees_removed_1", sep = "_"), ".csv"))
write.csv(trees_and_edges,paste0(out.path.BZE3, paste(unique(HBI_trees_update_1$inv)[1], "LT_edges", sep = "_"), ".csv"))
          

# export tree stand status of all trees nomatter if they have one, two or no forest edges at their plot
write.csv(all.trees.status.df, paste0(out.path.BZE3, paste(unique(HBI_trees_update_1$inv)[1], "all_trees_stand", sep = "_"), ".csv"))
# export areas and stand info of all sampling circuits, edges and remaining circles
write.csv(all.edges.area.df.nogeo,  paste0(out.path.BZE3, paste(unique(HBI_trees_update_1$inv)[1], "all_edges_rem_circles", sep = "_"), ".csv"))
          

# export list of plots where the edge polygones intersect within the 17.84 radius
write.csv(intersection.two.edges.warning.df.nogeo,  paste0(out.path.BZE3, paste(unique(HBI_trees_update_1$inv)[1], "edges_intersecting_warning", sep = "_"), ".csv"))
          
# exporting edge triangle polygones
write.csv(all.triangle.polys.df.nogeo, paste0(out.path.BZE3, paste(unique(HBI_trees_update_1$inv)[1], "all_edges_triangle_poly", sep = "_"), ".csv"))
# exporting edge intersection polygones 
write.csv(all.edge.intersections.poly, paste0(out.path.BZE3, paste(unique(HBI_trees_update_1$inv)[1], "all_edges_intersection_poly", sep = "_"), ".csv"))
# exporting all remaining circles
write.csv(all.remaning.circles.poly, paste0(out.path.BZE3, paste(unique(HBI_trees_update_1$inv)[1], "all_edges_intersection_poly", sep = "_"), ".csv"))



# 3.4. visulaizing for all plots, edges, trees -------------------------
dev.off()
for(i in 1:(nrow(HBI_trees %>% select(plot_ID) %>% distinct()))){
  # https://ggplot2.tidyverse.org/reference/ggsf.html
  
  #i = 2
  # i = which(grepl(50004, unique(HBI_trees$plot_ID)))
  my.plot.id = unique(HBI_trees$plot_ID)[i]
  #print(my.plot.id)
  
  c.df <- as.data.frame(cbind("lon" = 0, "lat" = 0))
  c.pt <- sf::st_as_sf(c.df, coords = c("lon", "lat"))
  c.poly.17 <- sf::st_buffer(c.pt, 17.84)
  c.poly.12 <- sf::st_buffer(c.pt, 12.62)
  c.poly.5 <- sf::st_buffer(c.pt, 5.64)
  
  
  print(ggplot() +
          ggtitle(my.plot.id)+
          geom_sf(data = c.poly.17, aes(alpha = 0))+
          geom_sf(data = c.poly.12, aes(alpha = 0))+
          geom_sf(data = c.poly.5, aes(alpha = 0))+
          geom_sf(data = triangle.e1.poly.df.nogeo$geometry[triangle.e1.poly.df.nogeo$plot_ID == my.plot.id], aes(alpha = 0))+
          geom_sf(data = triangle.e2.poly.df.nogeo$geometry[triangle.e2.poly.df.nogeo$plot_ID == my.plot.id], aes(alpha = 0))+ 
          geom_sf(data = all.trees.points.df.nogeo$geometry[all.trees.points.df.nogeo$plot_ID == my.plot.id], 
                  aes(color = all.trees.points.df.nogeo$t_stat[all.trees.points.df.nogeo$plot_ID == my.plot.id], 
                      size =  all.trees.points.df.nogeo$DBH_cm[all.trees.points.df.nogeo$plot_ID == my.plot.id]))+
          guides(color=guide_legend(title="tree status"))+
          guides(size=guide_legend(title="DBH cm"))+
          geom_sf_text(data = all.trees.points.df.nogeo$geometry[all.trees.points.df.nogeo$plot_ID == my.plot.id], 
                       aes(label = all.trees.points.df.nogeo$tree_ID[all.trees.points.df.nogeo$plot_ID == my.plot.id]))+
          xlim(-30, 30)+
          ylim(-30, 30)
        
  )
  
}


 
 
 
 
