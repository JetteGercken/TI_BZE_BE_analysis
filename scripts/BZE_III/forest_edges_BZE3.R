# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the peat land soil inventory
# forest edges  

# ----- 0. SETUP ---------------------------------------------------------------

# ----- 0.1. packages and functions --------------------------------------------


source("C:/Users/gercken/Documents/TI_BZE_BE_analysis/scripts/functions_library.R")


# ----- 0.2. working directory -------------------------------------------------
here::here()
getwd()

out.path.BZE3 <- ("output/out_data/out_data_BZE/") 

# ----- 0.3 data import --------------------------------------------------------
# LIVING TREES
# BZE3 BE dataset: this dataset contains the inventory data of the tree inventory accompanying the third national soil inventory
HBI_trees <- read.delim(file = here("data/input/BZE2_HBI/beab.csv"), sep = ",", dec = ",")
# BZE3 locations dataset: this dataset contains the coordinates of the center point of the tree inventory accompanying the third national soil inventory
HBI_loc <- read.delim(file = here("data/input/BZE2_HBI/location_HBI.csv"), sep = ";", dec = ",")


# BZE3 BE dataset: this dataset contains the inventory data of the tree inventory accompanying the third national soil inventory
# BZE3_trees <- read.delim(file = here("data/input/BZE3/BZE3_trees_total.csv"), sep = ";", dec = ",")

SP_names_com_ID_tapeS <- read.delim(file = here("output/out_data/x_bart_tapeS.csv"), sep = ",", dec = ",") 

forest_edges_HBI <- read.delim(file = here("data/input/BZE2_HBI/be_waldraender.csv"), sep = ";", dec = ",")
# forest_edges_BZE3 <-    

# creating dataset with information about the concentric sampling circles
data_circle <- data.frame(x0 = c(0,0,0),       # x of centre point of all 3 circles is 0 
                          y0 = c(0,0,0),       # y of centre point of all 3 circles is 0 
                          r0 = c( (0 + 564 * cos(0)), (0 + 1262 * cos(0)), (0 + 1784 * cos(0))), 
                          rmax = c(3000, 3000, 3000)) # these are the radi of the sampling circuits 

# REGENERATION 


# DEADWOOD


# ----- 0.6 harmonising column names & structure  -------------------------
# HBI 
colnames(HBI_trees) <- c("multi_stem", "D_mm", "DBH_class", "DBH_h_cm", "H_dm",
                         "azi_gon", "SP_code", "tree_ID", "plot_ID", "tree_status", 
                         "DBH_cm", "age", "C_layer", "C_h_dm", "Kraft", "Dist_cm", "age_meth")  
HBI_trees <- HBI_trees %>% select(plot_ID,  tree_ID ,  tree_status ,  multi_stem ,
                                  Dist_cm ,  azi_gon ,age ,  age_meth ,  SP_code , DBH_class ,  Kraft ,  
                                  C_layer , H_dm ,  C_h_dm , D_mm ,   DBH_h_cm ,  DBH_cm )
HBI_loc <- HBI_loc %>% select("ï..ToTraktId", "ToEckId", "K2_RW",
                              "K2_HW", "K3_RW", "K3_HW", "RW_MED",
                              "HW_MED",  "LAT_MED",  "LON_MED", 
                              "LAT_MEAN", "LON_MEAN")
colnames(HBI_loc) <- c("plot_ID", "ToEckId", "K2_RW",
                       "K2_HW", "K3_RW", "K3_HW", "RW_MED",
                       "HW_MED",  "LAT_MED",  "LON_MED", 
                       "LAT_MEAN", "LON_MEAN") 



# BZE3

# Forest edges 
colnames(forest_edges_HBI) <- c("plot_ID", "e_ID", "e_type", "e_form", 
                                "A_dist", "A_azi",  "B_dist", "B_azi", 
                                "T_dist", "T_azi") # t = turning point 

# ----- 1. joining in external info  --------------------------------------
# ----- 1.1. LIVING TREES -------------------------------------------------
# ----- 1.1.1. species & inventory names ----------------------------------------------
# ----- 1.1.1.1. HBI species & inventory ----------------------------------------------
HBI_trees <- HBI_trees %>% 
  mutate(inventory = "HBI") %>% 
  left_join(SP_names_com_ID_tapeS %>% 
              mutate(char_code_ger_lowcase = tolower(Chr_code_ger)), 
            by = c("SP_code" = "char_code_ger_lowcase")) %>% 
  mutate(DBH_cm = ifelse(DBH_h_cm == 130, D_mm/10, (D_mm*(1.0+(0.0011*(DBH_h_cm -130))))/10))


# check if there are no trees left that don´t have a SP_code in xBart/ SP_names_com_ID_tapeS
HBI_trees %>% 
  anti_join(SP_names_com_ID_tapeS %>% 
              mutate(char_code_ger_lowcase = tolower(Chr_code_ger)), 
            by = c("SP_code" = "char_code_ger_lowcase"))


# ----- 1.1.1.2. BZE3 species & inventory names ----------------------------------------------
# BZE3_trees <- BZE3_trees %>% 
#   mutate(inventory = "BZE3") %>% 
#   left_join(., SP_names_com_ID_tapeS %>% 
#               mutate(char_code_ger_lowcase = tolower(Chr_code_ger)), 
#             by = c("SP_code" = "char_code_ger_lowcase"))
# 
# 
# # check if there are no trees left that don´t have a SP_code in xBart/ SP_names_com_ID_tapeS
# BZE3_trees %>% 
#   anti_join(., SP_names_com_ID_tapeS %>% 
#               mutate(char_code_ger_lowcase = tolower(Chr_code_ger)), 
#             by = c("SP_code" = "char_code_ger_lowcase"))



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
  mutate(Dist_m = Dist_cm/100, 
         X_tree = coord(data_circle$x0[1], data_circle$y0[1], Dist_m, azi_gon, coordinate = "x"), 
         Y_tree = coord(data_circle$x0[1], data_circle$y0[1], Dist_m, azi_gon, coordinate = "y")) %>% 
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
  # find line parameters
  # 1. calculate x and y coordinates for all edge points
  mutate(X_A = ifelse(A_azi != "-2", coord(data_circle$x0[1], data_circle$y0[1], A_dist, azi_gon, coordinate = "x"), NA), # if the value is marked -2 its equal to an NA
         X_B = ifelse(B_azi != "-2", x_coord(B_dist, B_azi), NA),
         X_T = ifelse(T_azi != "-2", x_coord(T_dist, T_azi), NA), 
         Y_A = ifelse(A_azi != "-2", y_coord(A_dist, A_azi), NA), 
         Y_B = ifelse(B_azi != "-2", y_coord(B_dist, B_azi), NA), 
         Y_T = ifelse(T_azi != "-2", y_coord(T_dist, T_azi), NA)) %>% 
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
  mutate(t_status_AB_ABT = tree.status(e_form,
                                       0, 0, data_circle$r0[3],
                                       b0_AB, b1_AB,
                                       X_tree, Y_tree,
                                       X_A, Y_A, X_T, Y_T, b0_AT, b1_AT,
                                       data_circle$rmax[3]*2,
                                       X_B, Y_B,  b0_BT, b1_BT)) %>% 
  mutate(DBH_cm = ifelse(DBH_h_cm != 130, (D_mm*(1.0+(0.0011*(DBH_h_cm-130))))/10, D_mm/10)) %>% 
  # ---- 1.1.2.4. assigning plot area by according to diameter class (klubschwelle)  ---------------------------------------
mutate(edge_A_method = edge.A.method(e_form, DBH_cm,  X_A, X_B, X_T, Y_A, Y_B, Y_T, T_dist, t_status_AB_ABT), 
       plot_A =  edge.A(e_form, DBH_cm,  X_A, X_B, X_T, Y_A, Y_B, Y_T, T_dist, t_status_AB_ABT))



# ----- 1.1.2.6. exporting tree & edge data & edge areas -------------------------------------------------------------------
write.csv(trees_and_edges, paste0(out.path.BZE3,"LT_edges_HBI.csv"))



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
             aes(X_tree, Y_tree, colour = edge_A_method))+
  theme_bw()+ 
  facet_wrap(~plot_ID)  



# for plot 50080 there are two many edges measured
# for plot 50102 there is no intersection between the line BT and the 60m radius but I don´t know why. 

# ----- 2.1.3. visulaliszing forest edge_form 1 and edge_form 2 together using m_s_status --------
# plotting trees and interception lines divided in t_line_status
#AB line
ggplot() +  
  geom_circle(data = data_circle, aes(x0 = x0, y0 = y0, r = r0))+ # Draw ggplot2 plot with circle representing sampling circuits 
  geom_circle(data = data_circle, aes(x0 = x0, y0 = y0, r = rmax*2))+ # Draw ggplot2 plot with circle representing sampling circuits
  ### AB line
  geom_point(data = trees_and_edges %>%
               filter(e_form == "1") %>% 
               # inner_join(.,   forest_edges_HBI.man %>% 
               #              filter(e_form == "1") %>% 
               #              group_by(plot_ID) %>% 
               #              summarize(n = n()) %>% 
               #              filter(n <= 1), 
               #            by = "plot_ID") %>% 
               select(plot_ID, X1_inter_AB_17, X2_inter_AB_17, X_A, X_B, Y1_inter_AB_17, Y2_inter_AB_17, Y_A, Y_B) %>% 
               to_long(keys = c("X_name",  "Y_name"),
                       values = c( "X_value", "Y_value"),  
                       names(.)[2:5], names(.)[6:9]), 
             aes(x= X_value, y = Y_value, colour = X_name))+
  geom_line(data = trees_and_edges %>% 
              filter(e_form == "1") %>% 
              # inner_join(.,   forest_edges_HBI.man %>% 
              #              filter(e_form == "1") %>% 
              #              group_by(plot_ID) %>% 
              #              summarize(n = n()) %>% 
              #              filter(n <= 1), 
              #            by = "plot_ID") %>% 
              select(plot_ID, X_A, X_B, Y_A, Y_B) %>% 
              to_long(keys = c("X_name",  "Y_name"),
                      values = c( "X_value", "Y_value"),  
                      names(.)[2:3], names(.)[4:5]), 
            aes(x= X_value, y = Y_value))+
  geom_line(data = trees_and_edges %>% 
              filter(e_form == "1") %>% 
              # inner_join(.,   forest_edges_HBI.man %>% 
              #              filter(e_form == "1") %>% 
              #              group_by(plot_ID) %>% 
              #              summarize(n = n()) %>% 
              #              filter(n <= 1), 
              #            by = "plot_ID") %>% 
              select(plot_ID, X1_inter_AB_17, X_A, Y1_inter_AB_17, Y_A) %>% 
              to_long(keys = c("X_name",  "Y_name"),
                      values = c( "X_value", "Y_value"),
                      names(.)[2:3], names(.)[4:5]),  
            aes(x= X_value, y = Y_value, colour = X_name))+
  geom_line(data = trees_and_edges %>% 
              filter(e_form == "1") %>% 
              # inner_join(.,   forest_edges_HBI.man %>% 
              #              filter(e_form == "1") %>% 
              #              group_by(plot_ID) %>% 
              #              summarize(n = n()) %>% 
              #              filter(n <= 1), 
              #            by = "plot_ID") %>% 
              select(plot_ID, X2_inter_AB_17, X_A, Y2_inter_AB_17, Y_A) %>% 
              to_long(keys = c("X_name",  "Y_name"),
                      values = c( "X_value", "Y_value"),
                      names(.)[2:3], names(.)[4:5]),  
            aes(x= X_value, y = Y_value, colour = X_name))+
  geom_line(data = trees_and_edges %>% 
              filter(e_form == "1") %>% 
              # inner_join(.,   forest_edges_HBI.man %>% 
              #              filter(e_form == "1") %>% 
              #              group_by(plot_ID) %>% 
              #              summarize(n = n()) %>% 
              #              filter(n <= 1), 
              #            by = "plot_ID") %>% 
              select(plot_ID, X1_inter_AB_17, X_B, Y1_inter_AB_17, Y_B) %>% 
              to_long(keys = c("X_name",  "Y_name"),
                      values = c( "X_value", "Y_value"),
                      names(.)[2:3], names(.)[4:5]),  
            aes(x= X_value, y = Y_value, colour = X_name))+
  geom_line(data = trees_and_edges %>% 
              filter(e_form == "1") %>% 
              # inner_join(.,   forest_edges_HBI.man %>% 
              #              filter(e_form == "1") %>% 
              #              group_by(plot_ID) %>% 
              #              summarize(n = n()) %>% 
              #              filter(n <= 1), 
              #            by = "plot_ID") %>% 
              select(plot_ID, X2_inter_AB_17, X_B, Y2_inter_AB_17, Y_B) %>% 
              to_long(keys = c("X_name",  "Y_name"),
                      values = c( "X_value", "Y_value"),
                      names(.)[2:3], names(.)[4:5]),  
            aes(x= X_value, y = Y_value, colour = X_name))+
  # trees
  geom_point(data =  trees_and_edges %>% filter(e_form == "1"), #%>% 
             #              inner_join(.,   forest_edges_HBI.man %>% 
             #                           filter(e_form == "1") %>% 
             #                           group_by(plot_ID) %>% 
             #                           summarize(n = n()) %>% 
             #                           filter(n <= 1), 
             #                         by = "plot_ID"),
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



# 2.2.1. Visualising areas ------------------------------------------------

inter.line.circle.geometrical <- function(x.1, y.1, x.2, y.2, c.x0, c.y0, c.r0, coordinate){
  betrag.dist.minus.radius <- ifelse(((distance(c.y0, c.x0, y.1 , x.1)/100)-c.r0)<0, ((distance(c.y0, c.x0, y.1 , x.1)/100)-c.r0)*(-1), (distance(c.y0, c.x0, y.1 , x.1)/100)-c.r0);
  switch(coordinate, 
         x1 = coord(y.1, x.1, betrag.dist.minus.radius, azi(x.2, y.2, x.1, y.1), coordinate = "x"),
         y1 = coord(y.1, x.1, betrag.dist.minus.radius, azi(x.2, y.2, x.1, y.1), coordinate = "y"),
         x2 = coord(y.1, x.1, betrag.dist.minus.radius+c.r0*2, azi(x.2, y.2, x.1, y.1), coordinate = "x"),
         y2 = coord(y.1, x.1, betrag.dist.minus.radius+c.r0*2, azi(x.2, y.2, x.1, y.1), coordinate = "y"))
}


east.north.coord <- function(dist, azi, c.x, c.y, coordinate){
  # from Niko Knapps easting northing script
  # # Convert polar to cartesian coordinates
  # my.mp.x <- my.mp.distanz * sin(my.mp.azimut * pi/200)
  # my.mp.y <- my.mp.distanz * cos(my.mp.azimut * pi/200)
  # 
  # # Add the cartesian coordinates to the UTM coordinates of the magp
  # my.easting <- my.magp.easting + my.mp.x
  # my.northing <- my.magp.northing + my.mp.y
  
  
  switch(coordinate,
         # Convert polar to cartesian coordinates and add easting northing of central point
         lat = c.x + dist*sin(azi), 
         lon = c.y + dist*cos(azi))
  }

east.north.azi <- function(east.1, north.1, east.2, north.2){
  azi = atan((east.2 - east.1)/(north.2-north.1));
  delta.e = east.2-east.1;
  delta.n = north.2-north.1;
  azi.corrected = ifelse(delta.e >= 0 & delta.n > 0 | delta.e > 0 & delta.n >= 0, azi,                    # first quadrant x + y+
                         ifelse(delta.e >= 0 & delta.n < 0 |delta.e > 0 & delta.n <= 0, azi+200,         # second quadrant x + y-
                                ifelse(delta.e <= 0 & delta.n < 0 |delta.e < 0 & delta.n <= 0,  azi+200,   # third quadrant x- y-
                                       ifelse(delta.e <= 0 & delta.n > 0 | delta.e < 0 & delta.n >= 0, azi+400, NA))));
  return(azi.corrected)
}

east.north.dist <- function(east.1, north.1, east.2, north.2){
  d = sqrt(((east.2-east.1)^2)+(north.2-north.1)^2);
  return(d)
}

FE_loc_HBI.test <- forest_edges_HBI.man %>% 
  filter(plot_ID == 50005) %>% 
  left_join(HBI_loc %>% 
              filter(K3_HW >0), by = "plot_ID") %>% 
        # Convert polar to cartesian coordinates
  mutate(A_east = RW_MED + X_A,
         A_north = HW_MED + Y_A,
         B_east = RW_MED+ X_B,
         B_north = HW_MED+Y_B) %>% 
  mutate(AB_east_inter_1 = RW_MED + X1_inter_AB_17,
         AB_north_inter_1 = HW_MED + Y1_inter_AB_17,
         AB_east_inter_2 = RW_MED + X2_inter_AB_17,
         AB_north_inter_2 = HW_MED + Y2_inter_AB_17,) %>% 
  # mutate(X_A_GPS = coord(HW_MED, RW_MED,  A_dist/100, A_azi, coordinate = "x"), 
  #        X_B_GPS = coord(HW_MED, RW_MED, B_dist/100, B_azi, coordinate = "x"), 
  #        Y_A_GPS = coord(HW_MED, RW_MED, A_dist/100, A_azi, coordinate = "y"), 
  #        Y_B_GPS = coord(HW_MED, RW_MED, B_dist/100, B_azi, coordinate = "y"),
  #        # https://stackoverflow.com/questions/6091728/line-segment-circle-intersection
  #        AB_x1_inter_17 = coord(Y_A_GPS, X_A_GPS,  (distance(HW_MED, RW_MED, Y_A_GPS, X_A_GPS)-17.84), azi(X_B_GPS, Y_B_GPS, X_A_GPS, Y_A_GPS), coordinate = "x"), 
  #        AB_y1_inter_17 = coord(Y_A_GPS, X_A_GPS,  (distance(HW_MED, RW_MED, Y_A_GPS, X_A_GPS)-17.84), azi(X_B_GPS, Y_B_GPS, X_A_GPS, Y_A_GPS), coordinate = "y"),
  #        AB_x2_inter_17 = coord(Y_A_GPS, X_A_GPS,  (distance(HW_MED, RW_MED, Y_A_GPS, X_A_GPS)-17.84+17.84*2), azi(X_B_GPS, Y_B_GPS, X_A_GPS, Y_A_GPS), coordinate = "x"),
  #        AB_y2_inter_17 = coord(Y_A_GPS, X_A_GPS,  (distance(HW_MED, RW_MED, Y_A_GPS, X_A_GPS)-17.84+17.84*2), azi(X_B_GPS, Y_B_GPS, X_A_GPS, Y_A_GPS), coordinate = "y")) %>% 
  # mutate(X_D_GPS = coord(AB_y1_inter_17, AB_x1_inter_17, 17.84*2, 100, coordinate = "x"), 
  #        Y_D_GPS = coord(AB_y1_inter_17, AB_x1_inter_17, 17.84*2, 100, coordinate = "y"),
  #        X_E_GPS = coord(AB_y2_inter_17, AB_x2_inter_17, 17.84*2, 100, coordinate = "x"), 
  #        Y_E_GPS = coord(AB_y2_inter_17, AB_x2_inter_17, 17.84*2, 100, coordinate = "y"), 
  #        end_coord_x = X_D_GPS, 
  #        end_coord_y = Y_D_GPS) %>% 
   mutate(D_east = east.north.coord(1784*2, 100, AB_east_inter_1, AB_north_inter_1, coordinate = "lat"),
          D_north = east.north.coord(1784*2, 100,AB_east_inter_1, AB_north_inter_1,  coordinate = "lon"),
          E_east = east.north.coord(1784*2, 100, AB_east_inter_2, AB_north_inter_2, coordinate = "lat"),
          E_north = east.north.coord(1784*2, 100,AB_east_inter_2, AB_north_inter_2,  coordinate = "lon"), 
          end_east = AB_east_inter_1, 
          end_north = AB_north_inter_1) %>% 
 # mutate(b0_AB_GPS = intercept(X_A_GPS,Y_A_GPS, X_B_GPS, Y_B_GPS ), 
  #       b1_AB_GPS = slope(X_A_GPS,Y_A_GPS, X_B_GPS, Y_B_GPS )) %>% 
  # select(plot_ID, b0_AB_GPS, b1_AB_GPS, 
  #        A_dist, A_azi, B_dist, B_azi,
  #        RW_MED , X_A_GPS, X_B_GPS, X_D_GPS, X_E_GPS, AB_x1_inter_17, AB_x2_inter_17, end_coord_x, 
  #        HW_MED,  Y_A_GPS, Y_B_GPS, Y_D_GPS, Y_E_GPS, AB_y1_inter_17, AB_y2_inter_17, end_coord_y)  %>%
  select(plot_ID,  
         A_dist, A_azi, B_dist, B_azi,
         RW_MED, A_east, B_east, D_east, E_east, AB_east_inter_1, AB_east_inter_2, end_east, 
         HW_MED, A_north, B_north, D_north, E_north, AB_north_inter_1, AB_north_inter_2, end_north) %>%  
    to_long(keys = c("X_name",  "Y_name"),
            values = c( "lat", "lon"),  
            names(.)[6:13], names(.)[14:21]) %>% 
  mutate(order = case_when(X_name == "AB_east_inter_1" ~ 1,
                           X_name == "D_east" ~ 2, 
                           X_name == "E_east" ~ 3,
                           X_name == "AB_east_inter_2" ~ 4,
                           X_name == "end_east" ~ 5, 
                           TRUE ~ NA)) %>% 
  arrange(plot_ID, order)


plot(FE_loc_HBI.test$lat, FE_loc_HBI.test$lon)

# ggplot() +  
#   geom_circle(data = FE_loc_HBI.test %>% filter(X_name == "RW_MED" & Y_name == "HW_MED"), aes(x0 = x, y0 = y, r = 17.84))+ # Draw ggplot2 plot with circle representing sampling circuits 
#   #geom_circle(data = data_circle, aes(x0 = x0, y0 = y0, r = rmax*2))+ # Draw ggplot2 plot with circle representing sampling circuits
#   geom_point(data = FE_loc_HBI.test,
#              aes(x= x, y = y, colour = X_name))+
#   geom_segment(data =FE_loc_HBI.test, 
#                aes(x = FE_loc_HBI.test$x[FE_loc_HBI.test$X_name == "AB_x1_inter_17"], 
#                    y = FE_loc_HBI.test$y[FE_loc_HBI.test$X_name == "AB_x1_inter_17"], 
#                    xend = FE_loc_HBI.test$x[FE_loc_HBI.test$X_name == "AB_x2_inter_17"], 
#                    yend = FE_loc_HBI.test$y[FE_loc_HBI.test$X_name == "AB_x2_inter_17"], 
#                    colour = "segment")) +
#  geom_segment(data =FE_loc_HBI.test, 
#               aes(x = FE_loc_HBI.test$x[FE_loc_HBI.test$X_name == "X_D_GPS"], 
#                   y = FE_loc_HBI.test$y[FE_loc_HBI.test$X_name == "X_D_GPS"], 
#                   xend = FE_loc_HBI.test$x[FE_loc_HBI.test$X_name == "X_E_GPS"], 
#                   yend = FE_loc_HBI.test$y[FE_loc_HBI.test$X_name == "X_E_GPS"], 
#                   colour = "segment"))+
#   geom_segment(data =FE_loc_HBI.test, 
#                aes(x = FE_loc_HBI.test$x[FE_loc_HBI.test$X_name == "X_D_GPS"], 
#                    y = FE_loc_HBI.test$y[FE_loc_HBI.test$X_name == "X_D_GPS"], 
#                    xend = FE_loc_HBI.test$x[FE_loc_HBI.test$X_name == "X_E_GPS"], 
#                    yend = FE_loc_HBI.test$y[FE_loc_HBI.test$X_name == "X_E_GPS"], 
#                    colour = "segment"))
ggplot() +  
  geom_circle(data = FE_loc_HBI.test %>% filter(X_name == "RW_MED" & Y_name == "HW_MED"), aes(x0 = lat, y0 = lon, r = 3000))+ # Draw ggplot2 plot with circle representing sampling circuits 
  geom_circle(data = FE_loc_HBI.test %>% filter(X_name == "RW_MED" & Y_name == "HW_MED"), aes(x0 = lat, y0 = lon, r = 1784))+
  geom_point(data = FE_loc_HBI.test,
             aes(x= lat, y = lon, colour = X_name))+
  geom_segment(data =FE_loc_HBI.test, 
               aes(x = FE_loc_HBI.test$lat[FE_loc_HBI.test$X_name == "AB_east_inter_1"], 
                   y = FE_loc_HBI.test$lon[FE_loc_HBI.test$X_name == "AB_east_inter_1"], 
                   xend = FE_loc_HBI.test$lat[FE_loc_HBI.test$X_name == "AB_east_inter_2"], 
                   yend = FE_loc_HBI.test$lon[FE_loc_HBI.test$X_name == "AB_east_inter_2"], 
                   colour = "segment")) +
  geom_segment(data =FE_loc_HBI.test, 
               aes(x = FE_loc_HBI.test$lat[FE_loc_HBI.test$X_name == "D_east"], 
                   y = FE_loc_HBI.test$lon[FE_loc_HBI.test$X_name == "D_east"], 
                   xend = FE_loc_HBI.test$lat[FE_loc_HBI.test$X_name == "E_east"], 
                   yend = FE_loc_HBI.test$lon[FE_loc_HBI.test$X_name == "E_east"], 
                   colour = "segment"))+
  geom_segment(data =FE_loc_HBI.test, 
               aes(x = FE_loc_HBI.test$lat[FE_loc_HBI.test$X_name == "AB_east_inter_1"], 
                   y = FE_loc_HBI.test$lon[FE_loc_HBI.test$X_name == "AB_east_inter_1"], 
                   xend = FE_loc_HBI.test$lat[FE_loc_HBI.test$X_name == "D_east"], 
                   yend = FE_loc_HBI.test$lon[FE_loc_HBI.test$X_name == "D_east"], 
                   colour = "segment"))+
  geom_segment(data =FE_loc_HBI.test, 
               aes(x = FE_loc_HBI.test$lat[FE_loc_HBI.test$X_name == "AB_east_inter_2"], 
                   y = FE_loc_HBI.test$lon[FE_loc_HBI.test$X_name == "AB_east_inter_2"], 
                   xend = FE_loc_HBI.test$lat[FE_loc_HBI.test$X_name == "E_east"], 
                   yend = FE_loc_HBI.test$lon[FE_loc_HBI.test$X_name == "E_east"], 
                   colour = "segment"))

# center of plot
center_50005 <- terra::vect(FE_loc_HBI.test %>% filter(X_name == "RW_MED" & Y_name == "HW_MED"), 
                            geom=c("lon", "lat"), 
                            crs="epsg:25833",
                            keepgeom=FALSE)
#crs(center_50005)  <- "epsg:25833"

# 17 m circle around plot center
# https://rdrr.io/cran/terra/man/buffer.html
circle_17_50005 <- terra::buffer(center_50005, 17.84)
# create square
square.50005 <- terra::vect(FE_loc_HBI.test %>% 
                              filter(!(X_name %in% c("RW_MED", "A_east", "B_east")) & !(Y_name %in% c("HW_MED", "A_north", "B_north"))),
                            geom=c("lon", "lat"),
                            crs="epsg:25833",
                            keepgeom=TRUE)


e <- FE_loc_HBI.test %>%
  filter(!(X_name %in% c("RW_MED", "A_east", "B_east")) & !(Y_name %in% c("HW_MED", "A_north", "B_north"))) %>% 
  mutate(lon = as.integer(lon), 
         lat = as.integer(lat)) %>% 
  unite("geometry", c(lon, lat), sep = " ", remove = FALSE)%>%
  mutate(geometry = as.factor(geometry)) %>% 
    select(geometry)

#trying to recreate: "POLYGON ((5679011 2516981, 5679042 2516963, 5679052 2516998, 5679021 2517016, 5679011 2516981))" 
#paste("POLYGON", "(", "(", paste(e$geometry[1], e$geometry[2], e$geometry[3], e$geometry[4], e$geometry[5],sep = ", "), ")", ")", sep = "")
#"POLYGON((5679021.47358096 2517015.68025319, 5679011.35016525 2516981.46653, 5679042.11770261 2516963.39940393,5679052.24111832 2516997.61312711,5679021.47358096 2517015.68025319))"
#"POLYGON((5679011 2516981, 5679042 2516963, 5679052 2516997, 5679021 2517015, 5679011 2516981))"


p <- vect(c("POLYGON ((5679011 2516981, 5679042 2516963, 5679052 2516998, 5679021 2517016, 5679011 2516981))"), crs="epsg:25833")
p <- vect(c(paste("POLYGON", "(", "(", paste(e$geometry[1], e$geometry[2], e$geometry[3], e$geometry[4], e$geometry[5],sep = ", "), ")", ")", sep = "")), crs="epsg:25833")


#write_sf(square.50005.sf, paste0(field.table.path, "gis_messpunkt_", my.bfhnr, ".gpkg"), append=F)
plot(square.50005)
plot(p, add = T)
plot(circle_17_50005, add=T)

inter <- terra::intersect(circle_17_50005, p)
plot(inter)



### SF PACKAGE APPROACH
# create circle in st package:
center_50005.sf <- st_as_sf(FE_loc_HBI.test %>% filter(X_name == "RW_MED" & Y_name == "HW_MED") %>% select(lon, lat), 
                         coords = c("lat", "lon"), crs = 25833) 
circle_17_50005.sf <- st_buffer(center_50005.sf, dist = 17.84)
#terra::crs(square.50005)<- "epsg:4326"
square.50005.sf <- sf::st_as_sf(square.50005)
plot(square.50005.sf$geometry)

box.50005.sf <- FE_loc_HBI.test %>% 
  filter(!(X_name %in% c("RW_MED", "A_east", "B_east")) & !(Y_name %in% c("HW_MED", "A_north", "B_north"))) %>%
  select(lon, lat) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 25833) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

dat.poly <- FE_loc_HBI.test %>% 
  filter(!(X_name %in% c("RW_MED", "A_east", "B_east")) & !(Y_name %in% c("HW_MED", "A_north", "B_north"))) %>% select(lon, lat)
# https://stackoverflow.com/questions/69638192/draw-polygons-around-coordinates-in-r
hulls <- dat.poly %>%
  st_as_sf(coords = c("lon", "lat"), crs = 25833) %>%
  summarize(geometry = st_union(geometry)) %>%
  st_convex_hull()
plot(hulls)

plot(circle_17_50005.sf$geometry)
plot(square.50005.sf$geometry, add = T)
plot(box.50005$geometry)

# https://gis.stackexchange.com/questions/403977/sf-create-polygon-from-minimum-x-and-y-coordinates
square.50005 <- square.50005 %>% 
     st_bbox() %>% 
   st_as_sfc()


st_intersection(circle_17_50005.sf, hulls) 




#https://rdrr.io/cran/terra/man/buffer.html
# https://stackoverflow.com/questions/60769423/convert-point-to-rectangle-polygon-with-width-x-km-and-height-y-km-in-sf-r
# https://gis.stackexchange.com/questions/229453/create-a-circle-of-defined-radius-around-a-point-and-then-find-the-overlapping-a

shorter.site.line <- function(x1, x2, y1, y2, c.x0, c.y0, c.r0, l.b0, l.b1){
  # determin status of intersection: 
  i_status <-   ifelse(is.na(x1) & is.na(x2), " no I",      # if 0 solutions
                       ifelse(!is.na(x1) & !is.na(x2) & x1 == x2, "one I",            # if 1 solution
                              ifelse(x1 != x2, "two I")));      # so if the edge for is 1 and there are 2 interseections of the line with the respective circle 
  
  # calculate coordiantes of the middle of thie line between 
  x_m_line = (x1 - x2)/2;
  y_m_line = (y1 - y2)/2;
  # calculate the parameters of the equation between the middle of the line and the centre of the circle
  b1_MC = slope(c.x0, c.y0, x_m_line, y_m_line);
  b0_MC = intercept(c.x0, c.y0, x_m_line, y_m_line);
  # calcualte the x corrdiante of the interception of the line between M and the centre of the cirle and the circle at the given radio
  X1_inter_MC = intersection_line_circle(b0_MC, b1_MC, c.x0, c.y0, c.r0, coordinate = "x1"); 
  X2_inter_MC = intersection_line_circle(b0_MC, b1_MC, c.x0, c.y0, c.r0, coordinate = "x2");
  # insert the intersection x corodinate in the line function to get the respective y coordinate
  y1_inter_MC = intersection_line_circle(b0_MC, b1_MC, c.x0, c.y0, c.r0, coordinate = "y1"); 
  y2_inter_MC = intersection_line_circle(b0_MC, b1_MC, c.x0, c.y0, c.r0, coordinate = "y1");
  # distance between the intersections (inter_MC_1, inter_MC_2) to M on the line 
  dist_C_inter_1_MC = distance(X1_inter_MC, y1_inter_MC, x_m_line, y_m_line);
  dist_C_inter_2_MC = distance(X2_inter_MC, y2_inter_MC, x_m_line, y_m_line); 
  # find the x and y coordinate of the intersection on the shorter side , which is the side to exlcude from the plot 
  X_inter_MC_shorter_side = ifelse(dist_C_inter_1_MC < dist_C_inter_2_MC, X1_inter_MC, X2_inter_MC); 
  Y_inter_MC_shorter_side = ifelse(dist_C_inter_1_MC < dist_C_inter_2_MC, y1_inter_MC, y2_inter_MC);
  d_inter_MC_shorter_side = distance(X_inter_MC_shorter_side, Y_inter_MC_shorter_side, x_m_line, y_m_line)
  switch(coordinate, 
         x = X_inter_MC_shorter_side,
         y = Y_inter_MC_shorter_side)
}


dat_sf <- st_as_sf(FE_loc_HBI.test, coords = c("RW_MED", "HW_MED"), crs = 4326)
points_sf <- st_as_sf(plot.50005.test, coords = c("x", "y"), crs = 4326)

# Buffer circles by 100m
dat_circles <- st_buffer(dat_sf, dist = (data_circle$r0[3]/100))
plot(dat_circles)

# Convert to sf, set the crs to EPSG:4326 (lat/long), 
# and transform to EPSG:3035
dat_sf <- st_as_sf(data_circle[3,] %>% select(-rmax), coords = c("x0", "y0"), crs = 4326)%>% 
  st_transform(3035)

# Buffer circles by 100m
dat_circles <- st_buffer(dat_sf, dist = (data_circle$r0/100))
plot(dat_circles)







# create plygones with recabgles 
edge.poly.df <- trees_and_edges %>%
  mutate(X_C = coord(Y1_inter_AB_17, X1_inter_AB_17, 1784, 100, coordinate = "x"), 
         Y_C = coord(Y1_inter_AB_17, X1_inter_AB_17, 1784, 100, coordinate = "y"),
         X_D = coord(Y2_inter_AB_17, X2_inter_AB_17, 1784, 100, coordinate = "x"), 
         Y_D = coord(Y2_inter_AB_17, X2_inter_AB_17, 1784, 100, coordinate = "y"), 
         X_E = X_A, 
         Y_E = Y_A) %>% 
select(plot_ID, e_form,
       X_A, X_B, X_C, X_D, X_E, X_inter_AT_triangle_60, X_inter_BT_triangle_60, X_T, 
       Y_A, Y_B, Y_C, Y_D, Y_E, Y_inter_AT_triangle_60, Y_inter_BT_triangle_60, Y_T) %>% 
  to_long(keys = c("X_name",  "Y_name"),
          values = c( "lat", "lon"),  
          names(.)[3:10], names(.)[11:18]) %>% 
  distinct() %>% 
  arrange(plot_ID)

plot(edge.poly.df$lat[edge.poly.df$plot_ID == 50005], edge.poly.df$lon[edge.poly.df$plot_ID == 50005] )

edge.poly.df.e1 <- edge.poly.df %>% 
                filter(e_form == 1 & X_name %in% c("X_A", "X_B", "X_C", "X_D", "X_E") & Y_name %in% c("Y_A", "Y_B", "Y_C", "Y_D", "Y_E")) %>% 
                group_by(plot_ID) %>% 
                filter(!is.na(lon) & !is.na(lat) & !is.nan(lon) & !is.nan(lat)) %>% 
                summarise(n_points = n()) %>% 
               filter(n_points == 5) %>% 
              select(plot_ID) %>% 
              distinct() %>% 
              left_join(., edge.poly.df %>% 
                          filter(e_form == 1 & X_name %in% c("X_A", "X_B", "X_C", "X_D", "X_E") & Y_name %in% c("Y_A", "Y_B", "Y_C", "Y_D", "Y_E")), 
                          #select("plot_ID", "lat", "lon") 
                        by = "plot_ID", 
                        multiple = "all") 
edge.poly.df.e1 <- edge.poly.df.e1 %>% 
  anti_join(edge.poly.df.e1 %>% 
  filter(is.na(lon) | is.na(lat) | is.nan(lon) | is.nan(lat)), 
  by = "plot_ID") %>% 
  select(plot_ID, lon, lat, X_name, Y_name) %>% 
  mutate(lon = as.numeric(lon), 
         lat = as.numeric(lat))
  

dat_ticino_sf <- st_as_sf(edge.poly.df.e1 %>% filter(plot_ID ==  50005) %>% select(lon, lat), coords = c("lat", "lon"), crs = 4326)
  plot(dat_ticino_sf)
  
  
  
  
  WGScoor<-  edge.poly.df.e1
  coordinates(WGScoor)=~lon+lat
  proj4string(WGScoor)<- CRS("+proj=longlat +datum=WGS84")
  #at this point you have something you can save as a shapefile with all the columns intact, but you seem to want to project out of WGS84, so let's do that:
LLcoor<-spTransform(WGScoor,CRS("+proj=longlat"))
#and let's save this:
 raster::shapefile(LLcoor, "MyShapefile.shp")
  


# Intersect the circles with the polygons
ticino_int_circles <- st_intersection(dat_ticino_sf, dat_circles)




