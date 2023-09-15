# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the peat land soil inventory
# forest edges  

# ----- 0. SETUP ---------------------------------------------------------------

# ----- 0.1. packages and functions --------------------------------------------


source(paste0(getwd(), "/scripts/functions_library.R"))


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
                          r0 = c(5.64, 12.62, 17.84), # darius in m
                          rmax = c(30.00, 30.00, 30.00)) # these are the radi of the sampling circuits in m

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



# 3. georefferencing edge data ------------------------------------------------

# 3.1. georefferencing "manually" -----------------------------------
# creating dataframe with caartesian coordinates of all edges
# by using polar coordinates to calculate the cartesian coordinates 
# by adding polar coordiantes calcualted through functions to the 
# cartesian coordinates (RW_MED = lat and HW_MED = lon) of the center point of the plot

# dataset with cart coordinates for all edges
FE_loc_HBI <- forest_edges_HBI.man %>% 
  filter(!is.na(e_form)) %>%
  left_join(HBI_loc %>% 
              filter(K3_HW >0), 
            by = "plot_ID") %>% 
  # Convert polar to cartesian coordinates (eastinf northing)
  mutate(A_east = RW_MED + X_A,
         A_north = HW_MED + Y_A,
         B_east = RW_MED + X_B,
         B_north = HW_MED + Y_B, 
         T_east = RW_MED + X_T, 
         T_north = HW_MED + Y_T) %>% 
  # eastin/ northing for intersection with a 30m circle
  mutate(AB_east_inter_1 = RW_MED + intersection_line_circle(b0_AB, b1_AB, data_circle$x0[3], data_circle$y0[3], data_circle$rmax[3], coordinate = "x1"), # X1_inter_AB_17,
         AB_north_inter_1 = HW_MED + intersection_line_circle(b0_AB, b1_AB, data_circle$x0[3], data_circle$y0[3], data_circle$rmax[3], coordinate = "y1"), #Y1_inter_AB_17,
         AB_east_inter_2 =  RW_MED + intersection_line_circle(b0_AB, b1_AB, data_circle$x0[3], data_circle$y0[3], data_circle$rmax[3], coordinate = "x2"), # X2_inter_AB_17,
         AB_north_inter_2 = HW_MED + intersection_line_circle(b0_AB, b1_AB, data_circle$x0[3], data_circle$y0[3], data_circle$rmax[3], coordinate = "y2")) %>%  #Y2_inter_AB_17,) %>% 
  # calculate point in right angle and 60m distance to each intersection with the 30m circle to create a square 
  mutate(D_east = coord(AB_east_inter_1, AB_north_inter_1, data_circle$r0[3]*2, 100, coordinate = "x"),
          D_north = coord(AB_east_inter_1, AB_north_inter_1, data_circle$r0[3]*2, 100, coordinate = "y"),
          E_east = coord(AB_east_inter_2, AB_north_inter_2, data_circle$r0[3]*2, 100, coordinate = "x"),
         # introduce end point for polygone -> has to be closed
          E_north = coord(AB_east_inter_2, AB_north_inter_2, data_circle$r0[3]*2, 100, coordinate = "y"),
          end_east = AB_east_inter_1, 
          end_north = AB_north_inter_1, 
          east_AT_inter_triangle_60 = RW_MED + inter.for.triangle(b0_AT, b1_AT, data_circle$x0[3], data_circle$y0[3], data_circle$rmax[3], X_A, Y_A, X_T, Y_T, coordinate = "x"),
          north_AT_inter_triangle_60 = HW_MED + inter.for.triangle(b0_AT, b1_AT, data_circle$x0[3], data_circle$y0[3], data_circle$rmax[3], X_A, Y_A, X_T, Y_T, coordinate = "y"),
          east_BT_inter_triangle_60 = RW_MED + inter.for.triangle(b0_BT, b1_BT, data_circle$x0[3], data_circle$y0[3], data_circle$rmax[3], X_B, Y_B, X_T, Y_T, coordinate = "x"),
          north_BT_inter_triangle_60 = HW_MED + inter.for.triangle(b0_BT, b1_BT, data_circle$x0[3], data_circle$y0[3], data_circle$rmax[3], X_B, Y_B, X_T, Y_T, coordinate = "y"), 
         end_triangle_east = T_east, 
         end_triangle_north = T_north)


## datasetwith cart coordiantes for edge form 1 : line
# georefferncing edges for edge form 1 using plot 50005 as an example
FE_loc_HBI.e1 <- FE_loc_HBI %>%
  filter(e_form == 1) %>% 
  filter(plot_ID == 50005) %>% 
  select(plot_ID,  
         A_dist, A_azi, B_dist, B_azi,
         RW_MED, A_east, B_east, D_east, E_east, AB_east_inter_1, AB_east_inter_2, end_east, 
         HW_MED, A_north, B_north, D_north, E_north, AB_north_inter_1, AB_north_inter_2, end_north) %>%  
    to_long(keys = c("X_name",  "Y_name"),
            values = c( "lat", "lon"),  
            names(.)[6:13], names(.)[14:21]) %>% 
  # create an order for the points, so that the polygone forms a square
  mutate(order = case_when(X_name == "AB_east_inter_1" ~ 1,
                           X_name == "D_east" ~ 2, 
                           X_name == "E_east" ~ 3,
                           X_name == "AB_east_inter_2" ~ 4,
                           X_name == "end_east" ~ 5, 
                           TRUE ~ NA)) %>% 
  arrange(plot_ID, order)


# create SpatVec with center of the plots as points: https://rdrr.io/cran/terra/man/vect.html
center.points <- terra::vect(FE_loc_HBI.e1 %>% filter(X_name == "RW_MED" & Y_name == "HW_MED"), 
                             geom=c("lon", "lat"), 
                             crs="epsg:25833",
                             keepgeom=FALSE)

# 17 m circle around plot center: https://rdrr.io/cran/terra/man/buffer.html
circle.17 <- terra::buffer(center.points, 17.84)

# create spatvectior with popoints of the square: 
square.points <- terra::vect(FE_loc_HBI.e1 %>% 
                               filter(!(X_name %in% c("RW_MED", "A_east", "B_east")) & !(Y_name %in% c("HW_MED", "A_north", "B_north"))),
                             geom=c("lon", "lat"),
                             crs="epsg:25833",
                             keepgeom=TRUE)

# prepare quare data to create SpatVec with geometry polygone
dat.poly <- FE_loc_HBI.e1 %>%
  filter(!(X_name %in% c("RW_MED", "A_east", "B_east")) & !(Y_name %in% c("HW_MED", "A_north", "B_north"))) %>% 
  mutate(lon = as.integer(lon), 
         lat = as.integer(lat)) %>% 
  unite("geometry", c(lon, lat), sep = " ", remove = FALSE)%>%
  mutate(geometry = as.factor(geometry)) %>% 
  select(geometry)

# turning points into polygone: https://rdrr.io/cran/terra/man/vect.html
# trying to recreate: #p <- vect(c("POLYGON ((5679011 2516981, 5679042 2516963, 5679052 2516998, 5679021 2517016, 5679011 2516981))"), crs="epsg:25833")
# which was the only way how the points were merged into a polygone
# successfull trial: paste("POLYGON", "(", "(", paste(e$geometry[1], e$geometry[2], e$geometry[3], e$geometry[4], e$geometry[5],sep = ", "), ")", ")", sep = "")
# lon, lat as numeric: "POLYGON((5679021.47358096 2517015.68025319, 5679011.35016525 2516981.46653, 5679042.11770261 2516963.39940393,5679052.24111832 2516997.61312711,5679021.47358096 2517015.68025319))" # with lon lat as numeric
# lon, lat as integer_ "POLYGON((5679011 2516981, 5679042 2516963, 5679052 2516997, 5679021 2517015, 5679011 2516981))"
square.poly <- vect(c(paste("POLYGON", "(", "(", paste(dat.poly$geometry[1], dat.poly$geometry[2], dat.poly$geometry[3], dat.poly$geometry[4], dat.poly$geometry[5],sep = ", "), ")", ")", sep = "")), crs="epsg:25833")

# plot circle, polygone and points
print(plot(square.points),
      plot(square.poly, add = T),
      plot(circle.17, add=T))

# compute intersection between square and circle: https://rdrr.io/github/rspatial/terra/man/intersect.html
inter <- terra::intersect(circle.17, square.poly)
plot(inter)

# get area of intersection
# https://stackoverflow.com/questions/73614988/get-intersection-area-between-two-polygons-when-using-terraintersect
   inter$area <- terra::expanse(inter)
   inter$area_c <- terra::expanse(circle.17)
   inter$area_rest <- inter$area_c -inter$area
   inter$area_e <- ifelse(inter$area < inter$area_rest, inter$area_e, inter$area_rest)
   inter$area_c <- ifelse(inter$area > inter$area_rest, inter$area, inter$area_rest)

# compare with area caclulcated by the function --> does not comply
   trees_and_edges %>% 
     filter(plot_ID == 50005 & trees_and_edges$DBH_cm >30 & t_status_AB_ABT == "B")
   
# plot circle and square
ggplot() +  
  geom_circle(data = FE_loc_HBI.e1 %>% filter(X_name == "RW_MED" & Y_name == "HW_MED"), aes(x0 = lat, y0 = lon, r = 30.00))+ # Draw ggplot2 plot with circle representing sampling circuits 
  geom_circle(data = FE_loc_HBI.e1 %>% filter(X_name == "RW_MED" & Y_name == "HW_MED"), aes(x0 = lat, y0 = lon, r = 17.84))+
  geom_point(data = FE_loc_HBI.e1,
             aes(x= lat, y = lon, colour = X_name))+
  geom_segment(data =FE_loc_HBI.e1, 
               aes(x = FE_loc_HBI.e1$lat[FE_loc_HBI.e1$X_name == "AB_east_inter_1"], 
                   y = FE_loc_HBI.e1$lon[FE_loc_HBI.e1$X_name == "AB_east_inter_1"], 
                   xend = FE_loc_HBI.e1$lat[FE_loc_HBI.e1$X_name == "AB_east_inter_2"], 
                   yend = FE_loc_HBI.e1$lon[FE_loc_HBI.e1$X_name == "AB_east_inter_2"], 
                   colour = "segment")) +
  geom_segment(data =FE_loc_HBI.e1, 
               aes(x = FE_loc_HBI.e1$lat[FE_loc_HBI.e1$X_name == "D_east"], 
                   y = FE_loc_HBI.e1$lon[FE_loc_HBI.e1$X_name == "D_east"], 
                   xend = FE_loc_HBI.e1$lat[FE_loc_HBI.e1$X_name == "E_east"], 
                   yend = FE_loc_HBI.e1$lon[FE_loc_HBI.e1$X_name == "E_east"], 
                   colour = "segment"))+
  geom_segment(data =FE_loc_HBI.e1, 
               aes(x = FE_loc_HBI.e1$lat[FE_loc_HBI.e1$X_name == "AB_east_inter_1"], 
                   y = FE_loc_HBI.e1$lon[FE_loc_HBI.e1$X_name == "AB_east_inter_1"], 
                   xend = FE_loc_HBI.e1$lat[FE_loc_HBI.e1$X_name == "D_east"], 
                   yend = FE_loc_HBI.e1$lon[FE_loc_HBI.e1$X_name == "D_east"], 
                   colour = "segment"))+
  geom_segment(data =FE_loc_HBI.e1, 
               aes(x = FE_loc_HBI.e1$lat[FE_loc_HBI.e1$X_name == "AB_east_inter_2"], 
                   y = FE_loc_HBI.e1$lon[FE_loc_HBI.e1$X_name == "AB_east_inter_2"], 
                   xend = FE_loc_HBI.e1$lat[FE_loc_HBI.e1$X_name == "E_east"], 
                   yend = FE_loc_HBI.e1$lon[FE_loc_HBI.e1$X_name == "E_east"], 
                   colour = "segment"))


## dataset with cart coordinates for edge form 2 : triangle
# georefferncing edges for edge form 2 using plot 50042 as an example
FE_loc_HBI.e2 <- FE_loc_HBI %>% 
  filter(e_form == 2) %>%
  filter(plot_ID == 50004) %>% 
  select(plot_ID,  
         RW_MED, A_east, B_east, T_east, east_AT_inter_triangle_60, east_BT_inter_triangle_60, end_triangle_east, 
         HW_MED, A_north, B_north, T_north, north_AT_inter_triangle_60, north_BT_inter_triangle_60, end_triangle_north) %>%  
  to_long(keys = c("X_name",  "Y_name"),
          values = c( "lat", "lon"),  
          names(.)[2:8], names(.)[9:15]) %>% 
  # create an order for the points, so that the polygone forms a square
  mutate(order = case_when(X_name == "T_east" ~ 1,
                           X_name == "east_AT_inter_triangle_60" ~ 2, 
                           X_name == "east_BT_inter_triangle_60" ~ 3,
                           X_name == "end_triangle_east" ~ 4, 
                           TRUE ~ NA)) %>% 
  arrange(plot_ID, order)

# georefferencing data: 
# create SpatVec with center of the plots as points: https://rdrr.io/cran/terra/man/vect.html
center.points <- terra::vect(FE_loc_HBI.e2 %>% filter(X_name == "RW_MED" & Y_name == "HW_MED"), 
                             geom=c("lon", "lat"), 
                             crs="epsg:25833",
                             keepgeom=FALSE)

# 17 m circle around plot center: https://rdrr.io/cran/terra/man/buffer.html
circle.17 <- terra::buffer(center.points, 17.84, capstyle = "round", joinstyle="round")

# create spatvectior with popoints of the square: 
triangle.points <- terra::vect(FE_loc_HBI.e2 %>% 
                                 filter(!(X_name %in% c("RW_MED", "A_east", "B_east")) & !(Y_name %in% c("HW_MED", "A_north", "B_north"))),
                               geom=c("lon", "lat"),
                               crs="epsg:25833",
                               keepgeom=TRUE)
# plot(triange.points)

# prepare quare data to create SpatVec with geometry polygone
triangle.dat.poly <- FE_loc_HBI.e2 %>%
  filter(!(X_name %in% c("RW_MED", "A_east", "B_east")) & !(Y_name %in% c("HW_MED", "A_north", "B_north"))) %>% 
  mutate(lon = as.integer(lon), 
         lat = as.integer(lat)) %>% 
  unite("geometry", c(lon, lat), sep = " ", remove = FALSE)%>%
  mutate(geometry = as.factor(geometry)) %>% 
  select(geometry)

# turning points into polygone: https://rdrr.io/cran/terra/man/vect.html
# trying to recreate: #p <- vect(c("POLYGON ((5679011 2516981, 5679042 2516963, 5679052 2516998, 5679021 2517016, 5679011 2516981))"), crs="epsg:25833")
# which was the only way how the points were merged into a polygone
# successfull trial: paste("POLYGON", "(", "(", paste(e$geometry[1], e$geometry[2], e$geometry[3], e$geometry[4], e$geometry[5],sep = ", "), ")", ")", sep = "")
# lon, lat as numeric: "POLYGON((5679021.47358096 2517015.68025319, 5679011.35016525 2516981.46653, 5679042.11770261 2516963.39940393,5679052.24111832 2516997.61312711,5679021.47358096 2517015.68025319))" # with lon lat as numeric
# lon, lat as integer_ "POLYGON((5679011 2516981, 5679042 2516963, 5679052 2516997, 5679021 2517015, 5679011 2516981))"
triangle.poly <- vect(c(paste("POLYGON", "(", "(", paste(triangle.dat.poly$geometry[1], triangle.dat.poly$geometry[2], triangle.dat.poly$geometry[3], triangle.dat.poly$geometry[4], sep = ", "), ")", ")", sep = "")), crs="epsg:25833")

# plot circle, polygone and points
print(plot(triangle.points),
      plot(triangle.poly, add = T),
      plot(circle.17, add=T))

# compute intersection between square and circle: https://rdrr.io/github/rspatial/terra/man/intersect.html
inter.e2 <- terra::intersect(circle.17, triangle.poly)
plot(inter.e2)
inter.e2$area <- terra::expanse(inter.e2)
inter.e2$area


terra::expanse(circle.17, unit="m", transform=TRUE)

point_sf <- st_as_sfc("POINT (5615244 2516952)", crs= 25833)
buffer_sf <- st_buffer(point_sf, dist = 17.84)
sf::st_area(buffer_sf)

# comparing it with the edge area caclucated by the function under plot_A in the "trees_and_edges" dataset
trees_and_edges %>% 
  filter(plot_ID == 50042 & trees_and_edges$DBH_cm >30 & t_status_AB_ABT == "B")


trees_and_edges %>% 
  filter(plot_ID == 50004 & trees_and_edges$DBH_cm >30 & trees_and_edges$edge_A_method == "c.A -edge.area")


# plot circle and trianle
ggplot() +  
  geom_circle(data = FE_loc_HBI.e2 %>% filter(X_name == "RW_MED" & Y_name == "HW_MED"), aes(x0 = lat, y0 = lon, r = 30.00))+ # Draw ggplot2 plot with circle representing sampling circuits 
  geom_circle(data = FE_loc_HBI.e2 %>% filter(X_name == "RW_MED" & Y_name == "HW_MED"), aes(x0 = lat, y0 = lon, r = 17.84))+
  geom_point(data = FE_loc_HBI.e2,
             aes(x= lat, y = lon, colour = X_name))+
  geom_segment(data =FE_loc_HBI.e2, 
               aes(x = FE_loc_HBI.e2$lat[FE_loc_HBI.e2$X_name == "T_east"], 
                   y = FE_loc_HBI.e2$lon[FE_loc_HBI.e2$X_name == "T_east"], 
                   xend = FE_loc_HBI.e2$lat[FE_loc_HBI.e2$X_name == "east_AT_inter_triangle_60"], 
                   yend = FE_loc_HBI.e2$lon[FE_loc_HBI.e2$X_name == "east_AT_inter_triangle_60"], 
                   colour = "segment")) +
  geom_segment(data =FE_loc_HBI.e2, 
               aes(x = FE_loc_HBI.e2$lat[FE_loc_HBI.e2$X_name == "T_east"], 
                   y = FE_loc_HBI.e2$lon[FE_loc_HBI.e2$X_name == "T_east"], 
                   xend = FE_loc_HBI.e2$lat[FE_loc_HBI.e2$X_name == "east_BT_inter_triangle_60"], 
                   yend = FE_loc_HBI.e2$lon[FE_loc_HBI.e2$X_name == "east_BT_inter_triangle_60"], 
                   colour = "segment"))+
  geom_segment(data =FE_loc_HBI.e2, 
               aes(x = FE_loc_HBI.e2$lat[FE_loc_HBI.e2$X_name == "east_AT_inter_triangle_60"], 
                   y = FE_loc_HBI.e2$lon[FE_loc_HBI.e2$X_name == "east_AT_inter_triangle_60"], 
                   xend = FE_loc_HBI.e2$lat[FE_loc_HBI.e2$X_name == "east_BT_inter_triangle_60"], 
                   yend = FE_loc_HBI.e2$lon[FE_loc_HBI.e2$X_name == "east_BT_inter_triangle_60"], 
                   colour = "segment"))



# 3.2. georefferencing per loop ---------------------------------------------------------------------------------
# creating dataframe with caartesian coordinates of all edges
# by using polar coordinates to calculate the cartesian coordinates 
# by adding polar coordiantes calcualted through functions to the 
# cartesian coordinates (RW_MED = lat and HW_MED = lon) of the center point of the plot

# https://stackoverflow.com/questions/26504736/create-a-list-of-spatial-polygon-dataframe-from-a-list-of-dataframe

# 3.2.1. georefferencing trough separate loops  ----------------------------------------------------------------

# 3.2.1.1. creating list of polygones for circles (17.84m) per plot  -------------------------------------------
# dataset with only edge forms 1 and 2 
forest_edges_HBI.man.sub <- forest_edges_HBI.man %>% 
  filter(e_form %in% c(1, 2))

## loop to create list with polygones for circles per plot center 
# create empty list to store circle polygones in 
circle.list <- vector("list", length = length(unique(forest_edges_HBI.man.sub$plot_ID)))
for(i in 1:length(unique(forest_edges_HBI.man.sub$plot_ID))) {
  # i = 1
  # georefferencing data: 
  
  # select plot ID accordint to positioin in the list
  my.plot.id <- forest_edges_HBI.man.sub[i, "plot_ID"] 
  my.e.form <- forest_edges_HBI.man.sub[i, "e_form"]
  #my.n.of.edges <- forest_edges_HBI.man %>% filter(plot_ID == my.plot.id) %>% group_by(plot_ID) %>% summarize(n = n()) %>% dplyr::pull(n)
  
  # assign crs
  my.utm.epsg <- 25833
  
  # select UTM corrdinates of the plot center
  my.center.easting <- HBI_loc[HBI_loc$plot_ID == my.plot.id, "RW_MED"]
  my.center.northing <- HBI_loc[HBI_loc$plot_ID == my.plot.id, "HW_MED"]
  center.df <- as.data.frame(cbind("id" = my.plot.id, 
                                   "lon" = my.center.northing, 
                                   "lat" = my.center.easting))
  
  # create sf point with center coordiantes
  center.point <- sf::st_as_sf(center.df, coords = c("lat", "lon"), crs = my.utm.epsg)
  # build polygon (circlular buffer) around center point
  circle.17 <- sf::st_buffer(center.point, 17.84)
  circle.12 <- sf::st_buffer(center.point, 12.62)
  circle.5 <- sf::st_buffer(center.point, 5.64)
 
  
  # creating polygones in r terra package
  # create SpatVec with center of the plots as points: https://rdrr.io/cran/terra/man/vect.html
  # center.points <- terra::vect(center.df, 
  #                              geom=c("my.center.northing", "my.center.easting"), 
  #                              crs="epsg:25833",
  #                              keepgeom=FALSE)
  # 17 m circle around plot center: https://rdrr.io/cran/terra/man/buffer.html
  # circle.17 <- terra::buffer(center.points, 17.84)
  # print circle and center 
  # print(plot(center.points),
  #       plot(circle.17, add = T))
  
  # saving circle polygones in a list
  circle.list[[i]] <- rbind(circle.17, circle.12, circle.5)
  
}
# circle.list
 circle.list.final <- rbindlist(circle.list)
 circle.poly.df <- as.data.frame(circle.list.final)
 
  
 
 

# 3.2.1.2. creating list of squared polygones for eddge form 1  ---------------------------
## loop to create list of polygones for edge form 1
 forest_edges_HBI.man.sub.e1 <- forest_edges_HBI.man%>% filter(e_form == 1) %>% filter(inter_status_AB_17 == "two I")
 square.list <- vector("list", length = length(forest_edges_HBI.man.sub.e1$plot_ID))
 for(i in 1:length((forest_edges_HBI.man.sub.e1$plot_ID)) ) {
   # i = 11
   # georefferencing data: 
   
   # select plot ID accordint to positioin in the list
   my.plot.id <- forest_edges_HBI.man.sub.e1[i, "plot_ID"] 
   my.e.id <- forest_edges_HBI.man.sub.e1[i, "e_ID"]
   my.e.form <- forest_edges_HBI.man.sub.e1[i, "e_form"]
   #my.n.of.edges <- forest_edges_HBI.man %>% filter(plot_ID == my.plot.id) %>% group_by(plot_ID) %>% summarize(n = n()) %>% dplyr::pull(n)
   
   # assign crs
   my.utm.epsg <- 25833
   
   # select UTM corrdinates of the plot center by plot ID
   my.center.easting <- HBI_loc[HBI_loc$plot_ID == my.plot.id, "RW_MED"]
   my.center.northing <- HBI_loc[HBI_loc$plot_ID == my.plot.id, "HW_MED"]
 
  # extract polar coordiantes of forest edge
  # point A 
  dist.A <- forest_edges_HBI.man.sub.e1[i, "A_dist"] 
  azi.A <- forest_edges_HBI.man.sub.e1[i, "A_azi"] 
  x.A <- dist.A*sin(azi.A)
  y.A <- dist.A*cos(azi.A)
  
  # point B
  dist.B <- forest_edges_HBI.man.sub.e1[i, "B_dist"] 
  azi.B <- forest_edges_HBI.man.sub.e1[i, "B_azi"] 
  x.B <- dist.B*sin(azi.B)
  y.B <- dist.B*cos(azi.B)

  b1 <- (y.B- y.A)/(x.B - x.A)
  b0 <- b1*x.B-y.B
  
  # calculate polar coordiantes of intersections of AB line with 
  AB.inter.x1 <- intersection_line_circle(b0, b1, data_circle$x0[3], data_circle$x0[3], data_circle$rmax[3], coordinate = "x1")
  AB.inter.y1 <- intersection_line_circle(b0, b1, data_circle$x0[3], data_circle$x0[3], data_circle$rmax[3], coordinate = "y1")
  AB.inter.x2 <- intersection_line_circle(b0, b1, data_circle$x0[3], data_circle$x0[3], data_circle$rmax[3], coordinate = "x2")
  AB.inter.y2 <- intersection_line_circle(b0, b1 ,data_circle$x0[3], data_circle$x0[3], data_circle$rmax[3], coordinate = "y2")
  
  my.inter.status <- intersection.status(AB.inter.x1, AB.inter.x2)
  
  #  polar coordinates of the corner points of square along the intersecting line AB 
  # by going 60m distance in a right angle from the respective intersection coordiante
  x.D <- coord(AB.inter.x1, AB.inter.y1,  data_circle$rmax[3]*2, 100, coordinate = "x")
  y.D <- coord(AB.inter.x1, AB.inter.y1,  data_circle$rmax[3]*2, 100, coordinate = "y")
  x.E <- coord(AB.inter.x2, AB.inter.y2,  data_circle$rmax[3]*2, 100, coordinate = "x")
  y.E <- coord(AB.inter.x2, AB.inter.y2,  data_circle$rmax[3]*2, 100, coordinate = "y")
  
  # UTM coordiantes of corner points 
  AB.inter.1.east <- my.center.easting + AB.inter.x1 
  AB.inter.1.north <- my.center.northing + AB.inter.y1
  AB.inter.2.east <- my.center.easting + AB.inter.x2 
  AB.inter.2.north <- my.center.northing + AB.inter.y2
  D.east <- my.center.easting + x.D
  D.north <- my.center.northing + y.D
  E.east <- my.center.easting + x.E
  E.north <- my.center.northing + y.E
  
  
  # create dataframe that holds coordinates of 
  square.df <- as.data.frame(cbind("lat" = c(AB.inter.1.east, D.east, E.east, AB.inter.2.east, AB.inter.1.east),
                                   "lon" = c(AB.inter.1.north, D.north, E.north, AB.inter.2.north, AB.inter.1.north),
                                   "id" = c(my.plot.id, my.plot.id, my.plot.id, my.plot.id, my.plot.id)
                                   ))%>% 
    mutate(lat = as.integer(lat), 
           lon = as.integer(lon)) %>% 
    unite("geometry", c(lon, lat), sep = " ", remove = FALSE)%>%
    mutate(geometry = as.factor(geometry))# %>% 
    #select(geometry)
  
 ##creating squares in terrra: 
  #square.poly <- terra::vect(c(paste("POLYGON", "(", "(", paste(square.df$geometry[1], square.df$geometry[2], square.df$geometry[3], square.df$geometry[4], square.df$geometry[5], sep = ", "), ")", ")", sep = "")), crs="epsg:25833")
  
 # creating squeares in sf: https://stackoverflow.com/questions/61215968/creating-sf-polygons-from-a-dataframe
  square.poly <- sfheaders::sf_polygon(obj = square.df
                                       , x = "lat"
                                       , y = "lon"
                                       , polygon_id = "id")
  # assing crs
  sf::st_crs(square.poly) <- my.utm.epsg
  
  print(plot(square.poly))
  
  square.list[[i]] <- c("e_id" = my.e.id, square.poly)

 } # closing loop for square polys of edge form 1
 
 square.list.final <- rbindlist(square.list)
 square.poly.df <- as.data.frame(square.list.final) %>% mutate("e_form" = 1)

 

 
 
 
# 3.2.1.2. creating list of triangle polygons for edge form 2 ---------------------------

 ## loop to create list of polygones for edge form 1
 forest_edges_HBI.man.sub.e2 <- forest_edges_HBI.man%>% filter(e_form == 2)
 triangle.list <- vector("list", length = length(forest_edges_HBI.man.sub.e2$plot_ID) )
 for(i in 1:length(forest_edges_HBI.man.sub.e2$plot_ID) ) {
   # i = 1
   # georefferencing data: 
   
   # select plot ID accordint to positioin in the list
   my.plot.id <- forest_edges_HBI.man.sub.e2[i, "plot_ID"] 
   my.e.id <- forest_edges_HBI.man.sub.e2[i, "e_ID"] 
   my.e.form <- forest_edges_HBI.man.sub.e2[i, "e_form"]
   #my.n.of.edges <- forest_edges_HBI.man %>% filter(plot_ID == my.plot.id) %>% group_by(plot_ID) %>% summarize(n = n()) %>% dplyr::pull(n)
   
   # assign crs
   my.utm.epsg <- 25833
   
   # select UTM corrdinates of the plot center
   my.center.easting <- HBI_loc[HBI_loc$plot_ID == my.plot.id, "RW_MED"]
   my.center.northing <- HBI_loc[HBI_loc$plot_ID == my.plot.id, "HW_MED"]
   
   # extract polar coordiantes of forest edge
   # point A 
   dist.A <- forest_edges_HBI.man.sub.e2[i, "A_dist"] 
   azi.A <- forest_edges_HBI.man.sub.e2[i, "A_azi"] 
   x.A <- dist.A*sin(azi.A)
   y.A <- dist.A*cos(azi.A)
   
   # point B
   dist.B <- forest_edges_HBI.man.sub.e2[i, "B_dist"] 
   azi.B <- forest_edges_HBI.man.sub.e2[i, "B_azi"] 
   x.B <- dist.B*sin(azi.B)
   y.B <- dist.B*cos(azi.B)
   
   # point T
   dist.T <- forest_edges_HBI.man.sub.e2[i, "T_dist"] 
   azi.T <- forest_edges_HBI.man.sub.e2[i, "T_azi"] 
   x.T <- dist.T*sin(azi.T)
   y.T <- dist.T*cos(azi.T)
   
   
   # select polar coordiantes of the points of the triangle corners via "inter_for_triangle"-function
   # for AT side
   AT.triangle.x <- inter.for.triangle(intercept(x.T, y.T, x.A, y.A), slope(x.T, y.T, x.A, y.A), 
                                       data_circle$x0[3], data_circle$y0[3], data_circle$rmax[3]*2, 
                                       x.A, y.A, x.T, y.T, 
                                       coordinate = "x")
   AT.triangle.y <- inter.for.triangle(intercept(x.T, y.T, x.A, y.A), slope(x.T, y.T, x.A, y.A), 
                                       data_circle$x0[3], data_circle$y0[3],data_circle$rmax[3]*2, 
                                       x.A, y.A, x.T, y.T, 
                                       coordinate = "y")
   # for BT side
   BT.triangle.x <- inter.for.triangle(intercept(x.T, y.T, x.B, y.B),slope(x.T, y.T, x.B, y.B), 
                                       data_circle$x0[3],data_circle$y0[3],data_circle$rmax[3]*2, 
                                       x.B, y.B, x.T, y.T, 
                                       coordinate = "x")
   BT.triangle.y <- inter.for.triangle(intercept(x.T, y.T, x.B, y.B), slope(x.T, y.T, x.B, y.B), 
                                       data_circle$x0[3], data_circle$y0[3], data_circle$rmax[3]*2, 
                                       x.B, y.B, x.T, y.T, 
                                       coordinate = "y")
   
   #calculate UTM coordiantes of triangle corners
   T.east <- my.center.easting + x.T
   T.north <- my.center.northing + y.T
   AT.triangle.east <- my.center.easting + AT.triangle.x
   AT.triangle.north <- my.center.northing + AT.triangle.y
   BT.triangle.east <- my.center.easting + BT.triangle.x
   BT.triangle.north <- my.center.northing + BT.triangle.y
   
   # create dataframe with triangle corner UTM coordiantes
   triangle.df <- as.data.frame(cbind("lat" = c(T.east, AT.triangle.east, BT.triangle.east, T.east), 
                                      "lon" = c(T.north, AT.triangle.north, BT.triangle.north, T.north), 
                                      "id" =  c(my.plot.id, my.plot.id, my.plot.id, my.plot.id)  ))%>%
     mutate(lon = as.integer(lon),
            lat = as.integer(lat)) %>%
     unite("geometry", c(lon, lat), sep = " ", remove = FALSE)%>%
     mutate(geometry = as.factor(geometry))
     #select(geometry)
   
   
   # terra: create polygone with corners of triangle
   #triangle.poly <- vect(c(paste("POLYGON", "(", "(", paste(triangle.df$geometry[1], triangle.df$geometry[2], triangle.df$geometry[3], triangle.df$geometry[4], sep = ", "), ")", ")", sep = "")), crs="epsg:25833")
   
   # createa polygone with triangle corners via sf package: https://r-spatial.github.io/sf/reference/st.html
   triangle.poly <- sfheaders::sf_polygon(obj = triangle.df
                                        , x = "lat"
                                        , y = "lon"
                                        , polygon_id = "id")
   # assing crs
   sf::st_crs(triangle.poly) <- my.utm.epsg
   
   # print triangle
   print(plot(triangle.poly))
   
   # save polygones in list
   triangle.list[[i]] <- c("e_id" = my.e.id, triangle.poly)
 }
 
 triangle.list.final <- rbindlist(triangle.list)
 triangle.poly.df <- as.data.frame(triangle.list.final) %>% mutate("e_form" = 2)
 
 
 

# 3.2.1.3. loop for intersections between circles and edges ---------------

 
 
 
 
 # intersection testrun: 
 # # option 1: find row number in final dataframe and look for row number in list ´, then turn list into dataframe and then into sf 
 #   # advatange keeps id of circle too, disadvatage: circle id doubles after intersecting
 #   # select list entry that correpons with my plot ID
 #    # https://stackoverflow.com/questions/20782218/how-to-find-row-number-of-a-value-in-r-code
 #   my.circle.list.id <- which(grepl(my.plot.id, circle.df$id))
 #   my.circle <- sf::st_as_sf (as.data.frame(circle.list[my.circle.list.id]))
 #   st_intersection(my.circle, square.poly)
 # there will always appear a warning this way saying: "attribute variables are assumed to be spatially constant throughout all geometries" solution is displayed here: https://github.com/r-spatial/sf/issues/406
 #   plot()
 #  
 # # option 2: select polygoen of circle from final circle df directly 
 #   # select the circle polygone corresponding with the plot ID
 #   my.circle <- sf::st_as_sf(circle.df$geometry[circle.df$id == my.plot.id])
 #   # calculate intersection
 #   st_intersection(my.circle, square.poly)
 #   plot(st_intersection(my.circle, square.poly))
 
 # dataprep for loop
 # bind polygone dataframes together
 edge.poly.df <- rbind(square.poly.df, triangle.poly.df)
 forest_edges_HBI.man.sub <- forest_edges_HBI.man.sub %>% filter(e_form == 1 & inter_status_AB_17 == "two I" | e_form == 2)
 
  # option 2: select the polygones from trianalge and square which comply with circle ID 
 for (i in 1:length(unique(forest_edges_HBI.man.sub$plot_ID))){ 
      # i = 23

   # select plot ID of the respective circle 
    my.plot.id <- forest_edges_HBI.man.sub[i, "plot_ID"]
    
    my.plot.polys.df <- edge.poly.df %>% filter(id == my.plot.id)
    
    nrow(plot.polys.df)
    # select the circle polygone corresponding with the plot ID
    my.circle <- sf::st_as_sf(circle.poly.df[i,])
    #plot(my.circle)
    
    # select the respective polygones the circle is intersected by
    
    
    # calculate intersection
    st_intersection(my.circle, square.poly)
    
    plot(st_intersection(my.circle, square.poly))
    
    
 }
 
 
 
 
 
 
# 3.2.2. georeffernecing everything in 1 loop -----------------------------


# create dataset that only contains plots with just one edge
forest_edges_HBI.man.sub <- forest_edges_HBI.man %>% 
  semi_join(., forest_edges_HBI.man %>% group_by(plot_ID) %>% summarize(n = n()) %>% filter(n <2) %>% select(plot_ID), 
            by = "plot_ID") %>% 
  filter(plot_ID %in% c(50005, 50052))
# create set with plots who have two edges
forest_edges_HBI.man.2eforms <- forest_edges_HBI.man %>% 
  anti_join(., forest_edges_HBI.man %>% group_by(plot_ID) %>% summarize(n = n()) %>% filter(n <2) %>% select(plot_ID), 
            by = "plot_ID")

# create empty list to store results in
area.list  <- vector("list", length = nrow(forest_edges_HBI.man.sub))
 for(i in 1:length(forest_edges_HBI.man.sub$plot_ID)) {
  # i = 2
  # georefferencing data: 
   
   # select plot ID accordint to positioin in the list
   my.plot.id <- forest_edges_HBI.man[i, "plot_ID"] 
   my.e.form <- forest_edges_HBI.man[i, "e_form"]
   my.n.of.edges <- forest_edges_HBI.man %>% filter(plot_ID == my.plot.id) %>% group_by(plot_ID) %>% summarize(n = n()) %>% dplyr::pull(n)
   
   # assign crs
   my.utm.epsg <- 25833
   
   # select UTM corrdinates of the plot center
   my.center.easting <- HBI_loc[HBI_loc$plot_ID == my.plot.id, "RW_MED"]
   my.center.northing <- HBI_loc[HBI_loc$plot_ID == my.plot.id, "HW_MED"]
   center.df <- as.data.frame(cbind(my.plot.id, my.center.northing, my.center.easting))
   
   # extract polar coordiantes of forest edge
   # point A 
   dist.A <- forest_edges_HBI.man[i, "A_dist"] 
   azi.A <- forest_edges_HBI.man[i, "A_azi"] 
   x.A <- dist.A*sin(azi.A)
   y.A <- dist.A*cos(azi.A)

   # point B
   dist.B <- forest_edges_HBI.man[i, "B_dist"] 
   azi.B <- forest_edges_HBI.man[i, "B_azi"] 
   x.B <- dist.B*sin(azi.B)
   y.B <- dist.B*cos(azi.B)

   # create SpatVec with center of the plots as points: https://rdrr.io/cran/terra/man/vect.html
   center.points <- terra::vect(center.df, 
                                geom=c("my.center.northing", "my.center.easting"), 
                                crs="epsg:25833",
                                keepgeom=FALSE)
   
   # 17 m circle around plot center: https://rdrr.io/cran/terra/man/buffer.html
   circle.17 <- terra::buffer(center.points, 17.84)
   print(#plot(center.points), 
         plot(circle.17))
   
   if(my.e.form == 1){
    # select points to build square for edge type 1 
   # calcualte polar corrdiantes intersection 
   # inter AB 1
   AB.inter.x1 <- intersection_line_circle(intercept(x.A, y.A, x.B, y.B), slope(x.A, y.A, x.B, y.B), 
                                           data_circle$x0[3], data_circle$x0[3], data_circle$rmax[3], 
                                           coordinate = "x1")
   AB.inter.y1 <- intersection_line_circle(intercept(x.A, y.A, x.B, y.B), 
                                           slope(x.A, y.A, x.B, y.B), 
                                           data_circle$x0[3], data_circle$x0[3], data_circle$rmax[3], 
                                           coordinate = "y1")
  
   AB.inter.x2 <- intersection_line_circle(intercept(x.A, y.A, x.B, y.B), 
                                           slope(x.A, y.A, x.B, y.B), 
                                           data_circle$x0[3], data_circle$x0[3], data_circle$rmax[3], 
                                           coordinate = "x2")
   AB.inter.y2 <- intersection_line_circle(intercept(x.A, y.A, x.B, y.B), 
                                           slope(x.A, y.A, x.B, y.B), 
                                           data_circle$x0[3], data_circle$x0[3], data_circle$rmax[3], 
                                           coordinate = "y2")
   
   #  polar coordinates of the corner points of square along the intersecting line AB 
     # by going 60m distance in a right angle from the respective intersection coordiante
   x.D <- coord(AB.inter.x1, AB.inter.y1,  data_circle$rmax[3]*2, 100, coordinate = "x")
   y.D <- coord(AB.inter.x1, AB.inter.y1,  data_circle$rmax[3]*2, 100, coordinate = "y")
   x.E <- coord(AB.inter.x2, AB.inter.y2,  data_circle$rmax[3]*2, 100, coordinate = "x")
   y.E <- coord(AB.inter.x2, AB.inter.y2,  data_circle$rmax[3]*2, 100, coordinate = "y")
   
   # UTM coordiantes of corner points 
   AB.inter.1.east <- my.center.easting + AB.inter.x1 
   AB.inter.1.north <- my.center.northing + AB.inter.y1
   AB.inter.2.east <- my.center.easting + AB.inter.x2 
   AB.inter.2.north <- my.center.northing + AB.inter.y2
   D.east <- my.center.easting + x.D
   D.north <- my.center.northing + y.D
   E.east <- my.center.easting + x.E
   E.north <- my.center.northing + y.E
   
   
   # create dataframe that holds coordinates of 
   square.df <- as.data.frame(cbind("lat" = c(AB.inter.1.east, D.east, E.east, AB.inter.2.east, AB.inter.1.east),
                                    "lon" = c(AB.inter.1.north, D.north, E.north, AB.inter.2.north, AB.inter.1.north),
                                    "id" = c(my.plot.id, my.plot.id, my.plot.id, my.plot.id, my.plot.id)
                                    ))%>% 
     mutate(lat = as.integer(lat), 
            lon = as.integer(lon)) %>% 
     unite("geometry", c(lon, lat), sep = " ", remove = FALSE)%>%
     mutate(geometry = as.factor(geometry)) %>% 
     select(geometry)
   
   square.poly <- vect(c(paste("POLYGON", "(", "(", paste(square.df$geometry[1], square.df$geometry[2], square.df$geometry[3], square.df$geometry[4], square.df$geometry[5], sep = ", "), ")", ")", sep = "")), crs="epsg:25833")
   
   print(plot(square.poly), 
         plot(circle.17, add = T))
   
   inter.square <- terra::intersect(circle.17, square.poly)
   # https://rdrr.io/cran/terra/man/erase.html
   remaining.cricle.squ <- terra::erase(circle.17,inter.square)
   
   print(plot(remaining.cricle.squ, col="tomato1"),
         plot(inter.square, col="palegreen2", add = T),
         plot(square.poly, add = T)
         #plot(circle.17, add = T)
         )
  
   # save area of the intresection into dataframe 
   inter.area <- terra::expanse(inter.square)
   inter.area <- data.table(inter.area)
   
   # closing "if" 
   # if edge form == 2 we need to build a triangle along the forest edge, not a 
   }else{
     # select polar coordiantes of point T (which doesn´t exist for e.form == 1)
     # point T 
     dist.T <- forest_edges_HBI.man[i, "T_dist"] 
     azi.T <- forest_edges_HBI.man[i, "T_azi"] 
     x.T <- dist.T*sin(azi.T)
     y.T <- dist.T*cos(azi.T)
     
     # select coordiantes of the points of the triangle corners
     # for AT side
     AT.triangle.x <- inter.for.triangle(intercept(x.T, y.T, x.A, y.A), 
                                          slope(x.T, y.T, x.A, y.A), 
                                          data_circle$x0[3], 
                                          data_circle$y0[3], 
                                          data_circle$rmax[3]*2, 
                                          x.A, y.A, 
                                          x.T, y.T, 
                                          coordinate = "x")
     AT.triangle.y <- inter.for.triangle(intercept(x.T, y.T, x.A, y.A), 
                                          slope(x.T, y.T, x.A, y.A), 
                                          data_circle$x0[3], 
                                          data_circle$y0[3], 
                                          data_circle$rmax[3]*2, 
                                          x.A, y.A, 
                                          x.T, y.T, 
                                          coordinate = "y")
     # for BT side
     BT.triangle.x <- inter.for.triangle(intercept(x.T, y.T, x.B, y.B), 
                                          slope(x.T, y.T, x.B, y.B), 
                                          data_circle$x0[3], 
                                          data_circle$y0[3], 
                                          data_circle$rmax[3]*2, 
                                          x.B, y.B, 
                                          x.T, y.T, 
                                          coordinate = "x")
     BT.triangle.y <- inter.for.triangle(intercept(x.T, y.T, x.B, y.B), 
                                          slope(x.T, y.T, x.B, y.B), 
                                          data_circle$x0[3], 
                                          data_circle$y0[3], 
                                          data_circle$rmax[3]*2, 
                                          x.B, y.B, 
                                          x.T, y.T, 
                                          coordinate = "y")
     
     #calculate UTM coordiantes of triangle corners
     T.east <- my.center.easting + x.T
     T.north <- my.center.northing + y.T
     AT.triangle.east <- my.center.easting + AT.triangle.x
     AT.triangle.north <- my.center.northing + AT.triangle.y
     BT.triangle.east <- my.center.easting + BT.triangle.x
     BT.triangle.north <- my.center.northing + BT.triangle.y
     
     # create dataframe with triangle corner UTM coordiantes
     triangle.df <- as.data.frame(cbind("lat" = c(T.east, AT.triangle.east, BT.triangle.east, T.east), 
                                        "lon" = c(T.north, AT.triangle.north, BT.triangle.north, T.north), 
                                        "id" = c(rep(my.plot.id, length("lon"))) 
                                        ))%>%
                                          mutate(lon = as.integer(lon),
                                                 lat = as.integer(lat)) %>%
                                          unite("geometry", c(lon, lat), sep = " ", remove = FALSE)%>%
                                          mutate(geometry = as.factor(geometry)) %>%
                                          select(geometry)
                                       
     
     # create polygone with corners of triangle
     triangle.poly <- vect(c(paste("POLYGON", "(", "(", paste(triangle.df$geometry[1], triangle.df$geometry[2], triangle.df$geometry[3], triangle.df$geometry[4], sep = ", "), ")", ")", sep = "")), crs="epsg:25833")
     
    print(plot(triangle.poly),
          plot(circle.17, add = T)
          ) 
    
    inter.triangle <- terra::intersect(circle.17, triangle.poly)
    remaining.cirlce.tri <- terra::erase(circle.17, inter.triangle)
    print(plot(circle.17), 
          plot(inter.triangle, add = T), 
          plot(triangle.poly, add = T), 
          plot(remaining.cirlce.tri, col="tomato1", add = T))
    
    inter.area <-  terra::expanse(inter.triangle)
    inter.area <- data.table(inter.area)
    
   } # closing else

   
   area.list[[i]] <- inter.area
   
 } # closing loop

# Bind the list elements together
area.dt <- rbindlist(area.list)  
  




