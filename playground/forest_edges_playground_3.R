# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the peat land soil inventory
# forest edges playground 2
# this script is meant to play around and to accumulate trials and errors

# ----- 0. SETUP ---------------------------------------------------------------
# ----- 0.1. packages and functions --------------------------------------------

#source(paste0(getwd(), "/playground/functions_library_playground.R"))
source(paste0(getwd(), "/scripts/functions_library.R"))

# ----- 0.2. working directory -------------------------------------------------
here::here()
getwd()

#out.path.BZE3 <- ("output/out_data/out_data_BZE/") 


# ----- 0.3 data import --------------------------------------------------------
# LIVING TREES
# BZE3 BE dataset: this dataset contains the inventory data of the tree inventory accompanying the third national soil inventory
HBI_trees <- read.delim(file = here("playground/data/input/beab.csv"), sep = ",", dec = ",")
# BZE3 locations dataset: this dataset contains the coordinates of the center point of the tree inventory accompanying the third national soil inventory
HBI_loc <- read.delim(file = here("playground/data/input/location_HBI.csv"), sep = ";", dec = ",")


# BZE3 BE dataset: this dataset contains the inventory data of the tree inventory accompanying the third national soil inventory
# BZE3_trees <- read.delim(file = here("data/input/BZE3/BZE3_trees_total.csv"), sep = ";", dec = ",")

SP_names_com_ID_tapeS <- read.delim(file = here("playground/data/input/x_bart_tapeS.csv"), sep = ",", dec = ",") 

forest_edges_HBI <- read.delim(file = here("playground/data/input/be_waldraender.csv"), sep = ";", dec = ",")
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
# this is necesarry to make the function work. why exactly remains unclear 
mutate(id_func = row_number()) %>%
  group_by(id_func) %>% 
  mutate(edge_A_method = edge.A(e_form, DBH_cm,  X_A, X_B, X_T, Y_A, Y_B, Y_T, T_dist, t_status_AB_ABT, output = "method"), 
         plot_A =  edge.A(e_form, DBH_cm,  X_A, X_B, X_T, Y_A, Y_B, Y_T, T_dist, t_status_AB_ABT, output = "area_m2")) %>% 
  ungroup()


# ----- 1.1.2.6. exporting tree & edge data & edge areas -------------------------------------------------------------------
# write.csv(trees_and_edges, paste0(out.path.BZE3,"LT_edges_HBI.csv"))



# 3.2.1.2. creating list of squared polygones for eddge form 1  -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## loop to create list of polygones for edge form 1
#forest_edges_HBI.man.sub.e1 <- forest_edges_HBI.man%>% filter(e_form == 1)#%>% filter(inter_status_AB_17 == "two I") # 63 of edge form 1 -> with intersection 43

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
  # list with inter and remaining circle areas areas
  edges.list.nogeo[[i]] <- inter.area.df
  
  # create lists with polgons of intersections if there are intersections, if there is non, save the polygone instead. 
  inter.poly.list.nogeo[[i]] <- if(isTRUE(nrow(inter.poly.17)!= 0)){c(inter.poly.17)}else{c(my.poly)}
  
  remaining.circle.poly.17$e_ID <- 0
  remaining.circle.poly.17$e_form <- 0
  
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
inter.poly.one.edge.df <- as.data.frame(inter.poly.list.final.nogeo)#[,c(2, 1, 3, 5)]%>% arrange(id, e_id)

# list of polygones of remainign circles 
rem.circle.poly.list.final.nogeo <- rbindlist(remaining.circle.poly.list.nogeo, fill = TRUE)
rem.circle.poly.df.nogeo <- as.data.frame(rem.circle.poly.list.final.nogeo)#[,c(2,1,4)]  %>% distinct()
# list of multipolygones of remaining circles
rem.circle.multipoly.list.final.nogeo <- rbindlist(remaining.circle.multipoly.list.nogeo)
rem.circle.multipoly.df.nogeo <- as.data.frame(rem.circle.multipoly.list.final.nogeo)#[,c(2,1,4)] %>% distinct()
# binding the both circle lists back together 
rem.circle.one.edge.df.nogeo <- rbind(rem.circle.poly.df.nogeo, rem.circle.multipoly.df.nogeo)

# compare area calcaulted from non georef and georef data
area.diff.geo.nogeo <- edges.area.df%>% 
  rename(plot_ID = id) %>% 
  rename(e_ID = e_id)%>% left_join(., edges.area.df.nogeo %>% 
                                     filter(CCS_r_m == 17.84) %>% 
                                     rename(area_m2_nogeo = area_m2), 
                                   by = c("plot_ID", "e_ID")) %>% 
  mutate(diff_m2 = as.numeric(area_m2) - as.numeric(area_m2_nogeo))
summary(area.diff.geo.nogeo)



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

for (i in 1:length(unique(forest_edges_HBI.man.sub.2.edges.nogeo$plot_ID))){ 
  #i = 1
  # i = which(grepl(50080, unique(forest_edges_HBI.man.sub.2.edges$plot_ID)))
  
  # select plot ID of the respective circle 
  my.plot.id <- unique(forest_edges_HBI.man.sub.2.edges.nogeo$plot_ID)[i]
  
  # select the UTM coordiantes of the center of the cirlce corresponding with the plot ID
  # my.center.easting <- HBI_loc[HBI_loc$plot_ID == my.plot.id, "RW_MED"]
  # my.center.northing <- HBI_loc[HBI_loc$plot_ID == my.plot.id, "HW_MED"]
  
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
  
  ## select the  polygones the circle is intersected by
  # select the polygones with the same plot ID as the cirlce
  my.plot.polys.df <- edge.poly.df.nogeo %>% filter(plot_ID == my.plot.id) %>% arrange(e_ID)
  # create the polygones of the edge geometries
  my.poly.1 <- sf::st_as_sf(my.plot.polys.df[1,])
  my.poly.2 <- sf::st_as_sf(my.plot.polys.df[2,])
  
  my.e.id.1 <- my.plot.polys.df$e_ID[1]
  my.e.id.2 <- my.plot.polys.df$e_ID[2]
  
  my.e.form.1 <- my.plot.polys.df$e_form[1]
  my.e.form.2 <- my.plot.polys.df$e_form[2]
  
   # # print edges and circle
   # print(plot(circle.17$geometry), 
   #       plot(my.poly.1$geometry,  add = T), 
   #       plot(my.poly.2$geometry, add = T))
  
### 17m circle 
  my.circle = circle.17
  ## create poolygon of intersection for first polygon with circle
  inter.poly.17.1  <- st_intersection(my.circle, my.poly.1)
  inter.status.poly.1 <- ifelse(nrow(inter.poly.17.1) == 0, "no intersections",
                                ifelse(my.e.form.1 == 1 & inter.poly.17.1$geometry == my.circle$geometry,  "no intersections",
                                       ifelse(my.e.form.1 == 2 & inter.poly.17.1$geometry == my.circle$geometry, "fully covering circle", 
                                              "partly intersecting")))
  # if the first ednge covers all of the circle remaining its going to be set to 0 so we know there are no direct intersections and the circle is passed on to the next edge to calcualte the intersection
  # https://www.statology.org/r-argument-is-of-length-zero/
  inter.poly.17.1 <- if(isTRUE(inter.poly.17.1) && inter.poly.17.1$geometry == my.circle$geometry){inter.poly.17.1 <- data.frame()}else{inter.poly.17.1}
  
  ## create poolygon of remaining circle after first edge polygone is intersected
  # create poly with remaining area: https://gis.stackexchange.com/questions/353633/r-spatial-erase-one-polygon-from-another-correct-use-of-st-difference
  remaining.circle.17.1 <- if(nrow(inter.poly.17.1)==0){my.circle}else{sf::st_difference(my.circle, inter.poly.17.1)}
  print(plot(remaining.circle.17.1$geometry, main = paste0(my.plot.id, "-", my.poly.1$e_form))) 
  
  ## create polygone of intersecting area of second polygone with remaining circle
  inter.poly.17.2 <- st_intersection(remaining.circle.17.1, my.poly.2)
  inter.status.poly.2 <- ifelse(nrow(inter.poly.17.2) == 0, "no intersections",
                                ifelse(my.e.form.2== 1 & inter.poly.17.2$geometry == remaining.circle.17.1$geometry,  "no intersections",
                                       ifelse(my.e.form.2 == 2 & inter.poly.17.2$geometry == remaining.circle.17.1$geometry, "fully covering circle", 
                                              "partly intersecting")))
  # if the second edge covers all of the circle remaining its going to be set to 0 so we know there are no direct intersections
  # https://www.statology.org/r-argument-is-of-length-zero/
  inter.poly.17.2 <- if(isTRUE(inter.poly.17.2) && inter.poly.17.2$geometry == remaining.circle.17.1$geometry){inter.poly.17.2 <- data.frame()}else{inter.poly.17.2}
  
  ## create polygone of the  remaining cricle after both intersects are decucted
  # so the area of the frst remining circle minus the area of the second remaining circle 
  remaining.circle.17.1.and.2.poly <- if(nrow(inter.poly.17.2)==0){remaining.circle.17.1}else{sf::st_difference(remaining.circle.17.1, inter.poly.17.2)}
  print(plot(remaining.circle.17.1.and.2.poly$geometry, main = paste0(my.plot.id, "-", my.poly.2$e_form))) 
  
  
  ## calculate the area
  # area of the intersection 1
  inter.1.area <- ifelse(nrow(inter.poly.1) == 0, 0, sf::st_area(inter.poly.1))
  # area of the intersection polygone 2
  inter.2.area <- ifelse(nrow(inter.poly.2) == 0, 0, sf::st_area(inter.poly.2))
  #  area of the remaining circle, after both intersections are deducted
  remaining.circle.area <- sf::st_area(remaining.circle.1.and.2.poly)
  # save area in dataframe
  inter.area.df <- as.data.frame(
    cbind(
      "id" = c(my.plot.id, my.plot.id, my.plot.id), 
      "e_id" = c(my.poly.1$e_id, my.poly.2$e_id, 0), 
      "e_form" = c(my.poly.1$e_form, my.poly.2$e_form, 0),
      "shape" = c("edge", "edge", "circle"),
      "inter_stat" = c(inter.status.poly.1, inter.status.poly.2, 0),
      "area_m2" = c(inter.1.area, inter.2.area, remaining.circle.area)
    ))
  # save dataframe per plot in list
  edges.list.two.edges[[i]] <- inter.area.df
  
  
  ## save intersection polygones in list
  # poly.1
  inter.poly.1.list[[i]] <- if(nrow(inter.poly.1)!= 0){c("e_id" = my.poly.1$e_id, "id" = my.poly.1$id, "e_form" = my.poly.1$e_form, inter.poly.1)
  }else{c("e_id" = my.poly.1$e_id, "id" = my.poly.1$id, "e_form" = my.poly.1$e_form, my.poly.1)}
  # poly.2
  inter.poly.2.list[[i]] <- if(nrow(inter.poly.2)!= 0){c("e_id" = my.poly.2$e_id, "id" = my.poly.2$id, "e_form" = my.poly.2$e_form, inter.poly.2)
  }else{c("e_id" = my.poly.2$e_id, "id" = my.poly.2$id, "e_form" = my.poly.2$e_form, my.poly.2)}
  
  ## save the reimaingf circle polygones in a list
  # create list wit polygones of the remaining cirlce when it´s only one polygone
  rem.circle.poly.2.edges.list[[i]] <- if(st_geometry_type(remaining.circle.1.and.2.poly)== "POLYGON"){c("e_id" = 0, remaining.circle.1.and.2.poly)}else{}
  # create list wit polygones of the remaining cirlce when it´s a multipoligone
  rem.circle.multipoly.2.edges.list[[i]] <- if(st_geometry_type(remaining.circle.1.and.2.poly)== "MULTIPOLYGON"){c("e_id" = 0, remaining.circle.1.and.2.poly)}else{}
  
}

# save areas into dataframe
edges.list.two.edges.final <- rbindlist(edges.list.two.edges)
edges.area.two.edges.df <- as.data.frame(edges.list.two.edges.final)

# save intersection polygones into dataframe 
# list of polygones 1 of forest edges 
inter.poly.1.list.final <- rbindlist(inter.poly.1.list, fill=TRUE)
inter.poly.1.two.edges.df <- as.data.frame(inter.poly.1.list.final)[,c(2, 1, 3, 8)]
# list of polygones 2 of forest edges 
inter.poly.2.list.final <- rbindlist(inter.poly.2.list, fill=TRUE)
inter.poly.2.two.edges.df <- as.data.frame(inter.poly.2.list.final)[,c(2, 1, 3, 7)]
# bind the both edges per plot together
inter.poly.two.edges.df <- rbind(inter.poly.1.two.edges.df, inter.poly.2.two.edges.df) %>% arrange(id, e_id)

# list of polygones of remainign circles 
rem.circle.poly.two.edges.list.final <- rbindlist(rem.circle.poly.2.edges.list, fill = TRUE)
rem.circle.poly.two.edges.df <- as.data.frame(rem.circle.poly.two.edges.list.final)[,c(2,1,7)]  %>% distinct()
# list of multipolygones of remaining circles
rem.circle.multipoly.two.edges.list.final <- rbindlist(rem.circle.multipoly.2.edges.list)
rem.circle.multipoly.two.edges.df <- as.data.frame(rem.circle.multipoly.two.edges.list.final)[,c(2,1,15)] %>% distinct()
# binding the both circle lists back together 
rem.circle.two.edges.df <- if(nrow(rem.circle.poly.two.edges.df) != 0 && nrow(rem.circle.multipoly.two.edges.list.final) != 0){
  rbind(rem.circle.poly.two.edges.df, rem.circle.multipoly.two.edges.df)
}else{rem.circle.poly.two.edges.df}


all.edges.area.df <- rbind(edges.area.df, edges.area.two.edges.df)




# 3.2.1.4. sorting trees into edge and remaining circle polygones ---------

trees.one.edge <- HBI_trees %>%
  # filter only for trees that are located in plots with a forest edge
  semi_join(forest_edges_HBI.man %>% filter(e_form == 1 | e_form == 2) %>%
              #& inter_status_AT_17 == "two I" | e_form == 2 & inter_status_BT_17 == "two I") %>% 
              select(plot_ID) %>% distinct(), by = "plot_ID") %>% 
  # filter for trees located in plots htat haev only one forest edge
  anti_join(forest_edges_HBI.man %>% filter(e_form == 1 | e_form == 2 & inter_status_AT_17 == "two I" | e_form == 2 & inter_status_BT_17 == "two I") %>% group_by(plot_ID) %>% summarise(n = n()) %>% filter(n > 1) %>% select(plot_ID), by = "plot_ID") %>% 
  # remove plots that do now have a corresponding center coordiante in the HBI loc document
  semi_join(HBI_loc %>% filter(!is.na( RW_MED) & !is.na(HW_MED)) %>%  select(plot_ID)  %>% distinct(), by = "plot_ID")

tree.status.list <- vector("list", length = length(trees.one.edge$tree_ID))
tree.points.list <- vector("list", length = length(trees.one.edge$tree_ID))

for (i in 1:length(trees.one.edge$tree_ID)){ 
  #i = 1
  #i = which(grepl(50080, unique(trees.one.edge$plot_ID)))
  
  # select plot ID accordint to positioin in the list
  my.plot.id <- trees.one.edge[i, "plot_ID"] 
  my.tree.id <- trees.one.edge[i, "tree_ID"]
  
  # select the remaining cirlce we want to intersect the tree with
  my.rem.circle <- sf::st_as_sf(rem.circle.one.edge.df %>% filter(id == my.plot.id) %>% distinct())
  my.inter <- sf::st_as_sf(inter.poly.one.edge.df %>% filter(id == my.plot.id) %>% distinct())
  
  # assign crs
  my.utm.epsg <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"
  
  # select UTM corrdinates of the plot center
  my.center.easting <- HBI_loc[HBI_loc$plot_ID == my.plot.id, "RW_MED"]
  my.center.northing <- HBI_loc[HBI_loc$plot_ID == my.plot.id, "HW_MED"]
  
  # extract polar coordiantes of forest edge
  # point A 
  dist.tree <- trees.one.edge[i, "Dist_cm"]/100 
  azi.tree <- trees.one.edge[i, "azi_gon"] 
  x.tree <- dist.tree*sin(azi.tree)   # longitude, easting, RW, X
  y.tree <- dist.tree*cos(azi.tree)   # latitude, northing, HW, y 
  
  # transform polar into cartesian coordiantes
  tree.east <- my.center.easting + x.tree
  tree.north <- my.center.northing + y.tree
  
  # save cartesian coordiantes in dataframe
  tree.coord.df <- as.data.frame(cbind(
    "id" = c(my.plot.id), 
    "t_id" = c(my.tree.id),
    "lon" = c(tree.east),
    "lat" = c(tree.north)
  ))
  
  
  # create sf point object from dataframe
  #https://stackoverflow.com/questions/52551016/creating-sf-points-from-multiple-lat-longs
  tree.sf <-  sf::st_as_sf(tree.coord.df, coords = c("lon", "lat"), remove = FALSE)
  # assing CRS to points
  sf::st_crs(tree.sf) <- my.utm.epsg
  
  # print(plot(my.inter$geometry), 
  #       plot(my.rem.circle$geometry, add = T), 
  #       plot(tree.sf$geometry, add = T)
  #       )
  
  inter.tree.circle <- sf::st_intersection(tree.sf, my.rem.circle)
  inter.tree.edge <- sf::st_intersection(tree.sf, my.inter)
  
  tree_status <- ifelse(nrow(inter.tree.edge)!= 0, "B", 
                        ifelse(nrow(inter.tree.circle) != 0,  "A",
                               "warning"))
  
  tree.status.list[[i]] <- as.data.frame(cbind(
    "id" = c(my.plot.id), 
    "t_id" = c(my.tree.id),
    "lon" = c(tree.coord.df$lon),
    "lat" = c(tree.coord.df$lat),
    "t_stat" = c(tree_status))) 
  
  tree.points.list[[i]] <- c("t_stat" = tree_status, tree.sf)
  
  
}

# save tree corodiantes and status into dataframe
tree.status.list.one.edge.final <- rbindlist(tree.status.list)
tree.status.one.edge.df <- as.data.frame(tree.status.list.one.edge.final)
# save tree sf into dataframe
tree.points.list.one.edge.final <- rbindlist(tree.points.list)
tree.points.one.edge.df <- as.data.frame(tree.points.list.one.edge.final)





# intersection of trees with 2 edges
trees.two.edges <- HBI_trees %>%
  # filter only for trees that are located in plots with a forest edge
  semi_join(forest_edges_HBI.man %>% filter(e_form == 1 | e_form == 2) %>% 
              #& inter_status_AT_17 == "two I" | e_form == 2 & inter_status_BT_17 == "two I") %>% 
              select(plot_ID) %>% distinct(), by = "plot_ID") %>% 
  # filter for trees located in plots htat haev only one forest edge
  semi_join(forest_edges_HBI.man %>% filter(e_form == 1 | e_form == 2 & inter_status_AT_17 == "two I" | e_form == 2 & inter_status_BT_17 == "two I") %>% group_by(plot_ID) %>% summarise(n = n()) %>% filter(n > 1) %>% select(plot_ID), by = "plot_ID") %>% 
  # remove plots that do now have a corresponding center coordiante in the HBI loc document
  semi_join(HBI_loc %>% filter(!is.na( RW_MED) & !is.na(HW_MED)) %>%  select(plot_ID)  %>% distinct(), by = "plot_ID")

tree.status.two.edges.list <- vector("list", length = length(trees.two.edges$tree_ID))
tree.points.two.edges.list <- vector("list", length = length(trees.two.edges$tree_ID))

for (i in 1:length(trees.two.edges$tree_ID)){ 
  # i = 1
  #i = which(grepl(50080, (trees.two.edges$plot_ID)))
  
  # select plot ID accordint to positioin in the list
  my.plot.id <- trees.two.edges[i, "plot_ID"] 
  my.tree.id <- trees.two.edges[i, "tree_ID"]
  
  # select the remaining cirlce we want to intersect the tree with
  my.rem.circle <- sf::st_as_sf(rem.circle.two.edges.df %>% filter(id == my.plot.id) %>% distinct())
  my.edges.df <- inter.poly.two.edges.df %>% filter(id == my.plot.id) %>% distinct() %>% arrange(e_id)
  my.inter.1 <- sf::st_as_sf(my.edges.df[1,])
  my.inter.2 <- sf::st_as_sf(my.edges.df[2,])
  
  # assign crs
  my.utm.epsg <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"
  
  # select UTM corrdinates of the plot center
  my.center.easting <- HBI_loc[HBI_loc$plot_ID == my.plot.id, "RW_MED"]
  my.center.northing <- HBI_loc[HBI_loc$plot_ID == my.plot.id, "HW_MED"]
  
  # extract polar coordiantes of forest edge
  # point A 
  dist.tree <- trees.two.edges[i, "Dist_cm"]/100 
  azi.tree <- trees.two.edges[i, "azi_gon"] 
  x.tree <- dist.tree*sin(azi.tree)   # longitude, easting, RW, X
  y.tree <- dist.tree*cos(azi.tree)   # latitude, northing, HW, y 
  
  # transform polar into cartesian coordiantes
  tree.east <- my.center.easting + x.tree
  tree.north <- my.center.northing + y.tree
  
  # save cartesian coordiantes in dataframe
  tree.coord.df <- as.data.frame(cbind(
    "id" = c(my.plot.id), 
    "t_id" = c(my.tree.id),
    "lon" = c(tree.east),
    "lat" = c(tree.north)
  ))
  
  # create sf point object from dataframe
  #https://stackoverflow.com/questions/52551016/creating-sf-points-from-multiple-lat-longs
  tree.sf <-  sf::st_as_sf(tree.coord.df, coords = c("lon", "lat"), remove = FALSE)
  # assing CRS to points
  sf::st_crs(tree.sf) <- my.utm.epsg
  
  # print(plot(my.inter$geometry), 
  #       plot(my.rem.circle$geometry, add = T), 
  #       plot(tree.sf$geometry, add = T)
  #       )
  
  inter.tree.circle <- sf::st_intersection(tree.sf, my.rem.circle)
  inter.tree.edge.1 <- sf::st_intersection(tree.sf, my.inter.1)
  inter.tree.edge.2 <- sf::st_intersection(tree.sf, my.inter.2)
  
  tree_status <- ifelse(nrow(inter.tree.edge.1)!= 0 & nrow(inter.tree.edge.2)== 0 & nrow(inter.tree.circle)== 0,  "B", 
                        ifelse(nrow(inter.tree.edge.2)!= 0 & nrow(inter.tree.edge.1)== 0 & nrow(inter.tree.circle)== 0,  "C", 
                               ifelse(nrow(inter.tree.circle)!= 0 & nrow(inter.tree.edge.1)== 0 & nrow(inter.tree.edge.2)== 0,  "A",
                                      ifelse(nrow(inter.tree.circle)== 0 & nrow(inter.tree.edge.1)!= 0 & nrow(inter.tree.edge.2)!= 0,  "warning",
                                             "warning"))))
  
  tree.status.two.edges.list[[i]] <- as.data.frame(cbind(
    "id" = c(my.plot.id), 
    "t_id" = c(my.tree.id),
    "lon" = c(tree.coord.df$lon),
    "lat" = c(tree.coord.df$lat),
    "t_stat" = c(tree_status))) 
  
  tree.points.two.edges.list[[i]] <- c("t_stat" = tree_status, tree.sf)
  
  
}

# save tree corodiantes and status into dataframe
tree.status.list.two.edges.final <- rbindlist(tree.status.two.edges.list)
tree.status.two.edges.df <- as.data.frame(tree.status.list.two.edges.final)
# save tree sf into dataframe
tree.points.list.two.edges.final <- rbindlist(tree.points.two.edges.list)
tree.points.two.edges.df <- as.data.frame(tree.points.list.two.edges.final)

all.trees.points.df <- rbind(tree.points.one.edge.df,tree.points.two.edges.df) %>% distinct()







# -----1.1.3. loops based on area function over all 3 sampling circuits -----------------------
# -----1.1.3.1, loop based on area function over 17m circuit -----------------------
#edge.area <- function(e.form, dbh.cm, x.a, x.b, x.t, y.a, y.b, y.t, t.dist, tree_status, output){
# x1| y1 and x2|y2 belong to the intersections of the line or two points on a line
# c.x0, c.y0 are the center coordinates of the circle
# c.r0 is the radius of the circle
# l.b0, l.b1 are the parameters of the line we assign the edges for
#  xa, xb, xc, ya, yb, yc are the coordinates of the triangle corners that were used to identiy the "out" / "B" trees
# c.seg.a means the area of the cirle segment (circle bow) or the circle segmetns per CCS, c.a means the area if the whole circle
edge.area.func.17.list <- vector("list", length = length(forest_edges_HBI.man$plot_ID))

for (i in 1:length(forest_edges_HBI.man$plot_ID)){
  # i = 20
  
  p_id = forest_edges_HBI.man[i, "plot_ID"]
  e_id = forest_edges_HBI.man[i, "e_ID"]
  e.form = forest_edges_HBI.man[i, "e_form"]
  x.a = forest_edges_HBI.man$X_A[forest_edges_HBI.man$plot_ID == p_id & forest_edges_HBI.man$e_ID == e_id]
  x.b = forest_edges_HBI.man$X_B[forest_edges_HBI.man$plot_ID == p_id & forest_edges_HBI.man$e_ID == e_id]
  x.t = forest_edges_HBI.man$X_T[forest_edges_HBI.man$plot_ID == p_id & forest_edges_HBI.man$e_ID == e_id]
  y.a = forest_edges_HBI.man$Y_A[forest_edges_HBI.man$plot_ID == p_id & forest_edges_HBI.man$e_ID == e_id]
  y.b = forest_edges_HBI.man$Y_B[forest_edges_HBI.man$plot_ID == p_id & forest_edges_HBI.man$e_ID == e_id]
  y.t = forest_edges_HBI.man$Y_T[forest_edges_HBI.man$plot_ID == p_id & forest_edges_HBI.man$e_ID == e_id]
  t.dist= forest_edges_HBI.man$T_dist[forest_edges_HBI.man$plot_ID == p_id & forest_edges_HBI.man$e_ID == e_id]
  tree_status= "B"
  
  
  # select the diameter of the circle depending on the trees diameter
  c.x0 = 0;
  c.y0 = 0; 
  c.r0 =  17.84;
  
  ## calcualte slope and intercept of AT and BT line to calcualte intersections
  l.AB.b0 = ifelse(e.form == "1", intercept(x.a, y.a, x.b, y.b), NA);
  l.AB.b1 = ifelse(e.form == "1", slope(x.a, y.a, x.b, y.b), NA);
  l.AT.b0 = ifelse(e.form == "2", intercept(x.t, y.t, x.a, y.a), NA);
  l.AT.b1 = ifelse(e.form == "2", slope(x.t, y.t, x.a, y.a), NA);
  l.BT.b0 = ifelse(e.form == "2", intercept(x.t, y.t, x.b, y.b), NA);
  l.BT.b1 = ifelse(e.form == "2", slope(x.t, y.t, x.b, y.b), NA);
  # y = y-achsenabschnitt (intercept) + steigung (slope) * x
  
  ## calculate intersections between AB, AT and BT line with respective sampling circle
  # AB line
  x1.inter.AB <- intersection_line_circle(l.AB.b0, l.AB.b1, c.x0, c.y0, c.r0, coordinate = "x1");
  x2.inter.AB <- intersection_line_circle(l.AB.b0, l.AB.b1, c.x0, c.y0, c.r0, coordinate = "x2");
  y1.inter.AB <- intersection_line_circle(l.AB.b0, l.AB.b1, c.x0, c.y0, c.r0, coordinate = "y1");
  y2.inter.AB <- intersection_line_circle(l.AB.b0, l.AB.b1, c.x0, c.y0, c.r0, coordinate = "y2");
  # AT line
  x1.inter.AT <- intersection_line_circle(l.AT.b0, l.AT.b1, c.x0, c.y0, c.r0, coordinate = "x1");
  x2.inter.AT <- intersection_line_circle(l.AT.b0, l.AT.b1, c.x0, c.y0, c.r0, coordinate = "x2");
  y1.inter.AT <- intersection_line_circle(l.AT.b0, l.AT.b1, c.x0, c.y0, c.r0, coordinate = "y1");
  y2.inter.AT <- intersection_line_circle(l.AT.b0, l.AT.b1, c.x0, c.y0, c.r0, coordinate = "y2");        
  # BT line
  x1.inter.BT <- intersection_line_circle(l.BT.b0, l.BT.b1, c.x0, c.y0, c.r0, coordinate = "x1");
  x2.inter.BT <- intersection_line_circle(l.BT.b0, l.BT.b1, c.x0, c.y0, c.r0, coordinate = "x2");
  y1.inter.BT <- intersection_line_circle(l.BT.b0, l.BT.b1, c.x0, c.y0, c.r0, coordinate = "y1");
  y2.inter.BT <- intersection_line_circle(l.BT.b0, l.BT.b1, c.x0, c.y0, c.r0, coordinate = "y2"); 
  
  
  ## assign intersection status of AB, AT and BT lines with respective sampling circle
  i_status.AB <-   ifelse(is.na(x1.inter.AB) & is.na(x2.inter.AB), " no I",      # if 0 solutions
                          ifelse(!is.na(x1.inter.AB) & !is.na(x2.inter.AB) & x1.inter.AB == x2.inter.AB, "one I",            # if 1 solution
                                 ifelse(x1.inter.AB != x2.inter.AB, "two I")));      # so if the edge for is 1 and there are 2 interseections of the line with the respective circle 
  
  i_status.AT <-   ifelse(is.na(x1.inter.AT) & is.na(x2.inter.AT), " no I",      # if 0 solutions
                          ifelse(!is.na(x1.inter.AT) & !is.na(x2.inter.AT) & x1.inter.AT == x2.inter.AT, "one I",            # if 1 solution
                                 ifelse(x1.inter.AT != x2.inter.AT, "two I")));      # so if the edge for is 1 and there are 2 interseections of the line with the respective circle 
  
  i_status.BT <-   ifelse(is.na(x1.inter.BT) & is.na(x2.inter.BT), " no I",      # if 0 solutions
                          ifelse(!is.na(x1.inter.BT) & !is.na(x2.inter.BT) & x1.inter.BT == x2.inter.BT, "one I",            # if 1 solution
                                 ifelse(x1.inter.BT != x2.inter.BT, "two I")));      # so if the edge for is 1 and there are 2 interseections of the line with the respective circle
  
  ## build triangle with 60 circle to test if MC lies inside or not
  # select the intersection coordinates for the triangle on AT line
  x.AT.inter.triangle.60 = inter.for.triangle(l.AT.b0, l.AT.b1,  c.x0, c.y0, 300, x.a, y.a, x.t, y.t, coordinate = "x" );
  # calculate y for AT triangle
  y.AT.inter.triangle.60 = inter.for.triangle(l.AT.b0, l.AT.b1,  c.x0, c.y0, 300, x.a, y.a, x.t, y.t, coordinate = "y" );
  #BT 
  # select the intersection coordinates for the triangle on BT line
  x.BT.inter.triangle.60 = inter.for.triangle(l.BT.b0, l.BT.b1,  c.x0, c.y0, 300, x.b, y.b, x.t, y.t, coordinate = "x" );
  # calculate y for BT triangle
  y.BT.inter.triangle.60 = inter.for.triangle(l.BT.b0, l.BT.b1,  c.x0, c.y0, 300, x.b, y.b, x.t, y.t, coordinate = "y" );
  
  ## build circle segment
  # calculate intersections with sampling circle (17,12,5m)
  # if AT intersects the cirlce twice, but BT doesnt, x1 = AT_inter_1 and x2 = AT_inter_2
  # if AT intersects the cirlce twice, but BT doesnt, l.b0 = l.AT.b0 and l.b1 = l.AT.b1
  # if BT intersects the circle twice but AT doesn´t, x1 = BT_inter_1 and x2 = BT_inter_2
  # if BT intersects the cirlce twice, but BT doesnt, l.b0 = l.BT.b0 and l.b1 = l.BT.b1
  # if AB intersects the circle twice , x1 = AB_inter_1 and x2 = AB_inter_2, l.b0 = l.AB.b0 and l.b1 = l.AB.b1
  # if AT and BT intersect the cirlce twise we have to put B in a second variable
  # if T lies inside the circle, interA with the same direction as A to T is x1 and inter B with the same direction (azimute) as B to T is x2
  x1 = ifelse(e.form == "1" & i_status.AB == "two I", x1.inter.AB, 
              ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT != "two I", x1.inter.AT, 
                     ifelse(e.form == "2" & t.dist > c.r0 & i_status.BT == "two I" & i_status.AT != "two I", x1.inter.BT,
                            ifelse(e.form == "2" & t.dist <= c.r0 & i_status.AT == "two I" & i_status.BT == "two I", inter.for.triangle(l.AT.b0, l.AT.b1,  c.x0, c.y0, 300, x.a, y.a, x.t, y.t, coordinate = "x" ), 
                                   ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I", x1.inter.AT, 
                                          NA)))));
  x2 = ifelse(e.form == "1" & i_status.AB == "two I", x2.inter.AB, 
              ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT != "two I", x2.inter.AT, 
                     ifelse(e.form == "2" & t.dist > c.r0 & i_status.BT == "two I" & i_status.AT != "two I", x2.inter.BT,
                            ifelse(e.form == "2" & t.dist <= c.r0 & i_status.AT == "two I" & i_status.BT == "two I", inter.for.triangle(l.BT.b0, l.BT.b1,  c.x0, c.y0, 300, x.b, y.b, x.t, y.t, coordinate = "x" ), 
                                   ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I", x2.inter.AT, 
                                          NA)))));
  y1 = ifelse(e.form == "1" & i_status.AB == "two I", y1.inter.AB, 
              ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT != "two I", y1.inter.AT, 
                     ifelse(e.form == "2" & t.dist > c.r0 & i_status.BT == "two I" & i_status.AT != "two I", y1.inter.BT,
                            ifelse(e.form == "2" & t.dist <= c.r0 & i_status.AT == "two I" & i_status.BT == "two I", inter.for.triangle(l.AT.b0, l.AT.b1,  c.x0, c.y0, 300, x.a, y.a, x.t, y.t, coordinate = "y" ), 
                                   ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I", y1.inter.AT, 
                                          NA)))));
  y2 = ifelse(e.form == "1" & i_status.AB == "two I", y2.inter.AB, 
              ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT != "two I", y2.inter.AT, 
                     ifelse(e.form == "2" & t.dist > c.r0 & i_status.BT == "two I" & i_status.AT != "two I", y2.inter.BT,
                            ifelse(e.form == "2" & t.dist <= c.r0 & i_status.AT == "two I" & i_status.BT == "two I", inter.for.triangle(l.BT.b0, l.BT.b1,  c.x0, c.y0, 300, x.b, y.b, x.t, y.t, coordinate = "y" ), 
                                   ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I", y2.inter.AT, 
                                          NA)))));
  # create another intersection pair for circles that are intersected by both arms of the triangle  
  x.1.bsite = ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I", x1.inter.BT, NA);
  x.2.bsite = ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I", x2.inter.BT, NA);
  y.1.bsite = ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I", y1.inter.BT, NA);
  y.2.bsite = ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I", y2.inter.BT, NA);
  
  # circle segment on AB or AT or BT side
  # calculate angle between the lines from sampling cirlce intersections to center
  
  # calcualte circle segment area: 
  # if t is inside the circle we have to fraw a ne circle around T and the intersections by deductin tht distance between T to the center from the total radius of the circle
  c.cone.A = ifelse(e.form == 2 & t.dist <= c.r0 & i_status.AT == "two I" | 
                      e.form == 2 & t.dist <= c.r0 & i_status.BT == "two I" , triangle.circle.poly.intersection(x1, y1, x2, y2, x.t, y.t, c.r0),
                    ifelse(e.form == 2 & t.dist > c.r0 & i_status.AT == "two I" | 
                             e.form == 2 & t.dist > c.r0 & i_status.BT == "two I", cone.area(c.x0, c.y0, x1, y1, x2, y2, c.r0), NA)); 
  c.cone.A.bsite = ifelse(e.form == 2 & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I", 
                          cone.area(c.x0, c.y0,  x.1.bsite, y.1.bsite, x.2.bsite, y.2.bsite, c.r0), NA)
  
  # calcualte triangle between turning point or center of circle and the intersection with the respective cricle
  triangle.A.asite = ifelse(e.form == 2 &  t.dist > c.r0, triangle.area(c.x0, c.y0, x1, y1, x2, y2, method = "three.points"), NA)
  triangle.A.bsite = ifelse(e.form == 2 &  t.dist > c.r0  & i_status.AT == "two I" & i_status.BT == "two I", triangle.area(c.x0, c.y0, x.1.bsite, y.1.bsite, x.2.bsite, y.2.bsite, method = "three.points"), NA)
  
  # calculate circle segment trough withdrawing triangle from cone for edge form 2 where 
  circle.seg.A.asite = ifelse(e.form == 2 &  t.dist > c.r0, c.cone.A - triangle.A.asite, NA)
  circle.seg.A.bsite = ifelse(e.form == 2 &  t.dist > c.r0  & i_status.AT == "two I" & i_status.BT == "two I", c.cone.A.bsite -triangle.A.bsite, NA)
  
  circle.seg.A.e1 = ifelse(e.form == 1 & i_status.AB == "two I", CircleSegmnent(x1, y1, x2, y2, c.r0), 0)
  
  # calcualte circle area
  c.A = pi*c.r0^2;
  
  
  ## calculate coordiantes of the middle of thie line between 
  x_m_line = (x1 + x2)/2;
  y_m_line = (y1 + y2)/2;
  # calculate the parameters of the equation between the middle of the line and the centre of the circle
  b1_MC = slope(c.x0, c.y0, x_m_line, y_m_line);
  b0_MC = intercept(c.x0, c.y0, x_m_line, y_m_line);
  # calcualte the x corrdiante of the interception of the line between M and the centre of the cirle and the circle at the given radio
  X1_inter_MC = intersection_line_circle(b0_MC, b1_MC, c.x0, c.y0, c.r0, coordinate = "x1"); 
  X2_inter_MC = intersection_line_circle(b0_MC, b1_MC, c.x0, c.y0, c.r0, coordinate = "x2");
  # insert the intersection x corodinate in the line function to get the respective y coordinate
  y1_inter_MC = intersection_line_circle(b0_MC, b1_MC, c.x0, c.y0, c.r0, coordinate = "y1");
  y2_inter_MC = intersection_line_circle(b0_MC, b1_MC, c.x0, c.y0, c.r0, coordinate = "y2");
  
  # finde the x coordiante of the intersection that is within the triangle (p.in.tr)
  # if inter_1_MC or inter_MC_2 is element of the triangle and the distance between the intersection 
  # and the middle point of the line is greater then the distanc between the intersection that is outside the triangle and by that inside the plot 
  # deduct the circle segment from the whole plot area (because the larger part of the plot belongs to category B)
  # if the itnersection that is element to the triangle lies on the shorter side of the line, use the circle segment / circle bows area as the edge area
  
  # edge.1.A = ifelse(e.from == "1", c.seg.A, NA);
  ## return the area of the bigger or smaller circle segment, depending on which one of the both lies inside the triangle for edge form == 2 and only one arm intersecting the circle
  edge.2.line.A = ifelse(e.form == "2" &  t.dist > c.r0 & i_status.AT == "two I" & i_status.BT != "two I" & 
                           p.in.triangle(x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t, X1_inter_MC, y1_inter_MC) == "B" &
                           distance(X1_inter_MC, y1_inter_MC, x_m_line, y_m_line) > distance(X2_inter_MC, y2_inter_MC, x_m_line, y_m_line) |
                           
                           e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT != "two I" & 
                           p.in.triangle(x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t, X2_inter_MC, y2_inter_MC) == "B" &
                           distance(X2_inter_MC, y2_inter_MC, x_m_line, y_m_line) > distance(X1_inter_MC, y1_inter_MC, x_m_line, y_m_line)|
                           
                           e.form == "2" & t.dist > c.r0 & i_status.AT != "two I" & i_status.BT == "two I" & 
                           p.in.triangle(x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t, X1_inter_MC, y1_inter_MC) == "B" &
                           distance(X1_inter_MC, y1_inter_MC, x_m_line, y_m_line) > distance(X2_inter_MC, y2_inter_MC, x_m_line, y_m_line) |
                           
                           e.form == "2" & t.dist > c.r0 & i_status.AT != "two I" & i_status.BT == "two I" & 
                           p.in.triangle(x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t, X2_inter_MC, y2_inter_MC) == "B" &
                           distance(X2_inter_MC, y2_inter_MC, x_m_line, y_m_line) > distance(X1_inter_MC, y1_inter_MC, x_m_line, y_m_line),
                         c.A - circle.seg.A.asite, circle.seg.A.asite);
  
  # if the middle point is not inside the triangle, but the edge for is 2 and there are 2 intersections for both arms, while the turning point is outisde the circle, 
  # we have to calcualte the area on both sides of the lines and then deduct them from each other as they will both extend to the same circle segment
  edge.A.e2.center.not.in.triangle = ifelse(e.form == "2" &  t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I" & 
                                              p.in.triangle(x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t, c.x0, c.y0) == "A",
                                            abs(circle.seg.A.asite - circle.seg.A.bsite), NA) 
  
  # this is when the respective cirlce (could be also the inner cricle for edge type 1) doesn´t have intersections with the edge line but may still be located in the edge area
  # this is, however unlikely for edge type 1 because it assigns the edge area always to the smaller side of the circle so that a whole circle is unlikely to be inside of it
  # edge.whole.circle.A = ifelse(e.form == "2" & i_status.AT != "two I" & i_status.BT != "two I" & p.in.triangle(x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t, c.x0, c.y0) == "B", c.A, NA)
  
  # for edge form == 1 it´s always the circle segment, cause the trees in  
  edge.area = ifelse(e.form == "1" & tree_status == "B" & i_status.AB == "two I", circle.seg.A.e1, 
                     # if only one side of triangle is intersection 
                     ifelse(e.form == "2" & tree_status == "B" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT != "two I"|
                              e.form == "2" & tree_status == "B"&  t.dist > c.r0 & i_status.BT == "two I" & i_status.AT != "two I", edge.2.line.A,
                            # t is inside circle so whole cone is the edge area
                            ifelse(e.form == "2" & tree_status == "B"&  t.dist <= c.r0 & i_status.AT == "two I" & i_status.BT == "two I", c.cone.A, 
                                   # both arms of triangle cut circle so triangle area is between the both circle segments anf center of cirlce is inside triangle
                                   ifelse(e.form == "2" & tree_status == "B" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I" & p.in.triangle(x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t, c.x0, c.y0) == "B", c.A - (circle.seg.A.bsite + circle.seg.A.asite), 
                                          # if the middle point is not inside the triangle, but the edge for is 2 and there are 2 intersections for both arms, while the turning point is outisde the circle, 
                                          # we have to calcualte the area on both sides of the lines and then deduct them from each other as they will both extend to the same circle segment
                                          ifelse(e.form == "2" &  t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I" & p.in.triangle(x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t, c.x0, c.y0) == "A", edge.A.e2.center.not.in.triangle,
                                                 # this is when the respective cirlce (could be also the inner cricle for edge type 1) doesn´t have intersections with the edge line but may still be located in the edge area
                                                 # this is, however unlikely for edge type 1 because it assigns the edge area always to the smaller side of the circle so that a whole circle is unlikely to be inside of it
                                                 ifelse(e.form == "2" & i_status.AT != "two I" & i_status.BT != "two I" &  p.in.triangle(x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t, c.x0, c.y0) == "B", c.A,
                                                        #ifelse(e.form == "1" & tree_status == "B" & i_status.AB != "two I", c.A,
                                                        0))))));
  edge.method = ifelse(e.form == "1" & tree_status == "B" & i_status.AB == "two I", "CircleSeg_e1", 
                       # if only one side of triangle is intersection 
                       ifelse(e.form == "2" & tree_status == "B" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT != "two I"|
                                e.form == "2" & tree_status == "B"&  t.dist > c.r0 & i_status.BT == "two I" & i_status.AT != "two I", "e2_line_CirSeg_in_triangle",
                              # t is inside circle so whole cone is the edge area
                              ifelse(e.form == "2" & tree_status == "B"&  t.dist <= c.r0 & i_status.AT == "two I" & i_status.BT == "two I", "e2_cone", 
                                     # both arms of triangle cut circle so triangle area is between the both circle segments anf center of cirlce is inside triangle
                                     ifelse(e.form == "2" & tree_status == "B" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I" & p.in.triangle(x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t, c.x0, c.y0) == "B", "e2_aside_bside_minus_whole_cirlce", 
                                            # if the middle point is not inside the triangle, but the edge for is 2 and there are 2 intersections for both arms, while the turning point is outisde the circle, 
                                            # we have to calcualte the area on both sides of the lines and then deduct them from each other as they will both extend to the same circle segment
                                            ifelse(e.form == "2" &  t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I" & p.in.triangle(x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t, c.x0, c.y0) == "A", "e2.center.not.in.triangle_aside_minus_bside",
                                                   # this is when the respective cirlce (could be also the inner cricle for edge type 1) doesn´t have intersections with the edge line but may still be located in the edge area
                                                   # this is, however unlikely for edge type 1 because it assigns the edge area always to the smaller side of the circle so that a whole circle is unlikely to be inside of it
                                                   ifelse(e.form == "2" & i_status.AT != "two I" & i_status.BT != "two I" &  p.in.triangle(x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t, c.x0, c.y0) == "B", "whole_circle",
                                                          #ifelse(e.form == "1" & tree_status == "B" & i_status.AB != "two I", c.A,
                                                          "no edge area"))))))
  
  rem.circle.area = c.A - edge.area; 
  # there is a problem here because for plot with two edges, the remaining circle area will be reduced by the area of both edges, which the function cannot provide for now
  # thus it could be smarter to just get the edge area returned per plot and circle and then reduce the remaining area by the area of the respective edges
  area = ifelse(tree_status == "A" | is.na(e.form), rem.circle.area, edge.area);
  
  
  as.data.frame(cbind("id" = c(my.plot.id, my.plot.id),
                      "e_id" = c(my.e.id,  0),
                      "e_form" = c(my.e.form, 0),
                      "shape" = c("edge", "circle"),
                      "inter_stat" = c(inter.status.poly, 0),
                      "area_m2" = c(inter.area, remaining.circle.area)))
  # list with inter and remaining circle areas areas
  edges.list[[i]] <- inter.area.df
  
  edge.area.func.17.list[[i]] <- as.data.frame(cbind("plot_ID" = c(p_id), 
                                                     "e_ID" = c(e_id), 
                                                     "e_form" = c(e.form),
                                                     "e_area_m2" = c(edge.area)))
  
}

all.edge.area.func.17.list <- rbindlist(edge.area.func.17.list)
all.edge.area.func.17.df <- as.data.frame(all.edge.area.func.17.list)






# -----1.1.3.2. loop based on area function over 12m circuit -----------------------

edge.area.func.12.list <- vector("list", length = length(forest_edges_HBI.man$plot_ID))

for (i in 1:length(forest_edges_HBI.man$plot_ID)){
  # i = 20
  
  p_id = forest_edges_HBI.man[i, "plot_ID"]
  e_id = forest_edges_HBI.man[i, "e_ID"]
  e.form = forest_edges_HBI.man[i, "e_form"]
  x.a = forest_edges_HBI.man$X_A[forest_edges_HBI.man$plot_ID == p_id & forest_edges_HBI.man$e_ID == e_id]
  x.b = forest_edges_HBI.man$X_B[forest_edges_HBI.man$plot_ID == p_id & forest_edges_HBI.man$e_ID == e_id]
  x.t = forest_edges_HBI.man$X_T[forest_edges_HBI.man$plot_ID == p_id & forest_edges_HBI.man$e_ID == e_id]
  y.a = forest_edges_HBI.man$Y_A[forest_edges_HBI.man$plot_ID == p_id & forest_edges_HBI.man$e_ID == e_id]
  y.b = forest_edges_HBI.man$Y_B[forest_edges_HBI.man$plot_ID == p_id & forest_edges_HBI.man$e_ID == e_id]
  y.t = forest_edges_HBI.man$Y_T[forest_edges_HBI.man$plot_ID == p_id & forest_edges_HBI.man$e_ID == e_id]
  t.dist= forest_edges_HBI.man$T_dist[forest_edges_HBI.man$plot_ID == p_id & forest_edges_HBI.man$e_ID == e_id]
  tree_status= "B"
  
  
  # select the diameter of the circle depending on the trees diameter
  c.x0 = 0;
  c.y0 = 0; 
  c.r0 = 12.62;
  
  ## calcualte slope and intercept of AT and BT line to calcualte intersections
  l.AB.b0 = ifelse(e.form == "1", intercept(x.a, y.a, x.b, y.b), NA);
  l.AB.b1 = ifelse(e.form == "1", slope(x.a, y.a, x.b, y.b), NA);
  l.AT.b0 = ifelse(e.form == "2", intercept(x.t, y.t, x.a, y.a), NA);
  l.AT.b1 = ifelse(e.form == "2", slope(x.t, y.t, x.a, y.a), NA);
  l.BT.b0 = ifelse(e.form == "2", intercept(x.t, y.t, x.b, y.b), NA);
  l.BT.b1 = ifelse(e.form == "2", slope(x.t, y.t, x.b, y.b), NA);
  # y = y-achsenabschnitt (intercept) + steigung (slope) * x
  
  ## calculate intersections between AB, AT and BT line with respective sampling circle
  # AB line
  x1.inter.AB <- intersection_line_circle(l.AB.b0, l.AB.b1, c.x0, c.y0, c.r0, coordinate = "x1");
  x2.inter.AB <- intersection_line_circle(l.AB.b0, l.AB.b1, c.x0, c.y0, c.r0, coordinate = "x2");
  y1.inter.AB <- intersection_line_circle(l.AB.b0, l.AB.b1, c.x0, c.y0, c.r0, coordinate = "y1");
  y2.inter.AB <- intersection_line_circle(l.AB.b0, l.AB.b1, c.x0, c.y0, c.r0, coordinate = "y2");
  # AT line
  x1.inter.AT <- intersection_line_circle(l.AT.b0, l.AT.b1, c.x0, c.y0, c.r0, coordinate = "x1");
  x2.inter.AT <- intersection_line_circle(l.AT.b0, l.AT.b1, c.x0, c.y0, c.r0, coordinate = "x2");
  y1.inter.AT <- intersection_line_circle(l.AT.b0, l.AT.b1, c.x0, c.y0, c.r0, coordinate = "y1");
  y2.inter.AT <- intersection_line_circle(l.AT.b0, l.AT.b1, c.x0, c.y0, c.r0, coordinate = "y2");        
  # BT line
  x1.inter.BT <- intersection_line_circle(l.BT.b0, l.BT.b1, c.x0, c.y0, c.r0, coordinate = "x1");
  x2.inter.BT <- intersection_line_circle(l.BT.b0, l.BT.b1, c.x0, c.y0, c.r0, coordinate = "x2");
  y1.inter.BT <- intersection_line_circle(l.BT.b0, l.BT.b1, c.x0, c.y0, c.r0, coordinate = "y1");
  y2.inter.BT <- intersection_line_circle(l.BT.b0, l.BT.b1, c.x0, c.y0, c.r0, coordinate = "y2"); 
  
  
  ## assign intersection status of AB, AT and BT lines with respective sampling circle
  i_status.AB <-   ifelse(is.na(x1.inter.AB) & is.na(x2.inter.AB), " no I",      # if 0 solutions
                          ifelse(!is.na(x1.inter.AB) & !is.na(x2.inter.AB) & x1.inter.AB == x2.inter.AB, "one I",            # if 1 solution
                                 ifelse(x1.inter.AB != x2.inter.AB, "two I")));      # so if the edge for is 1 and there are 2 interseections of the line with the respective circle 
  
  i_status.AT <-   ifelse(is.na(x1.inter.AT) & is.na(x2.inter.AT), " no I",      # if 0 solutions
                          ifelse(!is.na(x1.inter.AT) & !is.na(x2.inter.AT) & x1.inter.AT == x2.inter.AT, "one I",            # if 1 solution
                                 ifelse(x1.inter.AT != x2.inter.AT, "two I")));      # so if the edge for is 1 and there are 2 interseections of the line with the respective circle 
  
  i_status.BT <-   ifelse(is.na(x1.inter.BT) & is.na(x2.inter.BT), " no I",      # if 0 solutions
                          ifelse(!is.na(x1.inter.BT) & !is.na(x2.inter.BT) & x1.inter.BT == x2.inter.BT, "one I",            # if 1 solution
                                 ifelse(x1.inter.BT != x2.inter.BT, "two I")));      # so if the edge for is 1 and there are 2 interseections of the line with the respective circle
  
  ## build triangle with 60 circle to test if MC lies inside or not
  # select the intersection coordinates for the triangle on AT line
  x.AT.inter.triangle.60 = inter.for.triangle(l.AT.b0, l.AT.b1,  c.x0, c.y0, 300, x.a, y.a, x.t, y.t, coordinate = "x" );
  # calculate y for AT triangle
  y.AT.inter.triangle.60 = inter.for.triangle(l.AT.b0, l.AT.b1,  c.x0, c.y0, 300, x.a, y.a, x.t, y.t, coordinate = "y" );
  #BT 
  # select the intersection coordinates for the triangle on BT line
  x.BT.inter.triangle.60 = inter.for.triangle(l.BT.b0, l.BT.b1,  c.x0, c.y0, 300, x.b, y.b, x.t, y.t, coordinate = "x" );
  # calculate y for BT triangle
  y.BT.inter.triangle.60 = inter.for.triangle(l.BT.b0, l.BT.b1,  c.x0, c.y0, 300, x.b, y.b, x.t, y.t, coordinate = "y" );
  
  ## build circle segment
  # calculate intersections with sampling circle (17,12,5m)
  # if AT intersects the cirlce twice, but BT doesnt, x1 = AT_inter_1 and x2 = AT_inter_2
  # if AT intersects the cirlce twice, but BT doesnt, l.b0 = l.AT.b0 and l.b1 = l.AT.b1
  # if BT intersects the circle twice but AT doesn´t, x1 = BT_inter_1 and x2 = BT_inter_2
  # if BT intersects the cirlce twice, but BT doesnt, l.b0 = l.BT.b0 and l.b1 = l.BT.b1
  # if AB intersects the circle twice , x1 = AB_inter_1 and x2 = AB_inter_2, l.b0 = l.AB.b0 and l.b1 = l.AB.b1
  # if AT and BT intersect the cirlce twise we have to put B in a second variable
  # if T lies inside the circle, interA with the same direction as A to T is x1 and inter B with the same direction (azimute) as B to T is x2
  x1 = ifelse(e.form == "1" & i_status.AB == "two I", x1.inter.AB, 
              ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT != "two I", x1.inter.AT, 
                     ifelse(e.form == "2" & t.dist > c.r0 & i_status.BT == "two I" & i_status.AT != "two I", x1.inter.BT,
                            ifelse(e.form == "2" & t.dist <= c.r0 & i_status.AT == "two I" & i_status.BT == "two I", inter.for.triangle(l.AT.b0, l.AT.b1,  c.x0, c.y0, 300, x.a, y.a, x.t, y.t, coordinate = "x" ), 
                                   ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I", x1.inter.AT, 
                                          NA)))));
  x2 = ifelse(e.form == "1" & i_status.AB == "two I", x2.inter.AB, 
              ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT != "two I", x2.inter.AT, 
                     ifelse(e.form == "2" & t.dist > c.r0 & i_status.BT == "two I" & i_status.AT != "two I", x2.inter.BT,
                            ifelse(e.form == "2" & t.dist <= c.r0 & i_status.AT == "two I" & i_status.BT == "two I", inter.for.triangle(l.BT.b0, l.BT.b1,  c.x0, c.y0, 300, x.b, y.b, x.t, y.t, coordinate = "x" ), 
                                   ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I", x2.inter.AT, 
                                          NA)))));
  y1 = ifelse(e.form == "1" & i_status.AB == "two I", y1.inter.AB, 
              ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT != "two I", y1.inter.AT, 
                     ifelse(e.form == "2" & t.dist > c.r0 & i_status.BT == "two I" & i_status.AT != "two I", y1.inter.BT,
                            ifelse(e.form == "2" & t.dist <= c.r0 & i_status.AT == "two I" & i_status.BT == "two I", inter.for.triangle(l.AT.b0, l.AT.b1,  c.x0, c.y0, 300, x.a, y.a, x.t, y.t, coordinate = "y" ), 
                                   ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I", y1.inter.AT, 
                                          NA)))));
  y2 = ifelse(e.form == "1" & i_status.AB == "two I", y2.inter.AB, 
              ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT != "two I", y2.inter.AT, 
                     ifelse(e.form == "2" & t.dist > c.r0 & i_status.BT == "two I" & i_status.AT != "two I", y2.inter.BT,
                            ifelse(e.form == "2" & t.dist <= c.r0 & i_status.AT == "two I" & i_status.BT == "two I", inter.for.triangle(l.BT.b0, l.BT.b1,  c.x0, c.y0, 300, x.b, y.b, x.t, y.t, coordinate = "y" ), 
                                   ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I", y2.inter.AT, 
                                          NA)))));
  # create another intersection pair for circles that are intersected by both arms of the triangle  
  x.1.bsite = ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I", x1.inter.BT, NA);
  x.2.bsite = ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I", x2.inter.BT, NA);
  y.1.bsite = ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I", y1.inter.BT, NA);
  y.2.bsite = ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I", y2.inter.BT, NA);
  
  # circle segment on AB or AT or BT side
  # calculate angle between the lines from sampling cirlce intersections to center
  
  # calcualte circle segment area: 
  # if t is inside the circle we have to fraw a ne circle around T and the intersections by deductin tht distance between T to the center from the total radius of the circle
  c.cone.A = ifelse(e.form == 2 & t.dist <= c.r0 & i_status.AT == "two I" | 
                      e.form == 2 & t.dist <= c.r0 & i_status.BT == "two I" , triangle.circle.poly.intersection(x1, y1, x2, y2, x.t, y.t, c.r0),
                    ifelse(e.form == 2 & t.dist > c.r0 & i_status.AT == "two I" | 
                             e.form == 2 & t.dist > c.r0 & i_status.BT == "two I", cone.area(c.x0, c.y0, x1, y1, x2, y2, c.r0), NA)); 
  c.cone.A.bsite = ifelse(e.form == 2 & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I", 
                          cone.area(c.x0, c.y0,  x.1.bsite, y.1.bsite, x.2.bsite, y.2.bsite, c.r0), NA)
  
  # calcualte triangle between turning point or center of circle and the intersection with the respective cricle
  triangle.A.asite = ifelse(e.form == 2 &  t.dist > c.r0, triangle.area(c.x0, c.y0, x1, y1, x2, y2, method = "three.points"), NA)
  triangle.A.bsite = ifelse(e.form == 2 &  t.dist > c.r0  & i_status.AT == "two I" & i_status.BT == "two I", triangle.area(c.x0, c.y0, x.1.bsite, y.1.bsite, x.2.bsite, y.2.bsite, method = "three.points"), NA)
  
  # calculate circle segment trough withdrawing triangle from cone for edge form 2 where 
  circle.seg.A.asite = ifelse(e.form == 2 &  t.dist > c.r0, c.cone.A - triangle.A.asite, NA)
  circle.seg.A.bsite = ifelse(e.form == 2 &  t.dist > c.r0  & i_status.AT == "two I" & i_status.BT == "two I", c.cone.A.bsite -triangle.A.bsite, NA)
  
  circle.seg.A.e1 = ifelse(e.form == 1 & i_status.AB == "two I", CircleSegmnent(x1, y1, x2, y2, c.r0), 0)
  
  # calcualte circle area
  c.A = pi*c.r0^2;
  
  
  ## calculate coordiantes of the middle of thie line between 
  x_m_line = (x1 + x2)/2;
  y_m_line = (y1 + y2)/2;
  # calculate the parameters of the equation between the middle of the line and the centre of the circle
  b1_MC = slope(c.x0, c.y0, x_m_line, y_m_line);
  b0_MC = intercept(c.x0, c.y0, x_m_line, y_m_line);
  # calcualte the x corrdiante of the interception of the line between M and the centre of the cirle and the circle at the given radio
  X1_inter_MC = intersection_line_circle(b0_MC, b1_MC, c.x0, c.y0, c.r0, coordinate = "x1"); 
  X2_inter_MC = intersection_line_circle(b0_MC, b1_MC, c.x0, c.y0, c.r0, coordinate = "x2");
  # insert the intersection x corodinate in the line function to get the respective y coordinate
  y1_inter_MC = intersection_line_circle(b0_MC, b1_MC, c.x0, c.y0, c.r0, coordinate = "y1");
  y2_inter_MC = intersection_line_circle(b0_MC, b1_MC, c.x0, c.y0, c.r0, coordinate = "y2");
  
  # finde the x coordiante of the intersection that is within the triangle (p.in.tr)
  # if inter_1_MC or inter_MC_2 is element of the triangle and the distance between the intersection 
  # and the middle point of the line is greater then the distanc between the intersection that is outside the triangle and by that inside the plot 
  # deduct the circle segment from the whole plot area (because the larger part of the plot belongs to category B)
  # if the itnersection that is element to the triangle lies on the shorter side of the line, use the circle segment / circle bows area as the edge area
  
  # edge.1.A = ifelse(e.from == "1", c.seg.A, NA);
  ## return the area of the bigger or smaller circle segment, depending on which one of the both lies inside the triangle for edge form == 2 and only one arm intersecting the circle
  edge.2.line.A = ifelse(e.form == "2" &  t.dist > c.r0 & i_status.AT == "two I" & i_status.BT != "two I" & 
                           p.in.triangle(x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t, X1_inter_MC, y1_inter_MC) == "B" &
                           distance(X1_inter_MC, y1_inter_MC, x_m_line, y_m_line) > distance(X2_inter_MC, y2_inter_MC, x_m_line, y_m_line) |
                           
                           e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT != "two I" & 
                           p.in.triangle(x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t, X2_inter_MC, y2_inter_MC) == "B" &
                           distance(X2_inter_MC, y2_inter_MC, x_m_line, y_m_line) > distance(X1_inter_MC, y1_inter_MC, x_m_line, y_m_line)|
                           
                           e.form == "2" & t.dist > c.r0 & i_status.AT != "two I" & i_status.BT == "two I" & 
                           p.in.triangle(x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t, X1_inter_MC, y1_inter_MC) == "B" &
                           distance(X1_inter_MC, y1_inter_MC, x_m_line, y_m_line) > distance(X2_inter_MC, y2_inter_MC, x_m_line, y_m_line) |
                           
                           e.form == "2" & t.dist > c.r0 & i_status.AT != "two I" & i_status.BT == "two I" & 
                           p.in.triangle(x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t, X2_inter_MC, y2_inter_MC) == "B" &
                           distance(X2_inter_MC, y2_inter_MC, x_m_line, y_m_line) > distance(X1_inter_MC, y1_inter_MC, x_m_line, y_m_line),
                         c.A - circle.seg.A.asite, circle.seg.A.asite);
  
  # if the middle point is not inside the triangle, but the edge for is 2 and there are 2 intersections for both arms, while the turning point is outisde the circle, 
  # we have to calcualte the area on both sides of the lines and then deduct them from each other as they will both extend to the same circle segment
  edge.A.e2.center.not.in.triangle = ifelse(e.form == "2" &  t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I" & 
                                              p.in.triangle(x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t, c.x0, c.y0) == "A",
                                            abs(circle.seg.A.asite - circle.seg.A.bsite), NA) 
  
  # this is when the respective cirlce (could be also the inner cricle for edge type 1) doesn´t have intersections with the edge line but may still be located in the edge area
  # this is, however unlikely for edge type 1 because it assigns the edge area always to the smaller side of the circle so that a whole circle is unlikely to be inside of it
  # edge.whole.circle.A = ifelse(e.form == "2" & i_status.AT != "two I" & i_status.BT != "two I" & p.in.triangle(x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t, c.x0, c.y0) == "B", c.A, NA)
  
  # for edge form == 1 it´s always the circle segment, cause the trees in  
  edge.area = ifelse(e.form == "1" & tree_status == "B" & i_status.AB == "two I", circle.seg.A.e1, 
                     # if only one side of triangle is intersection 
                     ifelse(e.form == "2" & tree_status == "B" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT != "two I"|
                              e.form == "2" & tree_status == "B"&  t.dist > c.r0 & i_status.BT == "two I" & i_status.AT != "two I", edge.2.line.A,
                            # t is inside circle so whole cone is the edge area
                            ifelse(e.form == "2" & tree_status == "B"&  t.dist <= c.r0 & i_status.AT == "two I" & i_status.BT == "two I", c.cone.A, 
                                   # both arms of triangle cut circle so triangle area is between the both circle segments anf center of cirlce is inside triangle
                                   ifelse(e.form == "2" & tree_status == "B" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I" & p.in.triangle(x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t, c.x0, c.y0) == "B", c.A - (circle.seg.A.bsite + circle.seg.A.asite), 
                                          # if the middle point is not inside the triangle, but the edge for is 2 and there are 2 intersections for both arms, while the turning point is outisde the circle, 
                                          # we have to calcualte the area on both sides of the lines and then deduct them from each other as they will both extend to the same circle segment
                                          ifelse(e.form == "2" &  t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I" & p.in.triangle(x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t, c.x0, c.y0) == "A", edge.A.e2.center.not.in.triangle,
                                                 # this is when the respective cirlce (could be also the inner cricle for edge type 1) doesn´t have intersections with the edge line but may still be located in the edge area
                                                 # this is, however unlikely for edge type 1 because it assigns the edge area always to the smaller side of the circle so that a whole circle is unlikely to be inside of it
                                                 ifelse(e.form == "2" & i_status.AT != "two I" & i_status.BT != "two I" &  p.in.triangle(x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t, c.x0, c.y0) == "B", c.A,
                                                        #ifelse(e.form == "1" & tree_status == "B" & i_status.AB != "two I", c.A,
                                                        0))))));
  edge.method = ifelse(e.form == "1" & tree_status == "B" & i_status.AB == "two I", "CircleSeg_e1", 
                       # if only one side of triangle is intersection 
                       ifelse(e.form == "2" & tree_status == "B" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT != "two I"|
                                e.form == "2" & tree_status == "B"&  t.dist > c.r0 & i_status.BT == "two I" & i_status.AT != "two I", "e2_line_CirSeg_in_triangle",
                              # t is inside circle so whole cone is the edge area
                              ifelse(e.form == "2" & tree_status == "B"&  t.dist <= c.r0 & i_status.AT == "two I" & i_status.BT == "two I", "e2_cone", 
                                     # both arms of triangle cut circle so triangle area is between the both circle segments anf center of cirlce is inside triangle
                                     ifelse(e.form == "2" & tree_status == "B" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I" & p.in.triangle(x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t, c.x0, c.y0) == "B", "e2_aside_bside_minus_whole_cirlce", 
                                            # if the middle point is not inside the triangle, but the edge for is 2 and there are 2 intersections for both arms, while the turning point is outisde the circle, 
                                            # we have to calcualte the area on both sides of the lines and then deduct them from each other as they will both extend to the same circle segment
                                            ifelse(e.form == "2" &  t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I" & p.in.triangle(x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t, c.x0, c.y0) == "A", "e2.center.not.in.triangle_aside_minus_bside",
                                                   # this is when the respective cirlce (could be also the inner cricle for edge type 1) doesn´t have intersections with the edge line but may still be located in the edge area
                                                   # this is, however unlikely for edge type 1 because it assigns the edge area always to the smaller side of the circle so that a whole circle is unlikely to be inside of it
                                                   ifelse(e.form == "2" & i_status.AT != "two I" & i_status.BT != "two I" &  p.in.triangle(x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t, c.x0, c.y0) == "B", "whole_circle",
                                                          #ifelse(e.form == "1" & tree_status == "B" & i_status.AB != "two I", c.A,
                                                          "no edge area"))))))
  
  rem.circle.area = c.A - edge.area; 
  # there is a problem here because for plot with two edges, the remaining circle area will be reduced by the area of both edges, which the function cannot provide for now
  # thus it could be smarter to just get the edge area returned per plot and circle and then reduce the remaining area by the area of the respective edges
  area = ifelse(tree_status == "A" | is.na(e.form), rem.circle.area, edge.area);
  
  
  as.data.frame(cbind("id" = c(my.plot.id, my.plot.id),
                      "e_id" = c(my.e.id,  0),
                      "e_form" = c(my.e.form, 0),
                      "shape" = c("edge", "circle"),
                      "inter_stat" = c(inter.status.poly, 0),
                      "area_m2" = c(inter.area, remaining.circle.area)))
  # list with inter and remaining circle areas areas
  edges.list[[i]] <- inter.area.df
  
  edge.area.func.12.list[[i]] <- as.data.frame(cbind("plot_ID" = c(p_id), 
                                                     "e_ID" = c(e_id), 
                                                     "e_form" = c(e.form),
                                                     "e_area_m2" = c(edge.area)))
  
}

all.edge.area.func.12.list <- rbindlist(edge.area.func.12.list)
all.edge.area.func.12.df <- as.data.frame(all.edge.area.func.12.list)







# -----1.1.3.2. loop based on area function over 5m circuit -----------------------
edge.area.func.5.list <- vector("list", length = length(forest_edges_HBI.man$plot_ID))

for (i in 1:length(forest_edges_HBI.man$plot_ID)){
  # i = 20
  
  p_id = forest_edges_HBI.man[i, "plot_ID"]
  e_id = forest_edges_HBI.man[i, "e_ID"]
  e.form = forest_edges_HBI.man[i, "e_form"]
  x.a = forest_edges_HBI.man$X_A[forest_edges_HBI.man$plot_ID == p_id & forest_edges_HBI.man$e_ID == e_id]
  x.b = forest_edges_HBI.man$X_B[forest_edges_HBI.man$plot_ID == p_id & forest_edges_HBI.man$e_ID == e_id]
  x.t = forest_edges_HBI.man$X_T[forest_edges_HBI.man$plot_ID == p_id & forest_edges_HBI.man$e_ID == e_id]
  y.a = forest_edges_HBI.man$Y_A[forest_edges_HBI.man$plot_ID == p_id & forest_edges_HBI.man$e_ID == e_id]
  y.b = forest_edges_HBI.man$Y_B[forest_edges_HBI.man$plot_ID == p_id & forest_edges_HBI.man$e_ID == e_id]
  y.t = forest_edges_HBI.man$Y_T[forest_edges_HBI.man$plot_ID == p_id & forest_edges_HBI.man$e_ID == e_id]
  t.dist= forest_edges_HBI.man$T_dist[forest_edges_HBI.man$plot_ID == p_id & forest_edges_HBI.man$e_ID == e_id]
  tree_status= "B"
  
  
  # select the diameter of the circle depending on the trees diameter
  c.x0 = 0;
  c.y0 = 0; 
  c.r0 = 5.64;
  
  ## calcualte slope and intercept of AT and BT line to calcualte intersections
  l.AB.b0 = ifelse(e.form == "1", intercept(x.a, y.a, x.b, y.b), NA);
  l.AB.b1 = ifelse(e.form == "1", slope(x.a, y.a, x.b, y.b), NA);
  l.AT.b0 = ifelse(e.form == "2", intercept(x.t, y.t, x.a, y.a), NA);
  l.AT.b1 = ifelse(e.form == "2", slope(x.t, y.t, x.a, y.a), NA);
  l.BT.b0 = ifelse(e.form == "2", intercept(x.t, y.t, x.b, y.b), NA);
  l.BT.b1 = ifelse(e.form == "2", slope(x.t, y.t, x.b, y.b), NA);
  # y = y-achsenabschnitt (intercept) + steigung (slope) * x
  
  ## calculate intersections between AB, AT and BT line with respective sampling circle
  # AB line
  x1.inter.AB <- intersection_line_circle(l.AB.b0, l.AB.b1, c.x0, c.y0, c.r0, coordinate = "x1");
  x2.inter.AB <- intersection_line_circle(l.AB.b0, l.AB.b1, c.x0, c.y0, c.r0, coordinate = "x2");
  y1.inter.AB <- intersection_line_circle(l.AB.b0, l.AB.b1, c.x0, c.y0, c.r0, coordinate = "y1");
  y2.inter.AB <- intersection_line_circle(l.AB.b0, l.AB.b1, c.x0, c.y0, c.r0, coordinate = "y2");
  # AT line
  x1.inter.AT <- intersection_line_circle(l.AT.b0, l.AT.b1, c.x0, c.y0, c.r0, coordinate = "x1");
  x2.inter.AT <- intersection_line_circle(l.AT.b0, l.AT.b1, c.x0, c.y0, c.r0, coordinate = "x2");
  y1.inter.AT <- intersection_line_circle(l.AT.b0, l.AT.b1, c.x0, c.y0, c.r0, coordinate = "y1");
  y2.inter.AT <- intersection_line_circle(l.AT.b0, l.AT.b1, c.x0, c.y0, c.r0, coordinate = "y2");        
  # BT line
  x1.inter.BT <- intersection_line_circle(l.BT.b0, l.BT.b1, c.x0, c.y0, c.r0, coordinate = "x1");
  x2.inter.BT <- intersection_line_circle(l.BT.b0, l.BT.b1, c.x0, c.y0, c.r0, coordinate = "x2");
  y1.inter.BT <- intersection_line_circle(l.BT.b0, l.BT.b1, c.x0, c.y0, c.r0, coordinate = "y1");
  y2.inter.BT <- intersection_line_circle(l.BT.b0, l.BT.b1, c.x0, c.y0, c.r0, coordinate = "y2"); 
  
  
  ## assign intersection status of AB, AT and BT lines with respective sampling circle
  i_status.AB <-   ifelse(is.na(x1.inter.AB) & is.na(x2.inter.AB), " no I",      # if 0 solutions
                          ifelse(!is.na(x1.inter.AB) & !is.na(x2.inter.AB) & x1.inter.AB == x2.inter.AB, "one I",            # if 1 solution
                                 ifelse(x1.inter.AB != x2.inter.AB, "two I")));      # so if the edge for is 1 and there are 2 interseections of the line with the respective circle 
  
  i_status.AT <-   ifelse(is.na(x1.inter.AT) & is.na(x2.inter.AT), " no I",      # if 0 solutions
                          ifelse(!is.na(x1.inter.AT) & !is.na(x2.inter.AT) & x1.inter.AT == x2.inter.AT, "one I",            # if 1 solution
                                 ifelse(x1.inter.AT != x2.inter.AT, "two I")));      # so if the edge for is 1 and there are 2 interseections of the line with the respective circle 
  
  i_status.BT <-   ifelse(is.na(x1.inter.BT) & is.na(x2.inter.BT), " no I",      # if 0 solutions
                          ifelse(!is.na(x1.inter.BT) & !is.na(x2.inter.BT) & x1.inter.BT == x2.inter.BT, "one I",            # if 1 solution
                                 ifelse(x1.inter.BT != x2.inter.BT, "two I")));      # so if the edge for is 1 and there are 2 interseections of the line with the respective circle
  
  ## build triangle with 60 circle to test if MC lies inside or not
  # select the intersection coordinates for the triangle on AT line
  x.AT.inter.triangle.60 = inter.for.triangle(l.AT.b0, l.AT.b1,  c.x0, c.y0, 300, x.a, y.a, x.t, y.t, coordinate = "x" );
  # calculate y for AT triangle
  y.AT.inter.triangle.60 = inter.for.triangle(l.AT.b0, l.AT.b1,  c.x0, c.y0, 300, x.a, y.a, x.t, y.t, coordinate = "y" );
  #BT 
  # select the intersection coordinates for the triangle on BT line
  x.BT.inter.triangle.60 = inter.for.triangle(l.BT.b0, l.BT.b1,  c.x0, c.y0, 300, x.b, y.b, x.t, y.t, coordinate = "x" );
  # calculate y for BT triangle
  y.BT.inter.triangle.60 = inter.for.triangle(l.BT.b0, l.BT.b1,  c.x0, c.y0, 300, x.b, y.b, x.t, y.t, coordinate = "y" );
  
  ## build circle segment
  # calculate intersections with sampling circle (17,12,5m)
  # if AT intersects the cirlce twice, but BT doesnt, x1 = AT_inter_1 and x2 = AT_inter_2
  # if AT intersects the cirlce twice, but BT doesnt, l.b0 = l.AT.b0 and l.b1 = l.AT.b1
  # if BT intersects the circle twice but AT doesn´t, x1 = BT_inter_1 and x2 = BT_inter_2
  # if BT intersects the cirlce twice, but BT doesnt, l.b0 = l.BT.b0 and l.b1 = l.BT.b1
  # if AB intersects the circle twice , x1 = AB_inter_1 and x2 = AB_inter_2, l.b0 = l.AB.b0 and l.b1 = l.AB.b1
  # if AT and BT intersect the cirlce twise we have to put B in a second variable
  # if T lies inside the circle, interA with the same direction as A to T is x1 and inter B with the same direction (azimute) as B to T is x2
  x1 = ifelse(e.form == "1" & i_status.AB == "two I", x1.inter.AB, 
              ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT != "two I", x1.inter.AT, 
                     ifelse(e.form == "2" & t.dist > c.r0 & i_status.BT == "two I" & i_status.AT != "two I", x1.inter.BT,
                            ifelse(e.form == "2" & t.dist <= c.r0 & i_status.AT == "two I" & i_status.BT == "two I", inter.for.triangle(l.AT.b0, l.AT.b1,  c.x0, c.y0, 300, x.a, y.a, x.t, y.t, coordinate = "x" ), 
                                   ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I", x1.inter.AT, 
                                          NA)))));
  x2 = ifelse(e.form == "1" & i_status.AB == "two I", x2.inter.AB, 
              ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT != "two I", x2.inter.AT, 
                     ifelse(e.form == "2" & t.dist > c.r0 & i_status.BT == "two I" & i_status.AT != "two I", x2.inter.BT,
                            ifelse(e.form == "2" & t.dist <= c.r0 & i_status.AT == "two I" & i_status.BT == "two I", inter.for.triangle(l.BT.b0, l.BT.b1,  c.x0, c.y0, 300, x.b, y.b, x.t, y.t, coordinate = "x" ), 
                                   ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I", x2.inter.AT, 
                                          NA)))));
  y1 = ifelse(e.form == "1" & i_status.AB == "two I", y1.inter.AB, 
              ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT != "two I", y1.inter.AT, 
                     ifelse(e.form == "2" & t.dist > c.r0 & i_status.BT == "two I" & i_status.AT != "two I", y1.inter.BT,
                            ifelse(e.form == "2" & t.dist <= c.r0 & i_status.AT == "two I" & i_status.BT == "two I", inter.for.triangle(l.AT.b0, l.AT.b1,  c.x0, c.y0, 300, x.a, y.a, x.t, y.t, coordinate = "y" ), 
                                   ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I", y1.inter.AT, 
                                          NA)))));
  y2 = ifelse(e.form == "1" & i_status.AB == "two I", y2.inter.AB, 
              ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT != "two I", y2.inter.AT, 
                     ifelse(e.form == "2" & t.dist > c.r0 & i_status.BT == "two I" & i_status.AT != "two I", y2.inter.BT,
                            ifelse(e.form == "2" & t.dist <= c.r0 & i_status.AT == "two I" & i_status.BT == "two I", inter.for.triangle(l.BT.b0, l.BT.b1,  c.x0, c.y0, 300, x.b, y.b, x.t, y.t, coordinate = "y" ), 
                                   ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I", y2.inter.AT, 
                                          NA)))));
  # create another intersection pair for circles that are intersected by both arms of the triangle  
  x.1.bsite = ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I", x1.inter.BT, NA);
  x.2.bsite = ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I", x2.inter.BT, NA);
  y.1.bsite = ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I", y1.inter.BT, NA);
  y.2.bsite = ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I", y2.inter.BT, NA);
  
  # circle segment on AB or AT or BT side
  # calculate angle between the lines from sampling cirlce intersections to center
  
  # calcualte circle segment area: 
  # if t is inside the circle we have to fraw a ne circle around T and the intersections by deductin tht distance between T to the center from the total radius of the circle
  c.cone.A = ifelse(e.form == 2 & t.dist <= c.r0 & i_status.AT == "two I" | 
                      e.form == 2 & t.dist <= c.r0 & i_status.BT == "two I" , triangle.circle.poly.intersection(x1, y1, x2, y2, x.t, y.t, c.r0),
                    ifelse(e.form == 2 & t.dist > c.r0 & i_status.AT == "two I" | 
                             e.form == 2 & t.dist > c.r0 & i_status.BT == "two I", cone.area(c.x0, c.y0, x1, y1, x2, y2, c.r0), NA)); 
  c.cone.A.bsite = ifelse(e.form == 2 & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I", 
                          cone.area(c.x0, c.y0,  x.1.bsite, y.1.bsite, x.2.bsite, y.2.bsite, c.r0), NA)
  
  # calcualte triangle between turning point or center of circle and the intersection with the respective cricle
  triangle.A.asite = ifelse(e.form == 2 &  t.dist > c.r0, triangle.area(c.x0, c.y0, x1, y1, x2, y2, method = "three.points"), NA)
  triangle.A.bsite = ifelse(e.form == 2 &  t.dist > c.r0  & i_status.AT == "two I" & i_status.BT == "two I", triangle.area(c.x0, c.y0, x.1.bsite, y.1.bsite, x.2.bsite, y.2.bsite, method = "three.points"), NA)
  
  # calculate circle segment trough withdrawing triangle from cone for edge form 2 where 
  circle.seg.A.asite = ifelse(e.form == 2 &  t.dist > c.r0, c.cone.A - triangle.A.asite, NA)
  circle.seg.A.bsite = ifelse(e.form == 2 &  t.dist > c.r0  & i_status.AT == "two I" & i_status.BT == "two I", c.cone.A.bsite -triangle.A.bsite, NA)
  
  circle.seg.A.e1 = ifelse(e.form == 1 & i_status.AB == "two I", CircleSegmnent(x1, y1, x2, y2, c.r0), 0)
  
  # calcualte circle area
  c.A = pi*c.r0^2;
  
  
  ## calculate coordiantes of the middle of thie line between 
  x_m_line = (x1 + x2)/2;
  y_m_line = (y1 + y2)/2;
  # calculate the parameters of the equation between the middle of the line and the centre of the circle
  b1_MC = slope(c.x0, c.y0, x_m_line, y_m_line);
  b0_MC = intercept(c.x0, c.y0, x_m_line, y_m_line);
  # calcualte the x corrdiante of the interception of the line between M and the centre of the cirle and the circle at the given radio
  X1_inter_MC = intersection_line_circle(b0_MC, b1_MC, c.x0, c.y0, c.r0, coordinate = "x1"); 
  X2_inter_MC = intersection_line_circle(b0_MC, b1_MC, c.x0, c.y0, c.r0, coordinate = "x2");
  # insert the intersection x corodinate in the line function to get the respective y coordinate
  y1_inter_MC = intersection_line_circle(b0_MC, b1_MC, c.x0, c.y0, c.r0, coordinate = "y1");
  y2_inter_MC = intersection_line_circle(b0_MC, b1_MC, c.x0, c.y0, c.r0, coordinate = "y2");
  
  # finde the x coordiante of the intersection that is within the triangle (p.in.tr)
  # if inter_1_MC or inter_MC_2 is element of the triangle and the distance between the intersection 
  # and the middle point of the line is greater then the distanc between the intersection that is outside the triangle and by that inside the plot 
  # deduct the circle segment from the whole plot area (because the larger part of the plot belongs to category B)
  # if the itnersection that is element to the triangle lies on the shorter side of the line, use the circle segment / circle bows area as the edge area
  
  # edge.1.A = ifelse(e.from == "1", c.seg.A, NA);
  ## return the area of the bigger or smaller circle segment, depending on which one of the both lies inside the triangle for edge form == 2 and only one arm intersecting the circle
  edge.2.line.A = ifelse(e.form == "2" &  t.dist > c.r0 & i_status.AT == "two I" & i_status.BT != "two I" & 
                           p.in.triangle(x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t, X1_inter_MC, y1_inter_MC) == "B" &
                           distance(X1_inter_MC, y1_inter_MC, x_m_line, y_m_line) > distance(X2_inter_MC, y2_inter_MC, x_m_line, y_m_line) |
                           
                           e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT != "two I" & 
                           p.in.triangle(x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t, X2_inter_MC, y2_inter_MC) == "B" &
                           distance(X2_inter_MC, y2_inter_MC, x_m_line, y_m_line) > distance(X1_inter_MC, y1_inter_MC, x_m_line, y_m_line)|
                           
                           e.form == "2" & t.dist > c.r0 & i_status.AT != "two I" & i_status.BT == "two I" & 
                           p.in.triangle(x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t, X1_inter_MC, y1_inter_MC) == "B" &
                           distance(X1_inter_MC, y1_inter_MC, x_m_line, y_m_line) > distance(X2_inter_MC, y2_inter_MC, x_m_line, y_m_line) |
                           
                           e.form == "2" & t.dist > c.r0 & i_status.AT != "two I" & i_status.BT == "two I" & 
                           p.in.triangle(x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t, X2_inter_MC, y2_inter_MC) == "B" &
                           distance(X2_inter_MC, y2_inter_MC, x_m_line, y_m_line) > distance(X1_inter_MC, y1_inter_MC, x_m_line, y_m_line),
                         c.A - circle.seg.A.asite, circle.seg.A.asite);
  
  # if the middle point is not inside the triangle, but the edge for is 2 and there are 2 intersections for both arms, while the turning point is outisde the circle, 
  # we have to calcualte the area on both sides of the lines and then deduct them from each other as they will both extend to the same circle segment
  edge.A.e2.center.not.in.triangle = ifelse(e.form == "2" &  t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I" & 
                                              p.in.triangle(x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t, c.x0, c.y0) == "A",
                                            abs(circle.seg.A.asite - circle.seg.A.bsite), NA) 
  
  # this is when the respective cirlce (could be also the inner cricle for edge type 1) doesn´t have intersections with the edge line but may still be located in the edge area
  # this is, however unlikely for edge type 1 because it assigns the edge area always to the smaller side of the circle so that a whole circle is unlikely to be inside of it
  # edge.whole.circle.A = ifelse(e.form == "2" & i_status.AT != "two I" & i_status.BT != "two I" & p.in.triangle(x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t, c.x0, c.y0) == "B", c.A, NA)
  
  # for edge form == 1 it´s always the circle segment, cause the trees in  
  edge.area = ifelse(e.form == "1" & tree_status == "B" & i_status.AB == "two I", circle.seg.A.e1, 
                     # if only one side of triangle is intersection 
                     ifelse(e.form == "2" & tree_status == "B" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT != "two I"|
                              e.form == "2" & tree_status == "B"&  t.dist > c.r0 & i_status.BT == "two I" & i_status.AT != "two I", edge.2.line.A,
                            # t is inside circle so whole cone is the edge area
                            ifelse(e.form == "2" & tree_status == "B"&  t.dist <= c.r0 & i_status.AT == "two I" & i_status.BT == "two I", c.cone.A, 
                                   # both arms of triangle cut circle so triangle area is between the both circle segments anf center of cirlce is inside triangle
                                   ifelse(e.form == "2" & tree_status == "B" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I" & p.in.triangle(x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t, c.x0, c.y0) == "B", c.A - (circle.seg.A.bsite + circle.seg.A.asite), 
                                          # if the middle point is not inside the triangle, but the edge for is 2 and there are 2 intersections for both arms, while the turning point is outisde the circle, 
                                          # we have to calcualte the area on both sides of the lines and then deduct them from each other as they will both extend to the same circle segment
                                          ifelse(e.form == "2" &  t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I" & p.in.triangle(x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t, c.x0, c.y0) == "A", edge.A.e2.center.not.in.triangle,
                                                 # this is when the respective cirlce (could be also the inner cricle for edge type 1) doesn´t have intersections with the edge line but may still be located in the edge area
                                                 # this is, however unlikely for edge type 1 because it assigns the edge area always to the smaller side of the circle so that a whole circle is unlikely to be inside of it
                                                 ifelse(e.form == "2" & i_status.AT != "two I" & i_status.BT != "two I" &  p.in.triangle(x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t, c.x0, c.y0) == "B", c.A,
                                                        #ifelse(e.form == "1" & tree_status == "B" & i_status.AB != "two I", c.A,
                                                        0))))));
  edge.method = ifelse(e.form == "1" & tree_status == "B" & i_status.AB == "two I", "CircleSeg_e1", 
                       # if only one side of triangle is intersection 
                       ifelse(e.form == "2" & tree_status == "B" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT != "two I"|
                                e.form == "2" & tree_status == "B"&  t.dist > c.r0 & i_status.BT == "two I" & i_status.AT != "two I", "e2_line_CirSeg_in_triangle",
                              # t is inside circle so whole cone is the edge area
                              ifelse(e.form == "2" & tree_status == "B"&  t.dist <= c.r0 & i_status.AT == "two I" & i_status.BT == "two I", "e2_cone", 
                                     # both arms of triangle cut circle so triangle area is between the both circle segments anf center of cirlce is inside triangle
                                     ifelse(e.form == "2" & tree_status == "B" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I" & p.in.triangle(x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t, c.x0, c.y0) == "B", "e2_aside_bside_minus_whole_cirlce", 
                                            # if the middle point is not inside the triangle, but the edge for is 2 and there are 2 intersections for both arms, while the turning point is outisde the circle, 
                                            # we have to calcualte the area on both sides of the lines and then deduct them from each other as they will both extend to the same circle segment
                                            ifelse(e.form == "2" &  t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I" & p.in.triangle(x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t, c.x0, c.y0) == "A", "e2.center.not.in.triangle_aside_minus_bside",
                                                   # this is when the respective cirlce (could be also the inner cricle for edge type 1) doesn´t have intersections with the edge line but may still be located in the edge area
                                                   # this is, however unlikely for edge type 1 because it assigns the edge area always to the smaller side of the circle so that a whole circle is unlikely to be inside of it
                                                   ifelse(e.form == "2" & i_status.AT != "two I" & i_status.BT != "two I" &  p.in.triangle(x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t, c.x0, c.y0) == "B", "whole_circle",
                                                          #ifelse(e.form == "1" & tree_status == "B" & i_status.AB != "two I", c.A,
                                                          "no edge area"))))))
  
  rem.circle.area = c.A - edge.area; 
  # there is a problem here because for plot with two edges, the remaining circle area will be reduced by the area of both edges, which the function cannot provide for now
  # thus it could be smarter to just get the edge area returned per plot and circle and then reduce the remaining area by the area of the respective edges
  area = ifelse(tree_status == "A" | is.na(e.form), rem.circle.area, edge.area);
  
  
  as.data.frame(cbind("id" = c(my.plot.id, my.plot.id),
                      "e_id" = c(my.e.id,  0),
                      "e_form" = c(my.e.form, 0),
                      "shape" = c("edge", "circle"),
                      "inter_stat" = c(inter.status.poly, 0),
                      "area_m2" = c(inter.area, remaining.circle.area)))
  # list with inter and remaining circle areas areas
  edges.list[[i]] <- inter.area.df
  
  edge.area.func.5.list[[i]] <- as.data.frame(cbind("plot_ID" = c(p_id), 
                                                    "e_ID" = c(e_id), 
                                                    "e_form" = c(e.form),
                                                    "e_area_m2" = c(edge.area)))
  
}

all.edge.area.func.5.list <- rbindlist(edge.area.func.5.list)
all.edge.area.func.5.df <- as.data.frame(all.edge.area.func.5.list)




forest_edges_HBI.man.17.sub.2.edges.func <- forest_edges_HBI.man %>% 
  semi_join(forest_edges_HBI.man %>% filter(e_form == 1 | e_form == 2 & inter_status_AT_17 == "two I" | e_form == 2 & inter_status_BT_17 == "two I") %>% group_by(plot_ID) %>% summarise(n = n()) %>% filter(n > 1) %>% select(plot_ID), by = "plot_ID") # 14 plots iwth 2 edges --> 28 rows

for (i in 1:length(unique(forest_edges_HBI.man.17.sub.2.edges.func$plot_ID))) {
  # i = 1
  # 
  #i = which(grepl(50075, unique(forest_edges_HBI.man.17.sub.2.edges.func$plot_ID)))
  
  p_id = unique(forest_edges_HBI.man.17.sub.2.edges.func$plot_ID)[i]
  my.edges.df <- all.edge.area.func.17.df %>% filter(plot_ID == p_id) %>% arrange(e_ID)
  my.edge.1.A <- my.edges.df[1,4]
  my.edge.2.A <- my.edges.df[2,4]
  
  # select the circle polygone corresponding with the plot ID
  my.circle.17 = 17.84^2*pi
  my.circle.12 = 12.62^2*pi
  my.circle.5 = 5.64^2*pi
  
  #calcualte remaining circles seperately
  my.rem.circle.1.17 = my.circle.17 - my.edge.1.A
  my.rem.circle.2.17 = my.circle.17 - my.edge.2.A
  
  my.edge.2.A-19
  my.rem.circle.2.17-19
  
  
  
}














jeremy.test.df <- rbind(forest_edges_HBI.man %>%
                          filter(e_form == 2 ) %>%
                          mutate(id_func = row_number()) %>%
                          group_by(id_func) %>%
                          # test the edge form 2 function
                          mutate(jeremy.test = triangle.circle.poly.intersection(
                            inter.for.triangle(b0_AT, b1_AT, data_circle$x0[3], data_circle$y0[3], data_circle$rmax[3]*10 , X_A, Y_A, X_T, Y_T, coordinate = "x"), 
                            inter.for.triangle(b0_AT, b1_AT, data_circle$x0[3], data_circle$y0[3], data_circle$rmax[3]*10 , X_A, Y_A, X_T, Y_T, coordinate = "y"),
                            inter.for.triangle(b0_BT, b1_BT, data_circle$x0[3], data_circle$y0[3], data_circle$rmax[3]*10 , X_B, Y_B, X_T, Y_T, coordinate = "x"), 
                            inter.for.triangle(b0_BT, b1_BT, data_circle$x0[3], data_circle$y0[3], data_circle$rmax[3]*10 , X_B, Y_B, X_T, Y_T, coordinate = "y"),
                            X_T, Y_T, 
                            17.84
                          ))%>% 
                          ungroup() %>% 
                          select(plot_ID, e_ID, e_form,inter_status_AB_17,  inter_status_AT_17, inter_status_BT_17, jeremy.test), 
                        # test the CircleSegment function 
                        forest_edges_HBI.man  %>%
                          filter(e_form == 1) %>% 
                          mutate(id_func = row_number()) %>%
                          group_by(id_func) %>%
                          mutate(jeremy.test = CircleSegmnent(X1_inter_AB_17, Y1_inter_AB_17, X2_inter_AB_17, Y2_inter_AB_17, 17.84))%>%
                          ungroup() %>% 
                          select(plot_ID, e_ID, e_form, inter_status_AB_17,  inter_status_AT_17, inter_status_BT_17, jeremy.test)) %>% 
  # check area function 
  left_join(., 
            forest_edges_HBI.man %>% 
              mutate(id_func = row_number()) %>%
              group_by(id_func) %>%
              mutate(area.func.test = edge.A(e_form, 35, X_A, X_B, X_T, Y_A, Y_B, Y_T, T_dist, "B", output = "area_m2")) %>% 
              ungroup() %>%
              select(plot_ID, e_ID, e_form, area.func.test), 
            by = c("plot_ID", "e_ID", "e_form")) %>%
  # check georef results
  left_join(., 
            all.edges.area.df %>% 
              mutate(id = as.integer(id),
                     e_id = as.integer(e_id),
                     e_form = as.integer(e_form), 
                     area_m2 = as.numeric(area_m2)) %>% 
              select(id, e_id, e_form, area_m2), 
            by = c(c("plot_ID" = "id"), c("e_ID"="e_id"), "e_form")) %>% 
  left_join(., 
            all.edge.area.func.17.df, 
            by = c("plot_ID", "e_ID", "e_form")) %>% 
  mutate(diff_jeremy.circleseg_vs_area.func = jeremy.test - area.func.test, 
         diff_area.func_vs_georef = area.func.test - area_m2, 
         diff_loop_area_func = area.func.test - e_area_m2) %>% 
  mutate(warn_jeremy.circleseg_vs_area.func = ifelse(jeremy.test != area.func.test, "WARN", "FINE"), 
         #warn_jeremy.circleseg_vs_georef = ifelse(jeremy.test != area_m2, "WARN", "FINE"), 
         warn_area.func_vs_georef = ifelse( area.func.test != area_m2, "WARN", "FINE"))





summary(jeremy.test.df)

print(jeremy.test.df%>% 
        filter(warn_area.func_vs_georef == "WARN" & diff_area.func_vs_georef > 15 |
                 warn_area.func_vs_georef == "WARN" & diff_area.func_vs_georef < -15), n = 62)

jeremy.test.df%>% filter(plot_ID == 50080)
colnames(one.edge.area.df)
colnames(two.edges.area.df)

view(rbind(one.edge.area.df, 
      two.edges.area.df %>% select(-(inter_stat))) %>% filter(shape == "edge" & CCS_radius == 17.84) %>% 
# check georef results
left_join(., 
          all.edges.area.df %>% 
            mutate(#id = as.integer(id),
                   #e_id = as.integer(e_id),
                   #e_form = as.integer(e_form), 
                   area_m2_nongeo = as.numeric(area_m2)) %>% 
            select(id, e_id, e_form, area_m2_nongeo), 
          by = c(c("plot_ID" = "id"), c("e_ID"="e_id"), "e_form")) %>% 
  mutate(diff_georef_non_georef = as.numeric(area_m2) - area_m2_nongeo))
  
  
  
### loop over forest edges area sf without georef ---------------------------
# one edge plot loop over forest edges area sf without georef ---------------------------

forest_edges_HBI.man.sub.1.edge <-  forest_edges_HBI.man%>% 
  filter(e_form == 1 |e_form == 2 & inter_status_AT_17 == "two I" | e_form == 2 & inter_status_BT_17 == "two I") %>% 
  # remove plots that have two edges
  anti_join(forest_edges_HBI.man %>%  
              filter(e_form == 1 | e_form == 2  & inter_status_AT_17 == "two I" | e_form == 2 & inter_status_BT_17 == "two I") %>% 
              group_by(plot_ID) %>% 
              summarise(n = n()) %>% 
              filter(n > 1) %>% 
              select(plot_ID), by = "plot_ID") # 14 plots with 2 edges --> 28 rows -> 53 left

one.edge.area.list <- vector("list", length = length(forest_edges_HBI.man.sub.1.edge$plot_ID)*6)
one.edge.tree.status <- vector("list", length = length(forest_edges_HBI.man.sub.1.edge$plot_ID))

for(i in 1:length(forest_edges_HBI.man.sub.1.edge$plot_ID)) {
  # i = 1
  # i = which(grepl(50023, forest_edges_HBI.man.sub.1.edge$plot_ID))
  c.x0 = 0
  c.y0 = 0
  c.r.inter = 120
  
  # select plot ID, edge form and edge_ID accordint to positioin in the list
  my.plot.id <- forest_edges_HBI.man.sub.1.edge[i, "plot_ID"] 
  my.e.id <- forest_edges_HBI.man.sub.1.edge[i, "e_ID"]
  my.e.form <- forest_edges_HBI.man.sub.1.edge[i, "e_form"]
  
  # extract polar coordiantes of forest edge
  # point A 
  dist.A <- forest_edges_HBI.man.sub.1.edge[i, "A_dist"] 
  azi.A <- forest_edges_HBI.man.sub.1.edge[i, "A_azi"] 
  x.A <- dist.A*sin(azi.A)       # this is: easting, longitude, RW
  y.A <- dist.A*cos(azi.A)       # this is: northing, latitude, HW
  
  # point B
  dist.B <- forest_edges_HBI.man.sub.1.edge[i, "B_dist"] 
  azi.B <- forest_edges_HBI.man.sub.1.edge[i, "B_azi"] 
  x.B <- dist.B*sin(azi.B)      # this is: easting, longitude, RW
  y.B <- dist.B*cos(azi.B)      # this is: northing, latitude, HW
  
  #point T
  dist.T <- forest_edges_HBI.man.sub.1.edge[i, "T_dist"] 
  azi.T <- forest_edges_HBI.man.sub.1.edge[i, "T_azi"] 
  x.T <- dist.T*sin(azi.T)      # this is: easting, longitude, RW
  y.T <- dist.T*cos(azi.T)      # this is: northing, latitude, HW
  
  # calculate polar coordiantes of intersections of line with a 60m radius
  x1 <- ifelse(my.e.form  == 1, intersection_line_circle(intercept(x.A , y.A , x.B , y.B ), slope(x.A , y.A , x.B , y.B ), c.x0, c.y0, c.r.inter, coordinate = "x1"),
               ifelse(my.e.form  == 2 & 
                        forest_edges_HBI.man.sub.1.edge$inter_status_AT_17[i] == "two I" &  
                        forest_edges_HBI.man.sub.1.edge$inter_status_BT_17[i] != "two I", intersection_line_circle(intercept(x.A , y.A , x.T , y.T ), slope(x.A , y.A , x.T , y.T ), c.x0, c.y0, c.r.inter, coordinate = "x1"),
                      ifelse(my.e.form  == 2 & 
                               forest_edges_HBI.man.sub.1.edge$inter_status_BT_17[i] == "two I" &  
                               forest_edges_HBI.man.sub.1.edge$inter_status_AT_17[i] != "two I", intersection_line_circle(intercept(x.B , y.B , x.T , y.T ), slope(x.B , y.B , x.T , y.T ), c.x0, c.y0, c.r.inter, coordinate = "x1"),
                             inter.for.triangle(intercept(x.T , y.T , x.A , y.A ), slope(x.T , y.T , x.A , y.A ), c.x0, c.y0, c.r.inter, x.A , y.A , x.T , y.T , coordinate = "x"))))
  
  y1  <- ifelse(my.e.form  == 1, intersection_line_circle(intercept(x.A , y.A , x.B , y.B ), slope(x.A , y.A , x.B , y.B ), c.x0, c.y0, c.r.inter, coordinate = "y1"),
                ifelse(my.e.form  == 2 & 
                         forest_edges_HBI.man.sub.1.edge$inter_status_AT_17[i] == "two I" &  
                         forest_edges_HBI.man.sub.1.edge$inter_status_BT_17[i] != "two I", intersection_line_circle(intercept(x.A , y.A , x.T , y.T ), slope(x.A , y.A , x.T , y.T ), c.x0, c.y0, c.r.inter, coordinate = "y1"),
                       ifelse(my.e.form  == 2 & 
                                forest_edges_HBI.man.sub.1.edge$inter_status_BT_17[i] == "two I" &  
                                forest_edges_HBI.man.sub.1.edge$inter_status_AT_17[i] != "two I", intersection_line_circle(intercept(x.B , y.B , x.T , y.T ), slope(x.B , y.B , x.T , y.T ), c.x0, c.y0, c.r.inter, coordinate = "y1"),
                              inter.for.triangle(intercept(x.T , y.T , x.A , y.A ), slope(x.T , y.T , x.A , y.A ), c.x0, c.y0, c.r.inter, x.A , y.A , x.T , y.T , coordinate = "y"))))
  
  # x2 of AB line or inter.for.triangle of BT line
  x2  <- ifelse(my.e.form  == 1, intersection_line_circle(intercept(x.A , y.A , x.B , y.B ), slope(x.A , y.A , x.B , y.B ), c.x0, c.y0, c.r.inter, coordinate = "x2"),
                ifelse(my.e.form  == 2 & 
                         forest_edges_HBI.man.sub.1.edge$inter_status_AT_17[i] == "two I" &  
                         forest_edges_HBI.man.sub.1.edge$inter_status_BT_17[i] != "two I", intersection_line_circle(intercept(x.A , y.A , x.T , y.T ), slope(x.A , y.A , x.T , y.T ), c.x0, c.y0, c.r.inter, coordinate = "x2"),
                       ifelse(my.e.form  == 2 & 
                                forest_edges_HBI.man.sub.1.edge$inter_status_BT_17[i] == "two I" &  
                                forest_edges_HBI.man.sub.1.edge$inter_status_AT_17[i] != "two I", intersection_line_circle(intercept(x.B , y.B , x.T , y.T ), slope(x.B , y.B , x.T , y.T ), c.x0, c.y0, c.r.inter, coordinate = "x2"),
                              inter.for.triangle(intercept(x.T , y.T , x.B , y.B ), slope(x.T , y.T , x.B , y.B ), c.x0, c.y0, c.r.inter, x.B , y.B , x.T , y.T , coordinate = "x"))))
  
  y2  <- ifelse(my.e.form  == 1, intersection_line_circle(intercept(x.A , y.A , x.B , y.B ), slope(x.A , y.A , x.B , y.B ), c.x0, c.y0, c.r.inter, coordinate = "y2"),
                ifelse(my.e.form  == 2 & 
                         forest_edges_HBI.man.sub.1.edge$inter_status_AT_17[i] == "two I" &  
                         forest_edges_HBI.man.sub.1.edge$inter_status_BT_17[i] != "two I", intersection_line_circle(intercept(x.A , y.A , x.T , y.T ), slope(x.A , y.A , x.T , y.T ), c.x0, c.y0, c.r.inter, coordinate = "y2"),
                       ifelse(my.e.form  == 2 & 
                                forest_edges_HBI.man.sub.1.edge$inter_status_BT_17[i] == "two I" &  
                                forest_edges_HBI.man.sub.1.edge$inter_status_AT_17[i] != "two I", intersection_line_circle(intercept(x.B , y.B , x.T , y.T ), slope(x.B , y.B , x.T , y.T ), c.x0, c.y0, c.r.inter, coordinate = "y2"),
                              inter.for.triangle(intercept(x.T , y.T , x.B , y.B ), slope(x.T , y.T , x.B , y.B ), c.x0, c.y0, c.r.inter, x.B , y.B , x.T , y.T , coordinate = "y"))))
  
  # for edge form 1 we have to consider that the square has to be directed into the direction of the smaller half of the circle
  # calculate coordiantes of the middle of thie line between 
  x_m_line = ( x1 + x2)/2
  y_m_line = ( y1 +  y2)/2
  # # calculate the parameters of the equation between the middle of the line and the centre of the circle
  b1_MC = slope(c.x0, c.y0, x_m_line, y_m_line)
  b0_MC = intercept(c.x0, c.y0, x_m_line, y_m_line)
  # # calcualte the x corrdiante of the interception of the line between M and the centre of the cirle and the circle at the given radio
  X1_inter_MC = intersection_line_circle(b0_MC, b1_MC, c.x0, c.y0, c.r.inter, coordinate = "x1") 
  X2_inter_MC = intersection_line_circle(b0_MC, b1_MC, c.x0, c.y0, c.r.inter, coordinate = "x2")
  # # insert the intersection x corodinate in the line function to get the respective y coordinate
  y1_inter_MC = intersection_line_circle(b0_MC, b1_MC, c.x0, c.y0, c.r.inter, coordinate = "y1") 
  y2_inter_MC = intersection_line_circle(b0_MC, b1_MC, c.x0, c.y0, c.r.inter, coordinate = "y2")
  # # distance between the intersections (inter_MC_1, inter_MC_2) to M on the line 
  dist_C_inter_1_MC = distance(X1_inter_MC, y1_inter_MC, x_m_line, y_m_line)
  dist_C_inter_2_MC = distance(X2_inter_MC, y2_inter_MC, x_m_line, y_m_line) 
  # find the x and y coordinate of the intersection on the shorter side , which is the side to exlcude from the plot 
  # creating the polar coordiantes of a turning point of a triangle by selecting the intersection of the 
  # line from the middle of the AB.inter-ray and the circle center (MC_line) with 
  # the 60m radius at the "shorter side" so the intersection of the MC_line with a 60m radius that has le lest distance to the MC point on the AB.inter-ray
  X_inter_MC_shorter_side = ifelse(my.e.form == 1 & dist_C_inter_1_MC < dist_C_inter_2_MC, X1_inter_MC, 
                                   ifelse(my.e.form == 1 & dist_C_inter_1_MC > dist_C_inter_2_MC, X2_inter_MC, 
                                          ifelse(my.e.form == 2 & p.in.triangle(
                                            inter.for.triangle(intercept(x.T , y.T , x.A, y.A), slope(x.T , y.T , x.A, y.A), c.x0, c.y0, c.r.inter, x.A, y.A, x.T , y.T , coordinate = "x"),
                                            inter.for.triangle(intercept(x.T , y.T , x.B , y.B ), slope(x.T , y.T , x.B , y.B ), c.x0, c.y0, c.r.inter, x.B , y.B , x.T , y.T , coordinate = "x"), 
                                            x.T,
                                            inter.for.triangle(intercept(x.T , y.T , x.A, y.A), slope(x.T , y.T , x.A, y.A), c.x0, c.y0, c.r.inter, x.A, y.A, x.T , y.T , coordinate = "y"),
                                            inter.for.triangle(intercept(x.T , y.T , x.B , y.B ), slope(x.T , y.T , x.B , y.B ), c.x0, c.y0, c.r.inter, x.B , y.B , x.T , y.T , coordinate = "y"), 
                                            y.T,
                                            intersection_line_circle(b0_MC, b1_MC, c.x0, c.y0, 17.84, coordinate = "x1"),
                                            intersection_line_circle(b0_MC, b1_MC, c.x0, c.y0, 17.84, coordinate = "y1")
                                          ) == "B", X1_inter_MC,X2_inter_MC )))
  Y_inter_MC_shorter_side = ifelse(my.e.form == 1 & dist_C_inter_1_MC < dist_C_inter_2_MC, y1_inter_MC, 
                                   ifelse(my.e.form == 1 & dist_C_inter_1_MC > dist_C_inter_2_MC, y2_inter_MC, 
                                          ifelse(my.e.form == 2 & p.in.triangle(
                                            inter.for.triangle(intercept(x.T , y.T , x.A, y.A), slope(x.T , y.T , x.A, y.A), c.x0, c.y0, c.r.inter, x.A, y.A, x.T , y.T , coordinate = "x"),
                                            inter.for.triangle(intercept(x.T , y.T , x.B , y.B ), slope(x.T , y.T , x.B , y.B ), c.x0, c.y0, c.r.inter, x.B , y.B , x.T , y.T , coordinate = "x"), 
                                            x.T,
                                            inter.for.triangle(intercept(x.T , y.T , x.A, y.A), slope(x.T , y.T , x.A, y.A), c.x0, c.y0, c.r.inter, x.A, y.A, x.T , y.T , coordinate = "y"),
                                            inter.for.triangle(intercept(x.T , y.T , x.B , y.B ), slope(x.T , y.T , x.B , y.B ), c.x0, c.y0, c.r.inter, x.B , y.B , x.T , y.T , coordinate = "y"), 
                                            y.T,
                                            intersection_line_circle(b0_MC, b1_MC, c.x0, c.y0, 17.84, coordinate = "x1"),
                                            intersection_line_circle(b0_MC, b1_MC, c.x0, c.y0, 17.84, coordinate = "y1")
                                          ) == "B", y1_inter_MC, y2_inter_MC )))
  
  # create dataframe to create polygone from it
  if(isTRUE(my.e.form == 1 |
            my.e.form == 2 & forest_edges_HBI.man.sub.1.edge$inter_status_AT_17[i] == "two I" & forest_edges_HBI.man.sub.1.edge$inter_status_BT_17[i] != "two I" |
            my.e.form == 2 & forest_edges_HBI.man.sub.1.edge$inter_status_BT_17[i] == "two I" & forest_edges_HBI.man.sub.1.edge$inter_status_AT_17[i] != "two I")==TRUE){
    poly.data = matrix(c(x1, y1,
                         x2, y2,
                         X_inter_MC_shorter_side, Y_inter_MC_shorter_side,
                         x1,  y1), ncol = 2, byrow = TRUE)
  }else{poly.data = matrix(c(x1, y1,
                             x2, y2,
                             x.T, y.T,
                             x1,  y1), ncol = 2, byrow = TRUE)}
  
  # create triangular polygone
  triangle.poly <- sf::st_polygon(list(poly.data))
  
  
  # center point of circle
  pt.circle <- sf::st_point(c(0,0))
 
  
  
  # create circle polygone
  circle.poly.17 <- sf::st_buffer(pt.circle, dist = 17.84)
  circle.poly.12 <- sf::st_buffer(pt.circle, dist = 12.62)
  circle.poly.5 <- sf::st_buffer(pt.circle, dist =  5.64)
  plot(triangle.poly, main = my.plot.id)
  plot(circle.poly.17, add = T)
  
  
  # intersection between circle and triangle
  inter.poly.17 <- sf::st_intersection(circle.poly.17, triangle.poly)
  inter.poly.12 <- sf::st_intersection(circle.poly.12, triangle.poly)
  inter.poly.5 <- sf::st_intersection(circle.poly.5, triangle.poly)
  # remainign circle between circle and triangle
  remaining.circle.poly.17  <- if(isTRUE(nrow(inter.poly.17)==0)==TRUE){circle.poly.17}else{sf::st_difference(circle.poly.17, inter.poly.17)}
  remaining.circle.poly.12  <- if(isTRUE(nrow(inter.poly.12)==0)==TRUE){circle.poly.12}else{sf::st_difference(circle.poly.12, inter.poly.12)}
  remaining.circle.poly.5  <- if(isTRUE(nrow(inter.poly.5)==0)==TRUE){circle.poly.5}else{sf::st_difference(circle.poly.5, inter.poly.5)}
  
   # print(plot(circle.poly.17, col = "red", main = paste0(my.plot.id, "-", my.e.id, "circle 3")),
   #       plot(inter.poly.17, col = "blue", add = T), 
   #       plot(pt.trees, add = T))
  
  
  # sort trees according to their allocation into remaining circle and intersection polygones
  my.trees.df <- trees_and_edges %>% filter(plot_ID == my.plot.id)
  for (i in 1:length(my.trees.df$tree_ID)) {
    # i=1
    x.tree <-(my.trees.df$Dist_cm[i])/100*sin(my.trees.df$azi_gon[i])      # this is: easting, longitude, RW
    y.tree <- (my.trees.df$Dist_cm[i])/100*cos(my.trees.df$azi_gon[i])
    my.tree.matrix <- matrix(c(x.tree, y.tree), ncol = 2, byrow = TRUE)
    pt.tree <- sf::st_point(my.tree.matrix)
    edge_tree_inter <- sf::st_intersection(inter.poly.17, pt.tree)
    rem_circ_tree_inter <- sf::st_intersection(remaining.circle.poly.17, pt.tree)
    tree.status = ifelse(length(edge_tree_inter) != 0, "B", 
                         ifelse(length(rem_circ_tree_inter) != 0, "A", "warning"))
    my.trees.df$tree_stat_poly[[i]] <- tree.status
  }
  
  
  
  # calculate area of intersection 
  area.intresection.17 <- sf::st_area(inter.poly.17)
  area.intresection.12 <- sf::st_area(inter.poly.12)
  area.intresection.5 <- sf::st_area(inter.poly.5)
  # calculate area of remaining circle
  area.rem.circle.17 <- sf::st_area(remaining.circle.poly.17)
  area.rem.circle.12 <- sf::st_area(remaining.circle.poly.12)
  area.rem.circle.5 <- sf::st_area(remaining.circle.poly.5)
  
  # save area per sampling circuit in one dataframe
  one.edge.area.list[[i]] <- as.data.frame(cbind("plot_ID" = c(my.plot.id, my.plot.id, my.plot.id, 
                                                               my.plot.id, my.plot.id, my.plot.id),
                                                 "e_ID" = c(my.e.id, my.e.id, my.e.id, 
                                                            0, 0, 0),
                                                 "e_form" = c(my.e.form , my.e.form, my.e.form, 
                                                              0, 0, 0),
                                                 "shape" = c("edge", "edge", "edge", 
                                                             "circle", "circle", "circle"),
                                                 "CCS_radius" = c(17.84, 12.62,  5.64, 
                                                                  17.84, 12.62,  5.64),
                                                 "area_m2" = c(area.intresection.17, area.intresection.12, area.intresection.5, 
                                                               area.rem.circle.17, area.rem.circle.12, area.rem.circle.5)))
  one.edge.tree.status[[i]] <- as.data.frame(cbind(
    "plot_ID" = c(my.trees.df$plot_ID),
    "tree_ID" = c(my.trees.df$tree_ID),
    "t_stat" = c(my.trees.df$tree_stat_poly)
  ))
  
}
one.edge.area.final <- rbindlist(one.edge.area.list)
one.edge.area.df <- as.data.frame(one.edge.area.final)

one.edge.tree.status.final <- rbindlist(one.edge.tree.status)
one.edge.tree.df <- as.data.frame(one.edge.tree.status.final)






# two edges plot loop over forest edges area sf without georef ---------------------------
forest_edges_HBI.man.sub.2.edges <-  forest_edges_HBI.man %>% 
  filter(e_form == 1 |e_form == 2 & inter_status_AT_17 == "two I" | e_form == 2 & inter_status_BT_17 == "two I") %>% 
  # remove plots that have two edges
  semi_join(forest_edges_HBI.man %>%  
              filter(e_form == 1 | e_form == 2  & inter_status_AT_17 == "two I" | e_form == 2 & inter_status_BT_17 == "two I") %>% 
              group_by(plot_ID) %>% 
              summarise(n = n()) %>% 
              filter(n > 1) %>% 
              select(plot_ID), by = "plot_ID") # 14 plots with 2 edges --> 28 rows -> 53 left

two.edges.area.list <- vector("list", length = length(unique(forest_edges_HBI.man.sub.2.edges$plot_ID))*18)
two.edges.tree.status <- vector("list", length = length(unique(forest_edges_HBI.man.sub.2.edges$plot_ID)))
inter.poly.17.1.list<- vector("list", length = length(unique(forest_edges_HBI.man.sub.2.edges$plot_ID)))
inter.poly.17.2.list<- vector("list", length = length(unique(forest_edges_HBI.man.sub.2.edges$plot_ID)))

for(i in 1:length(unique(forest_edges_HBI.man.sub.2.edges$plot_ID)) ) {
  # i = 3
  # i = which(grepl(50080,unique(forest_edges_HBI.man.sub.2.edges$plot_ID)))
  c.x0 = 0
  c.y0 = 0
  c.r.inter = 120
  
  # select plot ID, edge form and edge_ID accordint to positioin in the list
  my.plot.id <- unique(forest_edges_HBI.man.sub.2.edges$plot_ID)[i]
  my.edges.df <- forest_edges_HBI.man.sub.2.edges %>% filter(plot_ID == my.plot.id) %>% arrange(e_ID)
  
  # select id of respective edge
  my.e.id.1 <- my.edges.df$e_ID[1]
  my.e.id.2 <- my.edges.df$e_ID[2]
  
  # select id of respective edge
  my.e.form.1 <- my.edges.df$e_form[1]
  my.e.form.2 <- my.edges.df$e_form[2]
  
  
  ### polygone 1
  # extract polar coordiantes of forest edge 1
  # point A 
  dist.A.1 <- my.edges.df$A_dist[1]
  azi.A.1 <- my.edges.df$A_azi[1]
  x.A.1 <- dist.A*sin(azi.A.1)       # this is: easting, longitude, RW
  y.A.1 <- dist.A*cos(azi.A.1)       # this is: northing, latitude, HW
  # point B
  dist.B.1 <- my.edges.df$B_dist[1]
  azi.B.1 <- my.edges.df$B_azi[1]
  x.B.1 <- dist.B*sin(azi.B.1)      # this is: easting, longitude, RW
  y.B.1 <- dist.B*cos(azi.B.1)      # this is: northing, latitude, HW
  # point T
  dist.T.1 <- my.edges.df$T_dist[1]
  azi.T.1 <- my.edges.df$T_azi[1]
  x.T.1 <- dist.B*sin(azi.T.1)      # this is: easting, longitude, RW
  y.T.1 <- dist.B*cos(azi.T.1)      # this is: northing, latitude, HW
  # calculate polar coordiantes of intersections of line with a 60m radius
  # x1 of AB line or inter.for.triangle of AT line
  x1.1 <- ifelse(my.e.form.1 == 1, intersection_line_circle(intercept(x.A.1, y.A.1, x.B.1, y.B.1), slope(x.A.1, y.A.1, x.B.1, y.B.1), c.x0, c.y0, c.r.inter, coordinate = "x1"),
                 ifelse(my.e.form.1 == 2 & 
                          my.edges.df$inter_status_AT_17[1] == "two I" &  
                          my.edges.df$inter_status_BT_17[1] != "two I", intersection_line_circle(intercept(x.A.1, y.A.1, x.T.1, y.T.1), slope(x.A.1, y.A.1, x.T.1, y.T.1), c.x0, c.y0, c.r.inter, coordinate = "x1"),
                        ifelse(my.e.form.1 == 2 & 
                                 my.edges.df$inter_status_BT_17[1] == "two I" &  
                                 my.edges.df$inter_status_AT_17[1] != "two I", intersection_line_circle(intercept(x.B.1, y.B.1, x.T.1, y.T.1), slope(x.B.1, y.B.1, x.T.1, y.T.1), c.x0, c.y0, c.r.inter, coordinate = "x1"),
                               inter.for.triangle(intercept(x.T.1, y.T.1, x.A.1, y.A.1), slope(x.T.1, y.T.1, x.A.1, y.A.1), c.x0, c.y0, c.r.inter, x.A.1, y.A.1, x.T.1, y.T.1, coordinate = "x"))))
  
  y1.1 <- ifelse(my.e.form.1 == 1, intersection_line_circle(intercept(x.A.1, y.A.1, x.B.1, y.B.1), slope(x.A.1, y.A.1, x.B.1, y.B.1), c.x0, c.y0, c.r.inter, coordinate = "y1"),
                 ifelse(my.e.form.1 == 2 & 
                          my.edges.df$inter_status_AT_17[1] == "two I" &  
                          my.edges.df$inter_status_BT_17[1] != "two I", intersection_line_circle(intercept(x.A.1, y.A.1, x.T.1, y.T.1), slope(x.A.1, y.A.1, x.T.1, y.T.1), c.x0, c.y0, c.r.inter, coordinate = "y1"),
                        ifelse(my.e.form.1 == 2 & 
                                 my.edges.df$inter_status_BT_17[1] == "two I" &  
                                 my.edges.df$inter_status_AT_17[1] != "two I", intersection_line_circle(intercept(x.B.1, y.B.1, x.T.1, y.T.1), slope(x.B.1, y.B.1, x.T.1, y.T.1), c.x0, c.y0, c.r.inter, coordinate = "y1"),
                               inter.for.triangle(intercept(x.T.1, y.T.1, x.A.1, y.A.1), slope(x.T.1, y.T.1, x.A.1, y.A.1), c.x0, c.y0, c.r.inter, x.A.1, y.A.1, x.T.1, y.T.1, coordinate = "y"))))
  
  # x2 of AB line or inter.for.triangle of BT line
  x2.1 <- ifelse(my.e.form.1 == 1, intersection_line_circle(intercept(x.A.1, y.A.1, x.B.1, y.B.1), slope(x.A.1, y.A.1, x.B.1, y.B.1), c.x0, c.y0, c.r.inter, coordinate = "x2"),
                 ifelse(my.e.form.1 == 2 & 
                          my.edges.df$inter_status_AT_17[1] == "two I" &  
                          my.edges.df$inter_status_BT_17[1] != "two I", intersection_line_circle(intercept(x.A.1, y.A.1, x.T.1, y.T.1), slope(x.A.1, y.A.1, x.T.1, y.T.1), c.x0, c.y0, c.r.inter, coordinate = "x2"),
                        ifelse(my.e.form.1 == 2 & 
                                 my.edges.df$inter_status_BT_17[1] == "two I" &  
                                 my.edges.df$inter_status_AT_17[1] != "two I", intersection_line_circle(intercept(x.B.1, y.B.1, x.T.1, y.T.1), slope(x.B.1, y.B.1, x.T.1, y.T.1), c.x0, c.y0, c.r.inter, coordinate = "x2"),
                               inter.for.triangle(intercept(x.T.1, y.T.1, x.B.1, y.B.1), slope(x.T.1, y.T.1, x.B.1, y.B.1), c.x0, c.y0, c.r.inter, x.B.1, y.B.1, x.T.1, y.T.1, coordinate = "x"))))
  
  y2.1 <- ifelse(my.e.form.1 == 1, intersection_line_circle(intercept(x.A.1, y.A.1, x.B.1, y.B.1), slope(x.A.1, y.A.1, x.B.1, y.B.1), c.x0, c.y0, c.r.inter, coordinate = "y2"),
                 ifelse(my.e.form.1 == 2 & 
                          my.edges.df$inter_status_AT_17[1] == "two I" &  
                          my.edges.df$inter_status_BT_17[1] != "two I", intersection_line_circle(intercept(x.A.1, y.A.1, x.T.1, y.T.1), slope(x.A.1, y.A.1, x.T.1, y.T.1), c.x0, c.y0, c.r.inter, coordinate = "y2"),
                        ifelse(my.e.form.1 == 2 & 
                                 my.edges.df$inter_status_BT_17[1] == "two I" &  
                                 my.edges.df$inter_status_AT_17[1] != "two I", intersection_line_circle(intercept(x.B.1, y.B.1, x.T.1, y.T.1), slope(x.B.1, y.B.1, x.T.1, y.T.1), c.x0, c.y0, c.r.inter, coordinate = "y2"),
                               inter.for.triangle(intercept(x.T.1, y.T.1, x.B.1, y.B.1), slope(x.T.1, y.T.1, x.B.1, y.B.1), c.x0, c.y0, c.r.inter, x.B.1, y.B.1, x.T.1, y.T.1, coordinate = "y"))))
  
  
  # for edge form 1 we have to consider that the square has to be directed into the direction of the smaller half of the circle
  # calculate coordiantes of the middle of thie line between 
  x_m_line.1 = (x1.1 +  x2.1)/2
  y_m_line.1 = ( y1.1 +  y2.1)/2
  # calculate the parameters of the equation between the middle of the line and the centre of the circle
  b1_MC.1 = slope(c.x0, c.y0, x_m_line.1, y_m_line.1)
  b0_MC.1 = intercept(c.x0, c.y0, x_m_line.1, y_m_line.1)
  # calcualte the x corrdiante of the interception of the line between M and the centre of the cirle and the circle at the given radio
  X1_inter_MC.1 = intersection_line_circle(b0_MC.1, b1_MC.1, c.x0, c.y0, c.r.inter, coordinate = "x1") 
  X2_inter_MC.1 = intersection_line_circle(b0_MC.1, b1_MC.1, c.x0, c.y0, c.r.inter, coordinate = "x2")
  # insert the intersection x corodinate in the line function to get the respective y coordinate
  y1_inter_MC.1 = intersection_line_circle(b0_MC.1, b1_MC.1, c.x0, c.y0, c.r.inter, coordinate = "y1") 
  y2_inter_MC.1 = intersection_line_circle(b0_MC.1, b1_MC.1, c.x0, c.y0, c.r.inter, coordinate = "y2")
  # distance between the intersections (inter_MC_1, inter_MC_2) to M on the line 
  dist_C_inter_1_MC.1 = distance(X1_inter_MC.1, y1_inter_MC.1, x_m_line.1, y_m_line.1)
  dist_C_inter_2_MC.1 = distance(X2_inter_MC.1, y2_inter_MC.1, x_m_line.1, y_m_line.1) 
  # for e_form 1: find the x and y coordinate of the intersection on the shorter side , which is the side to exlcude from the plot 
  # the intersection of a line through the middle point of the AB.intersection-ray and the center of the plot with a 60m circle (MC_line) is used to 
  # create the polar coordiantes of a turning point of a triangle by selecting the rrespective intersection that 
  # has the least distance to the MC point on the AB.inter-ray
  # for e form 2: we poroceede similarly,just that the intersection of the MC lien we choose doesn´t have to be the one on the shorter side of the circle but 
  # the side/ intersection that would normally lie inside a triangle with the extend of the AT- and BT- line intersection with a 60m circle radius and the turning pouint T (so the usual procedure to create a triangle)
  X_inter_MC_shorter_side.1 = ifelse(my.e.form.1 == 1 & dist_C_inter_1_MC.1 < dist_C_inter_2_MC.1, X1_inter_MC.1, 
         ifelse(my.e.form.1 == 1 & dist_C_inter_1_MC.1 > dist_C_inter_2_MC.1, X2_inter_MC.1, 
                ifelse(my.e.form.1 == 2 & p.in.triangle(
                  inter.for.triangle(intercept(x.T.1 , y.T.1 , x.A.1, y.A.1), slope(x.T.1 , y.T.1 , x.A.1, y.A.1), c.x0, c.y0, c.r.inter, x.A.1, y.A.1, x.T.1 , y.T.1, coordinate = "x"),
                  inter.for.triangle(intercept(x.T.1 , y.T.1 , x.B.1 , y.B.1 ), slope(x.T.1 , y.T.1 , x.B.1 , y.B.1 ), c.x0, c.y0, c.r.inter, x.B.1 , y.B.1 , x.T.1 , y.T.1 , coordinate = "x"), 
                  x.T.1,
                  inter.for.triangle(intercept(x.T.1 , y.T.1 , x.A.1, y.A.1), slope(x.T.1 , y.T.1 , x.A.1, y.A.1), c.x0, c.y0, c.r.inter, x.A.1, y.A.1, x.T.1 , y.T.1 , coordinate = "y"),
                  inter.for.triangle(intercept(x.T.1 , y.T.1 , x.B.1 , y.B.1 ), slope(x.T.1 , y.T.1 , x.B.1 , y.B.1 ), c.x0, c.y0, c.r.inter, x.B.1 , y.B.1 , x.T.1 , y.T.1 , coordinate = "y"), 
                  y.T.1,
                  intersection_line_circle(b0_MC.1, b1_MC.1, c.x0, c.y0, 17.84, coordinate = "x1"),
                  intersection_line_circle(b0_MC.1, b1_MC.1, c.x0, c.y0, 17.84, coordinate = "y1")
                ) == "B", X1_inter_MC.1,X2_inter_MC.1 )))
  Y_inter_MC_shorter_side.1 = ifelse(my.e.form.1 == 1 & dist_C_inter_1_MC.1 < dist_C_inter_2_MC.1, y1_inter_MC.1, 
                                     ifelse(my.e.form.1 == 1 & dist_C_inter_1_MC.1 > dist_C_inter_2_MC.1, y2_inter_MC.1, 
                                            ifelse(my.e.form.1 == 2 & p.in.triangle(
                                              inter.for.triangle(intercept(x.T.1 , y.T.1 , x.A.1, y.A.1), slope(x.T.1 , y.T.1 , x.A.1, y.A.1), c.x0, c.y0, c.r.inter, x.A.1, y.A.1, x.T.1 , y.T.1, coordinate = "x"),
                                              inter.for.triangle(intercept(x.T.1 , y.T.1 , x.B.1 , y.B.1 ), slope(x.T.1 , y.T.1 , x.B.1 , y.B.1 ), c.x0, c.y0, c.r.inter, x.B.1 , y.B.1 , x.T.1 , y.T.1 , coordinate = "x"), 
                                              x.T.1,
                                              inter.for.triangle(intercept(x.T.1 , y.T.1 , x.A.1, y.A.1), slope(x.T.1 , y.T.1 , x.A.1, y.A.1), c.x0, c.y0, c.r.inter, x.A.1, y.A.1, x.T.1 , y.T.1 , coordinate = "y"),
                                              inter.for.triangle(intercept(x.T.1 , y.T.1 , x.B.1 , y.B.1 ), slope(x.T.1 , y.T.1 , x.B.1 , y.B.1 ), c.x0, c.y0, c.r.inter, x.B.1 , y.B.1 , x.T.1 , y.T.1 , coordinate = "y"), 
                                              y.T.1,
                                              intersection_line_circle(b0_MC.1, b1_MC.1, c.x0, c.y0, 17.84, coordinate = "x1"),
                                              intersection_line_circle(b0_MC.1, b1_MC.1, c.x0, c.y0, 17.84, coordinate = "y1")
                                            ) == "B", y1_inter_MC.1, y2_inter_MC.1)))
  
  # create dataframe to create polygone from it
  if(isTRUE(my.e.form.1 == 1 |
            my.e.form.1 == 2 & my.edges.df$inter_status_AT_17[1] == "two I" & my.edges.df$inter_status_BT_17[1] != "two I" |
            my.e.form.1 == 2 & my.edges.df$inter_status_BT_17[1] == "two I" & my.edges.df$inter_status_AT_17[1] != "two I")==TRUE){
    poly.data.1 = matrix(c(x1.1, y1.1,
                           x2.1, y2.1,
                           X_inter_MC_shorter_side.1, Y_inter_MC_shorter_side.1,
                           x1.1,  y1.1), ncol = 2, byrow = TRUE)
  }else{poly.data.1 = matrix(c(x1.1, y1.1,
                               x2.1, y2.1,
                               x.T.1, y.T.1,
                               x1.1,  y1.1), ncol = 2, byrow = TRUE)}
  
  # create triangular polygone
  triangle.poly.1 <- sf::st_polygon(list(poly.data.1))

  
  ### polygone 2 
  # extract polar coordiantes of forest edge 1
  # point A 
  dist.A.2 <- my.edges.df$A_dist[2]
  azi.A.2 <- my.edges.df$A_azi[2]
  x.A.2 <- dist.A*sin(azi.A.2)       # this is: easting, longitude, RW
  y.A.2 <- dist.A*cos(azi.A.2)       # this is: northing, latitude, HW
  
  # point B
  dist.B.2 <- my.edges.df$B_dist[2]
  azi.B.2 <- my.edges.df$B_azi[2]
  x.B.2 <- dist.B*sin(azi.B.2)      # this is: easting, longitude, RW
  y.B.2 <- dist.B*cos(azi.B.2)      # this is: northing, latitude, HW
  
  # point T
  dist.T.2 <- my.edges.df$T_dist[2]
  azi.T.2 <- my.edges.df$T_azi[2]
  x.T.2 <- dist.B*sin(azi.T.2)      # this is: easting, longitude, RW
  y.T.2 <- dist.B*cos(azi.T.2)      # this is: northing, latitude, HW
  
  # calculate polar coordiantes of intersections of line with a 60m radius
  # x1 of AB line or inter.for.triangle of AT line
  x1.2 <- ifelse(my.e.form.2 == 1, intersection_line_circle(intercept(x.A.2, y.A.2, x.B.2, y.B.2), slope(x.A.2, y.A.2, x.B.2, y.B.2), c.x0, c.y0, c.r.inter, coordinate = "x1"),
                 # if the edge form is 2 bit only one side of the triangle intersects the circle, treat this side like a line and use the intersection sof the respective line (AT or BT) as x1 and x2
                 ifelse(my.e.form.2 == 2 & 
                          my.edges.df$inter_status_AT_17[2] == "two I" &  
                          my.edges.df$inter_status_BT_17[2] != "two I", intersection_line_circle(intercept(x.A.2, y.A.2, x.T.2, y.T.2), slope(x.A.2, y.A.2, x.T.2, y.T.2), c.x0, c.y0, c.r.inter, coordinate = "x1"),
                        ifelse(my.e.form.2 == 2 & 
                                 my.edges.df$inter_status_BT_17[2] == "two I" &  
                                 my.edges.df$inter_status_AT_17[2] != "two I", intersection_line_circle(intercept(x.B.2, y.B.2, x.T.2, y.T.2), slope(x.B.2, y.B.2, x.T.2, y.T.2), c.x0, c.y0, c.r.inter, coordinate = "x1"),
                               inter.for.triangle(intercept(x.T.2, y.T.2, x.A.2, y.A.2), slope(x.T.2, y.T.2, x.A.2, y.A.2), c.x0, c.y0, c.r.inter, x.A.2, y.A.2, x.T.2, y.T.2, coordinate = "x"))))
  
  y1.2 <- ifelse(my.e.form.2 == 1, intersection_line_circle(intercept(x.A.2, y.A.2, x.B.2, y.B.2), slope(x.A.2, y.A.2, x.B.2, y.B.2), c.x0, c.y0, c.r.inter, coordinate = "y1"),
                 # if the edge form is 2 bit only one side of the triangle intersects the circle, treat this side like a line and use the intersection sof the respective line (AT or BT) as x1 and x2
                 ifelse(my.e.form.2 == 2 & 
                          my.edges.df$inter_status_AT_17[2] == "two I" &  
                          my.edges.df$inter_status_BT_17[2] != "two I", intersection_line_circle(intercept(x.A.2, y.A.2, x.T.2, y.T.2), slope(x.A.2, y.A.2, x.T.2, y.T.2), c.x0, c.y0, c.r.inter, coordinate = "y1"),
                        ifelse(my.e.form.2 == 2 & 
                                 my.edges.df$inter_status_BT_17[2] == "two I" &  
                                 my.edges.df$inter_status_AT_17[2] != "two I", intersection_line_circle(intercept(x.B.2, y.B.2, x.T.2, y.T.2), slope(x.B.2, y.B.2, x.T.2, y.T.2), c.x0, c.y0, c.r.inter, coordinate = "y1"),
                               inter.for.triangle(intercept(x.T.2, y.T.2, x.A.2, y.A.2), slope(x.T.2, y.T.2, x.A.2, y.A.2), c.x0, c.y0, c.r.inter, x.A.2, y.A.2, x.T.2, y.T.2, coordinate = "y"))))
  
  # x2 of AB line or inter.for.triangle of BT line
  x2.2 <- ifelse(my.e.form.2 == 1, intersection_line_circle(intercept(x.A.2, y.A.2, x.B.2, y.B.2), slope(x.A.2, y.A.2, x.B.2, y.B.2), c.x0, c.y0, c.r.inter, coordinate = "x2"),
                 # if the edge form is 2 bit only one side of the triangle intersects the circle, treat this side like a line and use the intersection sof the respective line (AT or BT) as x1 and x2
                 ifelse(my.e.form.2 == 2 & 
                          my.edges.df$inter_status_AT_17[2] == "two I" &  
                          my.edges.df$inter_status_BT_17[2] != "two I", intersection_line_circle(intercept(x.A.2, y.A.2, x.T.2, y.T.2), slope(x.A.2, y.A.2, x.T.2, y.T.2), c.x0, c.y0, c.r.inter, coordinate = "x2"),
                        ifelse(my.e.form.2 == 2 & 
                                 my.edges.df$inter_status_BT_17[2] == "two I" &  
                                 my.edges.df$inter_status_AT_17[2] != "two I", intersection_line_circle(intercept(x.B.2, y.B.2, x.T.2, y.T.2), slope(x.B.2, y.B.2, x.T.2, y.T.2), c.x0, c.y0, c.r.inter, coordinate = "x2"),
                               inter.for.triangle(intercept(x.T.2, y.T.2, x.B.2, y.B.2), slope(x.T.2, y.T.2, x.B.2, y.B.2), c.x0, c.y0, c.r.inter, x.B.2, y.B.2, x.T.2, y.T.2, coordinate = "x"))))

  y2.2 <- ifelse(my.e.form.2 == 1, intersection_line_circle(intercept(x.A.2, y.A.2, x.B.2, y.B.2), slope(x.A.2, y.A.2, x.B.2, y.B.2), c.x0, c.y0, c.r.inter, coordinate = "y2"),
                 # if the edge form is 2 bit only one side of the triangle intersects the circle, treat this side like a line and use the intersection sof the respective line (AT or BT) as x1 and x2
                 ifelse(my.e.form.2 == 2 & 
                          my.edges.df$inter_status_AT_17[2] == "two I" &  
                          my.edges.df$inter_status_BT_17[2] != "two I", intersection_line_circle(intercept(x.A.2, y.A.2, x.T.2, y.T.2), slope(x.A.2, y.A.2, x.T.2, y.T.2), c.x0, c.y0, c.r.inter, coordinate = "y2"),
                        ifelse(my.e.form.2 == 2 & 
                                 my.edges.df$inter_status_BT_17[2] == "two I" &  
                                 my.edges.df$inter_status_AT_17[2] != "two I", intersection_line_circle(intercept(x.B.2, y.B.2, x.T.2, y.T.2), slope(x.B.2, y.B.2, x.T.2, y.T.2), c.x0, c.y0, c.r.inter, coordinate = "y2"),
                               inter.for.triangle(intercept(x.T.2, y.T.2, x.B.2, y.B.2), slope(x.T.2, y.T.2, x.B.2, y.B.2), c.x0, c.y0, c.r.inter, x.B.2, y.B.2, x.T.2, y.T.2, coordinate = "y"))))
  
  
  # for edge form 1 we have to consider that the square has to be directed into the direction of the smaller half of the circle
  # calculate coordiantes of the middle of thie line between 
  x_m_line.2 = (x1.2 +  x2.2)/2
  y_m_line.2 = (y1.2 +  y2.2)/2
  # calculate the parameters of the equation between the middle of the line and the centre of the circle
  b1_MC.2 = slope(c.x0, c.y0, x_m_line.2, y_m_line.2)
  b0_MC.2 = intercept(c.x0, c.y0, x_m_line.2, y_m_line.2)
  # calcualte the x corrdiante of the interception of the line between M and the centre of the cirle and the circle at the given radio
  X1_inter_MC.2 = intersection_line_circle(b0_MC.2, b1_MC.2, c.x0, c.y0, c.r.inter, coordinate = "x1") 
  X2_inter_MC.2 = intersection_line_circle(b0_MC.2, b1_MC.2, c.x0, c.y0, c.r.inter, coordinate = "x2")
  # insert the intersection x corodinate in the line function to get the respective y coordinate
  y1_inter_MC.2 = intersection_line_circle(b0_MC.2, b1_MC.2, c.x0, c.y0, c.r.inter, coordinate = "y1") 
  y2_inter_MC.2 = intersection_line_circle(b0_MC.2, b1_MC.2, c.x0, c.y0, c.r.inter, coordinate = "y2")
  # distance between the intersections (inter_MC_1, inter_MC_2) to M on the line 
  dist_C_inter_1_MC.2 = distance(X1_inter_MC.2, y1_inter_MC.2, x_m_line.2, y_m_line.2)
  dist_C_inter_2_MC.2 = distance(X2_inter_MC.2, y2_inter_MC.2, x_m_line.2, y_m_line.2) 
  # for e_form 1: find the x and y coordinate of the intersection on the shorter side , which is the side to exlcude from the plot 
  # the intersection of a line through the middle point of the AB.intersection-ray and the center of the plot with a 60m circle (MC_line) is used to 
  # create the polar coordiantes of a turning point of a triangle by selecting the rrespective intersection that 
  # has the least distance to the MC point on the AB.inter-ray
  # for e form 2 we poroceede similarly,just that the intersection of the MC lien we choose doesn´t have to be the one on the shorter side of the circle but 
  # the side/ intersection that would normally lie inside a triangle with the extend of the AT- and BT- line intersection with a 60m circle radius and the turning pouint T (so the usual procedure to create a triangle)
  
  X_inter_MC_shorter_side.2 = ifelse(my.e.form.2 == 1 & dist_C_inter_1_MC.2 < dist_C_inter_2_MC.2, X1_inter_MC.2, 
                                     ifelse(my.e.form.2 == 1 & dist_C_inter_1_MC.2 > dist_C_inter_2_MC.2, X2_inter_MC.2, 
                                            ifelse(my.e.form.2 == 2 & p.in.triangle(
                                              inter.for.triangle(intercept(x.T.2 , y.T.2 , x.A.2, y.A.2), slope(x.T.2 , y.T.2 , x.A.2, y.A.2), c.x0, c.y0, c.r.inter, x.A.2, y.A.2, x.T.2 , y.T.2, coordinate = "x"),
                                              inter.for.triangle(intercept(x.T.2 , y.T.2 , x.B.2 , y.B.2 ), slope(x.T.2 , y.T.2 , x.B.2 , y.B.2 ), c.x0, c.y0, c.r.inter, x.B.2 , y.B.2 , x.T.2 , y.T.2 , coordinate = "x"), 
                                              x.T.2,
                                              inter.for.triangle(intercept(x.T.2 , y.T.2 , x.A.2, y.A.2), slope(x.T.2 , y.T.2 , x.A.2, y.A.2), c.x0, c.y0, c.r.inter, x.A.2, y.A.2, x.T.2 , y.T.2 , coordinate = "y"),
                                              inter.for.triangle(intercept(x.T.2 , y.T.2 , x.B.2 , y.B.2 ), slope(x.T.2 , y.T.2 , x.B.2 , y.B.2 ), c.x0, c.y0, c.r.inter, x.B.2 , y.B.2 , x.T.2 , y.T.2 , coordinate = "y"), 
                                              y.T.2,
                                              intersection_line_circle(b0_MC.2, b1_MC.2, c.x0, c.y0, 17.84, coordinate = "x1"),
                                              intersection_line_circle(b0_MC.2, b1_MC.2, c.x0, c.y0, 17.84, coordinate = "y1")
                                            ) == "B", X1_inter_MC.2,X2_inter_MC.2 )))
  Y_inter_MC_shorter_side.2 = ifelse(my.e.form.2 == 1 & dist_C_inter_1_MC.2 < dist_C_inter_2_MC.2, y1_inter_MC.2, 
                                     ifelse(my.e.form.2 == 1 & dist_C_inter_1_MC.2 > dist_C_inter_2_MC.2, y2_inter_MC.2, 
                                            ifelse(my.e.form.2 == 2 & p.in.triangle(
                                              inter.for.triangle(intercept(x.T.2 , y.T.2 , x.A.2, y.A.2), slope(x.T.2 , y.T.2 , x.A.2, y.A.2), c.x0, c.y0, c.r.inter, x.A.2, y.A.2, x.T.2 , y.T.2, coordinate = "x"),
                                              inter.for.triangle(intercept(x.T.2 , y.T.2 , x.B.2 , y.B.2 ), slope(x.T.2 , y.T.2 , x.B.2 , y.B.2 ), c.x0, c.y0, c.r.inter, x.B.2 , y.B.2 , x.T.2 , y.T.2 , coordinate = "x"), 
                                              x.T.2,
                                              inter.for.triangle(intercept(x.T.2 , y.T.2 , x.A.2, y.A.2), slope(x.T.2 , y.T.2 , x.A.2, y.A.2), c.x0, c.y0, c.r.inter, x.A.2, y.A.2, x.T.2 , y.T.2 , coordinate = "y"),
                                              inter.for.triangle(intercept(x.T.2 , y.T.2 , x.B.2 , y.B.2 ), slope(x.T.2 , y.T.2 , x.B.2 , y.B.2 ), c.x0, c.y0, c.r.inter, x.B.2 , y.B.2 , x.T.2 , y.T.2 , coordinate = "y"), 
                                              y.T.2,
                                              intersection_line_circle(b0_MC.2, b1_MC.2, c.x0, c.y0, 17.84, coordinate = "x1"),
                                              intersection_line_circle(b0_MC.2, b1_MC.2, c.x0, c.y0, 17.84, coordinate = "y1")
                                            ) == "B", y1_inter_MC.2, y2_inter_MC.2)))
  
  # create dataframe to create polygone from it
  if(isTRUE(my.e.form.2 == 1 |
            my.e.form.2 == 2 & my.edges.df$inter_status_AT_17[2] == "two I" & my.edges.df$inter_status_BT_17[2] != "two I" |
            my.e.form.2 == 2 & my.edges.df$inter_status_BT_17[2] == "two I" & my.edges.df$inter_status_AT_17[2] != "two I")==TRUE){
    poly.data.2 = matrix(c(x1.2, y1.2,
                           x2.2, y2.2,
                           X_inter_MC_shorter_side.2, Y_inter_MC_shorter_side.2,
                           x1.2,  y1.2), ncol = 2, byrow = TRUE)
  }else{poly.data.2 = matrix(c(x1.2, y1.2,
                               x2.2, y2.2,
                               x.T.2, y.T.2,
                               x1.2,  y1.2), ncol = 2, byrow = TRUE)}
  
  # create triangular polygone
  triangle.poly.2 <- sf::st_polygon(list(poly.data.2))

  
  
  ### calcualte intersections with all 3 sampling circles
  # center point of circle
  pt.circle <- sf::st_point(c(0,0))
  
  # create circle polygone
  circle.poly.17 <- sf::st_buffer(pt.circle, dist = 17.84)
  circle.poly.12 <- sf::st_buffer(pt.circle, dist = 12.62)
  circle.poly.5 <- sf::st_buffer(pt.circle, dist =  5.64)
  # 
  print(plot(triangle.poly.1, main = my.plot.id),
   plot(triangle.poly.2, add = T),
   plot(circle.poly.17, add = T))
  # 
  
  ###  circle 17 intersection between circle and triangle
  # poly 1
  inter.poly.17.1 <- sf::st_intersection(circle.poly.17, triangle.poly.1)
  inter.status.poly.17.1 <- ifelse(length(inter.poly.17.1) == 0, "no intersections",
                                   ifelse(my.e.id.1 == 1 & inter.poly.17.1 == circle.poly.17,  "no intersections",
                                          ifelse(my.e.id.1 == 2 & inter.poly.17.1 == circle.poly.17, "fully covering circle", 
                                                 "partly intersecting")))
  # if the first ednge covers all of the circle remaining its going to be set to 0 so we know there are no direct intersections and the circle is passed on to the next edge to calcualte the intersection
  # https://www.statology.org/r-argument-is-of-length-zero/
  inter.poly.17.1 <- if(isTRUE(inter.poly.17.1) && inter.poly.17.1 == circle.poly.17){inter.poly.17.1 <- data.frame()}else{inter.poly.17.1}
  ## create poolygon of remaining circle after first edge polygone is intersected
  # create poly with remaining area: https://gis.stackexchange.com/questions/353633/r-spatial-erase-one-polygon-from-another-correct-use-of-st-difference
  remaining.circle.17.1 <- if(isTRUE(length(inter.poly.17.1) == 0)==TRUE){circle.poly.17}else{sf::st_difference(circle.poly.17, inter.poly.17.1)}
  # poly 2
  ## create polygone of intersecting area of second polygone with remaining circle
  inter.poly.17.2 <- st_intersection(remaining.circle.17.1, triangle.poly.2)
  inter.status.poly.17.2 <- ifelse(length(inter.poly.17.2) == 0, "no intersections",
                                   ifelse(my.e.id.2 == 1 & inter.poly.17.2 == remaining.circle.17.1,  "no intersections",
                                          ifelse(my.e.id.2 == 2 & inter.poly.17.2 == remaining.circle.17.1, "fully covering circle", 
                                                 "partly intersecting")))
  # if the second edge covers all of the circle remaining its going to be set to 0 so we know there are no direct intersections
  # https://www.statology.org/r-argument-is-of-length-zero/
  inter.poly.17.2 <- if(isTRUE(inter.poly.17.2) && inter.poly.17.2 == remaining.circle.17.1){inter.poly.17.2 <- data.frame()}else{inter.poly.17.2}
  ## create polygone of the  remaining cricle after both intersects are decucted
  # so the area of the frst remining circle minus the area of the second remaining circle 
  remaining.circle.17.1.and.2.poly <- if(isTRUE(length(inter.poly.17.2) == 0)==TRUE){remaining.circle.17.1}else{sf::st_difference(remaining.circle.17.1, inter.poly.17.2)}
  print(plot(remaining.circle.17.1.and.2.poly, main  = my.plot.id), 
       # plot(triangle.poly.1, add = T), 
       # plot(triangle.poly.2, add = T)
       )
  
  
  ###  circle 12 intersection between circle and triangle
  # poly 1
  inter.poly.12.1 <- sf::st_intersection(circle.poly.12, triangle.poly.1)
  inter.status.poly.12.1 <- ifelse(length(inter.poly.12.1) == 0, "no intersections",
                                   ifelse(my.e.id.1 == 1 & inter.poly.12.1 == circle.poly.12,  "no intersections",
                                          ifelse(my.e.id.1 == 2 & inter.poly.12.1 == circle.poly.12, "fully covering circle", 
                                                 "partly intersecting")))
  # if the first ednge covers all of the circle remaining its going to be set to 0 so we know there are no direct intersections and the circle is passed on to the next edge to calcualte the intersection
  # https://www.statology.org/r-argument-is-of-length-zero/
  inter.poly.12.1 <- if(isTRUE(inter.poly.12.1) && inter.poly.12.1 == circle.poly.12){inter.poly.12.1 <- data.frame()}else{inter.poly.12.1}
  ## create poolygon of remaining circle after first edge polygone is intersected
  # create poly with remaining area: https://gis.stackexchange.com/questions/353633/r-spatial-erase-one-polygon-from-another-correct-use-of-st-difference
  remaining.circle.12.1 <- if(isTRUE(length(inter.poly.12.1) == 0)==TRUE){circle.poly.12}else{sf::st_difference(circle.poly.12, inter.poly.12.1)}
  # poly 2
  ## create polygone of intersecting area of second polygone with remaining circle
  inter.poly.12.2 <- st_intersection(remaining.circle.12.1, triangle.poly.2)
  inter.status.poly.12.2 <- ifelse(length(inter.poly.12.2) == 0, "no intersections",
                                   ifelse(my.e.id.2 == 1 & inter.poly.12.2== remaining.circle.12.1,  "no intersections",
                                          ifelse(my.e.id.2 == 2 & inter.poly.12.2 == remaining.circle.12.1, "fully covering circle", 
                                                 "partly intersecting")))
  # if the second edge covers all of the circle remaining its going to be set to 0 so we know there are no direct intersections
  # https://www.statology.org/r-argument-is-of-length-zero/
  inter.poly.12.2 <- if(isTRUE(inter.poly.12.2) && inter.poly.12.2 == remaining.circle.12.1){inter.poly.12.2 <- data.frame()}else{inter.poly.12.2}
  ## create polygone of the  remaining cricle after both intersects are decucted
  # so the area of the frst remining circle minus the area of the second remaining circle 
  remaining.circle.12.1.and.2.poly <- if(isTRUE(length(inter.poly.12.2) == 0)==TRUE){remaining.circle.12.1}else{sf::st_difference(remaining.circle.12.1, inter.poly.12.2)}
 
  
  ###  circle 5 intersection between circle and triangle
  # poly 1
  inter.poly.5.1 <- sf::st_intersection(circle.poly.5, triangle.poly.1)
  inter.status.poly.5.1 <- ifelse(length(inter.poly.5.1) == 0, "no intersections",
                                  ifelse(my.e.id.1 == 1 & inter.poly.5.1 == circle.poly.5,  "no intersections",
                                         ifelse(my.e.id.1 == 2 & inter.poly.5.1 == circle.poly.5, "fully covering circle", 
                                                "partly intersecting")))
  # if the first ednge covers all of the circle remaining its going to be set to 0 so we know there are no direct intersections and the circle is passed on to the next edge to calcualte the intersection
  # https://www.statology.org/r-argument-is-of-length-zero/
  inter.poly.5.1 <- if(isTRUE(inter.poly.5.1) && inter.poly.5.1$geometry == circle.poly.5$geometry){inter.poly.5.1 <- data.frame()}else{inter.poly.5.1}
  ## create poolygon of remaining circle after first edge polygone is intersected
  # create poly with remaining area: https://gis.stackexchange.com/questions/353633/r-spatial-erase-one-polygon-from-another-correct-use-of-st-difference
  remaining.circle.5.1 <- if(isTRUE(length(inter.poly.5.1) == 0) == TRUE){circle.poly.5}else{sf::st_difference(circle.poly.5, inter.poly.5.1)}
  # poly 2
  ## create polygone of intersecting area of second polygone with remaining circle
  inter.poly.5.2 <- st_intersection(remaining.circle.5.1, triangle.poly.2)
  inter.status.poly.5.2 <- ifelse(length(inter.poly.5.2) == 0, "no intersections",
                                  ifelse(my.e.id.2 == 1 & inter.poly.5.2 == remaining.circle.5.1,  "no intersections",
                                         ifelse(my.e.id.2 == 2 & inter.poly.5.2 == remaining.circle.5.1, "fully covering circle", 
                                                "partly intersecting")))
  # if the second edge covers all of the circle remaining its going to be set to 0 so we know there are no direct intersections
  # https://www.statology.org/r-argument-is-of-length-zero/
  inter.poly.5.2 <- if(isTRUE(inter.poly.5.2) && inter.poly.5.2 == remaining.circle.5.1){inter.poly.5.2 <- data.frame()}else{inter.poly.5.2}
  ## create polygone of the  remaining cricle after both intersects are decucted
  # so the area of the frst remining circle minus the area of the second remaining circle 
  remaining.circle.5.1.and.2.poly <- if(isTRUE(length(inter.poly.5.2) == 0)==TRUE){remaining.circle.5.1}else{sf::st_difference(remaining.circle.5.1, inter.poly.5.2)}
  
  
  ## calculate the area od the intersection polygones and the remaining cirlce
  ## 17m circle
  # area of the intersection 1
  inter.1.area.17 <- ifelse(length(inter.poly.17.1) == 0, 0, sf::st_area(inter.poly.17.1))
  # area of the intersection polygone 2
  inter.2.area.17 <- ifelse(length(inter.poly.17.2) == 0, 0, sf::st_area(inter.poly.17.2))
  #  area of the remaining circle, after both intersections are deducted
  remaining.circle.area.17 <- sf::st_area(remaining.circle.17.1.and.2.poly)
  # save area in dataframe
  inter.area.df.17 <- as.data.frame(
    cbind(
      "plot_ID" = c(my.plot.id, my.plot.id, my.plot.id), 
      "e_ID" = c(my.e.id.1, my.e.id.2, 0), 
      "e_form" = c(my.e.form.1, my.e.form.2, 0),
      "shape" = c("edge", "edge", "circle"),
      "inter_stat" = c(inter.status.poly.17.1, inter.status.poly.17.2, 0),
      "CCS_radius" = c(17.84, 17.84,  17.84), 
      "area_m2" = c(inter.1.area.17, inter.2.area.17, remaining.circle.area.17)
    ))
  ## 12m circle
  # area of the intersection 1
  inter.1.area.12 <- ifelse(length(inter.poly.12.1) == 0, 0, sf::st_area(inter.poly.12.1))
  # area of the intersection polygone 2
  inter.2.area.12 <- ifelse(length(inter.poly.12.2) == 0, 0, sf::st_area(inter.poly.12.2))
  #  area of the remaining circle, after both intersections are deducted
  remaining.circle.area.12 <- sf::st_area(remaining.circle.12.1.and.2.poly)
  # save area in dataframe
  inter.area.df.12 <- as.data.frame(
    cbind(
      "plot_ID" = c(my.plot.id, my.plot.id, my.plot.id), 
      "e_ID" = c(my.e.id.1, my.e.id.2, 0), 
      "e_form" = c(my.e.form.1, my.e.form.2, 0),
      "shape" = c("edge", "edge", "circle"),
      "inter_stat" = c(inter.status.poly.12.1, inter.status.poly.12.2, 0),
      "CCS_radius" = c(12.62,  12.62,   12.62), 
      "area_m2"  = c(inter.1.area.12, inter.2.area.12, remaining.circle.area.12)
    ))
  ## 5m circle
  # area of the intersection 1
  inter.1.area.5 <- ifelse(length(inter.poly.5.1) == 0, 0, sf::st_area(inter.poly.5.1))
  # area of the intersection polygone 2
  inter.2.area.5 <- ifelse(length(inter.poly.5.2) == 0, 0, sf::st_area(inter.poly.5.2))
  #  area of the remaining circle, after both intersections are deducted
  remaining.circle.area.5 <- sf::st_area(remaining.circle.5.1.and.2.poly)
  # save area in dataframe
  inter.area.df.5 <- as.data.frame(
    cbind(
      "plot_ID" = c(my.plot.id, my.plot.id, my.plot.id), 
      "e_ID" = c(my.e.id.1, my.e.id.2, 0), 
      "e_form" = c(my.e.form.1, my.e.form.2, 0),
      "shape" = c("edge", "edge", "circle"),
      "inter_stat" = c(inter.status.poly.5.1, inter.status.poly.5.2, 0),
      "CCS_radius" = c(5.64,  5.64,   5.64),
      "area_m2" = c(inter.1.area.5, inter.2.area.5, remaining.circle.area.5)
    ))
  
  
  # bind all are dataframes of each sampling circuit together
  edge_area_df <- rbind(inter.area.df.5, inter.area.df.12, inter.area.df.17)
  # export binded area dataframes into final list
  two.edges.area.list[[i]] <- edge_area_df
  
  # ## save intersection polygones in list
  # # poly.1
  # inter.poly.17.1.list[[i]] <- if(length(inter.poly.17.1)!= 0){c("e_id" = my.e.id.1, "id" = my.plot.id, "e_form" = my.e.form.1, "geometry" = inter.poly.17.1)
  # }else{c("e_id" = my.e.id.1, "id" = my.plot.id, "e_form" = my.e.form.1, "geometry" = triangle.poly.2)}
  # # poly.2
  # inter.poly.17.2.list[[i]] <- if(nrow(inter.poly.2)!= 0){c("e_id" = my.poly.2$e_id, "id" = my.poly.2$id, "e_form" = my.poly.2$e_form, inter.poly.2)
  # }else{c("e_id" = my.poly.2$e_id, "id" = my.poly.2$id, "e_form" = my.poly.2$e_form, my.poly.2)}
  # 
  # ## save the reimaingf circle polygones in a list
  # # create list wit polygones of the remaining cirlce when it´s only one polygone
  # rem.circle.poly.2.edges.list[[i]] <- if(st_geometry_type(remaining.circle.1.and.2.poly)== "POLYGON"){c("e_id" = 0, remaining.circle.1.and.2.poly)}else{}
  # # create list wit polygones of the remaining cirlce when it´s a multipoligone
  # rem.circle.multipoly.2.edges.list[[i]] <- if(st_geometry_type(remaining.circle.1.and.2.poly)== "MULTIPOLYGON"){c("e_id" = 0, remaining.circle.1.and.2.poly)}else{}
  
  
  
  
  # sort trees according to their allocation into remaining circle and intersection polygones
  my.trees.df <- trees_and_edges %>% filter(plot_ID == my.plot.id)
  for (i in 1:length(my.trees.df$tree_ID)) {
    # i=1
    x.tree <-(my.trees.df$Dist_cm[i])/100*sin(my.trees.df$azi_gon[i])      # this is: easting, longitude, RW
    y.tree <- (my.trees.df$Dist_cm[i])/100*cos(my.trees.df$azi_gon[i])
    my.tree.matrix <- matrix(c(x.tree, y.tree), ncol = 2, byrow = TRUE)
    pt.tree <- sf::st_point(my.tree.matrix)
    edge.1_tree_inter <- sf::st_intersection(inter.poly.17.1, pt.tree)
    edge.2_tree_inter <- sf::st_intersection(inter.poly.17.2, pt.tree)
    rem_circ_tree_inter <- sf::st_intersection(remaining.circle.17.1.and.2.poly, pt.tree)
    # assign tree status depending on location in intersection poly 1, 2 or remainig circle
    tree.status = ifelse(isTRUE(length(edge.1_tree_inter) != 0), "B",
                         ifelse(isTRUE(length(edge.2_tree_inter) != 0), "C",
                                ifelse(isTRUE(length(rem_circ_tree_inter) != 0), "A", "warning")))
    my.trees.df$tree_stat_poly[[i]] <- tree.status
    
  }
  
  # 
  # print(plot(circle.poly.17, col = "red", main = paste0(my.plot.id, "-", my.e.id.1, "-", my.e.id.2, "-", "circle 3")),
  #       plot(inter.poly.17.1, col = "blue", add = T), 
  #       plot(inter.poly.17.2, col = "green", add = T))
  # # print(plot(circle.poly.12, col = "red", main = paste0(my.plot.id, "-", my.e.id, "circle 2")),
  # #       plot(inter.poly.12, col = "blue", add = T))
  # # print(plot(circle.poly.5, col = "red", main = paste0(my.plot.id, "-", my.e.id, "circle 1")),
  # #       plot(inter.poly.5, col = "blue", add = T))
  
  
  
  
  
}


two.edges.area.final <- rbindlist(two.edges.area.list)
two.edges.area.df <- as.data.frame(two.edges.area.final)


two.edges.tree.status.final <- rbindlist(one.edge.tree.status)
two.edges.tree.df <- as.data.frame(one.edge.tree.status.final)





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
              select(plot_ID, X2_inter_AB_17, X_B, Y2_inter_AB_17, Y_B) %>% 
              to_long(keys = c("X_name",  "Y_name"),
                      values = c( "X_value", "Y_value"),
                      names(.)[2:3], names(.)[4:5]),  
            aes(x= X_value, y = Y_value, colour = X_name))+
  # trees
  geom_point(data =  trees_and_edges %>% filter(e_form == "1"),
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
  mutate(AB_east_inter_1 = RW_MED + intersection_line_circle(b0_AB, b1_AB, data_circle$x0[3], data_circle$x0[3], data_circle$rmax[3], coordinate = "x1"), # X1_inter_AB_17,
         AB_north_inter_1 = HW_MED + intersection_line_circle(b0_AB, b1_AB, data_circle$x0[3], data_circle$x0[3], data_circle$rmax[3], coordinate = "y1"), #Y1_inter_AB_17,
         AB_east_inter_2 =  RW_MED + intersection_line_circle(b0_AB, b1_AB, data_circle$x0[3], data_circle$x0[3], data_circle$rmax[3], coordinate = "x2"), # X2_inter_AB_17,
         AB_north_inter_2 = HW_MED + intersection_line_circle(b0_AB, b1_AB, data_circle$x0[3], data_circle$x0[3], data_circle$rmax[3], coordinate = "y2")) %>%  #Y2_inter_AB_17,) %>% 
  # calculate point in right angle and 60m distance to each intersection with the 30m circle to create a square 
  mutate(D_east = coord(AB_east_inter_1, AB_north_inter_1, data_circle$r0[3]*2, 100, coordinate = "x"),
         D_north = coord(AB_east_inter_1, AB_north_inter_1, data_circle$r0[3]*2, 100, coordinate = "y"),
         E_east = coord(AB_east_inter_2, AB_north_inter_2, data_circle$r0[3]*2, 100, coordinate = "x"),
         # introduce end point for polygone -> has to be closed
         E_north = coord(AB_east_inter_2, AB_north_inter_2, data_circle$r0[3]*2, 100, coordinate = "y"),
         end_east = AB_east_inter_1, 
         end_north = AB_north_inter_1, 
         east_AT_inter_triangle_60 = RW_MED + inter.for.triangle(b0_AT, b1_AT, data_circle$x0[3], data_circle$y0[3], data_circle$rmax[3]*2, X_A, Y_A, X_T, Y_T, coordinate = "x"),
         north_AT_inter_triangle_60 = HW_MED + inter.for.triangle(b0_AT, b1_AT, data_circle$x0[3], data_circle$y0[3], data_circle$rmax[3]*2, X_A, Y_A, X_T, Y_T, coordinate = "y"),
         east_BT_inter_triangle_60 = RW_MED + inter.for.triangle(b0_BT, b1_BT, data_circle$x0[3], data_circle$y0[3], data_circle$rmax[3]*2, X_B, Y_B, X_T, Y_T, coordinate = "x"),
         north_BT_inter_triangle_60 = HW_MED + inter.for.triangle(b0_BT, b1_BT, data_circle$x0[3], data_circle$y0[3], data_circle$rmax[3]*2, X_B, Y_B, X_T, Y_T, coordinate = "y"), 
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
  filter(plot_ID == 50042) %>% 
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
circle.17 <- terra::buffer(center.points, 17.84)

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

# comparing it with the edge area caclucated by the function under plot_A in the "trees_and_edges" dataset
trees_and_edges %>% 
  filter(plot_ID == 50042 & trees_and_edges$DBH_cm >30 & t_status_AB_ABT == "B")


# plot circle and trianle
ggplot() +  
  geom_circle(data = FE_loc_HBI.e2 %>% filter(X_name == "RW_MED" & Y_name == "HW_MED"), aes(x0 = lat, y0 = lon, r = 30.00*2))+ # Draw ggplot2 plot with circle representing sampling circuits 
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


jeremy.test.df <- rbind(forest_edges_HBI.man %>%
                          filter(e_form == 2 ) %>%
                          mutate(id_func = row_number()) %>%
                          group_by(id_func) %>%
                          # test the edge form 2 function
                          mutate(jeremy.test = triangle.circle.intersection(
                            inter.for.triangle(b0_AT, b1_AT, data_circle$x0[3], data_circle$y0[3], data_circle$rmax[3]*10 , X_A, Y_A, X_T, Y_T, coordinate = "x"), 
                            inter.for.triangle(b0_AT, b1_AT, data_circle$x0[3], data_circle$y0[3], data_circle$rmax[3]*10 , X_A, Y_A, X_T, Y_T, coordinate = "y"),
                            inter.for.triangle(b0_BT, b1_BT, data_circle$x0[3], data_circle$y0[3], data_circle$rmax[3]*10 , X_B, Y_B, X_T, Y_T, coordinate = "x"), 
                            inter.for.triangle(b0_BT, b1_BT, data_circle$x0[3], data_circle$y0[3], data_circle$rmax[3]*10 , X_B, Y_B, X_T, Y_T, coordinate = "y"),
                            X_T, Y_T, 
                            17.84
                          ))%>% 
                          ungroup() %>% 
                          select(plot_ID, e_ID, e_form,inter_status_AB_17,  inter_status_AT_17, inter_status_BT_17, jeremy.test), 
                        # test the CircleSegment function 
                        forest_edges_HBI.man  %>%
                          filter(e_form == 1) %>% 
                          mutate(id_func = row_number()) %>%
                          group_by(id_func) %>%
                          mutate(jeremy.test = CircleSegmnent(X1_inter_AB_17, Y1_inter_AB_17, X2_inter_AB_17, Y2_inter_AB_17, 17.84))%>%
                          ungroup() %>% 
                          select(plot_ID, e_ID, e_form, inter_status_AB_17,  inter_status_AT_17, inter_status_BT_17, jeremy.test)
) %>% 
  # check area function 
  left_join(., 
            forest_edges_HBI.man %>% 
              mutate(id_func = row_number()) %>%
              group_by(id_func) %>%
              mutate(area.func.test = edge.area(e_form, 35, X_A, X_B, X_T, Y_A, Y_B, Y_T, T_dist, "B")) %>% 
              ungroup() %>%
              select(plot_ID, e_ID, e_form, area.func.test), 
            by = c("plot_ID", "e_ID", "e_form")) %>%
  # check georef results
  left_join(., 
            all.edges.area.df %>% 
              mutate(id = as.integer(id),
                     e_id = as.integer(e_id),
                     e_form = as.integer(e_form), 
                     area_m2 = as.numeric(area_m2)) %>% 
              select(id, e_id, e_form, area_m2), 
            by = c(c("plot_ID" = "id"), c("e_ID"="e_id"), "e_form")) %>% 
  mutate(diff_jeremy.circleseg_vs_area.func = jeremy.test - area.func.test, 
         diff_jeremy.circleseg_vs_georef = jeremy.test - area_m2,
         diff_area.func_vs_georef = area.func.test - area_m2) %>% 
  mutate(warn_jeremy.circleseg_vs_area.func = ifelse(jeremy.test != area.func.test, "WARN", "FINE"), 
         warn_jeremy.circleseg_vs_georef = ifelse(jeremy.test != area_m2, "WARN", "FINE"), 
         warn_area.func_vs_georef = ifelse( area.func.test != area_m2, "WARN", "FINE"))

summary(jeremy.test.df)

jeremy.test.df %>% 
  filter(warn_area.func_vs_georef == "WARN" & diff_area.func_vs_georef > 5 |
           warn_area.func_vs_georef == "WARN" & diff_area.func_vs_georef < -5)



jeremy(0,1,1,0,1,1,2)
plot(circle.poly)

CircleSegmnent()

pt.circle <- sf::st_point(c(0,0))
circle.poly <- sf::st_buffer(pt.circle, dist = 0)
x =  matrix(c(0,1,1,0,1,1,0,1), ncol=2, byrow = TRUE)
x.poly <- sf::st_polygon(list(x))
plot(circle.poly)
plot(x.poly, add =T)
st_area(st_intersection(circle.poly, x.poly))




e1.test.df <- forest_edges_HBI.man %>% select(plot_ID, e_ID, e_form, 
                                              X_T, Y_T,
                                              X_A, 
                                              Y_A, 
                                              X_B,
                                              Y_B) %>%
  filter(e_form == 1) %>% 
  #filter(plot_ID == 50006 ) %>%
  mutate(CircleSegmnent.test = CircleSegmnent(X_A, 
                                              Y_A, 
                                              X_B,
                                              Y_B, 
                                              17.84)) 

e1.test.df <- e1.test.df %>% 
  left_join(., all.edges.area.df %>% filter(e_form == 1) %>% 
              mutate(id = as.integer(id), 
                     e_id = as.integer(e_id),
                     e_form = as.integer(e_form), 
                     area_m2 = as.numeric(area_m2)) %>% 
              select(id, e_id, e_form, area_m2), 
            by = c(c("plot_ID" = "id"), c("e_ID"="e_id"), "e_form")) %>% 
  mutate(area.diff= CircleSegmnent.test-area_m2) %>% 
  arrange(area.diff)

summary(e1.test.df)

x1 =25.3
x2 = -20.9
y1 = 8.72
y2 = -19.0 
r = 17.84
# standart line equation parameters: onmicalculator.com
A = y2-y1
B = x2 - x1
C = y1*B - A*x1
# shortest distance between center and AB line: chilimath.com
# center is always 0|0
d = abs(C)/sqrt(A^2+B^2)
# height of the cirlce regment between line and circle perimeter
h = ifelse(d<=r, r-d, 0)
# calculate area of cirlce segment with heigth and radius : wikipedia.de
area = r^2*acos(1-(h/r))-(r-h)*sqrt(r^2-(r-h)^2)




# 3.2.1.1. creating list of polygones for circles (17.84m) per plot  -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# dataset with only edge forms 1 and 2 
forest_edges_HBI.man.sub <- forest_edges_HBI.man %>% 
  filter(e_form %in% c(1, 2)) %>% 
  semi_join(HBI_loc %>% filter(!is.na( RW_MED) & !is.na(HW_MED)) %>%  select(plot_ID)  %>% distinct(), by = "plot_ID") 



## loop to create list with polygones for circles per plot center 
# create empty list to store circle polygones in 
circle.list <- vector("list", length = length(forest_edges_HBI.man.sub$plot_ID))
for(i in 1:length(forest_edges_HBI.man.sub$plot_ID)) {
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
                                   "lat" = my.center.northing, 
                                   "lon" = my.center.easting))
  
  # create sf point with center coordiantes
  center.point <- sf::st_as_sf(center.df, coords = c("lon", "lat"), crs = my.utm.epsg)
  # build polygon (circlular buffer) around center point
  circle.17 <- sf::st_buffer(center.point, 17.84)
  # circle.12 <- sf::st_buffer(center.point, 12.62)
  # circle.5 <- sf::st_buffer(center.point, 5.64)
  
  
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
  # circle.list[[i]] <- rbind(circle.17, circle.12, circle.5)
  circle.list[[i]] <- circle.17
  
}
# circle.list
circle.list.final <- rbindlist(circle.list)
circle.poly.df <- as.data.frame(circle.list.final)





# 3.2.1.2. creating list of squared polygones for eddge form 1  -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## loop to create list of polygones for edge form 1
#forest_edges_HBI.man.sub.e1 <- forest_edges_HBI.man%>% filter(e_form == 1)#%>% filter(inter_status_AB_17 == "two I") # 63 of edge form 1 -> with intersection 43

forest_edges_HBI.man.sub.e1 <-  forest_edges_HBI.man%>% filter(e_form == 1) %>% 
  semi_join(HBI_loc %>% filter(!is.na( RW_MED) & !is.na(HW_MED)) %>%  select(plot_ID)  %>% distinct(), by = "plot_ID") # 62

square.list <- vector("list", length = length(forest_edges_HBI.man.sub.e1$plot_ID))
square.coords <- vector("list", length = length(forest_edges_HBI.man.sub.e1$plot_ID)*5)

for(i in 1:length(forest_edges_HBI.man.sub.e1$plot_ID) ) {
  # i = 37
  # i = which(grepl(50086, forest_edges_HBI.man.sub.e1$plot_ID))
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
  x.A <- dist.A*sin(azi.A)       # this is: easting, longitude, RW
  y.A <- dist.A*cos(azi.A)       # this is: northing, latitude, HW
  
  # point B
  dist.B <- forest_edges_HBI.man.sub.e1[i, "B_dist"] 
  azi.B <- forest_edges_HBI.man.sub.e1[i, "B_azi"] 
  x.B <- dist.B*sin(azi.B)      # this is: easting, longitude, RW
  y.B <- dist.B*cos(azi.B)      # this is: northing, latitude, HW
  
  b1 <- (y.B- y.A)/(x.B - x.A)
  b0 <- y.B - b1*x.B
  
  # calculate polar coordiantes of intersections of AB line with 
  AB.inter.x1 <- intersection_line_circle(b0, b1, data_circle$x0[3], data_circle$x0[3], data_circle$rmax[3]*2, coordinate = "x1") # this is: easting, longitude, RW
  AB.inter.y1 <- intersection_line_circle(b0, b1, data_circle$x0[3], data_circle$x0[3], data_circle$rmax[3]*2, coordinate = "y1") # this is: northing, latitude, HW
  AB.inter.x2 <- intersection_line_circle(b0, b1, data_circle$x0[3], data_circle$x0[3], data_circle$rmax[3]*2, coordinate = "x2") # this is: easting, longitude, RW
  AB.inter.y2 <- intersection_line_circle(b0, b1 ,data_circle$x0[3], data_circle$x0[3], data_circle$rmax[3]*2, coordinate = "y2") # this is: northing, latitude, HW
  
  my.inter.status <- intersection.status(AB.inter.x1, AB.inter.x2)
  
  
  # for edge form 1 i have to cinsiderthat the square has ot "look" into the direction of the shorter side
  # calculate coordiantes of the middle of thie line between 
  x_m_line = (AB.inter.x1 + AB.inter.x2)/2
  y_m_line = (AB.inter.y1 + AB.inter.y2)/2
  # calculate the parameters of the equation between the middle of the line and the centre of the circle
  b1_MC = slope(data_circle$x0[3], data_circle$y0[3], x_m_line, y_m_line)
  b0_MC = intercept(data_circle$x0[3], data_circle$y0[3], x_m_line, y_m_line)
  # calcualte the x corrdiante of the interception of the line between M and the centre of the cirle and the circle at the given radio
  X1_inter_MC = intersection_line_circle(b0_MC, b1_MC, data_circle$x0[3], data_circle$y0[3], data_circle$rmax[3]*2, coordinate = "x1") 
  X2_inter_MC = intersection_line_circle(b0_MC, b1_MC,  data_circle$x0[3], data_circle$y0[3], data_circle$rmax[3]*2, coordinate = "x2")
  # insert the intersection x corodinate in the line function to get the respective y coordinate
  y1_inter_MC = intersection_line_circle(b0_MC, b1_MC,  data_circle$x0[3], data_circle$y0[3], data_circle$rmax[3]*2, coordinate = "y1") 
  y2_inter_MC = intersection_line_circle(b0_MC, b1_MC,  data_circle$x0[3], data_circle$y0[3], data_circle$rmax[3]*2, coordinate = "y2")
  # distance between the intersections (inter_MC_1, inter_MC_2) to M on the line 
  dist_C_inter_1_MC = distance(X1_inter_MC, y1_inter_MC, x_m_line, y_m_line)
  dist_C_inter_2_MC = distance(X2_inter_MC, y2_inter_MC, x_m_line, y_m_line) 
  # find the x and y coordinate of the intersection on the shorter side , which is the side to exlcude from the plot 
  X_inter_MC_shorter_side = ifelse(dist_C_inter_1_MC < dist_C_inter_2_MC, X1_inter_MC, X2_inter_MC) 
  Y_inter_MC_shorter_side = ifelse(dist_C_inter_1_MC < dist_C_inter_2_MC, y1_inter_MC, y2_inter_MC)
  
  azi.for.square.corners <- azi(X_inter_MC_shorter_side, Y_inter_MC_shorter_side, x_m_line, y_m_line)
  
  #  polar coordinates of the corner points of square along the intersecting line AB 
  # by going 60m distance in a right angle from the respective intersection coordiante
  x.D <- coord(AB.inter.x1, AB.inter.y1,  data_circle$rmax[3]*2, azi.for.square.corners, coordinate = "x")  # this is: easting, longitude, RW
  y.D <- coord(AB.inter.x1, AB.inter.y1,  data_circle$rmax[3]*2, azi.for.square.corners, coordinate = "y")  # this is: northing, latitude, HW
  x.E <- coord(AB.inter.x2, AB.inter.y2,  data_circle$rmax[3]*2, azi.for.square.corners, coordinate = "x")  # this is: easting, longitude, RW
  y.E <- coord(AB.inter.x2, AB.inter.y2,  data_circle$rmax[3]*2, azi.for.square.corners, coordinate = "y")  # this is: northing, latitude, HW
  
  # test.df<- as.data.frame(cbind(x <- c(0, AB.inter.x1, AB.inter.x2, x_m_line, X1_inter_MC, X2_inter_MC, x.D, x.E), 
  #                                y <- c(0, AB.inter.y1, AB.inter.y2, y_m_line, y1_inter_MC, y2_inter_MC, y.D, y.E),
  #                                type <- c("center", "AB.inter.x1", "AB.inter.x2", "x_m_line", "X1_inter_MC", "X2_inter_MC", "x.D", "x.E")
  #                                ))
  #  print(ggplot()+
  #    geom_circle(data = data_circle, aes(x0 = x0, y0 = y0, r = r0))+ # Draw ggplot2 plot with circle representing sampling circuits 
  #    geom_circle(data = data_circle, aes(x0 = x0, y0 = y0, r = rmax))+ # Draw ggplot2 plot with circle representing sampling circuits
  #    geom_circle(data = data_circle, aes(x0 = x0, y0 = y0, r = rmax*2))+ # Draw ggplot2 plot with circle representing sampling circuits
  #   geom_point(data = test.df, aes(x = x, y = y, color = type)))
  
  # UTM coordiantes of corner points 
  AB.inter.1.east <- my.center.easting + AB.inter.x1 
  AB.inter.1.north <- my.center.northing + AB.inter.y1
  AB.inter.2.east <- my.center.easting + AB.inter.x2 
  AB.inter.2.north <- my.center.northing + AB.inter.y2
  D.east <- my.center.easting + x.D
  D.north <- my.center.northing + y.D
  E.east <- my.center.easting + x.E
  E.north <- my.center.northing + y.E
  
  # creating the turning point od a triangle by selecting the intersection of the 
  # line from the middle of the AB.inter-ray and the circle center (MC_line) with 
  # the 60m radius at the "shorter side" so the intersection of the MC_line with a 60m radius that has le lest distance to the MC point on the AB.inter-ray
  turning.east <- my.center.easting + X_inter_MC_shorter_side
  turning.north <- my.center.northing + Y_inter_MC_shorter_side
  
  # create dataframe that holds coordinates of 
  square.df <- as.data.frame(cbind("lon" = c(AB.inter.1.east, D.east, E.east, AB.inter.2.east, AB.inter.1.east),
                                   "lat" = c(AB.inter.1.north, D.north, E.north, AB.inter.2.north, AB.inter.1.north),
                                   "id" = c(my.plot.id, my.plot.id, my.plot.id, my.plot.id, my.plot.id),
                                   "e_id" = c(my.e.id, my.e.id, my.e.id, my.e.id, my.e.id )
  ))%>% 
    mutate(lat = as.integer(lat), 
           lon = as.integer(lon)) %>% 
    unite("geometry", c(lon, lat), sep = " ", remove = FALSE)%>%
    mutate(geometry = as.factor(geometry))# %>% 
  #select(geometry)
  
  triangle.e1.df <- as.data.frame(cbind("lon" = c(turning.east, AB.inter.1.east, AB.inter.2.east, turning.east),
                                        "lat" = c(turning.north, AB.inter.1.north, AB.inter.2.north,  turning.north),
                                        "id" = c(my.plot.id, my.plot.id, my.plot.id, my.plot.id),
                                        "e_id" = c(my.e.id, my.e.id, my.e.id, my.e.id)))%>%
    mutate(lat = as.integer(lat), 
           lon = as.integer(lon)) %>% 
    unite("geometry", c(lon, lat), sep = " ", remove = FALSE)%>%
    mutate(geometry = as.factor(geometry))# %>% 
  #select(geometry)
  
  ##creating squares in terrra: 
  #square.poly <- terra::vect(c(paste("POLYGON", "(", "(", paste(square.df$geometry[1], square.df$geometry[2], square.df$geometry[3], square.df$geometry[4], square.df$geometry[5], sep = ", "), ")", ")", sep = "")), crs="epsg:25833")
  
  
  # creating squeares in sf: https://stackoverflow.com/questions/61215968/creating-sf-polygons-from-a-dataframe
  square.poly <- sfheaders::sf_polygon(obj = triangle.e1.df  ##### !!! change back to square.df if you want squares
                                       , x = "lon"
                                       , y = "lat"
                                       , polygon_id = "id")
  # assing crs
  sf::st_crs(square.poly) <- my.utm.epsg
  
  print(plot(square.poly$geometry, main = my.plot.id))
  
  square.list[[i]] <- c("e_id" = my.e.id, square.poly)
  
  # save coordiantes of polygones in list
  square.coords[[i]] <- square.df
  
} # closing loop for square polys of edge form 1

square.list.final <- rbindlist(square.list)
square.poly.df <- as.data.frame(square.list.final) %>% mutate("e_form" = 1)

square.coords.list <- rbindlist(square.coords)
square.coords.df <- as.data.frame(square.coords.list) %>% 
  mutate("e_form" = 1)




# 3.2.1.2. creating list of triangle polygons for edge form 2 ----------------------------------------------------------------------------------------------------------------------------------------------------------------

## loop to create list of polygones for edge form 1
forest_edges_HBI.man.sub.e2 <- forest_edges_HBI.man%>% filter(e_form == 2) %>%  # nrow = 21
  semi_join(HBI_loc %>% filter(!is.na( RW_MED) & !is.na(HW_MED)) %>%  select(plot_ID)  %>% distinct(), by = "plot_ID")  # nrow = 21

triangle.list <- vector("list", length = length(forest_edges_HBI.man.sub.e2$plot_ID) )

triangle.coords <- vector("list", length = length(forest_edges_HBI.man.sub.e2$plot_ID)*4 )

for(i in 1:length(forest_edges_HBI.man.sub.e2$plot_ID) ) {
  # i = 1
  # i = which(grepl(50102, forest_edges_HBI.man.sub.e2$plot_ID))
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
  x.A <- dist.A*sin(azi.A)   # longitude, easting, RW, X
  y.A <- dist.A*cos(azi.A)   # latitude, northing, HW, y 
  
  # point B
  dist.B <- forest_edges_HBI.man.sub.e2[i, "B_dist"] 
  azi.B <- forest_edges_HBI.man.sub.e2[i, "B_azi"] 
  x.B <- dist.B*sin(azi.B)   # longitude, easting, RW, X
  y.B <- dist.B*cos(azi.B)   # latitude, northing, HW, y 
  
  # point T
  dist.T <- forest_edges_HBI.man.sub.e2[i, "T_dist"] 
  azi.T <- forest_edges_HBI.man.sub.e2[i, "T_azi"] 
  x.T <- dist.T*sin(azi.T)   # longitude, easting, RW, X
  y.T <- dist.T*cos(azi.T)   # latitude, northing, HW, y 
  
  
  # select polar coordiantes of the points of the triangle corners via "inter_for_triangle"-function
  # for AT side
  AT.triangle.x <- inter.for.triangle(intercept(x.T, y.T, x.A, y.A), slope(x.T, y.T, x.A, y.A), 
                                      data_circle$x0[3], data_circle$y0[3], data_circle$rmax[3]*3, 
                                      x.A, y.A, x.T, y.T, 
                                      coordinate = "x")                              # longitude, easting, RW, X
  AT.triangle.y <- inter.for.triangle(intercept(x.T, y.T, x.A, y.A), slope(x.T, y.T, x.A, y.A), 
                                      data_circle$x0[3], data_circle$y0[3],data_circle$rmax[3]*3, 
                                      x.A, y.A, x.T, y.T, 
                                      coordinate = "y")                              # latitude, northing, HW, y 
  # for BT side
  BT.triangle.x <- inter.for.triangle(intercept(x.T, y.T, x.B, y.B),slope(x.T, y.T, x.B, y.B), 
                                      data_circle$x0[3],data_circle$y0[3],data_circle$rmax[3]*3, 
                                      x.B, y.B, x.T, y.T, 
                                      coordinate = "x")                              # longitude, easting, RW, X
  BT.triangle.y <- inter.for.triangle(intercept(x.T, y.T, x.B, y.B), slope(x.T, y.T, x.B, y.B), 
                                      data_circle$x0[3], data_circle$y0[3], data_circle$rmax[3]*3, 
                                      x.B, y.B, x.T, y.T, 
                                      coordinate = "y")                              # latitude, northing, HW, y 
  
  
  # test.df<- as.data.frame(cbind(x <- c(0, x.A, x.B, AT.triangle.x, BT.triangle.x, x.T),
  #                               y <- c(0, y.A, y.B, AT.triangle.y, BT.triangle.y, y.T),
  #                               type <- c("center", "x.A", "x.B", "AT.triangle.x", "BT.triangle.x", "x.T")
  #                                 ))
  #   print(ggplot()+
  #     geom_circle(data = data_circle, aes(x0 = x0, y0 = y0, r = r0))+ # Draw ggplot2 plot with circle representing sampling circuits 
  #     geom_circle(data = data_circle, aes(x0 = x0, y0 = y0, r = rmax))+ # Draw ggplot2 plot with circle representing sampling circuits
  #     geom_circle(data = data_circle, aes(x0 = x0, y0 = y0, r = rmax*2))+ # Draw ggplot2 plot with circle representing sampling circuits
  #       geom_circle(data = data_circle, aes(x0 = x0, y0 = y0, r = rmax*3))+
  #       geom_point(data = test.df, aes(x = x, y = y, color = type)))
  
  
  #calculate UTM coordiantes of triangle corners
  T.east <- my.center.easting + x.T                                            # longitude, easting, RW, X
  T.north <- my.center.northing + y.T                                          # latitude, northing, HW, y 
  AT.triangle.east <- my.center.easting + AT.triangle.x                        # longitude, easting, RW, X
  AT.triangle.north <- my.center.northing + AT.triangle.y                      # latitude, northing, HW, y 
  BT.triangle.east <- my.center.easting + BT.triangle.x                        # longitude, easting, RW, X
  BT.triangle.north <- my.center.northing + BT.triangle.y                      # latitude, northing, HW, y 
  
  # create dataframe with triangle corner UTM coordiantes
  triangle.df <- as.data.frame(cbind("lon" = c(T.east, AT.triangle.east, BT.triangle.east, T.east),       # longitude, easting, RW, X
                                     "lat" = c(T.north, AT.triangle.north, BT.triangle.north, T.north),   # latitude, northing, HW, y
                                     "id" =  c(my.plot.id, my.plot.id, my.plot.id, my.plot.id), 
                                     "e_id" = c(my.e.id, my.e.id, my.e.id, my.e.id )))%>%
    mutate(lon = as.integer(lon),
           lat = as.integer(lat)) %>%
    unite("geometry", c(lon, lat), sep = " ", remove = FALSE)%>%
    mutate(geometry = as.factor(geometry))
  #select(geometry)
  
  
  # terra: create polygone with corners of triangle
  #triangle.poly <- vect(c(paste("POLYGON", "(", "(", paste(triangle.df$geometry[1], triangle.df$geometry[2], triangle.df$geometry[3], triangle.df$geometry[4], sep = ", "), ")", ")", sep = "")), crs="epsg:25833")
  
  # createa polygone with triangle corners via sf package: https://r-spatial.github.io/sf/reference/st.html
  triangle.poly <- sfheaders::sf_polygon(obj = triangle.df
                                         , x = "lon"
                                         , y = "lat"
                                         , polygon_id = "id")
  # assing crs
  sf::st_crs(triangle.poly) <- my.utm.epsg
  
  # print triangle
  print(plot(triangle.poly$geometry, main = my.plot.id))
  
  # save polygones in list
  triangle.list[[i]] <- c("e_id" = my.e.id, triangle.poly)
  
  # save coordiantes of polygones in list
  triangle.coords[[i]] <- triangle.df
}


# list of polygones
triangle.list.final <- rbindlist(triangle.list)
triangle.poly.df <- as.data.frame(triangle.list.final) %>% mutate("e_form" = 2)

#list of coordiantes of triangle polygones
triangle.coords.list <- rbindlist(triangle.coords)
triangle.coords.df <- as.data.frame(triangle.coords.list) %>% 
  mutate("e_form" = 2) 


# 3.2.1.3. loop for intersections between circles and edges -------------------------------------------------------------------------------------------------------------------------------------
# 3.2.1.3.1. loop for intersections for plots with only one edge  -------------------------------------------------------------------------------------------------------------------------------

# dataprep for loop
# bind polygone dataframes together
edge.poly.df <- rbind(square.poly.df, triangle.poly.df) # rows: 83
# createa dataframe with plots that have only one forest edges
forest_edges_HBI.man.sub <- forest_edges_HBI.man %>% # rows:84
  # select only plots with a known edge form
  filter(e_form == 1 | e_form == 2) %>%  # rows:84
  # remove plots that have two edges
  anti_join(forest_edges_HBI.man %>% filter(e_form == 1 | e_form == 2) %>% group_by(plot_ID) %>% summarise(n = n()) %>% filter(n > 1) %>% select(plot_ID), by = "plot_ID") %>% # 15 plots iwth 2 edges --> 30 rows -> 54 left
  # remove plots that do now have a corresponding center coordiante in the HBI loc document
  semi_join(HBI_loc %>% filter(!is.na( RW_MED) & !is.na(HW_MED)) %>%  select(plot_ID)  %>% distinct(), by = "plot_ID") # nrow = 53 --> there are 2 plots without corresponding 

edges.list <- vector("list", length = length(unique(forest_edges_HBI.man.sub$plot_ID)))
inter.poly.list <- vector("list", length = length(unique(forest_edges_HBI.man.sub$plot_ID)))
inter.poly.NA.list <- vector("list", length = length(unique(forest_edges_HBI.man.sub$plot_ID)))
remaining.circle.poly.list <- vector("list", length = length(unique(forest_edges_HBI.man.sub$plot_ID)))
remaining.circle.multipoly.list <- vector("list", length = length(unique(forest_edges_HBI.man.sub$plot_ID)))

for (i in 1:length(unique(forest_edges_HBI.man.sub$plot_ID))){ 
  # i = 2
  
  # select plot ID of the respective circle 
  my.plot.id <- forest_edges_HBI.man.sub[i, "plot_ID"]
  my.e.form <- edge.poly.df$e_form[edge.poly.df$id == my.plot.id]
  my.e.id <- edge.poly.df$e_id[edge.poly.df$id == my.plot.id]
  
  
  # select the polygones with the same plot ID as the cirlce
  # my.plot.polys.df <- edge.poly.df %>% filter(id == my.plot.id) 
  # count the numbers of rows of the polygone datafarme of the plot
  # my.poly.count <- nrow(my.plot.polys.df)
  
  # select the circle polygone corresponding with the plot ID
  # my.circle.list.id <- which(grepl(my.plot.id, circle.df$id))
  my.circle <- sf::st_as_sf(circle.poly.df %>% filter(id == my.plot.id) %>% distinct())
  #plot(my.circle)
  
  # select the respective polygones the circle is intersected by
  my.poly <- sf::st_as_sf(edge.poly.df %>% filter(id == my.plot.id))
  
  # if(my.poly.count > 1){my.poly.2 <- sf::st_as_sf(my.plot.polys.df[2,])} else{}
  
  # print(plot(my.circle$geometry), 
  #       plot(my.poly.1$geometry, col = "blue", add = T), 
  #       plot(my.poly.2$geometry, col = "red", add = T))
  # 
  print(plot(my.circle$geometry, main = paste0(my.plot.id,  sep = ",", my.e.id)), 
        plot(my.poly$geometry, add = T))
  
  # calculate intersection for firest polygone 
  inter.poly  <- sf::st_intersection(my.circle, my.poly)
  inter.status.poly <- ifelse(nrow(inter.poly) == 0, "no intersections",
                              ifelse(inter.poly$e_id == 1 & inter.poly$geometry == my.circle$geometry,  "no intersections",
                                     ifelse(inter.poly$e_id == 2 & inter.poly$geometry == my.circle$geometry, "fully covering circle", 
                                            "partly intersecting")))
  
  # this is just to remove all the additional attributes from the intersection polygone
  inter.poly  <- sf::st_intersection(my.circle, st_geometry(my.poly))
  
  # if the ednge covers all of the circle remaining, the inter.polygone its going to be set to 0 so we know there are no direct intersections
  inter.poly <- if(isTRUE(inter.poly) && inter.poly$geometry == remaining.circle$geometry){inter.poly <- data.frame()}else{inter.poly}
  
  remaining.circle.poly  <- if(nrow(inter.poly)==0){my.circle}else{sf::st_difference(my.circle, inter.poly)}
  
  # calculate area
  # intersection
  inter.area <- ifelse(nrow(inter.poly) == 0, 0, sf::st_area(inter.poly))
  #remaining circle
  remaining.circle.area <- ifelse(nrow(remaining.circle.poly) == 0, 0, sf::st_area(remaining.circle.poly))
  # create area dataframe for areas
  inter.area.df <- as.data.frame(cbind("id" = c(my.plot.id, my.plot.id),
                                       "e_id" = c(my.e.id,  0),
                                       "e_form" = c(my.e.form, 0),
                                       "shape" = c("edge", "circle"),
                                       "inter_stat" = c(inter.status.poly, 0),
                                       "area_m2" = c(inter.area, remaining.circle.area)))
  # list with inter and remaining circle areas areas
  edges.list[[i]] <- inter.area.df
  
  # create lists with polgons of intersections if there are intersections, if there is non, save the polygone instead. 
  inter.poly.list[[i]] <- if(nrow(inter.poly)!= 0){c("e_id" = my.poly$e_id, "id" = my.poly$id, "e_form" = my.poly$e_form, inter.poly)
  }else{c("e_id" = my.poly$e_id, "id" = my.poly$id, "e_form" = my.poly$e_form, my.poly)}
  
  # inter.poly.NA.list[[i]] <- if(nrow(inter.poly)== 0){c("e_id" = my.e.id, "id" = my.plot.id, "geometry" = 0)}else{}
  
  # create list wit polygones of the remaining cirlce when it´s only one polygone
  remaining.circle.poly.list[[i]] <- if(st_geometry_type(remaining.circle.poly)== "POLYGON"){c("e_id" = 0, remaining.circle.poly)}else{}
  # create list wit polygones of the remaining cirlce when it´s a multipoligone
  remaining.circle.multipoly.list[[i]] <- if(st_geometry_type(remaining.circle.poly)== "MULTIPOLYGON"){c("e_id" = 0, remaining.circle.poly)}else{}
  
  
}

# list of areas
edges.area.list.final <- rbindlist(edges.list)
edges.area.df <- as.data.frame(edges.area.list.final)


# list of polygones of forest edges 
inter.poly.list.final <- rbindlist(inter.poly.list, fill=TRUE)
inter.poly.one.edge.df <- as.data.frame(inter.poly.list.final)[,c(2, 1, 3, 7)]%>% arrange(id, e_id)
# # select only those rows from the NA dataframe that are not intersecting
# inter.poly.list.NA.final <- rbindlist( inter.poly.NA.list[lengths(inter.poly.NA.list) > 0L])
# inter.poly.NA.df <- as.data.frame(inter.poly.list.NA.final)

# list of polygones of remainign circles 
rem.circle.poly.list.final <- rbindlist(remaining.circle.poly.list, fill = TRUE)
rem.circle.poly.df <- as.data.frame(rem.circle.poly.list.final)[,c(2,1,3)]  %>% distinct()
# list of multipolygones of remaining circles
rem.circle.multipoly.list.final <- rbindlist(remaining.circle.multipoly.list)
rem.circle.multipoly.df <- as.data.frame(rem.circle.multipoly.list.final)[,c(2,1,4)] %>% distinct()
# binding the both circle lists back together 
rem.circle.one.edge.df <- rbind(rem.circle.poly.df, rem.circle.multipoly.df)







# 3.2.1.3.1. loop for intersections for plots with two edges ----------------------------------------------------------------------------------------------------------------------------
# dataprep for loop
# createa dataframe with plots that have only one forest edges
forest_edges_HBI.man.sub.2.edges <- forest_edges_HBI.man %>% # rows:84
  # select only plots with a known edge form
  filter(e_form == 1 | e_form == 2) %>%  # rows:84
  #filter(inter_status_AB_17 == "two I") %>% 
  # remove plots that have two edges
  semi_join(forest_edges_HBI.man %>% filter(e_form == 1 | e_form == 2) %>% group_by(plot_ID) %>% summarise(n = n()) %>% filter(n > 1) %>% select(plot_ID), by = "plot_ID") %>% # 15 plots iwth 2 edges --> 30 rows -> 54 left
  # remove plots that do now have a corresponding center coordiante in the HBI loc document
  semi_join(HBI_loc %>% filter(!is.na( RW_MED) & !is.na(HW_MED)) %>%  select(plot_ID)  %>% distinct(), by = "plot_ID") # nrow = 53 --> there are 2 plots without corresponding 

# prepare output lists
# list to save areas in
edges.list.two.edges <- vector("list", length = length(unique(forest_edges_HBI.man.sub.2.edges$plot_ID)))
# list to save the first intersection polygone per plot in
inter.poly.1.list <- vector("list", length = length(unique(forest_edges_HBI.man.sub.2.edges$plot_ID)))
# list to save the second intersection polygone per plot in
inter.poly.2.list <- vector("list", length = length(unique(forest_edges_HBI.man.sub.2.edges$plot_ID)))
# list to save the remaining circle polygones per plot in
rem.circle.poly.2.edges.list <- vector("list", length = length(unique(forest_edges_HBI.man.sub.2.edges$plot_ID)))
# list to save the remaining circle MULTIpolygones per plot in
rem.circle.multipoly.2.edges.list <- vector("list", length = length(unique(forest_edges_HBI.man.sub.2.edges$plot_ID)))

for (i in 1:length(unique(forest_edges_HBI.man.sub.2.edges$plot_ID))){ 
  #i = 1
  
  # select plot ID of the respective circle 
  my.plot.id <- unique(forest_edges_HBI.man.sub.2.edges$plot_ID)[i]
  # my.e.form <- edge.poly.df$e_form[edge.poly.df$id == my.plot.id]
  #my.e.id <- edge.poly.df$e_id[edge.poly.df$id == my.plot.id]
  
  
  # select the polygones with the same plot ID as the cirlce
  my.plot.polys.df <- edge.poly.df %>% filter(id == my.plot.id) %>% arrange(e_id)
  
  # select the circle polygone corresponding with the plot ID
  # my.circle.list.id <- which(grepl(my.plot.id, circle.df$id))
  my.circle <- sf::st_as_sf(circle.poly.df %>% filter(id == my.plot.id) %>% distinct())
  #plot(my.circle)
  
  # select the respective polygones the circle is intersected by
  my.poly.1 <- sf::st_as_sf(my.plot.polys.df[1,])
  my.poly.2 <- sf::st_as_sf(my.plot.polys.df[2,])
  
  
  #  print(plot(my.poly.1$geometry, main= my.plot.id), 
  #       plot(my.poly.2$geometry,
  #  col = "red", 
  #           add = T), 
  #     plot(my.circle$geometry,
  #   col = "blue", 
  #         add = T))
  
  #  print(plot(my.circle$geometry), 
  #       plot(my.poly.1$geometry, add = T), 
  #       plot(my.poly.2$geometry, add = T))
  
  # calculate intersection for firest polygone 
  inter.poly.1  <- st_intersection(my.circle, my.poly.1)
  inter.status.poly.1 <- ifelse(nrow(inter.poly.1) == 0, "no intersections",
                                ifelse(inter.poly.1$e_id == 1 & inter.poly.1$geometry == my.circle$geometry,  "no intersections",
                                       ifelse(inter.poly.1$e_id == 2 & inter.poly.1$geometry == my.circle$geometry, "fully covering circle", 
                                              "partly intersecting")))
  inter.poly.1 <- if(isTRUE(inter.poly.1) && inter.poly.1$geometry == my.circle$geometry){inter.poly.1 <- data.frame()}else{inter.poly.1}
  
  
  # calcualte remaining circle
  # create poly woth remaining area: https://gis.stackexchange.com/questions/353633/r-spatial-erase-one-polygon-from-another-correct-use-of-st-difference
  remaining.circle.1 <- if(nrow(inter.poly.1)==0){my.circle}else{sf::st_difference(my.circle, inter.poly.1)}
  # if the circle is entirely covered by the polygone, we have to correct its area back to the whole circle
  # remaining.circle.1 <- if(nrow(remaining.circle.1)==0){my.circle}else{remaining.circle.1}
  
  
  #  print(plot(remaining.circle.1$geometry, main = my.plot.id)) 
  #       plot(inter.poly.1$geometry, col = "red", add = T))
  
  # calculate intersecting area of second polygone by withdrawing it from remaining circle
  inter.poly.2 <- st_intersection(remaining.circle.1, my.poly.2)
  inter.status.poly.2 <- ifelse(nrow(inter.poly.2) == 0, "no intersections",
                                ifelse(inter.poly.2$e_id == 1 & inter.poly.2$geometry == remaining.circle.1$geometry,  "no intersections",
                                       ifelse(inter.poly.2$e_id == 2 & inter.poly.2$geometry == remaining.circle.1$geometry, "fully covering circle", 
                                              "partly intersecting")))
  
  
  # if the second ednge covers all of the circle remaining its going to be set to 0 so we know there are no direct intersections
  inter.poly.2 <- if(isTRUE(inter.poly.2) && inter.poly.2$geometry == remaining.circle.1$geometry){inter.poly.2 <- data.frame()}else{inter.poly.2}
  
  
  # calculate the area remaining if both intersects are decucted
  # so the area of the frst remining circle minus the area of the second remaining circle 
  remaining.circle.1.and.2.poly <- if(nrow(inter.poly.2)==0){remaining.circle.1}else{sf::st_difference(remaining.circle.1, inter.poly.2)}
  
  # print(plot(remaining.circle.1.and.2.poly$geometry, main = my.plot.id)) 
  
  # calculate the area of the intersection 1
  inter.1.area <- ifelse(nrow(inter.poly.1) == 0, 0, sf::st_area(inter.poly.1))
  #calculate the area of the intersection polygone 2
  inter.2.area <- ifelse(nrow(inter.poly.2) == 0, 0, sf::st_area(inter.poly.2))
  # calculate the area of the remaining circle, after both intersections are deducted
  remaining.circle.area <- sf::st_area(remaining.circle.1.and.2.poly)
  # save area in dataframe
  inter.area.df <- as.data.frame(
    cbind(
      "id" = c(my.plot.id, my.plot.id, my.plot.id), 
      "e_id" = c(my.poly.1$e_id, my.poly.2$e_id, 0), 
      "e_form" = c(my.poly.1$e_form, my.poly.2$e_form, 0),
      "shape" = c("edge", "edge", "circle"),
      "inter_stat" = c(inter.status.poly.1, inter.status.poly.2, 0),
      "area_m2" = c(inter.1.area, inter.2.area, remaining.circle.area)
    ))
  # save dataframe per plot in list
  edges.list.two.edges[[i]] <- inter.area.df
  
  
  # save intersection polygones in list
  # poly.1
  inter.poly.1.list[[i]] <- if(nrow(inter.poly.1)!= 0){c("e_id" = my.poly.1$e_id, "id" = my.poly.1$id, "e_form" = my.poly.1$e_form, inter.poly.1)
  }else{c("e_id" = my.poly.1$e_id, "id" = my.poly.1$id, "e_form" = my.poly.1$e_form, my.poly.1)}
  # poly.2
  inter.poly.2.list[[i]] <- if(nrow(inter.poly.2)!= 0){c("e_id" = my.poly.2$e_id, "id" = my.poly.2$id, "e_form" = my.poly.2$e_form, inter.poly.2)
  }else{c("e_id" = my.poly.2$e_id, "id" = my.poly.2$id, "e_form" = my.poly.2$e_form, my.poly.2)}
  
  # save the reimaingf circle polygones in a list
  # create list wit polygones of the remaining cirlce when it´s only one polygone
  rem.circle.poly.2.edges.list[[i]] <- if(st_geometry_type(remaining.circle.1.and.2.poly)== "POLYGON"){c("e_id" = 0, remaining.circle.1.and.2.poly)}else{}
  # create list wit polygones of the remaining cirlce when it´s a multipoligone
  rem.circle.multipoly.2.edges.list[[i]] <- if(st_geometry_type(remaining.circle.1.and.2.poly)== "MULTIPOLYGON"){c("e_id" = 0, remaining.circle.1.and.2.poly)}else{}
  
}

# save areas into dataframe
edges.list.two.edges.final <- rbindlist(edges.list.two.edges)
edges.area.two.edges.df <- as.data.frame(edges.list.two.edges.final)

# save intersection polygones into dataframe 
# list of polygones 1 of forest edges 
inter.poly.1.list.final <- rbindlist(inter.poly.1.list, fill=TRUE)
inter.poly.1.two.edges.df <- as.data.frame(inter.poly.1.list.final)[,c(2, 1, 3, 8)]
# list of polygones 2 of forest edges 
inter.poly.2.list.final <- rbindlist(inter.poly.2.list, fill=TRUE)
inter.poly.2.two.edges.df <- as.data.frame(inter.poly.2.list.final)[,c(2, 1, 3, 7)]
# bind the both edges per plot together
inter.poly.two.edges.df <- rbind(inter.poly.1.two.edges.df, inter.poly.2.two.edges.df) %>% arrange(id, e_id)

# list of polygones of remainign circles 
rem.circle.poly.two.edges.list.final <- rbindlist(rem.circle.poly.2.edges.list, fill = TRUE)
rem.circle.poly.two.edges.df <- as.data.frame(rem.circle.poly.two.edges.list.final)[,c(2,1,7)]  %>% distinct()
# list of multipolygones of remaining circles
rem.circle.multipoly.two.edges.list.final <- rbindlist(rem.circle.multipoly.2.edges.list)
rem.circle.multipoly.two.edges.df <- as.data.frame(rem.circle.multipoly.two.edges.list.final)[,c(2,1, rem.circle.multipoly.two.edges.df$geometry)] %>% distinct()
# binding the both circle lists back together 
rem.circle.two.edges.df <- rbind(rem.circle.poly.two.edges.df, rem.circle.multipoly.two.edges.df)








# 3.2.1.4. visualising loops results -----------------------------
# for 1 plot
# https://ggplot2.tidyverse.org/reference/ggsf.html
p_id =   50059 
ggplot() +
  geom_sf(data = triangle.e1.poly.df$geometry[triangle.e1.poly.df$id ==p_id], aes(alpha = 0))+
  geom_sf(data = triangle.e2.poly.df$geometry[triangle.e2.poly.df$id == p_id], aes(alpha = 0))+
  geom_sf(data = circle.poly.df$geometry[circle.poly.df$id == p_id], aes(alpha = 0))+
  geom_sf(data = tree.points.one.edge.df$geometry[tree.status.one.edge.df$id == p_id], 
          aes(color = tree.points.one.edge.df$t_stat[tree.status.one.edge.df$id == p_id]))

# for all plots
for(i in 1:length(unique(circle.poly.df$id))){
  # https://ggplot2.tidyverse.org/reference/ggsf.html
  
  #i = 62
  my.plot.id = unique(circle.poly.df)[i, "id"]
  
  print(ggplot() +
          ggtitle(paste0(my.plot.id, " - ", triangle.e1.poly.df$e_form[triangle.e1.poly.df$id == my.plot.id], " - " , triangle.e2.poly.df$e_form[triangle.e2.poly.df$id == my.plot.id]))+ 
          geom_sf(data = triangle.e1.poly.df$geometry[triangle.e1.poly.df$id == my.plot.id], aes(alpha = 0))+
          geom_sf(data = triangle.e2.poly.df$geometry[triangle.e2.poly.df$id == my.plot.id], aes(alpha = 0))+
          geom_sf(data = circle.poly.df$geometry[circle.poly.df$id == my.plot.id], aes(alpha = 0))+
          geom_sf(data = all.trees.points$geometry[all.trees.points$id == my.plot.id], aes(color = all.trees.points$t_stat[all.trees.points$id == my.plot.id])))
  
}









# N. loop for tirangles wich intersect only on one side -------------------
# this was meant to select those x and y coordiantes of intersectiion between  a line thourghthe center of the AT or BT line and the center of the cirlce and the 17.84 cirlce, 
# that are located inside the triangle made from the inter.for.triangle function reaching out to the 60m radius circle

X_inter_MC_shorter_side = ifelse(my.e.form  == 1, p.site.line(x1, x2, y1, y2, c.x0, c.y0, c.r.inter, intercept(x.A , y.A , x.B , y.B ), slope(x.A , y.A , x.B , y.B ), 0, 0, output = "x_short"),
                                 ifelse(my.e.form == 2 & forest_edges_HBI.man.sub.1.edge$inter_status_AT_17[i] == "two I" & forest_edges_HBI.man.sub.1.edge$inter_status_BT_17[i] != "two I",
                                        p.site.triangle(intersection_line_circle(intercept(x.A , y.A , x.T , y.T ), slope(x.A , y.A , x.T , y.T ), c.x0, c.y0, 17.84, coordinate = "x1"), 
                                                        intersection_line_circle(intercept(x.A , y.A , x.T , y.T ), slope(x.A , y.A , x.T , y.T ), c.x0, c.y0, 17.84, coordinate = "x2"),
                                                        intersection_line_circle(intercept(x.A , y.A , x.T , y.T ), slope(x.A , y.A , x.T , y.T ), c.x0, c.y0, 17.84, coordinate = "y1"), 
                                                        intersection_line_circle(intercept(x.A , y.A , x.T , y.T ), slope(x.A , y.A , x.T , y.T ), c.x0, c.y0, 17.84, coordinate = "y2"), 
                                                        c.x0, c.y0, c.r.inter,
                                                        intercept(x.T , y.T , x.A , y.A), 
                                                        slope(x.T , y.T , x.A , y.A), 
                                                        inter.for.triangle(intercept(x.T , y.T , x.A , y.A ), slope(x.T , y.T , x.A , y.A ), c.x0, c.y0, c.r.inter, x.A , y.A , x.T , y.T , coordinate = "x"), 
                                                        inter.for.triangle(intercept(x.T , y.T , x.B , y.B ), slope(x.T , y.T , x.B , y.B ), c.x0, c.y0, c.r.inter, x.B , y.B , x.T , y.T , coordinate = "x"), 
                                                        x.T, 
                                                        inter.for.triangle(intercept(x.T , y.T , x.A , y.A ), slope(x.T , y.T , x.A , y.A ), c.x0, c.y0, c.r.inter, x.A , y.A , x.T , y.T , coordinate = "y"), 
                                                        inter.for.triangle(intercept(x.T , y.T , x.B , y.B ), slope(x.T , y.T , x.B , y.B ), c.x0, c.y0, c.r.inter, x.B , y.B , x.T , y.T , coordinate = "y"),
                                                        y.T, 
                                                        0, 0, 
                                                        output = "x_inside_triangle"), 
                                        ifelse(my.e.form == 2 & forest_edges_HBI.man.sub.1.edge$inter_status_BT_17[i] == "two I" & forest_edges_HBI.man.sub.1.edge$inter_status_AT_17[i] != "two I",
                                               p.site.triangle(intersection_line_circle(intercept(x.B , y.B , x.T , y.T ), slope(x.B , y.B , x.T , y.T ), c.x0, c.y0, 17.84, coordinate = "x1"), 
                                                               intersection_line_circle(intercept(x.B , y.B , x.T , y.T ), slope(x.B , y.B , x.T , y.T ), c.x0, c.y0, 17.84, coordinate = "x2"),
                                                               intersection_line_circle(intercept(x.B , y.B , x.T , y.T ), slope(x.B , y.B , x.T , y.T ), c.x0, c.y0, 17.84, coordinate = "y1"), 
                                                               intersection_line_circle(intercept(x.B, y.B , x.T , y.T ), slope(x.B , y.B , x.T , y.T ), c.x0, c.y0, 17.84, coordinate = "y2"),  
                                                               c.x0, c.y0, c.r.inter, 
                                                               intercept(x.T , y.T , x.B , y.B), 
                                                               slope(x.T , y.T , x.B , y.B), 
                                                               inter.for.triangle(intercept(x.T , y.T , x.A , y.A ), slope(x.T , y.T , x.A , y.A ), c.x0, c.y0, c.r.inter, x.A , y.A , x.T , y.T , coordinate = "x"), 
                                                               inter.for.triangle(intercept(x.T , y.T , x.B , y.B ), slope(x.T , y.T , x.B , y.B ), c.x0, c.y0, c.r.inter, x.B , y.B , x.T , y.T , coordinate = "x"), 
                                                               x.T, 
                                                               inter.for.triangle(intercept(x.T , y.T , x.A , y.A ), slope(x.T , y.T , x.A , y.A ), c.x0, c.y0, c.r.inter, x.A , y.A , x.T , y.T , coordinate = "y"), 
                                                               inter.for.triangle(intercept(x.T , y.T , x.B , y.B ), slope(x.T , y.T , x.B , y.B ), c.x0, c.y0, c.r.inter, x.B , y.B , x.T , y.T , coordinate = "y"),
                                                               y.T, 
                                                               0, 0, 
                                                               output = "x_inside_triangle"),NA)))
Y_inter_MC_shorter_side = ifelse(my.e.form  == 1, p.site.line(x1, x2, y1, y2, c.x0, c.y0, c.r.inter, intercept(x.A , y.A , x.B , y.B ), slope(x.A , y.A , x.B , y.B ), 0, 0, output = "y_short"),
                                 ifelse(my.e.form == 2 & forest_edges_HBI.man.sub.1.edge$inter_status_AT_17[i] == "two I" & forest_edges_HBI.man.sub.1.edge$inter_status_BT_17[i] != "two I",
                                        p.site.triangle(intersection_line_circle(intercept(x.A , y.A , x.T , y.T ), slope(x.A , y.A , x.T , y.T ), c.x0, c.y0, 17.84, coordinate = "x1"), 
                                                        intersection_line_circle(intercept(x.A , y.A , x.T , y.T ), slope(x.A , y.A , x.T , y.T ), c.x0, c.y0, 17.84, coordinate = "x2"),
                                                        intersection_line_circle(intercept(x.A , y.A , x.T , y.T ), slope(x.A , y.A , x.T , y.T ), c.x0, c.y0, 17.84, coordinate = "y1"), 
                                                        intersection_line_circle(intercept(x.A , y.A , x.T , y.T ), slope(x.A , y.A , x.T , y.T ), c.x0, c.y0, 17.84, coordinate = "y2"),
                                                        c.x0, c.y0, c.r.inter,
                                                        intercept(x.T , y.T , x.A , y.A), 
                                                        slope(x.T , y.T , x.A , y.A), 
                                                        inter.for.triangle(intercept(x.T , y.T , x.A , y.A ), slope(x.T , y.T , x.A , y.A ), c.x0, c.y0, c.r.inter, x.A , y.A , x.T , y.T , coordinate = "x"), 
                                                        inter.for.triangle(intercept(x.T , y.T , x.B , y.B ), slope(x.T , y.T , x.B , y.B ), c.x0, c.y0, c.r.inter, x.B , y.B , x.T , y.T , coordinate = "x"), 
                                                        x.T, 
                                                        inter.for.triangle(intercept(x.T , y.T , x.A , y.A ), slope(x.T , y.T , x.A , y.A ), c.x0, c.y0, c.r.inter, x.A , y.A , x.T , y.T , coordinate = "y"), 
                                                        inter.for.triangle(intercept(x.T , y.T , x.B , y.B ), slope(x.T , y.T , x.B , y.B ), c.x0, c.y0, c.r.inter, x.B , y.B , x.T , y.T , coordinate = "y"),
                                                        y.T, 
                                                        0, 0, 
                                                        output = "y_inside_triangle"), 
                                        ifelse(my.e.form == 2 & forest_edges_HBI.man.sub.1.edge$inter_status_BT_17[i] == "two I" & forest_edges_HBI.man.sub.1.edge$inter_status_AT_17[i] != "two I",
                                               p.site.triangle(intersection_line_circle(intercept(x.B , y.B , x.T , y.T ), slope(x.B , y.B , x.T , y.T ), c.x0, c.y0, 17.84, coordinate = "x1"), 
                                                               intersection_line_circle(intercept(x.B , y.B , x.T , y.T ), slope(x.B , y.B , x.T , y.T ), c.x0, c.y0, 17.84, coordinate = "x2"),
                                                               intersection_line_circle(intercept(x.B , y.B , x.T , y.T ), slope(x.B , y.B , x.T , y.T ), c.x0, c.y0, 17.84, coordinate = "y1"), 
                                                               intersection_line_circle(intercept(x.B, y.B , x.T , y.T ), slope(x.B , y.B , x.T , y.T ), c.x0, c.y0, 17.84, coordinate = "y2"),  
                                                               c.x0, c.y0, c.r.inter, 
                                                               intercept(x.T , y.T , x.B , y.B), 
                                                               slope(x.T , y.T , x.B , y.B), 
                                                               inter.for.triangle(intercept(x.T , y.T , x.A , y.A ), slope(x.T , y.T , x.A , y.A ), c.x0, c.y0, c.r.inter, x.A , y.A , x.T , y.T , coordinate = "x"), 
                                                               inter.for.triangle(intercept(x.T , y.T , x.B , y.B ), slope(x.T , y.T , x.B , y.B ), c.x0, c.y0, c.r.inter, x.B , y.B , x.T , y.T , coordinate = "x"), 
                                                               x.T, 
                                                               inter.for.triangle(intercept(x.T , y.T , x.A , y.A ), slope(x.T , y.T , x.A , y.A ), c.x0, c.y0, c.r.inter, x.A , y.A , x.T , y.T , coordinate = "y"), 
                                                               inter.for.triangle(intercept(x.T , y.T , x.B , y.B ), slope(x.T , y.T , x.B , y.B ), c.x0, c.y0, c.r.inter, x.B , y.B , x.T , y.T , coordinate = "y"),
                                                               y.T, 
                                                               0, 0, 
                                                               output = "y_inside_triangle"),NA)))










# plot all edges together for plot_ID 
my.p.id = 50059

#AB line
ggplot() +  
  geom_circle(data = data_circle, aes(x0 = x0, y0 = y0, r = r0))+ # Draw ggplot2 plot with circle representing sampling circuits 
  geom_circle(data = data_circle, aes(x0 = x0, y0 = y0, r = rmax*2))+ # Draw ggplot2 plot with circle representing sampling circuits
  ### AB line
  geom_point(data = forest_edges_HBI.man %>%
               filter(e_form == "1" & plot_ID == my.p.id) %>% 
               select(plot_ID, X1_inter_AB_17, X2_inter_AB_17, X_A, X_B, Y1_inter_AB_17, Y2_inter_AB_17, Y_A, Y_B) %>% 
               to_long(keys = c("X_name",  "Y_name"),
                       values = c( "X_value", "Y_value"),  
                       names(.)[2:5], names(.)[6:9]), 
             aes(x= X_value, y = Y_value, colour = X_name))+
  geom_line(data = forest_edges_HBI.man %>% 
              filter(e_form == "1" & plot_ID == my.p.id) %>% 
              select(plot_ID, X_A, X_B, Y_A, Y_B) %>% 
              to_long(keys = c("X_name",  "Y_name"),
                      values = c( "X_value", "Y_value"),  
                      names(.)[2:3], names(.)[4:5]), 
            aes(x= X_value, y = Y_value))+
  geom_line(data = forest_edges_HBI.man %>% 
              filter(e_form == "1" & plot_ID == my.p.id) %>% 
              select(plot_ID, X1_inter_AB_17, X_A, Y1_inter_AB_17, Y_A) %>% 
              to_long(keys = c("X_name",  "Y_name"),
                      values = c( "X_value", "Y_value"),
                      names(.)[2:3], names(.)[4:5]),  
            aes(x= X_value, y = Y_value, colour = X_name))+
  geom_line(data = forest_edges_HBI.man %>% 
              filter(e_form == "1" & plot_ID == my.p.id) %>% 
              select(plot_ID, X2_inter_AB_17, X_A, Y2_inter_AB_17, Y_A) %>% 
              to_long(keys = c("X_name",  "Y_name"),
                      values = c( "X_value", "Y_value"),
                      names(.)[2:3], names(.)[4:5]),  
            aes(x= X_value, y = Y_value, colour = X_name))+
  geom_line(data = forest_edges_HBI.man %>% 
              filter(e_form == "1" & plot_ID == my.p.id) %>% 
              select(plot_ID, X1_inter_AB_17, X_B, Y1_inter_AB_17, Y_B) %>% 
              to_long(keys = c("X_name",  "Y_name"),
                      values = c( "X_value", "Y_value"),
                      names(.)[2:3], names(.)[4:5]),  
            aes(x= X_value, y = Y_value, colour = X_name))+
  geom_line(data = forest_edges_HBI.man %>% 
              filter(e_form == "1" & plot_ID == my.p.id) %>% 
              select(plot_ID, X2_inter_AB_17, X_B, Y2_inter_AB_17, Y_B) %>% 
              to_long(keys = c("X_name",  "Y_name"),
                      values = c( "X_value", "Y_value"),
                      names(.)[2:3], names(.)[4:5]),  
            aes(x= X_value, y = Y_value, colour = X_name))+
  # trees
  geom_point(data =  trees_and_edges %>% 
               filter(plot_ID == my.p.id),
             aes(X_tree, Y_tree, colour = t_status_AB_ABT))
#theme_bw()+
#facet_wrap(~plot_ID)

# forest edge type 2 
# if the # i removed, this part allows to plot plots with forest edges with a turning point
### AT line
# ggplot() +  
# geom_circle(data = data_circle, aes(x0 = x0, y0 = y0, r = r0))+ # Draw ggplot2 plot with circle representing sampling circuits 
# geom_circle(data = data_circle, aes(x0 = x0, y0 = y0, r = rmax*2))+ # Draw ggplot2 plot with circle representing sampling circuits
geom_point(data = forest_edges_HBI.man %>% 
             filter(e_form == "2" & plot_ID == my.p.id) %>% 
             select(plot_ID, X_A, X_T, Y_A, Y_T) %>% 
             to_long(keys = c("X_name",  "Y_name"),
                     values = c( "X_value", "Y_value"),  
                     names(.)[2:3], names(.)[4:5]), 
           aes(x= X_value, y = Y_value, colour = "T"))+
  geom_line(data = forest_edges_HBI.man %>% 
              filter(e_form == "2" & plot_ID == my.p.id) %>% 
              select(plot_ID, X_A, X_T, Y_A, Y_T) %>% 
              to_long(keys = c("X_name",  "Y_name"),
                      values = c( "X_value", "Y_value"),  
                      names(.)[2:3], names(.)[4:5]), 
            aes(x= X_value, y = Y_value))+
  # intersections choosen to draw the triangle AT
  geom_point(data = forest_edges_HBI.man %>% 
               filter(e_form == "2" & plot_ID == my.p.id) %>% 
               select(plot_ID, X_inter_AT_triangle_60, X_A, Y_inter_AT_triangle_60, Y_A) %>% 
               to_long(keys = c("X_name",  "Y_name"),
                       values = c( "X_value", "Y_value"),  
                       names(.)[2:3], names(.)[4:5]), 
             aes(x= X_value, y = Y_value, colour = "A_Intercept"))+
  geom_line(data = forest_edges_HBI.man %>% 
              filter(e_form == "2" & plot_ID == my.p.id) %>% 
              select(plot_ID, X_inter_AT_triangle_60, X_A, Y_inter_AT_triangle_60, Y_A) %>% 
              to_long(keys = c("X_name",  "Y_name"),
                      values = c( "X_value", "Y_value"),  
                      names(.)[2:3], names(.)[4:5]), 
            aes(x= X_value, y = Y_value))+
  # BT line 
  geom_point(data = forest_edges_HBI.man %>%
               filter(e_form == "2" & plot_ID == my.p.id) %>% 
               select(plot_ID, X_B, X_T, Y_B, Y_T) %>% 
               to_long(keys = c("X_name",  "Y_name"),
                       values = c( "X_value", "Y_value"),  
                       names(.)[2:3], names(.)[4:5]), 
             aes(x= X_value, y = Y_value, colour = "T"))+
  geom_line(data = forest_edges_HBI.man %>%
              filter(e_form == "2" & plot_ID == my.p.id) %>% 
              select(plot_ID, X_B, X_T, Y_B, Y_T) %>% 
              to_long(keys = c("X_name",  "Y_name"),
                      values = c( "X_value", "Y_value"),  
                      names(.)[2:3], names(.)[4:5]), 
            aes(x= X_value, y = Y_value))+
  # intersections choosen to draw the triangle BT
  geom_point(data = forest_edges_HBI.man %>% 
               filter(e_form == "2" & plot_ID == my.p.id) %>% 
               select(plot_ID, X_inter_BT_triangle_60, X_B, Y_inter_BT_triangle_60, Y_B) %>% 
               to_long(keys = c("X_name",  "Y_name"),
                       values = c( "X_value", "Y_value"),  
                       names(.)[2:3], names(.)[4:5]), 
             aes(x= X_value, y = Y_value, colour = "B_intercept"))+
  geom_line(data = forest_edges_HBI.man %>% 
              filter(e_form == "2" & plot_ID == my.p.id) %>% 
              select(plot_ID, X_inter_BT_triangle_60, X_B, Y_inter_BT_triangle_60, Y_B) %>% 
              to_long(keys = c("X_name",  "Y_name"),
                      values = c( "X_value", "Y_value"),  
                      names(.)[2:3], names(.)[4:5]), 
            aes(x= X_value, y = Y_value))+
  # trees
  geom_point(data =  trees_and_edges %>%  filter(plot_ID == my.p.id), 
             aes(X_tree, Y_tree, colour = t_status_AB_ABT))+
  theme_bw()+ 
  facet_wrap(~plot_ID) 


# case wise loops for forest edges areas and coordinates ------------------


# 3.2.1. georefferencing trough separate loops  --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# 3.2.1.1. creating list of polygones for circles (17.84m) per plot  -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# dataset with only edge forms 1 and 2 
forest_edges_HBI.man.sub <- forest_edges_HBI.man %>% 
  filter(e_form %in% c(1, 2)) %>% 
  semi_join(HBI_loc %>% filter(!is.na( RW_MED) & !is.na(HW_MED)) %>%  select(plot_ID)  %>% distinct(), by = "plot_ID") 

## loop to create list with polygones for circles per plot center 
# create empty list to store circle polygones in 
circle.list <- vector("list", length = length(forest_edges_HBI.man.sub$plot_ID))

for(i in 1:length(forest_edges_HBI.man.sub$plot_ID)) {
  # i = 1
  # georefferencing data: 
  
  # select plot ID accordint to positioin in the list
  my.plot.id <- forest_edges_HBI.man.sub[i, "plot_ID"] 
  my.e.form <- forest_edges_HBI.man.sub[i, "e_form"]
  
  # assign crs
  my.utm.epsg <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"
  
  # select UTM corrdinates of the plot center
  my.center.easting <- HBI_loc[HBI_loc$plot_ID == my.plot.id, "RW_MED"]
  my.center.northing <- HBI_loc[HBI_loc$plot_ID == my.plot.id, "HW_MED"]
  center.df <- as.data.frame(cbind("id" = my.plot.id, 
                                   "lat" = my.center.northing, 
                                   "lon" = my.center.easting))
  
  # create sf point with center coordiantes
  center.point <- sf::st_as_sf(center.df, coords = c("lon", "lat"), crs = my.utm.epsg)
  
  # build polygon (circlular buffer) around center point
  circle.17 <- sf::st_buffer(center.point, 17.84)
  # circle.12 <- sf::st_buffer(center.point, 12.62)
  # circle.5 <- sf::st_buffer(center.point, 5.64)
  
  
  # saving circle polygones in a list
  # circle.list[[i]] <- rbind(circle.17, circle.12, circle.5)
  circle.list[[i]] <- circle.17
  
}
# circle.list
circle.list.final <- rbindlist(circle.list)
circle.poly.df <- as.data.frame(circle.list.final)





# 3.2.1.2. creating list of squared polygones for eddge form 1  -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## loop to create list of polygones for edge form 1
#forest_edges_HBI.man.sub.e1 <- forest_edges_HBI.man%>% filter(e_form == 1)#%>% filter(inter_status_AB_17 == "two I") # 63 of edge form 1 -> with intersection 43

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

triangle.e1.coords.list.nogeo <- rbindlist(triangle.e1.coords)
triangle.e1.coords.df.nogeo <- as.data.frame(triangle.e1.coords.list) %>% 
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
  dist.A <- forest_edges_HBI.man.sub.e2[i, "A_dist"] 
  azi.A <- forest_edges_HBI.man.sub.e2[i, "A_azi"] 
  x.A <- dist.A*sin(azi.A)   # longitude, easting, RW, X
  y.A <- dist.A*cos(azi.A)   # latitude, northing, HW, y 
  
  # point B
  dist.B <- forest_edges_HBI.man.sub.e2[i, "B_dist"] 
  azi.B <- forest_edges_HBI.man.sub.e2[i, "B_azi"] 
  x.B <- dist.B*sin(azi.B)   # longitude, easting, RW, X
  y.B <- dist.B*cos(azi.B)   # latitude, northing, HW, y 
  
  # point T
  dist.T <- forest_edges_HBI.man.sub.e2[i, "T_dist"] 
  azi.T <- forest_edges_HBI.man.sub.e2[i, "T_azi"] 
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
forest_edges_HBI.man.sub.1.edge <- forest_edges_HBI.man %>% # rows:84
  # select only plots with a known edge form and for edge 2 only those that actually intersect the 17m circle
  filter(e_form == 1 | e_form == 2 & inter_status_AT_17 == "two I" | e_form == 2 & inter_status_BT_17 == "two I") %>%  # rows:81
  # remove plots that have two edges
  anti_join(forest_edges_HBI.man %>%  filter(e_form == 1 | e_form == 2 & inter_status_AT_17 == "two I" | e_form == 2 & inter_status_BT_17 == "two I") %>% 
              group_by(plot_ID) %>% summarise(n = n()) %>% filter(n > 1) %>% select(plot_ID), by = "plot_ID")# %>% # 14 plots with 2 edges --> 28 rows -> 53 left
  # remove plots that do now have a corresponding center coordiante in the HBI loc document
  #semi_join(HBI_loc %>% filter(!is.na( RW_MED) & !is.na(HW_MED)) %>%  select(plot_ID)  %>% distinct(), by = "plot_ID") # nrow = 52 --> there is 1 plots without corresponding 

edges.list.nogeo <- vector("list", length = length(unique(forest_edges_HBI.man.sub.1.edge$plot_ID)))
inter.poly.list.nogeo <- vector("list", length = length(unique(forest_edges_HBI.man.sub.1.edge$plot_ID)))
#inter.poly.NA.list <- vector("list", length = length(unique(forest_edges_HBI.man.sub.1.edge$plot_ID)))
remaining.circle.poly.list.nogeo <- vector("list", length = length(unique(forest_edges_HBI.man.sub.1.edge$plot_ID)))
remaining.circle.multipoly.list.nogeo <- vector("list", length = length(unique(forest_edges_HBI.man.sub.1.edge$plot_ID)))

# loop for 17m radius circle
for (i in 1:length(unique(forest_edges_HBI.man.sub.1.edge$plot_ID))){ 
  # i = 48
  
  # select plot ID of the respective circle 
  my.plot.id <- forest_edges_HBI.man.sub.1.edge[i, "plot_ID"]
  my.e.form <- edge.poly.df$e_form[edge.poly.df$id == my.plot.id]
  my.e.id <- edge.poly.df$e_id[edge.poly.df$id == my.plot.id]
  
  # select the circle polygone corresponding with the plot ID
  # my.circle.17 <- sf::st_as_sf(circle.poly.df %>% filter(id == my.plot.id) %>% distinct())
  
  # circle data
  center.df<- as.data.frame(cbind(c.x0, c.y0))
  c.x0 = 0 
  c.y0 = 0
  c.r3 = 17.84
  c.r2 = 12.62
  c.r1 = 5.64
  
  # build polygon (circlular buffer) around center point
  circle.pt <- sf::st_as_sf(center.df, coords = c("c.x0", "c.y0"))
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
  # list with inter and remaining circle areas areas
  edges.list.nogeo[[i]] <- inter.area.df
  
  # create lists with polgons of intersections if there are intersections, if there is non, save the polygone instead. 
  inter.poly.list.nogeo[[i]] <- if(isTRUE(nrow(inter.poly.17)!= 0)){c(inter.poly.17)}else{c(my.poly)}
  
  remaining.circle.poly.17$e_ID <- 0
  remaining.circle.poly.17$e_form <- 0
  
  # create list wit polygones of the remaining cirlce when it´s only one polygone
  remaining.circle.poly.list.nogeo[[i]] <- if(st_geometry_type(remaining.circle.poly.17)== "POLYGON"){c(remaining.circle.poly.17)}else{}
  # create list wit polygones of the remaining cirlce when it´s a multipoligone
  remaining.circle.multipoly.list.nogeo[[i]] <- if(st_geometry_type(remaining.circle.poly.17)== "MULTIPOLYGON"){c(remaining.circle.poly.17)}else{}
  
  
}


# list of areas
edges.area.list.final.nogeo <- rbindlist(edges.list.nogeo)
edges.area.df.nogeo <- as.data.frame(edges.area.list.final.nogeo)

# Fehler in rbindlist(inter.poly.list, fill = TRUE) : 
#   Class attribute on column 5 of item 2 does not match with column 4 of item 1.

# list of polygones of forest edges 
inter.poly.list.final.nogeo <- rbindlist(inter.poly.list.nogeo, fill=TRUE)
inter.poly.one.edge.df <- as.data.frame(inter.poly.list.final.nogeo)#[,c(2, 1, 3, 5)]%>% arrange(id, e_id)

# list of polygones of remainign circles 
rem.circle.poly.list.final <- rbindlist(remaining.circle.poly.list, fill = TRUE)
rem.circle.poly.df <- as.data.frame(rem.circle.poly.list.final)#[,c(2,1,4)]  %>% distinct()
# list of multipolygones of remaining circles
rem.circle.multipoly.list.final <- rbindlist(remaining.circle.multipoly.list)
rem.circle.multipoly.df <- as.data.frame(rem.circle.multipoly.list.final)#[,c(2,1,4)] %>% distinct()
# binding the both circle lists back together 
rem.circle.one.edge.df <- rbind(rem.circle.poly.df, rem.circle.multipoly.df)

area.diff.geo.nogeo <- edges.area.df%>% 
  rename(plot_ID = id) %>% 
  rename(e_ID = e_id)%>% left_join(., edges.area.df.nogeo %>% 
                                     filter(CCS_r_m == 17.84) %>% 
                                                     rename(area_m2_nogeo = area_m2), 
                                                   by = c("plot_ID", "e_ID")) %>% 
  mutate(diff_m2 = as.numeric(area_m2) - as.numeric(area_m2_nogeo))
summary(area.diff.geo.nogeo)



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
edges.list.two.edges <- vector("list", length = length(unique(forest_edges_HBI.man.sub.2.edges$plot_ID)))
# list to save the first intersection polygone per plot in
inter.poly.1.list <- vector("list", length = length(unique(forest_edges_HBI.man.sub.2.edges$plot_ID)))
# list to save the second intersection polygone per plot in
inter.poly.2.list <- vector("list", length = length(unique(forest_edges_HBI.man.sub.2.edges$plot_ID)))
# list to save the remaining circle polygones per plot in
rem.circle.poly.2.edges.list <- vector("list", length = length(unique(forest_edges_HBI.man.sub.2.edges$plot_ID)))
# list to save the remaining circle MULTIpolygones per plot in
rem.circle.multipoly.2.edges.list <- vector("list", length = length(unique(forest_edges_HBI.man.sub.2.edges$plot_ID)))

for (i in 1:length(unique(forest_edges_HBI.man.sub.2.edges$plot_ID))){ 
  #i = 14
  # i = which(grepl(50080, unique(forest_edges_HBI.man.sub.2.edges$plot_ID)))
  
  # select plot ID of the respective circle 
  my.plot.id <- unique(forest_edges_HBI.man.sub.2.edges$plot_ID)[i]
  
  # select the circle polygone corresponding with the plot ID
  my.circle <- sf::st_as_sf(circle.poly.df %>% filter(id == my.plot.id) %>% distinct())
  #plot(my.circle)
  
  ## select the  polygones the circle is intersected by
  # select the polygones with the same plot ID as the cirlce
  my.plot.polys.df <- edge.poly.df.nogeo %>% filter(id == my.plot.id) %>% arrange(e_ID)
  # create the polygones of the edge geometries
  my.poly.1 <- sf::st_as_sf(my.plot.polys.df[1,])
  my.poly.2 <- sf::st_as_sf(my.plot.polys.df[2,])
  
  # print edges and circle
  # print(plot(my.poly.1$geometry), 
  #       plot(my.poly.2$geometry, add = T), 
  #       plot(my.circle$geometry, add = T)
  #       )
  
  
  ## create poolygon of intersection for first polygon with circle
  inter.poly.1  <- st_intersection(my.circle, my.poly.1)
  inter.status.poly.1 <- ifelse(nrow(inter.poly.1) == 0, "no intersections",
                                ifelse(inter.poly.1$e_id == 1 & inter.poly.1$geometry == my.circle$geometry,  "no intersections",
                                       ifelse(inter.poly.1$e_id == 2 & inter.poly.1$geometry == my.circle$geometry, "fully covering circle", 
                                              "partly intersecting")))
  # if the first ednge covers all of the circle remaining its going to be set to 0 so we know there are no direct intersections and the circle is passed on to the next edge to calcualte the intersection
  # https://www.statology.org/r-argument-is-of-length-zero/
  inter.poly.1 <- if(isTRUE(inter.poly.1) && inter.poly.1$geometry == my.circle$geometry){inter.poly.1 <- data.frame()}else{inter.poly.1}
  
  
  ## create poolygon of remaining circle after first edge polygone is intersected
  # create poly with remaining area: https://gis.stackexchange.com/questions/353633/r-spatial-erase-one-polygon-from-another-correct-use-of-st-difference
  remaining.circle.1 <- if(nrow(inter.poly.1)==0){my.circle}else{sf::st_difference(my.circle, inter.poly.1)}
  print(plot(remaining.circle.1$geometry, main = paste0(my.plot.id, "-", my.poly.1$e_form))) 
  
  
  ## create polygone of intersecting area of second polygone with remaining circle
  inter.poly.2 <- st_intersection(remaining.circle.1, my.poly.2)
  inter.status.poly.2 <- ifelse(nrow(inter.poly.2) == 0, "no intersections",
                                ifelse(inter.poly.2$e_id == 1 & inter.poly.2$geometry == remaining.circle.1$geometry,  "no intersections",
                                       ifelse(inter.poly.2$e_id == 2 & inter.poly.2$geometry == remaining.circle.1$geometry, "fully covering circle", 
                                              "partly intersecting")))
  # if the second edge covers all of the circle remaining its going to be set to 0 so we know there are no direct intersections
  # https://www.statology.org/r-argument-is-of-length-zero/
  inter.poly.2 <- if(isTRUE(inter.poly.2) && inter.poly.2$geometry == remaining.circle.1$geometry){inter.poly.2 <- data.frame()}else{inter.poly.2}
  
  
  ## create polygone of the  remaining cricle after both intersects are decucted
  # so the area of the frst remining circle minus the area of the second remaining circle 
  remaining.circle.1.and.2.poly <- if(nrow(inter.poly.2)==0){remaining.circle.1}else{sf::st_difference(remaining.circle.1, inter.poly.2)}
  print(plot(remaining.circle.1.and.2.poly$geometry, main = paste0(my.plot.id, "-", my.poly.2$e_form))) 
  
  
  ## calculate the area
  # area of the intersection 1
  inter.1.area <- ifelse(nrow(inter.poly.1) == 0, 0, sf::st_area(inter.poly.1))
  # area of the intersection polygone 2
  inter.2.area <- ifelse(nrow(inter.poly.2) == 0, 0, sf::st_area(inter.poly.2))
  #  area of the remaining circle, after both intersections are deducted
  remaining.circle.area <- sf::st_area(remaining.circle.1.and.2.poly)
  # save area in dataframe
  inter.area.df <- as.data.frame(
    cbind(
      "id" = c(my.plot.id, my.plot.id, my.plot.id), 
      "e_id" = c(my.poly.1$e_id, my.poly.2$e_id, 0), 
      "e_form" = c(my.poly.1$e_form, my.poly.2$e_form, 0),
      "shape" = c("edge", "edge", "circle"),
      "inter_stat" = c(inter.status.poly.1, inter.status.poly.2, 0),
      "area_m2" = c(inter.1.area, inter.2.area, remaining.circle.area)
    ))
  # save dataframe per plot in list
  edges.list.two.edges[[i]] <- inter.area.df
  
  
  ## save intersection polygones in list
  # poly.1
  inter.poly.1.list[[i]] <- if(nrow(inter.poly.1)!= 0){c("e_id" = my.poly.1$e_id, "id" = my.poly.1$id, "e_form" = my.poly.1$e_form, inter.poly.1)
  }else{c("e_id" = my.poly.1$e_id, "id" = my.poly.1$id, "e_form" = my.poly.1$e_form, my.poly.1)}
  # poly.2
  inter.poly.2.list[[i]] <- if(nrow(inter.poly.2)!= 0){c("e_id" = my.poly.2$e_id, "id" = my.poly.2$id, "e_form" = my.poly.2$e_form, inter.poly.2)
  }else{c("e_id" = my.poly.2$e_id, "id" = my.poly.2$id, "e_form" = my.poly.2$e_form, my.poly.2)}
  
  ## save the reimaingf circle polygones in a list
  # create list wit polygones of the remaining cirlce when it´s only one polygone
  rem.circle.poly.2.edges.list[[i]] <- if(st_geometry_type(remaining.circle.1.and.2.poly)== "POLYGON"){c("e_id" = 0, remaining.circle.1.and.2.poly)}else{}
  # create list wit polygones of the remaining cirlce when it´s a multipoligone
  rem.circle.multipoly.2.edges.list[[i]] <- if(st_geometry_type(remaining.circle.1.and.2.poly)== "MULTIPOLYGON"){c("e_id" = 0, remaining.circle.1.and.2.poly)}else{}
  
}

# save areas into dataframe
edges.list.two.edges.final <- rbindlist(edges.list.two.edges)
edges.area.two.edges.df <- as.data.frame(edges.list.two.edges.final)

# save intersection polygones into dataframe 
# list of polygones 1 of forest edges 
inter.poly.1.list.final <- rbindlist(inter.poly.1.list, fill=TRUE)
inter.poly.1.two.edges.df <- as.data.frame(inter.poly.1.list.final)[,c(2, 1, 3, 8)]
# list of polygones 2 of forest edges 
inter.poly.2.list.final <- rbindlist(inter.poly.2.list, fill=TRUE)
inter.poly.2.two.edges.df <- as.data.frame(inter.poly.2.list.final)[,c(2, 1, 3, 7)]
# bind the both edges per plot together
inter.poly.two.edges.df <- rbind(inter.poly.1.two.edges.df, inter.poly.2.two.edges.df) %>% arrange(id, e_id)

# list of polygones of remainign circles 
rem.circle.poly.two.edges.list.final <- rbindlist(rem.circle.poly.2.edges.list, fill = TRUE)
rem.circle.poly.two.edges.df <- as.data.frame(rem.circle.poly.two.edges.list.final)[,c(2,1,7)]  %>% distinct()
# list of multipolygones of remaining circles
rem.circle.multipoly.two.edges.list.final <- rbindlist(rem.circle.multipoly.2.edges.list)
rem.circle.multipoly.two.edges.df <- as.data.frame(rem.circle.multipoly.two.edges.list.final)[,c(2,1,15)] %>% distinct()
# binding the both circle lists back together 
rem.circle.two.edges.df <- if(nrow(rem.circle.poly.two.edges.df) != 0 && nrow(rem.circle.multipoly.two.edges.list.final) != 0){
  rbind(rem.circle.poly.two.edges.df, rem.circle.multipoly.two.edges.df)
}else{rem.circle.poly.two.edges.df}




all.edges.area.df <- rbind(edges.area.df, edges.area.two.edges.df)




# 3.2.1.4. sorting trees into edge and remaining circle polygones ---------

trees.one.edge <- HBI_trees %>%
  # filter only for trees that are located in plots with a forest edge
  semi_join(forest_edges_HBI.man %>% filter(e_form == 1 | e_form == 2) %>%
              #& inter_status_AT_17 == "two I" | e_form == 2 & inter_status_BT_17 == "two I") %>% 
              select(plot_ID) %>% distinct(), by = "plot_ID") %>% 
  # filter for trees located in plots htat haev only one forest edge
  anti_join(forest_edges_HBI.man %>% filter(e_form == 1 | e_form == 2 & inter_status_AT_17 == "two I" | e_form == 2 & inter_status_BT_17 == "two I") %>% group_by(plot_ID) %>% summarise(n = n()) %>% filter(n > 1) %>% select(plot_ID), by = "plot_ID") %>% 
  # remove plots that do now have a corresponding center coordiante in the HBI loc document
  semi_join(HBI_loc %>% filter(!is.na( RW_MED) & !is.na(HW_MED)) %>%  select(plot_ID)  %>% distinct(), by = "plot_ID")

tree.status.list <- vector("list", length = length(trees.one.edge$tree_ID))
tree.points.list <- vector("list", length = length(trees.one.edge$tree_ID))

for (i in 1:length(trees.one.edge$tree_ID)){ 
  #i = 1
  #i = which(grepl(50080, unique(trees.one.edge$plot_ID)))
  
  # select plot ID accordint to positioin in the list
  my.plot.id <- trees.one.edge[i, "plot_ID"] 
  my.tree.id <- trees.one.edge[i, "tree_ID"]
  
  # select the remaining cirlce we want to intersect the tree with
  my.rem.circle <- sf::st_as_sf(rem.circle.one.edge.df %>% filter(id == my.plot.id) %>% distinct())
  my.inter <- sf::st_as_sf(inter.poly.one.edge.df %>% filter(id == my.plot.id) %>% distinct())
  
  # assign crs
  my.utm.epsg <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"
  
  # select UTM corrdinates of the plot center
  my.center.easting <- HBI_loc[HBI_loc$plot_ID == my.plot.id, "RW_MED"]
  my.center.northing <- HBI_loc[HBI_loc$plot_ID == my.plot.id, "HW_MED"]
  
  # extract polar coordiantes of forest edge
  # point A 
  dist.tree <- trees.one.edge[i, "Dist_cm"]/100 
  azi.tree <- trees.one.edge[i, "azi_gon"] 
  x.tree <- dist.tree*sin(azi.tree)   # longitude, easting, RW, X
  y.tree <- dist.tree*cos(azi.tree)   # latitude, northing, HW, y 
  
  # transform polar into cartesian coordiantes
  tree.east <- my.center.easting + x.tree
  tree.north <- my.center.northing + y.tree
  
  # save cartesian coordiantes in dataframe
  tree.coord.df <- as.data.frame(cbind(
    "id" = c(my.plot.id), 
    "t_id" = c(my.tree.id),
    "lon" = c(tree.east),
    "lat" = c(tree.north)
  ))
  
  
  # create sf point object from dataframe
  #https://stackoverflow.com/questions/52551016/creating-sf-points-from-multiple-lat-longs
  tree.sf <-  sf::st_as_sf(tree.coord.df, coords = c("lon", "lat"), remove = FALSE)
  # assing CRS to points
  sf::st_crs(tree.sf) <- my.utm.epsg
  
  # print(plot(my.inter$geometry), 
  #       plot(my.rem.circle$geometry, add = T), 
  #       plot(tree.sf$geometry, add = T)
  #       )
  
  inter.tree.circle <- sf::st_intersection(tree.sf, my.rem.circle)
  inter.tree.edge <- sf::st_intersection(tree.sf, my.inter)
  
  tree_status <- ifelse(nrow(inter.tree.edge)!= 0, "B", 
                        ifelse(nrow(inter.tree.circle) != 0,  "A",
                               "warning"))
  
  tree.status.list[[i]] <- as.data.frame(cbind(
    "id" = c(my.plot.id), 
    "t_id" = c(my.tree.id),
    "lon" = c(tree.coord.df$lon),
    "lat" = c(tree.coord.df$lat),
    "t_stat" = c(tree_status))) 
  
  tree.points.list[[i]] <- c("t_stat" = tree_status, tree.sf)
  
  
}

# save tree corodiantes and status into dataframe
tree.status.list.one.edge.final <- rbindlist(tree.status.list)
tree.status.one.edge.df <- as.data.frame(tree.status.list.one.edge.final)
# save tree sf into dataframe
tree.points.list.one.edge.final <- rbindlist(tree.points.list)
tree.points.one.edge.df <- as.data.frame(tree.points.list.one.edge.final)





# intersection of trees with 2 edges
trees.two.edges <- HBI_trees %>%
  # filter only for trees that are located in plots with a forest edge
  semi_join(forest_edges_HBI.man %>% filter(e_form == 1 | e_form == 2) %>% 
              #& inter_status_AT_17 == "two I" | e_form == 2 & inter_status_BT_17 == "two I") %>% 
              select(plot_ID) %>% distinct(), by = "plot_ID") %>% 
  # filter for trees located in plots htat haev only one forest edge
  semi_join(forest_edges_HBI.man %>% filter(e_form == 1 | e_form == 2 & inter_status_AT_17 == "two I" | e_form == 2 & inter_status_BT_17 == "two I") %>% group_by(plot_ID) %>% summarise(n = n()) %>% filter(n > 1) %>% select(plot_ID), by = "plot_ID") %>% 
  # remove plots that do now have a corresponding center coordiante in the HBI loc document
  semi_join(HBI_loc %>% filter(!is.na( RW_MED) & !is.na(HW_MED)) %>%  select(plot_ID)  %>% distinct(), by = "plot_ID")

tree.status.two.edges.list <- vector("list", length = length(trees.two.edges$tree_ID))
tree.points.two.edges.list <- vector("list", length = length(trees.two.edges$tree_ID))

for (i in 1:length(trees.two.edges$tree_ID)){ 
  # i = 1
  #i = which(grepl(50080, (trees.two.edges$plot_ID)))
  
  # select plot ID accordint to positioin in the list
  my.plot.id <- trees.two.edges[i, "plot_ID"] 
  my.tree.id <- trees.two.edges[i, "tree_ID"]
  
  # select the remaining cirlce we want to intersect the tree with
  my.rem.circle <- sf::st_as_sf(rem.circle.two.edges.df %>% filter(id == my.plot.id) %>% distinct())
  my.edges.df <- inter.poly.two.edges.df %>% filter(id == my.plot.id) %>% distinct() %>% arrange(e_id)
  my.inter.1 <- sf::st_as_sf(my.edges.df[1,])
  my.inter.2 <- sf::st_as_sf(my.edges.df[2,])
  
  # assign crs
  my.utm.epsg <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"
  
  # select UTM corrdinates of the plot center
  my.center.easting <- HBI_loc[HBI_loc$plot_ID == my.plot.id, "RW_MED"]
  my.center.northing <- HBI_loc[HBI_loc$plot_ID == my.plot.id, "HW_MED"]
  
  # extract polar coordiantes of forest edge
  # point A 
  dist.tree <- trees.two.edges[i, "Dist_cm"]/100 
  azi.tree <- trees.two.edges[i, "azi_gon"] 
  x.tree <- dist.tree*sin(azi.tree)   # longitude, easting, RW, X
  y.tree <- dist.tree*cos(azi.tree)   # latitude, northing, HW, y 
  
  # transform polar into cartesian coordiantes
  tree.east <- my.center.easting + x.tree
  tree.north <- my.center.northing + y.tree
  
  # save cartesian coordiantes in dataframe
  tree.coord.df <- as.data.frame(cbind(
    "id" = c(my.plot.id), 
    "t_id" = c(my.tree.id),
    "lon" = c(tree.east),
    "lat" = c(tree.north)
  ))
  
  # create sf point object from dataframe
  #https://stackoverflow.com/questions/52551016/creating-sf-points-from-multiple-lat-longs
  tree.sf <-  sf::st_as_sf(tree.coord.df, coords = c("lon", "lat"), remove = FALSE)
  # assing CRS to points
  sf::st_crs(tree.sf) <- my.utm.epsg
  
  # print(plot(my.inter$geometry), 
  #       plot(my.rem.circle$geometry, add = T), 
  #       plot(tree.sf$geometry, add = T)
  #       )
  
  inter.tree.circle <- sf::st_intersection(tree.sf, my.rem.circle)
  inter.tree.edge.1 <- sf::st_intersection(tree.sf, my.inter.1)
  inter.tree.edge.2 <- sf::st_intersection(tree.sf, my.inter.2)
  
  tree_status <- ifelse(nrow(inter.tree.edge.1)!= 0 & nrow(inter.tree.edge.2)== 0 & nrow(inter.tree.circle)== 0,  "B", 
                        ifelse(nrow(inter.tree.edge.2)!= 0 & nrow(inter.tree.edge.1)== 0 & nrow(inter.tree.circle)== 0,  "C", 
                               ifelse(nrow(inter.tree.circle)!= 0 & nrow(inter.tree.edge.1)== 0 & nrow(inter.tree.edge.2)== 0,  "A",
                                      ifelse(nrow(inter.tree.circle)== 0 & nrow(inter.tree.edge.1)!= 0 & nrow(inter.tree.edge.2)!= 0,  "warning",
                                             "warning"))))
  
  tree.status.two.edges.list[[i]] <- as.data.frame(cbind(
    "id" = c(my.plot.id), 
    "t_id" = c(my.tree.id),
    "lon" = c(tree.coord.df$lon),
    "lat" = c(tree.coord.df$lat),
    "t_stat" = c(tree_status))) 
  
  tree.points.two.edges.list[[i]] <- c("t_stat" = tree_status, tree.sf)
  
  
}

# save tree corodiantes and status into dataframe
tree.status.list.two.edges.final <- rbindlist(tree.status.two.edges.list)
tree.status.two.edges.df <- as.data.frame(tree.status.list.two.edges.final)
# save tree sf into dataframe
tree.points.list.two.edges.final <- rbindlist(tree.points.two.edges.list)
tree.points.two.edges.df <- as.data.frame(tree.points.list.two.edges.final)

all.trees.points.df <- rbind(tree.points.one.edge.df,tree.points.two.edges.df) %>% distinct()


