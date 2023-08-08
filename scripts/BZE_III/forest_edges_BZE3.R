# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the peat land soil inventory
# heiht of living trees  

# ----- 0. SETUP ---------------------------------------------------------------
# ----- 0.1. Packages  ---------------------------------------------------------
## datamanagement
# install.packages("usethis")
#  install.packages("here")
#  install.packages("readr")
#  install.packages("tidyverse")
#  install.packages("tibble")
#  install.packages("dplyr")
#  install.packages("data.table")
#  install.packages("broom")
#  install.packages("purrr")
#  install.packages("devtools")
#  ## laTex
#  install.packages("stargazer")  #for compatability with Latex
#  install.packages("tikzDevice") #for compatability with Latex#
#  # visualisation
#  install.packages("ggthemes")
#  install.packages("ggplot2")
#  install.packages("reshape2") #for multiple y values
#  install.packages("ggforce") #for zooming in parts of the plot
# install.packages("ggforce")             # Install ggforce package
#  options(tz="CA")
#  install.packages("reshape2")
#  # analysis
#  install.packages("corrplot")
#  install.packages("AICcmodavg")
#  # forest related
#   install.packages("forestmangr")
#  install.packages("rBDAT")
#  install.packages("TapeR")
# install.packages("pkgbuild")
#  library("devtools")
#  if (! require("remotes")) 
#    install.packages("remotes")
#  remotes::install_gitlab("vochr/TapeS", build_vignettes = TRUE)
# install.packages("magrittr")
# install.packages("sjmisc")
# if(!require(devtools)) install.packages("devtools")
# devtools::install_github("kassambara/ggcorrplot")
#


# ----- 0.2. library   ---------------------------------------------------------
# datamanagement
library("usethis")
library("here")
library("readr")
library("tidyverse")
library("tibble")
library("dplyr")
library("data.table")
require(data.table)
library("broom")
library("purrr")
library("remotes")
library("devtools")
# laTex
library("stargazer")  #for compatability with Latex
library("tikzDevice") #for compatability with Latex
# visualisation
library("ggthemes")
library("ggplot2")
library("reshape2") #for multiple y values
library("ggforce") #for zooming in parts of the plot
options(tz="CA")
library("reshape2")
# analysis
library("corrplot")
library("AICcmodavg")
library("ggcorrplot")
# forest related
library("forestmangr")
library("rBDAT")
library("TapeR")
if (! require("remotes")) 
  install.packages("remotes")
library("remotes")
#devtools::install_gitlab("vochr/TapeS", build_vignettes = TRUE)
#remotes::install_gitlab("vochr/TapeS", build_vignettes = TRUE)
library("TapeS")
require(TapeS)
vignette("tapes", package = "TapeS")
library(magrittr)
library(sjmisc)
library("ggforce")                      # Load ggforce package


# ----- 0.3. working directory -------------------------------------------------
here::here()
getwd()

out.path.BZE3 <- ("output/out_data/out_data_BZE/") 

# ----- 0.4 data import -------------------------------------------------------
# LIVING TREES
# BZE3 BE dataset: this dataset contains the inventory data of the tree inventory accompanying the third national soil inventory
HBI_trees <- read.delim(file = here("data/input/BZE2_HBI/beab.csv"), sep = ",", dec = ",")

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

# BZE3

# Forest edges 
colnames(forest_edges_HBI) <- c("plot_ID", "e_ID", "e_type", "e_form", 
                                "A_dist", "A_azi",  "B_dist", "B_azi", 
                                "T_dist", "T_azi") # t = turning point 


# ----- 0.5 functions ---------------------------------------------------------------
# ---- 0.5.1. classes ---------------------------------------------------------------
# area of a circle
c_A <- function(r){
  circle_area <- r^2*pi;
  return(circle_area)
}
# ----- 0.5.1.1. DBH class ----------------------------------------------------------
DBH_c_function <- function(dbh){
  # create label for diameter classes according to BZE3 Bestandesaufnahmeanleitung
  labs_DBH <- c(seq(5, 55, by = 5)) ; 
  DBH_c <- cut(as.numeric(dbh),                               # cut the diameter
               breaks = c(seq(5, 55, by = 5), Inf),  # in sequences of 5
               labels = labs_DBH,                    # and label it according to labs (1.4.1)
               right = FALSE);
  return(DBH_c)
}

# ----- 0.5.1.2. age class ----------------------------------------------------------
# defining age classes from 1 to 160 in steps of 20
# this is a preparation fot the comparison with carbon stocks calcualted by te
labs_age <- c(seq(1, 180, by = 20))



# ----- 0.5.3. coordinate functions ---------------------------------------
# http://www.markusbaumi.ch/schule/formel/azimut.pdf
y_coord <- function(Dcp, AZIcp){      # originally x_coord
  # Xc =  x coordinate of centre = 0 
  # Dcp = Distance between point and centre
  # AZIcp=  Azimute betweeen Point and centre
  Xc <- 0;   # this is set to 2000 to avoid negative 
  X = Xc + Dcp * cos(AZIcp);
  return(X)
}

x_coord <- function(Dcp, AZIcp){  # originally y_coord
  # Yc =  y coordinate of centre = 0 
  # Dcp = Distance between point and centre
  # AZIcp=  Azimute betweeen Point and centre
  Yc <- 0;
  Y = Yc + Dcp * sin(AZIcp);
  return(Y)
}



# ----- 0.5.4. slope line -------------------------------------------------

slope <- function(x1, y1, x2, y2){
  b1 = (y2 - y1)/(x2 - x1);
  return(b1)
}

# ----- 0.5.5. intercept y axis line -------------------------------------------------

intercept <- function(x1, y1, slope){
  # resolve line function towards b0 after inserting known coordinates and slope
  # Y_A = b1_AB*X_A + b0_AB | (-b1_AB*X_A) 
  # Y_A - b1_AB*X_A = b0_AB 
  b0 = y1 - slope*x1;
  return(b0)
}


# ----- 0.5.6. intercept y axis line -------------------------------------------------
l <- function(b0, b1, x){
  y = b0 + b1*x;
  return(y)
}


# ----- 0.5.7. intersection circle line -----------------------------------

# equation circle r^2 = (x - xc)^2 + (y - yc)^2 ==>  r = sqr((x - xc)^2 + (y - yc)^2)
# equation line: y = b1*x + b0 
# equation intersection by inserting line into circle: r2 = (x - xc)^2 + (equation line - yc)^2 =  r2 = (x - xc)^2 + (( b1*x + b0) - yc)^2
# --> if distance of the line is higher then r of the circle, so there´s no result --> the line is not within the circle
# --> if distance of the line is higher equal r of the circle, so there´s 1 result the line touches the cicle
# --> if distance of the line is higher then r of the circle, so there are 2 results the line intersects the circle

# equation of x intersection:  (x - xc)^2 + ( b1*x + b0  - yc)^2)

# do a test run whith a plot that has only one edge
# l.df <- forest_edges_HBI %>% filter(plot_ID ==  "50005")
# l = l.df$e_b0_AB + l.df$e_b1_AB * X
# c.df <- data_circle %>% filter(r0 == 1784)
# c = c.df$r0^2 = (X - c.df$x0)^2 + (Y - c.df$y0)^2
# insert: 
# c.df$r0^2 = (X - c.df$x0)^2 + (l.df$e_b1_AB * X + l.df$e_b0_AB - c.df$y0)^2 
# resolve brakets (a-b)^2 = a^2 + 2*a*b + b^2; (a+b)^2 = a^2 + 2*a*b + b^2  
# a^2 -  2* a     * b + b^2         +              a^2       - 2*    a          * b                       + b^2
# c.df$r0^2 = 1*X^2 -  2*c.df$x0*X  + c.df$x0^2   +   l.df$e_b1_AB^2 * X^2 - 2*l.df$e_b1_AB*X* (l.df$e_b0_AB - c.df$y0) + (l.df$e_b0_AB - c.df$y0)^2
# summarize/ order: x2 + x + c = 
# c.df$r0^2 = 1*X^2  +   l.df$e_b1_AB^2 * X^2 -   2*c.df$x0*X - 2*l.df$e_b1_AB*X*(l.df$e_b0_AB - c.df$y0)   +     c.df$x0^2 + (l.df$e_b0_AB - c.df$y0)^2
#     r^2 =        a*           x2  +                                b                       *x    +     c 
# c.df$r0^2 = (1 +l.df$e_b1_AB^2)*X^2 -   (2*c.df$x0 - 2*l.df$e_b1_AB*(l.df$e_b0_AB - c.df$y0))*X   +     c.df$x0^2 + (l.df$e_b0_AB - c.df$y0)^2
# move r to other side 
# 0 = (1 +l.df$e_b1_AB^2)*X^2 -   (2*c.df$x0 - 2*l.df$e_b1_AB*(l.df$e_b0_AB - c.df$y0))*X   +     c.df$x0^2 + (l.df$e_b0_AB - c.df$y0)^2 - c.df$r0^2
# divide by a before x2
# 0 = ((1 +l.df$e_b1_AB^2)/(1 +l.df$e_b1_AB^2))*X^2  -   ((2*c.df$x0 - 2*l.df$e_b1_AB*(l.df$e_b0_AB - c.df$y0))/(1 +l.df$e_b1_AB^2))*X   +     (c.df$x0^2 + (l.df$e_b0_AB - c.df$y0)^2 - c.df$r0^2)/(1 +l.df$e_b1_AB^2)
# insert intro p/q fomrula
# x1 = -(p/2)+(sqrt((p/2)^2-q))
# x2 = -(p/2)-(sqrt((p/2)^2-q))

# p = b so the number before x in quadratic formula
# q = c so the number at the end of quadratic fomula


# x1 of intersection
intersection_c_lx1 <- function(l.b0, l.b1, c.y0, c.x0, c.r0) {
  
  # quadratic formula
  # 0 = ((1 +l.df$e_b1_AB^2)/(1 +l.df$e_b1_AB^2))*X^2  -   ((2*c.df$x0 - 2*l.df$e_b1_AB*(l.df$e_b0_AB - c.df$y0))/(1 +l.df$e_b1_AB^2))*X   +     (c.df$x0^2 + (l.df$e_b0_AB - c.df$y0)^2 - c.df$r0^2)/(1 +l.df$e_b1_AB^2)
  
  # x1 = -(p/2)+(sqrt((p/2)^2-q))
  # x2 = -(p/2)-(sqrt((p/2)^2-q))
  
  # p = b so the number before x in quadratic formula
  # q = c so the number at the end of quadratic fomula
  
  p = ((2*c.x0) + (2*l.b1*(l.b0 - c.y0)))/(1 + l.b1^2);
  q = (c.x0^2 + (l.b0 - c.y0)^2 - c.r0^2)/(1 +l.b1^2);
  x1 <-  -(p/2) + sqrt( ((p*-1)/2)^2-q );
  x2 <- - (p/2) - sqrt( ((p*-1)/2)^2-q );
  i.df <- as.data.frame(cbind(x1, x2));
  # https://community.rstudio.com/t/how-do-i-write-a-function-that-will-tell-me-if-an-equation-has-no-solution/159834/6
  # if no solutions
  # ifelse(
  #   is.na(x1) & is.na(x2), return(NA), 
  #   # if 1 solution
  #   ifelse(x1 == x2, return(x1), 
  #         # if 2 solutions
  #          ifelse(x1 != x2, return(x1))
  #         )
  #   )
  return(x1)
}


# x2 of intersection
intersection_c_lx2 <- function(l.b0, l.b1, c.y0, c.x0, c.r0) {
  
  # quadratic formula
  # 0 = ((1 +l.df$e_b1_AB^2)/(1 +l.df$e_b1_AB^2))*X^2  -   ((2*c.df$x0 - 2*l.df$e_b1_AB*(l.df$e_b0_AB - c.df$y0))/(1 +l.df$e_b1_AB^2))*X   +     (c.df$x0^2 + (l.df$e_b0_AB - c.df$y0)^2 - c.df$r0^2)/(1 +l.df$e_b1_AB^2)
  
  # x1 = -(p/2)+(sqrt((p/2)^2-q))
  # x2 = -(p/2)-(sqrt((p/2)^2-q))
  
  # p = b so the number before x in quadratic formula
  # q = c so the number at the end of quadratic fomula

  p = ((2*c.x0) + (2*l.b1*(l.b0 - c.y0)))/(1 + l.b1^2);
  q = (c.x0^2 + (l.b0 - c.y0)^2 - c.r0^2)/(1 +l.b1^2); 
  x1 <-  -(p/2) + sqrt( ((p*-1)/2)^2-q );
  x2 <- - (p/2) - sqrt( ((p*-1)/2)^2-q );
  i.df <- as.data.frame(cbind(x1, x2));

  return(x2)
}



# ----- 0.5.7.1. intersection status -----------------------------------------

intersection.status <- function(inter_x1, inter_x2) {
  i_status <-   ifelse(is.na(inter_x1) & is.na(inter_x2), " no I",      # if 0 solutions
                     ifelse(inter_x1 == inter_x2, "one I",            # if 1 solution
                            ifelse(inter_x1 != inter_x2, "two I")));
  return(i_status)
}




# ----0.5.8. azimut -------------------------------------------------------
azimut <- function(x2, y2, x1, y1){
 # azi = atan((y2 - y1)/(x2 - x1));
  azi = atan((x2 - x1)/(y2 - y1));
  return(azi)
}



# ----0.5.8. azimut -------------------------------------------------------
azi_correction <- function(x2, y2, x1, y1, azi){
  delta_x = x2 -x1 ;
  delta_y = y2-y1 ; 
  azi_corrected = ifelse(delta_x >= 0 & delta_y > 0 | delta_x > 0 & delta_y >= 0, azi,                    # first quadrant x + y+
                         ifelse(delta_x >= 0 & delta_y < 0 |delta_x > 0 & delta_y <= 0, azi+200,         # second quadrant x + y-
                                ifelse(delta_x <= 0 & delta_y < 0 |delta_x < 0 & delta_y <= 0,  azi+200,   # third quadrant x- y-
                             ifelse(delta_x <= 0 & delta_y > 0 | delta_x < 0 & delta_y >= 0, azi+400, NA
                                    )
                             )
                             )
                         );
  return(azi_corrected)
}



# ----- 0.5.8. distance between two points --------------------------------
distance <- function(x2, y2, x1, y1){
  d = sqrt(((y2 - y1)^2) + ((x2 - x1)^2));
  return(d)
}

# ------ 0.5.9. check if point lays in triangle  --------------------------
# this link https://stackoverflow.com/questions/2049582/how-to-determine-if-a-point-is-in-a-2d-triangle led me to the following links: 
# http://totologic.blogspot.com/2014/01/accurate-point-in-triangle-test.html
# https://www.geogebra.org/m/c8DwbVTP
# https://en.wikipedia.org/wiki/Barycentric_coordinate_system


p.in.triangle <- function(xa, xb, xc, ya, yb, yc, xp, yp){
  a = ((xp - xc)*(yb - yc) + (xc - xb)*(yp - yc)) / ((yb - yc)*(xa - xc) + (xc - xb)*(ya - yc));
  b = ((xp - xc)*(yc - ya) + (xa - xc)*(yp - yc)) / ((yb - yc)*(xa - xc) + (xc - xb)*(ya - yc));
  c = 1 - a - b;
  in.or.out = ifelse(0 <= a & a <= 1 & 0 <= b  & b <= 1 & 0 <= c & c <= 1, "A", "B");
  return(in.or.out)
}



# ------0.5.10. angle triabnle for area calculations ----------------------
# calculating angle between two lines at their point of intersection
angle_triangle <- function(x3, y3, x1, y1, x2, y2){
  b1_1 = (y1-y3)/(x1-x3)
  b1_2 = (y2-y3)/(x2-x3)
  # https://studyflix.de/mathematik/schnittwinkel-berechnen-5408
  m = (b1_1 - b1_2)/(1 + b1_1*b1_2);
  m_betrag = ifelse(m >= 0, m, m*(-1));
  # bogenmas in rad
  angle.between.two.lines.rad = atan(m_betrag);
  # transfer bogenmas in rad into Kreismaß in degrees 
 # https://www.matheretter.de/wiki/bogenmass-umrechnen
  angle.between.two.lines.degrees = angle.between.two.lines.rad*(180/pi);
  # transfer degrees into gon 
  angle.between.two.lines.gon = (angle.between.two.lines.degrees/360)*400;
  return(angle.between.two.lines.degrees)
}



# ------0.5.11. cirlce segment area  ----------------------
# calculate the area of a cirlce segment by the angle between the two branches of the segment 
circle_seg_A <- function(r, angle){
  A_c_seg = (pi*r^2) * angle/400; 
  return(A_c_seg)
}

# ------0.5.12. triangle area  ----------------------
triangle_A <- function(x1, x2, x3, y1, y2, y3){
  # x1|y1 and x2|y2 should be the intersections with the circle, 
  # x3|y3 should be the turning point or centre of the cirlce 
  
 # https://www.lernhelfer.de/schuelerlexikon/mathematik-abitur/artikel/flaecheninhalt-eines-dreiecks
  A_tri =  0.5*(x1*(y2-y3) + x2*(y3-y1) + x3*(y3-y2)) ;
    
  return(A_tri)
}


# ----- 0.5.13. selecting/ calculating total edge area per plot  ----------

tot.edge.A <- function(area_AT_AB_side, area_BT_side){
  A <- ifelse(!is.na(area_AT_AB_side) & !is.na(area_BT_side), area_AT_AB_side + area_BT_side,
              ifelse(!is.na(area_AT_AB_side) & is.na(area_BT_side), area_AT_AB_side, 
                     ifelse(is.na(area_AT_AB_side) & !is.na(area_BT_side), area_BT_side, 
                            0
                            )
                     ) 
              );
  return(A)
} 

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
  mutate(X_tree = x_coord(Dist_cm, azi_gon), 
         Y_tree = y_coord(Dist_cm, azi_gon)) %>% 
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
  mutate(X_A = ifelse(A_azi != "-2", x_coord(A_dist, A_azi), NA), # if the value is marked -2 its equal to an NA
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
  mutate(b0_AB = ifelse(e_form == "1", intercept(X_A, Y_A, b1_AB), NA), 
         b0_AT = ifelse(e_form == "2", intercept(X_T, Y_T, b1_AT), NA),
         b0_BT = ifelse(e_form == "2", intercept(X_T, Y_T, b1_BT), NA)) %>% 
### 17m circle --> used for tree status also   
  # find x coordinate of the interception between line and 17.84m circle: insert line equation in circle equation (function: intersection_C_lx1, intersection_lx1)
  # for AB line 
  mutate(X1_inter_AB_17 = intersection_c_lx1(b0_AB, b1_AB,  data_circle$y0[3], data_circle$x0[3], data_circle$r0[3]),
         X2_inter_AB_17 = intersection_c_lx2(b0_AB, b1_AB, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3]), 
         inter_status_AB_17 = intersection.status(X1_inter_AB_17, X2_inter_AB_17),
         # for AT line
         X1_inter_AT_17 =intersection_c_lx1(b0_AT, b1_AT, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3]),
         X2_inter_AT_17 =intersection_c_lx2(b0_AT, b1_AT, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3]), 
         inter_status_AT_17 = intersection.status(X1_inter_AT_17, X2_inter_AT_17),
         # for BT line
         X1_inter_BT_17 =intersection_c_lx1(b0_BT, b1_BT, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3]),
         X2_inter_BT_17 =intersection_c_lx2(b0_BT, b1_BT, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3]), 
         inter_status_BT_17 = intersection.status(X1_inter_BT_17, X2_inter_BT_17)) %>%   
  # y intersection with 17m circle: insert x of intercept with circle in equation of line
  # AB line 
  mutate(Y1_inter_AB_17 = l(b0_AB, b1_AB, X1_inter_AB_17), 
         Y2_inter_AB_17 = l(b0_AB, b1_AB, X2_inter_AB_17), 
         # AT line 
       Y1_inter_AT_17 = l(b0_AT, b1_AT, X1_inter_AT_17), 
       Y2_inter_AT_17 = l(b0_AT, b1_AT, X2_inter_AT_17), 
       # BT line 
       Y1_inter_BT_17 = l(b0_BT, b1_BT, X1_inter_BT_17), 
       Y2_inter_BT_17 = l(b0_BT, b1_BT, X2_inter_BT_17)) %>%
##### if ever i build a loop for this it´ll have to start with thte intersection status
### for 12m circle   
   # interception status between line and 12.68m circle: insert line equation in circle equation (function: intersection_C_lx1, intersection_lx1)
         # for AB line 
   mutate(X1_inter_AB_12 = intersection_c_lx1(b0_AB, b1_AB,  data_circle$y0[2], data_circle$x0[2], data_circle$r0[2]),
          X2_inter_AB_12 = intersection_c_lx2(b0_AB, b1_AB, data_circle$y0[2], data_circle$x0[2], data_circle$r0[2]), 
          inter_status_AB_12 = intersection.status(intersection_c_lx1(b0_AB, b1_AB,  data_circle$y0[2], data_circle$x0[2], data_circle$r0[2]),
                                                   intersection_c_lx2(b0_AB, b1_AB, data_circle$y0[2], data_circle$x0[2], data_circle$r0[2])),  # if 2 solutions
          # for AT line
          X1_inter_AT_12 = intersection_c_lx1(b0_AT, b1_AT, data_circle$y0[2], data_circle$x0[2], data_circle$r0[2]),
          X2_inter_AT_12 = intersection_c_lx2(b0_AT, b1_AT, data_circle$y0[2], data_circle$x0[2], data_circle$r0[2]), 
          inter_status_AT_12 = intersection.status(intersection_c_lx1(b0_AT, b1_AT, data_circle$y0[2], data_circle$x0[2], data_circle$r0[2]),
                                                   intersection_c_lx2(b0_AT, b1_AT, data_circle$y0[2], data_circle$x0[2], data_circle$r0[2])), # if 2 solutions
          # for BT line
          X1_inter_BT_12 =intersection_c_lx1(b0_BT, b1_BT, data_circle$y0[2], data_circle$x0[2], data_circle$r0[2]),
          X2_inter_BT_12 =intersection_c_lx2(b0_BT, b1_BT, data_circle$y0[2], data_circle$x0[2], data_circle$r0[2]), 
          inter_status_BT_12 = intersection.status(intersection_c_lx1(b0_BT, b1_BT, data_circle$y0[2], data_circle$x0[2], data_circle$r0[2]),
                                                   intersection_c_lx2(b0_BT, b1_BT, data_circle$y0[2], data_circle$x0[2], data_circle$r0[2]))) %>% 
  # y intercept with 12m circle: insert x of intercept with circle in equation of line
  # AB line 
  mutate(Y1_inter_AB_12 = l(b0_AB, b1_AB, X1_inter_AB_12), 
         Y2_inter_AB_12 = l(b0_AB, b1_AB, X2_inter_AB_12), 
         # AT line 
         Y1_inter_AT_12 = l(b0_AT, b1_AT, X1_inter_AT_12), 
         Y2_inter_AT_12 = l(b0_AT, b1_AT, X2_inter_AT_12), 
         # BT line 
         Y1_inter_BT_12 = l(b0_BT, b1_BT, X1_inter_BT_12), 
         Y2_inter_BT_12 = l(b0_BT, b1_BT, X2_inter_BT_12)) %>%
 ### for 5m circle   
   # interception status between line and 5.64 m circle: insert line equation in circle equation (function: intersection_C_lx1, intersection_lx1)
          # for AB line 
   mutate(X1_inter_AB_5 = intersection_c_lx1(b0_AB, b1_AB,  data_circle$y0[1], data_circle$x0[1], data_circle$r0[1]),
          X2_inter_AB_5 = intersection_c_lx2(b0_AB, b1_AB, data_circle$y0[1], data_circle$x0[1], data_circle$r0[1]), 
          inter_status_AB_5 = intersection.status(intersection_c_lx1(b0_AB, b1_AB,  data_circle$y0[1], data_circle$x0[1], data_circle$r0[1]), 
                                                       intersection_c_lx2(b0_AB, b1_AB, data_circle$y0[1], data_circle$x0[1], data_circle$r0[1])),
          # for AT line
          X1_inter_AT_5 =intersection_c_lx1(b0_AT, b1_AT, data_circle$y0[1], data_circle$x0[1], data_circle$r0[1]),
          X2_inter_AT_5 =intersection_c_lx2(b0_AT, b1_AT, data_circle$y0[1], data_circle$x0[1], data_circle$r0[1]), 
          inter_status_AT_5 = intersection.status(intersection_c_lx1(b0_AT, b1_AT, data_circle$y0[1], data_circle$x0[1], data_circle$r0[1]), 
                                                  intersection_c_lx2(b0_AT, b1_AT, data_circle$y0[1], data_circle$x0[1], data_circle$r0[1])),
          # for BT line
          X1_inter_BT_5 =intersection_c_lx1(b0_BT, b1_BT, data_circle$y0[1], data_circle$x0[1], data_circle$r0[1]),
          X2_inter_BT_5 =intersection_c_lx2(b0_BT, b1_BT, data_circle$y0[1], data_circle$x0[1], data_circle$r0[1]), 
          inter_status_BT_5 = intersection.status(intersection_c_lx1(b0_BT, b1_BT, data_circle$y0[1], data_circle$x0[1], data_circle$r0[1]), 
                                                  intersection_c_lx2(b0_BT, b1_BT, data_circle$y0[1], data_circle$x0[1], data_circle$r0[1]))) %>%
  # y intercept with 5m circle: insert x of intercept with circle in equation of line
  # AB line 
  mutate(Y1_inter_AB_5 = l(b0_AB, b1_AB, X1_inter_AB_5), 
         Y2_inter_AB_5 = l(b0_AB, b1_AB, X2_inter_AB_5), 
         # AT line 
         Y1_inter_AT_5 = l(b0_AT, b1_AT, X1_inter_AT_5), 
         Y2_inter_AT_5 = l(b0_AT, b1_AT, X2_inter_AT_5), 
         # BT line 
         Y1_inter_BT_5 = l(b0_BT, b1_BT, X1_inter_BT_5), 
         Y2_inter_BT_5 = l(b0_BT, b1_BT, X2_inter_BT_5)) %>%
         # distance interception centre --> to see if points are actually placed on the rim of the circle 
  mutate(inter_1_dist = distance(X1_inter_AB_17, Y1_inter_AB_17, 0, 0),     # this is just to control if the whole thing worked and 
      # selecting intersections on the "right" side to check if point lies within triangle
         #  to calculate the triangles Barycentric coordinates we need 3 points: A, B, C = centre point
         # in case T lies within the circle, we want R to select A and B from the intersection with the circle.
         # Whereby we have to use a wider radius, to make sure that trees located the halfmoon of the circle cut by the triangle (Kreisbogen) are selected too. 
         # when t lies inside the circle (so both lines reach outside) ue only intersception point where direction between inter_AT and AT is equal choose this x, we need a buffer tho  
         # the following statement says:  if T lies within circle check if the slope of x_inter_1  or the slope of x_inter_2 is equal to the slope of AT,
         #                                choose the x which has the same slope (x_inter_1 or x_inter_2)as the second point on the line (A or B) 
         #                                but with a buffer of + 216, which is why it has to be newly calculated 
         # find the intercept of circle and line that prolonges the line between a and t or B and T
         azi_C_AB_inter_1 = azi_correction(X1_inter_AB_17, Y1_inter_AB_17, 0, 0, azimut(X1_inter_AB_17, Y1_inter_AB_17, 0, 0)),
         azi_C_AB_inter_2 = azi_correction(X2_inter_AB_17, Y2_inter_AB_17, 0, 0, azimut(X2_inter_AB_17, Y2_inter_AB_17, 0, 0)),
         # AT line 
         azi_T_A = azi_correction(X_A, Y_A, X_T, Y_T, azimut(X_A, Y_A, X_T, Y_T)),
         azi_T_AT_inter_1 = azi_correction(X1_inter_AT_17, Y1_inter_AT_17, X_T, Y_T, azimut(X1_inter_AT_17, Y1_inter_AT_17, X_T, Y_T)),
         azi_T_AT_inter_2 = azi_correction(X2_inter_AT_17, Y2_inter_AT_17, X_T, Y_T, azimut(X2_inter_AT_17, Y2_inter_AT_17, X_T, Y_T)),
         # BT line
         azi_T_B = azi_correction(X_B, Y_B, X_T, Y_T, azimut(X_B, Y_B, X_T, Y_T)),
         azi_T_BT_inter_1 = azi_correction(X1_inter_BT_17, Y1_inter_BT_17, X_T, Y_T, azimut(X1_inter_BT_17, Y1_inter_BT_17, X_T, Y_T)),
         azi_T_BT_inter_2 = azi_correction(X2_inter_BT_17, Y2_inter_BT_17, X_T, Y_T, azimut(X2_inter_BT_17, Y2_inter_BT_17, X_T, Y_T)),
         # for those turning points that lay outside the circle, select the intercetion point with the gratest distance to c and prolong it
         dist_T_AT_inter_1 = distance(X1_inter_AT_17, Y1_inter_AT_17, X_T, Y_T), 
         dist_T_AT_inter_2 = distance(X2_inter_AT_17, Y2_inter_AT_17, X_T, Y_T), 
         dist_T_BT_inter_1 = distance(X1_inter_BT_17, Y1_inter_BT_17, X_T, Y_T), 
         dist_T_BT_inter_2 = distance(X2_inter_BT_17, Y2_inter_BT_17, X_T, Y_T), 
         # if azimut T to A  identical to azimut T to intercept 1 A and circle use this intercept (inter_AT_1) for the triable, if azimut T to A identical to azimute T to intercept 2 between A and  circle use this intercept (inter_AT_2)
         X_inter_AT_triangle_60 = case_when(T_dist <= 1784 &  azi_T_AT_inter_1 == azi_T_A ~ intersection_c_lx1(b0_AT,b1_AT,0,0, data_circle$rmax[3]*2),
                                            T_dist <= 1784 & azi_T_AT_inter_2 == azi_T_A ~  intersection_c_lx2(b0_AT, b1_AT, 0, 0,  data_circle$rmax[3]*2),
                                            T_dist > 1784 & dist_T_AT_inter_1 > dist_T_AT_inter_2 ~ intersection_c_lx1(b0_AT,b1_AT,0,0, data_circle$rmax[3]*2), 
                                            T_dist > 1784 & dist_T_AT_inter_2 > dist_T_AT_inter_1 ~ intersection_c_lx2(b0_AT,b1_AT,0,0, data_circle$rmax[3]*2), 
                                            TRUE ~ NA ), 
         X_inter_BT_triangle_60 = case_when(T_dist <= 1784 & azi_T_BT_inter_1 == azi_T_B ~ intersection_c_lx1(b0_BT,b1_BT, 0, 0, data_circle$rmax[3]*2),
                                            T_dist <= 1784 & azi_T_BT_inter_2 == azi_T_B ~  intersection_c_lx2(b0_BT, b1_BT, 0, 0, data_circle$rmax[3]*2),
                                            T_dist > 1784 & dist_T_BT_inter_1 > dist_T_BT_inter_2 ~ intersection_c_lx1(b0_BT,b1_BT,0,0, data_circle$rmax[3]*2), 
                                            T_dist > 1784 & dist_T_BT_inter_2 > dist_T_BT_inter_1 ~ intersection_c_lx2(b0_BT,b1_BT,0,0, data_circle$rmax[3]*2), 
                                            TRUE ~ NA)) %>% 
  # calcualte y to the x that lie in the same direction then the second point on the line, if turning points lies witin circle and lines "reach out"
  mutate(Y_inter_AT_triangle_60 = l(b0_AT, b1_AT, X_inter_AT_triangle_60),  
         Y_inter_BT_triangle_60 = l(b0_BT, b1_BT, X_inter_BT_triangle_60)) 

# new approach to localise points when edge is a line: 
# https://math.stackexchange.com/questions/1577062/how-to-know-if-a-given-point-is-inside-a-2d-circles-segment




# ----- 1.1.2.3. edge area: circle segments, triangles  ---------------------------------------------
# select respective intersection x and y that lies in the same direction as the secnd point of the line if:
# - the forest edges type == 2 
# - the turning point is situated within the circle 
# - so both arms have 2 intersections with the circle and we can

forest_edges_HBI.man <- 
  forest_edges_HBI.man %>% 
      # X & y coordinate of intersection with 17m circle that corresponds with the azimute between from T to A to be sure we select the intersection on the "ricght side"
  mutate(X_inter_AT_17_triangle = case_when(e_form == 2 & T_dist <= data_circle$r0[3] &  azi_T_AT_inter_1 == azi_T_A ~ intersection_c_lx1(b0_AT, b1_AT, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3]), # X1
                                          e_form == 2 & T_dist <=  data_circle$r0[3] & azi_T_AT_inter_2 == azi_T_A ~  intersection_c_lx2(b0_AT, b1_AT, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3]),    # X2 
                                          TRUE ~ NA ), 
          X_inter_BT_17_triangle = case_when(e_form == 2 & T_dist <=  data_circle$r0[3] & azi_T_BT_inter_1 == azi_T_B ~ intersection_c_lx1(b0_BT, b1_BT, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3]),    # X1
                                           e_form == 2 & T_dist <=  data_circle$r0[3] & azi_T_BT_inter_2 == azi_T_B ~ intersection_c_lx2(b0_BT, b1_BT, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3]),    # X2
                                          TRUE ~ NA),
          Y_inter_AT_17_triangle = case_when(e_form == 2 & T_dist <=  data_circle$r0[3] &  azi_T_AT_inter_1 == azi_T_A ~ l(b0_AT, b1_AT, intersection_c_lx1(b0_AT, b1_AT, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3])), #  Y1_inter_AT_17,
                                          e_form == 2 & T_dist <=  data_circle$r0[3] & azi_T_AT_inter_2 == azi_T_A ~ l(b0_AT, b1_AT, intersection_c_lx2(b0_AT, b1_AT, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3])), # Y2_inter_AT_17,
                                          TRUE ~ NA ), 
          Y_inter_BT_17_triangle = case_when(e_form == 2 & T_dist <=  data_circle$r0[3] & azi_T_BT_inter_1 == azi_T_B ~  l(b0_BT, b1_BT, intersection_c_lx1(b0_BT, b1_BT, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3])), # Y1_inter_BT_17,
                                          e_form == 2 & T_dist <=  data_circle$r0[3] & azi_T_BT_inter_2 == azi_T_B ~  l(b0_BT, b1_BT, intersection_c_lx2(b0_BT, b1_BT, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3])),
                                          TRUE ~ NA),
      # x & y coordinate of intersection with 12m cirle that correspondes with azimute between turning point and second point on line
          X_inter_AT_12_triangle = case_when(e_form == 2 & T_dist <= data_circle$r0[2] &  azi_T_AT_inter_1 == azi_T_A ~ intersection_c_lx1(b0_AT, b1_AT, data_circle$y0[2], data_circle$x0[2], data_circle$r0[2]), # X1
                                           e_form == 2 & T_dist <=  data_circle$r0[2] & azi_T_AT_inter_2 == azi_T_A ~  intersection_c_lx2(b0_AT, b1_AT, data_circle$y0[2], data_circle$x0[2], data_circle$r0[2]),    # X2 
                                           TRUE ~ NA ), 
          X_inter_BT_12_triangle = case_when(e_form == 2 & T_dist <=  data_circle$r0[2] & azi_T_BT_inter_1 == azi_T_B ~ intersection_c_lx1(b0_BT, b1_BT, data_circle$y0[2], data_circle$x0[2], data_circle$r0[2]),    # X1
                                           e_form == 2 & T_dist <=  data_circle$r0[2] & azi_T_BT_inter_2 == azi_T_B ~ intersection_c_lx2(b0_BT, b1_BT, data_circle$y0[2], data_circle$x0[2], data_circle$r0[2]),    # X2
                                           TRUE ~ NA),
          Y_inter_AT_12_triangle = case_when(e_form == 2 & T_dist <=  data_circle$r0[2] &  azi_T_AT_inter_1 == azi_T_A ~ l(b0_AT, b1_AT, intersection_c_lx1(b0_AT, b1_AT, data_circle$y0[2], data_circle$x0[2], data_circle$r0[2])), #  Y1_inter_AT_17,
                                           e_form == 2 & T_dist <=  data_circle$r0[2] & azi_T_AT_inter_2 == azi_T_A ~ l(b0_AT, b1_AT, intersection_c_lx2(b0_AT, b1_AT, data_circle$y0[2], data_circle$x0[2], data_circle$r0[2])), # Y2_inter_AT_17,
                                           TRUE ~ NA ), 
          Y_inter_BT_12_triangle = case_when(e_form == 2 & T_dist <=  data_circle$r0[2] & azi_T_BT_inter_1 == azi_T_B ~  l(b0_BT, b1_BT, intersection_c_lx1(b0_BT, b1_BT, data_circle$y0[2], data_circle$x0[2], data_circle$r0[2])), # Y1_inter_BT_17,
                                           e_form == 2 & T_dist <=  data_circle$r0[2] & azi_T_BT_inter_2 == azi_T_B ~  l(b0_BT, b1_BT, intersection_c_lx2(b0_BT, b1_BT, data_circle$y0[2], data_circle$x0[2], data_circle$r0[2])),
                                           TRUE ~ NA),
      # x & y coordinate of intersection with 5m cirle that correspondes with azimute between turning point and second point on line
          X_inter_AT_5_triangle = case_when(e_form == 2 & T_dist <= data_circle$r0[1] &  azi_T_AT_inter_1 == azi_T_A ~ intersection_c_lx1(b0_AT, b1_AT, data_circle$y0[1], data_circle$x0[1], data_circle$r0[1]), # X1
                                             e_form == 2 & T_dist <=  data_circle$r0[1] & azi_T_AT_inter_2 == azi_T_A ~  intersection_c_lx2(b0_AT, b1_AT, data_circle$y0[1], data_circle$x0[1], data_circle$r0[1]),    # X2 
                                             TRUE ~ NA ), 
          X_inter_BT_5_triangle = case_when(e_form == 2 & T_dist <=  data_circle$r0[1] & azi_T_BT_inter_1 == azi_T_B ~ intersection_c_lx1(b0_BT, b1_BT, data_circle$y0[1], data_circle$x0[1], data_circle$r0[1]),    # X1
                                             e_form == 2 & T_dist <=  data_circle$r0[1] & azi_T_BT_inter_2 == azi_T_B ~ intersection_c_lx2(b0_BT, b1_BT, data_circle$y0[1], data_circle$x0[1], data_circle$r0[1]),    # X2
                                             TRUE ~ NA),
          Y_inter_AT_5_triangle = case_when(e_form == 2 & T_dist <=  data_circle$r0[1] &  azi_T_AT_inter_1 == azi_T_A ~ l(b0_AT, b1_AT, intersection_c_lx1(b0_AT, b1_AT, data_circle$y0[1], data_circle$x0[1], data_circle$r0[1])), #  Y1_inter_AT_17,
                                             e_form == 2 & T_dist <=  data_circle$r0[1] & azi_T_AT_inter_2 == azi_T_A ~ l(b0_AT, b1_AT, intersection_c_lx2(b0_AT, b1_AT, data_circle$y0[1], data_circle$x0[1], data_circle$r0[1])), # Y2_inter_AT_17,
                                             TRUE ~ NA ), 
          Y_inter_BT_5_triangle = case_when(e_form == 2 & T_dist <=  data_circle$r0[1] & azi_T_BT_inter_1 == azi_T_B ~  l(b0_BT, b1_BT, intersection_c_lx1(b0_BT, b1_BT, data_circle$y0[1], data_circle$x0[1], data_circle$r0[1])), # Y1_inter_BT_17,
                                           e_form == 2 & T_dist <=  data_circle$r0[1] & azi_T_BT_inter_2 == azi_T_B ~  l(b0_BT, b1_BT, intersection_c_lx2(b0_BT, b1_BT, data_circle$y0[1], data_circle$x0[1], data_circle$r0[1])),
                                           TRUE ~ NA)) %>%  
       # calculate the angle the two lines have at point T: https://studyflix.de/mathematik/schnittwinkel-berechnen-5408
          # this is always gonna be the same but i have to consider how the area of the circle is going to change depending on where the location of the turning point 
                                 # if edge type is a line calcualte the intersection angle between the lines betweeen A to the centre and 0 to the centre
 mutate(angle_ABT_ABC_AC = case_when(e_form == "1" & inter_status_AB_17 == "two I" ~ angle_triangle(0, 0, X_A, Y_A, X_B, Y_B),
                                   # with turning point and outiside of circle and both or at least 1 arms reaches in calcualte the angle between interception linbes from interception point 1 and 2 of the line AT with the circle to the 
                                  T_dist >  data_circle$r0[3] & e_form == "2" & inter_status_AT_17 == "two I" &  T_dist >  data_circle$r0[3] &  e_form == "2" & inter_status_BT_17 == "two I"  ~  angle_triangle(0, 0, X1_inter_AT_17, Y1_inter_AT_17, X2_inter_AT_17, Y2_inter_AT_17), # BT side follows in next column
                                  T_dist >  data_circle$r0[3] & e_form == "2" & inter_status_AT_17 == "two I" &  T_dist >  data_circle$r0[3] &  e_form == "2" & inter_status_BT_17 != "two I"  ~  angle_triangle(0, 0, X1_inter_AT_17, Y1_inter_AT_17, X2_inter_AT_17, Y2_inter_AT_17),
                                  # if t lies outside and there´s no or just one intersection on each line we don´t need the angle cause all trees are inside the plot
                                  T_dist >  data_circle$r0[3] & e_form == "2" & inter_status_AT_17 != "two I" &  T_dist >  data_circle$r0[3] &  e_form == "2" & inter_status_BT_17 != "two I"  ~  NA, 
                                  T_dist >  data_circle$r0[3] & e_form == "2" & inter_status_AT_17 != "two I" &  T_dist >  data_circle$r0[3] &  e_form == "2" & inter_status_BT_17 == "two I"  ~  NA, # follows in next column
                                  # if t lies inside the circle and both arms reach out, calculate the anlge between at point T where AT and BT meet 
                                  T_dist <=  data_circle$r0[3] & e_form == "2" & inter_status_AT_17 == "two I" &  T_dist <=  data_circle$r0[3] &  e_form == "2" & inter_status_BT_17 == "two I"  ~  angle_triangle(0, 0, X_inter_AT_17_triangle, Y_inter_AT_17_triangle, X_inter_BT_17_triangle, Y_inter_BT_17_triangle),
                                  # if t lies inside the circle and only tha  AT arms reaches out/ has two intersections  calculate the anlge between the intersectios of A with the circle and T
                                  T_dist <=  data_circle$r0[3] & e_form == "2" & inter_status_AT_17 == "two I" &  T_dist <=  data_circle$r0[3] &  e_form == "2" & inter_status_BT_17 != "two I"  ~  angle_triangle(0, 0, X1_inter_AT_17, Y1_inter_AT_17, X2_inter_AT_17, Y2_inter_AT_17), 
                                  T_dist <=  data_circle$r0[3] & e_form == "2" & inter_status_AT_17 != "two I" &  T_dist <=  data_circle$r0[3] &  e_form == "2" & inter_status_BT_17 != "two I"  ~  NA, # no itnersechtion means every thing is inside
                                  T_dist <=  data_circle$r0[3] & e_form == "2" & inter_status_AT_17 != "two I" &  T_dist <= data_circle$r0[3] &  e_form == "2" & inter_status_BT_17 == "two I"  ~  NA, # follows in next column
                                  TRUE ~ NA),
                                    # turning point outside the cirle and there are two intersections of AT and BT calcuale angle between at the intersections of the lines from the respectivce intersections to centre of the plot
        angle_ABT_ABC_BC = case_when(T_dist >  data_circle$r0[3] & e_form == "2" & inter_status_AT_17 == "two I" &  T_dist >  data_circle$r0[3] &  e_form == "2" & inter_status_BT_17 == "two I"  ~  angle_triangle(0, 0, X1_inter_BT_17, Y1_inter_BT_17, X2_inter_BT_17, Y2_inter_BT_17), 
                                     # turning point outside the circle and there are no or one intersection on the AT branch but 2 intersections on the BT line calculate the angle between the lines of the intersections where they meet in the centre of the plot
                                     T_dist >  data_circle$r0[3] & e_form == "2" & inter_status_AT_17 != "two I" &  T_dist >  data_circle$r0[3] &  e_form == "2" & inter_status_BT_17 == "two I"  ~  angle_triangle(0, 0, X1_inter_BT_17, Y1_inter_BT_17, X2_inter_BT_17, Y2_inter_BT_17),
                                     # if t lies inside the circle and only arm BT  reaches out/ has two intersections  calculate the anlge between the intersectios of B with the circle and T
                                     T_dist <=  data_circle$r0[3] & e_form == "2" & inter_status_AT_17 != "two I" &  T_dist <=  data_circle$r0[3] &  e_form == "2" & inter_status_BT_17 == "two I"  ~  angle_triangle(0, 0, X1_inter_BT_17, Y1_inter_BT_17, X2_inter_BT_17, Y2_inter_BT_17),
                                     TRUE ~ NA)) %>% 
  # calculating the area affected by the forest edge for 17.74m circuit: 
        # if T lies within the circle, the area to of the forest edge is the area of the whole circle segment determined by the corrected X and Y 
        # if T lies outside the cirlce and there are two intersections for each side of the triangle, we have to calcualte the circle segment drawn between 
          # (1) the cirlce centre, inter1_AT and inter2_AT and (2) the cirlce centre, inter1_BT and inter2_BT following we have to calcualte the area of the triangle drwan between 
          # (1) the cirlce centre, inter1_AT and inter2_AT and (2) the cirlce centre, inter1_BT and inter2_BT and withdraw it from the whole segments area
        # if there is no T, so the e_form == 1 is we calcualte the circle segment drawn by  the circle centre, A and B and then withdraw the are aof the triangle between the circle centre, A and B
                                # for edges without T --> no turning point and 2 intersections of the line 
   mutate(circle_segment_ABC_AC_17_cm2 = case_when(e_form == "1" & inter_status_AB_17 == "two I" ~ circle_seg_A(data_circle$r0[3], angle_ABT_ABC_AC),
                                # for T outside circle
                                       # with turning point and outiside of circle and both or at least 1 arms reaches in calcualte the circle intersection between interception linbes from interception point 1 and 2 of the line AT with the circle to the 
                                      T_dist >  data_circle$r0[3] & e_form == "2" & inter_status_AT_17 == "two I" &  T_dist >  data_circle$r0[3] &  e_form == "2" & inter_status_BT_17 == "two I"  ~  circle_seg_A(data_circle$r0[3], angle_ABT_ABC_AC), # BT side follows in next column
                                      T_dist >  data_circle$r0[3] & e_form == "2" & inter_status_AT_17 == "two I" &  T_dist >  data_circle$r0[3] &  e_form == "2" & inter_status_BT_17 != "two I"  ~  circle_seg_A(data_circle$r0[3], angle_ABT_ABC_AC),
                                      # if t lies outside and there´s no or just one intersection on each line we don´t need the circle intersection cause all trees are inside the plot
                                      T_dist >  data_circle$r0[3] & e_form == "2" & inter_status_AT_17 != "two I" &  T_dist >  data_circle$r0[3] &  e_form == "2" & inter_status_BT_17 != "two I"  ~  NA, 
                                      T_dist >  data_circle$r0[3] & e_form == "2" & inter_status_AT_17 != "two I" &  T_dist >  data_circle$r0[3] &  e_form == "2" & inter_status_BT_17 == "two I"  ~  NA, # follows in next column
                                 # for T inside cirlce 
                                      # if t lies inside the circle and both arms reach out, calculate the circle intersection between at point T where AT and BT meet 
                                      T_dist <=  data_circle$r0[3] & e_form == "2" & inter_status_AT_17 == "two I" &  T_dist <=  data_circle$r0[3] &  e_form == "2" & inter_status_BT_17 == "two I"  ~  circle_seg_A(data_circle$r0[3], angle_ABT_ABC_AC),
                                      # if t lies inside the circle and both arm AT arms reaches out/ has two intersections  calculate the circle intersection between A inter 1 and A inter 2 and centre
                                      T_dist <=  data_circle$r0[3] & e_form == "2" & inter_status_AT_17 == "two I" &  T_dist <=  data_circle$r0[3] &  e_form == "2" & inter_status_BT_17 != "two I"  ~  circle_seg_A(data_circle$r0[3], angle_ABT_ABC_AC), 
                                      # if T lies inside the cirlce but there are no intersections whatsoever, we don´t calcualte any areas
                                      T_dist <=  data_circle$r0[3] & e_form == "2" & inter_status_AT_17 != "two I" &  T_dist <=  data_circle$r0[3] &  e_form == "2" & inter_status_BT_17 != "two I"  ~  NA, 
                                      T_dist <=  data_circle$r0[3] & e_form == "2" & inter_status_AT_17 != "two I" &  T_dist <= data_circle$r0[3] &  e_form == "2" & inter_status_BT_17 == "two I"  ~  NA, # follows in next column
                                      TRUE ~ NA),
                                # for T outside circle 
                                     # if t lies outside and both arms intersect the circle or only the BT arm intersects the circle, we need a circle segment between B1, B2 and the circle centre
         circle_segment_ABC_BC_17_cm2 = case_when(T_dist >  data_circle$r0[3] & e_form == "2" & inter_status_AT_17 == "two I" &  T_dist >  data_circle$r0[3] &  e_form == "2" & inter_status_BT_17 == "two I"  ~  circle_seg_A(data_circle$r0[3], angle_ABT_ABC_BC), 
                                      T_dist >  data_circle$r0[3] & e_form == "2" & inter_status_AT_17 != "two I" &  T_dist >  data_circle$r0[3] &  e_form == "2" & inter_status_BT_17 == "two I"  ~  circle_seg_A(data_circle$r0[3], angle_ABT_ABC_BC), 
                                # for T inside circle     
                                      # if t lies inside the cirlce and the only BT arm intersects the circle, we need a circle segment between B1, B2 and the circle centre, 
                                      # if t lise inside the cirlce and both arms intersepct the circle, we already calcualted the whoke circle segment area of A-B-T in the previous column
                                      T_dist <=  data_circle$r0[3] & e_form == "2" & inter_status_AT_17 != "two I" &  T_dist <=  data_circle$r0[3] &  e_form == "2" & inter_status_BT_17 == "two I"  ~  circle_seg_A(data_circle$r0[3], angle_ABT_ABC_BC), 
                                      TRUE ~ NA),
                                # for edges without T
         triangle_ABC_AC_17_cm2 = case_when(e_form == "1" & inter_status_AB_17 == "two I" ~ triangle_A(X1_inter_AB_17, X2_inter_AB_17, 0, Y1_inter_AB_17, Y2_inter_AB_17, 0),
                                # for T outside circle   
                                     # with turning point and outiside of circle and both or at least 1 arms reaches in calcualte the triangle between interception linbes from interception point 1 and 2 of the line AT with the circle to the 
                                     # if T lies outside and both arms reach into the circle we have to calculate the triangle between AT_inter_1, AT_inter_2 and the centre of the circle on both sides, whereby we start with the AT line
                                     T_dist >  data_circle$r0[3] & e_form == "2" & inter_status_AT_17 == "two I" &  T_dist >  data_circle$r0[3] &  e_form == "2" & inter_status_BT_17 == "two I"  ~  triangle_A(X1_inter_AT_17, X2_inter_AT_17, 0, Y1_inter_AT_17, Y2_inter_AT_17, 0), # the triangle on the BT side follows in the next column
                                     # if T is outside the cirlce and only the AT arm intersects with the cirlce, we calcualte the triangle between centre and the intersections of A 
                                     T_dist >  data_circle$r0[3] & e_form == "2" & inter_status_AT_17 == "two I" &  T_dist >  data_circle$r0[3] &  e_form == "2" & inter_status_BT_17 != "two I"  ~  triangle_A(X1_inter_AT_17, X2_inter_AT_17, 0, Y1_inter_AT_17, Y2_inter_AT_17, 0),
                                     # if t lies outside and there´s no or just one intersection on each line we don´t need the angle cause all trees are inside the plot
                                     T_dist >  data_circle$r0[3] & e_form == "2" & inter_status_AT_17 != "two I" &  T_dist >  data_circle$r0[3] &  e_form == "2" & inter_status_BT_17 != "two I"  ~  NA, 
                                     # if T lies outside the circle and there is an intersection of the BT but not of the AT line, well calcualte the area of the triangle between BT_inter_1, BT_inter_2 and the centre of the plot in the next column
                                     T_dist >  data_circle$r0[3] & e_form == "2" & inter_status_AT_17 != "two I" &  T_dist >  data_circle$r0[3] &  e_form == "2" & inter_status_BT_17 == "two I"  ~  NA, # follows in next column
                                 # for T inside circle  
                                     # if t lies inside the circle and both arms reach out, calculate the triable between at point T where amd the correcft A and B intesctions
                                     T_dist <=  data_circle$r0[3] & e_form == "2" & inter_status_AT_17 == "two I" &  T_dist <=  data_circle$r0[3] &  e_form == "2" & inter_status_BT_17 == "two I"  ~  triangle_A(X_inter_AT_17_triangle, X_inter_BT_17_triangle, X_T, Y_inter_AT_17_triangle, Y_inter_BT_17_triangle, Y_T),
                                     # if t lies inside the circle and only arm AT arms reaches out/ has two intersections  calculate the triable between AT_inter_1, A_inter_2 and centre of the plot
                                     T_dist <=  data_circle$r0[3] & e_form == "2" & inter_status_AT_17 == "two I" &  T_dist <=  data_circle$r0[3] &  e_form == "2" & inter_status_BT_17 != "two I"  ~  triangle_A(X1_inter_AT_17, X2_inter_AT_17, 0, Y1_inter_AT_17, Y2_inter_AT_17, 0), 
                                     # if T lies inside the cirle and there are no intersections we dont calcualte anything
                                     T_dist <=  data_circle$r0[3] & e_form == "2" & inter_status_AT_17 != "two I" &  T_dist <=  data_circle$r0[3] &  e_form == "2" & inter_status_BT_17 != "two I"  ~  NA, 
                                     T_dist <=  data_circle$r0[3] & e_form == "2" & inter_status_AT_17 != "two I" &  T_dist <= data_circle$r0[3] &  e_form == "2" & inter_status_BT_17 == "two I"  ~  NA, # follows in next column
                                     TRUE ~ NA),
                                   # if T lies outside and both arms intersect, we need the triangle on the B side of the intersections between centre, B_inter1, B_inter_2
         triangle_ABC_BC_17_cm2 = case_when(T_dist >  data_circle$r0[3] & e_form == "2" & inter_status_AT_17 == "two I" &  T_dist >  data_circle$r0[3] &  e_form == "2" & inter_status_BT_17 == "two I"  ~  triangle_A(X1_inter_BT_17, X2_inter_BT_17, 0, Y1_inter_BT_17, Y2_inter_BT_17, 0), 
                                     # if T leis outside and only the BT side intesects the circle, we need to calculate the trianlge between the B intersections and the centre of the circle
                                     T_dist >  data_circle$r0[3] & e_form == "2" & inter_status_AT_17 != "two I" &  T_dist >  data_circle$r0[3] &  e_form == "2" & inter_status_BT_17 == "two I"  ~   triangle_A(X1_inter_BT_17, X2_inter_BT_17, 0, Y1_inter_BT_17, Y2_inter_BT_17, 0),
                                     # if t lies inside the cirlce and the only BT arm intersects the circle, we need a triangle between B1, B2 and the circle centre, 
                                     # if both arms intserct the circle, we already calcualted the triangle between ABT in the previous column
                                     T_dist <=  data_circle$r0[3] & e_form == "2" & inter_status_AT_17 != "two I" &  T_dist <=  data_circle$r0[3] &  e_form == "2" & inter_status_BT_17 == "two I"  ~  triangle_A(X1_inter_BT_17, X2_inter_BT_17, 0, Y1_inter_BT_17, Y2_inter_BT_17, 0), 
                                           TRUE ~ NA), 
         # calculatint the edge area:
                               # for 17m plot without trunign poinz
         edge_area_ABC_AC_17_cm2 = case_when(e_form == "1" & inter_status_AB_17 == "two I" ~ circle_segment_ABC_AC_17_cm2 - triangle_ABC_AC_17_cm2,
                                # for T outside circle
                                         # with turning point and outiside of circle and both or at least 1 arms reaches in calcualte the circle - triangle for both lines AT and BT whereby we start with AT  
                                          T_dist >  data_circle$r0[3] & e_form == "2" & inter_status_AT_17 == "two I" &  T_dist >  data_circle$r0[3] &  e_form == "2" & inter_status_BT_17 == "two I"  ~  circle_segment_ABC_AC_17_cm2 - triangle_ABC_AC_17_cm2, # BT side follows in next column
                                          T_dist >  data_circle$r0[3] & e_form == "2" & inter_status_AT_17 == "two I" &  T_dist >  data_circle$r0[3] &  e_form == "2" & inter_status_BT_17 != "two I"  ~  circle_segment_ABC_AC_17_cm2 - triangle_ABC_AC_17_cm2,
                                          # if t lies outside and there´s no or just one intersection on each line we don´t need the circle intersection cause all trees are inside the plot
                                          T_dist >  data_circle$r0[3] & e_form == "2" & inter_status_AT_17 != "two I" &  T_dist >  data_circle$r0[3] &  e_form == "2" & inter_status_BT_17 != "two I"  ~  NA, 
                                          T_dist >  data_circle$r0[3] & e_form == "2" & inter_status_AT_17 != "two I" &  T_dist >  data_circle$r0[3] &  e_form == "2" & inter_status_BT_17 == "two I"  ~  NA, # follows in next column
                                  # for T inside cirlce 
                                          # if t lies inside the circle and both arms reach out, calculate the area of the edge area is going to be the circle intersection of the lines AT and BT in point T 
                                          T_dist <=  data_circle$r0[3] & e_form == "2" & inter_status_AT_17 == "two I" &  T_dist <=  data_circle$r0[3] &  e_form == "2" & inter_status_BT_17 == "two I"  ~  circle_segment_ABC_AC_17_cm2,
                                          # if t lies inside the circle and both arm AT arms reaches out/ has two intersections  calculate the circle intersection between A inter 1 and A inter 2 and centre
                                          T_dist <=  data_circle$r0[3] & e_form == "2" & inter_status_AT_17 == "two I" &  T_dist <=  data_circle$r0[3] &  e_form == "2" & inter_status_BT_17 != "two I"  ~  circle_segment_ABC_AC_17_cm2 - triangle_ABC_AC_17_cm2, 
                                          # if T lies inside the cirlce but there are no intersections whatsoever, we don´t calcualte any areas
                                          T_dist <=  data_circle$r0[3] & e_form == "2" & inter_status_AT_17 != "two I" &  T_dist <=  data_circle$r0[3] &  e_form == "2" & inter_status_BT_17 != "two I"  ~  NA, 
                                          T_dist <=  data_circle$r0[3] & e_form == "2" & inter_status_AT_17 != "two I" &  T_dist <= data_circle$r0[3] &  e_form == "2" & inter_status_BT_17 == "two I"  ~  NA, # follows in next column
                                          TRUE ~ NA),
                             # for T outside circle 
                                           # if t lies outside and both arms intersect the circle or only the BT arm intersects the circle, we need a circle segment between B1, B2 and the circle centre
         edge_area_ABC_BC_17_cm2 = case_when(T_dist >  data_circle$r0[3] & e_form == "2" & inter_status_AT_17 == "two I" &  T_dist >  data_circle$r0[3] &  e_form == "2" & inter_status_BT_17 == "two I"  ~  circle_segment_ABC_BC_17_cm2 - triangle_ABC_BC_17_cm2,   
                                          T_dist >  data_circle$r0[3] & e_form == "2" & inter_status_AT_17 != "two I" &  T_dist >  data_circle$r0[3] &  e_form == "2" & inter_status_BT_17 == "two I"  ~  circle_segment_ABC_BC_17_cm2 - triangle_ABC_BC_17_cm2, 
                              # for T inside circle     
                                          # if t lies inside the cirlce and the only BT arm intersects the circle, we need a circle segment between B1, B2 and the circle centre, 
                                          # if t lise inside the cirlce and both arms intersepct the circle, we already calcualted the whole circle segment area of A-B-T in the previous column
                                          T_dist <=  data_circle$r0[3] & e_form == "2" & inter_status_AT_17 != "two I" &  T_dist <=  data_circle$r0[3] &  e_form == "2" & inter_status_BT_17 == "two I"  ~  circle_segment_ABC_BC_17_cm2 - triangle_ABC_BC_17_cm2, 
                                           TRUE ~ NA), 
         edge_area_ABC_AC_17_ha = (edge_area_ABC_AC_17_cm2/10000)/10000,   # transfor area in cm2 into area in ha /10000 for m2, /10000 for ha --> afterwards check if results are plausible 
         edge_area_ABC_BC_17_ha = (edge_area_ABC_BC_17_cm2/10000)/10000) %>% 
  # for 12m plot
  mutate(circle_segment_ABC_AC_12_cm2 = case_when(e_form == "1" & inter_status_AB_12 == "two I" ~ circle_seg_A(data_circle$r0[2], angle_ABT_ABC_AC),
                                                  # for T outside circle
                                                  # with turning point and outiside of circle and both or at least 1 arms reaches in calcualte the circle intersection between interception linbes from interception point 1 and 2 of the line AT with the circle to the 
                                                  T_dist >  data_circle$r0[2] & e_form == "2" & inter_status_AT_12 == "two I" &  T_dist >  data_circle$r0[2] &  e_form == "2" & inter_status_BT_12 == "two I"  ~  circle_seg_A(data_circle$r0[2], angle_ABT_ABC_AC), # BT side follows in next column
                                                  T_dist >  data_circle$r0[2] & e_form == "2" & inter_status_AT_12 == "two I" &  T_dist >  data_circle$r0[2] &  e_form == "2" & inter_status_BT_12 != "two I"  ~  circle_seg_A(data_circle$r0[2], angle_ABT_ABC_AC),
                                                  # if t lies outside and there´s no or just one intersection on each line we don´t need the circle intersection cause all trees are inside the plot
                                                  T_dist >  data_circle$r0[2] & e_form == "2" & inter_status_AT_12 != "two I" &  T_dist >  data_circle$r0[2] &  e_form == "2" & inter_status_BT_12 != "two I"  ~  NA, 
                                                  T_dist >  data_circle$r0[2] & e_form == "2" & inter_status_AT_12 != "two I" &  T_dist >  data_circle$r0[2] &  e_form == "2" & inter_status_BT_12 == "two I"  ~  NA, # follows in next column
                                                  # for T inside cirlce 
                                                  # if t lies inside the circle and both arms reach out, calculate the circle intersection between at point T where AT and BT meet 
                                                  T_dist <=  data_circle$r0[2] & e_form == "2" & inter_status_AT_12 == "two I" &  T_dist <=  data_circle$r0[2] &  e_form == "2" & inter_status_BT_12 == "two I"  ~  circle_seg_A(data_circle$r0[2], angle_ABT_ABC_AC),
                                                  # if t lies inside the circle and both arm AT arms reaches out/ has two intersections  calculate the circle intersection between A inter 1 and A inter 2 and centre
                                                  T_dist <=  data_circle$r0[2] & e_form == "2" & inter_status_AT_12 == "two I" &  T_dist <=  data_circle$r0[2] &  e_form == "2" & inter_status_BT_12 != "two I"  ~  circle_seg_A(data_circle$r0[2], angle_ABT_ABC_AC), 
                                                  # if T lies inside the cirlce but there are no intersections whatsoever, we don´t calcualte any areas
                                                  T_dist <=  data_circle$r0[2] & e_form == "2" & inter_status_AT_12 != "two I" &  T_dist <=  data_circle$r0[2] &  e_form == "2" & inter_status_BT_12 != "two I"  ~  NA, 
                                                  T_dist <=  data_circle$r0[2] & e_form == "2" & inter_status_AT_12 != "two I" &  T_dist <= data_circle$r0[2] &  e_form == "2" & inter_status_BT_12 == "two I"  ~  NA, # follows in next column
                                                  TRUE ~ NA),
         # for T outside circle 
         # if t lies outside and both arms intersect the circle or only the BT arm intersects the circle, we need a circle segment between B1, B2 and the circle centre
         circle_segment_ABC_BC_12_cm2 = case_when(T_dist >  data_circle$r0[2] & e_form == "2" & inter_status_AT_12 == "two I" &  T_dist >  data_circle$r0[2] &  e_form == "2" & inter_status_BT_12 == "two I"  ~  circle_seg_A(data_circle$r0[2], angle_ABT_ABC_BC), 
                                                  T_dist >  data_circle$r0[2] & e_form == "2" & inter_status_AT_12 != "two I" &  T_dist >  data_circle$r0[2] &  e_form == "2" & inter_status_BT_12 == "two I"  ~  circle_seg_A(data_circle$r0[2], angle_ABT_ABC_BC), 
                                                  # for T inside circle     
                                                  # if t lies inside the cirlce and the only BT arm intersects the circle, we need a circle segment between B1, B2 and the circle centre, 
                                                  # if t lise inside the cirlce and both arms intersepct the circle, we already calcualted the whoke circle segment area of A-B-T in the previous column
                                                  T_dist <=  data_circle$r0[2] & e_form == "2" & inter_status_AT_12 != "two I" &  T_dist <=  data_circle$r0[2] &  e_form == "2" & inter_status_BT_12 == "two I"  ~  circle_seg_A(data_circle$r0[2], angle_ABT_ABC_BC), 
                                                  TRUE ~ NA),
         # for edges without T
         triangle_ABC_AC_12_cm2 = case_when(e_form == "1" & inter_status_AB_12 == "two I" ~ triangle_A(X1_inter_AB_12, X2_inter_AB_12, 0, Y1_inter_AB_12, Y2_inter_AB_12, 0),
                                            # for T outside circle   
                                            # with turning point and outiside of circle and both or at least 1 arms reaches in calcualte the triangle between interception linbes from interception point 1 and 2 of the line AT with the circle to the 
                                            # if T lies outside and both arms reach into the circle we have to calculate the triangle between AT_inter_1, AT_inter_2 and the centre of the circle on both sides, whereby we start with the AT line
                                            T_dist >  data_circle$r0[2] & e_form == "2" & inter_status_AT_12 == "two I" &  T_dist >  data_circle$r0[2] &  e_form == "2" & inter_status_BT_12 == "two I"  ~  triangle_A(X1_inter_AT_12, X2_inter_AT_12, 0, Y1_inter_AT_12, Y2_inter_AT_12, 0), # the triangle on the BT side follows in the next column
                                            # if T is outside the cirlce and only the AT arm intersects with the cirlce, we calcualte the triangle between centre and the intersections of A 
                                            T_dist >  data_circle$r0[2] & e_form == "2" & inter_status_AT_12 == "two I" &  T_dist >  data_circle$r0[2] &  e_form == "2" & inter_status_BT_12 != "two I"  ~  triangle_A(X1_inter_AT_12, X2_inter_AT_12, 0, Y1_inter_AT_12, Y2_inter_AT_12, 0),
                                            # if t lies outside and there´s no or just one intersection on each line we don´t need the angle cause all trees are inside the plot
                                            T_dist >  data_circle$r0[2] & e_form == "2" & inter_status_AT_12 != "two I" &  T_dist >  data_circle$r0[2] &  e_form == "2" & inter_status_BT_12 != "two I"  ~  NA, 
                                            # if T lies outside the circle and there is an intersection of the BT but not of the AT line, well calcualte the area of the triangle between BT_inter_1, BT_inter_2 and the centre of the plot in the next column
                                            T_dist >  data_circle$r0[2] & e_form == "2" & inter_status_AT_12 != "two I" &  T_dist >  data_circle$r0[2] &  e_form == "2" & inter_status_BT_12 == "two I"  ~  NA, # follows in next column
                                            # for T inside circle  
                                            # if t lies inside the circle and both arms reach out, calculate the triable between at point T where amd the correcft A and B intesctions
                                            T_dist <=  data_circle$r0[2] & e_form == "2" & inter_status_AT_12 == "two I" &  T_dist <=  data_circle$r0[2] &  e_form == "2" & inter_status_BT_12 == "two I"  ~  triangle_A(X_inter_AT_12_triangle, X_inter_BT_12_triangle, X_T, Y_inter_AT_12_triangle, Y_inter_BT_12_triangle, Y_T),
                                            # if t lies inside the circle and only arm AT arms reaches out/ has two intersections  calculate the triable between AT_inter_1, A_inter_2 and centre of the plot
                                            T_dist <=  data_circle$r0[2] & e_form == "2" & inter_status_AT_12 == "two I" &  T_dist <=  data_circle$r0[2] &  e_form == "2" & inter_status_BT_12 != "two I"  ~  triangle_A(X1_inter_AT_12, X2_inter_AT_12, 0, Y1_inter_AT_12, Y2_inter_AT_12, 0), 
                                            # if T lies inside the cirle and there are no intersections we dont calcualte anything
                                            T_dist <=  data_circle$r0[2] & e_form == "2" & inter_status_AT_12 != "two I" &  T_dist <=  data_circle$r0[2] &  e_form == "2" & inter_status_BT_12 != "two I"  ~  NA, 
                                            T_dist <=  data_circle$r0[2] & e_form == "2" & inter_status_AT_12 != "two I" &  T_dist <= data_circle$r0[2] &  e_form == "2" & inter_status_BT_12 == "two I"  ~  NA, # follows in next column
                                            TRUE ~ NA),
         # if T lies outside and both arms intersect, we need the triangle on the B side of the intersections between centre, B_inter1, B_inter_2
         triangle_ABC_BC_12_cm2 = case_when(T_dist >  data_circle$r0[2] & e_form == "2" & inter_status_AT_12 == "two I" &  T_dist >  data_circle$r0[2] &  e_form == "2" & inter_status_BT_12 == "two I"  ~  triangle_A(X1_inter_BT_12, X2_inter_BT_12, 0, Y1_inter_BT_12, Y2_inter_BT_12, 0), 
                                            # if T leis outside and only the BT side intesects the circle, we need to calculate the trianlge between the B intersections and the centre of the circle
                                            T_dist >  data_circle$r0[2] & e_form == "2" & inter_status_AT_12 != "two I" &  T_dist >  data_circle$r0[2] &  e_form == "2" & inter_status_BT_12 == "two I"  ~   triangle_A(X1_inter_BT_12, X2_inter_BT_12, 0, Y1_inter_BT_12, Y2_inter_BT_12, 0),
                                            # if t lies inside the cirlce and the only BT arm intersects the circle, we need a triangle between B1, B2 and the circle centre, 
                                            # if both arms intserct the circle, we already calcualted the triangle between ABT in the previous column
                                            T_dist <=  data_circle$r0[2] & e_form == "2" & inter_status_AT_12 != "two I" &  T_dist <=  data_circle$r0[2] &  e_form == "2" & inter_status_BT_12 == "two I"  ~  triangle_A(X1_inter_BT_12, X2_inter_BT_12, 0, Y1_inter_BT_12, Y2_inter_BT_12, 0), 
                                            TRUE ~ NA), 
         # calculatint the edge area:
                                       # without trunign poinz
         edge_area_ABC_AC_12_cm2 = case_when(e_form == "1" & inter_status_AB_12 == "two I" ~ circle_segment_ABC_AC_12_cm2 - triangle_ABC_AC_12_cm2,
                                       # for T outside circle
                                             # with turning point and outiside of circle and both or at least 1 arms reaches in calcualte the circle - triangle for both lines AT and BT whereby we start with AT  
                                             T_dist >  data_circle$r0[2] & e_form == "2" & inter_status_AT_12 == "two I" &  T_dist >  data_circle$r0[2] &  e_form == "2" & inter_status_BT_12 == "two I"  ~  circle_segment_ABC_AC_12_cm2 - triangle_ABC_AC_12_cm2, # BT side follows in next column
                                             T_dist >  data_circle$r0[2] & e_form == "2" & inter_status_AT_12 == "two I" &  T_dist >  data_circle$r0[2] &  e_form == "2" & inter_status_BT_12 != "two I"  ~  circle_segment_ABC_AC_12_cm2 - triangle_ABC_AC_12_cm2,
                                             # if t lies outside and there´s no or just one intersection on each line we don´t need the circle intersection cause all trees are inside the plot
                                             T_dist >  data_circle$r0[2] & e_form == "2" & inter_status_AT_12 != "two I" &  T_dist >  data_circle$r0[2] &  e_form == "2" & inter_status_BT_12 != "two I"  ~  NA, 
                                             T_dist >  data_circle$r0[2] & e_form == "2" & inter_status_AT_12 != "two I" &  T_dist >  data_circle$r0[2] &  e_form == "2" & inter_status_BT_12 == "two I"  ~  NA, # follows in next column
                                       # for T inside cirlce 
                                             # if t lies inside the circle and both arms reach out, calculate the area of the edge area is going to be the circle intersection of the lines AT and BT in point T 
                                             T_dist <=  data_circle$r0[2] & e_form == "2" & inter_status_AT_12 == "two I" &  T_dist <=  data_circle$r0[2] &  e_form == "2" & inter_status_BT_12 == "two I"  ~  circle_segment_ABC_AC_12_cm2,
                                             # if t lies inside the circle and both arm AT arms reaches out/ has two intersections  calculate the circle intersection between A inter 1 and A inter 2 and centre
                                             T_dist <=  data_circle$r0[2] & e_form == "2" & inter_status_AT_12 == "two I" &  T_dist <=  data_circle$r0[2] &  e_form == "2" & inter_status_BT_12 != "two I"  ~  circle_segment_ABC_AC_12_cm2 - triangle_ABC_AC_12_cm2, 
                                             # if T lies inside the cirlce but there are no intersections whatsoever, we don´t calcualte any areas
                                             T_dist <=  data_circle$r0[2] & e_form == "2" & inter_status_AT_12 != "two I" &  T_dist <=  data_circle$r0[2] &  e_form == "2" & inter_status_BT_12 != "two I"  ~  NA, 
                                             T_dist <=  data_circle$r0[2] & e_form == "2" & inter_status_AT_12 != "two I" &  T_dist <= data_circle$r0[2] &  e_form == "2" & inter_status_BT_12 == "two I"  ~  NA, # follows in next column
                                             TRUE ~ NA),
                                      # for T outside circle 
                                            # if t lies outside and both arms intersect the circle or only the BT arm intersects the circle, we need a circle segment between B1, B2 and the circle centre
         edge_area_ABC_BC_12_cm2 = case_when(T_dist >  data_circle$r0[2] & e_form == "2" & inter_status_AT_12 == "two I" &  T_dist >  data_circle$r0[2] &  e_form == "2" & inter_status_BT_12 == "two I"  ~  circle_segment_ABC_BC_12_cm2 - triangle_ABC_BC_12_cm2,   
                                             T_dist >  data_circle$r0[2] & e_form == "2" & inter_status_AT_12 != "two I" &  T_dist >  data_circle$r0[2] &  e_form == "2" & inter_status_BT_12 == "two I"  ~  circle_segment_ABC_BC_12_cm2 - triangle_ABC_BC_12_cm2, 
                                      # for T inside circle     
                                             # if t lies inside the cirlce and the only BT arm intersects the circle, we need a circle segment between B1, B2 and the circle centre, 
                                             # if t lise inside the cirlce and both arms intersepct the circle, we already calcualted the whole circle segment area of A-B-T in the previous column
                                             T_dist <=  data_circle$r0[2] & e_form == "2" & inter_status_AT_12 != "two I" &  T_dist <=  data_circle$r0[2] &  e_form == "2" & inter_status_BT_12 == "two I"  ~  circle_segment_ABC_BC_12_cm2 - triangle_ABC_BC_12_cm2, 
                                             TRUE ~ NA), 
         edge_area_ABC_AC_12_ha = (edge_area_ABC_AC_12_cm2/10000)/10000,   # transfor area in cm2 into area in ha /10000 for m2, /10000 for ha --> afterwards check if results are plausible 
         edge_area_ABC_BC_12_ha = (edge_area_ABC_BC_12_cm2/10000)/10000) %>% 
  # for 5m plot
  mutate(circle_segment_ABC_AC_5_cm2 = case_when(e_form == "1" & inter_status_AB_5 == "two I" ~ circle_seg_A(data_circle$r0[1], angle_ABT_ABC_AC),
                                                  # for T outside circle
                                                  # with turning point and outiside of circle and both or at least 1 arms reaches in calcualte the circle intersection between interception linbes from interception point 1 and 2 of the line AT with the circle to the 
                                                  T_dist >  data_circle$r0[1] & e_form == "2" & inter_status_AT_5 == "two I" &  T_dist >  data_circle$r0[1] &  e_form == "2" & inter_status_BT_5 == "two I"  ~  circle_seg_A(data_circle$r0[1], angle_ABT_ABC_AC), # BT side follows in next column
                                                  T_dist >  data_circle$r0[1] & e_form == "2" & inter_status_AT_5 == "two I" &  T_dist >  data_circle$r0[1] &  e_form == "2" & inter_status_BT_5 != "two I"  ~  circle_seg_A(data_circle$r0[1], angle_ABT_ABC_AC),
                                                  # if t lies outside and there´s no or just one intersection on each line we don´t need the circle intersection cause all trees are inside the plot
                                                  T_dist >  data_circle$r0[1] & e_form == "2" & inter_status_AT_5 != "two I" &  T_dist >  data_circle$r0[1] &  e_form == "2" & inter_status_BT_5 != "two I"  ~  NA, 
                                                  T_dist >  data_circle$r0[1] & e_form == "2" & inter_status_AT_5 != "two I" &  T_dist >  data_circle$r0[1] &  e_form == "2" & inter_status_BT_5 == "two I"  ~  NA, # follows in next column
                                                  # for T inside cirlce 
                                                  # if t lies inside the circle and both arms reach out, calculate the circle intersection between at point T where AT and BT meet 
                                                  T_dist <=  data_circle$r0[1] & e_form == "2" & inter_status_AT_5 == "two I" &  T_dist <=  data_circle$r0[1] &  e_form == "2" & inter_status_BT_5 == "two I"  ~  circle_seg_A(data_circle$r0[1], angle_ABT_ABC_AC),
                                                  # if t lies inside the circle and both arm AT arms reaches out/ has two intersections  calculate the circle intersection between A inter 1 and A inter 2 and centre
                                                  T_dist <=  data_circle$r0[1] & e_form == "2" & inter_status_AT_5 == "two I" &  T_dist <=  data_circle$r0[1] &  e_form == "2" & inter_status_BT_5 != "two I"  ~  circle_seg_A(data_circle$r0[1], angle_ABT_ABC_AC), 
                                                  # if T lies inside the cirlce but there are no intersections whatsoever, we don´t calcualte any areas
                                                  T_dist <=  data_circle$r0[1] & e_form == "2" & inter_status_AT_5 != "two I" &  T_dist <=  data_circle$r0[1] &  e_form == "2" & inter_status_BT_5 != "two I"  ~  NA, 
                                                  T_dist <=  data_circle$r0[1] & e_form == "2" & inter_status_AT_5 != "two I" &  T_dist <= data_circle$r0[1] &  e_form == "2" & inter_status_BT_5 == "two I"  ~  NA, # follows in next column
                                                  TRUE ~ NA),
         # for T outside circle 
         # if t lies outside and both arms intersect the circle or only the BT arm intersects the circle, we need a circle segment between B1, B2 and the circle centre
         circle_segment_ABC_BC_5_cm2 = case_when(T_dist >  data_circle$r0[1] & e_form == "2" & inter_status_AT_5 == "two I" &  T_dist >  data_circle$r0[1] &  e_form == "2" & inter_status_BT_5 == "two I"  ~  circle_seg_A(data_circle$r0[1], angle_ABT_ABC_BC), 
                                                  T_dist >  data_circle$r0[1] & e_form == "2" & inter_status_AT_5 != "two I" &  T_dist >  data_circle$r0[1] &  e_form == "2" & inter_status_BT_5 == "two I"  ~  circle_seg_A(data_circle$r0[1], angle_ABT_ABC_BC), 
                                                  # for T inside circle     
                                                  # if t lies inside the cirlce and the only BT arm intersects the circle, we need a circle segment between B1, B2 and the circle centre, 
                                                  # if t lise inside the cirlce and both arms intersepct the circle, we already calcualted the whoke circle segment area of A-B-T in the previous column
                                                  T_dist <=  data_circle$r0[1] & e_form == "2" & inter_status_AT_5 != "two I" &  T_dist <=  data_circle$r0[1] &  e_form == "2" & inter_status_BT_5 == "two I"  ~  circle_seg_A(data_circle$r0[1], angle_ABT_ABC_BC), 
                                                  TRUE ~ NA),
         # for edges without T
         triangle_ABC_AC_5_cm2 = case_when(e_form == "1" & inter_status_AB_5 == "two I" ~ triangle_A(X1_inter_AB_5, X2_inter_AB_5, 0, Y1_inter_AB_5, Y2_inter_AB_5, 0),
                                            # for T outside circle   
                                            # with turning point and outiside of circle and both or at least 1 arms reaches in calcualte the triangle between interception linbes from interception point 1 and 2 of the line AT with the circle to the 
                                            # if T lies outside and both arms reach into the circle we have to calculate the triangle between AT_inter_1, AT_inter_2 and the centre of the circle on both sides, whereby we start with the AT line
                                            T_dist >  data_circle$r0[1] & e_form == "2" & inter_status_AT_5 == "two I" &  T_dist >  data_circle$r0[1] &  e_form == "2" & inter_status_BT_5 == "two I"  ~  triangle_A(X1_inter_AT_5, X2_inter_AT_5, 0, Y1_inter_AT_5, Y2_inter_AT_5, 0), # the triangle on the BT side follows in the next column
                                            # if T is outside the cirlce and only the AT arm intersects with the cirlce, we calcualte the triangle between centre and the intersections of A 
                                            T_dist >  data_circle$r0[1] & e_form == "2" & inter_status_AT_5 == "two I" &  T_dist >  data_circle$r0[1] &  e_form == "2" & inter_status_BT_5 != "two I"  ~  triangle_A(X1_inter_AT_5, X2_inter_AT_5, 0, Y1_inter_AT_5, Y2_inter_AT_5, 0),
                                            # if t lies outside and there´s no or just one intersection on each line we don´t need the angle cause all trees are inside the plot
                                            T_dist >  data_circle$r0[1] & e_form == "2" & inter_status_AT_5 != "two I" &  T_dist >  data_circle$r0[1] &  e_form == "2" & inter_status_BT_5 != "two I"  ~  NA, 
                                            # if T lies outside the circle and there is an intersection of the BT but not of the AT line, well calcualte the area of the triangle between BT_inter_1, BT_inter_2 and the centre of the plot in the next column
                                            T_dist >  data_circle$r0[1] & e_form == "2" & inter_status_AT_5 != "two I" &  T_dist >  data_circle$r0[1] &  e_form == "2" & inter_status_BT_5 == "two I"  ~  NA, # follows in next column
                                            # for T inside circle  
                                            # if t lies inside the circle and both arms reach out, calculate the triable between at point T where amd the correcft A and B intesctions
                                            T_dist <=  data_circle$r0[1] & e_form == "2" & inter_status_AT_5 == "two I" &  T_dist <=  data_circle$r0[1] &  e_form == "2" & inter_status_BT_5 == "two I"  ~  triangle_A(X_inter_AT_5_triangle, X_inter_BT_5_triangle, X_T, Y_inter_AT_5_triangle, Y_inter_BT_5_triangle, Y_T),
                                            # if t lies inside the circle and only arm AT arms reaches out/ has two intersections  calculate the triable between AT_inter_1, A_inter_2 and centre of the plot
                                            T_dist <=  data_circle$r0[1] & e_form == "2" & inter_status_AT_5 == "two I" &  T_dist <=  data_circle$r0[1] &  e_form == "2" & inter_status_BT_5 != "two I"  ~  triangle_A(X1_inter_AT_5, X2_inter_AT_5, 0, Y1_inter_AT_5, Y2_inter_AT_5, 0), 
                                            # if T lies inside the cirle and there are no intersections we dont calcualte anything
                                            T_dist <=  data_circle$r0[1] & e_form == "2" & inter_status_AT_5 != "two I" &  T_dist <=  data_circle$r0[1] &  e_form == "2" & inter_status_BT_5 != "two I"  ~  NA, 
                                            T_dist <=  data_circle$r0[1] & e_form == "2" & inter_status_AT_5 != "two I" &  T_dist <= data_circle$r0[1] &  e_form == "2" & inter_status_BT_5 == "two I"  ~  NA, # follows in next column
                                            TRUE ~ NA),
         # if T lies outside and both arms intersect, we need the triangle on the B side of the intersections between centre, B_inter1, B_inter_2
         triangle_ABC_BC_5_cm2 = case_when(T_dist >  data_circle$r0[1] & e_form == "2" & inter_status_AT_5 == "two I" &  T_dist >  data_circle$r0[1] &  e_form == "2" & inter_status_BT_5 == "two I"  ~  triangle_A(X1_inter_BT_5, X2_inter_BT_5, 0, Y1_inter_BT_5, Y2_inter_BT_5, 0), 
                                            # if T leis outside and only the BT side intesects the circle, we need to calculate the trianlge between the B intersections and the centre of the circle
                                            T_dist >  data_circle$r0[1] & e_form == "2" & inter_status_AT_5 != "two I" &  T_dist >  data_circle$r0[1] &  e_form == "2" & inter_status_BT_5 == "two I"  ~   triangle_A(X1_inter_BT_5, X2_inter_BT_5, 0, Y1_inter_BT_5, Y2_inter_BT_5, 0),
                                            # if t lies inside the cirlce and the only BT arm intersects the circle, we need a triangle between B1, B2 and the circle centre, 
                                            # if both arms intserct the circle, we already calcualted the triangle between ABT in the previous column
                                            T_dist <=  data_circle$r0[1] & e_form == "2" & inter_status_AT_5 != "two I" &  T_dist <=  data_circle$r0[1] &  e_form == "2" & inter_status_BT_5 == "two I"  ~  triangle_A(X1_inter_BT_5, X2_inter_BT_5, 0, Y1_inter_BT_5, Y2_inter_BT_5, 0), 
                                            TRUE ~ NA), 
         # calculatint the edge area:
         # without trunign poinz
         edge_area_ABC_AC_5_cm2 = case_when(e_form == "1" & inter_status_AB_5 == "two I" ~ circle_segment_ABC_AC_5_cm2 - triangle_ABC_AC_5_cm2,
                                             # for T outside circle
                                             # with turning point and outiside of circle and both or at least 1 arms reaches in calcualte the circle - triangle for both lines AT and BT whereby we start with AT  
                                             T_dist >  data_circle$r0[1] & e_form == "2" & inter_status_AT_5 == "two I" &  T_dist >  data_circle$r0[1] &  e_form == "2" & inter_status_BT_5 == "two I"  ~  circle_segment_ABC_AC_5_cm2 - triangle_ABC_AC_5_cm2, # BT side follows in next column
                                             T_dist >  data_circle$r0[1] & e_form == "2" & inter_status_AT_5 == "two I" &  T_dist >  data_circle$r0[1] &  e_form == "2" & inter_status_BT_5 != "two I"  ~  circle_segment_ABC_AC_5_cm2 - triangle_ABC_AC_5_cm2,
                                             # if t lies outside and there´s no or just one intersection on each line we don´t need the circle intersection cause all trees are inside the plot
                                             T_dist >  data_circle$r0[1] & e_form == "2" & inter_status_AT_5 != "two I" &  T_dist >  data_circle$r0[1] &  e_form == "2" & inter_status_BT_5 != "two I"  ~  NA, 
                                             T_dist >  data_circle$r0[1] & e_form == "2" & inter_status_AT_5 != "two I" &  T_dist >  data_circle$r0[1] &  e_form == "2" & inter_status_BT_5 == "two I"  ~  NA, # follows in next column
                                             # for T inside cirlce 
                                             # if t lies inside the circle and both arms reach out, calculate the area of the edge area is going to be the circle intersection of the lines AT and BT in point T 
                                             T_dist <=  data_circle$r0[1] & e_form == "2" & inter_status_AT_5 == "two I" &  T_dist <=  data_circle$r0[1] &  e_form == "2" & inter_status_BT_5 == "two I"  ~  circle_segment_ABC_AC_5_cm2,
                                             # if t lies inside the circle and both arm AT arms reaches out/ has two intersections  calculate the circle intersection between A inter 1 and A inter 2 and centre
                                             T_dist <=  data_circle$r0[1] & e_form == "2" & inter_status_AT_5 == "two I" &  T_dist <=  data_circle$r0[1] &  e_form == "2" & inter_status_BT_5 != "two I"  ~  circle_segment_ABC_AC_5_cm2 - triangle_ABC_AC_5_cm2, 
                                             # if T lies inside the cirlce but there are no intersections whatsoever, we don´t calcualte any areas
                                             T_dist <=  data_circle$r0[1] & e_form == "2" & inter_status_AT_5 != "two I" &  T_dist <=  data_circle$r0[1] &  e_form == "2" & inter_status_BT_5 != "two I"  ~  NA, 
                                             T_dist <=  data_circle$r0[1] & e_form == "2" & inter_status_AT_5 != "two I" &  T_dist <= data_circle$r0[1] &  e_form == "2" & inter_status_BT_5 == "two I"  ~  NA, # follows in next column
                                             TRUE ~ NA),
         # for T outside circle 
         # if t lies outside and both arms intersect the circle or only the BT arm intersects the circle, we need a circle segment between B1, B2 and the circle centre
         edge_area_ABC_BC_5_cm2 = case_when(T_dist >  data_circle$r0[1] & e_form == "2" & inter_status_AT_5 == "two I" &  T_dist >  data_circle$r0[1] &  e_form == "2" & inter_status_BT_5 == "two I"  ~  circle_segment_ABC_BC_5_cm2 - triangle_ABC_BC_5_cm2,   
                                             T_dist >  data_circle$r0[1] & e_form == "2" & inter_status_AT_5 != "two I" &  T_dist >  data_circle$r0[1] &  e_form == "2" & inter_status_BT_5 == "two I"  ~  circle_segment_ABC_BC_5_cm2 - triangle_ABC_BC_5_cm2, 
                                             # for T inside circle     
                                             # if t lies inside the cirlce and the only BT arm intersects the circle, we need a circle segment between B1, B2 and the circle centre, 
                                             # if t lise inside the cirlce and both arms intersepct the circle, we already calcualted the whole circle segment area of A-B-T in the previous column
                                             T_dist <=  data_circle$r0[1] & e_form == "2" & inter_status_AT_5 != "two I" &  T_dist <=  data_circle$r0[1] &  e_form == "2" & inter_status_BT_5 == "two I"  ~  circle_segment_ABC_BC_5_cm2 - triangle_ABC_BC_5_cm2, 
                                             TRUE ~ NA), 
         edge_area_ABC_AC_5_ha = (edge_area_ABC_AC_5_cm2/10000)/10000,   # transfor area in cm2 into area in ha /10000 for m2, /10000 for ha --> afterwards check if results are plausible 
         edge_area_ABC_BC_5_ha = (edge_area_ABC_BC_5_cm2/10000)/10000, 
         edge_area_total_5_ha = tot.edge.A(edge_area_ABC_AC_5_ha, edge_area_ABC_BC_5_ha), 
         edge_area_total_12_ha = tot.edge.A(edge_area_ABC_AC_12_ha, edge_area_ABC_BC_12_ha),
         edge_area_total_17_ha = tot.edge.A(edge_area_ABC_AC_17_ha, edge_area_ABC_BC_17_ha)) %>% 
  mutate(plot_A_17_ha = (c_A(data_circle$r0[3])/10000)/10000, 
         plot_A_12_ha = (c_A(data_circle$r0[2])/10000)/10000, 
         plot_A_5_ha = (c_A(data_circle$r0[1])/10000)/10000 )


# there will always occur the following error as for some lines there are no intersections, so the intersection function returns NaNs
    # In argument: `X_inter_AT_17_triangle = case_when(...)`.
    # Caused by warning in `sqrt()`:
    #   ! NaNs wurden erzeugt


#----1.1.2.4. tree-edge-status by combining tree and edge data ---------------------------------------
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
                     X1_inter_AB_17, X2_inter_AB_17, Y1_inter_AB_17, Y2_inter_AB_17, inter_status_AB_17, azi_C_AB_inter_1, azi_C_AB_inter_2, 
                     X1_inter_AT_17, X2_inter_AT_17, Y1_inter_AT_17, Y2_inter_AT_17, inter_status_AT_17,
                     X1_inter_BT_17, X2_inter_BT_17,  Y1_inter_BT_17, Y2_inter_BT_17, inter_status_BT_17,
                     X1_inter_AB_12, X2_inter_AB_12, Y1_inter_AB_12, Y2_inter_AB_12, inter_status_AB_12,
                     X1_inter_AT_12, X2_inter_AT_12, Y1_inter_AT_12, Y2_inter_AT_12, inter_status_AT_12,
                     X1_inter_BT_12, X2_inter_BT_12, Y1_inter_BT_12, Y2_inter_BT_12, inter_status_BT_12,
                     X1_inter_AB_5, X2_inter_AB_5, Y1_inter_AB_5, Y2_inter_AB_5, inter_status_AB_5, 
                     X1_inter_AT_5, X2_inter_AT_5, Y1_inter_AT_5, Y2_inter_AT_5, inter_status_AT_5, 
                     X1_inter_BT_5, X2_inter_BT_5, Y1_inter_BT_5, Y2_inter_BT_5, inter_status_BT_5,  
                     X_inter_AT_triangle_60, X_inter_BT_triangle_60, Y_inter_AT_triangle_60, Y_inter_BT_triangle_60, 
                     X_inter_AT_17_triangle, X_inter_BT_17_triangle, Y_inter_AT_17_triangle, Y_inter_BT_17_triangle,
                     X_inter_AT_12_triangle, X_inter_BT_12_triangle, Y_inter_AT_12_triangle, Y_inter_BT_12_triangle, 
                     X_inter_AT_5_triangle, X_inter_BT_5_triangle, Y_inter_AT_5_triangle, Y_inter_BT_5_triangle,
                     inter_status_AB_17, inter_status_AT_17, inter_status_BT_17, 
                     edge_area_ABC_AC_17_ha, edge_area_ABC_BC_17_ha, 
                     edge_area_ABC_AC_12_ha, edge_area_ABC_BC_12_ha, 
                     edge_area_ABC_AC_5_ha, edge_area_ABC_BC_5_ha, 
                     edge_area_total_17_ha, edge_area_total_12_ha, edge_area_total_5_ha) %>% 
              mutate(lower_azi_AB_inter = ifelse(azi_C_AB_inter_1 < azi_C_AB_inter_2, azi_C_AB_inter_1, azi_C_AB_inter_2), 
                     upper_azi_AB_inter = ifelse(azi_C_AB_inter_1 > azi_C_AB_inter_2, azi_C_AB_inter_1, azi_C_AB_inter_2), 
                     angle_AB_inter_17 = angle_triangle(0,0, X1_inter_AB_17, Y1_inter_AB_17, X2_inter_AB_17, Y2_inter_AB_17)), 
            by = c("plot_ID", "e_ID", "e_type", "e_form")) %>% 
  # calculate the Y of the edge for the x of the tree
  # new approach by Johanna Garthe
  # insert y and x of tree in implizite function of line function: 0 = a*x + b - y --> if result > 0 --> group 1, if result <0 --> group 2, if result = 0 --> group 0
  mutate(Y_AB_t = l(b0_AB, b1_AB, X_tree),    # calcualte y of function at the x of the tree 
         dist_y_Xtree = distance(X_tree, Y_AB_t, 0, 0),
         angle_AB_inter_1_tree =  angle_triangle(0, 0, X1_inter_AB_17, Y1_inter_AB_17, X_tree, Y_tree),
         azi_AB_inter_1_2 = azi_correction( X2_inter_AB_17, Y2_inter_AB_17, X1_inter_AB_17, Y1_inter_AB_17, azimut( X2_inter_AB_17, Y2_inter_AB_17, X1_inter_AB_17, Y1_inter_AB_17)),
         azi_AB_inter_1_tree = azi_correction(X_tree, Y_tree, X1_inter_AB_17, Y1_inter_AB_17, azimut(X_tree, Y_tree, X1_inter_AB_17, Y1_inter_AB_17)),
         Y_AB_t_implicit = b0_AB  + b1_AB *X_tree - Y_tree, 
         Y_AT_t_implicit = b0_AT + b1_AT *X_tree - Y_tree,
         Y_BT_t_implicit = b0_BT  + b1_BT *X_tree - Y_tree) %>%
  # assign a tree-edge-status that calls trees with a Y higher then the respective edge-functions Y
  # if edge form == 1 choose only those trees that lie within in the azimutes of the intercepts
  mutate(t_AB_status = ifelse(e_form == 1 & Y_AB_t_implicit == 0, "on line", 
                              ifelse(e_form == 1 & Y_AB_t_implicit > 0, "C", "D")),
         # new approach to localise points when edge is a line: 
         # https://math.stackexchange.com/questions/1577062/how-to-know-if-a-given-point-is-inside-a-2d-circles-segment
         # t_AB_status_test = ifelse(e_form == 1 &
         #                             Dist_cm <= 1784 & 
         #                             Dist_cm >= dist_y_Xtree & 
         #                             angle_AB_inter_1_tree <= angle_AB_inter_17, "C", "D"), 
         t_AB_status_test = ifelse(e_form == 1 &
                                     Dist_cm <= 1784 &
                                     Dist_cm >= dist_y_Xtree &
                                     (azi_C_AB_inter_1*azi_gon) >= 0 &
                                     (azi_AB_inter_1_2*azi_AB_inter_1_tree) <= 0 &
                                     (azi_C_AB_inter_2*azi_gon) <= 0, "C", "D"), 
         t_AT_status = ifelse(Y_AT_t_implicit == 0, "on line", ifelse(Y_AT_t_implicit > 0, "B", "A")), 
         t_BT_status = ifelse(Y_BT_t_implicit == 0, "on line", ifelse(Y_BT_t_implicit > 0, "B", "A")),
         t_ABT_status = case_when(inter_status_AT_17 == "two I" & inter_status_BT_17 == "two I" ~ p.in.triangle(X_inter_AT_triangle_60, X_inter_BT_triangle_60, X_T, Y_inter_AT_triangle_60, Y_inter_BT_triangle_60, Y_T, X_tree, Y_tree),
                                   # if only one arm of the triangle crosses the circle/ has two intersections withthe circle, use the respective arm as a line and assign tree status according to line procedure 
                                   inter_status_AT_17 != "two I" & inter_status_BT_17 == "two I" ~ t_BT_status, 
                                   inter_status_AT_17 == "two I" & inter_status_BT_17 != "two I" ~ t_AT_status,
                                   # if non of the arms touches the circle, assign all trees inside the circle to one group
                                   inter_status_AT_17 != "two I" & inter_status_BT_17 != "two I" ~ "A", 
                                   TRUE ~ NA), 
         # this combines the previous statements of the tree status  
          t_status_AB_ABT = case_when(e_form == "1" & Y_AB_t_implicit == 0 ~ "on line", 
                                      e_form == "1" & Y_AB_t_implicit > 0 ~ "C",
                                      e_form == "1" & Y_AB_t_implicit < 0 ~ "D",
                                      e_form == "2" & inter_status_AT_17 == "two I" & e_form == "2" & inter_status_BT_17 == "two I" ~ p.in.triangle(X_inter_AT_triangle_60, X_inter_BT_triangle_60, X_T, Y_inter_AT_triangle_60, Y_inter_BT_triangle_60, Y_T, X_tree, Y_tree),
                                      # if only one arm of the triangle crosses the circle/ has two intersections withthe circle, use the respective arm as a line and assign tree status according to line procedure 
                                      e_form == "2" & inter_status_AT_17 != "two I" & e_form == "2" & inter_status_BT_17 == "two I" ~ t_BT_status, 
                                      e_form == "2" & inter_status_AT_17 == "two I" & e_form == "2" & inter_status_BT_17 != "two I" ~ t_AT_status,
                                      # if non of the arms touches the circle, assign all trees inside the circle to one group
                                      e_form == "2" & inter_status_AT_17 != "two I" & e_form == "2" & inter_status_BT_17 != "two I" ~ "A", 
                                      TRUE ~ NA)) 
# assigning tree status according to frequency of the groups per plot
   # (1) identify if there is more then 1 tree status per plot (A vs. B, C vs. D)
   # (2) identify there are more then two identify the group that has includes the most trees
   # (3) assign the group "main" int the column m_s_status to the most frequent t_status_AB_ABT status per plot 
   # (4) assing the group "side" in the column m_s_status to the lest frequent group per plot
trees_and_edges <- trees_and_edges %>% 
  # this join holds a dataset that assignes "main" to those trees whose AB or ABT status is the most frequent at the respecitive plot and 
  # "side" to the status category that is least frequent per plot so we can split out trees in main and side stand
  left_join(., rbind( 
## for edge form = 1
  # main info forest edges form 1
  trees_and_edges %>% 
    filter(e_form == 1) %>% 
    group_by(plot_ID, t_AB_status) %>% 
    summarise(N = n()) %>%                    # count the status groups per plot 
    top_n(., 1) %>%                           # select the highest value per plot (to the status with the highest count)
    rename("t_status" = "t_AB_status") %>%    # give the t_AB_status with the highest count/ frequency the m_s_status == "main"
    mutate(m_s_status = "main_AB"), 
  # dataset with plots that have 2 categroeis of tree status
  trees_and_edges %>% 
    filter(e_form == 1) %>% 
    # this inner join allows to select only those plots that have 2 or more categories of tree status, so we can assign "side" to them 
    # but won't assign side to plots we´ve allready adssigned "main above, because they only have 1 enty which is the highest and the lowest in that case
    inner_join(., trees_and_edges %>% 
                 filter(e_form == 1) %>% 
                 select(plot_ID, t_AB_status) %>% 
                 distinct() %>%
                 group_by(plot_ID) %>%
                 summarise(N = n()) %>%  # summarize how many stati are there per plot
                 filter( N > 1) %>%      # filter those that have more then 1 tree status catagory per plot
                 select(plot_ID) %>% 
                 distinct(),    # pick only plot IDs of those plots that hacve more then 1 category
               by = "plot_ID") %>% 
    group_by(plot_ID, t_AB_status) %>%
    summarise(N = n()) %>%                # count the oobservations for the t_AB_stati 
    top_n(., -1) %>%                      # select the status group with the lowest frequency per plot
    rename("t_status" = "t_AB_status") %>% 
    mutate(m_s_status = "side_AB"),       # assign the category "side_AB" to the lest frequent tree status group per plot
## for forest egde form = 2
  trees_and_edges %>% 
    filter(e_form == 2) %>% 
    group_by(plot_ID, t_ABT_status) %>% 
    summarise(N = n()) %>%
    top_n(., 1) %>% 
    rename("t_status" = "t_ABT_status") %>% 
    mutate(m_s_status = "main_ABT"),
  # dataset with plots that have 2 categroeis of tree status
  trees_and_edges %>% 
    filter(e_form == 2) %>% 
    # this inner join allows to select only those plots that have 2 or more categories of tree status, so we can assign "side" to them 
    # but won't assign side to plots we´ve allready adssigned "main above, because they only have 1 enty which is the highest and the lowest in that case
    inner_join(., trees_and_edges %>% 
                 filter(e_form == 2) %>% 
                 select(plot_ID, t_ABT_status) %>% 
                 distinct() %>%
                 group_by(plot_ID) %>%
                 summarise(N = n()) %>%  # summarize how many stati are there per plot
                 filter( N > 1) %>%      # filter those that have more then 1 tree status catagory per plot
                 select(plot_ID) %>% 
                 distinct(),    # pick only plot IDs of those plots that have more then 1 status, so they require a side and a main plot
               by = "plot_ID") %>% 
    group_by(plot_ID, t_ABT_status) %>%
    summarise(N = n()) %>%
    top_n(., -1) %>%
    rename("t_status" = "t_ABT_status") %>% 
    mutate(m_s_status = "side_ABT") %>% 
  arrange(plot_ID) %>% 
  select(plot_ID, t_status, m_s_status)),            # closing rbind
by = c("plot_ID", "t_status_AB_ABT" = "t_status"))   # finisching left join  
    



# ---- 1.1.2.5. assigning plot area by according to diameter class (klubschwelle)  ---------------------------------------
trees_and_edges %>% 
  mutate(plot_A_ha = case_when(DBH_cm >= 7 & DBH_cm < 10 & is.na(e_form) ~ (c_A(data_circle$r0[1])/10000)/10000,
                               DBH_cm >= 10 & DBH_cm < 30 & is.na(e_form) ~ (c_A(data_circle$r0[2])/10000)/10000,
                               DBH_cm >= 30 & is.na(e_form) ~ (c_A(data_circle$r0[3])/10000)/10000,
                               # for trees that are in a circle that has an edge
                               # here i am facing a problem because i actually don´t know if the tree is positioned in the "edge part" or the "inner part" 
                               # as i assing the catagory mai/ side_stand to those trees who have a less frequent category, assuming that the category with the most 
                              # if the tree is from the inner circle, and the trees category is main, and there is an edge area calculated for that plot and circuit, 
                              # use the area of the inner cirlce minus the edge area
                               DBH_cm >= 7 & DBH_cm < 10 & !is.na(e_form) & 
                                startsWith(m_s_status, "main_") & !is.na(edge_area_ABC_AC_5_ha) |
                                DBH_cm >= 7 & DBH_cm < 10 & !is.na(e_form) & 
                                startsWith(m_s_status, "main_") & !is.na(edge_area_ABC_AC_5_ha) & !is.na(edge_area_ABC_BC_5_ha) ~ ((c_A(data_circle$r0[1])/10000)/10000) - edge_area_total_5_ha, 
                              DBH_cm >= 10 & DBH_cm < 30 & !is.na(e_form) & 
                                startsWith(m_s_status, "main_") & !is.na(edge_area_ABC_AC_12_ha) | !is.na(edge_area_ABC_BC_12_ha) ~ ((c_A(data_circle$r0[2])/10000)/10000) - edge_area_total_12_ha,
                              DBH_cm >= 30 & !is.na(e_form) & 
                                startsWith(m_s_status, "main_") & !is.na(edge_area_ABC_AC_17_ha) | !is.na(edge_area_ABC_BC_17_ha) ~ ((c_A(data_circle$r0[3])/10000)/10000) - edge_area_total_17_ha, 
                              TRUE ~ NA))
         

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
   geom_point(data =  trees_and_edges %>% 
                filter(e_form == "1") %>% 
                inner_join(.,   forest_edges_HBI.man %>% 
                             filter(e_form == "1" ) %>% 
                             group_by(plot_ID) %>% 
                             summarize(n = n()) %>% 
                             filter(n <= 1), 
                           by = "plot_ID"),
              aes(X_tree, Y_tree, colour =  t_AB_status_test))+
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
             aes(X_tree, Y_tree, colour = t_ABT_status))+
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
               aes(X_tree, Y_tree, colour = m_s_status))+
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
              aes(X_tree, Y_tree, colour = m_s_status))+
   theme_bw()+ 
   facet_wrap(~plot_ID) 





# ----- NOTES -------------------------------------------------------------

# ----- estiamting coefficients of lines through edges via forest managemer package  -------------------------------------------------------------------

# coefficients for forest edges 
forest_edges_HBI %>% 
  # calculate coordinates for all 
  mutate(X_A = ifelse(A_azi != "-2", x_coord(A_dist, A_azi), NA), # if the value is marked -2 its equal to an NA
         X_B = ifelse(B_azi != "-2", x_coord(B_dist, B_azi), NA),
         X_T = ifelse(T_azi != "-2", x_coord(T_dist, T_azi), NA), 
         Y_A = ifelse(A_azi != "-2", y_coord(A_dist, A_azi), NA), 
         Y_B = ifelse(B_azi != "-2", y_coord(B_dist, B_azi), NA), 
         Y_T = ifelse(T_azi != "-2", y_coord(T_dist, T_azi), NA))%>%
  group_by(plot_ID) %>% 
  left_join(., 
            # 1. a) dataset with coefficients of line going only trough A and B without Knickpunkt
            forest_edges_HBI %>% 
              # filter for forest edges that have a relevance for tree calculations and dont have a turning point
              # filter(e_form == "1" & e_type %in% c("1", "2", "3", "4")) %>%
              filter(e_form == "1"& A_azi != "-2" & B_azi != "-2" & T_azi == "-2") %>%
              # calculate coordinates from Azimut and distance
              mutate(X_A = ifelse(A_azi != "-2", x_coord(A_dist, A_azi), NA), # if the value is marked -2 its equal to an NA
                     X_B = ifelse(B_azi != "-2", x_coord(B_dist, B_azi), NA),
                     Y_A = ifelse(A_azi != "-2", y_coord(A_dist, A_azi), NA), 
                     Y_B = ifelse(B_azi != "-2", y_coord(B_dist, B_azi), NA))%>%
              # pivotingx and y to fit lm: https://stackoverflow.com/questions/70700654/pivot-longer-with-names-pattern-and-pairs-of-columns
              to_long(keys = c("X_name",  "Y_name"), 
                      values = c( "X_value", "Y_value"),  
                      names(.)[11:12], names(.)[13:14]) %>%
              group_by(plot_ID, e_form) %>%
              # https://quantifyinghealth.com/line-equation-from-2-points-in-r/
              lm_table(Y_value ~ X_value , output = "table") %>% 
              rename("e_b0_AB" = "b0") %>% 
              rename("e_b1_AB" = "b1") %>% 
              select(plot_ID,e_form, e_b0_AB, e_b1_AB), 
            by = c("plot_ID", "e_form")) %>% 
  left_join(., 
            # 1.b) for forest edge form 2 that has a tunring point
            left_join(
              # 1.b) 1. dataset with coefficients of line from A to T
              forest_edges_HBI %>% 
                # filter for forest edges that have a relevance for tree calculations 
                #filter(e_form != "1" & T_azi != "-2" & e_type %in% c("1", "2", "3", "4")) %>% 
                filter(A_azi != "-2" & B_azi != "-2" & T_azi != "-2") %>% 
                mutate(X_A =  ifelse(A_azi != "-2", x_coord(A_dist, A_azi), NA),
                       X_TA = ifelse(e_form != "1" & T_azi != "-2", x_coord(T_dist, T_azi), NA),
                       X_B =  ifelse(B_azi != "-2", x_coord(B_dist, B_azi), NA),
                       X_TB = ifelse(e_form != "1" & T_azi != "-2", x_coord(T_dist, T_azi), NA),
                       Y_A =  ifelse(A_azi != "-2", y_coord(A_dist, A_azi), NA),
                       Y_TA = ifelse(e_form != "1" & T_azi != "-2", y_coord(T_dist, T_azi), NA),
                       Y_B =  ifelse(B_azi != "-2", y_coord(B_dist, B_azi), NA),
                       Y_TB = ifelse(e_form != "1" & T_azi != "-2", y_coord(T_dist, T_azi), NA)) %>%
                # pivotingx and y to fit lm: https://stackoverflow.com/questions/70700654/pivot-longer-with-names-pattern-and-pairs-of-columns
                to_long(keys = c("X_A_T_name", "X_B_T_name", "Y_A_T_name", "Y_B_T_name"), 
                        values = c("X_A_T_value", "X_B_T_value", "Y_A_T_value", "Y_B_T_value"),  
                        names(.)[11:12], names(.)[13:14], names(.)[15:16], names(.)[17:18]) %>%
                group_by(plot_ID, e_form) %>%
                # https://quantifyinghealth.com/line-equation-from-2-points-in-r/
                lm_table(Y_A_T_value ~ na.omit(X_A_T_value), output = "table") %>% 
                select(plot_ID, e_form, b0, b1) %>% 
                rename("e_b0_AT" = "b0") %>% 
                rename("e_b1_AT" = "b1"), 
              # 1.b) 2. dataset with coefficients of line from B to T
              forest_edges_HBI %>% 
                # filter for forest edges that have a relevance for tree calculations 
                # filter(e_form != "1" & T_azi != "-2" & e_type %in% c("1", "2", "3", "4")) %>% 
                filter(A_azi != "-2" & B_azi != "-2" & T_azi != "-2") %>% 
                mutate(X_A = x_coord(A_dist, A_azi),
                       X_TA = ifelse(e_form != "1" & T_azi != "-2", x_coord(T_dist, T_azi), NA),
                       X_B = x_coord(B_dist, B_azi),
                       X_TB = ifelse(e_form != "1" & T_azi != "-2", x_coord(T_dist, T_azi), NA),
                       Y_A = y_coord(A_dist, A_azi),
                       Y_TA = ifelse(e_form != "1" & T_azi != "-2", y_coord(T_dist, T_azi), NA),
                       Y_B = y_coord(B_dist, B_azi),
                       Y_TB = ifelse(e_form != "1" & T_azi != "-2", y_coord(T_dist, T_azi), NA)) %>%
                # pivotingx and y to fit lm: https://stackoverflow.com/questions/70700654/pivot-longer-with-names-pattern-and-pairs-of-columns
                to_long(keys = c("X_A_T_name", "X_B_T_name", "Y_A_T_name", "Y_B_T_name"), 
                        values = c("X_A_T_value", "X_B_T_value", "Y_A_T_value", "Y_B_T_value"),  
                        names(.)[11:12], names(.)[13:14], names(.)[15:16], names(.)[17:18]) %>%
                group_by(plot_ID, e_form) %>%
                # https://quantifyinghealth.com/line-equation-from-2-points-in-r/
                lm_table(Y_B_T_value ~ na.omit(X_B_T_value), output = "table") %>% 
                select(plot_ID, e_form, b0, b1) %>% 
                rename("e_b0_BT" = "b0") %>% 
                rename("e_b1_BT" = "b1"), 
              by = c("plot_ID", "e_form")), 
            by = c("plot_ID", "e_form")) 


# ----- N. intersections of lines AB, AT, BT with all samlping cir --------
forest_edges_HBI %>% 
   filter(e_form %in% c("1", "2")) %>% 
   # find line parameters
   # 1. calculate x and y coordinates for all edge points
   mutate(X_A = ifelse(A_azi != "-2", x_coord(A_dist, A_azi), NA), # if the value is marked -2 its equal to an NA
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
   mutate(b0_AB = ifelse(e_form == "1", intercept(X_A, Y_A, b1_AB), NA), 
          b0_AT = ifelse(e_form == "2", intercept(X_T, Y_T, b1_AT), NA),
          b0_BT = ifelse(e_form == "2", intercept(X_T, Y_T, b1_BT), NA)) %>% 
   ### 17m circle --> used for tree status also   
   # find x coordinate of the interception between line and 17.84m circle: insert line equation in circle equation (function: intersection_C_lx1, intersection_lx1)
   # for AB line 
   mutate(X1_inter_AB_17 = intersection_c_lx1(b0_AB, b1_AB,  data_circle$y0[3], data_circle$x0[3], data_circle$r0[3]),
          X2_inter_AB_17 = intersection_c_lx2(b0_AB, b1_AB, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3]), 
          inter_status_AB_17 = intersection.status(X1_inter_AB_17, X2_inter_AB_17),
          # for AT line
          X1_inter_AT_17 =intersection_c_lx1(b0_AT, b1_AT, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3]),
          X2_inter_AT_17 =intersection_c_lx2(b0_AT, b1_AT, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3]), 
          inter_status_AT_17 = intersection.status(X1_inter_AT_17, X2_inter_AT_17),
          # for BT line
          X1_inter_BT_17 =intersection_c_lx1(b0_BT, b1_BT, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3]),
          X2_inter_BT_17 =intersection_c_lx2(b0_BT, b1_BT, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3]), 
          inter_status_BT_17 =  intersection.status(X1_inter_BT_17, X2_inter_BT_17)) %>%   
   # y intercept with 17m circle: insert x of intercept with circle in equation of line
   # AB line 
   mutate(Y1_inter_AB_17 = l(b0_AB, b1_AB, X1_inter_AB_17), 
          Y2_inter_AB_17 = l(b0_AB, b1_AB, X2_inter_AB_17), 
          # AT line 
          Y1_inter_AT_17 = l(b0_AT, b1_AT, X1_inter_AT_17), 
          Y2_inter_AT_17 = l(b0_AT, b1_AT, X2_inter_AT_17), 
          # BT line 
          Y1_inter_BT_17 = l(b0_BT, b1_BT, X1_inter_BT_17), 
          Y2_inter_BT_17 = l(b0_BT, b1_BT, X2_inter_BT_17)) %>%
   ### for 12m circle   
   # x coordinate interception between line and 12.68m circle: insert line equation in circle equation (function: intersection_C_lx1, intersection_lx1)
   # for AB line 
   mutate(X1_inter_AB_12 = intersection_c_lx1(b0_AB, b1_AB,  data_circle$y0[3], data_circle$x0[3], data_circle$r0[2]),
          X2_inter_AB_12 = intersection_c_lx2(b0_AB, b1_AB, data_circle$y0[3], data_circle$x0[3], data_circle$r0[2]), 
          inter_status_AB_12 = intersection.status(intersection_c_lx1(b0_AB, b1_AB,  data_circle$y0[3], data_circle$x0[3], data_circle$r0[2]),
                                                   intersection_c_lx2(b0_AB, b1_AB, data_circle$y0[3], data_circle$x0[3], data_circle$r0[2])),
          # for AT line
          X1_inter_AT_12 = intersection_c_lx1(b0_AT, b1_AT, data_circle$y0[3], data_circle$x0[3], data_circle$r0[2]),
          X2_inter_AT_12 = intersection_c_lx2(b0_AT, b1_AT, data_circle$y0[3], data_circle$x0[3], data_circle$r0[2]), 
          inter_status_AT_12 = intersection.status(intersection_c_lx1(b0_AT, b1_AT, data_circle$y0[3], data_circle$x0[3], data_circle$r0[2]),
                                                   intersection_c_lx2(b0_AT, b1_AT, data_circle$y0[3], data_circle$x0[3], data_circle$r0[2])),
          # for BT line
          X1_inter_BT_12 =intersection_c_lx1(b0_BT, b1_BT, data_circle$y0[3], data_circle$x0[3], data_circle$r0[2]),
          X2_inter_BT_12 =intersection_c_lx2(b0_BT, b1_BT, data_circle$y0[3], data_circle$x0[3], data_circle$r0[2]), 
          inter_status_BT_12 = ifelse(is.na(X1_inter_BT_12) & is.na(X2_inter_BT_12), " no I",            # if 0 solution
                                      ifelse(X1_inter_BT_12 == X2_inter_BT_12, "one I",                  # if 1 solution
                                             ifelse(X1_inter_BT_12 != X2_inter_BT_12, "two I"))) ) %>%   # if 2 solutions
   # y intercept with 12m circle: insert x of intercept with circle in equation of line
   # AB line 
   mutate(Y1_inter_AB_12 = l(b0_AB, b1_AB, X1_inter_AB_12), 
          Y2_inter_AB_12 = l(b0_AB, b1_AB, X2_inter_AB_12), 
          # AT line 
          Y1_inter_AT_12 = l(b0_AT, b1_AT, X1_inter_AT_12), 
          Y2_inter_AT_12 = l(b0_AT, b1_AT, X2_inter_AT_12), 
          # BT line 
          Y1_inter_BT_12 = l(b0_BT, b1_BT, X1_inter_BT_12), 
          Y2_inter_BT_12 = l(b0_BT, b1_BT, X2_inter_BT_12)) %>%
   ### for 5m circle   
   # x coordinate interception between line and 5.64 m circle: insert line equation in circle equation (function: intersection_C_lx1, intersection_lx1)
   # for AB line 
   mutate(X1_inter_AB_5 = intersection_c_lx1(b0_AB, b1_AB,  data_circle$y0[1], data_circle$x0[1], data_circle$r0[1]),
          X2_inter_AB_5 = intersection_c_lx2(b0_AB, b1_AB, data_circle$y0[1], data_circle$x0[1], data_circle$r0[1]), 
          inter_status_AB_5 = intersection.status(intersection_c_lx1(b0_AB, b1_AB,  data_circle$y0[1], data_circle$x0[1], data_circle$r0[1]), 
                                                  intersection_c_lx2(b0_AB, b1_AB, data_circle$y0[1], data_circle$x0[1], data_circle$r0[1])),
          # for AT line
          X1_inter_AT_5 =intersection_c_lx1(b0_AT, b1_AT, data_circle$y0[1], data_circle$x0[1], data_circle$r0[1]),
          X2_inter_AT_5 =intersection_c_lx2(b0_AT, b1_AT, data_circle$y0[1], data_circle$x0[1], data_circle$r0[1]), 
          inter_status_AT_5 = ifelse(is.na(X1_inter_AT_5) & is.na(X2_inter_AT_5), " no I",     # if 0 solutions
                                     ifelse(X1_inter_AT_5 == X2_inter_AT_5, "one I",           # if 1 solution
                                            ifelse(X1_inter_AT_5 != X2_inter_AT_5, "two I"))), # if 2 solutions
          # for BT line
          X1_inter_BT_5 =intersection_c_lx1(b0_BT, b1_BT, data_circle$y0[1], data_circle$x0[1], data_circle$r0[1]),
          X2_inter_BT_5 =intersection_c_lx2(b0_BT, b1_BT, data_circle$y0[1], data_circle$x0[1], data_circle$r0[1]), 
          inter_status_BT_5 = ifelse(is.na(X1_inter_BT_5) & is.na(X2_inter_BT_5), " no I",            # if 0 solution
                                     ifelse(X1_inter_BT_5 == X2_inter_BT_5, "one I",                  # if 1 solution
                                            ifelse(X1_inter_BT_5 != X2_inter_BT_5, "two I"))) ) %>%   # if 2 solutions
   # y intercept with 5m circle: insert x of intercept with circle in equation of line
   # AB line 
   mutate(Y1_inter_AB_5 = l(b0_AB, b1_AB, X1_inter_AB_5), 
          Y2_inter_AB_5 = l(b0_AB, b1_AB, X2_inter_AB_5), 
          # AT line 
          Y1_inter_AT_5 = l(b0_AT, b1_AT, X1_inter_AT_5), 
          Y2_inter_AT_5 = l(b0_AT, b1_AT, X2_inter_AT_5), 
          # BT line 
          Y1_inter_BT_5 = l(b0_BT, b1_BT, X1_inter_BT_5), 
          Y2_inter_BT_5 = l(b0_BT, b1_BT, X2_inter_BT_5)) %>%
   # distance interception centre --> to see if points are actually placed on the rim of the circle 
   mutate(inter_1_dist = distance(X1_inter_AB_17, Y1_inter_AB_17, 0, 0),     # this is just to control if the whole thing worked and 
          #  to calculate the triangles Barycentric coordinates we need 3 points: A, B, C = centre point
          # in case T lies within the circle, we want R to select A and B from the intersection with the circle.
          # Whereby we have to use a wider radius, to make sure that trees located the halfmoon of the circle cut by the triangle (Kreisbogen) are selected too. 
          # when t lies inside the circle (so both lines reach outside) ue only intersception point where direction between inter_AT and AT is equal choose this x, we need a buffer tho  
          # the following statement says:  if T lies within circle check if the slope of x_inter_1  or the slope of x_inter_2 is equal to the slope of AT,
          #                                choose the x which has the same slope (x_inter_1 or x_inter_2)as the second point on the line (A or B) 
          #                                but with a buffer of + 216, which is why it has to be newly calculated 
          # find the intercept of circle and line that prolonges the line between a and t or B and T
          # AT line 
          azi_T_A = azi_correction(X_A, Y_A, X_T, Y_T, azimut(X_A, Y_A, X_T, Y_T)),
          azi_T_AT_inter_1 = azi_correction(X1_inter_AT_17, Y1_inter_AT_17, X_T, Y_T, azimut(X1_inter_AT_17, Y1_inter_AT_17, X_T, Y_T)),
          azi_T_AT_inter_2 = azi_correction(X2_inter_AT_17, Y2_inter_AT_17, X_T, Y_T, azimut(X2_inter_AT_17, Y2_inter_AT_17, X_T, Y_T)),
          # BT line
          azi_T_B = azi_correction(X_B, Y_B, X_T, Y_T, azimut(X_B, Y_B, X_T, Y_T)),
          azi_T_BT_inter_1 = azi_correction(X1_inter_BT_17, Y1_inter_BT_17, X_T, Y_T, azimut(X1_inter_BT_17, Y1_inter_BT_17, X_T, Y_T)),
          azi_T_BT_inter_2 = azi_correction(X2_inter_BT_17, Y2_inter_BT_17, X_T, Y_T, azimut(X2_inter_BT_17, Y2_inter_BT_17, X_T, Y_T)),
          # for those turning points that lay outside the circle, select the intercetion point with the gratest distance to c and prolong it
          dist_T_AT_inter_1 = distance(X1_inter_AT_17, Y1_inter_AT_17, X_T, Y_T), 
          dist_T_AT_inter_2 = distance(X2_inter_AT_17, Y2_inter_AT_17, X_T, Y_T), 
          dist_T_BT_inter_1 = distance(X1_inter_BT_17, Y1_inter_BT_17, X_T, Y_T), 
          dist_T_BT_inter_2 = distance(X2_inter_BT_17, Y2_inter_BT_17, X_T, Y_T), 
          # if azimut T to A  identical to azimut T to intercept 1 A and circle use this intercept (inter_AT_1) for the triable, if azimut T to A identical to azimute T to intercept 2 between A and  circle use this intercept (inter_AT_2)
          X_inter_AT_triangle_60 = case_when(T_dist <= 1784 &  azi_T_AT_inter_1 == azi_T_A ~ intersection_c_lx1(b0_AT,b1_AT,0,0, data_circle$rmax[3]*2),
                                             T_dist <= 1784 & azi_T_AT_inter_2 == azi_T_A ~  intersection_c_lx2(b0_AT, b1_AT, 0, 0,  data_circle$rmax[3]*2),
                                             T_dist > 1784 & dist_T_AT_inter_1 > dist_T_AT_inter_2 ~ intersection_c_lx1(b0_AT,b1_AT,0,0, data_circle$rmax[3]*2), 
                                             T_dist > 1784 & dist_T_AT_inter_2 > dist_T_AT_inter_1 ~ intersection_c_lx2(b0_AT,b1_AT,0,0, data_circle$rmax[3]*2), 
                                             TRUE ~ NA ), 
          X_inter_BT_triangle_60 = case_when(T_dist <= 1784 & azi_T_BT_inter_1 == azi_T_B ~ intersection_c_lx1(b0_BT,b1_BT, 0, 0, data_circle$rmax[3]*2),
                                             T_dist <= 1784 & azi_T_BT_inter_2 == azi_T_B ~  intersection_c_lx2(b0_BT, b1_BT, 0, 0, data_circle$rmax[3]*2),
                                             T_dist > 1784 & dist_T_BT_inter_1 > dist_T_BT_inter_2 ~ intersection_c_lx1(b0_BT,b1_BT,0,0, data_circle$rmax[3]*2), 
                                             T_dist > 1784 & dist_T_BT_inter_2 > dist_T_BT_inter_1 ~ intersection_c_lx2(b0_BT,b1_BT,0,0, data_circle$rmax[3]*2), 
                                             TRUE ~ NA)) %>% 
   # calcualte y to the x that lie in the same direction then the second point on the line, if turning points lies witin circle and lines "reach out"
   mutate(Y_inter_AT_triangle_60 = l(b0_AT, b1_AT, X_inter_AT_triangle_60),  
          Y_inter_BT_triangle_60 = l(b0_BT, b1_BT, X_inter_BT_triangle_60)) 
 
 
