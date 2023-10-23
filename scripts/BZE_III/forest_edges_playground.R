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


source("C:/Users/gercken/Documents/TI_BZE_BE_analysis/scripts/BZE_III/testfunction.R")


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

intercept <- function(x1, y1, x2, y2){
  # resolve line function towards b0 after inserting known coordinates and slope
  # Y_A = b1_AB*X_A + b0_AB | (-b1_AB*X_A) 
  # Y_A - b1_AB*X_A = b0_AB 
  b1 = (y2 - y1)/(x2 - x1);
  b0 = y1 - b1*x1;
  return(b0)
}


# ----- 0.5.6.  line -------------------------------------------------
l <- function(b0, b1, x){
  y = b0 + b1*x;
  return(y)
}



# ----0.5.7. azimut -------------------------------------------------------
azi <- function(x2, y2, x1, y1){
  azi = atan((x2 - x1)/(y2 - y1));
  delta_x = x2 -x1 ;
  delta_y = y2-y1 ; 
  azi_corrected = ifelse(delta_x >= 0 & delta_y > 0 | delta_x > 0 & delta_y >= 0, azi,                    # first quadrant x + y+
                         ifelse(delta_x >= 0 & delta_y < 0 |delta_x > 0 & delta_y <= 0, azi+200,         # second quadrant x + y-
                                ifelse(delta_x <= 0 & delta_y < 0 |delta_x < 0 & delta_y <= 0,  azi+200,   # third quadrant x- y-
                                       ifelse(delta_x <= 0 & delta_y > 0 | delta_x < 0 & delta_y >= 0, azi+400, NA))));
  return(azi_corrected)
}


# ----- 0.5.8. distance between two points --------------------------------
distance <- function(x2, y2, x1, y1){
  d = sqrt(((y2 - y1)^2) + ((x2 - x1)^2));
  return(d)
}


# ----- 0.5.9. intersection circle line -----------------------------------

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


# intersection between line and circle via switch function 
intersection_line_circle <- function(l.b0, l.b1, c.y0, c.x0, c.r0, coordinate) {
  
  # quadratic formula
  # 0 = ((1 +l.df$e_b1_AB^2)/(1 +l.df$e_b1_AB^2))*X^2  -   ((2*c.df$x0 - 2*l.df$e_b1_AB*(l.df$e_b0_AB - c.df$y0))/(1 +l.df$e_b1_AB^2))*X   +     (c.df$x0^2 + (l.df$e_b0_AB - c.df$y0)^2 - c.df$r0^2)/(1 +l.df$e_b1_AB^2)
  
  # x1 = -(p/2)+(sqrt((p/2)^2-q))
  # x2 = -(p/2)-(sqrt((p/2)^2-q))
  
  # p = b so the number before x in quadratic formula
  # q = c so the number at the end of quadratic fomula
  
  p = ((2*c.x0) + (2*l.b1*(l.b0 - c.y0)))/(1 + l.b1^2);
  q = (c.x0^2 + (l.b0 - c.y0)^2 - c.r0^2)/(1 +l.b1^2);
  
  switch(coordinate, 
         x1 =  -(p/2) + sqrt( ((p*-1)/2)^2-q ),
         x2 =  -(p/2) - sqrt( ((p*-1)/2)^2-q ),
         y1 = l.b0 + l.b1*(-(p/2) + sqrt( ((p*-1)/2)^2-q )),
         y2 = l.b0 + l.b1*( -(p/2) - sqrt( ((p*-1)/2)^2-q ))
         )
}



# ----- 0.5.9.1. intersection status -----------------------------------------

intersection.status <- function(inter_x1, inter_x2) {
  i_status <-   ifelse(is.na(inter_x1) & is.na(inter_x2), " no I",      # if 0 solutions
                       ifelse(inter_x1 == inter_x2, "one I",            # if 1 solution
                              ifelse(inter_x1 != inter_x2, "two I")));
  return(i_status)
}


# ----- 0.5.10 identifying tree status  ---------------------------------------------------------
# ----- 0.5.10.1. edge type 1 = straigt line  ---------------------------------------------------------
# find the site of the line that has the  smaller half of the circle
# we need this for the tree status function

# this function has two steps: 
# 1. suggested by Johanna Garthe: sort trees with implicit function
    # insert y and x of tree in implizite function of line function: 0 = a*x + b - y --> if result > 0 --> group 1, if result <0 --> group 2, if result = 0 --> group 0
    # mutate(Y_AB_t = l(b0_AB, b1_AB, X_tree),    # calcualte y of function at the x of the tree 
    #    dist_y_Xtree = distance(X_tree, Y_AB_t, 0, 0),
    #    Y_AB_t_implicit = b0_AB  + b1_AB *X_tree - Y_tree, 
    #    Y_AT_t_implicit = b0_AT + b1_AT *X_tree - Y_tree,
    #    Y_BT_t_implicit = b0_BT  + b1_BT *X_tree - Y_tree) %>%

# 2. suggested by Alexandr Chepovskii: assign smaller side to "B" by identifyying the result of an implicit function of two points on ooposite sites of the line 
    # assign a tree-edge-status that calls trees with the same result as the implicit function of the middlepoint-center-line 
    # intersction point on the shorter side of the middlepoint center line
    #if there are two intersection and the Y inter status of 
    # middle.point.to.line is a function that determines if the result of an implicit function has to be positive or negative to be outside the line 
    # thus if the edge is a line with two intersection we asssign the 

p.site.line <- function(x1, x2, y1, y2, c.x0, c.y0, c.r0, l.b0, l.b1, x.tree, y.tree){
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
  # insert coordinates that are for sure on the smaller side of the two halves of the circle into the implicit equation: 
  Y_MC_implicit = l.b0  + l.b1 * X_inter_MC_shorter_side - Y_inter_MC_shorter_side;
  Y_implicit_status_M_line = ifelse(Y_MC_implicit >= 0, "positive",          # "y imlicit has to be positive too for tree to be on the "outside side", 
                                       # as the result of the implicit equation that contains the 
                                       # point that is for sure in the smaller cirlce segment, has a positive impllciti equation result", 
                                       "negative");          # "y imlicit has to be negative for tree to be outside", 
  Y_tree_implicit = l.b0  + l.b1 * x.tree - y.tree;
  Y_implicit_status_tree_line =  ifelse(i_status == "two I" & Y_implicit_status_M_line == "positive" & Y_tree_implicit >= 0 |
                                         i_status == "two I" & Y_implicit_status_M_line == "negative" &  Y_tree_implicit < 0 |
                                         i_status != "two I",  
                                       "A", "B"); # if the line is crossing the plot by two intersections and there 
  
  return(Y_implicit_status_tree_line)
}


# ----- 0.5.10.2. edge type 2 = line with turning point  --------------------------------------------------------------------------------------------------
# ----- 0.5.10.2.1. select intersection for triangle in correct direction -----------------------------------------------------------------------------------------
# select the intersection coordinates that have the same azimute as A to T and B to T
inter.for.triangle <- function(l.b0, l.b1, c.x0, c.y0, c.r0.inter, x, y, x.t, y.t, coordinate){
  # x and y are the corodinates of the point that we compare the azimute of the intersection to, so e.g X_A, Y_A
  # x.t and y.t are the coordiantes of the turning point, for edge form 1 these coordinates area always 0 | 0
  # l.b0 and l.b1 are the line functions parameters so b0_AB, b1_AB etc. 
  # c.r0.inter means the r0 at which i want to have the intersection, which is not necesarrily similar to the c.r0 that is used to locate T 
  
  # calcualte x coordinates of the possible intresections
  x1.inter <- intersection_line_circle (l.b0, l.b1, c.x0, c.y0, c.r0.inter, coordinate = "x1");
  x2.inter <- intersection_line_circle (l.b0, l.b1, c.x0, c.y0, c.r0.inter, coordinate = "x2");
  # calcualte y coordinates of the possible intresections: 
  y1.inter <- intersection_line_circle(l.b0, l.b1, c.x0, c.y0, c.r0.inter, coordinate = "y1");
  y2.inter <- intersection_line_circle(l.b0, l.b1, c.x0, c.y0, c.r0.inter, coordinate = "y2");
  
# azimut between intersection point 1 and the turning point
azi.inter.1.t <- azi(x1.inter, y1.inter, x.t, y.t);
azi.inter.2.t <- azi(x2.inter, y2.inter, x.t, y.t);
# azimut between the other opint on the line ant the trunign point
azi.point.t <- azi(x, y, x.t, y.t);
switch(coordinate,
       x = ifelse(azi.inter.1.t == azi.point.t,x1.inter,
            ifelse(azi.inter.2.t == azi.point.t, x2.inter , NA)), 
       y = ifelse(azi.inter.1.t == azi.point.t, y1.inter , 
                  ifelse(azi.inter.2.t == azi.point.t, y2.inter, NA)))
}

# ------ 0.5.10.2.2. check if point lays in triangle  -------------------------------------------------------------------------------------
# this link https://stackoverflow.com/questions/2049582/how-to-determine-if-a-point-is-in-a-2d-triangle led me to the following links: 
# http://totologic.blogspot.com/2014/01/accurate-point-in-triangle-test.html
# https://www.geogebra.org/m/c8DwbVTP
# https://en.wikipedia.org/wiki/Barycentric_coordinate_system

# this function identifies if a point is located inside the triangle drawn by the (1) turning point (T(xc|yc)), 
# (2) the intersection of the AT line with a 60m radius circle (inter_AT_60(xa|ya)) and 
# (3) the intersection of the BT line with a 60m radius circle (inter_BT_60(xb|yb))
p.in.triangle <- function(xa, xb, xc, ya, yb, yc, xp, yp){
  a = ((xp - xc)*(yb - yc) + (xc - xb)*(yp - yc)) / ((yb - yc)*(xa - xc) + (xc - xb)*(ya - yc));
  b = ((xp - xc)*(yc - ya) + (xa - xc)*(yp - yc)) / ((yb - yc)*(xa - xc) + (xc - xb)*(ya - yc));
  c = 1 - a - b;
  in.or.out = ifelse(0 <= a & a <= 1 & 0 <= b  & b <= 1 & 0 <= c & c <= 1, "B", "A");
  # B = out = point is inside triangle, so outside plot
  # A = in =  point is outside triangle, so inside plot
  return(in.or.out)
}

# ------ 0.5.10.2.3. check if point is located on the triangle site of a line -------------------------------------------------------------------------------------
# this function helps to sort trees if the edge type is a triangle, with only one arm crossing the sampling circuit
  # - it identifies two points that are for sure located on oposites sites of the line, created by the arm of the triangle, reaching into the circle
  # - this is carried out by drawing a straight line through the middle of the edge line and the center of the circuit and following identifiy the intersections between the circle and the line
  # - following the both points are tested regarding their position to the triangle
  # - the intersection point of the midddle-point-center-line which is located inside the triangle (p.in.triangle == "B") is inserted into the implicit function of the line of the arm that reaches into the circle
  # - this way we know which result the implicit function of the trees needs to have to allocate the tree to the triangle side of the line, and the not triangle side of the line
  # - this is necesarry because we can´t apply out susal procedure for line edges, where we just sort the trees into B if they are located in the smaller half of the circle and into A if they are located in the bigger half
  # - in case of a triangle shaped edge which affects the circle like a line shaped edge we have to find the side of the circle that reaches inside the triangle 

p.site.triangle <- function(x1, x2, y1, y2, c.x0, c.y0, c.r0,l.b0, l.b1, xa, xb, xc, ya, yb, yc, x.tree, y.tree){
  # x1| y1 and x2|y2 belong to the intersections of the line or two points on a line
  # c.x0, c.y0 are the center coordinates of the circle
  # c.r0 is the radius of the circle
  # l.b0, l.b1 are the parameters of the line we assign the edges for
  #  xa, xb, xc, ya, yb, yc are the coordinates of the triangle corners that were used to identiy the "out" / "B" trees
  
  # determine intersection status of the line
  i_status <-   ifelse(is.na(x1) & is.na(x2), " no I",      # if 0 solutions
                       ifelse(!is.na(x1) & !is.na(x2) & x1 == x2, "one I",            # if 1 solution
                              ifelse(x1 != x2, "two I")));      # so if the edge for is 1 and there are 2 interseections of the line with the respective circle 
  
  # calculate coordiantes of the middle of thie line between intersection 1 and 2
  x_m_line = (x1 - x2)/2;
  y_m_line = (y1 - y2)/2;
  # calculate the parameters of the equation between the middle of the line and the centre of the circle
  b1_MC = slope(c.x0, c.y0, x_m_line, y_m_line);
  b0_MC = intercept(c.x0, c.y0,  x_m_line, y_m_line);
##### i stopped here #####
  # calcualte the x corrdiante of the interception of the line between M and the centre of the cirle and the circle at the given radio
  X1_inter_MC = intersection_line_circle(b0_MC, b1_MC, c.x0, c.y0, c.r0, coordinate = "x1"); 
  X2_inter_MC = intersection_line_circle(b0_MC, b1_MC, c.x0, c.y0, c.r0, coordinate = "x2");
  # insert the intersection x corodinate in the line function to get the respective y coordinate
  y1_inter_MC = intersection_line_circle(b0_MC, b1_MC, c.x0, c.y0, c.r0, coordinate = "y1");
  y2_inter_MC = intersection_line_circle(b0_MC, b1_MC, c.x0, c.y0, c.r0, coordinate = "y2");
  
  # finde the x coordiante of the intersection that is within the 60m triangle (p.in.tr)
  X_inter_MC_inside_triangle = ifelse(p.in.triangle(xa, xb, xc, ya, yb, yc, X1_inter_MC, y1_inter_MC) == "B", X1_inter_MC, X2_inter_MC); 
  # calcualte the y coordinate associated to the intersection in the triangle
  Y_inter_MC_inside_triangle = l.b0 + l.b1*X_inter_MC_inside_triangle;
  
  # check the result of the impliyit funtion for the MC intersection inside the triangle: 
  # " if we sort the trees by the implicit function, which value/ categrory must they have to be inside the triangle"
  Y_MC_implicit = l.b0 + l.b1*X_inter_MC_inside_triangle - Y_inter_MC_inside_triangle;
  Y_MC_implicit_status =  ifelse(Y_MC_implicit >= 0,  "positive",          # "y imlicit has to be positive too for point to be inside the triangle, 
                                                                             # as the result of the implicit equation that contains the  point that is for sure in the triangle has a positive impllciti equation result", 
                                    "negative") ;         # "y imlicit has to be negative for point to be inside the triangle
  
  # obtain the result if thee respective trees implicit equation
   Y_tree_implicit = l.b0 + l.b1*x.tree - y.tree;
   
   # if the trees implicit result is the same as the one of the implicit function of the intersection that is surely in the triangle, return B, else A
   Y_tree_implicit_status =  ifelse(i_status == "two I" & Y_MC_implicit_status == "positive" & Y_tree_implicit > 0 |
                                     i_status == "two I" & Y_MC_implicit_status == "negative" &  Y_tree_implicit < 0,
                                    "B", "A");
 
  return(Y_tree_implicit_status) 
}


# ----- 0.5.10.3. final treee edge status for all edge types -----------------------------------------------------------
# combining tree status assesment in 1 statement
tree.status <- function(
  # intersection status
  edge_form,
  # finding smaller side of cirlce
  c.x0, c.y0, c.r017, l.AB.b0, l.AB.b1, # x1, x2, y1, y2 are the intersection cooridnates of the line 
  # implicit funtion with tree coordinates
  x.tree, y.tree, 
  # select the intersection coordinates for the triangle on AT line
  x.a, y.a, x.t, y.t ,l.AT.b0, l.AT.b1, c.r060, #c.x0, c.y0,  
  x.b, y.b, l.BT.b0, l.BT.b1 ){ #x.t, y.t , # c.x0, c.y0, c.r060
  
  # calculate intersections with circle
  # AB line
  x1.inter.AB <- intersection_line_circle(l.AB.b0, l.AB.b1, c.y0, c.x0, c.r017, coordinate = "x1");
  y1.inter.AB <- intersection_line_circle(l.AB.b0, l.AB.b1, c.y0, c.x0, c.r017, coordinate = "y1");
  x2.inter.AB <- intersection_line_circle(l.AB.b0, l.AB.b1, c.y0, c.x0, c.r017, coordinate = "x2");
  y2.inter.AB <- intersection_line_circle(l.AB.b0, l.AB.b1, c.y0, c.x0, c.r017, coordinate = "y2");
  # AT line
  x1.inter.AT <- intersection_line_circle(l.AT.b0, l.AT.b1, c.y0, c.x0, c.r017, coordinate = "x1");
  y1.inter.AT <- intersection_line_circle(l.AT.b0, l.AT.b1, c.y0, c.x0, c.r017, coordinate = "y1");
  x2.inter.AT <- intersection_line_circle(l.AT.b0, l.AT.b1, c.y0, c.x0, c.r017, coordinate = "x2");
  y2.inter.AT <- intersection_line_circle(l.AT.b0, l.AT.b1, c.y0, c.x0, c.r017, coordinate = "y2");
  # BT line
  x1.inter.BT <- intersection_line_circle(l.BT.b0, l.BT.b1, c.y0, c.x0, c.r017, coordinate = "x1");
  y1.inter.BT <- intersection_line_circle(l.BT.b0, l.BT.b1, c.y0, c.x0, c.r017, coordinate = "y1");
  x2.inter.BT <- intersection_line_circle(l.BT.b0, l.BT.b1, c.y0, c.x0, c.r017, coordinate = "x2");
  y2.inter.BT <- intersection_line_circle(l.BT.b0, l.BT.b1, c.y0, c.x0, c.r017, coordinate = "y2");
  
  
  # check out the intersection status of the respective plot of the  tree
  i_status.AB <-   ifelse(is.na(x1.inter.AB) & is.na(x2.inter.AB), " no I",      # if 0 solutions
                          ifelse(!is.na(x1.inter.AB) & !is.na(x2.inter.AB) & x1.inter.AB == x2.inter.AB, "one I",            # if 1 solution
                                 ifelse(x1.inter.AB != x2.inter.AB, "two I")));      # so if the edge for is 1 and there are 2 interseections of the line with the respective circle 
  i_status.AT <-   ifelse(is.na(x1.inter.AT) & is.na(x2.inter.AT), " no I",      # if 0 solutions
                          ifelse(!is.na(x1.inter.AT) & !is.na(x2.inter.AT) & x1.inter.AT == x2.inter.AT, "one I",            # if 1 solution
                                 ifelse(x1.inter.AT != x2.inter.AT, "two I")));      # so if the edge for is 1 and there are 2 interseections of the line with the respective circle 
  
  i_status.BT <-   ifelse(is.na(x1.inter.BT) & is.na(x2.inter.BT), " no I",      # if 0 solutions
                          ifelse(!is.na(x1.inter.BT) & !is.na(x2.inter.BT) & x1.inter.BT == x2.inter.BT, "one I",            # if 1 solution
                                 ifelse(x1.inter.BT != x2.inter.BT, "two I")));      # so if the edge for is 1 and there are 2 interseections of the line with the respective circle 
  ## for edge form 2 --> triangle
  # find intersection on the "right" side (with the same "direction as the line from T to the respective point) 
  # and calcualte the respective intersection coordinate (x1 vs. x2) for a cricle with a radius = 60m
  # AT
  # select the intersection coordinates for the triangle on AT line
  x.AT.inter.triangle.60 = inter.for.triangle(l.AT.b0, l.AT.b1,  c.x0, c.y0, c.r060, x.a, y.a, x.t, y.t, coordinate = "x" );
  # calculate y for AT triangle
  y.AT.inter.triangle.60 = inter.for.triangle(l.AT.b0, l.AT.b1,  c.x0, c.y0, c.r060, x.a, y.a, x.t, y.t, coordinate = "y" );
  #BT 
  # select the intersection coordinates for the triangle on BT line
  x.BT.inter.triangle.60 = inter.for.triangle(l.BT.b0, l.BT.b1,  c.x0, c.y0, c.r060, x.b, y.b, x.t, y.t, coordinate = "x" );
  # calculate y for BT triangle
  y.BT.inter.triangle.60 = inter.for.triangle(l.BT.b0, l.BT.b1,  c.x0, c.y0, c.r060, x.b, y.b, x.t, y.t, coordinate = "y" );
  
  
  ## For edge form == 1 & for edge form == 2when there are only 2 intersections instead of 4  --> straight line 
  # assigne tree status depending on shorter/ longer side of circle for edge for 1
  # assigning tree status depending on inside/ outside triangle for edge side 2 
  tree_status_AB_line = ifelse(edge_form == "1", p.site.line(x1.inter.AB, x2.inter.AB, y1.inter.AB, y2.inter.AB, c.x0, c.y0, c.r017, l.AB.b0, l.AB.b1, x.tree, y.tree),NA);
  tree_status_AT_line = p.site.triangle(x1.inter.AT, x2.inter.AT, y1.inter.AT, y2.inter.AT,
                                        c.x0, c.y0, c.r017,
                                        l.AT.b0, l.AT.b1,
                                        x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, 
                                        y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t,
                                        x.tree, y.tree);
  tree_status_BT_line = p.site.triangle(x1.inter.BT, x2.inter.BT, y1.inter.BT, y2.inter.BT,
                                        c.x0, c.y0, c.r017,
                                        l.BT.b0, l.BT.b1,
                                        x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, 
                                        y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t,
                                        x.tree, y.tree);
  ## for edge form 2
  # check if tree is located inside triangle: Beyericentrig triangle function
  tree_stat_triangle = p.in.triangle(x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t, x.tree, y.tree);
  # B = out = point is inside triangle, so outside plot
  # A = in =  point is outside triangle, so inside plot
  
  tree_status = ifelse(edge_form == "2" & i_status.AT == "two I" & i_status.BT == "two I", tree_stat_triangle,
                       # if only one arm of the triangle crosses the circle/ has two intersections with the circle, use the respective arm as a line and assign tree status according to line procedure 
                       ifelse(edge_form == "2" & i_status.AT != "two I" & i_status.BT == "two I", tree_status_BT_line, 
                              ifelse(edge_form == "2" & i_status.AT == "two I" & i_status.BT != "two I", tree_status_AT_line,
                                     # for edge form 1 the selection of the tree status according to the inter status is carried out within the p.site.line function so it doest have to happen here
                                     ifelse(edge_form == "1", tree_status_AB_line,
                                            # if non of the arms or lines touches the circle, assign all trees inside the circle to one group
                                            ifelse(edge_form == "2" & i_status.AT != "two I" & i_status.BT != "two I", "A", NA)))));
  return(tree_status)
}


# ----- 0.5.11. edge area -------------------------------------------------------------------------------------------------------

# ----- 0.5.11.1. assign correct area to trees according to their category (A/B)  ----------
# to select which area we have assign to the edge and which we have to asssign to the main stand
# we have to find out on which side of the line the "B" and on which side the "A" trees are located
# as we know if the result of the implicit function has to be positive or negative for the tree to lie
# outside the plot, we can calcualte the intersections of a line through the center of the edge line 
# and the center of the plot. 
# Following we check which of the intersections is element of the triangle or if the result of the implicit function 
# of the intersection comlies with the result the implicitf function nee to have for atree to be outside (middle.point.to.line)

# this function should eable us to skip the part where we have to calcualte the intersections etc. for each circle and line
# this way we´ll just return the area per sampling circle 

edge.A <- function(e.form, dbh.cm, x.a, x.b, x.t, y.a, y.b, y.t, t.dist, tree_status){
  # x1| y1 and x2|y2 belong to the intersections of the line or two points on a line
  # c.x0, c.y0 are the center coordinates of the circle
  # c.r0 is the radius of the circle
  # l.b0, l.b1 are the parameters of the line we assign the edges for
  #  xa, xb, xc, ya, yb, yc are the coordinates of the triangle corners that were used to identiy the "out" / "B" trees
  # c.seg.a means the area of the cirle segment (circle bow) or the circle segmetns per CCS, c.a means the area if the whole circle
  
  # select the diameter of the circle depending on the trees diameter
  c.x0 = 0;
  c.y0 = 0; 
  c.r0 = ifelse(dbh.cm >= 7 & dbh.cm < 10, 564, 
                ifelse(dbh.cm >= 10 & dbh.cm < 30,  1262, 
                       ifelse(dbh.cm >= 30, 1784, NA)))
  
  
  ## calcualte slope and intercept of AT and BT line to calcualte intersections
  l.AB.b0 = ifelse(e.form == "1", intercept(x.a, y.a, x.b, y.b), NA);
  l.AB.b1 = ifelse(e.form == "1", slope(x.a, y.a, x.b, y.b), NA);
  l.AT.b0 = ifelse(e.form == "2", intercept(x.t, y.t, x.a, y.a), NA);
  l.AT.b1 = ifelse(e.form == "2", slope(x.t, y.t, x.a, y.a), NA);
  l.BT.b0 = ifelse(e.form == "2", intercept(x.t, y.t, x.b, y.b), NA);
  l.BT.b1 = ifelse(e.form == "2", slope(x.t, y.t, x.b, y.b), NA);
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
  x.AT.inter.triangle.60 = inter.for.triangle(l.AT.b0, l.AT.b1,  c.x0, c.y0, 6000, x.a, y.a, x.t, y.t, coordinate = "x" );
  # calculate y for AT triangle
  y.AT.inter.triangle.60 = inter.for.triangle(l.AT.b0, l.AT.b1,  c.x0, c.y0, 6000, x.a, y.a, x.t, y.t, coordinate = "y" );
  #BT 
  # select the intersection coordinates for the triangle on BT line
  x.BT.inter.triangle.60 = inter.for.triangle(l.BT.b0, l.BT.b1,  c.x0, c.y0, 6000, x.b, y.b, x.t, y.t, coordinate = "x" );
  # calculate y for BT triangle
  y.BT.inter.triangle.60 = inter.for.triangle(l.BT.b0, l.BT.b1,  c.x0, c.y0, 6000, x.b, y.b, x.t, y.t, coordinate = "y" );
  
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
                            ifelse(e.form == "2" & t.dist <= c.r0 & i_status.AT == "two I" & i_status.BT == "two I", inter.for.triangle(l.AT.b0, l.AT.b1,  c.x0, c.y0, c.r0, x.a, y.a, x.t, y.t, coordinate = "x" ), 
                                   ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I", x1.inter.AT, 
                                          NA)))));
  x2 = ifelse(e.form == "1" & i_status.AB == "two I", x2.inter.AB, 
              ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT != "two I", x2.inter.AT, 
                     ifelse(e.form == "2" & t.dist > c.r0 & i_status.BT == "two I" & i_status.AT != "two I", x2.inter.BT,
                            ifelse(e.form == "2" & t.dist <= c.r0 & i_status.AT == "two I" & i_status.BT == "two I", inter.for.triangle(l.BT.b0, l.BT.b1,  c.x0, c.y0, c.r0, x.b, y.b, x.t, y.t, coordinate = "x" ), 
                                   ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I", x2.inter.AT, 
                                          NA)))));
  y1 = ifelse(e.form == "1" & i_status.AB == "two I", x1.inter.AB, 
              ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT != "two I", y1.inter.AT, 
                     ifelse(e.form == "2" & t.dist > c.r0 & i_status.BT == "two I" & i_status.AT != "two I", y1.inter.BT,
                            ifelse(e.form == "2" & t.dist <= c.r0 & i_status.AT == "two I" & i_status.BT == "two I", inter.for.triangle(l.AT.b0, l.AT.b1,  c.x0, c.y0, c.r0, x.a, y.a, x.t, y.t, coordinate = "y" ), 
                                   ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I", y1.inter.AT, 
                                          NA)))));
  y2 = ifelse(e.form == "1" & i_status.AB == "two I", y2.inter.AB, 
              ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT != "two I", y2.inter.AT, 
                     ifelse(e.form == "2" & t.dist > c.r0 & i_status.BT == "two I" & i_status.AT != "two I", y2.inter.BT,
                            ifelse(e.form == "2" & t.dist <= c.r0 & i_status.AT == "two I" & i_status.BT == "two I", inter.for.triangle(l.BT.b0, l.BT.b1,  c.x0, c.y0, c.r0, x.b, y.b, x.t, y.t, coordinate = "y" ), 
                                   ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I", y2.inter.AT, 
                                          NA)))));
  # create another intersection pair for circles that are intersected by both arms of the triangle  
  x.1.bsite = ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I", x1.inter.BT, NA);
  x.2.bsite = ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I", x2.inter.BT, NA);
  y.1.bsite = ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I", x1.inter.BT, NA);
  y.2.bsite = ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I", x2.inter.BT, NA);
  
  # circle segment on AB or AT or BT side
  # calculate angle between the lines from sampling cirlce intersections to center
  azi_inter1_center = azi(x1, y1, c.x0, c.y0);
  azi_inter2_center = azi(x2, y2, c.x0, c.y0);
  # to not receive negative angles, we have to deduct azi of intersection 1 from azi of intersection 2 and multiply it ith -1 in case it´s negative
  azi_between_IC_lines = ifelse((azi_inter1_center-azi_inter2_center)<0, (azi_inter1_center-azi_inter2_center)*(-1), azi_inter1_center-azi_inter2_center);
  # calcualte circle segment area: 
  c.cone.A = (pi*c.r0^2) * azi_between_IC_lines/400;  # /400 because our azi (gon)is in gon not degrees 
  # calcualte circle area
  c.A = pi*c.r0^2
  # calculate area of triangle between the intersections with the sampling circle and the center of the cirlce
  trianlge.A =  0.5*(x1*(y2-c.y0) + x2*(c.y0-y1) + c.x0*(c.y0-y2)) ;
  # calculate circle segment trouhg withdrawing triangle from cone: 
  c.seg.A = c.cone.A-trianlge.A
  
  # circle segment on BT side, if AT and BT side have intersection
  # calculate angle between the lines from sampling cirlce intersections to center
  azi_inter1.bsite_center = azi(x.1.bsite, y.1.bsite, c.x0, c.y0);
  azi_inter2.bsite_center = azi(x.2.bsite, y.2.bsite, c.x0, c.y0);
  # to not receive negative angles, we have to deduct azi of intersection 1 from azi of intersection 2 and multiply it ith -1 in case it´s negative
  azi_between_IC_lines.bsite = ifelse((azi_inter1.bsite_center-azi_inter2.bsite_center)<0, (azi_inter1.bsite_center-azi_inter2.bsite_center)*(-1), azi_inter1.bsite_center-azi_inter2.bsite_center);
  # calcualte circle segment area: 
  c.cone.A.bsite = (pi*c.r0^2) * azi_between_IC_lines.bsite/400;  # /400 because our azi (gon)is in gon not degrees 
  # calcualte circle area
  c.A.bsite = pi*c.r0^2
  # calculate area of triangle between the intersections with the sampling circle and the center of the cirlce
  trianlge.A.bsite =  0.5*(x.1.bsite*(y.2.bsite-c.y0) + x.2.bsite*(c.y0-y.1.bsite) + c.x0*(c.y0-y.2.bsite)) ;
  # calculate circle segment trouhg withdrawing triangle from cone: 
  c.seg.A.bsite = c.cone.A.bsite-trianlge.A.bsite
  
  
  ## calculate coordiantes of the middle of thie line between 
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
  y2_inter_MC = intersection_line_circle(b0_MC, b1_MC, c.x0, c.y0, c.r0, coordinate = "y2");
  
  # finde the x coordiante of the intersection that is within the triangle (p.in.tr)
  # if inter_1_MC or inter_MC_2 is element of the triangle and the distance between the intersection 
  # and the middle point of the line is greater then the distanc between the intersection that is outside the triangle and by that inside the plot 
  # deduct the circle segment from the whole plot area (because the larger part of the plot belongs to category B)
  # if the itnersection that is element to the triangle lies on the shorter side of the line, use the circle segment / circle bows area as the edge area
  B_trees_edge.a = ifelse(p.in.triangle(x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t, X1_inter_MC, y1_inter_MC) == "B" &
                            distance(X1_inter_MC, y1_inter_MC, x_m_line, y_m_line) > distance(X2_inter_MC, y2_inter_MC, x_m_line, y_m_line) |
                            p.in.triangle(x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t, X1_inter_MC, y1_inter_MC) == "B" &
                            distance(X2_inter_MC, y2_inter_MC, x_m_line, y_m_line) > distance(X1_inter_MC, y1_inter_MC, x_m_line, y_m_line),
                          c.A - c.seg.A, c.seg.A); 
  
  # edge.1.A = ifelse(e.from == "1", c.seg.A, NA);
  ## return the area of the bigger or smaller circle segment, depending on which one of the both lies inside the triangle for edge form == 2 and only one arm intersecting the circle
  edge.2.line.A = ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT != "two I" & p.in.triangle(x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t, X1_inter_MC, y1_inter_MC) == "B" &
                           distance(X1_inter_MC, y1_inter_MC, x_m_line, y_m_line) > distance(X2_inter_MC, y2_inter_MC, x_m_line, y_m_line) |
                           e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT != "two I" & p.in.triangle(x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t, X1_inter_MC, y1_inter_MC) == "B" &
                           distance(X2_inter_MC, y2_inter_MC, x_m_line, y_m_line) > distance(X1_inter_MC, y1_inter_MC, x_m_line, y_m_line)|
                           e.form == "2" & t.dist > c.r0 & i_status.AT != "two I" & i_status.BT == "two I" & p.in.triangle(x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t, X1_inter_MC, y1_inter_MC) == "B" &
                           distance(X1_inter_MC, y1_inter_MC, x_m_line, y_m_line) > distance(X2_inter_MC, y2_inter_MC, x_m_line, y_m_line) |
                           e.form == "2" & t.dist > c.r0 & i_status.AT != "two I" & i_status.BT == "two I" & p.in.triangle(x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t, X1_inter_MC, y1_inter_MC) == "B" &
                           distance(X2_inter_MC, y2_inter_MC, x_m_line, y_m_line) > distance(X1_inter_MC, y1_inter_MC, x_m_line, y_m_line),
                         c.A - c.seg.A, c.seg.A);
  
  # this is when the respective cirlce (could be also the inner cricle for edge type 1) doesn´t have intersections with the edge line but may still be located in the edge area
  # this is, however unlikely for edge type 1 because it assigns the edge area always to the smaller side of the circle so that a whole circle is unlikely to be inside of it
  edge.whole.circle.A = ifelse(e.form == "2" & i_status.AT != "two I" & i_status.BT != "two I" & p.in.triangle(x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t, c.x0, c.y0) == "B", c.A, NA)
  
  # for edge form == 1 it´s always the circle segment, cause the trees in  
  edge.area = ifelse(e.form == "1" & tree_status == "B" & i_status.AB == "two I", c.seg.A, 
                            ifelse(e.form == "2" & tree_status == "B" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT != "two I"|
                                     e.form == "2" & tree_status == "B"&  t.dist > c.r0 & i_status.BT == "two I" & i_status.AT != "two I", edge.2.line.A,
                                   ifelse(e.form == "2" & tree_status == "B"& t.dist <= c.r0 & i_status.AT == "two I" & i_status.BT == "two I", c.cone.A, 
                                          ifelse(e.form == "2" & tree_status == "B" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I", c.seg.A.bsite + c.seg.A, 
                                                 ifelse(e.form == "2" & i_status.AT != "two I" & i_status.BT != "two I" & 
                                                 p.in.triangle(x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t, c.x0, c.y0) == "B", edge.whole.circle.A,
                                                        0)))));
  circle.area = c.A - edge.area; 
  area.method = ifelse(tree_status == "A" | is.na(e.form), circle.area, edge.area);
  
  return(area.method)
}



# this is the function that describes the method applied to find the edge area
edge.A.method <- function(e.form, dbh.cm, x.a, x.b, x.t, y.a, y.b, y.t, t.dist, tree_status){
  # x1| y1 and x2|y2 belong to the intersections of the line or two points on a line
  # c.x0, c.y0 are the center coordinates of the circle
  # c.r0 is the radius of the circle
  # l.b0, l.b1 are the parameters of the line we assign the edges for
  #  xa, xb, xc, ya, yb, yc are the coordinates of the triangle corners that were used to identiy the "out" / "B" trees
  # c.seg.a means the area of the cirle segment (circle bow) or the circle segmetns per CCS, c.a means the area if the whole circle
  
  # select the diameter of the circle depending on the trees diameter
  c.x0 = 0;
  c.y0 = 0; 
  c.r0 = ifelse(dbh.cm >= 7 & dbh.cm < 10, 564, 
                ifelse(dbh.cm >= 10 & dbh.cm < 30,  1262, 
                       ifelse(dbh.cm >= 30, 1784, NA)))
  
  
  ## calcualte slope and intercept of AT and BT line to calcualte intersections
  l.AB.b0 = ifelse(e.form == "1", intercept(x.a, y.a, x.b, y.b), NA);
  l.AB.b1 = ifelse(e.form == "1", slope(x.a, y.a, x.b, y.b), NA);
  l.AT.b0 = ifelse(e.form == "2", intercept(x.t, y.t, x.a, y.a), NA);
  l.AT.b1 = ifelse(e.form == "2", slope(x.t, y.t, x.a, y.a), NA);
  l.BT.b0 = ifelse(e.form == "2", intercept(x.t, y.t, x.b, y.b), NA);
  l.BT.b1 = ifelse(e.form == "2", slope(x.t, y.t, x.b, y.b), NA);
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
  x.AT.inter.triangle.60 = inter.for.triangle(l.AT.b0, l.AT.b1,  c.x0, c.y0, 6000, x.a, y.a, x.t, y.t, coordinate = "x" );
  # calculate y for AT triangle
  y.AT.inter.triangle.60 = inter.for.triangle(l.AT.b0, l.AT.b1,  c.x0, c.y0, 6000, x.a, y.a, x.t, y.t, coordinate = "y" );
  #BT 
  # select the intersection coordinates for the triangle on BT line
  x.BT.inter.triangle.60 = inter.for.triangle(l.BT.b0, l.BT.b1,  c.x0, c.y0, 6000, x.b, y.b, x.t, y.t, coordinate = "x" );
  # calculate y for BT triangle
  y.BT.inter.triangle.60 = inter.for.triangle(l.BT.b0, l.BT.b1,  c.x0, c.y0, 6000, x.b, y.b, x.t, y.t, coordinate = "y" );
  
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
                            ifelse(e.form == "2" & t.dist <= c.r0 & i_status.AT == "two I" & i_status.BT == "two I", inter.for.triangle(l.AT.b0, l.AT.b1,  c.x0, c.y0, c.r0, x.a, y.a, x.t, y.t, coordinate = "x" ), 
                                   ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I", x1.inter.AT, 
                                          NA)))));
  x2 = ifelse(e.form == "1" & i_status.AB == "two I", x2.inter.AB, 
              ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT != "two I", x2.inter.AT, 
                     ifelse(e.form == "2" & t.dist > c.r0 & i_status.BT == "two I" & i_status.AT != "two I", x2.inter.BT,
                            ifelse(e.form == "2" & t.dist <= c.r0 & i_status.AT == "two I" & i_status.BT == "two I", inter.for.triangle(l.BT.b0, l.BT.b1,  c.x0, c.y0, c.r0, x.b, y.b, x.t, y.t, coordinate = "x" ), 
                                   ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I", x2.inter.AT, 
                                          NA)))));
  y1 = ifelse(e.form == "1" & i_status.AB == "two I", x1.inter.AB, 
              ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT != "two I", y1.inter.AT, 
                     ifelse(e.form == "2" & t.dist > c.r0 & i_status.BT == "two I" & i_status.AT != "two I", y1.inter.BT,
                            ifelse(e.form == "2" & t.dist <= c.r0 & i_status.AT == "two I" & i_status.BT == "two I", inter.for.triangle(l.AT.b0, l.AT.b1,  c.x0, c.y0, c.r0, x.a, y.a, x.t, y.t, coordinate = "y" ), 
                                   ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I", y1.inter.AT, 
                                          NA)))));
  y2 = ifelse(e.form == "1" & i_status.AB == "two I", y2.inter.AB, 
              ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT != "two I", y2.inter.AT, 
                     ifelse(e.form == "2" & t.dist > c.r0 & i_status.BT == "two I" & i_status.AT != "two I", y2.inter.BT,
                            ifelse(e.form == "2" & t.dist <= c.r0 & i_status.AT == "two I" & i_status.BT == "two I", inter.for.triangle(l.BT.b0, l.BT.b1,  c.x0, c.y0, c.r0, x.b, y.b, x.t, y.t, coordinate = "y" ), 
                                   ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I", y2.inter.AT, 
                                          NA)))));
  # create another intersection pair for circles that are intersected by both arms of the triangle  
  x.1.bsite = ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I", x1.inter.BT, NA);
  x.2.bsite = ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I", x2.inter.BT, NA);
  y.1.bsite = ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I", x1.inter.BT, NA);
  y.2.bsite = ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I", x2.inter.BT, NA);
  
  # circle segment on AB or AT or BT side
  # calculate angle between the lines from sampling cirlce intersections to center
  azi_inter1_center = azi(x1, y1, c.x0, c.y0);
  azi_inter2_center = azi(x2, y2, c.x0, c.y0);
  # to not receive negative angles, we have to deduct azi of intersection 1 from azi of intersection 2 and multiply it ith -1 in case it´s negative
  azi_between_IC_lines = ifelse((azi_inter1_center-azi_inter2_center)<0, (azi_inter1_center-azi_inter2_center)*(-1), azi_inter1_center-azi_inter2_center);
  # calcualte circle segment area: 
  c.cone.A = (pi*c.r0^2) * azi_between_IC_lines/400;  # /400 because our azi (gon)is in gon not degrees 
  # calcualte circle area
  c.A = pi*c.r0^2
  # calculate area of triangle between the intersections with the sampling circle and the center of the cirlce
  trianlge.A =  0.5*(x1*(y2-c.y0) + x2*(c.y0-y1) + c.x0*(c.y0-y2)) ;
  # calculate circle segment trouhg withdrawing triangle from cone: 
  c.seg.A = c.cone.A-trianlge.A
  
  # circle segment on BT side, if AT and BT side have intersection
  # calculate angle between the lines from sampling cirlce intersections to center
  azi_inter1.bsite_center = azi(x.1.bsite, y.1.bsite, c.x0, c.y0);
  azi_inter2.bsite_center = azi(x.2.bsite, y.2.bsite, c.x0, c.y0);
  # to not receive negative angles, we have to deduct azi of intersection 1 from azi of intersection 2 and multiply it ith -1 in case it´s negative
  azi_between_IC_lines.bsite = ifelse((azi_inter1.bsite_center-azi_inter2.bsite_center)<0, (azi_inter1.bsite_center-azi_inter2.bsite_center)*(-1), azi_inter1.bsite_center-azi_inter2.bsite_center);
  # calcualte circle segment area: 
  c.cone.A.bsite = (pi*c.r0^2) * azi_between_IC_lines.bsite/400;  # /400 because our azi (gon)is in gon not degrees 
  # calcualte circle area
  c.A.bsite = pi*c.r0^2
  # calculate area of triangle between the intersections with the sampling circle and the center of the cirlce
  trianlge.A.bsite =  0.5*(x.1.bsite*(y.2.bsite-c.y0) + x.2.bsite*(c.y0-y.1.bsite) + c.x0*(c.y0-y.2.bsite)) ;
  # calculate circle segment trouhg withdrawing triangle from cone: 
  c.seg.A.bsite = c.cone.A.bsite-trianlge.A.bsite
  
  
  ## calculate coordiantes of the middle of thie line between 
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
  y2_inter_MC = intersection_line_circle(b0_MC, b1_MC, c.x0, c.y0, c.r0, coordinate = "y2");
  
  # finde the x coordiante of the intersection that is within the triangle (p.in.tr)
  # if inter_1_MC or inter_MC_2 is element of the triangle and the distance between the intersection 
  # and the middle point of the line is greater then the distanc between the intersection that is outside the triangle and by that inside the plot 
  # deduct the circle segment from the whole plot area (because the larger part of the plot belongs to category B)
  # if the itnersection that is element to the triangle lies on the shorter side of the line, use the circle segment / circle bows area as the edge area
  B_trees_edge.a = ifelse(p.in.triangle(x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t, X1_inter_MC, y1_inter_MC) == "B" &
                            distance(X1_inter_MC, y1_inter_MC, x_m_line, y_m_line) > distance(X2_inter_MC, y2_inter_MC, x_m_line, y_m_line) |
                            p.in.triangle(x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t, X1_inter_MC, y1_inter_MC) == "B" &
                            distance(X2_inter_MC, y2_inter_MC, x_m_line, y_m_line) > distance(X1_inter_MC, y1_inter_MC, x_m_line, y_m_line),
                          c.A - c.seg.A, c.seg.A); 
  
  # edge.1.A = ifelse(e.from == "1", c.seg.A, NA);
  ## return the area of the bigger or smaller circle segment, depending on which one of the both lies inside the triangle for edge form == 2 and only one arm intersecting the circle
  edge.2.line.A = ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT != "two I" & p.in.triangle(x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t, X1_inter_MC, y1_inter_MC) == "B" &
                           distance(X1_inter_MC, y1_inter_MC, x_m_line, y_m_line) > distance(X2_inter_MC, y2_inter_MC, x_m_line, y_m_line) |
                           e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT != "two I" & p.in.triangle(x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t, X1_inter_MC, y1_inter_MC) == "B" &
                           distance(X2_inter_MC, y2_inter_MC, x_m_line, y_m_line) > distance(X1_inter_MC, y1_inter_MC, x_m_line, y_m_line)|
                           e.form == "2" & t.dist > c.r0 & i_status.AT != "two I" & i_status.BT == "two I" & p.in.triangle(x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t, X1_inter_MC, y1_inter_MC) == "B" &
                           distance(X1_inter_MC, y1_inter_MC, x_m_line, y_m_line) > distance(X2_inter_MC, y2_inter_MC, x_m_line, y_m_line) |
                           e.form == "2" & t.dist > c.r0 & i_status.AT != "two I" & i_status.BT == "two I" & p.in.triangle(x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t, X1_inter_MC, y1_inter_MC) == "B" &
                           distance(X2_inter_MC, y2_inter_MC, x_m_line, y_m_line) > distance(X1_inter_MC, y1_inter_MC, x_m_line, y_m_line),
                         c.A - c.seg.A, c.seg.A);
  # this is when the respective cirlce (could be also the inner cricle for edge type 1) doesn´t have intersections with the edge line but may still be located in the edge area
  # this is, however unlikely for edge type 1 because it assigns the edge area always to the smaller side of the circle so that a whole circle is unlikely to be inside of it
  edge.whole.circle.A = ifelse(e.form == "2" & i_status.AT != "two I" & i_status.BT != "two I" & p.in.triangle(x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t, c.x0, c.y0) == "B", c.A, NA)
  # edge.2.cone.A = ifelse(e.from == "2" & t.dist <= c.r0, c.cone.A, NA);
  # edge.2.triangle.A = ifelse(e.from == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I", c.seg.A.bsite + c.seg.A, NA)
  
  # for edge form == 1 it´s always the circle segment, cause the trees in  
  edge.area.method = ifelse(e.form == "1" & tree_status == "B" & i_status.AB == "two I", "c.seg.A", 
                ifelse(e.form == "2" & tree_status == "B" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT != "two I"|
                         e.form == "2" & tree_status == "B"&  t.dist > c.r0 & i_status.BT == "two I" & i_status.AT != "two I", "edge.2.line.A",
                       ifelse(e.form == "2" & tree_status == "B"& t.dist <= c.r0 & i_status.AT == "two I" & i_status.BT == "two I", "c.cone.A", 
                              ifelse(e.form == "2" & tree_status == "B" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I", "c.seg.A.bsite + c.seg.A", 
                                     ifelse(e.form == "2" & i_status.AT != "two I" & i_status.BT != "two I" & p.in.triangle(x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t, c.x0, c.y0) == "B", "edge.whole.circle.A",
                                            NA)))));
  circle.area.method = "c.A -edge.area"; 
  area.method = ifelse(tree_status == "A" | is.na(e.form), circle.area.method, edge.area.method);
  return(area.method)
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
             aes(X_tree, Y_tree, colour = t_status_AB_ABT_test))+
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


  

# ----- NOTES -------------------------------------------------------------

# n. old functions --------------------------------------------------------

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

# select the intersection coordinates that have the same azimute as A to T and B to T
select.inter.for.triangle <- function(t.dist, c.r0, azi_inter_1, azi_inter_2, azi_centre, x1, x2){
  x <- ifelse(t.dist <= c.r0 & azi_inter_1 == azi_centre, x1, 
              ifelse(t.dist <= c.r0 & azi_inter_2 == azi_centre, x2, NA));
  return(x)
}



#----N. 0.5.14.1. azimut -------------------------------------------------------
azimut <- function(x2, y2, x1, y1){
  # azi = atan((y2 - y1)/(x2 - x1));
  azi = atan((x2 - x1)/(y2 - y1));
  return(azi)
}

azi_correction <- function(x2, y2, x1, y1, azi){
  delta_x = x2 -x1 ;
  delta_y = y2-y1 ; 
  azi_corrected = ifelse(delta_x >= 0 & delta_y > 0 | delta_x > 0 & delta_y >= 0, azi,                    # first quadrant x + y+
                         ifelse(delta_x >= 0 & delta_y < 0 |delta_x > 0 & delta_y <= 0, azi+200,         # second quadrant x + y-
                                ifelse(delta_x <= 0 & delta_y < 0 |delta_x < 0 & delta_y <= 0,  azi+200,   # third quadrant x- y-
                                       ifelse(delta_x <= 0 & delta_y > 0 | delta_x < 0 & delta_y >= 0, azi+400, NA))));
  return(azi_corrected)
}



# ------0.5.11.2. cirlce segment area  ----------------------
# calculate the area of a cirlce segment by the angle between the two branches of the segment 
circle_seg_A <- function(r, angle){
  A_c_seg = (pi*r^2) * angle/400; 
  return(A_c_seg)
}

# ------0.5.11.3. triangle area  ----------------------
triangle_A <- function(x1, x2, x3, y1, y2, y3){
  # x1|y1 and x2|y2 should be the intersections with the circle, 
  # x3|y3 should be the turning point or centre of the cirlce 
  # https://www.lernhelfer.de/schuelerlexikon/mathematik-abitur/artikel/flaecheninhalt-eines-dreiecks
  A_tri =  0.5*(x1*(y2-y3) + x2*(y3-y1) + x3*(y3-y2)) ;
  return(A_tri)
}

# ------0.5.11.1. angle triabnle for area calculations --------------------------------------------------------------------------
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


# ----- 0.5.11.4. selecting/ calculating total edge area per edge type per plot  ----------
tot.edge.A <- function(area_AT_AB_side, area_BT_side){
  A <- ifelse(!is.na(area_AT_AB_side) & !is.na(area_BT_side), area_AT_AB_side + area_BT_side,
              ifelse(!is.na(area_AT_AB_side) & is.na(area_BT_side), area_AT_AB_side, 
                     ifelse(is.na(area_AT_AB_side) & !is.na(area_BT_side), area_BT_side, 
                            0)));
  return(A)
} 

# ----- 0.5.11.5. assign correct area to trees according to their category (A/B)  ----------
# to select which area we have assign to the edge and which we have to asssign to the main stand
# we have to find out on which side of the line the "B" and on which side the "A" trees are located
# as we know if the result of the implicit function has to be positive or negative for the tree to lie
# outside the plot, we can calcualte the intersections of a line through the center of the edge line 
# and the center of the plot. 
# Following we check which of the intersections is element of the triangle or if the result of the implicit function 
# of the intersection comlies with the result the implicitf function nee to have for atree to be outside (middle.point.to.line)
# this is for edge type 2 with t outside the circle and one lines intersecting  
identify.edge.area <- function(x1, x2, y1, y2, c.x0, c.y0, c.r0,l.b0, l.b1, xa, xb, x.t, ya, yb, y.t, c.seg.a, c.a, tree_status){
  # x1| y1 and x2|y2 belong to the intersections of the line or two points on a line
  # c.x0, c.y0 are the center coordinates of the circle
  # c.r0 is the radius of the circle
  # l.b0, l.b1 are the parameters of the line we assign the edges for
  #  xa, xb, xc, ya, yb, yc are the coordinates of the triangle corners that were used to identiy the "out" / "B" trees
  # c.seg.a means the area of the cirle segment (circle bow) or the circle segmetns per CCS, c.a means the area if the whole circle
  
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
  y2_inter_MC = intersection_line_circle(b0_MC, b1_MC, c.x0, c.y0, c.r0, coordinate = "y2");
  
  # finde the x coordiante of the intersection that is within the triangle (p.in.tr)
  # if inter_1_MC or inter_MC_2 is element of the triangle and the distance between the intersection 
  # and the middle point of the line is greater then the distanc between the intersection that is outside the triangle and by that inside the plot 
  # deduct the circle segment from the whole plot area (because the larger part of the plot belongs to category B)
  # if the itnersection that is element to the triangle lies on the shorter side of the line, use the circle segment / circle bows area as the edge area
  B_trees_edge.a = ifelse(p.in.triangle(xa, xb, x.t, ya, yb, y.t, X1_inter_MC, y1_inter_MC) == "B" &
                            distance(X1_inter_MC, y1_inter_MC, x_m_line, y_m_line) > distance(X2_inter_MC, y2_inter_MC, x_m_line, y_m_line) |
                            p.in.triangle(xa, xb, x.t, ya, yb, y.t, X2_inter_MC, y2_inter_MC) == "B" &
                            distance(X2_inter_MC, y2_inter_MC, x_m_line, y_m_line) > distance(X1_inter_MC, y1_inter_MC, x_m_line, y_m_line),
                          c.a - c.seg.a, c.seg.a); 
  # if inter_1_MC or inter_2_MC is not element of the triangle and the distance between this intersection and the middle of the line is bigger then the 
  # then distance between the intersection that is element to the triangle , so the A side of the line is the bigger side, deduct the circle segment area 
  # from the cirlce area, 
  # if the intersection is not element of the triangle but situated on the smaller side of the line (shorter distance between intersection and point M), 
  # the area of A trees has to be the area of the circle segment itself
  A_trees_edge.a = ifelse(p.in.triangle(xa, xb, xc, ya, yb, yc, X1_inter_MC, y1_inter_MC) == "A" &
                            distance(X1_inter_MC, y1_inter_MC, x_m_line, y_m_line) > distance(X2_inter_MC, y2_inter_MC, x_m_line, y_m_line) |
                            p.in.triangle(xa, xb, xc, ya, yb, yc, X2_inter_MC, y2_inter_MC) == "A" &
                            distance(X2_inter_MC, y2_inter_MC, x_m_line, y_m_line) > distance(X1_inter_MC, y1_inter_MC, x_m_line, y_m_line),
                          c.a - c.seg.a, c.seg.a);
  # if the tree status is "outside" "B", return the area calcualted for the "B" side of the plot, else return the area of the "A" side
  area = ifelse(tree_status == "B", B_trees_edge.a, A_trees_edge.a);
  
  return(area)
}

# this function should eable us to skip the part where we have to calcualte the intersections etc. for each circle and line
# this way we´ll just return the area per sampling circle 
edge.A.site.triangle <- function(e.form, c.x0, c.y0, c.r0, x.a, x.b, x.t, y.a, y.b, y.t, t.dist, tree_status){
  # x1| y1 and x2|y2 belong to the intersections of the line or two points on a line
  # c.x0, c.y0 are the center coordinates of the circle
  # c.r0 is the radius of the circle
  # l.b0, l.b1 are the parameters of the line we assign the edges for
  #  xa, xb, xc, ya, yb, yc are the coordinates of the triangle corners that were used to identiy the "out" / "B" trees
  # c.seg.a means the area of the cirle segment (circle bow) or the circle segmetns per CCS, c.a means the area if the whole circle
  
  
  ## calcualte slope and intercept of AT and BT line to calcualte intersections
  l.AB.b0 = ifelse(e.form == "1", intercept(x.a, y.a, x.b, y.b), NA);
  l.AB.b1 = ifelse(e.form == "1", slope(x.a, y.a, x.b, y.b), NA);
  l.AT.b0 = ifelse(e.form == "2", intercept(x.t, y.t, x.a, y.a), NA);
  l.AT.b1 = ifelse(e.form == "2", slope(x.t, y.t, x.a, y.a), NA);
  l.BT.b0 = ifelse(e.form == "2", intercept(x.t, y.t, x.b, y.b), NA);
  l.BT.b1 = ifelse(e.form == "2", slope(x.t, y.t, x.b, y.b), NA);
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
  x.AT.inter.triangle.60 = inter.for.triangle(l.AT.b0, l.AT.b1,  c.x0, c.y0, 6000, x.a, y.a, x.t, y.t, coordinate = "x" );
  # calculate y for AT triangle
  y.AT.inter.triangle.60 = inter.for.triangle(l.AT.b0, l.AT.b1,  c.x0, c.y0, 6000, x.a, y.a, x.t, y.t, coordinate = "y" );
  #BT 
  # select the intersection coordinates for the triangle on BT line
  x.BT.inter.triangle.60 = inter.for.triangle(l.BT.b0, l.BT.b1,  c.x0, c.y0, 6000, x.b, y.b, x.t, y.t, coordinate = "x" );
  # calculate y for BT triangle
  y.BT.inter.triangle.60 = inter.for.triangle(l.BT.b0, l.BT.b1,  c.x0, c.y0, 6000, x.b, y.b, x.t, y.t, coordinate = "y" );
  
  ## build circle segment
  # calculate intersections with sampling circle (17,12,5m)
  # if AT intersects the cirlce twice, but BT doesnt, x1 = AT_inter_1 and x2 = AT_inter_2
  # if AT intersects the cirlce twice, but BT doesnt, l.b0 = l.AT.b0 and l.b1 = l.AT.b1
  # if BT intersects the circle twice but AT doesn´t, x1 = BT_inter_1 and x2 = BT_inter_2
  # if BT intersects the cirlce twice, but BT doesnt, l.b0 = l.BT.b0 and l.b1 = l.BT.b1
  # if AB intersects the circle twice , x1 = AB_inter_1 and x2 = AB_inter_2, l.b0 = l.AB.b0 and l.b1 = l.AB.b1
  # if AT and BT intersect the cirlce twise we have to put B in a second variable
  # if T lies inside the circle, interA with the same direction as A to T is x1 and inter B with the same direction (azimute) as B to T is x2
  x1 = ifelse(e.from == "1" & i_status.AB == "two I", x1.inter.AB, 
              ifelse(e.from == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT != "two I", x1.inter.AT, 
                     ifelse(e.from == "2" & t.dist > c.r0 & i_status.BT == "two I" & i_status.AT != "two I", x1.inter.BT,
                            ifelse(e.from == "2" & t.dist <= c.r0 & i_status.AT == "two I" & i_status.BT == "two I", inter.for.triangle(l.AT.b0, l.AT.b1,  c.x0, c.y0, c.r0, x.a, y.a, x.t, y.t, coordinate = "x" ), 
                                   ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I", x1.inter.AT, 
                                          NA)))));
  x2 = ifelse(e.from == "1" & i_status.AB == "two I", x2.inter.AB, 
              ifelse(e.from == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT != "two I", x2.inter.AT, 
                     ifelse(e.from == "2" & t.dist > c.r0 & i_status.BT == "two I" & i_status.AT != "two I", x2.inter.BT,
                            ifelse(e.from == "2" & t.dist <= c.r0 & i_status.AT == "two I" & i_status.BT == "two I", inter.for.triangle(l.BT.b0, l.BT.b1,  c.x0, c.y0, c.r0, x.b, y.b, x.t, y.t, coordinate = "x" ), 
                                   ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I", x2.inter.AT, 
                                          NA)))));
  y1 = ifelse(e.from == "1" & i_status.AB == "two I", x1.inter.AB, 
              ifelse(e.from == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT != "two I", y1.inter.AT, 
                     ifelse(e.from == "2" & t.dist > c.r0 & i_status.BT == "two I" & i_status.AT != "two I", y1.inter.BT,
                            ifelse(e.from == "2" & t.dist <= c.r0 & i_status.AT == "two I" & i_status.BT == "two I", inter.for.triangle(l.AT.b0, l.AT.b1,  c.x0, c.y0, c.r0, x.a, y.a, x.t, y.t, coordinate = "y" ), 
                                   ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I", y1.inter.AT, 
                                          NA)))));
  x2 = ifelse(e.from == "1" & i_status.AB == "two I", y2.inter.AB, 
              ifelse(e.from == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT != "two I", y2.inter.AT, 
                     ifelse(e.from == "2" & t.dist > c.r0 & i_status.BT == "two I" & i_status.AT != "two I", y2.inter.BT,
                            ifelse(e.from == "2" & t.dist <= c.r0 & i_status.AT == "two I" & i_status.BT == "two I", inter.for.triangle(l.BT.b0, l.BT.b1,  c.x0, c.y0, c.r0, x.b, y.b, x.t, y.t, coordinate = "y" ), 
                                   ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I", y2.inter.AT, 
                                          NA)))));
  # create another intersection pair for circles that are intersected by both arms of the triangle  
  x.1.bsite = ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I", x1.inter.BT, NA);
  x.2.bsite = ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I", x2.inter.BT, NA);
  y.1.bsite = ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I", x1.inter.BT, NA);
  y.2.bsite = ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I", x2.inter.BT, NA);
  
  # circle segment on AB or AT or BT side
  # calculate angle between the lines from sampling cirlce intersections to center
  azi_inter1_center = azi(x1, y1, c.x0, c.y0);
  azi_inter2_center = azi(x2, y2, c.x0, c.y0);
  # to not receive negative angles, we have to deduct azi of intersection 1 from azi of intersection 2 and multiply it ith -1 in case it´s negative
  azi_between_IC_lines = ifelse((azi_inter1_center-azi_inter2_center)<0, (azi_inter1_center-azi_inter2_center)*(-1), azi_inter1_center-azi_inter2_center);
  # calcualte circle segment area: 
  c.cone.A = (pi*c.r0^2) * angle/400;  # /400 because our azi (gon)is in gon not degrees 
  # calcualte circle area
  c.A = pi*c.r0^2
  # calculate area of triangle between the intersections with the sampling circle and the center of the cirlce
  trianlge.A =  0.5*(x1*(y2-c.y0) + x2*(c.y0-y1) + c.x0*(c.y0-y2)) ;
  # calculate circle segment trouhg withdrawing triangle from cone: 
  c.seg.A = c.cone.A-trianlge.A
  
  # circle segment on BT side, if AT and BT side have intersection
  # calculate angle between the lines from sampling cirlce intersections to center
  azi_inter1.bsite_center = azi(x.1.bsite, y.1.bsite, c.x0, c.y0);
  azi_inter2.bsite_center = azi(x.2.bsite, y.2.bsite, c.x0, c.y0);
  # to not receive negative angles, we have to deduct azi of intersection 1 from azi of intersection 2 and multiply it ith -1 in case it´s negative
  azi_between_IC_lines = ifelse((azi_inter1.bsite_center-azi_inter2.bsite_center)<0, (azi_inter1.bsite_center-azi_inter2.bsite_center)*(-1), azi_inter1.bsite_center-azi_inter2.bsite_center);
  # calcualte circle segment area: 
  c.cone.A.bsite = (pi*c.r0^2) * angle/400;  # /400 because our azi (gon)is in gon not degrees 
  # calcualte circle area
  c.A.bsite = pi*c.r0^2
  # calculate area of triangle between the intersections with the sampling circle and the center of the cirlce
  trianlge.A.bsite =  0.5*(x.1.bsite*(y.2.bsite-c.y0) + x.2.bsite*(c.y0-y.1.bsite) + c.x0*(c.y0-y.2.bsite)) ;
  # calculate circle segment trouhg withdrawing triangle from cone: 
  c.seg.A.bsite = c.cone.A.bsite-trianlge.A.bsite
  
  
  ## calculate coordiantes of the middle of thie line between 
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
  y2_inter_MC = intersection_line_circle(b0_MC, b1_MC, c.x0, c.y0, c.r0, coordinate = "y2");
  
  # finde the x coordiante of the intersection that is within the triangle (p.in.tr)
  # if inter_1_MC or inter_MC_2 is element of the triangle and the distance between the intersection 
  # and the middle point of the line is greater then the distanc between the intersection that is outside the triangle and by that inside the plot 
  # deduct the circle segment from the whole plot area (because the larger part of the plot belongs to category B)
  # if the itnersection that is element to the triangle lies on the shorter side of the line, use the circle segment / circle bows area as the edge area
  B_trees_edge.a = ifelse(p.in.triangle(x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t, X1_inter_MC, y1_inter_MC) == "B" &
                            distance(X1_inter_MC, y1_inter_MC, x_m_line, y_m_line) > distance(X2_inter_MC, y2_inter_MC, x_m_line, y_m_line) |
                            p.in.triangle(x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t, X1_inter_MC, y1_inter_MC) == "B" &
                            distance(X2_inter_MC, y2_inter_MC, x_m_line, y_m_line) > distance(X1_inter_MC, y1_inter_MC, x_m_line, y_m_line),
                          c.A - c.seg.A, c.seg.A); 
  
  # edge.1.A = ifelse(e.from == "1", c.seg.A, NA);
  ## return the area of the bigger or smaller circle segment, depending on which one of the both lies inside the triangle for edge form == 2 and only one arm intersecting the circle
  edge.2.line.A = ifelse(e.from == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT != "two I" & p.in.triangle(x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t, X1_inter_MC, y1_inter_MC) == "B" &
                           distance(X1_inter_MC, y1_inter_MC, x_m_line, y_m_line) > distance(X2_inter_MC, y2_inter_MC, x_m_line, y_m_line) |
                           e.from == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT != "two I" & p.in.triangle(x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t, X1_inter_MC, y1_inter_MC) == "B" &
                           distance(X2_inter_MC, y2_inter_MC, x_m_line, y_m_line) > distance(X1_inter_MC, y1_inter_MC, x_m_line, y_m_line)|
                           e.from == "2" & t.dist > c.r0 & i_status.AT != "two I" & i_status.BT == "two I" & p.in.triangle(x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t, X1_inter_MC, y1_inter_MC) == "B" &
                           distance(X1_inter_MC, y1_inter_MC, x_m_line, y_m_line) > distance(X2_inter_MC, y2_inter_MC, x_m_line, y_m_line) |
                           e.from == "2" & t.dist > c.r0 & i_status.AT != "two I" & i_status.BT == "two I" & p.in.triangle(x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t, X1_inter_MC, y1_inter_MC) == "B" &
                           distance(X2_inter_MC, y2_inter_MC, x_m_line, y_m_line) > distance(X1_inter_MC, y1_inter_MC, x_m_line, y_m_line),
                         c.A - c.seg.A, c.seg.A);
  # edge.2.cone.A = ifelse(e.from == "2" & t.dist <= c.r0, c.cone.A, NA);
  # edge.2.triangle.A = ifelse(e.from == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I", c.seg.A.bsite + c.seg.A, NA)
  
  # for edge form == 1 it´s always the circle segment, cause the trees in  
  area = ifelse(e.from == "1" & i_status.AB == "two I", c.seg.A, 
                ifelse(e.from == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT != "two I"|
                         e.from == "2" & t.dist > c.r0 & i_status.BT == "two I" & i_status.AT != "two I", edge.2.line.A,
                       ifelse(e.from == "2" & t.dist <= c.r0 & i_status.AT == "two I" & i_status.BT == "two I", c.cone.A, 
                              ifelse(e.form == "2" & t.dist > c.r0 & i_status.AT == "two I" & i_status.BT == "two I", c.seg.A.bsite + c.seg.A, 
                                     NA))));
  return(area)
}

# N. 0.5.8.1.1. check for relation between intersections of middle-point-center-line and point and edge line  ---------------------------------------------------------
middle.point.to.line <- function(x1, x2, y1, y2, c.x0, c.y0, c.r0, l.b0, l.b1){
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
  y2_inter_MC = intersection_line_circle(b0_MC, b1_MC, c.x0, c.y0, c.r0, coordinate = "y2");
  # distance between the intersections (inter_MC_1, inter_MC_2) to M on the line 
  dist_C_inter_1_MC = distance(X1_inter_MC, y1_inter_MC, x_m_line, y_m_line);
  dist_C_inter_2_MC = distance(X2_inter_MC, y2_inter_MC, x_m_line, y_m_line); 
  # find the x and y coordinate of the intersection on the shorter side , which is the side to exlcude from the plot 
  X_inter_MC_shorter_side = ifelse(dist_C_inter_1_MC < dist_C_inter_2_MC, X1_inter_MC, X2_inter_MC); 
  Y_inter_MC_shorter_side = ifelse(dist_C_inter_1_MC < dist_C_inter_2_MC, y1_inter_MC, y2_inter_MC);
  # insert coordinates that are for sure on the smaller side of the two halves of the circle into the implicit equation: 
  Y_MC_implicit = l.b0  + l.b1 * X_inter_MC_shorter_side - Y_inter_MC_shorter_side;
  Y_implicit_status_M_line = ifelse(Y_MC_implicit > 0, "positive",  "negative");        # "y imlicit has to be positive too for tree to be outside, 
  # as the result of the implicit equation that contains the 
  # point that is for sure in the smaller cirlce segment, has a positive impllciti equation result", 
  # Y_MC_implicit < 0 ~ "negative",          # "y imlicit has to be negative for tree to be outside", 
  #TRUE ~ "equal");
  
  return(Y_implicit_status_M_line)
}
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


# ----- N.1. old way: intersections of lines AB, AT, BT with all sampling circuits --------
# the intersections of each circuit with the line or triangle were used to calcualte the edges area later. 
# now this part is covered in one function

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
  mutate(b0_AB = ifelse(e_form == "1", intercept(X_A, Y_A,  X_B, Y_B), NA), 
         b0_AT = ifelse(e_form == "2", intercept(X_T, Y_T, X_A, Y_A), NA),
         b0_BT = ifelse(e_form == "2", intercept(X_T, Y_T,  X_B, Y_B), NA)) %>% 
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
         azi_T_A = azi(X_A, Y_A, X_T, Y_T),
         azi_T_AT_inter_1 = azi(X1_inter_AT_17, Y1_inter_AT_17, X_T, Y_T),
         azi_T_AT_inter_2 = azi(X2_inter_AT_17, Y2_inter_AT_17, X_T, Y_T),
         # BT line
         azi_T_B = azi(X_B, Y_B, X_T, Y_T),
         azi_T_BT_inter_1 = azi(X1_inter_BT_17, Y1_inter_BT_17, X_T, Y_T),
         azi_T_BT_inter_2 = azi(X2_inter_BT_17, Y2_inter_BT_17, X_T, Y_T),
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






forest_edges_HBI.man %>% 
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
  ##### if ever i build a loop for this it´ll have to start with thte intersection status
  ### for 12m circle   
  # interception status between line and 12.68m circle: insert line equation in circle equation (function: intersection_line_circle)
  # for AB line 
  mutate(X1_inter_AB_12 = intersection_line_circle(b0_AB, b1_AB,  data_circle$y0[2], data_circle$x0[2], data_circle$r0[2],  coordinate="x1"),
         X2_inter_AB_12 = intersection_line_circle(b0_AB, b1_AB, data_circle$y0[2], data_circle$x0[2], data_circle$r0[2], coordinate="x2"), 
         inter_status_AB_12 = intersection.status(intersection_line_circle(b0_AB, b1_AB,  data_circle$y0[2], data_circle$x0[2], data_circle$r0[2], coordinate= "x1"),
                                                  intersection_line_circle(b0_AB, b1_AB, data_circle$y0[2], data_circle$x0[2], data_circle$r0[2], coordinate= "x2")),  # if 2 solutions
         # for AT line
         X1_inter_AT_12 = intersection_line_circle(b0_AT, b1_AT, data_circle$y0[2], data_circle$x0[2], data_circle$r0[2], coordinate="x1"),
         X2_inter_AT_12 = intersection_line_circle(b0_AT, b1_AT, data_circle$y0[2], data_circle$x0[2], data_circle$r0[2], coordinate="x2"), 
         inter_status_AT_12 = intersection.status(intersection_line_circle(b0_AT, b1_AT, data_circle$y0[2], data_circle$x0[2], data_circle$r0[2], coordinate="x1"),
                                                  intersection_line_circle(b0_AT, b1_AT, data_circle$y0[2], data_circle$x0[2], data_circle$r0[2], coordinate="x2")), # if 2 solutions
         # for BT line
         X1_inter_BT_12 =intersection_line_circle(b0_BT, b1_BT, data_circle$y0[2], data_circle$x0[2], data_circle$r0[2], coordinate="x1"),
         X2_inter_BT_12 =intersection_line_circle(b0_BT, b1_BT, data_circle$y0[2], data_circle$x0[2], data_circle$r0[2], coordinate="x2"), 
         inter_status_BT_12 = intersection.status(intersection_line_circle(b0_BT, b1_BT, data_circle$y0[2], data_circle$x0[2], data_circle$r0[2], coordinate="x1"),
                                                  intersection_line_circle(b0_BT, b1_BT, data_circle$y0[2], data_circle$x0[2], data_circle$r0[2], coordinate="x2"))) %>% 
  # y intercept with 12m circle: insert x of intercept with circle in equation of line
  # AB line 
  mutate(Y1_inter_AB_12 = intersection_line_circle(b0_AB, b1_AB,  data_circle$y0[2], data_circle$x0[2], data_circle$r0[2],  coordinate="y1"), 
         Y2_inter_AB_12 = intersection_line_circle(b0_AB, b1_AB,  data_circle$y0[2], data_circle$x0[2], data_circle$r0[2],  coordinate="y2"),  
         # AT line 
         Y1_inter_AT_12 =  intersection_line_circle(b0_AT, b1_AT, data_circle$y0[2], data_circle$x0[2], data_circle$r0[2], coordinate="y1"), 
         Y2_inter_AT_12 =  intersection_line_circle(b0_AT, b1_AT, data_circle$y0[2], data_circle$x0[2], data_circle$r0[2], coordinate="y2"), 
         # BT line 
         Y1_inter_BT_12 = intersection_line_circle(b0_BT, b1_BT, data_circle$y0[2], data_circle$x0[2], data_circle$r0[2], coordinate="y1"),
         Y2_inter_BT_12 = intersection_line_circle(b0_BT, b1_BT, data_circle$y0[2], data_circle$x0[2], data_circle$r0[2], coordinate="y2")) %>%
  ### for 5m circle   
  # interception status between line and 5.64 m circle: insert line equation in circle equation (function: intersection_line_circle)
  # for AB line 
  mutate(X1_inter_AB_5 = intersection_line_circle(b0_AB, b1_AB,  data_circle$y0[1], data_circle$x0[1], data_circle$r0[1], coordinate="x1"),
         X2_inter_AB_5 = intersection_line_circle(b0_AB, b1_AB, data_circle$y0[1], data_circle$x0[1], data_circle$r0[1], coordinate="x2"), 
         inter_status_AB_5 = intersection.status(intersection_line_circle(b0_AB, b1_AB,  data_circle$y0[1], data_circle$x0[1], data_circle$r0[1] , coordinate="x1"), 
                                                 intersection_line_circle(b0_AB, b1_AB, data_circle$y0[1], data_circle$x0[1], data_circle$r0[1], coordinate="x2")),
         # for AT line
         X1_inter_AT_5 =intersection_line_circle(b0_AT, b1_AT, data_circle$y0[1], data_circle$x0[1], data_circle$r0[1], coordinate="x1"),
         X2_inter_AT_5 =intersection_line_circle(b0_AT, b1_AT, data_circle$y0[1], data_circle$x0[1], data_circle$r0[1], coordinate="x2"), 
         inter_status_AT_5 = intersection.status(intersection_line_circle(b0_AT, b1_AT, data_circle$y0[1], data_circle$x0[1], data_circle$r0[1], coordinate="x1"), 
                                                 intersection_line_circle(b0_AT, b1_AT, data_circle$y0[1], data_circle$x0[1], data_circle$r0[1], coordinate="x2")),
         # for BT line
         X1_inter_BT_5 =intersection_line_circle(b0_BT, b1_BT, data_circle$y0[1], data_circle$x0[1], data_circle$r0[1], coordinate="x1"),
         X2_inter_BT_5 =intersection_line_circle(b0_BT, b1_BT, data_circle$y0[1], data_circle$x0[1], data_circle$r0[1], coordinate="x2"), 
         inter_status_BT_5 = intersection.status(intersection_line_circle(b0_BT, b1_BT, data_circle$y0[1], data_circle$x0[1], data_circle$r0[1], coordinate="x1"), 
                                                 intersection_line_circle(b0_BT, b1_BT, data_circle$y0[1], data_circle$x0[1], data_circle$r0[1], coordinate="x2"))) %>%
  # y intercept with 5m circle: insert x of intercept with circle in equation of line
  # AB line 
  mutate(Y1_inter_AB_5 = intersection_line_circle(b0_BT, b1_BT, data_circle$y0[1], data_circle$x0[1], data_circle$r0[1], coordinate="y1"), 
         Y2_inter_AB_5 = intersection_line_circle(b0_BT, b1_BT, data_circle$y0[1], data_circle$x0[1], data_circle$r0[1], coordinate="y2"), 
         # AT line 
         Y1_inter_AT_5 = intersection_line_circle(b0_AT, b1_AT, data_circle$y0[1], data_circle$x0[1], data_circle$r0[1], coordinate="y1"), 
         Y2_inter_AT_5 = intersection_line_circle(b0_AT, b1_AT, data_circle$y0[1], data_circle$x0[1], data_circle$r0[1], coordinate="y2"), 
         # BT line 
         Y1_inter_BT_5 = intersection_line_circle(b0_BT, b1_BT, data_circle$y0[1], data_circle$x0[1], data_circle$r0[1], coordinate="y1"), 
         Y2_inter_BT_5 = intersection_line_circle(b0_BT, b1_BT, data_circle$y0[1], data_circle$x0[1], data_circle$r0[1], coordinate="y2")) %>%
# N.2. edge form 1 (AB line) intersection of A to center and B to center lines with 60m radius --------

# ----- N. 0.5.8.2. for line with turning point  ---------------------------------------------------------



forest_edges_HBI.man %>% 
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
  mutate(#alpha_AB_x1_x2 = azi_C_AB_inter_1 -azi_C_AB_inter_2 ,
    #beta_AB_x1_x2 = azi_C_AB_inter_2 -azi_C_AB_inter_1 ,
    #lower_azi_AB_inter = ifelse(azi_C_AB_inter_1 < azi_C_AB_inter_2, azi_C_AB_inter_1, azi_C_AB_inter_2),
    #upper_azi_AB_inter = ifelse(azi_C_AB_inter_1 < azi_C_AB_inter_2, azi_C_AB_inter_2, azi_C_AB_inter_1),
    #lower_azi_AB_stat = ifelse(azi_C_AB_inter_1 < azi_C_AB_inter_2, "x1", "x2"),
    #upper_azi_AB_stat = ifelse(azi_C_AB_inter_1 < azi_C_AB_inter_2, "x2", "x1"),
    # x1 between 200 - 400, x2 between 0 -200
    x_left_stat = left.inter(azi_C_AB_inter_1, azi_C_AB_inter_2, "x1", "x2" ), 
    # select x coordinate on the left side
    x_left_AB_inter_17 = left.inter(azi_C_AB_inter_1, azi_C_AB_inter_2, X1_inter_AB_17, X2_inter_AB_17),
    # select y coordinate on the left side
    y_left_AB_inter_17 =  left.inter(azi_C_AB_inter_1, azi_C_AB_inter_2, Y1_inter_AB_17, Y2_inter_AB_17),
    # select x coordinate on the right side
    x_right_AB_inter_17 = right.inter(azi_C_AB_inter_1, azi_C_AB_inter_2, X1_inter_AB_17, X2_inter_AB_17),
    # select y coordinate on the right side
    y_right_AB_inter_17 =  right.inter(azi_C_AB_inter_1, azi_C_AB_inter_2, Y1_inter_AB_17, Y2_inter_AB_17),
    angle_AB_inter_17_gon = ifelse((azi_C_AB_inter_1 - azi_C_AB_inter_2) <0, (azi_C_AB_inter_1 - azi_C_AB_inter_2)*(-1), azi_C_AB_inter_1 - azi_C_AB_inter_2), 
    angle_AC_BC_grad = angle_AB_inter_17_gon*0.9,
    azi_start_AB_inter_17 = left.inter(azi_C_AB_inter_1, azi_C_AB_inter_2, azi_C_AB_inter_1, azi_C_AB_inter_2),
    azi_end_AB_inter_17 = right.inter(azi_C_AB_inter_1, azi_C_AB_inter_2, azi_C_AB_inter_1, azi_C_AB_inter_2),
    azi_end_AB_inter_17_sum = azi_start_AB_inter_17 + angle_AB_inter_17_gon,
    azi_end_AB_inter_17_corr = ifelse(azi_end_AB_inter_17 > 400, azi_end_AB_inter_17 -400, azi_end_AB_inter_17), 
    x1_inter_AC_60 = intersection_c_lx1(intercept(0,0, X_A, Y_A), 
                                        slope(0,0, X_A, Y_A), 
                                        0, 0, data_circle$rmax[3]*2),
    x2_inter_AC_60 = intersection_c_lx2(intercept(0,0, X_A, Y_A),
                                        slope(0,0, X_A, Y_A),
                                        0, 0, data_circle$rmax[3]*2),
    y1_inter_AC_60 = l(intercept(0,0, X_A, Y_A), 
                       slope(0,0, X_A, Y_A), 
                       x1_inter_AC_60),
    y2_inter_AC_60 = l(intercept(0,0, X_A, Y_A), 
                       slope(0,0, X_A, Y_A), 
                       x2_inter_AC_60),
    X_inter_AC_triangle_60 = select.inter.for.triangle(0,
                                                       data_circle$rmax[3]*2,
                                                       azi(x1_inter_AC_60, y1_inter_AC_60, 0, 0), 
                                                       azi(x2_inter_AC_60, y2_inter_AC_60, 0, 0),
                                                       azi(X_A, Y_A, 0, 0), 
                                                       x1_inter_AC_60, 
                                                       x2_inter_AC_60), 
    Y_inter_AC_triangle_60 = l(intercept(0,0, X_A, Y_A), 
                               slope(0,0, X_A, Y_A), 
                               X_inter_AC_triangle_60),
    x1_inter_BC_60 = intersection_c_lx1(intercept(0,0, X_B, Y_B), 
                                        slope(0,0, X_B, Y_B), 
                                        0, 0, data_circle$rmax[3]*2),
    x2_inter_BC_60 = intersection_c_lx2(intercept(0,0, X_B, Y_B),
                                        slope(0,0, X_B, Y_B),
                                        0, 0, data_circle$rmax[3]*2),
    y1_inter_BC_60 = l(intercept(0,0, X_B, Y_B), 
                       slope(0,0, X_B, Y_B), 
                       x1_inter_BC_60),
    y2_inter_BC_60 = l(intercept(0,0, X_B, Y_B), 
                       slope(0,0, X_B, Y_B), 
                       x2_inter_BC_60),
    X_inter_BC_triangle_60 = select.inter.for.triangle(0, # tdist
                                                       data_circle$rmax[3]*2, # cro
                                                       azi(x1_inter_BC_60, y1_inter_BC_60, 0, 0), # azi inter 1,
                                                       azi(x2_inter_AC_60, y2_inter_BC_60, 0, 0), # azi inter 2
                                                       azi(X_A, Y_A, 0, 0),     # azi center 
                                                       x1_inter_BC_60, x2_inter_AC_60),
    Y_inter_BC_triangle_60 = l(intercept(0,0, X_B, Y_B), 
                               slope(0,0, X_B, Y_B),
                               X_inter_BC_triangle_60),
    x1_inter_AB_60 = intersection_c_lx1(b0_AB, b1_AB, 0, 0, data_circle$rmax[3]*2),
    x2_inter_AB_60 = intersection_c_lx2(b0_AB, b1_AB, 0, 0, data_circle$rmax[3]*2), 
    y1_inter_AB_60 = l(b0_AB, b1_AB, x1_inter_AB_60), 
    y2_inter_AB_60 = l(b0_AB, b1_AB, x2_inter_AB_60))





# N. 0.5.14 select he right interception according to azuimute old way---------------------------------------------------------------



# this way is not stored in a funnxtion to make the selection easier
forest_edges_HBI.man %>% 
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
       X_inter_AT_triangle_60_old = case_when(T_dist <= 1784 &  azi_T_AT_inter_1 == azi_T_A ~ intersection_c_lx1(b0_AT,b1_AT,0,0, data_circle$rmax[3]*2),
                                              T_dist <= 1784 & azi_T_AT_inter_2 == azi_T_A ~  intersection_c_lx2(b0_AT, b1_AT, 0, 0,  data_circle$rmax[3]*2),
                                              T_dist > 1784 & dist_T_AT_inter_1 > dist_T_AT_inter_2 ~ intersection_c_lx1(b0_AT,b1_AT,0,0, data_circle$rmax[3]*2), 
                                              T_dist > 1784 & dist_T_AT_inter_2 > dist_T_AT_inter_1 ~ intersection_c_lx2(b0_AT,b1_AT,0,0, data_circle$rmax[3]*2), 
                                              TRUE ~ NA ),
       X_inter_AT_triangle_60 = inter.for.triangle(b0_AT, b1_AT, 0, 0, data_circle$rmax[3]*2, X_A, Y_A, X_T, Y_T, coordinate = "x"),
       X_inter_BT_triangle_60_old = case_when(T_dist <= 1784 & azi_T_BT_inter_1 == azi_T_B ~ intersection_c_lx1(b0_BT,b1_BT, 0, 0, data_circle$rmax[3]*2),
                                              T_dist <= 1784 & azi_T_BT_inter_2 == azi_T_B ~  intersection_c_lx2(b0_BT, b1_BT, 0, 0, data_circle$rmax[3]*2),
                                              T_dist > 1784 & dist_T_BT_inter_1 > dist_T_BT_inter_2 ~ intersection_c_lx1(b0_BT,b1_BT,0,0, data_circle$rmax[3]*2), 
                                              T_dist > 1784 & dist_T_BT_inter_2 > dist_T_BT_inter_1 ~ intersection_c_lx2(b0_BT,b1_BT,0,0, data_circle$rmax[3]*2), 
                                              TRUE ~ NA), 
       X_inter_BT_triangle_60 = inter.for.triangle(b0_BT, b1_BT, 0, 0, data_circle$rmax[3]*2, X_B, Y_B, X_T, Y_T, coordinate = "x")) %>% 
  # calcualte y to the x that lie in the same direction then the second point on the line, if turning points lies witin circle and lines "reach out"
  mutate(Y_inter_AT_triangle_60_old = l(b0_AT, b1_AT, X_inter_AT_triangle_60),  
         Y_inter_AT_triangle_60 = inter.for.triangle(b0_AT, b1_AT, 0, 0, data_circle$rmax[3]*2, X_A, Y_A, X_T, Y_T, coordinate = "y"),
         Y_inter_BT_triangle_60_old = l(b0_BT, b1_BT, X_inter_BT_triangle_60), 
         Y_inter_BT_triangle_60 = inter.for.triangle(b0_BT, b1_BT, 0, 0, data_circle$rmax[3]*2, X_B, Y_B, X_T, Y_T, coordinate = "y")) 

# N. 0.5.14. select the coordiantes/ azimute of more left intersection  ----------
left.inter <- function(azi_1, azi_2, coord_1, coord_2){
  coordinate <- ifelse(azi_1 >= 200 & azi_1 <= 400 &
                         azi_2 >= 0 & azi_2 <= 200 & 
                         azi_1 > azi_2 | 
                         # x1 & x2 between 200 - 400 
                         azi_1 >= 200 & azi_1 <= 400 &
                         azi_2 >= 200 & azi_2 <= 400 & 
                         azi_1 < azi_2 |
                         # x1 & x2 between 0 - 200
                         azi_1 >= 0 & azi_1 <= 200 &
                         azi_2 >= 0 & azi_2 <= 200 &
                         azi_1 < azi_2,  coord_1,
                       ifelse(
                         # x1 between 0 - 200  & x2 between 200 - 400 
                         azi_1 >= 0 & azi_1 <= 200 &
                           azi_2 >= 200 & azi_2 <= 400 &
                           azi_1 < azi_2, coord_2,
                         ifelse(is.na(azi_1) | is.na(azi_2), NA, coord_2
                         )
                       )
  );
  return(coordinate)
}


right.inter <- function(azi_1, azi_2, coord_1, coord_2){
  coordinate <- ifelse(azi_1 >= 200 & azi_1 <= 400 &
                         azi_2 >= 0 & azi_2 <= 200 & 
                         azi_1 > azi_2 | 
                         # x1 & x2 between 200 - 400 
                         azi_1 >= 200 & azi_1 <= 400 &
                         azi_2 >= 200 & azi_2 <= 400 & 
                         azi_1 < azi_2 |
                         # x1 & x2 between 0 - 200
                         azi_1 >= 0 & azi_1 <= 200 &
                         azi_2 >= 0 & azi_2 <= 200 &
                         azi_1 < azi_2,  coord_2,
                       ifelse(
                         # x1 between 0 - 200  & x2 between 200 - 400 
                         azi_1 >= 0 & azi_1 <= 200 &
                           azi_2 >= 200 & azi_2 <= 400 &
                           azi_1 < azi_2, coord_1,
                         ifelse(is.na(azi_1) | is.na(azi_2), NA, coord_1
                         )
                       )
  );
  return(coordinate)
}



#N. assigning tree status according to frequency of the groups per plot
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


# N. calcualte the edge area  ---------------------------------------------

forest_edges_HBI.man <- 
  forest_edges_HBI.man %>% 
  # X & y coordinate of intersection with 17m circle that corresponds with the azimute between from T to A or T to B to be sure we select the intersection on the "ricght side"
  mutate(X_inter_AT_17_triangle = inter.for.triangle(b0_AT, b1_AT, data_circle$x0[3], data_circle$y0[3], data_circle$r0[3], X_A, Y_A, X_T, Y_T, coordinate = "x"), 
         X_inter_BT_17_triangle = inter.for.triangle(b0_BT, b1_BT, data_circle$x0[3], data_circle$y0[3], data_circle$r0[3], X_B, Y_B, X_T, Y_T, coordinate = "x"),
         Y_inter_AT_17_triangle = inter.for.triangle(b0_AT, b1_AT, data_circle$x0[3], data_circle$y0[3], data_circle$r0[3], X_A, Y_A, X_T, Y_T, coordinate = "y"), 
         Y_inter_BT_17_triangle = inter.for.triangle(b0_BT, b1_BT, data_circle$x0[3], data_circle$y0[3], data_circle$r0[3], X_B, Y_B, X_T, Y_T, coordinate = "y"),
         # x & y coordinate of intersection with 12m cirle that correspondes with azimute between turning point and second point on line
         X_inter_AT_12_triangle = inter.for.triangle(b0_AT, b1_AT, data_circle$x0[2], data_circle$y0[2], data_circle$r0[2], X_A, Y_A, X_T, Y_T, coordinate = "x"), 
         X_inter_BT_12_triangle = inter.for.triangle(b0_BT, b1_BT, data_circle$x0[2], data_circle$y0[2], data_circle$r0[2], X_B, Y_B, X_T, Y_T, coordinate = "x"),
         Y_inter_AT_12_triangle = inter.for.triangle(b0_AT, b1_AT, data_circle$x0[2], data_circle$y0[2], data_circle$r0[2], X_A, Y_A, X_T, Y_T, coordinate = "y"), 
         Y_inter_BT_12_triangle = inter.for.triangle(b0_BT, b1_BT, data_circle$x0[2], data_circle$y0[2], data_circle$r0[2], X_B, Y_B, X_T, Y_T, coordinate = "y"),
         # x & y coordinate of intersection with 5m cirle that correspondes with azimute between turning point and second point on line
         X_inter_AT_5_triangle = inter.for.triangle(b0_AT, b1_AT, data_circle$x0[1], data_circle$y0[1], data_circle$r0[1], X_A, Y_A, X_T, Y_T, coordinate = "x"), 
         X_inter_BT_5_triangle = inter.for.triangle(b0_BT, b1_BT, data_circle$x0[1], data_circle$y0[1], data_circle$r0[1], X_B, Y_B, X_T, Y_T, coordinate = "x"),
         Y_inter_AT_5_triangle = inter.for.triangle(b0_AT, b1_AT, data_circle$x0[1], data_circle$y0[1], data_circle$r0[1], X_A, Y_A, X_T, Y_T, coordinate = "y"), 
         Y_inter_BT_5_triangle = inter.for.triangle(b0_BT, b1_BT, data_circle$x0[1], data_circle$y0[1], data_circle$r0[1], X_B, Y_B, X_T, Y_T, coordinate = "y")) %>%  
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
         edge_area_eform_5_ha = tot.edge.A(edge_area_ABC_AC_5_ha, edge_area_ABC_BC_5_ha), 
         edge_area_eform_12_ha = tot.edge.A(edge_area_ABC_AC_12_ha, edge_area_ABC_BC_12_ha),
         edge_area_eform_17_ha = tot.edge.A(edge_area_ABC_AC_17_ha, edge_area_ABC_BC_17_ha)) 

# N.0.5.8. find the treee edge status -----------------------------------------------------------
# combining tree status assesment in 1 statement
tree.status.1 <- function(
  # intersection status
  edge_form,
  # finding smaller side of cirlce
  x1.inter.AB, x2.inter.AB, y1.inter.AB, y2.inter.AB, c.x0, c.y0, c.r017, l.AB.b0, l.AB.b1, # x1, x2, y1, y2 are the intersection cooridnates of the line 
  # implicit funtion with tree coordinates
  x.tree, y.tree, 
  # select the intersection coordinates for the triangle on AT line
  x1.inter.AT, y1.inter.AT, x2.inter.AT, y2.inter.AT, x.a, y.a, x.t, y.t ,l.AT.b0, l.AT.b1, c.r060, #c.x0, c.y0,  
  x1.inter.BT, y1.inter.BT, x2.inter.BT, y2.inter.BT, x.b, y.b, l.BT.b0, l.BT.b1 #x.t, y.t , # c.x0, c.y0, c.r060
){
  
  ## For edge form == 1 --> straight line 
  if(edge_form == 1){
    # check out the intersection status of the respective plot of the  tree
    i_status.AB <-   ifelse(is.na(x1.inter.AB) & is.na(x2.inter.AB), " no I",      # if 0 solutions
                            ifelse(!is.na(x1.inter.AB) & !is.na(x2.inter.AB) & x1.inter.AB == x2.inter.AB, "one I",            # if 1 solution
                                   ifelse(x1.inter.AB != x2.inter.AB, "two I")))      # so if the edge for is 1 and there are 2 interseections of the line with the respective circle 
    
    # identfy shorter side of the cirlce separeted by the AB line
    x_m_line = (x1 - x2)/2
    y_m_line = (y1 - y2)/2
    # calculate the parameters of the equation between the middle of the line and the centre of the circle
    b1_MC = slope(c.x0, c.y0, x_m_line, y_m_line)
    b0_MC = intercept(c.x0, c.y0, x_m_line, y_m_line)
    # calcualte the x corrdiante of the interception of the line between M and the centre of the cirle and the circle at the given radius
    X1_inter_MC = intersection_c_lx1(b0_MC, b1_MC, c.x0, c.y0, c.r017) 
    X2_inter_MC = intersection_c_lx2(b0_MC, b1_MC, c.x0, c.y0, c.r017)
    # insert the intersection x corodinate in the line function to get the respective y coordinate
    y1_inter_MC = l(b0_MC, b1_MC, X1_inter_MC) 
    y2_inter_MC = l(b0_MC, b1_MC, X2_inter_MC)
    # distance between the intersections (inter_MC_1, inter_MC_2) to M on the line 
    dist_C_inter_1_MC = distance(X1_inter_MC, y1_inter_MC, x_m_line, y_m_line)
    dist_C_inter_2_MC = distance(X2_inter_MC, y2_inter_MC, x_m_line, y_m_line) 
    # find the x and y coordinate of the intersection on the shorter side , which is the side to exlcude from the plot 
    X_inter_MC_shorter_side = ifelse(dist_C_inter_1_MC < dist_C_inter_2_MC, X1_inter_MC, X2_inter_MC) 
    Y_inter_MC_shorter_side = ifelse(dist_C_inter_1_MC < dist_C_inter_2_MC, y1_inter_MC, y2_inter_MC)
    # insert coordinates that are for sure on the smaller side of the two halves of the circle into the implicit equation: 
    Y_MC_implicit = l.b0  + l.b1 * X_inter_MC_shorter_side - Y_inter_MC_shorter_side
    # Y_stat_MC_implicit = middle.point.to.line(x1, x2, y1, y2, c.x0, c.y0, c.r0, l.AB.b0, l.AB.b1);
    # calculate result of implicit function of AB line with tree coordinates
    Y_implicit_status_tree_line = l.AB.b0  + l.AB.b1 * x.tree - y.tree
    # assigne tree status depending on shorter/ longer side of circle 
    tree_stat_edge_1 = ifelse(i_status.AB == "two I" & Y_MC_implicit > 0 & Y_implicit_status_tree_line > 0 |
                                i_status.AB == "two I" & Y_MC_implicit < 0 &  Y_implicit_status_tree_line < 0 | 
                                i_status.AB == "two I",  
                              "A", "B")
    return(tree_stat_edge_1)
  }
  
  else if(edge_form == 2){
    # find intersection on the "right" side (with the same "direction as the line from T to the respective point) 
    # and calcualte the respective intersection coordinate (x1 vs. x2) for a cricle with a radius = 60m
    # AT
    # select the intersection coordinates for the triangle on AT line
    x.AT.inter.triangle.60 = ifelse(t.dist <= c.ro & azi(x1.inter.AT, y1.inter.AT, x.t, y.t) == azi(x.a, y.a, x.t, y.t), intersection_c_lx1(l.AT.b0, l.AT.b1, c.x0, c.y0, c.r060), 
                                    ifelse(t.dist <= c.ro & azi(x2.inter.AT, y2.inter.AT, x.t, y.t) == azi(x.a, y.a, x.t, y.t), intersection_c_lx2(l.AT.b0, l.AT.b1, c.x0, c.y0, c.r060),
                                           ifelse(t.dist > c.ro & distance(x1.inter.AT, y1.inter.AT, x.t, y.t) > distance(x2.inter.AT, y2.inter.AT, x.t, y.t), intersection_c_lx1(l.AT.b0, l.AT.b1, c.x0, c.y0, c.r060), 
                                                  ifelse(t.dist > c.ro & distance(x2.inter.AT, y2.inter.AT, x.t, y.t) > distance(x1.inter.AT, y1.inter.AT, x.t, y.t), intersection_c_lx2(l.AT.b0, l.AT.b1, c.x0, c.y0, c.r060), NA))))
    # calculate y for triangle
    y.AT.inter.triangle.60 = l.AT.b0 + l.AT.b1*x.AT.inter.triangle.60
    #BT 
    # select the intersection coordinates for the triangle on BT line
    x.BT.inter.triangle.60 = ifelse(t.dist <= c.ro & azi(x1.inter.BT, y1.inter.BT, x.t, y.t) == azi(x.b, y.b, x.t, y.t), intersection_c_lx1(l.BT.b0, l.BT.b1, c.x0, c.y0, c.r060), 
                                    ifelse(t.dist <= c.ro & azi(x2.inter.BT, y2.inter.BT, x.t, y.t) == azi(x.b, y.b, x.t, y.t), intersection_c_lx2(l.BT.b0, l.BT.b1, c.x0, c.y0, c.r060),
                                           ifelse(t.dist > c.ro & distance(x1.inter.BT, y1.inter.BT, x.t, y.t) > distance(x2.inter.BT, y2.inter.BT, x.t, y.t), intersection_c_lx1(l.BT.b0, l.BT.b1, c.x0, c.y0, c.r060), 
                                                  ifelse(t.dist > c.ro & distance(x2.inter.BT, y2.inter.BT, x.t, y.t) > distance(x1.inter.BT, y1.inter.BT, x.t, y.t), intersection_c_lx2(l.BT.b0, l.BT.b1, c.x0, c.y0, c.r060), NA))))
    # calculate y for triangle
    y.BT.inter.triangle.60 = l.BT.b0 + l.BT.b1*x.BT.inter.triangle.60
    
    # check if tree is located inside triangle: Beyericentrig triangle function
    a = ((x.tree - x.t)*(y.BT.inter.triangle.60 - y.t) + (x.t - x.BT.inter.triangle.60)*(y.tree)) / ((y.BT.inter.triangle.60 - y.t)*(x.AT.inter.triangle.60 - x.t) + (x.t - x.BT.inter.triangle.60)*(y.AT.inter.triangle.60 - y.t))
    b = ((x.tree - x.t)*(y.t - y.AT.inter.triangle.60) + (x.AT.inter.triangle.60 - x.t)*(y.tree - y.t)) / ((y.BT.inter.triangle.60 - y.t)*(x.AT.inter.triangle.60 - x.t) + (x.t - x.BT.inter.triangle.60)*(y.AT.inter.triangle.60 - y.t))
    c = 1 - a - b
    in.or.out = ifelse(0 <= a & a <= 1 & 0 <= b  & b <= 1 & 0 <= c & c <= 1, "B", "A")
    # B = out = point is inside triangle, so outside plot
    # A = in =  point is outside triangle, so inside plot
    return(in.or.out)} else {return(NA)};
  
  
}

# tree status test 
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
                     X_inter_AT_triangle_60, X_inter_BT_triangle_60, Y_inter_AT_triangle_60, Y_inter_BT_triangle_60),
            by = c("plot_ID", "e_ID", "e_type", "e_form")) %>% 
  # calculate the Y of the edge for the x of the tree
  # new approach by Johanna Garthe
  # insert y and x of tree in implizite function of line function: 0 = a*x + b - y --> if result > 0 --> group 1, if result <0 --> group 2, if result = 0 --> group 0
  mutate(Y_AB_t = l(b0_AB, b1_AB, X_tree),    # calcualte y of function at the x of the tree 
         dist_y_Xtree = distance(X_tree, Y_AB_t, 0, 0),
         Y_AB_t_implicit = b0_AB  + b1_AB *X_tree - Y_tree, 
         Y_AT_t_implicit = b0_AT + b1_AT *X_tree - Y_tree,
         Y_BT_t_implicit = b0_BT  + b1_BT *X_tree - Y_tree) %>%
  # assign a tree-edge-status that calls trees with the same result as the implicit function of the middlepoint-center-line 
  # intersction point on the shorter side of the middlepoint center line
  #if there are two intersection and the Y inter status of 
  # middle.point.to.line is a function that determines if the result of an implicit function has to be positive or negative to be outside the line 
  # thus if the edge is a line with two intersection we asssign the 
  mutate(t_AB_status = ifelse(e_form == 1, p.site.line(X1_inter_AB_17, X2_inter_AB_17, Y1_inter_AB_17, Y2_inter_AB_17, 0, 0,  data_circle$r0[3], b0_AB, b1_AB, X_tree, Y_tree), NA), 
         t_AT_status_test = p.site.triangle(X1_inter_AT_17, X2_inter_AT_17,  Y1_inter_AT_17,  Y2_inter_AT_17,
                                            0, 0,  data_circle$r0[3],  b0_AT, b1_AT,
                                            X_inter_AT_triangle_60,  X_inter_BT_triangle_60,  X_T, 
                                            Y_inter_AT_triangle_60, Y_inter_BT_triangle_60,  Y_T, 
                                            X_tree, Y_tree),
         t_BT_status_test =  p.site.triangle(X1_inter_BT_17, X2_inter_BT_17, Y1_inter_BT_17, Y2_inter_BT_17, 
                                             0, 0,  data_circle$r0[3], b0_BT, b1_BT,
                                             X_inter_AT_triangle_60, X_inter_BT_triangle_60, X_T, 
                                             Y_inter_AT_triangle_60, Y_inter_BT_triangle_60, Y_T, 
                                             X_tree, Y_tree),
         t_ABT_status_test = case_when(inter_status_AT_17 == "two I" & inter_status_BT_17 == "two I" ~ p.in.triangle(X_inter_AT_triangle_60, X_inter_BT_triangle_60, X_T, Y_inter_AT_triangle_60, Y_inter_BT_triangle_60, Y_T, X_tree, Y_tree),
                                       # if only one arm of the triangle crosses the circle/ has two intersections with the circle, use the respective arm as a line and assign tree status according to line procedure 
                                       inter_status_AT_17 != "two I" & inter_status_BT_17 == "two I" ~ t_BT_status_test, 
                                       inter_status_AT_17 == "two I" & inter_status_BT_17 != "two I" ~ t_AT_status_test,
                                       # if non of the arms touches the circle, assign all trees inside the circle to one group
                                       inter_status_AT_17 != "two I" & inter_status_BT_17 != "two I" ~ "A", 
                                       TRUE ~ NA))




# N. assign correct adge area to trees depending on DBH and CCS -----------

trees_and_edges %>%
  mutate(DBH_cm = ifelse(DBH_h_cm != 130, (D_mm*(1.0+(0.0011*(DBH_h_cm-130))))/10, D_mm/10)) %>% 
  mutate(edge_A_method = edge.A.site.triangle.method(e_form,DBH_cm,  X_A, X_B, X_T, Y_A, Y_B, Y_T, T_dist, t_status_AB_ABT_test)) %>% 
  mutate(plot_A = case_when(
    # for 5m circle     
    ## total circle   
    # edge_area_eform_5_ha contains the total edge area per edge from, so for form 2 it´l include both sides of the area that is cut from the circle by the tirangles arms
    # if there is no edge at the plot calcualte the whole circle area as the plot area
    DBH_cm >= 7 & DBH_cm < 10 & is.na(e_form)|
      # or if the plot has an edge form but only one tree status and non of the lines are intersecting the circle calcualte the whole circle area as the plot area
      !is.na(e_form) & t_status_AB_ABT %in% c("A", "B") & DBH_cm >= 7 & DBH_cm < 10 & inter_status_BT_5 != "two I" & inter_status_AT_5 != "two I" & inter_status_AB_5 != "two I"  ~ (c_A(data_circle$r0[1])/10000)/10000,
    ## circle - circle segment 
    # if the edge form is 1 and the tree lies inside the plot (status == A) and the circle is cut by the edge with 2 intersections, deduct the edge area from the main area
    t_status_AB_ABT == "A" & DBH_cm >= 7 & DBH_cm < 10 & e_form == "1" & inter_status_AB_5 == "two I"|
      # or if the edge for is 2 and T lies outside the circle and both arms of the triangle are intersecting the circle, 
      # deduct the area of the circle intersections from the total circle area for tree status B, because B means the tree is inside the triangle, which covers a the area between the two intersection lines, while the A area
      # is the space cut from the circle by the triangle arms that reach in
      t_status_AB_ABT == "B" & DBH_cm >= 7 & DBH_cm < 10 & e_form == "2" & T_dist > data_circle$r0[1] & inter_status_AT_5 == "two I" & inter_status_BT_5 == "two I"|
      # or if e_form == 2 and T lies outside the circle, trees with status B are assigned to the area of the cirlce- circle segment
      t_status_AB_ABT == "A" & DBH_cm >= 7 & DBH_cm < 10 & e_form == "2" & T_dist <= data_circle$r0[1] ~ ((c_A(data_circle$r0[1])/10000)/10000 - edge_area_eform_5_ha),
    ## circle segment    
    # if the edge form is 1 and the trees lies outside the plot (status == B) and the circle is cut by the edge with 2 intersections, use the edge area, cause for AB lines we always assign the B status to the smaller side of the circle
    t_status_AB_ABT == "B" & DBH_cm >= 7 & DBH_cm < 10 & e_form == "1" & inter_status_AB_5 == "two I"|
      # or if the edge for is 2 and T lies outside the circle and both arms of the triangle are intersecting the circle, 
      # assign the area of the circle intersections for trees with status A because A means the tree is outside the triangle, 
      # so the A area is the space cut from the circle by the triangle arms that reach in
      t_status_AB_ABT == "A" & DBH_cm >= 7 & DBH_cm < 10 & e_form == "2" & T_dist > data_circle$r0[1] & inter_status_AT_5 == "two I" & inter_status_BT_5 == "two I"|
      # if edge_form == 2 and T lies inside the circle trees with status B are assigned to the area of the circle segment
      t_status_AB_ABT == "B" & DBH_cm >= 7 & DBH_cm < 10 & e_form == "2" & T_dist <= data_circle$r0[1] ~ edge_area_eform_5_ha,
    ## identify.edge.area function   
    # if the edge form is 2 and T lies outside the circle and only one side of the triangle is intersecting the circle apply the identify.edge.area function 
    # this function is adapted to the tree status, so we don´t have to specify it here
    t_status_AB_ABT %in% c("A", "B") & DBH_cm >= 7 & DBH_cm < 10 & e_form == "2" & T_dist > data_circle$r0[1] & inter_status_AT_5 == "two I" & inter_status_BT_5 != "two I" ~ identify.edge.area(X1_inter_AT_5, X2_inter_AT_5, Y1_inter_AT_5, Y2_inter_AT_5, 0, 0, data_circle$r0[1], b0_AT, b1_AT, X_inter_AT_triangle_60, X_inter_BT_triangle_60, X_T, Y_inter_AT_triangle_60, Y_inter_BT_triangle_60, Y_T, edge_area_eform_5_ha, (c_A(data_circle$r0[1])/10000)/10000, t_status_AB_ABT),
    t_status_AB_ABT %in% c("A", "B") & DBH_cm >= 7 & DBH_cm < 10 & e_form == "2" & T_dist > data_circle$r0[1] & inter_status_BT_5 == "two I" & inter_status_AT_5 != "two I" ~ identify.edge.area(X1_inter_BT_5, X2_inter_BT_5, Y1_inter_BT_5, Y2_inter_BT_5, 0, 0, data_circle$r0[1], b0_BT, b1_BT, X_inter_AT_triangle_60, X_inter_BT_triangle_60, X_T, Y_inter_AT_triangle_60, Y_inter_BT_triangle_60, Y_T, edge_area_eform_5_ha, (c_A(data_circle$r0[1])/10000)/10000, t_status_AB_ABT),
    
    # for 12m circle: trees from 10 to 30 cm DBH     
    ## total circle   
    # if there is no edge at the plot calcualte the whole circle area as the plot area
    DBH_cm >= 10 & DBH_cm < 30 & is.na(e_form)|
      # or if the plot has an edge form but only one tree status and non of the lines are intersecting the circle calcualte the whole circle area as the plot area
      !is.na(e_form) & t_status_AB_ABT %in% c("A", "B") & DBH_cm >= 10 & DBH_cm < 30 & inter_status_BT_12 != "two I" & inter_status_AT_12 != "two I" & inter_status_AB_12 != "two I"  ~ (c_A(data_circle$r0[2])/10000)/10000,
    ## circle - circle segment 
    # if the edge form is 1 and the tree lies inside the plot (status == A) and the circle is cut by the edge with 2 intersections, deduct the edge area from the main area
    t_status_AB_ABT == "A" & DBH_cm >= 10 & DBH_cm < 30& e_form == "1" & inter_status_AB_12 == "two I"|
      # or if the edge for is 2 and T lies outside the circle and both arms of the triangle are intersecting the circle, 
      # deduct the area of the circle intersections from the total circle area for tree status B, because B means the tree is inside the triangle, which covers a the area between the two intersection lines, while the A area
      # is the space cut from the circle by the triangle arms that reach in
      t_status_AB_ABT == "B" & DBH_cm >= 10 & DBH_cm < 30 & e_form == "2" & T_dist > data_circle$r0[2] & inter_status_AT_12 == "two I" & inter_status_BT_12 == "two I"|
      # or if e_form == 2 and T lies outside the circle, trees with status B are assigned to the area of the cirlce- circle segment
      t_status_AB_ABT == "A" & DBH_cm >= 10 & DBH_cm < 30 & e_form == "2" & T_dist <= data_circle$r0[2] ~ ((c_A(data_circle$r0[2])/10000)/10000 - edge_area_eform_12_ha),
    ## circle segment    
    # if the edge form is 1 and the trees lies outside the plot (status == B) and the circle is cut by the edge with 2 intersections, use the edge area, cause for AB lines we always assign the B status to the smaller side of the circle
    t_status_AB_ABT == "B" & DBH_cm >= 10 & DBH_cm < 30 & e_form == "1" & inter_status_AB_12 == "two I"|
      # or if the edge for is 2 and T lies outside the circle and both arms of the triangle are intersecting the circle, 
      # assign the area of the circle intersections for trees with status A because A means the tree is outside the triangle, 
      # so the A area is the space cut from the circle by the triangle arms that reach in
      t_status_AB_ABT == "A" & DBH_cm >= 10 & DBH_cm < 30 & e_form == "2" & T_dist > data_circle$r0[2] & inter_status_AT_12 == "two I" & inter_status_BT_12 == "two I"|
      # if edge_form == 2 and T lies inside the circle trees with status B are assigned to the area of the circle segment
      t_status_AB_ABT == "B" & DBH_cm >= 10 & DBH_cm < 30 & e_form == "2" & T_dist <= data_circle$r0[2] ~ edge_area_eform_12_ha,
    ## identify.edge.area function   
    # if the edge form is 2 and T lies outside the circle and only one side of the triangle is intersecting the circle apply the identify.edge.area function 
    # this function is adapted to the tree status, so we don´t have to specify it here
    t_status_AB_ABT %in% c("A", "B") & DBH_cm >= 10 & DBH_cm < 30 & e_form == "2" & T_dist > data_circle$r0[2] & inter_status_AT_12 == "two I" & inter_status_BT_12 != "two I" ~ identify.edge.area(X1_inter_AT_12, X2_inter_AT_12, Y1_inter_AT_12, Y2_inter_AT_12, 0, 0, data_circle$r0[2], b0_AT, b1_AT, X_inter_AT_triangle_60, X_inter_BT_triangle_60, X_T, Y_inter_AT_triangle_60, Y_inter_BT_triangle_60, Y_T, edge_area_eform_12_ha, (c_A(data_circle$r0[2])/10000)/10000, t_status_AB_ABT),
    t_status_AB_ABT %in% c("A", "B") & DBH_cm >= 10 & DBH_cm < 30 & e_form == "2" & T_dist > data_circle$r0[2] & inter_status_BT_12 == "two I" & inter_status_AT_12 != "two I" ~ identify.edge.area(X1_inter_BT_12, X2_inter_BT_12, Y1_inter_BT_12, Y2_inter_BT_12, 0, 0, data_circle$r0[2], b0_BT, b1_BT, X_inter_AT_triangle_60, X_inter_BT_triangle_60, X_T, Y_inter_AT_triangle_60, Y_inter_BT_triangle_60, Y_T, edge_area_eform_12_ha, (c_A(data_circle$r0[2])/10000)/10000, t_status_AB_ABT),
    
    
    # for 17m circle: trees above 30 cm DBH     
    ## total circle   
    # if there is no edge at the plot calcualte the whole circle area as the plot area
    DBH_cm >= 30 & is.na(e_form)|
      # or if the plot has an edge form but only one tree status and non of the lines are intersecting the circle calcualte the whole circle area as the plot area
      !is.na(e_form) & t_status_AB_ABT %in% c("A", "B") & DBH_cm >= 30 & inter_status_BT_17 != "two I" & inter_status_AT_17 != "two I" & inter_status_AB_17 != "two I"  ~ (c_A(data_circle$r0[3])/10000)/10000,
    ## circle - circle segment 
    # if the edge form is 1 and the tree lies inside the plot (status == A) and the circle is cut by the edge with 2 intersections, deduct the edge area from the main area
    t_status_AB_ABT == "A" & DBH_cm >= 30 & e_form == "1" & inter_status_AB_17 == "two I"|
      # or if the edge for is 2 and T lies outside the circle and both arms of the triangle are intersecting the circle, 
      # deduct the area of the circle intersections from the total circle area for tree status B, because B means the tree is inside the triangle, which covers a the area between the two intersection lines, while the A area
      # is the space cut from the circle by the triangle arms that reach in
      t_status_AB_ABT == "B" & DBH_cm >= 30 & e_form == "2" & T_dist > data_circle$r0[3] & inter_status_AT_17 == "two I" & inter_status_BT_17 == "two I"|
      # or if e_form == 2 and T lies outside the circle, trees with status B are assigned to the area of the cirlce- circle segment
      t_status_AB_ABT == "A" & DBH_cm >= 30 & e_form == "2" & T_dist <= data_circle$r0[3] ~ ((c_A(data_circle$r0[3])/10000)/10000 - edge_area_eform_17_ha),
    ## circle segment    
    # if the edge form is 1 and the trees lies outside the plot (status == B) and the circle is cut by the edge with 2 intersections, use the edge area, cause for AB lines we always assign the B status to the smaller side of the circle
    t_status_AB_ABT == "B" & DBH_cm >= 30 & e_form == "1" & inter_status_AB_17 == "two I"|
      # or if the edge for is 2 and T lies outside the circle and both arms of the triangle are intersecting the circle, 
      # assign the area of the circle intersections for trees with status A because A means the tree is outside the triangle, 
      # so the A area is the space cut from the circle by the triangle arms that reach in
      t_status_AB_ABT == "A" & DBH_cm >= 30 & e_form == "2" & T_dist > data_circle$r0[3] & inter_status_AT_17 == "two I" & inter_status_BT_17 == "two I"|
      # if edge_form == 2 and T lies inside the circle trees with status B are assigned to the area of the circle segment
      t_status_AB_ABT == "B" & DBH_cm >= 30 & e_form == "2" & T_dist <= data_circle$r0[3] ~ edge_area_eform_17_ha,
    ## identify.edge.area function   
    # if the edge form is 2 and T lies outside the circle and only one side of the triangle is intersecting the circle apply the identify.edge.area function 
    # this function is adapted to the tree status, so we don´t have to specify it here
    t_status_AB_ABT %in% c("A", "B") & DBH_cm >= 30 & e_form == "2" & T_dist > data_circle$r0[3] & inter_status_AT_17 == "two I" & inter_status_BT_17 != "two I" ~ identify.edge.area(X1_inter_AT_17, X2_inter_AT_17, Y1_inter_AT_17, Y2_inter_AT_17, 0, 0, data_circle$r0[3], b0_AT, b1_AT, X_inter_AT_triangle_60, X_inter_BT_triangle_60, X_T, Y_inter_AT_triangle_60, Y_inter_BT_triangle_60, Y_T, edge_area_eform_17_ha, (c_A(data_circle$r0[3])/10000)/10000, t_status_AB_ABT),
    t_status_AB_ABT %in% c("A", "B") & DBH_cm >= 30 & e_form == "2" & T_dist > data_circle$r0[3] & inter_status_BT_17 == "two I" & inter_status_AT_17 != "two I" ~ identify.edge.area(X1_inter_BT_17, X2_inter_BT_17, Y1_inter_BT_17, Y2_inter_BT_17, 0, 0, data_circle$r0[3], b0_BT, b1_BT, X_inter_AT_triangle_60, X_inter_BT_triangle_60, X_T, Y_inter_AT_triangle_60, Y_inter_BT_triangle_60, Y_T, edge_area_eform_17_ha, (c_A(data_circle$r0[3])/10000)/10000, t_status_AB_ABT),
    TRUE ~ NA))




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






################################ !!!!!!!!!!  FOREST EDGES OLD SCRIPT !!!!!! ##################################### -----------------------------------------



# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the peat land soil inventory
# heiht of living trees  

# ----- F. 0. SETUP ---------------------------------------------------------------
# ----- F. 0.1. Packages  ---------------------------------------------------------
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


# ----- F.0.2. library   ---------------------------------------------------------
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

# ----- F.0.4 data import -------------------------------------------------------
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


# ----- F.0.6 harmonising column names & structure  -------------------------
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


# ----- F.0.5 functions ---------------------------------------------------------------
# ---- F. 0.5.1. classes ---------------------------------------------------------------
# area of a circle
c_A <- function(r){
  circle_area <- r^2*pi;
  return(circle_area)
}
# ----- F. 0.5.1.1. DBH class ----------------------------------------------------------
DBH_c_function <- function(dbh){
  # create label for diameter classes according to BZE3 Bestandesaufnahmeanleitung
  labs_DBH <- c(seq(5, 55, by = 5)) ; 
  DBH_c <- cut(as.numeric(dbh),                               # cut the diameter
               breaks = c(seq(5, 55, by = 5), Inf),  # in sequences of 5
               labels = labs_DBH,                    # and label it according to labs (1.4.1)
               right = FALSE);
  return(DBH_c)
}

# ----- F.0.5.1.2. age class ----------------------------------------------------------
# defining age classes from 1 to 160 in steps of 20
# this is a preparation fot the comparison with carbon stocks calcualted by te
labs_age <- c(seq(1, 180, by = 20))



# ----- <f.0.5.3. coordinate functions ---------------------------------------
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



# ----- F.0.5.4. slope line -------------------------------------------------

slope <- function(x1, y1, x2, y2){
  b1 = (y2 - y1)/(x2 - x1);
  return(b1)
}

# ----- F.0.5.5. intercept y axis line -------------------------------------------------

intercept <- function(x1, y1, slope){
  # resolve line function towards b0 after inserting known coordinates and slope
  # Y_A = b1_AB*X_A + b0_AB | (-b1_AB*X_A) 
  # Y_A - b1_AB*X_A = b0_AB 
  b0 = y1 - slope*x1;
  return(b0)
}


# ----- F.0.5.6. intercept y axis line -------------------------------------------------
l <- function(b0, b1, x){
  y = b0 + b1*x;
  return(y)
}


# ----- F.0.5.7. intersection circle line -----------------------------------

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



# ----- F.0.5.7.1. intersection status -----------------------------------------

intersection.status <- function(inter_x1, inter_x2) {
  i_status <-   ifelse(is.na(inter_x1) & is.na(inter_x2), " no I",      # if 0 solutions
                       ifelse(inter_x1 == inter_x2, "one I",            # if 1 solution
                              ifelse(inter_x1 != inter_x2, "two I")));
  return(i_status)
}




# ----F.0.5.8. azimut -------------------------------------------------------
azimut <- function(x2, y2, x1, y1){
  # azi = atan((y2 - y1)/(x2 - x1));
  azi = atan((x2 - x1)/(y2 - y1));
  return(azi)
}



# ----F.0.5.8. azimut -------------------------------------------------------
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



# ---- F.0.5.8. distance between two points --------------------------------
distance <- function(x2, y2, x1, y1){
  d = sqrt(((y2 - y1)^2) + ((x2 - x1)^2));
  return(d)
}



# ----- F. 0.5.8. treee edge status -----------------------------------------------------------
# ----- F. 0.5.8.1. for straight line  ---------------------------------------------------------
# ----- F. 0.5.8.1.1. check for relation between intersections of middle-point-center-line and point and edge line  ---------------------------------------------------------
middle.point.to.line <- function(x1, x2, y1, y2, c.x0, c.y0, c.r0, l.b0, l.b1){
  # calculate coordiantes of the middle of thie line between 
  x_m_line = (x1 - x2)/2;
  y_m_line = (y1 - y2)/2;
  # calculate the parameters of the equation between the middle of the line and the centre of the circle
  b1_MC = slope(c.x0, c.y0, x_m_line, y_m_line);
  b0_MC = intercept(c.x0, c.y0, b1_MC);
  # calcualte the x corrdiante of the interception of the line between M and the centre of the cirle and the circle at the given radio
  X1_inter_MC = intersection_c_lx1(b0_MC, b1_MC, c.x0, c.y0, c.r0); 
  X2_inter_MC = intersection_c_lx2(b0_MC, b1_MC, c.x0, c.y0, c.r0);
  # insert the intersection x corodinate in the line function to get the respective y coordinate
  y1_inter_MC = l(b0_MC, b1_MC, X1_inter_MC); 
  y2_inter_MC = l(b0_MC, b1_MC, X2_inter_MC);
  # distance between the intersections (inter_MC_1, inter_MC_2) to M on the line 
  dist_C_inter_1_MC = distance(X1_inter_MC, y1_inter_MC, x_m_line, y_m_line);
  dist_C_inter_2_MC = distance(X2_inter_MC, y2_inter_MC, x_m_line, y_m_line); 
  # find the x and y coordinate of the intersection on the shorter side , which is the side to exlcude from the plot 
  X_inter_MC_shorter_side = ifelse(dist_C_inter_1_MC < dist_C_inter_2_MC, X1_inter_MC, X2_inter_MC); 
  Y_inter_MC_shorter_side = ifelse(dist_C_inter_1_MC < dist_C_inter_2_MC, y1_inter_MC, y2_inter_MC);
  # insert coordinates that are for sure on the smaller side of the two halves of the circle into the implicit equation: 
  Y_MC_implicit = l.b0  + l.b1 * X_inter_MC_shorter_side - Y_inter_MC_shorter_side;
  Y_implicit_status_M_line = case_when(Y_MC_implicit > 0 ~ "positive",          # "y imlicit has to be positive too for tree to be outside, 
                                       # as the result of the implicit equation that contains the 
                                       # point that is for sure in the smaller cirlce segment, has a positive impllciti equation result", 
                                       Y_MC_implicit < 0 ~ "negative",          # "y imlicit has to be negative for tree to be outside", 
                                       TRUE ~ "equal");
  return(Y_implicit_status_M_line)
}




# ----- F. 0.5.8.2. for line with turning point  ---------------------------------------------------------
# select the intersection coordinates that have the same azimute as A to T and B to T
select.inter.for.triangle <- function(t.dist, c.ro, azi_inter_1, azi_inter_2, azi_centre, x1, x2){
  x <- ifelse(t.dist <= c.ro & azi_inter_1 == azi_centre, x1, 
              ifelse(t.dist <= c.ro & azi_inter_2 == azi_centre, x2, NA));
  return(x)
}

# ----- F. 0.5.8.2.1. check if point lays in triangle  -------------------------------------------------------------------------------------
# this link https://stackoverflow.com/questions/2049582/how-to-determine-if-a-point-is-in-a-2d-triangle led me to the following links: 
# http://totologic.blogspot.com/2014/01/accurate-point-in-triangle-test.html
# https://www.geogebra.org/m/c8DwbVTP
# https://en.wikipedia.org/wiki/Barycentric_coordinate_system

p.in.triangle <- function(xa, xb, xc, ya, yb, yc, xp, yp){
  a = ((xp - xc)*(yb - yc) + (xc - xb)*(yp - yc)) / ((yb - yc)*(xa - xc) + (xc - xb)*(ya - yc));
  b = ((xp - xc)*(yc - ya) + (xa - xc)*(yp - yc)) / ((yb - yc)*(xa - xc) + (xc - xb)*(ya - yc));
  c = 1 - a - b;
  in.or.out = ifelse(0 <= a & a <= 1 & 0 <= b  & b <= 1 & 0 <= c & c <= 1, "B", "A");
  # B = out = point is inside triangle, so outside plot
  # A = in =  point is outside triangle, so inside plot
  return(in.or.out)
}



# ----- F. 0.5.9. edge area --------------------------------------------------

# ----- F.0.5.9.1. angle triabnle for area calculations ----------------------
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


# ----- F.0.5.9.2. cirlce segment area  ----------------------
# calculate the area of a cirlce segment by the angle between the two branches of the segment 
circle_seg_A <- function(r, angle){
  A_c_seg = (pi*r^2) * angle/400; 
  return(A_c_seg)
}

# ----- F.0.5.9.3. triangle area  ----------------------
triangle_A <- function(x1, x2, x3, y1, y2, y3){
  # x1|y1 and x2|y2 should be the intersections with the circle, 
  # x3|y3 should be the turning point or centre of the cirlce 
  # https://www.lernhelfer.de/schuelerlexikon/mathematik-abitur/artikel/flaecheninhalt-eines-dreiecks
  A_tri =  0.5*(x1*(y2-y3) + x2*(y3-y1) + x3*(y3-y2)) ;
  return(A_tri)
}


# ----- F. 0.5.9.4. selecting/ calculating total edge area per edge type per plot  ----------
tot.edge.A <- function(area_AT_AB_side, area_BT_side){
  A <- ifelse(!is.na(area_AT_AB_side) & !is.na(area_BT_side), area_AT_AB_side + area_BT_side,
              ifelse(!is.na(area_AT_AB_side) & is.na(area_BT_side), area_AT_AB_side, 
                     ifelse(is.na(area_AT_AB_side) & !is.na(area_BT_side), area_BT_side, 
                            0)));
  return(A)
} 

# ----- F. 0.5.9.5. assign correct area to trees according to their category (A/B)  ----------
# to select which area we have assign to the edge and which we have to asssign to the main stand
# we have to find out on which side of the line the "B" and on which side the "A" trees are located
# as we know if the result of the implicit function has to be positive or negative for the tree to lie
# outside the plot, we can calcualte the intersections of a line through the center of the edge line 
# and the center of the plot. 
# Following we check which of the intersections is element of the triangle or if the result of the implicit function 
# of the intersection comlies with the result the implicitf function nee to have for atree to be outside (middle.point.to.line)
identify.edge.area <- function(x1, x2, y1, y2, c.x0, c.y0, c.r0,l.b0, l.b1, xa, xb, xc, ya, yb, yc, c.seg.a, c.a, tree_status){
  # x1| y1 and x2|y2 belong to the intersections of the line or two points on a line
  # c.x0, c.y0 are the center coordinates of the circle
  # c.r0 is the radius of the circle
  # l.b0, l.b1 are the parameters of the line we assign the edges for
  #  xa, xb, xc, ya, yb, yc are the coordinates of the triangle corners that were used to identiy the "out" / "B" trees
  # c.seg.a means the area of the cirle segment (circle bow) or the circle segmetns per CCS, c.a means the area if the whole circle
  
  # calculate coordiantes of the middle of thie line between 
  x_m_line = (x1 - x2)/2;
  y_m_line = (y1 - y2)/2;
  # calculate the parameters of the equation between the middle of the line and the centre of the circle
  b1_MC = slope(c.x0, c.y0, x_m_line, y_m_line);
  b0_MC = intercept(c.x0, c.y0, b1_MC);
  # calcualte the x corrdiante of the interception of the line between M and the centre of the cirle and the circle at the given radio
  X1_inter_MC = intersection_c_lx1(b0_MC, b1_MC, c.x0, c.y0, c.r0); 
  X2_inter_MC = intersection_c_lx2(b0_MC, b1_MC, c.x0, c.y0, c.r0);
  # insert the intersection x corodinate in the line function to get the respective y coordinate
  y1_inter_MC = l(b0_MC, b1_MC, X1_inter_MC); 
  y2_inter_MC = l(b0_MC, b1_MC, X2_inter_MC);
  
  # finde the x coordiante of the intersection that is within the triangle (p.in.tr)
  # if inter_1_MC or inter_MC_2 is element of the triangle and the distance between the intersection 
  # and the middle point of the line is greater then the distanc between the intersection that is outside the triangle and by that inside the plot 
  # deduct the circle segment from the whole plot area (because the larger part of the plot belongs to category B)
  # if the itnersection that is element to the triangle lies on the shorter side of the line, use the circle segment / circle bows area as the edge area
  B_trees_edge.a = ifelse(p.in.triangle(xa, xb, xc, ya, yb, yc, X1_inter_MC, y1_inter_MC) == "B" &
                            distance(X1_inter_MC, y1_inter_MC, x_m_line, y_m_line) > distance(X2_inter_MC, y2_inter_MC, x_m_line, y_m_line) |
                            p.in.triangle(xa, xb, xc, ya, yb, yc, X2_inter_MC, y2_inter_MC) == "B" &
                            distance(X2_inter_MC, y2_inter_MC, x_m_line, y_m_line) > distance(X1_inter_MC, y1_inter_MC, x_m_line, y_m_line),
                          c.a - c.seg.a, c.seg.a); 
  # if inter_1_MC or inter_2_MC is not element of the triangle and the distance between this intersection and the middle of the line is bigger then the 
  # then distance between the intersection that is element to the triangle , so the A side of the line is the bigger side, deduct the circle segment area 
  # from the cirlce area, 
  # if the intersection is not element of the triangle but situated on the smaller side of the line (shorter distance between intersection and point M), 
  # the area of A trees has to be the area of the circle segment itself
  A_trees_edge.a = ifelse(p.in.triangle(xa, xb, xc, ya, yb, yc, X1_inter_MC, y1_inter_MC) == "A" &
                            distance(X1_inter_MC, y1_inter_MC, x_m_line, y_m_line) > distance(X2_inter_MC, y2_inter_MC, x_m_line, y_m_line) |
                            p.in.triangle(xa, xb, xc, ya, yb, yc, X2_inter_MC, y2_inter_MC) == "A" &
                            distance(X2_inter_MC, y2_inter_MC, x_m_line, y_m_line) > distance(X1_inter_MC, y1_inter_MC, x_m_line, y_m_line),
                          c.a - c.seg.a, c.seg.a);
  # if the tree status is "outside" "B", return the area calcualted for the "B" side of the plot, else return the area of the "A" side
  area = ifelse(tree_status == "B", B_trees_edge.a, A_trees_edge.a);
  
  return(area)
}



# ----- F. 1. joining in external info  --------------------------------------
# ----- F. 1.1. LIVING TREES -------------------------------------------------
# ----- F. 1.1.1. species & inventory names ----------------------------------------------
# ----- F. 1.1.1.1. HBI species & inventory ----------------------------------------------
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


# ----- F. 1.1.1.2. BZE3 species & inventory names ----------------------------------------------
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



# ----- F. 1.1.2. forest edges -----------------------------------------------
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


# ----- F. 1.1.2.1. join in edge info to tree dataset ------------------------
# ----- F. 1.1.2.1.1. HBI join in forest edge info per plot -----------------------------------------------
HBI_trees <- HBI_trees %>% 
  # calculate the coordinates of every tree
  mutate(X_tree = x_coord(Dist_cm, azi_gon), 
         Y_tree = y_coord(Dist_cm, azi_gon)) %>% 
  # join in the forest edge information per plot 
  left_join(., forest_edges_HBI %>% 
              select(plot_ID, e_ID, e_type, e_form), 
            by = "plot_ID", 
            multiple = "all") # this is necesarry since there are, apperently, multiple edges per plot 

# ----- F. 1.1.2.1.2. BZE3 join in forest edge info per plot -----------------------------------------------
# BZE3_trees <- BZE3_trees %>% 
#   # calculate the coordinates of every tree
#   mutate(X_tree = x_coord(Dist_cm, azi_gon), 
#          Y_tree = y_coord(Dist_cm, azi_gon)) %>% 
#   # join in the forest edge information per plot 
#   left_join(., forest_edges_HBI %>% 
#               select(plot_ID, e_ID, e_type, e_form), 
#             by = "plot_ID", 
#             multiple = "all") # this is necesarry since there are, apperently, multiple edges per plot 


# ----- F. 1.1.2.2. edge  point coordinates,  line parameters, intersections with circles -----------------------------------------------------------
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
         Y_inter_BT_triangle_60 = l(b0_BT, b1_BT, X_inter_BT_triangle_60)) %>% 
  
  mutate(Y_implicit_status_AB_line = middle.point.to.line(X1_inter_AB_17, X2_inter_AB_17, Y1_inter_AB_17, Y2_inter_AB_17, 0, 0,  data_circle$r0[3], b0_AB, b1_AB),
         Y_implicit_status_AT_line = middle.point.to.line(X1_inter_AT_17, X2_inter_AT_17, Y1_inter_AT_17, Y2_inter_AT_17, 0, 0,  data_circle$r0[3], b0_AT, b1_AT),
         Y_implicit_status_BT_line = middle.point.to.line(X1_inter_BT_17, X2_inter_BT_17, Y1_inter_BT_17, Y2_inter_BT_17, 0, 0,  data_circle$r0[3], b0_BT, b1_BT))


# new approach to localise points when edge is a line: 
# https://math.stackexchange.com/questions/1577062/how-to-know-if-a-given-point-is-inside-a-2d-circles-segment


# ----- F. 1.1.2.3. edge area: circle segments, triangles  ---------------------------------------------
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
         edge_area_eform_5_ha = tot.edge.A(edge_area_ABC_AC_5_ha, edge_area_ABC_BC_5_ha), 
         edge_area_eform_12_ha = tot.edge.A(edge_area_ABC_AC_12_ha, edge_area_ABC_BC_12_ha),
         edge_area_eform_17_ha = tot.edge.A(edge_area_ABC_AC_17_ha, edge_area_ABC_BC_17_ha)) 

forest_edges_HBI.man <- forest_edges_HBI.man %>% 
  # summaroze the edge are per plot and sampling circuit
  left_join(., forest_edges_HBI.man %>% 
              filter(!is.na(e_form)) %>% 
              select(plot_ID, e_form,
                     edge_area_ABC_AC_17_ha, edge_area_ABC_BC_17_ha, 
                     edge_area_ABC_AC_12_ha, edge_area_ABC_BC_12_ha,
                     edge_area_ABC_AC_5_ha, edge_area_ABC_BC_5_ha) %>% 
              pivot_longer(edge_area_ABC_AC_17_ha:edge_area_ABC_BC_5_ha, names_to = "CCS", values_to = "area_ha") %>% 
              mutate(CCS = case_when(endsWith(CCS, "17_ha")~ "edge_area_plot_17_ha", 
                                     endsWith(CCS, "12_ha")~ "edge_area_plot_12_ha",
                                     endsWith(CCS, "5_ha")~ "edge_area_plot_5_ha",
                                     TRUE ~ NA)) %>% 
              group_by(plot_ID, CCS) %>%
              summarize(total_area_AB_ABT_ha = sum(na.omit(area_ha))) %>% 
              pivot_wider(names_from = CCS, values_from = total_area_AB_ABT_ha), 
            by = "plot_ID")



# there will always occur the following error as for some lines there are no intersections, so the intersection function returns NaNs
# In argument: `X_inter_AT_17_triangle = case_when(...)`.
# Caused by warning in `sqrt()`:
#   ! NaNs wurden erzeugt




#---- 1.1.2.4. tree-edge-status by combining tree and edge data ---------------------------------------
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
                     X1_inter_AB_17, X2_inter_AB_17, Y1_inter_AB_17, Y2_inter_AB_17, inter_status_AB_17, azi_C_AB_inter_1, azi_C_AB_inter_2, Y_implicit_status_AB_line,
                     X1_inter_AT_17, X2_inter_AT_17, Y1_inter_AT_17, Y2_inter_AT_17, inter_status_AT_17, Y_implicit_status_AT_line, 
                     X1_inter_BT_17, X2_inter_BT_17,  Y1_inter_BT_17, Y2_inter_BT_17, inter_status_BT_17, Y_implicit_status_BT_line,
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
                     edge_area_eform_17_ha, edge_area_eform_12_ha, edge_area_eform_5_ha, 
                     edge_area_plot_17_ha, edge_area_plot_12_ha, edge_area_plot_5_ha),
            by = c("plot_ID", "e_ID", "e_type", "e_form")) %>% 
  # calculate the Y of the edge for the x of the tree
  # new approach by Johanna Garthe
  # insert y and x of tree in implizite function of line function: 0 = a*x + b - y --> if result > 0 --> group 1, if result <0 --> group 2, if result = 0 --> group 0
  mutate(Y_AB_t = l(b0_AB, b1_AB, X_tree),    # calcualte y of function at the x of the tree 
         dist_y_Xtree = distance(X_tree, Y_AB_t, 0, 0),
         # angle_AB_inter_right_tree =  angle_triangle(0, 0, x_right_AB_inter_17, y_right_AB_inter_17, X_tree, Y_tree),
         azi_AB_inter_1_2 = azi_correction( X2_inter_AB_17, Y2_inter_AB_17, X1_inter_AB_17, Y1_inter_AB_17, azimut( X2_inter_AB_17, Y2_inter_AB_17, X1_inter_AB_17, Y1_inter_AB_17)),
         azi_AB_inter_1_tree = azi_correction(X_tree, Y_tree, X1_inter_AB_17, Y1_inter_AB_17, azimut(X_tree, Y_tree, X1_inter_AB_17, Y1_inter_AB_17)),
         Y_AB_t_implicit = b0_AB  + b1_AB *X_tree - Y_tree, 
         Y_AT_t_implicit = b0_AT + b1_AT *X_tree - Y_tree,
         Y_BT_t_implicit = b0_BT  + b1_BT *X_tree - Y_tree) %>%
  # assign a tree-edge-status that calls trees with the same result as the implicit function of the middlepoint-center-line 
  # intersction point on the shorter side of the middlepoint center line
  #if there are two intersection and the Y inter status of 
  # middle.point.to.line is a function that determines if the result of an implicit function has to be positive or negative to be outside the line 
  # thus if the edge is a line with two intersection we asssign the 
  mutate(t_AB_status = ifelse(e_form == 1 & inter_status_AB_17 == "two I" & middle.point.to.line(X1_inter_AB_17, X2_inter_AB_17, Y1_inter_AB_17, Y2_inter_AB_17, 0, 0,  data_circle$r0[3], b0_AB, b1_AB)  == "positive" & Y_AB_t_implicit > 0 |
                                e_form == 1 & inter_status_AB_17 == "two I" & middle.point.to.line(X1_inter_AB_17, X2_inter_AB_17, Y1_inter_AB_17, Y2_inter_AB_17, 0, 0,  data_circle$r0[3], b0_AB, b1_AB) == "negative" &  Y_AB_t_implicit < 0 | 
                                e_form == 1 & inter_status_AB_17 != "two I" , 
                              "A", "B"),
         # t_AB_status_2 = ifelse(e_form == 1 & Y_AB_t_implicit == 0, "on line", 
         #                        ifelse(e_form == 1 & Y_AB_t_implicit > 0, "C", "D")),
         t_AT_status = ifelse(e_form == 2 & inter_status_AT_17 == "two I" & middle.point.to.line(X1_inter_AT_17, X2_inter_AT_17, Y1_inter_AT_17, Y2_inter_AT_17, 0, 0,  data_circle$r0[3], b0_AT, b1_AT)  == "positive" & Y_AT_t_implicit > 0 |
                                e_form == 2 & inter_status_AT_17 == "two I" & middle.point.to.line(X1_inter_AT_17, X2_inter_AT_17, Y1_inter_AT_17, Y2_inter_AT_17, 0, 0,  data_circle$r0[3], b0_AT, b1_AT) == "negative" &  Y_AT_t_implicit < 0 | 
                                e_form == 2 & inter_status_AT_17 != "two I" , 
                              "A", "B"),
         t_BT_status =ifelse(e_form == 2 & inter_status_BT_17 == "two I" & middle.point.to.line(X1_inter_BT_17, X2_inter_BT_17, Y1_inter_BT_17, Y2_inter_BT_17, 0, 0,  data_circle$r0[3], b0_BT, b1_BT)  == "positive" & Y_BT_t_implicit > 0 |
                               e_form == 2 & inter_status_BT_17 == "two I" & middle.point.to.line(X1_inter_BT_17, X2_inter_BT_17, Y1_inter_BT_17, Y2_inter_BT_17, 0, 0,  data_circle$r0[3], b0_BT, b1_BT) == "negative" &  Y_BT_t_implicit < 0 | 
                               e_form == 2 & inter_status_BT_17 != "two I" , 
                             "A", "B"),
         t_ABT_status = case_when(inter_status_AT_17 == "two I" & inter_status_BT_17 == "two I" ~ p.in.triangle(X_inter_AT_triangle_60, X_inter_BT_triangle_60, X_T, Y_inter_AT_triangle_60, Y_inter_BT_triangle_60, Y_T, X_tree, Y_tree),
                                  # if only one arm of the triangle crosses the circle/ has two intersections with the circle, use the respective arm as a line and assign tree status according to line procedure 
                                  inter_status_AT_17 != "two I" & inter_status_BT_17 == "two I" ~ t_BT_status, 
                                  inter_status_AT_17 == "two I" & inter_status_BT_17 != "two I" ~ t_AT_status,
                                  # if non of the arms touches the circle, assign all trees inside the circle to one group
                                  inter_status_AT_17 != "two I" & inter_status_BT_17 != "two I" ~ "A", 
                                  TRUE ~ NA), 
         # this combines the previous statements of the tree status  
         t_status_AB_ABT = case_when(e_form == "1" ~  t_AB_status,
                                     e_form == "2"  ~ t_ABT_status,
                                     TRUE ~ NA) )

# ---- 1.1.2.5. assigning plot area by according to diameter class (klubschwelle)  ---------------------------------------
# https://stackoverflow.com/questions/66252569/using-ifelse-conditional-on-multiple-columns
trees_and_edges %>%
  mutate(DBH_cm = ifelse(DBH_h_cm != 130, (D_mm*(1.0+(0.0011*(DBH_h_cm-130))))/10, D_mm/10)) %>% 
  mutate(plot_A = case_when(
    # for 5m circle     
    ## total circle   
    # edge_area_eform_5_ha contains the total edge area per edge from, so for form 2 it´l include both sides of the area that is cut from the circle by the tirangles arms
    # if there is no edge at the plot calcualte the whole circle area as the plot area
    DBH_cm >= 7 & DBH_cm < 10 & is.na(e_form)|
      # or if the plot has an edge form but only one tree status and non of the lines are intersecting the circle calcualte the whole circle area as the plot area
      !is.na(e_form) & t_status_AB_ABT %in% c("A", "B") & DBH_cm >= 7 & DBH_cm < 10 & inter_status_BT_5 != "two I" & inter_status_AT_5 != "two I" & inter_status_AB_5 != "two I"  ~ (c_A(data_circle$r0[1])/10000)/10000,
    ## circle - circle segment 
    # if the edge form is 1 and the tree lies inside the plot (status == A) and the circle is cut by the edge with 2 intersections, deduct the edge area from the main area
    t_status_AB_ABT == "A" & DBH_cm >= 7 & DBH_cm < 10 & e_form == "1" & inter_status_AB_5 == "two I"|
      # or if the edge for is 2 and T lies outside the circle and both arms of the triangle are intersecting the circle, 
      # deduct the area of the circle intersections from the total circle area for tree status B, because B means the tree is inside the triangle, which covers a the area between the two intersection lines, while the A area
      # is the space cut from the circle by the triangle arms that reach in
      t_status_AB_ABT == "B" & DBH_cm >= 7 & DBH_cm < 10 & e_form == "2" & T_dist > data_circle$r0[1] & inter_status_AT_5 == "two I" & inter_status_BT_5 == "two I"|
      # or if e_form == 2 and T lies outside the circle, trees with status B are assigned to the area of the cirlce- circle segment
      t_status_AB_ABT == "A" & DBH_cm >= 7 & DBH_cm < 10 & e_form == "2" & T_dist <= data_circle$r0[1] ~ ((c_A(data_circle$r0[1])/10000)/10000 - edge_area_eform_5_ha),
    ## circle segment    
    # if the edge form is 1 and the trees lies outside the plot (status == B) and the circle is cut by the edge with 2 intersections, use the edge area, cause for AB lines we always assign the B status to the smaller side of the circle
    t_status_AB_ABT == "B" & DBH_cm >= 7 & DBH_cm < 10 & e_form == "1" & inter_status_AB_5 == "two I"|
      # or if the edge for is 2 and T lies outside the circle and both arms of the triangle are intersecting the circle, 
      # assign the area of the circle intersections for trees with status A because A means the tree is outside the triangle, 
      # so the A area is the space cut from the circle by the triangle arms that reach in
      t_status_AB_ABT == "A" & DBH_cm >= 7 & DBH_cm < 10 & e_form == "2" & T_dist > data_circle$r0[1] & inter_status_AT_5 == "two I" & inter_status_BT_5 == "two I"|
      # if edge_form == 2 and T lies inside the circle trees with status B are assigned to the area of the circle segment
      t_status_AB_ABT == "B" & DBH_cm >= 7 & DBH_cm < 10 & e_form == "2" & T_dist <= data_circle$r0[1] ~ edge_area_eform_5_ha,
    ## identify.edge.area function   
    # if the edge form is 2 and T lies outside the circle and only one side of the triangle is intersecting the circle apply the identify.edge.area function 
    # this function is adapted to the tree status, so we don´t have to specify it here
    t_status_AB_ABT %in% c("A", "B") & DBH_cm >= 7 & DBH_cm < 10 & e_form == "2" & T_dist > data_circle$r0[1] & inter_status_AT_5 == "two I" & inter_status_BT_5 != "two I" ~ identify.edge.area(X1_inter_AT_5, X2_inter_AT_5, Y1_inter_AT_5, Y2_inter_AT_5, 0, 0, data_circle$r0[1], b0_AT, b1_AT, X_inter_AT_triangle_60, X_inter_BT_triangle_60, X_T, Y_inter_AT_triangle_60, Y_inter_BT_triangle_60, Y_T, edge_area_eform_5_ha, (c_A(data_circle$r0[1])/10000)/10000, t_status_AB_ABT),
    t_status_AB_ABT %in% c("A", "B") & DBH_cm >= 7 & DBH_cm < 10 & e_form == "2" & T_dist > data_circle$r0[1] & inter_status_BT_5 == "two I" & inter_status_AT_5 != "two I" ~ identify.edge.area(X1_inter_BT_5, X2_inter_BT_5, Y1_inter_BT_5, Y2_inter_BT_5, 0, 0, data_circle$r0[1], b0_BT, b1_BT, X_inter_AT_triangle_60, X_inter_BT_triangle_60, X_T, Y_inter_AT_triangle_60, Y_inter_BT_triangle_60, Y_T, edge_area_eform_5_ha, (c_A(data_circle$r0[1])/10000)/10000, t_status_AB_ABT),
    
    # for 12m circle: trees from 10 to 30 cm DBH     
    ## total circle   
    # if there is no edge at the plot calcualte the whole circle area as the plot area
    DBH_cm >= 10 & DBH_cm < 30 & is.na(e_form)|
      # or if the plot has an edge form but only one tree status and non of the lines are intersecting the circle calcualte the whole circle area as the plot area
      !is.na(e_form) & t_status_AB_ABT %in% c("A", "B") & DBH_cm >= 10 & DBH_cm < 30 & inter_status_BT_12 != "two I" & inter_status_AT_12 != "two I" & inter_status_AB_12 != "two I"  ~ (c_A(data_circle$r0[2])/10000)/10000,
    ## circle - circle segment 
    # if the edge form is 1 and the tree lies inside the plot (status == A) and the circle is cut by the edge with 2 intersections, deduct the edge area from the main area
    t_status_AB_ABT == "A" & DBH_cm >= 10 & DBH_cm < 30& e_form == "1" & inter_status_AB_12 == "two I"|
      # or if the edge for is 2 and T lies outside the circle and both arms of the triangle are intersecting the circle, 
      # deduct the area of the circle intersections from the total circle area for tree status B, because B means the tree is inside the triangle, which covers a the area between the two intersection lines, while the A area
      # is the space cut from the circle by the triangle arms that reach in
      t_status_AB_ABT == "B" & DBH_cm >= 10 & DBH_cm < 30 & e_form == "2" & T_dist > data_circle$r0[2] & inter_status_AT_12 == "two I" & inter_status_BT_12 == "two I"|
      # or if e_form == 2 and T lies outside the circle, trees with status B are assigned to the area of the cirlce- circle segment
      t_status_AB_ABT == "A" & DBH_cm >= 10 & DBH_cm < 30 & e_form == "2" & T_dist <= data_circle$r0[2] ~ ((c_A(data_circle$r0[2])/10000)/10000 - edge_area_eform_12_ha),
    ## circle segment    
    # if the edge form is 1 and the trees lies outside the plot (status == B) and the circle is cut by the edge with 2 intersections, use the edge area, cause for AB lines we always assign the B status to the smaller side of the circle
    t_status_AB_ABT == "B" & DBH_cm >= 10 & DBH_cm < 30 & e_form == "1" & inter_status_AB_12 == "two I"|
      # or if the edge for is 2 and T lies outside the circle and both arms of the triangle are intersecting the circle, 
      # assign the area of the circle intersections for trees with status A because A means the tree is outside the triangle, 
      # so the A area is the space cut from the circle by the triangle arms that reach in
      t_status_AB_ABT == "A" & DBH_cm >= 10 & DBH_cm < 30 & e_form == "2" & T_dist > data_circle$r0[2] & inter_status_AT_12 == "two I" & inter_status_BT_12 == "two I"|
      # if edge_form == 2 and T lies inside the circle trees with status B are assigned to the area of the circle segment
      t_status_AB_ABT == "B" & DBH_cm >= 10 & DBH_cm < 30 & e_form == "2" & T_dist <= data_circle$r0[2] ~ edge_area_eform_12_ha,
    ## identify.edge.area function   
    # if the edge form is 2 and T lies outside the circle and only one side of the triangle is intersecting the circle apply the identify.edge.area function 
    # this function is adapted to the tree status, so we don´t have to specify it here
    t_status_AB_ABT %in% c("A", "B") & DBH_cm >= 10 & DBH_cm < 30 & e_form == "2" & T_dist > data_circle$r0[2] & inter_status_AT_12 == "two I" & inter_status_BT_12 != "two I" ~ identify.edge.area(X1_inter_AT_12, X2_inter_AT_12, Y1_inter_AT_12, Y2_inter_AT_12, 0, 0, data_circle$r0[2], b0_AT, b1_AT, X_inter_AT_triangle_60, X_inter_BT_triangle_60, X_T, Y_inter_AT_triangle_60, Y_inter_BT_triangle_60, Y_T, edge_area_eform_12_ha, (c_A(data_circle$r0[2])/10000)/10000, t_status_AB_ABT),
    t_status_AB_ABT %in% c("A", "B") & DBH_cm >= 10 & DBH_cm < 30 & e_form == "2" & T_dist > data_circle$r0[2] & inter_status_BT_12 == "two I" & inter_status_AT_12 != "two I" ~ identify.edge.area(X1_inter_BT_12, X2_inter_BT_12, Y1_inter_BT_12, Y2_inter_BT_12, 0, 0, data_circle$r0[2], b0_BT, b1_BT, X_inter_AT_triangle_60, X_inter_BT_triangle_60, X_T, Y_inter_AT_triangle_60, Y_inter_BT_triangle_60, Y_T, edge_area_eform_12_ha, (c_A(data_circle$r0[2])/10000)/10000, t_status_AB_ABT),
    
    
    # for 17m circle: trees above 30 cm DBH     
    ## total circle   
    # if there is no edge at the plot calcualte the whole circle area as the plot area
    DBH_cm >= 30 & is.na(e_form)|
      # or if the plot has an edge form but only one tree status and non of the lines are intersecting the circle calcualte the whole circle area as the plot area
      !is.na(e_form) & t_status_AB_ABT %in% c("A", "B") & DBH_cm >= 30 & inter_status_BT_17 != "two I" & inter_status_AT_17 != "two I" & inter_status_AB_17 != "two I"  ~ (c_A(data_circle$r0[3])/10000)/10000,
    ## circle - circle segment 
    # if the edge form is 1 and the tree lies inside the plot (status == A) and the circle is cut by the edge with 2 intersections, deduct the edge area from the main area
    t_status_AB_ABT == "A" & DBH_cm >= 30 & e_form == "1" & inter_status_AB_17 == "two I"|
      # or if the edge for is 2 and T lies outside the circle and both arms of the triangle are intersecting the circle, 
      # deduct the area of the circle intersections from the total circle area for tree status B, because B means the tree is inside the triangle, which covers a the area between the two intersection lines, while the A area
      # is the space cut from the circle by the triangle arms that reach in
      t_status_AB_ABT == "B" & DBH_cm >= 30 & e_form == "2" & T_dist > data_circle$r0[3] & inter_status_AT_17 == "two I" & inter_status_BT_17 == "two I"|
      # or if e_form == 2 and T lies outside the circle, trees with status B are assigned to the area of the cirlce- circle segment
      t_status_AB_ABT == "A" & DBH_cm >= 30 & e_form == "2" & T_dist <= data_circle$r0[3] ~ ((c_A(data_circle$r0[3])/10000)/10000 - edge_area_eform_17_ha),
    ## circle segment    
    # if the edge form is 1 and the trees lies outside the plot (status == B) and the circle is cut by the edge with 2 intersections, use the edge area, cause for AB lines we always assign the B status to the smaller side of the circle
    t_status_AB_ABT == "B" & DBH_cm >= 30 & e_form == "1" & inter_status_AB_17 == "two I"|
      # or if the edge for is 2 and T lies outside the circle and both arms of the triangle are intersecting the circle, 
      # assign the area of the circle intersections for trees with status A because A means the tree is outside the triangle, 
      # so the A area is the space cut from the circle by the triangle arms that reach in
      t_status_AB_ABT == "A" & DBH_cm >= 30 & e_form == "2" & T_dist > data_circle$r0[3] & inter_status_AT_17 == "two I" & inter_status_BT_17 == "two I"|
      # if edge_form == 2 and T lies inside the circle trees with status B are assigned to the area of the circle segment
      t_status_AB_ABT == "B" & DBH_cm >= 30 & e_form == "2" & T_dist <= data_circle$r0[3] ~ edge_area_eform_17_ha,
    ## identify.edge.area function   
    # if the edge form is 2 and T lies outside the circle and only one side of the triangle is intersecting the circle apply the identify.edge.area function 
    # this function is adapted to the tree status, so we don´t have to specify it here
    t_status_AB_ABT %in% c("A", "B") & DBH_cm >= 30 & e_form == "2" & T_dist > data_circle$r0[3] & inter_status_AT_17 == "two I" & inter_status_BT_17 != "two I" ~ identify.edge.area(X1_inter_AT_17, X2_inter_AT_17, Y1_inter_AT_17, Y2_inter_AT_17, 0, 0, data_circle$r0[3], b0_AT, b1_AT, X_inter_AT_triangle_60, X_inter_BT_triangle_60, X_T, Y_inter_AT_triangle_60, Y_inter_BT_triangle_60, Y_T, edge_area_eform_17_ha, (c_A(data_circle$r0[3])/10000)/10000, t_status_AB_ABT),
    t_status_AB_ABT %in% c("A", "B") & DBH_cm >= 30 & e_form == "2" & T_dist > data_circle$r0[3] & inter_status_BT_17 == "two I" & inter_status_AT_17 != "two I" ~ identify.edge.area(X1_inter_BT_17, X2_inter_BT_17, Y1_inter_BT_17, Y2_inter_BT_17, 0, 0, data_circle$r0[3], b0_BT, b1_BT, X_inter_AT_triangle_60, X_inter_BT_triangle_60, X_T, Y_inter_AT_triangle_60, Y_inter_BT_triangle_60, Y_T, edge_area_eform_17_ha, (c_A(data_circle$r0[3])/10000)/10000, t_status_AB_ABT),
    TRUE ~ NA))










# ----- F. 1.1.2.6. exporting tree & edge data & edge areas -------------------------------------------------------------------
write.csv(trees_and_edges, paste0(out.path.BZE3,"LT_edges_HBI.csv"))



# ----- F. 2. visualization  -------------------------------------------------
# ----- F. 2.1.   Living trees visualization forest edges -----------------------------------
# ----- F. 2.1.1. AB lines, edge form 1, visualization forest edges -----------------------------------
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
             aes(X_tree, Y_tree, colour = t_AB_status))+
  theme_bw()+
  facet_wrap(~plot_ID)


# ----- F. 2.1.2. ABT lines, edge form 2, Visualisation forest edges -----------------------------------
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

# ----- F. 2.1.3. visulaliszing forest edge_form 1 and edge_form 2 together using m_s_status --------
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





# ----- F. NOTES -------------------------------------------------------------

# ----- F. estiamting coefficients of lines through edges via forest managemer package  -------------------------------------------------------------------

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


# ----- F. N.1. intersections of lines AB, AT, BT with all samlping cir --------
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



# N.2. edge form 1 (AB line) intersection of A to center and B to center lines with 60m radius --------
forest_edges_HBI.man %>% 
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
  mutate(#alpha_AB_x1_x2 = azi_C_AB_inter_1 -azi_C_AB_inter_2 ,
    #beta_AB_x1_x2 = azi_C_AB_inter_2 -azi_C_AB_inter_1 ,
    #lower_azi_AB_inter = ifelse(azi_C_AB_inter_1 < azi_C_AB_inter_2, azi_C_AB_inter_1, azi_C_AB_inter_2),
    #upper_azi_AB_inter = ifelse(azi_C_AB_inter_1 < azi_C_AB_inter_2, azi_C_AB_inter_2, azi_C_AB_inter_1),
    #lower_azi_AB_stat = ifelse(azi_C_AB_inter_1 < azi_C_AB_inter_2, "x1", "x2"),
    #upper_azi_AB_stat = ifelse(azi_C_AB_inter_1 < azi_C_AB_inter_2, "x2", "x1"),
    # x1 between 200 - 400, x2 between 0 -200
    x_left_stat = left.inter(azi_C_AB_inter_1, azi_C_AB_inter_2, "x1", "x2" ), 
    # select x coordinate on the left side
    x_left_AB_inter_17 = left.inter(azi_C_AB_inter_1, azi_C_AB_inter_2, X1_inter_AB_17, X2_inter_AB_17),
    # select y coordinate on the left side
    y_left_AB_inter_17 =  left.inter(azi_C_AB_inter_1, azi_C_AB_inter_2, Y1_inter_AB_17, Y2_inter_AB_17),
    # select x coordinate on the right side
    x_right_AB_inter_17 = right.inter(azi_C_AB_inter_1, azi_C_AB_inter_2, X1_inter_AB_17, X2_inter_AB_17),
    # select y coordinate on the right side
    y_right_AB_inter_17 =  right.inter(azi_C_AB_inter_1, azi_C_AB_inter_2, Y1_inter_AB_17, Y2_inter_AB_17),
    angle_AB_inter_17_gon = ifelse((azi_C_AB_inter_1 - azi_C_AB_inter_2) <0, (azi_C_AB_inter_1 - azi_C_AB_inter_2)*(-1), azi_C_AB_inter_1 - azi_C_AB_inter_2), 
    angle_AC_BC_grad = angle_AB_inter_17_gon*0.9,
    azi_start_AB_inter_17 = left.inter(azi_C_AB_inter_1, azi_C_AB_inter_2, azi_C_AB_inter_1, azi_C_AB_inter_2),
    azi_end_AB_inter_17 = right.inter(azi_C_AB_inter_1, azi_C_AB_inter_2, azi_C_AB_inter_1, azi_C_AB_inter_2),
    azi_end_AB_inter_17_sum = azi_start_AB_inter_17 + angle_AB_inter_17_gon,
    azi_end_AB_inter_17_corr = ifelse(azi_end_AB_inter_17 > 400, azi_end_AB_inter_17 -400, azi_end_AB_inter_17), 
    x1_inter_AC_60 = intersection_c_lx1(intercept(X_A, Y_A, slope(0,0, X_A, Y_A)), 
                                        slope(0,0, X_A, Y_A), 
                                        0, 0, data_circle$rmax[3]*2),
    x2_inter_AC_60 = intersection_c_lx2(intercept(X_A, Y_A, slope(0,0, X_A, Y_A)),
                                        slope(0,0, X_A, Y_A),
                                        0, 0, data_circle$rmax[3]*2),
    y1_inter_AC_60 = l(intercept(X_A, Y_A, slope(0,0, X_A, Y_A)), 
                       slope(0,0, X_A, Y_A), 
                       x1_inter_AC_60),
    y2_inter_AC_60 = l(intercept(X_A, Y_A, slope(0,0, X_A, Y_A)), 
                       slope(0,0, X_A, Y_A), 
                       x2_inter_AC_60),
    X_inter_AC_triangle_60 = select.inter.for.triangle(0,
                                                       data_circle$rmax[3]*2,
                                                       azi_correction(x1_inter_AC_60, y1_inter_AC_60, 0, 0, 
                                                                      azimut(x1_inter_AC_60, y1_inter_AC_60, 0, 0)), 
                                                       azi_correction(x2_inter_AC_60, y2_inter_AC_60, 0, 0, 
                                                                      azimut(x2_inter_AC_60, y2_inter_AC_60, 0, 0)),
                                                       azi_correction(X_A, Y_A, 0, 0, 
                                                                      azimut(X_A, Y_A, 0, 0)), 
                                                       x1_inter_AC_60, 
                                                       x2_inter_AC_60), 
    Y_inter_AC_triangle_60 = l(intercept(X_A, Y_A, slope(0,0, X_A, Y_A)), 
                               slope(0,0, X_A, Y_A), 
                               X_inter_AC_triangle_60),
    x1_inter_BC_60 = intersection_c_lx1(intercept(X_B, Y_B, slope(0,0, X_B, Y_B)), 
                                        slope(0,0, X_B, Y_B), 
                                        0, 0, data_circle$rmax[3]*2),
    x2_inter_BC_60 = intersection_c_lx2(intercept(X_B, Y_B, slope(0,0, X_B, Y_B)),
                                        slope(0,0, X_B, Y_B),
                                        0, 0, data_circle$rmax[3]*2),
    y1_inter_BC_60 = l(intercept(X_B, Y_B, slope(0,0, X_B, Y_B)), 
                       slope(0,0, X_B, Y_B), 
                       x1_inter_BC_60),
    y2_inter_BC_60 = l(intercept(X_B, Y_B, slope(0,0, X_B, Y_B)), 
                       slope(0,0, X_B, Y_B), 
                       x2_inter_BC_60),
    X_inter_BC_triangle_60 = select.inter.for.triangle(0, # tdist
                                                       data_circle$rmax[3]*2, # cro
                                                       azi_correction(x1_inter_BC_60, y1_inter_BC_60, 0, 0, # azi inter 1
                                                                      azimut(x1_inter_BC_60, y1_inter_BC_60, 0, 0)),
                                                       azi_correction(x2_inter_AC_60, y2_inter_BC_60, 0, 0, # azi inter 2
                                                                      azimut(x2_inter_BC_60, y2_inter_BC_60, 0, 0)),
                                                       azi_correction(X_A, Y_A, 0, 0,     # azi center
                                                                      azimut(X_A, Y_A, 0, 0)), 
                                                       x1_inter_BC_60, x2_inter_AC_60),
    Y_inter_BC_triangle_60 = l(intercept(X_B, Y_B, slope(0,0, X_B, Y_B)), 
                               slope(0,0, X_B, Y_B),
                               X_inter_BC_triangle_60),
    x1_inter_AB_60 = intersection_c_lx1(b0_AB, b1_AB, 0, 0, data_circle$rmax[3]*2),
    x2_inter_AB_60 = intersection_c_lx2(b0_AB, b1_AB, 0, 0, data_circle$rmax[3]*2), 
    y1_inter_AB_60 = l(b0_AB, b1_AB, x1_inter_AB_60), 
    y2_inter_AB_60 = l(b0_AB, b1_AB, x2_inter_AB_60))


# N. 0.5.14. select the coordiantes/ azimute of more left intersection  ----------
left.inter <- function(azi_1, azi_2, coord_1, coord_2){
  coordinate <- ifelse(azi_1 >= 200 & azi_1 <= 400 &
                         azi_2 >= 0 & azi_2 <= 200 & 
                         azi_1 > azi_2 | 
                         # x1 & x2 between 200 - 400 
                         azi_1 >= 200 & azi_1 <= 400 &
                         azi_2 >= 200 & azi_2 <= 400 & 
                         azi_1 < azi_2 |
                         # x1 & x2 between 0 - 200
                         azi_1 >= 0 & azi_1 <= 200 &
                         azi_2 >= 0 & azi_2 <= 200 &
                         azi_1 < azi_2,  coord_1,
                       ifelse(
                         # x1 between 0 - 200  & x2 between 200 - 400 
                         azi_1 >= 0 & azi_1 <= 200 &
                           azi_2 >= 200 & azi_2 <= 400 &
                           azi_1 < azi_2, coord_2,
                         ifelse(is.na(azi_1) | is.na(azi_2), NA, coord_2
                         )
                       )
  );
  return(coordinate)
}


right.inter <- function(azi_1, azi_2, coord_1, coord_2){
  coordinate <- ifelse(azi_1 >= 200 & azi_1 <= 400 &
                         azi_2 >= 0 & azi_2 <= 200 & 
                         azi_1 > azi_2 | 
                         # x1 & x2 between 200 - 400 
                         azi_1 >= 200 & azi_1 <= 400 &
                         azi_2 >= 200 & azi_2 <= 400 & 
                         azi_1 < azi_2 |
                         # x1 & x2 between 0 - 200
                         azi_1 >= 0 & azi_1 <= 200 &
                         azi_2 >= 0 & azi_2 <= 200 &
                         azi_1 < azi_2,  coord_2,
                       ifelse(
                         # x1 between 0 - 200  & x2 between 200 - 400 
                         azi_1 >= 0 & azi_1 <= 200 &
                           azi_2 >= 200 & azi_2 <= 400 &
                           azi_1 < azi_2, coord_1,
                         ifelse(is.na(azi_1) | is.na(azi_2), NA, coord_1
                         )
                       )
  );
  return(coordinate)
}



#N. assigning tree status according to frequency of the groups per plot
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







