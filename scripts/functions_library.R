# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the peat land soil inventory
# Functions & require


# ----- 0. SETUP ---------------------------------------------------------------
# ----- 0.1. Packages  ---------------------------------------------------------
## datamanagement
#  install.packages("usethis")
#   install.packages("here")
#   install.packages("readr")
#   install.packages("tidyverse")
#   install.packages("tibble")
#   install.packages("dplyr")
#   install.packages("data.table")
#   install.packages("broom")
# install.packages("purrr")
# install.packages("stringr")
#   install.packages("devtools")
#   ## laTex
#   install.packages("stargazer")  #for compatability with Latex
#   install.packages("tikzDevice") #for compatability with Latex#
#   # visualisation
#   install.packages("ggthemes")
#   install.packages("ggplot2")
#   install.packages("reshape2") #for multiple y values
#   install.packages("ggforce") #for zooming in parts of the plot
#  install.packages("ggforce")             # Install ggforce package
#   options(tz="CA")
#   install.packages("reshape2")
#   # analysis
#   install.packages("corrplot")
#   install.packages("AICcmodavg")
#   # forest related
#    install.packages("forestmangr")
#   install.packages("rBDAT")
#   install.packages("TapeR")
#  install.packages("pkgbuild")
#   require("devtools")
#   if (! require("remotes")) 
#     install.packages("remotes")
#   remotes::install_gitlab("vochr/TapeS", build_vignettes = TRUE)
#  install.packages("magrittr")
#  install.packages("sjmisc")
#  if(!require(devtools)) install.packages("devtools")
#  devtools::install_github("kassambara/ggcorrplot")
# # spatial
#  install.packages("sf")
#  install.packages("rgdal")
#  install.packages("terra")
# install.packages("sfheaders")



# ----- 0.2. require   ---------------------------------------------------------
# datamanagement
require(usethis)
require(here)
require(readr)
require(tidyverse)
require(tibble)
require(dplyr)
require(data.table)
require(broom)
require(purrr)
require(remotes)
require(devtools)
library(stringr)
# laTex
require(stargazer)  #for compatability with Latex
require(tikzDevice) #for compatability with Latex
# visualisation
require(ggthemes)
require(ggplot2)
require(reshape2) #for multiple y values
require(ggforce) #for zooming in parts of the plot
options(tz="CA")
# analysis
require(corrplot)
require(AICcmodavg)
require(ggcorrplot)
# forest related
require(forestmangr)
require(rBDAT)
require(TapeR)
if (! require("remotes")) 
  install.packages("remotes")
require(remotes)
#devtools::install_gitlab("vochr/TapeS", build_vignettes = TRUE)
#remotes::install_gitlab("vochr/TapeS", build_vignettes = TRUE)
require(TapeS)
require(magrittr)
require(sjmisc)
require(ggforce)                      # Load ggforce package
# spatial 
require(sf)
require(rgdal)
require(terra)
require(sfheaders)

# ----- 0.3. working directory -------------------------------------------------
here::here()



# ----- 1. Functions -----------------------------------------------------------


# ----- 1.1. area circle --------------------------------------------------
# area of a circle
c_A <- function(r){
  circle_area <- r^2*pi;
  return(circle_area)
}

# ----- 1.2 DBH class ----------------------------------------------------------
DBH_c_function <- function(dbh){
  # create label for diameter classes according to BZE3 Bestandesaufnahmeanleitung
  labs_DBH <- c(seq(5, 55, by = 5)) ; 
  DBH_c <- cut(as.numeric(dbh),                               # cut the diameter
               breaks = c(seq(5, 55, by = 5), Inf),  # in sequences of 5
               labels = labs_DBH,                    # and label it according to labs (1.4.1)
               right = FALSE);
  return(DBH_c)
}

# ----- 1.3 age class ----------------------------------------------------------
# defining age classes from 1 to 160 in steps of 20
# this is a preparation fot the comparison with carbon stocks calcualted by te
labs_age <- c(seq(1, 180, by = 20))



# ----- 1.4 coordinate functions ---------------------------------------

# http://www.markusbaumi.ch/schule/formel/azimut.pdf

coord <- function(x.c, y.c, d, azi, coordinate){
  # Xc =  x coordinate of centre = 0 
  # yc = y coordinate of circle centre = 0
  # d = Distance between point and other ppoint (centre)
  # azi =  Azimute betweeen Point and other ppoint (centre)
  
  switch(coordinate, 
  x = x.c + d * sin(azi),  # x is the latitude or "easting"
  y = y.c + d * cos(azi)  #  y is the longitude or "northing"
  )  
}
  
  
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




pick_utm_epsg <- function(lon){
  if(lon < 6){
    epsg <- 32631
  }else if(lon >= 6 & lon < 12){
    epsg <- 32632
  }else if(lon >= 12){
    epsg <- 32633
  }
  return(epsg)
}


# ----1.4.2. azimut -------------------------------------------------------
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



# ------1.4.3. angle between 2 lines --------------------------------------------------------------------------
# calculating angle between two lines at their point of intersection
angle.vectors <- function(x.0, y.0, x.1, y.1, x.2, y.2, unit){
  # calculate vector from center/ turning point to respective other point on the line: https://studyflix.de/mathematik/vektor-berechnen-4349
  x.a = x.1 - x.0
  y.a = y.1 - y.0
  x.b = x.2 - x.0
  y.b = y.2 - y.0
  # calculate ange betweern vectors: 
    # https://studyflix.de/mathematik/winkel-zwischen-zwei-vektoren-2251 
   # https://www.schuelerhilfe.de/online-lernen/1-mathematik/720-winkel-zwischen-vektoren
  scalar.porduct = x.a * x.b +  y.a * y.b
  length.a = sqrt(abs(x.a)^2 + abs(y.a)^2)
  length.b = sqrt(abs(x.b)^2 + abs(y.b)^2)
  cos.minus.1 = (scalar.porduct)/(length.a * length.b)
  angle.rad = acos(cos.minus.1)
  angle.degrees = angle.rad*(180/pi)
  angle.gon = angle.rad*(180/pi)*0.9
  switch(unit, 
         rad = angle.rad, 
         degrees = angle.degrees, 
         gon = angle.gon)
}

# ----- 1.4.3. distance between two points --------------------------------
distance <- function(x2, y2, x1, y1){
  d = sqrt(((y2 - y1)^2) + ((x2 - x1)^2));
  return(d)
}



# ----- 1.5 line functions ------------------------------------------------
# ----- 1.5.1. slope line -------------------------------------------------
# this function calculates the slope of a line between two points
slope <- function(x1, y1, x2, y2){
  b1 = (y2 - y1)/(x2 - x1);
  return(b1)
}

# ----- 1.5.2. intercept y axis line -------------------------------------------------
# this function returns the intercept of a line between two points
intercept <- function(x1, y1, x2, y2){
  # resolve line function towards b0 after inserting known coordinates and slope
  # Y_A = b1_AB*X_A + b0_AB | (-b1_AB*X_A) 
  # Y_A - b1_AB*X_A = b0_AB 
  b1 = (y2 - y1)/(x2 - x1);
  b0 = y1 - b1*x1;
  return(b0)
}

# ----- 1.5.3.  line -----------------------------------------------------------
l <- function(b0, b1, x){
  y = b0 + b1*x;
  return(y)
}



# ----- 1.6. intersection ------------------------------------------------------
# ----- 1.6.1. intersections circle line ---------------------------------------
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
# c.df <- data_circle %>% filter(r0 == 17.84)
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



# ----- 1.6.2. intersection status -----------------------------------------

intersection.status <- function(inter_x1, inter_x2) {
  i_status <-   ifelse(is.na(inter_x1) & is.na(inter_x2), " no I",      # if 0 solutions
                       ifelse(inter_x1 == inter_x2, "one I",            # if 1 solution
                              ifelse(inter_x1 != inter_x2, "two I")));
  return(i_status)
}





# ----- 1.7. tree to edge status  ---------------------------------------------------------
# ----- 1.7.1. for straigt line  ---------------------------------------------------------
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

p.site.line <- function(x1, x2, y1, y2, c.x0, c.y0, c.r0, l.b0, l.b1, x.tree, y.tree, output){
  # determin status of intersection: 
  i_status <-   ifelse(is.na(x1) & is.na(x2), " no I",      # if 0 solutions
                       ifelse(!is.na(x1) & !is.na(x2) & x1 == x2, "one I",            # if 1 solution
                              ifelse(x1 != x2, "two I")));      # so if the edge for is 1 and there are 2 interseections of the line with the respective circle 
  
  # calculate coordiantes of the middle of thie line between 
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
  # if the result of the impliyit function of the trees corodinates corresponds with the result of the intersection on the shorter side of the circle, the tree status has to be B
  Y_implicit_status_tree_line =  ifelse(i_status == "two I" & Y_implicit_status_M_line == "positive" & Y_tree_implicit >= 0 |
                                          i_status == "two I" & Y_implicit_status_M_line == "negative" &  Y_tree_implicit < 0,
                                         # i_status != "two I",  
                                        "B", "A"); # if the line is crossing the plot by two intersections and there 
  switch (output,
    "tree_stat" = Y_implicit_status_tree_line, 
    "x_short" = X_inter_MC_shorter_side, 
    "y_short" = X_inter_MC_shorter_side
  )
  
}


# ----- 1.7.2. line with turning point  --------------------------------------------------------------------------------------------------
# ----- 1.7.2.1. select intersection for triangle in correct direction -----------------------------------------------------------------------------------------
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
  
  # reducing the number of digits so there wont occure weird differences that make r return NA
  azi.inter.1.t <- format(round(azi.inter.1.t, 10), nsmall = 10) 
  azi.inter.2.t <- format(round(azi.inter.2.t, 10), nsmall = 10)
  azi.point.t <- format(round(azi.point.t, 10), nsmall = 10)
  
  switch(coordinate,
         x = ifelse(azi.inter.1.t == azi.point.t,x1.inter,
                    ifelse(azi.inter.2.t == azi.point.t, x2.inter , NA)), 
         y = ifelse(azi.inter.1.t == azi.point.t, y1.inter , 
                    ifelse(azi.inter.2.t == azi.point.t, y2.inter, NA)))
}

# ------ 1.7.2.2. check if point lays in triangle  -------------------------------------------------------------------------------------
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

# ------ 1.7.2.3. check if point is located on the triangle site of a line -------------------------------------------------------------------------------------
# this function helps to sort trees if the edge type is a triangle, with only one arm crossing the sampling circuit
# - it identifies two points that are for sure located on oposites sites of the line, created by the arm of the triangle, reaching into the circle
# - this is carried out by drawing a straight line through the middle of the edge line and the center of the circuit and following identifiy the intersections between the circle and the line
# - following the both points are tested regarding their position to the triangle
# - the intersection point of the midddle-point-center-line which is located inside the triangle (p.in.triangle == "B") is inserted into the implicit function of the line of the arm that reaches into the circle
# - this way we know which result the implicit function of the trees needs to have to allocate the tree to the triangle side of the line, and the not triangle side of the line
# - this is necesarry because we can´t apply out susal procedure for line edges, where we just sort the trees into B if they are located in the smaller half of the circle and into A if they are located in the bigger half
# - in case of a triangle shaped edge which affects the circle like a line shaped edge we have to find the side of the circle that reaches inside the triangle 

p.site.triangle <- function(x1, x2, y1, y2, c.x0, c.y0, c.r0,l.b0, l.b1, xa, xb, xc, ya, yb, yc, x.tree, y.tree, output){
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
  x_m_line = (x1 + x2)/2;
  y_m_line = (y1 + y2)/2;
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
  
  switch(output, 
         "tree_stat" = Y_tree_implicit_status, 
         "x_inside_triangle" = X_inter_MC_inside_triangle, 
         "y_inside_triangle" = Y_inter_MC_inside_triangle) 
}


# ----- 1.7.3. final treee edge status for all edge types -----------------------------------------------------------
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
  tree_status_AB_line = ifelse(edge_form == "1", p.site.line(x1.inter.AB, x2.inter.AB, y1.inter.AB, y2.inter.AB, c.x0, c.y0, c.r017, l.AB.b0, l.AB.b1, x.tree, y.tree, output = "tree_stat"),NA);
  tree_status_AT_line = p.site.triangle(x1.inter.AT, x2.inter.AT, y1.inter.AT, y2.inter.AT,
                                        c.x0, c.y0, c.r017,
                                        l.AT.b0, l.AT.b1,
                                        x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, 
                                        y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t,
                                        x.tree, y.tree, 
                                        output = "tree_stat");
  tree_status_BT_line = p.site.triangle(x1.inter.BT, x2.inter.BT, y1.inter.BT, y2.inter.BT,
                                        c.x0, c.y0, c.r017,
                                        l.BT.b0, l.BT.b1,
                                        x.AT.inter.triangle.60, x.BT.inter.triangle.60, x.t, 
                                        y.AT.inter.triangle.60, y.BT.inter.triangle.60, y.t,
                                        x.tree, y.tree, 
                                        output = "tree_stat");
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


# ----- 1.8. edge area -------------------------------------------------------------------------------------------------------

# ----- 1.8.1. assign correct area to trees according to their category (A/B)  ----------
# to select which area we have assign to the edge and which we have to asssign to the main stand
# we have to find out on which side of the line the "B" and on which side the "A" trees are located
# as we know if the result of the implicit function has to be positive or negative for the tree to lie
# outside the plot, we can calcualte the intersections of a line through the center of the edge line 
# and the center of the plot. 
# Following we check which of the intersections is element of the triangle or if the result of the implicit function 
# of the intersection comlies with the result the implicitf function nee to have for atree to be outside (middle.point.to.line)

# this function should eable us to skip the part where we have to calcualte the intersections etc. for each circle and line
# this way we´ll just return the area per sampling circle 


# new way to calculate circle segment that doesn't rely on comparing the distance of the center point of the line to the intersections of 
# a line through the center of the line to the circle center
# developed by Alexandr Chepowskii

CircleSegmnent <- function(x1,y1,x2,y2,r){
  # standart line equation parameters: onmicalculator.com
  A = y2-y1
  B = x2 - x1
  C = y1*B - A*x1
  
  # calcualte height of circle segment if edge form is 1 and height can be calculated from shortest distance
  # shortest distance between center and AB line: chilimath.com
  # center is always 0|0
  d = abs(C)/sqrt(A^2+B^2)
  # height of the cirlce regment between line and circle perimeter
  h = ifelse(d<=r, r-d, 0)
  
  # calculate area of cirlce segment with heigth and radius : wikipedia.de
  area = r^2*acos(1-(h/r))-(r-h)*sqrt(r^2-(r-h)^2)
  return(area)
}


cone.area <- function(x.0, y.0, x1, y1, x2, y2, r){
  angle.between.lines = angle.vectors(x.0, y.0, x1, y1, x2, y2, unit = "degrees")
  cone.A = (pi*r^2)*angle.between.lines/360
  return(cone.A)
}
  


triangle.area <- function(x.0, y.0, x1, y1, x2, y2, method){
  # standart line equation parameters: onmicalculator.com
  A = y2-y1
  B = x2 - x1
  C = y1*B - A*x1
  # shortest distance between center and AB line: chilimath.com
  h.triangle  = abs(A*x.0 + B*y.0 + C)/sqrt(A^2+B^2)
  g.triangle = distance(x1,y1,x2,y2)
  switch(method, 
         shortest.dist = (h.triangle/2)*g.triangle,
         # https://en.wikipedia.org/wiki/Area_of_a_triangle
         three.points = 0.5*abs((x1*(y2-y.0) + x2*(y.0 - y1) + x.0*(y.0-y2)))
         )
  
}


# new approach to calcualte intersection area developed by Alexandr Chepowskii
triangle.circle.poly.intersection <- function(x1,y1,x2,y2,x3,y3,r){
  # center point of circle
  pt.circle <- sf::st_point(c(0,0))
  circle.poly <- sf::st_buffer(pt.circle, dist = r)
  # triangle polygone 
  poly.data = matrix(c(x1,y1,x2,y2,x3,y3,x1,y1), ncol = 2, byrow = TRUE)
  triangle.poly <- sf::st_polygon(list(poly.data))
  # intersection between circle and triangle
  intersection.circle.trianlge <- sf::st_intersection(circle.poly, triangle.poly)
  #calculate area of intersection if intersection is not empty
  area.intresection <- sf::st_area(intersection.circle.trianlge)
  
  return(area.intresection)
}






edge.A <- function(e.form, dbh.cm, x.a, x.b, x.t, y.a, y.b, y.t, t.dist, tree_status, output){
  # x1| y1 and x2|y2 belong to the intersections of the line or two points on a line
  # c.x0, c.y0 are the center coordinates of the circle
  # c.r0 is the radius of the circle
  # l.b0, l.b1 are the parameters of the line we assign the edges for
  #  xa, xb, xc, ya, yb, yc are the coordinates of the triangle corners that were used to identiy the "out" / "B" trees
  # c.seg.a means the area of the cirle segment (circle bow) or the circle segmetns per CCS, c.a means the area if the whole circle
  
  # p_id = 50112
  # e.form = forest_edges_HBI.man$e_form[forest_edges_HBI.man$plot_ID == p_id]
  # dbh.cm = 35
  # x.a = forest_edges_HBI.man$X_A[forest_edges_HBI.man$plot_ID == p_id]
  # x.b = forest_edges_HBI.man$X_B[forest_edges_HBI.man$plot_ID == p_id]
  # x.t = forest_edges_HBI.man$X_T[forest_edges_HBI.man$plot_ID == p_id]
  # y.a = forest_edges_HBI.man$Y_A[forest_edges_HBI.man$plot_ID == p_id]
  # y.b = forest_edges_HBI.man$Y_B[forest_edges_HBI.man$plot_ID == p_id]
  # y.t = forest_edges_HBI.man$Y_T[forest_edges_HBI.man$plot_ID == p_id]
  # t.dist= forest_edges_HBI.man$T_dist[forest_edges_HBI.man$plot_ID == p_id]
  # tree_status= "B"
  
  
  # select the diameter of the circle depending on the trees diameter
  c.x0 = 0;
  c.y0 = 0; 
  c.r0 =   ifelse(dbh.cm >= 7 & dbh.cm < 10, 5.64, 
                  ifelse(dbh.cm >= 10 & dbh.cm < 30,  12.62,
                         ifelse(dbh.cm >= 30, 17.84, NA)))
  
  
  # 
  
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
  c.cone.A = ifelse(e.form == 2 & t.dist <= c.r0, triangle.circle.poly.intersection(x1, y1, x2, y2, x.t, y.t, c.r0),
                    ifelse(e.form == 2 & t.dist > c.r0, cone.area(c.x0, c.y0, x1, y1, x2, y2, c.r0), NA)); 
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
  rem.circle.area = c.A - edge.area; 
  # there is a problem here because for plot with two edges, the remaining circle area will be reduced by the area of both edges, which the function cannot provide for now
  # thus it could be smarter to just get the edge area returned per plot and circle and then reduce the remaining area by the area of the respective edges
  area = ifelse(tree_status == "A" | is.na(e.form), rem.circle.area, edge.area);
  
  
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
  area.method =  ifelse(tree_status == "A" | is.na(e.form), "rem.circle.area", edge.method);
 
  
  switch(output, 
         "edge.only" = edge.area,
         "edge.method.only" = edge.method,
         "area_m2" = area, 
         "method" = area.method)
}



