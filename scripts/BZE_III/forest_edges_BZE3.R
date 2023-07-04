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
x_coord <- function(Dcp, AZIcp){ 
  # Xc =  x coordinate of centre = 0 
  # Dcp = Distance between point and centre
  # AZIcp=  Azimute betweeen Point and centre
  Xc <- 0;   # this is set to 2000 to avoid negative 
  X = Xc + Dcp * cos(AZIcp);
  return(X)
}

y_coord <- function(Dcp, AZIcp){ 
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


# ----- 0.5.6. intersection circle line -----------------------------------

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


intersection_c_l_status <- function(x1, x2) {
  
  # quadratic formula
  # 0 = ((1 +l.df$e_b1_AB^2)/(1 +l.df$e_b1_AB^2))*X^2  -   ((2*c.df$x0 - 2*l.df$e_b1_AB*(l.df$e_b0_AB - c.df$y0))/(1 +l.df$e_b1_AB^2))*X   +     (c.df$x0^2 + (l.df$e_b0_AB - c.df$y0)^2 - c.df$r0^2)/(1 +l.df$e_b1_AB^2)
  
  # x1 = -(p/2)+(sqrt((p/2)^2-q))
  # x2 = -(p/2)-(sqrt((p/2)^2-q))
  
  # p = b so the number before x in quadratic formula
  # q = c so the number at the end of quadratic fomula
 
  # c.y0 = 0; 
  # c.x0 = 0;
  # c.r0 =1784;
  # 
  # p = ((2*c.x0) + (2*l.b1*(l.b0 - c.y0)))/(1 + l.b1^2);
  # q = (c.x0^2 + (l.b0 - c.y0)^2 - c.r0^2)/(1 +l.b1^2);
  # x1 <-  (-(p/2) + sqrt( ((p*-1)/2)^2-q ));
  # x2 <- (-(p/2) - sqrt( ((p*-1)/2)^2-q ));
  # i.df <- as.data.frame(cbind(p_ID, x1, x2))
  
  # https://community.rstudio.com/t/how-do-i-write-a-function-that-will-tell-me-if-an-equation-has-no-solution/159834/6
  # if no solutions
 ifelse(
      is.na(x1) & is.na(x2), return(" no I"), 
      # if 1 solution
      ifelse(x1 = x2, return("one I"), 
             # if 2 solutions
             ifelse(x1 != x2, return("two I")
                    )
             )
      )
}


# x1 of intersection
intersection_c_lx1 <- function(l.b0, l.b1, c.y0, c.x0, c.r0) {
  
  # quadratic formula
  # 0 = ((1 +l.df$e_b1_AB^2)/(1 +l.df$e_b1_AB^2))*X^2  -   ((2*c.df$x0 - 2*l.df$e_b1_AB*(l.df$e_b0_AB - c.df$y0))/(1 +l.df$e_b1_AB^2))*X   +     (c.df$x0^2 + (l.df$e_b0_AB - c.df$y0)^2 - c.df$r0^2)/(1 +l.df$e_b1_AB^2)
  
  # x1 = -(p/2)+(sqrt((p/2)^2-q))
  # x2 = -(p/2)-(sqrt((p/2)^2-q))
  
  # p = b so the number before x in quadratic formula
  # q = c so the number at the end of quadratic fomula
  
  c.y0 = 0; 
  c.x0 = 0;
 # c.r0 =1784;
  
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
  #c.y0 = 0; 
  #c.x0 = 0;
  #c.r0 =1784;
  
  p = ((2*c.x0) + (2*l.b1*(l.b0 - c.y0)))/(1 + l.b1^2);
  q = (c.x0^2 + (l.b0 - c.y0)^2 - c.r0^2)/(1 +l.b1^2); 
  x1 <-  -(p/2) + sqrt( ((p*-1)/2)^2-q );
  x2 <- - (p/2) - sqrt( ((p*-1)/2)^2-q );
  i.df <- as.data.frame(cbind(x1, x2));
  
  # https://community.rstudio.com/t/how-do-i-write-a-function-that-will-tell-me-if-an-equation-has-no-solution/159834/6
  # if no solutions
  # ifelse(
  #   is.na(i.df$x1) & is.na(i.df$x2), return(NA), 
  #   # if 1 solution
  #   ifelse(i.df$x1 == i.df$x2, return(NA), 
  #          # if 2 solutions
  #          ifelse(i.df$x1 != i.df$x2, return(i.df$x2))
  #   )
  # )
  return(x2)
}




# ----0.5.7. azimut -------------------------------------------------------
azimut <- function(x2, y2, x1, y1){
  azi = atan((y2 - y1)/(x2 - x1));
  return(azi)
}



# ----0.5.8. azimut -------------------------------------------------------
azi_correction <- function(x, y, azi){
  azi = ifelse(x > 0 & y > 0, azi,                    # first quadrant x + y+
               ifelse(x > 0 & y < 0, -1*azi+400,         # second quadrant x + y-
                      ifelse(x < 0 & y < 0,  azi+200,   # third quadrant x- y- 
                             ifelse(x < 0 & y > 0, -1*azi+200, NA)
                             )
                      )
               );
  return(azi)
}



# ----- 0.5.8. distance between two points --------------------------------
distance <- function(x2, y2, x1, y1){
  
   d = sqrt(((y2 - y1)^2) + ((x2 - x1)^2))
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




# ----- 1. joining in external info  --------------------------------------


# ----- 1.1. LIVING TREES -------------------------------------------------

# ----- 1.1.1. species & inventory names ----------------------------------------------
HBI_trees <- HBI_trees %>% 
  mutate(inventory = "HBI") %>% 
  left_join(SP_names_com_ID_tapeS %>% 
              mutate(char_code_ger_lowcase = tolower(Chr_code_ger)), 
            by = c("SP_code" = "char_code_ger_lowcase"))


# check if there are no trees left that don´t have a SP_code in xBart/ SP_names_com_ID_tapeS
HBI_trees %>% 
  anti_join(SP_names_com_ID_tapeS %>% 
              mutate(char_code_ger_lowcase = tolower(Chr_code_ger)), 
            by = c("SP_code" = "char_code_ger_lowcase"))

# BZE3_trees <- BZE3_trees %>% 
#   mutate(inventory = "HBI") %>% 
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



# ----- 1.1.3. forest edges -----------------------------------------------
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


# ----- 1.1.3.1. join in forest edge info per plot -----------------------------------------------
HBI_trees <- HBI_trees %>% 
  # calculate the coordinates of every tree
  mutate(X_tree = x_coord(Dist_cm, azi_gon), 
         Y_tree = y_coord(Dist_cm, azi_gon)) %>% 
  # join in the forest edge information per plot 
  left_join(., forest_edges_HBI %>% 
              select(plot_ID, e_ID, e_type, e_form), 
            by = "plot_ID", 
            multiple = "all") # this is necesarry since there are, apperently, multiple edges per plot 


# ----- 1.1.3.2. estimate parameters for edge lines -----------------------------------------------
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



# 1.1.3.2 edgde coordinates,  line parameters, intersections (coor --------

# set up gerade from 2 points manually
forest_edges_HBI.man <- forest_edges_HBI %>% 
  filter(e_form %in% c("1", "2")) %>% 
# find line parameters
  # 1. calculate coordinates for all 
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
  # 3. intercept line y axis b0 : insert known point: XA YA
         # Y_A = b1_AB*X_A + b0_AB -- -b1_AB*X_A --> b0_AB =  Y_A - b1_AB*X_A
  mutate(b0_AB = ifelse(e_form == "1", intercept(X_A, Y_A, b1_AB), NA), 
         b0_AT = ifelse(e_form == "2", intercept(X_T, Y_T, b1_AT), NA),
         b0_BT = ifelse(e_form == "2", intercept(X_T, Y_T, b1_BT), NA)) %>% 
# find x of intercept with circle: insert line equation in circle equation
        # for AB line 
  mutate(X1_inter_AB =intersection_c_lx1(b0_AB, b1_AB,  data_circle$y0[3], data_circle$x0[3], data_circle$r0[3]),
         X2_inter_AB =intersection_c_lx2(b0_AB, b1_AB, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3]), 
         inter_status_AB = ifelse(is.na(X1_inter_AB) & is.na(X2_inter_AB), " no I",      # if 0 solutions
                                  ifelse(X1_inter_AB == X2_inter_AB, "one I",            # if 1 solution
                                         ifelse(X1_inter_AB != X2_inter_AB, "two I"))),  # if 2 solutions
         # for AT line
         X1_inter_AT =intersection_c_lx1(b0_AT, b1_AT, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3]),
         X2_inter_AT =intersection_c_lx2(b0_AT, b1_AT, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3]), 
         inter_status_AT = ifelse(is.na(X1_inter_AT) & is.na(X2_inter_AT), " no I",     # if 0 solutions
                                  ifelse(X1_inter_AT == X2_inter_AT, "one I",           # if 1 solution
                                         ifelse(X1_inter_AT != X2_inter_AT, "two I"))), # if 2 solutions
         # for BT line
         X1_inter_BT =intersection_c_lx1(b0_BT, b1_BT, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3]),
         X2_inter_BT =intersection_c_lx2(b0_BT, b1_BT, data_circle$y0[3], data_circle$x0[3], data_circle$r0[3]), 
         inter_status_BT = ifelse(is.na(X1_inter_BT) & is.na(X2_inter_BT), " no I",            # if 0 solution
                                  ifelse(X1_inter_BT == X2_inter_BT, "one I",                  # if 1 solution
                                         ifelse(X1_inter_BT != X2_inter_BT, "two I"))) ) %>%   # if 2 solutions
  # y intercept wih cirlce: insert x of intercept with circle in equation of line
         # AB line 
  mutate(Y1_inter_AB = b0_AB + b1_AB*X1_inter_AB, 
         Y2_inter_AB = b0_AB + b1_AB*X2_inter_AB, 
         # AT line 
         Y1_inter_AT = b0_AT + b1_AT*X1_inter_AT, 
         Y2_inter_AT = b0_AT + b1_AT*X2_inter_AT, 
         # BT line 
         Y1_inter_BT = b0_BT + b1_BT*X1_inter_BT, 
         Y2_inter_BT = b0_BT + b1_BT*X2_inter_BT) %>% 
  # azimut of intersection points (calcaulte azimute with azimute function, then correct it depending on the quadrant of x_intersection and y_intersection)
         # AB line 
  mutate(azi1_inter_AB = azi_correction(X1_inter_AB, Y1_inter_AB, azimut(X1_inter_AB, Y1_inter_AB, 0, 0)), 
         azi2_inter_AB = azi_correction(X2_inter_AB, Y2_inter_AB , azimut(X2_inter_AB, Y2_inter_AB, 0, 0)),
         # AT line
         azi1_inter_AT = azi_correction(X1_inter_AT, Y1_inter_AT, azimut(X1_inter_AT, Y1_inter_AT, 0, 0)), 
         azi2_inter_AT = azi_correction(X2_inter_AT, Y2_inter_AT, azimut(X2_inter_AT, Y2_inter_AT, 0, 0)), 
         # BT line 
         azi1_inter_BT = azi_correction(X1_inter_BT, Y1_inter_BT, azimut(X1_inter_BT, Y1_inter_BT, 0, 0)), 
         azi2_inter_BT = azi_correction(X2_inter_BT, Y2_inter_BT, azimut(X2_inter_BT, Y2_inter_BT, 0, 0)) ,
    # distance interception centre --> to see if points are actually placed on the rim of the circle 
         inter_1_dist = distance(X1_inter_AB, Y1_inter_AB, 0, 0),     # this is just to control if the whole thing worked and 
    # to calculate the triangles Barycentric coordinates we need 3 points: A, B, C = centre point
       # in case T lies within the circle, we want R to select A and B from the intersection with the circle.
       # Whereby we have to use a wider radius, to make sure that trees located the halfmoon of the circle cut by the triangle (Kreisbogen) are selected too. 
    # when t lies inside the circle (so both lines reach outside) ue only intersception point where direction between inter_AT and AT is equal choose this x, we need a buffer tho  
    # the following statement says:  if T lies within circle check if the slope of x_inter_1  or the slope of x_inter_2 is equal to the slope of AT,
    #                                choose the x which has the same slope (x_inter_1 or x_inter_2)as the second point on the line (A or B) 
    #                                but with a buffer of + 216, which is why it has to be newly calculated 
    # find the intercept of circle and line that prolonges the line between a and t or B and T
     # AT line 
    azi_T_A = azi_correction(X_A, Y_A, azimut(X_A, Y_A, X_T, Y_T)),
    azi_T_AT_inter_1 = azi_correction(X1_inter_AT, Y1_inter_AT, azimut(X1_inter_AT, Y1_inter_AT, X_T, Y_T)),
    azi_T_AT_inter_2 = azi_correction(X2_inter_AT, Y2_inter_AT, azimut(X2_inter_AT, Y2_inter_AT, X_T, Y_T)),
     # BT line
    azi_T_B = azi_correction(X_B, Y_B, azimut(X_B, Y_B, X_T, Y_T)),
    azi_T_BT_inter_1 = azi_correction(X1_inter_BT, Y1_inter_BT, azimut(X1_inter_BT, Y1_inter_BT, X_T, Y_T)),
    azi_T_BT_inter_2 = azi_correction(X2_inter_BT, Y2_inter_BT, azimut(X2_inter_BT, Y2_inter_BT, X_T, Y_T)),
    # for those turning points that lay outside the circle, select the intercetion point with the gratest distance to c and prolong it
    dist_T_AT_inter_1 = distance(X1_inter_AT, Y1_inter_AT, X_T, Y_T), 
    dist_T_AT_inter_2 = distance(X2_inter_AT, Y2_inter_AT, X_T, Y_T), 
    dist_T_BT_inter_1 = distance(X1_inter_BT, Y1_inter_BT, X_T, Y_T), 
    dist_T_BT_inter_2 = distance(X2_inter_BT, Y2_inter_BT, X_T, Y_T), 
    # if azimut T to A  identical to azimut T to intercept 1 A and circle use this intercept (inter_AT_1) for the triable, if azimut T to A identical to azimute T to intercept 2 between A and  circle use this intercept (inter_AT_2)
    X_inter_AT_triangle = case_when(T_dist < 1784 &  azi_T_AT_inter_1 == azi_T_A ~ intersection_c_lx1(b0_AT,b1_AT,0,0, data_circle$rmax[3]*2),
                               T_dist < 1784 & azi_T_AT_inter_2 == azi_T_A ~  intersection_c_lx2(b0_AT, b1_AT, 0, 0,  data_circle$rmax[3]*2),
                               T_dist > 1784 & dist_T_AT_inter_1 > dist_T_AT_inter_2 ~ intersection_c_lx1(b0_AT,b1_AT,0,0, data_circle$rmax[3]*2), 
                               T_dist > 1784 & dist_T_AT_inter_2 > dist_T_AT_inter_1 ~ intersection_c_lx2(b0_AT,b1_AT,0,0, data_circle$rmax[3]*2), 
                               TRUE ~ NA ), 
    X_inter_BT_triangle = case_when(T_dist < 1784 & azi_T_BT_inter_1 == azi_T_B ~ intersection_c_lx1(b0_BT,b1_BT, 0, 0, data_circle$rmax[3]*2),
                               T_dist < 1784 & azi_T_BT_inter_2 == azi_T_B ~  intersection_c_lx2(b0_BT, b1_BT, 0, 0, data_circle$rmax[3]*2),
                               T_dist > 1784 & dist_T_BT_inter_1 > dist_T_BT_inter_2 ~ intersection_c_lx1(b0_BT,b1_BT,0,0, data_circle$rmax[3]*2), 
                               T_dist > 1784 & dist_T_BT_inter_2 > dist_T_BT_inter_1 ~ intersection_c_lx2(b0_BT,b1_BT,0,0, data_circle$rmax[3]*2), 
                               TRUE ~ NA)) %>% 
  # calcualte y to the x that lie in the same direction then the second point on the line, if turning points lies witin circle and lines "reach out"
  mutate(Y_inter_AT_triangle = b0_AT + b1_AT*X_inter_AT_triangle,  
         Y_inter_BT_triangle = b0_BT + b1_BT*X_inter_BT_triangle) 



# ---- combining tree and edge data ---------------------------------------

# next step will be to join the forest edges dataset into the trees datset, 
# via b0 and b1 and then compare the calculated tree_y with the functions result
# if the tree_y is higher then the function_y we have to do something with the tree...
# etc. assiningg another plot iD or something. 
trees_and_edges <-
HBI_trees  %>% 
  # join in edges info per plot
  left_join(., forest_edges_HBI.man, 
            by = c("plot_ID", "e_ID", "e_type", "e_form")) %>% 
  # calculate the Y of the edge for the x of the tree
  mutate(Y_AB_tree =  b0_AB  + b1_AB *X_tree,
         # new approach by Johanna Garthe
         # insert y and x of tree in implizite function of line function: 0 = a*x + b - y --> if result > 0 --> group 1, if result <0 --> group 2, if result = 0 --> group 0
         Y_AB_t_implicit = b0_AB  + b1_AB *X_tree - Y_tree, 
         Y_AT_t_implicit = b0_AT + b1_AT *X_tree - Y_tree,
         Y_BT_t_implicit = b0_BT  + b1_BT *X_tree - Y_tree         ) %>%
  # filter for trees that lay withing the angle of the interception
     # therefore i need to find those trees whose azimut lies over the lower and under the higher azimute of the interceptions
     # thus i have to order the interceptions azimutes, elsewise I´ll exclude trees that lie in the "outer" angle of the interception
              # if azimut intersection 1 is lower then azi_inter_2 keep azi_inter_1 as the lower value, else choose the value from inter_2
              # if azimut intersection 2 is higher then azi_inter_1 keep azi_inter_2 as the upper value, else choose the value from inter_1
  mutate(azi_inter_lower = ifelse(azi1_inter_AB < azi2_inter_AB, azi1_inter_AB, azi2_inter_AB), 
        azi_inter_upper = ifelse (azi2_inter_AB > azi1_inter_AB, azi2_inter_AB, azi1_inter_AB)) %>% 
  # assign a tree-edge-status that calls trees with a Y higher then the respective edge-functions Y
    # if edge form == 1 choose only those trees that lie within in the azimutes of the intercepts
   mutate(t_AB_status = ifelse(e_form == 1 & Y_AB_t_implicit == 0, "on line", 
                               ifelse(e_form == 1 & Y_AB_t_implicit > 0, "C", "D")), 
          t_ABT_status = p.in.triangle(X_inter_AT_triangle, X_inter_BT_triangle, X_T, Y_inter_AT_triangle, Y_inter_BT_triangle, Y_T, X_tree, Y_tree),
          t_AT_status = ifelse(Y_AT_t_implicit == 0, "on line", ifelse(Y_AT_t_implicit > 0, "outside", "inside")), 
          t_BT_status = ifelse(Y_BT_t_implicit == 0, "on line", ifelse(Y_BT_t_implicit > 0, "outside", "inside")), 

          # assign combined tree status per plot
          # com_l_status = case_when(t_AB_status == "inside" & is.na(t_AT_status) & is.na(t_BT_status) ~ "inside",
          #                          is.na(t_AB_status) & t_AT_status == "inside" & t_BT_status == "inside" ~ "inside", 
          #                          t_AB_status == "inside" & t_AT_status == "inside" & t_BT_status == "inside" ~ "inside", 
          #                          TRUE ~ "outside")
          )

# Maybe i have to work wit triangle ?
  # https://stackoverflow.com/questions/2049582/how-to-determine-if-a-point-is-in-a-2d-triangle
  # http://totologic.blogspot.com/2014/01/accurate-point-in-triangle-test.html

 forest_edges_HBI.man %>% 
  filter(e_form == "2") %>% 
mutate(b1_AT_inter_1 = slope(X_T, Y_T, X1_inter_AT, Y1_inter_AT), 
       b1_AT_inter_2 = slope(X_T, Y_T, X2_inter_AT, Y2_inter_AT), 
       ) %>% 
  select(plot_ID, b1_AT, b1_AT_inter_1, b1_AT_inter_2)

forest_edges_HBI.man %>% 
  filter(e_form == "1") %>% 
  mutate(b1_AB_inter_1 = slope(X_A, Y_A, X1_inter_AB, Y1_inter_AB ), 
         b1_AB_inter_2 = slope(X_A, Y_A, X2_inter_AB, Y2_inter_AB ), 
  ) %>% 
  select(plot_ID, b1_AB, b1_AB_inter_1, b1_AB_inter_2)

# ----- 3. visulaization  -------------------------------------------------


# ----- 3.1. Visualisation forest edges -----------------------------------
# maybe it´ll help to plot them 






# testing if intersection calculation worked if manually calculated
ggplot() +                             
  geom_circle(data = data_circle, aes(x0 = x0, y0 = y0, r = r0))+ # Draw ggplot2 plot with circle representing sampling circuits 
  # geom_point(data =  trees_and_edges %>% 
  #              filter(e_form == 1), 
  #            aes(X_tree, Y_tree)) +
  geom_point(data = forest_edges_HBI.man %>% 
               filter(e_form == "1"), 
             aes(X1_inter_AB, Y1_inter_AB, colour = "inter 1"))+
  geom_point(data = forest_edges_HBI.man %>% 
               filter(e_form == "1" ), 
             aes(X2_inter_AB, Y2_inter_AB, colour = "inter 2"))+
  # these are  AB for forest types with only 2 edges
  geom_point(data = forest_edges_HBI.man %>%
               filter(e_form == "1"),
               aes(X_A, Y_A, color = "edges A"))+
  geom_point(data = forest_edges_HBI.man %>%
               filter(e_form == "1"),
               aes(X_B, Y_B, color = "edges B"))+
  # geom_abline(data = forest_edges_HBI.man %>% 
  #               filter(e_form == "1"), 
  #             intercept = forest_edges_HBI.man$b0_AB, 
  #             slope = forest_edges_HBI.man$b1_AB)+
  geom_point(data =  trees_and_edges %>% 
               filter(!is.na(e_form) & e_form == "1"), 
             aes(X_tree, Y_tree, color = com_l_status)) +
  facet_wrap(~plot_ID)+ 
  theme_bw()


# plotting trees and interception lines divided in t_line_status
ggplot() +  
  geom_circle(data = data_circle, aes(x0 = x0, y0 = y0, r = r0))+ # Draw ggplot2 plot with circle representing sampling circuits 
  # AB line
  geom_point(data = trees_and_edges %>%
               filter(e_form == "1") %>% 
               inner_join(.,   forest_edges_HBI.man %>% 
                            filter(e_form == "1") %>% 
                            group_by(plot_ID) %>% 
                            summarize(n = n()) %>% 
                            filter(n <= 1), 
                          by = "plot_ID") %>% 
              select(plot_ID, X1_inter_AB, X2_inter_AB, X_A, X_B, Y1_inter_AB, Y2_inter_AB, Y_A, Y_B) %>% 
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
                select(plot_ID, X1_inter_AB, X2_inter_AB, X_A, X_B, Y1_inter_AB, Y2_inter_AB, Y_A, Y_B) %>% 
                to_long(keys = c("X_name",  "Y_name"),
                        values = c( "X_value", "Y_value"),  
                        names(.)[2:5], names(.)[6:9]), 
              aes(x= X_value, y = Y_value, colour = X_name))+
  geom_point(data =  trees_and_edges %>% filter(e_form == "1") %>% 
               inner_join(.,   forest_edges_HBI.man %>% 
                            filter(e_form == "1") %>% 
                            group_by(plot_ID) %>% 
                            summarize(n = n()) %>% 
                            filter(n <= 1), 
                          by = "plot_ID"),
             aes(X_tree, Y_tree, colour = t_AB_status))+
  facet_wrap(~plot_ID)
  # trees

  # AT line
  geom_point(data = trees_and_edges %>% 
              filter(e_form == "2") %>% 
              select(plot_ID, X_A, X_T, Y_A, Y_T) %>% 
              to_long(keys = c("X_name",  "Y_name"),
                      values = c( "X_value", "Y_value"),  
                      names(.)[2:3], names(.)[4:5]), 
            aes(x= X_value, y = Y_value, colour = "AT"))+
  geom_line(data = trees_and_edges %>% 
              filter(e_form == "2") %>% 
              select(plot_ID, X_A, X_T, Y_A, Y_T) %>% 
              to_long(keys = c("X_name",  "Y_name"),
                      values = c( "X_value", "Y_value"),  
                      names(.)[2:3], names(.)[4:5]), 
            aes(x= X_value, y = Y_value, colour = "AT"))+
  # BT line 
  geom_point(data = trees_and_edges %>%
              filter(e_form == "2") %>% 
              select(plot_ID, X_B, X_T, Y_B, Y_T) %>% 
              to_long(keys = c("X_name",  "Y_name"),
                      values = c( "X_value", "Y_value"),  
                      names(.)[2:3], names(.)[4:5]), 
            aes(x= X_value, y = Y_value, colour = "BT"))+
  geom_line(data = trees_and_edges %>%
                filter(e_form == "2") %>% 
                select(plot_ID, X_B, X_T, Y_B, Y_T) %>% 
                to_long(keys = c("X_name",  "Y_name"),
                        values = c( "X_value", "Y_value"),  
                        names(.)[2:3], names(.)[4:5]), 
              aes(x= X_value, y = Y_value, colour = "BT"))+
  facet_wrap(~plot_ID)  


  forest_edges_HBI.man %>% 
    filter(e_form == "1") %>% 
    group_by(plot_ID) %>% 
    summarize(n = n()) %>% 
    filter(n <= 1)



# just AT and BT line
# AT line
ggplot() +  
  geom_circle(data = data_circle, aes(x0 = x0, y0 = y0, r = r0))+ # Draw ggplot2 plot with circle representing sampling circuits
 # AT line whne T outside cirlce
  geom_point(data = trees_and_edges %>% 
               filter(e_form == "2") %>% 
               select(plot_ID, X1_inter_AT, X_T, Y1_inter_AT, Y_T) %>% 
               to_long(keys = c("X_name",  "Y_name"),
                       values = c( "X_value", "Y_value"),   
                       names(.)[2:3], names(.)[4:5]),
             aes(x= X_value, y = Y_value, colour = "AT"))+
  geom_line(data = trees_and_edges %>% 
              filter(e_form == "2") %>% 
              select(plot_ID, X2_inter_AT, X_T, Y2_inter_AT, Y_T) %>% 
              to_long(keys = c("X_name",  "Y_name"),
                      values = c( "X_value", "Y_value"),  
                      names(.)[2:3], names(.)[4:5]),
            aes(x= X_value, y = Y_value, colour = "AT"))+
  # AT line when T inside circle
  geom_point(data = trees_and_edges %>% 
             filter(e_form == "2") %>% 
             select(plot_ID, correct_X_inter_AT, X_T, correct_Y_inter_AT, Y_T) %>% 
             to_long(keys = c("X_name",  "Y_name"),
                     values = c( "X_value", "Y_value"),   
                     names(.)[2:3], names(.)[4:5]),
           aes(x= X_value, y = Y_value, colour = "AT"))+
  geom_line(data = trees_and_edges %>% 
              filter(e_form == "2") %>% 
              select(plot_ID, correct_X_inter_AT, X_T, correct_Y_inter_AT, Y_T) %>% 
              to_long(keys = c("X_name",  "Y_name"),
                      values = c( "X_value", "Y_value"),  
                      names(.)[2:3], names(.)[4:5]),
            aes(x= X_value, y = Y_value, colour = "AT"))+
  # BT line 
  geom_point(data = trees_and_edges %>%
               filter(e_form == "2" ) %>% 
               select(plot_ID, X1_inter_BT, X_T, Y1_inter_BT, Y_T) %>% 
               to_long(keys = c("X_name",  "Y_name"),
                       values = c( "X_value", "Y_value"), 
                       names(.)[2:3], names(.)[4:5]), 
             aes(x= X_value, y = Y_value, colour = "BT"))+
  geom_line(data = trees_and_edges %>%
              filter(e_form == "2") %>% 
              select(plot_ID, X2_inter_BT, X_T, Y2_inter_BT, Y_T) %>% 
              to_long(keys = c("X_name",  "Y_name"),
                      values = c( "X_value", "Y_value"),  
                      names(.)[2:3], names(.)[4:5]), 
            aes(x= X_value, y = Y_value, colour = "BT"))+
  
  # BT line  with T in circle 
  geom_point(data = trees_and_edges %>%
               filter(e_form == "2" & T_dist < 1784) %>% 
               select(plot_ID, correct_X_inter_BT, X_T, correct_Y_inter_BT, Y_T) %>% 
               to_long(keys = c("X_name",  "Y_name"),
                       values = c( "X_value", "Y_value"), 
                       names(.)[2:3], names(.)[4:5]), 
             aes(x= X_value, y = Y_value, colour = "BT"))+
  geom_line(data = trees_and_edges %>%
              filter(e_form == "2"& T_dist < 1784) %>% 
              select(plot_ID, correct_X_inter_BT, X_T, correct_Y_inter_BT, Y_T) %>% 
              to_long(keys = c("X_name",  "Y_name"),
                      values = c( "X_value", "Y_value"),  
                      names(.)[2:3], names(.)[4:5]), 
            aes(x= X_value, y = Y_value, colour = "BT"))+
  # trees
   geom_point(data =  trees_and_edges %>% 
                filter(e_form == "2"),
              aes(X_tree, Y_tree, color = com_l_status))+
  facet_wrap(~plot_ID) 


# truned line in 1 go 
ggplot() +  
  geom_circle(data = data_circle, aes(x0 = x0, y0 = y0, r = r0))+ # Draw ggplot2 plot with circle representing sampling circuits
geom_point(data = trees_and_edges %>%
            filter(e_form == "2") %>% 
            select(plot_ID, correct_X_inter_BT, X_T, correct_X_inter_AT,  correct_Y_inter_BT, Y_T, correct_Y_inter_AT) %>% 
            to_long(keys = c("X_name",  "Y_name"),
                    values = c( "X_value", "Y_value"),  
                    names(.)[2:4], names(.)[5:7]), 
          aes(x= X_value, y = Y_value, colour = "BTA"))+
  facet_wrap(~plot_ID)


# testing if intersection calculation worked
ggplot() +                             
  geom_circle(data = data_circle, aes(x0 = x0, y0 = y0, r = r0))+ # Draw ggplot2 plot with circle representing sampling circuits 
  # geom_point(data =  trees_and_edges %>% 
  #              filter(e_form == 1), 
  #            aes(X_tree, Y_tree)) +
  geom_point(data = forest_edges_HBI %>% 
               filter(e_form == "1"), 
             aes(inter_x1, inter_y1, colour = "inter 1"))+
  geom_point(data = forest_edges_HBI %>% 
               filter(e_form == "1" ), 
             aes(inter_x2, inter_y2, colour = "inter 2"))+
  # these are lines through AB for forest types with only 2 edges
  geom_point(data = forest_edges_HBI %>%
               to_long(keys = c("X_name",  "Y_name"),
                       values = c( "X_value", "Y_value"),  
                       names(.)[11:13], names(.)[14:16]) %>% 
               filter(e_form == "1" & X_name %in% c("X_A", "X_B")), 
             aes(X_value, Y_value, color = "edges"))+
  facet_wrap(~plot_ID)+ 
  theme_bw




# this one displays the trees per pot, the sampling circuis and the respective forest edges measuring points and lines drawn through htem 
# separated by forest type 
ggplot() +                             
  geom_circle(data = data_circle, aes(x0 = x0, y0 = y0, r = r0))+ # Draw ggplot2 plot with circle representing sampling circuits 
  geom_point(data =  trees_and_edges %>% 
               filter(!is.na(e_form)), 
             aes(X_tree, Y_tree, color = t_e_status)) +
# geom_point(data = forest_edges_HBI %>%
#              filter(!is.na(e_form)) %>% 
#              to_long(keys = c("X_name",  "Y_name"), 
#                      values = c( "X_value", "Y_value"),  
#                      names(.)[11:13], names(.)[14:16]), 
          # aes(X_value, Y_value, color = "edges"))+
  # these are lines through AB for forest types with only 2 edges
  geom_point(data = forest_edges_HBI %>%
                to_long(keys = c("X_name",  "Y_name"),
                        values = c( "X_value", "Y_value"),  
                        names(.)[11:13], names(.)[14:16]) %>% 
                filter(e_form == "1" & X_name %in% c("X_A", "X_B")), 
              aes(X_value, Y_value, color = "edges_f1_AB"))+
  geom_line(data = forest_edges_HBI %>%
               to_long(keys = c("X_name",  "Y_name"),
                       values = c( "X_value", "Y_value"),  
                       names(.)[11:13], names(.)[14:16]) %>% 
               filter(e_form == "1" & X_name %in% c("X_A", "X_B")), 
             aes(X_value, Y_value, color = "edges_f1_AB"))+
  # these are lines through AT for forest types with turning point
  geom_point(data = forest_edges_HBI %>%
                to_long(keys = c("X_name",  "Y_name"),
                        values = c( "X_value", "Y_value"),
                        names(.)[11:13], names(.)[14:16]) %>% 
                filter(e_form != "1" & X_name %in% c("X_A", "X_T")), 
              aes(X_value, Y_value, color = "edges_f2_AT"))+
  geom_line(data = forest_edges_HBI %>%
               to_long(keys = c("X_name",  "Y_name"),
                       values = c( "X_value", "Y_value"),
                       names(.)[11:13], names(.)[14:16]) %>% 
               filter(e_form != "1" & X_name %in% c("X_A", "X_T")), 
             aes(X_value, Y_value, color = "edges_f2_AT"))+
  # these are lines through BT for forest types with turning point 
  geom_point(data = forest_edges_HBI %>% 
                to_long(keys = c("X_name",  "Y_name"),
                        values = c( "X_value", "Y_value"),
                        names(.)[11:13], names(.)[14:16]) %>% 
                filter(e_form != "1" & X_name %in% c("X_B", "X_T")), 
              aes(X_value, Y_value, color = "edges_f2_BT"))+
  geom_line(data = forest_edges_HBI %>% 
               to_long(keys = c("X_name",  "Y_name"),
                       values = c( "X_value", "Y_value"),
                       names(.)[11:13], names(.)[14:16]) %>% 
               filter(e_form != "1" & X_name %in% c("X_B", "X_T")), 
             aes(X_value, Y_value, color = "edges_f2_BT"))+
  # these are lines through the function of 
  # geom_point(data = trees_and_edges %>%
  #               filter(!is.na(e_form)), 
  #             aes(X_tree, Y_e_tree, color = "edges_trees"))+
  geom_vline(xintercept = 2000)+
  geom_hline(yintercept=2000)+ 
  #xlim(-1784, 1784)+ 
  #ylim(- 1784, 1784)+ 
  facet_wrap(~plot_ID)





# plotting 
ggplot() +                             
  geom_circle(data = data_circle, aes(x0 = x0, y0 = y0, r = r0))+ # Draw ggplot2 plot with circle representing sampling circuits 
  geom_point(data =  trees_and_edges %>% 
               filter(!is.na(e_form)), 
             aes(X_tree, Y_tree, color = t_e_status)) +
  # geom_point(data = forest_edges_HBI %>%  
  #                           filter(!is.na(e_form)) %>% 
  #                           to_long(keys = c("X_name",  "Y_name"), 
  #                                   values = c( "X_value", "Y_value"),  
  #                                   names(.)[11:13], names(.)[14:16]) %>% 
  #                           mutate(X_value = case_when(X_name == "X_A" ~ x_coord(1784, A_azi),   # change the coordinates to draw the line through 
  #                                                      X_name == "X_B" ~ x_coord(1784, B_azi),
  #                                                      X_name == "X_T" ~ X_value,
  #                                                      TRUE ~ NA), 
  #                                  Y_value = case_when(Y_name == "Y_A" ~ y_coord(1784, A_azi), 
  #                                                      Y_name == "Y_B" ~ y_coord(1784, B_azi),
#                                                      X_name == "Y_T" ~ Y_value,
#                                                      TRUE ~ NA)),  
#            aes(X_value, Y_value, color = "edges_prolonged"))+
geom_point(data = forest_edges_HBI %>%  
             filter(!is.na(e_form)) %>% 
             to_long(keys = c("X_name",  "Y_name"), 
                     values = c( "X_value", "Y_value"),  
                     names(.)[11:13], names(.)[14:16]), 
           aes(X_value, Y_value, color = "edges"))+
  # geom_line(data = forest_edges_HBI %>%  
  #              filter(!is.na(e_form)) %>% 
  #              to_long(keys = c("X_name",  "Y_name"), 
  #                      values = c( "X_value", "Y_value"),  
  #                      names(.)[11:13], names(.)[14:16]) %>% 
  #              mutate(X_value = case_when(X_name == "X_A" ~ x_coord(1784, A_azi),   # change the coordinates to draw the line through 
  #                                         X_name == "X_B" ~ x_coord(1784, B_azi),
  #                                         X_name == "X_T" ~ X_value,
  #                                         TRUE ~ NA), 
  #                     Y_value = case_when(Y_name == "Y_A" ~ y_coord(1784, A_azi), 
  #                                         Y_name == "Y_B" ~ y_coord(1784, B_azi),
#                                         X_name == "Y_T" ~ Y_value,
#                                         TRUE ~ NA)),  
#            aes(X_value, Y_value, color = "edges_prolonged"))+
# geom_smooth(data = forest_edges_HBI %>%  
#                 filter(!is.na(e_form)) %>% 
#                 to_long(keys = c("X_name",  "Y_name"), 
#                         values = c( "X_value", "Y_value"),  
#                         names(.)[11:13], names(.)[14:16]), 
#               aes(X_value, Y_value, color = "edges"), method='lm', se=FALSE)+
geom_smooth(data = forest_edges_HBI %>%
              to_long(keys = c("X_name",  "Y_name"),
                      values = c( "X_value", "Y_value"),  
                      names(.)[11:13], names(.)[14:16]) %>% 
              filter(e_form == "1" & X_name %in% c("X_A", "X_B")), 
            aes(X_value, Y_value, color = "edges_f1_AB"), method='lm', se=FALSE)+
  geom_smooth(data = forest_edges_HBI %>% 
                to_long(keys = c("X_name",  "Y_name"),
                        values = c( "X_value", "Y_value"),
                        names(.)[11:13], names(.)[14:16]) %>% 
                filter(X_name %in% c("X_A", "X_T")), 
              aes(X_value, Y_value, color = "edges_f2_AT"), method='lm', se=FALSE)+
  geom_smooth(data = forest_edges_HBI %>% 
                to_long(keys = c("X_name",  "Y_name"),
                        values = c( "X_value", "Y_value"),
                        names(.)[11:13], names(.)[14:16]) %>% 
                filter(X_name %in% c("X_B", "X_T")), 
              aes(X_value, Y_value, color = "edges_f2_BT"), method='lm', se=FALSE)+
  # geom_smooth(data = trees_and_edges %>% 
  #               filter(!is.na(e_form)), 
  #             aes(X_tree, Y_e_tree, color = "edges_trees"), method='lm', se=FALSE)+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept=0)+ 
  geom_vline(xintercept = 0)+
  geom_hline(yintercept=0)+ 
  #xlim(-1784, 1784)+ 
  #ylim(- 1784, 1784)+ 
  facet_wrap(~plot_ID)


ggplot() +                             
  geom_circle(data = data_circle, aes(x0 = x0, y0 = y0, r = r0))+ # Draw ggplot2 plot with circle representing sampling circuits 
  geom_point(data =  trees_and_edges %>% 
               filter(!is.na(e_form)), 
             aes(X_tree, Y_tree)) +
  geom_point(data =  forest_edges_HBI %>%  
               filter(!is.na(e_form)) %>% 
               to_long(keys = c("X_name",  "Y_name"), 
                       values = c( "X_value", "Y_value"),  
                       names(.)[11:13], names(.)[14:16]), 
             aes(X_value, Y_value, color = "edges"))+
  geom_line(data = forest_edges_HBI %>% 
              filter(!is.na(e_form)) %>%
              to_long(keys = c("X_name",  "Y_name"),
                      values = c( "X_value", "Y_value"),
                      names(.)[11:13], names(.)[14:16]), 
            aes(X_value, Y_value, color = "edges"))+ 
  geom_vline(xintercept = 0)+
  geom_hline(yintercept=0)+ 
  xlim(-1784, 1784)+ 
  ylim(- 1784, 1784)+ 
  facet_wrap(~plot_ID)


ggplot() +                             
  geom_circle(data = data_circle, aes(x0 = x0, y0 = y0, r = r0))+ # Draw ggplot2 plot with circle representing sampling circuits 
  geom_point(data =  trees_and_edges %>% 
               filter(!is.na(e_form)), 
             aes(X_tree, Y_tree, color = t_e_status)) +
  geom_point(data =  forest_edges_HBI %>%  
               filter(!is.na(e_form)) %>% 
               to_long(keys = c("X_name",  "Y_name"), 
                       values = c( "X_value", "Y_value"),  
                       names(.)[11:13], names(.)[14:16]), 
             aes(X_value, Y_value, color = "edges"))+
  geom_line(data =  forest_edges_HBI %>%  
              filter(!is.na(e_form)) %>% 
              to_long(keys = c("X_name",  "Y_name"), 
                      values = c( "X_value", "Y_value"),  
                      names(.)[11:13], names(.)[14:16]), 
            aes(X_value, Y_value, color = "edges"))+
  geom_point(data = trees_and_edges %>% 
               filter(!is.na(e_form)), 
             aes(X_tree, Y_e_tree, color = "edges_trees"))+ 
  geom_line(data = trees_and_edges %>% 
              filter(!is.na(e_form)), 
            aes(X_tree, Y_e_tree, color = "edges_trees"))+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept=0)+ 
  xlim(-1784, 1784)+ 
  ylim(- 1784, 1784)+ 
  facet_wrap(~plot_ID)


ggplot() +                             
  geom_circle(data = data_circle, aes(x0 = x0, y0 = y0, r = r0))+ # Draw ggplot2 plot with circle representing sampling circuits 
  geom_point(data =  trees_and_edges %>% 
               filter(!is.na(e_form)), 
             aes(X_tree, Y_tree, color = t_e_status)) +
  # these are lines through the function of the edges reagrding to the tree coorsinates
  geom_smooth(data = trees_and_edges %>% 
              filter(!is.na(e_form)), 
            aes(X_tree, Y_e_tree, color = "edges_trees"), method='lm', se=FALSE)+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept=0)+ 
  xlim(-1784, 1784)+ 
  ylim(- 1784, 1784)+ 
  facet_wrap(~plot_ID)




# ----- NOTES -------------------------------------------------------------


# ----- assinging tree status depending on position to edge line ----------

# assign trees status depending on position of line through edges points
mutate(t_e_status = case_when(Y_e_tree < 2000 & X_tree < 2000 &  Y_tree < Y_e_tree ~ "tree_outside",   # x negative, y negative (below 2000)
                              Y_e_tree > 2000 & X_tree < 2000 &  Y_tree > Y_e_tree ~ "tree_outside",   # x negative, y positive 
                              Y_e_tree < 2000 & X_tree > 2000 &  Y_tree < Y_e_tree ~ "tree_outside",   # x positive, y negative
                              Y_e_tree > 2000 & X_tree > 2000 &  Y_tree > Y_e_tree ~ "tree_outside",   # x positive, y positive
                              is.na(e_ID) ~ "no_edge",
                              TRUE ~ "tree_inside"))
