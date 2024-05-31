# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the peat land soil inventory
# Functions & require


# ----- 0. SETUP ---------------------------------------------------------------
# ----- 0.1. Packages  ---------------------------------------------------------
## datamanagement
#  install.packages("usethis")
#  install.packages('RPostgreSQL')
#  install.packages("RPostgres")
#  install.packages("DBI")
#   install.packages("here")
#   install.packages("readr")
#   install.packages("tidyverse")
#   install.packages("tibble")
#   install.packages("dplyr")
#   install.packages("data.table")
#   install.packages("broom")
#   install.packages("purrr")
#   install.packages("devtools")
#   install.packages("plyr")
#  install.packages("RODBC")
#  install.packages("rstudioapi")
#  install.packages("gsubfn")
#   ## laTex
#   install.packages("stargazer")  #for compatability with Latex
#   install.packages("tikzDevice") #for compatability with Latex#
#   # visualisation
#   install.packages("ggthemes")
#   install.packages("ggplot2")
#   install.packages("reshape2") #for multiple y values
#   install.packages("ggforce") #for zooming in parts of the plot
#   install.packages("ggrepel")
#   options(tz="CA")
#   install.packages("reshape2")
#   install.packages("gridExtra")
# # analysis
#   install.packages("corrplot")
#   install.packages("AICcmodavg")
# # forest related
#    install.packages("forestmangr")
#   install.packages("rBDAT")
#   install.packages("TapeR")
#  install.packages("pkgbuild")
 # require("devtools")
 #   if (! require("remotes")) 
 #     install.packages("remotes")
 #   remotes::install_gitlab("vochr/tapes", build_vignettes = TRUE)
 #  remotes::install_gitlab("vochr/TapeS", build_vignettes = TRUE)
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
library(RPostgreSQL)
library(RPostgres)
library(DBI)
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
library(RODBC)
library(rstudioapi)
library(gsubfn)
#require(plyr)
# laTex
require(stargazer)  #for compatability with Latex
require(tikzDevice) #for compatability with Latex
# visualisation
require(ggthemes)
require(ggplot2)
require(reshape2) #for multiple y values
require(ggforce) #for zooming in parts of the plot
require(ggrepel)
require(gridExtra)
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
#require(tapes)
require(magrittr)
require(sjmisc)
require(ggforce)                      # Load ggforce package
# spatial 
require(sf)
# require(rgdal)
require(terra)
require(sfheaders)





# ----- 0.3. working directory -------------------------------------------------
here::here()


# ----- 0.4 import parameters etc. for functions --------------------------

# ----- 0.4.1. diameter correction Dahm parameters ------------------------
# change region sheet to x_ld_neu aus code tables
DBH_region <- read.delim(file = here("data/input/general/neu_x_ld.csv"), sep = ";", dec = ",")
DBH_region <- DBH_region %>% dplyr::select(ICode, KurzD,  LangD, bl, region)
colnames(DBH_region) <- c("icode_reg", "reg_shortG", "reg_longG","country",  "region")

# change tangenz csv to neu_k_tangens from code tabellen in 
DBH_tan <- read.delim(file = here("data/input/general/neu_k_tangenz.csv"), sep = ";", dec = ",")
DBH_tan <- DBH_tan %>% dplyr::select( ba_bwi, region, tangenz, Icode)
colnames(DBH_tan) <- c("SP_BWI1",  "region", "tangenz", "icode")
dput(DBH_tan)


# ----- 0.4.2. nitrogen content datasets ----------------------------------
## nitrogen content in foliage based on nitrgen content in leafe samples of the national soil inventory 
  # import
N_con_f <-  read.delim(file = here("output/out_data/out_data_momok/N_con_foliage_MOMOK.csv"), sep = ",", dec = ",")
  # harmonize N_con_f compartiment names with trees compartiments names, which are based on TapeS compartiment names
N_con_f <- N_con_f %>% mutate(compartiment = case_when(compartiment == "f" ~ "ndl",
                                                       TRUE ~ compartiment))
## nitrogen content in woody compartiments and needles 
  # reference: 
  # Rumpf, Sabine & Schoenfelder, Egbert & Ahrends, Bernd. (2018). Biometrische Schätzmodelle für Nährelementgehalte in Baumkompartimenten.
  # https://www.researchgate.net/publication/329912524_Biometrische_Schatzmodelle_fur_Nahrelementgehalte_in_Baumkompartimenten, 
  # Tab.: 3.2 - 3.6, S. 10
  # import
N_con_w <-  read.delim(file = here("output/out_data/out_data_momok/N_con_wood_Rumpf.csv"), sep = ",", dec = ",")
  # hamronizing compartimebnt names between nitrogen datasets and TapeS based trees dataset compartiment names
N_con_w <- N_con_w %>% mutate(compartiment = case_when(compartiment == "f" ~ "ndl", 
                                                       compartiment == "swb" ~ "sb", 
                                                       compartiment == "stwb" ~"stb",
                                                       compartiment == "fw" ~ "fwb", 
                                                       TRUE ~ compartiment))

## belowground biomass notrogen contents in percent (mgg/1000)
  # reference: 
  # Jacobsen et al. 2003; 
  # Gehalte chemischer Elemente in Baumkompartimenten Literaturstudie und Datensammlung, 
  # Berichte des Forschungszentrums Waldökosysteme, Reihe B, Bd. 69, 2003
  # Carsten Jacobsen, Peter Rademacher, Henning Meesenburg und Karl Josef Meiwes
  # Niedersächsische Forstliche Versuchsanstalt;
  # N Gehalte Grobwurzeln (D > 2mm), Tab. 7
  # import
N_con_bg <- as.data.frame(cbind("SP_group" = c("EI", "BU" , "FI" , "KI", "KIN" , "BI" , "LA"), 
                                "N_con" = c(3.71,3.03, 4.14, 1.77,  1.76, 3.7, 2.8)/1000, 
                                "compartiment" = c("bg", "bg", "bg", "bg", "bg", "bg", "bg")))


# 0.4.3. import species names dataest x_bart ------------------------------
# species names & codes 
SP_names_com_ID_tapeS <- read.delim(file = here("output/out_data/x_bart_tapeS.csv"), sep = ";", dec = ".", 
                                    encoding = "UTF-8", 
                                    stringsAsFactors=FALSE
                                    ) 
# the join always works like this: 
# left_join(., SP_names_com_ID_tapeS %>% 
#             mutate(char_code_ger_lowcase = tolower(Chr_code_ger)), 
#           by = c("SP_code" = "char_code_ger_lowcase"))



# 0.4.4. create sampling cuicits dataset ------------------------------
# creating dataset with information about the concentric sampling circles
data_circle <- data.frame(x0 = c(0,0,0),       # x of centre point of all 3 circles is 0 
                          y0 = c(0,0,0),       # y of centre point of all 3 circles is 0 
                          r0 = c(5.64, 12.62, 17.84), # darius in m
                          rmax = c(30.00, 30.00, 30.00)) # these are the radi of the sampling circuits in m


# ----- 1. Functions -----------------------------------------------------------
# this function exports tables with list columns to csv: 
# https://stackoverflow.com/questions/48024266/save-a-data-frame-with-list-columns-as-csv-file
tibble_with_lists_to_csv <- function(tibble_object, file_path_name) {
  set_lists_to_chars <- function(x) { 
    if(class(x) == 'list') { y <- paste(unlist(x[1]), sep='', collapse=', ') } else { y <- x  } 
    return(y) }
  new_frame <- data.frame(lapply(tibble_object, set_lists_to_chars), stringsAsFactors = F)
  write.csv(new_frame, file=file_path_name, row.names = FALSE)
}

# this function will enable to run a greorefferenced version of a script or not
georef_on_off <- function(path.script.nogeo, path.script.geo, georefference){
  switch(georefference, 
         "georefferenced" = source(path.script.geo), 
         "not_georefferenced" = source(path.script.nogeo))
}

# ----- 1.1. area circle --------------------------------------------------
# area of a circle
c_A <- function(r){
  circle_area <- r^2*pi;
  return(circle_area)
}

# ----- 1.2 DBH  ----------------------------------------------------------
# 1.2.1. DBH class --------------------------------------------------------
DBH_c_function <- function(dbh){
  # create label for diameter classes according to BZE3 Bestandesaufnahmeanleitung
  labs_DBH <- c(seq(5, 55, by = 5)) ; 
  DBH_c <- cut(as.numeric(dbh),                               # cut the diameter
               breaks = c(seq(5, 55, by = 5), Inf),  # in sequences of 5
               labels = labs_DBH,                    # and label it according to labs (1.4.1)
               right = FALSE);
  return(DBH_c)
}

# 1.2.2. DBH correction --------------------------------------------------------
# conversion of DBH from not-breastheight to brestheight diameter via BWI method
# source: BWI Methodikband, 5.2.1.1. - BHD bei Probebäumen mit geänderter Messhöhe - Regressionsverfahren
#         BWI Regressionsgleichung bereitgestellt von Heino Polley, verwendet in Datenerfassungssoftware 2002, 2008 und 2012
DBH_BWI <- function(d.mm, d.h.cm){
  dbh_cm = (d.mm*(1.0+(0.0011*(d.h.cm -130))))/10 # diveided by 10 to return cm
  return(dbh_cm)
}

# correcting diameter that was not measured at 1.3m (Breastheight)
# Refference: \\wo-sfs-001v-ew\INSTITUT\a7bze\ZZ_BZE3_Bestand\Erfassungssoftware\BHD_Umrechung
#               Brusthöhendurchmesser bei abweichender Meßhöhe nach Abholzigkeitsfunktion lt. Stefan Dahm 
#             tabes required to perfom linkage between trees dataset and tangenz dataste: x_ba.Ba_BWI1, x_bl.Region, [k_Tangenz (Ba_Bwi1, Region)].Ba_BWI1+Region bwi.xyk1.k_tangenz 
DBH_Dahm <- function(plot.id, d.mm, d.h.cm, spec){
  # avoidind error: "longer object length is not a multiple of shorter object length" by https://stackoverflow.com/questions/10865095/why-do-i-get-warning-longer-object-length-is-not-a-multiple-of-shorter-object-l
  # we have to do two things: 
  # link the species with the right ICode by their BWI_SP_oode
  # link the plot ID with the state it´s situated in and the state with the region code

  ## select thh correct tangenz accordint to species and
  # select the ba_BWI1 sep
  sp_tan <- spec # unique(as.character(DBH_SP$SP_BWI1[which(toupper(DBH_SP$SP_BWI1)%in% toupper(spec))]))
  # determine ld based on plot ID
  # determine state the plot is located in by it´s plot ID
  # for plots with 5 digits it´s the first number, for plots with 6 the first two 
  # these numbers comply with the column icode_reg in the DBH_region dataset so that the extracted country code can be immediately translated into 
  # the DBH tangez region 
  # https://www.geeksforgeeks.org/count-number-of-characters-in-string-in-r/
  # https://stackoverflow.com/questions/61954941/extract-first-x-digits-of-n-digit-numbers
  ld_icode <- ifelse(str_length(plot.id) == 5, substr(plot.id, 1, 1), substr(plot.id, 1, 2))
  #ld <- DBH_region$icode_reg[which(grepl(ld_plot, DBH_region$icode_reg))]
  # select the region belonign to the state that belongs to the plot_ID from DBH_region dataset 
  reg_tan <- unique(DBH_region$region[which(DBH_region$icode_reg %in% ld_icode)])
  # select the tangenz belonging to the reion and species from DBH_tan dataset
  tangenz <- DBH_tan$tangenz[DBH_tan$SP_BWI1 %in% as.character(sp_tan) & DBH_tan$region %in% reg_tan][1]
  # calcualte DBH according to function by Stefan Dahm
  dbh.dahm.cm = ((d.mm)+2*((d.h.cm)-130)/tangenz )/10
  
   return(dbh.dahm.cm)
}



# ----- 1.3 dbh to size class ----------------------------------------------------------
# defining regernation size class
sizeclass_to_d <- function(size.class){
  d.cm = case_when(size.class == 0 ~ 0, 
                   size.class == 1 ~ (4.9+0)/2, 
                   size.class == 2 ~ (5.9+5)/2,
                   TRUE ~ (6.9+6)/2);
  return(d.cm)
}




## age classes label
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


pick_utm <- function(lon){
  if(lon < 6){
    utm <- 31
  }else if(lon >= 6 & lon < 12){
    utm <- 32
  }else if(lon >= 12){
    utm <- 33
  }
  return(utm)
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

# ----- 1.8.1.1. geometrical forms areas  ----------------------------------------------------------------------------
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



# ----- 1.8.1.2. triangle cirlce intersection area via polygone  ----------------------------------------------------------------------------

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


# ----- 1.8.2. final dge area  ----------------------------------------------------------------------------

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

# ---- 1.9. HEIGHTS ------------------------------------------------------
# ---- 1.9.1. height coefficient selection ------------------------------------------------------
# this function is used to select the coefficients of the height models depending on the R2
# for x, y,a, b (can be whatever)
f = function(x,y,a,b){
  # do the following: if x is na, or x is smaller then y, then use a, if not use b 
  answer <- ifelse(is.na(x)| x < y, a, b)
  return(answer)}


# ---- 1.9.2. einheitshoehenkurve------------------------------------------------------
# ---- 1.9.2.1. Sloboda ------------------------------------------------------
ehk_sloboda <- function(spec, d_i, d_mean, d_g, h_g) { #, id_broken) {
  # regarding the units: all diameters in mm and all heights in dm 
  # od, if the /10 in the h_pred fruntion would be removed: all diameter in cm all heights in m 
  # spec = H_SP_group 
  # d_i = measured dbh in mm
  # d_g = dbh of a stem representing the mean of the stand in mm
  # h_g = height of a stem representing the mean of the stand in dm 
  # h_pred = predicted height in m
  
  k0 <- c(fi = 0.183, ta = 0.079, dgl = 0.24, ki = 0.29, lae = 0.074, bu = 0.032, ei = 0.102, alh = 0.122, aln = 0.032)
  k1 <- c(fi = 5.688, ta = 3.992, dgl = 6.033, ki = 1.607, lae = 3.692, bu = 6.04, ei = 3.387, alh = 5.04, aln = 4.24)
  k2 <- c(fi = 0.29, ta = 0.317, dgl = 0.33, ki = 0.388, lae = 0.342, bu = 0.367, ei = 0.488, alh = 0.47, aln = 0.461)
  h_mean <- (h_g - 1.3)/(exp(k0[tolower(spec)]*(1 - d_mean/d_g))*exp(k1[tolower(spec)]*(1/d_mean - 1/d_g))) + 1.3;
  h_pred <- ((1.3 + (h_mean - 1.3)*exp(k0[tolower(spec)]*(1 - d_mean/d_i))*exp(k1[tolower(spec)]*(1/d_mean - 1/d_i)))/10); # divide by 10 to get height in m
  # this part is silenced, because there is no Hoehenkennzahl documented for MoMoK 
  # and BZE because they dont do a Winkelzähprobe
  # Reduction factor depending on whether crown or stem is broken or not 
  # if (length(id_broken) == length(d_i)) {
  #   f_red <- rep(1.0, length(d_i));
  #   f_red[which(id_broken == 0)] <- 1.0;
  #   f_red[which(id_broken == 1)] <- 1 - 2/h_pred[which(id_broken == 1)];
  #   f_red[which(id_broken == 2)] <- 1 - k2[tolower(spec[which(id_broken == 2)])];
  # } else if (length(id_broken) == 1) {
  #   if (id_broken == 0) f_red <-  1.0
  #   else if (id_broken == 1) f_red <- 1 - 2/h_pred
  #   else if (id_broken == 2) f_red <- 1 - k2[tolower(spec)]
  # }  
  return(h_pred)#*f_red) # predicted height in m
}

# ---- 1.9.2.2. Curtis ------------------------------------------------------
# --> this one is only applied when there is literally not information to calculate the height, 
# except of the diameter
h_curtis <- function(spec, d) {
  # spec = species group
  # diameter in mm 
  b0 <- c(fi = 434.1235, bu = 382.0202, ta = 453.5538, ki = 359.7162, lae = 421.4473, dgl = 481.5531, ei = 348.3262);
  b1 <- c(fi = -65586.6915, bu = -51800.9382, ta = -81132.5221, ki = -42967.9947, lae = -60241.2948, dgl = -81754.2523, ei = -46547.3645);
  b2 <- c(fi = 3074967.1738, bu = 2374368.3254, ta = 4285801.5636, ki = 1763359.9972, lae = 2895409.6245, dgl = 4193121.2406, ei = 2119420.9444);
  return((b0[tolower(spec)] + b1[tolower(spec)]*1/d + b2[tolower(spec)]*1/d^2)/10)   # divide by 10 to transform dm into meters
}

# ---- 1.9.3. self-fitted nls height models ------------------------------------------------------
# ---- 1.9.3.1. species- & plot-wise self-fitted nls models ------------------------------------------------------
# self made nls models for heights per species across all plots
h_nls_SP <- function(spec, d){
  # https://statisticsglobe.com/convert-data-frame-column-to-a-vector-in-r
  b0 <- dplyr::pull(coeff_H_SP, b0, SP_code);
  b1 <- dplyr::pull(coeff_H_SP, b1, SP_code);
  b2 <- dplyr::pull(coeff_H_SP, b2, SP_code);
  return(b0[spec] * (1 - exp( -b1[spec] * d))^b2[spec])
}

# ---- 1.9.3.2. species-wise self-fitted nls models ------------------------------------------------------
# self mase nls models for heights per species per plot
h_nls_SP_P <- function(plot_spec, d) {
  # because I cannot combine 3 variabels in one vector, 
  b0 <- coeff_H_SP_P %>% unite(SP_P_ID, plot_ID, SP_code, sep = "", remove = FALSE) %>% dplyr::pull(b0, SP_P_ID);
  b1 <- coeff_H_SP_P %>% unite(SP_P_ID, plot_ID, SP_code, sep = "", remove = FALSE) %>% dplyr::pull(b1, SP_P_ID);
  b2 <- coeff_H_SP_P %>% unite(SP_P_ID, plot_ID, SP_code, sep = "", remove = FALSE) %>% dplyr::pull(b2, SP_P_ID);
  return(b0[plot_spec] * (1 - exp( -b1[plot_spec] * d))^b2[plot_spec])
}


# 1.9.4. height dreisatz when models fail ---------------------------------------------------------------------------------------
h_proportional <- function(dg.cm, hg.m, dbh.cm){
  h.m = (dg.cm/hg.m)*dbh.cm
  return(h.m)
}





# 1.10. select the correct inventory name -------------------------------------------------------------------
# https://www.geeksforgeeks.org/check-if-a-numeric-value-falls-between-a-range-in-r-programming-between-function/
inv_name <- function(inv.year){
  inv <- ifelse(inv.year < 2004, "warning", 
                ifelse(between(inv.year, 2005, 2007), "BZE2", 
                ifelse(between(inv.year, 2011, 2013), "HBI", 
                ifelse(between(inv.year, 2022, 2025), "BZE3",
                       ifelse(between(inv.year, 2033, 2035) , "BZE4", "BZE5"
         )))));
  return(inv)
}




# 1.11. Biomass -------------------------------------------------------------------------------------------
# 1.11.1. Wutzler: Foliage on beach trees Biomass functions -------------------------------------------------
# reference: 
# Wutzler, Thomas & Wirth, Christian & Schumacher, Jens. (2008). 
# Generic biomass functions for Common beech (Fagus sylvatica) in Central Europe: 
# Predictions and components of uncertainty. 
# Canadian Journal of Forest Research - Journal Canadien de la Recherche Forestiere, 
# v.38, 1661-1675 (2008). 38. 10.1139/X07-194. 


### foliage of broadleaved trees according to Wutzler et al. 2008
# to aply this function the Oberhoehe and the elevation above sea level are required

# DHC 4c model
Wutzler_fB_L <- function(d, h, alt, si){   # DHC 4c model
  b = 0;
  b0 = 0.0561;
  b1 = 2.07;
  b2 = (-1.09);
  bssi = 0.0137;
  bsalt = (-0.00000329);
  # from marks file: ((b0 + bsalt*alt) * DBH^(b1+bsi*SI) * H^b2
  # from Wutzler 2008, Annex 3: 
  #         biomass = (b0 + 0+ bssi*si+ bsalt*atitude)*(DBH^b1)*(H^b2)
  return(# so its either this: (b0 + 0 + bssi*SI + bsalt*alt)*d^b1*h^b2) 
    # or this from Mark: 
    (b0+bsalt*alt)*d^(b1+bssi*si)*h^b2)
}

#DH3, 4a Model, table: 4   
Wutzler_fB_L1 <- function(d, h){  #DH3 4a Model 
  b0 = 0.0377;
  b1 = 2.43;
  b2 = (-0.913);
  return(b0*d^b1*h^b2)
}

# 1.11.2. GHGI: belowground Biomass functions -------------------------------------------------
## belowground phytomass GHGI
GHGI_bB <- function(spec, d){
  # function for soft hard woods requires DBH in mm, 
  # thus we have to transform input DBH in cm into DBH in mm by dividing by 10
  dbh <- ifelse(spec != "shw", d, d*10);
  b0 <- c(fi = 0.003720, ki = 0.006089, bu = 0.018256, ei= 0.028000, shw = 0.000010);#shwr =0.000010, shwrs = 0.000116);
  b1 <- c(fi = 2.792465, ki = 2.739073, bu = 2.321997, ei= 2.440000, shw = 2.529000); #shwr =2.529000, shwrs = 2.290300);
  # this would return the root + stump biomas for soft hardwoods but only the root biomass for all other species groups
  # ifelse(spec != "shw", b0[spec]*d^b1[spec], (b0[spec]*d^b1[spec])+(0.000116*d^2.290300))
  return(b0[spec]*dbh^b1[spec]) 
}

# 1.11.3. regeneration ---------------------------------------------------------------

# 1.11.3.1. below <1.3m height Biomass functions -------------------------------------------------

# 1.11.3.1.1. GHGI: below <1.3m height Biomass functions -------------------------------------------------
## above ground biomass for trees <1.3m GHGI (equation: 6, coefficient table: 4)
GHGI_aB_Hb1.3 <- function(spec, h){  # here instead of species group i´ll link the formula to a column with he categories broadleafed and coniferous trees
  b0 <- c(NB = 0.23059, LB = 0.04940);
  b1 <- c(NB = 2.20101, LB = 2.54946);
  return(b0[spec]*h^b1[spec])
}

# 1.11.3.1.2. Wolff: below <1.3m height Biomass kompartiments -------------------------------------------------
# reference: 
# Tab. 4:
# DVFFA – Sektion Ertragskunde, Jahrestagung 2009
# "Biomasse- und Elementgehalte im Unterwuchs – erste Ergebnisse für Flächen des Forstlichen Umweltmonitorings in Rheinland-Pfalz"
# Wolff, B.*; Bolte, A.**; Bielefeldt, J.**; Czajkowski, T.**
# * Fachhochschule Eberswalde (FHE), Fachgebiet Wald und Umwelt, A.-Möller-Straße 1, 16225 Eberswalde
# ** Johann Heinrich v. Thünen-Institut (vTI), Institut für Waldökologie und Waldinventuren,  A.-Möller-Straße 1, 16225 Eberswalde

# we need the throot to transform the whd-to-h equation
# https://stackoverflow.com/questions/58447310/how-to-use-the-nthroot-function-in-r
nthroot = function(x,n) {
  (abs(x)^(1/n))*sign(x)
}

h.to.whd <- function(h, spec_wolff){
  # the original function is as follows: 
  # h = a*whd^b
  # now we have to transform it into wdh = ...
  # the steps are: 
  # h/a = whd^b
  # bth-root(h/a) = whd
  # h is the trees height in cm 
  # whd is the radius at hte stem base (Wurzelhalsdurchmesser) in mm
 a <- c(BAH = 4.3239, BI = 9.50205, BU= 4.8909, 
        VB = 6.2529, EI = 8.2332, ES =  3.4668, FKD = 10.26,  #in BWI: SLB; FKD = fauliger Kreuzdorn --> Faulbaum --> Rhamnus frangula
        FI = 3.674, GIN = 12.322,  # Ginster = GIN
        HOL =  5.5999,  # holunder
        KI =  7.9661);
 b <- c(BAH = 1.2531, BI = 1, BU = 1.1404, VB = 1.0844, EI = 1, 
        ES = 1.3483, FKD = 1.0269,  #in BWI: SLB; FKD = fauliger Kreuzdorn --> Faulbaum --> Rhamnus frangula
        FI = 1.0905 , GIN = 1, # Ginster
        HOL =  1.1832,
        KI = 0.9366);
 
 whd.mm = (nthroot((h/a[spec_wolff]), b[spec_wolff])) # input for the function is in mm 
 
 return(whd.mm)
  
  
}

wolff.bio.below.1m <- function(h.cm, spec_wolff, compartiment){
    # h           is column H_cm
    # spec_wolff  is column RG_Wolff_bio
    # compartiment is in"switch" but to use it as input for the poorter functions one should choose "stem"
    
    a.wdh <- c(BAH = 4.3239, BI = 9.50205, BU= 4.8909, 
               VB = 6.2529, EI = 8.2332, ES =  3.4668, FKD = 10.26,  #in BWI: SLB; FKD = fauliger Kreuzdorn --> Faulbaum --> Rhamnus frangula
               FI = 3.674, GIN = 12.322,  # Ginster = GIN
               HOL =  5.5999,  # holunder
               KI =  7.9661);
    b.wdh <- c(BAH = 1.2531, BI = 1, BU = 1.1404, VB = 1.0844, EI = 1, 
               ES = 1.3483, FKD = 1.0269,  #in BWI: SLB; FKD = fauliger Kreuzdorn --> Faulbaum --> Rhamnus frangula
               FI = 1.0905 , GIN = 1, # Ginster
               HOL =  1.1832,
               KI = 0.9366);
    # diameter at the stem base (Wurzelhalsdurchmesser) in mm
    whd = (nthroot((h.cm/a.wdh[spec_wolff]), b.wdh[spec_wolff]));  
    
    # parameters for total abovground biomass
    a <- c(BAH = -4.116664, BI = -4.374745, BU= -5.329977, 
           VB = - 5.511373, EI = -6.890656, ES = -5.980901,
           FKD = -5.805027, FI = -4.365029, GIN = -5.007328, 
           HOL = -5.596683, KI = -4.054296);
    b <- c(BAH = 2.103417, BI = 1.952172, BU = 1.504128, 
           VB = 1.102974, EI = 0.992884, ES = 1.042600, 
           FKD = 1.268980, FI = 1.873336, GIN = 1.475571, 
           HOL = 1.133249, KI = 1.586179);
    c  <- c(BAH = 0.499551, BI = 0.731565, BU = 1.182288, 
            VB = 1.326973, EI = 1.769866, ES = 1.519227, 
            FKD = 1.293988, FI = 0.977148, GIN = 1.069508, 
            HOL = 1.366231, KI = 0.911674);
    # paremeters for branch + stem compartimen
    d <- c(BAH = -5.255099, BI = 4.586260, BU = -6.160292, 
           VB = -6.050794, EI = -7.338351, ES = -6.623514, 
           FKD = -6.678752, FI = -5.295721, GIN = -5.329752, 
           HOL =  -6.697415, KI = -6.221465);
    e <- c(BAH = 2.055909, BI = 2.317019, BU = 1.719560, 
           VB = 1.218037, EI = 1.100074, ES = 1.241859, 
           FKD = 1.709183, FI = 1.905118, GIN = 1.568195, 
           HOL = 1.685368, KI = 1.506163);
    f <- c(BAH = 0.729275, BI = 0.495968, BU = 1.204329, 
           VB = 1.361229, EI = 1.739085, ES = 1.496878, 
           FKD = 1.231672, FI = 1.044837, GIN = 1.079322, 
           HOL = 1.207866, KI = 1.327712); 
    # paremeters for folliage compartiment
    x <- c(BAH = -3.729638, BI =  -6.108247, BU = -5.385717, 
           VB = -7.216398, EI = -8.201379, ES = -6.584228, 
           FKD =  -5.943471, FI = -4.823170, GIN =  -6.240936,
           HOL =  -4.956678, KI = -3.179742);
    y <- c(BAH = 2.350116, BI = 1.369987, BU = 1.260222, 
           VB = 0.497185, EI = 0.900570, ES = 0.877136, 
           FKD = 0.875969, FI = 1.873277, GIN = 0.701319, 
           HOL = 0.486142, KI = 1.767087);
    z <- c(BAH = -0.067977, BI = 1.175875, BU = 0.945780, 
           VB = 1.581494, EI = 1.833317, ES = 1.42240, 
           FKD = 1.193355, FI = 0.884113, GIN = 1.109381, 
           HOL = 1.248850, KI = 0.392809);
    
    # unfortunately the publication does not display which unit the output biomass has
    switch(
      compartiment,
      "ag" = exp(a[spec_wolff])*whd^b[spec_wolff]*h.cm^c[spec_wolff],
      "stem" = exp(d[spec_wolff])*whd^e[spec_wolff]*h.cm^f[spec_wolff],
      "foliage" = exp(x[spec_wolff])*whd^y[spec_wolff]*h.cm^z[spec_wolff]
    )
    
  }

# 1.11.3.1.3. Poorter: below <1.3m height Biomass kompartiments -------------------------------------------------
Poorter_rg_RSR_RLR <- function(ag.kg, spec, compartiment){ # instead of the species I have to put NH_LH here
  # equation to transform aboveground into belowground biomass : stem:root-ratio
 # quadratische ergänzung der Funktion: stem = a + b1*root + b2*root^2
                                      # sten = b2*root^2 + b1*root + a
                                      # stem = a*root^2 + b*root + c
                                      # 0 = a*root^2 + b*root + c-y
                                      # c = c-y
                                      # (-b-sqrt(b^2-4*a*c))/2*a = x1
                                      # (-b + sqrt(b^2-4*a*c))/2*a = x1
  # https://www.mathepanik.de/Klassen/Klasse_10/Lektion_Kl_10_L_parabeln_gleichung_loesen.php
  c <- c(NB = -0.070, LB = -0.097);   # a
  b <- c(NB = 1.236, LB = 1.071);     # b1  
  a <- c(NB = -0.0186, LB = 0.01794); # b2
  # 10-log of belowground biomass in g (*1000)
  ag_g <- ag.kg*1000;
  # withdraw y from c to create a function that equals to 0 so we apply the quadratic function
  cy <- as.data.frame(c[spec]-log10(ag_g))[,1];
  # calculate two possible results for the biomass at the given y
  log.bg.x1 = (-b[spec]-sqrt(b[spec]^2-4*abs(a[spec])*abs(cy)))/(2*a[spec]);
  log.bg.x2 = (-b[spec]+sqrt(b[spec]^2-4*abs(a[spec])*abs(cy)))/(2*a[spec]);
  # a) backtranform  logarithm: https://studyflix.de/mathematik/logarithmus-aufloesen-4573
  # log_a(b) = c ---> b = a^c
  # b) transform leaf biomass in g into kg by dividing by 1000
  bg.kg.x1 = as.data.frame((10^log.bg.x1)/1000)[,1]
  bg.kg.x2 = as.data.frame((10^log.bg.x2)/1000)[,1]
  ag_minus_x1 = ag.kg - bg.kg.x1
  ag_minus_x2 = ag.kg - bg.kg.x2
  
  # if x1 is lower then zero while x2 is higher then zero but below the stem mass choose x2, if not choose x1
   bg_bio_kg =  ifelse(bg.kg.x1 >= 0 & abs(ag_minus_x1) < abs(ag_minus_x2), bg.kg.x1, 
                      ifelse(bg.kg.x2 >= 0 & abs(ag_minus_x2) < abs(ag_minus_x1), bg.kg.x2, 
                             NA))
  
  # equation to transform belowground into foliage biomass : leaf:root-ratio
  bg_g <- bg_bio_kg*1000;             # belowground biomass in g (*1000)
  a1 <- c(NB = 0.243, LB =  0.090);
  b1 <- c(NB =  0.924, LB =  0.889);
  b2 <- c(NB = -0.0282, LB = -0.0254);
  log.10.f_bio <- a1[spec]+ b1[spec]* log10(bg_g)+ b2[spec]*log10(bg_g)^2;
  # a) backtranform  logarithm: https://studyflix.de/mathematik/logarithmus-aufloesen-4573
  # log_a(b) = c ---> b = a^c
  # b) transform leaf biomass in g into kg by dividing by 1000
  f_bio_kg <- (10^log.10.f_bio)/1000;
  
  # equation to transform root into stem biomass
  a3 <- c(NB = -0.070, LB = -0.097);   # a
  b3 <- c(NB = 1.236, LB = 1.071);     # b1  
  b4 <- c(NB = -0.0186, LB = 0.01794); # b2
  # 10-log of belowground biomass in g (*1000)
  log.10.stem_bio <- a3[spec]+ b3[spec]*log10(bg_g) + b4[spec]*log10(bg_g)^2;
  # a) backtranform  logarithm: https://studyflix.de/mathematik/logarithmus-aufloesen-4573
  # log_a(b) = c ---> b = a^c
  # b) transform leaf biomass in g into kg by dividing by 1000
  stem_bio_kg <- (10^log.10.stem_bio)/1000;
  
  switch(compartiment, 
         "bg" = bg_bio_kg, 
         "foliage" = f_bio_kg, 
         "stem" = stem_bio_kg, 
         "x1" = bg.kg.x1, 
         "x2" = bg.kg.x2)
}

# 1.11.3.2. biomass RG >1.3m ----------------------------------------------

# 1.11.3.2.1. GHGI: biomass RG >1.3m ----------------------------- --------
## above ground biomass for trees >1.3m height and < 10cm DBH GHGI (equation: 5, coefficients table: 3)
# B_H1.3_DBHb10 = above-ground phytomass in kg per individual tree,
# b0, bs, b3 = coefficients of the function,
# DBH = Diameter at breast height in cm,
# ds = Diameter-validity boundary for this function = 10 cm/
GHGI_aB_H1.3_DBHb10 <- function(spec, d){
  b0 <- c(fi = 0.41080, ki = 0.41080, bu = 0.09644 , ei= 0.09644, shw =0.09644);
  bs <- c(fi = 26.63122 , ki = 19.99943 , bu = 33.22328, ei= 28.94782, shw =16.86101);
  b3 <- c(fi = 0.01370, ki = 0.00916, bu = 0.01162, ei= 0.01501, shw = -0.00551);
  ds <- c(fi = 10, ki = 10, bu = 10, ei= 10, shw =10);
  return(b0[spec]+((bs[spec] - b0[spec])/ds[spec]^2 + b3[spec]*(d-ds[spec]))*d^2)
}

# 1.11.3.2.2. compartiments biomass RG >1.3m ----------------------------- --------
Poorter_rg_with_bg <- function(bg.kg, spec, compartiment){ # instead of the species I have to put NH_LH here
  # equation to transform aboveground into belowground biomass : stem:root-ratio
  
  # equation to transform belowground into foliage biomass : leaf:root-ratio
  bg_g <- bg.kg*1000;             # belowground biomass in g (*1000)
  a1 <- c(NB = 0.243, LB =  0.090);
  b1 <- c(NB =  0.924, LB =  0.889);
  b2 <- c(NB = -0.0282, LB = -0.0254);
  log.10.f_bio <- a1[spec]+ b1[spec]* log10(bg_g)+ b2[spec]*log10(bg_g)^2;
  # a) backtranform  logarithm: https://studyflix.de/mathematik/logarithmus-aufloesen-4573
  # log_a(b) = c ---> b = a^c
  # b) transform leaf biomass in g into kg by dividing by 1000
  f_bio_kg <- (10^log.10.f_bio)/1000;
  
  # equation to transform root into stem biomass
  a3 <- c(NB = -0.070, LB = -0.097);   # a
  b3 <- c(NB = 1.236, LB = 1.071);     # b1  
  b4 <- c(NB = -0.0186, LB = 0.01794); # b2
  # 10-log of belowground biomass in g (*1000)
  log.10.stem_bio <- a3[spec]+ b3[spec]*log10(bg_g) + b4[spec]*log10(bg_g)^2;
  # a) backtranform  logarithm: https://studyflix.de/mathematik/logarithmus-aufloesen-4573
  # log_a(b) = c ---> b = a^c
  # b) transform leaf biomass in g into kg by dividing by 1000
  stem_bio_kg <- (10^log.10.stem_bio)/1000;
  
  switch(compartiment, 
         "foliage" = f_bio_kg, 
         "stem" = stem_bio_kg)
}



# 1.11.4. deadwood --------------------------------------------------------
# 1.11.4.1. Volume Deadwood according to BWI ----------------------------------------------------------
# here we have to consider, that in case of MoMok there were no different types pf diameter taken
# e.g  min diameter, max diameter, middle diam
# the volume calautation follows the procedure described in BWI Methodikband, 

# volume for deadwood when 
# Dm was taken (Mittendurchmesser) or 
# Totholztyp == 3 (liegend, stark, Burchstück) & L_m <3m
V_DW_cylinder <- function(d.m, l.m){
  V.m3 = ((d.m/2)^2*pi)*l.m
  return(V.m3)
}

# Volume for deadwood when 
# !(DW_type %in% c(1, 6, 4) | DW_type == 3 & L_m > 3m)
V_DW_whole <- function(spec_tpS, d, dh, l){          # I don´t know if this can work
  spp = na.omit(spec_tpS); # for this Ill first have to create species groups that correspond with TapeS
  Dm = na.omit(as.list(d));
  Hm = na.omit(as.list(1.3)); # height at which diameter was taken, has to be 1.3m becaus ehtese are the deadwood pieces that do stil have a DBH
  Ht = na.omit(l);
  obj.dw <- tprTrees(spp, Dm, Hm, Ht, inv = 4);
  return (tprVolume(obj.dw[obj.dw@monotone == TRUE]))
}

# ---- 1.11.4.2. Biomass Deadwood according to BWI ----------------------------------------------------------
B_DW <- function(V.m3, dec.type, SP){     # dec_SP = a column that holds the degree of decay and the species type has to be created (united)
  # *1000 to transform density in g/cm3 into kg/m3: https://www.translatorscafe.com/unit-converter/de-DE/density/4-1/Gramm/Kubikzentimeter-Kilogramm/Kubikmeter/
  # spec_ 
  # 2_ = coniferous tree
  # 1_ = broadleafed tree
  # 3_ = oak
  # d.m is diameter in meter
  # l.m is length in meter
  # dec.type is the state of decay
  # sp is the species
  
  # calculate volume
  # create combination of decay and species to select correct BEF 
  SP_dec <- paste0(SP, "_", dec.type)
  # introcude biomass conversion factor
  BEF <- c("2_1" = 0.372*1000, "2_2" = 0.308*1000, "2_3" = 0.141*1000, "2_4" = 0.123*1000,   # conferous trees according to Faver
           "1_1" = 0.58*1000, "1_2" = 0.37*1000, "1_3" = 0.21*1000, "1_4" = 0.26*1000,       # broadleaved trees according to Müller-Ursing
           "3_1" = 0.58*1000, "3_2" = 0.37*1000, "3_3" = 0.21*1000, "3_4" = 0.26*1000);      # oak
  # calculate biomass in kg based on volume in m3 
  B.kg <- as.numeric(V.m3)*BEF[SP_dec]
           
  return(B.kg)
}

# relative density for tapeS deadwood compartiments
# Biomasse unzersetzt * (100% - relative Veränderung der Dichte) = 
# B * (1-(D1 - D2/ D1))
rdB_DW <- function(B, dec.type, SP){     # a column that holds the degree of decay and the species type has to be created (united)
  # create combination of decay and species to select correct BEF 
  SP_dec <- paste0(SP, "_", dec.type)
  # relatve density
  rd <- c("2_1" = 1, "2_2" = (1-((0.372-0.308)/0.372)), "2_3" = (1-((0.372-0.141)/0.372)) , "2_4" = (1-((0.372-0.123)/0.372)) ,   # relative change in density of conferous trees according to Faver based on 100% = 0.372
          "1_1" = 1 , "1_2" =  (1-((0.58-0.37)/0.58)), "1_3" = (1-((0.58-0.21)/0.58)) , "1_4" = (1-((0.58-0.26)/0.58)) ,       #  relative change in density of broadleaved trees according to Müller-Ursing basen on 100% = 0.58
          "3_1" = 1, "3_2" = (1-((0.58-0.37)/0.58)), "3_3" = (1-((0.58-0.21)/0.58)), "3_4" = (1-((0.58-0.26)/0.58)) );      # relative change in density of oak trees according to Müller-Ursing basen on 100% = 0.58
  return(B*rd[SP_dec])
}



# ----- 1.12. Nitrogen stock  --------------------------------------------
# nitrogen stock for woody compartiments
N_all_com <- function(B, N_spec_w_rumpf, N_spec_f_BZE, N_spec_bg_Jacobsen, comp.trees){
  n_con_w <- N_con_w  %>%  filter(compartiment != "ndl") %>% unite("SP_com", SP_BWI:compartiment, remove = FALSE)%>% dplyr::pull(N_con, SP_com);
  n_con_f <- N_con_f %>% dplyr::pull(N_con, N_f_SP_group_MoMoK) 
  # this function may have to be be adapted to the new dataset of the NSI which provides accurate N cocntents for all species and foliage
  # proably I will also have to assign new species groups to acces the foliage dataset correctly
  n_con_bg <- c(EI = 3.71, BU = 3.03, FI = 4.14, KI = 1.77, KIN = 1.76, BI = 3.7, LA = 2.8)/1000;# divide concentration in mg per g by 1000 to get concentration in percent/ decimal number of percent 
  # unite the compartiment and species to select the correct nitrogen content
  SP_compart_Rumpf <- paste0(N_spec_w_rumpf, "_", comp.trees);
  
 # calculate nitrogen content in the aboveground and belowground compartiments but without sums (total or total aboveground N) 
  N <- case_when(
    comp.trees == "ndl" ~ 0, # as.numeric(B)*as.numeric(n_con_f[N_spec_f_BZE]), # accordint to the BZE we don´t have to deliver this compartiments nitrogen stock so we wont 
    comp.trees == "bg" ~ as.numeric(B)*as.numeric(n_con_bg[N_spec_bg_Jacobsen]), 
    !(comp.trees %in% ("ag, total, ndl, bg")) ~ as.numeric(B)*as.numeric(n_con_w[SP_compart_Rumpf]),
    TRUE ~ NA)
  
  return(N)
}


# 1.13. carbon stock ------------------------------------------------------
carbon <- function(B){
 C <- B*0.5;
 return(C)
}



# 1.14. KG to Tons --------------------------------------------------------

ton <- function(kg){
  t = kg/1000
  return(t)
}







# 1.15 assign stand type species group ------------------------------------

standtype <- function(bot.genus, LH.NH){ 
  sttype_SP_group = case_when(bot.genus == "Picea" ~ "FI", 
                              bot.genus == "Pinus" ~ "KI",
                              !(bot.genus %in% c("Picea", "Pinus")) & LH.NH == "NB" ~ "aNH", 
                              bot.genus == "Fagus" ~ "BU", 
                              bot.genus == "Quercus" ~ "EI", 
                              !(bot.genus %in% c("Fagus", "Quercus")) & LH.NH == "LB" ~ "aLH", 
                              TRUE ~ NA);
  return(sttype_SP_group)
}




# 1.16. assign SD class for plausibility test and visulalization ----------

SD_class <- function(sd_pred_diff, diff_b){
  # call it group 1 if the difference is below or equal the respective SD
  # call it group 2 if the difference is below or equal the 2 times respective SD (SD*2)
  # call it group 3 if the difference is below or equal the 3 times respective SD (SD*3)
  # call it group 4 if the difference is higher the  3 times the respective SD (SD*4)
  
  # transfer negative differnce in postiive ones to enable comparisson with SD with is +/- 
  diff_b_betrag = ifelse(diff_b <0, diff_b*(-1), diff_b);  
  
  sd_cl_1 = 1*sd_pred_diff ;
  sd_cl_2 = 2*sd_pred_diff ;
  sd_cl_3 = 3*sd_pred_diff ;
  
  sd_cl_df <- ifelse(diff_b_betrag <= sd_cl_1 , "1",
                     ifelse(diff_b_betrag > sd_cl_1  & diff_b_betrag <= sd_cl_2 , "2",
                            ifelse(diff_b_betrag > sd_cl_2  & diff_b_betrag <= sd_cl_3 , "3", 
                                   ifelse(diff_b_betrag > sd_cl_3 , "4", "5"))));
  return(sd_cl_df)
}




#1.17.  summarize data with varying grouing variables ---------------------------
# chatGBT helped me here 
summarize_data <- function(data, group_vars, columns, operation) {
  sd_df <- data %>%
    group_by(across(all_of(group_vars), .names = "{.col}")) %>%
    summarise(across(all_of(columns), \(x) sd(x, na.rm = TRUE))) # sd, na.rm = TRUE))
  mean_df <- data %>%
    group_by(across(all_of(group_vars), .names = "{.col}")) %>%
    summarise(across(all_of(columns),\(x) mean(x, na.rm = TRUE))) # mean, na.rm = TRUE))
  sum_df <- data %>%
    group_by(across(all_of(group_vars), .names = "{.col}")) %>%
    summarise(across(all_of(columns), \(x) sum(x, na.rm = TRUE)))
  switch(operation, 
         sd_df = sd_df,
         mean_df = mean_df, 
         sum_df = sum_df)
}




# 1.18. biodiversity index variabels --------------------------------------


# 1.18.1. RMS root mean square ----------------------------------------------
# https://www.statisticshowto.com/quadratic-mean/
RMS <- function(x){
  rms = sqrt(sum(x^2)/length(x));
  return(rms)
} 



# 1.18.2. biodiv index variable score -------------------------------------------------
# Reference: 
# Storch, F., Dormann, C.F. & Bauhus, 
# J. Quantifying forest structural diversity based on large-scale inventory data: 
# a new approach to support biodiversity monitoring. For. Ecosyst. 5, 34 (2018). 
# https://doi.org/10.1186/s40663-018-0151-1

FSI <- function(x){
  x.min = min(x);
  x.max = max(x); 
  variable.score = (as.numeric(x) - as.numeric(x.min))/(as.numeric(x.max) - as.numeric(x.min));
  return(variable.score)
}



# 1.18.3. bark type function ----------------------------------------------

bark_type <- function(my.dbh.cm, chr.code.ger, output){
  # my.dbh.cm = trees_data$DBH_cm[1]
  # chr.code.ger = trees_data$Chr_code_ger[1]
  
  # select the botanical name of the speices according to the german abbreviation of the species name
  bot.name <- SP_names_com_ID_tapeS$bot_name[SP_names_com_ID_tapeS$Chr_code_ger ==  chr.code.ger]
  # select bark type speices gorup belonging to the botanical name of the species
  my.bark.spp <- unique(SP_names_com_ID_tapeS$bark_type_SP_group[SP_names_com_ID_tapeS$bot_name ==  bot.name]);
  #my.bark.spp <- unique(SP_names_com_ID_tapeS$bark_type_SP_group[SP_names_com_ID_tapeS$bot_name == SP_names_com_ID_tapeS$bot_name[SP_names_com_ID_tapeS$Chr_code_ger == chr.code.ger]]);
  # select the barl type of the bark type species group
  my.bark.type <- unique(bark_div$bark_type[bark_div$species ==  my.bark.spp]);
  
  # identify the upper and lower borders for the bark subtypes
  u_border_cm_TY1 <- as.numeric(bark_div$u_border_cm_TY1[bark_div$species == my.bark.spp]);
  l_border_cm_TY2 <- as.numeric(bark_div$l_border_cm_TY2[bark_div$species == my.bark.spp]);
  u_border_cm_TY2 <- as.numeric(bark_div$u_border_cm_TY2[bark_div$species == my.bark.spp]);
  l_border_cm_TY3 <- as.numeric(bark_div$l_border_cm_TY3[bark_div$species == my.bark.spp]);
  
  # determine subtype of the barktype according to DBH
  my.bark.sub.type <- 
     ifelse(!is.na(u_border_cm_TY1) & my.dbh.cm < u_border_cm_TY1, "TY1", 
            # if there is an upper and lower border for type 2 and the diamter is within it
            ifelse(!is.na(l_border_cm_TY2) & !is.na(u_border_cm_TY2) & my.dbh.cm >= l_border_cm_TY2 & my.dbh.cm < u_border_cm_TY2,  "TY2", 
                   # i# if there is only a lower border for type 2 and the diameter is bejond it 
                   ifelse(!is.na(l_border_cm_TY2) & is.na(u_border_cm_TY2) & is.na(l_border_cm_TY3) & my.dbh.cm >= l_border_cm_TY2, "TY2",
                          # if there is a lower border for type 2 and for type 3 but no upper for type 2 and the diameter is between type 2 and 3
                          ifelse(!is.na(l_border_cm_TY2) & is.na(u_border_cm_TY2) & !is.na(l_border_cm_TY3) & my.dbh.cm >= l_border_cm_TY2 & my.dbh.cm < l_border_cm_TY3, "TY2", 
                                 # if there is a lower border for type 3 and the diameter is above it 
                                 ifelse(!is.na(l_border_cm_TY3) & my.dbh.cm >= l_border_cm_TY3,"TY3",
                                        # if there are no diameter specific bark types --> for most of the spp. species groups 
                                        ifelse(is.na(u_border_cm_TY1) & is.na(l_border_cm_TY2) & is.na(u_border_cm_TY2) & is.na(l_border_cm_TY3), my.bark.type, 
                                               "no subtype"
                                        ))))));
 
   # combine bark type and subtype, if they are not the same: 
  # because "paste0" comand creates list we hafd to use thise one:https://stackoverflow.com/questions/10341114/alternative-function-to-paste
  my.bark.type.subtype <- ifelse(my.bark.sub.type == my.bark.type | my.bark.sub.type == "no subtype", as.character(my.bark.type), as.character(fn$paste("$my.bark.type_$my.bark.sub.type")))
  
  # combine species group and bark subtype (if there is one) 
  # --> according to a Mail from Felix Storch,  this is the porper way to assign bark type for the FSI calculation since the species matters for the bark type 
  my.bark.spp.subtype <- ifelse(my.bark.sub.type == my.bark.type | my.bark.sub.type == "no subtype", as.character(my.bark.spp), as.character(fn$paste("$my.bark.spp$my.bark.sub.type")))
  
  

  switch(output,
         "bark_ty_subty" = my.bark.type.subtype,
         "bark_spp_subty" = my.bark.spp.subtype,
         "bark_ty" = my.bark.type,
         "bark_sub_ty" = my.bark.sub.type, 
         "bark_type_species" = my.bark.spp)
}


# 1.18.4. FSI LT fruit and pollination type -------------------------------
fruit_type <- function(age, chr.code.ger, output){
  # age = trees_data$age[1]
  # chr.code.ger = "Es" # trees_data$Chr_code_ger[1]
  # select the correct fruit type species group, based on the fruit typ species group that is assinged to the german_character speices name in the x_bart dataset
  my.fruit.spp <- unique(SP_names_com_ID_tapeS$fruit_type_SP_group[SP_names_com_ID_tapeS$bot_name == SP_names_com_ID_tapeS$bot_name[SP_names_com_ID_tapeS$Chr_code_ger == chr.code.ger]]);
  # select the fructivication age of he respective species group
  my.fruct.age <- fruit_div$fruct_age[fruit_div$species == my.fruit.spp];
  # if the age of the tree is above the fructivication age selet the fruit type belonging to that respective species
  my.fruit.type <- ifelse(age >= my.fruct.age, fruit_div$fruit_type[fruit_div$species == my.fruit.spp], NA);
  # ifthe age of the tree is above the fructivication age, select the pollination type belonging to that respective fruit type species group (my.fruit.spp)
  my.poll.type <- ifelse(age >= my.fruct.age, fruit_div$pollination_type[fruit_div$species == my.fruit.spp], NA);
  
  switch (output,
          "fruit" = my.fruit.type, 
          "pollen" = my.poll.type
  )
  
}



# 1.19. connection to SQL server ------------------------------------------
sqlconnection <- function(my_db, my_server, my_port, my_username, my_passwort){
  dbconnection <- dbConnect(RPostgres::Postgres(), 
                            dbname = my_db, 
                            host=my_server, 
                            port=my_port, 
                            user=my_username, 
                            password = my_passwort
                            )  
  
  return(dbconnection)
  }
 


# 2. writing datasets 11.12.2023 ----------------------------------------------------------------
# if womeone does not have the x-bart tables or the info about the nitrogen content but still wants to use this functions 
# the following part will allow to write the datasets instead of importing them
# functionto write code that the returns a for a r readable list to save df in R skript permently
# https://stackoverflow.com/questions/68649942/how-can-i-attach-dataframes-to-an-r-script
# dput(df)
# i will have to do this for 
  # - x_bart
  # - x_tangenz

# 2.1. x_bart ------------------------------------------------------------------
# this is the result of a dput of SP_names_com_ID_tapes which will later on be x_bart, state now: 11.12.2023, 11:54
#dput(SP_names_com_ID_tapeS)
if(!exists('SP_names_com_ID_tapeS')){
  SP_names_com_ID_tapeS <-as.data.frame(structure(list(X = 1:232, 
                             Nr_code = c(100L, 101L, 102L, 103L, 
                                         106L, 109L, 110L, 111L, 112L, 113L, 114L, 115L, 118L, 119L, 120L, 
                                        121L, 122L, 125L, 127L, 130L, 131L, 132L, 201L, 202L, 203L, 204L, 
                                        210L, 211L, 231L, 235L, -9L, -2L, -1L, 104L, 105L, 107L, 108L, 
                                        116L, 117L, 123L, 124L, 126L, 128L, 129L, 133L, 134L, 135L, 136L, 
                                        137L, 138L, 139L, 140L, 141L, 142L, 143L, 144L, 145L, 146L, 147L, 
                                        148L, 150L, 151L, 152L, 153L, 154L, 156L, 157L, 158L, 159L, 160L, 
                                        161L, 162L, 163L, 164L, 165L, 166L, 167L, 168L, 199L, 200L, 205L, 
                                        206L, 207L, 208L, 209L, 212L, 213L, 214L, 215L, 216L, 217L, 218L, 
                                        219L, 220L, 221L, 222L, 223L, 224L, 225L, 226L, 227L, 228L, 229L, 
                                        230L, 232L, 233L, 234L, 236L, 237L, 238L, 239L, 299L, 303L, 304L, 
                                        305L, 306L, 307L, 308L, 309L, 310L, 311L, 312L, 313L, 314L, 315L, 
                                        316L, 317L, 318L, 319L, 320L, 321L, 322L, 323L, 324L, 325L, 326L, 
                                        327L, 328L, 329L, 330L, 331L, 332L, 333L, 334L, 335L, 336L, 337L, 
                                        338L, 339L, 340L, 341L, 342L, 343L, 345L, 346L, 347L, 348L, 349L, 
                                        350L, 351L, 352L, 353L, 354L, 355L, 356L, 357L, 358L, 359L, 360L, 
                                        361L, 362L, 363L, 364L, 365L, 366L, 367L, 368L, 369L, 370L, 371L, 
                                        372L, 373L, 374L, 375L, 376L, 377L, 378L, 379L, 380L, 381L, 382L, 
                                        383L, 384L, 385L, 386L, 387L, 388L, 389L, 390L, 391L, 392L, 393L, 
                                        394L, 395L, 396L, 398L, 399L, 400L, 401L, 403L, 404L, 406L, 407L, 
                                        408L, 409L, 410L, 411L, 412L, 413L, 414L, 415L, 416L, 417L, 418L, 
                                        419L, 420L, 421L, 422L, 423L, 424L, 425L, 426L), 
                             Chr_code_ger = c("Ah", 
                                              "SAh", "BAh", "Er", "Bi", "HBu", "EKa", "RBu", "GEs", "Kir", 
                                              "Pa", "Eis", "REi", "Rob", "Wei", "Vbe", "Li", "Ul", "BPa", "Els", 
                                              "Es", "FAh", "WTa", "KTa", "Lae", "ELa", "SKi", "GKi", "RFi", 
                                              "Str", "fehlt", "nicht ausgeprägt", "nicht erhoben", "SEr", "WEr", 
                                              "SBi", "MBi", "TEi", "SEi", "WLi", "SLi", "As", "BUl", "BWe", 
                                              "FUl", "FLu", "GPa", "GEr", "Hi", "HBi", "JBi", "Me", "Mi", "Nu", 
                                              "Pl", "RKa", "SWe", "SNu", "SPa", "Sp", "SPi", "SuE", "FTk", 
                                              "STk", "Tu", "WEs", "WAp", "WBi", "WZw", "ZEi", "AsH", "Edl", 
                                              "Ei", "WPa", "WWE", "WDe", "WDs", "HPa", "sLb", "Ta", "JLa", 
                                              "Fi", "GFi", "SFi", "Ki", "Dgl", "KiB", "BKi", "SZy", "STa", 
                                              "DKi", "ETa", "Eib", "HTa", "HLä", "Ara", "Le", "LTa", "Mes", 
                                              "NTa", "OFi", "PKi", "PTa", "RZy", "Se", "TaS", "FiS", "SLä", 
                                              "VTa", "WFi", "ZKi", "sNd", "SWa", "KFi", "LZe", "CTa", "GWa", 
                                              "FrA", "SbA", "KAp", "KBi", "HEr", "ÖEb", "WEb", "OBu", "BSl", 
                                              "WPi", "Eri", "EWa", "OWa", "PWa", "SaW", "TWa", "HZw", "GBu", 
                                              "Ohb", "HaN", "Euk", "FaM", "SbE", "MaE", "ESt", "EOe", "HoB", 
                                              "OPl", "MaB", "StE", "PoE", "UnE", "GaE", "SeE", "VaE", "FlE", 
                                              "PyE", "SiE", "KoE", "MzE", "AaW", "LaW", "AfT", "Jbb", "GJb", 
                                              "BHe", "SHe", "ELo", "GMy", "SSL", "TPi", "SWd", "SKd", "FjB", 
                                              "KaS", "AzL", "GaB", "ZAE", "BTa", "GTa", "SpT", "AtZ", "HiZ", 
                                              "WZe", "MZy", "KKi", "KaK", "AKi", "ScK", "ShK", "SeK", "ISK", 
                                              "MoK", "ZyZ", "Hei", "KEi", "AEi", "PEi", "LEi", "BEi", "MEi", 
                                              "AuE", "ArE", "EAh", "ScA", "SiA", "TSA", "GöB", "PMB", "SHi", 
                                              "BHa", "ZgW", "AGR", "GGR", "BGB", "PET", "BSt", "RuK", "TäK", 
                                              "APl", "KSP", "LoK", "StW", "ScD", "GbE", "PuW", "MaW", "KoW", 
                                              "ÖMb", "SoT", "ESZ", "SiL", "HLT", "FUE", "FeU"), 
                             name = c("Ahorn",
                                      "Spitz-Ahorn", "Berg-Ahorn", "Erle", "Birke", "Hainbuche", "Edel-Kastanie", 
                                     "Rot-Buche", "Gewoehnliche Esche", "Vogel-Kirsche", "Pappel", 
                                     "Eiche", "Rot-Eiche", "Robinie", "Weide", "Vogelbeere", "Linde", 
                                     "Ulme", "Balsampappel", "Elsbeere", "Esche", "Feldahorn", "Weiß-Tanne", 
                                     "Kuesten-Tanne", "Laerche", "Europaeische Laerche", "Schwarz-Kiefer", 
                                     "Gewoehnliche Kiefer", "Rotfichte", "Strobe", "Merkmal vergessen, nicht rekonstruierbar oder unbekannt", 
                                     "Merkmal nicht ausgeprägt/nicht vorhanden", "Merkmal nicht erhoben", 
                                     "Schwarz-Erle", "Grau-Erle", "Sand-Birke", "Moor-Birke", "Trauben-Eiche", 
                                     "Stiel-Eiche", "Winter-Linde", "Sommer-Linde", "Aspe/Zitterpappel", 
                                     "Bergulme", "Bruchweide", "Feldulme", "Flatterulme", "Graupappel", 
                                     "Gruenerle", "Hickory", "Hybridbirke", "Japanbirke", "Mehlbeere", 
                                     "Mispel", "Walnussbaum", "Platane", "Rosskastanie", "Salweide", 
                                     "Schwarznuss", "Schwarzpappel", "Speierling", "Spirke", "Sumpfeiche", 
                                     "Fruehbl. Traubenkirsche", "Spaetbl. Traubenkirsche", "Tulpenbaum", 
                                     "Weißesche", "Wildapfel", "Wildbirne", "Wildzwetschge", "Zerreiche", 
                                     "Aspenhybriden", "Edellaubholz", "Eiche", "Weisspappel", "Weißweide", 
                                     "Eingriffeliger Weißdorn", "Weißdorn", "Hybrid Pappel", "sonstige Laubbaeume", 
                                     "Tanne", "Japanische Laerche", "Fichte", "Gewoehnliche Fichte", 
                                     "Sitka-Fichte", "Kiefer", "Douglasie", "Bankskiefer", "Bergkiefer", 
                                     "Scheinzypresse", "Silbertanne", "Drehkiefer", "Edeltanne", "Eibe", 
                                     "Hemlocktanne", "Hybridlaerche", "Japantanne", "Lebensbaum", 
                                     "Lowes Tanne", "Urweltmammutbaum", "Nordmannstanne", "Omorika Fichte", 
                                     "Pechkiefer", "Purpurtanne", "Rauchzypresse", "Riesenmammutbaum", 
                                     "Sicheltanne", "Stechfichte", "Sumpflaerche", "Veits Tanne", 
                                     "Weißfichte", "Zirbelkiefer", "sonstige Nadelbaeume", "Stinkende Wacholder", 
                                     "Kaukasus-Fichte", "Libanon-Zeder", "Kilikische Tanne", "Gemeiner Wacholder", 
                                     "Französische Ahorn", "Schneeball-Ahorn", "Kulturapfel", "Kulturbirne", 
                                     "Herzblättrige Erle\n", "Östliche Erdbeerbaum", "Westlicher Erdbeerbaum", 
                                     "Orient-Buche", "Breitblättrige Steinlinde", "Wilde Pistazie", 
                                     "Erikagewächs Strauch", "Griechischer Wacholder", "Stech-Wacholder", 
                                     "Phönizische Wacholder", "Stink-Wacholder/Sadebaum", "Spanischer Wacholder", 
                                     "Hauszwetschge", "Gewöhnliche Buchsbaum", "Orientalische Hainbuche", 
                                     "Haselnussstrauch", "Eukalypten", "? Fagus moesiaca", "Schmalblättrige Esche", 
                                     "Manna-Esche", "Europäische Stechpalme", "Echtem Ölbaum", "Hopfenbuche", 
                                     "Orientalische Platane", "Mandelbaum", "Stech-/Kermes-Eiche", 
                                     "Portugiesische-/Zen-Eiche", "Ungarische Eiche", "Gallen-/Färber-Eiche", 
                                     "Steineiche", "Valonia Eiche", "Flaumeiche", "Pyrenäen-Eiche", 
                                     "Steineiche", "Korkeiche", "Mazedonische Eiche", "Asch-Weide", 
                                     "Lavendel-Weide", "Afrikanische Tamariske", "Johannisbrotbaum", 
                                     "Gewöhnliche Judasbaum", "Baumheide", "Spanische Besen-Heide", 
                                     "Echte Lorbeer", "Gemeine Myrte", "Steinlinde", "Terpentin-Pistazie", 
                                     "Schwarzer Weißdorn", "Stechpalmen-Kreuzdorn", "Fjellbirke", 
                                     "Kanarische Stechpalme", "Azoren-Lorbeer", "Gagelbaum", "Zentralanatolische Eichen", 
                                     "Bulgarische Tanne", "Griechische Tanne", "Spanische Tanne", 
                                     "Atlas-Zeder", "Himalaya-Zeder", "Weiße Zeder", "Mittelmeerzypresse", 
                                     "Kalabrische Kiefer", "Kanarische Kiefer", "Aleppo-Kiefer", "Schlangenhaut-Kiefer", 
                                     "Schlangenhaut-Kiefer", "See-Kiefer", "Italienische Steinkiefer", 
                                     "Monterey-Kiefer", "Zypern-Zeder", "Hartwiss-Eiche", "Kasnak-Eiche", 
                                     "Aleppo-Eiche", "Persische Eiche", "Libanon-Eiche", "Brant'sche Eiche", 
                                     "Mount Tabor-Eiche", "Auchers Eiche", "Armenische Eiche", "Eschen-Ahorn / Eschenblättriger Ahorn", 
                                     "Schneeball-Ahorn / Schneeballblättriger Ahorn", "Silber-Ahorn", 
                                     "Tatarische Steppen-Ahorn", "Götterbaum", " Papiermaulbeerbaum", 
                                     "Schindelborkige Hickorynuss / Schuppenrinden-Hickorynus", "Baum-Hasel, auch Tärkische Hasel", 
                                     " Zweigriffelige Weißdorn", "Alpen-Goldregen", "Gewöhnlicher Goldregen", 
                                     "Blauglockenbaum", "Petterie", "Breitblättrige Steinlinde", "Rumelische Kiefer, Mazedonische Kiefer oder Balkankiefe", 
                                     "Tränenkiefer", "Ahornblättrige Platane/Bastard-Platane, Gewöhnliche Pla", 
                                     "Kanadische Schwarz-Pappel oder Rosenkranz-Pappel", "Lorbeerkirsche/Pontische Lorbeer-Kirsche und Kirschlorb", 
                                     "Steinweichsel, Felsen-oder Weichselkirsche", "Schlehdorn, Schwarzdorn", 
                                     "Gestieltblätige Eiche", "Purpurweide / Bachweide", "Mandelweide", 
                                     "Korb-Weide / Hanf-Weide", "Ostalpen-Mehlbeere, Alpen-Mehlbeere und Alpen-Oxelbirne", 
                                     "Sommertamariske", "Echte Sumpfzypresse", "Silber-Linde", "Kanadische Hemlocktanne / Kanadische Schierlingstanne", 
                                     "Flatterulme / Flatterrüster", "Feldulme / Iper"), 
                             bot_name = c("Acer spp.",
                                          "Acer platanoides", "Acer pseudoplatanus", "Alnus spp.", "Betula spp.", 
                                         "Carpinus betulus", "Castanea sativa", "Fagus sylvatica", "Fraxinus excelsior", 
                                         "Prunus avium", "Populus spp.", "Quercus spp.", "Quercus rubra", 
                                         "Robinia pseudoacacia", "Salix spp.", "Sorbus aucuparia", "Tilia spp.", 
                                         "Ulmus spp.", "Populus balsamifera", "Sorbus torminalis", "Fraxinus excelsior", 
                                         "Acer campestre", "Abies alba", "Abies grandis", "Larix spp.", 
                                         "Larix decidua", "Pinus nigra", "Pinus sylvestris", "Picea abies", 
                                         "Pinus strobus", "-2", "-2", "-2", "Alnus glutinosa", "Alnus incana", 
                                         "Betula pendula", "Betula pubescens", "Quercus petraea", "Quercus robur ", 
                                         "Tilia cordata", "Tilia platyphyllos", "Populus tremula", "Ulmus glabra", 
                                         "Salix fragilis", "Ulmus campestris", "Ulmus laevis (vorher effusa)", 
                                         "Populus x canescens", "Alnus viridis", "Carya spp.", "Betula x hybrida", 
                                         "Betula platyphylla var. Japonica", "Sorbus aria", "Mespilus germanica", 
                                         "Juglans regia", "Platanus x hybrida", "Aesculus hippocastanum", 
                                         "Salix caprea", "Juglans nigra", "Populus nigra", "Sorbus domestica", 
                                         "Pinus uncinata", "Quercus palustris", "Prunus padus", "Prunus serotina", 
                                         "Liriodendron tulipifera", "Fraxinus americana", "Malus sylvestris", 
                                         "Pyrus pyraster", "Prunus cerasifera", "Quercus cerris", "Populus hybrides", 
                                         "alii frons noble", "Quercus robur x petraea", "Populus alba", 
                                         "Salix alba", "Crataegus monogyna", "Crataegus spp.", "Populus hybrides", 
                                         "alii frons", "Abies spp.", "Larix kaempferi ", "Picea spp.", 
                                         "Picea abies ", "Picea sitchensis", "Pinus spp.", "Pseudotsuga menziestii", 
                                         "Pinus banksiana", "Pinus mugo", "Chamaecyparis lawsoniana", 
                                         "Abies concolor", "Pinus contorta", "Abies procera", "Taxus baccata", 
                                         "Tsuga spp.", "Larix eurolepis", "Araucaria araucana", "Thuja spp.", 
                                         "abies …", "Metasequoia glyptostroboides", "Abies nordmanniana", 
                                         "Picea omorika", "Pinus rigida", "Abies amabilis", "Calocedrus decurrens", 
                                         "Sequoiadendron giganteum", "Cryptomeria japonica", "Picea pungens", 
                                         "Larix laricina", "Abies veitchii", "Picea glauca", "Pinus cembra", 
                                         "alii acus", "Juniperus foetidissima", "Picea orientalis", "Cedrus libani", 
                                         "Abies cilicica", "Juniperus communis", "Acer monspessulanum", 
                                         "Acer opalus", "Malus domestica", "Pyrus communis", "Alnus cordata ", 
                                         "Arbutus andrachne", "Arbutus unedo", "Fagus orientalis", "Phillyrea latifolia", 
                                         "Pistacia lentiscus", "Erica manipuliflora", "Juniperus excelsa", 
                                         "Juniperus oxycedrus", "Juniperus phoenicea", "Juniperus sabina", 
                                         "Juniperus thurifera", "Prunus domestica", "Buxus sempervirens", 
                                         "Carpinus orientalis", "Corylus avellana", "Eucalyptus spp.", 
                                         "Fagus moesiaca", "Fraxinus angustifolia spp. Oxycarpa", "Fraxinus ornus", 
                                         "Ilex aquifolium", "Olea europaea", "Ostrya carpinifolia", "Platanus orientalis", 
                                         "Prunus dulcis", "Quercus coccifera", "Quercus faginea", "Quercus frainetto", 
                                         "Quercus fruticosa", "Quercus ilex", "Quercus macrolepis", "Quercus pubescens", 
                                         "Quercus pyrenaica", "Quercus rotundifolia", "Quercus suber", 
                                         "Quercus trojana", "Salix cinerea", "Salix eleagnos", "Tamarix africana", 
                                         "Ceratonia siliqua", "Cercis siliquastrum", "Erica arborea", 
                                         "Erica scoparia", "Laurus nobilis", "Myrtus communis", "Phillyrea angustifolia", 
                                         "Pistacia terebinthus", "Rhamnus oleoides", "Rhamnus alaternus", 
                                         "Betula tortuosa", "Ilex canariensis", "Laurus canariensis", 
                                         "Myrica faya", "Quercus ?", "Abies borisii-regis", "Abies cephalonica", 
                                         "Abies pinsapo", "Cedrus atlantica", "Cedrus deodara", "Cupressus lusitanica", 
                                         "Cupressus sempervirens", "Pinus brutia", "Pinus canariensis", 
                                         "Pinus halepensis", "Pinus heldreichii", "Pinus leucodermis", 
                                         "Pinus pinaster", "Pinus pinea", "Pinus radiata", "Cedrus brevifolia", 
                                         "Quercus hartwissiana", "Quercus vulcanica", "Quercus infectoria", 
                                         "Quercus macranthera", "Quercus libani", "Quercus brantii", "Quercus ithaburensis", 
                                         "Quercus aucheri", "Quercus pontica", "Acer negundo", "Acer obtusatum", 
                                         "Acer saccharinum", "Acer tataricum", "Ailanthus altissima", 
                                         "Broussonetia papyrifera", "Carya ovata", "Corylus colurna", 
                                         "Crataegus laevigata", "Laburnum alpinum", "Laburnum anagyroides", 
                                         "Paulownia tomentosa", "Petteria ramentacea", "Phillyrea media", 
                                         "Pinus peuce", "Pinus wallichiana", "Platanus x acerifolia", 
                                         "Populus deltoides", "Prunus laurocerasus", "Prunus mahaleb", 
                                         "Prunus sppinosa", "Quercus pedunculiflora", "Salix purpurea", 
                                         "Salix triandra", "Salix viminalis", "Sorbus austriaca", "Tamarix ramosissima", 
                                         "Taxodium distichum", "Tilia tomentosa", "Tsuga canadensis", 
                                         "Ulmus laevis", "Ulmus minor"),
                             bot_genus = c("Acer", "Acer","Acer", "Alnus", "Betula", "Carpinus", "Castanea", "Fagus", "Fraxinus",
                                           "Prunus", "Populus", "Quercus", "Quercus", "Robinia", "Salix", 
                                           "Sorbus", "Tilia", "Ulmus", "Populus", "Sorbus", "Fraxinus", 
                                           "Acer", "Abies", "Abies", "Larix", "Larix", "Pinus", "Pinus", 
                                           "Picea", "Pinus", "-2", "-2", "-2", "Alnus", "Alnus", "Betula", 
                                           "Betula", "Quercus", "Quercus", "Tilia", "Tilia", "Populus", 
                                           "Ulmus", "Salix", "Ulmus", "Ulmus", "Populus", "Alnus", "Carya", 
                                           "Betula", "Betula", "Sorbus", "Mespilus", "Juglans", "Platanus", 
                                           "Aesculus", "Salix", "Juglans", "Populus", "Sorbus", "Pinus", 
                                           "Quercus", "Prunus", "Prunus", "Liriodendron", "Fraxinus", "Malus", 
                                           "Pyrus", "Prunus", "Quercus", "Populus", "alii", "Quercus", "Populus", 
                                           "Salix", "Crataegus", "Crataegus", "Populus", "alii", "Abies", 
                                           "Larix", "Picea", "Picea", "Picea", "Pinus", "Pseudotsuga", "Pinus", 
                                           "Pinus", "Chamaecyparis", "Abies", "Pinus", "Abies", "Taxus", 
                                           "Tsuga", "Larix", "Araucaria", "Thuja", "abies", "Metasequoia", 
                                           "Abies", "Picea", "Pinus", "Abies", "Calocedrus", "Sequoiadendron", 
                                           "Cryptomeria", "Picea", "Larix", "Abies", "Picea", "Pinus", "alii", 
                                           "Juniperus", "Picea", "Cedrus", "Abies", "Juniperus", "Acer", 
                                           "Acer", "Malus", "Pyrus", "Alnus", "Arbutus", "Arbutus", "Fagus", 
                                           "Phillyrea", "Pistacia", "Erica", "Juniperus", "Juniperus", "Juniperus", 
                                           "Juniperus", "Juniperus", "Prunus", "Buxus", "Carpinus", "Corylus", 
                                           "Eucalyptus", "Fagus", "Fraxinus", "Fraxinus", "Ilex", "Olea", 
                                           "Ostrya", "Platanus", "Prunus", "Quercus", "Quercus", "Quercus", 
                                           "Quercus", "Quercus", "Quercus", "Quercus", "Quercus", "Quercus", 
                                           "Quercus", "Quercus", "Salix", "Salix", "Tamarix", "Ceratonia", 
                                           "Cercis", "Erica", "Erica", "Laurus", "Myrtus", "Phillyrea", 
                                           "Pistacia", "Rhamnus", "Rhamnus", "Betula", "Ilex", "Laurus", 
                                           "Myrica", "Quercus", "Abies", "Abies", "Abies", "Cedrus", "Cedrus", 
                                           "Cupressus", "Cupressus", "Pinus", "Pinus", "Pinus", "Pinus", 
                                           "Pinus", "Pinus", "Pinus", "Pinus", "Cedrus", "Quercus", "Quercus", 
                                           "Quercus", "Quercus", "Quercus", "Quercus", "Quercus", "Quercus", 
                                           "Quercus", "Acer", "Acer", "Acer", "Acer", "Ailanthus", "Broussonetia", 
                                           "Carya", "Corylus", "Crataegus", "Laburnum", "Laburnum", "Paulownia", 
                                           "Petteria", "Phillyrea", "Pinus", "Pinus", "Platanus", "Populus", 
                                           "Prunus", "Prunus", "Prunus", "Quercus", "Salix", "Salix", "Salix", 
                                           "Sorbus", "Tamarix", "Taxodium", "Tilia", "Tsuga", "Ulmus", "Ulmus"), 
                             bot_species = c("spp.", "platanoides", "pseudoplatanus", "spp.", 
                                            "spp.", "betulus", "sativa", "sylvatica", "excelsior", "avium", 
                                            "spp.", "spp.", "rubra", "pseudoacacia", "spp.", "aucuparia", 
                                            "spp.", "spp.", "balsamifera", "torminalis", "excelsior", "campestre", 
                                            "alba", "grandis", "spp.", "decidua", "nigra", "sylvestris", 
                                            "abies", "strobus", "-2", "-2", "-2", "glutinosa", "incana", 
                                            "pendula", "pubescens", "petraea", "robur ", "cordata", "platyphyllos", 
                                            "tremula", "glabra", "fragilis", "campestris", "laevis (vorher effusa)", 
                                            "x canescens", "viridis", "spp.", "x hybrida", "platyphylla var. Japonica", 
                                            "aria", "germanica", "regia", "x hybrida", "hippocastanum", "caprea", 
                                            "nigra", "nigra", "domestica", "uncinata", "palustris", "padus", 
                                            "serotina", "tulipifera", "americana", "sylvestris", "pyraster", 
                                            "cerasifera", "cerris", "hybrides", "frons noble", "robur x petraea", 
                                            "alba", "alba", "monogyna", "spp.", "hybrides", "frons", "spp.", 
                                            "kaempferi ", "spp.", "abies ", "sitchensis", "spp.", "menziestii", 
                                            "banksiana", "mugo", "lawsoniana", "concolor", "contorta", "procera", 
                                            "baccata", "spp.", "eurolepis", "araucana", "spp.", "…", "glyptostroboides", 
                                            "nordmanniana", "omorika", "rigida", "amabilis", "decurrens", 
                                            "giganteum", "japonica", "pungens", "laricina", "veitchii", "glauca", 
                                            "cembra", "acus", "foetidissima", "orientalis", "libani", "cilicica", 
                                            "communis", "monspessulanum", "opalus", "domestica", "communis", 
                                            "cordata ", "andrachne", "unedo", "orientalis", "latifolia", 
                                            "lentiscus", "manipuliflora", "excelsa", "oxycedrus", "phoenicea", 
                                            "sabina", "thurifera", "domestica", "sempervirens", "orientalis", 
                                            "avellana", "spp.", "moesiaca", "angustifolia spp. Oxycarpa", 
                                            "ornus", "aquifolium", "europaea", "carpinifolia", "orientalis", 
                                            "dulcis", "coccifera", "faginea", "frainetto", "fruticosa", "ilex", 
                                            "macrolepis", "pubescens", "pyrenaica", "rotundifolia", "suber", 
                                            "trojana", "cinerea", "eleagnos", "africana", "siliqua", "siliquastrum", 
                                            "arborea", "scoparia", "nobilis", "communis", "angustifolia", 
                                            "terebinthus", "oleoides", "alaternus", "tortuosa", "canariensis", 
                                            "canariensis", "faya", "?", "borisii-regis", "cephalonica", "pinsapo", 
                                            "atlantica", "deodara", "lusitanica", "sempervirens", "brutia", 
                                            "canariensis", "halepensis", "heldreichii", "leucodermis", "pinaster", 
                                            "pinea", "radiata", "brevifolia", "hartwissiana", "vulcanica", 
                                            "infectoria", "macranthera", "libani", "brantii", "ithaburensis", 
                                            "aucheri", "pontica", "negundo", "obtusatum", "saccharinum", 
                                            "tataricum", "altissima", "papyrifera", "ovata", "colurna", "laevigata", 
                                            "alpinum", "anagyroides", "tomentosa", "ramentacea", "media", 
                                            "peuce", "wallichiana", "x acerifolia", "deltoides", "laurocerasus", 
                                            "mahaleb", "sppinosa", "pedunculiflora", "purpurea", "triandra", 
                                            "viminalis", "austriaca", "ramosissima", "distichum", "tomentosa", 
                                            "canadensis", "laevis", "minor"), 
                          Flora_EU = c("095.001.999.DL","095.001.001", "095.001.005", "034.002.999", "034.001.999.DL",
                                       "035.001.001", "036.003.001", "036.001.001", "139.004.003", "080.035.014", 
                                       "031.002.999.DL", "036.004.999.DL", "036.004.001", "081.030.001", 
                                       "031.001.999.DL", "080.028.002", "105.001.999.DL", "037.001.999.DL", 
                                       "", "080.028.003", "139.004.003", "095.001.003", "026.001.006", 
                                       "026.001.002", "026.005.999.DL", "026.005.002", "026.007.006", 
                                       "026.007.007", "026.004.001", "026.007.018", "", "", "", "034.002.002", 
                                       "034.002.004", "034.001.001", "034.001.002", "036.004.011", "036.004.014", 
                                       "105.001.005", "105.001.003", "031.002.004", "037.001.001", "031.001.002", 
                                       "", "", "031.002.002", "034.002.001", "", "", "", "080.028.005", 
                                       "080.033.001", "033.001.001", "", "097.001.001", "031.001.041", 
                                       "033.001.002", "031.022.008", "080.028.001", "026.007.009", "036.004.002", 
                                       "080.035.017", "080.035.018", "064.001.001", "", "080.027.003", 
                                       "080.026.004", "080.035.007", "036.004.008", "", "", "036.004.999.DL", 
                                       "031.022.001", "031.001.003", "080.034.014", "080.034.999.DL", 
                                       "", "999.999.001.DL", "026.001.999.DL", "026.005.001", "026.004.999.DL", 
                                       "026.004.001", "026.004.006", "026.007.999.DL", "026.002.001", 
                                       "026.007.004", "026.007.008", "028.002.001", "", "026.007.001", 
                                       "026.001.001", "029.001.001", "", "", "", "", "", "", "026.001.005", 
                                       "", "", "", "", "027.002.001", "027.004.001", "026.004.005", 
                                       "", "", "026.004.003", "026.007.014", "999.999.002.DL", "028.005.008", 
                                       "026.004.002", "", "", "028.005.002", "095.001.013", "095.001.008", 
                                       "080.027.006", "080.026.013", "034.002.005", "132.012.002", "132.012.001", 
                                       "", "139.008.002", "094.003.004", "132.001.012", "028.005.009", 
                                       "028.005.003", "028.005.006", "028.005.010", "028.005.007", "?", 
                                       "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", 
                                       "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", 
                                       "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", 
                                       "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", 
                                       "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", 
                                       "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", 
                                       "", ""), 
                             LH_NH = c("LB", "LB", "LB", "LB", "LB", "LB", "LB",
                                       "LB", "LB", "LB", "LB", "LB", "LB", "LB", "LB", "LB", "LB", "LB", 
                                        "LB", "LB", "LB", "LB", "NB", "NB", "NB", "NB", "NB", "NB", "NB", 
                                        "NB", "", "", "", "LB", "LB", "LB", "LB", "LB", "LB", "LB", "LB", 
                                        "LB", "LB", "LB", "LB", "LB", "LB", "LB", "LB", "LB", "LB", "LB", 
                                        "LB", "LB", "LB", "LB", "LB", "LB", "LB", "LB", "NB", "LB", "LB", 
                                        "LB", "LB", "LB", "LB", "LB", "LB", "LB", "LB", "LB", "LB", "LB", 
                                        "LB", "LB", "LB", "LB", "LB", "NB", "NB", "NB", "NB", "NB", "NB", 
                                        "NB", "NB", "NB", "NB", "NB", "NB", "NB", "NB", "NB", "NB", "NB", 
                                        "NB", "NB", "NB", "NB", "NB", "NB", "NB", "NB", "NB", "NB", "NB", 
                                        "NB", "NB", "NB", "NB", "NB", "NB", "NB", "NB", "NB", "NB", "LB", 
                                        "LB", "LB", "LB", "LB", "LB", "LB", "LB", "LB", "LB", "LB", "NB", 
                                        "NB", "NB", "NB", "NB", "LB", "LB", "LB", "LB", "LB", "LB", "LB", 
                                        "LB", "LB", "LB", "LB", "LB", "LB", "LB", "LB", "LB", "LB", "LB", 
                                        "LB", "LB", "LB", "LB", "LB", "LB", "LB", "LB", "LB", "LB", "LB", 
                                        "LB", "LB", "LB", "LB", "LB", "LB", "LB", "LB", "LB", "LB", "LB", 
                                        "LB", "LB", "NB", "NB", "NB", "NB", "NB", "NB", "NB", "NB", "NB", 
                                        "NB", "NB", "NB", "NB", "NB", "NB", "NB", "LB", "LB", "LB", "LB", 
                                        "LB", "LB", "LB", "LB", "LB", "LB", "LB", "LB", "LB", "LB", "LB", 
                                        "LB", "LB", "LB", "LB", "LB", "LB", "LB", "LB", "NB", "NB", "LB", 
                                        "LB", "LB", "LB", "LB", "LB", "LB", "LB", "LB", "LB", "LB", "LB", 
                                        "LB", "NB", "LB", "LB"), 
                          IPC = c(215L, 4L, 227L, 216L, 212L, 13L, 15L, 20L, 22L, 36L, 211L, 99L, 53L, 56L, 62L, 64L, 210L, 
                                 213L, 230L, 66L, 22L, 1L, 100L, 103L, 197L, 116L, 129L, 134L, 
                                 118L, 133L, 0L, NA, 0L, 7L, 8L, 10L, 11L, 48L, 51L, 68L, 69L, 
                                 35L, 70L, 61L, 72L, 71L, 32L, 9L, 99L, 214L, 99L, 63L, 248L, 
                                 26L, 99L, 221L, 58L, 25L, 34L, 65L, 135L, 237L, 38L, 39L, 247L, 
                                 226L, 220L, 253L, 232L, 41L, 33L, 99L, 98L, 31L, 57L, 90L, 217L, 
                                 33L, 99L, 198L, 117L, 199L, 118L, 120L, 199L, 136L, 153L, 128L, 
                                 140L, 199L, 124L, 106L, 137L, 139L, 150L, 157L, 138L, 199L, 160L, 
                                 104L, 119L, 155L, 147L, 158L, 161L, 159L, 152L, 199L, 149L, 151L, 
                                 123L, 199L, 145L, 146L, 143L, 142L, 111L, 2L, 3L, 27L, 40L, 6L, 
                                 74L, 73L, 19L, 82L, 84L, 79L, 144L, 112L, 113L, 114L, 115L, 233L, 
                                 12L, 14L, 16L, 17L, 18L, 21L, 23L, 24L, 28L, 29L, 30L, 37L, 42L, 
                                 43L, 44L, 45L, 46L, 47L, 49L, 50L, 52L, 54L, 55L, 59L, 60L, 67L, 
                                 75L, 76L, 77L, 78L, 80L, 81L, 83L, 85L, 86L, 87L, 88L, 91L, 92L, 
                                 93L, 94L, 101L, 102L, 105L, 107L, 108L, 109L, 110L, 121L, 122L, 
                                 125L, 126L, 127L, 130L, 131L, 132L, 141L, 201L, 202L, 203L, 204L, 
                                 205L, 206L, 207L, 208L, 209L, 222L, 223L, 224L, 225L, 242L, 243L, 
                                 244L, 245L, 246L, 228L, 229L, 249L, 250L, 251L, 154L, 156L, 252L, 
                                 231L, 234L, 235L, 236L, 238L, 239L, 240L, 241L, 254L, 162L, 163L, 
                                 255L, 164L, 71L, 72L), 
                          WZE = c(30011L, 30011L, 30011L, 30011L,30011L, 30011L, 30011L, 30016L, 30011L, 30011L, 30011L, 30013L, 
                                  30011L, 30011L, 30011L, 30011L, 30011L, 30011L, 30011L, 30011L, 
                                  30011L, 30011L, 30012L, 30012L, 30012L, 30012L, 30012L, 30015L, 
                                  30014L, 30012L, NA, NA, NA, 30011L, 30011L, 30011L, 30011L, 30013L, 
                                  30013L, 30011L, 30011L, 30011L, 30011L, 30011L, 30011L, 30011L, 
                                  30011L, 30011L, 30011L, 30011L, 30011L, 30011L, 30011L, 30011L, 
                                  30011L, 30011L, 30011L, 30011L, 30011L, 30011L, 30012L, 30011L, 
                                  30011L, 30011L, 30011L, 30011L, 30011L, 30011L, 30011L, 30011L, 
                                  30011L, 30011L, 30013L, 30011L, 30011L, 30011L, 30011L, 30011L, 
                                  30011L, 30012L, 30012L, 30012L, 30014L, 30012L, 30015L, 30012L, 
                                  30012L, 30012L, 30012L, 30012L, 30012L, 30012L, 30012L, 30012L, 
                                  30012L, 30012L, 30012L, 30012L, 30012L, 30012L, 30012L, 30012L, 
                                  30012L, 30012L, 30012L, 30012L, 30012L, 30012L, 30012L, 30012L, 
                                  30012L, 30012L, 30012L, 30012L, 30012L, 30012L, 30012L, 30011L, 
                                  30011L, 30011L, 30011L, 30011L, 30011L, 30011L, 30011L, 30011L, 
                                  30011L, 30011L, 30012L, 30012L, 30012L, 30012L, 30012L, 30011L, 
                                  30011L, 30011L, 30011L, 30011L, 30011L, 30011L, 30011L, 30011L, 
                                  30011L, 30011L, 30011L, 30011L, 30011L, 30011L, 30011L, 30011L, 
                                  30011L, 30011L, 30011L, 30011L, 30011L, 30011L, 30011L, 30011L, 
                                  30011L, 30011L, 30011L, 30011L, 30011L, 30011L, 30011L, 30011L, 
                                  30011L, 30011L, 30011L, 30011L, 30011L, 30011L, 30011L, 30011L, 
                                  30011L, 30012L, 30012L, 30012L, 30012L, 30012L, 30012L, 30012L, 
                                  30012L, 30012L, 30012L, 30012L, 30012L, 30012L, 30012L, 30012L, 
                                  30012L, 30011L, 30011L, 30011L, 30011L, 30011L, 30011L, 30011L, 
                                  30011L, 30011L, 30011L, 30011L, 30011L, 30011L, 30011L, 30011L, 
                                  30011L, 30011L, 30011L, 30011L, 30011L, 30011L, 30011L, 30011L, 
                                  30012L, 30012L, 30011L, 30011L, 30011L, 30011L, 30011L, 30011L, 
                                  30011L, 30011L, 30011L, 30011L, 30011L, 30011L, 30011L, 30012L, 
                                  30011L, 30011L), 
                          BWI = c("AH", "SAH", "BAH", "ERL", "BI", "HBU", 
                                  "KA", "BU", "ES", "KIR", "PA", "EI", "REI", "ROB", "SLB", "VB", 
                                  "LI", "UL", "PA", "VB", "ES", "FAH", "TA", "TA", "LAE", "ELA", 
                                  "KI", "KI", "FI", "WEY", "", "", "", "ERL", "ERL", "BI", "BI", 
                                  "EI", "EI", "LI", "LI", "PA", "UL", "SLB", "UL", "UL", "PA", 
                                  "ERL", "SLB", "BI", "BI", "VB", "SLB", "SLB", "SLB", "KA", "SLB", 
                                  "SLB", "PA", "VB", "KI", "EI", "KIR", "KIR", "SLB", "ES", "SLB", 
                                  "SLB", "KIR", "EI", "PA", "SLB", "EI", "PA", "SLB", "SLB", "SLB", 
                                  "PA", "SLB", "TA", "JLA", "FI", "FI", "FI", "KI", "DGL", "KI", 
                                  "KI", "SNB", "TA", "KI", "TA", "EIB", "SNB", "LAE", "SNB", "SNB", 
                                  "TA", "SNB", "TA", "FI", "KI", "TA", "SNB", "SNB", "SNB", "FI", 
                                  "LAE", "TA", "FI", "KI", "SNB", "SNB", "FI", "SNB", "TA", "SNB", 
                                  "AH", "AH", "SLB", "SLB", "ERL", "SLB", "SLB", "BU", "SLB", "SLB", 
                                  "SLB", "SNB", "SNB", "SNB", "SNB", "SNB", "KIR", "SLB", "HBU", 
                                  "SLB", "SLB", "BU", "ES", "ES", "SLB", "SLB", "SLB", "SLB", "KIR", 
                                  "EI", "EI", "EI", "EI", "EI", "EI", "EI", "EI", "EI", "EI", "EI", 
                                  "SLB", "SLB", "SLB", "SLB", "SLB", "SLB", "SLB", "SLB", "SLB", 
                                  "SLB", "SLB", "SLB", "SLB", "BI", "SLB", "SLB", "SLB", "EI", 
                                  "TA", "TA", "TA", "SNB", "SNB", "SNB", "SNB", "KI", "KI", "KI", 
                                  "KI", "KI", "KI", "KI", "KI", "SNB", "EI", "EI", "EI", "EI", 
                                  "EI", "EI", "EI", "EI", "EI", "AH", "AH", "AH", "AH", "SLB", 
                                  "SLB", "SLB", "SLB", "SLB", "SLB", "SLB", "SLB", "SLB", "SLB", 
                                  "KI", "KI", "SLB", "PA", "KIR", "KIR", "KIR", "EI", "SLB", "SLB", 
                                  "SLB", "VB", "SLB", "SLB", "LI", "SNB", "UL", "UL"), 
                          BZE_al = c("WAHR", "WAHR", "WAHR", "WAHR", "WAHR", "WAHR", "FALSCH", "WAHR", "WAHR", 
                                     "WAHR", "WAHR", "FALSCH", "WAHR", "WAHR", "WAHR", "WAHR", "WAHR", 
                                     "WAHR", "FALSCH", "WAHR", "FALSCH", "WAHR", "WAHR", "WAHR", "WAHR", 
                                     "WAHR", "WAHR", "WAHR", "FALSCH", "WAHR", "FALSCH", "FALSCH", 
                                     "FALSCH", "WAHR", "WAHR", "WAHR", "WAHR", "WAHR", "WAHR", "WAHR", 
                                     "WAHR", "WAHR", "WAHR", "FALSCH", "FALSCH", "FALSCH", "FALSCH", 
                                     "FALSCH", "FALSCH", "FALSCH", "WAHR", "WAHR", "FALSCH", "WAHR", 
                                     "FALSCH", "WAHR", "WAHR", "FALSCH", "WAHR", "WAHR", "FALSCH", 
                                     "WAHR", "WAHR", "WAHR", "WAHR", "FALSCH", "WAHR", "WAHR", "FALSCH", 
                                     "WAHR", "FALSCH", "FALSCH", "WAHR", "FALSCH", "FALSCH", "FALSCH", 
                                     "FALSCH", "WAHR", "WAHR", "WAHR", "WAHR", "FALSCH", "WAHR", "WAHR", 
                                     "FALSCH", "WAHR", "WAHR", "WAHR", "FALSCH", "FALSCH", "FALSCH", 
                                     "WAHR", "WAHR", "WAHR", "FALSCH", "FALSCH", "WAHR", "FALSCH", 
                                     "FALSCH", "WAHR", "FALSCH", "FALSCH", "FALSCH", "FALSCH", "FALSCH", 
                                     "FALSCH", "FALSCH", "FALSCH", "FALSCH", "FALSCH", "WAHR", "WAHR", 
                                     "FALSCH", "FALSCH", "FALSCH", "FALSCH", "FALSCH", "WAHR", "WAHR", 
                                     "FALSCH", "WAHR", "FALSCH", "FALSCH", "FALSCH", "FALSCH", "FALSCH", 
                                     "FALSCH", "FALSCH", "FALSCH", "FALSCH", "FALSCH", "FALSCH", "FALSCH", 
                                     "FALSCH", "FALSCH", "WAHR", "FALSCH", "FALSCH", "FALSCH", "FALSCH", 
                                     "FALSCH", "FALSCH", "FALSCH", "FALSCH", "FALSCH", "FALSCH", "FALSCH", 
                                     "FALSCH", "FALSCH", "FALSCH", "FALSCH", "FALSCH", "FALSCH", "FALSCH", 
                                     "FALSCH", "FALSCH", "FALSCH", "FALSCH", "FALSCH", "FALSCH", "FALSCH", 
                                     "FALSCH", "FALSCH", "FALSCH", "FALSCH", "FALSCH", "FALSCH", "FALSCH", 
                                     "FALSCH", "FALSCH", "FALSCH", "FALSCH", "FALSCH", "FALSCH", "FALSCH", 
                                     "FALSCH", "FALSCH", "FALSCH", "FALSCH", "FALSCH", "FALSCH", "FALSCH", 
                                     "FALSCH", "FALSCH", "FALSCH", "FALSCH", "FALSCH", "FALSCH", "FALSCH", 
                                     "FALSCH", "FALSCH", "FALSCH", "FALSCH", "FALSCH", "FALSCH", "FALSCH", 
                                     "FALSCH", "FALSCH", "FALSCH", "FALSCH", "FALSCH", "FALSCH", "FALSCH", 
                                     "FALSCH", "FALSCH", "FALSCH", "FALSCH", "FALSCH", "FALSCH", "FALSCH", 
                                     "FALSCH", "FALSCH", "FALSCH", "FALSCH", "FALSCH", "FALSCH", "FALSCH", 
                                     "FALSCH", "FALSCH", "FALSCH", "FALSCH", "FALSCH", "FALSCH", "FALSCH", 
                                     "FALSCH", "FALSCH", "FALSCH", "FALSCH", "FALSCH", "FALSCH", "WAHR", 
                                     "FALSCH"), 
                          tpS_SP_com_name = c("Acer spp.", "Acer platanoides", 
                                    "Acer pseudoplatanus", "Alnus spp.", "Betula spp.", "Carpinus betulus", 
                                    "Castanea sativa", "Fagus sylvatica", "Fraxinus excelsior", "Prunus avium", 
                                    "Populus spp.", "Quercus spp.", "Quercus rubra", "Robinia pseudoacacia", 
                                    "Salix spp.", "Sorbus aucuparia", "Tilia spp.", "Ulmus spp.", 
                                    "Populus balsamifera", "Sorbus torminalis", "Fraxinus excelsior", 
                                    "Acer campestre", "Abies alba", "Abies grandis", "Larix spp.", 
                                    "Larix decidua", "Pinus nigra", "Pinus sylvestris", "Picea abies", 
                                    "Pinus strobus", "missing", "missing", "missing", "Alnus spp.", 
                                    "Alnus spp.", "Betula spp.", "Betula spp.", "Quercus spp.", "Quercus spp.", 
                                    "Tilia spp.", "Tilia spp.", "Populus spp.", "Ulmus spp.", "Salix spp.", 
                                    "Ulmus spp.", "Ulmus spp.", "Populus spp.", "Alnus spp.", "Magnoliopsida trees", 
                                    "Betula spp.", "Betula spp.", "Sorbus aucuparia", "Magnoliopsida trees", 
                                    "Magnoliopsida trees", "Magnoliopsida trees", "Magnoliopsida trees", 
                                    "Salix spp.", "Magnoliopsida trees", "Populus spp.", "Sorbus aucuparia", 
                                    "Pinus sylvestris", "Quercus spp.", "Prunus avium", "Prunus avium", 
                                    "Magnoliopsida trees", "Fraxinus excelsior", "Magnoliopsida trees", 
                                    "Magnoliopsida trees", "Prunus avium", "Quercus spp.", "Populus spp.", 
                                    "Magnoliopsida trees", "Quercus spp.", "Populus spp.", "Salix spp.", 
                                    "Magnoliopsida trees", "Magnoliopsida trees", "Populus spp.", 
                                    "Magnoliopsida trees", "Abies alba", "Larix spp.", "Picea abies", 
                                    "Picea abies", "Picea abies", "Pinus sylvestris", "Pseudotsuga menziesii", 
                                    "Pinus sylvestris", "Pinus sylvestris", "Coniferales trees", 
                                    "Abies alba", "Pinus sylvestris", "Abies alba", "Coniferales trees", 
                                    "Tsuga heterophylla", "Larix spp.", "Coniferales trees", "Thuja plicata", 
                                    "Coniferales trees", "Coniferales trees", "Abies alba", "Picea abies", 
                                    "Pinus sylvestris", "Abies alba", "Coniferales trees", "Coniferales trees", 
                                    "Coniferales trees", "Picea abies", "Larix spp.", "Abies alba", 
                                    "Picea abies", "Pinus sylvestris", "Coniferales trees", "Coniferales trees", 
                                    "Picea abies", "Coniferales trees", "Abies alba", "Coniferales trees", 
                                    "Acer spp.", "Acer spp.", "Magnoliopsida trees", "Magnoliopsida trees", 
                                    "Alnus spp.", "Magnoliopsida trees", "Magnoliopsida trees", "Fagus sylvatica", 
                                    "Magnoliopsida trees", "Magnoliopsida trees", "Magnoliopsida trees", 
                                    "Coniferales trees", "Coniferales trees", "Coniferales trees", 
                                    "Coniferales trees", "Coniferales trees", "Prunus avium", "Magnoliopsida trees", 
                                    "Carpinus betulus", "Magnoliopsida trees", "Magnoliopsida trees", 
                                    "Fagus sylvatica", "Fraxinus excelsior", "Fraxinus excelsior", 
                                    "Magnoliopsida trees", "Magnoliopsida trees", "Magnoliopsida trees", 
                                    "Magnoliopsida trees", "Prunus avium", "Quercus spp.", "Quercus spp.", 
                                    "Quercus spp.", "Quercus spp.", "Quercus spp.", "Quercus spp.", 
                                    "Quercus spp.", "Quercus spp.", "Quercus spp.", "Quercus spp.", 
                                    "Quercus spp.", "Salix spp.", "Salix spp.", "Magnoliopsida trees", 
                                    "Magnoliopsida trees", "Magnoliopsida trees", "Magnoliopsida trees", 
                                    "Magnoliopsida trees", "Magnoliopsida trees", "Magnoliopsida trees", 
                                    "Magnoliopsida trees", "Magnoliopsida trees", "Magnoliopsida trees", 
                                    "Magnoliopsida trees", "Betula spp.", "Magnoliopsida trees", 
                                    "Magnoliopsida trees", "Magnoliopsida trees", "Quercus spp.", 
                                    "Abies alba", "Abies alba", "Abies alba", "Coniferales trees", 
                                    "Coniferales trees", "Coniferales trees", "Coniferales trees", 
                                    "Pinus sylvestris", "Pinus sylvestris", "Pinus sylvestris", "Pinus sylvestris", 
                                    "Pinus sylvestris", "Pinus sylvestris", "Pinus sylvestris", "Pinus sylvestris", 
                                    "Coniferales trees", "Quercus spp.", "Quercus spp.", "Quercus spp.", 
                                    "Quercus spp.", "Quercus spp.", "Quercus spp.", "Quercus spp.", 
                                    "Quercus spp.", "Quercus spp.", "Acer spp.", "Acer spp.", "Acer spp.", 
                                    "Acer spp.", "Magnoliopsida trees", "Magnoliopsida trees", "Magnoliopsida trees", 
                                    "Magnoliopsida trees", "Magnoliopsida trees", "Magnoliopsida trees", 
                                    "Magnoliopsida trees", "Magnoliopsida trees", "Magnoliopsida trees", 
                                    "Magnoliopsida trees", "Pinus sylvestris", "Pinus sylvestris", 
                                    "Magnoliopsida trees", "Populus spp.", "Prunus avium", "Prunus avium", 
                                    "Prunus avium", "Quercus spp.", "Salix spp.", "Salix spp.", "Salix spp.", 
                                    "Sorbus aucuparia", "Magnoliopsida trees", "Magnoliopsida trees", 
                                    "Tilia spp.", "Tsuga heterophylla", "Ulmus spp.", "Ulmus spp."), 
                          tpS_ID = c(22L, 24L, 23L, 28L, 26L, 16L, 33L, 15L, 21L, 29L, 
                                     19L, 17L, 18L, 31L, 34L, 36L, 27L, 30L, 20L, 32L, 21L, 25L, 3L, 
                                     4L, 9L, 10L, 6L, 5L, 1L, 7L, NA, NA, NA, 28L, 28L, 26L, 26L, 
                                     17L, 17L, 27L, 27L, 19L, 30L, 34L, 30L, 30L, 19L, 28L, 35L, 26L, 
                                     26L, 36L, 35L, 35L, 35L, 35L, 34L, 35L, 19L, 36L, 5L, 17L, 29L, 
                                     29L, 35L, 21L, 35L, 35L, 29L, 17L, 19L, 35L, 17L, 19L, 34L, 35L, 
                                     35L, 19L, 35L, 3L, 9L, 1L, 1L, 1L, 5L, 8L, 5L, 5L, 14L, 3L, 5L, 
                                     3L, 14L, 13L, 9L, 14L, 12L, 14L, 14L, 3L, 1L, 5L, 3L, 14L, 14L, 
                                     14L, 1L, 9L, 3L, 1L, 5L, 14L, 14L, 1L, 14L, 3L, 14L, 22L, 22L, 
                                     35L, 35L, 28L, 35L, 35L, 15L, 35L, 35L, 35L, 14L, 14L, 14L, 14L, 
                                     14L, 29L, 35L, 16L, 35L, 35L, 15L, 21L, 21L, 35L, 35L, 35L, 35L, 
                                     29L, 17L, 17L, 17L, 17L, 17L, 17L, 17L, 17L, 17L, 17L, 17L, 34L, 
                                     34L, 35L, 35L, 35L, 35L, 35L, 35L, 35L, 35L, 35L, 35L, 35L, 26L, 
                                     35L, 35L, 35L, 17L, 3L, 3L, 3L, 14L, 14L, 14L, 14L, 5L, 5L, 5L, 
                                     5L, 5L, 5L, 5L, 5L, 14L, 17L, 17L, 17L, 17L, 17L, 17L, 17L, 17L, 
                                     17L, 22L, 22L, 22L, 22L, 35L, 35L, 35L, 35L, 35L, 35L, 35L, 35L, 
                                     35L, 35L, 5L, 5L, 35L, 19L, 29L, 29L, 29L, 17L, 34L, 34L, 34L, 
                                     36L, 35L, 35L, 27L, 13L, 30L, 30L), 
                          H_SP_group = c("bu", "bu", 
                                          "bu", "bu", "bu", "bu", "bu", "bu", "bu", "bu", "bu", "ei", "ei", 
                                          "bu", "bu", "bu", "bu", "bu", "bu", "bu", "bu", "bu", "ta", "ta", 
                                          "lae", "lae", "ki", "ki", "fi", "ki", "fi", "fi", "fi", "bu", 
                                          "bu", "bu", "bu", "ei", "ei", "bu", "bu", "bu", "bu", "bu", "bu", 
                                          "bu", "bu", "bu", "bu", "bu", "bu", "bu", "bu", "bu", "bu", "bu", 
                                          "bu", "bu", "bu", "bu", "ki", "ei", "bu", "bu", "bu", "bu", "bu", 
                                          "bu", "bu", "ei", "bu", "bu", "ei", "bu", "bu", "bu", "bu", "bu", 
                                          "bu", "ta", "lae", "fi", "fi", "fi", "ki", "dgl", "ki", "ki", 
                                          "fi", "ta", "ki", "ta", "fi", "fi", "lae", "fi", "fi", "fi", 
                                          "fi", "ta", "fi", "ki", "ta", "fi", "fi", "fi", "fi", "lae", 
                                          "ta", "fi", "ki", "fi", "fi", "fi", "fi", "ta", "fi", "bu", "bu", 
                                          "bu", "bu", "bu", "bu", "bu", "bu", "bu", "bu", "bu", "fi", "fi", 
                                          "fi", "fi", "fi", "bu", "bu", "bu", "bu", "bu", "bu", "bu", "bu", 
                                          "bu", "bu", "bu", "bu", "bu", "ei", "ei", "ei", "ei", "ei", "ei", 
                                          "ei", "ei", "ei", "ei", "ei", "bu", "bu", "bu", "bu", "bu", "bu", 
                                          "bu", "bu", "bu", "bu", "bu", "bu", "bu", "bu", "bu", "bu", "bu", 
                                          "ei", "ta", "ta", "ta", "fi", "fi", "fi", "fi", "ki", "ki", "ki", 
                                          "ki", "ki", "ki", "ki", "ki", "fi", "ei", "ei", "ei", "ei", "ei", 
                                          "ei", "ei", "ei", "ei", "bu", "bu", "bu", "bu", "bu", "bu", "bu", 
                                          "bu", "bu", "bu", "bu", "bu", "bu", "bu", "ki", "ki", "bu", "bu", 
                                          "bu", "bu", "bu", "ei", "bu", "bu", "bu", "bu", "bu", "bu", "bu", 
                                          "fi", "bu", "bu"), 
                          BWI_SP_group = c("aLh", "aLh", "aLh", "aLn", 
                                           "aLn", "aLh", "aLh", "bu", "aLh", "aLn", "aLn", "ei", "ei", "aLh", 
                                          "aLn", "aLh", "aLh", "aLh", "aLn", "aLh", "aLh", "aLh", "fi", 
                                          "fi", "ki", "ki", "ki", "ki", "fi", "ki", "other", "other", "other", 
                                          "aLn", "aLn", "aLn", "aLn", "ei", "ei", "aLh", "aLh", "aLn", 
                                          "aLh", "aLn", "aLh", "aLh", "aLn", "aLn", "aLn", "aLn", "aLn", 
                                          "aLh", "aLn", "aLh", "aLh", "aLh", "aLn", "aLh", "aLn", "aLh", 
                                          "ki", "ei", "aLn", "aLn", "aLn", "aLh", "aLn", "aLn", "aLn", 
                                          "ei", "aLn", "aLn", "ei", "aLn", "aLn", "aLn", "aLn", "aLn", 
                                          "aLn", "fi", "ki", "fi", "fi", "fi", "ki", "fi", "ki", "ki", 
                                          "fi", "fi", "ki", "fi", "fi", "fi", "ki", "fi", "fi", "fi", "fi", 
                                          "fi", "fi", "ki", "fi", "fi", "fi", "fi", "fi", "ki", "fi", "fi", 
                                          "ki", "fi", "fi", "fi", "fi", "fi", "fi", "aLh", "aLh", "aLn", 
                                          "aLn", "aLn", "aLn", "aLn", "bu", "aLn", "aLn", "aLn", "fi", 
                                          "fi", "fi", "fi", "fi", "aLn", "aLn", "aLh", "aLh", "aLn", "bu", 
                                          "aLh", "aLh", "aLn", "aLn", "aLn", "aLh", "aLh", "ei", "ei", 
                                          "ei", "ei", "ei", "ei", "ei", "ei", "ei", "ei", "ei", "aLn", 
                                          "aLn", "aLn", "aLn", "aLn", "aLn", "aLn", "aLn", "aLn", "aLn", 
                                          "aLn", "aLh", "aLh", "aLn", "aLn", "aLn", "aLn", "ei", "fi", 
                                          "fi", "fi", "fi", "fi", "fi", "fi", "ki", "ki", "ki", "ki", "ki", 
                                          "ki", "ki", "ki", "fi", "ei", "ei", "ei", "ei", "ei", "ei", "ei", 
                                          "ei", "ei", "aLh", "aLh", "aLh", "aLh", "aLn", "aLn", "aLn", 
                                          "aLh", "aLn", "aLn", "aLn", "aLn", "aLn", "aLn", "ki", "ki", 
                                          "aLh", "aLn", "aLn", "aLn", "aLn", "ei", "aLn", "aLn", "aLn", 
                                          "aLh", "aLn", "aLn", "aLh", "fi", "aLh", "aLh"), 
                      Bio_SP_group = c("bu","bu", "bu", "shw", "shw", "bu", "bu", "bu", "bu", "shw", "shw", 
                                        "ei", "ei", "bu", "shw", "bu", "bu", "bu", "shw", "bu", "bu", 
                                        "bu", "fi", "fi", "ki", "ki", "ki", "ki", "fi", "ki", "other", 
                                        "other", "other", "shw", "shw", "shw", "shw", "ei", "ei", "bu", 
                                        "bu", "shw", "bu", "shw", "bu", "bu", "shw", "shw", "shw", "shw", 
                                        "shw", "bu", "shw", "bu", "bu", "bu", "shw", "bu", "shw", "bu", 
                                        "ki", "ei", "shw", "shw", "shw", "bu", "shw", "shw", "shw", "ei", 
                                        "shw", "shw", "ei", "shw", "shw", "shw", "shw", "shw", "shw", 
                                        "fi", "ki", "fi", "fi", "fi", "ki", "fi", "ki", "ki", "fi", "fi", 
                                        "ki", "fi", "fi", "fi", "ki", "fi", "fi", "fi", "fi", "fi", "fi", 
                                        "ki", "fi", "fi", "fi", "fi", "fi", "ki", "fi", "fi", "ki", "fi", 
                                        "fi", "fi", "fi", "fi", "fi", "bu", "bu", "shw", "shw", "shw", 
                                        "shw", "shw", "bu", "shw", "shw", "shw", "fi", "fi", "fi", "fi", 
                                        "fi", "shw", "shw", "bu", "bu", "shw", "bu", "bu", "bu", "shw", 
                                        "shw", "shw", "bu", "bu", "ei", "ei", "ei", "ei", "ei", "ei", 
                                        "ei", "ei", "ei", "ei", "ei", "shw", "shw", "shw", "shw", "shw", 
                                        "shw", "shw", "shw", "shw", "shw", "shw", "bu", "bu", "shw", 
                                        "shw", "shw", "shw", "ei", "fi", "fi", "fi", "fi", "fi", "fi", 
                                        "fi", "ki", "ki", "ki", "ki", "ki", "ki", "ki", "ki", "fi", "ei", 
                                        "ei", "ei", "ei", "ei", "ei", "ei", "ei", "ei", "bu", "bu", "bu", 
                                        "bu", "shw", "shw", "shw", "bu", "shw", "shw", "shw", "shw", 
                                        "shw", "shw", "ki", "ki", "bu", "shw", "shw", "shw", "shw", "ei", 
                                        "shw", "shw", "shw", "bu", "shw", "shw", "bu", "fi", "bu", "bu"), 
                      N_SP_group = c("AH", "AH", "AH", "ERL", "BI", "BU", "BU",
                                     "BU", "ES", "BI", "BI", "EI", "EI", "BU", "BI", "BU", "BU", "BU", 
                                      "BI", "BU", "ES", "AH", "FI", "FI", "KI", "KI", "KI", "KI", "FI", 
                                      "KI", "other", "other", "other", "ERL", "ERL", "BI", "BI", "EI", 
                                      "EI", "BU", "BU", "BI", "BU", "BI", "BU", "BU", "BI", "ERL", 
                                      "BI", "BI", "BI", "BU", "BI", "BU", "BU", "BU", "BI", "BU", "BI", 
                                      "BU", "KI", "EI", "BI", "BI", "BI", "ES", "BI", "BI", "BI", "EI", 
                                      "BI", "BI", "EI", "BI", "BI", "BI", "BI", "BI", "BI", "FI", "KI", 
                                      "FI", "FI", "FI", "KI", "FI", "KI", "KI", "FI", "FI", "KI", "FI", 
                                      "FI", "FI", "KI", "FI", "FI", "FI", "FI", "FI", "FI", "KI", "FI", 
                                      "FI", "FI", "FI", "FI", "KI", "FI", "FI", "KI", "FI", "FI", "FI", 
                                      "FI", "FI", "FI", "AH", "AH", "BI", "BI", "ERL", "BI", "BI", 
                                      "BU", "BI", "BI", "BI", "FI", "FI", "FI", "FI", "FI", "BI", "BI", 
                                      "BU", "BU", "BI", "BU", "ES", "ES", "BI", "BI", "BI", "BU", "BU", 
                                      "EI", "EI", "EI", "EI", "EI", "EI", "EI", "EI", "EI", "EI", "EI", 
                                      "BI", "BI", "BI", "BI", "BI", "BI", "BI", "BI", "BI", "BI", "BI", 
                                      "BU", "BU", "BI", "BI", "BI", "BI", "EI", "FI", "FI", "FI", "FI", 
                                      "FI", "FI", "FI", "KI", "KI", "KI", "KI", "KI", "KI", "KI", "KI", 
                                      "FI", "EI", "EI", "EI", "EI", "EI", "EI", "EI", "EI", "EI", "AH", 
                                      "AH", "AH", "AH", "BI", "BI", "BI", "BU", "BI", "BI", "BI", "BI", 
                                      "BI", "BI", "KI", "KI", "BU", "BI", "BI", "BI", "BI", "EI", "BI", 
                                      "BI", "BI", "BU", "BI", "BI", "BU", "FI", "BU", "BU"), 
                      N_bg_SP_group = c("BU", "BU", "BU", "BI", "BI", "BU", "BU", "BU", "BU", "BI", "BI", "EI", 
                                       "EI", "BU", "BI", "BU", "BU", "BU", "BI", "BU", "BU", "BU", "FI", 
                                       "FI", "LA", "LA", "KIN", "KI", "FI", "KI", "other", "other", 
                                       "other", "BI", "BI", "BI", "BI", "EI", "EI", "BU", "BU", "BI", 
                                       "BU", "BI", "BU", "BU", "BI", "BI", "BI", "BI", "BI", "BU", "BI", 
                                       "BU", "BU", "BU", "BI", "BU", "BI", "BU", "KI", "EI", "BI", "BI", 
                                       "BI", "BU", "BI", "BI", "BI", "EI", "BI", "BI", "EI", "BI", "BI", 
                                       "BI", "BI", "BI", "BI", "FI", "LA", "FI", "FI", "FI", "KI", "FI", 
                                       "KI", "KI", "FI", "FI", "KI", "FI", "FI", "FI", "LA", "FI", "FI", 
                                       "FI", "FI", "FI", "FI", "KI", "FI", "FI", "FI", "FI", "FI", "LA", 
                                       "FI", "FI", "KI", "FI", "FI", "FI", "FI", "FI", "FI", "BU", "BU", 
                                       "BI", "BI", "BI", "BI", "BI", "BU", "BI", "BI", "BI", "FI", "FI", 
                                       "FI", "FI", "FI", "BI", "BI", "BU", "BU", "BI", "BU", "BU", "BU", 
                                       "BI", "BI", "BI", "BU", "BU", "EI", "EI", "EI", "EI", "EI", "EI", 
                                       "EI", "EI", "EI", "EI", "EI", "BI", "BI", "BI", "BI", "BI", "BI", 
                                       "BI", "BI", "BI", "BI", "BI", "BU", "BU", "BI", "BI", "BI", "BI", 
                                       "EI", "FI", "FI", "FI", "FI", "FI", "FI", "FI", "KI", "KI", "KI", 
                                       "KI", "KI", "KI", "KI", "KI", "FI", "EI", "EI", "EI", "EI", "EI", 
                                       "EI", "EI", "EI", "EI", "BU", "BU", "BU", "BU", "BI", "BI", "BI", 
                                       "BU", "BI", "BI", "BI", "BI", "BI", "BI", "KI", "KI", "BU", "BI", 
                                       "BI", "BI", "BI", "EI", "BI", "BI", "BI", "BU", "BI", "BI", "BU", 
                                       "FI", "BU", "BU"), 
                      N_f_SP_group_MoMoK = c("aLB", "aLB", "aLB", 
                                             "ERL", "BI", "aLB", "aLB", "aLB", "aLB", "aLB", "aLB", "aLB", 
                                             "aLB", "aLB", "aLB", "aLB", "aLB", "aLB", "aLB", "aLB", "aLB", 
                                             "aLB", "aNB", "aNB", "aNB", "aNB", "KI", "KI", "FI", "KI", "other", 
                                             "other", "other", "ERL", "ERL", "BI", "BI", "aLB", "aLB", "aLB", 
                                             "aLB", "aLB", "aLB", "aLB", "aLB", "aLB", "aLB", "ERL", "aLB", 
                                             "BI", "BI", "aLB", "aLB", "aLB", "aLB", "aLB", "aLB", "aLB", 
                                             "aLB", "aLB", "KI", "aLB", "aLB", "aLB", "aLB", "aLB", "aLB", 
                                             "aLB", "aLB", "aLB", "aLB", "aLB", "aLB", "aLB", "aLB", "aLB", 
                                             "aLB", "aLB", "aLB", "aNB", "aNB", "FI", "FI", "FI", "KI", "aNB", 
                                             "KI", "KI", "aNB", "aNB", "KI", "aNB", "aNB", "aNB", "aNB", "aNB", 
                                             "aNB", "aNB", "aNB", "aNB", "FI", "KI", "aNB", "aNB", "aNB", 
                                             "aNB", "FI", "aNB", "aNB", "FI", "KI", "aNB", "aNB", "FI", "aNB", 
                                             "aNB", "aNB", "aLB", "aLB", "aLB", "aLB", "ERL", "aLB", "aLB", 
                                             "aLB", "aLB", "aLB", "aLB", "aNB", "aNB", "aNB", "aNB", "aNB", 
                                             "aLB", "aLB", "aLB", "aLB", "aLB", "aLB", "aLB", "aLB", "aLB", 
                                             "aLB", "aLB", "aLB", "aLB", "aLB", "aLB", "aLB", "aLB", "aLB", 
                                             "aLB", "aLB", "aLB", "aLB", "aLB", "aLB", "aLB", "aLB", "aLB", 
                                             "aLB", "aLB", "aLB", "aLB", "aLB", "aLB", "aLB", "aLB", "aLB", 
                                             "aLB", "BI", "aLB", "aLB", "aLB", "aLB", "aNB", "aNB", "aNB", 
                                             "aNB", "aNB", "aNB", "aNB", "KI", "KI", "KI", "KI", "KI", "KI", 
                                             "KI", "KI", "aNB", "aLB", "aLB", "aLB", "aLB", "aLB", "aLB", 
                                             "aLB", "aLB", "aLB", "aLB", "aLB", "aLB", "aLB", "aLB", "aLB", 
                                             "aLB", "aLB", "aLB", "aLB", "aLB", "aLB", "aLB", "aLB", "KI", 
                                             "KI", "aLB", "aLB", "aLB", "aLB", "aLB", "aLB", "aLB", "aLB", 
                                             "aLB", "aLB", "aLB", "aLB", "aLB", "aNB", "aLB", "aLB"), 
                      RG_Wolff_bio = c("BAH","BAH", "BAH", "BI", "BI", "ES", "ES", "BU", "ES", "BI", "BI", 
                                       "EI", "EI", "ES", "BI", "VB", "ES", "ES", "BI", "VB", "ES", "BAH", 
                                       "FI", "FI", "FI", "FI", "KI", "KI", "FI", "KI", "other", "other", 
                                       "other", "BI", "BI", "BI", "BI", "EI", "EI", "ES", "ES", "BI", 
                                       "ES", "BI", "ES", "ES", "BI", "BI", "BI", "BI", "BI", "VB", "BI", 
                                       "ES", "ES", "ES", "BI", "ES", "BI", "VB", "KI", "EI", "BI", "BI", 
                                       "BI", "ES", "BI", "BI", "BI", "EI", "BI", "BI", "EI", "BI", "BI", 
                                       "BI", "BI", "BI", "BI", "FI", "FI", "FI", "FI", "FI", "KI", "FI", 
                                       "KI", "KI", "FI", "FI", "KI", "FI", "FI", "FI", "FI", "FI", "FI", 
                                       "FI", "FI", "FI", "FI", "KI", "FI", "FI", "FI", "FI", "FI", "FI", 
                                       "FI", "FI", "KI", "FI", "FI", "FI", "FI", "FI", "FI", "BAH", 
                                       "BAH", "BI", "BI", "BI", "BI", "BI", "BU", "BI", "BI", "BI", 
                                       "FI", "FI", "FI", "FI", "FI", "BI", "BI", "ES", "ES", "BI", "BU", 
                                       "ES", "ES", "BI", "BI", "BI", "ES", "ES", "EI", "EI", "EI", "EI", 
                                       "EI", "EI", "EI", "EI", "EI", "EI", "EI", "BI", "BI", "BI", "BI", 
                                       "BI", "BI", "BI", "BI", "BI", "BI", "BI", "FKD", "FKD", "BI", 
                                       "BI", "BI", "BI", "EI", "FI", "FI", "FI", "FI", "FI", "FI", "FI",
                                       "KI", "KI", "KI", "KI", "KI", "KI", "KI", "KI", "FI", "EI", "EI", 
                                       "EI", "EI", "EI", "EI", "EI", "EI", "EI", "BAH", "BAH", "BAH", 
                                       "BAH", "BI", "BI", "BI", "ES", "BI", "BI", "BI", "BI", "BI", 
                                       "BI", "KI", "KI", "ES", "BI", "BI", "BI", "BI", "EI", "BI", "BI", 
                                       "BI", "VB", "BI", "BI", "ES", "FI", "ES", "ES")), 
                      class = "data.frame", 
                      row.names = c(NA, -232L)))}else{
                        print("SP_names_com_ID_tapeS already exists")
                      }


# 2.2. Nitrogen content --------------------------------------------------------------------------------------------------------
# 2.2.1. Nitrogen content wood ------------------------------------------------------------------------------------------
# this is the result of a dput of N_con_w( nitrogen content in woody compartiments Rumpf et al. 2018), state now: 11.12.2023, 11:54
# dput(N_con_w)
if(!exists('N_con_w')){
  N_con_w <- as.data.frame(
  structure(list(X = 1:48, SP_com = c("BU_stw", "BU_stwb", "BU_sw", 
                                    "BU_swb", "BU_fw", "EI_stw", "EI_stwb", "EI_sw", "EI_swb", "EI_fw", 
                                    "ES_stw", "ES_stwb", "ES_sw", "ES_swb", "ES_fw", "AH_stw", "AH_stwb", 
                                    "AH_sw", "AH_swb", "AH_fw", "BI_stw", "BI_stwb", "BI_sw", "BI_swb", 
                                    "BI_fw", "ERL_stw", "ERL_stwb", "ERL_sw", "ERL_swb", "ERL_fw", 
                                    "FI_stw", "FI_stwb", "FI_sw", "FI_swb", "FI_fw", "FI_f", "KI_stw", 
                                    "KI_stwb", "KI_sw", "KI_swb", "KI_fw", "KI_f", "DGL_stw", "DGL_stwb", 
                                    "DGL_sw", "DGL_swb", "DGL_fw", "DGL_f"), 
               SP_BWI = c("BU", "BU","BU", "BU", "BU", "EI", "EI", "EI", "EI", "EI", "ES", "ES", "ES", 
                          "ES", "ES", "AH", "AH", "AH", "AH", "AH", "BI", "BI", "BI", "BI", 
                          "BI", "ERL", "ERL", "ERL", "ERL", "ERL", "FI", "FI", "FI", "FI", 
                          "FI", "FI", "KI", "KI", "KI", "KI", "KI", "KI", "DGL", "DGL", 
                          "DGL", "DGL", "DGL", "DGL"), 
               compartiment = c("stw", "stb", "sw", "sb", "fwb", "stw", "stb", "sw", "sb", "fwb", "stw", "stb", "sw", 
                                "sb", "fwb", "stw", "stb", "sw", "sb", "fwb", "stw", "stb", "sw", 
                                "sb", "fwb", "stw", "stb", "sw", "sb", "fwb", "stw", "stb", "sw", 
                                "sb", "fwb", "ndl", "stw", "stb", "sw", "sb", "fwb", "ndl", "stw", 
                                "stb", "sw", "sb", "fwb", "ndl"), 
               N_mean_gkg = c("1.335", "7.227", "1.335", "7.227", "4.601", "1.752", "6.507", "1.752", "6.507", 
                               "6.209", "1.438", "5.348", "1.438", "5.348", "3.721", "1.465", 
                               "7.729", "1.465", "7.729", "4.278", "1.828", "6.131", "1.828", 
                               "6.131", "6.057", "2.475", "11.028", "2.475", "11.028", "7.214", 
                               "0.812", "4.84", "0.812", "4.84", "4.343", "12.978", "0.794", 
                               "4.339", "0.794", "4.339", "4.058", "15.201", "0.701", "3.91", 
                               "0.701", "3.91", "4.203", "15.166"), 
               reference = c("Rumpf et al. 2018", "Rumpf et al. 2018", "Rumpf et al. 2018", "Rumpf et al. 2018", 
                            "Rumpf et al. 2018", "Rumpf et al. 2018", "Rumpf et al. 2018", 
                            "Rumpf et al. 2018", "Rumpf et al. 2018", "Rumpf et al. 2018", 
                            "Rumpf et al. 2018", "Rumpf et al. 2018", "Rumpf et al. 2018", 
                            "Rumpf et al. 2018", "Rumpf et al. 2018", "Rumpf et al. 2018", 
                            "Rumpf et al. 2018", "Rumpf et al. 2018", "Rumpf et al. 2018", 
                            "Rumpf et al. 2018", "Rumpf et al. 2018", "Rumpf et al. 2018", 
                            "Rumpf et al. 2018", "Rumpf et al. 2018", "Rumpf et al. 2018", 
                            "Rumpf et al. 2018", "Rumpf et al. 2018", "Rumpf et al. 2018", 
                            "Rumpf et al. 2018", "Rumpf et al. 2018", "Rumpf et al. 2018", 
                            "Rumpf et al. 2018", "Rumpf et al. 2018", "Rumpf et al. 2018", 
                            "Rumpf et al. 2018", "Rumpf et al. 2018", "Rumpf et al. 2018", 
                            "Rumpf et al. 2018", "Rumpf et al. 2018", "Rumpf et al. 2018", 
                            "Rumpf et al. 2018", "Rumpf et al. 2018", "Rumpf et al. 2018", 
                            "Rumpf et al. 2018", "Rumpf et al. 2018", "Rumpf et al. 2018", 
                            "Rumpf et al. 2018", "Rumpf et al. 2018"), 
               N_con = c("0.001335", "0.007227", "0.001335", "0.007227", "0.004601", "0.001752", "0.006507", 
                         "0.001752", "0.006507", "0.006209", "0.001438", "0.005348", "0.001438", 
                         "0.005348", "0.003721", "0.001465", "0.007729", "0.001465", "0.007729", 
                         "0.004278", "0.001828", "0.006131", "0.001828", "0.006131", "0.006057", 
                         "0.002475", "0.011028", "0.002475", "0.011028", "0.007214", "0.000812", 
                         "0.00484", "0.000812", "0.00484", "0.004343", "0.012978", "0.000794", 
                         "0.004339", "0.000794", "0.004339", "0.004058", "0.015201", "0.000701", 
                         "0.00391", "0.000701", "0.00391", "0.004203", "0.015166")), 
          class = "data.frame", 
          row.names = c(NA, -48L)))}else{
            print("N_con_w already exists")
          }


# 2.2.2. Nitrogen content foliage -----------------------------------------
# this is the result of a dput of N_con_f (nitrogen content in foliage), state now: 11.12.2023, 11:54
#dput(N_con_f)
if(!exists('N_con_f')){N_con_f <- as.data.frame(
  structure(list(X = 1:6, name = c("Birke", "Erle", "Gewoehnliche Fichte","Gewoehnliche Kiefer", "LB", "NB"), 
                 N_f_SP_group_MoMoK = c("BI", "ERL", "FI", "KI", "aLB", "aNB"), 
                 compartiment = c("ndl", "ndl", "ndl", "ndl", "ndl", "ndl"), 
                 SP_com = c("BI_f", "ERL_f", "FI_f", "KI_f", "aLB_f", "aNB_f"),
                 LH_NH = c("LB", "LB", "NB", "NB",  "LB", "NB"),
                 N_mean_gkg = c("27.9538461538462", "27.2095238095238", "14.1362851664451", "17.0535167934124", "27.581684981685", "15.5949009799288"), 
                 N_con = c("0.0279538461538462", "0.0272095238095238", "0.0141362851664451", "0.0170535167934124", "0.027581684981685", "0.0155949009799288")), 
            class = "data.frame", 
            row.names = c(NA, -6L)))}else{
              print("N_con_f already exists")
            }


# 2.3. DBH transformation -------------------------------------------------


# 2.3.1. DBH region -------------------------------------------------------
# this is the output of a dput of DBH_region which is called neu_x_ld.csv in the BZE database, state now: 11.12.2023, 11:54
# dput(DBH_region)
if(!exists('DBH_region')){
  DBH_region <- as.data.frame(
  structure(list(icode_reg = c(-9L, -2L, -1L, 1L, 2L, 3L, 4L, 5L,6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 20L), 
                 reg_shortG = c("fehlt",  "nicht ausgeprägt", "nicht erhoben", "SH", "HH", "NI", "HB",
                                "NW", "HE", "RP", "BW", "BY", "SL", "BE", "BB", "MV", "SN", "ST",  "TH", "TD"), 
                 reg_longG = c("Merkmal vergessen, nicht rekonstruierbar oder unbekannt", "Merkmal nicht ausgeprägt/nicht vorhanden", 
                               "Merkmal nicht erhoben", "Schleswig-Holstein", "Hansestadt Hamburg", "Niedersachsen", 
                               "Hansestadt Bremen", "Nordrhein-Westfalen", "Hessen", "Rheinland-Pfalz", 
                               "Baden-Württemberg", "Bayern", "Saarland", "Berlin", "Brandenburg", 
                               "Mecklenburg-Vorpommern", "Sachsen", "Sachsen-Anhalt", "Thüringen",  "Testdaten"), 
                 country = c(-9L, -2L, -1L, 1L, 1L, 3L, 3L, 5L,6L, 7L, 8L, 9L, 10L, 12L, 12L, 13L, 14L, 15L, 16L, 20L), 
                 region = c(-2L, -2L, -2L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 2L, 2L, 2L, 1L, 2L, 2L, 2L, -2L)), 
            class = "data.frame", 
            row.names = c(NA, -20L)))}else{
              print("DBH_region already exists")
            }

# 2.3.2. DBH_tangenz ------------------------------------------------------
# this is the output of a dput of DBH_region which is called neu_x_ld.csv in the BZE database, state now: 11.12.2023, 11:54
#dput(DBH_tan)
if(!exists('DBH_tan')){
  DBH_tan <- as.data.frame(
  structure(list(SP_BWI1 = c("AH", "AH", "AH", "BAH", "BAH", "BAH", 
                             "BI", "BI", "BI", "BLB", "BLB", "BLB", "BPA", "BPA", "BPA", "BU", 
                             "BU", "BU", "DGL", "DGL", "DGL", "EI", "EI", "EI", "EIB", "EIB", 
                             "EIB", "EL", "EL", "EL", "ELA", "ELA", "ELA", "ERL", "ERL", "ERL", 
                             "ES", "ES", "ES", "FAH", "FAH", "FAH", "FI", "FI", "FI", "HBU", 
                             "HBU", "HBU", "JLA", "JLA", "JLA", "KA", "KA", "KA", "KI", "KI", 
                             "KI", "KIR", "KIR", "KIR", "KTA", "KTA", "KTA", "LAE", "LAE", 
                             "LAE", "LB", "LB", "LB", "LI", "LI", "LI", "NB", "NB", "NB", 
                             "PA", "PA", "PA", "REI", "REI", "REI", "ROB", "ROB", "ROB", "SAH", 
                             "SAH", "SAH", "SFI", "SFI", "SFI", "SKI", "SKI", "SKI", "SLB", 
                             "SLB", "SLB", "SNB", "SNB", "SNB", "TA", "TA", "TA", "THU", "THU", 
                             "THU", "TSU", "TSU", "TSU", "UL", "UL", "UL", "VB", "VB", "VB", 
                             "WEI", "WEI", "WEI", "WEY", "WEY", "WEY"), 
                 region = c(1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 
                           1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 
                           2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 
                           3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 
                           1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 
                           2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 
                           3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 
                           1L, 2L, 3L, 1L, 2L, 3L), 
                 tangenz = c(40, 45.6, 44.5, 40, 45.6, 44.5, 41.1, 42, 45.5, 37, 42.2, 40, 38.8, 40, 43.7, 43.8, 44.3, 
                            48.3, 52.5, 45.9, 52.4, 35.6, 42.9, 43.9, 38.8, 63.5, 46.7, 26.5, 
                            26.5, 30.4, 60.8, 55.6, 67, 45, 44.8, 46.3, 40.1, 37.9, 46.7, 
                            24.3, 24.3, 32.5, 54, 54, 63.8, 35.6, 37.3, 35.4, 60.8, 55.6, 
                            67, 26, 36.4, 36.2, 52.8, 54, 60, 45.2, 29.8, 38.9, 48, 43.3, 
                            49.3, 60.8, 55.6, 67, 37, 33.2, 33.5, 26.7, 43, 37.9, 38.8, 63.5, 
                            46.7, 38.8, 40, 43.7, 35.6, 42.9, 43.9, 30, 36.3, 36.5, 40, 45.6, 
                            44.5, 50.3, 45.2, 70.7, 40, 46.9, 52.8, 37, 33.2, 33.5, 38.8, 
                            63.5, 46.7, 49.5, 61.8, 62, 31.7, 31.7, 48, 43.2, 43.2, 43.2, 
                            26, 21.9, 26.5, 42, 34.2, 36.6, 30, 30, 31.7, 50.7, 65.5, 59.6), 
                 icode = c(901L, 901L, 901L, 902L, 902L, 902L, 903L, 903L, 
                            903L, 904L, 904L, 904L, 905L, 905L, 905L, 906L, 906L, 906L, 907L, 
                            907L, 907L, 908L, 908L, 908L, 94L, 94L, 94L, 909L, 909L, 909L, 
                            910L, 910L, 910L, 911L, 911L, 911L, 912L, 912L, 912L, 913L, 913L, 
                            913L, 914L, 914L, 914L, 915L, 915L, 915L, 916L, 916L, 916L, 917L, 
                            917L, 917L, 918L, 918L, 918L, 919L, 919L, 919L, 920L, 920L, 920L, 
                            921L, 921L, 921L, 929L, 929L, 929L, 922L, 922L, 922L, 930L, 930L, 
                            930L, 923L, 923L, 923L, 924L, 924L, 924L, 925L, 925L, 925L, 926L, 
                            926L, 926L, 927L, 927L, 927L, 928L, 928L, 928L, 929L, 929L, 929L, 
                            930L, 930L, 930L, 931L, 931L, 931L, 932L, 932L, 932L, 933L, 933L, 
                            933L, 934L, 934L, 934L, 935L, 935L, 935L, 936L, 936L, 936L, 937L, 
                            937L, 937L)), 
            class = "data.frame", 
            row.names = c(NA, -120L)))}else{
              print("DBH_tan already exists")
            }



