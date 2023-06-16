# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the peat land soil inventory
# data preparation  


# goal/ purpose of this script
  # - remove NAs: 
  #        - diameter
  #        - diameter measuring height
  # - assign necesarry species codes: 
  #        - living trees: BWI, Bio, height, N_SP_group, tapeS
  #        - regeneration: Annighöfer? --> rather not because we don´t use it for biomass calculation anymore 
  

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

# ----- 0.3. working directory -------------------------------------------------
here::here()
getwd()



# ----- 0.4 data import -------------------------------------------------------
# LIVING TREES
# BZE3 BE dataset: this dataset contains the inventory data of the tree inventory 
#                  accompanying the third national soil inventory

HBI_trees <- read.delim(file = here("data/input/BZE2_HBI/beab.csv"), sep = ",", dec = ",")
# BZE3_trees <- read.delim(file = here("data/input/BZE3/BZE3_trees_total.csv"), sep = ";", dec = ",")
# coeff_H_comb <- read.delim(file = here("data/input/BZE3/coeff_H_BZE.csv"), sep = ";", dec = ",")
# SP_names_com_ID_tapeS <- read.delim(file = here("output/out_data/coeff_H_BZE.csv"), sep = ";", dec = ",") 
# forest_edges <- 
forest_edges_HBI <- read.delim(file = here("data/input/BZE2_HBI/be_waldraender.csv"), sep = ";", dec = ",")
   



# REGENERATION 


# DEADWOOD


# ----- 0.6 harmonising column names & structure  -------------------------
# HBI 
colnames(HBI_trees) <- c("multi_stem", "D_mm", "stufe", "DBH_h_cm", "H_dm",
                         "azi_gon", "SP_code", "tree_ID", "plot_ID", "tree_status", 
                         "DBH_cm", "age", "C_layer", "C_h_dm", "Kraft", "Dist_cm", "age_meth")  
HBI_trees <- HBI_trees %>% select(plot_ID,  tree_ID ,  tree_status ,  multi_stem ,
                                  Dist_cm ,  azi_gon ,age ,  age_meth ,  SP_code , stufe ,  Kraft ,  
                                  C_layer , H_dm ,  C_h_dm , D_mm ,   DBH_h_cm ,  DBH_cm )

# Forest edges 
colnames(forest_edges_HBI) <- c("plot_ID", "e_ID", "e_type", "e_form", 
                                "A_dist", "A_azi",  "B_dist", "B_azi", 
                                "T_dist", "T_azi") # t = turning point 

# ----- 0.5 functions ---------------------------------------------------------------
# http://www.markusbaumi.ch/schule/formel/azimut.pdf
x_coord <- function(Dcp, AZIcp){ 
  # Xc =  x coordinate of centre = 0 
  # Dcp = Distance between point and centre
  # AZIcp=  Azimute betweeen Point and centre
  Xc <- 0;
  X = Xc + Dcp * sin(AZIcp);
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



# ----- 1. joining in external info  --------------------------------------


# ----- 1.1. LIVING TREES -------------------------------------------------

# ----- 1.1.1. species names ----------------------------------------------



 # check if there are no trees left that don´t have a SP_code in xBart/ SP_names_com_ID_tapeS



# ----- 1.1.2. height coefficients  ----------------------------------------------



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

# 1a) for waldrandform == 2 we have a turning point in the graph, for WFR == 1 we don´t
   # --> build two lin models (1) X|Y anfang, X|Y Knickpunkt, (2) (1) X|Y ende, X|Y Knickpunkt,
# 1b) for waldrandfrom == 1 
  # create 1 lm function for forest edge 
      # through  X|Y of intersection with sampling circuit 

# 2) calculate X|Y of each tree
    # y tree = y centre + distance * (sin(Azimut between centre and point)

# 3a) filter for trees with x between x anfang and x Knickpunkt and x ende and x knickpunkt 
#     check if y at respeective x is higher then y andfang or y ende
# 3b) filter for trees with Y < Y forest edge function at given X



# coefficients for forest edges 
forest_edges_HBI <- 
forest_edges_HBI %>% 
  # filter for forest edges that have a relevance for tree calculations 
  filter(e_form %in% c("1", "2") & e_type %in% c("1", "2", "3", "4")) %>% 
  mutate(X_A = x_coord(A_dist, A_azi), 
         X_B = x_coord(B_dist, B_azi),
         X_T = ifelse(e_form == "2", x_coord(T_dist, T_azi), NA), 
         Y_A = y_coord(A_dist, A_azi), 
         Y_B = y_coord(B_dist, B_azi), 
         Y_T = ifelse(e_form == "2", y_coord(T_dist, T_azi), NA))%>%
  group_by(plot_ID) %>% 
  left_join(., 
  # 1. dataset with coefficients of line going only trough A and B without Knickpunkt
            forest_edges_HBI %>% 
  # filter for forest edges that have a relevance for tree calculations and dont have a turning point
  filter(e_form == "1" & e_type %in% c("1", "2", "3", "4")) %>%
    # calculate coordinates from Azimut and distance
  mutate(X_A = x_coord(A_dist, A_azi), 
         X_B = x_coord(B_dist, B_azi),
         Y_A = y_coord(A_dist, A_azi), 
         Y_B = y_coord(B_dist, B_azi),)%>%
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
# 2. for forest edge form 2 that has a tunring point
left_join(
  # 2. a) dataset with coefficients of line from A to T
  forest_edges_HBI %>% 
  # filter for forest edges that have a relevance for tree calculations 
  filter(e_form == "2" & e_type %in% c("1", "2", "3", "4")) %>% 
  mutate(X_A = x_coord(A_dist, A_azi),
         X_TA = ifelse(e_form == "2", x_coord(T_dist, T_azi), NA),
         X_B = x_coord(B_dist, B_azi),
         X_TB = ifelse(e_form == "2", x_coord(T_dist, T_azi), NA),
         Y_A = y_coord(A_dist, A_azi),
         Y_TA = ifelse(e_form == "2", y_coord(T_dist, T_azi), NA),
         Y_B = y_coord(B_dist, B_azi),
         Y_TB = ifelse(e_form == "2", y_coord(T_dist, T_azi), NA)) %>%
  # pivotingx and y to fit lm: https://stackoverflow.com/questions/70700654/pivot-longer-with-names-pattern-and-pairs-of-columns
  to_long(keys = c("X_A_T_name", "X_B_T_name", "Y_A_T_name", "Y_B_T_name"), 
          values = c("X_A_T_value", "X_B_T_value", "Y_A_T_value", "Y_B_T_value"),  
          names(.)[11:12], names(.)[13:14], names(.)[15:16], names(.)[17:18]) %>%
  group_by(plot_ID, e_form) %>%
  # https://quantifyinghealth.com/line-equation-from-2-points-in-r/
  lm_table(Y_A_T_value ~ X_A_T_value, output = "table") %>% 
  select(plot_ID, e_form, b0, b1) %>% 
  rename("e_b0_AT" = "b0") %>% 
  rename("e_b1_AT" = "b1"), 
# 2.b) dataset with coefficients of line from B to T
  forest_edges_HBI %>% 
    # filter for forest edges that have a relevance for tree calculations 
    filter(e_form == "2" & e_type %in% c("1", "2", "3", "4")) %>% 
    mutate(X_A = x_coord(A_dist, A_azi),
           X_TA = ifelse(e_form == "2", x_coord(T_dist, T_azi), NA),
           X_B = x_coord(B_dist, B_azi),
           X_TB = ifelse(e_form == "2", x_coord(T_dist, T_azi), NA),
           Y_A = y_coord(A_dist, A_azi),
           Y_TA = ifelse(e_form == "2", y_coord(T_dist, T_azi), NA),
           Y_B = y_coord(B_dist, B_azi),
           Y_TB = ifelse(e_form == "2", y_coord(T_dist, T_azi), NA)) %>%
    # pivotingx and y to fit lm: https://stackoverflow.com/questions/70700654/pivot-longer-with-names-pattern-and-pairs-of-columns
    to_long(keys = c("X_A_T_name", "X_B_T_name", "Y_A_T_name", "Y_B_T_name"), 
            values = c("X_A_T_value", "X_B_T_value", "Y_A_T_value", "Y_B_T_value"),  
            names(.)[11:12], names(.)[13:14], names(.)[15:16], names(.)[17:18]) %>%
    group_by(plot_ID, e_form) %>%
    # https://quantifyinghealth.com/line-equation-from-2-points-in-r/
   lm_table(Y_B_T_value ~ X_B_T_value, output = "table") %>% 
    select(plot_ID, e_form, b0, b1) %>% 
    rename("e_b0_BT" = "b0") %>% 
    rename("e_b1_BT" = "b1"), 
by = c("plot_ID", "e_form")), 
by = c("plot_ID", "e_form")) 


# next step will be to join the forest edges dataset into the trees datset , calcuate the Y of each tree_x 
# via b0 and b1 and then compare the tree_y with the functions result
# if the tree_y is higher then the function_y we have to do something with the tree...
# etc. assing another plot iD or something. 

# further i have to think about what to do in terms of the area of the plot --> 
# i´ll have to cut the circles individually and change their area individually 

# doesn´t make sense because i need two y values in case of e_form == 2
#%>% 
# just to rearange to "to_longer"
# select(plot_ID, e_ID, e_type, e_form, 
#        A_dist, A_azi, B_dist, B_azi, T_dist, T_azi, X_A, X_B, Y_A, Y_B, 
#        e_b0_AB, e_b0_AT, e_b0_BT, 
#        e_b1_AB, e_b1_AT,  e_b1_BT)
#   to_long(keys = c("points_e_b0", "points_e_b1"), 
#           values = c("e_b0", "e_b1"), 
#           names(.)[15:17], names(.)[18:20])
  

# ----- 1.2. REGENERATION -------------------------------------------------
# ----- 1.3. DEADWOOD -------------------------------------------------
