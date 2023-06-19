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
# BZE3 BE dataset: this dataset contains the inventory data of the tree inventory accompanying the third national soil inventory
HBI_trees <- read.delim(file = here("data/input/BZE2_HBI/beab.csv"), sep = ",", dec = ",")

# BZE3 BE dataset: this dataset contains the inventory data of the tree inventory accompanying the third national soil inventory
# BZE3_trees <- read.delim(file = here("data/input/BZE3/BZE3_trees_total.csv"), sep = ";", dec = ",")

 SP_names_com_ID_tapeS <- read.delim(file = here("output/out_data/x_bart_tapeS.csv"), sep = ",", dec = ",") 

forest_edges_HBI <- read.delim(file = here("data/input/BZE2_HBI/be_waldraender.csv"), sep = ";", dec = ",")
# forest_edges_BZE3 <-    



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
# ---- 0.5.1. classes -----------------------------------------------------
# ----- 0.5.1.1. DBH class ----------------------------------------------------------
DBH_c_function <- function(dbh){
  # create label for diameter classes according to BZE3 Bestandesaufnahmeanleitung
  labs_DBH <- c(seq(5, 55, by = 5)) ; 
  DBH_c <- cut(dbh,                               # cut the diameter
               breaks = c(seq(5, 55, by = 5), Inf),  # in sequences of 5
               labels = labs_DBH,                    # and label it according to labs (1.4.1)
               right = FALSE);
  return(DBH_c)
}

# ----- 0.5.1.2. age class ----------------------------------------------------------
# defining age classes from 1 to 160 in steps of 20
# this is a preparation fot the comparison with carbon stocks calcualted by te
labs_age <- c(seq(1, 180, by = 20))


# ----- 0.5.2. height functions -------------------------------------------
# ----- 0.5.2.1. selection of height function -------------------------------------------
# this function is used to select the coefficients of the height models depending on the R2
# for x, y,a, b (can be whatever)
f = function(x,y,a,b){
  # do the following: if x is na, or x is smaller then y, then use a, if not use b 
  answer <- ifelse(is.na(x)| x < y, a, b)
  return(answer)}


# ---- 0.5.2.2. einheitshoehenkurve------------------------------------------------------
# ---- 0.5.2.2.1 Sloboda ------------------------------------------------------
ehk_sloboda <- function(spec, d_i, d_mean, d_g, h_g) { #, id_broken) {
  k0 <- c(fi = 0.183, ta = 0.097, dgl = 0.24, ki = 0.29, lae = 0.074, bu = 0.032, ei = 0.102, alh = 0.122, aln = 0.032)
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
  return(h_pred)#*f_red)
}

# ---- 0.5.2.2.2. Curtis ------------------------------------------------------
# --> this one is only applied when there is literally not information to calculate the height, 
# except of the diameter
h_curtis <- function(spec, d) {
  b0 <- c(fi = 434.1235, bu = 382.0202, ta = 453.5538, ki = 359.7162, lae = 421.4473, dgl = 481.5531, ei = 348.3262);
  b1 <- c(fi = -65586.6915, bu = -51800.9382, ta = -81132.5221, ki = -42967.9947, lae = -60241.2948, dgl = -81754.2523, ei = -46547.3645);
  b2 <- c(fi = 3074967.1738, bu = 2374368.3254, ta = 4285801.5636, ki = 1763359.9972, lae = 2895409.6245, dgl = 4193121.2406, ei = 2119420.9444);
  return((b0[tolower(spec)] + b1[tolower(spec)]*1/d + b2[tolower(spec)]*1/d^2)/10)   # divide by 10 to transform dm into meters
}

# ---- 0.5.2.3. self-fitted nls models ------------------------------------------------------
# ---- 0.5.2.3.1. species- & plot-wise self-fitted nls models ------------------------------------------------------
# self made nls models for heights per species across all plots
h_nls_SP <- function(spec, d){
  # https://statisticsglobe.com/convert-data-frame-column-to-a-vector-in-r
  b0 <- dplyr::pull(coeff_H_SP, b0, SP_code);
  b1 <- dplyr::pull(coeff_H_SP, b1, SP_code);
  b2 <- dplyr::pull(coeff_H_SP, b2, SP_code);
  return(b0[spec] * (1 - exp( -b1[spec] * d))^b2[spec])
}
# ---- 0.5.2.3.2. species-wise self-fitted nls models ------------------------------------------------------
# self mase nls models for heights per species per plot
h_nls_SP_P <- function(plot_spec, d) {
  # because I cannot combine 3 variabels in one vector, 
  b0 <- coeff_H_SP_P %>% unite(SP_P_ID, plot_ID, SP_code, sep = "", remove = FALSE) %>% dplyr::pull(b0, SP_P_ID);
  b1 <- coeff_H_SP_P %>% unite(SP_P_ID, plot_ID, SP_code, sep = "", remove = FALSE) %>% dplyr::pull(b1, SP_P_ID);
  b2 <- coeff_H_SP_P %>% unite(SP_P_ID, plot_ID, SP_code, sep = "", remove = FALSE) %>% dplyr::pull(b2, SP_P_ID);
  return(b0[plot_spec] * (1 - exp( -b1[plot_spec] * d))^b2[plot_spec])
}



# ----- 0.5.3. coordinate functions ---------------------------------------
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


# ----- 1.1.2. height coefficients  ----------------------------------------------

# ------ 1.1.2.1. binding HBI & BZE3 together for heights -----------------
# trees_total <- rbind(HBI_trees %>% mutate(inventory = "HBI"),    # adding column displaying inventory year
#                      BZE3_trees %>% mutate(inventory = "BZE3"))  # adding column displaying inventory year


# ----- 1.1.2.2. changing units, etc.  ----------------------------
trees_total <- trees_total %>% 
  mutate(H_m = H_dm/10,                                            # change unit of height into m insted of dm by divinding by 10 
         DBH_h_cm = ifelse(is.na(DBH_h_cm), 130, DBH_h_cm),        # assing DBH measuring height of 130cm when missing
         DBH_h_m = DBH_h_cm/100 ,                                  # change unit of DBH measuring height from cm into m by dividing by 100         
         # TapeS : aing diameter at 0.3 tree height to trees_total dataframe
         #https://gitlab.com/vochr/tapes/-/blob/master/vignettes/tapes.rmd
                              # create tprTrees ojekt
         DBH_cm = tprDiameter(tprTrees(spp = tpS_ID,               # tapeS Specie code
                                       Dm = as.list(D_mm/10),      # diameter in cm
                                       Hm = as.list(DBH_h_m),      # measuring height diameter
                                       #Ht = estHeight(d13 = dw_D_g, sp = SP_dw_tps)                  # height of the tree --> problematic, because we don´t have it yet, because we calculate if from the DBH
                                       inv = 4),                   # use TapeR curves from NFI4 
                              Hx = 1.3, cp=FALSE),                 # calculate diameter at 1.3 m height via TapeS
         DBH_class = ifelse(is.na(DBH_class), DBH_c_function(DBH_cm, DBH_class)), 
         BA_m2 = c_A(DBH_cm/2)*0.0001)                             # 0.0001 to change unit from cm2 to m2
         

# ----- 1.1.2.3. non linear height model h ~ DBH per species and plot ----------------------------
# to calculate individual tree heights for trees of the samme species and plot 
# where the height has not been sampled we create a non-linear regression for the heights
# in the following a dataframe with regression coefficients per 
# species per plot is created if there are more then 3 heights measured per species and plot

# coefficents of non-linear height model per species and plot
# https://rdrr.io/cran/forestmangr/f/vignettes/eq_group_fit_en.Rmd
coeff_H_SP_P <- left_join(
  # select variables needed for modeling 
  trees_total %>% select(plot_ID, SP_code, H_m, DBH_cm, DBH_class) %>% 
    # filter for measured heights that also the necessary info about the diameter and measuring height of diameter
    filter(!is.na(H_m) & !is.na(DBH_cm) & !is.na(DBH_class)) %>% 
    group_by(plot_ID, SP_code) %>%
    # filter for plots that have at least 3 heights measured per species
    filter(n() >= 3),
  # creaing & joining in coeff_H_SP_P dataset 
  trees_total %>% 
    select(plot_ID, SP_code, H_m, DBH_cm, DBH_class) %>% 
    filter(!is.na(H_m) & !is.na(DBH_cm) & !is.na(DBH_class) ) %>% 
    #filter(DBH_cm <= 150) %>% 
    group_by(plot_ID, SP_code) %>% 
    # filter for plots where there is at least 3 heights measured for each species
    #https://stackoverflow.com/questions/20204257/subset-data-frame-based-on-number-of-rows-per-group
    filter(n() >= 3)%>%    
    group_by(plot_ID, SP_code) %>%
    nls_table( H_m ~ b0 * (1 - exp( -b1 * DBH_cm))^b2, 
               mod_start = c(b0=23, b1=0.03, b2 =1.3), 
               output = "table") %>%
    arrange(plot_ID, SP_code), 
  by = c("plot_ID", "SP_code"))%>%
  # mutating statistical precictors
  mutate(H_est = b0 * (1 - exp( -b1 * DBH_cm))^b2) %>% 
  group_by(plot_ID, SP_code) %>% 
  summarise( b0 = mean(b0), 
             b1 = mean(b1), 
             b2 = mean(b2), 
             #https://rdrr.io/cran/forestmangr/f/vignettes/eq_group_fit_en.Rmd
             bias = bias_per(y = H_m, yhat = H_est),
             rsme = rmse_per(y = H_m, yhat = H_est),
             #https://stackoverflow.com/questions/14530770/calculating-r2-for-a-nonlinear-least-squares-fit
             R2 = max(cor(H_m, H_est),0)^2,
             #https://stats.stackexchange.com/questions/11676/pseudo-r-squared-formula-for-glms
             mean_h = mean(H_m), 
             #N = length(H_m), 
             SSres = sum((H_m-H_est)^2), 
             SStot = sum((H_m-mean_h)^2), 
             pseu_R2 = 1-(SSres/SStot), 
             diff_h = mean(H_m - H_est))


# ----- 1.1.2.3. non linear height model h ~ DBH per species over all plots ----------------------------

# coefficents of non-linear height model per species but over all plots: 
# https://rdrr.io/cran/forestmangr/f/vignettes/eq_group_fit_en.Rmd
#  building separate dataframe for speicies soecific models adding adding bias, rmse and rsqrd 

coeff_H_SP <- left_join(trees_total %>% 
                          select(SP_code, H_m, DBH_cm, DBH_class) %>% 
                          filter(!is.na(H_m) & !is.na(DBH_cm) & !is.na(DBH_class)) %>% 
                          group_by(SP_code) %>% 
                          filter(n() >= 3),
                        trees_total %>% 
                          select(SP_code, H_m, DBH_cm, DBH_class) %>% 
                          filter(!is.na(H_m) & !is.na(DBH_cm) & !is.na(DBH_class) ) %>% 
                          #filter(DBH_cm <= 150) %>% 
                          group_by(SP_code) %>% 
                          # filter for plots where there is at least 3 heights measured for each species
                          #https://stackoverflow.com/questions/20204257/subset-data-frame-based-on-number-of-rows-per-group
                          filter(n() >= 3)%>%    
                          group_by(SP_code) %>%
                          nls_table( H_m ~ b0 * (1 - exp( -b1 * DBH_cm))^b2, 
                                     mod_start = c(b0=23, b1=0.03, b2 =1.3), 
                                     output = "table"), 
                        by = c("SP_code"))%>%
  mutate(H_est = b0 * (1 - exp( -b1 * DBH_cm))^b2) %>% 
  group_by(SP_code) %>% 
  summarise( b0 = mean(b0), 
             b1 = mean(b1), 
             b2 = mean(b2), 
             #https://rdrr.io/cran/forestmangr/f/vignettes/eq_group_fit_en.Rmd
             bias = bias_per(y = H_m, yhat = H_est),
             rsme = rmse_per(y = H_m, yhat = H_est),
             #https://stackoverflow.com/questions/14530770/calculating-r2-for-a-nonlinear-least-squares-fit
             R2 = max(cor(H_m, H_est),0)^2,
             #https://stats.stackexchange.com/questions/11676/pseudo-r-squared-formula-for-glms
             mean_h = mean(H_m), 
             #N = length(H_m), 
             SSres = sum((H_m-H_est)^2), 
             SStot = sum((H_m-mean_h)^2), 
             pseu_R2 = 1-(SSres/SStot), 
             diff_h = mean(H_m - H_est)) %>% 
  mutate(plot_ID = as.factor('all')) %>% 
  select(plot_ID, SP_code, b0, b1, b2, bias, rsme, R2, mean_h, SSres, SStot, pseu_R2, diff_h)

# ----- 1.1.2.4. combined coefficients of height models ---------------------
coeff_H_comb <- rbind(coeff_H_SP_P %>% mutate(plot_ID = as.factor(plot_ID)), coeff_H_SP)

# ---- 1.1.2.5. joining coordinates into trees datset --------------------------


# ---- 1.1.2.6. exporting height coefficient dataset --------------------------

write.csv(coeff_H_comb, "output/out_data/coeff_H_BZE.csv")



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
  # 1. a) dataset with coefficients of line going only trough A and B without Knickpunkt
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
# 1.b) for forest edge form 2 that has a tunring point
left_join(
  # 1.b) 1. dataset with coefficients of line from A to T
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
# 1.b) 2. dataset with coefficients of line from B to T
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
HBI_trees %>% 
  mutate(X_tree = x_coord(Dist_cm, azi_gon), 
         Y_tree = y_coord(Dist_cm, azi_gon)) %>% 
  # first join in forest edge type and ID
  left_join(., forest_edges_HBI %>% select(plot_ID,e_ID, e_type, e_form), 
            by = "plot_ID", 
            multiple = "all") %>% 
  # then join in parameters by forest type
  left_join(., forest_edges_HBI %>% select(plot_ID, e_ID, e_type, e_form, A_azi,  B_azi, X_T, e_b0_AB, e_b1_AB, e_b0_AT, e_b1_AT, e_b0_BT, e_b1_BT),
            by = c("plot_ID","e_ID", "e_type", "e_form"))%>% 
    # calculate the Y of the edge for the x of the tree
    mutate(Y_e_tree = case_when(e_form == "1" ~ e_b0_AB + e_b1_AB*X_tree, 
                                # if forest edge form == 2 so there is a  turning point, 
                                e_form == "2" & data.table::between(X_tree, x_coord(1700,  A_azi), X_T)  ~ e_b0_AT + e_b1_AT*X_tree, # calculate y with AT parameters, if x of tree lies within x of A where it meets the outer sampling circuit (17m distance) and X of T  
                                e_form == "2" & data.table::between(X_tree, x_coord(1700,  B_azi), X_T) ~ e_b0_BT + e_b1_BT*X_tree,  # calculate y with BT parameters, if x of tree lies within x of B where it meets the outer sampling circuit (17m distance) and X of T  
                                
                                TRUE ~ NA)) %>% 
  #filter(e_type %in% c("1", "2"))   # there are 1590 trees in plots with an edge 
  filter(Y_tree > Y_e_tree)          # out of which 312 have a y higher then the y of the edge line at the respective x coordinate of the tree 


# apatently there are plots with 2 edges: which caused the warning when joining edge info into the trees dataset
forest_edges_HBI %>% select(plot_ID) %>% distinct() %>% summarise(n = n()) # 69
forest_edges_HBI %>% select(e_form) %>%  ungroup() %>% distinct() %>% summarise(n = n()) #75
forest_edges_HBI %>% select(plot_ID, e_form) %>%  group_by(plot_ID) %>% distinct() %>% summarise(n = n()) %>% filter(n > 1)
forest_edges_HBI %>% filter(plot_ID %in% c(50042, 50057, 50075, 50102, 50122, 50142))

# further i have to think about what to do in terms of the area of the plot --> 
# i´ll have to cut the circles individually and change their area individually 


  

# ----- 1.2. REGENERATION -------------------------------------------------
# ----- 1.3. DEADWOOD -------------------------------------------------
