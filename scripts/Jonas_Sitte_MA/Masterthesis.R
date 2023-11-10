# Author: Jonas Sitte
# Master thesis - "Peatland Forests' growth and structure"
# University of Goettingen & Thünen-Institute of Forest Ecosystems

# Topic: Forest Growth and Forest Structure Indices

# ----- 0. SETUP ---------------------------------------------------------------
# ----- 0.1. Packages  ---------------------------------------------------------

## data management  ---
# install.packages("dplyr")
# install.packages("tidyverse")
## visualization ---
# install.packages("ggplot2")
# install.packages("ggpmisc")
# install.packages("directlabels")
#install.packages("patchwork")
# install.packages("ggrepel")
# install.packages("RColorBrewer")
## forest related ---
# install.packages("forestmangr")
# install.packages("TapeR")
# install.packages("TapeS")
## statistical tests ---
# install.packages("multcomp")
# install.packages("car")
# install.packages("coefplot")
# install.packages("SciViews")
# install.packages("lme4")

# ----- 0.2. Libraries  --------------------------------------------------------

## datamanagement  ---
library(dplyr)
library(tidyverse)
## visualization  ---
library(ggplot2)
library(ggpmisc)
library(patchwork)
library(directlabels)
library(ggrepel)
library(RColorBrewer)
## forest related  ---
library(forestmangr)
library(TapeR)
library(TapeS)
## statistical tests ---
library(multcomp)
library(car)
library(coefplot)
library(SciViews)
library(lme4)

# ----- 0.3. working directory -------------------------------------------------
here()
in.out.JS <- "scripts/Jonas_Sitte_MA/"


# ----- 1. DATA ----------------------------------------------------------------
# ------ 1.1. import -----------------------------------------------------------
# ------- 1.1.1. Sites  --------------------------------------------------------

Sites <- read.delim(file = here(paste0(in.out.JS, "sites2.csv")), sep = ";", dec = ",")
str(Sites)


# ------- 1.1.2. Trees  --------------------------------------------------------

#Trees <- read.csv2("Einzelbaummessungen_neu.csv", header = T, sep = ";", dec = ",", check.names = F)
Trees <- read.delim(file = here(paste0(in.out.JS, "Einzelbaummessungen_new.csv")), header = T, sep = ";", dec = ",", check.names = F)
str(Trees)

Trees_SR <- read.delim(file = here(paste0(in.out.JS, "Species_richness.csv")), header = T, sep = ";", dec = ",", check.names = F)
str(Trees_SR)

# adapted data_frame for tree heights evaluation
trees_total <- Trees

# ------- 1.1.3. Regeneration  -------------------------------------------------

#Reg <- read.csv2("Verjuengung_neu.csv", header = T, sep = ";", dec = ",", check.names = F)
Reg <- read.delim(file = here(paste0(in.out.JS, "Verjuengung_new.csv")), header = T, sep = ";", dec = ",", check.names = F)
str(Reg)

Reg2 <- read.delim(file = here(paste0(in.out.JS, "Verjuengung2.csv")), header = T, sep = ";", dec = ",", check.names = F)
str(Reg2)

# ------- 1.1.4. Deadwood  -----------------------------------------------------

#DW <- read.csv2("Totholz_neu.csv", header = T, sep = ";", dec = ",", check.names = F)
DW <- read.delim(file = here(paste0(in.out.JS, "Totholz_new.csv")), header = T, sep = ";", dec = ",", check.names = F)
str(DW)

# ------- 1.1.6. Soil Properties  -----------------------------------------------
pH <- read.delim(file = here(paste0(in.out.JS, "pH_value.csv")), header = T, sep = ";", dec = ",", check.names = F)
str(pH)

# ------ 1.2. colnames, vector type --------------------------------------------
# ------- 1.2.1. Sites  --------------------------------------------------------

# rename columns
colnames(Sites) <- c("ID", "MoMoK_Nr", "incl", "name", "site", "state", 
                     "peatland_type", "hydrology", "peat_thickness", 
                     "sec_soil_develp", "tree_spec", "site_type", "tree_closure")

# filter sites that have actually been finished yet (exclude sites which still needs to be sampled)
Sites <- Sites %>%  filter(incl == "Y")

# translate into English
Sites <- 
  Sites %>% 
  mutate(peatland_type = ifelse (peatland_type == "Niedermoor", "fen", 
                                 ifelse (peatland_type == "Hochmoor", "bog", 
                                         ifelse (peatland_type == "Übergangsmoor", "transitional bog","gley bog")))) %>%
  mutate(hydrology = ifelse (hydrology == "norm", "wet", "drained")) %>%
  mutate(sec_soil_develp = ifelse(sec_soil_develp == "keine", "non", 
                                  ifelse(sec_soil_develp == "wenig", "low", 
                                         ifelse(sec_soil_develp == "mittel", "medium", "high")))) %>% 
  mutate(tree_spec = ifelse(tree_spec == "Birke", "birch", 
                            ifelse(tree_spec == "Erle", "alder",
                                   ifelse(tree_spec == "Kiefer", "pine", "spruce")))) %>% 
  mutate(tree_closure = ifelse(tree_closure == "vereinzelt", "singulary", 
                               ifelse(tree_closure == "relativ geschlossen", "dense", "closed")))


# ------- 1.2.2. Trees  --------------------------------------------------------
# -------- 1.2.2.1 General  ----------------------------------------------------

# rename columns
colnames(Trees) <- c("MoMoK_Nr", "Name", "state", "date", "Nr_PK", "TNr", "ZW", 
                     "St", "tree_spec_code", "tree_spec", "level", "Krafts class",
                     "age", "age_meth", "DBH", "DBH_height", "girth tape", 
                     "DBH_class", "height", "crown_base_height", "distance", 
                     "azimuth_gon", "azimuth_degree", "note")

colnames(Trees_SR) <- c("MoMoK_Nr", "name", "state", "date", "ID", "tree_spec")

# change data type
Trees$MoMoK_Nr <- as.character(Trees$MoMoK_Nr)

# delete rows of MoMoK-Plot 25080 because not all measurements were done
Trees <- Trees[!(Trees$MoMoK_Nr == 25080),]

# convert units
Trees$DBH <- Trees$DBH / 10 # convert DBH unit from mm to cm
Trees$DBH_height <- Trees$DBH_height / 100 # convert DBH height unit from cm to m
Trees$height <- Trees$height / 10 # convert tree height unit from dm to m
Trees$crown_base_height <- Trees$crown_base_height / 10 # convert crown base height unit from dm to m
Trees$distance <- Trees$distance / 100 # convert distance unit from cm to m


# -------- 1.2.2.2 tree_total adaption  ----------------------------------------

colnames(trees_total) <- c("plot_ID", "loc_name", "state", "date", "CCS_nr", "t_ID", "st_ID",
                           "pieces", "SP_nr", "SP_code", "C_layer", "Kraft",
                           "age", "age_m", "DBH_mm", "DBH_h_cm", "DBH_p_mm",
                           "DBH_class", "H_dm", "CH_dm", "dist_m",
                           "azimut_g", "azimut_d", "note")
trees_total$C_layer <- as.numeric(trees_total$C_layer)
# there was a mistake in the species abbreviations where Alnus glutinosa was abbreviated with RER (Roterle) instead of SER (Schwarzerle)
trees_total$SP_code[trees_total$SP_code == "RER"] <- "SER"  
trees_total$SP_code <- as.factor(trees_total$SP_code)

trees_total <- trees_total %>%
  # assign tree groups to all tree species to estimate missing heights with SLOBODA function
  mutate(H_SP_group_slo = case_when( 
    SP_code == "GFI" ~ "fi", # spruce species
    SP_code == "WTA" ~ "ta", # fir species
    SP_code == "DGL" ~ "dgl", # douglas fir
    SP_code %in% c("BKI", "GKI", "STR") ~ "ki", # pine species
    SP_code == "ELA" ~ "lae", # larch species
    SP_code == "RBU" ~ "bu", # beech species
    SP_code %in% c("SEI", "TEI", "REI") ~ "ei", # oak species
    SP_code %in% c("BAH", "SAH", "HBU") ~ "alh", # other broadleave-species with high life expectancy and rotation periods
    SP_code %in% c("SER", "MBI", "SWE", "SPA", "STK", "VBE") ~ "aln", # other broadleave-species with low life expectancy and rotation periods
    TRUE ~ NA_character_
  )) %>% 
  # assign tree groups to all tree species to estimate missing heights with SLOBODA function
  mutate(H_SP_group_curt = case_when(
    SP_code == "GFI" ~ "fi", # spruce and other conifer species
    SP_code %in% c("SER", "MBI", "SWE", "SPA", "STK", "VBE") ~ "bu", # beech trees and other decidious tree species
    SP_code == "WTA" ~ "ta", # fir species
    SP_code %in% c("BKI", "GKI", "STR") ~ "ki", # pine species
    SP_code == "ELA" ~ "lae", # larch species
    SP_code == "DGL" ~ "dgl", # douglas fir
    SP_code %in% c("SEI", "TEI", "REI") ~ "ei", # oak species
    TRUE ~ NA_character_
  )) %>% 
  # assign tree groups to all tree species to estimate tree biomass with Vonderach and Huber equation
  mutate(H_SP_group_bm1 = case_when(
    SP_code %in%c("ELA", "GFI") ~ "fi", # spruce and other conifer species
    SP_code == "WTA" ~ "ta", # fir species
    SP_code == "DGL" ~ "dgl", # douglas fir
    SP_code %in% c("BKI", "GKI", "STR") ~ "ki", # pine species
    SP_code %in% c("SER", "MBI", "SWE", "SPA", "STK", "VBE") ~ "bu",  # beech trees and other decidious tree species
    SP_code %in% c("SEI", "TEI", "REI") ~ "ei", # oak species
    SP_code == "ES" ~ "es", # ash species
    SP_code %in% c("BAH", "SAH") ~ "ah", # maple species
    TRUE ~ NA_character_
  )) %>% 
  mutate(H_SP_group_bm2 = case_when(
    SP_code == "GFI" ~ "fi", # spruce and other conifer species
    SP_code == "WTA" ~ "ta", # fir species
    SP_code == "DGL" ~ "dgl", # douglas fir
    SP_code %in% c("BKI", "GKI", "STR") ~ "ki", # pine species
    SP_code == "ELA" ~ "lae", # larch species
    SP_code == "SPA" ~ "pa", # poplar species
    SP_code == "RBU" ~ "bu", # beech species
    SP_code %in% c("SEI", "TEI", "REI") ~ "ei", # oak species
    SP_code %in% c("BAH", "SAH") ~ "ah", # maple species
    SP_code == "SER" ~ "er", # alder species
    SP_code %in% c("ES", "HBU") ~ "alh", # other broadleave-species with high life expectancy and rotation periods
    SP_code %in% c("MBI", "SWE", "STK", "VBE") ~ "aln", # other broadleave-species with low life expectancy and rotation periods
    TRUE ~ NA_character_
  )) %>% 
  mutate(H_SP_group_wood = case_when(
    SP_code %in% c("GFI", "GKI", "BKI", "STR", "ELA", "DGL", "WTA") ~ "con", # coniferous wood
    SP_code %in% c("SEI", "TEI", "REI", "STK", "VBE") ~ "ringpor", # ring-porous wood
    SP_code %in% c("RBU", "SER", "HBU", "BAH", "SAH", "MBI", "SPA", "SWE") ~ "difpor", # diffuse- porous wood
    TRUE ~ NA_character_
  )) %>% 
  mutate(tpS_ID = case_when( #according to tprSpeciesCode(inSp=NULL)
    SP_code == "GFI" ~ "Fi", # spruce and other conifer species
    SP_code == "WTA" ~ "Ta", # fir species
    SP_code == "DGL" ~ "DG", # douglas fir
    SP_code %in% c("BKI", "GKI", "STR") ~ "Kie", # pine species
    SP_code == "ELA" ~ "EL", # larch species
    SP_code == "SPA" ~ "Pa", # poplar species
    SP_code == "RBU" ~ "Bu", # beech species
    SP_code %in% c("SEI", "TEI") ~ "Ei", # oak species
    SP_code == "REI" ~ "RE", # red oak
    SP_code == "BAH" ~ "BA", # sycamore
    SP_code == "SAH" ~ "SA", # Norway maple
    SP_code == "SER" ~ "Er", # alder species
    SP_code == "ES" ~ "Es", # ash species
    SP_code == "HBU" ~ "HB", # hornbeam
    SP_code == "MBI" ~ "Bi", # birch species
    SP_code == "SWE" ~ "We", # willow species
    SP_code == "VBE" ~ "VB", # rowan
    SP_code %in% c("STK") ~ "LB", # other broadleave-species with low life expectancy and rotation periods
    TRUE ~ NA_character_
  )) %>% 
  mutate(Bio_SP_group = case_when(
    SP_code %in% c("GFI", "WTA", "DGL", "ELA") ~ "fi", # spruce and other conifer species
    SP_code %in% c("BKI", "GKI", "STR") ~ "ki", # pine species
    SP_code %in% c("RBU", "BAH", "SAH", "SER", "ES", "HBU") ~ "bu", # beach and other hardwood broadleaf species
    SP_code %in% c("SEI", "TEI", "REI") ~ "ei", # oak species
    SP_code %in% c("SPA", "MBI", "SWE", "VBE", "STK") ~ "shw", # other softwood broadlead species
  )) %>%
  mutate(LH_NH = case_when(
    SP_code %in% c("GFI", "GKI", "BKI", "STR", "ELA", "DGL", "WTA") ~ "NB", # coniferous wood
    SP_code %in% c("SEI", "TEI", "REI", "STK", "VBE", "RBU", "SER", "HBU", "BAH", "SAH", "MBI", "SPA", "SWE") ~ "LB", # ring-porous wood
    SP_code %in% c() ~ "difpor", # diffuse- porous wood
    TRUE ~ NA_character_
  )) %>% 
  # join in numeric species codes form TapeS species code list
  left_join(., as.data.frame(tprSpeciesCode(inSp = NULL, outSp = NULL)) %>% 
              rename("tapes_no" = "ID"), 
            by = c("tpS_ID" = "kurz"))



# ------- 1.2.3. Regeneration  -------------------------------------------------

# rename columns
colnames(Reg) <- c("MoMoK_Nr", "Name", "state", "date", "Nr_Reg_PK", "direction",
                   "distance_MB", "distance_max", "TNr", "tree_spec_code", 
                   "tree_spec", "height", "height_class")

colnames(Reg2) <- c("MoMoK_Nr", "Name", "state", "date", "TNr", "tree_spec")

# delete rows of MoMoK-Plot 25080 because not all measurements were done
Reg <- Reg[!(Reg$MoMoK_Nr == 25080),]

# translate into English
Reg <- 
  Reg %>% 
  mutate(Nr_Reg_PK = ifelse (Nr_Reg_PK == "N", "N", ifelse (Nr_Reg_PK == "O", "E", 
                                                            ifelse (Nr_Reg_PK == "S", "S", "W")))) %>%
  mutate(direction = ifelse (direction == "Nord", "north", ifelse (direction == "Ost", "east", 
                                                                   ifelse (direction == "Sued", "south", "west"))))

# convert units
Reg$height <- Reg$height / 100 # convert tree height unit from cm to m
Reg$distance_MB <- Reg$distance_MB / 100 # convert distance to middle point unit from cm to m
Reg$distance_max <- Reg$distance_max / 100 # convert distance unit from cm to m

# ------- 1.2.4. Deadwood  -----------------------------------------------------

# rename columns
colnames(DW) <- c("MoMoK_Nr", "Name", "state", "date", "Nr_PK", "DW_NR",
                  "treespec_group", "type", "height_length", "diameter", 
                  "decomposition")

# delete rows of MoMoK-Plot 25080 because not all measurements were done
DW <- DW[!(DW$MoMoK_Nr == 25080),]

# change data type
DW$type <- as.factor(DW$type)

# convert units
DW$height_length <- DW$height_length / 10 # convert tree height unit from cm to m


#setwd("C:/Users/jonas/Documents/Jonas Sitte/Ausbildung, Studium, Job/Studium/Master Universität Göttingen/Module/Master-Thesis/Peatland_Forest Structure/Masterthesis")


# ------- 1.2.5. Soil Properties (von Cornelius) --------------------------------

# rename columns
colnames(pH) <- c("soil_depth", "pH_value", "plot", "MoMoK_Nr", "method", "comment")

# change data type
pH$soil_depth <- as.numeric(pH$soil_depth)
pH$MoMoK_Nr <- as.character(pH$MoMoK_Nr)


setwd("C:/Users/jonas/Documents/Jonas Sitte/Ausbildung, Studium, Job/Studium/Master Universität Göttingen/Module/Master-Thesis/Peatland_Forest Structure/Masterthesis")


# ----- 2. FUNCTIONS -----------------------------------------------------------
# ------ 2.1. classes ----------------------------------------------------------

# area of a circle
c_A <- function(r){
  circle_area <- r^2*pi;
  return(circle_area)
}

# ------- 2.1.1. DBH class -----------------------------------------------------
DBH_c_function <- function(dbh){
  # create label for diameter classes according to BZE3 Bestandesaufnahmeanleitung
  labs_DBH <- c(seq(5, 55, by = 5)) ; 
  DBH_c <- cut(as.numeric(dbh),                               # cut the diameter
               breaks = c(seq(5, 55, by = 5), Inf),  # in sequences of 5
               labels = labs_DBH,                    # and label it according to labs (1.4.1)
               right = FALSE);
  return(DBH_c)
}

# ------- 2.1.2. age class -----------------------------------------------------
# defining age classes from 1 to 160 in steps of 20
# this is a preparation for the comparison with carbon stocks calculated by te
labs_age <- c(seq(1, 180, by = 20))



# ------ 2.2. height functions -------------------------------------------------
# ------- 2.2.1. selection of height function  ---------------------------------
# this function is used to select the coefficients of the height models depending on the R2
# for x, y,a, b (can be whatever)
f = function(x,y,a,b){
  # do the following: if x is na, or x is smaller then y, then use a, if not use b 
  answer <- ifelse(is.na(x)| x < y, a, b)
  return(answer)}


# ------- 2.2.2. Einheitshöhenkurve --------------------------------------------
# -------- 2.2.2.1 Sloboda  ----------------------------------------------------
ehk_sloboda <- function(spec, d_i, d_mean, d_g, h_g) { #, id_broken) {
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
  return(h_pred)#*f_red)
}


# -------- 2.2.2.2. Curtis  ----------------------------------------------------
# --> this one is only applied when there is literally not information to calculate the height, 
# except of the diameter
h_curtis <- function(spec, d) {
  b0 <- c(fi = 434.1235, bu = 382.0202, ta = 453.5538, ki = 359.7162, lae = 421.4473, dgl = 481.5531, ei = 348.3262);
  b1 <- c(fi = -65586.6915, bu = -51800.9382, ta = -81132.5221, ki = -42967.9947, lae = -60241.2948, dgl = -81754.2523, ei = -46547.3645);
  b2 <- c(fi = 3074967.1738, bu = 2374368.3254, ta = 4285801.5636, ki = 1763359.9972, lae = 2895409.6245, dgl = 4193121.2406, ei = 2119420.9444);
  return((b0[tolower(spec)] + b1[tolower(spec)]*1/d + b2[tolower(spec)]*1/d^2)/10)   # divide by 10 to transform dm into meters
}

# ------- 2.2.3. self-fitted nls models ----------------------------------------
# -------- 2.2.3.1. species- & plot-wise self-fitted nls models  ---------------
# self made nls models for heights per species across all plots
h_nls_SP <- function(spec, d){
  # https://statisticsglobe.com/convert-data-frame-column-to-a-vector-in-r
  b0 <- dplyr::pull(coeff_H_SP, b0, SP_code);
  b1 <- dplyr::pull(coeff_H_SP, b1, SP_code);
  b2 <- dplyr::pull(coeff_H_SP, b2, SP_code);
  return(b0[spec] * (1 - exp( -b1[spec] * d))^b2[spec])
}
# -------- 2.2.3.2. species-wise self-fitted nls models  -----------------------
# self made nls models for heights per species per plot
h_nls_SP_P <- function(plot_spec, d) {
  # because I cannot combine 3 variabels in one vector, 
  b0 <- coeff_H_SP_P %>% unite(SP_P_ID, plot_ID, SP_code, sep = "", remove = FALSE) %>% dplyr::pull(b0, SP_P_ID);
  b1 <- coeff_H_SP_P %>% unite(SP_P_ID, plot_ID, SP_code, sep = "", remove = FALSE) %>% dplyr::pull(b1, SP_P_ID);
  b2 <- coeff_H_SP_P %>% unite(SP_P_ID, plot_ID, SP_code, sep = "", remove = FALSE) %>% dplyr::pull(b2, SP_P_ID);
  return(b0[plot_spec] * (1 - exp( -b1[plot_spec] * d))^b2[plot_spec])
}



# ------ 2.3. coordinate functions ---------------------------------------------
# http://www.markusbaumi.ch/schule/formel/azimut.pdf
x_coord <- function(Dcp, AZIcp){ 
  # Xc =  x coordinate of centre = 0 
  # Dcp = Distance between point and centre
  # AZIcp=  Azimute betweeen Point and centre
  Xc <- 2000;
  X = Xc + Dcp * cos(AZIcp);
  return(X)
}

y_coord <- function(Dcp, AZIcp){ 
  # Yc =  y coordinate of centre = 0 
  # Dcp = Distance between point and centre
  # AZIcp=  Azimute betweeen Point and centre
  Yc <- 2000;
  Y = Yc + Dcp * sin(AZIcp);
  return(Y)
}




# ------ 2.4. biomass volume function ------------------------------------------
# ------- 2.4.1. Vondernach - oiB -----------------------------------------------
Vondr_oiB <- function(spec, d, h){  # I´ll use the species groups assigned for the calculation of the heights
  # coefficients for coarsewood
  b0 <- c(fi = 0, ta = 0, dgl = 0, ki = 0, bu = -5.6602, ei = -5.9489, es=0, ah=0); 
  b1 <- c(fi = 0.0157, ta = 0.0074, dgl = 0.0128, ki=0.0169, bu=0.022, ei=0.0257, es=0.0128, ah=0.028 );
  b2 <- c(fi = 1.735, ta = 1.6476, dgl = 1.9541, ki=1.9894, bu=2.0971, ei=2.0738, es=1.9623, ah=2.1304);
  b3 <- c(fi = 1.2177, ta = 1.5543, dgl = 1.0539, ki=0.9378, bu=0.8957, ei=0.8508, es=1.1824, ah=0.7078);
  # coefficients for coarsewood bark
  b4 <- c(fi = 0, ta = 0, dgl = 0, ki = 0, bu = 0, ei =0, es=0, ah=0.5195); 
  b5 <- c(fi = 0.0042, ta = 0.0017, dgl = 0.0027, ki=0.0044, bu=0.0017, ei=0.006, es=0.001, ah=0.004 );
  b6 <- c(fi = 1.6026, ta = 1.304, dgl = 1.8296, ki=1.9594, bu=2.0245, ei=2.0101, es=1, ah=2.068);
  b7 <- c(fi = 1.0239, ta = 1.8956, dgl = 1.0032, ki=0.6641, bu=0.9396, ei=0.778, es=1.6592, ah=0.6965);
  # coefficients for folliage
  b8 <- c(fi = -2.6807, ta = 0, dgl = -2.3439, ki = 0, bu = 0, ei = 0, es=0, ah=0); # 0 for all broadleafed trees
  b9 <- c(fi = 0.4639, ta = 0.0961, dgl = 0.3042, ki=0.0967, bu=0, ei=0, es=0, ah=0 );
  b10 <- c(fi = 2.0861, ta = 1.7136, dgl = 2.4415, ki=2.3781, bu=0, ei=0, es=0, ah=0);
  b11 <- c(fi = -0.9393, ta = 0, dgl = -1.2707, ki=-1.1093, bu=0, ei=0, es=0, ah=0);
  # coefficients for fine wood inkluding bark
  b12 <- c(fi = 0, ta = 0, dgl = 0, ki = 0, bu = -5.6602, ei = -5.9489, es=0, ah=0); # für alle auser buche 0
  b13 <- c(fi = 0.0157, ta = 0.0074, dgl = 0.0128, ki=0.0169, bu=0.022, ei=0.0257, es=0.0128, ah=0.028 );
  b14 <- c(fi = 1.735, ta = 1.6476, dgl = 1.9541, ki=1.9894, bu=2.0971, ei=2.0738, es=1.9623, ah=2.1304);
  b15 <- c(fi = 1.2177, ta = 1.5543, dgl = 1.0539, ki=0.9378, bu=0.8957, ei=0.8508, es=1.1824, ah=0.7078);
  return((b0[spec] + b1[spec]* d^b2[spec] * h^b3[spec]) +            # coarsewood without bark
           (b4[spec] + b5[spec]* d^b6[spec] * h^b7[spec]) +          # bark of coarsewood
           (b8[spec] + b9[spec]* d^b10[spec] * h^b11[spec])+         # foliage (only for coniferous trees) 
           (b12[spec] + b13[spec]* d^b14[spec] * h^b15[spec]))           # fine branches including bark
}


# ------- 2.4.2. Huber (adapted)  -----------------------------------------------
Huber_oiB <- function(spec_part, spec, wood, A, h){
# conversion from plot size (in m2) to one hectare of forest area
  #fa <- 1 / c_A(12.62) * 10000
# species-related percentages of... (Jacobsen et al. 2003)
  cw <- c(fi = 0.679, ki = 0.709, bu = 0.763, ei = 0.689); #  coarsewood + bark
  branch <- c(fi = 0.167 + 0.094, ki = 0.18 + 0.049, bu = 0.175, ei = 0.188) # branches + foliage for coniferous trees
# bole shape coefficient
  SF <- c(fi = 0.5, ta = 0.5, ki = 0.5, dgl = 0.5, lae = 0.5, pa = 0.5, bu = 0.5, ei = 0.5, ah = 0.5, er = 0.5,  alh = 0.5, aln = 0.5);
# density by KOLLMANN 1982 in t/m3 to convert the volume (m3) into dry mass (kg)
  D <- c(fi = 0.3788, ta = 0.3629, dgl = 0.4141, ki = 0.4307, lae = 0.4873, pa = 0.3538, bu = 0.5583, ei = 0.5707, ah = 0.5222, er = 0.4283, alh = 0.5642, aln = 0.4618); 
# correction factor for density of branches (HAKKILA 1972)
  CF <- c(con = 1.3444, difpor = 1.0961, ringpor = 1.0611) 
  return(#fa * ( # conversion to 1 hectare
           (cw[spec_part] * A * h * SF[spec] * D[spec]*1000) + # biomass of coarsewood + bark in kg
           (branch[spec_part] * A * h * SF[spec] * D[spec]*1000 * CF[wood])) # biomass of branches + foliage for coniferous trees in kg
#)
}

# ------- 2.4.3. UBA - total above biomass --------------------------------------
# https://www.umweltbundesamt.de/sites/default/files/medien/1410/publikationen/2020-04-15-climate-change_23-2020_nir_2020_en_0.pdf
## aboveground biomass in kg per tree, for trees DBH > 10cm & DBH < species specific threshold GHGI
# where B = above-ground phytomass in kg per individual tree,
# b0,1,2,3 and k1,2 = coefficients of the эarklund function,
# DBH = Diameter at breast height in cm,
# D03 = Diameter in cm at 30% of tree height,
# H = tree height in m
UBA_aB_DBHa10 <- function(spec, d, d03, h){
  b0 <- c(fi = 0.75285, ki = 0.33778, bu = 0.16787, ei= 0.09428, shw =0.27278);
  b1 <- c(fi = 2.84985, ki = 2.84055 , bu = 6.25452, ei= 10.26998, shw =4.19240);
  b2 <- c(fi = 6.03036, ki = 6.34964, bu = 6.64752, ei= 8.13894, shw = 5.96298);
  b3 <- c(fi = 0.62188, ki = 0.62755, bu = 0.80745, ei= 0.55845, shw = 0.81031);
  k1 <- c(fi = 42.0, ki = 18.0, bu = 11.0, ei= 400.0, shw =13.7);
  k2 <- c(fi = 24.0, ki = 23.0, bu = 135.0, ei= 8.0, shw =66.8);
  return(b0[spec]*exp(b1[spec]*(d/(d+k1[spec])))*exp(b2[spec]*(d03/(d03+k2[spec])))*h^b3[spec])
}
## above ground biomass for trees >1.3m height and < 10cm DBH GHGI
# B_H1.3_DBHb10 = above-ground phytomass in kg per individual tree,
# b0, bs, b3 = coefficients of the function,
# DBH = Diameter at breast height in cm,
# ds = Diameter-validity boundary for this function = 10 cm/
UBA_aB_H1.3_DBHb10 <- function(spec, d){
  b0 <- c(fi = 0.41080, ki = 0.41080, bu = 0.09644 , ei= 0.09644, shw =0.09644);
  bs <- c(fi = 26.63122 , ki = 19.99943 , bu = 33.22328, ei= 28.94782, shw =16.86101);
  b3 <- c(fi = 0.01370, ki = 0.00916, bu = 0.01162, ei= 0.01501, shw = -0.00551);
  ds <- c(fi = 10, ki = 10, bu = 10, ei= 10, shw =10);
  return(b0[spec]+((bs[spec] - b0[spec])/ds[spec]^2 + b3[spec]*(d-ds[spec]))*d^2)
}
## above ground biomass for trees <1.3m GHGI
UBA_aB_Hb1.3 <- function(spec, h){  # here instead of species group i´ll link the formula to a column with he categories broadleafed and coniferous trees
  b0 <- c(NB = 0.23059, LB = 0.04940);
  b1 <- c(NB = 2.20101, LB = 2.54946);
  return(b0[spec]*h^b1[spec])
}

##aboveground biomass for trees <10cm DBH and < species specific threshold
# BAB= BS∗[1+ b1k1/(DBHs+ k1)^2]∗(DBH − DBHs)+b2k2/(D03s+ k2)2∗(D03− D03s)+b3/Hs∗(H − Hs)
# Bs = the biomass at the tree species-specific DBH threshold DBHs --> does that mean the "normal biomass according to Dunger_aB_DBHa10?
# species specific thresholds: 
# Spruce = 	69.0
# Pine = 59.0
# beech = 86.0
# oak = 94.0
# soft hardwoods = 113.0
# DBH1.3s = DHB threshold
# D03s = D03+ c0*DBHs^c1−c0*DBHs^c1
# Hs = H +(a + b / DBHs)^−3−(a + b / DBH)^−3
UBA_aB_DBHath <- function(spec, d, d03, h){
  b0 <- c(fi = 0.75285, ki = 0.33778, bu = 0.16787, ei= 0.09428, shw =0.27278);
  b1 <- c(fi = 2.84985, ki = 2.84055 , bu = 6.25452, ei= 10.26998, shw =4.19240);
  b2 <- c(fi = 6.03036, ki = 6.34964, bu = 6.64752, ei= 8.13894, shw = 5.96298);
  b3 <- c(fi = 0.62188, ki = 0.62755, bu = 0.80745, ei= 0.55845, shw = 0.81031);
  k1 <- c(fi = 42.0, ki = 18.0, bu = 11.0, ei= 400.0, shw =13.7);
  k2 <- c(fi = 24.0, ki = 23.0, bu = 135.0, ei= 8.0, shw =66.8);
  Bs <- b0[spec]*exp(b1[spec]*(d/(d+k1[spec])))*exp(b2[spec]*(d03/(d03+k2[spec])))*h^b3[spec];
  DBHs <- c(fi = 69.0, ki = 59.0, bu = 86.0, ei = 94.0, shw = 113.0);
  c0 <- c(fi = 1.07843, ki = 0.89009, bu = 0.84014, ei = 0.87633, shw = 0.86720);
  c1 <- c(fi = 0.91204, ki = 0.95747, bu = 0.98970, ei = 0.98279, shw = 0.96154);
  d03s <- d03 + c0[spec]*DBHs[spec]^c1[spec] - c0[spec]*DBHs[spec]^c1[spec];
  a <- c(fi = 0.27407, ki = 0.29722, bu = 0.29397, ei = 0.31567, shw = 0.28064);
  b <- c(fi = 2.22031, ki = 1.98688, bu = 1.76894, ei = 1.63335, shw = 2.40288);
  Hs <-  h + ((a[spec] + b[spec] / DBHs[spec])^(-3)) - ((a[spec] + b[spec] / d)^(-3));
  return(Bs*(1 + (b1[spec]*k1[spec]/((DBHs[spec]+ k1[spec])^2))*(d-DBHs[spec]) + 
               (b2[spec]*k2[spec]/(d03s+ k2[spec])^2)*(d03-d03s) + 
               (b3[spec]/Hs)*(h-Hs)))
}


# ------- 2.4.4. TapeS - total above biomass - all -----------------------------
# Kändler, G. and B. Boesch (2012). Methodenentwicklung für die 3. Bundeswaldinventur: Modul 3 
# Überprüfung und Neukonzeption einer Biomassefunktion - Abschlussbericht. Im Auftrag des 
# Bundesministeriums für Ernährung, Landwirtschaft und Verbraucherschutz in Zusammenarbeit 
# mit dem Institut für Waldoekologie und Waldinventur des Johann Heinrich von Thünen-Instituts, FVA-BW: 71.
tapes_aB <- function(spec_tpS, d, dh, h){         
  spp = na.omit(spec_tpS);
  Dm = na.omit(as.list(d));
  Hm = na.omit(as.list(dh));
  Ht = na.omit(h);
  obj.tbio <- tprTrees(spp, Dm, Hm, Ht, inv = 4);
  tapesab <- tprBiomass(obj.tbio, component="agb");
  return(tapesab)
}


# ------- 2.4.5. UBA - total belowground biomass ----------------------------
## belowground phytomass GHGI
UBA_bB <- function(spec, d){
  b0 <- c(fi = 0.003720, ki = 0.006089, bu = 0.018256, ei= 0.028000, shw = 0.000010);#shwr =0.000010, shwrs = 0.000116);
  b1 <- c(fi = 2.792465, ki = 2.739073, bu = 2.321997, ei= 2.440000, shw =2.529000); #shwr =2.529000, shwrs = 2.290300);
  return(ifelse(spec != "shw", b0[spec]*d^b1[spec], (b0[spec]*d^b1[spec])+(0.000116*d^2.290300))) 
}

# ------- 2.4.6. Own Attempt  --------------------------------------------------

#BIOM_gen <- function(spec, d){
#  a <- c(GFI = 0.0004947, GKI = 0.02157, STR = 0.02157, WTA = 5.2193*10^(-4), DGL = 0.111, RBU = 0.0762, SER = 0.0841, MBI = 0.00029, SPA = 0.0519);
#  b <- c(GFI = 2.9487, GKI = 2.221205, STR = 2.221205, WTA = 1.459, DGL = 2.397, RBU = 2.523, SER = 2.4501, MBI = 2.50038, SPA = 2.545); 
#  return(a[spec]*d^b[spec]) 
#}

BIOM_fir <- function(spec, d){ # 5 - DBH in cm, Biomass in t!!! (R2 = ?)
  a <- c(WTA = 5.2193*10^-4);
  b <- c(WTA = 1.459); 
  return(a[spec] * d^b[spec] * 1000) # multiplied by 1000 as conversion from tons into kg
}

BIOM_maple <- function(spec, d){ # 8 - DBH in cm, Biomass in kg (R2 = 0.995)
  a <- c(BAH = -2.7018, SAH = -2.7018);
  b <- c(BAH = 2.5751, SAH = 2.5751); 
  return(a[spec] + b[spec] *log(d)) # log() = ln
}

BIOM_alder <- function(spec, d){ # 19 - DBH in cm, Biomass in kg (R2 = 0.991)
  a <- c(SER = 0.0841);
  b <- c(SER = 2.4501); 
  return(a[spec]*d^b[spec]) 
}

BIOM_birch <- function(spec, d, h){ # 46 - DBH in cm, H in m, Biomass in kg (R2 = 0.994)
  a <- c(MBI = -1.6047);
  b <- c(MBI = 0.9450); 
  return(a[spec] + b[spec] * log(d^2 * h)) # log() = ln
}

BIOM_beech <- function(spec, d, h){ # 86 - DBM in cm, H in m, Biomass in kg (R2 = 0.997)
  a <- c(RBU = -2.872, TKI = -2.872, VBE = -2.872);
  b <- c(RBU = 2.095, TKI = 2.095, VBE = 2.095); 
  c <- c(RBU = 0.678, TKI = 0.678, VBE = 0.678);
  return(a[spec] + b[spec] * log(d) + c[spec] * log(h)) # log() = ln
}

BIOM_larch <- function(spec, d){ # 138 - DBH in cm, Biomass in t!!!) (R2 = ?)
  a <- c(ELA = 0.00564);
  b <- c(ELA = 3.041*10^-5); 
  c <- c(ELA = 2.1058);
  return(a[spec] + b[spec] * d^c[spec] * 1000) # multiplied by 1000 as conversion from tons into kg 
}

BIOM_spruce <- function(spec, d){ # 147 - DBH in cm, Biomass in kg (R2 = 0.995)
  a <- c(GFI = -43.13);
  b <- c(GFI = 2.25); 
  c <- c(GFI = 0.452);
  return(a[spec] + b[spec] * d + c[spec] * d^2) 
}

BIOM_pine <- function(spec, d){ # 463 - DBH in cm, Biomass in g!!! (R2 = 0.999)
  a <- c(GKI = 22.63177, BKI = 22.63177, STR = 22.63177);
  b <- c(GKI = -6.7506, BKI = -6.7506, STR = -6.7506); 
  c <- c(GKI = 49, BKI = 49, STR = 49);
  return(a[spec] * d^2 + b[spec] * (d^2 - c[spec]) / 1000) # divided by 1000 as conversion from gramms into kg
}

BIOM_poplar <- function(spec, d){ # 516 - DBH in mm!!!, Biomass in kg (R2 = 0.959)
  a <- c(SPA = 5.75*10^-4, SWE = 5.75*10^-4);
  b <- c(SPA = 1.873298, SWE = 1.873298); 
  return(a[spec] * (d/1000)^b[spec]) # divided by 1000 as conversion from mm into cm
}
  
BIOM_dougfir <- function(spec, d, h){ # 538 - DBH in cm, H in m, Biomass in kg (R2 = 0.998)
  a <- c(DGL = -2.535);
  b <- c(DGL = 2.009);
  c <- c(DGL = 0.709);
  return(a[spec] + b[spec] * log(d) + c[spec] * log(h)) # log() = ln
}

BIOM_oak <- function(spec, d){ # 598 - DBH in cm, Biomass in kg (R2 = 0.94)
  a <- c(TEI = -1.56, SEI = -1.56, REI = -1.56);
  b <- c(TEI = 2.44, SEI = 2.44, REI = 2.44); 
  return(a[spec] + b[spec] * lg(d)) # lg() = log10
}

# ------ 2.5. forest structure indices -----------------------------------------
# ------- 2.5.1. Forest Structure Index  ----------------------------------------

FSI <- function(variable_index, plot){
  
  return(mean(variable_index[plot]))
}
# ------- 2.5.2. calculation of indices from specific variables  ----------------
Vi <- function(variable){
    return(variable - min(variable)) / (max(variable) - min(variable))
}
# ------- 2.5.3. Variables  ----------------------------------------------------
# -------- 2.5.3.1. DBHq  ------------------------------------------------------
# Quadratic mean diameter at breast height (DBH >= 7 cm)
# Growing stock

# calculate the quadratic mean (root square mean) of DBH grouped by sample plots
#var_DBHq <- function(DBH, plot){
#  return(sqrt(mean(DBH[plot]^2)))
#}

var_DBHq <- function(data) {
  result <- data %>%
    group_by(plot_ID) %>%
    summarise(DBHq = sqrt(mean(DBH_cm)^2))
  return(result)
}

# -------- 2.5.3.2. DBHsd  -----------------------------------------------------

# DBH, standard deviation (DBH >= 7 cm)
# Uneven-ageness

# calculate the standard deviation of DBH grouped by sample plots
#var_DBHsd <- function(DBH, plot){
#  return(sd(DBH[plot]))
#}

var_DBHsd <- function(data) {
  result <- data %>%
    group_by(plot_ID) %>%
    summarise(DBHsd = sd(DBH_cm))
  return(result)
}  
  
# -------- 2.5.3.3. Vol40  -----------------------------------------------------

# Volume / ha of trees (DBH >= 40 cm)
# Volume of large living trees

# calculate the volumne of all trees above a DBH of 40 cm, grouped by sample plots

#var_VOL40 <- function(df){
#  filt_df <- subset(df, DBH_cm >= 40); # filter to only include trees with a DBH above 40cm
#  FZ <- 0.5;
#  sum_vol40 <- sum((((filt_df$DBH_cm / 100) / # conversion of DBH from cm to m
#                    2) ^2 * pi) * # conversion of diameter to radius (d/2), circle area = r^2 * pi
#                    filt_df$h_m * FZ); # Hubers formula: basal area * height * shape coefficient (Formzahl)
#  r_SP <- 12.62;
#  return(sum_vol40[filt_df$plot_ID] / c_A(r_SP) * 10000) # conversion from plot size to the area of a hectare
#}

var_Vol40 <- function(data) {
  FZ <- 0.5;
  r_SP <- 12.62;
  result <- data %>%
    filter(DBH_cm >= 40) %>% # filter to only include trees with a DBH above 40cm
    group_by(plot_ID) %>%
    summarise(VOl40 = sum((((DBH_cm / 100) / # conversion of DBH from cm to m
                             2) ^2 * pi) * # conversion of diameter to radius (d/2), circle area = r^2 * pi
                             H_m * FZ) # Hubers formula: basal area * height * shape coefficient (Formzahl)
                             / c_A(r_SP) * 10000) # conversion from plot size to the area of a hectare
  return(result)
}  

# -------- 2.5.3.4. HEIGHTsd  --------------------------------------------------

# Tree height, standard deviation (DBH >= 7 cm)
# Vertical heterogeneity

# calculate the standard deviation of tree heights grouped by sample plots
#var_Heightsd <- function(h, plot){
#  return(sd(h[plot]))
#}

var_HEIGHTsd <- function(data) {
  result <- data %>%
    group_by(plot_ID) %>%
    summarise(HEIGHTsd = sd(H_m))
  return(result)
}  

# -------- 2.5.3.5. DW downed (DWd)  -------------------------------------------

# Downed deadwood, average mean diameter
# Deadwood downed

# calculate the mean of downed deadwood grouped by sample plots
#var_DWd <- function(df){
#  filt_df <- subset(df, type %in% c(1, 5, 6)); # filter to only include standing deadwood from type 1, 5 or 6
#  mean_DWd <- mean(filt_df$diameter);
#  return(mean_DWd[filt_df$MoMoK_Nr])
#}

var_DWd <- function(data) {
  filt_df <- subset(data, type %in% c(1, 4, 5, 6)); # filter to only include standing deadwood from type 1, 5 or 6
  result <- filt_df %>%
    group_by(MoMoK_Nr) %>%
    summarise(DWd = mean(diameter))
  return(result)
}  

# -------- 2.5.3.6. DW standing (DWs)  -----------------------------------------

# Standing deadwood, mean DBH
# Deadwood standing

# calculate the mean of standing deadwood grouped by sample plots
#var_DWs <- function(df){
#  filt_df <- subset(df, type %in% c(2, 3, 4)); # filter to only include standing deadwood from type 2, 3 or 4
#  mean_DWs <- mean(filt_df$diameter);
#  return(mean_DWs[filt_df$MoMoK_Nr])
#}

var_DWs <- function(data) {
  filt_df <- subset(data, type %in% c(2, 3)); # filter to only include standing deadwood from type 2, 3 or 4
  result <- filt_df %>%
    group_by(MoMoK_Nr) %>%
    summarise(DWs = mean(diameter))
  return(result)
}  

# -------- 2.5.3.7. Decay classes (DC)  ----------------------------------------

# Number of decay classes
# Deadwood decay classes

# calculate the occuring abundance of decay classes on each sample plots
#var_DC <- function(decomp_class, plot){
#  return(length(unique(decomp_class))[plot])
#}

var_DC <- function(data) {
  result <- data %>%
    group_by(plot_ID) %>%
    summarise(DC = length(unique(decomposition)))
  return(result)
}  

# -------- 2.5.3.8. SRreg and SR -----------------------------------------------

# Number of tree species in the regeneration layer
# Tree regeneration diversity

# Number of tree species (DBH >= 7 cm)
# Compositional heterogeneity

## 1. Take number of regeneration species from the regeneration survey of the forest inventory
# calculate the species richness of trees in the regenation sphere (SRreg1)
## 2. Take number of regeneration species from the vegetation survey
# calculate the species richness of trees in the regenation  (SRreg2) and main stand sphere (SR2)
## 3. Take number of regeneration species from the forest inventory
# calculate the species richness of trees in the main stand sphere (SR1)
#var_SR <- function(spec, plot){
#  return(length(unique(spec))[plot])
#}

var_SR <- function(data) {
  result <- data %>%
    group_by(plot_ID) %>%
    summarise(SR = length(unique(tree_spec)))
  return(result)
}  

# ----- 3. CALCULATIONS --------------------------------------------------------
# ------ 3.1. Living trees -----------------------------------------------------

# changing units, etc.
trees_total <- trees_total %>% 
  #filter(H_m %in% c(0, 1)) %>%                                    # filter for correct tree status: -9, -2, -1, 2, 3, 4, 6, 7 have to be excluded from biomass calculation
  mutate(H_m = H_dm/10,                                            # change unit of height into m instead of dm by divinding by 10 
         DBH_h_cm = ifelse(is.na(DBH_h_cm), 130, DBH_h_cm),        # assign DBH measuring height of 130cm when missing
         DBH_h_m = DBH_h_cm/100,                                   # change unit of DBH measuring height from cm into m by dividing by 100  
         # TapeS : diameter at 1.3m tree height to trees_total dataframe
         #https://gitlab.com/vochr/tapes/-/blob/master/vignettes/tapes.rmd
         # H_m_tps = as.numeric(ifelse(is.na(H_dm) | H_dm != "-2", estHeight(d13 = D_mm/10, sp = as.numeric(tpS_ID)), H_dm/10)),
         # DBH_cm_tps = ifelse(is.na(H_dm) & DBH_h_cm != 130 | H_dm != "-2" & DBH_h_cm != 130, 
         # create tprTrees ojekt
         #                     tprDiameter(tprTrees(spp = tpS_ID[is.na(H_dm) & DBH_h_cm != 130 | H_dm != "-2" & DBH_h_cm != 130],               # tapeS Specie code
         #                                          Dm = as.list(D_mm[is.na(H_dm) & DBH_h_cm != 130 | H_dm != "-2" & DBH_h_cm != 130]/10),      # diameter in cm
         #                                          Hm = as.list(DBH_h_cm[is.na(H_dm) & DBH_h_cm != 130 | H_dm != "-2" & DBH_h_cm != 130]/100),      # measuring height diameter
         #                                          Ht = H_m_tps[is.na(H_dm) & DBH_h_cm != 130 | H_dm != "-2" & DBH_h_cm != 130],               # height of the tree --> problematic, because we don´t have it yet, because we calculate if from the DBH
         #                                          inv = 4),                   # use TapeR curves from NFI4
         #                                 Hx = rep(1.3, nrow = trees_total %>% filter(is.na(H_dm) & DBH_h_cm != 130 | H_dm != "-2" & DBH_h_cm != 130)), cp=FALSE),
         #                     D_mm/10),                 # calculate diameter at 1.3 m height via TapeS
         DBH_cm = ifelse(DBH_h_cm != 130, ((DBH_mm*(1.0+(0.0011*(DBH_h_cm-130))))/10), DBH_mm/10),       # calculate diameter at 1.3m height via Regression function of BWI (formula 5.2.1.1.)
         DBH_class = ifelse(is.na(DBH_class), DBH_c_function(DBH_cm), DBH_class),                    # assign diameter class
         BA_m2 = c_A(DBH_cm/2)*0.0001,                                                               # 0.0001 to change unit from cm2 to m2
         CC_no = case_when(DBH_cm >= 7  & DBH_cm < 10 ~ "1", 
                           DBH_cm >= 10 & DBH_cm < 30 ~ "2", 
                           DBH_cm >= 30 ~ "3", 
                           TRUE ~ NA), 
         CC_A_ha = case_when(DBH_cm >= 7  & DBH_cm < 10 ~ c_A(5.64)/10000, 
                             DBH_cm >= 10 & DBH_cm < 30 ~ c_A(12.62)/10000, 
                             DBH_cm >= 30 ~ c_A(17.84)/10000, 
                             TRUE ~ NA))                      

# ------- 3.1.1. heights: regression models for missing tree heights  ----------
# find the plots and species that won´t have a height regression model because 
# there are less then 3 measurements per plot
# ---> think about way to deal with them !!!!
trees_total %>% 
  dplyr::select(plot_ID, SP_code, H_dm, DBH_mm, Kraft) %>% 
  filter(!is.na(H_dm) & !is.na(DBH_mm)) %>% 
  group_by(plot_ID, SP_code) %>% 
  filter(n() < 3)%>%    # filter for plots where there are less then 3 heights measured for each species
  #group_by(plot_ID, SP_code) %>% 
  #lm_table(H_m ~ DBH_cm) %>% 
  arrange(plot_ID, SP_code)

# -------- 3.1.1.1. coefficents dataframe per SP and plot when >= 3 heights measured ----
# to calculate individual tree heights for trees of the same species and plot 
# where the height has not been sampled we create a non-linear regression for the heights
# in the following a dataframe with regression coefficients per 
# species per plot is created if there are more then 3 heights measured per species and plot

# coefficents of non-linear height model per species and plot
# https://rdrr.io/cran/forestmangr/f/vignettes/eq_group_fit_en.Rmd
coeff_H_SP_P <- left_join(trees_total %>% 
                            dplyr::select(plot_ID, SP_code, H_m, DBH_cm, DBH_class) %>% 
                            filter(!is.na(H_m) & !is.na(DBH_cm) & !is.na(DBH_class)) %>% 
                            group_by(plot_ID, SP_code) %>% 
                            filter(n() >= 3),
                          # coeff_H_SP_P dataset 
                          trees_total %>% 
                            dplyr::select(plot_ID, SP_code, H_m, DBH_cm, DBH_class) %>% 
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
  # mutating statistical predictors
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
             N = length(H_m), 
             SSres = sum((H_m-H_est)^2), 
             SStot = sum((H_m-mean_h)^2), 
             pseu_R2 = 1-(SSres/SStot), 
             diff_h = mean(H_m - H_est))

# -------- 3.1.1.2. coefficents dataframe per SP over all plots when >= 3 heights measured ----
# coefficents of non-linear height model per species but over all plots: 
# https://rdrr.io/cran/forestmangr/f/vignettes/eq_group_fit_en.Rmd
#  building separate dataframe for species specific models adding bias, rmse and rsqrd 

coeff_H_SP <- left_join(trees_total %>% 
                          dplyr::select(SP_code, H_m, DBH_cm, DBH_class) %>% 
                          filter(!is.na(H_m) & !is.na(DBH_cm) & !is.na(DBH_class)) %>% 
                          group_by(SP_code) %>% 
                          filter(n() >= 3),
                        trees_total %>% 
                          dplyr::select(SP_code, H_m, DBH_cm, DBH_class) %>% 
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
             N = length(H_m), 
             SSres = sum((H_m-H_est)^2), 
             SStot = sum((H_m-mean_h)^2), 
             pseu_R2 = 1-(SSres/SStot), 
             diff_h = mean(H_m - H_est)) %>% 
  mutate(plot_ID = as.factor('all')) %>% 
  dplyr::select(plot_ID, SP_code, b0, b1, b2, bias, rsme, R2, mean_h, N, SSres, SStot, pseu_R2, diff_h)

# -------- 3.1.1.3. combined coefficients of height models  --------------------
coeff_H_comb <- rbind(coeff_H_SP_P %>% mutate(plot_ID = as.factor(plot_ID)), coeff_H_SP)


# -------- 3.1.1.4. have a look at the quality of the models  ------------------
# from my previous attempts to fit and validate species specific but also total 
# dataset including regression models for the height, I know that actually the 
# DBH class is the better predictor 
# but on the other hand the diameter class is also less precise
summary(coeff_H_SP)
# the R2 is pretty poor for some plots 
coeff_H_SP %>% filter(R2 <= 0.3)
#view(trees_total %>% filter(plot_ID == 32080))

summary(coeff_H_SP_P)
coeff_H_comb %>% filter(diff_h >= 0.75)
#view(coeff_H_SP_P %>% filter(rsqrd<=0.5))


# -------- 3.1.1.5. join coefficients to the tree data set & calculate missing heights  ----
# estimating height by using different functions, depending on the models R2
trees_total_5 <- trees_total %>%
  unite(SP_P_ID, plot_ID, SP_code, sep = "", remove = FALSE) %>%            # create column matching vectorised coefficients of coeff_SP_P (1.3. functions, h_nls_SP_P, dplyr::pull)
  left_join(.,coeff_H_SP_P %>%                                              # joining R2 from coeff_SP_P -> R2.x
              dplyr::select(plot_ID, SP_code, R2) %>% 
              unite(SP_P_ID, plot_ID, SP_code, sep = "", remove = FALSE),   # create column matching vectorised coefficients of coeff_SP_P (1.3. functions, h_nls_SP_P, dplyr::pull)
            by = c("plot_ID", "SP_code", "SP_P_ID")) %>% 
  left_join(., coeff_H_SP %>% 
              dplyr::select(SP_code, R2),                              # joing R2 from coeff_SP data set -> R2.y
            by = "SP_code") %>%       
  left_join(., trees_total %>%                                  # this is creates a tree dataset with mean BHD, d_g, h_g per species per plot per canopy layer wich we need for SLOBODA 
              group_by(plot_ID, C_layer, SP_code) %>%             # group by plot and species and canopy layer to calcualte dg, hg 
              summarise(H_g = sum(mean(na.omit(H_m))*BA_m2)/sum(BA_m2),    # Hoehe des Grundflächemittelstammes, calculation according to S. Schnell
                        mean_DBH_mm = mean(DBH_mm),               # mean diameter per species per canopy layer per plot
                        D_g = ((sqrt(mean(BA_m2)/pi))*2)*100),   # Durchmesser des Grundflächenmittelstammes; *1000 to get from 1m -> 100cm -> 1000mm
            by = c("plot_ID", "SP_code", "C_layer")) %>% 
  mutate(R2_comb = f(R2.x, R2.y, R2.y, R2.x),                               # if R2 is na, put R2 from coeff_SP_P unless R2 from coeff_SP is higher
         H_method = case_when(is.na(H_m) & !is.na(R2.x) & R2.x > 0.70 | is.na(H_m) & R2.x > R2.y & R2.x > 0.70 ~ "coeff_SP_P", 
                              is.na(H_m) & is.na(R2.x) & R2.y > 0.70 | is.na(H_m) & R2.x < R2.y & R2.y > 0.70 ~ "coeff_sp",
                              is.na(H_m) & is.na(R2_comb) & !is.na(H_g)| is.na(H_m) & R2_comb < 0.70 & !is.na(H_g) ~ "ehk_sloboda",
                              is.na(H_m) & is.na(R2_comb) & is.na(H_g)| is.na(H_m) & R2_comb < 0.70 & is.na(H_g) ~ "h_curtis", 
                              TRUE ~ "sampled")) %>% 
  # When h_m is na but there is a plot and species wise model with R2 above 0.7, use the model to predict the height
  mutate(H_m = case_when(is.na(H_m) & !is.na(R2.x) & R2.x > 0.70 | is.na(H_m) & R2.x > R2.y & R2.x > 0.7 ~ h_nls_SP_P(SP_P_ID, DBH_cm),
                         # if H_m is na and there is an R2 from coeff_SP_P thats bigger then 0.7 or if there's no R2 from 
                         # coeff_SP_plot that´s bigger then R2 of coeff_SP_P while the given R2 from coeff_SP_P is above 
                         # 0.75 then use the SP_P models
                         is.na(H_m) & is.na(R2.x) & R2.y > 0.70 | is.na(H_m) & R2.x < R2.y & R2.y > 0.70 ~ h_nls_SP(SP_code, DBH_cm),
                         # when there´s still no model per species or plot, or the R2 of both self-made models is below 0.7 
                         # and H_m is na but there is a h_g and d_G
                         is.na(H_m) & is.na(R2_comb) & !is.na(H_g)| is.na(H_m) & R2_comb < 0.70 & !is.na(H_g) ~ ehk_sloboda(H_SP_group_slo, DBH_mm, mean_DBH_mm, D_g, H_g),
                         # when there´s still no model per species or plot, or the R2 of both self-made models is below 0.7 
                         # and hm is na and the Slobody function cannot eb applied because there is no h_g calculatable use the curtis function
                         is.na(H_m) & is.na(R2_comb) & is.na(H_g)| is.na(H_m) & R2_comb < 0.70 & is.na(H_g) ~ h_curtis(H_SP_group_curt, DBH_mm), 
                         TRUE ~ H_m),
         # this is meant for a plausability check so we can filter for trees with an inplausible height/ diameter ratio
         HD_value = (H_m*100)/DBH_cm) %>% 
  # calcualte mean height per species per plot to correct wrongly/ implausibly estimated heights
  left_join(., trees_total %>%                                 # dataset with mean sampled height per plot and species, to assign it to trees that don´t have a proper estimated height
              filter(!is.na(H_m)) %>%                              # filter for trees with measured height, so trees where H_m != NA
              group_by(plot_ID, SP_code) %>%                       # group by plot and species
              summarise(mean_H_m = mean(H_m)),                     # calculate mean height in m 
            by = c("plot_ID", "SP_code")) %>%                      # join in by plot_ID and SP_code
  # here we correct the estimated height if it happens to be below the height of diameter measurement
  mutate(H_m = case_when(H_method != "sampled" & is.na(H_m) | H_method != "sampled" & H_m < (DBH_h_cm/100) ~ mean_H_m, 
                         TRUE ~ H_m))  

trees_total_5 <- trees_total_5 %>%
  mutate(H_g = case_when(is.nan(H_g) ~ sum(mean(na.omit(H_m))*BA_m2)/sum(BA_m2),
                         TRUE ~ H_g))

# Check on NAs
sum(is.na(trees_total$H_m))
sum(is.na(trees_total_5$H_m))
sum(is.nan(trees_total_5$H_g))
sum(is.na(trees_total_5$DBH_h_cm))
sum(is.na(trees_total_5$DBH_cm))

# Check total amount of plots
length(unique(trees_total_5$plot_ID))

#(trees_NA <- trees_total_5[is.na(trees_total_5$H_m), ])

tree_parameter <- trees_total_5 %>% 
  dplyr::select(plot_ID, loc_name, state, t_ID, SP_nr, SP_code, C_layer, age, DBH_cm, BA_m2, H_m, H_method, R2_comb, D_g, H_g)

tree_parameter$plot_ID <- as.factor(tree_parameter$plot_ID)

tree_parameter <- merge(tree_parameter, 
                        Sites %>% dplyr::select(MoMoK_Nr, hydrology, peatland_type), by.x = "plot_ID", by.y = "MoMoK_Nr")

# ------ 3.2. Biomass volume ---------------------------------------------------
# ------- 3.2.1. TapeS ------------------------------------------------------------
plot_A_ha = c_A(12.62)*0.0001

trees_total_5 %>% dplyr::select(plot_ID, SP_code, t_ID, DBH_h_cm, H_m, H_method) %>% 
  mutate(DBH_h_m = ifelse(is.na(DBH_h_cm), 1.3, DBH_h_cm/100)) %>% 
  filter(DBH_h_m > H_m)

trees_total_5 <-  trees_total_5 %>%
  mutate(plot_A_ha = c_A(12.62)*0.0001) %>%                 # 0.0001 to change unit from m2 to hectare)
  # joining H_o100 in 
  left_join(., trees_total_5 %>%
              group_by(plot_ID, CCS_nr, C_layer, SP_code) %>%     # top 100 are determined per plot, smaöing corcuit, canopy layer and species
              #slice_max(DBH_cm, n = unique(floor(100/(1/trees_total_5$plot_A_ha))), with_ties = FALSE) %>%  # select top 100 representing rows --> 5 per plot basen on dreisatz (actually per sampling circuit)
              summarise(H_g_top = sum(mean(na.omit(H_m))*c_A((DBH_cm/2)/100))/sum(c_A((DBH_cm/2)/100))),    # Hoehe des Grundflächemittelstammes der 100 stärksten Bäume, calculation according to S. Schnell
            by = c("plot_ID", "CCS_nr", "C_layer", "SP_code")) %>% 
  # TapeS : aing diameter at 0.3 tree height to trees_total dataframe
  #https://gitlab.com/vochr/tapes/-/blob/master/vignettes/tapes.rmd
  mutate(DBH_h_m = ifelse(is.na(DBH_h_cm), 1.3, DBH_h_cm/100),
         D_03_cm = tprDiameter(tprTrees(spp = tapes_no, Dm = as.list(DBH_cm), Hm = as.list(DBH_h_m), Ht = H_m, inv = 4), Hx = 1/3*H_m, cp=FALSE),
  ) %>% 
  # biomass
  # aboveground biomass   # for trees above species specific diameter threshold
  mutate(aB_kg_GHG = case_when(Bio_SP_group == "fi" & DBH_cm >= 69.0 |
                                 Bio_SP_group == "ki" & DBH_cm >= 59.0 |
                                 Bio_SP_group == "bu" & DBH_cm >= 86.0 |
                                 Bio_SP_group == "ei" & DBH_cm >= 94.0 |
                                 Bio_SP_group == "shw" & DBH_cm >= 113.0 ~ UBA_aB_DBHath(Bio_SP_group, DBH_cm, D_03_cm, H_m), 
                               # trees >10cm DHB below species specific DBH threshold
                               Bio_SP_group == "fi" & DBH_cm >= 10 & DBH_cm < 69.0 |
                                 Bio_SP_group == "ki" & DBH_cm >= 10 & DBH_cm < 59.0 |
                                 Bio_SP_group == "bu" & DBH_cm >= 10 & DBH_cm < 86.0 |
                                 Bio_SP_group == "ei" & DBH_cm >= 10 & DBH_cm < 94.0 |
                                 Bio_SP_group == "shw" & DBH_cm >= 10 & DBH_cm < 113.0 ~ UBA_aB_DBHa10(Bio_SP_group, DBH_cm, D_03_cm, H_m), 
                               # trees < 10cm DBH & H < 1.3m 
                               DBH_cm < 10 & H_m >= 1.3 ~ UBA_aB_H1.3_DBHb10(Bio_SP_group, DBH_cm), 
                               H_m <= 1.3 ~ UBA_aB_Hb1.3(LH_NH, DBH_cm)),
         # belowground biomass
         bg = UBA_bB(Bio_SP_group, DBH_cm)) %>% 
  #  # compartiments TapeS
  mutate(ag = tapes_aB(tapes_no, DBH_cm, DBH_h_m, H_m),                        # total aboveground biomass
         f = ifelse(LH_NH == "NB", tapes_fB(tapes_no, DBH_cm, DBH_h_m, H_m),  # foliage conifers 
                    Wutzler_fB_L1(DBH_cm, H_m)),                            # foliage broadleaves
         fw = tapes_brB(tapes_no, DBH_cm, DBH_h_m, H_m),                      # Nichtderbholz, finebranches
         sw = tapes_swB(tapes_no, DBH_cm, DBH_h_m, H_m),                      #coarsewood without bark, Derbholz                    
         swb = tapes_swbB(tapes_no, DBH_cm, DBH_h_m, H_m),
         stw = tapes_stwB(tapes_no, DBH_cm, DBH_h_m, H_m),                    # stump wood 
         stwb = tapes_stwbB(tapes_no, DBH_cm, DBH_h_m, H_m),                  # stumbwood bark
         total = ag + bg) 

# ------- 3.2.2. Vonderach / Huber ----------------------------------------------
trees_total_5 <- trees_total_5 %>% 
  mutate(AGB_kg = Vondr_oiB(H_SP_group_bm1, DBH_cm, H_m)) %>% # calculate above-ground biomass by Vondernach
  # recalculate above-ground biomass of trees by Huber's equation for tree biomass which resulted in a negative value by Vondernach
  mutate(AGB_method = case_when(AGB_kg <= 0 ~ "Huber",
                                AGB_kg > 0 ~ "Vondernach")) %>% 
  mutate(AGB_kg = case_when(AGB_kg <= 0 ~ Huber_oiB(H_SP_group_bm1, H_SP_group_bm2, H_SP_group_wood, BA_m2, H_m),
                            AGB_kg > 0 ~ AGB_kg)) %>% 
  mutate()
# ------- 3.2.3. Own Attempt  ---------------------------------------------------
trees_total_5 <-  trees_total_5 %>%
  mutate(biom = case_when(SP_code == "SER" ~ BIOM_alder(SP_code, DBH_cm),
                          SP_code %in% c("RBU", "TKI", "VBE") ~ BIOM_beech(SP_code, DBH_cm, H_m),
                          SP_code == "MBI" ~ BIOM_birch(SP_code, DBH_cm, H_m),
                          SP_code == "DGL" ~ BIOM_dougfir(SP_code, DBH_cm, H_m),
                          SP_code == "WTA" ~ BIOM_fir(SP_code, DBH_cm),
                          SP_code == "ELA" ~ BIOM_larch(SP_code, DBH_cm),
                          SP_code %in% c("SAH", "BAH") ~ BIOM_maple(SP_code, DBH_cm),
                          SP_code %in% c("TEI", "SEI", "REI") ~ BIOM_oak(SP_code, DBH_cm),
                          SP_code %in% c("GKI", "BKI", "STR") ~ BIOM_pine(SP_code, DBH_cm),
                          SP_code %in% c("SPA", "SWE") ~ BIOM_poplar(SP_code, DBH_cm),
                          SP_code == "GFI" ~ BIOM_spruce(SP_code, DBH_cm)),
         biom_meth = case_when(SP_code == "SER" ~ "BIOM_alder",
                               SP_code %in% c("RBU", "TKI", "VBE") ~ "BIOM_beech",
                               SP_code == "MBI" ~ "BIOM_birch",
                               SP_code == "DGL" ~ "BIOM_dougfir",
                               SP_code == "WTA" ~ "BIOM_fir",
                               SP_code == "ELA" ~ "BIOM_larch",
                               SP_code %in% c("SAH", "BAH") ~ "BIOM_maple",
                               SP_code %in% c("TEI", "SEI", "REI") ~ "BIOM_oak",
                               SP_code %in% c("GKI", "BKI", "STR") ~ "BIOM_pine",
                               SP_code %in% c("SPA", "SWE") ~ "BIOM_poplar",
                               SP_code == "GFI" ~ "BIOM_spruce"))

trees_total_5 <-  trees_total_5 %>%
  mutate(biom = case_when(SP_code == "SER" ~ 0.0841 * DBH_cm^2.4501,
                          SP_code %in% c("RBU", "TKI", "VBE") ~ -2.872 + 2.095 * log(DBH_cm) + 0.678 * log(H_m),
                          SP_code == "MBI" ~ -1.6047 + 0.9450 * log(DBH_cm^2 * H_m),
                          SP_code == "DGL" ~ -2.535 + 2.009 * log(DBH_cm) + 0.709 * log(H_m),
                          SP_code == "WTA" ~ 5.2193*10^-4 * DBH_cm^1.459 * 1000,
                          SP_code == "ELA" ~ 0.00564 + 3.041*10^-5 * DBH_cm^2.1058 * 1000,
                          SP_code %in% c("SAH", "BAH") ~ -2.7018 + 2.5751 *log(DBH_cm),
                          SP_code %in% c("TEI", "SEI", "REI") ~ -1.56 + 2.44 * lg(DBH_cm),
                          SP_code %in% c("GKI", "BKI", "STR") ~ 22.63177 * DBH_cm^2 + -6.7506 * (DBH_cm^2 - 49) / 1000,
                          SP_code %in% c("SPA", "SWE") ~ 5.75*10^-4 * (DBH_cm/1000)^1.873298,
                          SP_code == "GFI" ~ -43.13 + 2.25 * DBH_cm + 0.452 * DBH_cm^2),
         biom_meth = case_when(SP_code == "SER" ~ "BIOM_alder",
                               SP_code %in% c("RBU", "TKI", "VBE") ~ "BIOM_beech",
                               SP_code == "MBI" ~ "BIOM_birch",
                               SP_code == "DGL" ~ "BIOM_dougfir",
                               SP_code == "WTA" ~ "BIOM_fir",
                               SP_code == "ELA" ~ "BIOM_larch",
                               SP_code %in% c("SAH", "BAH") ~ "BIOM_maple",
                               SP_code %in% c("TEI", "SEI", "REI") ~ "BIOM_oak",
                               SP_code %in% c("GKI", "BKI", "STR") ~ "BIOM_pine",
                               SP_code %in% c("SPA", "SWE") ~ "BIOM_poplar",
                               SP_code == "GFI" ~ "BIOM_spruce"))

# ------ 3.3 Forest Structure Indices ------------------------------------------
# ------- 3.3.1. Variable indices -------------------------------------------------

#FS_indices <- 
#    data.frame(Sites$MoMoK_Nr, Sites$name, Sites$peatland_type, Sites$hydrology, Sites$tree_spec) %>% 
#    mutate(DBHq <- var_DBHq(trees_total_5),
#           DBHsd <- var_DBHsd(trees_total_5),
#           Vol40 <- var_Vol40(trees_total_5),
#           HEIGHTsd <- var_HEIGHTsd(trees_total_5),
#           DWd <- var_DWd(DW),
#           DWs <- var_DWs(DW),
#           DC <- var_DC(DW),
#           SRreg <- var_SRreg(Reg),
#           SR <- var_SR(trees_total_5))

# ------- 3.3.2. Forest Structural Index ------------------------------------------


# ----- 4. FOREST GROWTH -------------------------------------------------------
# ------ 4.1 Forest Growth Tables ----------------------------------------------
# ------- 4.1.1 Alnus glutinosa ------------------------------------------------

# source: Lockow (1994) / LFE 2004

Alnus <- data.frame(age = seq(15, 110, 5), # Hg in meters
                    EKL0.5 = c(13.9, 17.3, 20, 22.3, 24.2, 25.7, 27, 28.2, 29.1, 30, 30.7, 31.3, 31.8, 32.2, 32.6, 32.9, 33.2, 33.4, 33.6, 33.8),
                    EKL0.75 = c(13.4, 16.7, 19.4, 21.6, 23.4, 24.9, 26.2, 27.3, 28.2, 29, 29.7, 30.3, 30.8, 31.2, 31.6, 31.9, 32.2, 32.4, 32.6, 32.7),
                    EKL1 = c(13, 16.2, 18.8, 20.9, 22.7, 24.1, 25.4, 26.4, 27.4, 28.1, 28.8, 29.3, 29.8, 30.3, 30.6, 30.9, 31.2, 31.4, 31.6, 31.7),
                    EKL1.25 = c(12.6, 15.7, 18.2, 20.2, 21.9, 23.4, 24.6, 25.6, 26.5, 27.2, 27.8, 28.4, 28.9, 29.3, 29.6, 29.9, 30.2, 30.4, 30.5, 30.7),
                    EKL1.5 = c(12.1, 15.1, 17.5, 19.5, 21.2, 22.6, 23.7, 24.7, 25.6, 26.3, 26.9, 27.4, 27.9, 28.3, 28.6, 28.9, 29.1, 29.3, 29.5, 29.7),
                    EKL1.75 = c(11.7, 14.6, 16.9, 18.8, 20.4, 21.8, 22.9, 23.9, 24.7, 25.4, 26, 26.5, 26.9, 27.3, 27.6, 27.9, 28.1, 28.3, 28.5, 28.6),
                    EKL2 = c(11.2, 14, 16.3, 18.2, 19.7, 21, 22.1, 23, 23.8, 24.5, 25, 25.5, 26, 26.3, 26.6, 26.9, 27.1, 27.3, 27.5, 27.6),
                    EKL2.25 = c(10.8, 13.5, 15.7, 17.5, 18.9, 20.2, 21.2, 22.1, 22.9, 23.6, 24.1, 24.6, 25, 25.3, 25.6, 25.9, 26.1, 26.3, 26.5, 26.6),
                    EKL2.5 = c(10.3, 12.9, 15, 16.8, 18.2, 19.4, 20.4, 21.3, 22, 22.6, 23.2, 23.6, 24, 24.4, 24.7, 24.9, 25.1, 25.3, 25.4, 25.5),
                    EKL2.75 = c(9.9, 12.4, 14.4, 16.1, 17.5, 18.6, 19.6, 20.4, 21.1, 21.7, 22.2, 22.7, 23.1, 23.4,23.7, 23.9, 24.1, 24.3, 24.4, 24.5),
                    EKL3 = c(9.5, 11.9, 13.8, 15.4, 16.7, 17.8, 18.8, 19.6, 20.2, 20.8, 21.3, 21.7, 22.1, 22.4, 22.7, 22.9, 23.1, 23.3, 23.4, 23.5),
                    EKL3.25 = c(9, 11.3, 13.2, 14.7, 16, 17, 17.9, 18.7, 19.3, 19.9, 20.4, 20.8, 21.1, 21.4, 21.7, 21.9, 22.1, 22.2, 22.4, 22.5),
                    EKL3.5 = c(8.6, 10.8, 12.6, 14, 15.2, 16.2, 17.1, 17.8, 18.5, 19, 19.4, 19.8, 20.2, 20.4, 20.7, 20.9, 21.1, 21.2, 21.3, 21.4))

Alnus_long <- Alnus %>% 
  pivot_longer(cols = starts_with("EKL"), names_to = "yield_class",
               values_to = "H_g")


# Grouped by plot ID
(Alnus_plot <- 
    ggplot() +
    geom_line(data = Alnus_long, aes(x = age, y = H_g,
                                     #linewidth = yield_class, 
                                     linetype = yield_class)) +
    scale_linetype_manual(values = c("dashed", "dashed", "solid", 
                                     "dashed", "dashed", "dashed", "solid",
                                     "dashed", "dashed", "dashed", "solid",
                                     "dashed", "dashed"), guide = "none") +
    geom_point(data = tree_parameter %>% 
                 filter(SP_code == "SER") %>%  
                 filter(!is.na(age)) %>% 
                 filter(age >= 15), 
               aes(x = age, y = H_g, col = plot_ID,
               ), stroke = 1.1, size = 2.5) +
    #scale_color_manual(name = "Plots", values = c("#8A5F18", "#DC9C34", "#EAC486", 
    #                                              "#92D050", "#538135", "#355222")) + # need two more
    geom_dl(data = Alnus_long, aes(x = age, y = H_g, #width = yield_class,
                                   label = yield_class), 
            method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8)) + 
    xlab("Age") +
    ylab("Hg") +
    ggtitle("Alnus glutinosa - plots") +
    #scale_y_continuous(breaks=c(10, 15, 20, 25, 30, 35), limits = c(7, 35)) +
    scale_x_continuous(breaks=c(25, 50, 75, 100), limits = c(10, 120)) +
    theme(axis.title.x = element_text(size = 14)) +
    theme(axis.text.x = element_text(size = 12)) +
    theme(axis.title.y = element_text(size = 14)) +
    theme(axis.text.y = element_text(size = 12)) +
    theme(panel.background = NULL))
ggsave("Images/Bon_Alnus_plot.png", dpi = 300)


# Grouped by hydrology and peatland type
(Alnus_hydrology <- 
    ggplot() +
    geom_line(data = Alnus_long, aes(x = age, y = H_g,
                                     #color = yield_class, 
                                     linetype = yield_class)) +
    scale_linetype_manual(values = c("dashed", "dashed", "solid", 
                                     "dashed", "dashed", "dashed", "solid",
                                     "dashed", "dashed", "dashed", "solid",
                                     "dashed", "dashed"), guide = "none") +
    #geom_dl(aes(label = Alnus_long$yield_class), method = list("last.points")) +
    geom_point(data = tree_parameter %>% 
                 filter(SP_code == "SER") %>%  
                 filter(!is.na(age)) %>% 
                 filter(age >= 15),  
               aes(x = age, y = H_g, col = hydrology, shape = peatland_type), 
               stroke = 1.1, size = 3) +
    #scale_shape_manual(values=c(1, 16)) +                                             # adapt number of shapes according to shown entities !!!!!
    scale_shape_manual(name = "Peatland type", values = c(15)) +
    scale_color_manual(name = "Hydrology", values = c("#936241", "#387286")) +
    geom_dl(data = Alnus_long, aes(x = age, y = H_g, #width = yield_class,
                                   label = yield_class), 
            method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.9)) + 
    guides(color = guide_legend(order = 1), 
           shape = guide_legend(order = 2)) +
    xlab("Age [years)") +
    ylab("Hg [m]") +
    #scale_y_continuous(breaks=c(10, 15, 20, 25, 30, 35), limits = c(8, 35)) +
    scale_x_continuous(breaks=c(25, 50, 75, 100), limits = c(10, 120)) +
    theme(axis.title.x = element_text(size = 16)) +
    theme(axis.text.x = element_text(size = 14)) +
    theme(axis.title.y = element_text(size = 16)) +
    theme(axis.text.y = element_text(size = 14)) +
    theme(legend.text = element_text(size = 13)) + 
    theme(legend.title = element_text(size = 15)) + 
    theme(panel.background = NULL))
ggsave("Images/Bon_Alnus_hydr_peatland.png", dpi = 300)


# ------- 4.1.2 Betula pubescens -----------------------------------------------

# source: Lockow (1996) / LFE 2004

Betula_pub <- data.frame(age = seq(30, 110, 5), # Hg in meters
                         EKL0 = c(14.1, 17.2, 19.7, 21.7, 23.2, 24.4, 25.2, 25.9, 26.3, 26.6, 26.8, 27, 27.1, 27.2, 27.3, 27.3, 27.3),
                         EKL0.25 = c(13.6, 16.5, 19, 20.9, 22.4, 23.5, 24.3, 24.9, 25.3, 25.6, 25.8, 26, 26.1, 26.2, 26.2, 26.3, 26.3),
                         EKL0.5 = c(13, 15.8, 18.2, 20, 21.5, 22.5, 23.3, 23.9, 24.3, 24.6, 24.8, 25, 25.1, 25.2, 25.2, 25.3, 25.3),
                         EKL0.75 = c(12.4, 15.2, 17.4, 19.2, 20.6, 21.6, 22.4, 22.9, 23.3, 23.6, 23.8, 24, 24.1, 24.1, 24.2, 24.2, 24.2),
                         EKL1 = c(11.9, 14.5, 16.7, 18.4, 19.7, 20.7, 21.4, 21.9, 22.3, 22.6, 22.8, 22.9, 23, 23.1, 23.2, 23.2, 23.2),
                         EKL1.25 = c(11.3, 13.8, 15.9, 17.6, 18.8, 19.8, 20.4, 21, 21.3, 21.6, 21.8, 21.9, 22, 22.1, 22.1, 22.2, 22.2),
                         EKL1.5 = c(10.8, 13.2, 15.2, 16.7, 17.9, 18.8, 19.5, 20, 20.3, 20.6, 20.8, 20.9, 21, 21, 21.1, 21.1, 21.2),
                         EKL1.75 = c(10.2, 12.5, 14.4, 15.9, 17, 17.9, 18.5, 19, 19.3, 19.6, 19.7, 19.9, 20, 20, 20.1, 20.1, 20.1),
                         EKL2 = c(9.6, 11.8, 13.6, 15.1, 16.2, 17, 17.6, 18, 18.3, 18.6, 18.7, 18.9, 18.9, 19, 19, 19.1, 19.1),
                         EKL2.25 = c(9.1, 11.2, 12.9, 14.2, 15.3, 16, 16.6, 17, 17.3, 17.6, 17.7, 17.8, 17.9, 18, 18, 18, 18.1),
                         EKL2.5 = c(8.5, 10.5, 12.1, 13.4, 14.4, 15.1, 15.7, 16.1, 16.4, 16.6, 16.7, 16.8, 16.9, 16.9, 17, 17, 17),
                         EKL2.75 = c(8, 9.8, 11.4, 12.6, 13.5, 14.2, 14.7, 15.1, 15.4, 15.6, 15.7, 15.8, 15.9, 15.9, 15.9, 16, 16),
                         EKL3 = c(7.4, 9.2, 10.6, 11.7, 12.6, 13.3, 13.8, 14.1, 14.4, 14.5, 14.7, 14.8, 14.8, 14.9, 14.9, 14.9, 15),
                         EKL3.25 = c(6.8, 8.5, 9.8, 10.9, 11.7, 12.3, 12.8, 13.1, 13.4, 13.5, 13.7, 13.7, 13.8, 13.9, 13.9, 13.9, 13.9),
                         EKL3.5 = c(NA, 7.8, 9.1, 10.1, 10.8, 11.4, 11.8, 12.1, 12.4, 12.5, 12.6, 12.7, 12.8, 12.8, 12.9, 12.9, 12.9))

Betula_pub_long <- Betula_pub %>% 
  pivot_longer(cols = starts_with("EKL"), names_to = "yield_class",
               values_to = "H_g")


# Grouped by plot ID
(Betula_pub_plot <- 
    ggplot() +
    geom_line(data = Betula_pub_long, aes(x = age, y = H_g,
                                          #color = yield_class, 
                                          linetype = yield_class)) +
    scale_linetype_manual(values = c("solid", "dashed", "dashed", "dashed",
                                     "solid", "dashed", "dashed", "dashed",
                                     "solid", "dashed", "dashed", "dashed",
                                     "solid", "dashed", "dashed"), guide = "none") +
    #geom_dl(aes(label = Betula_pub_long$yield_class), method = list("last.points")) +
    geom_point(data = tree_parameter %>% 
                 filter(SP_code == "MBI") %>%  
                 filter(!is.na(age)) %>% 
                 filter(age >= 30), 
               aes(x = age, y = H_g, col = plot_ID), stroke = 1.1, size = 2.5) +
    #scale_shape_manual(values=c(0, 1, 2)) +                                           # adapt number of shapes according to shown entities !!!!!
    scale_color_manual(name = "Plots", values = c("#8A5F18", "#EAC486","#538135")) +
    geom_dl(data = Betula_pub_long, aes(x = age, y = H_g, #width = yield_class,
                                        label = yield_class), 
            method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8)) + 
    xlab("Age") +
    ylab("Hg") +
    ggtitle("Betula pubescens - plot") +
    #scale_y_continuous(breaks=c(10, 15, 20, 25, 30, 35), limits = c(8, 35)) +
    scale_x_continuous(breaks=c(25, 50, 75, 100), limits = c(25, 117)) +
    theme(axis.title.x = element_text(size = 14)) +
    theme(axis.text.x = element_text(size = 12)) +
    theme(axis.title.y = element_text(size = 14)) +
    theme(axis.text.y = element_text(size = 12)) +
    theme(panel.background = NULL))
ggsave("Images/Bon_Betula_pubescens_plot.png", dpi = 300)


# Grouped by hydrology and peatland type
(Betula_pub_hydrology <- 
    ggplot() +
    geom_line(data = Betula_pub_long, aes(x = age, y = H_g,
                                          #color = yield_class, 
                                          linetype = yield_class)) +
    scale_linetype_manual(values = c("solid", "dashed", "dashed", "dashed",
                                     "solid", "dashed", "dashed", "dashed",
                                     "solid", "dashed", "dashed", "dashed",
                                     "solid", "dashed", "dashed"), guide = "none") +
    #geom_dl(aes(label = Betula_pub_long$yield_class), method = list("last.points")) +
    geom_point(data = tree_parameter %>% 
                 filter(SP_code == "MBI") %>%  
                 filter(!is.na(age)) %>% 
                 filter(age >= 30),  
               aes(x = age, y = H_g, col = hydrology, shape = peatland_type), 
               stroke = 1.1, size = 3) +
    #scale_shape_manual(values=c(1, 16)) +                                             # adapt number of shapes according to shown entities !!!!!
    scale_shape_manual(name = "Peatland type", values = c(15, 17)) +
    scale_color_manual(name = "Hydrology", values = c("#936241", "#387286")) +
    geom_dl(data = Betula_pub_long, aes(x = age, y = H_g, #width = yield_class,
                                        label = yield_class), 
            method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8)) +
    guides(color = guide_legend(order = 1),
           shape = guide_legend(order = 2)) +
    xlab("Age [years]") +
    ylab("Hg [m]") +
    #scale_y_continuous(breaks=c(10, 15, 20, 25, 30, 35), limits = c(8, 35)) +
    scale_x_continuous(breaks=c(25, 50, 75, 100), limits = c(25, 117)) +
    theme(axis.title.x = element_text(size = 16)) +
    theme(axis.text.x = element_text(size = 14)) +
    theme(axis.title.y = element_text(size = 16)) +
    theme(axis.text.y = element_text(size = 14)) +
    theme(legend.text = element_text(size = 13)) + 
    theme(legend.title = element_text(size = 15)) + 
    theme(panel.background = NULL))
ggsave("Images/Bon_Betula_pubescens_hydr_peatland.png", dpi = 300)


# ------- 4.1.3 Betula pendula -------------------------------------------------

# source: Lockow (1996) / LFE 2004

Betula_pen <- data.frame(age = seq(15, 110, 5), # Hg in meters
                         EKL0.5 = c(11.9, 15.6, 18.9, 21.9, 24.4, 26.5, 28.1, 29.4, 30.4, 31.2, 31.8, 32.2, 32.5, 32.8, 33, 33.1, 33.3, 33.3, 33.4, 33.4),
                         EKL0.75 = c(11.5, 15.1, 18.4, 21.2, 23.7, 25.6, 27.2, 28.5, 29.5, 30.2, 30.8, 31.2, 31.6, 31.8, 32, 32.1, 32.2, 32.3, 32.4, 32.4),
                         EKL1 = c(11.1, 14.6, 17.8, 20.5, 22.9, 24.8, 26.4, 27.6, 28.5, 29.3, 29.8, 30.2, 30.6, 30.8, 31, 31.1, 31.2, 31.3, 31.4, 31.4),
                         EKL1.25 = c(10.7, 14.1, 17.2, 19.9, 22.1, 24, 25.5, 26.7, 27.6, 28.3, 28.8, 29.2, 29.6, 29.8, 30, 30.1, 30.2, 30.3, 30.3, 30.4),
                         EKL1.5 = c(10.4, 13.6, 16.6, 19.2, 21.4, 23.2, 24.6, 25.8, 26.7, 27.3, 27.9, 28.3, 28.6, 28.8, 29, 29.1, 29.2, 29.3, 29.3, 29.4),
                         EKL1.75 = c(10, 13.1, 16, 18.5, 20.6, 22.4, 23.8, 24.9, 25.7, 26.4, 26.9, 27.3, 27.6, 27.8, 27.9, 28.1, 28.2, 28.2, 28.3, 28.3),
                         EKL2 = c(9.6, 12.6, 15.4, 17.8, 19.9, 21.6, 22.9, 24, 24.8, 25.4, 25.9, 26.3, 26.6, 26.8, 26.9, 27.1, 27.1, 27.2, 27.3, 27.3),
                         EKL2.25 = c(9.2, 12.1, 14.8, 17.1, 19.1, 20.7, 22, 23.1, 23.9, 24.5, 24.9, 25.3, 25.6, 25.8, 25.9, 26, 26.1, 26.2, 26.2, 26.3),
                         EKL2.5 = c(8.8, 11.6, 14.2, 16.4, 18.4, 19.9, 21.2, 22.2, 22.9, 23.5, 24, 24.3, 24.6, 24.8, 24.9, 25, 25.1, 25.2, 25.2, 25.3),
                         EKL2.75 = c(8.4, 11.1, 13.6, 15.8, 17.6, 19.1, 20.3, 21.3, 22, 22.6, 23, 23.3, 23.6, 23.8, 23.9, 24, 24.1, 24.2, 24.2, 24.2),
                         EKL3 = c(8, 10.6, 13, 15.1, 16.8, 18.3, 19.4, 20.3, 21.1, 21.6, 22, 22.3, 22.6, 22.8, 22.9, 23, 23.1, 23.1, 23.2, 23.2),
                         EKL3.25 = c(7.6, 10.1, 12.4, 14.4, 16.1, 17.5, 18.6, 19.4, 20.1, 20.6, 21, 21.3, 21.6, 21.8, 21.9, 22, 22.1, 22.1, 22.2, 22.2),
                         EKL3.5 = c(7.2, 9.6, 11.8, 13.7, 15.3, 16.6, 17.7, 18.5, 19.2, 19.7, 20.1, 20.4, 20.6, 20.7, 20.9, 21, 21, 21.1, 21.1, 21.2),
                         EKL3.75 = c(6.8, 9.1, 11.2, 13, 14.6, 15.8, 16.8, 17.6, 18.3, 18.7, 19.1, 19.4, 19.6, 19.7, 19.9, 20, 20, 20.1, 20.1, 20.1),
                         EKL4 = c(6.5, 8.6, 10.6, 12.3, 13.8, 15, 16, 16.7, 17.3, 17.8, 18.1, 18.4, 18.6, 18.7, 18.9, 18.9, 19, 19.1, 19.1, 19.1),
                         EKL4.25 = c(6.1, 8.1, 10, 11.6, 13, 14.2, 15.1, 15.8, 16.4, 16.8, 17.1, 17.4, 17.6, 17.7, 17.8, 17.9, 18, 18, 18.1, 18.1),
                         EKL4.5 = c(5.7, 7.6, 9.4, 11, 12.3, 13.4, 14.2, 14.9, 15.5, 15.9, 16.2, 16.4, 16.6, 16.7, 16.8, 16.9, 17, 17, 17, 17.1),
                         EKL4.75 = c(5.3, 7.1, 8.8, 10.3, 11.5, 12.6, 13.4, 14, 14.5, 14.9, 15.2, 15.4, 15.6, 15.7, 15.8, 15.9, 16, 16, 16, 16),
                         EKL5 = c(4.9, 6.6, 8.2, 9.6, 10.8, 11.7, 12.5, 13.1, 13.6, 13.9, 14.2, 14.4, 14.6, 14.7, 14.8, 14.9, 14.9, 15, 15, 15))

Betula_pen_long <- Betula_pen %>% 
  pivot_longer(cols = starts_with("EKL"), names_to = "yield_class",
               values_to = "H_g")


# Grouped by plot ID
(Betula_pen_plot <- 
    ggplot() +
    geom_line(data = Betula_pen_long, aes(x = age, y = H_g,
                                          #color = yield_class, 
                                          linetype = yield_class)) +
    scale_linetype_manual(values = c("dashed", "dashed",
                                     "solid", "dashed", "dashed", "dashed",
                                     "solid", "dashed", "dashed", "dashed",
                                     "solid", "dashed", "dashed", "dashed",
                                     "solid", "dashed", "dashed", "dashed",
                                     "solid"), guide = "none") +
    #geom_dl(aes(label = Betula_pen_long$yield_class), method = list("last.points")) +
    geom_point(data = tree_parameter %>% 
                 filter(SP_code == "MBI") %>%  
                 filter(!is.na(age)) %>% 
                 filter(age >= 15),  
               aes(x = age, y = H_g, col = plot_ID), stroke = 1.1, size = 2.5) +
    #scale_shape_manual(values=c(0, 1, 2)) +                                           # adapt number of shapes according to shown entities !!!!!
    scale_color_manual(name = "Plots", values = c("#8A5F18", "#EAC486","#538135")) +
    geom_dl(data = Betula_pen_long, aes(x = age, y = H_g, #width = yield_class,
                                        label = yield_class), 
            method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8)) + 
    xlab("Age") +
    ylab("Hg") +
    ggtitle("Betula pendula - plot") +
    #scale_y_continuous(breaks=c(10, 15, 20, 25, 30, 35), limits = c(8, 35)) +
    scale_x_continuous(breaks=c(25, 50, 75, 100), limits = c(12, 117)) +
    theme(axis.title.x = element_text(size = 14)) +
    theme(axis.text.x = element_text(size = 12)) +
    theme(axis.title.y = element_text(size = 14)) +
    theme(axis.text.y = element_text(size = 12)) +
    theme(panel.background = NULL))
ggsave("Images/Bon_Betula_pendula_plot.png", dpi = 300)


# Grouped by hydrology and peatland type
(Betula_pen_hydrology <- 
    ggplot() +
    geom_line(data = Betula_pen_long, aes(x = age, y = H_g,
                                          #color = yield_class, 
                                          linetype = yield_class)) +
    scale_linetype_manual(values = c("dashed", "dashed",
                                     "solid", "dashed", "dashed", "dashed",
                                     "solid", "dashed", "dashed", "dashed",
                                     "solid", "dashed", "dashed", "dashed",
                                     "solid", "dashed", "dashed", "dashed",
                                     "solid"), guide = "none") +
    #geom_dl(aes(label = Betula_pen_long$yield_class), method = list("last.points")) +
    geom_point(data = tree_parameter %>% 
                 filter(SP_code == "MBI") %>%  
                 filter(!is.na(age)) %>% 
                 filter(age >= 15),  
               aes(x = age, y = H_g, col = hydrology, shape = peatland_type), 
               stroke = 1.1, size = 3) +                                          # adapt number of shapes according to shown entities !!!!!
    scale_shape_manual(name = "Peatland type", values = c(15, 17)) +
    scale_color_manual(name = "Hydrology", values = c("#936241", "#387286")) +
    geom_dl(data = Betula_pen_long, aes(x = age, y = H_g, #width = yield_class,
                                        label = yield_class), 
            method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8)) +
    guides(color = guide_legend(order = 1), 
           shape = guide_legend(order = 2)) +
    xlab("Age [years]") +
    ylab("Hg [m]") +
    scale_x_continuous(breaks=c(25, 50, 75, 100), limits = c(12, 117)) +
    theme(axis.title.x = element_text(size = 16)) +
    theme(axis.text.x = element_text(size = 14)) +
    theme(axis.title.y = element_text(size = 16)) +
    theme(axis.text.y = element_text(size = 14)) +
    theme(legend.text = element_text(size = 13)) + 
    theme(legend.title = element_text(size = 15)) + 
    theme(panel.background = NULL))
ggsave("Images/Bon_Betula_pendula_hydr_peatland.png", dpi = 300)

# -------- 4.1.3.1 Comparison  -------------------------------------------------
# Betula pubescence
(Betula_pub_hydrology_comp <- 
   ggplot() +
   geom_line(data = Betula_pub_long, aes(x = age, y = H_g,
                                         #color = yield_class, 
                                         linetype = yield_class)) +
   scale_linetype_manual(values = c("solid", "dashed", "dashed", "dashed",
                                    "solid", "dashed", "dashed", "dashed",
                                    "solid", "dashed", "dashed", "dashed",
                                    "solid", "dashed", "dashed"), guide = "none") +
   #geom_dl(aes(label = Betula_pub_long$yield_class), method = list("last.points")) +
   geom_point(data = tree_parameter %>% 
                filter(SP_code == "MBI") %>%  
                filter(!is.na(age)) %>% 
                filter(age >= 30),  
              aes(x = age, y = H_g, col = hydrology, shape = peatland_type), 
              stroke = 1.1, size = 3) +
   #scale_shape_manual(values=c(1, 16)) +                                             # adapt number of shapes according to shown entities !!!!!
   scale_shape_manual(name = "Peatland type", values = c(15, 17)) +
   scale_color_manual(name = "Hydrology", values = c("#936241", "#387286")) +
   geom_dl(data = Betula_pub_long, aes(x = age, y = H_g, #width = yield_class,
                                       label = yield_class), 
           method = list(dl.trans(x = x + 0.2), "last.points", cex = 1.4)) + # single plot cex = 0.8
   #guides(color = guide_legend(order = 1),  # for single plot creation, delete hashtags
   #       shape = guide_legend(order = 2)) +
   guides(color = "none", # for comparison plot creation for betula, add hashtags
          shape = "none") +
   xlab("Age [years]") +
   ylab("Hg [m]") +
   #scale_y_continuous(breaks=c(10, 15, 20, 25, 30, 35), limits = c(8, 35)) +
   scale_x_continuous(breaks=c(25, 50, 75, 100), limits = c(25, 119)) +
   theme(axis.title.x = element_text(size = 22)) + #14
   theme(axis.text.x = element_text(size = 20)) + #12
   theme(axis.title.y = element_text(size = 22)) + #14
   theme(axis.text.y = element_text(size = 20)) +#12
   theme(panel.background = NULL))

# Betula pendula
(Betula_pen_hydrology_comp <- 
   ggplot() +
   geom_line(data = Betula_pen_long, aes(x = age, y = H_g,
                                         #color = yield_class, 
                                         linetype = yield_class)) +
   scale_linetype_manual(values = c("dashed", "dashed",
                                    "solid", "dashed", "dashed", "dashed",
                                    "solid", "dashed", "dashed", "dashed",
                                    "solid", "dashed", "dashed", "dashed",
                                    "solid", "dashed", "dashed", "dashed",
                                    "solid"), guide = "none") +
   #geom_dl(aes(label = Betula_pen_long$yield_class), method = list("last.points")) +
   geom_point(data = tree_parameter %>% 
                filter(SP_code == "MBI") %>%  
                filter(!is.na(age)) %>% 
                filter(age >= 15),  
              aes(x = age, y = H_g, col = hydrology, shape = peatland_type), 
              stroke = 1.1, size = 3) +                                          # adapt number of shapes according to shown entities !!!!!
   scale_shape_manual(name = "Peatland type", values = c(15, 17)) +
   scale_color_manual(name = "Hydrology", values = c("#936241", "#387286")) +
   geom_dl(data = Betula_pen_long, aes(x = age, y = H_g, #width = yield_class,
                                       label = yield_class), 
           method = list(dl.trans(x = x + 0.2), "last.points", cex = 1.2)) + # single plot cex = 0.8
   guides(color = guide_legend(order = 1), 
          shape = guide_legend(order = 2)) +
   xlab("Age [years]") +
   #ylab("Hg [m]") + # for single plot creation, delete hashtag
   scale_x_continuous(breaks=c(25, 50, 75, 100), limits = c(12, 119)) +
   theme(axis.title.x = element_text(size = 22)) + #14
   theme(axis.text.x = element_text(size = 20)) + #12
   theme(axis.title.y = element_blank()) + # for comparison plot creation for betula, add hashtag
   #theme(axis.title.y = element_text(size = 14)) + # for single plot creation, delete hashtag
   theme(axis.text.y = element_text(size = 22)) + #12
   theme(legend.text = element_text(size = 18)) + # for single plot creation, delete hashtag
   theme(legend.title = element_text(size = 20)) + # for single plot creation, delete hashtag
   theme(panel.background = NULL))

(Betula_comp <- Betula_pub_hydrology_comp | Betula_pen_hydrology_comp)

ggsave("Images/Bon_Betula_comp.png", dpi = 300)
# ------- 4.1.4 Picea abies ----------------------------------------------------

# source: Wiedemann (1936/42) / Schober 1974

Picea <- data.frame(age = seq(20, 120, 5), # Hg in meters
                    EKL1 = c(7.1, 9.2, 11.5, 14.1, 16.6, 19, 21.2, 23.1, 24.7, 26.1, 27.4, 28.6, 29.7, 30.7, 31.6, 32.5, 33.3, 34.1, 34.8, 35.4, 35.9),
                    EKL2 = c(5.1, 6.7, 8.6, 10.7, 12.8, 14.9, 16.9, 18.8, 20.5, 22, 23.3, 24.5, 25.6, 26.6, 27.6, 28.5, 29.3, 30.1, 30.8, 31.5, 32.1),
                    EKL3 = c(3.9, 5.1, 6.2, 7.6, 9.3, 11.3, 13.1, 14.7, 16.2, 17.6, 18.9, 20.1, 21.2, 22.2, 23.2, 24.1, 25, 25.9, 26.7, 27.5, 28.2),
                    EKL4 = c(NA, NA, 4.2, 5.5, 6.9, 8.3, 9.8, 11.3, 12.7, 14, 15.2, 16.3, 17.3, 18.3, 19.2, 20.1, 21, 21.8, 22.6, 23.3, 24),
                    EKL5 = c(NA, NA, NA, NA, 4.5, 5.6, 6.8, 8, 9.3, 10.5, 11.7, 12.8, 13.8, 14.8, 15.7, 16.5, 17.2, NA, NA, NA, NA))

Picea_long <- Picea %>% 
  pivot_longer(cols = starts_with("EKL"), names_to = "yield_class",
               values_to = "H_g")


# Grouped by plot ID
(Picea_plot <-
    ggplot() +
    geom_line(data = Picea_long, aes(x = age, y = H_g,
                                     #color = yield_class, 
                                     linetype = yield_class)) +
    scale_linetype_manual(values = c("solid", "solid", "solid", "solid", "solid"),
                          guide = "none") +
    #geom_dl(aes(label = Picea_long$yield_class), method = list("last.points")) +
    geom_point(data = tree_parameter %>% 
                 filter(SP_code == "GFI") %>%  
                 filter(!is.na(age)) %>% 
                 filter(age >= 20), 
               aes(x = age, y = H_g, col = plot_ID), stroke = 1.1, size = 2.5) +
    #scale_shape_manual(values=c(0, 1, 2)) +                                           # adapt number of shapes according to shown entities !!!!!
    scale_color_manual(name = "Plots", values = c("#8A5F18", "#EAC486","#538135", "#355222")) +
    geom_dl(data = Picea_long, aes(x = age, y = H_g, #width = yield_class,
                                   label = yield_class), 
            method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8)) + 
    xlab("Age") +
    ylab("Hg") +
    ggtitle("Picea abies - plot") +
    #scale_y_continuous(breaks=c(10, 15, 20, 25, 30, 35), limits = c(8, 35)) +
    scale_x_continuous(breaks=c(25, 50, 75, 100), limits = c(20, 125)) +
    theme(axis.title.x = element_text(size = 14)) +
    theme(axis.text.x = element_text(size = 12)) +
    theme(axis.title.y = element_text(size = 14)) +
    theme(axis.text.y = element_text(size = 12)) +
    theme(panel.background = NULL))
ggsave("Images/Bon_Picea_plot.png", dpi = 300)


# Grouped by hydrology and peatland type
(Picea_hydrology <- 
    ggplot() +
    geom_line(data = Picea_long, aes(x = age, y = H_g,
                                     #color = yield_class, 
                                     linetype = yield_class)) +
    scale_linetype_manual(values = c("solid", "solid", "solid", "solid", "solid"),
                          guide = "none") +
    #geom_dl(aes(label = Picea_long$yield_class), method = list("last.points")) +
    geom_point(data = tree_parameter %>% 
                 filter(SP_code == "GFI") %>%  
                 filter(!is.na(age)) %>% 
                 filter(age >= 20),  
               aes(x = age, y = H_g, col = hydrology, shape = peatland_type), 
               stroke = 1.1, size = 3) +
    #scale_shape_manual(values=c(1, 16)) +                                             # adapt number of shapes according to shown entities !!!!!
    scale_shape_manual(name = "Peatland type", values = c(16, 15)) +
    scale_color_manual(name = "Hydrology", values = c("#936241", "#387286")) +
    geom_dl(data = Picea_long, aes(x = age, y = H_g, #width = yield_class,
                                   label = yield_class), 
            method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8)) + 
    guides(color = guide_legend(order = 1), 
           shape = guide_legend(order = 2)) +
    xlab("Age [years]") +
    ylab("Hg [m]") +
    #scale_y_continuous(breaks=c(10, 15, 20, 25, 30, 35), limits = c(8, 35)) +
    scale_x_continuous(breaks=c(25, 50, 75, 100), limits = c(20, 125)) +
    theme(axis.title.x = element_text(size = 16)) +
    theme(axis.text.x = element_text(size = 14)) +
    theme(axis.title.y = element_text(size = 16)) +
    theme(axis.text.y = element_text(size = 14)) +
    theme(legend.text = element_text(size = 13)) + 
    theme(legend.title = element_text(size = 15)) + 
    theme(panel.background = NULL))
ggsave("Images/Bon_Picea_hydr_peatland.png", dpi = 300)


# ------- 4.1.5 Pinus mugo -----------------------------------------------------

# source: Lembcke, Knapp and Dittmar (1975) / LFE 2000

Pinus <- data.frame(age = seq(20, 140, 5), # Hg in meters
                    EKL0.5u = c(10.3, 12.9, 15.3, 17.4, 19.4, 21.2, 22.9, 24.5, 25.9, 27.2, 28.4, 29.6, 30.6, 31.6, 32.5, 33.3, 34, 34.7, 35.3, 35.8, 36.3, 36.7, 37, NA, NA),
                    EKL0 = c(9.5, 11.9, 14.2, 16.2, 18.1, 19.8, 21.4, 22.9, 24.2, 25.5, 26.7, 27.8, 28.8, 29.7, 30.5, 31.3, 32, 32.6, 33.2, 33.7, 34.2, 34.6, 34.9, NA, NA),
                    EKL0.5 = c(8.7, 11, 13.1, 15, 16.8, 18.4, 19.9, 21.3, 22.6, 23.8, 24.9, 25.9, 26.9, 27.8, 28.6, 29.3, 30, 30.6, 31.2, 31.6, 32.1, 32.4, 32.7, NA, NA),
                    EKL1 = c(8, 10.1, 12, 13.8, 15.4, 17, 18.4, 19.7, 21, 22.1, 23.2, 24.1, 25, 25.9, 26.7, 27.4, 28, 28.6, 29.1, 29.6, 30, 30.3, 30.6, NA, NA),
                    EKL1.5 = c(NA, 9.1, 10.9, 12.6, 14.1, 15.6, 16.9, 18.1, 19.3, 20.4, 21.4, 22.3, 23.2, 24, 24.7, 25.4, 26, 26.6, 27, 27.5, 27.9, 28.2, 28.5, NA, NA),
                    EKL2 = c(NA, 8.2, 9.8, 11.4, 12.8, 14.1, 15.4, 16.6, 17.7, 18.7, 19.6, 20.5, 21.3, 22.1, 22.8, 23.4, 24, 24.5, 25, 25.4, 25.8, 26.1, 26.3, NA, NA),
                    EKL2.5 = c(NA, NA, 8.7, 10.1, 11.5, 12.7, 13.9, 15, 16, 17, 17.9, 18.7, 19.5, 20.2, 20.9, 21.5, 22, 22.5, 22.9, 23.3, 23.6, 23.9, 24.2, NA, NA),
                    EKL3 = c(NA, NA, 7.7, 8.9, 10.2, 11.3, 12.4, 13.4, 14.4, 15.3, 16.1, 16.9, 17.6, 18.3, 18.9, 19.5, 20, 20.5, 20.9, 21.2, 21.5, 21.8, 22, 22.2, 22.3),
                    EKL3.5 = c(NA, NA, 6.6, 7.7, 8.8, 9.9, 10.9, 11.8, 12.7, 13.6, 14.3, 15.1, 15.8, 16.4, 17, 17.5, 18, 18.4, 18.8, 19.1, 19.4, 19.7, 19.9, 20, 20.1),
                    EKL4 = c(NA, NA, NA, 6.5, 7.5, 8.5, 9.4, 10.2, 11.1, 11.9, 12.6, 13.3, 13.9, 14.5, 15.1, 15.5, 16, 16.4, 16.8, 17.1, 17.3, 17.5, 17.7, 17.8, 17.9),
                    EKL4.5 = c(NA, NA, NA, NA, 6.2, 7, 7.9, 8.7, 9.4, 10.1, 10.8, 11.5, 12.1, 12.6, 13.1, 13.6, 14, 14.4, 14.7, 15, 15.2, 15.4, 15.6, 15.7, 15.7),
                    EKL5 = c(NA, NA, NA, NA, NA, NA, 6.4, 7.1, 7.8, 8.4, 9.1, 9.7, 10.2, 10.7, 11.2, 11.6, 12, 12.3, 12.6, 12.9, 13.1, 13.3, 13.4, 13.5, 13.5))

Pinus_long <- Pinus %>% 
  pivot_longer(cols = starts_with("EKL"), names_to = "yield_class",
               values_to = "H_g")

Pinus_long <- 
  Pinus_long %>% 
  mutate (yield_class = ifelse(yield_class == "EKL0.5u", "EKL+0.5", 
                               ifelse(yield_class == "EKL0", "EKL0", 
                                      ifelse(yield_class == "EKL0.5", "EKL0.5", 
                                             ifelse(yield_class == "EKL1", "EKL1", 
                                                    ifelse(yield_class == "EKL1.5", "EKL1.5", 
                                                           ifelse(yield_class == "EKL2", "EKL2", 
                                                                  ifelse(yield_class == "EKL2.5", "EKL2.5", 
                                                                         ifelse(yield_class == "EKL3", "EKL3", 
                                                                                ifelse(yield_class == "EKL3.5", "EKL3.5", 
                                                                                       ifelse(yield_class == "EKL4", "EKL4", 
                                                                                              ifelse(yield_class == "EKL4.5", "EKL4.5", "EKL5"))))))))))))


# Grouped by plot ID
(Pinus_plot <- 
    ggplot() +
    geom_line(data = Pinus_long, aes(x = age, y = H_g,
                                     #color = yield_class, 
                                     linetype = yield_class)) +
    scale_linetype_manual(values = c("dashed", "solid", "dashed", "solid", "dashed",
                                     "solid", "dashed", "solid", "dashed", "solid", "dashed", "solid"),
                          guide = "none") +
    #geom_dl(aes(label = Pinus_long$yield_class), method = list("last.points")) +
    geom_point(data = tree_parameter %>% 
                 filter(SP_code == c("BKI", "GKI")) %>%  
                 filter(!is.na(age)), 
               aes(x = age, y = H_g, col = plot_ID), stroke = 1.1, size = 2.5) +
    #scale_shape_manual(values=c(1)) +                                                 # adapt number of shapes according to shown entities !!!!!
    #scale_color_manual(name = "Plots", values = c("#538135")) +
    geom_dl(data = Pinus_long, aes(x = age, y = H_g, #width = yield_class,
                                   label = yield_class), 
            method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8)) + 
    xlab("Age") +
    ylab("Hg") +
    ggtitle("Pinus mugo - plot") +
    #scale_y_continuous(breaks=c(10, 15, 20, 25, 30, 35), limits = c(8, 35)) +
    scale_x_continuous(breaks=c(25, 50, 75, 100, 125, 150), limits = c(20, 150)) +
    theme(axis.title.x = element_text(size = 14)) +
    theme(axis.text.x = element_text(size = 12)) +
    theme(axis.title.y = element_text(size = 14)) +
    theme(axis.text.y = element_text(size = 12)) +
    theme(panel.background = NULL))
ggsave("Images/Bon_Pinus_plot.png", dpi = 300)


# Grouped by hydrology and peatland type
(Pinus_hydrology <- 
    ggplot() +
    geom_line(data = Pinus_long, aes(x = age, y = H_g,
                                     #color = yield_class, 
                                     linetype = yield_class)) +
    scale_linetype_manual(values = c("dashed", "solid", "dashed", "solid", 
                                     "dashed", "solid", "dashed", "solid", 
                                     "dashed", "solid", "dashed", "solid"),
                          guide = "none") +
    #geom_dl(aes(label = Pinus_long$yield_class), method = list("last.points")) +
    geom_point(data = tree_parameter %>% 
                 filter(SP_code == c("BKI", "GKI")) %>%  
                 filter(!is.na(age)), 
               aes(x = age, y = H_g, col = hydrology, shape = peatland_type), 
               stroke = 1.1, size = 3) +
    #scale_shape_manual(values=c(16)) +                                                # adapt number of shapes according to shown entities !!!!!
    scale_shape_manual(name = "Peatland type", values = c(16, 15)) +
    scale_color_manual(name = "Hydrology", values = c("#936241", "#387286")) +
    geom_dl(data = Pinus_long, aes(x = age, y = H_g, #width = yield_class,
                                   label = yield_class), 
            method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8)) + 
    guides(color = guide_legend(order = 1), 
           shape = guide_legend(order = 2)) +
    xlab("Age [years]") +
    ylab("Hg [m]") +
    #scale_y_continuous(breaks=c(10, 15, 20, 25, 30, 35), limits = c(8, 35)) +
    scale_x_continuous(breaks=c(25, 50, 75, 100, 125, 150), limits = c(20, 150)) +
    theme(axis.title.x = element_text(size = 16)) +
    theme(axis.text.x = element_text(size = 14)) +
    theme(axis.title.y = element_text(size = 16)) +
    theme(axis.text.y = element_text(size = 14)) +
    theme(legend.text = element_text(size = 13)) + 
    theme(legend.title = element_text(size = 15)) + 
    theme(panel.background = NULL))
ggsave("Images/Bon_Pinus_hydr_peatland.png", dpi = 300)


# ------ 4.2. Biomass volume (Vonderach/HUber) ---------------------------------------------------

biomass_VH <- aggregate(trees_total_5$AGB_kg, by = list(trees_total_5$plot_ID), FUN = sum)

names(biomass_VH) <- c("MoMoK_Nr", "AGB_kg")
biomass_VH <- merge(Sites, biomass_VH, by = "MoMoK_Nr")

biomass_VH$AGB_kg <- biomass_VH$AGB_kg / c_A(12.62) * 10000 / 1000

# ------- 4.2.1. plotting  ------------------------------------------------------
# ------- 4.2.1.1. plots ----------------------------------------------------------
# peatland types
biomass_VH %>% 
  ggplot(aes(x = MoMoK_Nr, y = AGB_kg, fill = peatland_type)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual("Peatland type",values = c("#70AD47", "#DC9C34", "#8A5F18")) +
  xlab("Plot") +
  ylab(expression(paste("Biomass ", " [kg ", ha^-1, "]"))) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.text.x = element_text(size = 14, angle = 90)) +
  theme(axis.title.y = element_text(size = 16)) +
  theme(axis.text.y = element_text(size = 14)) +
  theme(legend.text = element_text(size = 13)) + 
  theme(legend.title = element_text(size = 15)) + 
  theme(panel.background = NULL)
ggsave("Images/BM_VH_plots_peatland.png", dpi = 300)

# ------- 4.2.1.2. boxplots (median) ----------------------------------------------
# peatland types

BM_VH_pt_summary <-
  biomass_VH %>% 
  group_by(peatland_type, tree_spec) %>% 
  tally()

biomass_VH %>% 
  ggplot(aes(x = tree_spec, y = AGB_kg, fill = peatland_type)) +
  geom_boxplot() +
  scale_fill_manual(name = "Peatland Type", values = c("#70AD47", "#DC9C34", "#8A5F18")) +
  xlab("Tree Species") +
  ylab(expression(paste("Biomass ", " [kg ", ha^-1, "]"))) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.text.x = element_text(size = 14)) +
  theme(axis.title.y = element_text(size = 16)) +
  theme(axis.text.y = element_text(size = 14)) +
  theme(legend.text = element_text(size = 13)) + 
  theme(legend.title = element_text(size = 15)) + 
  theme(panel.background = NULL)
ggsave("Images/BM_VH_median_peatland.png", dpi = 300)

# hydrology
BM_VH_hy_summary <-
  biomass_VH %>% 
  group_by(hydrology, tree_spec) %>% 
  tally()

biomass_VH %>% 
  ggplot(aes(x = tree_spec, y = AGB_kg, fill = hydrology)) +
  geom_boxplot() +
  scale_fill_manual(name = "Hydrology", values = c("#70AD47", "#DC9C34")) +
  xlab("Tree Species") +
  ylab(expression(paste("Biomass ", " [kg ", ha^-1, "]"))) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.text.x = element_text(size = 14)) +
  theme(axis.title.y = element_text(size = 16)) +
  theme(axis.text.y = element_text(size = 14)) +
  theme(legend.text = element_text(size = 13)) + 
  theme(legend.title = element_text(size = 15)) + 
  theme(panel.background = NULL)
ggsave("Images/BM_VH_median_hydr.png", dpi = 300)

# tree species
BM_VH_ts_summary <-
  biomass_VH %>% 
  group_by(tree_spec) %>% 
  tally()

biomass_VH %>% 
  ggplot(aes(x = tree_spec, y = AGB_kg)) +
  geom_boxplot(fill = c("#538135", "#70AD47", "#DC9C34", "#8A5F18")) +
  xlab("Tree species") +
  ylab(expression(paste("Biomass ", " [kg ", ha^-1, "]"))) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.text.x = element_text(size = 14)) +
  theme(axis.title.y = element_text(size = 16)) +
  theme(axis.text.y = element_text(size = 14)) +
  theme(legend.text = element_text(size = 13)) + 
  theme(legend.title = element_text(size = 15)) + 
  theme(panel.background = NULL)
ggsave("Images/BM_VH_median_tree_species.png", dpi = 300)

# ------ 4.3. Biomass volume ---------------------------------------------------

biomass <- aggregate(trees_total_5$biom, by = list(trees_total_5$plot_ID), FUN = sum)

names(biomass) <- c("MoMoK_Nr", "AGB_kg")
biomass <- merge(Sites, biomass, by = "MoMoK_Nr")

biomass$AGB_kg <- biomass$AGB_kg / c_A(12.62) * 10000 / 1000

# ------- 4.3.1. plotting  -----------------------------------------------------
# ------- 4.3.1.1 plots ----------------------------------------------------------
# peatland types
biomass %>% 
  ggplot(aes(x = MoMoK_Nr, y = AGB_kg, fill = peatland_type)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual("Peatland type",values = c("#70AD47", "#DC9C34", "#8A5F18")) +
  xlab("Plot") +
  ylab(expression(paste("Biomass ", " [kg ", ha^-1, "]"))) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.text.x = element_text(size = 14, angle = 90)) +
  theme(axis.title.y = element_text(size = 16)) +
  theme(axis.text.y = element_text(size = 14)) +
  theme(legend.text = element_text(size = 13)) + 
  theme(legend.title = element_text(size = 15)) + 
  theme(panel.background = NULL)
ggsave("Images/BM_plots_peatland.png", dpi = 300)

# ------- 4.3.1.2 boxplots (median) ----------------------------------------------
# peatland types

BM_pt_summary <-
  biomass %>% 
  group_by(peatland_type, tree_spec) %>% 
  tally()

biomass %>% 
  ggplot(aes(x = tree_spec, y = AGB_kg, fill = peatland_type)) +
  geom_boxplot() +
  scale_fill_manual(name = "Peatland Type", values = c("#70AD47", "#DC9C34", "#8A5F18")) +
  xlab("Tree Species") +
  ylab(expression(paste("Biomass ", " [kg ", ha^-1, "]"))) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.text.x = element_text(size = 14)) +
  theme(axis.title.y = element_text(size = 16)) +
  theme(axis.text.y = element_text(size = 14)) +
  theme(legend.text = element_text(size = 13)) + 
  theme(legend.title = element_text(size = 15)) + 
  theme(panel.background = NULL)
ggsave("Images/BM_median_peatland.png", dpi = 300)

# hydrology
BM_hy_summary <-
  biomass %>% 
  group_by(hydrology, tree_spec) %>% 
  tally()

biomass %>% 
  ggplot(aes(x = tree_spec, y = AGB_kg, fill = hydrology)) +
  geom_boxplot() +
  scale_fill_manual(name = "Hydrology", values = c("#70AD47", "#DC9C34")) +
  xlab("Tree Species") +
  ylab(expression(paste("Biomass ", " [kg ", ha^-1, "]"))) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.text.x = element_text(size = 14)) +
  theme(axis.title.y = element_text(size = 16)) +
  theme(axis.text.y = element_text(size = 14)) +
  theme(legend.text = element_text(size = 13)) + 
  theme(legend.title = element_text(size = 15)) + 
  theme(panel.background = NULL)
ggsave("Images/BM_median_hydr.png", dpi = 300)

# tree species
BM_ts_summary <-
  biomass %>% 
  group_by(tree_spec) %>% 
  tally()

biomass %>% 
  ggplot(aes(x = tree_spec, y = AGB_kg)) +
  geom_boxplot(fill = c("#538135", "#70AD47", "#DC9C34", "#8A5F18")) +
  xlab("Tree species") +
  ylab(expression(paste("Biomass ", " [kg ", ha^-1, "]"))) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.text.x = element_text(size = 14)) +
  theme(axis.title.y = element_text(size = 16)) +
  theme(axis.text.y = element_text(size = 14)) +
  theme(legend.text = element_text(size = 13)) + 
  theme(legend.title = element_text(size = 15)) + 
  theme(panel.background = NULL)
ggsave("Images/BM_median_tree_species.png", dpi = 300)


# ----- 5. Forest Structure Index ----------------------------------------------
# ------ 5.1. Variables --------------------------------------------------------
# ------- 5.1.1. DBHq  ---------------------------------------------------------

# Quadratic mean diameter at breast height (DBH >= 7 cm)
# Growing stock

# calculate the quadratic mean (root square mean) of DBH grouped by sample plots
DBHq <- aggregate(DBH ~ MoMoK_Nr, data = Trees, function(x) sqrt(mean(x^2)))

# change column names
colnames(DBHq) <- c("MoMoK_Nr", "DBHq")

# calculate DBHq index
DBHq$DBHq_index <- (DBHq$DBHq - min(DBHq$DBHq)) / (max(DBHq$DBHq) - min(DBHq$DBHq))


# ------- 5.1.2. DBHsd  --------------------------------------------------------

# DBH, standard deviation (DBH >= 7 cm)
# Uneven-ageness

# calculate the standard deviation of DBH grouped by sample plots
DBHsd <- aggregate(Trees$DBH, list(Trees$MoMoK_Nr), FUN = sd)

# change column names
colnames(DBHsd) <- c("MoMoK_Nr", "DBHsd")

# calculate DBHq index
DBHsd$DBHsd_index <- (DBHsd$DBHsd - min(DBHsd$DBHsd)) / (max(DBHsd$DBHsd) - min(DBHsd$DBHsd))


# ------- 5.1.3. Vol40  --------------------------------------------------------

# Volume / ha of trees (DBH >= 40 cm)
# Volume of large living trees

# fixed factors
FZ <- 0.5 # Formzahl
radius_SP <- 12.62 # radius (m) of the sample plot
ha <- 10000 # m2 per ha

# calculate the volumne of all trees above a DBH of 40 cm, grouped by sample plots
Vol40 <- aggregate(((((DBH / 100) / 2)^2 * pi) * height * FZ) ~ MoMoK_Nr, # incl. DBH conversion of cm into m and diameter into radius
                   Trees[Trees$DBH >= 40, ], sum)

# change column names
colnames(Vol40) <- c("MoMoK_Nr", "Vol40")

Vol40$Vol40_ha <- Vol40$Vol40 / ((radius_SP)^2 * pi) * ha # extrapolation for one hectar

# calculate DBHq index
Vol40$Vol40_index <- (Vol40$Vol40_ha - min(Vol40$Vol40_ha)) / (max(Vol40$Vol40_ha) - min(Vol40$Vol40_ha))


# ------- 5.1.4. HEIGHTsd  -----------------------------------------------------

# Tree height, standard deviation (DBH >= 7 cm)
# Vertical heterogeneity

# calculate the standard deviation of tree heights grouped by sample plots
HEIGHTsd <- aggregate(Trees$height, list(Trees$MoMoK_Nr), FUN = sd, na.rm = TRUE) # remove NAs from df "Trees"

# change column names
colnames(HEIGHTsd) <- c("MoMoK_Nr", "HEIGHTsd")

HEIGHTsd <- HEIGHTsd[!is.na(HEIGHTsd$HEIGHTsd),] # remove NAs from the resulting data frame

# calculate DBHq index
HEIGHTsd$HEIGHTsd_index <- (HEIGHTsd$HEIGHTsd - min(HEIGHTsd$HEIGHTsd)) / (max(HEIGHTsd$HEIGHTsd) - min(HEIGHTsd$HEIGHTsd))


# ------- 5.1.5. DW downed (DWd)  ----------------------------------------------

# Downed deadwood, average mean diameter
# Deadwood downed

# subset only samples from downed deadwood
downed <- subset(DW, type %in% c(1, 4, 5, 6)) # only include standing deadwood when type 1 or 5

# calculate the mean of downed deadwood grouped by sample plots
DWd <- aggregate(downed$diameter, list(downed$MoMoK_Nr), FUN = mean, na.rm = TRUE)

# change column names
colnames(DWd) <- c("MoMoK_Nr", "DWd")

# calculate DBHq index
DWd$DWd_index <- (DWd$DWd - min(DWd$DWd)) / (max(DWd$DWd) - min(DWd$DWd))


# ------- 5.1.6. DW standing (DWs)  --------------------------------------------

# Standing deadwood, mean DBH
# Deadwood standing

# subset only samples from standing deadwood
standing <- subset(DW, type %in% c(2, 3)) # only include standing deadwood when type 2 or 3

# calculate the mean of standing deadwood grouped by sample plots
DWs <- aggregate(standing$diameter, list(standing$MoMoK_Nr), FUN = mean, na.rm = TRUE)

# change column names
colnames(DWs) <- c("MoMoK_Nr", "DWs")

# calculate DBHq index
DWs$DWs_index <- (DWs$DWs - min(DWs$DWs)) / (max(DWs$DWs) - min(DWs$DWs))


# ------- 5.1.7. Decay classes (DC)  -------------------------------------------

# Number of decay classes
# Deadwood decay classes

# calculate the occuring abundance of decay classes on each sample plots
DC <- aggregate(DW$decomposition, list(DW$MoMoK_Nr), function(x) length(unique(x)))

# change column names
colnames(DC) <- c("MoMoK_Nr", "DC")

# calculate DBHq index
DC$DC_index <- (DC$DC- min(DC$DC)) / (max(DC$DC) - min(DC$DC))


# ------- 5.1.8. SRreg  --------------------------------------------------------

# Number of tree species in the regeneration layer
# Tree regeneration diversity

## 1. Take number of regeneration species from the regeneration survey of the forest inventory
# calculate the species richness of trees in the regenation sphere
SRreg <- aggregate(Reg$tree_spec, list(Reg$MoMoK_Nr), function(x) length(unique(x)))

# change column names
colnames(SRreg) <- c("MoMoK_Nr", "SRreg")

# calculate DBHq index
SRreg$SRreg_index <- (SRreg$SRreg - min(SRreg$SRreg)) / (max(SRreg$SRreg) - min(SRreg$SRreg))


## 2. Take number of regeneration species from the vegetation survey
# calculate the species richness of trees in the regenation sphere
SRreg2 <- aggregate(Reg2$tree_spec, list(Reg2$MoMoK_Nr), function(x) length(unique(x)))

# change column names
colnames(SRreg2) <- c("MoMoK_Nr", "SRreg2")

# calculate DBHq index
SRreg2$SRreg2_index <- (SRreg2$SRreg2 - min(SRreg2$SRreg2)) / (max(SRreg2$SRreg2) - min(SRreg2$SRreg2))


# ------- 5.1.9. SR  -----------------------------------------------------------

# Number of tree species (DBH >= 7 cm)
# Compositional heterogeneity

# calculate the species richness of trees in the forest main stand
SR <- aggregate(Trees$tree_spec, list(Trees$MoMoK_Nr), function(x) length(unique(x)))

# change column names
colnames(SR) <- c("MoMoK_Nr", "SR")

# calculate DBHq index
SR$SR_index <- (SR$SR - min(SR$SR)) / (max(SR$SR) - min(SR$SR))

## 2. Take number of regeneration species from the vegetation survey
# calculate the species richness of trees in the main stand sphere
SR2 <- aggregate(Trees_SR$tree_spec, list(Trees_SR$MoMoK_Nr), function(x) length(unique(x)))

# change column names
colnames(SR2) <- c("MoMoK_Nr", "SR2")

# calculate DBHq index
SR2$SR2_index <- (SR2$SR2 - min(SR2$SR2)) / (max(SR2$SR2) - min(SR2$SR2))



# ------ 5.2. Variable indices -------------------------------------------------

Variable_Indeces <- merge(Sites, DBHq, by = "MoMoK_Nr", all = TRUE)
Variable_Indeces <- merge(Variable_Indeces, DBHsd, by = "MoMoK_Nr", all = TRUE)
Variable_Indeces <- merge(Variable_Indeces, Vol40, by = "MoMoK_Nr", all = TRUE)
Variable_Indeces <- merge(Variable_Indeces, HEIGHTsd, by = "MoMoK_Nr", all = TRUE)
Variable_Indeces <- merge(Variable_Indeces, DWd, by = "MoMoK_Nr", all = TRUE)
Variable_Indeces <- merge(Variable_Indeces, DWs, by = "MoMoK_Nr", all = TRUE)
Variable_Indeces <- merge(Variable_Indeces, DC, by = "MoMoK_Nr", all = TRUE)
Variable_Indeces <- merge(Variable_Indeces, SRreg, by = "MoMoK_Nr", all = TRUE)
Variable_Indeces <- merge(Variable_Indeces, SR, by = "MoMoK_Nr", all = TRUE)

Variable_Indeces[is.na(Variable_Indeces)] <- 0

write.table(Variable_Indeces, "Forest Structure Indices.csv", dec = ",", sep = ";", row.names=FALSE)

# 2.
Variable_Indeces2 <- merge(Sites, DBHq, by = "MoMoK_Nr", all = TRUE)
Variable_Indeces2 <- merge(Variable_Indeces2, DBHsd, by = "MoMoK_Nr", all = TRUE)
Variable_Indeces2 <- merge(Variable_Indeces2, Vol40, by = "MoMoK_Nr", all = TRUE)
Variable_Indeces2 <- merge(Variable_Indeces2, HEIGHTsd, by = "MoMoK_Nr", all = TRUE)
Variable_Indeces2 <- merge(Variable_Indeces2, DWd, by = "MoMoK_Nr", all = TRUE)
Variable_Indeces2 <- merge(Variable_Indeces2, DWs, by = "MoMoK_Nr", all = TRUE)
Variable_Indeces2 <- merge(Variable_Indeces2, DC, by = "MoMoK_Nr", all = TRUE)
Variable_Indeces2 <- merge(Variable_Indeces2, SRreg2, by = "MoMoK_Nr", all = TRUE)
Variable_Indeces2 <- merge(Variable_Indeces2, SR2, by = "MoMoK_Nr", all = TRUE)

Variable_Indeces2[is.na(Variable_Indeces2)] <- 0

write.table(Variable_Indeces2, "Forest Structure Indices 2.csv", dec = ",", sep = ";", row.names=FALSE)

# ------ 5.3. Forest Structural Index ------------------------------------------

Variable_Indeces$SUM_Indices <- 
  Variable_Indeces$DBHq_index +
  Variable_Indeces$DBHsd_index +
  Variable_Indeces$Vol40_index + 
  Variable_Indeces$HEIGHTsd_index + 
  Variable_Indeces$DWd_index +
  Variable_Indeces$DWs_index +
  Variable_Indeces$DC_index +
  Variable_Indeces$SRreg_index + 
  Variable_Indeces$SR_index

Variable_Indeces2$SUM_Indices2 <- 
  Variable_Indeces2$DBHq_index +
  Variable_Indeces2$DBHsd_index +
  Variable_Indeces2$Vol40_index + 
  Variable_Indeces2$HEIGHTsd_index + 
  Variable_Indeces2$DWd_index +
  Variable_Indeces2$DWs_index +
  Variable_Indeces2$DC_index +
  Variable_Indeces2$SRreg2_index + 
  Variable_Indeces2$SR2_index

NAV <- 9  # Number of applied variables

Variable_Indeces$FSI <- Variable_Indeces$SUM_Indices / NAV
Variable_Indeces2$FSI2 <- Variable_Indeces2$SUM_Indices2 / NAV


##  create a data frame with all indeces below another  ##

Index_long <- Variable_Indeces 

Index_long <- Index_long%>% 
  pivot_longer(cols = c(DBHq_index, DBHsd_index, Vol40_index, HEIGHTsd_index,
                        DWd_index, DWs_index, DC_index, SRreg_index, SR_index),
               names_to = "Indeces", values_to = "value")

Index_long <- Index_long[-c(3, 12:25)] # remove useless columns

Index_long$value_mean <- Index_long$value / 90

Index_long$Index_name <- Index_long$Indeces

Index_long <- 
  Index_long %>% 
  mutate(Index_name = ifelse(Index_name == "DBHq_index", "DBHq", 
                             ifelse(Index_name == "DBHsd_index", "DBHsd", 
                                    ifelse(Index_name == "DC_index", "DC", 
                                           ifelse(Index_name == "DWd_index", "DWd", 
                                                  ifelse(Index_name == "DWs_index", "DWs", 
                                                         ifelse(Index_name == "HEIGHTsd_index", "Hsd", 
                                                                ifelse(Index_name == "SR_index", "SR", 
                                                                       ifelse(Index_name == "SRreg_index", "SRreg", "Vol40")))))))))

# 2.
Index_long2 <- Variable_Indeces2 

Index_long2 <- Index_long2%>% 
  pivot_longer(cols = c(DBHq_index, DBHsd_index, Vol40_index, HEIGHTsd_index,
                        DWd_index, DWs_index, DC_index, SRreg2_index, SR2_index),
               names_to = "Indeces", values_to = "value")

Index_long2 <- Index_long2[-c(3, 12:25)] # remove useless columns

Index_long2$value_mean <- Index_long2$value / 90

Index_long2$Index_name <- Index_long2$Indeces

Index_long2 <- 
  Index_long2 %>% 
  mutate(Index_name = ifelse(Index_name == "DBHq_index", "DBHq", 
                             ifelse(Index_name == "DBHsd_index", "DBHsd", 
                                    ifelse(Index_name == "DC_index", "DC", 
                                           ifelse(Index_name == "DWd_index", "DWd", 
                                                  ifelse(Index_name == "DWs_index", "DWs", 
                                                         ifelse(Index_name == "HEIGHTsd_index", "Hsd", 
                                                                ifelse(Index_name == "SR2_index", "SR2",  
                                                                       ifelse(Index_name == "SRreg2_index", "SRreg2", "Vol40")))))))))

# ------ 5.4. plotting ---------------------------------------------------------
# ------- 5.4.1 plots ----------------------------------------------------------
# peatland types
Variable_Indeces %>% 
  ggplot(aes(x = MoMoK_Nr, y = FSI, fill = peatland_type)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual("Peatland type",values = c("#70AD47", "#DC9C34", "#8A5F18")) +
  xlab("Plot") +
  ylab("Forest Structure Index") +
  theme(axis.title.x = element_text(size = 14)) +
  theme(axis.text.x = element_text(size = 12, angle = 90)) +
  theme(axis.title.y = element_text(size = 14)) +
  theme(axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 11)) +
  theme(legend.text = element_text(size = 9)) +
  theme(panel.background = NULL)
ggsave("Images/FSI_plots_peatland.png", dpi = 300)

Variable_Indeces2 %>% 
  ggplot(aes(x = MoMoK_Nr, y = FSI2, fill = peatland_type)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual("Peatland type",values = c("#70AD47", "#DC9C34", "#8A5F18")) +
  xlab("Plot") +
  ylab("Forest Structure Index") +
  theme(axis.title.x = element_text(size = 14)) +
  theme(axis.text.x = element_text(size = 12, angle = 90)) +
  theme(axis.title.y = element_text(size = 14)) +
  theme(axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 11)) +
  theme(legend.text = element_text(size = 9)) +
  theme(panel.background = NULL)
ggsave("Images/FSI2_plots_peatland.png", dpi = 300)

# hydrology
Variable_Indeces %>% 
  ggplot(aes(x = MoMoK_Nr, y = FSI, fill = hydrology)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual("Hydrology",values = c("#70AD47", "#DC9C34")) +
  xlab("Plot") +
  ylab("Forest Structure Index") +
  theme(axis.title.x = element_text(size = 14)) +
  theme(axis.text.x = element_text(size = 12, angle = 90)) +
  theme(axis.title.y = element_text(size = 14)) +
  theme(axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 11)) +
  theme(legend.text = element_text(size = 9)) +
  theme(panel.background = NULL)
ggsave("Images/FSI_plots_hydr.png", dpi = 300)

Variable_Indeces2 %>% 
  ggplot(aes(x = MoMoK_Nr, y = FSI2, fill = hydrology)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual("Hydrology",values = c("#70AD47", "#DC9C34")) +
  xlab("Plot") +
  ylab("Forest Structure Index") +
  theme(axis.title.x = element_text(size = 14)) +
  theme(axis.text.x = element_text(size = 12, angle = 90)) +
  theme(axis.title.y = element_text(size = 14)) +
  theme(axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 11)) +
  theme(legend.text = element_text(size = 9)) +
  theme(panel.background = NULL)
ggsave("Images/FSI2_plots_hydr.png", dpi = 300)

# tree species
Variable_Indeces %>% 
  ggplot(aes(x = MoMoK_Nr, y = FSI, fill = tree_spec)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual("Tree species",values = c("#538135", "#70AD47", "#DC9C34", "#8A5F18")) +
  xlab("Plot") +
  ylab("Forest Structure Index") +
  theme(axis.title.x = element_text(size = 14)) +
  theme(axis.text.x = element_text(size = 12, angle = 90)) +
  theme(axis.title.y = element_text(size = 14)) +
  theme(axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 11)) +
  theme(legend.text = element_text(size = 9)) +
  theme(panel.background = NULL)
ggsave("Images/FSI_plots_tree_species.png", dpi = 300)

Variable_Indeces2 %>% 
  ggplot(aes(x = MoMoK_Nr, y = FSI2, fill = tree_spec)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual("Tree species",values = c("#538135", "#70AD47", "#DC9C34", "#8A5F18")) +
  xlab("Plot") +
  ylab("Forest Structure Index") +
  theme(axis.title.x = element_text(size = 14)) +
  theme(axis.text.x = element_text(size = 12, angle = 90)) +
  theme(axis.title.y = element_text(size = 14)) +
  theme(axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 11)) +
  theme(legend.text = element_text(size = 9)) +
  theme(panel.background = NULL)
ggsave("Images/FSI2_plots_tree_species.png", dpi = 300)


# ------- 5.4.2 boxplots (median) ----------------------------------------------
# peatland types
Variable_Indeces %>% 
  ggplot(aes(x = tree_spec, y = FSI, fill = peatland_type)) +
  geom_boxplot() +
  scale_fill_manual(name = "Peatland Type", values = c("#70AD47", "#DC9C34", "#8A5F18")) +
  xlab("Tree Species") +
  ylab("Forest Structure Index") +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.text.x = element_text(size = 14)) +
  theme(axis.title.y = element_text(size = 16)) +
  theme(axis.text.y = element_text(size = 14)) +
  theme(legend.text = element_text(size = 13)) + 
  theme(legend.title = element_text(size = 15)) + 
  theme(panel.background = NULL)
ggsave("Images/FSI_median_peatland.png", dpi = 300)

Variable_Indeces2 %>% 
  ggplot(aes(x = tree_spec, y = FSI2, fill = peatland_type)) +
  geom_boxplot() +
  scale_fill_manual(name = "Peatland Type", values = c("#70AD47", "#DC9C34", "#8A5F18")) +
  xlab("Tree Species") +
  ylab("Forest Structure Index") +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.text.x = element_text(size = 14)) +
  theme(axis.title.y = element_text(size = 16)) +
  theme(axis.text.y = element_text(size = 14)) +
  theme(legend.text = element_text(size = 13)) + 
  theme(legend.title = element_text(size = 15)) + 
  theme(panel.background = NULL)
ggsave("Images/FSI2_median_peatland.png", dpi = 300)

# hydrology
Variable_Indeces %>% 
  ggplot(aes(x = tree_spec, y = FSI, fill = hydrology)) +
  geom_boxplot() +
  scale_fill_manual(name = "Hydrology", values = c("#70AD47", "#DC9C34")) +
  xlab("Tree Species") +
  ylab("Forest Structure Index") +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.text.x = element_text(size = 14)) +
  theme(axis.title.y = element_text(size = 16)) +
  theme(axis.text.y = element_text(size = 14)) +
  theme(legend.text = element_text(size = 13)) + 
  theme(legend.title = element_text(size = 15)) + 
  theme(panel.background = NULL)
ggsave("Images/FSI_median_hydr.png", dpi = 300)

Variable_Indeces2 %>% 
  ggplot(aes(x = tree_spec, y = FSI2, fill = hydrology)) +
  geom_boxplot() +
  scale_fill_manual(name = "Hydrology", values = c("#70AD47", "#DC9C34")) +
  xlab("Tree Species") +
  ylab("Forest Structure Index") +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.text.x = element_text(size = 14)) +
  theme(axis.title.y = element_text(size = 16)) +
  theme(axis.text.y = element_text(size = 14)) +
  theme(legend.text = element_text(size = 13)) + 
  theme(legend.title = element_text(size = 15)) + 
  theme(panel.background = NULL)
ggsave("Images/FSI2_median_hydr.png", dpi = 300)

# tree species
Variable_Indeces %>% 
  ggplot(aes(x = tree_spec, y = FSI)) +
  geom_boxplot(fill = c("#538135", "#70AD47", "#DC9C34", "#8A5F18")) +
  xlab("Tree species") +
  ylab("Forest Structure Index") +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.text.x = element_text(size = 14)) +
  theme(axis.title.y = element_text(size = 16)) +
  theme(axis.text.y = element_text(size = 14)) +
  theme(legend.text = element_text(size = 13)) + 
  theme(legend.title = element_text(size = 15)) + 
  theme(panel.background = NULL)
ggsave("Images/FSI_median_tree_species.png", dpi = 300)

Variable_Indeces2 %>% 
  ggplot(aes(x = tree_spec, y = FSI2)) +
  geom_boxplot(fill = c("#538135", "#70AD47", "#DC9C34", "#8A5F18")) +
  xlab("Tree species") +
  ylab("Forest Structure Index") +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.text.x = element_text(size = 14)) +
  theme(axis.title.y = element_text(size = 16)) +
  theme(axis.text.y = element_text(size = 14)) +
  theme(legend.text = element_text(size = 13)) + 
  theme(legend.title = element_text(size = 15)) + 
  theme(panel.background = NULL)
ggsave("Images/FSI2_median_tree_species.png", dpi = 300)


# ------- 5.4.3 stacked indices ------------------------------------------------
# peatland types
Index_long %>% 
  ggplot(aes(x = peatland_type, y = value_mean)) + 
  geom_bar(aes(fill = Index_name), position="stack", stat="identity") +
  scale_fill_brewer(name = "Indices", palette = "BrBG") +
  facet_grid(~tree_spec) +
  xlab("Peatland type") +
  ylab("Forest Structure Index") +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.text.x = element_text(size = 14)) +
  theme(axis.title.y = element_text(size = 16)) +
  theme(axis.text.y = element_text(size = 14)) +
  theme(legend.text = element_text(size = 13)) + 
  theme(legend.title = element_text(size = 15)) + 
  theme(panel.background = NULL)
ggsave("Images/FSI_stacked_indices_peatland.png", dpi = 300)

Index_long2 %>% 
  ggplot(aes(x = peatland_type, y = value_mean)) + 
  geom_bar(aes(fill = Index_name), position="stack", stat="identity") +
  scale_fill_brewer(name = "Indices", palette = "BrBG") +
  facet_grid(~tree_spec) +
  xlab("Peatland type") +
  ylab("Forest Structure Index") +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.text.x = element_text(size = 14)) +
  theme(axis.title.y = element_text(size = 16)) +
  theme(axis.text.y = element_text(size = 14)) +
  theme(legend.text = element_text(size = 13)) + 
  theme(legend.title = element_text(size = 15)) + 
  theme(panel.background = NULL)
ggsave("Images/FSI2_stacked_indices_peatland.png", dpi = 300)

# hydrology
Index_long %>% 
  ggplot(aes(x = hydrology, y = value_mean)) + 
  geom_bar(aes(fill = Index_name), position="stack", stat="identity") +
  scale_fill_brewer(name = "Indices", palette = "BrBG") +
  facet_grid(~tree_spec) +
  xlab("Hydrology") +
  ylab("Forest Structure Index") +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.text.x = element_text(size = 14)) +
  theme(axis.title.y = element_text(size = 16)) +
  theme(axis.text.y = element_text(size = 14)) +
  theme(legend.text = element_text(size = 13)) + 
  theme(legend.title = element_text(size = 15)) + 
  theme(panel.background = NULL)
ggsave("Images/FSI_stacked_indices_hydr.png", dpi = 300)

Index_long2 %>% 
  ggplot(aes(x = hydrology, y = value_mean)) + 
  geom_bar(aes(fill = Index_name), position="stack", stat="identity") +
  scale_fill_brewer(name = "Indices", palette = "BrBG") +
  facet_grid(~tree_spec) +
  xlab("Hydrology") +
  ylab("Forest Structure Index") +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.text.x = element_text(size = 14)) +
  theme(axis.title.y = element_text(size = 16)) +
  theme(axis.text.y = element_text(size = 14)) +
  theme(legend.text = element_text(size = 13)) + 
  theme(legend.title = element_text(size = 15)) + 
  theme(panel.background = NULL)
ggsave("Images/FSI2_stacked_indices_hydr.png", dpi = 300)

# tree species
Index_long %>% 
  ggplot(aes(x = tree_spec, y = value_mean, fill = Index_name)) +
  geom_bar(position="stack", stat="identity") +
  scale_fill_brewer(name = "Indices", palette = "BrBG") + 
  geom_bar(position="stack", stat="identity") +
  xlab("Tree species") +
  ylab("Forest Structure Index") +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.text.x = element_text(size = 14)) +
  theme(axis.title.y = element_text(size = 16)) +
  theme(axis.text.y = element_text(size = 14)) +
  theme(legend.text = element_text(size = 13)) + 
  theme(legend.title = element_text(size = 15)) + 
  theme(panel.background = NULL)
ggsave("Images/FSI_stacked_indices_tree_species.png", dpi = 300)

Index_long2 %>% 
  ggplot(aes(x = tree_spec, y = value_mean, fill = Index_name)) +
  geom_bar(position="stack", stat="identity") +
  scale_fill_brewer(name = "Indices", palette = "BrBG") + 
  geom_bar(position="stack", stat="identity") +
  xlab("Tree species") +
  ylab("Forest Structure Index") +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.text.x = element_text(size = 14)) +
  theme(axis.title.y = element_text(size = 16)) +
  theme(axis.text.y = element_text(size = 14)) +
  theme(legend.text = element_text(size = 13)) + 
  theme(legend.title = element_text(size = 15)) + 
  theme(panel.background = NULL)
ggsave("Images/FSI2_stacked_indices_tree_species.png", dpi = 300)


# ----- 6. Biomass / Forest Structure ------------------------------------------
Variable_Indeces$AGB_kg <- biomass$AGB_kg

Variable_Indeces %>% 
  ggplot(aes(x = FSI, y = AGB_kg, col = peatland_type)) +
  geom_point(size = 2.5) +
  scale_color_manual(name = "Peatland Type", values = c("#70AD47", "#DC9C34", "#8A5F18")) +
  xlab("Forest Structure Index") +
  ylab(expression(paste("Biomass ", " [kg ", ha^-1, "]"))) +
  theme(axis.title.x = element_text(size = 14)) +
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title.y = element_text(size = 14)) +
  theme(axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 12)) +
  theme(panel.background = NULL)
ggsave("Images/LM_peatland.png", dpi = 300)

Variable_Indeces %>% 
  ggplot(aes(x = FSI, y = AGB_kg, col = hydrology)) +
  geom_point(size = 2.5) +
  scale_color_manual(name = "Hydrology", values = c("#70AD47", "#DC9C34")) +
  xlab("Forest Structure Index") +
  ylab(expression(paste("Biomass ", " [kg ", ha^-1, "]"))) +
  theme(axis.title.x = element_text(size = 14)) +
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title.y = element_text(size = 14)) +
  theme(axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 12)) +
  theme(panel.background = NULL)
ggsave("Images/LM_hydrology.png", dpi = 300)

Variable_Indeces %>% 
  ggplot(aes(x = FSI, y = AGB_kg, col = tree_spec)) +
  geom_point(size = 2.5) +
  scale_color_manual(name = "Tree species", values = c("#538135", "#70AD47", "#DC9C34", "#8A5F18")) +
  xlab("Forest Structure Index") +
  ylab(expression(paste("Biomass ", " [kg ", ha^-1, "]"))) +
  theme(axis.title.x = element_text(size = 14)) +
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title.y = element_text(size = 14)) +
  theme(axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 12)) +
  theme(panel.background = NULL)
ggsave("Images/LM_tree_spec.png", dpi = 300)

# Linear
Variable_Indeces %>% 
  ggplot(aes(x = FSI, y = AGB_kg)) +
  geom_point(size = 2.5) +
  geom_smooth(method=lm , color = "#538135", fill = "#DC9C34", se = TRUE) +
  stat_poly_eq(use_label("R2")) + # for lm equation add c("eq", "R2")
  xlab("Forest Structure Index") +
  ylab(expression(paste("Biomass ", " [kg ", ha^-1, "]"))) +
  theme(axis.title.x = element_text(size = 14)) +
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title.y = element_text(size = 14)) +
  theme(axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 12)) +
  theme(panel.background = NULL)
ggsave("Images/Linear_model.png", dpi = 300)

# Exponential
Variable_Indeces %>% 
  ggplot(aes(x = FSI, y = AGB_kg)) +
  geom_point(size = 2.5) +
  geom_smooth(method = "lm", formula= (y ~ exp(x)), color = "#538135", fill = "#DC9C34", se = TRUE) +
  stat_poly_eq(use_label("R2"), formula= (y ~ exp(x))) + # for lm equation add c("eq", "R2")
  xlab("Forest Structure Index") +
  ylab(expression(paste("Biomass ", " [kg ", ha^-1, "]"))) +
  theme(axis.title.x = element_text(size = 14)) +
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title.y = element_text(size = 14)) +
  theme(axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 12)) +
  theme(panel.background = NULL)
ggsave("Images/Exp_model.png", dpi = 300)

# Comparison
Variable_Indeces %>% 
  ggplot(aes(x = FSI, y = AGB_kg)) +
  geom_point(size = 2.5) +
  geom_smooth(method = "lm", color = "red", fill = "lightblue", se = TRUE) +
  stat_poly_eq(col = "red", use_label("R2")) + # for lm equation add c("eq", "R2")
  geom_smooth(method = "lm", formula= (y ~ exp(x)), color = "#538135", fill = "#DC9C34", se = TRUE) +
  stat_poly_eq(col = "#538135", use_label("R2"), formula= (y ~ exp(x)), label.y = 1) + # for lm equation add c("eq", "R2")
  xlab("Forest Structure Index") +
  ylab(expression(paste("Biomass ", " [kg ", ha^-1, "]"))) +
  theme(axis.title.x = element_text(size = 14)) +
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title.y = element_text(size = 14)) +
  theme(axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 12)) +
  theme(panel.background = NULL)
ggsave("Images/Comp_model.png", dpi = 300)

# ----- 7. STATISTICS ----------------------------------------------------------
# ------ 7.1 Forest growth -----------------------------------------------------

setwd("C:/Users/jonas/Documents/Jonas Sitte/Ausbildung, Studium, Job/Studium/Master Universität Göttingen/Module/Master-Thesis/Peatland_Forest Structure")

growth_class <- read.csv2("Bonitierung.csv", header = T, sep = ";", dec = ",")
str(growth_class)

setwd("C:/Users/jonas/Documents/Jonas Sitte/Ausbildung, Studium, Job/Studium/Master Universität Göttingen/Module/Master-Thesis/Peatland_Forest Structure/Masterthesis")

growth_class <- merge(Sites, growth_class, by = "MoMoK_Nr", all = TRUE)

growth_class$peatland_type <- as.factor(growth_class$peatland_type)
growth_class$hydrology <- as.factor(growth_class$hydrology)

gcl <- growth_class %>% 
  pivot_longer(cols = c("EKL_Alnus", "EKL_B.pendula", "EKL_B.pubescens", "EKL_Picea", "EKL_Pinus"), names_to = "treespec", values_to = "EKL")

gcl %>% 
  ggplot(aes(x = hydrology, y = EKL, fill = peatland_type)) +
  geom_boxplot() +
  facet_grid(~treespec) +
  scale_fill_manual(name = "Peatland Type", values = c("#70AD47", "#DC9C34", "#8A5F18")) +
  xlab("Hydrology") +
  ylab("Yield Class") +
  theme(axis.title.x = element_text(size = 14)) +
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title.y = element_text(size = 14)) +
  theme(axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 12)) +
  theme(panel.background = NULL)
ggsave("Images/FY_median_combined.png", dpi = 300)

gcl %>% 
  ggplot(aes(x = peatland_type, y = EKL)) +
  geom_boxplot() +
  facet_grid(~treespec) +
  xlab("Peatland Type") +
  ylab("Yield Class") +
  theme(axis.title.x = element_text(size = 14)) +
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title.y = element_text(size = 14)) +
  theme(axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 12)) +
  theme(panel.background = NULL)
ggsave("Images/FY_median_peatland_type.png", dpi = 300)

gcl %>% 
  ggplot(aes(x = hydrology, y = EKL)) +
  geom_boxplot() +
  facet_grid(~treespec) +
  xlab("Hydrology") +
  ylab("Yield Class") +
  theme(axis.title.x = element_text(size = 14)) +
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title.y = element_text(size = 14)) +
  theme(axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 12)) +
  theme(panel.background = NULL)
ggsave("Images/FY_median_hydrology.png", dpi = 300)

# ------- 7.1.1 Betula ---------------------------------------------------------

# comparison of B. pubescense with B. pendula
summary(growth_class$EKL_B.pubescens)
summary(growth_class$EKL_B.pendula)
psych::describeBy(growth_class$EKL_B.pubescens, growth_class$EKL_B.pendula)

# test for normal distribution
shapiro.test(growth_class$EKL_B.pubescens)
shapiro.test(growth_class$EKL_B.pendula)
hist(growth_class$EKL_B.pubescens)
hist(growth_class$EKL_B.pendula)
# test for equal variances
var.test(growth_class$EKL_B.pubescens, growth_class$EKL_B.pendula)

# Welch-test because not normal distributed
t.test(growth_class$EKL_B.pubescens, growth_class$EKL_B.pendula, 
       paired = F, alternative = "two.sided")                      # Welch

# t.test(growth_class$EKL_B.pendula, growth_class$EKL_B.pubescens, 
#        paired = T, alternative = "greater")                         # paired t-test (doesn't work, data is almost constant)

# betula_diff <- growth_class$EKL_B.pubescens - growth_class$EKL_B.pendula
# t.test(betula_diff, mu = 0, alternative = "greater")                # paired t-test (doesn't work, data is almost constant)

t.test(gcl$EKL, group = filter(gcl$treespec == c("EKL_B.pubescens", "EKL_B.pendula")), 
       var.equal = T, alternative = "two.sided")                    # t-test


# ------- 7.1.2 ANOVA ----------------------------------------------------------
# -------- 7.1.2.1 peatland types  ---------------------------------------------
# --------- 7.1.2.1.1 Alnus  ---------------------------------------------------
# non-parametric alternative 
# kruskal.test(EKL_Alnus ~ peatland_type, data = growth_class)

# pairwise.wilcox.test(growth_class$EKL_Alnus, growth_class$peatland_type, 
#                      p.adjust.method = "holm")

# --------- 7.1.2.1.2 Betula  --------------------------------------------------

# test for ANOVA requirements 
# (normal distribution and homoscedasticity of residuals)
anova_growth_betula_pt <- aov(EKL_B.pubescens ~ peatland_type, data = growth_class) # fit of ANOVA model

shapiro.test(residuals(anova_growth_betula_pt)) # test on normal distribution of the models residuals

leveneTest(anova_growth_betula_pt) # test on homoscedasticity of residuals

# ANOVA
lm<-lm(growth_class$EKL_B.pubescens~growth_class$peatland_type) # application of a linear model on the species richness data
shapiro.test(lm$residuals) # test for normal distribution of the residuals

anova(lm)

# Tukey test as Posthoc test
(tukey <- glht(anova_growth_betula_pt, linfct = mcp(peatland_type = "Tukey")))
(result <- cld(tukey, level = 0.05))

# non-parametric alternative
kruskal.test(EKL_B.pubescens ~ peatland_type, data = growth_class)

# pairwise.wilcox.test(growth_class$EKL_B.pubescens, growth_class$peatland_type, 
#                      p.adjust.method = "holm")

# --------- 7.1.2.1.3 Picea  ---------------------------------------------------
# non-parametric alternative
# kruskal.test(EKL_Picea ~ peatland_type, data = growth_class)

# pairwise.wilcox.test(growth_class$EKL_Picea, growth_class$peatland_type, 
#                      p.adjust.method = "holm")

# --------- 7.1.2.1.4 Pinus  ---------------------------------------------------
# non-parametric alternative
# kruskal.test(EKL_Pinus ~ peatland_type, data = growth_class)

# pairwise.wilcox.test(growth_class$EKL_Pinus, growth_class$peatland_type, 
#                      p.adjust.method = "holm")

# -------- 7.2.2.2 hydrology  --------------------------------------------------
# --------- 7.1.2.2.1 Alnus  ---------------------------------------------------

# test for ANOVA requirements 
# (normal distribution and homoscedasticity of residuals)
anova_growth_alnus_h <- aov(EKL_Alnus ~ hydrology, data = growth_class) # fit of ANOVA model

shapiro.test(residuals(anova_growth_alnus_h)) # test on normal distribution of the models residuals

leveneTest(anova_growth_alnus_h) # test on homoscedasticity of residuals

# ANOVA
lm<-lm(growth_class$EKL_Alnus~growth_class$hydrology) # application of a linear model on the species richness data
shapiro.test(lm$residuals) # test for normal distribution of the residuals

anova(lm)

# Tukey test as Posthoc test
(tukey <- glht(anova_growth_alnus_h, linfct = mcp(hydrology = "Tukey")))
(result <- cld(tukey, level = 0.05))

# non-parametric alternative
kruskal.test(EKL_Alnus ~ hydrology, data = growth_class)

pairwise.wilcox.test(growth_class$EKL_Alnus, growth_class$hydrology, p.adjust.method = "holm")

# --------- 7.1.2.2.2 Betula  --------------------------------------------------

# test for ANOVA requirements 
# (normal distribution and homoscedasticity of residuals)
anova_growth_betula_h <- aov(EKL_B.pubescens ~ hydrology, data = growth_class) # fit of ANOVA model

shapiro.test(residuals(anova_growth_betula_h)) # test on normal distribution of the models residuals

leveneTest(anova_growth_betula_h) # test on homoscedasticity of residuals

# ANOVA
lm<-lm(growth_class$EKL_B.pubescens~growth_class$hydrology) # application of a linear model on the species richness data
shapiro.test(lm$residuals) # test for normal distribution of the residuals

anova(lm)

# Tukey test as Posthoc test
(tukey <- glht(anova_growth_betula_h, linfct = mcp(hydrology = "Tukey")))
(result <- cld(tukey, level = 0.05))

# non-parametric alternative
kruskal.test(EKL_B.pubescens ~ hydrology, data = growth_class)

pairwise.wilcox.test(growth_class$EKL_B.pubescens, growth_class$hydrology, 
                     p.adjust.method = "holm")

# --------- 7.1.2.2.3 Picea  ---------------------------------------------------

# test for ANOVA requirements 
# (normal distribution and homoscedasticity of residuals)
anova_growth_picea_h <- aov(EKL_Picea ~ hydrology, data = growth_class) # fit of ANOVA model

shapiro.test(residuals(anova_growth_picea_h)) # test on normal distribution of the models residuals

leveneTest(anova_growth_picea_h) # test on homoscedasticity of residuals

# ANOVA
lm<-lm(growth_class$EKL_Picea~growth_class$hydrology) # application of a linear model on the species richness data
shapiro.test(lm$residuals) # test for normal distribution of the residuals

anova(lm)

# Tukey test as Posthoc test
(tukey <- glht(anova_growth_picea_h, linfct = mcp(hydrology = "Tukey")))
(result <- cld(tukey, level = 0.05))

# non-parametric alternative
kruskal.test(EKL_Picea ~ hydrology, data = growth_class)

pairwise.wilcox.test(growth_class$EKL_Picea, growth_class$hydrology, 
                     p.adjust.method = "holm")

# --------- 7.1.2.2.4 Pinus  ---------------------------------------------------

# test for ANOVA requirements 
# (normal distribution and homoscedasticity of residuals)

#anova_growth_pinus_h <- aov(EKL_Pinus ~ hydrology, data = growth_class) # fit of ANOVA model

#shapiro.test(residuals(anova_growth_pinus_h)) # test on normal distribution of the models residuals

#leveneTest(anova_growth_pinus_h) # test on homoscedasticity of residuals

# ANOVA
#lm<-lm(growth_class$EKL_Pinus~growth_class$hydrology) # application of a linear model on the species richness data
#shapiro.test(lm$residuals) # test for normal distribution of the residuals

#anova(lm)

# Tukey test as Posthoc test
#(tukey <- glht(anova_growth_pinus_h, linfct = mcp(hydrology = "Tukey")))
#(result <- cld(tukey, level = 0.05))

# non-parametric alternative
# kruskal.test(EKL_Pinus ~ hydrology, data = growth_class)

# pairwise.wilcox.test(growth_class$EKL_Pinus, growth_class$hydrology, 
#                      p.adjust.method = "holm")


# ------- 7.1.3 Linear Mixed Effect Models  ------------------------------------

FG_lmem_tree = lmer(EKL ~ treespec +
                      (1 + treespec | hydrology) +
                      (1 + treespec | peatland_type),
                    data = gcl, REML = FALSE)

summary(FG_lmem_tree)

FG_lmem_tree
coef(FG_lmem_tree)


LMEM_FG_tree <- coefplot(model = FG_lmem_tree, color = "black") +
  theme_bw() +
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank()) +
  theme (axis.title.y  = element_blank()) +
  theme (axis.text.y  = element_text(size=14)) +
  scale_y_discrete(name="",labels=c("Alder", "Sandy birch", "Downy birch", "Pine","Spruce")) +
  theme(axis.title.x = element_text(size=16)) +
  theme(axis.text.x  = element_text(size=14)) +
  scale_x_continuous(name="Regression Estimate") +
  labs(title = "")

LMEM_FG_tree$layers[[2]] <- geom_errorbarh(stat="identity", position = "identity", na.rm = FALSE,
                                           color = "#DC9C34", linewidth = 1,
                                           mapping = aes(xmin = LowOuter, xmax = HighOuter,
                                                         height = 0, linetype = Model))
LMEM_FG_tree$layers[[3]] <- geom_errorbarh(stat="identity", position = "identity", na.rm = FALSE,
                                           color = "#70AD47", linewidth = 2,
                                           mapping = aes(xmin = LowInner, xmax = HighInner,
                                                         height = 0, linetype = Model))

LMEM_FG_tree
ggsave("Images/LMEM_FSI_tree_species.jpg")

# ------ 7.2 Forest structure indeces ------------------------------------------
# ------- 7.2.1 t.tests --------------------------------------------------------

## compare SR indices for forest inventory and vegetation survey

# test for normal distribution
shapiro.test(SR$SR_index)
shapiro.test(SR2$SR2_index)
hist(SR$SR_index)
hist(SR2$SR2_index)

# Welch-test because not normal distributed
t.test(SR$SR_index, SR2$SR2_index, paired = F)


## compare SRreg indices for regeneration survey and vegetation survey

# test for normal distribution
shapiro.test(SRreg$SRreg_index)
shapiro.test(SRreg2$SRreg2_index)
hist(SRreg$SRreg_index)
hist(SRreg2$SRreg2_index)

# Welch-test because not normal distributed
t.test(SRreg$SRreg_index, SRreg2$SRreg2_index, paired = F)

# ------- 7.2.2 ANOVA  ---------------------------------------------------------

# Significance tests #
hist(Variable_Indeces$FSI) # visual test on normal distribution

shapiro.test(Variable_Indeces$FSI) # mathematical test on normal distribution

Variable_Indeces$FSI <- as.numeric(Variable_Indeces$FSI)
Variable_Indeces$tree_spec <- as.factor(Variable_Indeces$tree_spec)
Variable_Indeces$hydrology <- as.factor(Variable_Indeces$hydrology)
Variable_Indeces$peatland_type <- as.factor(Variable_Indeces$peatland_type)

# -------- 7.2.2.1 tree species  -----------------------------------------------
# test for ANOVA requirements 
# (normal distribution and homoscedasticity of residuals)
anova_FSI_tree_spec <- aov(FSI ~ tree_spec, data = Variable_Indeces) # fit of ANOVA model

shapiro.test(residuals(anova_FSI_tree_spec)) # test on normal distribution of the models residuals

leveneTest(anova_FSI_tree_spec) # test on homoscedasticity of residuals

# ANOVA
lm<-lm(Variable_Indeces$FSI~Variable_Indeces$tree_spec) # application of a linear model on the species richness data
shapiro.test(lm$residuals) # test for normal distribution of the residuals

anova(lm)

# Tukey test as Posthoc test
(tukey <- glht(anova_FSI_tree_spec, linfct = mcp(tree_spec = "Tukey")))
(result <- cld(tukey, level = 0.05))


# non-parametric alternative
kruskal.test(FSI ~ tree_spec, data = Variable_Indeces)

pairwise.wilcox.test(Variable_Indeces$FSI, Variable_Indeces$tree_spec, p.adjust.method = "holm")


# -------- 7.2.2.2 peatland types  ---------------------------------------------
# test for ANOVA requirements 
# (normal distribution and homoscedasticity of residuals)
anova_FSI_peatland_types <- aov(FSI ~ peatland_type, data = Variable_Indeces) # fit of ANOVA model

shapiro.test(residuals(anova_FSI_peatland_types)) # test on normal distribution of the models residuals

leveneTest(anova_FSI_peatland_types) # test on homoscedasticity of residuals

# ANOVA
lm<-lm(Variable_Indeces$FSI~Variable_Indeces$peatland_type) # application of a linear model on the species richness data
shapiro.test(lm$residuals) # test for normal distribution of the residuals

anova(lm)

# Tukey test as Posthoc test
(tukey <- glht(anova_FSI_peatland_types, linfct = mcp(peatland_type = "Tukey")))
(result <- cld(tukey, level = 0.05))


# non-parametric alternative
kruskal.test(FSI ~ peatland_type, data = Variable_Indeces)

pairwise.wilcox.test(Variable_Indeces$FSI, Variable_Indeces$peatland_type, p.adjust.method = "holm")

# -------- 7.2.2.3 hydrology  --------------------------------------------------
# test for ANOVA requirements 
# (normal distribution and homoscedasticity of residuals)
anova_FSI_hydrology <- aov(FSI ~ hydrology, data = Variable_Indeces) # fit of ANOVA model

shapiro.test(residuals(anova_FSI_hydrology)) # test on normal distribution of the models residuals

leveneTest(anova_FSI_hydrology) # test on homoscedasticity of residuals

# ANOVA
lm<-lm(Variable_Indeces$FSI~Variable_Indeces$hydrology) # application of a linear model on the species richness data
shapiro.test(lm$residuals) # test for normal distribution of the residuals

anova(lm)

# Tukey test as Posthoc test
(tukey <- glht(anova_FSI_hydrology, linfct = mcp(hydrology = "Tukey")))
(result <- cld(tukey, level = 0.05))


# non-parametric alternative
kruskal.test(FSI ~ hydrology, data = Variable_Indeces)

pairwise.wilcox.test(Variable_Indeces$FSI, Variable_Indeces$hydrology, p.adjust.method = "holm")

# ------- 7.2.3 Linear Mixed Effect Models  ------------------------------------
# -------- 7.2.3.1 tree species ------------------------------------------------        

FSI_lmem_tree = lmer(FSI ~ tree_spec +
                       (1 + tree_spec | hydrology) +
                       (1 + tree_spec | peatland_type),
                     data = Variable_Indeces, REML = FALSE)

summary(FSI_lmem_tree)

FSI_lmem_tree
coef(FSI_lmem_tree)


LMEM_FSI_tree <- coefplot(model = FSI_lmem_tree, color = "black") +
  theme_bw() +
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank()) +
  theme (axis.title.y  = element_blank()) +
  theme (axis.text.y  = element_text(size=14)) +
  scale_y_discrete(name="",labels=c("Alder", "Birch", "Pine","Spruce")) +
  theme(axis.title.x = element_text(size=16)) +
  theme(axis.text.x  = element_text(size=14)) +
  scale_x_continuous(name="Regression Estimate") +
  labs(title = "")

LMEM_FSI_tree$layers[[2]] <- geom_errorbarh(stat="identity", position = "identity", na.rm = FALSE,
                                            color = "#DC9C34", linewidth = 1,
                                            mapping = aes(xmin = LowOuter, xmax = HighOuter,
                                                          height = 0, linetype = Model))
LMEM_FSI_tree$layers[[3]] <- geom_errorbarh(stat="identity", position = "identity", na.rm = FALSE,
                                            color = "#70AD47", linewidth = 2,
                                            mapping = aes(xmin = LowInner, xmax = HighInner,
                                                          height = 0, linetype = Model))

LMEM_FSI_tree
ggsave("Images/LMEM_FSI_tree_species.jpg")

# -------- 7.2.3.2 peatland type ----------------------------------------------- 

FSI_lmem_peatland = lmer(FSI ~ peatland_type +
                           (1 + peatland_type | tree_spec) +
                           (1 + peatland_type | hydrology),
                         data = Variable_Indeces, REML = FALSE)

summary(FSI_lmem_peatland)

FSI_lmem_peatland
coef(FSI_lmem_peatland)


LMEM_FSI_peatland <- coefplot(model = FSI_lmem_peatland, color = "black") +
  theme_bw() +
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank()) +
  theme (axis.title.y  = element_blank()) +
  theme (axis.text.y  = element_text(size=14)) +
  scale_y_discrete(name="",labels=c("bog", "fen", "gley bog")) +
  theme(axis.title.x = element_text(size=16)) +
  theme(axis.text.x  = element_text(size=14)) +
  scale_x_continuous(name="Regression Estimate") +
  labs(title = "")

LMEM_FSI_peatland$layers[[2]] <- geom_errorbarh(stat="identity", position = "identity", na.rm = FALSE,
                                                color = "#DC9C34", linewidth = 1,
                                                mapping = aes(xmin = LowOuter, xmax = HighOuter,
                                                              height = 0, linetype = Model))
LMEM_FSI_peatland$layers[[3]] <- geom_errorbarh(stat="identity", position = "identity", na.rm = FALSE,
                                                color = "#70AD47", linewidth = 2,
                                                mapping = aes(xmin = LowInner, xmax = HighInner,
                                                              height = 0, linetype = Model))

LMEM_FSI_peatland
ggsave("Images/LMEM_FSI_peatland_type.jpg")

# -------- 7.2.3.3 hydrology --------------------------------------------------- 

FSI_lmem_hydro = lmer(FSI ~ hydrology +
                        (1 + hydrology | tree_spec) +
                        (1 + hydrology | peatland_type),
                      data = Variable_Indeces, REML = FALSE)

summary(FSI_lmem_hydro)

FSI_lmem_hydro
coef(FSI_lmem_hydro)


LMEM_FSI_hyrdo <- coefplot(model = FSI_lmem_hydro, color = "black") +
  theme_bw() +
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank()) +
  theme (axis.title.y  = element_blank()) +
  theme (axis.text.y  = element_text(size=14)) +
  scale_y_discrete(name="",labels=c("drained", "wet")) +
  theme(axis.title.x = element_text(size=16)) +
  theme(axis.text.x  = element_text(size=14)) +
  scale_x_continuous(name="Regression Estimate") +
  labs(title = "")

LMEM_FSI_hyrdo$layers[[2]] <- geom_errorbarh(stat="identity", position = "identity", na.rm = FALSE,
                                             color = "#DC9C34", linewidth = 1,
                                             mapping = aes(xmin = LowOuter, xmax = HighOuter,
                                                           height = 0, linetype = Model))
LMEM_FSI_hyrdo$layers[[3]] <- geom_errorbarh(stat="identity", position = "identity", na.rm = FALSE,
                                             color = "#70AD47", linewidth = 2,
                                             mapping = aes(xmin = LowInner, xmax = HighInner,
                                                           height = 0, linetype = Model))

LMEM_FSI_hyrdo
ggsave("Images/LMEM_FSI_hydrology.jpg")






# -------- 7.2.3.4 sonstiges --------------------------------------------------- 

FSI_lmem = lmer(FSI ~ tree_spec +
                  (1 + tree_spec | peatland_type) +
                  (1 + tree_spec | hydrology),
                data = Variable_Indeces, REML = FALSE)

summary(FSI_lmem)

FSI_lmem
coef(FSI_lmem)


LMEM_FSI <- coefplot(model = FSI_lmem, color = "black") +
  theme_bw() +
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank()) +
  theme (axis.title.y  = element_blank()) +
  theme (axis.text.y  = element_text(size=14)) +
  scale_y_discrete(name="",labels=c("Alder", "Birch", "Pine","Spruce")) +
  theme(axis.title.x = element_text(size=16)) +
  theme(axis.text.x  = element_text(size=14)) +
  scale_x_continuous(name="Regression Estimate") +
  labs(title = "")

LMEM_FSI$layers[[2]] <- geom_errorbarh(stat="identity", position = "identity", na.rm = FALSE,
                                       color = "#DC9C34", linewidth = 1,
                                       mapping = aes(xmin = LowOuter, xmax = HighOuter,
                                                     height = 0, linetype = Model))
LMEM_FSI$layers[[3]] <- geom_errorbarh(stat="identity", position = "identity", na.rm = FALSE,
                                       color = "#70AD47", linewidth = 2,
                                       mapping = aes(xmin = LowInner, xmax = HighInner,
                                                     height = 0, linetype = Model))

LMEM_FSI
ggsave("Images/LMEM_FSI.jpg")

# -------- 7.2.3.5 sonstiges --------------------------------------------------- 

FSI_lmem = lmer(FSI ~ tree_spec +
                  (1 + tree_spec | peatland_type) +
                  (1 + tree_spec | hydrology),
                data = Variable_Indeces, REML = FALSE)

summary(FSI_lmem)

FSI_lmem
coef(FSI_lmem)


LMEM_FSI <- coefplot(model = FSI_lmem, color = "black") +
  theme_bw() +
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank()) +
  theme (axis.title.y  = element_blank()) +
  theme (axis.text.y  = element_text(size=14)) +
  scale_y_discrete(name="",labels=c("Alder", "Birch", "Pine","Spruce")) +
  theme(axis.title.x = element_text(size=16)) +
  theme(axis.text.x  = element_text(size=14)) +
  scale_x_continuous(name="Regression Estimate") +
  labs(title = "")

LMEM_FSI$layers[[2]] <- geom_errorbarh(stat="identity", position = "identity", na.rm = FALSE,
                                       color = "#DC9C34", linewidth = 1,
                                       mapping = aes(xmin = LowOuter, xmax = HighOuter,
                                                     height = 0, linetype = Model))
LMEM_FSI$layers[[3]] <- geom_errorbarh(stat="identity", position = "identity", na.rm = FALSE,
                                       color = "#70AD47", linewidth = 2,
                                       mapping = aes(xmin = LowInner, xmax = HighInner,
                                                     height = 0, linetype = Model))

LMEM_FSI
ggsave("Images/LMEM_FSI_Biomass.jpg")
# ------- 7.2.4 Discriminant Analysis ------------------------------------------

# probably doesn't make any sense when prediction variables are not numeric


pH %>% 
  ggplot(aes(soil_depth, pH_value, col = MoMoK_Nr, linetype = method)) +
  geom_point() +
  geom_line() +
  scale_color_manual(name = "MoMoK_Nr", values = c("yellow4", "violetred4", "steelblue4", "springgreen4", "tomato4",
                                                   "thistle4", "slateblue4", "turquoise3", "peru", "palegreen3",
                                                   "red3", "rosybrown", "royalblue", "black", "grey39",
                                                   "orange", "navajowhite2", "lightsteelblue2", "lightgoldenrod2")) +
  xlab("Soil depth [cm]") +
  ylab("pH-value") +
  theme(axis.title.x = element_text(size = 14)) +
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title.y = element_text(size = 14)) +
  theme(axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 11)) +
  theme(legend.text = element_text(size = 9)) +
  theme(panel.background = NULL)
ggsave("Images/pH-value.png", dpi = 300)
