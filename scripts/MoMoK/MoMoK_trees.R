# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the peat land soil inventory
# Trees 


# ----- 0. SETUP ---------------------------------------------------------------
# ----- 0.1. Packages  ---------------------------------------------------------
## datamanagement
# install.packages("usethis")
# install.packages("here")
# install.packages("readr")
# install.packages("tidyverse")
# install.packages("tibble")
# install.packages("dplyr")
# install.packages("data.table")
# install.packages("broom")
# install.packages("purrr")
# ## laTex
# install.packages("stargazer")  #for compatability with Latex
# install.packages("tikzDevice") #for compatability with Latex
# # visualisation
# install.packages("ggthemes")
# install.packages("ggplot2")
# install.packages("reshape2") #for multiple y values
# install.packages("ggforce") #for zooming in parts of the plot
# options(tz="CA")
# install.packages("reshape2")
# # analysis
# install.packages("corrplot")
# install.packages("AICcmodavg")
# # forest related
# install.packages("forestmangr")
# install.packages("rBDAT")
# install.packages("TapeR")
# install.packages("pkgbuild")
# if (! require("remotes")) 
#   install.packages("remotes")
# remotes::install_gitlab("vochr/TapeS", build_vignettes = TRUE)


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
# forest related
library("forestmangr")
library("rBDAT")
library("TapeR")
library("TapeS")
require(TapeS)
vignette("tapes", package = "TapeS")

# ----- 0.3. working directory -------------------------------------------------
here::here()
getwd()

# ----- 1. DATA ----------------------------------------------------------------
# ----- 1.1. import ------------------------------------------------------------
# TREES
# as the CSVs come from excel with German settings, the delimiter is ';' and the decimals are separated by ','
# which is why I use "delim" to import the data: https://biostats-r.github.io/biostats/workingInR/005_Importing_Data_in_R.html
trees_total <- read.delim(file = here("data/input/MoMoK/trees_MoMoK_total.csv"), sep = ";", dec = ",") %>% 
  select(-Bemerkung) %>% 
  filter(!is.na("MoMoK_Nr"))
# this table displaying the species codes and names used for the MoMoK forest inventory was extracted from the latest working paper published in the MoMok folder:  
# \\fswo01-ew\INSTITUT\a7forum\LEVEL I\BZE\Moormonitoring\Arbeitsanleitungen\MoMoK
# I´l use it to assign the latin names to the assessed speices to then use them in TapeR and BDAT 
SP_names <- read.delim(file = here("data/input/BZE2_HBI/x_bart_neu.csv"), sep = ";", dec = ",") %>% 
  select(- c(anmerkung, beginn, ende)) %>% 
  # https://stackoverflow.com/questions/21003311/how-to-combine-multiple-character-columns-into-a-single-column-in-an-r-data-fram
  unite(bot_name, genus, species, sep = " ", remove = FALSE) %>%  # creating one column with complete botanic name
  mutate(bot_name = ifelse(bot_name == "-2 -2", -2, bot_name))   # the error codes are joined in one column too, which i don´t want, so I´ll keep them single
SP_TapeS <- TapeS::tprSpeciesCode(inSp = NULL, outSp = NULL)
SP_TapeS_test <- TapeS::tprSpeciesCode(inSp = NULL, outSp = NULL) #to test if species codes correspong between TapeS dataset and SP_names from BZE 

#DEADWOOD
DW_total <- read.delim(file = here("data/input/MoMoK/DW_MoMoK_total.csv"), sep = ";", dec = ",", stringsAsFactors=FALSE)# %>% 
  #mutate(tpS_ID = NA) # this is just for the function, let´s see if it works



# ----- 1.2. colnames, vector type ---------------------------------------------
colnames(trees_total) <- c("plot_ID", "loc_name", "state", "date", "CCS_nr", 
                           "t_ID", "st_ID", "pieces", "SP_nr", "SP_code", "C_layer", 
                           "Kraft", "age", "age_m", "DBH_mm", "DBH_h_cm", 
                           "DBH_p_mm", "DBH_class", "H_dm", "CH_dm", 
                           "azimut_g", "azimut_d", "dist_m")
trees_total$C_layer <- as.numeric(trees_total$C_layer)
#trees_total$Kraft <- as.numeric(trees_total$Kraft)
trees_total$SP_code[trees_total$SP_code == "RER"] <- "SER"
trees_total$SP_code <- as.factor(trees_total$SP_code)

colnames(SP_names) <- c("Nr_code", "Chr_code_ger", "name", "bot_name", "bot_genus", 
                        "bot_species", "Flora_EU", "LH_NH", "IPC", "WZE", "BWI",  
                        "BZE_al")
colnames(DW_total) <- c("plot_ID", "loc_name", "state", "date", "CCS_nr", "t_ID",
                        "SP_group", "DW_type", "L_dm", "D_cm", "dec_type")
# changing DW variable D_cm from character itno numeric variable
# https://stackoverflow.com/questions/11936339/replace-specific-characters-within-strings
DW_total$D_cm <- gsub(",", ".", DW_total$D_cm)
DW_total$D_cm <- as.numeric(DW_total$D_cm)
DW_total <- DW_total %>% filter(!is.na(D_cm))
# SP_group = Baumartengruppe totholz
# DW_type = standing, lying
# dec_type = decay type / Zersetzungsgrad



# ---- 1.3 functions ------------------------------------------------------
# ---- 1.3.1. circle ------------------------------------------------------
# area of a circle
c_A = function(r){
  circle_area <- r^2*pi
  return(circle_area)}

# ---- 1.3.2. HEIGHTS ------------------------------------------------------
# ---- 1.3.2.1. height coefficient selection ------------------------------------------------------
# this function is used to select the coefficients of the height models depending on the R2
# for x, y,a, b (can be whatever)
f = function(x,y,a,b){
  # do the following: if x is na, or x is smaller then y, then use a, if not use b 
  answer <- ifelse(is.na(x)| x < y, a, b)
  return(answer)}


# ---- 1.3.2.2. einheitshöhenkurve------------------------------------------------------
# ---- 1.3.2.2.1. Sloboda ------------------------------------------------------
ehk_sloboda <- function(spec, d_i, d_mean, d_g, h_g) { #, id_broken) {
  k0 <- c(fi = 0.183, ta = 0.097, dgl = 0.24, ki = 0.29, lae = 0.074, bu = 0.032, ei = 0.102, alh = 0.122, aln = 0.032)
  k1 <- c(fi = 5.688, ta = 3.992, dgl = 6.033, ki = 1.607, lae = 3.692, bu = 6.04, ei = 3.387, alh = 5.04, aln = 4.24)
  k2 <- c(fi = 0.29, ta = 0.317, dgl = 0.33, ki = 0.388, lae = 0.342, bu = 0.367, ei = 0.488, alh = 0.47, aln = 0.461)
  h_mean <- (h_g - 1.3)/(exp(k0[tolower(spec)]*(1 - d_mean/d_g))*exp(k1[tolower(spec)]*(1/d_mean - 1/d_g))) + 1.3;
  h_pred <- ((1.3 + (h_mean - 1.3)*exp(k0[tolower(spec)]*(1 - d_mean/d_i))*exp(k1[tolower(spec)]*(1/d_mean - 1/d_i)))/10); # divide by 10 to get height in m
  # this part is silenced, because there is no Höhenkennzahl documented for MoMoK 
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

# ---- 1.3.2.2.2. Curtis ------------------------------------------------------
# --> this one is only applied when there is literally not information to calculate the height, 
# except of the diameter
h_curtis <- function(spec, d) {
  b0 <- c(fi = 434.1235, bu = 382.0202, ta = 453.5538, ki = 359.7162, lae = 421.4473, dgl = 481.5531, ei = 348.3262);
  b1 <- c(fi = -65586.6915, bu = -51800.9382, ta = -81132.5221, ki = -42967.9947, lae = -60241.2948, dgl = -81754.2523, ei = -46547.3645);
  b2 <- c(fi = 3074967.1738, bu = 2374368.3254, ta = 4285801.5636, ki = 1763359.9972, lae = 2895409.6245, dgl = 4193121.2406, ei = 2119420.9444);
  
  return((b0[tolower(spec)] + b1[tolower(spec)]*1/d + b2[tolower(spec)]*1/d^2)/10)   # divide by 10 to transform dm into meters
}


# ---- 1.3.2.3. self-fitted nls models ------------------------------------------------------
# ---- 1.3.2.3.1. species- & plot-wise self-fitted nls models ------------------------------------------------------
# self made nls models for heights per species across all plots
h_nls_SP <- function(spec, d){
  # https://statisticsglobe.com/convert-data-frame-column-to-a-vector-in-r
  b0 <- dplyr::pull(coeff_H_SP, b0, SP_code);
  b1 <- dplyr::pull(coeff_H_SP, b1, SP_code);
  b2 <- dplyr::pull(coeff_H_SP, b2, SP_code);
  return(b0[spec] * (1 - exp( -b1[spec] * d))^b2[spec])
}
# ---- 1.3.2.3.2. species-wise self-fitted nls models ------------------------------------------------------
# self mase nls models for heights per species per plot
h_nls_SP_P <- function(plot_spec, d) {
  # because I cannot combine 3 variabels in one vector, 
  b0 <- coeff_H_SP_P %>% unite(SP_P_ID, plot_ID, SP_code, sep = "", remove = FALSE) %>% dplyr::pull(b0, SP_P_ID);
  b1 <- coeff_H_SP_P %>% unite(SP_P_ID, plot_ID, SP_code, sep = "", remove = FALSE) %>% dplyr::pull(b1, SP_P_ID);
  b2 <- coeff_H_SP_P %>% unite(SP_P_ID, plot_ID, SP_code, sep = "", remove = FALSE) %>% dplyr::pull(b2, SP_P_ID);
  return(b0[plot_spec] * (1 - exp( -b1[plot_spec] * d))^b2[plot_spec])
}


# ---- 1.3.3. LIVING BIOMASS ----------------------------------------------------------
# ---- 1.3.3.1. total aboveground biomass --------------------------------------
## Total ABOVEGROUND = Stemmwood+bark + finewood+bark + foliage

# ---- 1.3.3.1.1.  GHG inventory (Dunger et al.) total above biomass --------------------------------------
# https://www.umweltbundesamt.de/sites/default/files/medien/1410/publikationen/2020-04-15-climate-change_23-2020_nir_2020_en_0.pdf
## aboveground biomass in kg per tree, for trees DBH > 10cm GHGI
    # where B = above-ground phytomass in kg per individual tree,
    # b0,1,2,3 and k1,2 = coefficients of the эarklund function,
    # DBH = Diameter at breast height in cm,
    # D03 = Diameter in cm at 30% of tree height,
    # H = tree height in m
Dunger_aB_DBHa10 <- function(spec, d, d03, h){
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
Dunger_aB_H1.3_DBHb10 <- function(spec, d){
  b0 <- c(fi = 0.41080, ki = 0.41080, bu = 0.09644 , ei= 0.09644, shw =0.09644);
  bs <- c(fi = 26.63122 , ki = 19.99943 , bu = 33.22328, ei= 28.94782, shw =16.86101);
  b3 <- c(fi = 0.01370, ki = 0.00916, bu = 0.01162, ei= 0.01501, shw = -0.00551);
  ds <- c(fi = 10, ki = 10, bu = 10, ei= 10, shw =10);
  return(b0[spec]+(((bs[spec] - b0[spec])/ds[spec]^2)+b3[spec]*(d-ds[spec]))*d^2)
}
## above ground biomass for trees <1.3m GHGI
Dunger_aB_Hb1.3 <- function(spec, h){  # here instead of species group i´ll link the formula to a column with he categories broadleafed and coniferous trees
  b0 <- c(NB = 0.23059, LB = 0.04940);
  b1 <- c(NB = 2.20101, LB = 2.54946);
  return(b0[spec]*h^b1[spec])
}


# ---- 1.3.3.1.1. TapeS total above biomass - all -------------------------------------------
# Kändler, G. and B. Bösch (2012). Methodenentwicklung für die 3. Bundeswaldinventur: Modul 3 
# Überprüfung und Neukonzeption einer Biomassefunktion - Abschlussbericht. Im Auftrag des 
# Bundesministeriums für Ernährung, Landwirtschaft und Verbraucherschutz in Zusammenarbeit 
# mit dem Institut für Waldökologie und Waldinventur des Johann Heinrich von Thünen-Instituts, FVA-BW: 71.

tapes_aB <- function(spec_tpS, d, dh, h){         
  spp = na.omit(trees_total_5 %>% dplyr::pull(tpS_ID)); 
  Dm = na.omit(as.list(trees_total_5 %>% dplyr::pull(DBH_cm)));
  Hm = na.omit(as.list(trees_total_5 %>% mutate(D_h_m = (ifelse(is.na(DBH_h_cm), 130, DBH_h_cm))/100) %>% dplyr::pull(D_h_m))); # height at which diameter was taken, has to be 1.3m becaus ehtese are the deadwood pieces that do stil have a DBH
  Ht = na.omit(trees_total_5 %>% dplyr::pull(H_m));
  obj.tbio <- tprTrees(spp, Dm, Hm, Ht, inv = 4);
  return (tprBiomass(obj.tbio, component="all"))
}


# ---- 1.3.3.2. coarsewood biomass without bark ----------------------------------------------
# ---- 1.3.3.2.1. Wirth coarsewood biomass  without bark-----------------------------------------
# ---- 1.3.3.2.2. Wutzler coarsewood biomass  without bark---------------------------------------
# ---- 1.3.3.2.3. Vondernach coarsewood biomass  without bark - Dh ------------------------------------
Vondr_DhB <- function(spec, d, h){
  b0 <- c(fi = 0, ta = 0, dgl = 0, ki = 0, bu = -5.6602, ei = -5.9489, es=0, ah=0); # für alle auser buche 0
  b1 <- c(fi = 0.0157, ta = 0.0074, dgl = 0.0128, ki=0.0169, bu=0.022, ei=0.0257, es=0.0128, ah=0.028 );
  b2 <- c(fi = 1.735, ta = 1.6476, dgl = 1.9541, ki=1.9894, bu=2.0971, ei=2.0738, es=1.9623, ah=2.1304);
  b3 <- c(fi = 1.2177, ta = 1.5543, dgl = 1.0539, ki=0.9378, bu=0.8957, ei=0.8508, es=1.1824, ah=0.7078);
  return(b0[spec] + b1[spec]* d^b2[spec] * h^b3[spec])
}


# ---- 1.3.3.2.4. TapeS coarsewood biomass without bark - sw -----------------------------------------
tapes_StB <- function(spec_tpS, d, dh, h){         
  spp = na.omit(trees_total_5 %>% dplyr::pull(tpS_ID)); 
  Dm = na.omit(as.list(trees_total_5 %>% dplyr::pull(DBH_cm)));
  Hm = na.omit(as.list(trees_total_5 %>% mutate(D_h_m = (ifelse(is.na(DBH_h_cm), 130, DBH_h_cm))/100) %>% dplyr::pull(D_h_m))); # height at which diameter was taken, has to be 1.3m becaus ehtese are the deadwood pieces that do stil have a DBH
  Ht = na.omit(trees_total_5 %>% dplyr::pull(H_m));
  obj.tbio <- tprTrees(spp, Dm, Hm, Ht, inv = 4);
  return (tprBiomass(obj.tbio, component="sw") + tprBiomass(obj.tbio, component="stw")) # stem + stumb
}

# ---- 1.3.3.3. coarsewood bark -------------------------------------------------
# ---- 1.3.3.3.1. Wirth coarsewood bark -----------------------------------------
# ---- 1.3.3.3.2. Wutzler coarsewood bark ---------------------------------------
# ---- 1.3.3.3.3. Vondernach coarsewood bark R ------------------------------------
Vondr_DhRB <- function(spec, d, h){
  b0 <- c(fi = 0, ta = 0, dgl = 0, ki = 0, bu = 0, ei =0, es=0, ah=0.5195); # für alle auser buche 0
  b1 <- c(fi = 0.0042, ta = 0.0017, dgl = 0.0027, ki=0.0044, bu=0.0017, ei=0.006, es=0.001, ah=0.004 );
  b2 <- c(fi = 1.6026, ta = 1.304, dgl = 1.8296, ki=1.9594, bu=2.0245, ei=2.0101, es=1, ah=2.068);
  b3 <- c(fi = 1.0239, ta = 1.8956, dgl = 1.0032, ki=0.6641, bu=0.9396, ei=0.778, es=1.6592, ah=0.6965);
  return(b0[spec] + b1[spec]* d^b2[spec] * h^b3[spec])
}

# ---- 1.3.3.3.4. TapeS coarsewood bark "sb" + "stb" -----------------------------------------
tapes_StbB <- function(spec_tpS, d, dh, h){         
  spp = na.omit(trees_total_5 %>% dplyr::pull(tpS_ID)); 
  Dm = na.omit(as.list(trees_total_5 %>% dplyr::pull(DBH_cm)));
  Hm = na.omit(as.list(trees_total_5 %>% mutate(D_h_m = (ifelse(is.na(DBH_h_cm), 130, DBH_h_cm))/100) %>% dplyr::pull(D_h_m))); # height at which diameter was taken, has to be 1.3m becaus ehtese are the deadwood pieces that do stil have a DBH
  Ht = na.omit(trees_total_5 %>% dplyr::pull(H_m));
  obj.tbio <- tprTrees(spp, Dm, Hm, Ht, inv = 4);
  return (tprBiomass(obj.tbio, component="sb")+ tprBiomass(obj.tbio, component="stb"))
}

# ---- 1.3.4.4. stemmwood biomass with bark ----------------------------------------------
# ---- 1.3.4.4.1. Wirth stemmwood biomass with bark-----------------------------------------
# ---- 1.3.4.4.2. Wutzler stemmwood biomass with bark---------------------------------------
# ---- 1.3.4.4.3. Vondernach stemmwood biomass with bark - Dh + DhR ------------------------------------
# ---- 1.3.4.4.4. TapeS stemmwood biomass with bark - sw+sb -----------------------------------------
tapes_StbB <- function(spec_tpS, d, dh, h){         
  spp = na.omit(trees_total_5 %>% dplyr::pull(tpS_ID)); 
  Dm = na.omit(as.list(trees_total_5 %>% dplyr::pull(DBH_cm)));
  Hm = na.omit(as.list(trees_total_5 %>% mutate(D_h_m = (ifelse(is.na(DBH_h_cm), 130, DBH_h_cm))/100) %>% dplyr::pull(D_h_m))); # height at which diameter was taken, has to be 1.3m becaus ehtese are the deadwood pieces that do stil have a DBH
  Ht = na.omit(trees_total_5 %>% dplyr::pull(H_m));
  obj.tbio <- tprTrees(spp, Dm, Hm, Ht, inv = 4);
  return ((tprBiomass(obj.tbio, component="sw")) + (tprBiomass(obj.tbio, component="sb")))
}

# ---- 1.3.3.5. foliage biomass ------------------------------------------------
### FOLIAGE
# according to T. Riedels advice the foliage biomass for coniferous trees is calculated by 
# groups of conferous vs. broadleafed trees:
# Wirth et al. (2004) (https://www.researchgate.net/publication/8959167_Generic_biomass_functions_for_Norway_spruce_in_Central_Europe_-_A_meta-analysis_approach_toward_prediction_and_uncertainty_estimation) 
# for coniferous trees, whereby the models are actually fit for Picea abies, but in our case all coniferous trees are treated/ calculated as Picea abies
# Wutzler et al. (2008) (https://www.researchgate.net/profile/Christian-Wirth-4/publication/42089705_Generic_biomass_functions_for_Common_beech_Fagus_sylvatica_in_Central_Europe_Predictions_and_components_of_uncertainty/links/56195fe008aea80367203191/Generic-biomass-functions-for-Common-beech-Fagus-sylvatica-in-Central-Europe-Predictions-and-components-of-uncertainty.pdf?origin=publication_detail)
# for broadleaved trees, whereby the models are actually fit for Fagus silvatica, but in our case all broadleaved trees are treated/ calculated as Fagus sylvatica

# ---- 1.3.3.5.1. Wirth foliage (coniferous)-------------------------------
### foliage coniferous trees
 # lnW = b0 + b1*ln(D) + b2*(ln(D))^2 + b3*ln(H) + b4*(ln(H))^2 + b5*ln(A) + b6*HSL
 # lnW = b0 + b1*ln(D) + b2*(ln(D))^2 + b3*ln(H) + b4*(ln(H))^2 + b6*ln(A)
 # spec = species group, in this case CF/ BL (column: NH_LH), d = Diameter at breast height (cm), 
 # h = tree height (m), a = age (years), hsl = height above sea level (m)
Wirth_fB_N <- function(d, h, a){   # this is one of the lower ranked models, called DHA. the best would be DHAS which includes SI & HSL
  b0 = (-0.58133);
  b1 = 3.653845;
  b2 = (-0.21336);
  b3 = (-2.77755);
  b4 = 0.46540;
  b6 = (-0.42940);
  cf2 =  1.0183;         # correction factor 2 
  # in r ln is the same as log: https://stackoverflow.com/questions/24304936/r-using-equation-with-natural-logarithm-in-nls
  fB <- (exp(b0 + b1*log(d) + b2*(log(d))^2 + b3*log(h) + b4*(log(h))^2 + b6*log(a))*cf2);
  # as the inital LMER uses a natural logarithm on the outcome (Ln(W)) I´ll have to back transform it
  # https://www.geeksforgeeks.org/how-to-find-inverse-log-transformation-in-r/
  # y (foliage biomass) = ln (x --> whole formula of foliage)    ⇐⇒  e^y  = x
  return(fB)
}

# ---- 1.3.3.5.2. Wutzler foliage (broadleafed)-------------------------------
### foliage of broadleaved trees according to Wutzler et al. 2008
  # to aply this function the Oberhöhe and the elevation above sea level are required

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
          #biomass = (b0+0+bsage*age+bssi*si+bsalt*atitude)*(DBH^b1)*(H^b2)
  return(# so its either this: (b0 + 0 + bssi*SI + bsalt*alt)*d^b1*h^b2) 
         # or this from Mark: 
         (b0+bsalt*alt)*d^(b1+bssi*si)*h^b2)
}

#DH3 4a Model   
Wutzler_fB_L1 <- function(d, h){  #DH3 4a Model 
  b0 = 0.0377;
  b1 = 2.43;
  b2 = (-0.913);
  return(b0*d^b1*h^b2)
}


# ---- 1.3.3.5.3. Vondernach foliage - Nad ---------------------------------------





# ---- 1.3.3.5.4. tapeS foliage - ndl-------------------------------------------
# tapeS foliage
tapes_fB <- function(spec_tpS, d, dh, h){          
  spp = na.omit(trees_total_5 %>% dplyr::pull(tpS_ID));
  Dm = na.omit(as.list(trees_total_5 %>% dplyr::pull(DBH_cm)));
  Hm = na.omit(as.list(trees_total_5 %>% mutate(D_h_m = (ifelse(is.na(DBH_h_cm), 130, DBH_h_cm))/100) %>% dplyr::pull(D_h_m))); # height at which diameter 
  Ht = na.omit(trees_total_5 %>% dplyr::pull(H_m));
  obj.tbio <- tprTrees(spp, Dm, Hm, Ht, inv = 4);
  return (tprBiomass(obj.tbio, component="ndl"))
}


# ---- 1.3.3.6. fine branches --------------------------------------------------
# ---- 1.3.3.6.1. Wirth fine branches (coniferous) -------------------------------------------
### branches coniferous trees
Wirth_brB_N <- function(d, h, a){  # DHA
  b0 = -0.64565; 
  b1 = 2.85424;
  b2 = -2.98493;
  b3 = 0.41789;
  fbrB_N <- (b0+b1*log(d)+b2*log(h)+b3*(log(h))^2); #fresh branches DHA best base
  b4 = -1.21969;
  b5 = 1.49138;
  b6 = -1.286761;
  b7 = 0.18222;
  dbrB_N <- (b4+b5*log(d)+b6*log(h)+b7*(log(a)*log(d))); # dry brances DHA best base
  return(exp(fbrB_N)+exp(dbrB_N))
}

# ---- 1.3.3.6.2. Wutzler fine branches (broadleafed) -------------------------------------------
### branches broadleafed trees
Wutzler_brB_L1 <- function(d, h){  #DH3 4a Model 
  b0 = 0.123;
  b1 = 3.09;
  b2 = (-1.17);
  return(b0*d^b1*h^b2)
}

# ---- 1.3.3.6.3. TapeS fine branches - fwb -------------------------------------------
### tapeS fine branches 
tapes_brB <- function(spec_tpS, d, dh, h){          
  spp = na.omit(trees_total_5 %>% dplyr::pull(tpS_ID)); 
  Dm = na.omit(as.list(trees_total_5 %>% dplyr::pull(DBH_cm)));
  Hm = na.omit(as.list(trees_total_5 %>% mutate(D_h_m = (ifelse(is.na(DBH_h_cm), 130, DBH_h_cm))/100) %>% dplyr::pull(D_h_m))); # height at which diameter was taken, has to be 1.3m because these are the deadwood pieces that do still have a DBH
  Ht = na.omit(trees_total_5 %>% dplyr::pull(H_m));
  obj.tbio <- tprTrees(spp, Dm, Hm, Ht, inv = 4);
  return (tprBiomass(obj.tbio, component="fwb"))
}

# ---- 1.3.3.6.3. Vondernach fine branches - Ndh -------------------------------------------
Vondr_DhB <- function(spec, d, h){
  b0 <- c(fi = 0, ta = 0, dgl = 0, ki = 0, bu = -5.6602, ei = -5.9489, es=0, ah=0); # für alle auser buche 0
  b1 <- c(fi = 0.0157, ta = 0.0074, dgl = 0.0128, ki=0.0169, bu=0.022, ei=0.0257, es=0.0128, ah=0.028 );
  b2 <- c(fi = 1.735, ta = 1.6476, dgl = 1.9541, ki=1.9894, bu=2.0971, ei=2.0738, es=1.9623, ah=2.1304);
  b3 <- c(fi = 1.2177, ta = 1.5543, dgl = 1.0539, ki=0.9378, bu=0.8957, ei=0.8508, es=1.1824, ah=0.7078);
  return(b0[spec] + b1[spec]* d^b2[spec] * h^b3[spec])
}

# ---- 1.3.3.7. total bellowground biomass --------------------------------------
# ---- 1.3.3.7.1. according to GHG inventory --------------------------------------
## belowground phytomass GHGI
Dunger_bB <- function(spec, d){
  b0 <- c(fi = 0.003720, ki = 0.006089, bu = 0.018256, ei= 0.028000, shw = 0.000010);#shwr =0.000010, shwrs = 0.000116);
  b1 <- c(fi = 2.792465, ki = 2.739073, bu = 2.321997, ei= 2.440000, shw =2.529000); #shwr =2.529000, shwrs = 2.290300);
  return(ifelse(spec != "shw", b0[spec]*d^b1[spec], (b0[spec]*d^b1[spec])+(0.000116*d^2.290300))) 
}






# ---- 1.3.4. DEADWOOD BIOMASS ----------------------------------------------------------
# ---- 1.3.4.1. Volume Deadwood according to BWI ----------------------------------------------------------
# here we have to consider, that in case of MoMok there were no different types pf diameter taken
# e.g  min diameter, max diameter, middle diam
# the volume calautation follows the procedure described in BWI Methodikband, 

# volume for deadwood when 
    # Dm was taken (Mittendurchmesser) or 
    # Totholztyp == 3 (liegend, stark, Burchstück) & L_m <3m
V_DW_T1463 <- function(d, l){
  d <- DW_total %>% mutate(D_m = D_cm/100) %>% dplyr::pull(D_m);
  l <- DW_total %>% mutate(L_m = L_dm/10) %>% dplyr::pull(L_m);
  return(((d/2)^2*pi)*l)
}

# Volume for deadwood when 
   # !(DW_type %in% c(1, 6, 4) | DW_type == 3 & L_m > 3m)
V_DW_T253 <- function(spec_tpS, d, dh, l){          # I don´t know if this can work
  spp = na.omit(DW_total %>% filter(L_dm > 30) %>% dplyr::pull(tpS_ID)); # for this Ill first have to create species groups that correspond with TapeS
  Dm = na.omit(as.list(DW_total %>% filter(L_dm > 30) %>% dplyr::pull(D_cm)));
  Hm = na.omit(as.list(DW_total %>% filter(L_dm > 30) %>%  mutate(D_h_m = 1.3) %>% dplyr::pull(D_h_m))); # height at which diameter was taken, has to be 1.3m becaus ehtese are the deadwood pieces that do stil have a DBH
  Ht = na.omit(DW_total %>% filter(L_dm > 30) %>% mutate(L_m = L_dm/10) %>% dplyr::pull(L_m));
  obj.dw <- tprTrees(spp, Dm, Hm, Ht, inv = 4);
return (tprVolume(obj.dw[obj.dw@monotone == TRUE]))
}

# ---- 1.3.4.2. Biomass Deadwood according to BWI ----------------------------------------------------------
B_DW <- function(V, dec_SP){     # a column that holds the degree of decay and the species type has to be created (united)
  BEF <- c("2_1" = 0.372, "2_2" = 0.308, "2_3" = 0.141, "2_4" = 0.123,   # conferous trees according to Faver
           "1_1" = 0.58, "1_2" = 0.37, "1_3" = 0.21, "1_4" = 0.26,       # broadleaved trees according to Müller-Ursing
           "3_1" = 0.58, "3_2" = 0.37, "3_3" = 0.21, "3_4" = 0.26);      # oak
  return(V*BEF[dec_SP])
}

# ---- 1.3.4.3. Carbon deadwood according to IPCC default value from GHGI methodology 2006
C_DW <- function(V, dec_SP){   # a column that holds the degree of decay and the species type has to be created (united)
  BEF <- c("2_1" = 0.372, "2_2" = 0.308, "2_3" = 0.141, "2_4" = 0.123,   # conferous trees according to Faver
           "1_1" = 0.58, "1_2" = 0.37, "1_3" = 0.21, "1_4" = 0.26,       # broadleaved trees according to Müller-Ursing
           "3_1" = 0.58, "3_2" = 0.37, "3_3" = 0.21, "3_4" = 0.26);      # oak according to Müller-Ursing
  return(V*BEF[dec_SP]*0.5)   # defaul value for carbon content in deadwood = 0.5 according to IPCC GHG methodology 2006
}



# ----- 1.4. dealing with missing info ---------------------------------------------------
# check for variabels with NAs
summary(trees_total)
summary(DW_total) # there´s one D_cm that is NA because in the origianl dataset it´s called "s"--> I´ll exclude it

# ----- 1.4.1 assign DBH class to trees where DBH_class == 'NA' -----------------
# create label for diameter classes according to BZE3 Bestandesaufnahmeanleitung
labs <- c(seq(5, 55, by = 5)) 

# ----- 1.4.2. adding species names -----------------------------------------
# Goal 1: assiging the correct latin name to the individual trees through SP_names dataset
    # when trying to assign the correct latinn manes to the respective trees via their species code or species number 
    # it became evident that neither the areviations of the common species names, nor the species numbers correspond
    # further the species numbers are wronlgy assigned in the trees dataset
    # the most coherent or common variables appears to be the species common names abbreviations in the SP_names and trees_total dataset
    # though the character codes in the SP_names dataset are assessed in a mix of capital and not capital letters, 
    # while all abbreviations of common species names in the trees_total dataset are all in capital letters
    # thus, I´ll transform all the abbreviations of common species names in the SP_names dataset into capital letters, 
    # to enable the join.
# Goal 2: assingin the correct species codes from the TapeS SP file to trees_total to use TapeS later
    # the most correspondent variable/ column between TapeS and SP_names, and by that trees_total, which can access & join all SP_names columns 
    # but no or few tapeS_SP columns is the "BWI" column of SP_names and the "kurz" column of TapeS_SP when transformed into capital letters. 

# there was a mistake in the species codes as there was a confusion between the 
# german trivial names of schwarzerle (alnus glutinosa) and roterle (alnus rubra) which 
# whereby the first is sometimes also called roterle cause of the woods colour, however, 
# the distribution of alnus rubra extents mainly to north america so we can assume that those trees labbeled
# RER are actually supposed to be labelled SER


trees_total <- left_join(trees_total %>% 
                           #mutate(SP_code = ifelse(SP_code == "RER", "SER", SP_code)) %>% 
  # 1. replace missing DBH_class values with labels according to DBH_cm (1.4.1.)
                           mutate(H_m = H_dm*0.1,                        #transform height in dm into height in m 
                                  DBH_cm = DBH_mm*0.1) %>%               # transform DBH in mm into DBH in cm 
                           mutate(DBH_class = ifelse(is.na(DBH_class),   # mutate the column DBH_class if DBH_class is NA
                                                     cut(DBH_cm,         # cut the diameter
                                                         breaks = c(seq(5, 55, by = 5), Inf),  # in sequences of 5
                                                         labels = labs,                        # and label it according to labs (1.4.1)
                                                         right = FALSE),
                                                     as.numeric(DBH_class)),    # else keep existing DBH class as numeric
                                  BA_m2 = c_A(DBH_cm/2)*0.0001,                 # 0.0001 to change unit from cm2 to m2
                                  plot_A_ha = c_A(12.62)*0.0001,                # 0.0001 to change unit from m2 to hectare
                                  DBH_h_cm = ifelse(is.na(DBH_h_cm), 130, DBH_h_cm)) %>%   # dealing with missing DBH measurement heights by assuming if the height was not registered the DBH was taken at 1.3m stem height         
                           unite(ID_pt, plot_ID, t_ID, sep = "", remove = FALSE), # create unique tree ID from combination of plot and tree number for later work with TapeR & TapeS
   # 2. join the botanic names & German abbreviation into the tree data set 
                         SP_names %>% 
                           # because the number codes for the species in the trees_total dataset don´t correspond at 
                           # all with the SP_names codes, the German abbreviations are used for the join, as they are among all 
                           # avaiable codes the most coherent between the two datasets. 
                           # But because those are in capital and non-capital letters in the SP_names data set 
                           # I have to create a column where all of them are in capital letters, which then corresponds with how the species abbreviations were 
                           # documented for the MoMoK Bestandesaufnahme
                           # it seems, however that there´s also a column in the new, most recent species names dataframe of BZE, 
                           # which will probably allow to join the datasets just via the codes listed there as they are already in
                           # in capital letters
                           # https://www.datasciencemadesimple.com/convert-to-upper-case-in-r-dataframe-column-2/
                           mutate(Chr_ger_cap = toupper(Chr_code_ger), 
                                  # create column in SP_names that corresponds with TapeS species
                                  # --> the changes are carried out according to the anti join between trees_total & TapeS species, not
                                  # the join between SP_codes from BZE and TapeSP, this has to be done later
                                  tpS_com_ID = case_when(BWI == "KI" ~ 'KIE',
                                                         BWI == "ERL" ~ 'ER',
                                                        TRUE ~ BWI), 
                                  # create species groups, BWI uses for their volume calculations to use curtis & sloboda functions
                                  # BWI Methodikband: 
                                  # für die Höhenmessung wurde nach folgenden Baumartengruppen differenziert:
                                  # Fichte, Tanne, Douglasie, Kiefer, Lärche, Buche, Eiche. 
                                  # Alle anderen Nadelbäume werden der Fichte und alle anderen Laubbäume der Buche zugeordnet.
                                  H_SP_group = case_when(bot_genus == "Quercus"~ 'ei', 
                                                           LH_NH == "LB" & bot_genus != "Quercus" ~ 'bu', 
                                                           bot_genus == "Abies" ~ 'ta', 
                                                           bot_genus == "Pinus" ~ 'ki', 
                                                           bot_genus == "Pseudotsuga" ~ 'dgl',
                                                           LH_NH == "NB" & bot_genus == "Larix" ~ 'lae', 
                                                           TRUE ~ 'fi'),
                                  BWI_SP_group = case_when(LH_NH == "LB" & bot_genus == "Quercus"~ 'ei', 
                                                           LH_NH == "LB" & bot_genus == "Fagus"~ 'bu',
                                                           LH_NH == "LB" & bot_genus %in% c("Acer", 
                                                                                            "Platanus", 
                                                                                            "Fraxinus",
                                                                                            "Tilia", 
                                                                                            "Juglans", 
                                                                                            "Corylus", 
                                                                                            "Robinia", 
                                                                                            "Castanea", 
                                                                                            "Carpinus", 
                                                                                            "Aesculus", 
                                                                                            "Sorbus",
                                                                                            "Ulmus", 
                                                                                            "Rhamnus") | LH_NH == "LB" & bot_name == "Prunus dulcis" ~ 'aLh',
                                                           LH_NH == "LB" & !(bot_genus %in% c("Quercus", 
                                                                                              "Fagus",
                                                                                              "Acer", 
                                                                                              "Platanus", 
                                                                                              "Fraxinus",
                                                                                              "Tilia", 
                                                                                              "Juglans", 
                                                                                              "Corylus", 
                                                                                              "Robinia", 
                                                                                              "Castanea", 
                                                                                              "Carpinus", 
                                                                                              "Aesculus", 
                                                                                              "Sorbus",
                                                                                              "Ulmus", 
                                                                                              "Rhamnus")) | LH_NH == "LB" & bot_name != "Prunus dulcis" ~ 'aLn',
                                                           LH_NH == "NB" & bot_genus %in% c("Pinus", "Larix") ~ 'ki', 
                                                           LH_NH == "NB" & !(bot_genus %in% c("Pinus", "Larix"))  ~ 'fi', 
                                                           TRUE ~ 'other'), 
                                  Bio_SP_group = case_when(LH_NH == "LB" & bot_genus == "Quercus"~ 'ei',
                                                           # https://www.statology.org/not-in-r/
                                                           LH_NH == "LB" & bot_genus %in% c("Fagus",     # all species that are labelled "aLh" in the BWI are treated as  beech 
                                                                                            "Acer", 
                                                                                            "Platanus", 
                                                                                            "Fraxinus",
                                                                                            "Tilia", 
                                                                                            "Juglans", 
                                                                                            "Corylus", 
                                                                                            "Robinia", 
                                                                                            "Castanea", 
                                                                                            "Carpinus", 
                                                                                            "Aesculus", 
                                                                                            "Sorbus",
                                                                                            "Ulmus", 
                                                                                            "Rhamnus") | LH_NH == "LB" & bot_name == "Prunus dulcis" ~ 'bu',
                                                           LH_NH == "LB" & !(bot_genus %in% c("Quercus",  # all species that would be labelled "aLn" in the BWI species groups are allocated to soft hardwoods
                                                                                              "Fagus",
                                                                                              "Acer", 
                                                                                              "Platanus", 
                                                                                              "Fraxinus",
                                                                                              "Tilia", 
                                                                                              "Juglans", 
                                                                                              "Corylus", 
                                                                                              "Robinia", 
                                                                                              "Castanea", 
                                                                                              "Carpinus", 
                                                                                              "Aesculus", 
                                                                                              "Sorbus",
                                                                                              "Ulmus", 
                                                                                              "Rhamnus")) | LH_NH == "LB" & bot_name != "Prunus dulcis" ~ 'shw',
                                                           LH_NH == "NB" & bot_genus %in% c("Pinus", "Larix") ~ 'ki', 
                                                           LH_NH == "NB" & !(bot_genus %in% c("Pinus", "Larix"))  ~ 'fi', # all coniferous species that are not Pine or larch are treated as spruce
                                                           TRUE ~ 'other'))%>% 
                           dplyr::select(Chr_ger_cap, Chr_code_ger, bot_name, tpS_com_ID, H_SP_group,BWI_SP_group, LH_NH, BWI, Bio_SP_group), 
                         by = c("SP_code" = "Chr_ger_cap")) %>% 
  # 3. joing TapeS species codes via common SP_ID created above (tpS_com_ID)
  left_join(., SP_TapeS %>%                                          
              mutate(Chr_ger_cap = toupper(SP_TapeS$kurz)) %>%       # changing German abbreviations (kurz) into capital letters so it corresponds with BWI from SP_names
              rename(tpS_ID = ID) %>%                                # changing column name of species number codes in TapeS to avoid confusion with tree ID etc.
              select(Chr_ger_cap, tpS_ID), 
            by = c("tpS_com_ID" = "Chr_ger_cap")) %>% 
  select(-c(tpS_com_ID))                                         # kicking out variables only used for joining correct tree codes from TapeS
  




# checking if DBH_classs assignment worked
trees_total %>% filter(is.na(DBH_class)) # --> yes worked

# checking for species names that were net part of the species dataset
trees_total %>% filter(is.na(bot_name)) %>%  group_by(SP_code) %>% distinct()
# in case of this dataset it is only RER meaning "Rot Erle" which does not have a latin name assigned, 
# so I´ll do it manually. but for later analysis this has to be automatised or the source of error
# has to be removed when the raw data are created/ assessed

#checking if assignment of the tpS_ID assignment works
trees_total %>% filter(is.na(tpS_ID)) %>%  select(SP_code, bot_name)%>%  group_by(SP_code) %>% distinct()

# checking for trees where age is NA
trees_total %>% select(plot_ID, age) %>% group_by(plot_ID) %>% filter(!is.na(age)) %>% distinct()

# ISSUES LINKNING SPECIES IN TREE TOTAL TO TAPES
# We need a column with species codes/ names/ numbers that can be regonized by tapeS
# which is not the case for the current species related column in trees_total or SP_names
# when trying to join a column from the tapeS species codes dataset it becomes 
# evident that there is no shared code between the two datasets. Neither the botanical names, 
# nor the german abbreviations correspond. This is because the species list for the BZE and 
# the MoMoK locations is much more precise then the TapeS list, which groups species in to 
# species classes. 
# At first I thought they would correspond with those species codes and groups assessed in the 
# BWI (National forest Inventory NFI), but this is not the case. 
# I tried to link a capital lettered column of the German abbreviations in the TapeS dataset 
# with the capital lettered columns of the german abbreviation column of the SP_names dateset as well as the BWI column 
# (wich one would think should correspond), but that is not the case. Similar issues are faced trying to use the latin names
# for a join, because the Latin names in the SP_names and by that in the trees_total dataset are much more precise then in the 
# Thus it seems the onlyway to create a column in the trees_total or SP_names dataset. 
# in thte TapeS dataset the botalical names vary betweeen genus and species and genus spp. which makes it 
# very hard to find a common variable between the two sets. 
# where speiceis codes present in tapeS are manually assigned to the respecitive species in the MoMoK dataset. 
# This is okay, since there are only six species for MoMok
# for the BZE Bestandeserhebung, however, this will not work. 

# this displays the german species abbreviations in TapeS that cannot be linked to BWI
# codes in the SP_names dataset
anti_join(SP_TapeS_test %>% mutate(Chr_ger_cap = toupper(SP_TapeS_test$kurz)), 
          SP_names %>% mutate(Chr_ger_cap = toupper(Chr_code_ger)), 
          by = c("Chr_ger_cap"= "BWI"))

# THis displays the BWI abbreviations in trees_total that don´t find a match in SP_tapes when the brreviations are in capital letters
anti_join(trees_total %>% left_join(., SP_names %>% 
                                      add_row(Nr_code = NA, 
                                              Chr_code_ger = "REr",
                                              name = "Rot-Erle",
                                              bot_name = "Alnus rubra",
                                              bot_genus = "Alnus", 
                                              bot_species = "rubra",
                                              Flora_EU = NA, 
                                              LH_NH = "LB", 
                                              IPC = NA,
                                              WZE = NA, 
                                              BWI = "ER", # as there were no codes in d info for Alnus rubra available, I assign this species to Alnus spp. 
                                              BZE_al = NA)  %>%
                                      mutate(Chr_ger_cap = toupper(Chr_code_ger)) %>% 
                                      select(BWI, Chr_ger_cap, bot_name), 
                                    by = c("SP_code" = "Chr_ger_cap")), 
          SP_TapeS_test %>% 
            mutate(Chr_ger_cap = toupper(SP_TapeS_test$kurz)), 
          by = c("BWI" = "Chr_ger_cap")) %>%  
  group_by(BWI) %>% 
  distinct(BWI, bot_name)
# which is aparently only the both pine specise: 
# KI    Pinus mugo      
# KI    Pinus sylvestris
# thus, I will create a column in the SP_dataset or the trees_total dataset that 
# contains the BWI codes that TapeS can read to then join the column from tapeS that is readable to the tapeS package, 
# because for now I am joinin g based on the german abbreviations in tapeS in capital letters, which are not 
# part of the original TapeS dataframe ( as I added them with the mutate toupper)

# check which sp_codes from trees total are present in the TapeS package
anti_join(trees_total %>% select(Chr_code_ger, SP_code, bot_name), SP_TapeS_test, 
          by= c("bot_name" = "scientific")) %>% distinct()
# these species are present in the trees_total but not in the TapeS package, meaning 
# they´ll have to be harmonised first, to use TapeS for this dataset 
# thus I´ll create a column called TapeS_SP where I allocate the species 
# to the respective TapeS species codes
# or I add a column with capital latters to the tapes dataset and use it as a key 
# variable to join a TapeS indentificable column form the tapeS_sp dataset
#  SP_code         bot_name
# 1     RER      Alnus rubra
# 2     STK  Prunus serotina
# 3     GFI     Picea abies 
# 4     MBI Betula pubescens
# 5     BKI       Pinus mugo



        
SP_names_com_ID_tapeS <- left_join(rbind(
  # selecting those rows in SP_names (x_bart) that have a match in "scientific" of TapeS 
  # and create column called com_ID That holds that scientific names that are common between TapeS and SP_names x_bart
  inner_join(SP_names, SP_TapeS_test %>% select(scientific), by = c("bot_name" = "scientific")) %>% 
  mutate(tpS_SP_com_name = bot_name), 
 # selecting those rows in SP_names (x_bart) that do not have a match in "scientific" of TapeS 
  anti_join(SP_names, SP_TapeS_test  %>% select(scientific), by = c("bot_name" = "scientific")) %>% 
                                   # every acer not campestre, etc. is assigned to Acer spp. (the other species do have a match in TapeS_SP)
  mutate(tpS_SP_com_name = case_when(bot_genus == "Abies" & !(bot_species %in% c("grandis", "alba")) | bot_genus == "abies …" & !(bot_species %in% c("grandis", "alba")) ~ "Abies alba",
                                    # all Larix not kaemperi & decidua are assigned to Larix spp.
                                    bot_genus == "Larix" & !(bot_species %in% c("decidua", "kaempferi")) ~ "Larix spp.",
                                    # all picea are allocated to Picea abies cause TapeS doesn´t distinguish
                                    bot_genus == "Picea"  ~ "Picea abies",
                                    # all Pinus not "nigra", "strobus" are assigned to Pinus sylvestris
                                    bot_genus == "Pinus" & !(bot_species %in% c("nigra", "strobus")) ~  "Pinus sylvestris",
                                    # there is a spelling mistake in x-Bart spelling Pseudotsuga menziestii with a t wich hampers the join with TapeS_SP
                                    bot_genus == "Pseudotsuga" ~ "Pseudotsuga menziesii", 
                                    # all thuja species (whcih x_bart doesnt distinguish anyways) are treated as Thuja plicata
                                    bot_genus == "Thuja" ~ "Thuja plicata",
                                    # all tsuga are treated as tsuga heterophyllia cause TapeS only has that species of the genus
                                    bot_genus == "Tsuga" ~ "Tsuga heterophylla",
                                    # everything else NH belongs to other coniferous trees
                                    LH_NH == "NB" & !(bot_genus %in% c("Abies","Larix", "Picea","Pinus", "Pseudotsuga", "Thuja", "Tsuga"))~ "Coniferales trees", 
                                    bot_genus == "Acer" & !(bot_species %in% c("campestre", "platanoides",  "pseudoplatanus", "spp.")) ~ "Acer spp.",
                                    bot_genus == "Alnus" ~ "Alnus spp.", 
                                    bot_genus == "Betula" ~ "Betula spp.", 
                                    # all Carpinus species are treated as Carpinus betulus
                                    bot_genus == "Carpinus" ~ "Carpinus betulus",
                                    # all fagus species are treated as Fagus sylvatica
                                    bot_genus == "Fagus" ~ "Fagus sylvatica", 
                                    # all Fraxinus species are treated as Fraxinus excelsior
                                    bot_genus == "Fraxinus" ~ "Fraxinus excelsior",
                                    #all Populus species except populus balsamifera are assigned to Populus spp. 
                                    bot_genus == "Populus" & bot_species != "balsamifera"  ~ "Populus spp.", 
                                    # all Prunus species are treated as Prunus avium
                                    bot_genus == "Prunus"  ~ "Prunus avium",
                                    #all Quercus species except rubra balsamifera are assigned to Quercus spp. 
                                    bot_genus == "Quercus" & bot_species != "rubra"  ~ "Quercus spp.",
                                    # all Salix species are allocated to Salix spp.
                                    bot_genus == "Salix"  ~ "Salix spp.",
                                    #all Sorbus species except torminalis are assigned to Sorbus aucuparia 
                                    bot_genus == "Sorbus" & bot_species != "torminalis"  ~ "Sorbus aucuparia",
                                    # all Tilia species are allocated to Tilia spp. cause TapeS doesnt distinguish between the species
                                    bot_genus == "Tilia"  ~ "Tilia spp.",
                                    # all Ulmus species are allocated to Ulmus spp. cause TapeS doesnt distinguish between the species
                                    bot_genus == "Ulmus"  ~  "Ulmus spp.",
                                    bot_name == '-2' ~ "missing", 
                                    # everything else belongs to other broadleafed trees
                                    TRUE ~ "Magnoliopsida trees"))), 
 SP_TapeS_test %>% select(scientific, ID) %>% rename(tpS_ID = ID), 
 by = c("tpS_SP_com_name" = "scientific"))
# export x_bart with TapeS common ID: https://stackoverflow.com/questions/53089219/specify-path-in-write-csv-function
 write.csv(SP_names_com_ID_tapeS, "output/out_data/x_bart_tapeS.csv")        

# ----- 2. CALCULATIONS --------------------------------------------------------

# ----- 2.1. Regression models for missing tree heights ---------------------------------
# find the plots and species that won´t have a height regression model because 
# there are less then 3 measurements per plot
# ---> think about way to deal with them !!!!
trees_total %>% 
  select(plot_ID, SP_code, H_dm, DBH_mm, Kraft) %>% 
  filter(!is.na(H_dm) & !is.na(DBH_mm)) %>% 
  group_by(plot_ID, SP_code) %>% 
  filter(n() < 3)%>%    # filter for plots where there are less then 3 heights measured for each species
  #group_by(plot_ID, SP_code) %>% 
  #lm_table(H_m ~ DBH_cm) %>% 
  arrange(plot_ID, SP_code)

# ----- 2.1.1. coefficents dataframe per SP and plot when >= 3 heights measured --------
# to calculate individual tree heights for trees of the samme species and plot 
# where the height has not been sampled I will create a linear regression for the heights
# in the following i will create a dataframe with regression coefficients per 
# species per plot if there are more then 3 heights measured per species and plot

# coefficents of non-linear height model per species and plot
# https://rdrr.io/cran/forestmangr/f/vignettes/eq_group_fit_en.Rmd
coeff_H_SP_P <- left_join(trees_total %>% 
                            select(plot_ID, SP_code, H_m, DBH_cm, DBH_class) %>% 
                            filter(!is.na(H_m) & !is.na(DBH_cm) & !is.na(DBH_class)) %>% 
                            group_by(plot_ID, SP_code) %>% 
                            filter(n() >= 3),
                          # coeff_H_SP_P dataset 
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
             N = length(H_m), 
             SSres = sum((H_m-H_est)^2), 
             SStot = sum((H_m-mean_h)^2), 
             pseu_R2 = 1-(SSres/SStot), 
             diff_h = mean(H_m - H_est))

# ----- 2.1.2. coefficents dataframe per SP over all plots when >= 3 heights measured --------
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
             N = length(H_m), 
             SSres = sum((H_m-H_est)^2), 
             SStot = sum((H_m-mean_h)^2), 
             pseu_R2 = 1-(SSres/SStot), 
             diff_h = mean(H_m - H_est)) %>% 
  mutate(plot_ID = as.factor('all')) %>% 
  select(plot_ID, SP_code, b0, b1, b2, bias, rsme, R2, mean_h, N, SSres, SStot, pseu_R2, diff_h)

# ----- 2.1.3. combined coefficients of height models ---------------------
coeff_H_comb <- rbind(coeff_H_SP_P %>% mutate(plot_ID = as.factor(plot_ID)), coeff_H_SP)


# ----- 2.1.4. have a look at the quality of the models  ------------------------------
# from my previous attempts to fit and validate species specific but also total 
# dataset including regression models for the height, I know that actually the 
# DBH class is the better predictor 
# but on the other hand the diameter class is also less precise
summary(coeff_H_SP)
# the R2 is pretty poor for some plots 
coeff_heights %>% filter(Rsqr <= 0.3)
#view(trees_total %>% filter(plot_ID == 32080))

summary(coeff_H_SP_P)
coeff_nls_h_combined %>% filter(diff_h >= 0.75)
#view(coeff_H_SP_P %>% filter(rsqrd<=0.5))


# ----- 2.1.5. join coefficients to the tree data set & calculate missing heights  ----------------------------
# estimating height by using different functions, depending on the models R2
trees_total_5 <- trees_total %>%
  unite(SP_P_ID, plot_ID, SP_code, sep = "", remove = FALSE) %>%            # create column matching vectorised coefficients of coeff_SP_P (1.3. functions, h_nls_SP_P, dplyr::pull)
  left_join(.,coeff_H_SP_P %>%                                              # joining R2 from coeff_SP_P -> R2.x
              select(plot_ID, SP_code, R2) %>% 
              unite(SP_P_ID, plot_ID, SP_code, sep = "", remove = FALSE),   # create column matching vectorised coefficients of coeff_SP_P (1.3. functions, h_nls_SP_P, dplyr::pull)
            by = c("plot_ID", "SP_code", "SP_P_ID")) %>% 
  left_join(., coeff_H_SP %>% select(SP_code, R2),               # joing R2 from coeff_SP data set -> R2.y
            by = "SP_code") %>%       
  left_join(., trees_total %>%                                  # this is creates a tree dataset with mean BHD, d_g, h_g per species per plot per canopy layer wich we need for SLOBODA 
              group_by(plot_ID, C_layer, SP_code) %>%             # group by plot and species and canopy layer to calcualte dg, hg 
              summarise(H_g = sum(mean(na.omit(H_m))*BA_m2)/sum(BA_m2),    # Höhe des Grundflächemittelstammes, calculation according to S. Schnell
                        mean_DBH_mm = mean(DBH_mm),               # mean diameter per species per canopy layer per plot
                        D_g = ((sqrt((mean(BA_m2)/pi)))*2)*100),   # Durchmesser des Grundflächenmittelstammes; *1000 to get from 1m -> 100cm -> 1000mm
            by = c("plot_ID", "SP_code", "C_layer")) %>% 
  mutate(R2_comb = f(R2.x, R2.y, R2.y, R2.x),                               # if R2 is na, put R2 from coeff_SP_P unless R2 from coeff_SP is higher
         H_method = case_when(is.na(H_m) & !is.na(R2.x) & R2.x > 0.70 | is.na(H_m) & R2.x > R2.y & R2.x > 0.7 ~ "coeff_SP_P", 
                              is.na(H_m) & is.na(R2.x) & R2.y > 0.70| is.na(H_m) & R2.x < R2.y & R2.y > 0.70 ~ "coeff_sp",
                              is.na(H_m) & is.na(R2_comb) & !is.na(H_g)| is.na(H_m) & R2_comb < 0.70 & !is.na(H_g) ~ "ehk_sloboda",
                              is.na(H_m) & is.na(R2_comb) & is.na(H_g)| is.na(H_m) & R2_comb < 0.70 & is.na(H_g) ~ "h_curtis", 
                              TRUE ~ "sampled")) %>% 
  # When h_m is na but there is a plot and species wise model with R2 above 0.7, use the model to predict the height
  mutate(H_m = case_when(is.na(H_m) & !is.na(R2.x) & R2.x > 0.70 | is.na(H_m) & R2.x > R2.y & R2.x > 0.7 ~ h_nls_SP_P(SP_P_ID, DBH_cm),
                         # if H_m is na and there is an R2 from coeff_SP_P thats bigger then 0.75 or of theres no R2 from 
                         # coeff_SP_plot that´s bigger then R2 of coeff_SP_P while the given R2 from coeff_SP_P is above 
                         # 0.75 then use the SP_P models
                         is.na(H_m) & is.na(R2.x) & R2.y > 0.70 | is.na(H_m) & R2.x < R2.y & R2.y > 0.70 ~ h_nls_SP(SP_code, DBH_cm),
                         # when there´s still no model per species or plot, or the R2 of both self-made models is below 0.7 
                         # and hm is na but there is a h_g and d_G
                         is.na(H_m) & is.na(R2_comb) & !is.na(H_g)| is.na(H_m) & R2_comb < 0.70 & !is.na(H_g) ~ ehk_sloboda(H_SP_group, DBH_mm, mean_DBH_mm, D_g, H_g),
                         # when there´s still no model per species or plot, or the R2 of both self-made models is below 0.7 
                         # and hm is na and the Slobody function cannot eb applied because there is no h_g calculatable use the curtis function
                         is.na(H_m) & is.na(R2_comb) & is.na(H_g)| is.na(H_m) & R2_comb < 0.70 & is.na(H_g) ~ h_curtis(H_SP_group, DBH_mm), 
                         TRUE ~ H_m))

# ----- 2.2. Biomass living trees--------------------------------------------------------------
# input vairbales for the biomass models for the trees aboveground biomass without canopy are: 
# DBH, diameter at 1/3 of the tree height, species, tree height

# ----- 2.2.1. TAPES: goal: estimating diameter at 0.3 of tree height with TapeS -----------------------------------------------------------
#https://gitlab.com/vochr/tapes/-/blob/master/vignettes/tapes.rmd
help("tprBiomass")

# checking if assigning the species works
  # tprSpeciesCode(inSp = trees_total$tpS_ID, outSp = c("scientific"))

# https://softwareengineering.stackexchange.com/questions/307639/what-does-mapping-mean-in-programming
# The map function requires an array and another function. It returns a new array 
# which is the result of applying that function to all elements of the original array.
# All other uses of the term can, at least in my experience, be considered analogous 
# to this specific one. In the most general sense, "mapping" in programming means 
# taking several things and then somehow associating each of them with another thing
    # BaMap(Ba = trees_total$tpS_ID, type = c(NULL))

# ----- 2.2.1.1. create TapeS object -----------------------------------------------------------
spp = trees_total_5 %>% dplyr::pull(tpS_ID)
Dm = as.list(trees_total_5 %>% dplyr::pull(DBH_cm))
Hm = as.list(trees_total_5 %>% mutate(DBH_h_m = DBH_h_cm/100) %>% dplyr::pull(DBH_h_m))
Ht = trees_total_5 %>% dplyr::pull(H_m)
obj <- tprTrees(spp, Dm, Hm, Ht, inv = 4)


#plot(obj)

# ----- 2.2.1.2. dominant height -----------------------------------------------------------
# necesaryy as side index for the better broadleved models
# Arithmetisches Mittel der Höhe der 100 stärksten Bäume je ha. (In Deutschland auch als Spitzenhöhe h100 oder h200 bezeichnet; die WEISE�sche Oberhöhe [ho] entspricht der Höhe des Grundflächen- Mittelstammes der 20 % stärksten Bäume eines Bestandes).
# Wichtig: Die Art der Oberhöhe muss jeweils definiert werden.
# my problem: there are no 100 trees per plot and I don´t know how to estimate the height of the top 100

# https://statisticsglobe.com/select-top-n-highest-values-by-group-in-r#example-2-extract-top-n-highest-values-by-group-using-dplyr-package
# data %>%                                      # Top N highest values by group
#   arrange(desc(value)) %>% 
#   group_by(group) %>%
#   slice(1:3)

# https://rdrr.io/cran/dplyr/man/top_n.html
#df %>% top_n(2)  # highest values

# trees_total_5 %>% 
#   group_by(plot_ID) %>% 
#   summarize


# ----- 2.2.1.2. biomass trees -----------------------------------------------------------
trees_total_5 <- trees_total_5 %>% 
  # adding diameter at 0.3 tree height to trees_total dataframe
  mutate(D_03_cm = tprDiameter(obj, Hx = 1/3*Ht(obj), cp=FALSE)) %>% 
  # biomass
  # aboveground biomass 
  mutate(aB_kg = case_when(DBH_cm >= 10 ~ Dunger_aB_DBHa10(Bio_SP_group, DBH_cm, D_03_cm, H_m), 
                           DBH_cm < 10 & H_m >= 1.3 ~ Dunger_aB_H1.3_DBHb10(Bio_SP_group, DBH_cm), 
                           H_m >= 1.3 ~ Dunger_aB_Hb1.3(LH_NH, DBH_cm)),
        # belowground biomass
          Dunger_bB_kg = Dunger_bB(Bio_SP_group, DBH_cm)) %>% 
         # foliage biomass
   mutate(fB_kg = ifelse(LH_NH == "NB", Wirth_fB_N(DBH_cm, H_m, age), Wutzler_fB_L1(DBH_cm, H_m)),
          # branch biomass: this formula leads to branch biomass higher then the stem biomass which cannot be 
          brB_kg = ifelse(LH_NH == "NB", brB_N(DBH_cm, H_m, age), brB_L1(DBH_cm, H_m)), 
          # stem biomass = if coniferous: tot_bio NH - foliage, if not coniferous: keep aB_kg
          StB_kg = ifelse(LH_NH == "NB", (aB_kg-(fB_kg+brB_kg)), (aB_kg-brB_kg)), 
          # total aboveground = biomass if coniferous: keep aB_kg, if not coniferous: add foliage to stem
          totwaB_kg = ifelse(LH_NH == "NB", aB_kg, aB_kg+fB_kg),            # total woody aboveground biomass in KG per tree per plot
          totwaB_t_ha = totwaB_kg/1000*plot_A_ha,                            # total woody aboveground biomass in tons per hectare per tree
          fB_t_ha = fB_kg/1000*plot_A_ha)                                   # total foliage abiomass in tons per hectare per tree 





# ----- 2.3  Biomass dead trees -------------------------------------------

# ----- 2.3.1. species groups ---------------------------------------------
# to assing the right species group, i wnat to use the dominant species of the plot: 

# assigning dominant species of living trees to DW_total dataset by plot_ID
DW_total <- left_join(         # this join reffers to the last attached dataset which is the one holding the common IDs between SP_names & TapeS
  DW_total %>% 
  left_join(.,
            # dataset with percentage that the respective species contributes to total basal area per plot 
            left_join( 
              dom_SP_plot <- left_join(
                # data set with BA per species
                trees_total %>%
                  group_by(plot_ID, SP_code) %>%       # group by plot and species to calculate BA per species 
                  summarise(SP_BA_plot = sum(BA_m2),             # calculate BA per species per canopy layer per plot in m2
                            plot_A_ha = mean(plot_A_ha)) %>%     # plot area in hectare to calculate BA per ha
                  mutate(SP_BA_m2ha = SP_BA_plot/plot_A_ha), # calculate BA per species per plot in m2/ ha
                # dataset with total BA per plot
                trees_total %>%
                  group_by(plot_ID) %>%                         # group by plot to calculate total BA per plot
                  summarise(tot_BA_plot = sum(BA_m2),           # calculate total BA per plot in m2 by summarizing the BA of individual trees after grouping the dataset by plot
                            plot_A_ha = mean(plot_A_ha)) %>%    # plot area in hectare to calculate BA per ha
                  mutate(tot_BA_m2ha = tot_BA_plot/plot_A_ha), # calculate total BA per plot in m2 per hectare by dividing total BA m2/plot by area plot/ha 
                by=c("plot_ID", "plot_A_ha")) %>% 
                select(- c(plot_A_ha, tot_BA_plot)) %>%  # remove unnecessary variables
                mutate(BA_SP_per = (SP_BA_m2ha/tot_BA_m2ha)*100),   # calculate proportion of each species to total BA in percent, 
              # dataset selecting dominant species
              as.data.table(dom_SP_plot)[as.data.table(dom_SP_plot)[, .I[BA_SP_per == max(BA_SP_per)], by= plot_ID]$V1] %>% 
                rename(., dom_SP = SP_code) %>% 
                select(plot_ID, dom_SP), 
              by = "plot_ID") %>% 
              select(plot_ID, dom_SP) %>% 
              distinct(),
            by = "plot_ID"), 
  # joining the common species codes of SP_names and SP_tapeS fromSP_names_com_ID_tapeS and attaching the tpS_ID from SP_tapeS to it
  SP_names_com_ID_tapeS %>% 
              mutate(Chr_ger_cap = toupper(Chr_code_ger)) %>% 
              select(Chr_ger_cap, tpS_SP_com_name, tpS_ID, LH_NH), 
  by = c("dom_SP" = "Chr_ger_cap")) %>%
  mutate(L_m = L_dm/10,
         D_m = as.integer(D_cm)/100, 
         D_h_m = 1.3,
         dec_type_BWI = case_when(dec_type == 1 | dec_type == 2 ~ 1, 
                                  dec_type == 3 ~ 2, 
                                  dec_type == 4 ~ 3, 
                                  TRUE ~ 4))

# ----- 2.3.2. Deadwood volume, biomass, carbon ---------------------------------------------
DW_total <- DW_total %>% 
  unite("SP_dec_type", SP_group, dec_type_BWI, sep = "_", remove = FALSE)%>% 
  mutate(V_dw_meth = ifelse(DW_type %in% c(1, 6, 4) | DW_type == 3 & L_m < 3, "V_DW_T1463", "V_DW_T253"),
         V_dw_m3 = ifelse(DW_type %in% c(1, 6, 4) | DW_type == 3 & L_m > 3, V_DW_T1463(D_m, L_m), V_DW_T253(tpS_ID, D_cm, D_h_cm, L_m)),
         B_dw_kg = B_DW(V_dw_m3, SP_dec_type), 
         C_dw_kg = C_DW(V_dw_m3, SP_dec_type))




# ----- 2.4. Plot level data: Basal area, species composition, DBH (m, sd), H (m, sd) --------------------------------------------------------
# ----- 2.4.1. grouped by Plot, canopy layer, species ------------------------------------------------------------
trees_P_CP_SP <- left_join(
  # dataset with BA per species
  trees_total_5 %>%
    group_by(plot_ID, C_layer, SP_code) %>%         # group by plot and species to calculate BA per species 
    summarise(mean_DBH_cm = mean(DBH_cm),           # mean diameter per species per canopy layer per plot
              sd_DBH_cm = sd(DBH_cm),       
              mean_H_m = mean(H_m),                 # mean height per species per canopy layer per plot
              sd_height_m = sd(H_m),                # standard deviation of height --> structural richness indicator
              SP_BA_plot = sum(BA_m2),              # calculate BA per species per canopy layer per plot in m2
              mean_BA_SP_plot = mean(BA_m2),        # calculate mean BA in m2 per species per canopy payer per plot
              h_g = sum(mean(H_m)*BA_m2)/sum(BA_m2),
              d_g = ((sqrt((mean(BA_m2)/pi)))*2)*10,  # multiply by two to get radius into diameter, multiply by 10 to transform m into cm
              Nt_plot = n(),                          # counting number of observations per group to get number of trees per ha
              plot_A_ha = mean(plot_A_ha)) %>%        # plot area in hectare to calculate BA per ha
    mutate(SP_BA_m2ha = SP_BA_plot/plot_A_ha,         # calculate BA per species per plot in m2/ ha
           Nt_ha = Nt_plot/ plot_A_ha),                # number of trees per species and layer per hectare  
  # dataset with total BA per plot
  trees_total_5 %>%
    group_by(plot_ID, C_layer) %>%                  # group by plot to calculate total BA per plot
    summarise(tot_BA_plot = sum(BA_m2),             # calculate total BA per plot in m2 by summarizing the BA of individual trees after grouping the dataset by plot
              plot_A_ha = mean(plot_A_ha)) %>%      # plot area in hectare to calculate BA per ha
    mutate(tot_BA_m2ha = tot_BA_plot/plot_A_ha),    # calculate total BA per plot in m2 per hectare by dividing total BA m2/plot by area plot/ha 
  by=c("plot_ID","C_layer", "plot_A_ha")) %>% 
  select(- c(plot_A_ha, tot_BA_plot)) %>%           # remove unnecessary variables
  mutate(BA_SP_per = (SP_BA_m2ha/tot_BA_m2ha)*100)  # calculate proportion of each species to total BA in percent
# joining data set with dominant species using Ana Lucia Mendez Cartin´s code that filters for those species where BA_SP_per is max
trees_P_CP_SP <- left_join(trees_P_CP_SP, 
                           (as.data.table(trees_P_CP_SP)[as.data.table(trees_P_CP_SP)[, .I[BA_SP_per == max(BA_SP_per)], 
                                                                                      by= c("plot_ID", "C_layer")]$V1]) %>% 
                             rename(., dom_SP = SP_code) %>% 
                             select(plot_ID, C_layer, dom_SP), 
                           by = c("plot_ID", "C_layer"))



# ----- 2.4.2. grouped by Plot species ------------------------------------------------------------
trees_P_SP <- left_join(
  # data set with BA per species
  trees_total %>%
    group_by(plot_ID, SP_code) %>%       # group by plot and species to calculate BA per species 
    summarise(mean_DBH_cm = mean(DBH_cm),         # mean diameter per species per canopy layer per plot
              sd_DBH_cm = sd(DBH_cm),       
              mean_H_m = mean(H_m),                # mean height per species per canopy layer per plot
              sd_height_m = sd(H_m),               # standart deviation of height --> structual richness indicator
              SP_BA_plot = sum(BA_m2),             # calculate BA per species per canopy layer per plot in m2
              mean_BA_SP_plot = mean(BA_m2),       # calculate mean BA in m2 per species per canopy payer per plot
              Nt_plot = n(),
              plot_A_ha = mean(plot_A_ha)) %>%     # plot area in hectare to calculate BA per ha
    mutate(SP_BA_m2ha = SP_BA_plot/plot_A_ha),    # calculate BA per species per plot in m2/ ha
  # dataset with total BA per plot
  trees_total %>%
    group_by(plot_ID) %>%                         # group by plot to calculate total BA per plot
    summarise(tot_BA_plot = sum(BA_m2),           # calculate total BA per plot in m2 by summarizing the BA of individual trees after grouping the dataset by plot
              plot_A_ha = mean(plot_A_ha)) %>%    # plot area in hectare to calculate BA per ha
    mutate(tot_BA_m2ha = tot_BA_plot/plot_A_ha), # calculate total BA per plot in m2 per hectare by dividing total BA m2/plot by area plot/ha 
  by=c("plot_ID", "plot_A_ha")) %>% 
  select(- c(plot_A_ha, tot_BA_plot)) %>%  # remove unnecessary variables
  mutate(BA_SP_per = (SP_BA_m2ha/tot_BA_m2ha)*100)  # calculate proportion of each species to total BA in percent
# joining dataset with dominant species using Ana Lucia Mendez Cartins code that filters for those species where BA_SP_per is max
trees_P_SP <- left_join(trees_P_SP,
                        as.data.table(trees_P_SP)[as.data.table(trees_P_SP)[, .I[BA_SP_per == max(BA_SP_per)], by= plot_ID]$V1] %>% 
                          rename(., dom_SP = SP_code) %>% 
                          select(plot_ID, dom_SP), 
                        by = "plot_ID")


# ----- 2.4.2. grouped by Plot ------------------------------------------------------------

trees_P <- left_join(
  # data set with BA per species
  trees_total_5 %>%
    group_by(plot_ID) %>%       # group by plot and species to calculate BA per species 
    summarise(mean_DBH_cm = mean(DBH_cm),         # mean diameter per species per canopy layer per plot
              sd_DBH_cm = sd(DBH_cm),       
              mean_H_m = mean(H_m),                # mean height per species per canopy layer per plot
              sd_height_m = sd(H_m),               # standart deviation of height --> structual richness indicator
              SP_BA_plot = sum(BA_m2),             # calculate BA per species per canopy layer per plot in m2
              mean_BA_SP_plot = mean(BA_m2),       # calculate mean BA in m2 per species per canopy payer per plot
              Nt_plot = n(),
              plot_A_ha = mean(plot_A_ha)) %>%    # plot area in hectare to calculate BA per ha
    mutate(SP_BA_m2ha = SP_BA_plot/plot_A_ha),    # calculate BA per species per plot in m2/ ha
  # dataset with total BA per plot
  trees_total_5 %>%
    group_by(plot_ID) %>%                         # group by plot to calculate total BA per plot
    summarise(tot_BA_plot = sum(BA_m2),           # calculate total BA per plot in m2 by summarizing the BA of individual trees after grouping the dataset by plot
              plot_A_ha = mean(plot_A_ha)) %>%    # plot area in hectare to calculate BA per ha
    mutate(tot_BA_m2ha = tot_BA_plot/plot_A_ha), # calculate total BA per plot in m2 per hectare by dividing total BA m2/plot by area plot/ha 
  by=c("plot_ID", "plot_A_ha")) %>% 
  select(- c(plot_A_ha, tot_BA_plot)) %>%  # remove unnecessary variables
  mutate(BA_SP_per = (SP_BA_m2ha/tot_BA_m2ha)*100) %>%   # calculate proportion of each species to total BA in percent
  left_join(., trees_total %>%
              group_by(plot_ID) %>%
              select(plot_ID, SP_code) %>% 
              distinct(SP_code) %>% 
              summarize(n_SP_plot = n()),
              #summarise(n_SP_plot = length(SP_code)), 
            by = "plot_ID") %>% 
  left_join(., trees_P_SP %>% select(plot_ID, dom_SP) %>% distinct(), 
            by = "plot_ID")


# joining dataset with dominant species using Ana Lucia Mendez Cartins code that filters for those species where BA_SP_per is max
trees_P <- left_join(trees_P,trees_P_SP %>% 
                       select(plot_ID, dom_SP) %>% 
                       distinct(), 
                        by = "plot_ID")












# ------ 3. VISULAIZATION -------------------------------------------------
# ----- 3.1.5. visualization height regression -------------------------------------------------------------
# ----- 3.1.5.1. visualization height regression by plot and species ---------------------------------------
# nls: plot estimated heights against dbh by plot and species
ggplot(data = (left_join(trees_total %>% 
                          select(plot_ID, SP_code, H_m, DBH_cm, DBH_class) %>% 
                          filter(!is.na(H_m) & !is.na(DBH_cm)) %>% 
                          group_by(plot_ID, SP_code) %>% 
                          #filter(DBH_cm <= 150) %>% 
                          filter(n() >= 3),coeff_H_SP_P, by = c("plot_ID", "SP_code"))%>%
                mutate(H_est = b0 * (1 - exp( -b1 * DBH_cm))^b2)), 
      aes(x = DBH_cm, y = H_est, color = SP_code))+
 geom_point()+
 geom_smooth(method = "loess", se=TRUE)+
 facet_wrap(SP_code~plot_ID)+
 xlab("diameter ") +
 ylab("estimated height [m]")+
 #ylim(0, 50)+
 ggtitle("height estimated vs. diameter")+
 theme_light()+
 theme(legend.position = "non")

# nls: plot sampled heights against DBH
ggplot(data = trees_total %>% 
        select(plot_ID, SP_code, H_m, DBH_cm, DBH_class) %>% 
        filter(!is.na(H_m) & !is.na(DBH_cm)) %>% 
        group_by(plot_ID, SP_code) %>% 
        filter(n() >= 3),
      aes(x = DBH_cm, y = H_m, color = SP_code))+
 geom_point()+
 geom_smooth(method = "loess", se=TRUE)+
 facet_wrap(SP_code~plot_ID)+
 xlab("diameter ") +
 ylab("sampled height [m]")+
 ylim(0, 50)+
 ggtitle("height sampled vs. diameter")+
 theme_light()+
 theme(legend.position = "bottom")

# nls: plot estimated vs. sampled heights 
ggplot(data = (left_join(trees_total %>% 
                          select(plot_ID, SP_code, H_m, DBH_cm, DBH_class) %>% 
                          filter(!is.na(H_m) & !is.na(DBH_cm)) %>% 
                          group_by(plot_ID, SP_code) %>% 
                          filter(n() >= 3),coeff_H_SP_P, by = c("plot_ID", "SP_code"))%>%
                mutate(H_est = b0 * (1 - exp( -b1 * DBH_cm))^b2)), 
      aes(x = H_m, y = H_est, color = SP_code))+
 geom_point()+
 geom_smooth(method = "lm", se=TRUE)+
 facet_wrap(plot_ID ~ SP_code)+
 xlab("sampled height [m]") +
 ylab("estimated height nls model")+
 ggtitle("height estimated via nls vs. sampled height")+
 theme_light()+
 theme(legend.position = "non")

# ----- 3.1.5.2. visualization height regression by species over all plot ------------------------------------
ggplot(data = (left_join(trees_total %>% 
                          select(plot_ID, SP_code, H_m, DBH_cm, DBH_class) %>% 
                          filter(!is.na(H_m) & !is.na(DBH_cm)) %>% 
                          group_by(plot_ID, SP_code) %>% 
                          #filter(DBH_cm <= 150) %>% 
                          filter(n() >= 3),coeff_H_SP, by = "SP_code")%>%
                mutate(H_est = b0 * (1 - exp( -b1 * DBH_cm))^b2)), 
      aes(x = DBH_cm, y = H_est, color = SP_code))+
 geom_point()+
 geom_smooth(method = "loess", se=TRUE)+
 facet_wrap(~SP_code)+
 xlab("diameter ") +
 ylab("estimated height [m]")+
 ylim(0, 50)+
 ggtitle("height estimated vs. diameter")+
 theme_light()+
 theme(legend.position = "non")

# nls: plot sampled heights against DBH
ggplot(data = trees_total %>% 
        select(plot_ID, SP_code, H_m, DBH_cm, DBH_class) %>% 
        filter(!is.na(H_m) & !is.na(DBH_cm)) %>% 
        group_by(SP_code) %>% 
        filter(n() >= 3),
      aes(x = DBH_cm, y = H_m, color = SP_code))+
 geom_point()+
 geom_smooth(method = "loess", se=TRUE)+
 facet_wrap(~SP_code)+
 xlab("diameter ") +
 ylab("sampled height [m]")+
 ylim(0, 50)+
 ggtitle("height sampled vs. diameter")+
 theme_light()+
 theme(legend.position = "bottom")

# nls: plot estimated vs. sampled heights 
ggplot(data = (left_join(trees_total %>% 
                          select(plot_ID, SP_code, H_m, DBH_cm, DBH_class) %>% 
                          filter(!is.na(H_m) & !is.na(DBH_cm)) %>% 
                          group_by(plot_ID, SP_code) %>% 
                          filter(n() >= 3),coeff_H_SP, by = "SP_code")%>%
                mutate(H_est = b0 * (1 - exp( -b1 * DBH_cm))^b2)), 
      aes(x = H_m, y = H_est, color = SP_code))+
 geom_point()+
 geom_smooth(method = "lm", se=TRUE)+
 facet_wrap(~SP_code)+
 xlab("sampled height [m]") +
 ylab("estimated height nls model")+
 ggtitle("height estimated via nls vs. sampled height")+
 theme_light()+
 theme(legend.position = "non")




# ---- 3.1.5.2. visulisation height vs. DBH by different models/ methods ----------------------------------------------------------------


# plot estimated and samples heights vs. diameter by species, plot ad height method 
# (nls-SP-P, nls-SP, sloboda, curtis, sampled)
ggplot(data = trees_total_5, 
      aes(x = DBH_cm, y = H_m, color = H_method))+
 geom_point()+
 #geom_line(method = "lm")+
 #geom_smooth(method = "nls", se=TRUE)+
 facet_wrap(plot_ID~SP_code)+
 xlab("DBH") +
 ylab("height [m]")+
 ggtitle("height estimated via nls vs. sampled height per plot and species over diamater")+
 theme_light()+
 theme(legend.position = "bottom")

# (nls-SP-P, nls-SP, sloboda, curtis, sampled)
ggplot(data = trees_total_7, 
       aes(x = DBH_cm, y = H_m, color = H_method))+
  geom_point()+
  #geom_line(method = "lm")+
  #geom_smooth(method = "nls", se=TRUE)+
  facet_wrap(plot_ID~SP_code)+
  xlab("DBH") +
  ylab("height [m]")+
  ggtitle("height estimated via nls vs. sampled height per plot and species over diamater")+
  theme_light()+
  theme(legend.position = "bottom")

# plot estimated and samples heights vs. diameter by species, plot ad height method
# (nls-SP-P, nls-SP, sampled)
ggplot(data = trees_total_6, 
       aes(x = DBH_cm, y = H_m, color = H_method))+
  geom_point()+
  #geom_line(method = "lm")+
  #geom_smooth(method = "nls", se=TRUE)+
  facet_wrap(plot_ID~SP_code)+
  xlab("DBH") +
  ylab("height [m]")+
  ggtitle("height estimated via nls vs. sampled height per plot and species over diamater")+
  theme_light()+
  theme(legend.position = "bottom")

# if we compare he heights estimated by the curtis function with those estimated by the self-
# made models, a descriptive/ visual analysis shows that the self-fitted functions appear to be 
# more accurate, thus I´ll have a look at the 






# ----- NOTES ------------------------------------------------------------------
# -----N. 1.4.1 assign DBH class to trees where DBH_class == 'NA' -----------------
# i removed this from above as i could solve the issue in one case with the species names
# create label for diameter classes according to BZE3 Bestandesaufnahmeanleitung
labs <- c(seq(5, 55, by = 5)) 
# replace missing DBH_class values with labels according to DBH_cm
trees_total <- trees_total%>%
  # change unit of height and diameter
  mutate(H_m = H_dm*0.1,                        #transform height in dm into height in m 
         DBH_cm = DBH_mm*0.1) %>%               # transform DBH in mm into DBH in cm 
  mutate(DBH_class = ifelse(is.na(DBH_class),   # mutate the column DBH_class if DBH_class is NA
                            cut(DBH_cm,         # cut the diameter
                                breaks = c(seq(5, 55, by = 5), Inf),  # in sequences of 5
                                labels = labs,                         # and label it according to labs
                                right = FALSE),
                            as.numeric(DBH_class)),    # else keep existing DBH class as numeric
         BA_m2 = c_A(DBH_cm/2)*0.0001,                 # 0.0001 to change unit from cm2 to m2
         plot_A_ha = c_A(12.62)*0.0001) %>%            # 0.0001 to change unit from m2 to hectar
  unite(ID_pt, plot_ID, t_ID, sep = "", remove = FALSE)# creae unique tree ID from comination of plot and tree number for later work with TapeR & TapeS





# ----- N.1.1. adding alnus rubrato species dataset -----------------------
SP_names %>% 
  # https://stackoverflow.com/questions/28467068/how-to-add-a-row-to-a-data-frame-in-r
  # there is species related information for Alnus rubra missing, so I am adding it manually
  # which is in-official, which is why there are no information on the IPC forest etc.
   add_row(Nr_code = NA, 
           Chr_code_ger = "REr",
           name = "Rot-Erle",
           bot_name = "Alnus rubra",
           bot_genus = "Alnus", 
           bot_species = "rubra",
           Flora_EU = NA, 
           LH_NH = "LB", 
           IPC = NA,
           WZE = NA, 
           BWI = "ER", # as there were no codes in d info for Alnus rubra available, I assign this species to Alnus spp. 
           BZE_al = NA)

# ----- N.1. Notes regarding linear regression of height for total dataset -----
# ----- N.1.0. adding missing DBH classes  -------------------------------------
# https://stackoverflow.com/questions/32683599/r-ifelse-to-replace-values-in-a-column
trees_total$DBH_class <- ifelse(is.na(trees_total$DBH_class),  # if DBH_class is NA
                               cut(trees_total$DBH_cm,                    # cut the diameter 
                                   breaks = c(seq(5, 550, by = 5), Inf),  # in sequences of 5 
                                   labels = labs,                         # and label it according to labs
                                   right = FALSE),
                               as.numeric(trees_total$DBH_class)) # else keep DBH class


# https://stackoverflow.com/questions/19379081/how-to-replace-na-values-in-a-table-for-selected-columns
trees_total_3 <- trees_total  %>%
 mutate(H_m = H_dm*0.1, 
        H_cm =H_dm*10, 
        DBH_cm = DBH_mm*0.1)  %>%
 mutate(DBH_class, ~replace_na(., cut(DBH_cm,
                                      breaks = c(seq(0, 545, by = 5), Inf),
                                      labels = labs,
                                      right = FALSE)))
# ----- N.1.1. units & filter dataset ------------------------------------------
# convert height in dm to m via mutate at 
# --> doesn´t work # https://suzan.rbind.io/2018/02/dplyr-tutorial-2/#mutate-at-to-change-specific-columns
# msleep %>%
#   select(name, sleep_total:awake) %>%
#   mutate_at(vars(contains("sleep")), ~(.*60))

trees_height_total <- trees_total %>%
 filter(!is.na(H_dm) & !is.na(DBH_mm)) %>% # & !is.na(`Alt`) )%>%  # filter for those rows where height is measured
 mutate(H_m = H_dm*0.1,                    # convert height in dm to m
        H_cm =H_dm*10, 
        DBH_cm = DBH_mm*0.1)# %>% 
#group_by(plot_ID, SP_code)# %>% 
# mutate_at(.cols = `Hoehe [dm]`, ~(.*0.1))  

# ----- N.1.2. attempts to extract regression coefficents form whole dataset grouped by SP and plot ID ------------------------------------------
# filter dataframe for heights for >= 3 height measurements per SP_code and plot_ID
min3h_plot_SP <- trees_total %>% 
 dplyr::select(plot_ID, 
               SP_code, SP_nr, 
               C_layer, Kraft, CH_dm,
               H_dm, DBH_mm, H_m, DBH_cm, DBH_class, 
               age) %>% 
 filter(!is.na(H_dm) & !is.na(DBH_mm)) %>% 
 group_by(plot_ID, SP_code) %>% 
 filter(n() >= 3)%>% 
 arrange(plot_ID, SP_code)

# data.table package
# https://www.tutorialspoint.com/how-to-perform-group-wise-linear-regression-for-a-data-frame-in-r
min3h_plot_SP.dt <- data.table(min3h_plot_SP)
min3h_plot_SP.dt[,as.list(coef(lm(H_m ~ DBH_cm))), by= SP_code]

# broom package
# https://stackoverflow.com/questions/1169539/linear-regression-and-group-by-in-r
min3h_plot_SP %>% 
 group_by(plot_ID, SP_code) %>% 
 do(model = lm(as.numeric(H_m) ~ as.numeric(DBH_cm), data = .)) %>% 
 rowwise() %>% 
 tidy(model)

#tidyverse package
# https://stackoverflow.com/questions/1169539/linear-regression-and-group-by-in-r
min3h_plot_SP.df <- data.frame(min3h_plot_SP)

min3h_plot_SP.df %>% 
 group_by(plot_ID, SP_code) %>% 
 nest() %>% 
 mutate(model = map(min3h_plot_SP.df, 
                    ~lm(H_m ~ DBH_cm, data = min3h_plot_SP.df)))



# THIS ONE WORKS!!!!

# extracting table for coefficient for linear regression 
#forestmanager package
# https://search.r-project.org/CRAN/refmans/forestmangr/html/lm_table.html
coeff_heights_lm <-  trees_total %>% 
 select(plot_ID, SP_code, H_m, DBH_cm, DBH_class) %>% 
 filter(!is.na(H_m) & !is.na(DBH_cm)) %>% 
 group_by(plot_ID, SP_code) %>% 
 # filter for plots where there is at least 3 heights measured for each species
 #https://stackoverflow.com/questions/20204257/subset-data-frame-based-on-number-of-rows-per-group
 filter(n() >= 3)%>%    
 #group_by(plot_ID, SP_code) %>% 
 lm_table(H_m ~ DBH_cm) %>%      # the lm models the height based on the diamater at breast heigt 
 arrange(plot_ID, SP_code) 





# ----- N.1.2.1. SUCCESSFULL attempt to extract COEFFICIENTS for n --------
# extracting height coefficients for non.linear models: 
# to calculate individual tree heights for trees of the samme species and plot 
# where the height has not been sampled I will create a linear regression for the heights
# in the following i will create a dataframe with regression coefficients per 
# species per plot if there are more then 3 heights measured per species and plot

# ----- N.1.2.1.1. nls coefficients per species and plot ------------------


# coefficents of non-linear height model 
# https://rdrr.io/cran/forestmangr/f/vignettes/eq_group_fit_en.Rmd
coeff_H_SP_P <-  trees_total %>% 
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
 arrange(plot_ID, SP_code)

# adding bias, rmse and rsqrd to the coefficent dataframe
coeff_H_SP_P <- left_join(trees_total %>% 
                              select(plot_ID, SP_code, H_m, DBH_cm, DBH_class) %>% 
                              filter(!is.na(H_m) & !is.na(DBH_cm) & !is.na(DBH_class)) %>% 
                              group_by(plot_ID, SP_code) %>% 
                              filter(n() >= 3),coeff_H_SP_P, by = c("plot_ID", "SP_code"))%>%
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

# per species but over all plots: 

# adding bias, rmse and rsqrd to the coefficent dataframe
coeff_H_SP <- left_join(trees_total %>% 
                         select(SP_code, H_m, DBH_cm, DBH_class) %>% 
                         filter(!is.na(H_m) & !is.na(DBH_cm) & !is.na(DBH_class)) %>% 
                         group_by(SP_code) %>% 
                         filter(n() >= 3),coeff_H_SP, by = c("SP_code"))%>%
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
 mutate(plot_ID = 'all') %>% 
 select(plot_ID, SP_code, b0, b1, b2, bias, rsme, R2, mean_h, N, SSres, SStot, pseu_R2, diff_h)



# ----- N. 2.1.3. COBINED coeffcient dataframe wth statistical indic species-&plotwise + specieswise coefficents ---------------------
# biuilding one dataset with all coefficients, thse of the genearl model and those of the species and plot specific models
coeff_nls_h_combined <- rbind(
  # this 1st left join is the height coefficients dataset per plot and species with the respective predictors
  (left_join(trees_total %>% 
               select(plot_ID, SP_code, H_m, DBH_cm, DBH_class) %>% 
               filter(!is.na(H_m) & !is.na(DBH_cm) & !is.na(DBH_class)) %>% 
               group_by(plot_ID, SP_code) %>% 
               filter(n() >= 3),
             # joining the coefficents per species and plot to the trees dataset 
             coeff_H_SP_P, by = c("plot_ID", "SP_code"))%>%
     # predict the heights per speces and plot via joined coefficients and calculate RSME, BIAS, R2, etc. 
     mutate(H_est = b0 * (1 - exp( -b1 * DBH_cm))^b2, 
            plot_ID = as.factor(plot_ID)) %>% 
     group_by(plot_ID, SP_code) %>% 
     summarise( b0 = mean(b0), 
                b1 = mean(b1), 
                b2 = mean(b2), 
                # adding Bias, RSME, R2, pseudo R2 and difference between predicted and sampled heights to dataframe
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
                diff_h = mean(H_m - H_est))), 
  # this left join creates a dataset with height coefficients per species over all plots and model predictors by
  # joining the tree dataset with the coefficients_all dataset 
  (left_join(trees_total %>% 
               select(SP_code, H_m, DBH_cm, DBH_class) %>% 
               filter(!is.na(H_m) & !is.na(DBH_cm) & !is.na(DBH_class)) %>% 
               group_by(SP_code) %>% 
               filter(n() >= 3),
             # joining coefficents from species wise models over all plots
             coeff_H_SP, by = c("SP_code"))%>%
     # estimate height through joined parameters and calcualte RSME, BIAS, R2 etc. 
     mutate(H_est = b0 * (1 - exp( -b1 * DBH_cm))^b2) %>% 
     group_by(SP_code) %>% 
     summarise( b0 = mean(b0), 
                b1 = mean(b1), 
                b2 = mean(b2), 
                # adding Bias, RSME, R2, pseudo R2 and difference between predicted and sampled heights to dataframe
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
     mutate(plot_ID = as.factor("all")) %>% 
     select(plot_ID, SP_code, b0, b1, b2, bias, rsme, R2, mean_h, N, SSres, SStot, pseu_R2, diff_h)))


# ----- N.1.2.2. Attempts to join coefficients to tree dataset depending on --------
# The issue is the following: if the R2 of the species per plot is really poor,
# we need the coefficients of a more general model over all plots or plot groups to be joined
# if the respective r2 of that modelis still to low, we want R to use an external/ different model

# thus I have two ideas:
# 1. preparing coefficent dataset
# either I modify the coefficents table, meaning that i replace the coefficients
# plots and species where r2 < threshold. this, however, only word if the
# coefficents are exactly the same, so not for external models
# 2. trying an conditional join
# here i try to create a conditional joint/ merge of the coefficients which states that for a given (previously joined) R2 R should choose the coefficients or even fomulas to use from different datasets
# 3. coefficents
# I could also create vectors with the coefficents, no? and then tell R to use a, b, c, d, ... etc depending on the respective R2? 
# 4. use of different functions accordint to R2 

# THIS ONE WORKS
# checking if the code worked and the right columns were picked
trees_total_1 <- left_join(trees_total,
                          coeff_H_SP_P %>% 
                            select(plot_ID, SP_code, R2, b0, b1, b2), 
                          by = c("plot_ID", "SP_code")) %>% 
 # if R2 or the coefficients are NA use the more general model
 left_join(., coeff_H_SP %>% select(SP_code, R2, b0, b1, b2),
           by = "SP_code") %>% 
 mutate(R2 = ifelse(is.na(R2.x), R2.y, R2.x), 
        b0 = ifelse(is.na(b0.x), b0.y, b0.x), 
        b1 = ifelse(is.na(b1.x), b1.y, b1.x), 
        b2 = ifelse(is.na(b2.x), b2.y, b2.x)) %>% 
 # if the R2 of the species and plot specific regressions is lower then the R2 of the regressions across plots, use the coefficients from more general models
 left_join(., coeff_H_SP %>% select(SP_code, R2, b0, b1, b2),
           by = "SP_code") %>% 
 mutate(R2 = ifelse(R2.x.x < R2.y.y, R2.y.y, R2.x.x), 
        b0 = ifelse(R2.x.x < R2.y.y, b0.y.y, b0.x.x), 
        b1 = ifelse(R2.x.x < R2.y.y, b1.y.y, b1.x.x), 
        b2 = ifelse(R2.x.x < R2.y.y, b2.y.y, b2.x.x)) 
# remove the columns that were created by joiing the same variable multiple times (which was necesarry to compare)
#select(-c(ends_with(c(".x", ".x.x")), ends_with(c(".y", ".y.y")))) %>% 
#mutate(H_m = is.na(H_m), b0 * (1 - exp( -b1 * DBH_cm))^b2, H_m)


# !!!!!! THIS ONE WORKS EVEN BETTER !!!!!!
# joining respective coefficients with ifelse shortend: 
# first I join the coefficient of the models fitted per plot and species
trees_total_1 <- left_join(trees_total,
                          coeff_H_SP_P %>% 
                            select(plot_ID, SP_code, R2, b0, b1, b2), 
                          by = c("plot_ID", "SP_code")) %>% 
 # if R2 or the coefficients are NA use the respective columns of the more general model
 left_join(., coeff_H_SP %>% select(SP_code, R2, b0, b1, b2),
           by = "SP_code") %>% 
 # if R2 or coefficients are NA or R2of the species- & plotwise models is lower then the R2 of the spcieswise models across all plots
 # ise the respecitive column
 mutate(R2 = ifelse(is.na(R2.x)| R2.x < R2.y, R2.y, R2.x), 
        b0 = ifelse(is.na(b0.x)| R2.x < R2.y, b0.y, b0.x),
        b1 = ifelse(is.na(b1.x)| R2.x < R2.y, b1.y, b1.x),
        b2 = ifelse(is.na(b2.x)| R2.x < R2.y, b2.y, b2.x)) %>% 
 select(-c(ends_with(".x"), ends_with(".y"))) %>%
 # adding column to be able to track if the height was measured or estimated by the model
 mutate(H_method = ifelse(is.na(H_m), 'est', 'samp'), 
        # estimate missing heights
        H_m = ifelse(is.na(H_m), b0 * (1 - exp( -b1 * DBH_cm))^b2, H_m))


# joining respective coefficients with function (1.3.) 
# --> this code cannot apply the curtis function yet, but it would be possible to include that too 
trees_total_6 <- trees_total %>%
  left_join(.,coeff_H_SP_P %>% 
              select(plot_ID, SP_code, R2, b0, b1, b2), 
            by = c("plot_ID", "SP_code")) %>% 
  # if R2 or the coefficients are NA use the respective columns of the more general model
  left_join(., coeff_H_SP %>% select(SP_code, R2, b0, b1, b2),
            by = "SP_code") %>% 
  # this reffers to the function and meas: if R2 from coeff_H_SP_P is NA or if 
  # R2 from coeff_H_SP_P is smaller then R2 from coeff_H_SP then use the coeff_H_SP values
  # if not, keep the coeff_H_SP_P values
  mutate(H_method = case_when(is.na(H_m) & is.na(R2.x)| is.na(H_m) & R2.x < R2.y ~ "coeff_SP", 
                              is.na(H_m) & R2.x > R2.y ~ "coeff_SP_P",
                              !is.na(H_m) ~ "sampled", 
                              TRUE ~ "other")) %>% 
  mutate(R2 = f(R2.x, R2.y, R2.y, R2.x), 
         b0 = f(R2.x, R2.y, b0.y, b0.x), 
         b1 = f(R2.x, R2.y, b1.y, b1.x),
         b2 = f(R2.x, R2.y, b2.y, b2.x)) %>% 
  mutate(#H_method = ifelse(is.na(H_m), 'est', 'samp'), 
         # estimate missing heights
         H_m = ifelse(is.na(H_m), b0 * (1 - exp( -b1 * DBH_cm))^b2, H_m)) %>% 
  select(-c(ends_with(".x"), ends_with(".y")))

  
  
view(trees_total_5 %>% filter(plot_ID == 29090))

view(trees_total_1 %>% filter(is.na(R2.x)| R2.x.x < R2.y.y) %>% select(plot_ID, SP_code, R2.x, R2.y, R2.x.x, R2.y.y, R2, b0.x.x, b0.y.y, b0)) 


# check if the sorter verion of this code used above is also accurate
identical(trees_total[['R2']],trees_total_1[['R2']]) # --> yes it is


# joining coefficients & curtis function
trees_total_7 <- trees_total %>%
  unite(SP_P_ID, plot_ID, SP_code, sep = "", remove = FALSE) %>%            # create column matching vectorised coefficients of coeff_SP_P (1.3. functions, h_nls_SP_P, dplyr::pull)
  left_join(.,coeff_H_SP_P %>%                                              # joining R2 from coeff_SP_P -> R2.x
              select(plot_ID, SP_code, R2) %>% 
              unite(SP_P_ID, plot_ID, SP_code, sep = "", remove = FALSE),   # create column matching vectorised coefficients of coeff_SP_P (1.3. functions, h_nls_SP_P, dplyr::pull)
            by = c("plot_ID", "SP_code", "SP_P_ID")) %>% 
  left_join(., coeff_H_SP %>% select(SP_code, R2), 
            by = "SP_code") %>%       # joing R2 from coeff_SP data set -> R2.y
  mutate(R2_comb = f(R2.x, R2.y, R2.y, R2.x),                               # if R2 is na, put R2 from coeff_SP_P unless R2 from coeff_SP is higher
         H_method = case_when(is.na(H_m) & !is.na(R2.x) & R2.x > 0.70 | is.na(H_m) & R2.x > R2.y & R2.x > 0.7 ~ "coeff_SP_P", 
                              is.na(H_m) & is.na(R2.x) & R2.y > 0.70| is.na(H_m) & R2.x < R2.y & R2.y > 0.70 ~ "coeff_sp",
                              is.na(H_m) & is.na(R2_comb) | is.na(H_m) & R2_comb < 0.70 ~ "h_curtis", 
                              TRUE ~ "sampled")) %>% 
  # When h_m is na but there is a plot and species wise model with R2 above 0.7, use the model to predict the height
  mutate(H_m = case_when(is.na(H_m) & !is.na(R2.x) & R2.x > 0.70 | is.na(H_m) & R2.x > R2.y & R2.x > 0.7 ~ h_nls_SP_P(SP_P_ID, DBH_cm),
                         # if H_m is na and there is an R2 from coeff_SP_P thats bigger then 0.75 or of theres no R2 from 
                         # coeff_SP_plot that´s bigger then R2 of coeff_SP_P while the given R2 from coeff_SP_P is above 
                         # 0.75 then use the SP_P models
                         is.na(H_m) & is.na(R2.x) & R2.y > 0.70 | is.na(H_m) & R2.x < R2.y & R2.y > 0.70 ~ h_nls_SP(SP_code, DBH_cm),
                         # when there´s still no model per species or plot, or the R2 of both self-made models is below 0.7 
                         # and hm is na use the curtis function
                         is.na(H_m) & is.na(R2_comb) | is.na(H_m) & R2_comb < 0.70 ~ h_curtis(BWI_SP_group, DBH_mm), 
                         TRUE ~ H_m))














# as non of the follwing codes workd i had to find another solution. 
# my ideas are as follows: 
# something like dyplr and join by R2 < R2? 
# what i want to do is: 
# coefficients and r2  originating from Sp and Plot specific models should be replaced by coefficients 
# if their R2 is higher then the R2 of the respective excisting column
# in a second step i want those coefficients that are linked to a poor R2 
# (so an R2 below a certain threshold) to be replaced by coefficients/ models with better performance
# but first I have to find these models somehow... or get the heights from BD for those plots & species where the R2 was poor 
# I coul dput R2 Na to 0 so that i can be sure the respective coeffcients of a more general model is joined to the dataset


# if there is an R2 per plot per species 
if (trees_total[c('plot_ID', 'SP_code')] %in% 
   #join it to the trees dataset from the columns in coeff-combined that have oefficients per plot and species
   coeff_nls_h_combined){
 merge(trees_total, coeff_nls_h_combined[, c("plot_ID", "SP_code", "R2")],
       by = c('plot_ID', 'SP_code'))
 # if there is an R2 or the R2 is above 0.75 merge the coefficeints from the coeffcients per Sp per  plots dataset
} 


# via hested if statment
# if the plot ID and SP code appear in the plot and species wise coefficient dataset, join the coefficients of models that were fitted plot- and species-wise
# 1. CONDITION: if the merged R2 is lower then the R2 from the general model                            
trees_total_3 <- if (trees_total$R2[trees_total$SP_code] < coeff_H_SP$R2[coeff_H_SP$SP_code]){
 # 2. CONDITION : if the merged R2 is NA --> if there was no plot & species specific model
 if (is.na(trees_total$R2)){
   #  join the coefficients of a more general model, fitted per species across all plots
   merge(trees_total, coeff_H_SP[, c("SP_code", "R2", "b0", "b1", "b2")],
         by = 'SP_code')
 } # else merge the coefficients and R squared of the plot $ species specific models
}else merge(trees_total, coeff_H_SP_P[, c("plot_ID", "SP_code", "R2", "b0", "b1", "b2")],
           by = c('plot_ID', 'SP_code')) 


# if the R2 of the species- and plotwise models is lower then the species wise model accross all plots
trees_total_4 <- if (coeff_H_SP_P$R2 < coeff_H_SP$R2) { 
 # DO IF TRUE: merge coefficients form a more general model to the tree dataset
 merge(trees_total, coeff_H_SP[, c("SP_code", "R2", "b0", "b1", "b2")],
       by = 'SP_code')
 
}else merge(trees_total, coeff_H_SP[, c("SP_code", "R2", "b0", "b1", "b2")],
           by = 'SP_code')
# else join the coefficients of a more general model, fitted per species across all plots

# look where errors occure: 
anti_join(trees_total, trees_total_3, by = c("plot_ID", "SP_code"))








# via if statement 
# if nte joined R2 is below 0.5
trees_total_1 <- if(trees_total$R2 <= 0.5){
 merge(trees_total, 
       coeff_nls_h_combined[coeff_nls_h_combined$plot_ID == 'all' & coeff_nls_h_combined$R2 > 0.5, ][, c("SP_code", "b0", "b1", "b2")],
       by = 'SP_code') 
 #if non of statements is not true, so R2 >= 0.75, join coefficients of the SP- and Plot-wise models 
} else merge(trees_total, coeff_nls_h_combined[coeff_nls_h_combined$plot_ID != 'all', ][, c("plot_ID", "SP_code", "b0", "b1", "b2")],
            by = c('plot_ID', 'SP_code'))

# if the R2 joined from the SP and plotwise height coefficients is NA (so there was no model build for these plots and species) 
trees_total_2 <- if (trees_total$R2 =='NA'){
 # join the coefficients and R2 of the height models across all plots 
 merge(trees_total, coeff_nls_h_combined[coeff_nls_h_combined$plot_ID == 'all', ][, c("SP_code", "R2", "b0", "b1", "b2")],
       by = 'SP_code')
}else merge(trees_total, coeff_nls_h_combined[coeff_nls_h_combined$plot_ID != 'all', ][, c("plot_ID", "SP_code", "b0", "b1", "b2")],
           by = c('plot_ID', 'SP_code'))

# via nested if else statement: 
# if the R2 joined from the SP and plotwise height coefficients is NA (so there was no model build for these plots and species) 
trees_total_2 <- if (trees_total$R2 =='NA'){
 # join the coefficients and R2 of the height models across all plots 
 merge(trees_total, coeff_nls_h_combined[coeff_nls_h_combined$plot_ID == 'all', ][, c("SP_code", "R2", "b0", "b1", "b2")],
       by = 'SP_code')
 # this was my attempt of an nested if statement but it doesn´t work :(
 # if the R two is too low, use the coefficients of the height models across all plots
} else if(trees_total$R2 <= 0.75) {
 merge(trees_total, 
       coeff_nls_h_combined[coeff_nls_h_combined$plot_ID == 'all', ][, c("SP_code", "b0", "b1", "b2")],
       by = 'SP_code')
 # if non of these statements is true, neiter R2 == NA nor R2 <= 0.75, join coefficients of the SP- and Plot-wise models   
}else merge(trees_total, coeff_nls_h_combined[coeff_nls_h_combined$plot_ID != 'all', ][, c("plot_ID", "SP_code", "b0", "b1", "b2")],
           by = c('plot_ID', 'SP_code'))


# via ifelse statment: 
# ifelse(condition, do_if_true, do_if_false) 
# trees_total.1 <- ifelse (trees_total$R2 != 'NA' | trees_total$R2 >= 0.75, 
#        left_join(trees_total, 
#                  coeff_nls_h_combined %>% filter(plot_ID != "all") %>% select("plot_ID", "SP_code", "b0", "b1", "b2"), 
#                   by = c('plot_ID', 'SP_code')), 
# if there is no R2 or the R2 is below 0.75 merge the coefficeints from the coeffcients per Sp across all plots dataset
#        left_join(trees_total, coeff_H_SP %>% filter(plot_ID == "all") %>%  select("SP_code", "b0", "b1", "b2"),
#                   by = "SP_code"))

# ----- N.2. properly fit height model -----------------------------------------

# ----- N.2.2. height model for all species together ---------------------------
# ----- N.2.2.2. create training and testing / validation dataset --------------
# https://stackoverflow.com/questions/17200114/how-to-split-data-into-training-testing-sets-using-sample-function
## set sample size to 50% of the dataset --> split data in half
smp_size <- floor(0.75 * nrow(min3h_plot_SP))
## set the seed to make your partition reproducible
set.seed(123) 
train_ind <- sample(seq_len(nrow(min3h_plot_SP)), size = smp_size)
# create training and testin/ validation dataset
h_train <- min3h_plot_SP[train_ind, ]
h_test <- min3h_plot_SP[-train_ind, ]

# ----- N.2.2.3. check out potential explainatory variables  ---------------------
pairs(h_train %>% dplyr::select(H_m, as.numeric(SP_code), as.numeric(SP_nr), C_layer, Kraft, 
                               DBH_class,
                               CH_dm, DBH_cm))

# potetial explainatory variables: 
#   - SP_code
#   - DBH_class
#   - age    --> including age did not work because of to little variability and little data availability
#   - Kraft
#   - DBH_cm

# ----- N.2.2.4 build model with train data --------------------------------------
# create different models with different combinations of the explainatory variables 
h.tot.1 <- lm(formula = H_m ~ DBH_class + as.factor(SP_code) + Kraft + DBH_cm, 
             data = h_train)
h.tot.2 <- lm(formula = H_m ~ DBH_class + as.factor(SP_code) + Kraft, 
             data = h_train)
h.tot.3 <- lm(formula = H_m ~ DBH_class + as.factor(SP_code), 
             data = h_train)
h.tot.4 <- lm(formula = H_m ~ DBH_class, 
             data = h_train)
h.tot.5 <- lm(formula = H_m ~ DBH_class + Kraft + DBH_cm, 
             data = h_train)
h.tot.6 <- lm(formula = H_m ~ DBH_class + DBH_cm, 
             data = h_train)
h.tot.7 <- lm(formula = H_m ~  as.factor(SP_code) + Kraft + DBH_cm, 
             data = h_train)
h.tot.8 <- lm(formula = H_m ~  as.factor(SP_code) + DBH_cm, 
             data = h_train)

# check AIC of the models: 
# https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/AIC
# https://www.statology.org/aic-in-r/
models.h.tot <- list(h.tot.1, h.tot.2, h.tot.3, h.tot.4, h.tot.5, h.tot.6, h.tot.7, h.tot.8)
AIC(h.tot.1, h.tot.2, h.tot.3, h.tot.4, h.tot.5, h.tot.6, h.tot.7, h.tot.8)
# RESULTS AIC:
# model    df  AIC
# h.tot.1  9  644.6449
# h.tot.2  8  643.2544 ---> lowest AIC --> best model
# h.tot.3  8  797.3503 ---> summary and ANOVA, however, indicate that h.tot.3 is the best model
# h.tot.4  3  841.9807
# h.tot.5  5  685.6593
# h.tot.6  4  843.2195
# h.tot.7  8  961.1056
# h.tot.8  8 1249.8632

# check summary and anova of model with best AIC
summary(h.tot.2) # --> only SP_code and DBH_class are significant
anova(h.tot.2)   # --> only SP_code and DBH_class are significant
# check summar yand anova for model h.tot.3 with only SP_code and DBH_class
summary(h.tot.3) # --> only SP_code and DBH_class are significant
anova(h.tot.3)   # --> only SP_code and DBH_class are significant
# --> h.tot.3. is slightly simpler and shows a better R2 

# diagonostic plots: 
plot(h.tot.2) # --> cooks distance looks more evenly spread
plot(h.tot.3)


# RESULTS h.tot.3:  H_m ~ DBH_class + as.factor(SP_code), data = h_train)
#                           Estimate   Std. Error t value  Pr(>|t|)    
# (Intercept)                1.1088     0.7686    1.443    0.151    
# DBH_class                  2.6664     0.1179    22.618   < 2e-16 ***
# as.factor(SP_code)GFI      4.4935     0.8060    5.575    1.03e-07 ***
# as.factor(SP_code)GKI      1.2593     2.6968    0.467    0.641    
# as.factor(SP_code)MBI      6.7342     0.9992    6.740    2.72e-10 ***
# as.factor(SP_code)RER      5.9254     0.8556    6.926    1.00e-10 ***
# as.factor(SP_code)STK      2.5248     1.9608    1.288    0.200    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.565 on 160 degrees of freedom
# (46 Beobachtungen als fehlend gelöscht)
# Multiple R-squared:  0.8063,	Adjusted R-squared:  0.799 
# F-statistic:   111 on 6 and 160 DF,  p-value: < 2.2e-16

# resuting function: 
# h_m_est = 1.1088 + DBH_class*2.6664 + GKI*1.2593 + RER*5.9254 + STK*2.5248 + GFI*4.4935 + MBI*6.7342 + BKI 

# ----- N.2.2.5 validate model with test data ------------------------------------
# ----- N.2.2.5.1. fit h.tot.3 for test data -------------------------------------
h.3.val <- lm(formula = H_m ~ DBH_class + as.factor(SP_code), data = h_test)
summary(h.3.val)
anova(h.3.val) # only DBH_class is significant
plot(h.3.val)

# Coefficients:
#                         Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             1.7339     2.3834   0.727   0.4702    
# DBH_class               2.5774     0.2566   10.045  1.11e-13 ***
# as.factor(SP_code)GFI   4.1261     2.4448   1.688   0.0976 .  
# as.factor(SP_code)MBI   5.7992     2.6626   2.178   0.0341 *  
# as.factor(SP_code)RER   5.1307     2.6426   1.942   0.0577 .  
# as.factor(SP_code)STK   2.8500     3.3264   0.857   0.3956    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 3.326 on 51 degrees of freedom
# (14 Beobachtungen als fehlend gelöscht)
# Multiple R-squared:  0.7349,	Adjusted R-squared:  0.7089 
# F-statistic: 28.28 on 5 and 51 DF,  p-value: 1.324e-13

# ----- N.2.2.5.2. estimate heights via model h.tot.3 for height dataset -------
# assign trees species as variables
GKI <- trees_height_total$SP_code[trees_height_total$SP_code == "GKI"]
RER <- trees_height_total$SP_code[trees_height_total$SP_code == "RER"]
STK <- trees_height_total$SP_code[trees_height_total$SP_code == "STK"]
GFI <- trees_height_total$SP_code[trees_height_total$SP_code == "GFI"]
MBI <- trees_height_total$SP_code[trees_height_total$SP_code == "MBI"]
BKI <- trees_height_total$SP_code[trees_height_total$SP_code == "BKI"]

# ass column with estimated tree heights 
trees_height_total <- trees_height_total %>% 
 mutate(H_m_est = 1.1088 + DBH_class*2.6664+
          as.numeric(SP_code)) # I know that the numeric doesn´t make sense
#as.numeric(GKI)*1.2593 + as.numeric(RER)*5.9254 + as.numeric(STK)*2.5248 + as.numeric(GFI)*4.4935 + as.numeric(MBI)*6.7342 + as.numeric(BKI))
# actually I would have to write it like this: https://advstats.psychstat.org/book/mregression/catpredictor.php
#as.numeric(GKI)*1.2593 + as.numeric(RER)*5.9254 + as.numeric(STK)*2.5248 + as.numeric(GFI)*4.4935 + as.numeric(MBI)*6.7342)
#GKI*1.2593 + RER*5.9254 + STK*2.5248 + GFI*4.4935 + MBI*6.7342)

# checking if the heights are signidicantly diffferent
shapiro.test(trees_height_total$H_m) # not normally distributed
wilcox.test(trees_height_total$H_m, trees_height_total$H_m_est) 
# --> means of predicted and sampled height are significantly different

# ----- N.2.2.5.3. visualize estim. vs. sampled heights ------------------------
# plot estiamted vs. mesured tree height
# scatter plot
ggplot(data=trees_height_total, aes(x = H_m_est, y = H_m))+
 geom_point()+
 stat_smooth(method="lm",se=TRUE)+
 xlab("height [m] predicted by model h.tot.3") +
 ylab("height [m] measured")+
 ggtitle("all species: measured height [m] vs. height [m] predicted by model h.tot.3")+
 theme_light()+
 theme(legend.position = "non")

# boxlot
# create dataset for boxplot
box_h_mes <- trees_height_total %>% 
 dplyr::select(H_m) %>% 
 mutate(H_meth = "sampled",
        height_m = H_m) 
box_h_est <- trees_height_total %>% 
 dplyr::select(H_m_est) %>% 
 mutate(H_meth = "estimated", 
        height_m = H_m_est)
box_h <- rbind(box_h_mes %>% select(H_meth, height_m), box_h_est%>% select(H_meth, height_m))

ggplot(box_h, aes(x = as.factor(H_meth), y = height_m))+
 geom_boxplot()+
 xlab("predicted by model h.tot.3 vs. sampled tree height") +
 ylab("height [m]")+
 ggtitle("all species: measured height [m] vs. height [m] predicted by model h.tot.3")+
 theme_light()+
 theme(legend.position = "non")



# ----- N.2.3. properly fit one linear model per species -----------------------
# ----- N.2.3.1. find out how many species are there ---------------------------
trees_total %>% group_by(SP_code) %>% 
 distinct(SP_code)
# RER   Rot Erle                      Alnus rubra
# STK   Spätblühende Traubenkirsche   Prunus serotina 
# GFI   Gemeine Fichte                Picea abies 
# MBI   Moorbirke                     Betula pubescens
# BKI   Bergkiefer                    Pinus mugo
# GKI   Gemeine Kiefer                Pinus silvatica


# ----- N.2.3.2. Rot Erle Alnus rubra --------------------------------------------
# ----- N.2.3.2.1. filer rows where height != NA and species = Alnus rubra --------
height_RER <- min3h_plot_SP %>% 
 filter(!is.na(H_m) & !is.na(Kraft) & !is.na(C_layer) & SP_code == "RER") #%>% 
# mutate(H_m = `Hoehe [dm]`*0.1, 
#        H_cm = `Hoehe [dm]`*10, 
#        DBH_cm = `BHD [mm]`/10)   # add column with height in meter 1dm = 0.1m

# ----- N.2.3.2.2. create training and testing / validation dataset --------------
# https://stackoverflow.com/questions/17200114/how-to-split-data-into-training-testing-sets-using-sample-function
## set sample size to 50% of the dataset --> split data in half
smp_size <- floor(0.75 * nrow(height_RER))
## set the seed to make your partition reproducible
set.seed(123) 
train_ind <- sample(seq_len(nrow(height_RER)), size = smp_size)
# create training and testin/ validation dataset
h_RER_train <- height_RER[train_ind, ]
h_RER_test <- height_RER[-train_ind, ]

# ----- N.2.3.2.3. check out potetial explainatory variables ---------------------
pairs(height_RER %>% dplyr::select(H_dm, H_m, #as.numeric(C_layer), 
                                  #as.numeric(Kraft), 
                                  DBH_class, 
                                  CH_dm, DBH_mm))
# --> DBH class seems most promissing

# ----- N.2.3.2.3. create model based on diameter --------------------------------
h.RER <- lm(formula = H_m ~ DBH_class, 
           data = h_RER_train)
summary(h.RER) # model summary
anova(h.RER)   # --> Pr(>F) = 0.0005993 *** ---> DBH class is significant
plot(h.RER)

# RESULTS h_RER: 
# Coefficients:
#              Estimate    Std. Error t value  Pr(>|t|)    
# (Intercept)  12.5041     1.9774     6.323    2.31e-06 ***
#   DBH_class  1.6400      0.4097     4.003    0.000599 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 409 on 22 degrees of freedom
# (45 Beobachtungen als fehlend gelöscht)
# Multiple R-squared:  0.4214,	Adjusted R-squared:  0.3951 
# F-statistic: 16.02 on 1 and 22 DF,  p-value: 0.0005993

# ----- N.2.3.2.4. validate model based on diameter with test data----------------
h.RER.val <- lm(formula = H_m ~ DBH_class, data = h_RER_test)
summary(h.RER.val)
anova(h.RER.val) # --> Pr(>F) = 0.001278 ** --> significant
plot(h.RER.val)


# Coefficients:
#              Estimate   Std. Error t value  Pr(>|t|)   
# (Intercept)  10.2311    2.0548     4.979    0.00108 **
#   DBH_class  2.0900     0.4313     4.846    0.00128 **
---
 #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
 # 
 # Residual standard error: 331 on 8 degrees of freedom
 # (14 Beobachtungen als fehlend gelöscht)
 # Multiple R-squared:  0.7459,	Adjusted R-squared:  0.7141 
 # F-statistic: 23.48 on 1 and 8 DF,  p-value: 0.001278
 
 
 # create column for predicted height and plot predicated and measured height against each other 
 height_RER <- height_RER %>% 
 mutate(H_m_est = (12.5041 + DBH_class*1.6400)) 

# test for differences between predicted and sampled height
t.test(height_RER$H_m, height_RER$H_m_est) # p-value = 2.907e-06 --> significantly different :/

# visualize differences between estimated and measured height
ggplot(data=height_RER, aes(x = H_m_est, y = H_m))+
 geom_point()+
 stat_smooth(method="lm",se=TRUE)+
 xlab("height [m] predicted by model h_RER") +
 ylab("height [m] measured")+
 ggtitle("Alnus rubra: measured height [m] vs. height [m] predicted by model h_RER")+
 theme_light()+
 theme(legend.position = "non")



# ----- N.2.3.3.1. fit non linear model for RER heights -----------------------------
# https://stackoverflow.com/questions/33033176/using-r-to-fit-a-sigmoidal-curve
# https://data-flair.training/blogs/r-nonlinear-regression/
# model<- nls(bone~a-b*exp(-c*age),start=list(a=120,b=110,c=0.064))

h.RER.nls <- nls(H_m~a-b*exp(-c*DBH_cm),
                start=list(a=110,b=120,c=0.1),
                data = h_RER_train)
summary(h.RER.nls) # model summary
plot(h.RER.nls)


# ----- N.2.3.2.4. validate model based on diameter with test data----------------
h.RER.val.nls <- nls(H_m~a-b*exp(-c*DBH_cm),
                    start=list(a=21.73725,b=32.54252,c=0.09471),
                    data = h_RER_test)
summary(h.RER.val.nls)
plot(h.RER.val.nls)


# create column for predicted height and plot predicated and measured height against each other 
height_RER <- height_RER %>% 
 mutate(H_m_est_nls = 21.73725-32.54252*exp(-0.09471*DBH_cm))


# test for differences between predicted and sampled height
t.test(height_RER$H_m, height_RER$H_m_est_nls)

# visualize differences between estimated and measured height
ggplot(data=height_RER, aes(x = H_m_est_nls, y = H_m))+
 geom_point()+
 stat_smooth(method="lm",se=TRUE)+
 xlab("height [m] predicted by model h_RER") +
 ylab("height [m] measured")+
 ggtitle("Alnus rubra: measured height [m] vs. height [m] predicted by model h_RER")+
 theme_light()+
 theme(legend.position = "non")

ggplot(data=height_RER, aes(x = DBH_class, y = H_m_est_nls))+
 geom_point()+
 #geom_point(aes(x = DBH_cm, y = H_m))+
 stat_smooth(method = "loess", se=TRUE)+
 xlab("DBH in cm ") +
 ylab("height [m] predicted by model h_RER")+
 ggtitle("Alnus rubra: height [m] predicted by model h_RER vs. DBH")+
 theme_light()+
 theme(legend.position = "non")

ggplot(data=height_RER, aes(x = DBH_class, y = H_m))+
 geom_point()+
 #geom_point(aes(x = DBH_cm, y = H_m))+
 stat_smooth(method = "loess", se=TRUE)+
 xlab("DBH in cm ") +
 ylab("height [m] sampled ")+
 ggtitle("Alnus rubra: height [m] sampled vs. DBH")+
 theme_light()+
 theme(legend.position = "non")




# ----- N.2.2.2. TapeR ----------------------------------------------------
# ----- 2.2.2.2.TapeR_FIT_LME.f ----------------------------------------------------------
# aparently I first have to fit the taper curves in general via TapeR_FIT_LME.f
# input variabels are: 
# Id,
# x (heights measured), y (respective diameters measured), 
# knt_x (knots position of fixed effects), ord_x (oder of fixed effefcts 4 = cubic),
# knt_z (knots position of random effects), ord_z (oder of random effefcts 4 = cubic),
# IdKOVb = "pdSymm", control = list()

Id = (trees_total[,"ID"][trees_total$H_method == 'samp'])
x =  (((trees_total[,"DBH_h_cm"][trees_total$H_method == 'samp'])/100)/(trees_total[,"H_m"][trees_total$H_method == 'samp']))
y = (trees_total[,"DBH_cm"][trees_total$H_method == 'samp'])
#na.f <-  function(z) if(is.na(z)) {z=FALSE} else {if(z) {z}}



knt_x = c(0.0, 0.1, 0.75, 1.0); ord_x = 4 # B-Spline knots: fix effects; order (cubic = 4)
knt_z = c(0.0, 0.1 ,1.0); ord_z = 4 # B-Spline knots: rnd effects
# fit the model
taper.model <- TapeR_FIT_LME.f(Id, x, y, knt_x, ord_x, knt_z, ord_z,
                               IdKOVb = "pdSymm")



help("getHeight")
getHeight()

# load example data
data(DxHx.df)

# prepare the data (could be defined in the function directly)
Id = DxHx.df[,"Id"]
x = DxHx.df[,"Hx"]/DxHx.df[,"Ht"]#calculate relative heights
y = DxHx.df[,"Dx"]
# define the relative knot positions and order of splines
# https://stackoverflow.com/questions/51064686/error-in-chol-defaultcxx-the-leading-minor-of-order-is-not-positive-definite
# I changed the knots to much higher numbers because I kept receiving following error: 
# Error in chol.default((value + t(value))/2) : 
# the leading minor of order 1 is not positive definite
knt_x = c(0.0, 0.1, 0.75, 1.0) # B-Spline knots: fix effects
ord_x = 4 # ord = order (4 = cubic)
knt_z = c(0.0, 0.1, 1.0); ord_z = 4 # B-Spline knots: rnd effects

# fit the model
taper.model.1 <- TapeR_FIT_LME.f(Id, x, y, knt_x, ord_x, knt_z, ord_z,
                                 IdKOVb = "pdSymm", data = trees_total)
taper.model.1

TapeR::E_HDx_HmDm_HT.f(Dx, 
                       Hm, 
                       Dm,
                       mHt,
                       sHt = 0, 
                       par.lme = taper.model$par.lme)

# ----- 2.2.8. getting diameter at 1/3 of the tree height for biomass ----------

#tree <- buildTree(tree = list(spp=1, D1=30, H=27)
getSpeciesCode()

spp <- getSpeciesCode(trees_total$bot_name, "short") 
D1 <- as.numeric(trees_total$DBH_cm)
H1 <- as.numeric(trees_total$DBH_h_cm) 
H <- as.numeric(trees_total$H_m)

tree <- as.data.frame(cbind(spp, D1, H1, H))
help("buildTree")

tree <- buildTree(tree)         
         
         
         
# total working days 2023 Brandenburg from February onwards: 
tot_wd = 229
ho_wd = tot_wd*0.5
already_used_ho = 12
ho_planned_04_spain = 5
ho_planned_0809_FR_SP = 10
ho_planned_12_warm = 10
ho_planned_12_christmas = 5
ho_planned_tot = ho_planned_04_spain + ho_planned_0809_FR_SP + ho_planned_12_warm + ho_planned_12_christmas
weeks_away = 1+2+2+1 # weeks taht i am spending all days in homeoffice
remainung_ho_days <- ho_wd - (already_used_ho + ho_planned_tot)
current_KW_week <- 12 # 20.03-26.03
rem_ho_days_week <- remainung_ho_days/(52 - (current_KW_week + weeks_away))
