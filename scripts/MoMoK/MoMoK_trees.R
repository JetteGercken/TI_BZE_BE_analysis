# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the peat land soil inventory
# Trees 


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

# ----- 1. DATA ----------------------------------------------------------------
# ----- 1.1. import ------------------------------------------------------------
# LIVING TREES
# as the CSVs come from excel with German settings, the delimiter is ';' and the decimals are separated by ','
# which is why I use "delim" to import the data: https://biostats-r.github.io/biostats/workingInR/005_Importing_Data_in_R.html
trees_total <- read.delim(file = here("data/input/MoMoK/trees_MoMoK_total.csv"), sep = ";", dec = ",") %>% 
  select(-Bemerkung) %>% 
  filter(!is.na("MoMoK_Nr"))

# DEADWOOD: table containing deadwood inventory data of the MoMoK plots
DW_total <- read.delim(file = here("data/input/MoMoK/DW_MoMoK_total.csv"), sep = ";", dec = ",", stringsAsFactors=FALSE)# %>% 
  #mutate(tpS_ID = NA) # this is just for the function, let´s see if it works

# REGENERATION:table containing regeneration inventory data of the MoMoK plots
RG_total <- read.delim(file = here("data/input/MoMoK/RG_MoMoK_total.csv"), sep = ";", dec = ",")

# this table displays the peatland specific conditions at the respective MoMoK plots
site_info <- read.delim(file = here("data/input/MoMoK/momok_STO_plots.csv"), sep = ";", dec = ",")


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


# BWI DATA
# c stock ha by age and species
BWI_C_age_SP <- read.delim(file = here("data/input/General/BWI_C_age_SP.csv"), sep = ";", dec = ",")
# National forest inventory (BWI) general stand characteristics per ha by species (simulated single species stand)
BWI_stand_char_SP <- read.delim(file = here("data/input/General/zielmerkmale_SP_2017.csv"), sep = ";", dec = ",")
# National forest inventory (BWI) volume of deadwood by deadwood type and federal state
BWI_DW_V <- read.delim(file = here("data/input/General/DW_BWI.csv"), sep = ";", dec = ",")
# National forest inventory (BWI) carbon stock of deadwood by deadwood type from "Kohlenstoffinventur 2017"
BWI_DW_C <- read.delim(file = here("data/input/General/DW_C_TY_2017.csv"), sep = ";", dec = ",")
# National forest inventory (BWI) carbon, volume and biomass per deadwood biomass class "Kohlenstoffinventur 2017"
BWI_DW_iB <- read.delim(file = here("data/input/General/BWI_DW_iB.csv"), sep = ";", dec = ",")  # iB = item Biomass --> biomass per deadwood item



# ----- 1.2. colnames, vector type --------------------------------------------------------
# ----- 1.2.1. living trees --------------------------------------------------------------
colnames(trees_total) <- c("plot_ID", "loc_name", "state", "date", "CCS_nr", 
                           "t_ID", "st_ID", "pieces", "SP_nr", "SP_code", "C_layer", 
                           "Kraft", "age", "age_m", "DBH_mm", "DBH_h_cm", 
                           "DBH_p_mm", "DBH_class", "H_dm", "CH_dm", 
                           "azimut_g", "azimut_d", "dist_m")
trees_total$C_layer <- as.numeric(trees_total$C_layer)
# there was a mistake in the species abbreviations where Alnus glutinosa was abbreviated with RER (Roterle) instead of SER (Schwarzerle)
trees_total$SP_code[trees_total$SP_code == "RER"] <- "SER"  
trees_total$SP_code <- as.factor(trees_total$SP_code)


# ----- 1.2.2. species list BZE --------------------------------------------------------------
colnames(SP_names) <- c("Nr_code", "Chr_code_ger", "name", "bot_name", "bot_genus", 
                        "bot_species", "Flora_EU", "LH_NH", "IPC", "WZE", "BWI",  
                        "BZE_al")


# ----- 1.2.3. dead wood --------------------------------------------------------------
colnames(DW_total) <- c("plot_ID", "loc_name", "state", "date", "CCS_nr", "t_ID",
                        "SP_group", "DW_type", "L_dm", "D_cm", "dec_type")
# changing DW variable D_cm from character into numeric variable
# https://stackoverflow.com/questions/11936339/replace-specific-characters-within-strings
DW_total$D_cm <- gsub(",", ".", DW_total$D_cm)
DW_total$D_cm <- as.numeric(DW_total$D_cm)
DW_total %>% filter(is.na(D_cm))
DW_total <- DW_total %>% filter(!is.na(D_cm))
# SP_group = Baumartengruppe totholz
# DW_type = standing, lying, etc. 
# dec_type = decay type / Zersetzungsgrad


# ----- 1.2.4. regeneration --------------------------------------------------------------
colnames(RG_total) <- c("plot_ID", "loc_name", "state", "date", "CCS_nr", "CCS_position", 
                        "dist_MB", "CCS_max_dist", "t_ID", "SP_number", "SP_code", "H_cm", "D_class_cm")
RG_total$SP_code[RG_total$SP_code == "RER"] <- "SER"


# ----- 1.2.5. BWI data --------------------------------------------------------------
#LIVINg TREES
colnames(BWI_C_age_SP) <- c("BWI_SP_group", "unit_BWI", 
                            "1", "21", "41", "1-60", 
                            "61", "81", "101", "61-120",
                            "121", "141"  ,"161", ">120", "all", "Bemerkung")

# LIVING trees stand characteristics
# species groups abbreviations: 
    # case_when(BWI_SP_group == "Eiche" ~ "EI", 
    #           BWI_SP_group == "Buche" ~ "BU",
    #           BWI_SP_group == "andere Lb hoher Lebensdauer" ~ "ALH",
    #           BWI_SP_group == "andere Lb niedriger Lebensdauer" ~ "ALN",
    #           BWI_SP_group == "alle LaubbÃ¤ume" ~ "LB",
    #           BWI_SP_group == "Fichte" ~ "FI",
    #           BWI_SP_group == "Tanne" ~ "TA",
    #           BWI_SP_group == "Douglasie" ~ "DGL",
    #           BWI_SP_group == "Kiefer" ~ "KI",
    #           BWI_SP_group == "LÃ¤rche" ~ "LA",
    #           BWI_SP_group == "alle NadelbÃ¤ume" ~ "NB", 
    #           TRUE ~ "all"))
colnames(BWI_stand_char_SP) <- c("stand_characteristic", "unit_BWI", "EI", "BU", "ALH", "ALN", "LB", "FI", "TA", "DGL", "KI", "LA", "NB", "all", "Bemerkung")

# DEADWOOD
# deadwood categories MoMoK
  # stehend, ganzer Baum = 2
  # Bruchstück = 3
  # liegend ganzer Baum = 5
  # liegend stark = 1
  # haufen = 6
  # Wurzelstock = 4
# the deadwood categories of MoMoK do not comply with the deadwood categories in the BWI
# thus the category 1 occures twice
      # 1 = liegend, starkes totholz (lying, strong/ large deadwood without roots)
      # 1W = liegend starkes totholz mit wurzelstock (lying, strong/ large deadwood with roots)
      # later, however, they will be summarized as group 1 
# volume/ ha by federal state and deadwood type
colnames(BWI_DW_V) <- c("state", "unit_BWI", 
                        "2",  "3", "S", 
                        "5", "1W", "1", "L", 
                        "4", "6", "all", "Bemerkung")
# V, C, etc. per hectar by deadwood type
colnames(BWI_DW_C) <- c("Zielmerkmal", "unit_BWI", 
                        "2",  "3", "S", 
                        "5", "1W", "1", "L", 
                        "4", "6", "all", "Bemerkung")
# Volume, carbon etc. per hecktar by item-mass-class
colnames(BWI_DW_iB) <- c("Zielmerkmal", "unit_BWI", "0.05", "0.1", "0.2", "0.5", "0.6", "all", "Bemerkung")


# ----- 1.2.6. MoMoK plot info --------------------------------------------------------------
# "nr1_momok" "bze"       "bl"        "moortyp"   "hydro"     "bestand"   "bemerk"    "beschrit" 
# "kat_momok" "GK4_re"    "GK4_ho" 
colnames(site_info) <- c("plot_ID", "BZE", "state", "peat_type", "hydro_status", 
                         "SP_name", 
                         "Bemerkung", "Beschrit", 
                         "kat_momok", "GK4_re", "GK4_ho") 




# ---- 1.3 functions ------------------------------------------------------
# ---- 1.3.1. circle ------------------------------------------------------
# area of a circle
c_A = function(r){
  circle_area <- r^2*pi
  return(circle_area)}

# changing unit fromkg to t
# https://stackoverflow.com/questions/73674827/is-there-a-function-in-r-to-convert-the-units-of-multiple-columns-with-a-built-i  
tons = function(kg){
  tons <- kg/1000
  return(tons)
}

# ---- 1.3.2. HEIGHTS ------------------------------------------------------
# ---- 1.3.2.1. height coefficient selection ------------------------------------------------------
# this function is used to select the coefficients of the height models depending on the R2
# for x, y,a, b (can be whatever)
f = function(x,y,a,b){
  # do the following: if x is na, or x is smaller then y, then use a, if not use b 
  answer <- ifelse(is.na(x)| x < y, a, b)
  return(answer)}


# ---- 1.3.2.2. einheitshoehenkurve------------------------------------------------------
# ---- 1.3.2.2.1. Sloboda ------------------------------------------------------
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
## aboveground biomass in kg per tree, for trees DBH > 10cm & DHB < species speficic threshold GHGI
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
  return(b0[spec]+((bs[spec] - b0[spec])/ds[spec]^2 + b3[spec]*(d-ds[spec]))*d^2)
}
## above ground biomass for trees <1.3m GHGI
Dunger_aB_Hb1.3 <- function(spec, h){  # here instead of species group i´ll link the formula to a column with he categories broadleafed and coniferous trees
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
Dunger_aB_DBHath <- function(spec, d, d03, h){
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


# ---- 1.3.3.1.2. Vondernach - oiB ----------------------------------------------
Vondr_oiB <- function(spec, d, h){  # I´ll use the secies groups assigned for the calculation of the heights
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


# ---- 1.3.3.1.3. TapeS total above biomass - all -------------------------------------------
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
tapes_swB <- function(spec_tpS, d, dh, h){         
  spp = na.omit(spec_tpS);
  Dm = na.omit(as.list(d));
  Hm = na.omit(as.list(dh));
  Ht = na.omit(h);
  obj.tbio <- tprTrees(spp, Dm, Hm, Ht, inv = 4);
  sw.df <- tprBiomass(obj.tbio, component="sw");
  return(sw.df$sw) # solid wwood without bark
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
tapes_swbB <- function(spec_tpS, d, dh, h){         
  spp = na.omit(spec_tpS);
  Dm = na.omit(as.list(d));
  Hm = na.omit(as.list(dh));
  Ht = na.omit(h);
  obj.tbio <- tprTrees(spp, Dm, Hm, Ht, inv = 4);
  sb.df <- as.data.frame(tprBiomass(obj.tbio, component="sb"));
  return(sb.df$sb) # only select solid wood bark value and don´t print the header too: sd$sb
}


# ----- 1.3.4.4. stumpwood bark  ------------------------------------
# ----- 1.3.4.4.1. Tapes stumpwood "stb" ------------------------------------
tapes_stwbB <- function(spec_tpS, d, dh, h){         
  spp = na.omit(spec_tpS);
  Dm = na.omit(as.list(d));
  Hm = na.omit(as.list(dh));
  Ht = na.omit(h);
  obj.tbio <- tprTrees(spp, Dm, Hm, Ht, inv = 4);
  stb.df <- as.data.frame(tprBiomass(obj.tbio, component="stb"));
  return(stb.df$stb)
}

# ----- 1.3.4.4. stumwood   ------------------------------------
# ----- 1.3.4.4.1. Tapes stumpwood "st" ------------------------------------
tapes_stwB <- function(spec_tpS, d, dh, h){         
  spp = na.omit(spec_tpS);
  Dm = na.omit(as.list(d));
  Hm = na.omit(as.list(dh));
  Ht = na.omit(h);
  obj.tbio <- tprTrees(spp, Dm, Hm, Ht, inv = 4);
  stw.df <- as.data.frame(tprBiomass(obj.tbio, component="stw"));
  return(stw.df$stw) # solid wwood without bark
}


# ---- 1.3.4.6. coarsewood biomass with bark ----------------------------------------------
# ---- 1.3.4.6.1. Wirth coarsewood biomass with bark-----------------------------------------
# ---- 1.3.4.6.2. Wutzler coarsewood biomass with bark---------------------------------------
# ---- 1.3.4.6.3. Vondernach coarsewood biomass with bark - Dh + DhR ------------------------------------
Vondr_crWbB <- function(spec, d, h){  # I´ll use the secies groups assigned for the calculation of the heights
  b0 <- c(fi = 0, ta = 0, dgl = 0, ki = 0, bu = -5.6602, ei = -5.9489, es=0, ah=0); 
  b1 <- c(fi = 0.0157, ta = 0.0074, dgl = 0.0128, ki=0.0169, bu=0.022, ei=0.0257, es=0.0128, ah=0.028 );
  b2 <- c(fi = 1.735, ta = 1.6476, dgl = 1.9541, ki=1.9894, bu=2.0971, ei=2.0738, es=1.9623, ah=2.1304);
  b3 <- c(fi = 1.2177, ta = 1.5543, dgl = 1.0539, ki=0.9378, bu=0.8957, ei=0.8508, es=1.1824, ah=0.7078);
  b4 <- c(fi = 0, ta = 0, dgl = 0, ki = 0, bu = 0, ei =0, es=0, ah=0.5195); 
  b5 <- c(fi = 0.0042, ta = 0.0017, dgl = 0.0027, ki=0.0044, bu=0.0017, ei=0.006, es=0.001, ah=0.004 );
  b6 <- c(fi = 1.6026, ta = 1.304, dgl = 1.8296, ki=1.9594, bu=2.0245, ei=2.0101, es=1, ah=2.068);
  b7 <- c(fi = 1.0239, ta = 1.8956, dgl = 1.0032, ki=0.6641, bu=0.9396, ei=0.778, es=1.6592, ah=0.6965);
  return((b0[spec] + b1[spec]* d^b2[spec] * h^b3[spec]) + (b4[spec] + b5[spec]* d^b6[spec] * h^b7[spec]))
}

# ---- 1.3.4.6.4. TapeS total solid wood biomass with bark - sw+sb -----------------------------------------
tapes_crsWbB <- function(spec_tpS, d, dh, h){         
  spp = na.omit(spec_tpS);
  Dm = na.omit(as.list(d));
  Hm = na.omit(as.list(dh));
  Ht = na.omit(h);
  obj.tbio <- tprTrees(spp, Dm, Hm, Ht, inv = 4);
  #component <- c("sw", "sb", "ndl")
  # tprBiomass(obj, component=component)
  sw <- tprBiomass(obj.tbio, component="sw");
  sb <-  tprBiomass(obj.tbio, component="sb");
  #stw <- tprBiomass(obj.tbio, component="stw");
  #stb <- tprBiomass(obj.tbio, component="stb");
  crsWbB.df <- cbind(sw, sb) %>% mutate(crsWbB_kg = sw+sb);
  return(crsWbB.df$crsWbB_kg)
  # coarsewood + coarswood bark + stumbwood + stumbwoodbark 
  # L> I am unsure if this is correct, because I am not sure if solid wood includes the stumbwood already --> ask Sebstian?
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

#DH3 4a Model   
Wutzler_fB_L1 <- function(d, h){  #DH3 4a Model 
  b0 = 0.0377;
  b1 = 2.43;
  b2 = (-0.913);
  return(b0*d^b1*h^b2)
}

# ---- 1.3.3.5.3. Vondernach foliage - Nad ---------------------------------------
Vondr_fB <- function(spec, d, h){                                                  # H_SP_group
  b0 <- c(fi = -2.6807, ta = 0, dgl = -2.3439, ki = 0, bu = 0, ei = 0, es=0, ah=0); # 0 for all broadleafed trees
  b1 <- c(fi = 0.4639, ta = 0.0961, dgl = 0.3042, ki=0.0967, bu=0, ei=0, es=0, ah=0 );
  b2 <- c(fi = 2.0861, ta = 1.7136, dgl = 2.4415, ki=2.3781, bu=0, ei=0, es=0, ah=0);
  b3 <- c(fi = -0.9393, ta = 0, dgl = -1.2707, ki=-1.1093, bu=0, ei=0, es=0, ah=0);
  return(b0[spec] + b1[spec]* d^b2[spec] * h^b3[spec])
}

# ---- 1.3.3.5.4. tapeS foliage - ndl-------------------------------------------
# tapeS foliage
tapes_fB <- function(spec_tpS, d, dh, h){          
  spp = na.omit(spec_tpS);
  Dm = na.omit(as.list(d));
  Hm = na.omit(as.list(dh));
  Ht = na.omit(h);
  obj.tbio <- tprTrees(spp, Dm, Hm, Ht, inv = 4);
  needles <- as_tibble(tprBiomass(obj.tbio, component="ndl"));
  return(needles$ndl)
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
  # spp = na.omit(trees_total_5 %>% dplyr::pull(tpS_ID)); 
  # Dm = na.omit(as.list(trees_total_5 %>% dplyr::pull(DBH_cm)));
  # Hm = na.omit(as.list(trees_total_5 %>% mutate(D_h_m = (ifelse(is.na(DBH_h_cm), 130, DBH_h_cm))/100) %>% dplyr::pull(D_h_m))); # height at which diameter was taken, has to be 1.3m because these are the deadwood pieces that do still have a DBH
  # Ht = na.omit(trees_total_5 %>% dplyr::pull(H_m));
  spp = na.omit(spec_tpS);
  Dm = na.omit(as.list(d));
  Hm = na.omit(as.list(dh));
  Ht = na.omit(h);
  obj.tbio <- tprTrees(spp, Dm, Hm, Ht, inv = 4);
  branches <- as_tibble(tprBiomass(obj.tbio, component="fwb")); # doesn´t accept it as a dataframe elsewise
  return(branches$fwb)
}

# ---- 1.3.3.6.3. Vondernach fine branches - Ndh -------------------------------------------
Vondr_NhdB <- function(spec, d, h){                                                  # H_SP_group
  b0 <- c(fi = 2.133, ta = 0, dgl = 0, ki = 0, bu = 0, ei = 0, es=0, ah=0); # für alle auser buche 0
  b1 <- c(fi = 0.0241, ta = 0.0187, dgl = 0.1868, ki=0.2801, bu=0.3255, ei=0.376, es=0.2633, ah=0.0259);
  b2 <- c(fi = 2.9983, ta = 2.3598, dgl = 2.9652, ki=2.4138, bu=2.2399, ei=2.0763, es=1.686, ah=2.2687);
  b3 <- c(fi = -0.8549, ta = 0, dgl = -1.4856, ki=-1.1526, bu=-0.6099, ei=-0.6378, es=0, ah=0);
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
B_DW <- function(V, dec_SP){     # dec_SP = a column that holds the degree of decay and the species type has to be created (united)
  # *1000 to transform density in g/cm3 into kg/m3
  BEF <- c("2_1" = 0.372*1000, "2_2" = 0.308*1000, "2_3" = 0.141*1000, "2_4" = 0.123*1000,   # conferous trees according to Faver
           "1_1" = 0.58*1000, "1_2" = 0.37*1000, "1_3" = 0.21*1000, "1_4" = 0.26*1000,       # broadleaved trees according to Müller-Ursing
           "3_1" = 0.58*1000, "3_2" = 0.37*1000, "3_3" = 0.21*1000, "3_4" = 0.26*1000);      # oak
  return(V*BEF[dec_SP])
}

# relative density for tapeS deadwood compartiments
# Biomasse unzersetzt * (100% - relative Veränderung der Dichte) = 
# B * (1-(D1 - D2/ D1))

rdB_DW <- function(B, dec_SP){     # a column that holds the degree of decay and the species type has to be created (united)
  rd <- c("2_1" = 1, "2_2" = (1-((0.372-0.308)/0.372)), "2_3" = (1-((0.372-0.141)/0.372)) , "2_4" = (1-((0.372-0.123)/0.372)) ,   # relative change in density of conferous trees according to Faver based on 100% = 0.372
           "1_1" = 1 , "1_2" =  (1-((0.58-0.37)/0.58)), "1_3" = (1-((0.58-0.21)/0.58)) , "1_4" = (1-((0.58-0.26)/0.58)) ,       #  relative change in density of broadleaved trees according to Müller-Ursing basen on 100% = 0.58
           "3_1" = 1, "3_2" = (1-((0.58-0.37)/0.58)), "3_3" = (1-((0.58-0.21)/0.58)), "3_4" = (1-((0.58-0.26)/0.58)) );      # relative change in density of oak trees according to Müller-Ursing basen on 100% = 0.58
  return(B*rd[dec_SP])
}


# ---- 1.3.4.3. Carbon deadwood according to IPCC default value from GHGI methodology 2006
C_DW <- function(V, dec_SP){   # a column that holds the degree of decay and the species type has to be created (united)
  BEF <- c("2_1" = 0.372*1000, "2_2" = 0.308*1000, "2_3" = 0.141*1000, "2_4" = 0.123*1000,   # conferous trees according to Faver
           "1_1" = 0.58*1000, "1_2" = 0.37*1000, "1_3" = 0.21*1000, "1_4" = 0.26*1000,       # broadleaved trees according to Müller-Ursing
           "3_1" = 0.58*1000, "3_2" = 0.37*1000, "3_3" = 0.21*1000, "3_4" = 0.26*1000);      # oak
  return(V*BEF[dec_SP]*0.5)   # defaul value for carbon content in deadwood = 0.5 according to IPCC GHG methodology 2006
}


# ----- 1.3.5. REGENERATION BIOMASS --------------------------------------------
# ----- 1.3.5.1. root collar diameter in mm ---------------------------------------
# Annighoefer:  https://link.springer.com/article/10.1007/s10342-016-0937-z#Sec2
annighoefer_RCD <- function(d, LH_NH, d_h_measured){
  t_LH <- c("5" = 1.08, "10" = 1.16, "50" = 1.33, "130" = 1.45); # transformation factor broadleafed trees, the number describes the height at which the diameter was measured
  t_NH <- c("5" = 1.06, "10" = 1.13, "50" = 1.29, "130" = 1.45); # transformation factor coniferous trees
  return(ifelse(LH_NH == "NB", t_NH[d_h_measured]*d, t_LH[d_h_measured]*d))
}

# ----- 1.3.5.2. total aboveground biomass regeneration ------------------------
# this will require a new grouping of the species groups
# the species groups created in this code are taken from the BWI column of xbart
# this however, leads to the exclusion of salix as species specific factor which is now
# treated as sonstiges laubholz and Pinus unicata which is entirely excluded. 
# The advantage would be that I can use BWI_SP_group for the "spec" input group
# Because if I list them by their latin names, not all species in our dataset will find a coefficient that matches them
# while through a rougher/ more generalized grouping we ensure all species are covered. 
# Annighoefer: # https://link.springer.com/article/10.1007/s10342-016-0937-z#Sec2
annighoefer_rg_aB_H1.3_DBHb10 <- function(RCD, h, spec){
  b1 <- c(WTA = 1.87856, BAH = 0.21103, SBI = 0.37119, HBU = 0.35633, RBU = 0.62342, ES =0.07555, GFI = 2.24952, GKI = 0.75897,  
          SPI = 0.38946, # this is for Pinus unicata
          KIR =  0.34321, STK = 0.41845, DGL = 0.42058, TEI = 0.5274, SEI = 0.67311, REI = 0.10626, ROB = 0.98644, 
          WEI = 0.04368, # SBL represents Salix spec.
          VB = 0.52384, WLI = 0.10615);
  b2 <- c(WTA = 0.79034, BAH = 0.95964, SBI = 0.87982, HBU = 0.92508, RBU = 0.87409, ES = 1.07047, GFI = 0.76318, GKI = 0.85012,  
          # KI = 0.87595, # this is for Pinus unicata
          KIR =  0.91827, STK = 0.93306,  DGL = 0.92076, TEI = 0.81213, SEI = 0.85202, REI = 1.09349, ROB = 0.77535, 
          WEI = 1.12303, # SBL represents Salix spec.
          VB = 0.76575, WLI = 1.02416);
  return((b1[spec]*((RCD^2)*h)^b2[spec])/1000) # dicided by 100o as aB is in g AGB = aboveground biomass (g)
  }

# anighoefer below 1.3 --> no diameter available
 # AGB = b1*H^b2
annighoefer_rg_aB_bH1.3 <- function(h, spec){ # spec = Annig_SP_group
  b1 <- c(WTA = 0.03118, BAH = 0.00165, SBI = 0, HBU = 0.02147, RBU = 0.00135, ES =0.00158, 
          GFI = 0.08422, GKI = 0.02022,  
           SPI = 0.00073, # this is for Pinus unicata
          KIR =  0, STK = 0.00039, DGL = 0.00457, TEI = 0.00684, SEI = 0.00936, 
          REI = 0.00099, ROB = 0.00122, 
          WEI = 0.00001, # SBL represents Salix spec.
          VB = 0.00044, WLI = 0.00061);
  b2 <- c(WTA = 1.961, BAH = 2.26095, SBI = 5.34214, HBU = 1.70301, RBU = 2.31682, 
          ES = 2.28364, GFI = 1.78966, GKI = 1.9891,  
          SPI = 2.43282, # this is for Pinus unicata
          KIR =  3.88355, STK = 2.57002,  DGL = 2.11328, TEI = 2.03158, 
          SEI = 2.05293, REI = 2.20817, ROB = 2.33479, 
          WEI = 3.1988, # SBL represents Salix spec.
          VB = 2.33729, WLI = 2.62245);
  return((b1[spec]*h^b2[spec])/1000)
}


# Roehling/ Dunger/ GHGI/ BWI
# below 1.3 --> no diameter
Dunger_aB_Hb1.3 <- function(spec, h){  # here instead of species group i´ll link the formula to a column with he categories broadleafed and coniferous trees
  b0 <- c(NB = 0.23059, LB = 0.04940);
  b1 <- c(NB = 2.20101, LB = 2.54946);
  return(b0[spec]*h^b1[spec])
}

# above 1.3m, but below 10cm DBH --> DBH availlable 
Dunger_aB_H1.3_DBHb10 <- function(spec, d){         # spec = Bio_SP_group
  b0 <- c(fi = 0.41080, ki = 0.41080, bu = 0.09644 , ei= 0.09644, shw =0.09644);
  bs <- c(fi = 26.63122 , ki = 19.99943 , bu = 33.22328, ei= 28.94782, shw =16.86101);
  b3 <- c(fi = 0.01370, ki = 0.00916, bu = 0.01162, ei= 0.01501, shw = -0.00551);
  ds <- c(fi = 10, ki = 10, bu = 10, ei= 10, shw =10);
  return(b0[spec]+(((bs[spec] - b0[spec])/ds[spec]^2)+b3[spec]*(d-ds[spec]))*d^2)
}

# ----- 1.3.5.3. compartiment biomass regeneration -----------------------------
# the compartitioning according to Poorter requires knowledge of the root mas of the trees. Which we do not have at this point (11.04.2023)
# https://nph.onlinelibrary.wiley.com/doi/10.1111/j.1469-8137.2011.03952.x

# ----- 1.3.5.3.1. stem vs. root -----------------------------------------------
  # angiosperm = broad leafed
  # gymnosperm = coniferous
  #  y=a+b1*x+b2*x^2
 # The top part of the table gives data for a stepwise regression with quadratic polynomials 
 # of the form y=a+b1*x+b2*x^2, where y is the log10-transformed leaf or stem mass and 
 # x is the log10-transformed root mass. The bottom part shows the results of a reduced 
 # major axis (linear) regression with slope αRMA and intercept logβRMA
# RSR means root to shoot (root biomass ration to stem biomass)
Poorter_rg_RSR <- function(bB, spec){ # instead of the species I have to put NH_LH here
a <- c(NH = -0.070, LH = -0.097);
b1 <- c(NH = 1.236, LH = 1.071);
b2 <- c(NH = -0.0186, LH = 0.01794);
return(a[spec]+ b1[spec]*bB+ b2[spec]*bB^2)
}

# RSR = Shoot to root ratio  = (leaf + stem dry mass) ⁄ root dry mass
#  LAR = Leaf area ratio = Leaf area ⁄ total plant dry mass
#  LMF = Leaf mass fraction =  Leaf dry mass⁄ total plant dry mass

# ----- 1.3.5.3.2. leaf vs. root -----------------------------------------------
# RLR means roots leafes ratio
Poorter_rg_RLR <- function(bB, spec){ # instead of the species I have to put NH_LH here
  a <- c(NB = 0.243, LB = 0.090);
  b1 <- c(NB = 0.924, LB = 0.889);
  b2 <- c(NB = -0.0282, LB = -0.0254);
  return(a[spec]+ b1[spec]*bB+ b2[spec]*bB^2)
}


# ----- 1.3.6. Nitrogen stock  --------------------------------------------

N_all_com <- function(B, comp, SP_com){
  n_con <- N_con_comp  %>% dplyr::pull(N_con_per, SP_com); 
  return(B*n_con[SP_com])
}


# ----- 1.3.6.1. NItrogen foliage -----------------------------------------
N_f <- function(B_compartiment, spec){
  n_con_f <- N_con_comp %>% filter(compartiment== "f") %>% dplyr::pull(N_con_per, SP_BWI) 
  return(B_compartiment*n_con_f[spec])
}
# ----- 1.3.6.2. Nitrogen fine wood ---------------------------------------
N_fw <- function(B_compartiment, spec){
  n_con_fw <- N_con_comp %>% filter(compartiment== "fw") %>% dplyr::pull(N_con_per, SP_BWI) 
  return(B_compartiment*n_con_fw[spec])
}
# ----- 1.3.6.3. Nitrogen solid wood without bark -------------------------
N_sw <- function(B_compartiment, spec){
  n_con_sw <- N_con_comp %>% filter(compartiment== "sw") %>% dplyr::pull(N_con_per, SP_BWI) 
  return(B_compartiment*n_con_sw[spec])
}
# ----- 1.3.6.4. Nitrogen solid wood bark ---------------------------------
N_swb <- function(B_compartiment, spec){
  n_con_swb <- N_con_comp %>% filter(compartiment== "swb") %>% dplyr::pull(N_con_per, SP_BWI) 
  return(B_compartiment*n_con_swb[spec])
}

# ----- 1.3.6.5. Nitrogen stump wood  ---------------------------------
N_stw <- function(B_compartiment, spec){
  n_con_stw <- N_con_comp %>% filter(compartiment== "stw") %>% dplyr::pull(N_con_per, SP_BWI) 
  return(B_compartiment*n_con_stw[spec])
}

# ----- 1.3.6.6. Nitrogen stump wood bark ---------------------------------
N_stwb <- function(B_compartiment, spec){
  n_con_stwb <- N_con_comp %>% filter(compartiment== "stwb") %>% dplyr::pull(N_con_per, SP_BWI) 
  return(B_compartiment*n_con_stwb[spec])
}

# ----- 1.3.6.7. Nitrogen belowground ---------------------------------
# source: 
# Jacobsen et al. 2003; 
# Gehalte chemischer Elemente in Baumkompartimenten Literaturstudie und Datensammlung, 
# Berichte des Forschungszentrums Waldökosysteme, Reihe B, Bd. 69, 2003
# Carsten Jacobsen, Peter Rademacher, Henning Meesenburg und Karl Josef Meiwes
# Niedersächsische Forstliche Versuchsanstalt
N_bg <- function(B_compartiment, N_bg_spec){
  n_con_bg <- c(EI = 0.00371, BU = 0.00303, FI = 0.00414, KI = 0.00177, KIN = 0.00176, BI = 0.0037, LA = 0.0028);
  return(B_compartiment*n_con_bg[N_bg_spec])
}




# ----- 1.3.6.8. assigning SD class ----------------------------------------------
SD_class <- function(sd_bwi, diff_c){
  # call it group 1 if the difference is below or equal the respective SD
  # call it group 2 if the difference is below or equal the 2 times respective SD (SD*2)
  # call it group 3 if the difference is below or equal the 3 times respective SD (SD*3)
  # call it group 4 if the difference is higher the  3 times the respective SD (SD*4)
 
  # transfer negative differnce in postiive ones to enable comparisson with SD with is +/- 
 diff_c_betrag = ifelse(diff_c <0, diff_c*(-1), diff_c);  

 sd_cl_1 = 1*sd_bwi ;
 sd_cl_2 = 2*sd_bwi ;
 sd_cl_3 = 3*sd_bwi ;
 
 sd_cl_df <- ifelse(diff_c_betrag <= sd_cl_1 , "1",
        ifelse(diff_c_betrag > sd_cl_1  & diff_c_betrag <= sd_cl_2 , "2",
               ifelse(diff_c_betrag > sd_cl_2  & diff_c_betrag <= sd_cl_3 , "3", 
                      ifelse(diff_c_betrag > sd_cl_3 , "4", "5"))));
 
return(sd_cl_df)
  }



# ----- 1.4. dealing with missing info ---------------------------------------------------
# check for variabels with NAs
summary(trees_total)
summary(DW_total) # there´s one D_cm that is NA because in the origianl dataset it´s called "s"--> I´ll exclude it
summary(RG_total)

# ----- 1.4.1 assign DBH class to trees where DBH_class == 'NA' -----------------
# create label for diameter classes according to BZE3 Bestandesaufnahmeanleitung
labs <- c(seq(5, 55, by = 5)) 

# ----- 1.4.2 assign age class to trees -----------------------------------------
# defining age classes from 1 to 160 in steps of 20
# this is a preparation fot the comparison with carbon stocks calcualted by te
labs_age <- c(seq(1, 180, by = 20))

# ----- 1.4.2. tree species -----------------------------------------
# Goal 1: assiging the correct latin name to the individual trees through SP_names dataset
    # when trying to assign the correct latinn manes to the respective trees via their species code or species number 
    # it became evident that neither the areviations of the common species names, nor the species numbers correspond
    # further the species numbers are wronlgy assigned in the trees dataset
    # the most coherent or common variables appears to be the species common names abbreviations in the SP_names and trees_total dataset
    # though the character codes in the SP_names dataset are assessed in a mix of capital and not capital letters, 
    # while all abbreviations of common species names in the trees_total dataset are all in capital letters
    # thus, I´ll transform all the abbreviations of common species names in the SP_names dataset into capital letters, 
    # to enable the join. Chr_ger_cap
# Goal 2: assingin the correct species codes from the TapeS SP file to trees_total to use TapeS later
    # the most correspondent variable/ column between TapeS and SP_names, and by that trees_total, which can access & join all SP_names columns 
    # but no or few tapeS_SP columns is the "BWI" column of SP_names and the "kurz" column of TapeS_SP when transformed into capital letters. 



# creating a dataset with the species names from x-bart and the accordint TapeS codes 
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
  by = c("tpS_SP_com_name" = "scientific")) %>% 
  # available groups/ species : BU, EI, ES, AH, BI, ERL, FI, KI, DGL
  mutate( H_SP_group = case_when(bot_genus == "Quercus"~ 'ei', 
                                 LH_NH == "LB" & bot_genus != "Quercus" ~ 'bu', 
                                 bot_genus == "Abies" ~ 'ta', 
                                 bot_genus == "Pinus" ~ 'ki', 
                                 bot_genus == "Pseudotsuga" ~ 'dgl',
                                 LH_NH == "NB" & bot_genus == "Larix" ~ 'lae', 
                                 TRUE ~ 'fi'),
  # BWI species groups according to Methodikband zur 3. Waldinventur 2012: ei, bu, aLh, aLn, ki, fi
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
          # Biomass species groups to assgn the correct coefficients for the biomass functions of GHGI
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
                                   TRUE ~ 'other'), 
          N_SP_group = case_when(LH_NH == "LB" & bot_genus == "Quercus"~ 'EI', 
                          # LH_NH == "LB" & bot_genus == "Fagus"~ 'BU',
                           LH_NH == "LB" & bot_genus == "Acer"~ 'AH',
                           LH_NH == "LB" & bot_genus == "Fraxinus"~ 'ES',
                           LH_NH == "LB" & bot_genus == "Betula"~ 'BI',
                           LH_NH == "LB" & bot_genus == "Alnus"~ 'ERL',
                           LH_NH == "LB" & bot_genus %in% c("Fagus",
                                                            "Platanus", 
                                                            "Tilia", 
                                                            "Juglans", 
                                                            "Corylus", 
                                                            "Robinia", 
                                                            "Castanea", 
                                                            "Carpinus", 
                                                            "Aesculus", 
                                                            "Sorbus",
                                                            "Ulmus", 
                                                            "Rhamnus") | LH_NH == "LB" & bot_name == "Prunus dulcis" ~ 'BU', # species that are ususally "anderes Laubholz Hoher lebenserwartung are allocated to BU
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
                                                              "Rhamnus", 
                                                               "Betula", 
                                                               "Alnus")) | LH_NH == "LB" & bot_name != "Prunus dulcis" ~ 'BI',  # species that are usually "anderes laubholz niedriger Lebenserwartung are allovated to BI
                           LH_NH == "NB" & bot_genus %in% c("Pinus", "Larix") ~ 'KI', 
                           LH_NH == "NB" & bot_genus %in% c("Pseudotzuga") ~ 'DGL',
                           LH_NH == "NB" & !(bot_genus %in% c("Pinus", "Larix", "Pseudotzuga"))  ~ 'FI', # all species not Pinus, Larix or DOuglas fir are treated as Spruce 
                           TRUE ~ 'other'), 
  N_bg_SP_group = case_when(LH_NH == "LB" & bot_genus == "Quercus"~ 'EI', 
                           LH_NH == "LB" & bot_genus == "Fagus"~ 'BU',
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
                                                            "Rhamnus") | LH_NH == "LB" & bot_name == "Prunus dulcis" ~ 'BU',
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
                                                              "Rhamnus")) | LH_NH == "LB" & bot_name != "Prunus dulcis" ~ 'BI',
                           LH_NH == "NB" & bot_genus == "Pinus" & bot_name != "Pinus nigra" ~ 'KI', 
                           LH_NH == "NB" & bot_genus == "Pinus" & bot_name == "Pinus nigra" ~ 'KIN',
                           LH_NH == "NB" & bot_genus == "Larix" ~ 'LA',
                           LH_NH == "NB" & !(bot_genus %in% c("Pinus", "Larix"))  ~ 'FI', 
                           TRUE ~ 'other'))
# export x_bart with TapeS common ID: https://stackoverflow.com/questions/53089219/specify-path-in-write-csv-function
write.csv(SP_names_com_ID_tapeS, "output/out_data/x_bart_tapeS.csv")


# ----- 1.4.2.1. LIVING TREES: adding speices info -----------------------------------------
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
                          SP_names_com_ID_tapeS %>% 
                          # creating capital lettered column with german abbreviations to join species names from SP_names into trees_total
                           # https://www.datasciencemadesimple.com/convert-to-upper-case-in-r-dataframe-column-2/
                           mutate(Chr_ger_cap = toupper(Chr_code_ger), 
                                  # create column in SP_names that corresponds with TapeS species
                                  # --> the changes are carried out according to the anti join between trees_total & TapeS species, not
                                  # the join between SP_codes from BZE and TapeSP, this has to be done later
                                  #tpS_com_ID = case_when(BWI == "KI" ~ 'KIE',
                                   #                      BWI == "ERL" ~ 'ER',
                                    #                    TRUE ~ BWI), 
                                  # create species groups, BWI uses for their volume calculations to use curtis & sloboda functions
                                  # BWI Methodikband: 
                                  # für die Hoehenmessung wurde nach folgenden Baumartengruppen differenziert:
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
    # 3. joing TapeS species codes via common SP_ID created above (SP_names_com_ID_tapeS)
                           dplyr::select(Chr_ger_cap, Chr_code_ger, bot_name, H_SP_group,BWI_SP_group, LH_NH, BWI, Bio_SP_group, N_SP_group, N_bg_SP_group, tpS_SP_com_name, tpS_ID), 
                         by = c("SP_code" = "Chr_ger_cap"))
  




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
anti_join(trees_total %>% left_join(., 
                                    SP_names %>% 
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
                                      select(Chr_ger_cap), 
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





# ----- 1.4.2.2 REGENERATION: species names & groups, missing data ----------------------
# some species were wrongly assigned or assessed and have to be renamed: 
  # EB, EBS, SHO, SHA, FLB, KD, MKI
  # EB & EBS == Eberesche == Vogelbeere == Vbe
  # SHO == Schwarzer Holunder == Sonstiges Laubholz == sLb
  # SHA == Strauch Hasel == Haselnuss Strauch HaN
  # FLB == Faulbaum == Rhamnus frangere is alocated to Rahmnus alaternus SWD
  # KD == 
  # MKI == BKI 

RG_total <- RG_total %>%
  # transforming german species codes into capital characters to harmonise them with SP_names datase
  mutate(SP_code = toupper(SP_code)) %>% 
  # adding & chaging missing/ wrongly assessed species names
  mutate(SP_code = case_when(SP_code == "EB" | SP_code == "EBS" ~ "VBE", 
                              SP_code == "SHO" ~ "SLB", 
                              SP_code == "SHA" ~ "HAN", 
                              SP_code == "FLB" ~ "SWD", 
                              SP_code == "KD" ~ "SLB", 
                              SP_code == "RER" ~ "SER", 
                              SP_code == "MKI" ~ "BKI", 
                              TRUE ~ SP_code)) %>% 
  # 1. joining in general species dataset with botanical names, TapeS species groups, etc. 
  left_join(., SP_names_com_ID_tapeS %>% 
              mutate(Chr_ger_cap = toupper(Chr_code_ger), 
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
                                              TRUE ~ 'other')) %>% 
              dplyr::select(Chr_code_ger, Chr_ger_cap, bot_name, bot_genus, bot_species, LH_NH, BWI, BWI_SP_group, Bio_SP_group, N_SP_group, N_bg_SP_group, tpS_ID), 
            by = c("SP_code" = "Chr_ger_cap"))  %>%
  # creating Species groups to assign the corect coefficients of the Annighöfer biomass functions based on BWI species groups
  mutate(Annig_SP_group = case_when(bot_genus == "Abies" ~  'WTA', 
                                    bot_genus == "Acer"  ~  'BAH', 
                                    bot_genus == "Betula" ~ 'SBI', 
                                    bot_genus == "Carpinus" ~ 'HBU', 
                                    bot_genus == "Fagus" ~ 'RBU', 
                                    bot_genus == "Fraxinus" ~ 'ES', 
                                    bot_genus == "Picea" ~  'GFI', 
                                    bot_genus == "Pinus" & bot_species == "unicata" ~ 'SPI', 
                                    bot_genus == "Pinus" & bot_species != "unicata" ~ 'GKI', 
                                    bot_genus == "Prunus" & bot_species == "serotina"  ~  'STK', 
                                    bot_genus == "Prunus" & bot_species  != "serotina"  ~  'KIR', 
                                    bot_genus == "Pseudotsuga" ~ "DGL",
                                    bot_genus == "Quercus" & !(bot_species %in% c("robur", "rubra"))  ~  'TEI', 
                                    bot_genus == "Quercus" & bot_species == "robur" ~ 'SEI', 
                                    bot_genus == "Quercus" & bot_species == "rubra" ~ 'REI',
                                    bot_genus == "Robinia" ~ 'ROB', 
                                    bot_genus == "Salix" | 
                                      LH_NH == "LB" & BWI_SP_group == "aLn" & !(bot_genus %in% c("Acer",
                                                                                                 "Betula", 
                                                                                                 "Carpinus", 
                                                                                                 "Fagus", 
                                                                                                 "Fraxinus", 
                                                                                                 "Prunus", 
                                                                                                 "Quercus", 
                                                                                                 "Robinia", 
                                                                                                 "Sorbus", 
                                                                                                 "Tilia")) ~ 'WEI',  # all species allocated to soft hardwods / other broadleafed trees of short life span are treated as willow
                                    bot_genus == "Sorbus" ~ "VBE", 
                                    bot_genus == "Tilia" ~ "WLI", 
                                    LH_NH == "LB" & BWI_SP_group != "SLB" & !(bot_genus %in% c("Acer", 
                                                                                               "Betula",
                                                                                               "Carpinus", 
                                                                                               "Fagus", 
                                                                                               "Fraxinus",
                                                                                               "Prunus", 
                                                                                               "Quercus", 
                                                                                               "Robinia", 
                                                                                               "Salix", 
                                                                                               "Sorbus", 
                                                                                               "Tilia")) ~ 'RBU',  # all not allocated broadleafed species that are not soft hardwoods are treatet as beech
                                    LH_NH == "NB" & !(bot_genus %in% c("Abies", 
                                                                       "Picea", 
                                                                       "Pinus", 
                                                                       "Pseudotzuga")) ~ 'FI', 
                                    TRUE ~ 'NA'))

RG_total %>% filter(is.na(tpS_ID)) %>% select(SP_code) %>% distinct()
summary(SP_names_com_ID_tapeS)
summary(RG_total)



# ----- 1.4.3. BWI comparisson datasets ---------------------------------------------------------
# ----- 1.4.3.1. carbon living trees by age class and species group BWI comparisson datasets ---------------------------------------------------------
# CARBON LIVING TREES
# carbon stock in t per ha separeted by age and species groups
BWI_C_age_SP <- na.omit(BWI_C_age_SP) %>%
  # changing unit from kg to tons
  mutate(across(c("1", "21", "41", "1-60", "61", "81", "101", "61-120", "121", "141"  ,"161", ">120", "all"), ~ tons(.x))) %>% 
  # deselecting summaries
  select(-c( "1-60", "61-120", ">120", "Bemerkung")) %>% 
  # pivoting all ages into one column 
  pivot_longer(c("1", "21", "41", "61", "81", "101", "121", "141"  ,"161",  "all"), names_to = "age_class", values_to = "C_t_BWI") %>% 
  # pivoting SD next to carbon values
  pivot_wider(names_from = unit_BWI, values_from = C_t_BWI, values_fill = 0) %>% 
  # rename(new_column_name = old_column_name) https://sparkbyexamples.com/r-programming/dplyr-rename-column/
  rename("C_t_ha_BWI" = "[kg/ha]") %>% 
  rename("SD_C_BWI" = "SE95 Â±") %>% 
  # introducing BWI species abbreviations 
  mutate(BWI_SP_group = case_when(BWI_SP_group == "Eiche" ~ "EI", 
                                  BWI_SP_group == "Buche" ~ "BU",
                                  BWI_SP_group == "andere Lb hoher Lebensdauer" ~ "ALH",
                                  BWI_SP_group == "andere Lb niedriger Lebensdauer" ~ "ALN",
                                  BWI_SP_group == "alle LaubbÃ¤ume" ~ "LB",
                                  BWI_SP_group == "Fichte" ~ "FI",
                                  BWI_SP_group == "Tanne" ~ "TA",
                                  BWI_SP_group == "Douglasie" ~ "DGL",
                                  BWI_SP_group == "Kiefer" ~ "KI",
                                  BWI_SP_group == "LÃ¤rche" ~ "LA",
                                  BWI_SP_group == "alle NadelbÃ¤ume" ~ "NB", 
                                  TRUE ~ "all"))

# ----- 1.4.3.2. carbon and stand characteristics like number of stems for living trees by species group BWI comparisson datasets ---------------------------------------------------------
# STAND CHARACTERISTICS LIVING TREES
#zielmerkmale_SP_2017
BWI_stand_char_SP <- na.omit(BWI_stand_char_SP) %>%
    dplyr::select(-c("Bemerkung")) %>% 
    filter(stand_characteristic %in% c(#"BWI_SP_group", 
                                       "Grundfläche [m²/ha]", "Stammzahl [1/ha]",  
                                        "Biomasse [kg/ha]", "oberirdische Biomasse [kg/ha]", "unterirdische Biomasse [kg/ha]", 
                                        "Kohlenstoffmasse [kg/ha]", "oberirdische Kohlenstoffmasse [kg/ha]", "unterirdische Kohlenstoffmasse [kg/ha]", 
                                        "Vorrat [m³/ha]", "Waldfläche (gemäß Standflächenanteil) [ha]", "Zugehörige Holzbodenfläche des Auswertungsgebietes [ha]") & unit_BWI == "SE95 ±" |
             stand_characteristic %in% c(#"BWI_SP_group", 
                                         "Grundfläche [m²/ha]", "Stammzahl [1/ha]",  
                                         "Biomasse [kg/ha]", "oberirdische Biomasse [kg/ha]", "unterirdische Biomasse [kg/ha]", 
                                         "Kohlenstoffmasse [kg/ha]", "oberirdische Kohlenstoffmasse [kg/ha]", "unterirdische Kohlenstoffmasse [kg/ha]", 
                                         "Vorrat [m³/ha]", "Waldfläche (gemäß Standflächenanteil) [ha]", "Zugehörige Holzbodenfläche des Auswertungsgebietes [ha]") &  endsWith(unit_BWI, "ha]"))%>% 
  #filter(unit_BWI == "SE95 ±" | endsWith(unit_BWI, "ha]")) %>% 
  pivot_longer(c(EI:all), names_to = "BWI_SP_group", values_to = "values") %>% # 264
  # add sufix to stand_characteristics for pivoting wider:  https://stackoverflow.com/questions/36069257/adding-a-suffix-to-all-values-of-a-column-in-r
    mutate(stand_characteristic = ifelse(unit_BWI == "SE95 ±", paste0(stand_characteristic, "_SD"), stand_characteristic)) %>% 
    dplyr::select(- "unit_BWI") %>%                                          # have to deselect it otherwise the pivot wider wont work
  pivot_wider(names_from = stand_characteristic, values_from = values) %>% 
  #dplyr::select(BWI_SP_group, ends_with("ha]"), ends_with("ha]_SD")) %>% 
  # this is just to reorder to then pivot all compartiments into one column
  select("BWI_SP_group", "Grundfläche [m²/ha]", "Stammzahl [1/ha]",  
         "Biomasse [kg/ha]", "oberirdische Biomasse [kg/ha]", "unterirdische Biomasse [kg/ha]", 
         "Kohlenstoffmasse [kg/ha]", "oberirdische Kohlenstoffmasse [kg/ha]", "unterirdische Kohlenstoffmasse [kg/ha]", #9
         "Vorrat [m³/ha]",  
         "Waldfläche (gemäß Standflächenanteil) [ha]", "Zugehörige Holzbodenfläche des Auswertungsgebietes [ha]",
         # standart deviation columns
         "Grundfläche [m²/ha]_SD", "Stammzahl [1/ha]_SD",  #14
         "Biomasse [kg/ha]_SD", "oberirdische Biomasse [kg/ha]_SD", "unterirdische Biomasse [kg/ha]_SD", #17
         "Kohlenstoffmasse [kg/ha]_SD", "oberirdische Kohlenstoffmasse [kg/ha]_SD", "unterirdische Kohlenstoffmasse [kg/ha]_SD", 
         "Vorrat [m³/ha]_SD",  
         "Waldfläche (gemäß Standflächenanteil) [ha]_SD", "Zugehörige Holzbodenfläche des Auswertungsgebietes [ha]_SD") %>%  # ,
# pivoting B, C: https://stackoverflow.com/questions/70700654/pivot-longer-with-names-pattern-and-pairs-of-columns
to_long(keys = c("B_compartiment",  "C_compartiment", "B_SD_compartiment", "C_SD_compartiment"), 
        values = c( "B_kg", "C_kg", "SD_B", "SD_C"),  names(.)[4:6], names(.)[7:9], names(.)[15:17], names(.)[18:20]) %>% 
  # hanging the compartiments names and deselct the other compartiment columns: https://stackoverflow.com/questions/61425318/using-mutate-and-starts-with
  mutate(B_compartiment = case_when(startsWith(B_compartiment, "unterirdische") ~ "bg", 
                                    startsWith(B_compartiment, "oberirdische") ~ "ag",
                                    TRUE ~ "total")) %>% 
  rename("compartiment" = "B_compartiment") %>%
  rename("BA_m2_ha" = "Grundfläche [m²/ha]") %>% 
  rename("Nt_ha" = "Stammzahl [1/ha]") %>% 
  rename("SP_A_ha" = "Waldfläche (gemäß Standflächenanteil) [ha]") %>% 
  rename("total_A_ha" = "Zugehörige Holzbodenfläche des Auswertungsgebietes [ha]") %>% 
    rename("SD_BA" = "Grundfläche [m²/ha]_SD") %>% 
    rename("SD_Nt" = "Stammzahl [1/ha]_SD") %>% 
    rename("SD_SP_A" = "Waldfläche (gemäß Standflächenanteil) [ha]_SD") %>% 
    rename("SD_total_A" = "Zugehörige Holzbodenfläche des Auswertungsgebietes [ha]_SD") %>% 
  select(- c("C_compartiment", "B_SD_compartiment", "C_SD_compartiment")) %>% 
  # changing unit from kg to tons
  mutate(across(c("B_kg", "C_kg", "SD_B", "SD_C"), ~ tons(.x))) %>% 
  rename("B_t_ha_BWI" = "B_kg") %>% 
  rename("C_t_ha_BWI" = "C_kg")


# ----- 1.4.3.3. volume dead trees byfederal state BWI comparisson datasets ---------------------------------------------------------
# VOLUME DEADWOOD
BWI_DW_V <- BWI_DW_V %>% 
  select(-c("Bemerkung")) %>% 
   pivot_longer(c("2",  "3", "S", 
                  "5", "1W", "1", "L", 
                  "4", "6","all"), 
                names_to = "DW_type", 
                values_to = "V_m3_ha_BWI", 
                names_repair = "unique") %>% 
  mutate(state_abbreviation = case_when(state == "Baden-WÃ¼rttemberg" ~ "BW", 
                           state == "Bayern" ~ "BY", 
                           state == "Brandenburg + Berlin" ~"BB", 
                           state == "Hessen" ~ "HE", 
                           state == "Mecklenburg-Vorpommern" ~ "MV", 
                           state == "Niedersachsen" ~ "NI", 
                           state == "Nordrhein-Westfalen" ~ "NW", 
                           state == "Rheinland-Pfalz" ~ "RP", 
                           state == "Saarland" ~ "SL", 
                           state == "Sachsen" ~ "SN", 
                           state == "Sachsen-Anhalt" ~ "ST", 
                           state == "Schleswig-Holstein" ~ "SH", 
                           state == "ThÃ¼ringen" ~ "TH", 
                           state == "Hamburg + Bremen" ~ "HB", 
                           state == "Deutschland (alle LÃ¤nder)" ~ "all", 
                           TRUE ~ "NA")) 

# ----- 1.4.3.4. carbon dead trees by deadwood type BWI comparisson datasets ---------------------------------------------------------
# CARBON DEADWOOD
BWI_DW_C <- BWI_DW_C %>% 
    filter(unit_BWI == "SE95 Â±"| endsWith(unit_BWI, "ha]")) %>% 
    filter(Zielmerkmal == "Totholzvorrat [mÂ³/ha]" |
             Zielmerkmal == "Totholzmasse [t/ha]" |
             Zielmerkmal == "Totholz-Kohlenstoff [t/ha]") %>% 
    select(-c("Bemerkung")) %>% 
    select("2",  "3", "S", "5", "1W", "1", "L", "4", "6","all",
         "Zielmerkmal", "unit_BWI") %>%   
    mutate(Zielmerkmal = case_when( Zielmerkmal == "Totholzvorrat [mÂ³/ha]" & unit_BWI != "SE95 Â±" ~ "V_m3_ha",
                                    Zielmerkmal == "Totholzmasse [t/ha]" & unit_BWI != "SE95 Â±" ~ "B_t_ha",
                                    Zielmerkmal == "Totholz-Kohlenstoff [t/ha]" & unit_BWI != "SE95 Â±"~ "C_t_ha",
                                    Zielmerkmal == "Totholzvorrat [mÂ³/ha]" & unit_BWI == "SE95 Â±"  ~ "SD_V",
                                    Zielmerkmal == "Totholzmasse [t/ha]" & unit_BWI == "SE95 Â±" ~ "SD_B", 
                                    Zielmerkmal == "Totholz-Kohlenstoff [t/ha]" & unit_BWI == "SE95 Â±" ~ "SD_C",
                                   TRUE ~ NA)) %>%  
    select(- "unit_BWI") %>% 
    pivot_longer(c("2",  "3", "S", 
                 "5", "1W", "1", "L", 
                 "4", "6","all"), 
               names_to = "DW_type", 
               values_to = "values", 
               names_repair = "unique") %>% 
    distinct() %>% 
    pivot_wider(values_from = values, 
                names_from = Zielmerkmal)
  
  
# ----- 1.4.3.4. carbon dead trees by item mass class BWI comparisson datasets ---------------------------------------------------------
rbind(
  # dataset with picoted IB classes and ziemlermalnen
  BWI_DW_iB %>% 
    filter(unit_BWI == "SE95 Â±"| endsWith(unit_BWI, "ha]")) %>% # select only hectar values and standart deviation
    filter(Zielmerkmal == "Totholzvorrat [mÂ³/ha]" |
             Zielmerkmal == "Totholzmasse [t/ha]" |
             Zielmerkmal == "Totholz-Kohlenstoff [t/ha]") %>% 
    select(-c("Bemerkung")) %>%   
    mutate(Zielmerkmal = case_when( Zielmerkmal == "Totholzvorrat [mÂ³/ha]" & unit_BWI != "SE95 Â±" ~ "BWI_V_m3_ha",
                                    Zielmerkmal == "Totholzmasse [t/ha]" & unit_BWI != "SE95 Â±" ~ "BWI_B_t_ha",
                                    Zielmerkmal == "Totholz-Kohlenstoff [t/ha]" & unit_BWI != "SE95 Â±"~ "BWI_C_t_ha",
                                    Zielmerkmal == "Totholzvorrat [mÂ³/ha]" & unit_BWI == "SE95 Â±"  ~ "SD_V",
                                    Zielmerkmal == "Totholzmasse [t/ha]" & unit_BWI == "SE95 Â±" ~ "SD_B", 
                                    Zielmerkmal == "Totholz-Kohlenstoff [t/ha]" & unit_BWI == "SE95 Â±" ~ "SD_C",
                                    TRUE ~ NA)) %>%
    select(- "unit_BWI") %>% 
    pivot_longer(c( "0.05", "0.1", "0.2", "0.5", "0.6"), 
                 names_to = "iB_class", 
                 values_to = "values", 
                 names_repair = "unique") %>% 
    select(-c("all")),
  # dataset with picoted iB classes, ziemlerkmalen and shares
  BWI_DW_iB %>% 
  filter(unit_BWI == "SE95 Â±"| endsWith(unit_BWI, "ha]")) %>% # select only hectar values and standart deviation
  filter(Zielmerkmal == "Totholzvorrat [mÂ³/ha]" |
           Zielmerkmal == "Totholzmasse [t/ha]" |
           Zielmerkmal == "Totholz-Kohlenstoff [t/ha]") %>% 
  select(-c("Bemerkung")) %>%   
  mutate(Zielmerkmal = case_when( Zielmerkmal == "Totholzvorrat [mÂ³/ha]" & unit_BWI != "SE95 Â±" ~ "BWI_V_m3_ha",
                                  Zielmerkmal == "Totholzmasse [t/ha]" & unit_BWI != "SE95 Â±" ~ "BWI_B_t_ha",
                                  Zielmerkmal == "Totholz-Kohlenstoff [t/ha]" & unit_BWI != "SE95 Â±"~ "BWI_C_t_ha",
                                  Zielmerkmal == "Totholzvorrat [mÂ³/ha]" & unit_BWI == "SE95 Â±"  ~ "SD_V",
                                  Zielmerkmal == "Totholzmasse [t/ha]" & unit_BWI == "SE95 Â±" ~ "SD_B", 
                                  Zielmerkmal == "Totholz-Kohlenstoff [t/ha]" & unit_BWI == "SE95 Â±" ~ "SD_C",
                                  TRUE ~ NA)) %>%
  select(- "unit_BWI") %>% 
  pivot_longer(c( "0.05", "0.1", "0.2", "0.5", "0.6"), 
               names_to = "iB_class", 
               values_to = "values", 
               names_repair = "unique") %>% 
  mutate(share = values/all) %>%
  select(-c("all", "values")),
  
  
  # datasets with DW_type == "all" data pivot longer
 BWI_DW_iB %>% 
  filter(unit_BWI == "SE95 Â±"| endsWith(unit_BWI, "ha]")) %>% # select only hectar values and standart deviation
  filter(Zielmerkmal == "Totholzvorrat [mÂ³/ha]" |
           Zielmerkmal == "Totholzmasse [t/ha]" |
           Zielmerkmal == "Totholz-Kohlenstoff [t/ha]") %>% 
  select(-c("Bemerkung")) %>%   
  mutate(Zielmerkmal = case_when( Zielmerkmal == "Totholzvorrat [mÂ³/ha]" & unit_BWI != "SE95 Â±" ~ "BWI_V_m3_ha",
                                  Zielmerkmal == "Totholzmasse [t/ha]" & unit_BWI != "SE95 Â±" ~ "BWI_B_t_ha",
                                  Zielmerkmal == "Totholz-Kohlenstoff [t/ha]" & unit_BWI != "SE95 Â±"~ "BWI_C_t_ha",
                                  Zielmerkmal == "Totholzvorrat [mÂ³/ha]" & unit_BWI == "SE95 Â±"  ~ "SD_V",
                                  Zielmerkmal == "Totholzmasse [t/ha]" & unit_BWI == "SE95 Â±" ~ "SD_B", 
                                  Zielmerkmal == "Totholz-Kohlenstoff [t/ha]" & unit_BWI == "SE95 Â±" ~ "SD_C",
                                  TRUE ~ NA)) %>%
  select(- "unit_BWI") %>% 
  pivot_longer(c( "0.05", "0.1", "0.2", "0.5", "0.6"), 
               names_to = "iB_class", 
               values_to = "values", 
               names_repair = "unique") %>% 
  mutate(share = values/all) %>% 
  pivot_longer(c( "all"), 
               names_to = "iB_class_all", 
               values_to = "values_all", 
               names_repair = "unique") %>% 
  select(c("Zielmerkmal", "iB_class_all", "values_all")) %>% 
  # crating a column with share 100% and renaming "_all" columns to rbind correctly
    mutate(share = 1) %>% 
  rename("iB_class" = "iB_class_all") %>% 
  rename("values" = "values_all")) %>%
  distinct() %>% 
  arrange(Zielmerkmal) %>% 
  pivot_wider(values_from = values, 
              names_from = Zielmerkmal)
  
 
 
 
# pivot the iB classes
# calcualte the V share of each iB class in terms of total volume in all classes per hektar


# Totholzvorrat [mÂ³/ha]
# Totholzmasse [t/ha]
#  Totholz-Kohlenstoff [t/ha]


# ----- 1.4.4. Nitrogen content dataset ----------------------------------------
# Reference: 
  # Rumpf, Sabine & Schoenfelder, Egbert & Ahrends, Bernd. (2018). Biometrische Schätzmodelle für Nährelementgehalte in Baumkompartimenten.
  # https://www.researchgate.net/publication/329912524_Biometrische_Schatzmodelle_fur_Nahrelementgehalte_in_Baumkompartimenten
# EI, BU, FI, KI, BI, DGL, LÄ, TA 

N_con_comp <- as.data.frame(cbind(sp <- c("BU", "BU", "BU", "BU", "BU", 
                                          "EI", "EI", "EI", "EI", "EI", 
                                          "ES", "ES","ES", "ES", "ES", 
                                          "AH", "AH","AH", "AH", "AH", 
                                          "BI", "BI","BI", "BI", "BI", 
                                          "ERL", "ERL","ERL", "ERL", "ERL",
                                          "FI", "FI","FI", "FI", "FI", "FI", 
                                          "KI",  "KI","KI",  "KI",  "KI",  "KI", 
                                          "DGL", "DGL", "DGL", "DGL", "DGL", "DGL"),
                    compartiment <- c("stw", "stwb","sw", "swb", "fw",               # beech
                                      "stw", "stwb", "sw", "swb", "fw",               # oak
                                      "stw", "stwb", "sw", "swb", "fw",               # ash
                                      "stw", "stwb", "sw", "swb", "fw",               # maple
                                      "stw", "stwb", "sw", "swb", "fw",               # birch 
                                      "stw", "stwb", "sw", "swb", "fw",               # alder
                                      "stw", "stwb", "sw", "swb", "fw", "f",          # spruce
                                      "stw", "stwb", "sw", "swb", "fw", "f",          # pine
                                      "stw", "stwb", "sw", "swb", "fw", "f"),         # douglas fir
                    N_mean_gkg <- as.numeric(c(1.335, 7.227, 1.335, 7.227, 4.601,
                                               1.752, 6.507, 1.752, 6.507, 6.209, 
                                               1.438, 5.348, 1.438, 5.348, 3.721, 
                                               1.465, 7.729, 1.465, 7.729, 4.278, 
                                               1.828, 6.131, 1.828, 6.131, 6.057, 
                                               2.475, 11.028, 2.475, 11.028, 7.214, 
                                               0.812, 4.84, 0.812, 4.84, 4.343, 12.978, 
                                               0.794, 4.339, 0.794, 4.339, 4.058, 15.201, 
                                               0.701, 3.910, 0.701, 3.910, 4.203, 15.166)), 
                    data_source <- c("Rumpf et al. 2018", "Rumpf et al. 2018", "Rumpf et al. 2018", "Rumpf et al. 2018", "Rumpf et al. 2018", 
                                     "Rumpf et al. 2018", "Rumpf et al. 2018", "Rumpf et al. 2018", "Rumpf et al. 2018", "Rumpf et al. 2018",
                                     "Rumpf et al. 2018", "Rumpf et al. 2018", "Rumpf et al. 2018", "Rumpf et al. 2018", "Rumpf et al. 2018", 
                                     "Rumpf et al. 2018", "Rumpf et al. 2018", "Rumpf et al. 2018", "Rumpf et al. 2018", "Rumpf et al. 2018", 
                                     "Rumpf et al. 2018", "Rumpf et al. 2018", "Rumpf et al. 2018", "Rumpf et al. 2018", "Rumpf et al. 2018", 
                                     "Rumpf et al. 2018", "Rumpf et al. 2018", "Rumpf et al. 2018", "Rumpf et al. 2018", "Rumpf et al. 2018", 
                                     "Rumpf et al. 2018", "Rumpf et al. 2018", "Rumpf et al. 2018", "Rumpf et al. 2018", "Rumpf et al. 2018", "Rumpf et al. 2018", 
                                     "Rumpf et al. 2018", "Rumpf et al. 2018", "Rumpf et al. 2018", "Rumpf et al. 2018", "Rumpf et al. 2018", "Rumpf et al. 2018", 
                                     "Rumpf et al. 2018", "Rumpf et al. 2018", "Rumpf et al. 2018", "Rumpf et al. 2018", "Rumpf et al. 2018", "Rumpf et al. 2018")))
colnames(N_con_comp) <- c("SP_BWI", "compartiment", "N_mean_gkg", "reference")
N_con_comp <- N_con_comp %>% 
  mutate(N_con_per = as.numeric(N_mean_gkg)/1000) %>% 
  unite(SP_com, SP_BWI:compartiment, remove = FALSE)


# ----- 1.4.5. MoMoK site info dataset data wrangling ----------------------------------------
# assign expected growth bahaviour according to species and hydrological status
   # if its a peatland adapted species like Betula pubescens or Alnus glutionosa
       # -> we expect normal growth ("n" = 1) even on regenerated sites (naturnah)
   # if its a not peatland adapted species like Picea abies, Abies alba, Pinus mungo
       # -> we expect hampered ("h" = 0) growth on regenerated sites and normal growth on not regenated sites
# thus we have to assign a column with expected growth behavior depending on the species and hydrological status
# whereby every hydr_status that does not start with "naturnah" is categorized as not regenerated

site_info <- site_info %>% 
  select(plot_ID, peat_type, hydro_status, SP_name) %>% 
  # creating a column with the dominant species per plot 
      # a) therefore i need the BA of ever species per plot 
      # b) and set it in relation to the total BA per plot
      # c) and the select the species per plot which contributes most to the total BA
  left_join(., left_join(
    dom_SP_plot <- left_join(
    # a) data set with BA per species
      trees_total %>%
        group_by(plot_ID, SP_code) %>%       # group by plot and species to calculate BA per species 
        summarise(SP_BA_plot = sum(BA_m2),             # calculate BA per species per canopy layer per plot in m2
                  plot_A_ha = mean(plot_A_ha)) %>%     # plot area in hectare to calculate BA per ha
        mutate(SP_BA_m2ha = SP_BA_plot/plot_A_ha), # calculate BA per species per plot in m2/ ha
    # b) dataset with total BA per plot
      trees_total %>%
        group_by(plot_ID) %>%                         # group by plot to calculate total BA per plot
        summarise(tot_BA_plot = sum(BA_m2),           # calculate total BA per plot in m2 by summarizing the BA of individual trees after grouping the dataset by plot
                  plot_A_ha = mean(plot_A_ha)) %>%    # plot area in hectare to calculate BA per ha
        mutate(tot_BA_m2ha = tot_BA_plot/plot_A_ha), # calculate total BA per plot in m2 per hectare by dividing total BA m2/plot by area plot/ha 
      by=c("plot_ID", "plot_A_ha")) %>% 
        select(- c(plot_A_ha, tot_BA_plot)) %>%  # remove unnecessary variables
        mutate(BA_SP_per = (SP_BA_m2ha/tot_BA_m2ha)*100),   # calculate proportion of each species to total BA in percent, 
    # c) code selecting dominant species from the previous created dataset with the contribution of every species to the total basal area
    as.data.table(dom_SP_plot)[as.data.table(dom_SP_plot)[, .I[BA_SP_per == max(BA_SP_per)], by= plot_ID]$V1] %>% 
      rename(., dom_SP = SP_code) %>% 
      select(plot_ID, dom_SP), 
    by = "plot_ID") %>% 
      select(plot_ID, dom_SP) %>% 
      distinct(),
    by = "plot_ID") %>% 
  # translating the species names into BWI codes in capital letters to make joins more easy
  mutate(SP_ger_char_code = case_when(SP_name == "Erle" | 
                                        SP_name == "Roterle"| 
                                        startsWith(SP_name, "Erl") | 
                                        startsWith(SP_name, "diverse, Erle") |
                                        startsWith(SP_name, "diverse, Laubbäume") ~ "SER",
                                      SP_name == "Fichte" | 
                                        startsWith(SP_name, "Fichte") | 
                                        startsWith(SP_name, "diverse, Fichte") |
                                        startsWith(SP_name, "diverse, Nadelbäume") ~ "FI",
                                      SP_name == "Kiefer" | 
                                        startsWith(SP_name, "Kiefer") |
                                        startsWith(SP_name, "diverse, Kiefer") ~ "KI", 
                                      SP_name == "Birke" | 
                                        startsWith(SP_name, "diverse, Birke") ~ "MBI",
                                      SP_name == " " | SP_name == "" | SP_name == "k.A."~ NA, 
                             TRUE ~ NA)) %>% 
  mutate(dom_SP = ifelse(is.na(dom_SP), SP_ger_char_code, dom_SP)) %>% 
  left_join(., SP_names_com_ID_tapeS %>% 
              select(Chr_code_ger, LH_NH) %>% 
              mutate(Chr_code_ger = toupper(Chr_code_ger)), 
            by = c("dom_SP" = "Chr_code_ger")) %>%
                            # normal growth for broad leafed trees no matter the site
  mutate(growth = case_when(LH_NH == "LB" & 
                              hydro_status != "k.A" ~ 1,
                            # normal growth for conifers trees on not regenerated  sites
                            LH_NH == "NB" & 
                              !(startsWith(hydro_status, "naturnah")) & 
                                  hydro_status != "k.A" ~ 1,
                            # hampered growth for conifers on regenerated sites
                            LH_NH == "NB" & 
                              startsWith(hydro_status, "naturnah") & 
                              hydro_status != "k.A" ~ 0, 
                            TRUE ~ NA))

  
        

# these are the species names and the SP_code i selected for them
  # if theres´s mixed species stand i assing the first mentioned species as the domiannt species
site_info %>% select(SP_name) %>% distinct()
# SP_name                                         SP_code
# 1                                        Erle   SER 
# 2                          Erle (Esche, Ulme)   SER
# 3                                      Fichte   FI
# 4                                       Erl +   SER
#   5                                     Roterle SER
# 6  diverse, Fichte, Erle, Moorbirke, waldfrei   FI 
# 7                     diverse, Kiefer, Fichte   KI
# 8          diverse, Fichte, Kiefer, Moorbirke   FI
# 9                                      Kiefer   KI
# 10                                    Fichte    FI
# 11                                      Birke   MBI
# 12                      diverse, Erle / Birke   SER
# 13                                              NA
# 14                     diverse, Kiefer, Birke   KI
# 15                         diverse, Laubbäume   SER
# 16                                       k.A.   NA
# 17           Fichte, größtenteils abgestorben   FI
# 18                     diverse, Erle, Birke,…   SER
# ----- 2. CALCULATIONS --------------------------------------------------------


# ----- 2.1. LIVING TREES -----------------------------------------------------------------
# ----- 2.1.1. heights: regression models for missing tree heights ---------------------------------
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

# ----- 2.1.1.1. coefficents dataframe per SP and plot when >= 3 heights measured --------
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

# ----- 2.1.1.2. coefficents dataframe per SP over all plots when >= 3 heights measured --------
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

# ----- 2.1.1.3. combined coefficients of height models ---------------------
coeff_H_comb <- rbind(coeff_H_SP_P %>% mutate(plot_ID = as.factor(plot_ID)), coeff_H_SP)


# ----- 2.1.1.4. have a look at the quality of the models  ------------------------------
# from my previous attempts to fit and validate species specific but also total 
# dataset including regression models for the height, I know that actually the 
# DBH class is the better predictor 
# but on the other hand the diameter class is also less precise
summary(coeff_H_SP)
# the R2 is pretty poor for some plots 
coeff_heights %>% filter(Rsqr <= 0.3)
#view(trees_total %>% filter(plot_ID == 32080))

summary(coeff_H_SP_P)
coeff_H_comb %>% filter(diff_h >= 0.75)
#view(coeff_H_SP_P %>% filter(rsqrd<=0.5))


# ----- 2.1.1.5. join coefficients to the tree data set & calculate missing heights  ----------------------------
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
              summarise(H_g = sum(mean(na.omit(H_m))*BA_m2)/sum(BA_m2),    # Hoehe des Grundflächemittelstammes, calculation according to S. Schnell
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
                         TRUE ~ H_m), 
         HD_value = (H_m*100)/DBH_cm)     # this is meant for a plausability check so we can filter for trees with an inplausible height/ diameter ratio

# ----- 2.1.2. living tree biomass --------------------------------------------------------------
# input vairbales for the biomass models for the trees aboveground biomass without canopy are: 
# DBH, diameter at 1/3 of the tree height, species, tree height

# ----- 2.1.2.1 dominant height -----------------------------------------------------------

# ----- 2.1.2.1.1. my idea ----------------------------------------------------------------
# necesaryy as side index for the better broadleved models
# Arithmetisches Mittel der Hoehe der 100 stärksten Bäume je ha. (In Deutschland auch als Spitzenhoehe h100 oder h200 bezeichnet; die WEISE�sche Oberhoehe [ho] entspricht der Hoehe des Grundflächen- Mittelstammes der 20 % stärksten Bäume eines Bestandes).
# Wichtig: Die Art der Oberhoehe muss jeweils definiert werden.
# my problem: there are no 100 trees per plot and I don´t know how to estimate the height of the top 100

# https://statisticsglobe.com/select-top-n-highest-values-by-group-in-r#example-2-extract-top-n-highest-values-by-group-using-dplyr-package
# data %>%                                      # Top N highest values by group
#   arrange(desc(value)) %>% 
#   group_by(group) %>%
#   slice(1:3)

# https://rdrr.io/cran/dplyr/man/top_n.html
#df %>% top_n(2)  # highest values
# https://statisticsglobe.com/select-top-n-highest-values-by-group-in-r
# https://stackoverflow.com/questions/1563961/how-to-find-top-n-of-records-in-a-column-of-a-dataframe-using-r filter ..% of observations that represent top 100 trees


# top100 <- function(df, p_ID, plot_A){
#   df <- trees_total_5;                      # the dataframe I´m working with 
#   p_ID <- df %>% dplyr::pull(plot_ID)       # the plot ID 
#   n_plot <- df %>% group_by(plot_ID) %>%  count() %>% dplyr::pull(n, plot_ID);  # number of trees per plot
#   plot_A <- df %>% dplyr::pull(plot_A_ha);  # plot area in hectare
#   n_ha  <- n_plot/mean(plot_A);             # number of trees per hectare by dividing number of trees per plot by pplot area in hectare
#   p <- 100/n_ha;                            # percentage  representing 100 trees of the total number of trees per hectare for each plot
#   return(as.integer(floor(n_plot[p_ID]*p[p_ID])))              # number of rows to select from each 
# }
# 
# trees_total_5 %>% 
#   group_by(plot_ID) %>% 
#   slice_max(., DBH_cm, n = top100(trees_total_5, plot_ID, plot_A_ha):n()) %>% 
#   mutate(H_o = mean(H_m))


# ----- 2.1.2.1.2. dominant height for loop by Alex Cheypowski ----------------------------------------------------------------

#calculate the number of each plot to be selected and write them in vector
  # n_t100 <- trees_total_5 %>% 
  # group_by(plot_ID) %>% 
  # summarize(N_trees_plot = n(), 
  #           plot_A_ha = mean(plot_A_ha)) %>% 
  # mutate(N_trees_ha = N_trees_plot/plot_A_ha, 
  #        percent_t100 = as.numeric((100/N_trees_ha))) %>% 
  # mutate(percent_t100_corrected = as.numeric(ifelse(percent_t100 < 1.0, percent_t100, 1.0)), 
  #        n_rows_t100 = as.integer(N_trees_plot*percent_t100_corrected)) %>% # if the proportion woul dbe higher then the total amount of rows because there are so few trees per hectare calcuate with all the trees per plot (1)
  # select(n_rows_t100, plot_ID)


#calculate the fraction of each plot to be selected and write them in vector
# p_t100 <- trees_total_5 %>% 
#   group_by(plot_ID) %>% 
#   summarize(N_trees_plot = n(), 
#             plot_A_ha = mean(plot_A_ha)) %>% 
#   mutate(N_trees_ha = N_trees_plot/plot_A_ha, 
#          percent_t100 = as.numeric((100/N_trees_ha))) %>% 
#   mutate(percent_t100_corrected = as.numeric(ifelse(percent_t100 < 1.0, percent_t100, as.numeric(1.0))), 
#          n_rows_t100 = as.integer(N_trees_plot*percent_t100_corrected)) %>% # if the proportion woul dbe higher then the total amount of rows because there are so few trees per hectare calcuate with all the trees per plot (1)
#   dplyr::pull(as.numeric(percent_t100_corrected))

#create a vector to store final result(mean hight)
#set its length to number of unique plots
mean100top <- numeric(nrow(trees_total_5 %>% 
                             dplyr::select(plot_ID) %>% 
                             distinct()))


#make a for loop to do slice_max for each plot with it's corresponding proportion
# and write results in mean100top vector
for(id in unique(trees_total_5$plot_ID)){
  # calcuate number of rows to extract per plot
  n_t100 = trees_total_5 %>% 
    filter(plot_ID == id) %>% 
    summarize(plot_ID = mean(plot_ID), 
              N_trees_plot = n(), 
              plot_A_ha = mean(plot_A_ha)) %>% 
    mutate(N_trees_ha = N_trees_plot/plot_A_ha, 
           percent_t100 = as.numeric((100/N_trees_ha))) %>% 
    mutate(percent_t100_corrected = as.numeric(ifelse(percent_t100 < 1.0, percent_t100, 1.0)), 
           n_rows_t100 = as.integer(N_trees_plot*percent_t100_corrected)) %>% # if the proportion would be higher then the total amount of rows because there are so few trees per hectare calcuate with all the trees per plot (1)
    dplyr::pull(n_rows_t100) 
  
   # dataframe with mean height of top n rows 
  sliced_plot <- trees_total_5 %>%
    #group_by(plot_ID) %>% 
    filter(plot_ID == id) %>%
    slice_max(DBH_cm, n = n_t100, with_ties = FALSE) %>% # select top 100 representing rows
    summarise(plot_ID = mean(plot_ID),
              H_g_top = sum(mean(na.omit(H_m))*BA_m2)/sum(BA_m2),    # Hoehe des Grundflächemittelstammes, calculation according to S. Schnell
              top_H = mean(H_m))                         # calculate mean height
  
  mean100top[id] <- sliced_plot[1,2]
}

# ----- 2.1.2.1.3. dominant height dreisatz Lukas Mörler ----------------------------------------------------------------
# new idea by Lukas Mörler 31.04.2023: 

#                               10 000 m2 --> 100 Bäume
#                                    1 ha --> 100 Bäume 
#   1 /(1/0.05003) =  0.05003(plot area) --> 100/(1/0.05003) Bäume
#                                0.05003 --> 5 Bäume pro plot 

  # mean100top.1 <- numeric(nrow(trees_total_5 %>% 
  #                              dplyr::select(plot_ID) %>% 
  #                              distinct()))
  # 
  # for(id in unique(trees_total_5$plot_ID)){
  #   # calcuate number of rows to extract per plot
  #   # id = 26030
  #   
  #   n_t100 = as.numeric(trees_total_5 %>% 
  #     filter(plot_ID == id) %>% 
  #     mutate(n_t100 = as.numeric(ceiling(100/(1/plot_A_ha)))) %>%
  #     select(n_t100) %>% 
  #     distinct() %>% 
  #     dplyr::pull(n_t100))
  #   
  #   # dataframe with mean height of top n rows 
  #   sliced_plot <- trees_total_5 %>%
  #     #group_by(plot_ID) %>% 
  #     filter(plot_ID == id) %>%
  #     slice_max(DBH_cm, n = n_t100, with_ties = FALSE) %>% # select top 100 representing rows
  #     summarise(plot_ID = mean(plot_ID),
  #               H_g_top = sum(mean(na.omit(H_m))*BA_m2)/sum(BA_m2),    # Hoehe des Grundflächemittelstammes, calculation according to S. Schnell
  #               top_H = mean(H_m))                         # calculate mean height
  #   
  #   mean100top.1[id] <- sliced_plot[1,2]
  # }


# ----- 2.1.2.1.3. dominant height # dominant height without loop basen on Lukas Mörler ----------------------------------------------------------------
 # based on:  new idea by Lukas Mörler 31.04.2023: 
    #                               10 000 m2 --> 100 Bäume
    #                                    1 ha --> 100 Bäume 
    # 1 ha divided by 1: plot size so the resul will be the plot size --> the same has to be applied to the other side of the fomula
    #   1 /(1/0.05003) =  0.05003(plot area) --> 100/(1/0.05003) Bäume
    #                                0.05003 --> 5 Bäume pro plot 

# x = floor(100/(1/trees_total_5$plot_A_ha));
# 
# H_o <- trees_total_5 %>%
#   group_by(plot_ID, CCS_nr, C_layer, SP_code) %>% 
#   slice_max(DBH_cm, n = unique(floor(100/(1/trees_total_5$plot_A_ha))), with_ties = FALSE) %>%                  # select top 100 representing rows --> 5 per plot (actually per sampling circuit)
#   summarise(H_g_top = sum(mean(na.omit(H_m))*c_A((DBH_cm/2)/100))/sum(c_A((DBH_cm/2)/100)),    # Hoehe des Grundflächemittelstammes der 100 stärksten Bäume, calculation according to S. Schnell
#             top_H = mean(H_m))
# 

# ----- 2.1.2.2. estimated biomass & carbon living trees -----------------------------------------------------------
trees_total_5 <-  trees_total_5 %>%
  # joining H_o100 in 
  left_join(., trees_total_5 %>%
              group_by(plot_ID, CCS_nr, C_layer, SP_code) %>%     # top 100 are setermined per plot, smaöing corcuit, canopy layer and species
              slice_max(DBH_cm, n = unique(floor(100/(1/trees_total_5$plot_A_ha))), with_ties = FALSE) %>%                     # select top 100 representing rows --> 5 per plot basen on dreisatz (actually per sampling circuit)
              summarise(H_g_top = sum(mean(na.omit(H_m))*c_A((DBH_cm/2)/100))/sum(c_A((DBH_cm/2)/100))),    # Hoehe des Grundflächemittelstammes der 100 stärksten Bäume, calculation according to S. Schnell
            by = c("plot_ID", "CCS_nr", "C_layer", "SP_code")) %>% 
      # TapeS : aing diameter at 0.3 tree height to trees_total dataframe
        #https://gitlab.com/vochr/tapes/-/blob/master/vignettes/tapes.rmd
      mutate(D_03_cm = tprDiameter(tprTrees(spp = tpS_ID, Dm = as.list(DBH_cm), Hm = as.list(DBH_h_cm/100), Ht = H_m, inv = 4), Hx = 1/3*H_m, cp=FALSE),
             DBH_h_m = ifelse(is.na(DBH_h_cm), 1.3, DBH_h_cm/100)) %>% 
      # biomass
      # aboveground biomass   # for trees above species specific diameter threshold
      mutate(aB_kg_GHG = case_when(Bio_SP_group == "fi" & DBH_cm >= 69.0 |
                                 Bio_SP_group == "ki" & DBH_cm >= 59.0 |
                                 Bio_SP_group == "bu" & DBH_cm >= 86.0 |
                                 Bio_SP_group == "ei" & DBH_cm >= 94.0 |
                                 Bio_SP_group == "shw" & DBH_cm >= 113.0 ~ Dunger_aB_DBHath(Bio_SP_group, DBH_cm, D_03_cm, H_m), 
                               # trees >10cm DHB below species specific DBH threshold
                               Bio_SP_group == "fi" & DBH_cm >= 10 & DBH_cm < 69.0 |
                                 Bio_SP_group == "ki" & DBH_cm >= 10 & DBH_cm < 59.0 |
                                 Bio_SP_group == "bu" & DBH_cm >= 10 & DBH_cm < 86.0 |
                                 Bio_SP_group == "ei" & DBH_cm >= 10 & DBH_cm < 94.0 |
                                 Bio_SP_group == "shw" & DBH_cm >= 10 & DBH_cm < 113.0 ~ Dunger_aB_DBHa10(Bio_SP_group, DBH_cm, D_03_cm, H_m), 
                               # trees < 10cm DBH & H < 1.3m 
                               DBH_cm < 10 & H_m >= 1.3 ~ Dunger_aB_H1.3_DBHb10(Bio_SP_group, DBH_cm), 
                               H_m <= 1.3 ~ Dunger_aB_Hb1.3(LH_NH, DBH_cm)),
        # belowground biomass
             bg = Dunger_bB(Bio_SP_group, DBH_cm)) %>% 
      #  # compartiments TapeS
      mutate(ag = tapes_aB(tpS_ID, DBH_cm, DBH_h_m, H_m),                        # total aboveground biomass
             f = ifelse(LH_NH == "NB", tapes_fB(tpS_ID, DBH_cm, DBH_h_m, H_m),  # foliage conifers 
                                  Wutzler_fB_L1(DBH_cm, H_m)),                            # foliage broadleaves
             fw = tapes_brB(tpS_ID, DBH_cm, DBH_h_m, H_m),                      # Nichtderbholz, finebranches
             sw = tapes_swB(tpS_ID, DBH_cm, DBH_h_m, H_m),                      #coarsewood without bark, Derbholz                    
             swb = tapes_swbB(tpS_ID, DBH_cm, DBH_h_m, H_m),
             stw = tapes_stwB(tpS_ID, DBH_cm, DBH_h_m, H_m),                    # stump wood 
             stwb = tapes_stwbB(tpS_ID, DBH_cm, DBH_h_m, H_m),                  # stumbwood bark
             total = ag + bg) 

# exporting dataset with compartiemnts in columns for summary
trees_tot_piv_wider <- trees_total_5

# pivoting all compartiments in 1 column
trees_total_5 <- trees_total_5 %>% 
   pivot_longer(c(total, ag, bg, f, fw, sw, swb,  stw, stwb), 
                names_to = "compartiment", 
                values_to = "B_kg_tapes", 
                names_repair = "unique") %>% 
      # Carbon stock
    mutate(B_t_tapes = B_kg_tapes/1000, 
              C_aB_t_GHG = (aB_kg_GHG/1000)*0.5,
              C_t_tapes = B_t_tapes*0.5) 


# ----- 2.1.2.3. estimated nitrogen in living trees -----------------------------------------------------------
# there is a problem with the join but maybe i can solce it with join by sambling curcuit and canipy layer

trees_total_5 <-trees_total_5 %>% 
  # joingin nitrogen content in
  left_join(., rbind(
    #dataset with nitrogen atock in compartiments & belowground
    trees_total_5_comb_bg <- trees_total_5 %>% 
      unite(SP_com, c(N_SP_group, compartiment), remove = FALSE) %>% 
      mutate( N_t = ifelse(!(compartiment %in% c("bg", "ag", "total")), N_all_com(B_t_tapes[!(compartiment %in%c("bg", "ag", "total"))], SP_com), 
                           ifelse(compartiment == "bg", N_bg(B_t_tapes[compartiment == "bg"], N_bg_SP_group), 0))) %>% 
      filter(!(compartiment %in% c( "ag", "total"))) %>%                        # select only compartiment N stock and belowground N Stock
      select(ID_pt, CCS_nr, C_layer, compartiment, N_t),
    #  N for aboveground
    trees_total_5_comb_bg %>%
      filter(!(compartiment %in% c("bg", "ag", "total"))) %>% # filter for all compartiments and exlcude belowgroundand total from the sum
      group_by(ID_pt, CCS_nr, C_layer) %>%
      summarise(N_t = sum(N_t)) %>%
      mutate(compartiment = "ag"), 
    # N total
    trees_total_5_comb_bg %>%
      filter(!(compartiment %in% c("ag", "total"))) %>% # filter for all compartiembnts and belowground N stock
      group_by(ID_pt,  CCS_nr, C_layer) %>%
      summarise(N_t = sum(N_t)) %>%
      mutate(compartiment = "total")),
    by = c("ID_pt",  "CCS_nr", "C_layer", "compartiment")) 
          

# ----- 2.1.2.3. comparisson biomass trees -----------------------------------------------------------
biotest <- trees_total_5 %>% 
  select(plot_ID, SP_code, tpS_ID, Bio_SP_group, H_SP_group, LH_NH, DBH_cm, DBH_h_cm, D_03_cm, H_m, age, plot_A_ha) %>% 
  distinct() %>%  # this is to not select the multiple rows per tree that result from the pivoting of te compartiments 
  # adding diameter at 0.3 tree height to trees_total dataframe
  mutate(D_03_cm = tprDiameter(tprTrees(spp = tpS_ID, Dm = as.list(DBH_cm), Hm = as.list(DBH_h_cm/100), Ht = H_m, inv = 4), Hx = 1/3*H_m, cp=FALSE),
         DBH_h_m = DBH_h_cm/100) %>% 
  # biomass
        # GHG Dunger aboveground biomass (all woody compartments inkluding bark without leafes for broadleafed trees)
  mutate(GHG_aB_kg = case_when(DBH_cm >= 10 ~ Dunger_aB_DBHa10(Bio_SP_group, DBH_cm, D_03_cm, H_m), # total aboveground biomass
                           DBH_cm < 10 & H_m >= 1.3 ~ Dunger_aB_H1.3_DBHb10(Bio_SP_group, DBH_cm), 
                           H_m >= 1.3 ~ Dunger_aB_Hb1.3(LH_NH, DBH_cm)),
         # TapeS comparable to GHG aB (all woody compartments inkluding bark without leafes for broadleafed trees)
         tapes_ab_kg = tapes_aB(tpS_ID, DBH_cm, DBH_h_m, H_m),                      # total aboveground biomass
         # vondernach comparable to GHG aB (all woody compartments inkluding bark without leafes for broadleafed trees)
         Vondr_oiB_kg = Vondr_oiB(H_SP_group, DBH_cm, H_m),                          # total aboveground biomass
         # GHG Dunger
         GHG__bB_kg = Dunger_bB(Bio_SP_group, DBH_cm)) %>%                           # total belowground biomass
         # WUtzler, Wirth
  mutate(WuWi_fB_kg = ifelse(LH_NH == "NB", Wirth_fB_N(DBH_cm, H_m, age), Wutzler_fB_L1(DBH_cm, H_m)), # foliage
         WuWi_fB_t_ha = WuWi_fB_kg/1000*plot_A_ha,                                              # total foliage abiomass in tons per hectare per tree 
         # branch biomass: this formula leads to branch biomass higher then the stem biomass which cannot be 
         WuWi_brB_kg = ifelse(LH_NH == "NB", Wirth_brB_N(DBH_cm, H_m, age), Wutzler_brB_L1(DBH_cm, H_m)), 
         # stem biomass = if coniferous: tot_bio NH - foliage, if not coniferous: keep aB_kg
         GHG_WuWi_StB_kg = ifelse(LH_NH == "NB", (GHG_aB_kg-(WuWi_fB_kg+WuWi_brB_kg)), (GHG_aB_kg-WuWi_brB_kg)), 
         # total  aboveground = biomass if coniferous: keep aB_kg, if not coniferous: add foliage to woody compartments
         GHG_WuWi_totaB_kg = ifelse(LH_NH == "NB", GHG_aB_kg, GHG_aB_kg+WuWi_fB_kg),   # total aboveground biomass in KG per tree per plot
         GHG_WuWi_totaB_t_ha = GHG_WuWi_totaB_kg/1000*plot_A_ha,                       # total  aboveground biomass in tons per hectare per tree
         # TapeS
         tapes_fB_kg = tapes_fB(tpS_ID, DBH_cm, DBH_h_m, H_m),                        # foliage
         tapes_brB_kg = tapes_brB(tpS_ID, DBH_cm, DBH_h_m, H_m),                      # Nichtderbholz, finebranches
         tapes_DhB_kg = tapes_swB(tpS_ID, DBH_cm, DBH_h_m, H_m),                      #coarsewood without bark, Derbholz                    
         tapes_DhbB_kg = tapes_swbB(tpS_ID, DBH_cm, DBH_h_m, H_m),                    # bark of coarsewood,  Derbholzrinde 
         tapes_stwB_kg = tapes_stwB(tpS_ID, DBH_cm, DBH_h_m, H_m),                    # stump wood 
         tapes_stwbB_kg = tapes_stwbB(tpS_ID, DBH_cm, DBH_h_m, H_m),                # stumbwood bark 
         # Vondernach 
         Vondr_fB_kg = Vondr_fB(H_SP_group, DBH_cm, H_m),                              # foliage
         Vondr_brB_kg = Vondr_NhdB(H_SP_group, DBH_cm, H_m),                          # Nichtderbholz
         Vondr_DhB_kg = Vondr_DhB(H_SP_group, DBH_cm, H_m),                            # coarsewood without bark, Derbholz ohne Rinde
         Vondr_DhRB_kg = Vondr_DhRB(H_SP_group, DBH_cm, H_m),                          # bark of coarsewood, Derbholzrinde
         Vondr_crsWbB_kg = Vondr_crWbB(H_SP_group, DBH_cm, H_m)) %>%                    # coarsewood innkludng bark, Derbholz + Rinde
         # TapeS & GHG
  mutate(tps_GHG_waB = ifelse(LH_NH == "NB", GHG_aB_kg - tapes_fB_kg, GHG_aB_kg),      # woody aboveground biomass Derbholz + nichtderbholz + Rinde
         tps_GHG_crsWbB_kg = GHG_aB_kg - (tapes_fB_kg + tapes_brB_kg),                  # coarsewood including bark
         tps_GHG_brB_kg = GHG_aB_kg - (tapes_fB_kg + tapes_DhB_kg + tapes_DhbB_kg + tapes_stwB_kg + tapes_stwbB_kg),                 # fine branches 
         # Vondernach & GHG
         Vondr_GHG_waB = ifelse(LH_NH == "NB", GHG_aB_kg - Vondr_fB_kg, GHG_aB_kg),    # woody aboveground biomass Derbholz + nichtderbholz + Rinde
         Vondr_GHG_crsWbB_kg = GHG_aB_kg - (Vondr_fB_kg + Vondr_brB_kg),               # coarsewood including bark by removing foliage and branches
         Vondr_GHG_brB_kg = GHG_aB_kg - (Vondr_fB_kg + Vondr_crsWbB_kg)) %>%           # fine branches by withdrawing coarsewood and foliage 
         # stepwise
  mutate(# GHG-TapeS-stepwise
          swB_kg = ifelse(LH_NH == "NB", (GHG_aB_kg - (tapes_fB_kg +tapes_brB_kg+tapes_DhbB_kg+tapes_stwB_kg+tapes_stwbB_kg)),(GHG_aB_kg - (tapes_brB_kg+tapes_DhbB_kg+tapes_stwB_kg+tapes_stwbB_kg))), 
          swbB_kg = GHG_aB_kg - (swB_kg + tapes_fB_kg + tapes_brB_kg+tapes_stwB_kg+tapes_stwbB_kg), # solid wood bark 
          stwB_kg = GHG_aB_kg - (swB_kg + tapes_fB_kg + tapes_brB_kg +swbB_kg +tapes_stwbB_kg),     # stump wood biomass
          stwbB_kg = GHG_aB_kg - (swB_kg + tapes_fB_kg + tapes_brB_kg +swbB_kg + stwB_kg),          # stum wood bark biomass
          fwB_kg = GHG_aB_kg - (swB_kg + swbB_kg + tapes_fB_kg + stwbB_kg + stwB_kg),               # fine wood bionmass
          fB_kg = ifelse(LH_NH == "NB", GHG_aB_kg - (swB_kg + swbB_kg +  stwbB_kg + stwB_kg+ fwB_kg),  Wutzler_fB_L1(DBH_cm, H_m)),  # foliage biomass
          tot_aB_kg = ifelse(LH_NH == "NB", GHG_aB_kg, GHG_aB_kg+fB_kg),                                # total aboveground biomass
          tot_waB_kg = ifelse(LH_NH == "NB", GHG_aB_kg-fB_kg, GHG_aB_kg), 
          tot_GHG_tps = swB_kg+ swbB_kg + stwB_kg+ stwbB_kg + fwB_kg + fB_kg) %>% 
  mutate(diff_GHG_bef_af_tps = GHG_aB_kg - tot_GHG_tps, 
         diff_GHG_tps = GHG_aB_kg - tapes_ab_kg, 
         diff_Vondr_GHG = GHG_aB_kg - Vondr_oiB_kg, 
         diff_Vondr_tps = tapes_ab_kg - Vondr_oiB_kg)
                                         
summary(biotest)

# export biotest



# 2.2. DEAD WOOD ---------------------------------------------------------------

# Artencodes
# 1	Laubholz (außer Eiche)
# 2	Nadelholz
# 3	Eiche
# 4	Unbekannt

# totholztypen
# 1	= liegend; starkes Totholz; umfasst Stamm, Äste, Zweige, abgebrochene Kronen --> Mittendurchmesser 
# 2 = stehend, ganzer Baum; stehendes Totholz mit Ästen                          --> DBH
# 3 = stehend, Bruchstück; Baumstumpf ohne Äste                                  --> L_m > 1.3m DBH, else Mittendurchmesser
# 4 = Wurzelstock                                                                --> Größer Durchmesser der Schnittfläche
# 5 = liegend; ganzer Baum                                                       --> DBH
# 6 = im Haufen vorkommendes Totholz                                             --> Mittendurchmesser

# ----- 2.2.1. species groups, diameter mesuring height, units ---------------------------------------------
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
              select(Chr_ger_cap, tpS_SP_com_name, tpS_ID, LH_NH, N_SP_group, H_SP_group), 
  by = c("dom_SP" = "Chr_ger_cap")) %>%
  mutate(L_m = L_dm/10,
         D_m = as.numeric(D_cm)/100, 
         # if deeadwood type 2, 5 or 3 and L > 1.3m the DBH was taken and in this case compartiments can be calculated by tapeS
         D_h_cm = case_when(DW_type %in% c(2, 3, 5) & L_m  > 1.3  ~ 130,
                            DW_type == 4 ~ (L_m*100),                                 # Stump diameter measuring height will be at the length of the stump
                            TRUE ~ NA),                                             # for deadwood where the Mittendurchmesser was assessed we don´t know the height of diameter measurement 
         D_h_m = case_when(DW_type %in% c(2, 3, 5) & L_m  > 1.3  ~ 1.3,
                           DW_type == 4 ~ L_m,                                      # Stump diameter measuring height will be at the length of the stump
                           TRUE ~ NA), 
         # transforming Biosoil decay types into BWI decay types by joining Biosoil decay type 1 & 2 
         dec_type_BWI = case_when(dec_type == 1 | dec_type == 2 ~ 1, 
                                  dec_type == 3 ~ 2, 
                                  dec_type == 4 ~ 3, 
                                  TRUE ~ 4),
         # translating SP_groups into German character code
         SP_group_char = case_when(SP_group == 1 | (SP_group == 4 & LH_NH == "NB") ~ "FI", 
                                   SP_group == 2 | (SP_group == 4 & LH_NH == "LB") ~ "BU", 
                                   SP_group == 3 ~ "EI", 
                                   TRUE ~ NA), 
         # translating Species groups into TapeS codes
         SP_dw_tps = case_when(SP_group == 1 | (SP_group == 4 & LH_NH == "NB") ~ 1,
                               SP_group == 2 | (SP_group == 4 & LH_NH == "LB") ~ 15,
                               SP_group == 3 ~ 17,
                               TRUE ~ NA), 
         S_L_DW_type = case_when(DW_type %in% c(4, 2, 3) ~ "S",  # if the deadwood type is 2, 3 (stehend; ganzer Baum und Bruchstück) or 4 (Wurzelstock) --> standing deadwood (S)
                                 DW_type %in% c(6, 1, 5) ~ "L", # if the deadwood type is 6 (im Haufen vorkommendes Totholz), 1 (leigend, starkes Totholz), 5 (liegend ganzer Baum) --> lying deadwood (L)
                                 TRUE ~ NA),
         iB_class = case_when(V_dw_m3 < 0.05 ~ "0.05",  # Stückmasse Klasse --> class categorizing volume per deadwood item
                                     V_dw_m3 >= 0.05 & V_dw_m3 < 0.1 ~ "0.1", 
                                     V_dw_m3 >= 0.1 & V_dw_m3 <0.2 ~ "0.2", 
                                     V_dw_m3 >= 0.2 & V_dw_m3 <0.5 ~ "0.5", 
                                     TRUE ~ "0.6")) %>% 
  unite("SP_dec_type", SP_group, dec_type_BWI, sep = "_", remove = FALSE)


# ----- 2.2.2. Deadwood volume, biomass, carbon, compartiment methoden ---------------------------------------------
#Notes Nitrogen compartiments
     # to calcualte the nitrogen stock per compartiment the total biomass has to be divided into 
     # compartimetns according to the deadwood type & the state of decay to assign the correct nitrogen content
        # o	ganze Bäume (Totholztypen 2, 5) in Zersetzungstadien 1 & 2 --> Kompartimentierung mit TapeS in Nichtderbholz, Derbholz o.R., Derbholzrinde, Stock o.R., Stock
        # o	ganze Stämme/ starkes Totolz & Bruchstücke (Totholztypen 3, 1) in Zersetzungstadien 1 & 2 --> Kompartimentierung mit TapeS in Derbholz o.R., Derbholzrinde, Stock o.R., Stock
        # o	Wurzelstock (Totholztyp 4) in Zersetzungstadien 1& 2 --> Komartimentierung mit TapeS in Stock o.R, Stockrinde 
        # L--> this won´t work becaus it will only calcaulte a small percentage of the total mass so I have to treat it like a log
        # o	im Haufen vorkommendes Totholz (Totholztyp 6) in allen Zersetzungstadien & alle anderen Totholztypen in Zersetzungsstadien >= 3 --> keine Kompartimentierung

        # nihtderbholz: Totholztyp 2, 5, & Zersetzung 1, 2 , tapes_fwB(), 0
        # derbholz o.R.: Totholztyp 2, 5, & Zersetzung 1, 2  |  (Totholztypen 3, 1) & Zersetzungstadien 1 & 2 |(Totholztyp 4) in Zersetzungstadien 1& 2   tapes_swB(), B_dw_kg
        # derbholzrinde: Totholztyp 2, 5, & Zersetzung 1, 2  |  (Totholztypen 3, 1) & Zersetzungstadien 1 & 2 |(Totholztyp 4) in Zersetzungstadien 1& 2   tapes_swB(), B_dw_kg
        # stockholz: Totholztyp 2, 5, & Zersetzung 1, 2  |  (Totholztypen 3, 1) & Zersetzungstadien 1 & 2, tapes_stwB() 

# As tapeS can only return total trees Biomasses. Thus we´ll create "pseudo" trees for deadwood items that are 
# not whole trees but are still supposed to be compartitioned 
    # this are: deadwood fragments (Bruchstücke) --> DW_type == 3 
    #           stumps (Wurzelstöcke)            --> DW_type == 4


DW_total <- left_join(DW_total, 
                      # binding both datasets with bark ratio together whereby trees remain destinguishable because of the combination of tree ID and plot ID 
                      rbind(
                # 1. pseudo-tree dataset with bark ratio for solid wood of dead trees type 3 with decay state 1 or 2
                        #       L> For deadwoood type == 3 we have a dDBH, D_h_m but no proper height, so well estimate it with our height functions
                        (DW_total %>% 
                           # filter for DW 3 in low staes of decay
                           filter(DW_type == 3 & dec_type_BWI < 3 &  L_m  > 1.3) %>%
                           mutate(SP_code = dom_SP,
                                   D_mm = D_cm*10) %>% 
                           # unite(SP_P_ID, plot_ID, SP_code, sep = "", remove = FALSE) %>%            # create column matching vectorised coefficients of coeff_SP_P (1.3. functions, h_nls_SP_P, dplyr::pull)
                           #estimating height for Tapes for D_g deadwood species D_g
                           # to create a more generally applicable bark share the pseudo trees are going to be build around the diameter of the mean basal area per plot and species
                           left_join(., DW_total %>%
                                       filter(DW_type == 3 & dec_type_BWI < 3 & L_m  > 1.3) %>%                   
                                       mutate(BA_m2 = (D_m/2)^2*pi) %>% 
                                       group_by(plot_ID, SP_group_char, dec_type_BWI) %>%                  # group by plot and species and canopy layer to calcualte dg, hg 
                                       summarise(dw_D_g = ((sqrt((mean(BA_m2)/pi)))*2)*100,                # Durchmesser des Grundflächenmittelstammes; *100 to get from 1m -> 100cm -> 1000mm
                                                 SP_dw_tps = mean(SP_dw_tps),                              # this is just to keep the species group
                                                 mean_L = mean(L_m),                                      
                                                 dec_type_BWI = mean(dec_type_BWI)) %>%                   # this is just to keep the decay type     
                                       mutate(dw_H_dg_tapes = estHeight(d13 = dw_D_g, sp = SP_dw_tps)) %>% 
                                       select(plot_ID, SP_group_char, dec_type_BWI, mean_L, dw_H_dg_tapes, dw_D_g),    # estimate height for D_g of deadwood by deadwood species group                      
                                     by = c("plot_ID", "SP_group_char", "dec_type_BWI")) %>% 
                           # the same for the dg height estimated via tapeS
                           mutate(dw_H_dg_tapes = case_when(D_h_m > dw_H_dg_tapes & L_m > D_h_m & L_m > mean_L ~ L_m,
                                                            D_h_m > dw_H_dg_tapes & mean_L > D_h_m & L_m < mean_L ~ mean_L,
                                                            TRUE ~ dw_H_dg_tapes)) %>%
                           # if there are still rows were the DBH measurment height exceeds the estiamted height they are excluded --> in this case 1
                           filter(D_h_m < dw_H_dg_tapes) %>% 
                           # calculating biomass in compartiments via TapeS
                           mutate(tapeS_wood = tapes_swB(SP_dw_tps, dw_D_g, D_h_m,  dw_H_dg_tapes),                               # solid wood
                                  tapeS_bark = tapes_swbB(SP_dw_tps, dw_D_g, D_h_m,  estHeight(d13 = dw_D_g, sp = SP_dw_tps)),      # solid wood bark 
                                  dw_tapes_b_ratio = rdB_DW(tapeS_bark, SP_dec_type)/rdB_DW(tapeS_wood, SP_dec_type)) %>%      # ratio between solid wood bark vs. solid wood
                           dplyr::select(plot_ID, t_ID, DW_type, dec_type_BWI, CCS_nr, tapeS_wood, tapeS_bark, dw_tapes_b_ratio)), 
                  # 2. dataset with bark ratio for stump wood for deadwood type 4 in decay state 1 & 2
                        (DW_total %>% 
                           # filter for the respective deadwood type and decay state
                           filter(DW_type == 4 & dec_type_BWI < 3  & L_m  > 1.3) %>%
                           mutate(SP_code = dom_SP,
                                  D_mm = D_cm*10, 
                                  DBH_h_m = 1.3) %>% 
                           # estimating height for Tapes for D_g deadwood species D_g
                           # calculate DBH via Kublin 
                           # --> estimate height via TapeS with Kublin 
                           # --> use that height to calcualte new DBH in  tapeS 
                           # --> calculate D_g grouped by plot_ID and SP_dw_char and dec_type
                           # --> estimate height based on the tapes_D_g
                           left_join(., DW_total %>%
                                       filter(DW_type == 4 & dec_type_BWI < 3  & L_m > 1.3) %>%
                                       mutate(D_mm = D_cm*10,
                                              # estimating diameter via TapeS dirctly doesn´t work because on of the input variables is the tree height which is below 1.3m so it keeps returning 0 
                                              # https://stackoverflow.com/questions/22104774/how-to-initialize-a-vector-with-fixed-length-in-r
                                              # thus we are switching to the BWI taper formula (BWI methodikband chap.5.2.2) dz = d + 2((hd − 130)/tan α)
                                              tapeS_DBH_cm = tprDiameter(tprTrees(spp = SP_dw_tps,                                         # c) estimate diameter with TapeS
                                                                                  Dm = as.list(((D_mm*(1.0+(0.0011*(D_h_cm-130))))/10)),   # a) estimate DBH of the pseudotree via
                                                                                  Hm = as.list(D_h_m),
                                                                                  Ht = estHeight(d13 = ((D_mm*(1.0+(0.0011*(D_h_cm-130))))/10), 
                                                                                                 sp = SP_dw_tps), 
                                                                                  inv = 4), 
                                                                              Hx = rep(1.3, nrow(DW_total %>% filter(DW_type == 4 & dec_type_BWI < 3 & L_m > 1.3))), cp=FALSE), 
                                              BA_m2 = ((tapeS_DBH_cm/100)/2)^2*pi) %>%                    # calcualting basal area with tapeS diameter /100 to get from cm to m 
                                       group_by(plot_ID, SP_group_char, dec_type_BWI) %>% 
                                       summarise(dw_D_g = ((sqrt((mean(BA_m2)/pi)))*2)*100,                # Durchmesser des Grundflächenmittelstammes; *100 to get from 1m -> 100cm -> 1000mm
                                                 mean_L = mean(L_m),                                       # mean length
                                                 SP_dw_tps = mean(SP_dw_tps),                              # this is just to keep the species group
                                                 dec_type_BWI = mean(dec_type_BWI)) %>%                    # this is just to keep the dec type
                                       mutate(dw_H_dg_tapes = estHeight(d13 = dw_D_g, sp = SP_dw_tps)) %>%  # estimate height for D_g of deadwood by deadwood species group 
                                       select(plot_ID, SP_group_char, dec_type_BWI, mean_L, dw_H_dg_tapes, dw_D_g),                         
                                     by = c("plot_ID", "SP_group_char", "dec_type_BWI")) %>%
                           mutate(dw_H_dg_tapes = case_when(D_h_m > dw_H_dg_tapes & L_m > D_h_m & L_m > mean_L & L_m > dw_H_dg_tapes ~ L_m,
                                                            D_h_m > dw_H_dg_tapes & mean_L > D_h_m & L_m < mean_L & mean_L > dw_H_dg_tapes ~ mean_L,
                                                            TRUE ~ dw_H_dg_tapes)) %>%
                           # if there are still rows were the DBH measurment height exceeds the estiamted height they are excluded --> in this case 1
                           filter(DBH_h_m < dw_H_dg_tapes) %>% 
                           mutate(tapeS_wood = tapes_stwB(SP_dw_tps, dw_D_g, DBH_h_m, dw_H_dg_tapes), 
                                  tapeS_bark = tapes_stwbB(SP_dw_tps, dw_D_g, DBH_h_m, dw_H_dg_tapes), 
                                  dw_tapes_b_ratio = rdB_DW(tapeS_bark, SP_dec_type)/rdB_DW(tapeS_wood, SP_dec_type)) %>%  
                           # L> transforming tapeS biomass (for living trees)into tapeS biomass for dead trees via relative density (rdB_DW)
                           dplyr::select(plot_ID, t_ID, DW_type, dec_type_BWI, CCS_nr, tapeS_wood, tapeS_bark,  dw_tapes_b_ratio))), 
                      by = c("plot_ID", "t_ID", "CCS_nr", "DW_type", "dec_type_BWI")) %>% 
  #  volume, biomass, carbon
  mutate(V_dw_meth = ifelse(DW_type %in% c(1, 6, 4) | DW_type == 3 & L_m < 3, "V_DW_T1463", "V_DW_T253"),
         V_dw_m3 = ifelse(DW_type %in% c(1, 6, 4) | DW_type == 3 & L_m < 3, V_DW_T1463(D_m, L_m), V_DW_T253(tpS_ID, D_cm, D_h_cm, L_m)),
         B_dw_kg = B_DW(V_dw_m3, SP_dec_type), 
         C_dw_kg = C_DW(V_dw_m3,SP_dec_type))%>% 
  # calculating TapeS compartiments 
  mutate(dw_tapes_swB_kg = ifelse(DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m  > 1.3 & !is.na(D_h_m),      # yes: tapes_swB
                                  rdB_DW(tapes_swB(tpS_ID[DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m  > 1.3 & !is.na(D_h_m)],
                                                   D_cm[DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m  > 1.3 & !is.na(D_h_m)],
                                                   D_h_m[DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m  > 1.3 &!is.na(D_h_m)],
                                                   L_m[DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m  > 1.3 & !is.na(D_h_m)]),
                                         SP_dec_type[DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m  > 1.3 & !is.na(D_h_m)]),
                                  ifelse(DW_type == 3 & dec_type_BWI < 3 & !is.na(dw_tapes_b_ratio),       # "sw_tapeS_wood"
                                         rdB_DW(tapeS_wood[DW_type == 3 & dec_type_BWI < 3 & !is.na(dw_tapes_b_ratio)], 
                                                SP_dec_type[DW_type == 3 & dec_type_BWI < 3 & !is.na(dw_tapes_b_ratio)]), 
                                         0)),                                                              # "no"
         dw_tapes_swbB_kg = ifelse(DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m  > 1.3 & !is.na(D_h_m),    # yes: tapes_swbB
                                   rdB_DW(tapes_swbB(tpS_ID[DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m  > 1.3 & !is.na(D_h_m)],
                                                     D_cm[DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m  > 1.3 & !is.na(D_h_m)],
                                                     D_h_m[DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m  > 1.3 &!is.na(D_h_m)],
                                                     L_m[DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m  > 1.3 & !is.na(D_h_m)]),
                                          SP_dec_type[DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m  > 1.3 & !is.na(D_h_m)]), 
                                   ifelse(DW_type == 3 & dec_type_BWI < 3 & !is.na(dw_tapes_b_ratio),     #"sw_tapeS_bark"
                                          rdB_DW(tapeS_bark[DW_type == 3 & dec_type_BWI < 3 & !is.na(dw_tapes_b_ratio)],
                                                 SP_dec_type[DW_type == 3 & dec_type_BWI < 3 & !is.na(dw_tapes_b_ratio)]), 
                                          0)),                                                           # no
         dw_tapes_stwB_kg =  ifelse(DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m  > 1.3 & !is.na(D_h_m),   # yes: tapes_stwB
                                    rdB_DW(tapes_stwB(tpS_ID[DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m  > 1.3 & !is.na(D_h_m)],
                                                      D_cm[DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m  > 1.3 & !is.na(D_h_m)],
                                                      D_h_m[DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m  > 1.3 &!is.na(D_h_m)],
                                                      L_m[DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m  > 1.3 & !is.na(D_h_m)]),
                                           SP_dec_type[DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m  > 1.3 & !is.na(D_h_m)]),
                                    ifelse(DW_type == 4 & dec_type_BWI < 3 & !is.na(dw_tapes_b_ratio),     # st_tapeS_wood
                                           rdB_DW(tapeS_wood[DW_type == 4 & dec_type_BWI < 3 & !is.na(dw_tapes_b_ratio)], 
                                                  SP_dec_type[DW_type == 4 & dec_type_BWI < 3 & !is.na(dw_tapes_b_ratio)]), 
                                           0)),                                                            
         dw_tapes_stwbB_kg = ifelse(DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m  > 1.3 & !is.na(D_h_m),   # yes : tapes_stwbB
                                    rdB_DW(tapes_stwbB(tpS_ID[DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m  > 1.3 & !is.na(D_h_m)],
                                                       D_cm[DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m  > 1.3 & !is.na(D_h_m)],
                                                       D_h_m[DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m  > 1.3 &!is.na(D_h_m)],
                                                       L_m[DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m  > 1.3 & !is.na(D_h_m)]),
                                           SP_dec_type[DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m  > 1.3 & !is.na(D_h_m)]),
                                    ifelse(DW_type == 4 & dec_type_BWI < 3 & !is.na(dw_tapes_b_ratio),     # st_tapeS_bark
                                           rdB_DW(tapeS_bark[DW_type == 4 & dec_type_BWI < 3 & !is.na(dw_tapes_b_ratio)]),  
                                           0)),                                                           # no
         dw_tapes_fwB_kg = ifelse(DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m  > 1.3 & !is.na(D_h_m),     # yes: "tapes_foliage"
                                  # I have to add fine wood for BWI_dec_type 2 too,
                                  # but to be able to calculate the whole biomass stepwise, 
                                  # but I will not add it to the final dw_kg column
                                  rdB_DW(tapes_brB(tpS_ID[DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m  > 1.3 & !is.na(D_h_m)],
                                                   D_cm[DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m  > 1.3 & !is.na(D_h_m)],
                                                   D_h_m[DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m  > 1.3 &!is.na(D_h_m)],
                                                   L_m[DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m  > 1.3 & !is.na(D_h_m)]),
                                         SP_dec_type[DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m  > 1.3 & !is.na(D_h_m)]),
                                  0), # no
         dw_tapes_fB_kg = ifelse(DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m  > 1.3 & !is.na(D_h_m),     # yes: "tapes_foliage"
                                 # there is no foliage biomass for deadwood but i need it to correctly calcualte the tapeS compartiemnt stepwise,
                                 # but I will not add it to the final dw_kg column
                                 tapes_fB(tpS_ID[DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m  > 1.3 & !is.na(D_h_m)],
                                          D_cm[DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m  > 1.3 & !is.na(D_h_m)],
                                          D_h_m[DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m  > 1.3 &!is.na(D_h_m)],
                                          L_m[DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m  > 1.3 & !is.na(D_h_m)]),
                                 0),
         dw_tapes_ag_kg = ifelse(DW_type %in% c(2, 5) & dec_type_BWI == 1  & L_m  > 1.3, 
                                 B_dw_kg[DW_type %in% c(2, 5) & dec_type_BWI == 1  & L_m  > 1.3] + dw_tapes_fwB_kg[DW_type %in% c(2, 5) & dec_type_BWI == 1  & L_m  > 1.3],
                                 B_dw_kg)) %>% 
  # stepwise cacluation of compartiemnt biomass via bio - tapeS 
        # solid wood
  mutate(dw_swB_kg = ifelse(DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m  > 1.3 & !is.na(D_h_m), 
                            (B_dw_kg + dw_tapes_fwB_kg + dw_tapes_fB_kg) - (dw_tapes_swbB_kg + dw_tapes_stwB_kg + dw_tapes_stwbB_kg),
                            ifelse(DW_type == 3 & dec_type_BWI < 3 & !is.na(dw_tapes_b_ratio), 
                                   B_dw_kg-(B_dw_kg*(dw_tapes_swbB_kg/dw_tapes_swB_kg)),
                                   B_dw_kg)), 
         # solid wood bark
         dw_swbB_kg = ifelse(DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m  > 1.3 & !is.na(D_h_m),
                             (B_dw_kg + dw_tapes_fwB_kg + dw_tapes_fB_kg) -(dw_swB_kg + dw_tapes_stwB_kg + dw_tapes_stwbB_kg),
                             ifelse(DW_type == 3 & dec_type_BWI < 3 & !is.na(dw_tapes_b_ratio), 
                                    B_dw_kg - dw_swB_kg,
                                    0)),
         # stump wood
         dw_stwB_kg = ifelse(DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m  > 1.3 & !is.na(D_h_m),
                             (B_dw_kg +dw_tapes_fwB_kg + dw_tapes_fB_kg) -(dw_swB_kg + dw_swbB_kg + dw_tapes_stwbB_kg),
                             ifelse(DW_type == 4 & dec_type_BWI < 3 & !is.na(dw_tapes_b_ratio), 
                                    B_dw_kg- (B_dw_kg*(dw_tapes_stwbB_kg/dw_tapes_stwB_kg)),
                                    0)), 
         # stump wood bark
         dw_stwbB_kg = ifelse(DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m  > 1.3 & !is.na(D_h_m), 
                              (B_dw_kg + dw_tapes_fwB_kg + dw_tapes_fB_kg) -(dw_swB_kg + dw_swbB_kg + dw_stwB_kg),
                              ifelse(DW_type == 4 & dec_type_BWI < 3 & !is.na(dw_tapes_b_ratio),
                                     B_dw_kg - dw_stwB_kg,
                                     0)),
         # fine wood
         dw_fwB_kg = ifelse(DW_type %in% c(2, 5) & dec_type_BWI == 1  & L_m  > 1.3 & !is.na(D_h_m), 
                            dw_tapes_fwB_kg, 
                            0),
         # aboveground 
         dw_ag_kg = ifelse(DW_type %in% c(2, 5) & dec_type_BWI == 1 & L_m  > 1.3 & !is.na(D_h_m),
                           B_dw_kg + dw_fwB_kg, # for not decayed standng and lying whole trees we have to add the fine wood mass
                           B_dw_kg)) %>%
  # calcualte nitrogen stock per compartiment
  mutate(N_dw_fw_kg = N_fw(dw_fwB_kg, N_SP_group), 
         N_dw_sw_kg = N_sw(dw_swB_kg, N_SP_group), 
         N_dw_swb_kg = N_swb(dw_swbB_kg, N_SP_group), 
         N_dw_stw_kg = N_swb(dw_stwB_kg, N_SP_group),
         N_dw_stwb_kg = N_swb(dw_stwbB_kg, N_SP_group))%>% 
  mutate(ag_N_dw_kg = N_dw_fw_kg + N_dw_sw_kg + N_dw_swb_kg + N_dw_stw_kg + N_dw_stwb_kg)
  


# checking summary
summary(DW_total)

 
# exporting DW raw dataset with kompartiments 
write.csv(DW_total, "output/out_data/DW_total_compartimente_MoMoK.csv")
  


# removing compartiments from DW_total dataset for further calculations  
DW_total <- DW_total %>% 
  select(-c(dw_fwB_kg, dw_swB_kg, dw_swbB_kg, dw_stwB_kg,
            N_dw_fw_kg, N_dw_sw_kg, N_dw_swb_kg, N_dw_stw_kg, N_dw_stwb_kg,
            dw_tapes_b_ratio, 
            V_dw_meth, 
              dw_tapes_swB_kg, dw_tapes_swbB_kg, dw_tapes_stwB_kg, dw_tapes_stwbB_kg, dw_tapes_fwB_kg, dw_tapes_fB_kg,
             tapeS_wood, tapeS_bark))

 
summary(DW_total)

 





# ----- 2.3. REGENERATION ------------------------------------------------------
# ----- 2.3.1. biomass, carbon, nitrogen ---------------------------------------------------
# I have to do the bimass comparisson first because after i pivot RG_total under the same name 
RG_total_comparisson <- RG_total %>% 
  # assigning numeric diameters to size classes through mean per class 
  mutate(D_cm = case_when(D_class_cm == 0 ~ 0, 
                          D_class_cm == 1 ~ (4.9+0)/2, 
                          D_class_cm == 2 ~ (5.9+5)/2,
                          TRUE ~ (6.9+6)/2)) %>% 
  # total biomass
         # Biomass according to Annighoefer
         # estimating Root collar diameter via Annighoefer
  mutate(Annig_RCD_mm = ifelse(H_cm >= 130, annighoefer_RCD(D_cm, LH_NH, "130"), NA)) %>% # 130 = height at which the diameter was measured [cm]
  # Annighoefer biomass: if there is a DBH: Function including RCD, if H < 1.3m (so no diameter taken) use the Annighoefer equation that relies on height only
   mutate(Annig_aB_kg = ifelse(H_cm >= 130, annighoefer_rg_aB_H1.3_DBHb10(Annig_RCD_mm, H_cm, Annig_SP_group), annighoefer_rg_aB_bH1.3(H_cm, Annig_SP_group)),
  # GHGI biomass: if there is a DBH: function for trees above 1.3m but below 10cm DBH, for trees below 1.3m formula that relies on height
          RG_GHG_aB_kg = ifelse(H_cm >= 130, Dunger_aB_H1.3_DBHb10(Bio_SP_group, D_cm), Dunger_aB_Hb1.3(LH_NH, H_cm/100)),
          RG_GHG_bB_kg = ifelse (H_cm >= 130, Dunger_bB(Bio_SP_group, D_cm), 0), 
          RG_tapeS_ab_kg = ifelse( H_cm > 130 & D_class_cm > 0, 
                                   tapes_aB(RG_total %>%filter(H_cm > 130 & D_class_cm > 0) %>% dplyr::pull(tpS_ID), 
                                            RG_total %>%  mutate(D_cm = case_when(D_class_cm == 0 ~ 0, D_class_cm == 1 ~ (4.9+0)/2, D_class_cm == 2 ~ (5.9+5)/2,TRUE ~ (6.9+6)/2)) %>% 
                                              filter(H_cm > 130 & D_class_cm > 0) %>% dplyr::pull(D_cm), 
                                            dh = rep(1.3, nrow(RG_total %>% filter(H_cm > 130 & D_class_cm > 0))), 
                                           RG_total %>% filter(H_cm > 130 & D_class_cm > 0) %>% mutate (H_m = H_cm/100) %>% dplyr::pull(H_m)), 
                                   Dunger_aB_Hb1.3(LH_NH, H_cm/100))) %>% 
  # belated compartitioning via Poorter: calautaing foliage biomass to deduct it from total biomass
          # root to  leaf  ratio
  mutate(Poorter_fB_kg = Poorter_rg_RLR(RG_GHG_bB_kg, LH_NH),             
          # tapeS foliage when possible (coniferous trees above 1.3m heihgt and DBH > 0), if not possible foliage via Poorter or 0 
                             # broadleaved foliage via Poorter for trees that have a DBH because TapeS doesn´t have it, 
                             # NH trees with a belowgrond masss but below 1.3m won´t have a DBH so they don´t have a bB so they cannot have foliage calcualted via root-leaf-ratio  
         tapeS_Poorter_fB_kg = ifelse((LH_NH == "NB" & H_cm <= 130 & D_class_cm <= 0) | (LH_NH == "LB"), Poorter_rg_RLR(RG_GHG_bB_kg, LH_NH),
                             # coniferous trees foliage is calculated via TapeS for trees that have a DBH via                       
                                       ifelse(LH_NH == "NB" & H_cm >= 130 & D_class_cm > 0, 
                                              tapes_fB(spec_tpS = RG_total %>% filter(LH_NH == "NB" & H_cm > 130 & D_class_cm > 0) %>% dplyr::pull(tpS_ID),
                                                d = RG_total %>%  mutate(D_cm = case_when(D_class_cm == 0 ~ 0, D_class_cm == 1 ~ (4.9+0)/2, D_class_cm == 2 ~ (5.9+5)/2,TRUE ~ (6.9+6)/2)) %>%
                                                  filter(LH_NH == "NB" & H_cm > 130 & D_class_cm > 0) %>% dplyr::pull(D_cm),
                                                dh = rep(1.3, nrow(RG_total %>% filter(LH_NH == "NB" & H_cm > 130 & D_class_cm > 0))),
                                                h = RG_total %>% filter(LH_NH == "NB" & H_cm > 130 & D_class_cm > 0) %>% mutate (H_m = H_cm/100) %>% dplyr::pull(H_m)), 0))) %>%  
  mutate(tapeS_Poorter_meth = ifelse((LH_NH == "NB" & H_cm < 130 & D_class_cm <= 0) | LH_NH == "LB",'poorter', 
                                     ifelse( LH_NH == "NB" & H_cm >= 130 & D_class_cm > 0, 'tapes', 'non')), 
         # tapeS kept calcualting negative values for the foliage of some trees, 
         # which could be linked to the particularly small dimensions which TapeS models are not well fitted for
         # thus negataive foliage values are relplaced by the foliage clauted via Poorters RLR
         tapeS_Poorter_fB_kg = ifelse(tapeS_Poorter_fB_kg < 0, Poorter_fB_kg, tapeS_Poorter_fB_kg)) %>% 
# compartiments 
       # if subtracting the foliage mass frm the total biomass results in negative values, the compartiment in question is set to 0 
  mutate(GHG_Poorter_stem_kg = case_when(LH_NH == "NB" & RG_GHG_aB_kg > Poorter_fB_kg ~ RG_GHG_aB_kg-Poorter_fB_kg, 
                                         LH_NH == "LB" ~ RG_GHG_aB_kg, 
                                         TRUE ~ 0), 
         Annig_Poorter_stem_kg = case_when(LH_NH == "NB" & Annig_aB_kg > Poorter_fB_kg ~ Annig_aB_kg-Poorter_fB_kg, 
                                           LH_NH == "LB" ~ Annig_aB_kg, 
                                           TRUE ~ 0), 
         tapes_Poorter_stem_kg = case_when(LH_NH == "NB" & RG_tapeS_ab_kg > tapeS_Poorter_fB_kg ~ RG_tapeS_ab_kg-tapeS_Poorter_fB_kg, 
                                           LH_NH == "LB" ~ RG_tapeS_ab_kg, 
                                           TRUE ~ 0)) %>% 
  # Nitrogen 
 mutate(RG_C_aB_t = (RG_tapeS_ab_kg*0.5)/1000, 
        RG_C_bB_t = (RG_GHG_bB_kg*0.5)/1000,
        RG_N_stem_kg =  N_fw(tapes_Poorter_stem_kg, N_SP_group), 
        RG_N_f_kg = N_f(tapeS_Poorter_fB_kg, N_SP_group), 
        RG_N_total_kg = RG_N_stem_kg + RG_N_f_kg)





# pivoting RG total s that biomass, carbon etc. of all compartiments is in one column
RG_total<- RG_total %>% 
  # assigning numeric diameters to size classes through mean per class 
  mutate(D_cm = case_when(D_class_cm == 0 ~ 0, 
                          D_class_cm == 1 ~ (4.9+0)/2, 
                          D_class_cm == 2 ~ (5.9+5)/2,
                          TRUE ~ (6.9+6)/2)) %>% 
  # total biomass
  # beloeground biomass for trees aith DBH according to GHGI --> TapeS cannot calculate belowground. 
  mutate(RG_GHG_bB_kg = ifelse (H_cm >= 130, Dunger_bB(Bio_SP_group, D_cm), 0), 
         # if there is a DBH use tapeS aboveground biomass, if not use GHGI function
         RG_tapeS_ab_kg = ifelse( H_cm > 130 & D_class_cm > 0, 
                                  tapes_aB(RG_total %>%filter(H_cm > 130 & D_class_cm > 0) %>% dplyr::pull(tpS_ID), # spp
                                           RG_total %>%  mutate(D_cm = case_when(D_class_cm == 0 ~ 0,               # DBH, Dm    
                                                                                 D_class_cm == 1 ~ (4.9+0)/2, 
                                                                                 D_class_cm == 2 ~ (5.9+5)/2,
                                                                                 TRUE ~ (6.9+6)/2)) %>%
                                             filter(H_cm > 130 & D_class_cm > 0) %>% dplyr::pull(D_cm),             
                                           dh = rep(1.3, nrow(RG_total %>% filter(H_cm > 130 & D_class_cm > 0))),   # dh, D_h_m
                                           RG_total %>% filter(H_cm > 130 & D_class_cm > 0) %>%                     # H_m, Hm 
                                             mutate (H_m = H_cm/100) %>% 
                                             dplyr::pull(H_m)), 
                                  Dunger_aB_Hb1.3(LH_NH, H_cm/100))) %>%                  # if there´s no DBH use the GHGI formula for trees below 1.3m 
  # belated compartitioning via Poorter: calautaing foliage biomass to deduct it from total biomass
  # root to  leaf  ratio
  # tapeS foliage when possible (coniferous trees above 1.3m heihgt and DBH > 0), if not possible foliage via Poorter or 0 
  # broadleaved foliage via Poorter for trees that have a DBH because TapeS doesn´t have it, 
  # NH trees with a belowgrond masss but below 1.3m won´t have a DBH so they don´t have a bB so they cannot have foliage calcualted via root-leaf-ratio  
  mutate(tapeS_Poorter_fB_kg = ifelse((LH_NH == "NB" & H_cm <= 130 & D_class_cm <= 0) | (LH_NH == "LB"), Poorter_rg_RLR(RG_GHG_bB_kg, LH_NH),
                                      # coniferous trees foliage is calculated via TapeS for trees that have a DBH                       
                                      ifelse(LH_NH == "NB" & H_cm >= 130 & D_class_cm > 0, 
                                             tapes_fB(spec_tpS = RG_total %>% filter(LH_NH == "NB" & H_cm > 130 & D_class_cm > 0) %>% dplyr::pull(tpS_ID),
                                                      d = RG_total %>%  mutate(D_cm = case_when(D_class_cm == 0 ~ 0, D_class_cm == 1 ~ (4.9+0)/2, D_class_cm == 2 ~ (5.9+5)/2,TRUE ~ (6.9+6)/2)) %>%
                                                        filter(LH_NH == "NB" & H_cm > 130 & D_class_cm > 0) %>% dplyr::pull(D_cm),
                                                      dh = rep(1.3, nrow(RG_total %>% filter(LH_NH == "NB" & H_cm > 130 & D_class_cm > 0))),
                                                      h = RG_total %>% filter(LH_NH == "NB" & H_cm > 130 & D_class_cm > 0) %>% mutate (H_m = H_cm/100) %>% dplyr::pull(H_m)), 0))) %>%  
  # just to control if the tapes Poorter combination worked
  mutate(tapeS_Poorter_meth = ifelse((LH_NH == "NB" & H_cm < 130 & D_class_cm <= 0) | LH_NH == "LB",'poorter', 
                                     ifelse( LH_NH == "NB" & H_cm >= 130 & D_class_cm > 0, 'tapes', 'non')), 
         # tapeS kept calcualting negative values for the foliage of some trees, 
         # which could be linked to the particularly small dimensions which TapeS models are not well fitted for
         # thus negataive foliage values are relplaced by the foliage clauted via Poorters RLR
         tapeS_Poorter_fB_kg = ifelse(tapeS_Poorter_fB_kg < 0,  Poorter_rg_RLR(RG_GHG_bB_kg, LH_NH), tapeS_Poorter_fB_kg)) %>% 
  # compartiments 
  # if subtracting the foliage mass frm the total biomass results in negative values, the compartiment in question is set to 0 
  mutate(tapes_Poorter_stem_kg = case_when(LH_NH == "NB" & RG_tapeS_ab_kg > tapeS_Poorter_fB_kg ~ RG_tapeS_ab_kg-tapeS_Poorter_fB_kg, 
                                           LH_NH == "LB" ~ RG_tapeS_ab_kg, 
                                           TRUE ~ 0)) %>% 
  select(-tapeS_Poorter_meth) %>%
  # reordering for the pivot so the same compartiemtns are in the same  row
  select( plot_ID,loc_name, state,date,CCS_nr, CCS_position, dist_MB ,CCS_max_dist, t_ID, 
          SP_number, SP_code, Chr_code_ger,bot_name, bot_genus, bot_species, LH_NH, BWI, BWI_SP_group, Bio_SP_group, N_SP_group, N_bg_SP_group, tpS_ID, Annig_SP_group, 
          H_cm, D_class_cm, D_cm,
          tapeS_Poorter_fB_kg, tapes_Poorter_stem_kg, RG_tapeS_ab_kg, RG_GHG_bB_kg) %>% 
  # calculating carbon and nitrogen stock in all compartiments
  mutate(B_total_kg = RG_GHG_bB_kg+ RG_tapeS_ab_kg, 
         C_f_t = (tapeS_Poorter_fB_kg*0.5)/1000, 
         C_stem_t = (tapes_Poorter_stem_kg*0.5)/1000,
         C_aB_t = (RG_tapeS_ab_kg*0.5)/1000,
         C_bB_t = (RG_GHG_bB_kg*0.5)/1000,
         C_total_t = C_aB_t+C_bB_t,
         N_f_kg = N_f(tapeS_Poorter_fB_kg, N_SP_group),
         N_stem_kg =  N_fw(tapes_Poorter_stem_kg, N_SP_group), 
         N_ag_kg = N_stem_kg + N_f_kg,                          # there will be a lot of NA for the aboveground biomass as the lack of diameters doesnt allow to calculate the compartiments 
         N_bB_kg = N_bg(RG_GHG_bB_kg, N_bg_SP_group),   
         N_tot_kg = N_ag_kg + N_bB_kg) %>% 
  # pivoting B, C and N
  # https://stackoverflow.com/questions/70700654/pivot-longer-with-names-pattern-and-pairs-of-columns
  to_long(keys = c("B_compartiment",  "C_compartiment", "N_compartiment"), 
          values = c( "B_kg", "C_t",  "N_kg"),  names(.)[27:31], names(.)[32:36], names(.)[37:41] )%>% 
  # now the only thing left to do is changing the compartiments names and deselct the other compartiment columns
  mutate(B_compartiment = case_when(B_compartiment == "RG_GHG_bB_kg" ~ "bg", 
                                    B_compartiment == "RG_tapeS_ab_kg" ~ "ag", 
                                    B_compartiment == "tapeS_Poorter_fB_kg" ~ "f", 
                                    B_compartiment == "tapes_Poorter_stem_kg" ~ "stem", 
                                    B_compartiment == "B_total_kg" ~ "total", 
                                    TRUE ~ NA)) %>% 
  rename("compartiment" = "B_compartiment") %>% 
  select(-c(C_compartiment , N_compartiment))


summary(RG_total)







# ----- 2.5.PLOT LEVEL: Basal area, species composition, DBH (m, sd), H (m, sd), numer of species carbon stock etc -------------
# ----- 2.5.1. LIVING TREES plot level ------------------------------------------------------------
# ----- 2.5.1.1. grouped by Plot, canopy layer, species ------------------------------
trees_P_CP_SP <- left_join(
  # dataset with BA per species
  trees_total_5 %>%
    filter(compartiment == "ag") %>% 
    group_by(plot_ID, C_layer, SP_code) %>%         # group by plot and species to calculate BA per species 
    summarise(mean_DBH_cm = mean(DBH_cm),           # mean diameter per species per canopy layer per plot
              sd_DBH_cm = sd(DBH_cm),               # standart deviation of diameter per species, canopy layer and plot
              mean_H_m = mean(H_m),                 # mean height per species per canopy layer per plot
              sd_height_m = sd(H_m),                # standard deviation of height --> structural richness indicator
              SP_BA_plot = sum(BA_m2),              # calculate BA per species per canopy layer per plot in m2
              mean_BA_SP_plot = mean(BA_m2),        # calculate mean BA in m2 per species per canopy payer per plot
              h_g = sum(mean(H_m)*BA_m2)/sum(BA_m2),  # Höhe des Grundfächenmittelstammes
              d_g = ((sqrt((mean(BA_m2)/pi)))*2)*10,  # multiply by two to get radius into diameter, multiply by 10 to transform m into cm
              Nt_plot = n(),                           # STückzahl pro plot counting number of observations per group to get number of trees per ha
              plot_A_ha = mean(plot_A_ha)) %>%
    mutate(SP_BA_m2ha = SP_BA_plot/plot_A_ha,         # calculate BA per species per plot in m2/ ha
           Nt_ha = Nt_plot/ plot_A_ha),         
  # dataset with total BA per plot
  trees_total_5 %>%
    filter(compartiment == "ag") %>% 
    group_by(plot_ID, C_layer) %>%                  # group by plot to calculate total BA per plot
    summarise(tot_BA_plot = sum(BA_m2),             # calculate total BA per plot in m2 by summarizing the BA of individual trees after grouping the dataset by plot
              plot_A_ha = mean(plot_A_ha)) %>%      # plot area in hectare to calculate BA per ha
    mutate(tot_BA_m2ha = tot_BA_plot/plot_A_ha),     # calculate total BA per plot in m2 per hectare by dividing total BA m2/plot by area plot/ha
  by=c("plot_ID","C_layer", "plot_A_ha")) %>% 
  select(- c(plot_A_ha, tot_BA_plot)) %>%           # remove unnecessary variables
  mutate(BA_SP_per = (SP_BA_m2ha/tot_BA_m2ha)*100)  # calculate proportion of each species to total BA in percent
# joining data set with dominant species using Ana Lucia Mendez Cartin´s code that filters for those species where BA_SP_per is max
trees_P_CP_SP <- left_join(trees_P_CP_SP, 
                           (as.data.table(trees_P_CP_SP)[as.data.table(trees_P_CP_SP)[, .I[BA_SP_per == max(BA_SP_per)], 
                                                                                      by= c("plot_ID", "C_layer")]$V1]) %>% 
                             rename(., dom_SP = SP_code) %>% 
                             select(plot_ID, C_layer, dom_SP), 
                           by = c("plot_ID", "C_layer")) %>% 
  select(plot_ID, C_layer, 
         SP_code, 
         mean_DBH_cm, sd_DBH_cm, d_g, 
         mean_H_m, sd_height_m, h_g, 
         SP_BA_plot, mean_BA_SP_plot, SP_BA_m2ha, tot_BA_m2ha,
         Nt_plot, Nt_ha, 
         BA_SP_per, dom_SP) 

colnames(trees_P_CP_SP) <- c("Plot", "Bestandesschicht", 
                             "B_Art", 
                             "durchsch_D1.3_cm", "SD_D1.3_cm", "D_g_cm",
                             "durchsch_H_m", "SD_H_m" , "H_g_m",
                             "G_m2_Art", "durchsch_G_m2", "G_m2ha_Art", "G_m2ha_ges",
                            "Stückzahl_Nplot", "stückzahl_Nha",  
                             "G_Anteil_Art", "Hauptbaumart_G")

write.csv(trees_P_CP_SP, "output/out_data/forst_zusammenfassung_MoMoK.csv")

# ----- 2.5.1.2. grouped by Plot, species -----------------------------------------------------------------

# dataset with Biomass, Carbon and nitrogen per compartiment per speices per plot 
trees_P_SP <-  trees_total_5 %>%
    group_by(plot_ID, SP_code, compartiment) %>%
    summarize(B_t_P_SP = sum(B_t_tapes),             # Biomass per SP per compartiment
              C_t_P_SP = sum(C_t_tapes),             # Carbon per SP per compartiment    
              N_t_P_SP = sum(N_t),                   # Nitrogen sper SP per compartiment
              plot_A_ha = mean(plot_A_ha)) %>%      # plot area in hectare to reffer data to hectar
    mutate(MoMoK_A_ha = (50*50)/10000) %>%           # actual momok area in hectar) %>% 
             # data set with BA etc. per species per plot
  left_join(.,  trees_total_5 %>%
              filter(compartiment == "total") %>% 
              group_by(plot_ID, SP_code) %>%       # group by plot and species to calculate BA per species 
              # values per plot
              summarise(mean_DBH_cm = mean(DBH_cm),          # mean diameter per species  per plot
                        sd_DBH_cm = sd(DBH_cm),              # standard deviation of diameter 
                        mean_H_m = mean(H_m),                # mean height per species per  per plot
                        sd_height_m = sd(H_m),               # standart deviation of height --> structual richness indicator
                        SP_BA_plot = sum(BA_m2),             #  BA per species  per plot in m2
                        Nt_P_SP = n(), 
                        plot_A_ha = mean(plot_A_ha)) %>% 
              mutate(MoMoK_A_ha = (50*50)/10000, 
                     compartiment = "total", 
                     Nt_ha = Nt_P_SP/ plot_A_ha, 
                     Nt_MA = (Nt_P_SP/ plot_A_ha)*MoMoK_A_ha, 
                     SP_BA_m2ha = SP_BA_plot/ plot_A_ha, 
                     SP_BA_m2MA = (SP_BA_plot/ plot_A_ha)*MoMoK_A_ha) %>%             # this is to only add the values to one row per plot and species and avoid repeted plot-species wise values that are not grouped by compartiment and thus keep being repeated 
              # dataset with total BA per plot to calcualte share of each species by total basal area
              left_join(.,trees_total_5 %>%
                          filter(compartiment == "total") %>%
                          group_by(plot_ID) %>%                         # group by plot to calculate total BA per plot
                          summarise(tot_BA_plot = sum(BA_m2),           # calculate total BA per plot in m2 by summarizing the BA of individual trees after grouping the dataset by plot
                                    plot_A_ha = mean(plot_A_ha)) %>%    # plot area in hectare to calculate BA per ha
                          mutate(tot_BA_m2ha = tot_BA_plot/plot_A_ha,   # calculate total BA per plot in m2 per hectare by dividing total BA m2/plot by area plot/ha
                                 compartiment = "total"),               # just to ensure the join works
                        by=c("plot_ID", "plot_A_ha", "compartiment")) %>% 
              select(- c(plot_A_ha, tot_BA_plot, MoMoK_A_ha)) %>%  # remove unnecessary variables
              mutate(BA_SP_per = (SP_BA_m2ha/tot_BA_m2ha)*100),  # calculate proportion of each species to total BA in percent)
    by = c("plot_ID", "SP_code", "compartiment")) %>%   # join it to the dataset grouped by plot, species and compartiment 
  mutate(B_t_P_SP_ha = B_t_P_SP/plot_A_ha, 
         C_t_P_SP_ha = C_t_P_SP/plot_A_ha,
         N_t_P_SP_ha = N_t_P_SP/plot_A_ha,
         B_t_P_SP_MA = (B_t_P_SP/plot_A_ha)*MoMoK_A_ha, 
         C_t_P_SP_MA = (C_t_P_SP/plot_A_ha)*MoMoK_A_ha,
         N_t_P_SP_MA = (N_t_P_SP/plot_A_ha)*MoMoK_A_ha)

# joining dataset with dominant species using Ana Lucia Mendez Cartins code that filters for those species where BA_SP_per is max
trees_P_SP <- left_join(trees_P_SP,
                        as.data.table(trees_P_SP %>% filter(compartiment== "total"))[as.data.table(trees_P_SP %>% filter(compartiment== "total"))[, .I[BA_SP_per == max(BA_SP_per)], by= plot_ID]$V1] %>% 
                          rename(., dom_SP = SP_code) %>% 
                          select(plot_ID, compartiment, dom_SP), 
                        by = c("plot_ID", "compartiment"))

trees_P_SP.export <- trees_P_SP %>% 
  select(-c(MoMoK_A_ha, plot_A_ha)) %>% 
  select(plot_ID, SP_code, compartiment,
         mean_DBH_cm, sd_DBH_cm, mean_H_m, sd_height_m, 
         SP_BA_plot, SP_BA_m2ha, SP_BA_m2MA, tot_BA_m2ha, BA_SP_per, dom_SP,
         Nt_P_SP, Nt_ha, Nt_MA,
         B_t_P_SP, C_t_P_SP, N_t_P_SP,
         B_t_P_SP_ha, C_t_P_SP_ha, N_t_P_SP_ha,
         B_t_P_SP_MA, C_t_P_SP_MA, N_t_P_SP_MA)

colnames(trees_P_SP.export) <- c("Plot", "B_Art", "Kompartiment", 
                                 "durchsch_D1.3_cm", "SD_D1.3_cm" ,  "durchsch_H_m" , "SD_H_m",
                                 "G_m2_Art_Plot", "G_m2ha_Art", "G_m2MF_Art", "G_ges_m2ha", "G_Anteil_Art","Hauptbaumart_G",
                                 "Stückzahl_n_Art_Plot", "Stückzahl_n__ha"  , "Stückzahl_n_MF",
                                 "B_t_Plot_Art", "C_t_Plot_Art", "N_t_Plot_Art",
                                 "B_t_ha", "C_t_ha", "N_t_ha",
                                 "B_t_MF", "C_t_MF", "N_t_MF")
# exporting dataset
write.csv(trees_P_SP.export, "output/out_data/LB_Art_Plot_MoMoK.csv")

summary(trees_P_SP)

# ----- 2.5.1.3. grouped by Plot----------------------------------------------------------

trees_P <- trees_total_5 %>%
  group_by(plot_ID, compartiment) %>% 
  summarise(B_t_plot = sum(B_t_tapes),             # Biomass per plot per compartiment
            C_t_plot = sum(C_t_tapes),             # Carbon per plot per compartiment    
            N_t_plot = sum(N_t),                   # Nitrogen per plot per compartiment
            plot_A_ha = mean(plot_A_ha)) %>%       # plot area in hectare to reffer data to hectar later
  mutate(MoMoK_A_ha = (50*50)/10000) %>%           # MoMoK area in hectare to reffer data to MoMoK area later
  left_join(., trees_total_5 %>%
              filter(compartiment == "total") %>% 
              group_by(plot_ID) %>%                         # group by plot only to create column with 
              summarise(mean_DBH_cm = mean(DBH_cm),          # mean diameter per species  per plot
                        sd_DBH_cm = sd(DBH_cm),              # standard deviation of diameter 
                        mean_H_m = mean(H_m),                # mean height per species per  per plot
                        sd_height_m = sd(H_m),               # standart deviation height
                        BA_m2_plot = sum(BA_m2),            # calculate total BA per plot in m2 by summarizing the BA of individual trees after grouping the dataset by plot
                        Nt_plot = n(),                          # number of trees per plot
                        plot_A_ha = mean(plot_A_ha)) %>%    # plot area in hectare to calculate BA per ha
              mutate(MoMoK_A_ha = (50*50)/10000, 
                     BA_m2ha = BA_m2_plot/plot_A_ha,       # calculate total BA per plot in m2 per hectare by dividing total BA m2/plot by area plot/ha
                     BA_m2MA = (BA_m2_plot/plot_A_ha)*MoMoK_A_ha,
                     Nt_ha = Nt_plot/plot_A_ha,
                     Nt_MA = (Nt_plot/plot_A_ha)*MoMoK_A_ha,
                     compartiment = "total") %>% 
              select(-c(plot_A_ha, MoMoK_A_ha)),            # just to ensure the join works
            by = c("plot_ID", "compartiment")) %>%           # join it to the dataset grouped by plot, species and compartiment 
  mutate(B_t_ha = B_t_plot/plot_A_ha, 
         C_t_ha = C_t_plot/plot_A_ha,
         N_t_ha = N_t_plot/plot_A_ha,
         B_t_MA = (B_t_plot/plot_A_ha)*MoMoK_A_ha, 
         C_t_MA = (C_t_plot/plot_A_ha)*MoMoK_A_ha,
         N_t_MA = (N_t_plot/plot_A_ha)*MoMoK_A_ha) %>% 
  # dataset with dominatn species per plot
  left_join(., trees_P_SP %>% 
              ungroup() %>% # this was necesarry becaue it kept grouping the data by their species group
              dplyr::select(plot_ID, compartiment, dom_SP) %>%
              filter(compartiment == "total") %>%
              distinct(), 
            by = c("plot_ID", "compartiment")) %>% 
  #calculating number of species per plot
  left_join(., trees_total_5 %>%
              filter(compartiment == "total") %>% 
              select(plot_ID, SP_code) %>%
              group_by(plot_ID) %>% 
              distinct() %>% 
              summarise(N_SP_plot = n())%>%
              mutate(compartiment = "total"),
            by = c("plot_ID", "compartiment"))


# preparing trees_P for 
trees_P.export <- trees_P %>% 
  select(-c(MoMoK_A_ha,plot_A_ha)) %>% 
  select(plot_ID, compartiment,
         mean_DBH_cm, sd_DBH_cm, mean_H_m, sd_height_m, 
         BA_m2_plot, BA_m2MA, BA_m2ha, dom_SP, N_SP_plot,
         Nt_plot, Nt_ha, Nt_MA,
         B_t_plot, C_t_plot, N_t_plot, 
         B_t_ha, C_t_ha, N_t_ha,
         B_t_MA, C_t_MA, N_t_MA)

colnames(trees_P.export) <- c("Plot", "Kompartiment", 
                                 "durchsch_D1.3_cm", "SD_D1.3_cm" ,  "durchsch_H_m" , "SD_H_m",
                                 "G_m2_Plot", "G_m2ha", "G_m2MF", "Hauptbaumart_G", "Anzahl_Art_Plot",
                                 "Stückzahl_n_Art_Plot", "Stückzahl_n__ha"  , "Stückzahl_n_MF", 
                                 "B_t_Plot_Art", "C_t_Plot_Art", "N_t_Plot_Art",
                                 "B_t_ha", "C_t_ha", "N_t_ha",
                                 "B_t_MF", "C_t_MF", "N_t_MF")

write.csv(trees_P.export, "output/out_data/LB_Plot_MoMoK.csv")




# ----- 2.5.2. DEAD TREES plot level --------------------------------------------------------
# ----- 2.5.2.1. grouped by Plot, species, deadwood type, decay type -------------------------
DW_P_SP_TY_DEC <- DW_total %>% 
  group_by(plot_ID, SP_group, DW_type, dec_type) %>% 
  summarise(D_mean = mean(D_cm), 
            L_mean = mean(L_m),
            B_tot_t = sum(B_dw_kg/1000),
            C_tot_t = sum(C_dw_kg/1000), 
            N_tot_t = sum(ag_N_dw_kg/1000)) %>%
  # dataset with are per plot cnsidreing multpiple sampling circuits per plot
  left_join(., DW_total %>%
              select(plot_ID, CCS_nr) %>% 
              distinct() %>%
              mutate(CCS_A_ha = c_A(12.62)/10000) %>% 
              group_by(plot_ID) %>%
              summarize(plot_A_ha = sum(CCS_A_ha)), 
            by = "plot_ID") %>%
  mutate(B_tot_t_ha = B_tot_t/plot_A_ha, 
         C_tot_t_ha = C_tot_t/plot_A_ha,
         N_tot_t_ha = N_tot_t/plot_A_ha) %>% 
  # data set with total biomass, carbon, nitrogen per plot and species group
  left_join(., DW_total %>% 
              group_by(plot_ID, SP_group) %>% 
              summarise(plot_B_tot_t = sum(B_dw_kg/1000),
                        plot_C_tot_t = sum(C_dw_kg/1000), 
                        plot_N_tot_t = sum(ag_N_dw_kg/1000)), 
            by= c("plot_ID", "SP_group")) %>% 
  mutate(C_share = (C_tot_t/plot_C_tot_t)*100,
         N_share = (N_tot_t/plot_N_tot_t)*100, 
         B_share = (B_tot_t/plot_B_tot_t)*100) %>% 
  dplyr::select(- c("plot_B_tot_t", "plot_C_tot_t", "plot_N_tot_t"))

# ----- 2.5.2.2.grouped by plot, species, deadwood type -------------------------
DW_P_SP_TY <- DW_total %>% 
  group_by(plot_ID, SP_group, DW_type) %>% 
  summarise(D_mean = mean(D_cm), 
            L_mean = mean(L_m),
            B_tot_t = sum(B_dw_kg/1000),
            C_tot_t = sum(C_dw_kg/1000), 
            N_tot_t = sum(ag_N_dw_kg/1000)) %>%
  # dataset with are per plot cnsidreing multpiple sampling circuits per plot
  left_join(., DW_total %>%
              select(plot_ID, CCS_nr) %>% 
              distinct() %>%
              mutate(CCS_A_ha = c_A(12.62)/10000) %>% 
              group_by(plot_ID) %>%
              summarize(plot_A_ha = sum(CCS_A_ha)), 
            by = "plot_ID") %>%
  mutate(B_tot_t_ha = B_tot_t/plot_A_ha, 
         C_tot_t_ha = C_tot_t/plot_A_ha,
         N_tot_t_ha = N_tot_t/plot_A_ha) %>%  
  # data set with total biomass/ 
  left_join(., DW_total %>% 
              group_by(plot_ID, SP_group) %>% 
              summarise(plot_B_tot_t = sum(B_dw_kg/1000),
                        plot_C_tot_t = sum(C_dw_kg/1000), 
                        plot_N_tot_t = sum(ag_N_dw_kg/1000)), 
            by= c("plot_ID", "SP_group")) %>% 
  mutate(C_share = (C_tot_t/plot_C_tot_t)*100,
         N_share = (N_tot_t/plot_N_tot_t)*100, 
         B_share = (B_tot_t/plot_B_tot_t)*100) %>% 
  dplyr::select(- c("plot_B_tot_t", "plot_C_tot_t", "plot_N_tot_t"))

# ----- 2.5.2.3.grouped by decay type and deadwood type -------------------------
DW_P_TY_DEC <- DW_total %>% 
  group_by(plot_ID, dec_type, DW_type) %>% 
  summarise(D_mean = mean(D_cm), 
            L_mean = mean(L_m),
            B_tot_t = sum(B_dw_kg/1000),
            C_tot_t = sum(C_dw_kg/1000), 
            N_tot_t = sum(ag_N_dw_kg/1000)) %>%
  # dataset with are per plot cnsidreing multpiple sampling circuits per plot
  left_join(., DW_total %>%
              select(plot_ID, CCS_nr) %>% 
              distinct() %>%
              mutate(CCS_A_ha = c_A(12.62)/10000) %>% 
              group_by(plot_ID) %>%
              summarize(plot_A_ha = sum(CCS_A_ha)), 
            by = "plot_ID") %>%
  mutate(B_tot_t_ha = B_tot_t/plot_A_ha, 
         C_tot_t_ha = C_tot_t/plot_A_ha,
         N_tot_t_ha = N_tot_t/plot_A_ha) %>%  
  # data set with total biomass/ 
  left_join(., DW_total %>% 
              group_by(plot_ID) %>% 
              summarise(plot_B_tot_t = sum(B_dw_kg/1000),
                        plot_C_tot_t = sum(C_dw_kg/1000), 
                        plot_N_tot_t = sum(ag_N_dw_kg/1000)), 
            by= c("plot_ID")) %>% 
  mutate(C_share = (C_tot_t/plot_C_tot_t)*100,
         N_share = (N_tot_t/plot_N_tot_t)*100, 
         B_share = (B_tot_t/plot_B_tot_t)*100) %>% 
  dplyr::select(- c("plot_B_tot_t", "plot_C_tot_t", "plot_N_tot_t"))

# ----- 2.5.2.4.grouped by plot, species, decay type -----------------------------
DW_P_SP_DEC <- DW_total %>% 
  group_by(plot_ID, SP_group, dec_type) %>% 
  summarise(D_mean = mean(D_cm), 
            L_mean = mean(L_m),
            B_tot_t = sum(B_dw_kg/1000),
            C_tot_t = sum(C_dw_kg/1000), 
            N_tot_t = sum(ag_N_dw_kg/1000)) %>% 
  # dataset with are per plot cnsidreing multpiple sampling circuits per plot
  left_join(., DW_total %>%
              select(plot_ID, CCS_nr) %>% 
              distinct() %>%
              mutate(CCS_A_ha = c_A(12.62)/10000) %>% 
              group_by(plot_ID) %>%
              summarize(plot_A_ha = sum(CCS_A_ha)), 
            by = "plot_ID") %>%
  mutate(B_tot_t_ha = B_tot_t/plot_A_ha, 
         C_tot_t_ha = C_tot_t/plot_A_ha,
         N_tot_t_ha = N_tot_t/plot_A_ha) %>%  
  # data set with total biomass/ 
  left_join(., DW_total %>% 
              group_by(plot_ID, SP_group) %>% 
              summarise(plot_B_tot_t = sum(B_dw_kg/1000),
                        plot_C_tot_t = sum(C_dw_kg/1000), 
                        plot_N_tot_t = sum(ag_N_dw_kg/1000)), 
            by= c("plot_ID", "SP_group")) %>% 
  mutate(C_share = (C_tot_t/plot_C_tot_t)*100,
         N_share = (N_tot_t/plot_N_tot_t)*100, 
         B_share = (B_tot_t/plot_B_tot_t)*100) %>% 
  dplyr::select(- c("plot_B_tot_t", "plot_C_tot_t", "plot_N_tot_t"))

# ----- 2.5.2.5.deawood biomass, carbon and nitrogen grouped decay type -----------------------------
DW_P_DEC <- DW_total %>% 
  group_by(plot_ID, dec_type) %>% 
  summarise(D_mean = mean(D_cm), 
            L_mean = mean(L_m),
            B_tot_t = sum(B_dw_kg/1000),
            C_tot_t = sum(C_dw_kg/1000), 
            N_tot_t = sum(ag_N_dw_kg/1000)) %>% 
  # dataset with area per plot considering multipple sampling circuits per plot
  left_join(., DW_total %>%
              select(plot_ID, CCS_nr) %>% 
              distinct() %>%
              mutate(CCS_A_ha = c_A(12.62)/10000) %>% 
              group_by(plot_ID) %>%
              summarize(plot_A_ha = sum(CCS_A_ha)), 
            by = "plot_ID") %>%
  mutate(B_tot_t_ha = B_tot_t/plot_A_ha, 
         C_tot_t_ha = C_tot_t/plot_A_ha,
         N_tot_t_ha = N_tot_t/plot_A_ha) %>%  
  # data set with total biomass/ 
  left_join(., DW_total %>% 
              group_by(plot_ID) %>% 
              summarise(plot_B_tot_t = sum(B_dw_kg/1000),
                        plot_C_tot_t = sum(C_dw_kg/1000), 
                        plot_N_tot_t = sum(ag_N_dw_kg/1000)), 
            by= c("plot_ID")) %>% 
  mutate(C_share = (C_tot_t/plot_C_tot_t)*100,
         N_share = (N_tot_t/plot_N_tot_t)*100, 
         B_share = (B_tot_t/plot_B_tot_t)*100) %>% 
  dplyr::select(- c("plot_B_tot_t", "plot_C_tot_t", "plot_N_tot_t"))

# ----- 2.5.2.6.deawood by deadwood type -----------------------------
DW_P_TY <- DW_total %>% 
  # dataset grouped by plot and deadwood type 
  group_by(plot_ID, DW_type) %>% 
  summarise(D_mean = mean(D_cm), 
            L_mean = mean(L_m),
            B_tot_t = sum(B_dw_kg/1000),
            C_tot_t = sum(C_dw_kg/1000), 
            N_tot_t = sum(ag_N_dw_kg/1000), 
            V_tot_m3 = sum(V_dw_m3)) %>%  
            #plot_A_ha = sum(c_A(12.62)/10000)) %>% 
    # dataset with are per plot cnsidreing multpiple sampling circuits per plot
    left_join(., DW_total %>%
                select(plot_ID, CCS_nr) %>% 
                distinct() %>%
                mutate(CCS_A_ha = c_A(12.62)/10000) %>% 
                group_by(plot_ID) %>%
                summarize(plot_A_ha = sum(CCS_A_ha)), 
              by = "plot_ID") %>%
  mutate(B_tot_t_ha = B_tot_t/plot_A_ha, 
         C_tot_t_ha = C_tot_t/plot_A_ha,
         N_tot_t_ha = N_tot_t/plot_A_ha, 
         V_tot_m3_ha = V_tot_m3/plot_A_ha) %>%  
  # data set with total biomass/ 
  left_join(., DW_total %>% 
              group_by(plot_ID) %>% 
              summarise(plot_B_tot_t = sum(B_dw_kg/1000),
                        plot_C_tot_t = sum(C_dw_kg/1000), 
                        plot_N_tot_t = sum(ag_N_dw_kg/1000), 
                        plot_V_tot_m3 = sum(V_dw_m3)), 
            by= c("plot_ID")) %>% 
  mutate(C_share = (C_tot_t/plot_C_tot_t)*100,
         N_share = (N_tot_t/plot_N_tot_t)*100, 
         B_share = (B_tot_t/plot_B_tot_t)*100, 
         V_share = (V_tot_m3/plot_V_tot_m3)*100) 



# -----2.5.2.7. deadwood  by plot ---------------------------------------------------

DW_P <- DW_total %>% 
    group_by(plot_ID) %>% 
    summarise(#plot_A_ha = mean(CCS_A_ha),
            V_m3_plot = sum(na.omit(V_dw_m3)), 
            B_t_plot = sum(na.omit(B_dw_kg)/1000),
            # actually this should be the B_t_ha because it also includes the fine wood compartiment, 
            # which is not included in the transformation from volume to biomass performed by the BWI
            B_t_tapes_plot = sum(na.omit(dw_ag_kg)/ 1000), 
            C_t_plot = sum(na.omit(C_dw_kg)/1000), 
            N_t_plot = sum(na.omit(ag_N_dw_kg)/1000), 
            Nt_plot = n()) %>%
    # dataset with are per plot cnsidreing multpiple sampling circuits per plot
    left_join(., DW_total %>%
                select(plot_ID, CCS_nr) %>% 
                distinct() %>%
                mutate(CCS_A_ha = c_A(12.62)/10000) %>% 
                group_by(plot_ID) %>%
                summarize(plot_A_ha = sum(CCS_A_ha))%>% 
                mutate(MoMoK_A_ha = (50*50)/10000), 
              by = "plot_ID") %>% 
    mutate(V_m3_ha = V_m3_plot/plot_A_ha, 
           B_t_ha = B_t_plot/plot_A_ha, 
           B_t_tapes_ha = B_t_tapes_plot/ plot_A_ha, 
           C_t_ha = C_t_plot/plot_A_ha, 
           N_t_ha = N_t_plot/plot_A_ha, 
           Nt_ha = Nt_plot/plot_A_ha, 
           # referring carbon & nitrogen stocks to actual Momok size 
           V_m3_MA = (V_m3_plot/plot_A_ha)*MoMoK_A_ha,
           B_t_MA = (B_t_plot/plot_A_ha)*MoMoK_A_ha,
           B_t_tapes_MA = (B_t_tapes_plot/ plot_A_ha)*MoMoK_A_ha,
           C_t_MA = (C_t_plot/ plot_A_ha)*MoMoK_A_ha, 
           N_t_MA = (N_t_plot/plot_A_ha)*MoMoK_A_ha,
           Nt_MA = (Nt_plot/ plot_A_ha)*MoMoK_A_ha) %>% 
  # number of decay types per plot
  left_join(., DW_total %>% 
              select(plot_ID, dec_type) %>%
              group_by(plot_ID) %>%
              distinct() %>% 
              summarise(N_dec_type_plot = n()),
            by = "plot_ID") %>% 
  # number of deadwood types per plot
  left_join(., DW_total %>% 
            select(plot_ID, DW_type) %>%
            group_by(plot_ID) %>%
            distinct() %>% 
            summarise(N_DW_type_plot = n()),
          by = "plot_ID") 
  

summary(DW_P)




# ----- 2.5.4. REGENERATION plot level -------------------------------------------------------------------
# ----- 2.5.4.1. grouped by Plot and species  ------------------------------------------------------------------
RG_P_SP <- RG_total %>% 
  # sum plot area of all sampling circuits per plot together to calcualte plot area to reffer data to hectare
  left_join(., RG_total %>%
              filter(compartiment == "total") %>% 
              group_by(plot_ID, CCS_nr) %>% 
              summarise(CCS_max_dist_m = mean(CCS_max_dist/100)) %>%  # 10000 to transform m2 into ha, the plot radius has to be the distance of furthest plant to the RG sampling circuit
              mutate(CCS_A_ha = c_A(CCS_max_dist_m)/10000) %>% 
              group_by(plot_ID) %>% 
              summarise(plot_A_ha = sum(CCS_A_ha)), 
            by = "plot_ID") %>% 
  # summing up biomass, carbon and nitrogen per speices, plot and compartiment 
    group_by(plot_ID, SP_code, compartiment) %>%                            # group by plot and species to calculate BA per species 
    summarise(B_t_plot = sum(B_kg/1000), 
              C_t_plot = sum(C_t),                           # sum of  carbon stock per plot,  species and compartiment
              N_t_plot = sum(na.omit(N_kg/1000)),      # sum of aboveground Nitrogen stock per plot and species and compartiment
              plot_A_ha = mean(plot_A_ha)) %>% # plot area
    mutate(MoMok_A_ha = (50*50)/10000,                 # momok area 0.25 ha 
  # B, C, N hectar values
         B_t_ha = B_t_plot/plot_A_ha, 
         C_t_ha = C_t_plot/plot_A_ha, 
         N_t_ha = N_t_plot/plot_A_ha,
  # B, C, N  MoMoK area values 
         B_t_MA = (B_t_plot/plot_A_ha)*MoMok_A_ha,
         C_t_MA = (C_t_plot/plot_A_ha)*MoMok_A_ha,
         N_t_MA = (N_t_plot/plot_A_ha)*MoMok_A_ha) %>% 
  # join in mean diameter and height per plot and species group (grouped separately to avoid repetition with every compartiment)
  left_join(., RG_total %>% 
              filter(compartiment == "total") %>% 
              group_by(plot_ID, SP_code) %>%
              summarise(mean_D_cm = mean(D_cm),                         # mean diameter per species per canopy layer per plot
                        mean_H_m = mean(H_cm/100),                      # mean height per species per canopy layer per plot
                        #SP_BA_m2_plot = sum(c_A(D_cm/2)),               # Basal area in m2 per plot and speices --> doesnt make sense bacause not all trees have a diameter 
                        N_trees_plot = n()) %>%                          # number of individuals per plot and species
              mutate(compartiment = "total"),                           # just to enable a clean join
            by = c("plot_ID", "SP_code", "compartiment")) %>% 
  # join in C, N and number of trees of whole plot to calculate shares
  left_join(., RG_total %>%
              filter(compartiment == "total") %>%
              group_by(plot_ID, CCS_nr) %>%
              summarise(CCS_max_dist_m = mean(CCS_max_dist/100), 
                        CCS_tot_C_t = sum(C_t),                    # total C stock in tons per sampling circuit
                        CCs_tot_N_t = sum(na.omit(N_kg/1000)),     # total N stock in tons per sampling circuit
                        CCS_tot_N_trees = n()) %>%                 # 10000 to transform m2 into ha, the plot radius has to be the distance of furthest plant to the RG sampling circuit
              mutate(CCS_A_ha = c_A(CCS_max_dist_m)/10000) %>% 
              group_by(plot_ID) %>% 
              summarise(plot_A_ha = sum(CCS_A_ha),                  # calcualte plot area in ha by summing up area of sampling circuits per plot
                        plot_tot_C_t = sum(CCS_tot_C_t),                    # total C stock in tons per plot
                        plot_tot_N_t = sum(CCs_tot_N_t),     # total N stock in tons per plot
                        plot_tot_N_trees = sum(CCS_tot_N_trees)) %>%                 # total number of trees per plot
              mutate(compartiment = "total") %>% 
              select(-plot_A_ha),         
            by = c("plot_ID", "compartiment")) %>% 
  mutate(tot_N_trees_ha = plot_tot_N_trees/plot_A_ha,
         tot_N_trees_MA = (plot_tot_N_trees/plot_A_ha)*MoMok_A_ha,
         SP_N_trees_ha = N_trees_plot/ plot_A_ha,
         SP_N_trees_MA =( N_trees_plot/ plot_A_ha)*MoMok_A_ha,
         C_SP_share = (C_t_plot/plot_tot_C_t)*100, 
         N_SP_share = (N_t_plot/plot_tot_N_t)*100,
         N_trees_SP_share =  (N_trees_plot)/plot_tot_N_trees)





# ----- 2.5.4.2. grouped by Plot  ------------------------------------------------------------------
RG_P <- RG_total %>%
  # plot area of all sampling circuits together
  left_join(., RG_total %>%
              filter(compartiment == "total") %>% 
              group_by(plot_ID, CCS_nr) %>% 
              summarise(CCS_max_dist_m = mean(CCS_max_dist/100)) %>%  # 10000 to transform m2 into ha, the plot radius has to be the distance of furthest plant to the RG sampling circuit
              mutate(CCS_A_ha = c_A(CCS_max_dist_m)/10000) %>% 
              group_by(plot_ID) %>% 
              summarise(plot_A_ha = sum(CCS_A_ha)) %>% 
              mutate(MoMoK_A_ha = (50*50)/10000), 
            by = "plot_ID") %>% 
  # summing up biomass, carbon and nitrogen per speices, plot and compartiment 
  group_by(plot_ID, compartiment) %>%                            # group by plot and species to calculate BA per species 
  summarise(B_t_plot = sum(B_kg/1000), 
            C_t_plot = sum(C_t),                           # sum of  carbon stock per plot,  species and compartiment
            N_t_plot = sum(na.omit(N_kg/1000)),      # sum of aboveground Nitrogen stock per plot and species and compartiment
            plot_A_ha = mean(plot_A_ha)) %>% # plot area
  mutate(MoMok_A_ha = (50*50)/10000,                 # momok area 0.25 ha 
         # B, C, N hectar values
         B_t_ha = B_t_plot/plot_A_ha, 
         C_t_ha = C_t_plot/plot_A_ha, 
         N_t_ha = N_t_plot/plot_A_ha,
         # B, C, N  MoMoK area values 
         B_t_MA = (B_t_plot/plot_A_ha)*MoMok_A_ha,
         C_t_MA = (C_t_plot/plot_A_ha)*MoMok_A_ha,
         N_t_MA = (N_t_plot/plot_A_ha)*MoMok_A_ha) %>% 
  # number of species per plot
  left_join(., RG_total %>% 
              filter(compartiment == "total") %>% 
              select(plot_ID, SP_code) %>%
              group_by(plot_ID) %>%
              distinct() %>% 
              summarise(N_species_plot = n()) %>% 
              mutate(compartiment = "total"),
            by = c("plot_ID", "compartiment")) %>% 
  left_join(., RG_total %>% 
              filter(compartiment == "total") %>% 
              group_by(plot_ID) %>% 
              summarise(Nt_plot = n())%>% 
              mutate(compartiment = "total"), 
            by = c("plot_ID", "compartiment")) %>% 
  mutate(Nt_ha = Nt_plot/plot_A_ha, 
         Nt_MA = (Nt_plot/plot_A_ha)*MoMok_A_ha)






summary(RG_P)

# ----- 2.5.5.JOINT PLOTWISE: living trees, deadwood, regeneration  -------

plot_total <- rbind(
# living trees
 trees_P %>% 
   dplyr::select(plot_ID,compartiment,
                 B_t_plot, C_t_plot, N_t_plot, Nt_plot,    # per plot
                 B_t_MA, C_t_MA, N_t_MA, Nt_MA,            # per momok area 50X50m
                 B_t_ha, C_t_ha, N_t_ha, Nt_ha) %>%        # per hectare
  mutate(stand_component = "LT") %>% 
   dplyr::select(plot_ID,compartiment, stand_component,
                 B_t_plot, C_t_plot, N_t_plot, Nt_plot,    # per plot
                 B_t_MA, C_t_MA, N_t_MA, Nt_MA,            # per momok area 50X50m
                 B_t_ha, C_t_ha, N_t_ha, Nt_ha),           # per hectare
# regeneration trees
 RG_P %>%  
  dplyr::select(plot_ID, compartiment,
                B_t_plot, C_t_plot, N_t_plot, Nt_plot,    # per plot
                B_t_MA, C_t_MA, N_t_MA, Nt_MA,            # per momok area 50X50m
                B_t_ha, C_t_ha, N_t_ha, Nt_ha) %>%        # per hectare
  mutate(stand_component = "RG") %>% 
  dplyr::select(plot_ID, compartiment, stand_component,
                B_t_plot, C_t_plot, N_t_plot, Nt_plot,    # per plot
                B_t_MA, C_t_MA, N_t_MA, Nt_MA,            # per momok area 50X50m
                B_t_ha, C_t_ha, N_t_ha, Nt_ha),           # per hectar                    
# deadwood
 DW_P %>% 
  mutate(compartiment = "ag") %>% 
  dplyr::select(plot_ID, compartiment,
                B_t_plot, C_t_plot, N_t_plot, Nt_plot,    # per plot
                B_t_MA, C_t_MA, N_t_MA, Nt_MA,            # per momok area 50X50m
                B_t_ha, C_t_ha, N_t_ha, Nt_ha) %>%        # per hectare
  mutate(stand_component = "DW") %>% 
  dplyr::select(plot_ID, compartiment, stand_component,
                B_t_plot, C_t_plot, N_t_plot, Nt_plot,    # per plot
                B_t_MA, C_t_MA, N_t_MA, Nt_MA,            # per momok area 50X50m
                B_t_ha, C_t_ha, N_t_ha, Nt_ha))           # per hectar              

summary(plot_total)

write.csv(plot_total, "output/out_data/LB_RG_DW_Plot_MoMoK.csv")




# ----- 4. PLAUSIBILTY  --------------------------------------------------------

# ----- 4.1. LIVING TREES PLAUSIBILITY ------------------------------------
# ----- 4.1.1. HD value living trees plausibility test ------------------------------------
# via HD: 
# Vorbedingung: Die Bäume stehen im Hauptbestand.
# HD_Warnung 5,0-65 und 85-139,9
# HD_Fehler: 0-4,9 und >140,0

# trees with HD status "warning" and sampled height
trees_tot_piv_wider %>% 
  filter(C_layer == 1) %>% 
  mutate(HD_status = case_when(HD_value >= 5 & HD_value <= 65 | HD_value >= 85  & HD_value <= 139.9 ~ "WARNING", 
                               HD_value <= 4.9 | HD_value > 140 ~ "ERROR", 
                               TRUE ~ "FINE")) %>% 
  filter(HD_status == "WARNING" & H_method == "sampled")

# trees with HD status "error" and sampled height
trees_tot_piv_wider %>% 
  filter(C_layer == 1) %>% 
  mutate(HD_status = case_when(HD_value >= 5 & HD_value <= 65 | HD_value >= 85  & HD_value <= 139.9 ~ "WARNING", 
                               HD_value <= 4.9 | HD_value > 140 ~ "ERROR", 
                               TRUE ~ "FINE")) %>% 
  filter(HD_status == "ERROR" & H_method == "sampled")

summary(trees_tot_piv_wider %>% 
          filter(C_layer == 1) %>% 
          mutate(HD_status = case_when(HD_value >= 5 & HD_value <= 65 | HD_value >= 85  & HD_value <= 139.9 ~ "WARNING", 
                                       HD_value <= 4.9 | HD_value > 140 ~ "ERROR", 
                                       TRUE ~ "FINE")) %>% 
          filter(HD_status == "ERROR" & H_method == "sampled"))


# comparisson of HD between measured and tapeS generated heights
trees_total_5 %>% 
  filter(compartiment == "ag") %>%
  mutate(H_tapes = estHeight(d13 = (DBH_cm), sp = tpS_ID)) %>% 
  mutate(HD_tps = (H_tapes*100)/DBH_cm, 
         HD_diff = HD_value - HD_tps, 
         HD_status = case_when(HD_value >= 5 & HD_value <= 65 | HD_value >= 85  & HD_value <= 139.9 ~ "WARNING", 
                               HD_value <= 4.9 | HD_value > 140 ~ "ERROR", 
                               TRUE ~ "FINE"), 
         HD_tps_status = case_when(HD_tps >= 5 & HD_tps <= 65 | HD_tps >= 85  & HD_tps <= 139.9 ~ "WARNING", 
                                   HD_tps <= 4.9 | HD_tps > 140 ~ "ERROR", 
                                   TRUE ~ "FINE")) %>% 
  select(plot_ID, t_ID, SP_code, DBH_cm, H_method, H_m, H_tapes, HD_value, HD_tps, HD_diff, HD_status, HD_tps_status) #%>% 
  #filter(HD_tps_status == "WARNING" & H_method == "sampled") # 174
  # filter(HD_status == "WARNING" & H_method == "sampled") # 175 
  # filter(HD_status == "ERROR" & H_method == "sampled") # 5
  # filter(HD_tps_status == "ERROR" & H_method == "sampled") # 0



summary(trees_total_5 %>% 
  filter(compartiment == "ag") %>%
  mutate(H_tapes = estHeight(d13 = (DBH_cm), sp = tpS_ID)) %>% 
  mutate(HD_tps = (H_tapes*100)/DBH_cm, 
         HD_diff = HD_value - HD_tps))




# ----- 4.1.2. carbon BWI comparisson living trees plausibility test ------------------------------------
# ----- 4.1.2.1. check if calcualtion method matters by comparing GHG, pure tapeS and tapeS+modelled heights ------------------------------------
# by species by plot and age class
# --> won´t work because we need pseudo monocultures
c_comp_Momok_BWI_SP_A <- trees_total_5 %>%
  filter(compartiment=="ag") %>% 
  mutate(B_kg_tapes = tprBiomass(tprTrees(spp = tpS_ID,                                         
                                          Dm = as.list(DBH_cm),
                                          Hm = as.list(DBH_h_m ),
                                          Ht = estHeight(d13 = (DBH_cm), sp = tpS_ID),
                                          inv = 4), component = "agb"),
         C_t_tps_H_tps = (B_kg_tapes/1000)*0.5) %>% 
  group_by(plot_ID, BWI_SP_group, age) %>% 
  summarise(C_t_tapes = sum(C_t_tapes),
            C_t_H_tps = sum(C_t_tps_H_tps),
            C_aB_t_GHG = sum(C_aB_t_GHG),
            plot_A_ha = mean(plot_A_ha),
            Nt = n()) %>% 
  mutate(C_t_ha_tapes= C_t_tapes/ plot_A_ha, 
         C_t_ha_tps_H= C_t_tapes/ plot_A_ha,
         C_t_ha_GHG = C_aB_t_GHG/ plot_A_ha,
         Nt_ha = Nt/ plot_A_ha,
         BWI_SP_group = toupper(BWI_SP_group), 
         age_class = case_when(!is.na(age) ~ cut(age,         # cut the diameter
                                                 breaks = c(seq(1, 180, by = 20), Inf),  # in sequences of 5
                                                 labels = labs_age,                        # and label it according to labs (1.4.1)
                                                 right = FALSE),
                               TRUE ~ 'all')) %>%
  group_by(BWI_SP_group, age_class) %>% 
  summarise(C_t_ha_tapes = mean(C_t_ha_tapes),
            C_t_ha_tps_H = mean(C_t_ha_tps_H),
            C_t_ha_GHG = mean(C_t_ha_GHG)) %>%  
  left_join(.,BWI_C_age_SP, 
            by = c("BWI_SP_group" ,"age_class")) %>% 
  mutate(C_diff_tps_BWI = C_t_ha_tapes-C_t_ha_BWI,
         C_diff_tpsH_BWI = C_t_ha_tps_H-C_t_ha_BWI,
         C_diff_GHG_BWI = C_t_ha_GHG-C_t_ha_BWI)
summary(c_comp_Momok_BWI_SP_A)


# by species per plot with german average 
# --> won´t work because we need pseudo monocultures
# --> is only relevant to compare the biomass methods and see if they produce similar differences to
c_comp_Momok_BWI_SP <- trees_total_5 %>%
  filter(compartiment=="ag") %>% 
  mutate(B_kg_tapes = tprBiomass(tprTrees(spp = tpS_ID,                                         
                                          Dm = as.list(DBH_cm),
                                          Hm = as.list(DBH_h_m ),
                                          Ht = estHeight(d13 = (DBH_cm), sp = tpS_ID),
                                          inv = 4), component = "agb"),
         C_t_tps_H_tps = (B_kg_tapes/1000)*0.5) %>% 
  group_by(plot_ID, BWI_SP_group) %>% 
  summarise(C_t_tapes = sum(C_t_tapes),
            C_t_H_tps = sum(C_t_tps_H_tps),
            C_aB_t_GHG = sum(C_aB_t_GHG),
            plot_A_ha = mean(plot_A_ha),
            Nt = n()) %>%  
  mutate(C_t_ha_tapes= C_t_tapes/ plot_A_ha, 
         C_t_ha_tps_H= C_t_tapes/ plot_A_ha,
         C_t_ha_GHG = C_aB_t_GHG/ plot_A_ha,
         Nt_ha = Nt/ plot_A_ha,
         BWI_SP_group = toupper(BWI_SP_group)) %>%
  group_by(BWI_SP_group) %>% 
  summarise(C_t_ha_tapes = mean(C_t_ha_tapes),
            C_t_ha_tps_H = mean(C_t_ha_tps_H),
            C_t_ha_GHG = mean(C_t_ha_GHG)) %>%
  left_join(.,BWI_C_age_SP %>% 
              filter(age_class == "all") %>% 
              select(BWI_SP_group, C_t_ha_BWI), 
            by = c("BWI_SP_group")) %>% 
  mutate(C_diff_tps_BWI = C_t_ha_tapes-C_t_ha_BWI,
         C_diff_tpsH_BWI = C_t_ha_tps_H-C_t_ha_BWI,
         C_diff_GHG_BWI = C_t_ha_GHG-C_t_ha_BWI)




# comparisson of carbon stock in t per hektar per plot with overall german average
# across all species, ages and plots
c_comp_Momok_BWI_all <- trees_total_5 %>% 
  filter(compartiment == "ag") %>%
  mutate(H_tapes = ifelse(H_method == "sampled", H_m, estHeight(d13 = (DBH_cm), sp = tpS_ID)),
         B_kg_tapes = tprBiomass(tprTrees(spp = tpS_ID,                                         
                                          Dm = as.list(DBH_cm),
                                          Hm = as.list(DBH_h_m ),
                                          Ht = H_tapes,
                                          inv = 4), component = "agb"), 
         C_t_tps_H_tps = (B_kg_tapes/1000)*0.5) %>% 
  group_by(plot_ID) %>% 
  summarise(C_t_tapes = sum(C_t_tapes), 
            C_t_tps_H = sum(C_t_tps_H_tps), 
            C_t_GHG = sum(C_aB_t_GHG),
            plot_A_ha = mean(plot_A_ha), 
            Nt = n()) %>% 
  mutate(C_t_ha_tapes = C_t_tapes/plot_A_ha, 
         C_t_ha_tps_H = C_t_tps_H/ plot_A_ha, 
         C_t_ha_GHG = C_t_GHG/ plot_A_ha, 
         Nt_ha = Nt/plot_A_ha,
         BWI_SP_group = "all", 
         age_class = "all") %>% 
  left_join(., trees_P %>% 
              filter(compartiment == "total") %>% 
              select(plot_ID, dom_SP, mean_DBH_cm, mean_H_m), 
            by = "plot_ID") %>% 
  left_join(., BWI_C_age_SP %>% 
              filter(BWI_SP_group == "all" & age_class == "all"), 
            by = c("BWI_SP_group" ,"age_class")) %>% 
  mutate(C_diff_tps_BWI = C_t_ha_tapes-C_t_ha_BWI,
         C_diff_tpsH_BWI = C_t_ha_tps_H-C_t_ha_BWI,
         C_diff_GHG_BWI = C_t_ha_GHG-C_t_ha_BWI) %>% 
  select(-c(C_t_tapes,C_t_tps_H,C_t_GHG, plot_A_ha, Nt))


summary(c_comp_Momok_BWI_P)

# comparing C by dom SP without age with respective C stock per species group from BWI
C_comp_domSP_BWI<- trees_P %>%                  
    filter(compartiment == "total") %>%
    select(-c(C_t_ha, compartiment)) %>% 
    distinct() %>% 
    # just use aboveground carbon stock to enable proper comparisson with BWI data which do not include total C stock
    left_join(., trees_P %>%  
                filter(compartiment == "ag") %>%
                select(plot_ID, compartiment, C_t_ha) %>% 
                distinct(), 
              by = "plot_ID") %>%
    # assign BWI SP names to the dominant species of each plot so we can join in BWI Speices specific data
    left_join(., SP_names_com_ID_tapeS %>%       
                select(Chr_code_ger, BWI_SP_group) %>% 
                mutate(Chr_code_ger = toupper(Chr_code_ger), 
                       BWI_SP_group = toupper(BWI_SP_group)), 
              by = c("dom_SP" = "Chr_code_ger")) %>% 
    left_join(., BWI_stand_char_SP %>%
                filter(compartiment == "ag") %>% 
                rename("Nt_ha_BWI" = "Nt_ha") %>% 
                rename("BA_m2ha_BWI" = "BA_m2_ha") %>% 
                select(BWI_SP_group, C_t_ha_BWI, Nt_ha_BWI, BA_m2ha_BWI, SD_C) %>% 
                distinct(), 
              by = "BWI_SP_group") %>% 
    left_join(., DW_V_com %>% 
                select(plot_ID, V_m3_ha , V_diff), 
              by= "plot_ID") %>% 
    left_join(., site_info %>% 
                dplyr::select(plot_ID, growth), 
              by = "plot_ID") %>% 
    mutate(C_diff = C_t_ha - C_t_ha_BWI,
           Nt_diff = Nt_ha - Nt_ha_BWI, 
           BA_diff = BA_m2ha - BA_m2ha_BWI, 
           dom_SP = as.factor(dom_SP))




# ----- 4.1.3. comparission MoMoK vs. BWI C, N, BA by dom SP ---------------------------------------------
# select stand characteristics/ plot characteristics saved under compartiment "total"
cor.df <- trees_P %>% 
  filter(compartiment == "total") %>%
  # have to remove carbon because if i filter for "total" which includes all the stand characteristics and join it in under "ag" again
  select(-c(C_t_ha, compartiment)) %>% 
  distinct() %>% 
  # just use aboveground carbon stock to enable proper comparisson with BWI data which do not include total C stock
  left_join(., trees_P %>%  
              filter(compartiment == "ag") %>%
              select(plot_ID, compartiment, C_t_ha) %>% 
              distinct(), 
            by = "plot_ID") %>%
  # assign BWI SP names to the dominant species of each plot so we can join in BWI Speices specific data
  left_join(., SP_names_com_ID_tapeS %>%       
              select(Chr_code_ger, BWI_SP_group) %>% 
              mutate(Chr_code_ger = toupper(Chr_code_ger), 
                     BWI_SP_group = toupper(BWI_SP_group)), 
            by = c("dom_SP" = "Chr_code_ger")) %>% 
  left_join(., BWI_stand_char_SP %>%
              filter(compartiment == "ag") %>% 
              rename("Nt_ha_BWI" = "Nt_ha") %>% 
              rename("BA_m2ha_BWI" = "BA_m2_ha") %>% 
              select(BWI_SP_group, Nt_ha_BWI, C_t_ha_BWI, BA_m2ha_BWI) %>% 
              distinct(), 
            by = "BWI_SP_group") %>% 
  left_join(., DW_V_com %>% 
              select(plot_ID, V_m3_ha , V_diff), 
            by= "plot_ID") %>% 
  left_join(., site_info %>% 
              dplyr::select(plot_ID, growth), 
            by = "plot_ID") %>% 
  mutate(C_diff = C_t_ha - C_t_ha_BWI,
         Nt_diff = Nt_ha - Nt_ha_BWI, 
         BA_diff = BA_m2ha - BA_m2ha_BWI, 
         dom_SP = as.factor(dom_SP)) 

summary(cor.df)


# comparing stand characteristics by dominant species and age
C_comp_domSP_BWI_A <- trees_P %>%                  
  filter(compartiment == "total") %>%
  select(-c(C_t_ha, compartiment)) %>% 
  distinct()%>% 
  # join in age from trees total & create age classes
  left_join(., trees_total_5 %>% 
              select(plot_ID, age) %>% 
              distinct() %>% 
              # there are plots which have multiple ages. They are going to be averagedper plot to have one age class per plot
              group_by(plot_ID) %>% 
              summarise(age = mean(age)) %>% 
              mutate(age_class = case_when(!is.na(age) ~ cut(age,         # cut the diameter
                                                             breaks = c(seq(1, 180, by = 20), Inf),  # in sequences of 5
                                                             labels = labs_age,                        # and label it according to labs (1.4.1)
                                                             right = FALSE),
                                           TRUE ~ 'all')) %>% 
              select(-age) , 
            by = "plot_ID") %>% 
  # just use aboveground carbon stock to enable proper comparisson with BWI data which do not include total C stock
  left_join(., trees_P %>%  
              filter(compartiment == "ag") %>%
              select(plot_ID, compartiment, C_t_ha) %>% 
              distinct(), 
            by = "plot_ID") %>%
  # assign BWI SP names to the dominant species of each plot so we can join in BWI species-specific data
  left_join(., SP_names_com_ID_tapeS %>%       
              select(Chr_code_ger, BWI_SP_group) %>% 
              mutate(Chr_code_ger = toupper(Chr_code_ger), 
                     BWI_SP_group = toupper(BWI_SP_group)), 
            by = c("dom_SP" = "Chr_code_ger")) %>% 
  # join in BWI C stock per age class and species
  left_join(., BWI_C_age_SP %>% 
              select(BWI_SP_group, age_class, C_t_ha_BWI), 
            by = c("BWI_SP_group", "age_class")) %>% 
  # join in stand specific data (number of stems per ha, basal area m2ha) from the BWI 
  left_join(., BWI_stand_char_SP %>%
              filter(compartiment == "ag") %>%
              rename("Nt_ha_BWI" = "Nt_ha") %>% 
              rename("BA_m2ha_BWI" = "BA_m2_ha") %>% 
              select(BWI_SP_group, Nt_ha_BWI, BA_m2ha_BWI) %>% 
              distinct(), 
            by = "BWI_SP_group") %>% 
  # joining in Deadwood volume & deadwood volume differenc of MoMoK and BWI  
  left_join(., DW_V_com %>% 
              select(plot_ID, V_m3_ha , V_diff), 
            by= "plot_ID") %>% 
  # dataset with information about the site conditions& expected growth behavior
  left_join(., site_info %>% 
              dplyr::select(plot_ID, growth), 
            by = "plot_ID") %>% 
  mutate(C_diff = C_t_ha - C_t_ha_BWI,
         Nt_diff = Nt_ha - Nt_ha_BWI, 
         BA_diff = BA_m2ha - BA_m2ha_BWI, 
         dom_SP = as.factor(dom_SP))

summary(C_comp_domSP_BWI_A)




# comparing stand characterstics between pseudo monocultures of the respective mixed species 
# and BWI by pseudo mono species per plot 
comp_pseudo_mono <- trees_P_SP %>%   
  # dataset with trees_total stand characteristics apart from C stock
  filter(compartiment == "total") %>%
  select(-c(C_t_P_SP_ha, C_t_P_SP, compartiment)) %>% 
  distinct() %>% 
  # just use aboveground carbon stock to enable proper comparisson with BWI data which do not include total C stock
  left_join(., trees_P_SP %>%  
              filter(compartiment == "ag") %>%
              select(plot_ID, compartiment, SP_code, C_t_P_SP, C_t_P_SP_ha) %>% 
              distinct(), 
            by = c("plot_ID", "SP_code")) %>%
  # assign BWI SP names to the  species so we can join in BWI Speices specific data
  left_join(., SP_names_com_ID_tapeS %>%       
              select(Chr_code_ger, BWI_SP_group) %>% 
              mutate(Chr_code_ger = toupper(Chr_code_ger), 
                     BWI_SP_group = toupper(BWI_SP_group)), 
            by = c("SP_code" = "Chr_code_ger")) %>% 
  # join in age from trees total & create age classes
  left_join(., trees_total_5 %>% 
              select(plot_ID, age) %>% 
              distinct() %>% 
              # there are plots which have multiple ages. They are going to be averagedper plot to have one age class per plot
              group_by(plot_ID) %>% 
              summarise(age = mean(age)) %>% 
              mutate(age_class = case_when(!is.na(age) ~ cut(age,         # cut the diameter
                                                             breaks = c(seq(1, 180, by = 20), Inf),  # in sequences of 5
                                                             labels = labs_age,                        # and label it according to labs (1.4.1)
                                                             right = FALSE),
                                           TRUE ~ 'all')) %>% 
              select(-age) , 
            by = "plot_ID") %>% 
  # BWI stand characteristics dataset to compare N, C, BA by species group
  left_join(., BWI_stand_char_SP %>%
              filter(compartiment == "ag") %>% 
              rename("Nt_ha_BWI" = "Nt_ha") %>% 
              rename("BA_m2ha_BWI" = "BA_m2_ha") %>% 
              select(BWI_SP_group, Nt_ha_BWI, C_t_ha_BWI, SD_C, BA_m2ha_BWI) %>% 
              distinct(), 
            by = "BWI_SP_group") %>% 
  # dataset with deadwood volume
  left_join(., DW_V_com %>% 
              select(plot_ID, V_m3_ha , V_diff), 
            by= "plot_ID") %>% 
  # dataset with expected growth behavior at the respecitive plot, depending on dominant species and regeneration status of the peatland 
  left_join(., site_info %>% 
              dplyr::select(plot_ID, growth), 
            by = "plot_ID") %>%
  # create pseudo monocultures:
  # carbon stock of species per hectar divided by 1 Hektar mulitplied with the actual area covered by the species according to basal area 
  mutate(C_t_ha_mono = C_t_P_SP_ha/(1*(SP_BA_m2ha/tot_BA_m2ha)),
         #C_t_ha_P_mono =  C_t_P_SP/(plot_A_ha*(SP_BA_m2ha/tot_BA_m2ha)), 
         Nt_ha_mono = Nt_ha/(1*(SP_BA_m2ha/tot_BA_m2ha)), 
         BA_m2_ha_mono = SP_BA_m2ha/(1*SP_BA_m2ha/tot_BA_m2ha)) %>% 
  # calculate differences in carbon, basal area and number of stems
  mutate(C_diff = C_t_ha_mono - C_t_ha_BWI,
         Nt_diff = Nt_ha_mono - Nt_ha_BWI, 
         BA_diff = BA_m2_ha_mono - BA_m2ha_BWI, 
         dom_SP = as.factor(dom_SP), 
         SD_class = SD_class(SD_C, C_diff)) 





# ----- 4.2. DEAD TREES PLAUSIBILITY ------------------------------------
# ----- 4.2.1. DW volume üper hectar per plot compared with BWI DW volume (not separated by deadwood type) ------------------------------------
DW_V_C_com <- DW_P %>% 
  select(plot_ID, V_m3_ha, B_t_ha, C_t_ha) %>% 
  mutate(DW_type = "all") %>% 
  left_join(., trees_total %>% 
              select(plot_ID, state) %>% 
              distinct(), 
            by = "plot_ID") %>% 
  left_join(., BWI_DW_V %>%             # BWI 2012 Volumen pro Bundesland
              filter(DW_type == "all") %>% 
              select(state_abbreviation, V_m3_ha_BWI), 
            by = c("state" = "state_abbreviation")) %>% 
  left_join(., BWI_DW_C %>%             # BWI Kohlenstoff KI 2017
              filter(DW_type == "all") %>% 
              rename("BWI_C_t_ha" = "C_t_ha") %>% 
              select(DW_type, BWI_C_t_ha, SD_C), 
            by = "DW_type") %>% 
  mutate(V_diff = V_m3_ha - V_m3_ha_BWI,
         C_diff = C_t_ha - BWI_C_t_ha)

# export comparisson table
write.csv(DW_V_com, "output/out_data/V_comp_DW_BWI_MoMoK.csv")

# ----- 4.2.2. DW volume comparisson with BWI by DW tpye per plot and state ------------------------------------
# the comparisson via deadwood type resulted at first in great differences per deadwood type 
# this was, however, caused by the issue that the different reference areas per type were
# not facted in. To calculate the deadwood carbon or biomass per hectar in a comparable way,
# we have to divide it by the area that is actually occupied by this deadwood type
# which in our case we will use the volume share of each deadwood type to calculate the 
# area occupied by the respective type

DW_V_com_TY_ST <- DW_P_TY %>% 
  mutate(DW_type = as.character(DW_type), 
         V_pseu_mono = V_tot_m3_ha*(V_share/100)) %>% 
  select(plot_ID, DW_type, V_tot_m3_ha, V_pseu_mono) %>% 
  left_join(., trees_total %>% 
              select(plot_ID, state) %>% 
              distinct(), 
            by = "plot_ID") %>% 
  left_join(., BWI_DW_V %>% 
              #filter(DW_type == "all") %>% 
              mutate(DW_type = ifelse(DW_type != "1W", DW_type, "1")) %>% 
              select(state_abbreviation, DW_type, V_m3_ha_BWI), 
            by = c("state" = "state_abbreviation", "DW_type")) %>% 
  mutate(V_diff = V_tot_m3_ha - V_m3_ha_BWI, 
         V_diff_pseu_mono = V_pseu_mono - V_m3_ha_BWI)
summary(DW_V_com_TY_ST)


# ----- 4.2.3. DW volume & carbon comparisson with BWI by DW tpye per plot ------------------------------------
DW_C_V_comp <- DW_P_TY %>% 
  mutate(DW_type = as.character(DW_type), 
         V_pseu_mono = V_tot_m3_ha*(V_share/100), 
         C_pseu_mono = C_tot_t_ha*(C_share/100)) %>% 
  select(plot_ID, DW_type, C_tot_t_ha, V_tot_m3_ha, V_pseu_mono, C_pseu_mono) %>% 
  left_join(., BWI_DW_C %>% 
              mutate(DW_type = ifelse(DW_type != "1W", DW_type, "1")) %>% 
              group_by(DW_type) %>%  # here i am summarizing DW type "liegend stark ohne wurzlstock and liegend stark mit wurzelstock
              summarise(C_t_ha = mean(C_t_ha), 
                        B_t_ha = mean(B_t_ha), 
                        V_m3_ha = mean(V_m3_ha)) %>% 
              select(DW_type, V_m3_ha, C_t_ha), 
            by = "DW_type") %>% 
  mutate(V_diff = V_tot_m3_ha - V_m3_ha,
         C_diff = C_tot_t_ha - C_t_ha, 
         V_diff_pseu = V_pseu_mono - V_m3_ha,
         C_diff_pseu = C_pseu_mono - C_t_ha)

summary(DW_C_V_comp)

# -----2.5.2.8. deadwood comparisson by lying vs. standing plot ---------------------------------------------------

DW_comp_SL <- DW_total %>% 
  group_by(plot_ID, S_L_DW_type) %>% 
  summarise(#plot_A_ha = mean(CCS_A_ha),
    V_m3_SL = sum(na.omit(V_dw_m3)), 
    B_t_SL = sum(na.omit(B_dw_kg)/1000),
    # actually B_t_tapes_SL should be the main B_t_ha to calcuate with because it also includes the fine wood compartiment, 
    # which is not included in the transformation from volume to biomass performed by the BWI
    #B_t_tapes_SL = sum(na.omit(dw_ag_kg)/ 1000), 
    C_t_SL = sum(na.omit(C_dw_kg)/1000), 
    Nt_SL = n()) %>%
  # dataset with area per plot considreing calcutaling sum of the area pf all sampling circuits per plot(if there are multpiple sampling circuits per plot)
  left_join(., DW_total %>%
              select(plot_ID, CCS_nr) %>% 
              distinct() %>%
              mutate(CCS_A_ha = c_A(12.62)/10000) %>% 
              group_by(plot_ID) %>%
              summarize(plot_A_ha = sum(CCS_A_ha))%>% 
              mutate(MoMoK_A_ha = (50*50)/10000), 
            by = "plot_ID") %>% 
  # total volume, Biomass, and carbon per plot to calculate the V, B, and C chare
  left_join(., DW_total %>% 
              group_by(plot_ID) %>% 
              summarise(V_m3_tot_plot = sum(na.omit(V_dw_m3)/1000), 
                        B_t_tot_plot = sum(na.omit(B_dw_kg)/1000), 
                        C_t_tot_plot = sum(na.omit(C_dw_kg)/1000)), 
            by = "plot_ID") %>% 
  mutate(V_share = V_m3_SL/V_m3_tot_plot, 
         B_share = B_t_SL/B_t_tot_plot, 
         C_share = C_t_SL/C_t_tot_plot) %>% 
  mutate(V_m3_ha = V_m3_SL/plot_A_ha, 
         B_t_ha = B_t_SL/plot_A_ha, 
         #B_t_tapes_ha = B_t_tapes_plot/ plot_A_ha, 
         C_t_ha = C_t_SL/plot_A_ha, 
         #N_t_ha = N_t_plot/plot_A_ha, 
         Nt_ha = Nt_SL/plot_A_ha, 
         V_pseu_m3ha = V_m3_ha*V_share, 
         B_pseu_tha= B_t_ha*B_share, 
         C_pseu_tha = C_t_ha*C_share) %>% 
  # I have to join in the BWI cmparisson dataset by standing and lying dw type and
  left_join(., BWI_DW_C %>% 
              select(DW_type, C_t_ha, SD_C, V_m3_ha, SD_V) %>% 
              rename("C_t_ha_BWI" = "C_t_ha") %>% 
              rename("V_m3_ha_BWI" = "V_m3_ha"), 
            by = c("S_L_DW_type" = "DW_type")) %>% 
  # comparisson by pseudo monoculture of the S/L dw type
  mutate(V_pseu_diff = V_pseu_m3ha - V_m3_ha, 
         C_pseu_diff = C_pseu_tha - C_t_ha)
summary(DW_comp_SL)


# create biomass item class
DW_total  %>% 
  group_by(plot_ID, iB_class) %>% 
  summarise(N_iB_class = n(), 
            V_iB_class = sum(V_dw_m3)) %>% 
  left_join(., DW_total %>% 
              group_by(plot_ID) %>% 
              summarise(N_iB_class_plot = n(), 
                        V_iB_class_plot = sum(V_dw_m3)), 
            by = "plot_ID") %>% 
  mutate(iB_N_share = N_iB_class/N_iB_class_plot, 
         iB_V_share = V_iB_class/V_iB_class_plot)


# -----4.3.correlation between stand characteristics and C stocks -----------------------------------------------------------
# creating correlation matrix for every dominant species
for (SP in unique(cor.df$dom_SP)) {  # for each species in the cor.df dataframe
   # SP= "SER"
  cor.sp <- cor.df %>%       # select the variables that should be checked for correlation
    filter(dom_SP == SP) %>% 
    group_by(dom_SP) %>% 
    ungroup() %>% 
    select(#C_t_ha, 
           C_diff, 
           Nt_diff, 
           BA_diff, 
           Nt_ha, 
           V_m3_ha)

  print(ggcorrplot(cor.sp, 
                   #hc.order = TRUE, 
                   type = "lower",
                   lab = TRUE))
  print(SP)
  print(cor(cor.sp))
  print(summary(cor.sp))
}


# https://stackoverflow.com/questions/26202670/how-to-use-corrplot-with-simple-matrices 
  corrplot(cor(cor.sp), is.corr = FALSE, p.mat = testRes$p, method = 'circle', type = 'lower', insig='blank',
           order = 'AOE', diag = FALSE)$corrPos -> p1
  text(p1$x, p1$y, round(p1$corr, 2))

  


# ----- 4.4. Linear model C plausibility -------------------------------------------------------------
# 1. calualte differences in C, number of stem and BA differences between 
#    MoMoK and BWI
# 2. define threshold of difference 
#     L> "if the plots C stock exceedes or undergoes a range of .... have a closer look at it"
# 3. fit linear model including C, number of stem and BA differences between 
#    MoMoK and BWI 
# 4. filter plots that differ from the predicted C stock to a defined extend
# 5. check if the basal area differs significantly from BWI as well to "explain" the difference in C

# ----- 4.4.1. Linear model C plausibility by SP & plot -------------------------------------------------------------
# coefficents of non-linear height model per species and plot
#https://rdrr.io/cran/forestmangr/man/lm_table.html
# 1. dataset containing C, BA and Nt differences grouped by dominant species but not by age: 
cor.df <- cor.df %>% 
  # table with lm for C_diff = b0 + b1* BA_diff+ b2*Nt_diff
  left_join(.,cor.df %>% 
              # 3. fit linear model with 
              lm_table(C_diff ~ BA_diff + Nt_diff, "dom_SP", output = "table") %>% 
              select(dom_SP, b0, b1, b2, Rsqr) %>% 
              # replace NAs with 0 
              mutate(., across(c("b0","b1","b2", "Rsqr"), ~ replace(., is.na(.), 0))),
            by = "dom_SP") %>% 
  # 4. predict the expectable C_diff at the given basal area and tree number difference
  mutate(C_diff_pred = b0 + b1*BA_diff + b2*Nt_diff,  
         # 5. calculate difference between calculated values and predicted values 
         C_pred_vs_C_calc = C_diff - C_diff_pred) 
  
cor.df <- cor.df %>% 
    left_join(., cor.df %>% 
                group_by(dom_SP) %>%
                summarize(SD_SP_C_diff = sd(C_pred_vs_C_calc)),
              by = "dom_SP") %>% 
  mutate(SD_class = SD_class(SD_SP_C_diff, C_pred_vs_C_calc)) 

  

# ----- 4.4.2. Linear model C plausibility by pseudo mono stand and BWI group -------------------------------------------------------------
comp_pseudo_mono <- comp_pseudo_mono %>%
  # create linear model with BA diff and Nt diff per species group
  left_join(., comp_pseudo_mono %>% 
              lm_table(C_diff ~ BA_diff + Nt_diff, "BWI_SP_group", output = "table") %>% 
              select(BWI_SP_group, b0, b1, b2, Rsqr) %>% 
              # replace NAs with 0 
              mutate(., across(c("b0","b1","b2", "Rsqr"), ~ replace(., is.na(.), 0))),
            by = "BWI_SP_group") %>% 
  # predict the expectable C_diff at the given basal area and tree number difference
  mutate(C_diff_pred = b0 + b1*BA_diff + b2*Nt_diff,  
         # 5. calculate difference between calculated values and predicted values 
         C_pred_vs_C_calc = C_diff - C_diff_pred)
# calcualting SD of siffernce between calculated and predicted difference in C
comp_pseudo_mono <- comp_pseudo_mono %>%
  left_join(., comp_pseudo_mono %>%
              group_by(BWI_SP_group) %>% 
              summarize(SD_SP_C_diff = sd(C_pred_vs_C_calc)), 
            by = "BWI_SP_group") %>% 
  mutate(SD_class = SD_class(SD_SP_C_diff, C_pred_vs_C_calc)) 










# ----- 3. VISULAIZATION -------------------------------------------------
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
# creating dataset: joining coefficients & curtis function
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
# visulaization:(nls-SP-P, nls-SP, sloboda, curtis, sampled)
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



# (nls-SP-P, nls-SP, sampled)
# craeting adatset estimated and samples heights vs. diameter by species, plot ad height method
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

# visualization (nls-SP-P, nls-SP, sampled)
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






# ----- 3.1.5.4. heihgts with tapes all height methody compared -----------------

# preparing dataset with all height methods 
trees_total %>%
  unite(SP_P_ID, plot_ID, SP_code, sep = "", remove = FALSE) %>% 
  left_join(.,coeff_H_SP_P %>% 
              select(plot_ID, SP_code, R2, b0, b1, b2), 
            by = c("plot_ID", "SP_code")) %>% 
  # if R2 or the coefficients are NA use the respective columns of the more general model
  left_join(., coeff_H_SP %>% select(SP_code, R2, b0, b1, b2),
            by = "SP_code") %>% 
  left_join(., trees_total %>%                                  # this is creates a tree dataset with mean BHD, d_g, h_g per species per plot per canopy layer wich we need for SLOBODA 
              group_by(plot_ID, C_layer, SP_code) %>%             # group by plot and species and canopy layer to calcualte dg, hg 
              summarise(H_g = sum(mean(na.omit(H_m))*BA_m2)/sum(BA_m2),    # Hoehe des Grundflächemittelstammes, calculation according to S. Schnell
                        mean_DBH_mm = mean(DBH_mm),               # mean diameter per species per canopy layer per plot
                        D_g = ((sqrt((mean(BA_m2)/pi)))*2)*100),   # Durchmesser des Grundflächenmittelstammes; *1000 to get from 1m -> 100cm -> 1000mm
            by = c("plot_ID", "SP_code", "C_layer")) %>%
  mutate(H_m_sampled = ifelse(is.na(H_m), NA, H_m), 
         nls_H_SP_P = ifelse(is.na(H_m), h_nls_SP_P(SP_P_ID, DBH_cm), NA), 
         #nls_H_SP_P_meth = ifelse(is.na(H_m), "h_nls_SP_P", "sampled"), 
         nls_H_SP = ifelse(is.na(H_m), h_nls_SP(SP_code, DBH_cm), NA), 
         #nls_H_SP_meth = ifelse(is.na(H_m), "h_nls_SP", "sampled"), 
         tapeS_H = ifelse(is.na(H_m), estHeight(DBH_cm, tpS_ID), NA), 
         #tapeS_H_meth = ifelse(is.na(H_m), "tapeS", "sampled"),
         sloboda_H = ifelse(is.na(H_m), ehk_sloboda(H_SP_group, DBH_mm, mean_DBH_mm, D_g, H_g), NA), 
         #sloboda_H_meth = ifelse(is.na(H_m), "Sloboda", "sampled"),
         curtis_H = ifelse(is.na(H_m) & !is.na(H_g), h_curtis(H_SP_group, DBH_mm), NA), 
         #curtis_H_meth = ifelse(is.na(H_m), "Curtis", "sampled")
         ) %>% 
  select(plot_ID, SP_code, DBH_cm, 
         H_m_sampled, 
         nls_H_SP_P, 
         #nls_H_SP_P_meth,
         nls_H_SP, 
        # nls_H_SP_meth,
         tapeS_H, 
         #tapeS_H_meth, 
         sloboda_H, 
         #sloboda_H_meth,
        #curtis_H_meth,
         curtis_H) %>% 
  tidyr::gather("method", "height", 4:9) %>% 
  ggplot(., aes(DBH_cm, height))+
  geom_point(aes(colour = method))+
  facet_wrap(plot_ID~SP_code)


# ---- 3.2. Biomass visualization ----------------------------------------
# ---- 3.2.1. foliage Biomass visualization ----------------------------------------
biotest %>% 
  select(plot_ID, SP_code,DBH_cm, WuWi_fB_kg, tapes_fB_kg, Vondr_fB_kg, GHG_tps_fB_kg)%>% # 
  tidyr::gather("method", "biomass", 4:7) %>% 
  ggplot(., aes(DBH_cm, biomass))+
  geom_point(aes(colour = method))+
  #geom_line(aes(colour = method))+
  #geom_smooth(method = "lm", se=FALSE, color="black")+
  facet_wrap(plot_ID~SP_code)

# biomass accumulated
biotest %>% 
  select( plot_ID, SP_code, DBH_cm, WuWi_fB_kg, tapes_fB_kg, Vondr_fB_kg, GHG_tps_fB_kg)%>% 
  tidyr::gather("method", "biomass", 4:7) %>% 
  ggplot(., aes(method, biomass))+
  geom_bar(aes(fill = method), 
           stat="identity", 
           position=position_dodge())+
  #geom_line(aes(colour = method))+
  #geom_smooth(method = "lm", se=FALSE, color="black")+
  facet_wrap(plot_ID~SP_code)

# mean biomas, sp, plot
biotest %>% 
  select( plot_ID, SP_code, DBH_cm, WuWi_fB_kg, tapes_fB_kg, Vondr_fB_kg, GHG_tps_fB_kg)%>% 
  tidyr::gather("method", "biomass", 4:7) %>% 
  group_by(method, plot_ID, SP_code) %>% 
  summarize(mean_bio = mean(biomass)) %>% 
  ggplot(., aes(method, mean_bio))+
  geom_bar(aes(fill = method), 
           stat="identity", 
           position=position_dodge())+
  #geom_line(aes(colour = method))+
  #geom_smooth(method = "lm", se=FALSE, color="black")+
  facet_wrap(plot_ID~SP_code)


# tapeS GHG comparissson
##point
biotest %>% 
  select(plot_ID, SP_code,DBH_cm, tapes_fB_kg, GHG_tps_fB_kg)%>% # 
  tidyr::gather("method", "biomass", 4:5) %>% 
  ggplot(., aes(DBH_cm, biomass))+
  geom_point(aes(colour = method))+
  #geom_line(aes(colour = method))+
  #geom_smooth(method = "lm", se=FALSE, color="black")+
  facet_wrap(plot_ID~SP_code)
##bar
biotest %>% 
  select(plot_ID, SP_code,DBH_cm, tapes_fB_kg, GHG_tps_fB_kg)%>% # 
  tidyr::gather("method", "biomass", 4:5) %>%  
  ggplot(., aes(method, biomass))+
  geom_bar(aes(fill = method), 
           stat="identity", 
           position=position_dodge())+
  #geom_line(aes(colour = method))+
  #geom_smooth(method = "lm", se=FALSE, color="black")+
  facet_wrap(plot_ID~SP_code)


# Missing values for Boradleafed trees result from the fact that there is only one funciton to calacute them
# missing values for coniferous trees result from the missing ages, wich Wirths formula for foliage biomass requires
summary(biotest)
biotest %>% filter(is.na(WuWi_fB_kg))
biotest %>% filter(is.na(tapes_fB_kg))

# ---- 3.2.2. branch Biomass visualization ----------------------------------------
# points
biotest %>% 
  select(plot_ID, SP_code, DBH_cm, WuWi_brB_kg, tapes_brB_kg, Vondr_brB_kg, tps_GHG_brB_kg, Vondr_GHG_brB_kg, GHG_tps_brB_kg)%>% 
  tidyr::gather("method", "biomass", 4:9) %>% 
  ggplot(., aes(DBH_cm, biomass))+
  geom_point(aes(colour = method))+
  #geom_line(aes(colour = method))+
  #geom_smooth(method = "lm", se=FALSE, color="black")+
  facet_wrap(plot_ID~SP_code)
# bar
biotest %>% 
  select(plot_ID, SP_code, DBH_cm, WuWi_brB_kg, tapes_brB_kg, Vondr_brB_kg, tps_GHG_brB_kg, Vondr_GHG_brB_kg)%>% 
  tidyr::gather("method", "biomass", 4:8) %>% 
  ggplot(., aes(method, biomass))+
  geom_bar(aes(fill = method), 
           stat="identity", 
           position=position_dodge())+
  #geom_line(aes(colour = method))+
  #geom_smooth(method = "lm", se=FALSE, color="black")+
  facet_wrap(plot_ID~SP_code)


# tapeS GHG comparissson
##point
biotest %>% 
  select(plot_ID, SP_code,DBH_cm, tapes_brB_kg, GHG_tps_brB_kg, tps_GHG_brB_kg )%>%    #branches calculated in tapeS and GHG stepwise
  tidyr::gather("method", "biomass", 4:6) %>% 
  ggplot(., aes(DBH_cm, biomass))+
  geom_point(aes(colour = method))+
  #geom_line(aes(colour = method))+
  #geom_smooth(method = "lm", se=FALSE, color="black")+
  facet_wrap(plot_ID~SP_code)
##bar
biotest %>% 
  select(plot_ID, SP_code,DBH_cm, tapes_brB_kg, GHG_tps_brB_kg, tps_GHG_brB_kg)%>% # 
  tidyr::gather("method", "biomass", 4:6) %>%  
  ggplot(., aes(method, biomass))+
  geom_bar(aes(fill = method), 
           stat="identity", 
           position=position_dodge())+
  #geom_line(aes(colour = method))+
  #geom_smooth(method = "lm", se=FALSE, color="black")+
  facet_wrap(plot_ID~SP_code)

# ---- 3.2.3. coarsewood with bark Biomass visualization ----------------------------------------
# points
biotest %>% 
  select(plot_ID, SP_code, DBH_cm, GHG_WuWi_StB_kg, tapes_crsWbB_kg, Vondr_crsWbB_kg, tps_GHG_crsWbB_kg, Vondr_GHG_crsWbB_kg)%>% 
  tidyr::gather("method", "biomass", 4:8) %>% 
  ggplot(., aes(DBH_cm, biomass))+
  geom_point(aes(colour = method))+
  geom_line(aes(colour = method))+
  #geom_smooth(method = "lm", se=FALSE, color="black")+
  facet_wrap(plot_ID~SP_code)
# bar
biotest %>% 
  select(plot_ID, SP_code, DBH_cm, GHG_WuWi_StB_kg, tapes_crsWbB_kg, Vondr_crsWbB_kg, tps_GHG_crsWbB_kg, Vondr_GHG_crsWbB_kg)%>% 
  tidyr::gather("method", "biomass", 4:8) %>% 
  ggplot(., aes(method, biomass))+
  geom_bar(aes(fill = method), 
           stat="identity", 
           position=position_dodge())+
  #geom_line(aes(colour = method))+
  #geom_smooth(method = "lm", se=FALSE, color="black")+
  facet_wrap(plot_ID~SP_code)

# ---- 3.2.4. aboveground Biomasse visualization ----------------------------------------
# points
biotest %>% 
  select(plot_ID, SP_code, DBH_cm, GHG_aB_kg, tapes_ab_kg, Vondr_oiB_kg)%>% 
  tidyr::gather("method", "biomass", 4:6) %>% 
  ggplot(., aes(DBH_cm, biomass))+
  geom_point(aes(colour = method))+
  geom_line(aes(colour = method))+
  #geom_smooth(method = "lm", se=FALSE, color="black")+
  facet_wrap(plot_ID~SP_code)
# bar
biotest %>% 
  select(plot_ID, SP_code, DBH_cm, GHG_aB_kg, tot_GHG_tps, tapes_ab_kg, Vondr_oiB_kg)%>% 
  tidyr::gather("method", "biomass", 4:7) %>% 
  ggplot(., aes(method, biomass))+
  geom_bar(aes(fill = method), 
           stat="identity", 
           position=position_dodge())+
  #geom_line(aes(colour = method))+
  #geom_smooth(method = "lm", se=FALSE, color="black")+
  facet_wrap(plot_ID~SP_code)


biotest %>% 
  select(plot_ID, SP_code, DBH_cm, GHG_aB_kg, tot_GHG_tps, tapes_ab_kg, Vondr_oiB_kg)%>% 
  tidyr::gather("method", "biomass", 4:7) %>% 
  group_by(method, plot_ID, SP_code) %>% 
  summarize(mean_bio = mean(biomass)) %>% 
  ggplot(., aes(method, mean_bio))+
  geom_bar(aes(fill = method), 
           stat="identity", 
           position=position_dodge())+
  #geom_line(aes(colour = method))+
  #geom_smooth(method = "lm", se=FALSE, color="black")+
  facet_wrap(plot_ID~SP_code)


# ---- 3.2.5. GHG & GHG_tapeS compartiments ----------------------------------------
# bar
biotest %>% 
  select(plot_ID, SP_code, DBH_cm, 
         GHG_aB_kg, tps_GHG_waB,tps_GHG_crsWbB_kg, tapes_DhB_kg, tapes_DhbB_kg, fwB_kg, tapes_fB_kg) %>% 
  tidyr::gather("method", "biomass", 4:10) %>% 
  ggplot(., aes(method, biomass))+
  geom_bar(aes(fill = method), 
           stat="identity", 
           position=position_dodge())+
  scale_fill_discrete(labels = c("total aboveground biomass GHGI", "woody aboveground biomass", 
                               "coarsewood with bark", "coarsewood without bark", "bark", 
                               "non coarse wood incl. bark", "foliage tapes" ))+
  #geom_line(aes(colour = method))+
  #geom_smooth(method = "lm", se=FALSE, color="black")+
  facet_wrap(plot_ID~SP_code)



# ---- 3.2.5. GHG & GHG_vondernach compartiments ----------------------------------------
# bar
biotest %>% 
  select(plot_ID, SP_code, DBH_cm, 
         GHG_aB_kg, Vondr_GHG_waB, Vondr_GHG_crsWbB_kg,  Vondr_DhB_kg, Vondr_DhRB_kg, Vondr_GHG_brB_kg, Vondr_fB_kg) %>% 
  tidyr::gather("method", "biomass", 4:10) %>% 
  ggplot(., aes(method, biomass))+
  geom_bar(aes(fill = method), 
           stat="identity", 
           position=position_dodge())+
  scale_fill_discrete(labels = c("total aboveground biomass GHGI", "woody aboveground biomass", 
                                 "coarsewood with bark", "coarsewood without bark", "bark", 
                                 "non coarse wood incl. bark", "foliage vondernach" ))+
  #geom_line(aes(colour = method))+
  #geom_smooth(method = "lm", se=FALSE, color="black")+
  facet_wrap(plot_ID~SP_code)

# ---- 3.2.7. tapeS compartiments  ----------------------------------------
# tapes_ab_kg, tapes_DhB_kg, tapes_DhbB_kg, tapes_crsWbB_kg, tapes_fB_kg, tapes_brB_kg)%>% 
biotest %>% 
  select(plot_ID, SP_code, DBH_cm, 
         tapes_ab_kg, tapes_stwB_kg, tapes_stwbB_kg, tapes_DhB_kg, tapes_DhbB_kg, tapes_brB_kg,  tapes_fB_kg) %>% 
  tidyr::gather("method", "biomass", 4:10) %>% 
  ggplot(., aes(method, biomass))+
  geom_bar(aes(fill = method), 
           stat="identity", 
           position=position_dodge())+
  scale_fill_discrete(labels = c("total aboveground biomass", 
                                 "stumpwood with out bark", "stumpwood bark", 
                                 "coarsewood without bark", "bark", 
                                 "non coarse wood incl. bark", "foliage" ))+
  #geom_line(aes(colour = method))+
  #geom_smooth(method = "lm", se=FALSE, color="black")+
  facet_wrap(plot_ID~SP_code)


# ---- 3.2.7. GHG & tapeS compartiments with stepwise GHG & TapeS calcualtion  ----------------------------------------

# bar
 # all compartiments
biotest %>% 
  select(plot_ID, SP_code, DBH_cm, 
         #GHG_aB_kg, tapes_ab_kg, Vondr_oiB_kg,
         swB_kg, tapes_DhB_kg, Vondr_DhB_kg,
         stwB_kg,tapes_stwB_kg, 
         stwbB_kg, tapes_stwbB_kg,
         swbB_kg, tapes_DhbB_kg, Vondr_DhRB_kg,
         fwB_kg, tapes_brB_kg, Vondr_brB_kg,
         fB_kg, tapes_fB_kg, Vondr_fB_kg) %>% 
  tidyr::gather("method", "biomass", 4:19) %>% 
  mutate(gen_method = case_when(startsWith(method,'t') ~ "TapeS", 
                                startsWith(method, 'V')~ "Vondernach", 
                                TRUE~"stepwise GHGI"), 
         compartiment = case_when(#method %in% c("GHG_aB_kg", "tapes_ab_kg", "Vondr_oiB_kg") ~ "tot_aB",
           method %in% c("stwB_kg", "tapes_stwB_kg") ~ "stump wood (>7cm DBH, below cut)",
           method %in% c("stwbB_kg", "tapes_stwbB_kg") ~ "stump wood bark (>7cm DBH, below cut)",
           method %in% c("swB_kg", "tapes_DhB_kg", "Vondr_DhB_kg") ~ "solid wood (>7cm DBH)",
           method %in% c( "swbB_kg", "tapes_DhbB_kg", "Vondr_DhRB_kg") ~ "solid wood bark",
           method %in% c("fwB_kg", "tapes_brB_kg", "Vondr_brB_kg") ~ "fine wood (<7cm DBH, inkl. bark)", 
           TRUE~"foliage")) %>%
  group_by(method, gen_method, compartiment, plot_ID, SP_code) %>% 
  summarize(mean_bio = mean(biomass)) %>%
  ggplot(., aes(gen_method, mean_bio))+
  #ggplot(., aes(gen_method, biomass))+
  #geom_bar(aes(fill = reorder(compartiment, -biomass)), #color= "white", 
  geom_bar(aes(fill = reorder(compartiment, +mean_bio)), color= "white", 
            stat="identity", 
           # https://r-graph-gallery.com/48-grouped-barplot-with-ggplot2.html
            position="stack")+
  scale_fill_viridis_d(name = "compartiments")+
  #geom_line(aes(colour = method))+
  #geom_smooth(method = "lm", se=FALSE, color="black")+
  facet_wrap(plot_ID~SP_code)+
  theme_bw()

# bar
# all compartiments
biotest %>% 
  select(plot_ID, SP_code, DBH_cm, 
         swB_kg, tapes_DhB_kg, Vondr_DhB_kg,
         stwB_kg,tapes_stwB_kg, 
         stwbB_kg, tapes_stwbB_kg,
         swbB_kg, tapes_DhbB_kg, Vondr_DhRB_kg,
         fwB_kg, tapes_brB_kg, Vondr_brB_kg,
         fB_kg, tapes_fB_kg, Vondr_fB_kg) %>% 
  tidyr::gather("method", "biomass", 4:19) %>% 
  mutate(gen_method = case_when(startsWith(method,'t') ~ "TapeS", 
                                startsWith(method, 'V')~ "Vondernach", 
                                TRUE~"GHGI"), 
         compartiment = case_when(#method %in% c("GHG_aB_kg", "tapes_ab_kg", "Vondr_oiB_kg") ~ "tot_aB",
           method %in% c("stwB_kg", "tapes_stwB_kg") ~ "stump wood (>7cm DBH, below cut)",
           method %in% c("stwbB_kg", "tapes_stwbB_kg") ~ "stump wood bark (>7cm DBH, below cut)",
           method %in% c("swB_kg", "tapes_DhB_kg", "Vondr_DhB_kg") ~ "solid wood (>7cm DBH)",
           method %in% c( "swbB_kg", "tapes_DhbB_kg", "Vondr_DhRB_kg") ~ "solid wood bark",
           method %in% c("fwB_kg", "tapes_brB_kg", "Vondr_brB_kg") ~ "fine wood (<7cm DBH, inkl. bark)", 
           TRUE~"foliage")) %>%
  group_by(plot_ID, SP_code, gen_method, compartiment, biomass) %>% 
  summarize(mean_bio = mean(biomass)) %>%
  ggplot(., aes(gen_method, mean_bio))+
  #ggplot(., aes(gen_method, biomass))+
  #geom_bar(aes(fill = reorder(compartiment, -biomass)), #color= "white", 
  #scale_x_discrete(name = "compartiment")+
  geom_bar(aes(fill = reorder(compartiment, + mean_bio)),
           stat="identity", 
           # https://r-graph-gallery.com/48-grouped-barplot-with-ggplot2.html
           position="dodge")+
  #scale_fill_discrete()+
  scale_fill_viridis_d(name = "Compartiment")+
  #geom_line(aes(colour = method))+
  #geom_smooth(method = "lm", se=FALSE, color="black")+
  facet_wrap(plot_ID~SP_code)+
  theme_bw()


# bar
# foliage
biotest %>% 
  select(plot_ID, SP_code, DBH_cm,  
         GHG_tps_fB_kg, tapes_fB_kg) %>% 
  tidyr::gather("method", "biomass", 4:5) %>% 
  ggplot(., aes(method, biomass))+
  geom_bar(aes(fill = method), 
           stat="identity", 
           position=position_dodge())+
  scale_fill_viridis_discrete(labels = c("foliage GHG", "foliage tapes"))+
  #geom_line(aes(colour = method))+
  #geom_smooth(method = "lm", se=FALSE, color="black")+
  facet_wrap(plot_ID~SP_code)+
  theme_bw()

# bar
# branches
biotest %>% 
  select(plot_ID, SP_code, DBH_cm,  
         GHG_tps_brB_kg, tapes_brB_kg, tps_GHG_brB_kg) %>% 
  tidyr::gather("method", "biomass", 4:6) %>% 
  ggplot(., aes(method, biomass))+
  geom_bar(aes(fill = method), 
           stat="identity", 
           position=position_dodge())+
  scale_fill_discrete(labels = c("non coarse wood including brak GHG stepwise", "non coarse wood including bark tapes", "non coarse wood including bark GHG_tapes" ))+
  #geom_line(aes(colour = method))+
  #geom_smooth(method = "lm", se=FALSE, color="black")+
  facet_wrap(plot_ID~SP_code)



# bar
# coarse wood
biotest %>% 
  select(plot_ID, SP_code, DBH_cm,  
         GHG_tps_DhB_kg, tapes_DhB_kg) %>% 
  tidyr::gather("method", "biomass", 4:5) %>% 
  ggplot(., aes(method, biomass))+
  geom_bar(aes(fill = method), 
           stat="identity", 
           position=position_dodge())+
  scale_fill_discrete(labels = c(" coarse wood without bark GHG stepwise", "coarse wood without bark tapes" ))+
  #geom_line(aes(colour = method))+
  #geom_smooth(method = "lm", se=FALSE, color="black")+
  facet_wrap(plot_ID~SP_code)


# bar
# coarse wood bark
biotest %>% 
  select(plot_ID, SP_code, DBH_cm,  
         GHG_tps_DhRB_kg, tapes_DhbB_kg) %>% 
  tidyr::gather("method", "biomass", 4:5) %>% 
  ggplot(., aes(method, biomass))+
  geom_bar(aes(fill = method), 
           stat="identity", 
           position=position_dodge())+
  scale_fill_discrete(labels = c(" coarse woodbark GHG stepwise", "coarse wood bark tapes" ))+
  #geom_line(aes(colour = method))+
  #geom_smooth(method = "lm", se=FALSE, color="black")+
  facet_wrap(plot_ID~SP_code)


# bar
# total aboveground biomass
biotest %>% 
  select(plot_ID, SP_code, DBH_cm,  
         GHG_aB_kg, tot_GHG_tps, tapes_ab_kg) %>% 
  tidyr::gather("method", "biomass", 4:6) %>% 
  ggplot(., aes(method, biomass))+
  geom_bar(aes(fill = method), 
           stat="identity", 
           position=position_dodge())+
  scale_fill_discrete(labels = c(" total aboveground biomass GHG", " total aboveground biomass  tapes" , " total aboveground biomass GHG_tapeS stepwise"))+
  #geom_line(aes(colour = method))+
  #geom_smooth(method = "lm", se=FALSE, color="black")+
  facet_wrap(plot_ID~SP_code)




# points
biotest %>% 
  select(plot_ID, SP_code, DBH_cm, 
         GHG_aB_kg, tapes_ab_kg,
         GHG_tps_DhB_kg, tapes_DhB_kg, 
         GHG_tps_DhRB_kg, tapes_DhbB_kg,  
         GHG_tps_brB_kg, tapes_brB_kg, 
         GHG_tps_fB_kg, tapes_fB_kg) %>% 
  tidyr::gather("method", "biomass", 4:13) %>% 
  ggplot(., aes(DBH_cm, biomass))+
  geom_point(aes(colour = method))+
  geom_line(aes(colour = method))+
  scale_colour_discrete(labels = c("total aboveground biomass GHGI","total aboveground biomass TapeS", 
                                 "coarsewood without bark GHGI", "coarsewood without bark TapeS", 
                                 "bark GHGI", "bark TapeS", 
                                 "non coarse wood incl. bark GHG", "non coarse wood incl. bark TapeS", 
                                 "foliage GHG", "foliage tapes"))+
  #geom_line(aes(colour = method))+
  #geom_smooth(method = "lm", se=FALSE, color="black")+
  facet_wrap(plot_ID~SP_code)


# boxplot
biotest %>% 
  select(plot_ID, SP_code, DBH_cm, 
         GHG_aB_kg, tapes_ab_kg,
         GHG_tps_stwB_kg, tapes_stwB_kg, 
         GHG_tps_stwbB_kg, tapes_stwbB_kg,
         GHG_tps_DhB_kg, tapes_DhB_kg, 
         GHG_tps_DhRB_kg, tapes_DhbB_kg,  
         GHG_tps_brB_kg, tapes_brB_kg, 
         GHG_tps_fB_kg, tapes_fB_kg) %>% 
  tidyr::gather("method", "biomass", 4:17) %>% 
  ggplot(., aes(method, biomass))+
  geom_boxplot(aes(colour = method))+
  #geom_line(aes(colour = method))+
  scale_colour_discrete(labels = c("total aboveground biomass GHGI","total aboveground biomass TapeS", 
                                   "stump wood without bark GHGI", "stump wood  without bark TapeS", 
                                   "stump wood bark GHGI", "stump wood bark TapeS", 
                                   "coarsewood without bark GHGI", "coarsewood without bark TapeS", 
                                   "bark GHGI", "bark TapeS", 
                                   "non coarse wood incl. bark GHG", "non coarse wood incl. bark TapeS", 
                                   "foliage GHG", "foliage tapes"))+
  #geom_line(aes(colour = method))+
  #geom_smooth(method = "lm", se=FALSE, color="black")+
  facet_wrap(plot_ID~SP_code)


# ---- 3.2.8. Vondernach compartiments  ----------------------------------------
biotest %>% 
  select(plot_ID, SP_code, DBH_cm, 
         Vondr_oiB_kg, Vondr_crsWbB_kg, Vondr_DhB_kg, Vondr_DhRB_kg, Vondr_brB_kg, Vondr_fB_kg) %>% 
  tidyr::gather("method", "biomass", 4:9) %>% 
  ggplot(., aes(method, biomass))+
  geom_bar(aes(fill = method), 
           stat="identity", 
           position=position_dodge())+
  scale_fill_discrete(labels = c("woody aboveground biomass", 
                                 "coarsewood with bark", "coarsewood without bark", "bark", 
                                 "non coarse wood incl. bark", "foliage" ))+
  #geom_line(aes(colour = method))+
  #geom_smooth(method = "lm", se=FALSE, color="black")+
  facet_wrap(plot_ID~SP_code)



# ---- 3.2.9. differences biomass GHGI, TapeS, Vondernach compartiments  ----------------------------------------
# boxplot
biotest %>% 
  select(plot_ID, SP_code, DBH_cm, 
        # diff_GHG_bef_af_tps, 
         diff_GHG_tps, 
         diff_Vondr_GHG, 
         diff_Vondr_tps) %>% 
  tidyr::gather("method", "biomass", 4:6) %>% 
  ggplot(., aes(method, biomass))+
  geom_boxplot(aes(colour = method))+
  #geom_line(aes(colour = method))+
  xlab(" ")+
  scale_colour_discrete(labels = c(#"difference abovegrond biomass GHG stepwise - GHG", 
                                   "difference abovegrond biomass GHG - TapeS", 
                                   "difference abovegrond biomass GHG - Vondernach", 
                                   "difference abovegrond biomass TapeS - Vondernach"))+
  #ggtitle("Boxplots of the differences in total aboveground biomass [kg] by calculation method")+
  #geom_line(aes(colour = method))+
  #geom_smooth(method = "lm", se=FALSE, color="black")+
  facet_wrap(plot_ID~SP_code)+ 
  theme(legend.position="right", axis.text.x=element_blank(), axis.ticks.x=element_blank())+
  theme_bw()



# -----3.3.1. Visualization REGENERATION Biomass comparisson --------------

RG_total.test <- RG_total %>% 
  # assigning numeric diameters to size classes through mean per class 
  mutate(D_cm = case_when(D_class_cm == 0 ~ 0, 
                          D_class_cm == 1 ~ (4.9+0)/2, 
                          D_class_cm == 2 ~ (5.9+5)/2,
                          TRUE ~ (6.9+6)/2)) %>% 
  # biomass
  # Biomass according to Annighoefer
  # estimating Root collar diameter via Annighoefer
  mutate(Annig_RCD_mm = ifelse(H_cm >= 130, annighoefer_RCD(D_cm, LH_NH, "130"), NA)) %>% # 130 = height at which the diameter was measured [cm]
  # Annighoefer biomass: if there is a DBH: Function including RCD, if H < 1.3m (so no diameter taken) use the Annighoefer equation that relies on height only
  mutate(Annig_aB_kg = ifelse(H_cm >= 130, annighoefer_rg_aB_H1.3_DBHb10(Annig_RCD_mm, H_cm, Annig_SP_group), annighoefer_rg_aB_bH1.3(H_cm, Annig_SP_group)),
         # GHGI biomass: if there is a DBH: function for trees above 1.3m but below 10cm DBH, for trees below 1.3m formula that relies on height
         RG_GHG_aB_kg = ifelse(H_cm >= 130, Dunger_aB_H1.3_DBHb10(Bio_SP_group, D_cm), Dunger_aB_Hb1.3(LH_NH, H_cm/100)), 
         RG_GHG_bB_kg = ifelse (H_cm >= 130, Dunger_bB(Bio_SP_group, D_cm), 0)) %>% 
  # belated compartitioning via Poorter
  mutate(#Poorter_swB_kg = Poorter_rg_RSR(RG_GHG_bB_kg, LH_NH),           # root to leaf ratio
    Poorter_fB_kg = Poorter_rg_RSR(RG_GHG_bB_kg, LH_NH))%>%       # root to shoot ratio
  mutate(GHG_Poorter_stem_kg = ifelse(LH_NH == "NH", RG_GHG_aB_kg-Poorter_fB_kg, RG_GHG_aB_kg), 
         Annig_Poorter_stem_kg = ifelse(LH_NH == "NH", Annig_aB_kg-Poorter_fB_kg, Annig_aB_kg)) %>% 
  mutate(diff_GHG_Annig = RG_GHG_aB_kg - Annig_aB_kg)
# Nitrogen content
#mutate(N_fw)





summary(RG_total)


RG_total %>% 
  select(plot_ID, LH_NH, D_cm, Annig_aB_kg, RG_GHG_aB_kg)%>% 
  tidyr::gather("method", "biomass", 4:5) %>% 
  ggplot(., aes(D_cm, biomass, colour = method))+
  geom_point(aes(colour = method))+
  # geom_line(aes(colour = method))+
  geom_smooth(method = "loess", se=TRUE)+
  facet_wrap(~LH_NH)



RG_total %>% 
  select(plot_ID, LH_NH, H_cm, Annig_aB_kg, RG_GHG_aB_kg)%>% 
  tidyr::gather("method", "biomass", 4:5) %>% 
  ggplot(., aes(H_cm, biomass, colour = method))+
  geom_point(aes(colour = method))+
  # geom_line(aes(colour = method))+
  geom_smooth(method = "loess", se=TRUE)+
  facet_wrap(~LH_NH)

RG_total %>% 
  select(plot_ID, LH_NH, D_cm, Annig_aB_kg, RG_GHG_aB_kg)%>% 
  tidyr::gather("method", "biomass", 4:5) %>% 
  ggplot(., aes(method, biomass))+
  geom_boxplot(aes(colour = method))+
  facet_wrap(~LH_NH)



# ----- 3.3.2. visualization plotwise --------------------------------------------

plot_total %>% 
  filter(compartiment == "ag") %>% 
  ggplot()+
  geom_bar(aes(x = stand_component, y = C_t_ha, fill = stand_component), 
           stat="identity", 
           position=position_dodge())+
  facet_wrap(~plot_ID)



plot_total %>% 
  ggplot(., aes(stand_component, C_tot_t_ha))+
  geom_boxplot(aes(colour = stand_component))+
  facet_wrap(~plot_ID)


# biomass in kg per plot in the stand component deadwood (DW), living trees (LT),  
plot_total %>% 
  filter(compartiment == "ag") %>%
  select(plot_ID, stand_component, B_t_plot) %>%
  mutate(B_kg_plot = B_t_plot*1000) %>%  
  ggplot(., aes(stand_component, B_kg_plot))+
  #ggplot(., aes(gen_method, biomass))+
  #geom_bar(aes(fill = reorder(compartiment, -biomass)), #color= "white", 
  #scale_x_discrete(name = "compartiment")+
  geom_bar(aes(fill = reorder(stand_component, + B_kg_plot)),
           stat="identity", 
           # https://r-graph-gallery.com/48-grouped-barplot-with-ggplot2.html
           position="dodge")+
  #scale_fill_discrete()+
  scale_fill_viridis_d(name = "stand component")+
  #geom_line(aes(colour = method))+
  #geom_smooth(method = "lm", se=FALSE, color="black")+
  facet_wrap(~plot_ID)+
  theme_bw()

# ----- 3.4.1. Visualization of plaubility tests & comparisson with BWI data -----------------

# ----- 3.4.1.1.  carbon overall MoMoK compared with bwi carbon, n --------
# mean carbon stock per hectar over all plots and species
plot_total %>% 
  filter(compartiment == "ag") %>%
  select(plot_ID, stand_component, C_t_ha) %>%
  mutate(BWI_C_t_ha = 108.00) %>% 
  left_join(., na.omit(trees_P %>% select(plot_ID, dom_SP) %>% distinct()), 
            by = "plot_ID") %>% 
  # mutate(B_kg_plot = B_t_plot*1000) %>%  
  ggplot(., aes(x= stand_component))+
  geom_bar(aes(y = C_t_ha, fill = reorder(stand_component, + C_t_ha)),
           stat="identity", 
           # https://r-graph-gallery.com/48-grouped-barplot-with-ggplot2.html
           position="dodge")+
  geom_line(aes(y = BWI_C_t_ha), size = 0.5, color="red", group = 3)+
  #scale_fill_discrete()+
  scale_fill_viridis_d(name = "stand component")+
  #geom_line(aes(colour = method))+
  #geom_smooth(method = "lm", se=FALSE, color="black")+
  facet_wrap(dom_SP~plot_ID)+
  theme_bw()


# mean carbon stock per hectar over all plots and species 
rbind(plot_total %>% 
        filter(compartiment == "ag") %>%
        select(plot_ID, stand_component, C_t_ha) %>%
        group_by(stand_component) %>%
        summarise(C_t_ha = mean(C_t_ha)),
      BWI_C_age_SP %>%
        filter(BWI_SP_group == "all" & age_class == "all") %>%
        mutate(stand_component = "BWI") %>% 
        rename("C_t_ha" = "C_t_ha_BWI") %>% 
        select(stand_component, C_t_ha)) %>% 
  #mutate(B_t_plot = B_t_plot*1000) %>%  
  ggplot(., aes(stand_component, C_t_ha))+
  #ggplot(., aes(gen_method, biomass))+
  #geom_bar(aes(fill = reorder(compartiment, -biomass)), #color= "white", 
  #scale_x_discrete(name = "compartiment")+
  geom_bar(aes(fill = reorder(stand_component, + C_t_ha)),
           stat="identity", 
           # https://r-graph-gallery.com/48-grouped-barplot-with-ggplot2.html
           position="dodge")+
  #scale_fill_discrete()+
  scale_fill_viridis_d(name = "stand component")+
  #geom_line(aes(colour = method))+
  #geom_smooth(method = "lm", se=FALSE, color="black")+
  #facet_wrap(~plot_ID)+
  theme_bw()



# ----- 3.4.1.2. difference carbon MoMoK vs. BWI grouped by dominant species  -----------------
# C COMPARISON BY DOMINANT SPECIES 
# difference in C_t_ha vs. difference in N_ha without age groups
ggplot(data = cor.df, 
       aes(x = Nt_diff, y = C_diff, color = dom_SP))+
  geom_point()+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept=0)+
  xlim(min(cor.df$Nt_diff), min(cor.df$Nt_diff)*(-1))+
  ylim(min(cor.df$C_diff), min(cor.df$C_diff)*(-1))+
  # geom_line()+
  # facet_wrap(SP_code~plot_ID)+
  xlab("difference N per hectar ") +
  ylab("difference C per hectar")+
  #ylim(0, 50)+
  ggtitle("C diff vs. N diff")+
  theme_light()+
  theme(legend.position = "right")

# difference in C_t_ha vs. difference in BA_m2_ha without age groups
ggplot(data = cor.df, 
       aes(x = BA_diff, y = C_diff, color = dom_SP))+
  geom_point()+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept=0)+
  xlim(max(cor.df$BA_diff)*(-1), max(cor.df$BA_diff))+
  ylim(max(cor.df$C_diff)*(-1), max(cor.df$C_diff))+
  # geom_line()+
  # facet_wrap(SP_code~plot_ID)+
  xlab("difference BA m2 per hectar ") +
  ylab("difference C t per hectar")+
  #ylim(0, 50)+
  ggtitle("C diff vs. BA diff")+
  theme_light()+
  theme(legend.position = "right")

# difference in C vs. difference in DW volume without age or DW groups
ggplot(data = cor.df, 
       aes(x = BA_diff, y = V_diff, color = dom_SP))+
  geom_point()+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept=0)+
  xlim(max(cor.df$V_diff)*(-1), max(cor.df$V_diff))+
  ylim(max(cor.df$C_diff)*(-1), max(cor.df$C_diff))+
  # geom_line()+
  # facet_wrap(SP_code~plot_ID)+
  xlab("difference DW volume m3 per hectar ") +
  ylab("difference C t per hectar")+
  #ylim(0, 50)+
  ggtitle("C diff vs. DW volume diff")+
  theme_light()+
  theme(legend.position = "right")


# difference in C_t_ha per dom_SP C_t_ha_BWI per species from BWI
ggplot(data = cor.df, 
       aes(x = C_t_ha_BWI, y = C_t_ha, color = dom_SP))+
  geom_point()+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept=0)+
  #xlim(max(cor.df$BA_diff)*(-1), max(cor.df$BA_diff))+
  #ylim(max(cor.df$C_diff)*(-1), max(cor.df$C_diff))+
  # geom_line()+
  # facet_wrap(SP_code~plot_ID)+
  xlab("C BWI ") +
  ylab("C MoMoK")+
  #ylim(0, 50)+
  ggtitle("C MoMoK vs. C BWI")+
  theme_light()+
  theme(legend.position = "right")


cor.df %>% 
  ggplot(aes(x = BA_diff))+
  geom_point(aes(y = C_diff, color = dom_SP))+
  geom_smooth(aes(y = C_diff_pred, method = "loess"))+
  #geom_line(aes(y = C_diff_pred, color = dom_SP))+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept=0)+
  xlim(max(cor.df$BA_diff)*(-1), max(cor.df$BA_diff))+
  ylim(max(cor.df$C_diff)*(-1), max(cor.df$C_diff))+
  # geom_line()+
  # facet_wrap(~dom_SP)+
  xlab("difference BA m2 per hectar ") +
  ylab("difference C t per hectar")+
  #ylim(0, 50)+
  ggtitle("C diff vs. BA diff")+
  theme_light()+
  theme(legend.position = "right")



# ----- 3.4.1.3. difference carbon MoMoK vs. BWI  by pseudo monocultures  -----------------
comp_pseudo_mono %>% 
  ggplot(., aes(x = BA_diff))+
  geom_point(aes(y = C_diff, color = BWI_SP_group))+
  geom_smooth(aes(y = C_diff_pred, method = "loess"))+
  #geom_line(aes(y = C_diff_pred, color = dom_SP))+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept=0)+
  xlim(max(cor.df$BA_diff)*(-1), max(cor.df$BA_diff))+
  ylim(max(cor.df$C_diff)*(-1), max(cor.df$C_diff))+
  # geom_line()+
  facet_wrap(~BWI_SP_group)+
  xlab("difference BA m2 per hectar ") +
  ylab("difference C t per hectar")+
  #ylim(0, 50)+
  ggtitle("C diff vs. BA diff")+
  theme_light()+
  theme(legend.position = "right")

comp_pseudo_mono %>% 
  ggplot(., aes(x = BA_diff))+
  geom_point(aes(y = C_diff, color = BWI_SP_group))+
  geom_smooth(aes(y = C_diff_pred, method = "loess"), se = FALSE)+
  #geom_line(aes(y = C_diff_pred, color = dom_SP))+
  geom_smooth(aes(y = (C_diff_pred+SD_SP_C_diff), method = "loess", color = "SD_class 1"), se = FALSE)+
  geom_smooth(aes(y = (C_diff_pred-SD_SP_C_diff), method = "loess", color = "SD_class 1"), se = FALSE)+
  geom_smooth(aes(y = (C_diff_pred+2*SD_SP_C_diff), method = "loess", color = "SD_class 2"), se = FALSE)+
  geom_smooth(aes(y = (C_diff_pred-2*SD_SP_C_diff), method = "loess", color = "SD_class 2"), se = FALSE)+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept=0)+
  xlim(max(cor.df$BA_diff)*(-1), max(cor.df$BA_diff))+
  ylim(max(cor.df$C_diff)*(-1), max(cor.df$C_diff))+
  # geom_line()+
  facet_wrap(~BWI_SP_group)+
  xlab("difference BA m2 per hectar ") +
  ylab("difference C t per hectar")+
  #ylim(0, 50)+
  ggtitle("C diff vs. BA diff")+
  theme_light()+
  theme(legend.position = "right")


# ----- 3.4.1.4. difference carbon MoMoK vs. BWI  by dominant species and age class  -----------------
# C COMPARISON BY DOMINANT SPECIES AND AGE CLASS
# difference in C_t_ha per dom_SP and age vs. difference in N_ha
ggplot(data = C_comp_domSP_BWI_A, 
       aes(x = Nt_diff, y = C_diff, color = dom_SP))+
  geom_point()+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept=0)+
  xlim(min(C_comp_domSP_BWI_A$Nt_diff), min(C_comp_domSP_BWI_A$Nt_diff)*(-1))+
  ylim(min(C_comp_domSP_BWI_A$C_diff), min(C_comp_domSP_BWI_A$C_diff)*(-1))+
  # geom_line()+
  # facet_wrap(SP_code~plot_ID)+
  xlab("difference N per hectar ") +
  ylab("difference C per hectar")+
  #ylim(0, 50)+
  ggtitle("C diff vs. N diff")+
  theme_light()+
  theme(legend.position = "right")

# difference in C_t_ha per dom_SP and age vs. difference in basal area in m2 per ha from BWI
ggplot(data = C_comp_domSP_BWI_A, 
       aes(x = BA_diff, y = C_diff, color = dom_SP))+
  geom_point()+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept=0)+
  #xlim(max(cor.df$BA_diff)*(-1), max(cor.df$BA_diff))+
  #ylim(max(cor.df$C_diff)*(-1), max(cor.df$C_diff))+
  # geom_line()+
  # facet_wrap(SP_code~plot_ID)+
  xlab("difference in BA in m2 per hectar ") +
  ylab("difference C t per hectar")+
  #ylim(0, 50)+
  ggtitle("C diff vs. BA volume diff")+
  theme_light()+
  theme(legend.position = "right")

# difference in C_t_ha per dom_SP and age vs. C_t_ha_BWI per species and ageclass from BWI
ggplot(data = C_comp_domSP_BWI_A, 
       aes(x = C_t_ha_BWI, y = C_t_ha, color = dom_SP))+
  geom_point()+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept=0)+
  #xlim(max(cor.df$BA_diff)*(-1), max(cor.df$BA_diff))+
  #ylim(max(cor.df$C_diff)*(-1), max(cor.df$C_diff))+
  # geom_line()+
  # facet_wrap(SP_code~plot_ID)+
  xlab("C BWI ") +
  ylab("C MoMoK")+
  #ylim(0, 50)+
  ggtitle("C MoMoK vs. C BWI")+
  theme_light()+
  theme(legend.position = "right")








# ----- 3.4.2. deadwood C & V comparisson to BWI  -----------------------

# ----- 3.4.2.1. Deadwood comparisson with BWI by deadwood type -----------
summary(DW_C_V_comp)

DW_C_V_comp %>% 
  group_by(DW_type) %>% 
  summarise(C_tot_t_ha=mean(na.omit(C_tot_t_ha)), 
            V_tot_m3_ha = mean(na.omit(V_tot_m3_ha)), 
            C_t_ha = mean(C_t_ha), 
            V_m3_ha = mean(V_m3_ha)) %>% 
  ggplot(., aes(x= DW_type))+
  geom_bar(aes(y = C_tot_t_ha, fill = DW_type),
           stat="identity", 
           # https://r-graph-gallery.com/48-grouped-barplot-with-ggplot2.html
           position="dodge")+
  geom_line(aes(y = C_t_ha), size = 0.5, color = "red", group = 3)+
  #scale_fill_discrete()+
  scale_fill_viridis_d(name = "deadwood type")+
  #geom_line(aes(colour = method))+
  #geom_smooth(method = "lm", se=FALSE, color="black")+
  #facet_wrap(~plot_ID)+
  theme_bw()




# Deadwood volume and carbon compared with BWI  and Kohlenstoffinventur data grouped by SD class and somiant species
DW_V_C_com %>% 
  left_join(., trees_P %>% 
  select(plot_ID, dom_SP), 
  by = "plot_ID") %>% 
  mutate(SD_class = SD_class(SD_C, C_diff)) %>% 
  select(plot_ID, dom_SP, V_m3_ha, V_m3_ha_BWI, C_t_ha, BWI_C_t_ha, SD_class)%>% 
  tidyr::gather("method", "carbon", 4:5) %>% 
  mutate(SD_class = ifelse(method == "BWI_C_t_ha", "1", SD_class)) %>% 
  ggplot() +
  geom_bar(aes(x = method, y = carbon, fill = SD_class),
           stat="identity", 
           # https://r-graph-gallery.com/48-grouped-barplot-with-ggplot2.html
           position="dodge")+
  #geom_line(aes(x = plot_ID, y = 3.11), size = 0.5, color = "red", group = 3)+
  facet_wrap(~dom_SP)+
  theme_bw()
  



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
# (46 Beobachtungen als fehlend geloescht)
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
# (14 Beobachtungen als fehlend geloescht)
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
# (45 Beobachtungen als fehlend geloescht)
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
 # (14 Beobachtungen als fehlend geloescht)
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

nls(H_m~a-b*exp(-c*DBH_cm),
    start=list(a=110,b=120,c=0.1),
    data = trees_total %>% filter(!is.na(H_m)))
H_m.test.1 <- 35.31713-35.51816*exp(-0.03048*trees_total$DBH_cm)
nls(H_m~ a*(1 - exp( -b * DBH_cm))^c ,
    start=list(a=110,b=120,c=0.1),
    data = trees_total %>% filter(!is.na(H_m)))
H_m.test.2 <- 35.31713 * (1 - exp( -35.51816 * trees_total$DBH_cm))^0.03048     
  
  
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





# ----- N. nitrogen aboveground formulas  ---------------------------------

ag_N_t_func.5 <- function(df, N, comp, p_t_ID) {
  ag_N_ptID = df$df[[N]][df$df[[comp]]== "f"] + 
    df$df[[N]][df$df[[comp]]== "fw" ] + 
    df$df[[N]][df$df[[comp]]== "sw"] + 
    df$df[[N]][df$df[[comp]]== "swb"] + 
    df$df[[N]][df$df[[comp]]== "stw"] + 
    df$df[[N]][df$df[[comp]]== "stwb"]
  return(ag_N_ptID)
}

ag_N_t_func.5(trees_total_5, trees_total_5$N_t, trees_total_5$compartiment, trees_total_5$ID_pt)  





i = 333001
p_t_ID = 333001

ag_N_t_func.3 <- function(df, p_t_ID, comp, N){
  ag_N <- as.numeric()
  for(i in unique(trees_total_5$ID_pt)){
    ag_N[[i]] <- trees_total_5$N_t[trees_total_5$compartiment == "f" & trees_total_5$ID_pt == i] +
      trees_total_5$N_t[trees_total_5$compartiment == "fw" & trees_total_5$ID_pt == i] + 
      trees_total_5$N_t[trees_total_5$compartiment == "sw" & trees_total_5$ID_pt == i] + 
      trees_total_5$N_t[trees_total_5$compartiment == "swb" & trees_total_5$ID_pt == i] + 
      trees_total_5$N_t[trees_total_5$compartiment == "stw" & trees_total_5$ID_pt == i] + 
      trees_total_5$N_t[trees_total_5$compartiment == "stwb" & trees_total_5$ID_pt == i]
  } 
  ag_N_t = ag_N
  return(ag_N_t[p_t_ID])
}

trees_total_5 %>%
  rowwise() %>%
  mutate(sum = sum(across(compartiment %in% ), na.rm = T))

view(trees_total_5 %>% mutate(N_t = ifelse(compartiment == "ag", ag_N_t_func.3(trees_total_5, plot_ID, t_ID, N_t), N_t)))


N_ag_func <- function(df, p_ID, t_ID, N) {
  N_t_ag <- trees_total_5 %>% 
    group_by(plot_ID, t_ID) %>%
    summarise(N_t = unlist(across(N_t, sum, na.rm = TRUE))) %>% 
    dplyr::pull(N_t);
  return(N_t_ag)
}

trees_total_5$N_t[trees_total_5$compartiment == "fw"]

ag_N_t_func.2 <- function(df, p_t_ID, comp, N){
  ag_N_.vec <- numeric()
  for(i in unique(df$df[[p_t_ID]])){
    ag_N <- df$df[[N]][df$df[[comp]]== "f" & df$df[[p_t_ID]]== i] +
      df$df[[N]][df$df[[comp]]== "fw" & df$df[[p_t_ID]]== i] +
      df$df[[N]][df$df[[comp]]== "sw" & df$df[[p_t_ID]]== i] +
      df$df[[N]][df$df[[comp]]== "swb" & df$df[[p_t_ID]]== i] +
      df$df[[N]][df$df[[comp]]== "stw" & df$df[[p_t_ID]]== i] +
      df$df[[N]][df$df[[comp]]== "stwb"& df$df[[p_t_ID]]== i]
    
    ID <- df$df[[p_t_ID]][df$df[[p_t_ID]] == i]
    
    
    ag_N.vec[i] <- ag_N[1]}
  
  return(ag_N.vec[p_t_ID])
}

i = 333001


df = trees_total_5
p_t_ID = trees_total_5$plot_ID
comp = trees_total_5$compartiment
N = trees_total_5$N_t



ag_N_t_func.3(trees_total_5,trees_total_5$ID_pt, trees_total_5$compartiment, trees_total_5$N_t)

ag_N_t_func.4(trees_total_5,trees_total_5$ID_pt, trees_total_5$compartiment, trees_total_5$N_t)

view(ag_N)

ag_N$`2603044`

ag_N_t_func.4 <- function(df, p_t_ID, comp, N){
  ag_N.vec <- list()
  for(i in unique(trees_total_5$ID_pt)){
    ag_N <- trees_total_5$N_t[trees_total_5$compartiment == "f" & trees_total_5$ID_pt == i] +
      trees_total_5$N_t[trees_total_5$compartiment == "fw" & trees_total_5$ID_pt == i] + 
      trees_total_5$N_t[trees_total_5$compartiment == "sw" & trees_total_5$ID_pt == i] + 
      trees_total_5$N_t[trees_total_5$compartiment == "swb" & trees_total_5$ID_pt == i] + 
      trees_total_5$N_t[trees_total_5$compartiment == "stw" & trees_total_5$ID_pt == i] + 
      trees_total_5$N_t[trees_total_5$compartiment == "stwb" & trees_total_5$ID_pt == i]
    names(ag_N) <- NULL   
    
    ag_N_ID[i] <- unique(trees_total_5$ID_pt[trees_total_5$ID_pt == i])
    
    ag_N.vec[i] <- cbind(ag_N[1], ag_N_ID[1])
    #names(ag_N.vec) <- NULL
    print(ag_N.vec)
  } 
  return(ag_N.vec[ag_N_ID])
}


trees_total_5 %>% mutate(N_t = ifelse(compartiment != "ag", N_t, ag_N_t_func.4(trees_total_5,ID_pt, compartiment,N_t)))

ag_N_t_func.8 <- function(df, p_t_ID, comp, N){
  ag_N.vec <- numeric()
  for(i in unique(trees_total_5$ID_pt)){
    # summing up 
    ag_N <- (trees_total_5 %>%
               filter(ID_pt == i & !(compartiment %in% c("bg", "ag", "total"))) %>%
               group_by(ID_pt) %>%
               summarise(N_t = sum(N_t)) %>% 
               pull(N_t, ID_pt))
    
    ag_N.vec[i] <- as.vector(ag_N[1])
  }
  
  return(trees_total_5$N_t[trees_total_5$compartiment == "ag"] <- ag_N.vec )
}


as.vector(trees_total_5 %>% filter(.,!(compartiment %in% c("bg", "ag", "total")) ) %>%  group_by(ID_pt)  %>% summarise(sum(N_t)))
mutate(N_t = ifelse(compartiment != "ag", N_t, filter(.,!(compartiment %in% c("bg", "ag", "total")) ) %>%  group_by(ID_pt) %>% select(., N_t) %>% summarise(sum(N_t)) %>% pull()), 
       N_t_meth = ifelse(compartiment != "ag", "N_t", filter(.,!(compartiment %in% c("bg", "ag", "total")) ) %>%  group_by(ID_pt) %>% select(., N_t) %>% summarise(mean(t_ID))))

ag_N_t_func.2(trees_total_5, trees_total_5$compartiment, trees_total_5$N)

# test if the function works

b <- trees_total_5 %>% 
  group_by(ID_pt) %>%
  summarise(N_sum = sum(N_t))
as.vector(b$N_sum)  
b %>% dplyr::pull(N_sum, ID_pt)

ag_N_t_func.6 <- function(df, p_t_ID, N){
  for(i in unique(trees_total_5$ID_pt)){
    N_ag <- df %>% 
      filter(df[[p_t_ID]] == i ) %>% 
      group_by(df[[p_t_ID]]) %>%
      summarise(N_sum = sum(df[[N]]))
  }
  return(as.vector(N_ag$N_sum[1]))
}

ag_N_t_func.6(trees_total_5, trees_total_5$ID_pt, trees_total_5$N)

ag_N_t_func.7 <- function(df, p_t_ID, N){
  N_ag <- df %>% 
    filter(df[[p_t_ID]] == df[[p_t_ID]]) %>% 
    group_by(df[[p_t_ID]]) %>%
    summarise(N_sum = sum(df[[N]]));
  N_ag_vec <- as.vector(N_ag$N_sum);
  return(N_ag_vec[p_t_ID])
}
ag_N_t_func.7(trees_total_5 %>% filter(ID_pt ==333001), 333001, trees_total_5$N[trees_total_5$ID_pt ==333001])



5.616406e-03


trees_total_5 %>% 
  filter(ID_pt == 333001 ) %>% 
  mutate(N_t = ifelse(compartiment == "ag", N_ag_func(trees_total_5, plot_ID, t_ID, N_t), N_t)) %>%
  dplyr::pull(compartiment, N_t)


N_ag_func.1 <- function(df, p_ID, t_ID, N) {
  N_t_ag <- df %>% 
    group_by(df[[p_ID]], df[[t_ID]]) %>%
    summarise(N_t = unlist(across(df[[N]], sum, na.rm = TRUE))) %>% 
    dplyr::pull(N_t);
  return(N_t_ag)
}

N_ag_func.1(trees_total_5, trees_total_5$plot_ID, trees_total_5$t_ID, trees_total_5$N_t)        

# Nitrogen stock
N_t = case_when(compartiment == "foliage" ~ N_f(B_t_tapes, N_SP_group),
                compartiment == "fine_wood" ~ N_fw(B_t_tapes, N_SP_group),
                compartiment == "solid_wood" ~ N_sw(B_t_tapes, N_SP_group),
                compartiment == "solid_bark" ~ N_swb(B_t_tapes, N_SP_group), 
                N_stw_kg = N_sw(tapes_stwB_kg, N_SP_group),
                N_stwb_kg =  N_swb(tapes_stwbB_kg, N_SP_group)) %>% 
  mutate(tot_N_t = (N_f_kg + N_fw_kg + N_sw_kg + N_swb_kg)/1000, 
         tot_C_t = C_ab_t_tapes + C_bB_t)

view(  trees_total_5 %>% 
         mutate(N_t = ifelse(compartiment == "ag", unlist(across(N_t, sum, na.rm = TRUE)), N_t))
       
       
)

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


# --- N.3 plotwise livign trees -------------------------------------------

# ----- 2.5.2. grouped by Plot & species ------------------------------------------------------------
trees_P_SP <- left_join(
  # data set with BA per species
  trees_total_5 %>%
    group_by(plot_ID, SP_code) %>%       # group by plot and species to calculate BA per species 
    summarise(mean_DBH_cm = mean(DBH_cm),         # mean diameter per species per canopy layer per plot
              sd_DBH_cm = sd(DBH_cm),       
              mean_H_m = mean(H_m),                # mean height per species per canopy layer per plot
              sd_height_m = sd(H_m),               # standart deviation of height --> structual richness indicator
              SP_BA_plot = sum(BA_m2),             # calculate BA per species per canopy layer per plot in m2
              mean_BA_SP_plot = mean(BA_m2),       # calculate mean BA in m2 per species per canopy payer per plot
              Nt_plot = n(),
              C_aB_t = sum(C_ab_t_tapes), 
              C_bB_t = sum(C_bB_t), 
              C_tot_t = sum(C_ab_t_tapes+C_bB_t),
              N_aB_t = sum(na.omit(tot_N__t)),
              plot_A_ha = mean(plot_A_ha)) %>%     # plot area in hectare to calculate BA per ha
    mutate(SP_BA_m2ha = SP_BA_plot/plot_A_ha,
           C_aB_t_ha = C_aB_t/plot_A_ha, 
           C_bB_t_ha = C_bB_t/plot_A_ha,
           C_tot_t_ha = C_tot_t/ plot_A_ha, 
           N_aB_t_ha = N_aB_t/plot_A_ha),    # calculate BA per species per plot in m2/ ha
  # dataset with total BA per plot
  trees_total_5 %>%
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



B_tot_t_P_SP = sum((tapes_ab_kg/1000)+(bB_kg/1000)), # total biomass  in t per species per plot 
B_aB_t_P_SP = sum(tapes_ab_kg/1000),           # total aboveground biomass  in t per species per plot
B_bB_t_P_SP = sum(bB_kg/1000),                       # total belowgrond biomass  in t per species per plot
B_f_t_P_SP = sum(tapes_fB_kg/1000),            # foliage biomass in t per species per plot
B_fw_t_P_SP = sum(tapes_fwB_kg/1000),          # fine wood biomass in t per species per plot
B_sw_t_P_SP = sum(tapes_swB_kg/1000),          # solid wood biomassin t per species per plot
B_swb_t_P_SP = sum(tapes_swbB_kg/1000),        # solid wood bark biomass in t per species per plot
B_stw_t_P_SP = sum(tapes_stwB_kg/1000),        # stump wood biomass in t per species per plot
B_stwb_t_P_SP = sum(tapes_stwbB_kg/1000),      # stump wood bark biomass in t per species per plot
C_tot_t_P_SP = sum(C_ab_t_tapes+C_bB_t),    # total C stock in t per species per plot 
C_aB_t_P_SP = sum(C_ab_t_tapes),               # total aboveground C stock in t per species per plot
C_bB_t_P_SP = sum(C_bB_t),                     # total belowgrond C stock in t per species per plot
C_f_t_P_SP = sum(C_f_t),                       # foliage C stock in t per species per plot
C_fw_t_P_SP = sum( C_fw_t),                    # fine wood C stock in t per species per plot
C_sw_t_P_SP = sum( C_sw_t),                    # solid wood C stock in t per species per plot
C_swb_t_P_SP = sum( C_swb_t),                  # solid wood bark C stock in t per species per plot
C_stw_t_P_SP = sum( C_stw_t),                  # stump wood C stock in t per species per plot
C_stwb_t_P_SP = sum( C_stwb_t),                # stump wood bark C stock in t per species per plot
N_aB_t_P_SP = sum(na.omit(tot_N_t)),       # total aboveground N stock in t per species per plot
N_f_t_P_SP = sum(na.omit(N_f_kg/1000)),        # foliage N stock in t per species per plot
N_fw_t_P_SP = sum(na.omit(N_fw_kg/1000)),      # fine wood N stock in t per species per plot
N_sw_t_P_SP = sum(na.omit(N_sw_kg/1000)),      # solid wood N stock in t per species per plot
N_swb_t_P_SP = sum(na.omit(N_swb_kg/1000)),    # solid wood bark N stock in t per species per plot
N_stw_t_P_SP = sum(na.omit(N_stw_kg/1000)),    # stump wood N stock in t per species per plot
N_stwb_t_P_SP = sum(na.omit(N_stwb_kg/1000)),  # stump wood bark N stock in t per species per plot
%>%            
  # values per hectar
  mutate(SP_BA_m2ha = SP_BA_plot/plot_A_ha,           # basal area in m2/ha per species per plot
         Nt_ha = Nt_P_SP/ plot_A_ha,                   # number of trees per hectar per species per plot
         B_tot_t_ha = B_tot_t_P_SP/plot_A_ha,     # total biomass in t/ha per species per plot
         B_aB_t_ha = B_aB_t_P_SP/plot_A_ha,           # total aboveground biomass in t/ha per species per plot
         B_bB_t_ha = B_bB_t_P_SP/plot_A_ha,           # total belowground biomass in t/ha per species per plot
         B_f_t_ha = B_f_t_P_SP/plot_A_ha,             # foliage biomass in t/ha per species per plot
         B_fw_t_ha = B_fw_t_P_SP/plot_A_ha,           # fine wood biomass in t/ha per species per plot
         B_sw_t_ha = B_sw_t_P_SP/plot_A_ha,           # solid wood biomass in t/ha per species per plot
         B_swb_t_ha = B_swb_t_P_SP/plot_A_ha,         # solid wood bark biomass in t/ha per species per plot
         B_stw_t_ha = B_stw_t_P_SP/plot_A_ha,         # stump wood biomass in t/ha per species per plot
         B_stwb_t_ha = B_stwb_t_P_SP/plot_A_ha,       # stump wood bark biomass in t/ha per species per plot
         C_tot_t_ha = C_tot_t_P_SP/plot_A_ha,     # total C stock in t/ha per species per plot
         C_aB_t_ha = C_aB_t_P_SP/plot_A_ha,           # total aboveground C stock in t/ha per species per plot
         C_bB_t_ha = C_bB_t_P_SP/plot_A_ha,           # total belowground C stock in t/ha per species per plot
         C_f_t_ha = C_f_t_P_SP/plot_A_ha,             # foliage C stock in t/ha per species per plot
         C_fw_t_ha = C_fw_t_P_SP/plot_A_ha,           # fine wood C stock in t/ha per species per plot
         C_sw_t_ha = C_sw_t_P_SP/plot_A_ha,           # solid wood C stock in t/ha per species per plot
         C_swb_t_ha =  C_swb_t_P_SP/plot_A_ha,        # solid wood bark C stock in t/ha per species per plot
         C_stw_t_ha = C_stw_t_P_SP/plot_A_ha,         # stump wood C stock in t/ha per species per plot
         C_stwb_t_ha = C_stwb_t_P_SP/plot_A_ha,       # stump wood bark C stock in t/ha per species per plot
         N_aB_t_ha = N_aB_t_P_SP/plot_A_ha,       #  total aboveground N stock in t/ha species per plot 
         N_f_t_ha = N_f_t_P_SP/plot_A_ha,             # foliage N stock in t/ha per species per plot
         N_fw_t_ha = N_fw_t_P_SP/plot_A_ha,           # fine wood N stock in t/ha per species per plot
         N_sw_t_ha = N_sw_t_P_SP/plot_A_ha,           # solid wood N stock in t/ha per species per plot
         N_swb_t_ha = N_swb_t_P_SP/plot_A_ha,         # solid wood bark N stock in t/ha per species per plot
         N_stw_t_ha = N_stw_t_P_SP/plot_A_ha,         # stump wood N stock in t/ha per species per plot
         N_stwb_t_ha = N_stwb_t_P_SP/plot_A_ha) %>%   # stump wood bark N stock in t/ha per species per plot
  # values per MoMoK area
  mutate(SP_BA_m2MA = (SP_BA_plot/plot_A_ha)*MoMoK_A_ha,           # basal area in m2 per MoMoK area  per species per plot
         Nt_MA = (Nt_P_SP/ plot_A_ha)*MoMoK_A_ha,                   # number of trees per MoMoK area  per species per plot
         B_tot_t_MA = (B_tot_t_P_SP/plot_A_ha)*MoMoK_A_ha,     # total biomass in t per MoMoK area  per species per plot
         B_aB_t_MA = (B_aB_t_P_SP/plot_A_ha)*MoMoK_A_ha,           # total aboveground biomass in t per MoMoK area  per species per plot
         B_bB_t_MA = (B_bB_t_P_SP/plot_A_ha)*MoMoK_A_ha,           # total belowground biomass in t per MoMoK area  per species per plot
         B_f_t_MA = (B_f_t_P_SP/plot_A_ha)*MoMoK_A_ha,             # foliage biomass in t per MoMoK area  per species per plot
         B_fw_t_MA = (B_fw_t_P_SP/plot_A_ha)*MoMoK_A_ha,           # fine wood biomass in t per MoMoK area  per species per plot
         B_sw_t_MA = (B_sw_t_P_SP/plot_A_ha)*MoMoK_A_ha,           # solid wood biomass in t per MoMoK area  per species per plot
         B_swb_t_MA =(B_swb_t_P_SP/plot_A_ha)*MoMoK_A_ha,         # solid wood bark biomass in t per MoMoK area  per species per plot
         B_stw_t_MA = (B_stw_t_P_SP/plot_A_ha)*MoMoK_A_ha,         # stump wood biomass in t per MoMoK area  per species per plot
         B_stwb_t_MA = (B_stwb_t_P_SP/plot_A_ha)*MoMoK_A_ha,       # stump wood bark biomass in t per MoMoK area  per species per plot
         C_tot_t_MA = (C_tot_t_P_SP/plot_A_ha)*MoMoK_A_ha,     # total C stock in t per MoMoK area  per species per plot
         C_aB_t_MA = (C_aB_t_P_SP/plot_A_ha)*MoMoK_A_ha,           # total aboveground C stock in t per MoMoK area  per species per plot
         C_bB_t_MA= (C_bB_t_P_SP/plot_A_ha)*MoMoK_A_ha,           # total belowground C stock in t per MoMoK area  per species per plot
         C_f_t_MA = (C_f_t_P_SP/plot_A_ha)*MoMoK_A_ha,             # foliage C stock in t per MoMoK area  per species per plot
         C_fw_t_MA = (C_fw_t_P_SP/plot_A_ha)*MoMoK_A_ha,           # fine wood C stock in t per MoMoK area  per species per plot
         C_sw_t_MA = (C_sw_t_P_SP/plot_A_ha)*MoMoK_A_ha,           # solid wood C stock in t per MoMoK area  per species per plot
         C_swb_t_MA =  (C_swb_t_P_SP/plot_A_ha)*MoMoK_A_ha,        # solid wood bark C stock in t per MoMoK area  per species per plot
         C_stw_t_MA = (C_stw_t_P_SP/plot_A_ha)*MoMoK_A_ha,         # stump wood C stock in t per MoMoK area  per species per plot
         C_stwb_t_MA = (C_stwb_t_P_SP/plot_A_ha)*MoMoK_A_ha,       # stump wood bark C stock in t per MoMoK area  per species per plot
         N_aB_t_MA = (N_aB_t_P_SP/plot_A_ha)*MoMoK_A_ha,            #  total aboveground N stock in t per MoMoK area  species per plot 
         N_f_t_MA = (N_f_t_P_SP/plot_A_ha)*MoMoK_A_ha,             # foliage N stock in t per MoMoK area a per species per plot
         N_fw_t_MA = (N_fw_t_P_SP/plot_A_ha)*MoMoK_A_ha,           # fine wood N stock in t per MoMoK area  per species per plot
         N_sw_t_MA = (N_sw_t_P_SP/plot_A_ha)*MoMoK_A_ha,           # solid wood N stock in t per MoMoK area  per species per plot
         N_swb_t_MA = (N_swb_t_P_SP/plot_A_ha)*MoMoK_A_ha,         # solid wood bark N stock in t per MoMoK area  per species per plot
         N_stw_t_MA = (N_stw_t_P_SP/plot_A_ha)*MoMoK_A_ha,         # stump wood N stock in t per MoMoK area per species per plot
         N_stwb_t_MA =(N_stwb_t_P_SP/plot_A_ha)*MoMoK_A_ha)      # stump wood bark N stock in t per MoMoK area  per species per plot 

trees_P_SP.export <- trees_P_SP %>% 
  select(-c(MoMoK_A_ha, )) %>% 
  select(plot_ID, SP_code,
         mean_DBH_cm, sd_DBH_cm, mean_H_m, sd_height_m, 
         SP_BA_plot, SP_BA_m2ha, SP_BA_m2MA, tot_BA_m2ha, BA_SP_per, dom_SP,
         Nt_P_SP, Nt_ha, Nt_MA,
         B_tot_t_P_SP, B_aB_t_P_SP, B_bB_t_P_SP, B_f_t_P_SP, B_fw_t_P_SP, B_sw_t_P_SP,B_swb_t_P_SP, B_stw_t_P_SP, B_stwb_t_P_SP,
         C_tot_t_P_SP, C_aB_t_P_SP, C_bB_t_P_SP, C_f_t_P_SP, C_fw_t_P_SP, C_sw_t_P_SP, C_swb_t_P_SP, C_stw_t_P_SP, C_stwb_t_P_SP,
         N_aB_t_P_SP, N_f_t_P_SP, N_fw_t_P_SP,N_sw_t_P_SP,N_swb_t_P_SP,N_stw_t_P_SP,N_stwb_t_P_SP,
         B_tot_t_ha, B_aB_t_ha, B_bB_t_ha, B_f_t_ha, B_fw_t_ha, B_sw_t_ha, B_swb_t_ha, B_stw_t_ha, B_stwb_t_ha, 
         C_tot_t_ha, C_aB_t_ha, C_bB_t_ha, C_f_t_ha, C_fw_t_ha, C_sw_t_ha,C_swb_t_ha, C_stw_t_ha, C_stwb_t_ha, 
         N_aB_t_ha, N_f_t_ha, N_fw_t_ha, N_sw_t_ha, N_swb_t_ha, N_stw_t_ha, N_stwb_t_ha, 
         B_tot_t_MA, B_aB_t_MA, B_bB_t_MA, B_f_t_MA, B_fw_t_MA, B_sw_t_MA, B_swb_t_MA, B_stw_t_MA, B_stwb_t_MA, 
         C_tot_t_MA, C_aB_t_MA, C_bB_t_MA,C_f_t_MA, C_fw_t_MA, C_sw_t_MA, C_swb_t_MA, C_stw_t_MA, C_stwb_t_MA, 
         N_aB_t_MA, N_f_t_MA, N_fw_t_MA, N_sw_t_MA, N_swb_t_MA, N_stw_t_MA, N_stwb_t_MA)

colnames(trees_P_SP.export) <- c("plot_ID", "B_Art", 
                                 "durchsch_D1.3_cm","SD_D1.3_cm" ,  "durchsch_H_m" , "SD_H_m",
                                 "G_m2_Art_Plot", "G_m2ha_Art", "G_m2MF_Art", "G_ges_m2ha","G_Anteil_Art","Hauptbaumart_G",
                                 "Stückzahl_n_Art_Plot", "Stückzahl_n__ha"  , "Stückzahl_n_MF", # 15
                                 "B_ges_t_Art_Plot",  "B_oi_t_Art_Plot",   "B_bB_t_Art_Plot",   "B_Bl_t_Art_Plot",    "B_nDhmR_t_Art_Plot",   "B_DhoR_t_Art_Plot", "B_DhR_t_Art_Plot",  "B_StoR_t_Art_Plot",  "B_StR_t_Art_Plot", #24
                                 "C_ges_t_Art_Plot","C_oi_t_Art_Plot","C_ui_t_Art_Plot" ,"C_Bl_t_Art_Plot","C_nDhmR_t_Art_Plot","C_DhoR_t_Art_Plot","C_DhR_t_Art_Plot","C_StoR_t_Art_Plot","C_StR_t_Art_Plot", #33
                                 "N_oi_t_Art_Plot", "N_Bl_t_Art_Plot","N_nDhmR_t_Art_Plot",   "N_DhoR_t_Art_Plot",   "N_DhR_t_Art_Plot",  "N_StoR_t_Art_Plot" , "N_StR_t_Art_Plot",
                                 "B_ges_t_ha" ,"B_oi_t_ha","B_ui_t_ha" ,"B_Bl_t_ha" ,"B_nDhmR_t_ha","B_DhoR_t_ha","B_DhR_t_ha", "B_StoR_t_ha", "B_StR_t_ha",
                                 "C_ges_t_ha", "C_oi_t_ha", "C_ui_t_ha", "C_Bl_t_ha", "C_nDhmR_t_ha", "C_DhoR_t_ha", "C_DhR_t_ha", "C_StoR_t_ha", "C_StR_t_ha",
                                 "N_oi_t_ha", "N_Bl_t_ha", "N_nDhmR_t_ha", "N_DhoR_t_ha","N_DhR_t_ha", "N_StoR_t_ha", "N_StR_t_ha",
                                 "B_ges_t_MF", "B_oi_t_MF", "B_ui_t_MF", "B_Bl_t_MF","B_nDhmR_t_MF","B_DhoR_t_MF","B_DhR_t_MF", "B_StoR_t_MF","B_StR_t_MF",
                                 "C_ges_t_MF", "C_oi_t_MF", "C_ui_t_MF", "C_Bl_t_MF", "C_nDhmR_t_MF", "C_DhoR_t_MF", "C_DhR_t_MF", "C_StoR_t_MF", "C_StR_t_MF",
                                 "N_oi_t_MF" ,"N_Bl_t_MF","N_nDhmR_t_MF", "N_DhoR_t_MF", "N_DhR_t_MF","N_StoR_t_MF","N_StR_t_MF")

# N.4. Deadwood -----------------------------------------------------------

DW_total <- DW_total %>% 
  unite("SP_dec_type", SP_group, dec_type_BWI, sep = "_", remove = FALSE)%>% 
  mutate(V_dw_meth = ifelse(DW_type %in% c(1, 6, 4) | DW_type == 3 & L_m < 3, "V_DW_T1463", "V_DW_T253"),
         V_dw_m3 = ifelse(DW_type %in% c(1, 6, 4) | DW_type == 3 & L_m < 3, V_DW_T1463(D_m, L_m), V_DW_T253(tpS_ID, D_cm, D_h_cm, L_m)),
         B_dw_kg = B_DW(V_dw_m3, SP_dec_type)) %>% 
  # calulation method biomass compartiments for trees of deadwood type 1& 2 
  # no fine wood compartment for deadwood types that don´t include whole trees
  mutate(dw_tapes_fwB_meth =  case_when((DW_type == 2 & dec_type_BWI < 3 )| (DW_type == 5 & dec_type_BWI < 3) ~ "tapes_dw_fw", 
                                        TRUE ~ "not_existing"),
         # solid wood biomass for all dead trees that are in a early state of decay except of deadwood piles
         dw_tapes_sw_meth = case_when(DW_type != 6 & dec_type_BWI < 3 ~ "tapes_swB", 
                                      TRUE ~ "B_dw_kg"),
         # solid wood bark biomass for all dead trees that are in a early state of decay except of deadwood piles
         dw_tapes_swb_meth = case_when(DW_type != 6 & dec_type_BWI < 3 ~ "tapes_swbB", 
                                       TRUE ~ "not existing"), 
         # stump wood biomass for whole dead trees and logs in ealry stages of decay
         dw_tapes_stw_meth = case_when(DW_type %in% c(2, 5, 3, 1) & dec_type_BWI < 3 ~ "tapes_stwB", 
                                       TRUE ~ "not existing"),
         # stump wood bark for whole dead trees and logs in early stages of decay
         dw_tapes_stwb_meth = case_when(DW_type %in% c(2, 5, 3, 1) & dec_type_BWI < 3 ~ "tapes_stwbB", 
                                        TRUE ~ "not existing")) #%>% 
# metod biomass compartiments for trees of deadwood type 1& 2 
# mutate(dw_tapes_fwB_kg =  case_when(dw_tapes_fwB_meth == "tapes_dw_fw" & L_m > 3 ~ dw_tapes_fwB(tpS_ID, D_cm, D_h_m, L_m), 
#                                     TRUE ~ 0),
#      # solid wood biomass for all dead trees that are in a early state of decay except of deadwood piles
#      dw_tapes_sw_kg = case_when(DW_type != 6 & dec_type_BWI < 3 & L_m > 3 ~ dw_tapes_swB(tpS_ID, D_cm, D_h_m, L_m), 
#                                   TRUE ~ B_dw_kg)),
#      # solid wood bark biomass for all dead trees that are in a early state of decay except of deadwood piles
#      dw_tapes_swb_kg = case_when(DW_type != 6 & dec_type_BWI < 3 & L_m > 3 ~ tapes_swbB(tpS_ID, D_cm, D_h_m, L_m), 
#                                      TRUE ~ as.double(0)), 
#        # stump wood biomass for whole dead trees and logs in ealry stages of decay
#        dw_tapes_stw_kg = case_when(DW_type %in% c(2, 5, 3, 1) & dec_type_BWI < 3  & L_m > 3 ~ tapes_stwB(tpS_ID, D_cm, D_h_m, L_m), 
#                                      TRUE ~ as.double(0)),
#        # stump wood bark for whole dead trees and logs in early stages of decay
#        dw_tapes_stw_kg = case_when(DW_type %in% c(2, 5, 3, 1) & dec_type_BWI < 3  & L_m > 3 ~ tapes_stwB(tpS_ID, D_cm, D_h_m, L_m), 
#                                    TRUE ~ as.double(0))) #%>% 
# GHG-TapeS-stepwise
# mutate(dw_swB_kg = ifelse(DW_type %in% c(1, 2, 5) & L_m > 1.3 & dec_type %in% c(1,2), (B_dw_kg - (dw_tapes_brB_kg+ dw_tapes_DhbB_kg)), B_dw_kg), # if the decay state is higher then 1,2 use the total biomass for stem biomass 
#        dw_swbB_kg = ifelse(DW_type %in% c(1, 2, 5) & L_m > 1.3 & dec_type %in% c(1,2), (B_dw_kg - (dw_swB_kg + dw_tapes_brB_kg)), 0), # solid wood bark 
#        dw_fwB_kg = ifelse(DW_type %in% c(1, 2, 5) & L_m > 1.3 & dec_type %in% c(1,2), (B_dw_kg - (dw_swB_kg + dw_swbB_kg)),0)) %>% # fine wood bionmass
# select(-c(dw_tapes_brB_kg, dw_tapes_DhB_kg, dw_tapes_DhbB_kg)) %>%
# mutate(C_dw_kg = C_DW(V_dw_m3, SP_dec_type), 
#        N_dw_fw_kg = N_fw(dw_fwB_kg, N_SP_group), 
#        N_dw_sw_kg = N_sw(dw_swB_kg, N_SP_group), 
#       N_dw_swb_kg = N_swb(dw_swbB_kg, N_SP_group)) %>% 
# mutate(tot_N_dw_kg = N_dw_fw_kg + N_dw_sw_kg + N_dw_swb_kg)


# addding compartiments
# DW_total <- cbind(
#   # this is the dataset calculating the volume and biomass of deadwood and assigns compartimenition method according to decay state and deadwood type
#   (DW_total <- DW_total %>% 
#                     unite("SP_dec_type", SP_group, dec_type_BWI, sep = "_", remove = FALSE)%>% 
#                     mutate(V_dw_meth = ifelse(DW_type %in% c(1, 6, 4) | DW_type == 3 & L_m < 3, "V_DW_T1463", "V_DW_T253"),
#                            V_dw_m3 = ifelse(DW_type %in% c(1, 6, 4) | DW_type == 3 & L_m < 3, V_DW_T1463(D_m, L_m), V_DW_T253(tpS_ID, D_cm, D_h_cm, L_m)),
#                            B_dw_kg = B_DW(V_dw_m3, SP_dec_type)) %>% 
#                 # calulation method biomass compartiments for trees of deadwood type 1& 2 
#                     # no fine wood compartment for deadwood types that don´t include whole trees
#                     mutate(dw_tapes_fwB_meth =  case_when((DW_type == 2 & dec_type_BWI < 3)| (DW_type == 5 & dec_type_BWI < 3) ~ "tapes_dw_fw", 
#                                                           TRUE ~ "not_existing"),
#                            # solid wood biomass for all dead trees that are in a early state of decay except of deadwood piles
#                            dw_tapes_sw_meth = case_when(DW_type != 6 & dec_type_BWI < 3 ~ "tapes_swB", 
#                                                         TRUE ~ "B_dw_kg"),
#                            # solid wood bark biomass for all dead trees that are in a early state of decay except of deadwood piles
#                            dw_tapes_swb_meth = case_when(DW_type != 6 & dec_type_BWI < 3 ~ "tapes_swbB", 
#                                                          TRUE ~ "not existing"), 
#                            # stump wood biomass for whole dead trees and logs in ealry stages of decay
#                            dw_tapes_stw_meth = case_when(DW_type %in% c(2, 5, 3, 1) & dec_type_BWI < 3 ~ "tapes_stwB", 
#                                                          TRUE ~ "not existing"),
#                            # stump wood bark for whole dead trees and logs in early stages of decay
#                            dw_tapes_stwb_meth = case_when(DW_type %in% c(2, 5, 3, 1) & dec_type_BWI < 3 ~ "tapes_stwbB", 
#                                                           TRUE ~ "not existing"))),
#                   as_tibble(ifelse(DW_total$dw_tapes_sw_meth == "tapes_swB" & DW_total$L_m > 3, dw_tapes_swB(tpS_ID, D_cm, D_h_m, L_m), 0)),
#                   as_tibble(ifelse(DW_total$dw_tapes_swb_meth == "tapes_swbB" & DW_total$L_m > 3, dw_tapes_swbB(tpS_ID, D_cm, D_h_m, L_m), 0)),
#                   as_tibble(ifelse(DW_total$dw_tapes_stw_meth == "tapes_stwB" & DW_total$L_m > 3, dw_tapes_stwB(tpS_ID, D_cm, D_h_m, L_m), 0)), 
#                   as_tibble(ifelse(DW_total$dw_tapes_stwb_meth == "tapes_stwbB" & DW_total$L_m > 3, dw_tapes_stwbB(tpS_ID, D_cm, D_h_m, L_m), 0)), 
#                   as_tibble(ifelse(DW_total$dw_tapes_fwB_meth == "tapes_dw_fw" & DW_total$L_m > 3, dw_tapes_fwB(tpS_ID, D_cm, D_h_m, L_m), 0))) 
# 
# colnames(DW_total) <- c("plot_ID","loc_name", "state", "date", "CCS_nr", "t_ID", "SP_dec_type", "SP_group", "DW_type", "L_dm", "D_cm", "dec_type", "dom_SP",
#                         "tpS_SP_com_name", "tpS_ID", "LH_NH", "N_SP_group", "L_m", "D_m", "D_h_cm", "D_h_m", "dec_type_BWI",  "V_dw_meth", "V_dw_m3", "B_dw_kg", "dw_tapes_fwB_meth", "dw_tapes_sw_meth", 
#                         "dw_tapes_swb_meth", "dw_tapes_stw_meth", "dw_tapes_stwb_meth", "dw_tapes_swB_kg", "dw_tapes_swbB_kg", "dw_tapes_stwB_kg", "dw_tapes_stwbB_kg", "dw_tapes_fwB_kg")  


# For deadwood type 4 we still have to estimate the DBH
DW_total %>% 
  filter(DW_type == 4 & dec_type_BWI < 3 & !is.na(D_h_m)) %>% 
  mutate(SP_code = dom_SP, 
         H_m = NA, 
         D_mm = D_cm*10, 
         # estimating diameter via TapeS doesn´t work because on of the input variables is thei tree height wich is below 1.3m
         DBH_cm = tprDiameter(tprTrees(spp = tpS_ID, Dm = as.list(D_cm), Hm = as.list(D_h_m), Ht = L_m, inv = 4), Hx = rep(1.3, nrow(DW_total %>% filter(DW_type == 4 & dec_type_BWI < 3 & !is.na(D_h_m)))), cp=FALSE), 
         # thus we are switching to the BWI taper formula (BWI methodikband chap.5.2.2) dz = d + 2((hd − 130)/tan α)         
         DBH_cm_BWI = (D_mm-2*((D_h_cm-130)/tan(40)))/10, 
         # as the BWI taper formula returns negative values we´ll use a formula to estimate the DBH direktly by KUBLIN (BWI methodikband chap.5.2.2 ): dz = d ∗ (1.0 + (0.0011 ∗ (hd − 130)))
         DBH_cm_Kublin = (D_mm*(1.0+(0.0011*(D_h_cm-130))))/10, 
         DBH_mm_Kublin = D_mm*(1.0+(0.0011*(D_h_cm-130))), 
         DBH_h_m = 1.3) %>% 
  unite(SP_P_ID, plot_ID, SP_code, sep = "", remove = FALSE) %>%            # create column matching vectorised coefficients of coeff_SP_P (1.3. functions, h_nls_SP_P, dplyr::pull)
  left_join(., DW_total %>%
              filter(DW_type == 4 & dec_type_BWI < 3 & !is.na(D_h_m)) %>%                   # this is creates a tree dataset with mean BHD, d_g, h_g per species per plot per canopy layer wich we need for SLOBODA 
              mutate(SP_code = dom_SP, 
                     D_mm = D_cm*10, 
                     DBH_mm_Kublin = D_mm*(1.0+(0.0011*(D_h_cm-130))), 
                     DBH_cm_Kublin = (D_mm*(1.0+(0.0011*(D_h_cm-130))))/10, 
                     BA_m2 = ((DBH_cm_Kublin/100)/2)^2*pi) %>% 
              group_by(plot_ID, SP_code) %>%                               # group by plot and species and canopy layer to calcualte dg, hg 
              summarise(H_g = sum(mean(na.omit(L_m))*BA_m2)/sum(BA_m2),    # Hoehe des Grundflächemittelstammes, calculation according to S. Schnell
                        mean_DBH_mm_Kublin = mean(DBH_mm_Kublin),                            # mean diameter per species per canopy layer per plot
                        D_g = ((sqrt((mean(BA_m2)/pi)))*2)*100, 
                        mean_L = mean(L_m)),           # Durchmesser des Grundflächenmittelstammes; *1000 to get from 1m -> 100cm -> 1000mm
            by = c("plot_ID", "SP_code")) %>% 
  left_join(.,coeff_H_SP_P %>%                                             # joining R2 from coeff_SP_P -> R2.x
              select(plot_ID, SP_code, R2) %>% 
              unite(SP_P_ID, plot_ID, SP_code, sep = "", remove = FALSE),  # create column matching vectorised coefficients of coeff_SP_P (1.3. functions, h_nls_SP_P, dplyr::pull)
            by = c("plot_ID", "SP_code", "SP_P_ID")) %>% 
  left_join(., coeff_H_SP %>% 
              select(SP_code, R2),                                         # joing R2 from coeff_SP data set -> R2.y
            by = "SP_code") %>% 
  mutate(R2_comb = f(R2.x, R2.y, R2.y, R2.x), 
         H_method = case_when(is.na(H_m) & !is.na(R2.x) & R2.x > 0.70 | is.na(H_m) & R2.x > R2.y & R2.x > 0.7 ~ "coeff_SP_P", 
                              is.na(H_m) & is.na(R2.x) & R2.y > 0.70| is.na(H_m) & R2.x < R2.y & R2.y > 0.70 ~ "coeff_sp",
                              is.na(H_m) & is.na(R2_comb) & !is.na(H_g)| is.na(H_m) & R2_comb < 0.70 & !is.na(H_g) ~ "ehk_sloboda",
                              is.na(H_m) & is.na(R2_comb) & is.na(H_g)| is.na(H_m) & R2_comb < 0.70 & is.na(H_g) ~ "h_curtis", 
                              TRUE ~ "L_m"))%>% 
  mutate(H_m = case_when(is.na(H_m) & !is.na(R2.x) & R2.x > 0.70 | is.na(H_m) & R2.x > R2.y & R2.x > 0.7 ~ h_nls_SP_P(SP_P_ID, DBH_cm_Kublin),
                         # if H_m is na and there is an R2 from coeff_SP_P thats bigger then 0.75 or of theres no R2 from 
                         # coeff_SP_plot that´s bigger then R2 of coeff_SP_P while the given R2 from coeff_SP_P is above 
                         # 0.75 then use the SP_P models
                         is.na(H_m) & is.na(R2.x) & R2.y > 0.70 | is.na(H_m) & R2.x < R2.y & R2.y > 0.70 ~ h_nls_SP(SP_code, DBH_cm_Kublin),
                         # when there´s still no model per species or plot, or the R2 of both self-made models is below 0.7 
                         # and hm is na but there is a h_g and d_G
                         is.na(H_m) & is.na(R2_comb) & !is.na(H_g)| is.na(H_m) & R2_comb < 0.70 & !is.na(H_g) ~ ehk_sloboda(H_SP_group, DBH_mm_Kublin, mean_DBH_mm_Kublin, D_g, H_g),
                         # when there´s still no model per species or plot, or the R2 of both self-made models is below 0.7 
                         # and hm is na and the Slobody function cannot eb applied because there is no h_g calculatable use the curtis function
                         is.na(H_m) & is.na(R2_comb) & is.na(H_g)| is.na(H_m) & R2_comb < 0.70 & is.na(H_g) ~ h_curtis(H_SP_group, DBH_mm_Kublin), 
                         TRUE ~ L_m)) %>%
  mutate(H_m = case_when(D_h_m > H_m & L_m > D_h_m & L_m > mean_L & L_m > H_m ~ L_m,
                         D_h_m > H_m & mean_L > D_h_m & L_m < mean_L & mean_L > H_m ~ mean_L,
                         TRUE ~ H_m)) %>% 
  filter(DBH_h_m > H_m)



mutate(dw_tapes_stwB_kg = tapes_stwB(tpS_ID, D_cm, D_h_m, H_m), 
       dw_tapes_stwbB = tapes_stwbB(tpS_ID, D_cm, D_h_m, H_m), 
       dw_tapes_b_ratio = dw_tapes_stwbB/dw_tapes_stwB_kg) %>% 
  dplyr::select(plot_ID, t_ID, dw_tapes_b_ratio)



DW_total %>% 
  mutate(sw_dw_tapes_kg = ifelse(dw_tapes_swB_meth == "yes", 
                                 tprBiomass(tprTrees(spp = tpS_ID, Dm = as.list(D_cm), Hm = as.list(D_h_m), Ht = L_m, inv = 4), component = "sw"), 
                                 0))


dw_sw_tapes.1(DW_total$DW_type[!is.na(DW_total$D_h_m)], 
              DW_total$dec_type_BWI[!is.na(DW_total$D_h_m)], 
              DW_total$tpS_ID[!is.na(DW_total$D_h_m)], 
              DW_total$D_cm[!is.na(DW_total$D_h_m)], 
              DW_total$D_h_m[!is.na(DW_total$D_h_m)], 
              DW_total$L_m[!is.na(DW_total$D_h_m)])




dw_sw_tapes.1 <- function(dw_t, dec_t, tps_sp, d, dh, l){
  dw_sw <- as.vector(ifelse((dw_t == 2 & dec_t < 3 & !is.na(dh) & !is.na(l) & l > 1.3) |( dw_t == 5 & dec_t < 3 & !is.na(dh) & !is.na(l)  & l > 1.3), 
                            tprBiomass(tprTrees(spp = tps_sp, Dm = as.list(d), Hm = as.list(dh), Ht = l, inv = 4), component = "sw"), 
                            0));
  return(dw_sw)
}


dw_sw_tapes.2 <- function(dw_data, dw_t, dec_t, tps_sp, d, dh, l){
  spp = filter(dw_data, {{ dw_t }} == 2 & {{ dec_t}} < 3 & !is.na({{dh}}) & {{l}} > 1.3) %>% dplyr::pull({{tps_sp}});
  Dm = as.list(filter(dw_data,{{ dw_t }} == 2 & {{ dec_t}} < 3 & !is.na({{dh}}) & {{l}} > 1.3) %>% dplyr::pull({{d}}));
  Hm = as.list(filter(dw_data, {{ dw_t }} == 2 & {{ dec_t}} < 3 & !is.na({{dh}}) & {{l}} > 1.3) %>% dplyr::pull({{dh}}));
  Ht = filter(dw_data, {{ dw_t }} == 2 & {{ dec_t}} < 3 & !is.na({{dh}}) & {{l}} > 1.3) %>% dplyr::pull({{l}});
  obj.tbio <- if(length(spp) != 0) {tprTrees(spp, Dm, Hm, Ht, inv = 4)} else {list()};
  sw.df <- if (length(obj.tbio) != 0) {as_tibble(tprBiomass(obj.tbio[obj.tbio@monotone == TRUE], component = "sw"))}
  return(if (length(obj.tbio) != 0) {sw.df$sw})
}

dw_sw_tapes.2(DW_total, DW_total$DW_type, DW_total$dec_type_BWI, DW_total$tpS_ID,  DW_total$D_cm, DW_total$D_h_m, DW_total$L_m)


DW_total %>% 
  mutate(dw_tapes_swB_kg = case_when((DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m  > 1.3 & !is.na(D_h_m))  ~ tapes_swB(tpS_ID, D_cm, D_h_m, L_m), 
                                     DW_type == 3 & dec_type_BWI < 3 & !is.na(dw_tapes_b_ratio) ~ B_dw_kg-(B_dw_kg*dw_tapes_b_ratio),
                                     TRUE ~ 0))


#          dw_tapes_swbB_kg = case_when(DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m  > 1.3 ~ dw_tapes_swbB(tpS_ID, D_cm, D_h_m, L_m),
#                                       DW_type == 3 & dec_type_BWI < 3 & !is.na(dw_tapes_b_ratio) ~ B_dw_kg*dw_tapes_b_ratio,
#                                       TRUE ~ 0), 
#          dw_tapes_stwB_kg =  case_when(DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m  > 1.3 ~ dw_tapes_stwB(tpS_ID, D_cm, D_h_m, L_m), 
#                                       DW_type == 4 & dec_type_BWI < 3 & !is.na(dw_tapes_b_ratio) ~ B_dw_kg-(B_dw_kg*dw_tapes_b_ratio),
#                                       TRUE ~ 0),
#          dw_tapes_stwbB_kg = case_when(DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m  > 1.3 ~ dw_tapes_stwbB(tpS_ID, D_cm, D_h_m, L_m),
#                                       DW_type == 4 & dec_type_BWI < 3 & !is.na(dw_tapes_b_ratio) ~ B_dw_kg*dw_tapes_b_ratio,
#                                       TRUE ~ 0),
#          dw_tapes_fwB_kg = case_when(DW_type %in% c(2, 5) & dec_type_BWI == 1  & L_m  > 1.3 ~ dw_tapes_fwB(tpS_ID, D_cm, D_h_m, L_m),
#                                      TRUE ~ 0))
# GHG - TapeS compartiments
# nitrogen stock

DW_total %>% 
  filter(DW_type %in% c(2, 5) & dec_type_BWI < 3  & !is.na(D_h_m)) %>% 
  mutate(dw_tapes_swB_kg =  dw_tapes_swB(tpS_ID, D_cm, D_h_m, L_m))

DW_total %>% 
  mutate(dw_tapes_swB_kg =  case_when(DW_type %in% c(2, 5) & dec_type_BWI < 3 ~ dw_tapes_swB(tpS_ID, D_cm, D_h_m, L_m),
                                      TRUE ~ 0))
spec_tpS = DW_total %>% filter(DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m > 1.3) %>%  dplyr::pull(tpS_ID)
d = DW_total %>% filter(DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m > 1.3) %>%  dplyr::pull(D_cm)
dh = DW_total %>% filter(DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m > 1.3) %>%  dplyr::pull(D_h_m)
h = DW_total %>% filter(DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m > 1.3) %>%  dplyr::pull(L_m)
as.vector(dw_tapes_swB(spec_tpS, d, dh, h))  


# ----- 2.2.3.2. deadwood compartiments & nitrogen   -------------------------------------------------
# ----- 1.3.4.4. Deadwood compartiments ----------------------------------------

# ----- 1.3.4.4.1. solid wood tapeS --------------------------------------------
dw_tapes_swB <- function(spec_tpS, d, dh, h){         
  spp = na.omit(spec_tpS);
  Dm = na.omit(as.list(d));
  Hm = na.omit(as.list(dh));
  Ht = na.omit(h);
  obj.tbio <- if(length(spp) != 0) {tprTrees(spp, Dm, Hm, Ht, inv = 4)} else {list()};
  sw.df <- if (length(obj.tbio) != 0) {as_tibble(tprBiomass(obj.tbio[obj.tbio@monotone == TRUE], component = "sw"))}
  # most likely the GHGI does not inlude stump wood, so we cannot include it in the coarsewood calculation
  return(if (length(obj.tbio) != 0) {sw.df$sw})
}



# dw_tapes_swB <- function(spec_tpS, d, dh, h){         
#   spp = na.omit(DW_total %>% filter(DW_type %in% c(2, 5) & dec_type_BWI < 3 & L_m > 1.3) %>%  dplyr::pull(tpS_ID));
#   Dm = na.omit(as.list(DW_total %>% filter(DW_type %in% c(2, 5) & dec_type_BWI < 3 & L_m > 1.3) %>%  dplyr::pull(D_cm)));
#   Hm = na.omit(as.list(DW_total %>% filter(DW_type %in% c(2, 5) & dec_type_BWI < 3 & L_m > 1.3) %>%  dplyr::pull(D_h_m)));
#   Ht = na.omit(DW_total %>% filter(DW_type %in% c(2, 5) & dec_type_BWI < 3 & L_m > 1.3) %>%  dplyr::pull(L_m));
#   obj.tbio <- if(length(spp) != 0) {tprTrees(spp, Dm, Hm, Ht, inv = 4)} else {list()};
#   sw.df <- if (length(obj.tbio) != 0) {as_tibble(tprBiomass(obj.tbio[obj.tbio@monotone == TRUE], component = "sw"))};
#   # most likely the GHGI does not inlude stump wood, so we cannot include it in the coarsewood calculation
#   return(if (length(obj.tbio) != 0) {sw.df$sw})
# }
# 
# # ----- 1.3.4.4.2. solid wood bark tapeS ---------------------------------------
# dw_tapes_swbB <- function(spec_tpS, d, dh, h){         
#   spp = na.omit(DW_total %>% filter(DW_type %in% c(2, 5) & dec_type_BWI < 3 & L_m > 1.3) %>%  dplyr::pull(tpS_ID));
#   Dm = na.omit(as.list(DW_total %>% filter(DW_type %in% c(2, 5) & dec_type_BWI < 3 & L_m > 1.3) %>%  dplyr::pull(D_cm)));
#   Hm = na.omit(as.list(DW_total %>% filter(DW_type %in% c(2, 5) & dec_type_BWI < 3 & L_m > 1.3) %>%  dplyr::pull(D_h_m)));
#   Ht = na.omit(DW_total %>% filter(DW_type %in% c(2, 5) & dec_type_BWI < 3 & L_m > 1.3) %>%  dplyr::pull(L_m));
#   obj.tbio <- if(length(spp) != 0) {tprTrees(spp, Dm, Hm, Ht, inv = 4)} else {list()};
#   swb.df <- if (length(obj.tbio) != 0) {as_tibble(tprBiomass(obj.tbio[obj.tbio@monotone == TRUE], component = "sb"))};
#   # most likely the GHGI does not inlude stump wood, so we cannot include it in the coarsewood calculation
#   return(if (length(obj.tbio) != 0) {swb.df$sb})
# }
# 
# # ----- 1.3.4.4.3. stump wood tapeS --------------------------------------------
# dw_tapes_stwB <- function(spec_tpS, d, dh, h){         
#   spp = na.omit(DW_total %>% filter(DW_type %in% c(2, 5) & dec_type_BWI < 3 & L_m > 1.3) %>%  dplyr::pull(tpS_ID));
#   Dm = na.omit(as.list(DW_total %>% filter(DW_type %in% c(2, 5) & dec_type_BWI < 3 & L_m > 1.3) %>%  dplyr::pull(D_cm)));
#   Hm = na.omit(as.list(DW_total %>% filter(DW_type %in% c(2, 5) & dec_type_BWI < 3 & L_m > 1.3) %>%  dplyr::pull(D_h_m)));
#   Ht = na.omit(DW_total %>% filter(DW_type %in% c(2, 5) & dec_type_BWI < 3 & L_m > 1.3) %>%  dplyr::pull(L_m));
#   obj.tbio <- if(length(spp) != 0) {tprTrees(spp, Dm, Hm, Ht, inv = 4)} else {list()};
#   stw.df <- if (length(obj.tbio) != 0) {as_tibble(tprBiomass(obj.tbio[obj.tbio@monotone == TRUE], component = "stw"))};
#   # most likely the GHGI does not inlude stump wood, so we cannot include it in the coarsewood calculation
#   return(if (length(obj.tbio) != 0) {stw.df$stw})
# }
# 
# # ----- 1.3.4.4.4. stump wood bark tapeS ---------------------------------------
# dw_tapes_stwbB <- function(spec_tpS, d, dh, h){         
#   spp = na.omit(DW_total %>% filter(DW_type %in% c(2, 5) & dec_type_BWI < 3 & L_m > 1.3) %>%  dplyr::pull(tpS_ID));
#   Dm = na.omit(as.list(DW_total %>% filter(DW_type %in% c(2, 5) & dec_type_BWI < 3 & L_m > 1.3) %>%  dplyr::pull(D_cm)));
#   Hm = na.omit(as.list(DW_total %>% filter(DW_type %in% c(2, 5) & dec_type_BWI < 3 & L_m > 1.3) %>%  dplyr::pull(D_h_m)));
#   Ht = na.omit(DW_total %>% filter(DW_type %in% c(2, 5) & dec_type_BWI < 3 & L_m > 1.3) %>%  dplyr::pull(L_m));
#   obj.tbio <- if(length(spp) != 0) {tprTrees(spp, Dm, Hm, Ht, inv = 4)} else {list()};
#   stwb.df <- if (length(obj.tbio) != 0) {as_tibble(tprBiomass(obj.tbio[obj.tbio@monotone == TRUE], component = "stb"))};
#   # most likely the GHGI does not inlude stump wood, so we cannot include it in the coarsewood calculation
#   return(if(length(obj.tbio) != 0) {stwb.df$stb})
# }
# 
# # ----- 1.3.4.4.4. fine wood incl. bark tapeS ----------------------------------
# # dw_fwb_tapes
# dw_tapes_fwB <- function(spec_tpS, d, dh, h){         
#   spp = na.omit(DW_total %>% filter(DW_type %in% c(2, 5) & dec_type_BWI < 3 & L_m > 1.3) %>%  dplyr::pull(tpS_ID));
#   Dm = na.omit(as.list(DW_total %>% filter(DW_type %in% c(2, 5) & dec_type_BWI < 3 & L_m > 1.3) %>%  dplyr::pull(D_cm)));
#   Hm = na.omit(as.list(DW_total %>% filter(DW_type %in% c(2, 5) & dec_type_BWI < 3 & L_m > 1.3) %>%  dplyr::pull(D_h_m)));
#   Ht = na.omit(DW_total %>% filter(DW_type %in% c(2, 5) & dec_type_BWI < 3 & L_m > 1.3) %>%  dplyr::pull(L_m));
#   obj.tbio <- if(length(spp) != 0) {tprTrees(spp, Dm, Hm, Ht, inv = 4)} else {list()};
#   fw.df <- if (length(obj.tbio) != 0) {as_tibble(tprBiomass(obj.tbio[obj.tbio@monotone == TRUE], component = "fwb"))};
#   # most likely the GHGI does not inlude stump wood, so we cannot include it in the coarsewood calculation
#   return(if (length(obj.tbio) != 0) {fw.df$fwb})
# }




# DW_total <- DW_total %>% 
#   unite("SP_dec_type", SP_group, dec_type_BWI, sep = "_", remove = FALSE)%>% 
#   mutate(V_dw_meth = ifelse(DW_type %in% c(1, 6, 4) | DW_type == 3 & L_m < 3, "V_DW_T1463", "V_DW_T253"),
#          V_dw_m3 = ifelse(DW_type %in% c(1, 6, 4) | DW_type == 3 & L_m < 3, V_DW_T1463(D_m, L_m), V_DW_T253(tpS_ID, D_cm, D_h_cm, L_m)),
#          B_dw_kg = B_DW(V_dw_m3, SP_dec_type)) %>% 
#   # calulation method biomass compartiments for trees of deadwood type 1& 2 
#   # no fine wood compartment for deadwood types that don´t include whole trees, while Bruchstücke (1) are nirogen-wise treated as fine wood
#   mutate(dw_tapes_fwB_meth =  case_when(DW_type %in% c(2, 5) & dec_type_BWI == 1 ~ "tapes_dw_fw", 
#                                         TRUE ~ "not excisting" ),
#          # solid wood biomass for all dead trees that are in a early state of decay except of deadwood piles, stumps and "starkes Totholz" Bruchstücke ohne Wurzel
#          dw_tapes_sw_meth = case_when(DW_type %in% c(2, 5, 3) & dec_type_BWI < 3 ~ "tapes_swB", 
#                                      TRUE ~ "B_dw_kg"),
#          # solid wood bark biomass for all dead trees that are in a early state of decay except of deadwood piles
#          dw_tapes_swb_meth = case_when(DW_type %in% c(2, 5, 3) & dec_type_BWI < 3 ~ "tapes_swbB", 
#                                        TRUE ~ "not excisting"), 
#          # stump wood biomass for whole dead trees, fragments of deadwood and logs in early stages of decay
#          dw_tapes_stw_meth = case_when(DW_type %in% c(2, 5, 4) & dec_type_BWI < 3 ~ "tapes_stwB",
#                                        TRUE ~ "not excisting"),
#          # stump wood bark for whole dead trees and logs in early stages of decay
#          dw_tapes_stwb_meth = case_when(DW_type %in% c(2, 5, 4) & dec_type_BWI < 3 ~ "tapes_stwbB",
#                                         TRUE ~ "not excisting")) 

DW_total <- DW_total %>% 
  mutate(dw_tapes_swB_kg = as.vector(ifelse(dw_tapes_sw_meth == "tapes_swB" , dw_tapes_swB(tpS_ID, D_cm, D_h_m, L_m), 0)), 
         dw_tapes_swbB_kg = as.vector(ifelse(dw_tapes_swb_meth == "tapes_swbB" , dw_tapes_swbB(tpS_ID, D_cm, D_h_m, L_m), 0)),
         dw_tapes_stwB_kg = as.vector(ifelse(dw_tapes_stw_meth == "tapes_stwB", dw_tapes_stwB(tpS_ID, D_cm, D_h_m, L_m), 0)),
         # dw_tapes_stwbB_kg = as.vector(ifelse(dw_tapes_stwb_meth == "tapes_stwbB", dw_tapes_stwbB(tpS_ID, D_cm, D_h_m, L_m), 0)), 
         dw_tapes_fwB_kg = as.vector(ifelse(dw_tapes_fwB_meth == "tapes_dw_fw", dw_tapes_fwB(tpS_ID, D_cm, D_h_m, L_m), 0)))# %>% 
# GHG-TapeS-stepwise
# solid wood bark  for dead trees of all types except 6
# fine wood has to be added instead of deducted because it´s not part of the calculation of the total biomass
mutate(dw_swB_kg = case_when(DW_type %in% c(2, 5) & dec_type_BWI <3 & L_m > 3  ~ B_dw_kg - (dw_tapes_swbB_kg + dw_tapes_stwB_kg + dw_tapes_stwbB_kg + dw_tapes_fwB_kg),
                             DW_type == 3 & dec_type_BWI <3 & L_m > 3 ~ B_dw_kg - (dw_tapes_swbB_kg + dw_tapes_stwB_kg + dw_tapes_stwbB_kg),
                             DW_type == 1  & dec_type_BWI < 3 ~ 0, 
                             TRUE ~ B_dw_kg), 
       # solid wood bark  for dead trees of all types except 6
       dw_swbB_kg = case_when(DW_type %in% c(2, 5) & dec_type_BWI < 3 & L_m > 3 ~ B_dw_kg - (dw_swB_kg + dw_tapes_stwB_kg + dw_tapes_stwbB_kg + dw_tapes_fwB_kg),
                              DW_type == 3 & dec_type_BWI <3 & L_m > 3 ~ B_dw_kg - (dw_swB_kg + dw_tapes_stwB_kg + dw_tapes_stwbB_kg), 
                              TRUE ~ 0), 
       # stump wood only for dead trees of types 1, 2, 3, 5
       dw_stwB_kg = case_when(DW_type %in% c(2, 5) & dec_type_BWI < 3 & L_m > 3 ~ B_dw_kg - (dw_swB_kg + dw_swbB_kg + dw_tapes_stwbB_kg + dw_tapes_fwB_kg),
                              DW_type == 3 & dec_type_BWI <3 & L_m > 3 ~ B_dw_kg - (dw_swB_kg + dw_swbB_kg + dw_tapes_stwbB_kg), 
                              DW_type == 4 & dec_type_BWI < 3 & L_m > 3 ~ B_dw_kg -dw_tapes_stwbB_kg,
                              TRUE ~ 0), 
       # stump wood bark only for dead trees of types 1, 2, 3, 5
       dw_stwbB_kg = case_when(DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m > 3 ~ B_dw_kg - (dw_swB_kg + dw_swbB_kg+ dw_stwB_kg  + dw_tapes_fwB_kg),
                               DW_type == 3 & dec_type_BWI <3 & L_m > 3 ~ B_dw_kg - (dw_swB_kg + dw_swbB_kg+ dw_stwB_kg), 
                               DW_type == 4 & dec_type_BWI < 3 & L_m > 3 ~ B_dw_kg -dw_stwB_kg, 
                               TRUE ~ 0), 
       # fine wood only for deadwood types 2 & 5 
       dw_fwB_kg = case_when((DW_type == 2 & dec_type_BWI < 3 & L_m > 3)| (DW_type == 5 & dec_type_BWI < 3 & L_m > 3) ~ B_dw_kg - (dw_swB_kg + dw_swbB_kg+ dw_stwB_kg  + dw_stwbB_kg),
                             DW_type == 1  & dec_type_BWI < 3 ~ B_dw_kg,
                             TRUE ~ 0)) %>% 
  select(-c(dw_tapes_swB_kg, dw_tapes_swbB_kg, dw_tapes_stwB_kg, dw_tapes_stwbB_kg, dw_tapes_fwB_kg)) %>%
  mutate(C_dw_kg = C_DW(V_dw_m3, SP_dec_type), 
         N_dw_fw_kg = N_fw(dw_fwB_kg, N_SP_group), 
         N_dw_sw_kg = N_sw(dw_swB_kg, N_SP_group), 
         N_dw_swb_kg = N_swb(dw_swbB_kg, N_SP_group), 
         N_dw_stw_kg = N_swb(dw_stwB_kg, N_SP_group),
         N_dw_stwb_kg = N_swb(dw_stwbB_kg, N_SP_group)) %>% 
  mutate(tot_N_dw_kg = N_dw_fw_kg + N_dw_sw_kg + N_dw_swb_kg + N_dw_stw_kg + N_dw_stwb_kg)

summary(DW_total)
DW_total %>% filter(dw_swB_kg < 0)










# ----- N.4.2. Deadwood pseudo trees --------------------------------------

# 1. pseudo-tree dataset with bark ratio for solid wood of dead trees type 3 with decay state 1 or 2
#       L> For deadwoood type == 3 we have a dDBH, D_h_m but no proper height, so well estimate it with our height functions
DW_total %>% 
   # filter for DW 3 in low staes of decay
   filter(DW_type == 3 & dec_type_BWI < 3 &  L_m  > 1.3) %>% 
   # height estimation like for living trees  --> unneccesarry cause we have estimated heights via tapeS
   mutate(SP_code = dom_SP,
          H_m = NA, 
          D_mm = D_cm*10, ) %>% 
   unite(SP_P_ID, plot_ID, SP_code, sep = "", remove = FALSE) %>%            # create column matching vectorised coefficients of coeff_SP_P (1.3. functions, h_nls_SP_P, dplyr::pull)
   #estimating height for Tapes for D_g deadwood species D_g
   # to create a more generally applicabple bark share the pseudo trees are going to be build around the diameter of the mean basal area per plot and species
   left_join(., DW_total %>%
               filter(DW_type == 3 & dec_type_BWI < 3 & L_m  > 1.3) %>%                   
               mutate(BA_m2 = (D_m/2)^2*pi) %>% 
               group_by(plot_ID, SP_group_char) %>%                                # group by plot and species and canopy layer to calcualte dg, hg 
               summarise(dw_D_g = ((sqrt((mean(BA_m2)/pi)))*2)*100,                # Durchmesser des Grundflächenmittelstammes; *1000 to get from 1m -> 100cm -> 1000mm
                         SP_dw_tps = mean(SP_dw_tps)) %>%                          # this is just to keep the species group
               mutate(dw_H_dg_tapes = estHeight(d13 = dw_D_g, sp = SP_dw_tps)) %>% 
               select(plot_ID, SP_group_char, dw_H_dg_tapes, dw_D_g),    # estimate height for D_g of deadwood by deadwood species group                      
             by = c("plot_ID", "SP_group_char")) %>% 
   # estimating heights like for living trees --> not needed anymore
   # this is creates a tree dataset with mean BHD, d_g, h_g per species per plot per canopy layer wich we need for SLOBODA 
   left_join(., DW_total %>%
               filter(DW_type == 3 & dec_type_BWI < 3 & L_m  > 1.3) %>%                   
               mutate(SP_code = dom_SP, 
                      BA_m2 = (D_m/2)^2*pi, 
                      D_mm = D_cm*10) %>% 
               group_by(plot_ID, SP_code) %>%                               # group by plot and species and canopy layer to calcualte dg, hg 
               summarise(H_g = sum(mean(na.omit(L_m))*BA_m2)/sum(BA_m2),    # Hoehe des Grundflächemittelstammes, calculation according to S. Schnell
                         mean_D_mm = mean(D_mm),                            # mean diameter per species per canopy layer per plot
                         D_g = ((sqrt((mean(BA_m2)/pi)))*2)*100,            # Durchmesser des Grundflächenmittelstammes; *1000 to get from 1m -> 100cm -> 1000mm
                         mean_L = mean(L_m)),                               
             by = c("plot_ID", "SP_code")) %>% 
   left_join(.,coeff_H_SP_P %>%                                           # joining R2 from coeff_SP_P -> R2.x
               select(plot_ID, SP_code, R2) %>% 
               unite(SP_P_ID, plot_ID, SP_code, sep = "", remove = FALSE),  # create column matching vectorised coefficients of coeff_SP_P (1.3. functions, h_nls_SP_P, dplyr::pull)
             by = c("plot_ID", "SP_code", "SP_P_ID")) %>% 
   left_join(., coeff_H_SP %>% 
               select(SP_code, R2),                                         # joing R2 from coeff_SP data set -> R2.y
             by = "SP_code") %>% 
   mutate(R2_comb = f(R2.x, R2.y, R2.y, R2.x), 
          H_method = case_when(is.na(H_m) & !is.na(R2.x) & R2.x > 0.70 | is.na(H_m) & R2.x > R2.y & R2.x > 0.7 ~ "coeff_SP_P", 
                               is.na(H_m) & is.na(R2.x) & R2.y > 0.70| is.na(H_m) & R2.x < R2.y & R2.y > 0.70 ~ "coeff_sp",
                               is.na(H_m) & is.na(R2_comb) & !is.na(H_g)| is.na(H_m) & R2_comb < 0.70 & !is.na(H_g) ~ "ehk_sloboda",
                               is.na(H_m) & is.na(R2_comb) & is.na(H_g)| is.na(H_m) & R2_comb < 0.70 & is.na(H_g) ~ "h_curtis", 
                               TRUE ~ "L_m"))%>% 
   mutate(H_m = case_when(is.na(H_m) & !is.na(R2.x) & R2.x > 0.70 | is.na(H_m) & R2.x > R2.y & R2.x > 0.7 ~ h_nls_SP_P(SP_P_ID, D_cm),
                          # if H_m is na and there is an R2 from coeff_SP_P thats bigger then 0.75 or of theres no R2 from 
                          # coeff_SP_plot that´s bigger then R2 of coeff_SP_P while the given R2 from coeff_SP_P is above 
                          # 0.75 then use the SP_P models
                          is.na(H_m) & is.na(R2.x) & R2.y > 0.70 | is.na(H_m) & R2.x < R2.y & R2.y > 0.70 ~ h_nls_SP(SP_code, D_cm),
                          # when there´s still no model per species or plot, or the R2 of both self-made models is below 0.7 
                          # and hm is na but there is a h_g and d_G
                          is.na(H_m) & is.na(R2_comb) & !is.na(H_g)| is.na(H_m) & R2_comb < 0.70 & !is.na(H_g) ~ ehk_sloboda(H_SP_group, D_mm, mean_D_mm, D_g, H_g),
                          # when there´s still no model per species or plot, or the R2 of both self-made models is below 0.7 
                          # and hm is na and the Sloboda function cannot eb applied because there is no h_g calculatable use the curtis function
                          is.na(H_m) & is.na(R2_comb) & is.na(H_g)| is.na(H_m) & R2_comb < 0.70 & is.na(H_g) ~ h_curtis(H_SP_group, D_mm), 
                          TRUE ~ L_m),
          H_m_tapeS = estHeight(d13 = D_cm, sp = tpS_ID)) %>%
   # in case the estimated height is lower then the mean lenght or lenght of the respective tree, set height to lenght instead
   mutate(H_m = case_when(D_h_m > H_m & L_m > D_h_m & L_m > mean_L ~ L_m,
                          D_h_m > H_m & mean_L > D_h_m & L_m < mean_L ~ mean_L,
                          TRUE ~ H_m), 
          H_diff = H_m - H_m_tapeS) %>%
   # the same for the dg height estimated via tapeS
   mutate(dw_H_dg_tapes = case_when(D_h_m > dw_H_dg_tapes & L_m > D_h_m & L_m > mean_L ~ L_m,
                                    D_h_m > dw_H_dg_tapes & mean_L > D_h_m & L_m < mean_L ~ mean_L,
                                    TRUE ~ dw_H_dg_tapes), 
          H_dg_diff = H_m - dw_H_dg_tapes) %>%
   # if there are still rows were the DBH measurment height exceeds the estiamted height they are excluded --> in this case 1
   filter(D_h_m < dw_H_dg_tapes) %>% 
   # calculating biomass in compartiments via TapeS
   mutate(tapeS_wood = tapes_swB(SP_dw_tps, dw_D_g, D_h_m,  estHeight(d13 = dw_D_g, sp = SP_dw_tps)),    # solid wood
          tapeS_bark = tapes_swbB(SP_dw_tps, dw_D_g, D_h_m,  estHeight(d13 = dw_D_g, sp = SP_dw_tps)),      # solid wood bark 
          dw_tapes_b_ratio = tapeS_bark/tapeS_wood) %>%      # ratio between solid wood bark vs. solid wood
   dplyr::select(plot_ID, t_ID, DW_type, dec_type_BWI, CCS_nr, tapeS_wood, tapeS_bark, dw_tapes_b_ratio)





# estimating height like for living trees --> not necesarry anymore 
DW_total %>% 
unite(SP_P_ID, plot_ID, SP_code, sep = "", remove = FALSE) %>%            # create column matching vectorised coefficients of coeff_SP_P (1.3. functions, h_nls_SP_P, dplyr::pull)
  left_join(., DW_total %>%
              filter(DW_type == 4 & dec_type_BWI < 3  & L_m > 1.3) %>%                   # this is creates a tree dataset with mean BHD, d_g, h_g per species per plot per canopy layer wich we need for SLOBODA 
              mutate(SP_code = dom_SP, 
                     # estimating diameter
                     D_mm = D_cm*10, 
                     DBH_mm_Kublin = D_mm*(1.0+(0.0011*(D_h_cm-130))), 
                     DBH_cm_Kublin = (D_mm*(1.0+(0.0011*(D_h_cm-130))))/10, 
                     BA_m2 = ((DBH_cm_Kublin/100)/2)^2*pi) %>% 
              # estimating height based on DBH_Kublin --> unneccesarry cause we have estimated heights via tapeS
              group_by(plot_ID, SP_code) %>%                               # group by plot and species and canopy layer to calcualte dg, hg 
              summarise(H_g = sum(mean(na.omit(L_m))*BA_m2)/sum(BA_m2),    # Hoehe des Grundflächemittelstammes, calculation according to S. Schnell
                        mean_DBH_mm_Kublin = mean(DBH_mm_Kublin),                            # mean diameter per species per canopy layer per plot
                        D_g = ((sqrt((mean(BA_m2)/pi)))*2)*100, 
                        mean_L = mean(L_m)),           # Durchmesser des Grundflächenmittelstammes; *1000 to get from 1m -> 100cm -> 1000mm
            by = c("plot_ID", "SP_code")) %>% 
  left_join(.,coeff_H_SP_P %>%                                             # joining R2 from coeff_SP_P -> R2.x
              select(plot_ID, SP_code, R2) %>% 
              unite(SP_P_ID, plot_ID, SP_code, sep = "", remove = FALSE),  # create column matching vectorised coefficients of coeff_SP_P (1.3. functions, h_nls_SP_P, dplyr::pull)
            by = c("plot_ID", "SP_code", "SP_P_ID")) %>% 
  left_join(., coeff_H_SP %>% 
              select(SP_code, R2),                                         # joing R2 from coeff_SP data set -> R2.y
            by = "SP_code") %>%
  mutate(R2_comb = f(R2.x, R2.y, R2.y, R2.x), 
         H_method = case_when(is.na(H_m) & !is.na(R2.x) & R2.x > 0.70 | is.na(H_m) & R2.x > R2.y & R2.x > 0.7 ~ "coeff_SP_P", 
                              is.na(H_m) & is.na(R2.x) & R2.y > 0.70| is.na(H_m) & R2.x < R2.y & R2.y > 0.70 ~ "coeff_sp",
                              is.na(H_m) & is.na(R2_comb) & !is.na(H_g)| is.na(H_m) & R2_comb < 0.70 & !is.na(H_g) ~ "ehk_sloboda",
                              is.na(H_m) & is.na(R2_comb) & is.na(H_g)| is.na(H_m) & R2_comb < 0.70 & is.na(H_g) ~ "h_curtis", 
                              TRUE ~ "L_m"))%>% 
  mutate(H_m = case_when(is.na(H_m) & !is.na(R2.x) & R2.x > 0.70 | is.na(H_m) & R2.x > R2.y & R2.x > 0.7 ~ h_nls_SP_P(SP_P_ID, DBH_cm_Kublin),
                         # if H_m is na and there is an R2 from coeff_SP_P thats bigger then 0.75 or of theres no R2 from 
                         # coeff_SP_plot that´s bigger then R2 of coeff_SP_P while the given R2 from coeff_SP_P is above 
                         # 0.75 then use the SP_P models
                         is.na(H_m) & is.na(R2.x) & R2.y > 0.70 | is.na(H_m) & R2.x < R2.y & R2.y > 0.70 ~ h_nls_SP(SP_code, DBH_cm_Kublin),
                         # when there´s still no model per species or plot, or the R2 of both self-made models is below 0.7 
                         # and hm is na but there is a h_g and d_G
                         is.na(H_m) & is.na(R2_comb) & !is.na(H_g)| is.na(H_m) & R2_comb < 0.70 & !is.na(H_g) ~ ehk_sloboda(H_SP_group, DBH_mm_Kublin, mean_DBH_mm_Kublin, D_g, H_g),
                         # when there´s still no model per species or plot, or the R2 of both self-made models is below 0.7 
                         # and hm is na and the Slobody function cannot eb applied because there is no h_g calculatable use the curtis function
                         is.na(H_m) & is.na(R2_comb) & is.na(H_g)| is.na(H_m) & R2_comb < 0.70 & is.na(H_g) ~ h_curtis(H_SP_group, DBH_mm_Kublin), 
                         TRUE ~ L_m), 
         H_m_tapeS = estHeight(d13 = DBH_cm_Kublin, sp = tpS_ID))




DW_total <- left_join(DW_total, 
                      # binding both datasets with bark ratio together whereby trees remain destinguishable because of the combination of tree ID and plot ID 
                      rbind(
                        # 1. pseudo-tree dataset with bark ratio for solid wood of dead trees type 3 with decay state 1 or 2
                        #       L> For deadwoood type == 3 we have a dDBH, D_h_m but no proper height, so well estimate it with our height functions
                        (DW_total %>% 
                           # filter for DW 3 in low staes of decay
                           filter(DW_type == 3 & dec_type_BWI < 3 &  L_m  > 1.3) %>%
                           mutate(SP_code = dom_SP,
                                  H_m = NA, 
                                  D_mm = D_cm*10, ) %>% 
                           unite(SP_P_ID, plot_ID, SP_code, sep = "", remove = FALSE) %>%            # create column matching vectorised coefficients of coeff_SP_P (1.3. functions, h_nls_SP_P, dplyr::pull)
                           #estimating height for Tapes for D_g deadwood species D_g
                           # to create a more generally applicable bark share the pseudo trees are going to be build around the diameter of the mean basal area per plot and species
                           left_join(., DW_total %>%
                                       filter(DW_type == 3 & dec_type_BWI < 3 & L_m  > 1.3) %>%                   
                                       mutate(BA_m2 = (D_m/2)^2*pi) %>% 
                                       group_by(plot_ID, SP_group_char, dec_type_BWI) %>%                                # group by plot and species and canopy layer to calcualte dg, hg 
                                       summarise(dw_D_g = ((sqrt((mean(BA_m2)/pi)))*2)*100,                # Durchmesser des Grundflächenmittelstammes; *1000 to get from 1m -> 100cm -> 1000mm
                                                 SP_dw_tps = mean(SP_dw_tps), 
                                                 mean_L = mean(L_m)) %>%                          # this is just to keep the species group
                                       mutate(dw_H_dg_tapes = estHeight(d13 = dw_D_g, sp = SP_dw_tps)) %>% 
                                       select(plot_ID, SP_group_char, mean_L, dw_H_dg_tapes, dw_D_g),    # estimate height for D_g of deadwood by deadwood species group                      
                                     by = c("plot_ID", "SP_group_char", dec_type_BWI)) %>% 
                           # the same for the dg height estimated via tapeS
                           mutate(dw_H_dg_tapes = case_when(D_h_m > dw_H_dg_tapes & L_m > D_h_m & L_m > mean_L ~ L_m,
                                                            D_h_m > dw_H_dg_tapes & mean_L > D_h_m & L_m < mean_L ~ mean_L,
                                                            TRUE ~ dw_H_dg_tapes), 
                                  H_dg_diff = H_m - dw_H_dg_tapes) %>%
                           # if there are still rows were the DBH measurment height exceeds the estiamted height they are excluded --> in this case 1
                           filter(D_h_m < dw_H_dg_tapes) %>% 
                           # calculating biomass in compartiments via TapeS
                           mutate(tapeS_wood = tapes_swB(SP_dw_tps, dw_D_g, D_h_m,  dw_H_dg_tapes),                               # solid wood
                                  tapeS_bark = tapes_swbB(SP_dw_tps, dw_D_g, D_h_m,  estHeight(d13 = dw_D_g, sp = SP_dw_tps)),      # solid wood bark 
                                  dw_tapes_b_ratio = rdB_DW(tapeS_bark, SP_dec_type)/rdB_DW(tapeS_wood, SP_dec_type)) %>%      # ratio between solid wood bark vs. solid wood
                           dplyr::select(plot_ID, t_ID, DW_type, dec_type_BWI, CCS_nr, tapeS_wood, tapeS_bark, dw_tapes_b_ratio)), 
                        # 2. dataset with bark ratio for stump wood for deadwood type 4 in decay state 1 & 2
                        (DW_total %>% 
                           # filter for the respective deadwood type and decay state
                           filter(DW_type == 4 & dec_type_BWI < 3  & L_m  > 1.3) %>%
                           mutate(SP_code = dom_SP,
                                  H_m = NA, 
                                  D_mm = D_cm*10,
                                  DBH_h_m = 1.3) %>% 
                           # estimating height for Tapes for D_g deadwood species D_g
                             # calculate DBH via Kublin 
                             # --> estimate height via TapeS with Kublin 
                             # --> use that height to calcualte new DBH in  tapeS 
                             # --> calculate D_g grouped by plot_ID and SP_dw_char and dec_type
                             # --> estimate height based on the tapes_D_g
                           left_join(., DW_total %>%
                                       filter(DW_type == 4 & dec_type_BWI < 3  & L_m > 1.3) %>%
                                       mutate(BA_m2 = (D_m/2)^2*pi, 
                                              D_mm = D_cm*10, 
                                              # estimating diameter via TapeS dirctly doesn´t work because on of the input variables is the tree height which is below 1.3m so it keeps returning 0 
                                              # https://stackoverflow.com/questions/22104774/how-to-initialize-a-vector-with-fixed-length-in-r
                                              # thus we are switching to the BWI taper formula (BWI methodikband chap.5.2.2) dz = d + 2((hd − 130)/tan α)
                                              DBH_cm_Kublin = (D_mm*(1.0+(0.0011*(D_h_cm-130))))/10,               # a) estiamting diameter with Kublin because it doesnt require height
                                              H_tps_Kublin = estHeight(d13 = DBH_cm_Kublin, sp = SP_dw_tps),       # b) estimate height with TapeS based on Kublin
                                              tapeS_DBH_cm = tprDiameter(tprTrees(spp = SP_dw_tps,                 # c) estimate TapeS DBH with TapeS height based on Kublin
                                                                                  Dm = as.list(DBH_cm_Kublin),
                                                                                  Hm = as.list(D_h_m), 
                                                                                  Ht = H_tps_Kublin, inv = 4), 
                                                                         Hx = rep(1.3, nrow(DW_total %>% filter(DW_type == 4 & dec_type_BWI < 3 & L_m > 1.3))), cp=FALSE), 
                                              tapeS_DBH_cm.test = tprDiameter(tprTrees(spp = SP_dw_tps,                 # c) estimate TapeS DBH with TapeS height based on Kublin
                                                                                  Dm = as.list(((D_mm*(1.0+(0.0011*(D_h_cm-130))))/10)),   
                                                                                  Hm = as.list(D_h_m), 
                                                                                  Ht = estHeight(d13 = ((D_mm*(1.0+(0.0011*(D_h_cm-130))))/10), sp = SP_dw_tps), inv = 4), 
                                                                         Hx = rep(1.3, nrow(DW_total %>% filter(DW_type == 4 & dec_type_BWI < 3 & L_m > 1.3))), cp=FALSE),) %>% 
                                       group_by(plot_ID, SP_group_char) %>% 
                                       summarise(dw_D_g = ((sqrt((mean(BA_m2)/pi)))*2)*100,                # Durchmesser des Grundflächenmittelstammes; *1000 to get from 1m -> 100cm -> 1000mm
                                                 mean_L = mean(L_m),
                                                 SP_dw_tps = mean(SP_dw_tps)) %>%                          # this is just to keep the species group
                                       mutate(dw_H_dg_tapes = estHeight(d13 = dw_D_g, sp = SP_dw_tps)) %>%  # estimate height for D_g of deadwood by deadwood species group 
                                       select(plot_ID, SP_group_char, mean_L, dw_H_dg_tapes, dw_D_g),                         
                                     by = c("plot_ID", "SP_group_char")) %>%
                           mutate(dw_H_dg_tapes = case_when(D_h_m > dw_H_dg_tapes & L_m > D_h_m & L_m > mean_L & L_m > dw_H_dg_tapes ~ L_m,
                                                            D_h_m > dw_H_dg_tapes & mean_L > D_h_m & L_m < mean_L & mean_L > dw_H_dg_tapes ~ mean_L,
                                                            TRUE ~ dw_H_dg_tapes)) %>%
                           # if there are still rows were the DBH measurment height exceeds the estiamted height they are excluded --> in this case 1
                           filter(DBH_h_m < dw_H_dg_tapes) %>% 
                           mutate(tapeS_wood = tapes_stwB(SP_dw_tps, dw_D_g, DBH_h_m, dw_H_dg_tapes), 
                                  tapeS_bark = tapes_stwbB(SP_dw_tps, dw_D_g, DBH_h_m, dw_H_dg_tapes), 
                                  dw_tapes_b_ratio = rdB_DW(tapeS_bark, SP_dec_type)/rdB_DW(tapeS_wood, SP_dec_type)) %>%  
                           # L> transforming tapeS biomass (for living trees)into tapeS biomass for dead trees via relative density (rdB_DW)
                           dplyr::select(plot_ID, t_ID, DW_type, dec_type_BWI, CCS_nr, tapeS_wood, tapeS_bark,  dw_tapes_b_ratio))), 
                      by = c("plot_ID", "t_ID", "CCS_nr", "DW_type", "dec_type_BWI")) %>% 
  #  volume, biomass, carbon
  mutate(V_dw_meth = ifelse(DW_type %in% c(1, 6, 4) | DW_type == 3 & L_m < 3, "V_DW_T1463", "V_DW_T253"),
         V_dw_m3 = ifelse(DW_type %in% c(1, 6, 4) | DW_type == 3 & L_m < 3, V_DW_T1463(D_m, L_m), V_DW_T253(tpS_ID, D_cm, D_h_cm, L_m)),
         B_dw_kg = B_DW(V_dw_m3, SP_dec_type), 
         C_dw_kg = C_DW(V_dw_m3, SP_dec_type))%>% 
  # TapeS compartiments methods to be able to subset dataset and apply functions separately 
  mutate(dw_tapes_swB_meth = case_when(DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m  > 1.3 & !is.na(D_h_m)  ~ "yes", 
                                       DW_type == 3 & dec_type_BWI < 3 & !is.na(dw_tapes_b_ratio) ~ "sw_tapeS_wood",
                                       TRUE ~ "no"),
         dw_tapes_swbB_meth = case_when(DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m  > 1.3 & !is.na(D_h_m) ~ "yes",
                                        DW_type == 3 & dec_type_BWI < 3 & !is.na(dw_tapes_b_ratio) ~ "sw_tapeS_bark",
                                        TRUE ~ "no"),
         dw_tapes_stwB_meth =  case_when(DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m  > 1.3 & !is.na(D_h_m) ~ "yes",
                                         DW_type == 4 & dec_type_BWI < 3 & !is.na(dw_tapes_b_ratio) ~ "st_tapeS_wood",
                                         TRUE ~ "no"),
         dw_tapes_stwbB_meth = case_when(DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m  > 1.3 & !is.na(D_h_m) ~ "yes",
                                         DW_type == 4 & dec_type_BWI < 3 & !is.na(dw_tapes_b_ratio) ~ "st_tapeS_bark",
                                         TRUE ~ "no"),
         dw_tapes_fwB_meth = case_when(DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m  > 1.3 ~ "yes",
                                       # I have to add fine wood for BWI_dec_type 2 too,
                                       # but to be able to calculate the whole biomass stepwise, 
                                       # but I will not add it to the final dw_kg column
                                       TRUE ~ "no"))


# estimating diameter via TapeS dirctly doesn´t work because on of the input variables is the tree height which is below 1.3m so it keeps returning 0 
# https://stackoverflow.com/questions/22104774/how-to-initialize-a-vector-with-fixed-length-in-r
#DBH_cm = tprDiameter(tprTrees(spp = tpS_ID, Dm = as.list(D_cm), Hm = as.list(D_h_m), Ht = , inv = 4), Hx = rep(1.3, nrow(DW_total %>% filter(DW_type == 4 & dec_type_BWI < 3 & !is.na(D_h_m)))), cp=FALSE), 
# thus we are switching to the BWI taper formula (BWI methodikband chap.5.2.2) dz = d + 2((hd − 130)/tan α)         
DBH_cm_BWI = (D_mm-2*((D_h_cm-130)/tan(40)))/10, 
# as the BWI taper formula returns negative values we´ll use a formula to estimate the DBH direktly by KUBLIN (BWI methodikband chap.5.2.2 ): dz = d ∗ (1.0 + (0.0011 ∗ (hd − 130)))
DBH_cm_Kublin = (D_mm*(1.0+(0.0011*(D_h_cm-130))))/10,
H_tps_Kublin = estHeight(d13 = DBH_cm_Kublin, sp = SP_dw_tps), 
tapeS_DBH_cm = tprDiameter(tprTrees(spp = SP_dw_tps, 
                                    Dm = as.list(DBH_cm_Kublin), 
                                    Hm = as.list(D_h_m), 
                                    Ht = H_tps_Kublin, inv = 4), 
                           Hx = rep(1.3, nrow(DW_total %>% filter(DW_type == 4 & dec_type_BWI < 3 & L_m > 1.3))), cp=FALSE),
DBH_mm_Kublin = D_mm*(1.0+(0.0011*(D_h_cm-130))), 

DW_total <- left_join(DW_total, 
                      # binding both datasets with bark ratio together whereby trees remain destinguishable because of the combination of tree ID and plot ID 
                      rbind(
                        # 1. pseudo-tree dataset with bark ratio for solid wood of dead trees type 3 with decay state 1 or 2
                        #       L> For deadwoood type == 3 we have a dDBH, D_h_m but no proper height, so well estimate it with our height functions
                        (DW_total %>% 
                           # filter for DW 3 in low staes of decay
                           filter(DW_type == 3 & dec_type_BWI < 3 &  L_m  > 1.3) %>%
                           mutate(SP_code = dom_SP,
                                  H_m = NA, 
                                  D_mm = D_cm*10, ) %>% 
                           unite(SP_P_ID, plot_ID, SP_code, sep = "", remove = FALSE) %>%            # create column matching vectorised coefficients of coeff_SP_P (1.3. functions, h_nls_SP_P, dplyr::pull)
                           #estimating height for Tapes for D_g deadwood species D_g
                           # to create a more generally applicabple bark share the pseudo trees are going to be build around the diameter of the mean basal area per plot and species
                           left_join(., DW_total %>%
                                       filter(DW_type == 3 & dec_type_BWI < 3 & L_m  > 1.3) %>%                   
                                       mutate(BA_m2 = (D_m/2)^2*pi) %>% 
                                       group_by(plot_ID, SP_group_char) %>%                                # group by plot and species and canopy layer to calcualte dg, hg 
                                       summarise(dw_D_g = ((sqrt((mean(BA_m2)/pi)))*2)*100,                # Durchmesser des Grundflächenmittelstammes; *1000 to get from 1m -> 100cm -> 1000mm
                                                 SP_dw_tps = mean(SP_dw_tps), 
                                                 mean_L = mean(L_m)) %>%                          # this is just to keep the species group
                                       mutate(dw_H_dg_tapes = estHeight(d13 = dw_D_g, sp = SP_dw_tps)) %>% 
                                       select(plot_ID, SP_group_char, mean_L, dw_H_dg_tapes, dw_D_g),    # estimate height for D_g of deadwood by deadwood species group                      
                                     by = c("plot_ID", "SP_group_char")) %>% 
                           # the same for the dg height estimated via tapeS
                           mutate(dw_H_dg_tapes = case_when(D_h_m > dw_H_dg_tapes & L_m > D_h_m & L_m > mean_L ~ L_m,
                                                            D_h_m > dw_H_dg_tapes & mean_L > D_h_m & L_m < mean_L ~ mean_L,
                                                            TRUE ~ dw_H_dg_tapes), 
                                  H_dg_diff = H_m - dw_H_dg_tapes) %>%
                           # if there are still rows were the DBH measurment height exceeds the estiamted height they are excluded --> in this case 1
                           filter(D_h_m < dw_H_dg_tapes) %>% 
                           # calculating biomass in compartiments via TapeS
                           mutate(tapeS_wood = tapes_swB(SP_dw_tps, dw_D_g, D_h_m,  dw_H_dg_tapes),                               # solid wood
                                  tapeS_bark = tapes_swbB(SP_dw_tps, dw_D_g, D_h_m,  estHeight(d13 = dw_D_g, sp = SP_dw_tps)),      # solid wood bark 
                                  dw_tapes_b_ratio = rdB_DW(tapeS_bark, SP_dec_type)/rdB_DW(tapeS_wood, SP_dec_type)) %>%      # ratio between solid wood bark vs. solid wood
                           dplyr::select(plot_ID, t_ID, DW_type, dec_type_BWI, CCS_nr, tapeS_wood, tapeS_bark, dw_tapes_b_ratio)), 
                        # 2. dataset with bark ratio for stump wood for deadwood type 4 in decay state 1 & 2
                        (DW_total %>% 
                           # filter for the respective deadwood type and decay state
                           filter(DW_type == 4 & dec_type_BWI < 3  & L_m  > 1.3) %>%
                           mutate(SP_code = dom_SP,
                                  H_m = NA, 
                                  D_mm = D_cm*10,
                                  # estimating diameter in 1.3m height, as the diameters were measured at the top of the stum which is < 1.3m 
                                  # estimating diameter via TapeS doesn´t work because on of the input variables is the tree height which is below 1.3m so it keeps returning 0 
                                  # https://stackoverflow.com/questions/22104774/how-to-initialize-a-vector-with-fixed-length-in-r
                                  #DBH_cm = tprDiameter(tprTrees(spp = tpS_ID, Dm = as.list(D_cm), Hm = as.list(D_h_m), Ht = , inv = 4), Hx = rep(1.3, nrow(DW_total %>% filter(DW_type == 4 & dec_type_BWI < 3 & !is.na(D_h_m)))), cp=FALSE), 
                                  # thus we are switching to the BWI taper formula (BWI methodikband chap.5.2.2) dz = d + 2((hd − 130)/tan α)         
                                  DBH_cm_BWI = (D_mm-2*((D_h_cm-130)/tan(40)))/10, 
                                  # as the BWI taper formula returns negative values we´ll use a formula to estimate the DBH direktly by KUBLIN (BWI methodikband chap.5.2.2 ): dz = d ∗ (1.0 + (0.0011 ∗ (hd − 130)))
                                  DBH_cm_Kublin = (D_mm*(1.0+(0.0011*(D_h_cm-130))))/10,
                                  H_tps_Kublin = estHeight(d13 = DBH_cm_Kublin, sp = SP_dw_tps), 
                                  tapeS_DBH_cm = tprDiameter(tprTrees(spp = SP_dw_tps, 
                                                                      Dm = as.list(DBH_cm_Kublin), 
                                                                      Hm = as.list(D_h_m), 
                                                                      Ht = H_tps_Kublin, inv = 4), 
                                                             Hx = rep(1.3, nrow(DW_total %>% filter(DW_type == 4 & dec_type_BWI < 3 & L_m > 1.3))), cp=FALSE),
                                  DBH_mm_Kublin = D_mm*(1.0+(0.0011*(D_h_cm-130))), 
                                  DBH_h_m = 1.3) %>% 
                           # estimating height for Tapes for D_g deadwood species D_g 
                           left_join(., DW_total %>%
                                       filter(DW_type == 4 & dec_type_BWI < 3  & L_m > 1.3) %>%
                                       # calculate DBH via Kublin 
                                       # --> estimate height via TapeS with Kublin 
                                       # --> use that height to calcualte new DBH in  tapeS 
                                       # --> calculate D_g grouped by plot_ID and SP_dw_char
                                       # --> estimate height based on the tapes_D_g
                                       mutate(BA_m2 = (D_m/2)^2*pi, 
                                              D_mm = D_cm*10, 
                                              DBH_cm_Kublin = (D_mm*(1.0+(0.0011*(D_h_cm-130))))/10,
                                              H_tps_Kublin = estHeight(d13 = DBH_cm_Kublin, sp = SP_dw_tps), 
                                              tapeS_DBH_cm = tprDiameter(tprTrees(spp = SP_dw_tps, 
                                                                                  Dm = as.list(DBH_cm_Kublin), 
                                                                                  Hm = as.list(D_h_m), 
                                                                                  Ht = H_tps_Kublin, inv = 4), 
                                                                         Hx = rep(1.3, nrow(DW_total %>% filter(DW_type == 4 & dec_type_BWI < 3 & L_m > 1.3))), cp=FALSE)) %>% 
                                       group_by(plot_ID, SP_group_char) %>% 
                                       summarise(dw_D_g = ((sqrt((mean(BA_m2)/pi)))*2)*100,                # Durchmesser des Grundflächenmittelstammes; *1000 to get from 1m -> 100cm -> 1000mm
                                                 mean_L = mean(L_m),
                                                 SP_dw_tps = mean(SP_dw_tps)) %>%                          # this is just to keep the species group
                                       mutate(dw_H_dg_tapes = estHeight(d13 = dw_D_g, sp = SP_dw_tps)) %>% 
                                       select(plot_ID, SP_group_char, mean_L, dw_H_dg_tapes, dw_D_g),    # estimate height for D_g of deadwood by deadwood species group                      
                                     by = c("plot_ID", "SP_group_char")) %>%
                           mutate(dw_H_dg_tapes = case_when(D_h_m > dw_H_dg_tapes & L_m > D_h_m & L_m > mean_L & L_m > dw_H_dg_tapes ~ L_m,
                                                            D_h_m > dw_H_dg_tapes & mean_L > D_h_m & L_m < mean_L & mean_L > dw_H_dg_tapes ~ mean_L,
                                                            TRUE ~ dw_H_dg_tapes)) %>%
                           # if there are still rows were the DBH measurment height exceeds the estiamted height they are excluded --> in this case 1
                           filter(DBH_h_m < dw_H_dg_tapes) %>% 
                           mutate(tapeS_wood = tapes_stwB(SP_dw_tps, dw_D_g, DBH_h_m, dw_H_dg_tapes), 
                                  tapeS_bark = tapes_stwbB(SP_dw_tps, dw_D_g, DBH_h_m, dw_H_dg_tapes), 
                                  dw_tapes_b_ratio = rdB_DW(tapeS_bark, SP_dec_type)/rdB_DW(tapeS_wood, SP_dec_type)) %>%  
                           # L> transforming tapeS biomass (for living trees)into tapeS biomass for dead trees via relative density (rdB_DW)
                           dplyr::select(plot_ID, t_ID, DW_type, dec_type_BWI, CCS_nr, tapeS_wood, tapeS_bark,  dw_tapes_b_ratio))), 
                      by = c("plot_ID", "t_ID", "CCS_nr", "DW_type", "dec_type_BWI")) %>% 
  #  volume, biomass, carbon
  mutate(V_dw_meth = ifelse(DW_type %in% c(1, 6, 4) | DW_type == 3 & L_m < 3, "V_DW_T1463", "V_DW_T253"),
         V_dw_m3 = ifelse(DW_type %in% c(1, 6, 4) | DW_type == 3 & L_m < 3, V_DW_T1463(D_m, L_m), V_DW_T253(tpS_ID, D_cm, D_h_cm, L_m)),
         B_dw_kg = B_DW(V_dw_m3, SP_dec_type), 
         C_dw_kg = C_DW(V_dw_m3, SP_dec_type))%>% 
  # TapeS compartiments methods to be able to subset dataset and apply functions separately 
  mutate(dw_tapes_swB_meth = case_when(DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m  > 1.3 & !is.na(D_h_m)  ~ "yes", 
                                       DW_type == 3 & dec_type_BWI < 3 & !is.na(dw_tapes_b_ratio) ~ "sw_tapeS_wood",
                                       TRUE ~ "no"),
         dw_tapes_swbB_meth = case_when(DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m  > 1.3 & !is.na(D_h_m) ~ "yes",
                                        DW_type == 3 & dec_type_BWI < 3 & !is.na(dw_tapes_b_ratio) ~ "sw_tapeS_bark",
                                        TRUE ~ "no"),
         dw_tapes_stwB_meth =  case_when(DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m  > 1.3 & !is.na(D_h_m) ~ "yes",
                                         DW_type == 4 & dec_type_BWI < 3 & !is.na(dw_tapes_b_ratio) ~ "st_tapeS_wood",
                                         TRUE ~ "no"),
         dw_tapes_stwbB_meth = case_when(DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m  > 1.3 & !is.na(D_h_m) ~ "yes",
                                         DW_type == 4 & dec_type_BWI < 3 & !is.na(dw_tapes_b_ratio) ~ "st_tapeS_bark",
                                         TRUE ~ "no"),
         dw_tapes_fwB_meth = case_when(DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m  > 1.3 ~ "yes",
                                       # I have to add fine wood for BWI_dec_type 2 too,
                                       # but to be able to calculate the whole biomass stepwise, 
                                       # but I will not add it to the final dw_kg column
                                       TRUE ~ "no"))

# DBH_cm_Kublin = (D_mm*(1.0+(0.0011*(D_h_cm-130))))/10,               # a) estiamting diameter with Kublin because it doesnt require height
# H_tps_Kublin = estHeight(d13 = DBH_cm_Kublin, sp = SP_dw_tps),       # b) estimate height with TapeS based on Kublin
# tapeS_DBH_cm = tprDiameter(tprTrees(spp = SP_dw_tps,                 # c) estimate TapeS DBH with TapeS height based on Kublin
#                                     Dm = as.list(DBH_cm_Kublin),
#                                     Hm = as.list(D_h_m), 
#                                     Ht = H_tps_Kublin, inv = 4), 
#                            Hx = rep(1.3, nrow(DW_total %>% filter(DW_type == 4 & dec_type_BWI < 3 & L_m > 1.3))), cp=FALSE),





# calculating DW compartiments --> originally with dw_compartiemnt method





# TapeS compartiments methods to be able to subset dataset and apply functions separately 
# mutate(dw_tapes_swB_meth = case_when(DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m  > 1.3 & !is.na(D_h_m)  ~ "yes", 
#                                      DW_type == 3 & dec_type_BWI < 3 & !is.na(dw_tapes_b_ratio) ~ "sw_tapeS_wood",
#                                      TRUE ~ "no"),
#        dw_tapes_swbB_meth = case_when(DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m  > 1.3 & !is.na(D_h_m) ~ "yes",
#                                       DW_type == 3 & dec_type_BWI < 3 & !is.na(dw_tapes_b_ratio) ~ "sw_tapeS_bark",
#                                       TRUE ~ "no"),
#        dw_tapes_stwB_meth =  case_when(DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m  > 1.3 & !is.na(D_h_m) ~ "yes",
#                                        DW_type == 4 & dec_type_BWI < 3 & !is.na(dw_tapes_b_ratio) ~ "st_tapeS_wood",
#                                        TRUE ~ "no"),
#        dw_tapes_stwbB_meth = case_when(DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m  > 1.3 & !is.na(D_h_m) ~ "yes",
#                                        DW_type == 4 & dec_type_BWI < 3 & !is.na(dw_tapes_b_ratio) ~ "st_tapeS_bark",
#                                        TRUE ~ "no"),
#        dw_tapes_fwB_meth = case_when(DW_type %in% c(2, 5) & dec_type_BWI < 3  & L_m  > 1.3 ~ "yes",
#                                      # I have to add fine wood for BWI_dec_type 2 too,
#                                      # but to be able to calculate the whole biomass stepwise, 
#                                      # but I will not add it to the final dw_kg column
#                                      TRUE ~ "no"), 
#                           # for decay type 1 I have to add the Biomass of the fine wood compartment to get the total biomass because 
#                           # the B formula only covers stem and stump
#                           # and this helps with pivoting later
#        dw_tapes_ag_meth = case_when( DW_type %in% c(2, 5) & dec_type_BWI == 1  & L_m  > 1.3 ~ "B + fw", 
#                                     TRUE ~ "B")) 



# ----- 2.2.2. Deadwood compartiment biomass, nitrogen stock ---------------------------------------------
# addding compartiments biomasses to DW_total via filtered datasets
DW_total <- DW_total %>% 
  # TapeS compartiment biomass 
  # 1. solid wood 
  left_join(.,
            rbind(
              # dataset with whole trees in low satates of decay
              DW_total %>% 
                filter(dw_tapes_swB_meth == "yes") %>% 
                mutate(dw_tps_sw_kg = rdB_DW(tapes_swB(tpS_ID, D_cm, D_h_m, L_m), SP_dec_type)) %>% 
                dplyr::select(plot_ID, t_ID, DW_type, dec_type_BWI, CCS_nr, dw_tapes_swB_meth, dw_tps_sw_kg), 
              # dataset broken trees with sw comapartiment
              DW_total %>% 
                filter(dw_tapes_swB_meth == "sw_tapeS_wood") %>% 
                mutate(dw_tps_sw_kg = rdB_DW(tapeS_wood, SP_dec_type)) %>% 
                dplyr::select(plot_ID, t_ID, DW_type, dec_type_BWI, CCS_nr, dw_tapes_swB_meth, dw_tps_sw_kg),
              # dataset without sw compartiment
              DW_total %>% 
                filter(dw_tapes_swB_meth == "no") %>% 
                mutate(dw_tps_sw_kg = 0) %>% 
                dplyr::select(plot_ID, t_ID, DW_type, dec_type_BWI, CCS_nr, dw_tapes_swB_meth, dw_tps_sw_kg)),
            by = c("plot_ID", "t_ID", "DW_type", "dec_type_BWI", "CCS_nr", "dw_tapes_swB_meth")) %>% 
  # 2. solid wood bark
  left_join(.,
            rbind(
              # dataset with whole trees in low satates of decay
              DW_total %>% 
                filter(dw_tapes_swbB_meth == "yes") %>% 
                mutate(dw_tps_swb_kg = rdB_DW(tapes_swbB(tpS_ID, D_cm, D_h_m, L_m), SP_dec_type)) %>% 
                dplyr::select(plot_ID, t_ID, DW_type, dec_type_BWI, CCS_nr, dw_tapes_swbB_meth, dw_tps_swb_kg), 
              # dataset broken trees with swb comapartiment --> sw was calcualted via solid wood bark ratio*biomass and relative density
              DW_total %>% 
                filter(dw_tapes_swbB_meth == "sw_tapeS_bark") %>% 
                mutate(dw_tps_swb_kg = rdB_DW(tapeS_bark, SP_dec_type)) %>% 
                dplyr::select(plot_ID, t_ID, DW_type, dec_type_BWI, CCS_nr, dw_tapes_swbB_meth, dw_tps_swb_kg),
              # dataset without swb compartiment --> sw = 0
              DW_total %>% 
                filter(dw_tapes_swbB_meth == "no") %>% 
                mutate(dw_tps_swb_kg = 0) %>% 
                dplyr::select(plot_ID, t_ID, DW_type, dec_type_BWI, CCS_nr, dw_tapes_swbB_meth, dw_tps_swb_kg)),
            by = c("plot_ID", "t_ID", "DW_type", "dec_type_BWI", "CCS_nr", "dw_tapes_swbB_meth")) %>%
  # 3. stump wood
  left_join(.,
            rbind(
              # dataset with whole trees in low states of decay -> stw calculated via tape S
              DW_total %>% 
                filter(dw_tapes_stwB_meth == "yes") %>% 
                mutate(dw_tps_stw_kg =  rdB_DW(tapes_stwB(tpS_ID, D_cm, D_h_m, L_m), SP_dec_type)) %>% 
                dplyr::select(plot_ID, t_ID, DW_type, dec_type_BWI, CCS_nr, dw_tapes_stwB_meth, dw_tps_stw_kg), 
              # dataset broken trees with stw comapartiment --> stw was calcualted via: biomass - (stump bark ratio*biomass)* relative density
              DW_total %>% 
                filter(dw_tapes_stwB_meth == "st_tapeS_wood") %>% 
                mutate(dw_tps_stw_kg = rdB_DW(tapeS_wood, SP_dec_type)) %>% 
                dplyr::select(plot_ID, t_ID, DW_type, dec_type_BWI, CCS_nr, dw_tapes_stwB_meth, dw_tps_stw_kg),
              # dataset without stw compartiment stwb = 0 
              DW_total %>% 
                filter(dw_tapes_stwB_meth == "no") %>% 
                mutate(dw_tps_stw_kg = 0) %>% 
                dplyr::select(plot_ID, t_ID, DW_type, dec_type_BWI, CCS_nr, dw_tapes_stwB_meth, dw_tps_stw_kg)), 
            by = c("plot_ID", "t_ID", "DW_type", "dec_type_BWI", "CCS_nr", "dw_tapes_stwB_meth")) %>%
  # 4. solid wood bark
  left_join(.,
            rbind(
              # dataset with whole trees in low states of decay
              DW_total %>% 
                filter(dw_tapes_stwbB_meth == "yes") %>% 
                mutate(dw_tps_stwb_kg = rdB_DW(tapes_stwbB(tpS_ID, D_cm, D_h_m, L_m), SP_dec_type)) %>% 
                dplyr::select(plot_ID, t_ID, DW_type, dec_type_BWI, CCS_nr, dw_tapes_stwbB_meth, dw_tps_stwb_kg), 
              # dataset stumps with stb comapartiment
              DW_total %>% 
                filter(dw_tapes_stwbB_meth == "st_tapeS_bark") %>% 
                mutate(dw_tps_stwb_kg = rdB_DW(tapeS_bark, SP_dec_type)) %>% 
                dplyr::select(plot_ID, t_ID, DW_type, dec_type_BWI, CCS_nr, dw_tapes_stwbB_meth, dw_tps_stwb_kg),
              # dataset without swb compartiment
              DW_total %>% 
                filter(dw_tapes_stwbB_meth == "no") %>% 
                mutate(dw_tps_stwb_kg = 0) %>% 
                dplyr::select(plot_ID, t_ID, DW_type, dec_type_BWI, CCS_nr, dw_tapes_stwbB_meth, dw_tps_stwb_kg)), 
            by = c("plot_ID", "t_ID", "DW_type", "dec_type_BWI", "CCS_nr", "dw_tapes_stwbB_meth")) %>%
  # 5. fine wood including bark 
  left_join(., 
            rbind(
              # dataset with whole trees in low satates of decay
              DW_total %>% 
                filter(dw_tapes_fwB_meth == "yes") %>% 
                mutate(dw_tps_fw_kg =  rdB_DW(tapes_brB(tpS_ID, D_cm, D_h_m, L_m), SP_dec_type)) %>% 
                dplyr::select(plot_ID, t_ID, DW_type, dec_type_BWI, CCS_nr, dw_tapes_fwB_meth, dw_tps_fw_kg), 
              # dataset without fine wood compartimenz
              DW_total %>% 
                filter(dw_tapes_fwB_meth == "no") %>% 
                mutate(dw_tps_fw_kg = 0) %>% 
                dplyr::select(plot_ID, t_ID, DW_type, dec_type_BWI, CCS_nr, dw_tapes_fwB_meth, dw_tps_fw_kg)), 
            by = c("plot_ID", "t_ID", "DW_type", "dec_type_BWI", "CCS_nr", "dw_tapes_fwB_meth")) %>%
  # 6. foliage biomass (just pro forma, to deduct from the whole tree biomass, bec ause tapeS always creates a whole tree) 
  left_join(., 
            rbind(
              # dataset with whole trees in low satates of decay
              DW_total %>% 
                filter(dw_tapes_fwB_meth == "yes") %>% 
                mutate(dw_tps_f_kg =  rdB_DW(tapes_fB(tpS_ID, D_cm, D_h_m, L_m), SP_dec_type)) %>% 
                dplyr::select(plot_ID, t_ID, DW_type, dec_type_BWI, CCS_nr, dw_tapes_fwB_meth, dw_tps_f_kg), 
              # dataset without foliage compartimenz
              DW_total %>% 
                filter(dw_tapes_fwB_meth == "no") %>% 
                mutate(dw_tps_f_kg = 0) %>% 
                dplyr::select(plot_ID, t_ID, DW_type, dec_type_BWI, CCS_nr, dw_tapes_fwB_meth, dw_tps_f_kg)), 
            by = c("plot_ID", "t_ID", "DW_type", "dec_type_BWI", "CCS_nr", "dw_tapes_fwB_meth")) %>%
  # stepwise cacluation of compartiemnt biomass vie bio - tapeS 
  # solid wood
  mutate(dw_swB_kg = case_when(dw_tapes_swB_meth == "yes"  ~ (B_dw_kg + dw_tps_fw_kg + dw_tps_f_kg) - (dw_tps_swb_kg + dw_tps_stw_kg + dw_tps_stwb_kg),
                               dw_tapes_swB_meth == "sw_tapeS_wood" ~ B_dw_kg-(B_dw_kg*(dw_tps_swb_kg/dw_tps_sw_kg)),
                               TRUE ~ B_dw_kg), 
         # solid wood bark
         dw_swbB_kg = case_when(dw_tapes_swbB_meth == "yes"  ~ (B_dw_kg + dw_tps_fw_kg + dw_tps_f_kg) -(dw_swB_kg + dw_tps_stw_kg + dw_tps_stwb_kg),
                                dw_tapes_swbB_meth == "sw_tapeS_bark" ~ B_dw_kg- dw_swB_kg,
                                TRUE ~ 0),
         # stump wood
         dw_stwB_kg = case_when(dw_tapes_stwB_meth == "yes"  ~ (B_dw_kg + dw_tps_fw_kg + dw_tps_f_kg) -(dw_swB_kg + dw_swbB_kg + dw_tps_stwb_kg),
                                dw_tapes_stwB_meth == "st_tapeS_wood" ~ B_dw_kg- (B_dw_kg*(dw_tps_stwb_kg/dw_tps_stw_kg)),
                                TRUE ~ 0), 
         # stump wood bark
         dw_stwbB_kg = case_when(dw_tapes_stwbB_meth == "yes"  ~ (B_dw_kg + dw_tps_fw_kg + dw_tps_f_kg) -(dw_swB_kg + dw_swbB_kg + dw_stwB_kg),
                                 dw_tapes_stwbB_meth == "st_tapeS_bark" ~ B_dw_kg- dw_stwB_kg,
                                 TRUE ~ 0),
         # fine wood
         dw_fwB_kg = case_when(dw_tapes_fwB_meth == "yes"  ~  dw_tps_fw_kg,
                               dw_tapes_fwB_meth == "yes" & dec_type_BWI == 2 ~ 0,
                               TRUE ~ 0),
         # aboveground 
         dw_ag_kg = case_when(DW_type %in% c(2, 5) & dec_type_BWI == 1 & L_m  > 1.3 & !is.na(D_h_m) ~ B_dw_kg + dw_fwB_kg, # for not decayed standng and lying whole trees we have to add the fine wood mass 
                              TRUE ~ B_dw_kg)) %>%
  mutate(N_dw_fw_kg = N_fw(dw_fwB_kg, N_SP_group), 
         N_dw_sw_kg = N_sw(dw_swB_kg, N_SP_group), 
         N_dw_swb_kg = N_swb(dw_swbB_kg, N_SP_group), 
         N_dw_stw_kg = N_swb(dw_stwB_kg, N_SP_group),
         N_dw_stwb_kg = N_swb(dw_stwbB_kg, N_SP_group))%>% 
  mutate(ag_N_dw_kg = N_dw_fw_kg + N_dw_sw_kg + N_dw_swb_kg + N_dw_stw_kg + N_dw_stwb_kg) 
# ---- N. 5 REGENERTAION plotwise -----------------------------------------

# reffer values per plot to hectar --> plot size is still missing
# should be linked to the distance to the sampling circle centre and the distance of the furthest plant in the RG sampling circle
# %>%    # plot area in hectare to calculate BA per ha
mutate(BA_m2ha = BA_m2/plot_A_ha,      # calculate BA per species per plot in m2/ ha --> I don´t know how big the circles are :/
       C_aB_t_ha = C_aB_t/plot_A_ha, 
       C_bB_t_ha = C_bB_t/plot_A_ha,
       C_tot_t_ha = C_tot_t/ plot_A_ha, 
       N_aB_t_ha = N_aB_t/plot_A_ha)


# dataset with total BA per plot
RG_total %>%
  group_by(plot_ID) %>%                         # group by plot to calculate total BA per plot
  summarise(tot_BA_plot = sum(BA_m2),           # calculate total BA per plot in m2 by summarizing the BA of individual trees after grouping the dataset by plot
            plot_A_ha = mean(plot_A_ha)) %>%    # plot area in hectare to calculate BA per ha
  mutate(tot_BA_m2ha = tot_BA_plot/plot_A_ha) %>%  # calculate total BA per plot in m2 per hectare by dividing total BA m2/plot by area plot/ha 
#by=c("plot_ID", "plot_A_ha")) %>% 
  select(- c(plot_A_ha, tot_BA_plot)) %>%  # remove unnecessary variables
  mutate(BA_SP_per = (SP_BA_m2ha/tot_BA_m2ha)*100) %>%   # calculate proportion of each species to total BA in percent
  left_join(., trees_total_5 %>%
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



# ----- N. 5 plausibility -------------------------------------------------------

# ----- N. 4.1.2.1. carbon BWI comparisson living trees plausibility test ------------------------------------
# ----- N. 4.1.2.1.1. check if calcualtion method matters by comparing GHG, pure tapeS and tapeS+modelled heights ------------------------------------
# by species and age class
c_comp_Momok_BWI_SP_A <- trees_total_5 %>%
  filter(compartiment=="ag") %>% 
  mutate(B_kg_tapes = tprBiomass(tprTrees(spp = tpS_ID,                                         
                                          Dm = as.list(DBH_cm),
                                          Hm = as.list(DBH_h_m ),
                                          Ht = estHeight(d13 = (DBH_cm), sp = tpS_ID),
                                          inv = 4), component = "agb"),
         C_t_tps_H_tps = (B_kg_tapes/1000)*0.5) %>% 
  group_by(plot_ID, BWI_SP_group, age) %>% 
  summarise(C_t_tapes = sum(C_t_tapes),
            C_t_H_tps = sum(C_t_tps_H_tps),
            C_aB_t_GHG = sum(C_aB_t_GHG),
            plot_A_ha = mean(plot_A_ha),
            Nt = n()) %>% 
  mutate(C_t_ha_tapes= C_t_tapes/ plot_A_ha, 
         C_t_ha_tps_H= C_t_tapes/ plot_A_ha,
         C_t_ha_GHG = C_aB_t_GHG/ plot_A_ha,
         Nt_ha = Nt/ plot_A_ha,
         BWI_SP_group = toupper(BWI_SP_group), 
         age_class = case_when(!is.na(age) ~ cut(age,         # cut the diameter
                                                 breaks = c(seq(1, 180, by = 20), Inf),  # in sequences of 5
                                                 labels = labs_age,                        # and label it according to labs (1.4.1)
                                                 right = FALSE),
                               TRUE ~ 'all')) %>%
  group_by(BWI_SP_group, age_class) %>% 
  summarise(C_t_ha_tapes = mean(C_t_ha_tapes),
            C_t_ha_tps_H = mean(C_t_ha_tps_H),
            C_t_ha_GHG = mean(C_t_ha_GHG)) %>%  
  left_join(.,BWI_C_age_SP, 
            by = c("BWI_SP_group" ,"age_class")) %>% 
  mutate(C_diff_tps_BWI = C_t_ha_tapes-C_t_ha_BWI,
         C_diff_tpsH_BWI = C_t_ha_tps_H-C_t_ha_BWI,
         C_diff_GHG_BWI = C_t_ha_GHG-C_t_ha_BWI)
summary(c_comp_Momok_BWI_SP_A)


# ----- N.6 workdays ------------------------------------------------------
# total working days 2023 Brandenburg from February onwards: 
tot_wd = 229
ho_wd = tot_wd*0.5
already_used_ho_02 = 6
already_used_ho_03 = 7
already_used_ho_04 = 9
already_used_ho = already_used_ho_02 + already_used_ho_03 + already_used_ho_04
#ho_planned_04_spain = 5 --> in used for april
ho_planned_0809_FR_SP = 10
ho_planned_12_warm = 10
ho_planned_12_christmas = 5
ho_planned_tot = ho_planned_0809_FR_SP + ho_planned_12_warm + ho_planned_12_christmas
weeks_away = 1+2+2+1 # weeks taht i am spending all days in homeoffice
remainung_ho_days <- ho_wd - (already_used_ho + ho_planned_tot)
current_KW_week <- 17 # 24.04-31.04.
rem_ho_days_week <- remainung_ho_days/(52 - (current_KW_week + weeks_away))




# N.7 co2 calculations ----------------------------------------------------

CO2_anteil_atmosphere = 0.035 #% # Quelle: https://www.noaa.gov/jetstream/atmosphere
CO2_anteil_atmosphere_mensch = 0.012 #2% # Quelle: https://www.umweltbundesamt.de/service/uba-fragen/ist-nicht-der-menschliche-beitrag-treibhauseffekt
CO2_anteil_deutschland_gloable_emissionen = 0.02 # 2% Quelle: https://www.klimafakten.de/behauptungen/behauptung-deutschland-verursacht-nur-rund-zwei-prozent-des-weltweiten-co2-ausstosses#lang

CO2_einflus_deutschland_auf_globalen_CO2_anteil_in_Atmosphäre = CO2_anteil_atmosphere*CO2_anteil_atmosphere*CO2_anteil_deutschland_gloable_emissionen

0.0000245