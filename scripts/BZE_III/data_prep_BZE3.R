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

# BZE3_trees <- read.delim(file = here("data/input/BZE3/BZE3_trees_total.csv"), sep = ";", dec = ",")
# coeff_H_comb <- read.delim(file = here("data/input/BZE3/coeff_H_BZE.csv"), sep = ";", dec = ",")
# SP_names_com_ID_tapeS <- read.delim(file = here("output/out_data/coeff_H_BZE.csv"), sep = ";", dec = ",") 
# forest_edges <- 
   # filter for Waldrandform 
         # 1 =	L = 	Linie
         # 2 =	E	 = Eck
              # if waldrandform == 2 
                   # --> build two lin models (1) X|Y anfang, X|Y Knickpunkt, (2) (1) X|Y ende, X|Y Knickpunkt,
                   # filter for trees with x between x anfang and x Knickpunkt and x ende and x knickpunkt 
                   # check if y at respeective x is higher then y andfang or y ende
   # calculate X|Y of each tree
   # create function for forest edge 
          # X|Y of intersection with sampling circuit 
   # filter for trees with Y < Y forest edge function at given X



# REGENERATION 


# DEADWOOD



# ----- 1. joining in external info  --------------------------------------


# ----- 1.1. LIVING TREES -------------------------------------------------
 # check if there are no trees left that don´t have a SP_code in xBart/ SP_names_com_ID_tapeS


# ----- 1.2. REGENERATION -------------------------------------------------
# ----- 1.3. DEADWOOD -------------------------------------------------
