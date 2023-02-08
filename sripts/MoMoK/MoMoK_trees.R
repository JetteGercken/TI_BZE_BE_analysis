# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the peat land soil inventory
# Trees 


# ----- 0. SETUP ---------------------------------------------------------------
# ----- 0.1. Packages  ---------------------------------------------------------
# install.packages("usethis")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("here")
install.packages("tibble")
install.packages("")
install.packages("dplyr")
install.packages("stargazer")

# ----- 0.2. library   ---------------------------------------------------------
library("usethis")
library("tidyverse")
library("ggplot2")
library("here")
library("tibble")
library("dplyr")
library("stargazer")

# ----- 0.3. working directory -------------------------------------------------
here::here()
getwd()

# ----- 1. DATA ----------------------------------------------------------------
# ----- 1.1. import ------------------------------------------------------------
# as the CSVs come from excel with German settings, the delimiter is ';' and the decimals are separated by ','
# which is why I use "delim" to import the data: https://biostats-r.github.io/biostats/workingInR/005_Importing_Data_in_R.html

trees_total <- read_delim(file = here("data/input/trees_MoMoK_total.csv"))
colnames(trees_total) <- c("MoMoK_nr", "location_name", "state", "date", "CCS_nr", 
                           "t_ID", "st_ID", "pieces", "SP_nr", "SP_code", "layer", 
                           "Kraft", "age", "age_m", "DBH_mm", "DBH_h_cm", 
                           "DBH_p_mm", "DBH_class", "H_dm", "CH_dm", 
                           "azimut_g", "azimut_d", "dist_m")


# general structure to import stuff with here:  data <- read_csv(here("datafolder", "subfolder", "datafile.csv"))
#, header = TRUE, sep = ";", dec = ",")




# ----- NOTES ------------------------------------------------------------------

