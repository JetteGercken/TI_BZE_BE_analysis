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





# ----- 2. linear regression height ---------------------------------------

# ----- 2.1. change unit height from dm to m ------------------------------
# to calculate individual tree heights I will create a linear regression for the heights

# convert height in dm to m
# https://suzan.rbind.io/2018/02/dplyr-tutorial-2/#mutate-at-to-change-specific-columns
  # msleep %>%
  #   select(name, sleep_total:awake) %>%
  #   mutate_at(vars(contains("sleep")), ~(.*60))

trees_total <- trees_total %>%
  filter(drop_na(H_dm))
  mutate(H_m = H_dm*0.1)


mutate_at(c("height", "mass"), scale2)

# ----- 2.2. create one linear model per species --------------------------








# ----- NOTES ------------------------------------------------------------------




