# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the  national soil inventory
# growth regeneration


# ----- 0. SETUP ---------------------------------------------------------------

# ----- 0.1. packages and functions --------------------------------------------
source(paste0(getwd(), "/scripts/01_00_functions_library.R"))

# ----- 0.2. working directory -------------------------------------------------
here::here()
getwd()

out.path.BZE3 <- ("output/out_data/out_data_BZE/") 

# vorräte: über alle jahre 
# einzelbaumdurchmesser
# veränderung grundflächenanteil


# 0.3. data import --------------------------------------------------------
# BZE3 Data
# this dataset contains single plant data of regeneration inventory of BZE3
BZE3_RG <- read.delim(file = here(paste0(out.path.BZE3, "BZE3_RG_update_4.csv")), sep = ";", dec = ",")
# this dataset contains regeneration data summarized per hectar BZE3
BZE3_summary <- read.delim(file = here(paste0(out.path.BZE3, "BZE3_RG_stocks_ha_all_groups.csv")), sep = ";", dec = ",")

# HBI Data
# this dataset contains single plant data of regeneration inventory of HBI
HBI_RG <- read.delim(file = here(paste0(out.path.BZE3, "HBI_RG_update_4.csv")), sep = ";", dec = ",")
# this dataset contains regeneration data summarized per hectar HBI
HBI_summary <- read.delim(file = here(paste0(out.path.BZE3, "HBI_RG_stocks_ha_all_groups.csv")), sep = ";", dec = ",")





# 1. calculations ---------------------------------------------------------
# we need:
  # -  stock changes
  # - changes in number of species
  # - changes in n per ha







