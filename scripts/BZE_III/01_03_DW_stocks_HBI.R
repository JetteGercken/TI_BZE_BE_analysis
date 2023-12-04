# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the peat land soil inventory
# Deadwood biomass, carbon and nitrogen stock


# ----- 0. SETUP ---------------------------------------------------------------
# ----- 0.1. Packages & functions  ---------------------------------------------------------
source(paste0(getwd(), "/scripts/00_00_functions_library.R"))

# ----- 0.2. working directory -------------------------------------------------
here::here()
getwd()

out.path.BZE3 <- ("output/out_data/out_data_BZE/") 

# ----- 0.3 data import --------------------------------------------------------
# DEAD trees