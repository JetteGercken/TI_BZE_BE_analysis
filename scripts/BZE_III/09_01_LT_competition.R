# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the peat land soil inventory
# competition index HBI


# The possible competition indexes: 
 # - should be age independent
 # - can be distance dependent : 
    # - hegy 
 # - can be distance indepentent
 

# ----- 0. SETUP ---------------------------------------------------------------
# ----- 0.1. packages and functions --------------------------------------------
source(paste0(getwd(), "/scripts/01_00_functions_library.R"))

# ----- 0.2. working directory -------------------------------------------------
here::here()
getwd()

out.path.BZE3 <- ("output/out_data/out_data_BZE/") 
# 0.3. data import --------------------------------------------------------