# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the  national soil inventory
# cheking plausibility of the data 


# ----- 0. SETUP ---------------------------------------------------------------

# ----- 0.1. packages and functions --------------------------------------------
source(paste0(getwd(), "/scripts/00_00_functions_library.R"))

# ----- 0.2. working directory -------------------------------------------------
here::here()
getwd()

out.path.BZE3 <- ("output/out_data/out_data_BZE/") 


# 0.3 import data ---------------------------------------------------------


HBI_summary <- read.delim(file = here("output/out_data/out_data_BZE/HBI_LT_stocks_ha_P_SP_TY.csv"), sep = ";", dec = ",")


# create pseudo monocultures:
# carbon stock of species per hectar divided by 1 Hektar mulitplied with the actual area covered by the species according to basal area 
# mutate(C_t_ha_mono = C_t_P_SP_ha/(1*(SP_BA_m2ha/tot_BA_m2ha)),
       