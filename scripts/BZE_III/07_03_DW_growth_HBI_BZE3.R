# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the  national soil inventory
# growth deadwood


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
BZE3_DW <- read.delim(file = here(paste0(out.path.BZE3, "BZE3_DW_update_4.csv")), sep = ";", dec = ",")
BZE3_summary <- read.delim(file = here(paste0(out.path.BZE3, "BZE3_DW_stocks_ha_P_SP_TY.csv")), sep = ";", dec = ",")

HBI_DW <- read.delim(file = here(paste0(out.path.BZE3, "HBI_DW_update_4.csv")), sep = ";", dec = ",")
HBI_summary <- read.delim(file = here(paste0(out.path.BZE3, "HBI_DW_stocks_ha_P_SP_TY.csv")), sep = ";", dec = ",")

# 1. calculations ---------------------------------------------------------
