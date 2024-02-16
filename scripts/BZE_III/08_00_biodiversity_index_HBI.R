# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the  national soil inventory
# biodiverseity index
# HBI

# 1) quadratic mean diameter at breast height (DBH)
# 2) standard deviation of DBH
# 3) standard deviation of stand height
# 4) number of decay classes
# 5) bark-diversity index
# 6) trees with DBH ≥ 40 cm
# 7) diversity of flowering and fructification
# 8) average mean diameter of downed deadwood
# 9) mean DBH of standing deadwood
# 10) treespecies richness and 
# 11) tree species richness in the regeneration layer


# 0.SETUP --------------------------------------------------------------------------------------------------------------------
# 0.1. packages and functions -------------------------------------------------------------------------------------------------
source(paste0(getwd(), "/scripts/01_00_functions_library.R"))


# 0.2. working directory ------------------------------------------------------------------------------------------------------
here::here()
out.path.BZE3 <- ("output/out_data/out_data_BZE/") 


# 0.3 import data --------------------------------------------------------------------------------------------------------------
# livign trees
trees_data <- read.delim(file = here(paste0(out.path.BZE3, "HBI_LT_update_4.csv")), sep = ";", dec = ",")
trees_summary <-  read.delim(file = here(paste0(out.path.BZE3, trees_data$inv[1], "_LT_stocks_ha_all_groups.csv")), sep = ";", dec = ",")
# regeneration 
RG_data <- read.delim(file = here(paste0(out.path.BZE3, trees_data$inv[1], "_RG_update_4.csv")), sep = ";", dec = ",")
RG_summary <-  read.delim(file = here(paste0(out.path.BZE3, trees_data$inv[1], "_RG_stocks_ha_all_groups.csv")), sep = ";", dec = ",")
# deadwood
DW_data <- read.delim(file = here(paste0(out.path.BZE3, trees_data$inv[1], "_DW_update_4.csv")), sep = ";", dec = ",")
DW_summary <-  read.delim(file = here(paste0(out.path.BZE3, trees_data$inv[1], "_DW_stocks_ha_all_groups.csv")), sep = ";", dec = ",")



# 1. calculations -----------------------------------------------------------------------------------------------------------------------

# 1.1. living trees -------------------------------------------------------------------------------------------------------------------

# 1.1.1. quadratic mean diameter at breast height (DBH) ---------------------------------------------------------------------------------
trees_data %>% 

















