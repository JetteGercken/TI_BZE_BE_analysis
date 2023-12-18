# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the  national soil inventory
# stock per hectar summarising for regeneration (RG), living trees (LT) and 
# deadwood (DW)


# ----- 0. SETUP ---------------------------------------------------------------

# ----- 0.1. packages and functions --------------------------------------------
source(paste0(getwd(), "/scripts/00_00_functions_library.R"))

# ----- 0.2. working directory -------------------------------------------------
here::here()
getwd()

out.path.BZE3 <- ("output/out_data/out_data_BZE/") 


# ----- 0.3 data import --------------------------------------------------------
# regeneration
# this dataset contains the plant specific inventory data of the regenertaion inventory of the HBI (BZE2), including stand and area info,  species groups and B, C, N stocks per tree 
RG_data <- read.delim(file = here("output/out_data/out_data_BZE/HBI_RG_update_4.csv"), sep = ",", dec = ",")%>% mutate(inv_year = 2012, inv = inv_name(inv_year)) 
# this dataset contains the data of the deadwood inventory of the HBI (BZE2), including info about species groups and B, C, N stocks per tree 
DW_data <- read.delim(file = here("output/out_data/out_data_BZE/HBI_DW_update_4.csv"), sep = ",", dec = ",")%>% mutate(inv_year = 2012, inv = inv_name(inv_year)) 
# this dataset contains the data of the tree inventory of the HBI (BZE2), including stand and area info,  species groups and B, C, N stocks per tree 
tree_data <- read.delim(file = here("output/out_data/out_data_BZE/HBI_LT_update_4.csv"), sep = ",", dec = ",")%>% mutate(inv_year = 2012, inv = inv_name(inv_year)) 
