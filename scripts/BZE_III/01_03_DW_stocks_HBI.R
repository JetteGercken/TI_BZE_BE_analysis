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
HBI_DW <- read.delim(file = here("data/input/BZE2_HBI/bedw_liste.csv"), sep = ",", dec = ",")
                    #  bund_nr lfd_nr t     yp      baumgruppe anzahl  durchmesser laenge zersetzung
colnames(HBI_DW) <- c("plot_ID", "tree_ID", "dw_type", "dw_sp", "count", "d_mm", "l_dm", "decay")

# HBI point info
HBI_inv_info <- read.delim(file = here("data/input/BZE2_HBI/tit_1.csv"), sep = ",", dec = ",", stringsAsFactors=FALSE)
# HBI point/ inventory info
HBI_inv_info <- HBI_inv_info %>% dplyr::select(bund_nr, datum, status )
colnames(HBI_inv_info) <- c("plot_ID", "date", "plot_inventory_status")



# 0.4 dataprep  -----------------------------------------------------------

# 0.4.1. Inventory year & name --------------------------------------------------------
# create column that just contains year of inventory: https://www.geeksforgeeks.org/how-to-extract-year-from-date-in-r/
HBI_inv_info$date <- as.Date(HBI_inv_info$date)
HBI_inv_info$inv_year <- as.numeric(format(HBI_inv_info$date, "%Y"))
# this line can be removed later
HBI_inv_info <- HBI_inv_info %>% mutate(inv_year = ifelse(inv_year < 2012, 2012,inv_year), 
                                        inv = inv_name(inv_year))

# join inventory jear and name into deadwood tree dataset
HBI_DW <- HBI_DW %>% left_join(., HBI_inv_info %>% select(inv_year, inv, plot_ID), by = "plot_ID")

# 0.4.2. species names ----------------------------------------------------


# 1. calculations ------------------------------------------

# 1.2. volume --------------------------------------------------------------------


 



