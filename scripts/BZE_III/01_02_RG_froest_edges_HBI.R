# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the  national soil inventory
# forest edges processign for regeneration 

# ----- 0. SETUP ---------------------------------------------------------------

# ----- 0.1. packages and functions --------------------------------------------
source(paste0(getwd(), "/scripts/00_functions_library.R"))

# ----- 0.2. working directory -------------------------------------------------
here::here()
getwd()

out.path.BZE3 <- ("output/out_data/out_data_BZE/") 


# ----- 0.3 data import --------------------------------------------------------
# regeneration
# this dataset contains the plant specific inventory data of the regenertaion inventory of the HBI (BZE2) 
HBI_RG <- read.delim(file = here("data/input/BZE2_HBI/bejb.csv"), sep = ",", dec = ",") %>% mutate(inv_year = 2012)
# this dataset contains the position and extend of the sampling circle satelites of the regeneration inventory of the HBI (BZE2) 
HBI_RG_loc <- read.delim(file = here("data/input/BZE2_HBI/bej.csv"), sep = ",", dec = ",")





all.edges.intersection.poly <- read.delim(file = here(paste0(out.path.BZE3, inv_name(HBI_RG$inv_year[1]), "_all_edges_intersection_poly.csv")), sep = ";", dec = ".")



# 0.4 data prep: harmonise strings, assign columnnames etc. ---------------------------------------------------------------------
# assign column names 
                        # bund_nr     pk_nr      pk_richtung     pk_dist     pk_aufnahme      pk_maxdist
colnames(HBI_RG_loc) <- c("plot_ID", "CCS_nr", "CCS_position",  "CCS_dist", "RG_inv_status", "CCS_max_dist")
                    #  "bund_nr"  "pk_nr"  "lfd_nr"   "bart"  "hoehe"    "grklasse"
colnames(HBI_RG) <- c("plot_ID", "CCS_no", "t_ID", "SP_code", "H_cm", "D_class_cm", "inv_year")


# 1. calculations ---------------------------------------------------------


# 1.1. assign gon or degrees to exposition --------------------------------
HBI_RG_loc %>% 
  mutate(CCS_gon = case_when(CCS_expo == "n" ~ 0,
                             CCS_expo == "w" ~ 100,
                             CCS_expo == "s" ~ 200,
                             CCS_expo == "o" ~ 300), 
         x_CCS_center = coord(0, 0, CCS_dist, CCS_gon, coordinate = "x"), 
         y_CCS_center = coord(0, 0, CCS_dist, CCS_gon, coordinate = "y"))




