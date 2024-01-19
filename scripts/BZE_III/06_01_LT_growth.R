# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the  national soil inventory
# growth 


# ----- 0. SETUP ---------------------------------------------------------------

# ----- 0.1. packages and functions --------------------------------------------
source(paste0(getwd(), "/scripts/00_00_functions_library.R"))

# ----- 0.2. working directory -------------------------------------------------
here::here()
getwd()

out.path.BZE3 <- ("output/out_data/out_data_BZE/") 

# vorräte: über alle jahre 
# einzelbaumdurchmesser
# veränderung grundflächenanteil


# 0.3. data import --------------------------------------------------------
BZE3_trees <- read.delim(file = here("output/out_data/out_data_BZE/BZE3_LT_update_2.csv"), sep = ";", dec = ",")


# 1. calculations ---------------------------------------------------------

# 1.1. single tree growth -------------------------------------------------
# this inventory status means that the tree should have been assessed in the previous 
# invenotry but wasn´t
# thus we have to calculate how much the tree of that species at that plot would have grown 
# between the previous and current inventory, then deduct it from the diameter of the 
# respective tree in the current inventory and add the tree to the previous inventory with
# the same ID, tree status 0 and the reduced diameter
# for this inventory status 

## calculate averange annual diameter growth per single tree per species and plot 
growth_df <- left_join(
  # select trees that are repeatedly inventory, or unknown status
  BZE3_trees %>% 
    filter(tree_inventory_status %in% c(1, -9)) %>% 
    rename(BZE3_DBH_cm = DBH_cm) %>% 
    rename(BZE3_inv_year = inv_year) %>% 
    select(plot_ID, tree_ID, BZE3_inv_year, SP_code, BZE3_DBH_cm), 
  HBI_trees %>% 
    # select trees that were newly inventored, repeated inventory, or unknown status
     filter(tree_inventory_status %in% c(0, 1, -9))%>% 
     rename(HBI_DBH_cm = DBH_cm) %>% 
     rename(HBI_inv_year = inv_year) %>% 
     select(plot_ID, tree_ID, HBI_inv_year, SP_code, HBI_DBH_cm), 
   by = c("plot_ID", "tree_ID", "SP_code")) %>% 
  mutate(DBH_growth_cm = BZE3_DBH_cm - HBI_DBH_cm, 
         age_period = BZE3_inv_year- HBI_inv_year, 
         annual_growth_cm = DBH_growth_cm/age_period) %>% 
  group_by(plot_ID, SP_code) %>% 
  summarize(average_age_period_years = mean(age_period), 
            avg_annual_DBH_growth_cm = mean(annual_growth_cm))





