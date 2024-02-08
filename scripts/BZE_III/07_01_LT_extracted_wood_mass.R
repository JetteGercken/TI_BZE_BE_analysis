# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the  national soil inventory
# extracted amount of wood 

# ----- 0. SETUP ---------------------------------------------------------------
# ----- 0.1. packages and functions --------------------------------------------
source(paste0(getwd(), "/scripts/01_00_functions_library.R"))

# ----- 0.2. working directory -------------------------------------------------
here::here()
getwd()

out.path.BZE3 <- ("output/out_data/out_data_BZE/") 
# 0.3. data import --------------------------------------------------------
# tree data
BZE3_trees <- read.delim(file = here("output/out_data/out_data_BZE/BZE3_LT_update_4.csv"), sep = ";", dec = ",")
BZE3_trees_removed <- read.delim(file = here("output/out_data/out_data_BZE/BZE3_LT_removed_2.csv"), sep = ";", dec = ",")
HBI_trees <- read.delim(file = here("output/out_data/out_data_BZE/HBI_LT_update_4.csv"), sep = ";", dec = ",")
# growth
growth <- read.delim(file = here("output/out_data/out_data_BZE/HBI_BZE3_LT_dbh_growth.csv"), sep = ";", dec = ",")


# 1. calculations ---------------------------------------------------------

# 1.1. find  trees in HBI dataset that were found removed in BZE3  --------
trees_harvest <- HBI_trees %>% 
  semi_join(., BZE3_trees_removed %>% filter(tree_inventory_status == 2), 
            by = c("plot_ID", "tree_ID"))


# 1.2. add 4 times diameter growth to the diameter of the trees removed between HBI and BZE3 --------
# we will have to build a loop that selects the growth per plot, stand, c_layer, species
# if we can´t find average growth in this group, select growth by plot, stand, species
# if we can´t find average growth in this group, select growth by plot, species
# if we can´t find average growth in this group, select growth by species






