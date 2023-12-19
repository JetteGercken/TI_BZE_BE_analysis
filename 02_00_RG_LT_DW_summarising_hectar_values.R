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
RG_data <- read.delim(file = here("output/out_data/out_data_BZE/HBI_RG_update_4.csv"), sep = ";", dec = ",")
# this dataset contains the data of the deadwood inventory of the HBI (BZE2), including info about species groups and B, C, N stocks per tree 
DW_data <- read.delim(file = here("output/out_data/out_data_BZE/HBI_DW_update_4.csv"), sep = ";", dec = ",")
# this dataset contains the data of the tree inventory of the HBI (BZE2), including stand and area info,  species groups and B, C, N stocks per tree 
tree_data <- read.delim(file = here("output/out_data/out_data_BZE/HBI_LT_update_4.csv"), sep = ";", dec = ",")



# summarize all trees per plot, no further grouping variables


tree_data %>% 
  filter(compartiment == "ag") %>% 
  group_by(plot_ID, CCS_r_m, plot_A_ha, inv_year, compartiment) %>% 
  # convert Biomass into tons per hectar and sum it up per sampling circuit 
  reframe(B_CCS_t_ha = sum(ton(B_kg_tree))/plot_A_ha, # plot are is the area of the respecitve samplign circuit in ha 
            C_CCS_t_ha = sum(ton(C_kg_tree))/plot_A_ha,
            N_CCS_t_ha = sum(ton(N_kg_tree))/plot_A_ha, 
          no_trees_CCS_ha = n()/plot_A_ha) %>% 
  distinct() %>% 
  # now we summarise all the t/ha values of the cirlces per plot
  group_by(plot_ID, inv_year, compartiment) %>% 
  summarise(B_t_ha = sum(B_CCS_t_ha), 
            C_t_ha = sum(C_CCS_t_ha), 
            N_t_ha = sum(N_CCS_t_ha), 
            no_trees_ha = sum(no_trees_CCS_ha))
  
  

RG_data %>% 
  mutate(CCS_A_ha = area_m2)
  group_by(plot_ID, CCS_no, area_m2, inv_year, compartiment) %>% 
  # convert Biomass into tons per hectar and sum it up per sampling circuit 
  reframe(B_CCS_t_ha = sum(ton(B_kg_tree))/plot_A_ha, # plot are is the area of the respecitve samplign circuit in ha 
          C_CCS_t_ha = sum(ton(C_kg_tree))/plot_A_ha,
          N_CCS_t_ha = sum(ton(N_kg_tree))/plot_A_ha, 
          no_trees_CCS_ha = n()/plot_A_ha) %>% 
  distinct() %>% 
  # now we summarise all the t/ha values of the cirlces per plot
  group_by(plot_ID, inv_year, compartiment) %>% 
  summarise(B_t_ha = sum(B_CCS_t_ha), 
            C_t_ha = sum(C_CCS_t_ha), 
            N_t_ha = sum(N_CCS_t_ha), 
            no_trees_ha = sum(no_trees_CCS_ha))
  








 summary( tree_data %>% 
    filter(compartiment == "ag") %>% 
    group_by(plot_ID, CCS_r_m, plot_A_ha, inv_year, compartiment) %>% 
    # convert Biomass into tons per hectar and sum it up per sampling circuit 
    reframe(B_CCS_t_ha = sum(ton(B_kg_tree))/plot_A_ha, # plot are is the area of the respecitve samplign circuit in ha 
            C_CCS_t_ha = sum(ton(C_kg_tree))/plot_A_ha,
            N_CCS_t_ha = sum(ton(N_kg_tree))/plot_A_ha, 
            no_trees_CCS_ha = n()/plot_A_ha) %>% 
    distinct() %>% 
    # now we summarise all the t/ha values of the cirlces per plot
    group_by(plot_ID, inv_year, compartiment) %>% 
    summarise(B_t_ha = sum(B_CCS_t_ha), 
              C_t_ha = sum(C_CCS_t_ha), 
              N_t_ha = sum(N_CCS_t_ha), 
              no_trees_ha = sum(no_trees_CCS_ha)))






