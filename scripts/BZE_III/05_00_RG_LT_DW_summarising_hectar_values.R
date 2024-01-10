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
RG_stat_2 <- read.delim(file = here("output/out_data/out_data_BZE/HBI_RG_stat_2.csv"), sep = ";", dec = ",")
# this dataset contains the data of the deadwood inventory of the HBI (BZE2), including info about species groups and B, C, N stocks per tree 
DW_data <- read.delim(file = here("output/out_data/out_data_BZE/HBI_DW_update_4.csv"), sep = ";", dec = ",")
DW_stat_2 <- read.delim(file = here("output/out_data/out_data_BZE/HBI_DW_stat_2.csv"), sep = ";", dec = ",")
# this dataset contains the data of the tree inventory of the HBI (BZE2), including stand and area info,  species groups and B, C, N stocks per tree 
tree_data <- read.delim(file = here("output/out_data/out_data_BZE/HBI_LT_update_4.csv"), sep = ";", dec = ",")
LT_stat_2 <- read.delim(file = here("output/out_data/out_data_BZE/HBI_LT_stat_2.csv"), sep = ";", dec = ",")




# CALCULATIONS ------------------------------------------------------------


# 1. LIVING TREES -----------------------------------------------

# 1.1. number of trees  per hectar ----------------------------------------------
# summarize all trees per plot, no further grouping variables
LT_n_ha <-  tree_data %>% 
      filter(compartiment == "ag") %>% 
      group_by(plot_ID, CCS_r_m, plot_A_ha, inv_year) %>% 
     # sum number of trees  per sampling circuit and reffer to hectare
  reframe(n_trees_CCS_ha = n()/plot_A_ha) %>% 
      distinct() %>% 
      # now we summarise all the ha values of the cirlces per plot
      group_by(plot_ID, inv_year) %>% 
      summarise(n_ha = sum(n_trees_CCS_ha)) 

# 1.2. stocks per plot ------------------------------------------------------
if(nrow(trees_stat_2)!= 0 && isTRUE(trees_stat_2)){
  LT_stocks_ha <- rbind(tree_data  %>% 
                          group_by(plot_ID, CCS_r_m, plot_A_ha, inv_year, compartiment) %>% 
                          # convert Biomass into tons per hectar and sum it up per sampling circuit 
                          reframe(B_CCS_t_ha = sum(ton(B_kg_tree))/plot_A_ha, # plot are is the area of the respecitve samplign circuit in ha 
                                  C_CCS_t_ha = sum(ton(C_kg_tree))/plot_A_ha,
                                  N_CCS_t_ha = sum(ton(N_kg_tree))/plot_A_ha) %>% 
                          distinct(), 
                        trees_stat_2)%>% 
    # now we summarise all the t/ha values of the cirlces per plot
    group_by(plot_ID, inv_year, compartiment) %>% 
    summarise(B_t_ha = sum(B_CCS_t_ha), 
              C_t_ha = sum(C_CCS_t_ha), 
              N_t_ha = sum(N_CCS_t_ha)) %>% 
    mutate(stand_component = "LT")}else{
      LT_stocks_ha <- tree_data %>% 
        group_by(plot_ID, CCS_r_m, plot_A_ha, inv_year, compartiment) %>% 
        # convert Biomass into tons per hectar and sum it up per sampling circuit 
        reframe(B_CCS_t_ha = sum(ton(B_kg_tree))/plot_A_ha, # plot are is the area of the respecitve samplign circuit in ha 
                C_CCS_t_ha = sum(ton(C_kg_tree))/plot_A_ha,
                N_CCS_t_ha = sum(ton(N_kg_tree))/plot_A_ha) %>% 
        distinct()%>% 
        # now we summarise all the t/ha values of the cirlces per plot
        group_by(plot_ID, inv_year, compartiment) %>% 
        summarise(B_t_ha = sum(B_CCS_t_ha), 
                  C_t_ha = sum(C_CCS_t_ha), 
                  N_t_ha = sum(N_CCS_t_ha)) %>% 
        mutate(stand_component = "LT")
    }







# 2. REGENERATION ---------------------------------------------------------
# 2.1. plot area - sum off all sampling circuits ---------------------------
# if there are plots that are labelled empty but have to included in the eare calcualtion 
if(nrow(RG_stat_2) != 0 && isTRUE(RG_stat_2)){
  RG_plot_A_ha <- rbind(RG_data %>% 
                          mutate(plot_A_ha = area_m2/10000) %>% 
                          select(plot_ID, inv_year, CCS_nr, plot_A_ha) %>% 
                          distinct(), 
                        RG_stat_2 %>% 
                          select(plot_ID, inv_year, CCS_nr, plot_A_ha)) %>% 
    group_by(plot_ID, inv_year) %>% 
    summarise(plot_A_ha = sum(plot_A_ha))}else{
      RG_plot_A_ha <- RG_data %>% 
        mutate(plot_A_ha = area_m2/10000) %>% 
        select(plot_ID, inv_year, CCS_nr, plot_A_ha) %>% 
        distinct() %>% 
        group_by(plot_ID, inv_year) %>%
        summarise(plot_A_ha = sum(plot_A_ha))
    }



# 2.1. number of RG plants  per hectar ----------------------------------------------
n_ha_RG <- RG_data %>% 
  left_join(., RG_plot_A_ha, by = c("plot_ID", "inv_year")) %>% 
  filter(compartiment == "ag") %>% 
  group_by(plot_ID, CCS_nr, plot_A_ha, inv_year) %>% 
  # sum number of trees  per sampling circuit
  summarise(n_trees_CCS = n()) %>% 
  # summarise all the tree numers values of the individual cirlces per plot and reffer them to the plot area
  group_by(plot_ID, inv_year, plot_A_ha) %>% 
  reframe(n_ha = sum(n_trees_CCS)/plot_A_ha) %>% 
  distinct()





# 3. DEADWOOD -------------------------------------------------------------
# 3.1. number of DW items per hectar ----------------------------------------------
n_ha_DW <- DW_data %>% 
  filter(compartiment == "ag") %>% 
  group_by(plot_ID, inv_year) %>% 
  reframe(n_plot = n()/plot_A_ha) %>% 
  distinct()



# 3.2. stocks per hectar deadwood -----------------------------------------
if(nrow(DW_stat_2)!=0 && isTRUE(DW_stat_2)){
  DW_stocks_ha<- rbind(DW_data %>% 
  group_by(plot_ID, plot_A_ha, inv_year, compartiment) %>% 
  # convert Biomass into tons per hectar and divide it by the plot area to calculate stock per hectar
  reframe(B_t_ha = sum(ton(B_kg_tree))/plot_A_ha, # plot are is the area of the respecitive sampling circuit in ha 
          C_t_ha = sum(ton(C_kg_tree))/plot_A_ha,
          N_t_ha = sum(ton(N_kg_tree))/plot_A_ha) %>% 
  distinct(), 
 DW_stat_2) %>% 
  mutate(stand_component = "DW")}else{
    DW_stocks_ha<- DW_data %>% 
      group_by(plot_ID, plot_A_ha, inv_year, compartiment) %>% 
      # convert Biomass into tons per hectar and divide it by the plot area to calculate stock per hectar
      reframe(B_t_ha = sum(ton(B_kg_tree))/plot_A_ha, # plot are is the area of the respecitive sampling circuit in ha 
            C_t_ha = sum(ton(C_kg_tree))/plot_A_ha,
            N_t_ha = sum(ton(N_kg_tree))/plot_A_ha) %>% 
      distinct()
  }
 
  






