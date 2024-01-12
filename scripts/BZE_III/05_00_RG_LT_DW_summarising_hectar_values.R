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
# we have to calcualte this separately because the stocks per area are being calcualted per com
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




# 1.2. number of speices per plot -----------------------------------------



# 1.2. stocks per hektar ------------------------------------------------------
# 1.2.1. Plot: stocks per hektar ------------------------------------------------------

if(nrow(trees_stat_2)!= 0 && isTRUE(trees_stat_2)){
  LT_BCNBAn_ha <- rbind(tree_data  %>% 
                          group_by(plot_ID, CCS_r_m, inv_year, compartiment) %>% 
                          # convert Biomass into tons per hectar and sum it up per sampling circuit 
                          reframe(B_CCS_t_ha = sum(ton(B_kg_tree))/plot_A_ha, # plot are is the area of the respecitve samplign circuit in ha 
                                  C_CCS_t_ha = sum(ton(C_kg_tree))/plot_A_ha,
                                  N_CCS_t_ha = sum(ton(N_kg_tree))/plot_A_ha, 
                                  BA_CCS_m2_ha = sum(BA_m2)/plot_A_ha, 
                                  n_trees_CCS_ha = n()/plot_A_ha) %>% 
                          distinct(), 
                        trees_stat_2)%>% 
    # now we summarise all the t/ha values of the cirlces per plot
    group_by(plot_ID, inv_year, compartiment) %>% 
    summarise(B_t_ha = sum(B_CCS_t_ha), 
              C_t_ha = sum(C_CCS_t_ha), 
              N_t_ha = sum(N_CCS_t_ha), 
              BA_m2_ha = sum(BA_CCS_m2_ha), 
              n_ha = sum(n_trees_CCS_ha)) %>% 
    mutate(stand_component = "LT")}else{
      LT_BCNBAn_ha <- tree_data %>% 
        group_by(plot_ID, CCS_r_m, inv_year, compartiment) %>% 
        # convert Biomass into tons per hectar and sum it up per sampling circuit 
        reframe(B_CCS_t_ha = sum(ton(B_kg_tree))/plot_A_ha, # plot are is the area of the respecitve samplign circuit in ha 
                C_CCS_t_ha = sum(ton(C_kg_tree))/plot_A_ha,
                N_CCS_t_ha = sum(ton(N_kg_tree))/plot_A_ha, 
                BA_CCS_m2_ha = sum(BA_m2)/plot_A_ha, 
                n_trees_CCS_ha = n()/plot_A_ha) %>% 
        distinct()%>% 
        # now we summarise all the t/ha values of the cirlces per plot
        group_by(plot_ID, inv_year, compartiment) %>% 
        summarise(B_t_ha = sum(B_CCS_t_ha), 
                  C_t_ha = sum(C_CCS_t_ha), 
                  N_t_ha = sum(N_CCS_t_ha),
                  BA_m2_ha = sum(BA_CCS_m2_ha), 
                  n_ha = sum(n_trees_CCS_ha)) %>% 
        mutate(stand_component = "LT")
    }


# 1.2.2. Plot, species: stocks per hektar ------------------------------------------------------

LT_SP_BCNBA_ha <- tree_data %>% 
  group_by(plot_ID, CCS_r_m, inv_year, SP_code, compartiment) %>% 
  # convert Biomass into tons per hectar and sum it up per sampling circuit 
  reframe(B_CCS_t_ha = sum(ton(B_kg_tree))/plot_A_ha, # plot are is the area of the respecitve samplign circuit in ha 
          C_CCS_t_ha = sum(ton(C_kg_tree))/plot_A_ha,
          N_CCS_t_ha = sum(ton(N_kg_tree))/plot_A_ha, 
          BA_CCS_m2_ha = sum(BA_m2)/plot_A_ha) %>% 
  distinct()%>% 
  # now we summarise all the t/ha values of the cirlces per plot
  group_by(plot_ID, inv_year, SP_code, compartiment) %>% 
  summarise(B_t_ha = sum(B_CCS_t_ha), 
            C_t_ha = sum(C_CCS_t_ha), 
            N_t_ha = sum(N_CCS_t_ha), 
            BA_m2_ha = sum(BA_CCS_m2_ha)) %>% 
  mutate(stand_component = "LT") %>% 
  #calcualte species compostiion by calcualting the percent of the respective species contributes to the overall basal area 
  left_join(LT_BCNBA_ha %>% 
              select(plot_ID, inv_year, compartiment, BA_m2_ha) %>% 
              rename(BA_m2_ha_total = BA_m2_ha),
            by = c("plot_ID", "inv_year", "compartiment"), ) %>% 
  distinct() %>% 
  mutate(BA_percent = (BA_m2_ha/BA_m2_ha_total)*100) %>% 
  select(-"BA_m2_ha_total")

# 1.2.3. plot: species composition ------------------------------------------------------

for (i in length(unique(trees_data$plot_ID))) {
  # i = 1
  my.plot.id <- unique(trees_data$plot_ID)[i]
  my.sp.p.df <- unique(LT_SP_BCNBA_ha[LT_SP_BCNBA_ha$plot_ID == my.plot.id, ][, c("plot_ID", 
                                                                           "inv_year", 
                                                                           "SP_code",
                                                                           "BA_m2_ha", "BA_percent")])%>% 
    left_join(., 
              SP_names_com_ID_tapeS %>%
                mutate(char_code_ger_lowcase = tolower(Chr_code_ger)) %>%
                select(char_code_ger_lowcase, LH_NH, bot_genus), 
              by = c("SP_code" = "char_code_ger_lowcase"))  
  
  # calcaulte the composition / ration of coniferous and broadaleafed trees per plot  
  my.BLCF.p.df <- my.sp.p.df %>% 
    group_by(plot_ID, inv_year, LH_NH) %>% 
    summarize(BA_m2_ha = sum(BA_m2_ha), 
              BA_per_LHNH = sum(BA_percent))
    
  my.best.TY.p.df <- my.sp.p.df %>% 
    mutate(besttype_SP_group = case_when(bot_genus == "Picea" ~ "FI", 
                                         bot_genus == "Pinus" ~ "KI",
                                         !(bot_genus %in% c("Picea", "Pinus")) & LH_NH == "NB" ~ "aNH", 
                                         bot_genus == "Fagus" ~ "BU", 
                                         bot_genus == "Quercus" ~ "EI", 
                                         !(bot_genus %in% c("Fagus", "Quercus")) & LH_NH == "LB" ~ "aLH", 
                                         TRUE ~ NA)) %>% 
    group_by(plot_ID,  inv_year, besttype_SP_group, LH_NH) %>% 
    summarise(BA_percent = sum(BA_percent)) %>% 
     left_join(.,  my.sp.p.df %>% 
                group_by(plot_ID, inv_year, LH_NH) %>% 
                summarize(BA_m2_ha = sum(BA_m2_ha), 
                          BA_per_LHNH = sum(BA_percent)) %>%  
                select("plot_ID", "inv_year", "LH_NH","BA_per_LHNH" ), 
              by = c("plot_ID", "inv_year", "LH_NH")) %>% 
    mutate(besttype = (case_when(besttype_SP_group == "FI" & BA_percent >= 70 ~ "Fi-Rein" , 
                                       besttype_SP_group == "KI" & BA_percent >= 70 ~ "Ki-Rein",
                                       besttype_SP_group == "aNH" & BA_percent >= 70 ~ "sonst-Nd",
                                       besttype_SP_group == "BU" & BA_percent >= 70 ~ "Bu-Rein" , 
                                       besttype_SP_group == "EI" & BA_percent >= 70 ~ "Ei-Rein",
                                       besttype_SP_group == "aLH" & BA_percent >= 70 ~ "sonst-Ld",
                                       besttype_SP_group %in% c("BU", "EI", "aLH") & BA_percent < 70 &
                                         LH_NH == "LB" & BA_per_LHNH >30 ~ "Nd-Ld-Misch", 
                                       besttype_SP_group %in% c("FI", "KI", "aNH") & BA_percent < 70 &
                                         LH_NH == "NB" & BA_per_LHNH >30 ~ "Lb-Nd-Misch", 
                                       besttype_SP_group %in% c("FI", "KI", "aNH") & BA_percent < 70 &
                                         LH_NH == "LB" & BA_per_LHNH < 30 ~ "Nb-Lb<30", 
                                       besttype_SP_group %in% c("BU", "EI", "aLH") & BA_percent < 70 &
                                         LH_NH == "NB" & BA_per_LHNH >30 ~ "Lb-NB<30", 
                                       TRUE ~ NA)))
  
  
  
  #%>% 
    left_join(.,  my.sp.p.df %>% 
                group_by(plot_ID, inv_year, LH_NH) %>% 
                summarize(BA_m2_ha = sum(BA_m2_ha), 
                          BA_per_LHNH = sum(BA_percent)) %>%  
                select("plot_ID", "inv_year", "LH_NH","BA_per_LHNH" ), 
              by = c("plot_ID", "inv_year", "LH_NH"))  

     
           
    
  ))
  
  
}


LT_SP_composition <- 
  LT_SP_BCNBA_ha %>% filter(compartiment == "ag") %>% 
  select(plot_ID, inv_year, SP_code, BA_percent) %>% 
  distinct() %>% 
# now we try to introduce the stand type tpp 
  left_join(., 
            SP_names_com_ID_tapeS %>%
              mutate(char_code_ger_lowcase = tolower(Chr_code_ger)) %>%
              select(char_code_ger_lowcase, LH_NH, bot_genus), 
            by = c("SP_code" = "char_code_ger_lowcase"))  %>% 
  mutate(besttype_SP_group = case_when(bot_genus == "Picea" ~ "FI", 
                                       bot_genus == "Pinus" ~ "KI",
                                       !(bot_genus %in% c("Picea", "Pinus")) & LH_NH == "NB" ~ "aNH", 
                                       bot_genus == "Fagus" ~ "BU", 
                                       bot_genus == "Quercus" ~ "EI", 
                                       !(bot_genus %in% c("Fagus", "Quercus")) & LH_NH == "LB" ~ "aLH", 
                                       TRUE ~ NA)) %>% 
  group_by(plot_ID,  inv_year, besttype_SP_group, LH_NH) %>% 
  summarise(BA_percent = sum(BA_percent)) %>% 
  
  

    



# 1.3 forestry summary ----------------------------------------------------

# this shoudl contain the BA, Dg, Hg, 





  


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
 
  






