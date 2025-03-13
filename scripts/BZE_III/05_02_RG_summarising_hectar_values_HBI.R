# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the  national soil inventory
# HBI 
# stock per hectar summarising for regeneration (RG), 



# ----- 0. SETUP ---------------------------------------------------------------

# ----- 0.1. packages and functions --------------------------------------------
source(paste0(getwd(), "/scripts/01_00_functions_library.R"))

# ----- 0.2. working directory -------------------------------------------------
here::here()
getwd()

out.path.BZE3 <- paste0(getwd(), "/output/out_data/out_data_BZE/") 

# ----- 0.3 data import --------------------------------------------------------
# regeneration
# this dataset contains the plant specific inventory data of the regenertaion inventory of the HBI (BZE2), including stand and area info,  species groups and B, C, N stocks per tree 
RG_data <- read.delim(file = paste0(out.path.BZE3, "HBI_RG_update_4.csv"),sep = ",", dec = ".")
RG_stat_2 <- read.delim(file = paste0(out.path.BZE3, RG_data$inv[1], "_RG_stat_2.csv"), sep = ",", dec = ".") %>% 
  mutate(inv = inv_name(inv_year))



# 1. calculation: REGENERATION ---------------------------------------------------------
# 2.1. area - sum off all sampling circuits ---------------------------
# 2.1.1. plot area - sum off all sampling circuits ---------------------------
# if there are plots that are labelled empty but have to included in the eare calcualtion 
if(exists('RG_stat_2') == TRUE && nrow(RG_stat_2) != 0){
  RG_plot_A_ha <- rbind(RG_data %>% 
                          mutate(plot_A_ha = as.numeric(area_m2)/10000) %>% 
                          select(plot_ID, inv, CCS_nr, plot_A_ha) %>% 
                          distinct(), 
                        RG_stat_2 %>% 
                          # this is in case in 01_00_RG_LT_DW_plot_inv_status_sorting there were stat_2 datasets produced that do not hold any data but only NAs
                          filter(!is.na(plot_ID))%>% 
                          select(plot_ID, inv, CCS_nr, plot_A_ha) %>% 
                          distinct()
  )%>% 
    select(plot_ID, inv, CCS_nr, plot_A_ha)  %>%  
    arrange(plot_ID) %>% 
    group_by(plot_ID, inv) %>% 
    summarise(plot_A_ha = sum(as.numeric(plot_A_ha)))
}else{
  RG_plot_A_ha <- RG_data %>% 
    mutate(plot_A_ha = as.numeric(area_m2)/10000) %>% 
    select(plot_ID, inv, CCS_nr, plot_A_ha) %>% 
    distinct() %>% 
    group_by(plot_ID, inv) %>%
    summarise(plot_A_ha = sum(as.numeric(plot_A_ha)))
}


# 2.1.2. stand plot area - sum off all sampling circuits per stand per plot in ha ---------------------------
# if there are plots that are labelled empty but have to included in the eare calcualtion 
if(exists('RG_stat_2') == TRUE && nrow(RG_stat_2) != 0){
  RG_stand_plot_A_ha <- plyr::rbind.fill(
    RG_data %>% 
      mutate(plot_A_ha = as.numeric(area_m2)/10000) %>% 
      select(plot_ID, inv, CCS_nr, stand, plot_A_ha) %>% 
      distinct(), 
    RG_stat_2 %>% 
      # this is in case in 01_00_RG_LT_DW_plot_inv_status_sorting there were stat_2 datasets produced that do not hold any data but only NAs
      filter(!is.na(plot_ID))%>% 
      select(plot_ID, inv, CCS_nr, plot_A_ha) %>%
      distinct()
  )%>% 
    select(plot_ID, inv, CCS_nr, stand, plot_A_ha)  %>%  
    arrange(plot_ID) %>% 
    group_by(plot_ID, stand, inv) %>% 
    summarise(stand_plot_A_ha = sum(as.numeric(plot_A_ha)))
}else{
  RG_stand_plot_A_ha <- RG_data %>% 
    mutate(plot_A_ha = as.numeric(area_m2)/10000) %>% 
    select(plot_ID, inv, CCS_nr, stand, plot_A_ha) %>% 
    distinct() %>% 
    group_by(plot_ID, stand, inv) %>% 
    summarise(stand_plot_A_ha = sum(as.numeric(plot_A_ha)))
}


# 2.1.3. join are in RG_data ----------------------------------------------
RG_data <- RG_data %>% 
  left_join(., RG_plot_A_ha, by = c("plot_ID", "inv")) %>% 
  left_join(., RG_stand_plot_A_ha, by = c("plot_ID", "inv", "stand"))


if((exists('RG_stat_2') == TRUE && nrow(RG_stat_2) != 0)==T){
  RG_stat_2 <- RG_stat_2 %>% 
    mutate(stand = NA) %>%
    rename("CCS_A_ha" = "plot_A_ha" ) %>% 
    left_join(., RG_plot_A_ha, by = c("plot_ID", "inv")) %>% 
    left_join(., RG_stand_plot_A_ha, by = c("plot_ID", "inv", "stand"))
}

# 2.2. number of RG  plants  per hectar ----------------------------------------------
RG_n_ha <- RG_data %>% 
  filter(compartiment == "ag") %>% distinct() %>% 
  group_by(plot_ID, inv) %>% 
  # sum number of trees  per sampling circuit
  reframe(n_ha = n()/plot_A_ha) %>% 
  distinct() %>% 
  mutate(stand_component = "RG")

RG_n_ha_ST <- RG_data %>% 
  filter(compartiment == "ag") %>% distinct() %>% 
  group_by(plot_ID, inv, stand) %>% 
  # sum number of trees  per sampling circuit
  reframe(n_ha = n()/stand_plot_A_ha) %>% 
  distinct() %>% 
  mutate(stand_component = "RG")


# 2.3. number of RG  species per hectar ----------------------------------------------
RG_n_SP_plot <- RG_data %>%
  filter(compartiment == "ag") %>%
  select(plot_ID, inv, SP_code) %>% 
  group_by(plot_ID, inv) %>% 
  distinct() %>% 
  summarise(n_SP = n()) %>% 
  mutate(stand_component = "RG")


# 2.4. RG big summary combining all grouping variables --------------------------------------------------------
# 2.4.1. RG summary by plot, inventory, stand, compartiment, species and  -------------------------------------------
if(exists('RG_stat_2') == TRUE && nrow(RG_stat_2) != 0){
  RG_SP_ST_BCN_ha <- plyr::rbind.fill(
    RG_data %>%
      # stand plot area / area already contains those circle area that form emtpy cirlces to not overestimate the stocks
      group_by(plot_ID, stand_plot_A_ha, inv, stand, compartiment, SP_code) %>% 
      # sum number of trees  of sampling circuits and relate them to the area of all sampling circuits at that plot (even empty ones)
      reframe(B_t_ha = sum(ton(B_kg_tree))/stand_plot_A_ha, # plot are is the area of the respecitve samplign circuit in ha 
              C_t_ha = sum(ton(C_kg_tree))/stand_plot_A_ha,
              N_t_ha = sum(ton(N_kg_tree))/stand_plot_A_ha) %>% 
      distinct() , 
    # join in those cirlce areas that originate form emtpy cirlces to not overestimate the stocks 
    RG_stat_2 %>% 
      # this is in case in 01_00_RG_LT_DW_plot_inv_status_sorting there were stat_2 datasets produced that do not hold any data but only NAs
      filter(!is.na(plot_ID))  %>% 
      # only bind those RG_stat_2 plots in, that don´t have any data, meaning all CCS are empty 
      semi_join(., 
                RG_stat_2 %>% 
                  select(plot_ID, CCS_nr) %>% 
                  distinct() %>% 
                  group_by(plot_ID) %>% 
                  summarize(n_CCS = n()) %>% 
                  filter(n_CCS == 4), 
                by = "plot_ID") %>% 
      select(plot_ID, stand_plot_A_ha, inv, compartiment, B_t_ha, C_t_ha, N_t_ha) %>% 
      distinct()
  ) %>% arrange(plot_ID, stand, SP_code) %>% 
    mutate(stand_component = "RG")
  
}else{
  RG_SP_ST_BCN_ha <-        RG_data %>%
    # stand plot area / area already contains those circle area that form emtpy cirlces to not overestimate the stocks
    group_by(plot_ID, stand_plot_A_ha, inv, stand, compartiment, SP_code) %>% 
    # sum number of trees  of sampling circuits and relate them to the area of all sampling circuits at that plot (even empty ones)
    reframe(B_t_ha = sum(ton(B_kg_tree))/stand_plot_A_ha, # plot are is the area of the respecitve samplign circuit in ha 
            C_t_ha = sum(ton(C_kg_tree))/stand_plot_A_ha,
            N_t_ha = sum(ton(N_kg_tree))/stand_plot_A_ha) %>% 
    distinct()%>% 
    mutate(stand_component = "RG")
}


# 2.4.2. RG summary by plot, inventory, compartiment, species and  -------------------------------------------
if(exists('RG_stat_2') == TRUE && nrow(RG_stat_2) != 0){
  RG_SP_BCN_ha <- plyr::rbind.fill(
    RG_data %>%
      # stand plot area / area already contains those circle area that form emtpy cirlces to not overestimate the stocks
      group_by(plot_ID, plot_A_ha, inv, compartiment, SP_code) %>% 
      # sum number of trees  of sampling circuits and relate them to the area of all sampling circuits at that plot (even empty ones)
      reframe(B_t_ha = sum(ton(B_kg_tree))/plot_A_ha, # plot are is the area of the respecitve samplign circuit in ha 
              C_t_ha = sum(ton(C_kg_tree))/plot_A_ha,
              N_t_ha = sum(ton(N_kg_tree))/plot_A_ha) %>% 
      distinct() , 
    # join in those cirlce areas that originate form emtpy cirlces to not overestimate the stocks 
    RG_stat_2 %>% 
      # this is in case in 01_00_RG_LT_DW_plot_inv_status_sorting there were stat_2 datasets produced that do not hold any data but only NAs
      filter(!is.na(plot_ID))  %>% 
      # only bind those RG_stat_2 plots in, that don´t have any data, meaning all CCS are empty 
      semi_join(., 
                RG_stat_2 %>% 
                  select(plot_ID, CCS_nr) %>% 
                  distinct() %>% 
                  group_by(plot_ID) %>% 
                  summarize(n_CCS = n()) %>% 
                  filter(n_CCS == 4), 
                by = "plot_ID") %>% 
      select(plot_ID, inv, compartiment, B_t_ha, C_t_ha, N_t_ha) %>% 
      distinct()
  ) %>% arrange(plot_ID, SP_code) %>% 
    mutate(stand_component = "RG", 
           stand = "all")
  
}else{
  RG_SP_BCN_ha <-        RG_data %>%
    # stand plot area / area already contains those circle area that form emtpy cirlces to not overestimate the stocks
    group_by(plot_ID, plot_A_ha, inv, compartiment, SP_code) %>% 
    # sum number of trees  of sampling circuits and relate them to the area of all sampling circuits at that plot (even empty ones)
    reframe(B_t_ha = sum(ton(B_kg_tree))/plot_A_ha, # plot are is the area of the respecitve samplign circuit in ha 
            C_t_ha = sum(ton(C_kg_tree))/plot_A_ha,
            N_t_ha = sum(ton(N_kg_tree))/plot_A_ha) %>% 
    distinct()%>% 
    arrange(plot_ID, SP_code) %>%
    mutate(stand_component = "RG", 
           stand = "all")
}




## RG big summary final
RG_summary <- plyr::rbind.fill(
  # RG summray by plot, species, stand 
  RG_SP_ST_BCN_ha, 
  # 2.4.2. RG big summary by plot and species, without grouping by stand ---------------------------------------------------------
  RG_SP_BCN_ha, # statistical operation  
  # 2.4.3. RG summary by plot and stand, without grouping by species ---------------------------------------------------------
  summarize_data(RG_SP_ST_BCN_ha,
                 c("stand_component", "plot_ID", "inv", "compartiment", "stand"),  # variables to group by
                 c("B_t_ha", "C_t_ha", "N_t_ha"), # variables to sum up
                 operation = "sum_df") %>% # statistical operation
    left_join(., RG_n_ha_ST, by = c("plot_ID", "inv", "stand_component", "stand")) %>% 
    mutate(SP_code = "all"),
  # 2.4.4. RG summary by plot, inventory, compartiment, not by species no stand --------
  summarize_data(RG_SP_BCN_ha,
                 c("stand_component", "plot_ID", "inv", "compartiment"),  # variables to group by
                 c("B_t_ha", "C_t_ha", "N_t_ha"), # variables to sum up
                 operation = "sum_df") %>% # statistical operation 
    # join in number of plants and species per ha to plowise summary 
    left_join(., RG_n_ha %>% select(plot_ID, inv,stand_component, n_ha), 
              by = c("plot_ID", "inv", "stand_component")) %>% 
    left_join(., RG_n_SP_plot, 
              by = c("plot_ID", "inv", "stand_component")) %>% 
    mutate(n_ha = ifelse(is.na(n_ha), 0, n_ha), 
           n_SP = ifelse(is.na(n_SP), 0, n_SP), 
           stand = "all", 
           SP_code = "all")
) %>% # close rbind
  distinct() %>% 
  # # add stand type to the RG data accprding to plot ID
  #left_join(., LT_summary %>% select(inv, plot_ID, stand_type) %>% distinct(), by = c("plot_ID", "inv")) %>% 
  arrange(plot_ID)





# 3. export data ----------------------------------------------------------
write.csv(RG_summary, paste0(out.path.BZE3, paste(RG_summary$inv[1], "RG_stocks_ha_all_groups", sep = "_"), ".csv"), row.names = FALSE, fileEncoding = "UTF-8")





stop("this is where RG summary HBI")













# notes -------------------------------------------------------------------


RG_data %>% 
  filter(plot_ID == 140044 & compartiment == "ag") %>% 
  group_by(plot_ID, area_m2, plot_A_ha, CCS_nr, inv) %>% 
  # sum number of trees  per sampling circuit
  reframe(B_t_ha = sum(ton(B_kg_tree))/(area_m2/10000) , # plot are is the area of the respecitve samplign circuit in ha 
          C_t_ha = sum(ton(C_kg_tree))/(area_m2/10000),
          N_t_ha = sum(ton(N_kg_tree))/(area_m2/10000)) %>% 
  distinct() %>% 
  group_by(plot_ID, area_m2, plot_A_ha, inv) %>% 
  reframe(B_t_ha = sum((B_t_ha)), # plot are is the area of the respecitve samplign circuit in ha 
          C_t_ha = sum((B_t_ha)),
          N_t_ha = sum((B_t_ha)), 
          area_sum = mean(area_m2)/10000)



RG_data %>% 
  filter(plot_ID == 140044 & compartiment == "ag") %>% 
  group_by(plot_ID, plot_A_ha, CCS_nr, inv) %>% 
  # sum number of trees  per sampling circuit
  reframe(B_t_ha = sum(ton(B_kg_tree)) , # plot are is the area of the respecitve samplign circuit in ha 
          C_t_ha = sum(ton(C_kg_tree)),
          N_t_ha = sum(ton(N_kg_tree))) %>% 
  distinct() %>% 
  group_by(plot_ID, plot_A_ha, inv) %>% 
  reframe(B_t_ha = sum((B_t_ha))/plot_A_ha, # plot are is the area of the respecitve samplign circuit in ha 
          C_t_ha = sum((B_t_ha))/plot_A_ha,
          N_t_ha = sum((B_t_ha))/plot_A_ha)

RG_stat_2





