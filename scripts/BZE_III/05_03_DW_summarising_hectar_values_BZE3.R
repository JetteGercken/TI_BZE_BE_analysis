# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the  national soil inventory
# BZE3 
# summarising stock per hectar summarising for deadwood


# ----- 0. SETUP ---------------------------------------------------------------

# ----- 0.1. packages and functions --------------------------------------------
source(paste0(getwd(), "/scripts/01_00_functions_library.R"))

# ----- 0.2. working directory -------------------------------------------------
here::here()
getwd()

out.path.BZE3 <- paste0(getwd(), "/output/out_data/out_data_BZE/") 


# ----- 0.3 data import --------------------------------------------------------
# deadwood
# this dataset contains the data of the deadwood inventory of the HBI (BZE2), including info about species groups and B, C, N stocks per tree 
DW_data <- read.delim(file = paste0(out.path.BZE3, "BZE3_DW_update_4.csv"), sep = ",", dec = ".")
DW_stat_2 <- read.delim(file = paste0(out.path.BZE3, DW_data$inv[1], "_DW_stat_2.csv"), sep = ",", dec = ".") %>% 
  mutate(inv = inv_name(inv_year))



# 1. calculations DEADWOOD -------------------------------------------------------------
# 1.1. DW summary per plot per SP per DW type per Dec state ---------------------------------------------------------
# create one very fine grouped summary for deadwood which we sum up into different groups later on 
if(isTRUE(exists('DW_stat_2') == TRUE && nrow(DW_stat_2)!=0) ==T ){
  DW_BCN_ha_SP_TY_DEC_P <- plyr::rbind.fill(DW_data %>% 
                                              group_by(plot_ID, inv, dw_sp, dw_type, decay, compartiment) %>% 
                                              # convert Biomass into tons per hectar and divide it by the plot area to calculate stock per hectar
                                              reframe(B_t_ha = sum(ton(B_kg_tree))/plot_A_ha, # plot are is the area of the respecitive sampling circuit in ha 
                                                      C_t_ha = sum(ton(C_kg_tree))/plot_A_ha,
                                                      N_t_ha = sum(ton(N_kg_tree))/plot_A_ha, 
                                                      n_ha = dplyr::n()/plot_A_ha) %>% 
                                              distinct() , 
                                            DW_stat_2 %>% filter(!is.na(plot_ID)) %>% select(-c( plot_A_ha))
  ) %>% 
    mutate(stand_component = "DW")
}else{
  DW_BCN_ha_SP_TY_DEC_P <- DW_data %>% 
    group_by(plot_ID, inv, dw_sp, dw_type, decay, compartiment, plot_A_ha) %>% 
    # convert Biomass into tons per hectar and divide it by the plot area to calculate stock per hectar
    reframe(B_t_ha = sum(ton(B_kg_tree))/plot_A_ha, # plot are is the area of the respecitive sampling circuit in ha 
            C_t_ha = sum(ton(C_kg_tree))/plot_A_ha,
            N_t_ha = sum(ton(N_kg_tree))/plot_A_ha, 
            n_ha = dplyr::n()/plot_A_ha) %>% 
    distinct()
}


# 1.4. DW big summary including all grouping variables and combinations -------------------------

# 1.4.1. grouped by species, decay type, deadwoodtype, plot, compartiment, inventory ------------------------------------------------------------------
DW_summary <- 
  plyr::rbind.fill(
    DW_BCN_ha_SP_TY_DEC_P,
    # 1.4.2. grouped by species, deadwoodtype, plot, compartiment, inventory. not by decay type anymore------------------------------------------------------------------
    summarize_data(DW_BCN_ha_SP_TY_DEC_P, 
                   c("plot_ID", "inv", "dw_sp", "dw_type", "compartiment"), 
                   c("B_t_ha", "C_t_ha", "N_t_ha"), 
                   operation = "sum_df") %>%
      mutate(decay = "all"),
    
    # 1.4.3. DW grouped by species, decay, plot, compartiment, inventory, not by deadwood type anymore --------------------------------------------------------------
    summarize_data(DW_BCN_ha_SP_TY_DEC_P, 
                   c("plot_ID", "inv", "dw_sp", "decay", "compartiment"), 
                   c("B_t_ha", "C_t_ha", "N_t_ha"), 
                   operation = "sum_df") %>% 
      mutate(dw_type = "all") ,
    
    # 1.4.4. DW grouped by deadwoodtype, decay, plot, compartiment, inventory, not by species type anymore ---------------------------------------------------------------
    summarize_data(DW_BCN_ha_SP_TY_DEC_P, 
                   c("plot_ID", "inv", "dw_type", "decay", "compartiment"), 
                   c("B_t_ha", "C_t_ha", "N_t_ha"), 
                   operation = "sum_df") %>% 
      mutate(dw_sp = "all"),
    
    # 1.4.5. DW grouped by deadwoodtype, plot, compartiment, inventory, not by species and decay type anymore ---------------------------------------------------------------
    summarize_data(DW_BCN_ha_SP_TY_DEC_P, 
                   c("plot_ID", "inv", "dw_type", "compartiment"), 
                   c("B_t_ha", "C_t_ha", "N_t_ha"), 
                   operation = "sum_df") %>%
      left_join(., DW_data %>% 
                  filter(compartiment == "ag") %>% 
                  distinct() %>% 
                  group_by(plot_ID, inv, ST_LY_type, dw_type) %>% 
                  summarise(mean_d_cm = mean(d_cm),
                            sd_d_cm = sd(d_cm),
                            mean_l_m = mean(l_dm/10),
                            sd_l_m = sd(l_dm/10)),
                by = c("plot_ID", "inv", "dw_type"), 
                multiple = "all") %>% 
      mutate(dw_sp = "all", 
             decay = "all"),
    
    # 1.4.6. DW grouped by decay, plot, compartiment, inventory, not by species and deadwood type anymore ---------------------------------------------------------------
    summarize_data(DW_BCN_ha_SP_TY_DEC_P, 
                   c("plot_ID", "inv", "decay", "compartiment"), 
                   c("B_t_ha", "C_t_ha", "N_t_ha"), 
                   operation = "sum_df") %>%
      left_join(., DW_data %>% 
                  filter(compartiment == "ag") %>% 
                  distinct() %>% 
                  group_by(plot_ID, inv, decay) %>% 
                  summarise(mean_d_cm = mean(d_cm),
                            sd_d_cm = sd(d_cm),
                            mean_l_m = mean(l_dm/10),
                            sd_l_m = sd(l_dm/10)), 
                by = c("plot_ID", "inv", "decay"), 
                multiple = "all") %>%
      mutate(dw_sp = "all", 
             dw_type = "all") ,
    
    # 1.4.7. DW grouped by species group, plot, compartiment, inventory, not by decay and deadwood type anymore ---------------------------------------------------------------
    summarize_data(DW_BCN_ha_SP_TY_DEC_P, 
                   c("plot_ID", "inv", "dw_sp", "compartiment"), 
                   c("B_t_ha", "C_t_ha", "N_t_ha"), 
                   operation = "sum_df") %>% 
      # mean and sd of length and diameter of deadwood 
      left_join(., DW_data %>% 
                  filter(compartiment == "ag") %>% 
                  distinct() %>% 
                  group_by(plot_ID, inv, dw_sp) %>% 
                  summarise(mean_d_cm = mean(d_cm),
                            sd_d_cm = sd(d_cm),
                            mean_l_m = mean(l_dm/10),
                            sd_l_m = sd(l_dm/10)), 
                by = c("plot_ID", "inv", "dw_sp"), 
                multiple = "all") %>%
      mutate(decay = "all", 
             dw_type = "all") ,
    
    # 1.4.8.DW grouped by plot, compartiment, inventory, not by decay, species and deadwood type anymore ----------------------------------------------------------------
    summarize_data(DW_BCN_ha_SP_TY_DEC_P, 
                   c("plot_ID", "inv", "compartiment"), 
                   c("B_t_ha", "C_t_ha", "N_t_ha"), 
                   operation = "sum_df") %>%
      distinct() %>% 
      # average values over all deadwood items per plot
      left_join(., DW_data %>% 
                  filter(compartiment == "ag") %>% 
                  distinct() %>% 
                  group_by(plot_ID, inv) %>% 
                  summarise(mean_d_cm = mean(d_cm),
                            sd_d_cm = sd(d_cm),
                            mean_l_m = mean(l_dm/10),
                            sd_l_m = sd(l_dm/10)), 
                by = c("plot_ID", "inv"), 
                multiple = "all") %>%  
      # number of DW items per ha
      left_join(., DW_data %>% 
                  filter(compartiment == "ag") %>% 
                  group_by(plot_ID, inv) %>% 
                  reframe(n_ha = dplyr::n()/plot_A_ha) %>% 
                  distinct(), 
                multiple = "all",
                by = c("plot_ID", "inv")) %>% 
      # number of decay types per plot
      left_join(DW_data %>% 
                  filter(compartiment == "ag") %>% 
                  select(plot_ID, inv, decay) %>% 
                  distinct() %>% 
                  group_by(plot_ID, inv) %>% 
                  summarise(n_dec = dplyr::n()), 
                multiple = "all",
                by = c("plot_ID", "inv")) %>% 
      # number of deadwood types per plot
      left_join(DW_data %>% 
                  filter(compartiment == "ag") %>% 
                  select(plot_ID, inv, dw_type) %>% 
                  distinct() %>% 
                  group_by(plot_ID, inv) %>% 
                  summarise(n_dw_TY = dplyr::n()), 
                multiple = "all",
                by = c("plot_ID", "inv")) %>% 
      mutate(decay = "all", 
             dw_type = "all", 
             dw_sp = "all", 
             stand= "all", 
             SP_code = "all") 
  ) %>%  # close rbind
  # add stand component for those datasets where it´s not included yet
  mutate(stand_component = "DW") %>% 
  # # # add stand type to the RG data accprding to plot ID
  # left_join(., LT_summary %>% select(inv, plot_ID, stand_type) %>% distinct(), by = c("plot_ID", "inv")) %>% 
  distinct() %>% 
  arrange(plot_ID)



# 2. data export ----------------------------------------------------------
write.csv(DW_summary, paste0(out.path.BZE3, paste(DW_summary$inv[1], "DW_stocks_ha_all_groups", sep = "_"), ".csv"), row.names = FALSE, fileEncoding = "UTF-8")

stop("this is where DW summary BZE3 ends")
