# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the  national soil inventory
# biodiverseity index
# HBI

# 1) quadratic mean diameter at breast height (DBH)
# 2) standard deviation of DBH
# 3) standard deviation of stand height
# 10) trees pecies richness and 
# 5) bark-diversity index
# 6) volume of trees with DBH ≥ 40 cm
# 7) diversity of flowering and fructification trees

# 4) number of decay classes
# 8) average mean diameter of downed deadwood
# 9) mean DBH of standing deadwood

# 11) tree species richness in the regeneration layer



# NOTE: 
# I have to make sure that the empty plos are included in the index as well
# meaning for example DW plot 50145 which has no deadwood items and by that an average diameter of 0 has to be included in the caclualtions
# therefore it would be better to work only with the summaries, or, if summaries are created afterwards, join at least the plots in that have no
# values 

# 0.SETUP --------------------------------------------------------------------------------------------------------------------
# 0.1. packages and functions -------------------------------------------------------------------------------------------------
source(paste0(getwd(), "/scripts/01_00_functions_library.R"))


# 0.2. working directory ------------------------------------------------------------------------------------------------------
here::here()
out.path.BZE3 <- ("output/out_data/out_data_BZE/") 


# 0.3 import data --------------------------------------------------------------------------------------------------------------
# livign trees
trees_data <- read.delim(file = here(paste0(out.path.BZE3, "HBI_LT_update_4.csv")), sep = ";", dec = ",")
LT_summary <-  read.delim(file = here(paste0(out.path.BZE3, trees_data$inv[1], "_LT_stocks_ha_all_groups.csv")), sep = ";", dec = ",")
trees_stat_2 <- read.delim(file = here(paste0(out.path.BZE3, trees_data$inv[1], "_LT_stat_2.csv")), sep = ";", dec = ",") %>% select(-X)

# regeneration 
RG_data <- read.delim(file = here(paste0(out.path.BZE3, trees_data$inv[1], "_RG_update_4.csv")), sep = ";", dec = ",")
RG_summary <-  read.delim(file = here(paste0(out.path.BZE3, trees_data$inv[1], "_RG_stocks_ha_all_groups.csv")), sep = ";", dec = ",")
RG_stat_2 <- read.delim(file = here(paste0(out.path.BZE3, trees_data$inv[1], "_RG_stat_2.csv")), sep = ";", dec = ",")

# deadwood
DW_data <- read.delim(file = here(paste0(out.path.BZE3, trees_data$inv[1], "_DW_update_4.csv")), sep = ";", dec = ",")
DW_summary <-  read.delim(file = here(paste0(out.path.BZE3, trees_data$inv[1], "_DW_stocks_ha_all_groups.csv")), sep = ";", dec = ",")
DW_stat_2 <- read.delim(file = here(paste0(out.path.BZE3, trees_data$inv[1], "_DW_stat_2.csv")), sep = ";", dec = ",")



# 1. calculations -----------------------------------------------------------------------------------------------------------------------

# 1.1. living trees -------------------------------------------------------------------------------------------------------------------

# 1.1.1. LT quadratic mean diameter at breast height (DBH) ---------------------------------------------------------------------------------
# The quadratic mean (also called the root mean square*) is a type of average. 
# It measures the absolute magnitude of a set of numbers, and is calculated by:
# RMS = sqrt(sum(x)^2/n)

LT_DBH_RMS <- trees_data %>% 
  group_by(plot_ID) %>% 
  reframe(LT_RMS_DBH = RMS(DBH_cm)) %>% 
  distinct()
  

if(exists('trees_stat_2') == TRUE && nrow(trees_stat_2)!= 0){
  FSI_df <- plyr::rbind.fill(LT_DBH_RMS, 
                           # select only those plots with empty sampling circuits that have all 3 circuits empty
                           # by counting the circuits per plot and filtering for those with n_CCS ==3
                           trees_stat_2 %>% 
                             select(plot_ID, CCS_r_m) %>% 
                             distinct()%>% 
                             group_by(plot_ID) %>% 
                             summarise(n_CCS = n()) %>% 
                             filter(n_CCS == 3) %>% 
                             select(plot_ID)) %>% 
  # if the Rbind caused NAs to appear because there were whole plots without a any tree CCS then we have to set the respective variable to 0
  mutate(LT_RMS_DBH = ifelse(is.na(LT_RMS_DBH), 0, LT_RMS_DBH), 
         LT_FSI_DBH_RMS =  as.numeric(FSI(LT_RMS_DBH, min(LT_RMS_DBH), max(LT_RMS_DBH))))
}else{
  FSI_df <- LT_DBH_RMS %>% 
    mutate(LT_FSI_DBH_RMS =  as.numeric(FSI(LT_RMS_DBH, min(LT_RMS_DBH), max(LT_RMS_DBH))))
  }



# 1.1.2. LT standard deviation of DBH ------------------------------------------------------------------
FSI_df <- FSI_df %>% 
  left_join(., 
            LT_summary %>% 
              filter(plot_ID != "all" & SP_code == "all" & stand == "all") %>% 
              select(plot_ID, sd_DBH_cm) %>% 
              distinct() %>% 
              mutate(LT_FSI_DBH_SD =  as.numeric(FSI(sd_DBH_cm, min(sd_DBH_cm), max(sd_DBH_cm))), 
                     plot_ID = as.integer(plot_ID)) %>% 
              rename("LT_sd_DBH_cm" = "sd_DBH_cm"), 
            by = "plot_ID")


# 1.1.3. LT standard deviation of stand height ------------------------------------------------------------------
FSI_df <- FSI_df %>% 
  left_join(., 
            LT_summary %>% 
              filter(plot_ID != "all" & SP_code == "all" & stand == "all") %>% 
              select(plot_ID, sd_H_m) %>% 
              distinct() %>% 
              mutate(LT_FSI_H_SD =  as.numeric(FSI(sd_H_m, min(sd_H_m), max(sd_H_m))), 
                     plot_ID = as.integer(plot_ID)) %>% 
              rename("LT_sd_H_m" = "sd_H_m"), 
            by = "plot_ID")



# 1.1.4. tree species richness ------------------------------------------------------------------
FSI_df <- FSI_df %>% 
  left_join(., 
            LT_summary %>% 
              filter(plot_ID != "all" & SP_code == "all" & stand == "all") %>% 
              select(plot_ID, n_SP) %>% 
              distinct() %>% 
              mutate(LT_FSI_n_SP = as.numeric(FSI(n_SP, min(n_SP), max(n_SP))), 
                     plot_ID = as.integer(plot_ID)) %>% 
              rename("LT_n_SP" = "n_SP"), 
            by = "plot_ID")



# 1.1.5. bark-diversity index ------------------------------------------------------------------
# Diversity of bark types (smooth, fissured, peeling, scaly, cracked, etc.) in forest stands implies a variety of habitats
# for many species to be found there (insects, fungi, yeasts, spiders, epiphytes). 
# Tree diameter and bark-development phases are considered

# here we have to open a new category in x_bart assinging all given species to their bark type 
# an assistant should do this :/


# 1.1.6. volume of trees with DBH ≥ 40 cm ------------------------------------------------------------------



# 1.1.7. diversity of flowering and fructification trees ------------------------------------------------------------------
# i guess this reffers to how many different fructificating and flowering trees are at a plot? or per ha? 
# here we have to open a new column in x_bart divifing the trees in flowering & fructifying or not 



# 1.2. DEADWOOD -----------------------------------------------------------


# 1.2.1. number of decay classes ------------------------------------------------------------------
FSI_df <- FSI_df %>% 
  left_join(., DW_summary %>% 
  filter(plot_ID != "all" & decay == "all" & dw_type == "all" & dw_sp == "all") %>% 
  select(plot_ID, n_dec) %>% 
  distinct() %>% 
  mutate(DW_FSI_n_dec = as.numeric(FSI(n_dec, min(n_dec), max(n_dec)))) %>% 
  rename("DW_n_dec" = "n_dec"), 
  by = "plot_ID")

# 1.2.2. average mean diameter of downed deadwood -------------------------------------------------------------------
FSI_df <- FSI_df %>% 
  left_join(., DW_data %>% 
  filter(ST_LY_type == "L") %>% 
  group_by(plot_ID) %>%
  summarise(DW_LY_mean_D_cm = mean(d_cm)) %>% 
  distinct() %>% 
  mutate(DW_FSI_LY_mean_D_cm = as.numeric(FSI(DW_LY_mean_D_cm, min(DW_LY_mean_D_cm), max(DW_LY_mean_D_cm)))), 
  by = "plot_ID")
  



# 1.2.3. mean DBH of standing deadwood -------------------------------------------------------------------
FSI_df <- FSI_df %>% 
  left_join(., DW_data %>% 
  filter(ST_LY_type == "S") %>% 
  group_by(plot_ID) %>%
  summarise(DW_ST_mean_D_cm = mean(d_cm)) %>% 
  distinct() %>% 
  mutate(DW_FSI_ST_mean_D_cm = as.numeric(FSI(DW_ST_mean_D_cm, min(DW_ST_mean_D_cm), max(DW_ST_mean_D_cm)))), 
  by = "plot_ID")









