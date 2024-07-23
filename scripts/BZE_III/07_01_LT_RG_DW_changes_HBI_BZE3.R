# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the  national soil inventory
# growth 


# ----- 0. SETUP ---------------------------------------------------------------

# ----- 0.1. packages and functions --------------------------------------------
source(paste0(getwd(), "/scripts/01_00_functions_library.R"))

# ----- 0.2. working directory -------------------------------------------------
here::here()
getwd()

out.path.BZE3 <- ("output/out_data/out_data_BZE/") 

# vorräte: über alle jahre 
# einzelbaumdurchmesser
# veränderung grundflächenanteil


# 0.3. data import --------------------------------------------------------
# BZE3
# living trees
BZE3_trees <- read.delim(file = here(paste0(out.path.BZE3, "BZE3_LT_update_4.csv")), sep = ",", dec = ".")
# all LT, RG, DW summmaries together and total plot stock bze3
BZE3_summary <- read.delim(file = here(paste0(out.path.BZE3, BZE3_trees$inv[1], "_LT_RG_DW_stocks_ha_all_groups.csv")), sep = ",", dec = ".")
# standtype wise summary
BZE3_TY_summary <-  BZE3_summary %>% filter(plot_ID == "all") %>%  
  select(-c(plot_A_ha , n_stands, BA_percent, mean_DBH_cm, sd_DBH_cm, Dg_cm, mean_BA_m2, mean_H_m, sd_H_m, dw_sp, dw_type, decay, inv_year, ST_LY_type, mean_d_cm, sd_d_cm, mean_l_m, sd_l_m, n_dec, n_dw_TY)) %>% 
  distinct() 
# living trees summary: # we filter for plot and LT wise summaries, the summaries per stand type get their own df
BZE3_LT_summary <- BZE3_summary %>% filter(stand_component == "LT" & plot_ID != "all") %>% # read.delim(file = here(paste0(out.path.BZE3, BZE3_trees$inv[1], "_LT_stocks_ha_all_groups.csv")), sep = ";", dec = ",")
  select(-c(dw_sp, dw_type, decay, inv_year, ST_LY_type, mean_d_cm, sd_d_cm, mean_l_m, sd_l_m, n_dec, n_dw_TY))
# regeneration
# this dataset contains single plant data of regeneration inventory of BZE3
BZE3_RG <-  read.delim(file = here(paste0(out.path.BZE3, BZE3_trees$inv[1], "_RG_update_4.csv")), sep = ",", dec = ".")
# this dataset contains regeneration data summarized per hectar BZE3
BZE3_RG_summary <- BZE3_summary %>% filter(stand_component == "RG" & plot_ID != "all") #read.delim(file = here(paste0(out.path.BZE3, BZE3_trees$inv[1], "_RG_stocks_ha_all_groups.csv")), sep = ";", dec = ",")
# deadwood
# this dataset contains single plant data of deadwood inventory of HBI
BZE3_DW <- read.delim(file = here(paste0(out.path.BZE3, BZE3_trees$inv[1], "_DW_update_4.csv")), sep = ",", dec = ".")
# this dataset contains deadwood data summarized per hectar HBI
BZE3_DW_summary <- BZE3_summary %>% filter(stand_component == "DW" & plot_ID != "all") # read.delim(file = here(paste0(out.path.BZE3, BZE3_trees$inv[1], "_DW_stocks_ha_all_groups.csv")), sep = ";", dec = ",")
#FSI
BZE3_FSI <- read.delim(file = here(paste0(out.path.BZE3, BZE3_trees$inv[1], "_FSI.csv")), sep = ",", dec = ".")



# HBI Data
# living trees
HBI_trees <- read.delim(file = here(paste0(out.path.BZE3, "HBI_LT_update_4.csv")), sep = ",", dec = ".")
# all LT, RG, DW summmaries together and total plot stock HBI
HBI_summary <- read.delim(file = here(paste0(out.path.BZE3, HBI_trees$inv[1], "_LT_RG_DW_stocks_ha_all_groups.csv")), sep = ",", dec = ".")
# standtype wise summary
HBI_TY_summary <-  HBI_summary %>% filter( plot_ID == "all") %>% # we filter for plot and LT wise summaries, the summaries per stand type get their own df 
  select(-c(plot_A_ha , n_stands, BA_percent, mean_DBH_cm, sd_DBH_cm, Dg_cm, mean_BA_m2, mean_H_m, sd_H_m, dw_sp, dw_type, decay, inv_year, ST_LY_type, mean_d_cm, sd_d_cm, mean_l_m, sd_l_m, n_dec, n_dw_TY)) %>% 
  distinct() 
# living trees summary
HBI_LT_summary <-  HBI_summary %>% filter(stand_component == "LT"& plot_ID != "all") %>% # we filter for plot and LT wise summaries, the summaries per stand type get their own df 
  #read.delim(file = here(paste0(out.path.BZE3, HBI_trees$inv[1], "_LT_stocks_ha_all_groups.csv")), sep = ";", dec = ",")
  select(-c(dw_sp, dw_type, decay, inv_year, ST_LY_type, mean_d_cm, sd_d_cm, mean_l_m, sd_l_m, n_dec, n_dw_TY))
# regeneration
# this dataset contains single plant data of regeneration inventory of HBI
HBI_RG <- read.delim(file = here(paste0(out.path.BZE3, HBI_trees$inv[1], "_RG_update_4.csv")), sep = ",", dec = ".")
# this dataset contains regeneration data summarized per hectar HBI
HBI_RG_summary <- HBI_summary %>% filter(stand_component == "RG" & plot_ID != "all") #read.delim(file = here(paste0(out.path.BZE3, HBI_trees$inv[1], "_RG_stocks_ha_all_groups.csv")), sep = ";", dec = ",")
# deadwood 
# this dataset contains single plant data of deadwood inventory of HBI
HBI_DW <- read.delim(file = here(paste0(out.path.BZE3, HBI_trees$inv[1], "_DW_update_4.csv")),sep = ",", dec = ".")
# this dataset contains deadwood data summarized per hectar HBI
HBI_DW_summary <- HBI_summary %>% filter(stand_component == "DW" & plot_ID != "all")  #read.delim(file = here(paste0(out.path.BZE3, HBI_trees$inv[1], "_DW_stocks_ha_all_groups.csv")), sep = ";", dec = ",")
# FSI
HBI_FSI <- read.delim(file = here(paste0(out.path.BZE3, HBI_trees$inv[1], "_FSI.csv")), sep = ",", dec = ".")



# data sorting ------------------------------------------------------------
# as we collect all growth info in regard to the BZE3 by left_joining in HBI data,
# plots that have a calculated stock that are not present in BZE3 anymore , there will be no "negative growth" registered
# this is why we save perfectly fine HBI plots that do not have a counter/ Pertner plot in BZE3 in a separate dataset: 
HBI_strata_not_represented_in_BZE3 <- HBI_summary %>% 
  semi_join(., HBI_summary %>% 
              select(plot_ID, stand, SP_code, compartiment, dw_sp, dw_type, decay, stand_type ) %>% 
              distinct() %>% 
              anti_join(., BZE3_summary %>% 
                          select(plot_ID, stand, SP_code, compartiment,dw_sp,dw_type,decay, stand_type) %>% 
                          distinct(), by = c("plot_ID", "stand", "SP_code", "compartiment", "dw_sp", "dw_type","decay", "stand_type")), 
            by = c("plot_ID", "stand", "SP_code", "compartiment", "dw_sp", "dw_type","decay", "stand_type"))





# 1. LIVING TREES CALCULATIONS ---------------------------------------------------------
# 1.1. average single tree growth -------------------------------------------------
# this inventory status means that the tree should have been assessed in the previous 
# invenotry but wasn´t
# thus we have to calculate how much the tree of that species at that plot would have grown 
# between the previous and current inventory, then deduct it from the diameter of the 
# respective tree in the current inventory and add the tree to the previous inventory with
# the same ID, tree status 0 and the reduced diameter
# for this inventory status 

## join HBI and BZE3 single tree diameters together by tree & plot ID
dbh_growth_tree <- left_join(
  # select trees that are repeatedly inventory, or unknown status
  BZE3_trees %>% 
    filter(tree_inventory_status %in% c(1) & compartiment == "ag") %>% 
    rename(BZE3_DBH_cm = DBH_cm) %>% 
    rename(BZE3_inv_year = inv_year) %>% 
    select(plot_ID, tree_ID, BZE3_inv_year, stand, C_layer, SP_code, BZE3_DBH_cm), 
  HBI_trees %>% 
    # select trees that were newly inventored, repeated inventory, or unknown status
    filter(tree_inventory_status %in% c(0, 1, -9) & compartiment == "ag")%>% 
    distinct() %>% 
    rename(HBI_DBH_cm = DBH_cm) %>% 
    rename(HBI_inv_year = inv_year) %>% 
    select(plot_ID, tree_ID, HBI_inv_year, stand, C_layer, SP_code, HBI_DBH_cm), 
  by = c("plot_ID", "tree_ID", "C_layer", "stand", "SP_code"), 
  multiple = "all") %>%    
# there may be trees that are new in BZE3 and havent been inventorised in HBI
# so we have to put these trees DBHs to 0 and the invenotry year to the one of the other trees
# to calculate the increment properly 
  mutate(HBI_DBH_cm = ifelse(is.na(HBI_DBH_cm), 0, HBI_DBH_cm), 
       HBI_inv_year = ifelse(is.na(HBI_inv_year), 2012, HBI_inv_year)) %>% 
  mutate(DBH_growth_cm = BZE3_DBH_cm - HBI_DBH_cm, 
         age_period = BZE3_inv_year- HBI_inv_year, 
         annual_growth_cm = DBH_growth_cm/age_period) 

# 1.2. grouping growth ------------------------------------------------------------------
dbh_growth_summary <- plyr::rbind.fill(
  # growth by plot, species, canopy layer and stand
  summarize_data(dbh_growth_tree,
                 c("plot_ID", "stand", "SP_code", "C_layer"), 
                 c("age_period", "annual_growth_cm"), 
                 operation = "mean_df"), 
  # growth by plot, species and stand
  summarize_data(dbh_growth_tree, 
               c("plot_ID", "stand", "SP_code"), 
               c("age_period", "annual_growth_cm"), 
               operation = "mean_df") %>% 
    mutate(C_layer = "all"),
# growth by plot, species
  summarize_data(dbh_growth_tree, 
                 c("plot_ID", "SP_code"), 
                 c("age_period", "annual_growth_cm"), 
                 operation = "mean_df")%>% 
    mutate(C_layer = "all", 
           stand = "all"), 
# growth by species
  summarize_data(dbh_growth_tree, 
                 c("SP_code"), 
                 c("age_period", "annual_growth_cm"), 
                 operation = "mean_df")%>% 
    mutate(C_layer = "all", 
           stand = "all", 
           plot_ID = "all"), 
# growth by plot
summarize_data(dbh_growth_tree, 
               c("plot_ID"), 
               c("age_period", "annual_growth_cm"), 
               operation = "mean_df")%>% 
  mutate(C_layer = "all", 
         stand = "all", 
         SP_code = "all"))


# 1.3. changes in BA composition -------------------------------------------
# select all possible tree species and stands of the plots in BZE3 to create list 
# of all possible species per plot from both invenoties, to be able to join in basal area shares later and set missing shares to 0
BA_changes_SP_ST_P <- rbind(
  BZE3_LT_summary %>% 
  # filter for plot and species wise summary
  filter(plot_ID != "all" & SP_code != "all" & stand == "all" |
   # filter for plot, species and stand wise summary
           plot_ID != "all" & SP_code != "all" & stand != "all" & !is.na(stand)) %>%
  # select all possible plot_iD, species and stand combinations
  select(plot_ID, SP_code, stand) %>% distinct(), 
  HBI_LT_summary %>% 
  # filter join HBI summary only for those plots, that also appear in BZE3
  semi_join(BZE3_LT_summary, by = "plot_ID") %>% 
  # filter for plot and species wise summary
  filter(plot_ID != "all" & SP_code != "all" & stand == "all" |
           plot_ID != "all" & SP_code != "all" & stand != "all" & !is.na(stand)) %>%
  # select all possible plot_iD, species and stand combinations
  select(plot_ID, SP_code, stand) %>% distinct()) %>% 
  distinct() %>% 
  mutate(plot_ID = as.integer(plot_ID)) %>% 
  arrange(plot_ID) %>% 
  ## this is how I used to filter, before we decided to calculate the BA changes for the stand- wise summaries as well
  # rbind(BZE3_trees %>% select(plot_ID, SP_code, stand) %>% distinct(),
  #                   HBI_trees %>% select(plot_ID, SP_code, stand) %>% distinct()) %>% 
  # distinct() %>% 
  # arrange(plot_ID) %>% 
  # join basal area shares from species in BZE3 dataset according to species, and plot, if the species does have a basal area share in BZE3
  left_join(., BZE3_LT_summary %>% 
              # filter for plot and species wise summary
              filter(plot_ID != "all" & SP_code != "all" & stand == "all" |
                       plot_ID != "all" & SP_code != "all" & stand != "all" & !is.na(stand)) %>%
              # select the BA percent
              select(plot_ID, SP_code, stand, BA_percent) %>% 
              mutate(across(c("plot_ID"), as.integer)) %>% 
              rename(BA_percent_BZE3 = BA_percent) %>% 
              distinct(), 
            by = c("plot_ID", "SP_code", "stand")) %>% 
  # join basal area shares from species in HBI dataset according to species, and plot, if the species does have a basal area share in BZE3
  left_join(., HBI_LT_summary %>% 
              # filter for plot and species wise summary
              filter(plot_ID != "all" & SP_code != "all" & stand == "all"|
                       plot_ID != "all" & SP_code != "all" & stand != "all" & !is.na(stand)) %>%
              # select the BA percent
              select(plot_ID, SP_code, stand, BA_percent) %>% 
              mutate(across(c("plot_ID"), as.integer)) %>% 
              rename(BA_percent_HBI = BA_percent) %>% 
              distinct(), 
            by = c("plot_ID", "SP_code", "stand")) %>%
  # here we have to set the BA_percent that do not appear in the respective inventory to 0
  mutate(BA_percent_BZE3 = ifelse(is.na(BA_percent_BZE3), 0, BA_percent_BZE3), 
         BA_percent_HBI = ifelse(is.na(BA_percent_HBI), 0, BA_percent_HBI), 
         BA_percent_diff = BA_percent_BZE3-BA_percent_HBI, # calcualte difference in HBI and BZE3 BA share per plot and species
         #stand = "all", 
         C_layer = "all") 
  



# 1.4. changes in stocks per ha --------------------------------------------
trees_stock_changes_P <- 
  # we use a full join here, since we want to have all categories represented per plot in both inventories
  # e.g. a plot still had some t of spruce in HBI but doesnt habe them in BZE3 BE, we still want to calculate the change 
  full_join(BZE3_LT_summary %>% 
              select(stand_component, plot_ID, stand, stand_type, SP_code, compartiment, B_t_ha, C_t_ha, N_t_ha, n_ha, n_SP, 
                     mean_DBH_cm, sd_DBH_cm, Dg_cm, mean_BA_m2, mean_H_m, sd_H_m, Hg_m) %>%
              #add "_BZE3" to the names of the valriables we want to calculate the difference for:  https://rstats101.com/add-prefix-or-suffix-to-column-names-of-dataframe-in-r/
              rename_with(.fn = function(.x){paste0(.x,"_BZE3")},
                          .cols= c(B_t_ha, C_t_ha, N_t_ha, n_ha, n_SP, stand_type, 
                                   mean_DBH_cm, sd_DBH_cm, Dg_cm, mean_BA_m2, mean_H_m, sd_H_m, Hg_m)) %>%
              distinct(), 
            # jopin in HBI dataset via left join, so that only plots, species, C layers and stands are joined in that have a partner in the current inventory. 
            HBI_LT_summary %>% 
              semi_join(BZE3_LT_summary %>% select(plot_ID) %>% distinct(), by = "plot_ID") %>% 
              select(stand_component, plot_ID, stand, stand_type, SP_code, compartiment, B_t_ha, C_t_ha, N_t_ha, n_ha, n_SP, 
                     mean_DBH_cm, sd_DBH_cm, Dg_cm, mean_BA_m2, mean_H_m, sd_H_m, Hg_m) %>%
              # https://rstats101.com/add-prefix-or-suffix-to-column-names-of-dataframe-in-r/
              rename_with(.fn = function(.x){paste0(.x,"_HBI")}, 
                          .cols= c(B_t_ha, C_t_ha, N_t_ha, n_ha, n_SP, stand_type,
                                   mean_DBH_cm, sd_DBH_cm, Dg_cm, mean_BA_m2, mean_H_m, sd_H_m, Hg_m)) %>% 
              distinct(), 
            by = c("stand_component" ,"plot_ID", "compartiment", "SP_code", "stand")) %>% 
  # if there are plots/ species or stands that were not established in HBI and thus do not have stocks 
  # or if there are plots/ species or stands that are not present in BZE3 anymore but have stocks in HBI
  # we have to set their stock per ha to 0 to make sure the calculations can also track "negative growth"
  mutate(across(contains("t_ha") | contains("stand_type"), ~ifelse(is.na(.x), 0, .x)) )%>% 
  # for n_ha and n_SP we do the same but as these values were calculated only for the whole plot or the whole stand, 
  # so we apply the correction only to rows witch plot_ID != all, for stand !=all and stand == all and species == "all"
  mutate(across(contains("n_ha"), ~ifelse(is.na(.x) & 
                                            plot_ID != "all"&
                                            SP_code == "all", 0, .x)) ) %>% 
  # the number of species is only calcualted on plot level, meaning sp codes and stand have to be "all" for it to be turned from NA to 0 
  mutate(across(contains("n_SP"), ~ifelse(is.na(.x) & 
                                                plot_ID != "all"&
                                                stand == "all"   & 
                                                SP_code == "all", 0, .x)) ) %>%
  # for the means and sds we calcualted values in the catagories :SP_code, plot, stand and SP_code, plot and plot. 
  # Thus we have to set only NAs that occure in these groups and columns to 0 
  mutate(across(contains("mean") | 
                  contains("sd")  | 
                  contains("Hg")  | 
                  contains("Dg") , ~ifelse(is.na(.x) &  
                                             plot_ID != "all"& 
                                             SP_code != "all", 0, .x)) ) %>% 
  arrange(plot_ID, stand, SP_code, compartiment)

# substact columns edning on BZE3 from columns ednign with HBI 
# https://stackoverflow.com/questions/47478125/create-new-columns-by-substracting-column-pairs-from-each-other-in-r
pre_vars <- grep("_HBI", colnames(trees_stock_changes_P), value=TRUE)
post_vars <- grep("_BZE3", colnames(trees_stock_changes_P), value=TRUE)
trees_stock_changes_P[, paste0(str_sub(pre_vars, end=-5), "_diff")] <- trees_stock_changes_P[, post_vars] - trees_stock_changes_P[, pre_vars]
trees_stock_changes_P <- trees_stock_changes_P %>% arrange(plot_ID, stand, SP_code, compartiment) %>%
  mutate(C_layer = "all")



# binding all LT growth datasets together ---------------------------------
LT_changes <-  trees_stock_changes_P %>% 
  select(stand_component, plot_ID, stand, SP_code, compartiment, C_layer, contains("diff")) %>% 
  mutate(across(c("plot_ID"), as.character))  %>% 
  left_join(BA_changes_SP_ST_P %>% 
              select(plot_ID, stand, SP_code, C_layer, contains("diff")) %>% 
              mutate(across(c("plot_ID"), as.character)), 
            by = c("plot_ID", "stand", "SP_code", "C_layer") ) %>% 
  left_join(., dbh_growth_summary, 
            by = c("plot_ID", "stand", "C_layer", "SP_code")) %>%  
  mutate(stand_component = "LT") %>%
  select(stand_component, plot_ID, stand, C_layer, SP_code, compartiment, age_period, annual_growth_cm, contains("diff"))%>% 
  arrange(plot_ID)


# 2. REGENERATION CALCULATIONS --------------------------------------------
# 2.1. changes in stocks per ha --------------------------------------------
RG_stock_changes_P <- 
  full_join(BZE3_RG_summary %>% 
              select(stand_component, plot_ID, stand, SP_code, compartiment, B_t_ha, C_t_ha, N_t_ha, n_ha, n_SP) %>%
              # https://rstats101.com/add-prefix-or-suffix-to-column-names-of-dataframe-in-r/
              rename_with(.fn = function(.x){paste0(.x,"_BZE3")},
                          .cols= c(B_t_ha, C_t_ha, N_t_ha, n_ha, n_SP)), 
            HBI_RG_summary %>%
              semi_join(BZE3_RG_summary%>% select(plot_ID) %>% distinct(), by = "plot_ID") %>% 
              #filter(plot_ID != "all" & SP_code == "all" & stand == "all") %>% 
              select(stand_component, plot_ID, stand, SP_code, compartiment, B_t_ha, C_t_ha, N_t_ha, n_ha, n_SP) %>%
              # https://rstats101.com/add-prefix-or-suffix-to-column-names-of-dataframe-in-r/
              rename_with(.fn = function(.x){paste0(.x,"_HBI")}, 
                          .cols= c(B_t_ha, C_t_ha, N_t_ha, n_ha, n_SP)), 
            by = c("stand_component", "plot_ID", "compartiment", "SP_code", "stand")) %>% 
  # if there are plots/ species or stands that were not established in HBI and thus do not have stocks 
  # or if there are plots/ species or stands that are not present in BZE3 anymore but have stocks in HBI
  # we have to set their stock per ha to 0 to make sure the calculations can also track "negative growth"
  mutate(across(contains("t_ha"), ~ifelse(is.na(.x), 0, .x)) )%>% 
  # for n_ha and n_SP we do the same but as these values were calculated only for the whole plot we 
  # apply the correction only to rows witch plot_ID != all, but stand and species == "all"
  mutate(across(contains("n_ha") | contains("n_SP") , ~ifelse(is.na(.x) & 
                                                                plot_ID != "all"&
                                                                stand == "all" &  
                                                                SP_code == "all" |
                                                                is.na(.x) & 
                                                                plot_ID != "all"&
                                                                is.na(stand) &  
                                                                SP_code == "all" , 0, .x)) ) %>% 
  arrange(plot_ID, stand, SP_code, compartiment)

# substact columns edning on BZE3 from columns ednign with HBI 
# https://stackoverflow.com/questions/47478125/create-new-columns-by-substracting-column-pairs-from-each-other-in-r
pre_vars <- grep("_HBI", colnames(RG_stock_changes_P), value=TRUE)
post_vars <- grep("_BZE3", colnames(RG_stock_changes_P), value=TRUE)
RG_stock_changes_P[, paste0(str_sub(pre_vars, end=-5), "_diff")] <- RG_stock_changes_P[, post_vars] - RG_stock_changes_P[, pre_vars]
RG_stock_changes_P <- RG_stock_changes_P %>% arrange(plot_ID, stand, SP_code, compartiment)


RG_changes <- RG_stock_changes_P %>% select(stand_component, plot_ID, stand, SP_code, compartiment, contains("diff"))
  

# 3. DEADWOOD CALCULATIONS --------------------------------------------
# 3.1. changes in stocks per ha --------------------------------------------
DW_stock_changes_P <- 
  BZE3_DW_summary %>% 
  select(stand_component, plot_ID, dw_sp, dw_type, ST_LY_type, decay, 
  compartiment, B_t_ha, C_t_ha, N_t_ha, n_ha, n_dec, n_dw_TY, mean_d_cm, sd_d_cm, mean_l_m, sd_l_m) %>%
  # https://rstats101.com/add-prefix-or-suffix-to-column-names-of-dataframe-in-r/
  rename_with(.fn = function(.x){paste0(.x,"_BZE3")},
              .cols= c(B_t_ha, C_t_ha, N_t_ha, n_ha, n_dec, n_dw_TY, mean_d_cm, sd_d_cm, mean_l_m, sd_l_m)) %>% 
  full_join(., HBI_DW_summary %>% 
              # only select plots that are also present in BZE3
              # but among those we join in all the strata possible 
              semi_join(BZE3_DW_summary %>% select(plot_ID) %>% distinct(), by = "plot_ID") %>% 
              #filter(plot_ID != "all" & SP_code == "all" & stand == "all") %>% 
              select(stand_component, plot_ID, dw_sp, dw_type, ST_LY_type, decay, 
                     compartiment, B_t_ha, C_t_ha, N_t_ha, n_ha, n_dec, n_dw_TY, mean_d_cm, sd_d_cm, mean_l_m, sd_l_m) %>%
              # https://rstats101.com/add-prefix-or-suffix-to-column-names-of-dataframe-in-r/
              rename_with(.fn = function(.x){paste0(.x,"_HBI")}, 
                          .cols= c(B_t_ha, C_t_ha, N_t_ha, n_ha, n_dec, n_dw_TY, mean_d_cm, sd_d_cm,  mean_l_m, sd_l_m)), 
            by = c("stand_component", "plot_ID", "dw_sp", "dw_type", "ST_LY_type", "decay", "compartiment")) %>% 
  # if there are plots/ species or stands that were not established in HBI and thus do not have stocks 
  # or if there are plots/ species or stands that are not present in BZE3 anymore but have stocks in HBI
  # we have to set their stock per ha to 0 to make sure the calculations can also track "negative growth"
  mutate(across(contains("t_ha"), ~ifelse(is.na(.x), 0, .x)) )%>% 
  # for n_ha and n_dw_TY, n_dec we do the same but as these values were calculated only for the whole plot we 
  # apply the correction only to rows witch plot_ID != all, but decay, dw_type and species  == "all"
  mutate(across(contains("n_ha") | contains("n_dec") | contains("n_dw_TY") , ~ifelse(is.na(.x) & 
                                                                plot_ID != "all"&
                                                                dw_sp == "all" &
                                                                dw_type == "all" & 
                                                                  decay == "all" , 0, .x)) )


# 3.2.replace NA for missing average value columns  --------------------------------------------
# replace NA for missing average value columns 
  # Average values were calcualted for the following groups: 
    # everything != "all" excecpt decay == "all" 
    # everything ! = "all" except dw_type == "all"
    # Everything != "all" except dw_sp == "all"
    # https://stackoverflow.com/questions/69560076/r-applying-condition-across-multiple-columns-ignoring-na -->  !!! this doensnt work
 DW_stock_changes_P <- 
  DW_stock_changes_P %>% 
   # https://stackoverflow.com/questions/24015557/count-occurrences-of-value-in-a-set-of-variables-in-r-per-row
   mutate(all_count = apply(DW_stock_changes_P[, c("plot_ID", 
                                                   "dw_sp", 
                                                   "dw_type", 
                                                   "decay" )], 1, function(x) length(which(x=="all"))) >= 2) %>% 
   mutate(across(contains("mean") | contains("sd") , ~ifelse(is.na(.x) & (apply(DW_stock_changes_P[, c("plot_ID", 
                                                                                                       "dw_sp", 
                                                                                                       "dw_type", 
                                                                                                       "decay" )], 1, function(x) length(which(x=="all"))) >= 2) == T, 
                                                              0, .x)) ) %>%
   arrange(plot_ID, dw_sp, dw_type, ST_LY_type, decay)


# 3.3.substract stocks  --------------------------------------------------------------------------------
# substact columns edning on BZE3 from columns ednign with HBI 
# https://stackoverflow.com/questions/47478125/create-new-columns-by-substracting-column-pairs-from-each-other-in-r
pre_vars <- grep("_HBI", colnames(DW_stock_changes_P), value=TRUE)
post_vars <- grep("_BZE3", colnames(DW_stock_changes_P), value=TRUE)
DW_stock_changes_P[, paste0(str_sub(pre_vars, end=-5), "_diff")] <- DW_stock_changes_P[, post_vars] - DW_stock_changes_P[, pre_vars]
DW_stock_changes_P <- DW_stock_changes_P %>% arrange(plot_ID, dw_sp, dw_type, ST_LY_type, decay, compartiment)

DW_changes <- DW_stock_changes_P %>% select(stand_component, plot_ID, dw_sp, dw_type, ST_LY_type, decay, compartiment, contains("diff"))


# DW categories we have an average for: those that have two grouping variabels == "all" !!!!!!
# filter for any column containing "all": 
 # https://stackoverflow.com/questions/67092641/check-if-any-n-columns-contains-the-same-value-using-r
# if_all(y:z, ~ x == .x)
  # - plot_ID, inv, ST_LY_type, dw_type != "all", dw_sp = "all", decay = "all"
  # - plot_ID, inv, decay != "all", dw_sp = "all", dw_type = "all", is.na(ST_LY_type)
  # - plot_ID, inv, dw_sp != "all", decay = "all", dw_type = "all", is.na(ST_LY_type)
  # - plot_ID, inv != all, decay = "all", dw_type = "all", dw_sp = "all", is.na(ST_LY_type)



# 4. total changes over LT, RG, DW together -------------------------------
# 4.1. calculating total difference in stock per plot per ha --------------
# stand_component == "all"
all_components_stock_changes_P <- 
  # filter for the stand component "all" to get RG, DW, and LT stocks accumulated over the whole plot
  # but only select plotwise data, since the stand type wise summaries are compared in processing step 4 
  BZE3_summary %>% filter(stand_component == "all" & plot_ID != "all") %>%
  select(stand_component, plot_ID, compartiment, B_t_ha, C_t_ha, N_t_ha, stand_type) %>%
  # https://rstats101.com/add-prefix-or-suffix-to-column-names-of-dataframe-in-r/
  rename_with(.fn = function(.x){paste0(.x,"_BZE3")},
              .cols= c(B_t_ha, C_t_ha, N_t_ha, stand_type)) %>% 
  full_join(., HBI_summary %>% filter(stand_component == "all" & plot_ID != "all") %>%
              # make sure to only select those plots that are represented in both inventories
              # but use a full join to also make sure that within one plot all groups from both 
              # inventories are compared with each other
              semi_join(BZE3_summary %>% select(plot_ID) %>% distinct(), by = "plot_ID")%>% 
              select(stand_component, plot_ID, compartiment, B_t_ha, C_t_ha, N_t_ha, stand_type) %>%
              # https://rstats101.com/add-prefix-or-suffix-to-column-names-of-dataframe-in-r/
              rename_with(.fn = function(.x){paste0(.x,"_HBI")}, 
                          .cols= c(B_t_ha, C_t_ha, N_t_ha, stand_type)), 
            by = c("stand_component", "plot_ID", "compartiment")) %>% 
  # if there are plots/ species or stands that were not established in HBI and thus do not have stocks 
  # or if there are plots/ species or stands that are not present in BZE3 anymore but have stocks in HBI
  # we have to set their stock per ha to 0 to make sure the calculations can also track "negative growth"
  mutate(across(contains("t_ha") | contains("stand_type"), ~ifelse(is.na(.x), 0, .x)) ) 

# 4.2. substract columns ----------------------------------------------------------------------------------------
# substact columns edning on BZE3 from columns ednign with HBI 
# https://stackoverflow.com/questions/47478125/create-new-columns-by-substracting-column-pairs-from-each-other-in-r
pre_vars <- grep("_HBI", colnames(all_components_stock_changes_P), value=TRUE)
post_vars <- grep("_BZE3", colnames(all_components_stock_changes_P), value=TRUE)
all_components_stock_changes_P[, paste0(str_sub(pre_vars, end=-5), "_diff")] <- all_components_stock_changes_P[, post_vars] - all_components_stock_changes_P[, pre_vars]
all_components_stock_changes_P <- all_components_stock_changes_P %>% arrange(plot_ID, compartiment)

all_components_changes <- all_components_stock_changes_P %>% 
  select(stand_component, plot_ID, compartiment, contains("diff")) %>% 
  mutate(stand = "all", 
         C_layer = "all", 
         SP_code = "all")


# 4.3. binding all forest datasets (LT, RG, DW) together ------------------
LT_RG_DW_P_changes <- plyr::rbind.fill(LT_changes, 
                                       RG_changes,
                                       DW_changes, 
                                       all_components_changes) %>% 
  arrange(plot_ID)



# 5. STAND TYPE CALCULATIONS -------------------------------------------------------
# 5.1. prepare dataset for changes in stock per ha -------------------------------------------------------
# changes in stocks per ha 
TY_stock_changes_P <- 
  BZE3_TY_summary %>% 
  select(-c(inv)) %>% 
  # https://rstats101.com/add-prefix-or-suffix-to-column-names-of-dataframe-in-r/
  rename_with(.fn = function(.x){paste0(.x,"_BZE3")},
              .cols= c(B_t_ha, C_t_ha, N_t_ha, BA_m2_ha, n_ha, n_SP)) %>% 
  full_join(., HBI_TY_summary %>% 
              select(-c(inv)) %>% 
              # only select plots that are also present in BZE3
              # but among those we join in all the strata possible 
              semi_join(BZE3_TY_summary %>% select(plot_ID) %>% distinct(), by = "plot_ID") %>% 
              #filter(plot_ID != "all" & SP_code == "all" & stand == "all")
              # https://rstats101.com/add-prefix-or-suffix-to-column-names-of-dataframe-in-r/
              rename_with(.fn = function(.x){paste0(.x,"_HBI")}, 
                          .cols= c(B_t_ha, C_t_ha, N_t_ha, BA_m2_ha, n_ha, n_SP)), 
            by = c("stand_type", "stand_component", "plot_ID","dom_SP", "stand", "SP_code", "compartiment" )) %>% 
  # if there are plots/ species  or other strata that are in HBI forest type summaries but not in 
  # BZE3 forest type summaries or the other way round, we have to replace the NAs this results in by 0 
  # to make sure the calculations can also track "negative growth"
  mutate(across(contains("t_ha") | contains("n_ha") | contains("n_SP") | contains("BA_m2") , ~ifelse(is.na(.x), 0, .x)) ) 
 
# 5.2. substract respective columns of both inventories  -------------------------------------------------------
# substact columns edning on BZE3 from columns ednign with HBI 
 # https://stackoverflow.com/questions/47478125/create-new-columns-by-substracting-column-pairs-from-each-other-in-r
pre_vars <- grep("_HBI", colnames(TY_stock_changes_P), value=TRUE)
post_vars <- grep("_BZE3", colnames(TY_stock_changes_P), value=TRUE)
TY_stock_changes_P[, paste0(str_sub(pre_vars, end=-5), "_diff")] <- TY_stock_changes_P[, post_vars] - TY_stock_changes_P[, pre_vars]

# 5.3. prepare dataset for changes by forest type  -------------------------------------------------------
TY_stock_changes <- TY_stock_changes_P %>% 
  select("stand_type", "stand_component", "plot_ID","dom_SP", "stand", "SP_code", "compartiment", contains("diff"))






# 5. FSI changes ----------------------------------------------------------
FSI_changes_P <- 
  BZE3_FSI %>% 
  select(plot_ID, contains("FSI")) %>% 
  # https://rstats101.com/add-prefix-or-suffix-to-column-names-of-dataframe-in-r/
  rename_with(.fn = function(.x){paste0(.x,"_BZE3")},
              .cols= LT_FSI_DBH_RMS:FSI_total) %>% 
  left_join(.,  HBI_FSI %>% 
              select(plot_ID, contains("FSI")) %>% 
              # https://rstats101.com/add-prefix-or-suffix-to-column-names-of-dataframe-in-r/
              rename_with(.fn = function(.x){paste0(.x,"_HBI")},
                          .cols= LT_FSI_DBH_RMS:FSI_total), 
            by = "plot_ID")%>% 
  # if there are plots/ species or stands that were not established in HBI and thus do not have stocks 
  # or if there are plots/ species or stands that are not present in BZE3 anymore but have stocks in HBI
  # we have to set their stock per ha to 0 to make sure the calculations can also track "negative growth"
  mutate(across(contains("FSI"), ~ifelse(is.na(.x), 0, .x)) )

# substact columns edning on BZE3 from columns ednign with HBI 
# https://stackoverflow.com/questions/47478125/create-new-columns-by-substracting-column-pairs-from-each-other-in-r
pre_vars <- grep("_HBI", colnames(FSI_changes_P), value=TRUE)
post_vars <- grep("_BZE3", colnames(FSI_changes_P), value=TRUE)
FSI_changes_P[, paste0(str_sub(pre_vars, end=-5), "_diff")] <- FSI_changes_P[, post_vars] - FSI_changes_P[, pre_vars]
FSI_changes_P <- FSI_changes_P %>% arrange(plot_ID) %>% 
  select(plot_ID, contains("diff")) %>% 
  # as the FSI is calculated plot wise we don´t differ ebtween strata at the plot and have to set them to "all"
  mutate(stand_component = "all", 
         stand = "all", 
         C_layer = "all", 
         SP_code = "all") 










# 2. data export ----------------------------------------------------------
write.csv(LT_changes, paste0(out.path.BZE3, paste(HBI_trees$inv[1], BZE3_trees$inv[1], "LT_changes", sep = "_"), ".csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(RG_changes, paste0(out.path.BZE3, paste(HBI_RG$inv[1], BZE3_RG$inv[1], "RG_changes", sep = "_"), ".csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(DW_changes, paste0(out.path.BZE3, paste(HBI_DW$inv[1], BZE3_DW$inv[1], "DW_changes", sep = "_"), ".csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(LT_RG_DW_P_changes, paste0(out.path.BZE3, paste(HBI_trees$inv[1], BZE3_trees$inv[1], "LT_RG_DW_changes_all_groups_P", sep = "_"), ".csv"), row.names = FALSE, fileEncoding = "UTF-8")

write.csv(FSI_changes_P, paste0(out.path.BZE3, paste(HBI_trees$inv[1], BZE3_trees$inv[1], "FSI_changes_P", sep = "_"), ".csv"), row.names = FALSE, fileEncoding = "UTF-8")

# forest type wise summarised changes
write.csv(TY_stock_changes, paste0(out.path.BZE3, paste(HBI_trees$inv[1], BZE3_trees$inv[1], "LT_RG_DW_changes_all_groups_TY", sep = "_"), ".csv"), row.names = FALSE, fileEncoding = "UTF-8")


stop("this is where notes of growth script hbi bze3 start")


# notes -------------------------------------------------------------------


trees_stock_changes_P <- trees_stock_changes_P %>% select(stand_component, plot_ID,stand , stand_type, SP_code,compartiment, 
                                                          B_t_ha_BZE3, B_t_ha_HBI, B_t_ha_diff, 
                                                          C_t_ha_BZE3, C_t_ha_HBI, C_t_ha_diff, 
                                                          N_t_ha_BZE3, N_t_ha_HBI, N_t_ha_diff, 
                                                          n_ha_BZE3, n_ha_HBI, n_ha_diff, 
                                                          n_SP_BZE3, n_SP_HBI, n_SP_diff,
                                                          mean_DBH_cm_BZE3, mean_DBH_cm_HBI, mean_DBH_cm_diff, 
                                                          sd_DBH_cm_BZE3, sd_DBH_cm_HBI, sd_DBH_cm_diff, 
                                                          Dg_cm_BZE3, Dg_cm_HBI, Dg_cm_diff, 
                                                          mean_BA_m2_BZE3, mean_BA_m2_HBI, mean_BA_m2_diff, 
                                                          mean_H_m_BZE3, mean_H_m_HBI, mean_H_m_diff)




rbind(BZE3_LT_summary,
      HBI_LT_summary ) %>%
  # select all possible plot_iD, species and stand combinations
  select(plot_ID, SP_code, stand) %>% distinct()

nrow(rbind(BZE3_LT_summary,
           HBI_LT_summary ) %>%
       # select all possible plot_iD, species and stand combinations
       select(stand_component, plot_ID, stand, stand_type, SP_code, compartiment) %>% distinct()) # 6258

nrow(full_join(BZE3_LT_summary %>% 
                 select(stand_component, plot_ID, stand, stand_type, SP_code, compartiment) %>% distinct(), 
               HBI_LT_summary %>% 
                 select(stand_component, plot_ID, stand, stand_type, SP_code, compartiment) %>% distinct(), 
               by =c("stand_component", "stand_type" ,"plot_ID", "compartiment", "SP_code", "stand"))) # HBI and BZE3 have 6258 rows all together

nrow(left_join(BZE3_LT_summary %>% 
                 select(stand_component, plot_ID, stand, stand_type, SP_code, compartiment) %>% distinct(), 
               HBI_LT_summary %>% 
                 select(stand_component, plot_ID, stand, stand_type, SP_code, compartiment) %>% distinct(), 
               by =c("stand_component", "stand_type" ,"plot_ID", "compartiment", "SP_code", "stand"))) # HBI and BZE have 2424 rows in common


view(anti_join(HBI_LT_summary %>% 
                 select(stand_component, plot_ID, stand, stand_type, SP_code) %>% distinct(), 
               BZE3_LT_summary %>% 
                 select(stand_component, plot_ID, stand, stand_type, SP_code) %>% distinct(), 
               by =c("stand_component", "stand_type" ,"plot_ID", "SP_code", "stand"))) # HBI has 3834 rows that BZE3 doesnt have


view(anti_join(BZE3_LT_summary %>% 
                 select(stand_component, plot_ID, stand, stand_type, SP_code) %>% distinct(),
               HBI_LT_summary %>% 
                 select(stand_component, plot_ID, stand, stand_type, SP_code) %>% distinct(), 
               by =c("stand_component", "stand_type" ,"plot_ID", "SP_code", "stand"))) # BZE3 has 1020 rows that HBI doesn´t have


HBI_LT_summary %>% filter(plot_ID == 140041 & stand == "A" & SP_code == "sbi" & compartiment == "ag")

nrow(BZE3_LT_summary %>% 
       #filter(plot_ID != "all" & SP_code == "all" & stand == "all") %>% 
       select(stand_component, plot_ID, stand, stand_type, SP_code, compartiment, B_t_ha, C_t_ha, N_t_ha, n_ha, n_SP, 
              mean_DBH_cm, sd_DBH_cm, Dg_cm, mean_BA_m2, mean_H_m, sd_H_m, Hg_m) %>%
       #add "_BZE3" to the names of the valriables we want to calculate the difference for:  https://rstats101.com/add-prefix-or-suffix-to-column-names-of-dataframe-in-r/
       rename_with(.fn = function(.x){paste0(.x,"_BZE3")},
                   .cols= c(B_t_ha, C_t_ha, N_t_ha, n_ha, n_SP, mean_DBH_cm, sd_DBH_cm, Dg_cm, mean_BA_m2, mean_H_m, sd_H_m, Hg_m)) %>%
       distinct()  %>% 
       # jopin in HBI dataset via left join, so that only plots, species, C layers and stands are joined in that have a partner in the current inventory. 
       left_join(., HBI_LT_summary %>% 
                   #filter(plot_ID != "all" & SP_code == "all" & stand == "all") %>% 
                   select(stand_component, plot_ID, stand, stand_type, SP_code, compartiment, B_t_ha, C_t_ha, N_t_ha, n_ha, n_SP, 
                          mean_DBH_cm, sd_DBH_cm, Dg_cm, mean_BA_m2, mean_H_m, sd_H_m, Hg_m) %>%
                   # https://rstats101.com/add-prefix-or-suffix-to-column-names-of-dataframe-in-r/
                   rename_with(.fn = function(.x){paste0(.x,"_HBI")}, 
                               .cols= c(B_t_ha, C_t_ha, N_t_ha, n_ha, n_SP, 
                                        mean_DBH_cm, sd_DBH_cm, Dg_cm, mean_BA_m2, mean_H_m, sd_H_m, Hg_m)) %>% 
                   distinct(), 
                 by = c("stand_component", "stand_type" ,"plot_ID", "compartiment", "SP_code", "stand"))) # 2424


nrow(full_join(
  BZE3_LT_summary %>% 
    #filter(plot_ID != "all" & SP_code == "all" & stand == "all") %>% 
    select(stand_component, plot_ID, stand, stand_type, SP_code, compartiment, B_t_ha, C_t_ha, N_t_ha, n_ha, n_SP, 
           mean_DBH_cm, sd_DBH_cm, Dg_cm, mean_BA_m2, mean_H_m, sd_H_m, Hg_m) %>%
    #add "_BZE3" to the names of the valriables we want to calculate the difference for:  https://rstats101.com/add-prefix-or-suffix-to-column-names-of-dataframe-in-r/
    rename_with(.fn = function(.x){paste0(.x,"_BZE3")},
                .cols= c(B_t_ha, C_t_ha, N_t_ha, n_ha, n_SP, mean_DBH_cm, sd_DBH_cm, Dg_cm, mean_BA_m2, mean_H_m, sd_H_m, Hg_m)) %>%
    distinct(), 
  # jopin in HBI dataset via left join, so that only plots, species, C layers and stands are joined in that have a partner in the current inventory. 
  HBI_LT_summary %>% 
    #filter(plot_ID != "all" & SP_code == "all" & stand == "all") %>% 
    select(stand_component, plot_ID, stand, stand_type, SP_code, compartiment, B_t_ha, C_t_ha, N_t_ha, n_ha, n_SP, 
           mean_DBH_cm, sd_DBH_cm, Dg_cm, mean_BA_m2, mean_H_m, sd_H_m, Hg_m) %>%
    # https://rstats101.com/add-prefix-or-suffix-to-column-names-of-dataframe-in-r/
    rename_with(.fn = function(.x){paste0(.x,"_HBI")}, 
                .cols= c(B_t_ha, C_t_ha, N_t_ha, n_ha, n_SP, 
                         mean_DBH_cm, sd_DBH_cm, Dg_cm, mean_BA_m2, mean_H_m, sd_H_m, Hg_m)) %>% 
    distinct(), 
  by = c("stand_component", "stand_type" ,"plot_ID", "compartiment", "SP_code", "stand"))) # 6258






# full join changes culaction check
trees_stock_changes_P <- 
  full_join(BZE3_LT_summary %>% 
  #filter(plot_ID != "all" & SP_code == "all" & stand == "all") %>% 
  select(stand_component, plot_ID, stand, stand_type, SP_code, compartiment, B_t_ha, C_t_ha, N_t_ha, n_ha, n_SP, 
         mean_DBH_cm, sd_DBH_cm, Dg_cm, mean_BA_m2, mean_H_m, sd_H_m, Hg_m) %>%
  #add "_BZE3" to the names of the valriables we want to calculate the difference for:  https://rstats101.com/add-prefix-or-suffix-to-column-names-of-dataframe-in-r/
  rename_with(.fn = function(.x){paste0(.x,"_BZE3")},
              .cols= c(B_t_ha, C_t_ha, N_t_ha, n_ha, n_SP, mean_DBH_cm, sd_DBH_cm, Dg_cm, mean_BA_m2, mean_H_m, sd_H_m, Hg_m)) %>%
  distinct(), 
  # jopin in HBI dataset via left join, so that only plots, species, C layers and stands are joined in that have a partner in the current inventory. 
  HBI_LT_summary %>% 
    #filter(plot_ID != "all" & SP_code == "all" & stand == "all") %>% 
    select(stand_component, plot_ID, stand, stand_type, SP_code, compartiment, B_t_ha, C_t_ha, N_t_ha, n_ha, n_SP, 
           mean_DBH_cm, sd_DBH_cm, Dg_cm, mean_BA_m2, mean_H_m, sd_H_m, Hg_m) %>%
    # https://rstats101.com/add-prefix-or-suffix-to-column-names-of-dataframe-in-r/
    rename_with(.fn = function(.x){paste0(.x,"_HBI")}, 
                .cols= c(B_t_ha, C_t_ha, N_t_ha, n_ha, n_SP, 
                         mean_DBH_cm, sd_DBH_cm, Dg_cm, mean_BA_m2, mean_H_m, sd_H_m, Hg_m)) %>% 
              distinct(), 
            by = c("stand_component", "stand_type" ,"plot_ID", "compartiment", "SP_code", "stand")) %>% 
  # if there are plots/ species or stands that were not established in HBI and thus do not have stocks 
  # or if there are plots/ species or stands that are not present in BZE3 anymore but have stocks in HBI
  # we have to set their stock per ha to 0 to make sure the calculations can also track "negative growth"
  mutate(across(contains("t_ha"), ~ifelse(is.na(.x), 0, .x)) )%>% 
  # for n_ha and n_SP we do the same but as these values were calculated only for the whole plot we 
  # apply the correction only to rows witch plot_ID != all, but stand and species == "all"
  mutate(across(contains("n_ha") | 
                  contains("n_SP")  | 
                  contains("Hg")  | 
                  contains("Dg"), ~ifelse(is.na(.x) & 
                                            plot_ID != "all"&
                                            stand == "all" & 
                                            SP_code == "all", 0, .x)) ) %>% 
  # for the means and sds we calcualted values in the catagories : SP_code, plot and plot. Thus we have to set only NAs that occure in these groups and columns to 0 
  mutate(across(contains("mean") | contains("sd")  | contains("Hg")  | contains("Dg") , ~ifelse(is.na(.x) & 
                                                                                                  plot_ID != "all"&
                                                                                                  stand == "all" & 
                                                                                                  SP_code != "all", 0, .x)) ) %>% 
  arrange(plot_ID, stand, SP_code, compartiment)

# substact columns edning on BZE3 from columns ednign with HBI 
# https://stackoverflow.com/questions/47478125/create-new-columns-by-substracting-column-pairs-from-each-other-in-r
pre_vars <- grep("_HBI", colnames(trees_stock_changes_P), value=TRUE)
post_vars <- grep("_BZE3", colnames(trees_stock_changes_P), value=TRUE)
trees_stock_changes_P[, paste0(str_sub(pre_vars, end=-5), "_diff")] <- trees_stock_changes_P[, post_vars] - trees_stock_changes_P[, pre_vars]
trees_stock_changes_P <- trees_stock_changes_P %>% arrange(plot_ID, stand, SP_code, compartiment) %>%
  mutate(C_layer = "all")
