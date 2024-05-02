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
BZE3_trees <- read.delim(file = here(paste0(out.path.BZE3, "BZE3_LT_update_4.csv")), sep = ";", dec = ",")
BZE3_summary <- read.delim(file = here(paste0(out.path.BZE3, "BZE3_LT_stocks_ha_all_groups.csv")), sep = ";", dec = ",")

HBI_trees <- read.delim(file = here(paste0(out.path.BZE3, "HBI_LT_update_4.csv")), sep = ";", dec = ",")
HBI_summary <- read.delim(file = here(paste0(out.path.BZE3, "HBI_LT_stocks_ha_all_groups.csv")), sep = ";", dec = ",")



# 1. calculations ---------------------------------------------------------
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
growth_summary <- plyr::rbind.fill(
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
           plot_ID = "all"))




# 1.3. changes in BA composition -------------------------------------------
# select all possible tree species per plot
BA_changes_SP_P <- rbind(BZE3_trees %>% select(plot_ID, SP_code) %>% distinct(),
                    HBI_trees %>% select(plot_ID, SP_code) %>% distinct()) %>% 
  distinct() %>% 
  arrange(plot_ID) %>% 
  left_join(., BZE3_summary %>% 
              # filter for plot and species wise summary
              filter(plot_ID != "all" & SP_code != "all" & stand == "all") %>%
              # select the BA percent
              select(plot_ID, SP_code, BA_percent) %>% 
              mutate(across(c("plot_ID"), as.integer)) %>% 
              rename(BA_percent_BZE3 = BA_percent) %>% 
              distinct(), 
            by = c("plot_ID", "SP_code")) %>% 
  left_join(., HBI_summary %>% 
              # filter for plot and species wise summary
              filter(plot_ID != "all" & SP_code != "all" & stand == "all") %>%
              # select the BA percent
              select(plot_ID, SP_code, BA_percent) %>% 
              mutate(across(c("plot_ID"), as.integer)) %>% 
              rename(BA_percent_HBI = BA_percent) %>% 
              distinct(), 
            by = c("plot_ID", "SP_code")) %>%
  # here we have to set the BA_percent that do not appear in the respective inventory to 0
  mutate(BA_percent_BZE3 = ifelse(is.na(BA_percent_BZE3), 0, BA_percent_BZE3), 
         BA_percent_HBI = ifelse(is.na(BA_percent_HBI), 0, BA_percent_HBI), 
         BA_percent_difference = BA_percent_BZE3-BA_percent_HBI)
  


# 1.4. changes in stocks per ha --------------------------------------------
stock_changes_P <- 
BZE3_summary %>% 
  #filter(plot_ID != "all" & SP_code == "all" & stand == "all") %>% 
  select(plot_ID, stand, stand_type, SP_code, compartiment, B_t_ha, C_t_ha, N_t_ha, n_ha, n_SP) %>%
  # https://rstats101.com/add-prefix-or-suffix-to-column-names-of-dataframe-in-r/
  rename_with(.fn = function(.x){paste0(.x,"_BZE3")},
              .cols= c(B_t_ha, C_t_ha, N_t_ha, n_ha, n_SP)) %>% 
  left_join(., HBI_summary %>% 
              #filter(plot_ID != "all" & SP_code == "all" & stand == "all") %>% 
              select(plot_ID, stand, stand_type, SP_code, compartiment, B_t_ha, C_t_ha, N_t_ha, n_ha, n_SP) %>%
              # https://rstats101.com/add-prefix-or-suffix-to-column-names-of-dataframe-in-r/
              rename_with(.fn = function(.x){paste0(.x,"_HBI")}, 
                          .cols= c(B_t_ha, C_t_ha, N_t_ha, n_ha, n_SP)), 
            by = c("stand_type" ,"plot_ID", "compartiment", "SP_code", "stand")) %>% 
  # if there are plots/ species or stands that were not established in HBI and thus do not have stocks 
  # or if there are plots/ species or stands that are not present in BZE3 anymore but have stocks in HBI
  # we have to set their stock per ha to 0 to make sure the calculations can also track "negative growth"
  mutate(across(contains("t_ha"), ~ifelse(is.na(.x), 0, .x)) )%>% 
  # for n_ha and n_SP we do the same but as these values were calculated only for the whole plot we 
  # apply the correction only to rows witch plot_ID != all, but stand and species == "all"
  mutate(across(contains("n_ha") | contains("n_SP") , ~ifelse(is.na(.x) & 
                                                      plot_ID != "all"&
                                                      stand == "all" & 
                                                      SP_code == "all", 0, .x)) ) %>% 
  arrange(plot_ID, stand, SP_code, compartiment)

# substact columns edning on BZE3 from columns ednign with HBI 
# https://stackoverflow.com/questions/47478125/create-new-columns-by-substracting-column-pairs-from-each-other-in-r
pre_vars <- grep("_HBI", colnames(stock_changes_P), value=TRUE)
post_vars <- grep("_BZE3", colnames(stock_changes_P), value=TRUE)
stock_changes_P[, paste0(str_sub(pre_vars, end=-5), "_diff")] <- stock_changes_P[, post_vars] - stock_changes_P[, pre_vars]
stock_changes_P <- stock_changes_P %>% arrange(plot_ID, stand, SP_code, compartiment)

left_join(BZE3_summary[2416,],
HBI_summary %>% filter( plot_ID == "all" & is.na(stand) & SP_code == "all" & compartiment == "ag" & stand_type == 1) %>% distinct()
,by = c("plot_ID", "stand_type", "compartiment"))



# 2. data export ----------------------------------------------------------
write.csv2(growth_summary, paste0(out.path.BZE3, paste(inv_name(HBI_trees$inv_year)[1], inv_name(BZE3_trees$inv_year)[1], "LT_dbh_growth", sep = "_"), ".csv"))



