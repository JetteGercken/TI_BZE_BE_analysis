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
BZE3_trees_removed <- read.delim(file = here(paste0(out.path.BZE3, "BZE3_LT_removed.csv")), sep = ",", dec = ".")
BZE3_trees <- read.delim(file = here(paste0(out.path.BZE3, "BZE3_LT_update_4.csv")), sep = ",", dec = ".")
HBI_trees <- read.delim(file = here(paste0(out.path.BZE3, "HBI_LT_update_4.csv")), sep = ",", dec = ".") 
HBI_trees_stat_2 <- read.delim(file = here(paste0(out.path.BZE3, HBI_trees$inv[1], "_LT_stat_2.csv")), sep = ",", dec = ".") %>% 
  mutate(inv = inv_name(inv_year))

# tree summaries 
HBI_summary <- read.delim(file = here(paste0(out.path.BZE3, "HBI_LT_RG_DW_stocks_ha_all_groups.csv")), sep = ",", dec = ".") 
BZE3_summary <- read.delim(file = here(paste0(out.path.BZE3, "BZE3_LT_RG_DW_stocks_ha_all_groups.csv")), sep = ",", dec = ".") 

  
# height coefficient 
# recreate the same datasets we use in the height calculation script
coeff_H_SP_P <- read.delim(file = here(paste0(out.path.BZE3,"coef_H_nls.csv")), sep = ",", dec = ".") %>% filter(plot_ID != "all")
coeff_H_SP <- read.delim(file = here(paste0(out.path.BZE3,"coef_H_nls.csv")), sep = ",", dec = ".") %>% filter(plot_ID == "all")


# 0.4 data prep -----------------------------------------------------------
# 0.4.1. add DBH class and state to tree datasets -----------------------------------------------------------
BZE3_trees_removed <- BZE3_trees_removed  %>%    
  # select(-c(compartiment, B_kg_tree, C_kg_tree, N_kg_tree)) %>% 
  mutate(DBH_class_10 = DBH_c_function(DBH_cm, "class_10"), 
         state = ifelse(stringr::str_length(plot_ID) == 5, substr(plot_ID, 1, 1), substr(plot_ID, 1, 2))) %>% 
  distinct()

BZE3_trees <- BZE3_trees %>% 
  select(-c(compartiment, B_kg_tree, C_kg_tree, N_kg_tree)) %>% 
  mutate(DBH_class_10 = DBH_c_function(DBH_cm, "class_10"), 
         state = ifelse(stringr::str_length(plot_ID) == 5, substr(plot_ID, 1, 1), substr(plot_ID, 1, 2))) %>% 
  distinct()

HBI_trees <- HBI_trees %>% 
  select(-c(compartiment, B_kg_tree, C_kg_tree, N_kg_tree)) %>% 
  mutate(DBH_class_10 = DBH_c_function(DBH_cm, "class_10"), 
         state = ifelse(stringr::str_length(plot_ID) == 5, substr(plot_ID, 1, 1), substr(plot_ID, 1, 2))) %>% 
  distinct()


# 0.4.2. filter summaries -------------------------------------------------
# tree summaries 
HBI_summary <- HBI_summary %>% 
  filter(stand_component == "LT" & plot_ID != "all") %>% 
  select(inv, plot_ID, stand, SP_code, mean_DBH_cm,Dg_cm, Hg_m) %>% distinct()

BZE3_summary <- BZE3_summary %>% 
  filter(stand_component == "LT" & plot_ID != "all") %>% 
  select(inv, plot_ID, stand, SP_code, mean_DBH_cm,Dg_cm, Hg_m)%>% distinct()



# 1. calculations --------------------------------------------------------------------------------------
# 1.1. find  trees in HBI dataset that were found removed in BZE3  ------------------------------------
# filter join for trees that are labelled with tree inventory status 2 in the BZE3 (post) inventory 
# we have to look for them in HBI_update_3 because we need the trees with the height already estimated
# filter for number 7 too
trees_harvested <- HBI_trees %>%
  # remove tree inventory status from HBI dataset as we want to keep the status that was assingned in BZE3
  select(-tree_inventory_status) %>% 
  semi_join(., BZE3_trees_removed %>% 
              filter(tree_inventory_status %in% c(2, 7)), # filter for trees in HBI which have the same plot_ID and tree_ID of those marked 2 in BZE3
            by = c("plot_ID", "tree_ID")) %>% 
  left_join(., BZE3_trees_removed %>% select(plot_ID, tree_ID, tree_inventory_status) %>% 
              filter(tree_inventory_status %in% c(2, 7)), # filter for trees in HBI which have the same plot_ID and tree_ID of those marked 2 in BZE3
            by = c("plot_ID", "tree_ID")) %>% 
  distinct()



# 1.2. calcualte & model annual DBH growth ------------------------------------------------------------------------------
# 1.2.1. calcualte annual DBH growth  ------------------------------------------------------------------------------
## join HBI and BZE3 single tree diameters together by tree & plot ID and calculate differences
dbh_growth_tree <- left_join(
 ## BZE3 trees
  # select trees that are repeatedly inventory, or unknown status
  BZE3_trees %>% 
    filter(tree_inventory_status %in% c(1)) %>% 
    rename(BZE3_DBH_cm = DBH_cm) %>% 
    rename(BZE3_inv_year = inv_year) %>% 
    select(state, plot_ID, tree_ID, BZE3_inv_year, stand, DBH_class_10, SP_code, BZE3_DBH_cm), 
 ## HBI trees
  HBI_trees %>% 
    # select trees that were newly inventored, repeated inventory, or unknown status
    filter(tree_inventory_status %in% c(0, 1, -9))%>% 
    distinct() %>% 
    rename(HBI_DBH_cm = DBH_cm) %>% 
    rename(HBI_inv_year = inv_year) %>% 
    select(state, plot_ID, tree_ID, HBI_inv_year, stand, DBH_class_10, SP_code, HBI_DBH_cm), 
  by = c("state", "plot_ID", "tree_ID", "DBH_class_10", "stand", "SP_code"), 
  multiple = "all") %>%    
  # there may be trees that are new in BZE3 and havent been inventorised in HBI
  # so we have to put these trees DBHs to 0 and the invenotry year to the one of the other trees
  # to calculate the increment properly 
  mutate(HBI_DBH_cm = ifelse(is.na(HBI_DBH_cm), 0, HBI_DBH_cm), 
         HBI_inv_year = ifelse(is.na(HBI_inv_year), 2012, HBI_inv_year)) %>% 
  # calcualte difference 
  mutate(DBH_growth_cm = BZE3_DBH_cm - HBI_DBH_cm,            # difference between BZE3 diameter and HBI diameter
         age_period = BZE3_inv_year- HBI_inv_year,            # years passed between BZE3 and HBI
         annual_growth_cm = DBH_growth_cm/age_period)         # average DBH growth per year
  


# 1.2.2. summarize annual DBH growth  ------------------------------------------------------------------------------
# summarze dbh growth in different categories 
growth <- plyr::rbind.fill(
  # annual dbh growth by plot, species, DBH class and stand
  summarize_data(dbh_growth_tree,
                 c("plot_ID", "stand", "SP_code", "DBH_class_10"), 
                 c("annual_growth_cm", "age_period"), 
                 operation = "mean_df"), 
  # annual dbh growth by plot, species and DBH class
  summarize_data(dbh_growth_tree, 
                 c("plot_ID", "DBH_class_10", "SP_code"), 
                 c("annual_growth_cm", "age_period"), 
                 operation = "mean_df") %>% 
    mutate(stand = "all"))


# 1.2.3. coefficients for growth dbh model via nls_forest  ------------------------------------------------------------------------------
growth_coeff <- left_join(
  # select variables needed for modeling 
  dbh_growth_tree %>% select(state, SP_code, annual_growth_cm, BZE3_DBH_cm) %>% 
    # make sure only trees with calcualted growth are inlcuded in modelling 
    filter(!is.na(annual_growth_cm) & !is.na(BZE3_DBH_cm)) %>% 
    group_by(state, SP_code) %>%
    # filter for plots that have at least 3 dbh growths measured per species and dbh class
     filter(n() >= 3),
  # creating & joining in growth coefficients dataset 
  dbh_growth_tree %>% select(state, SP_code, annual_growth_cm, BZE3_DBH_cm) %>% 
    # make sure only trees with calcualted growth are inlcuded in modelling 
    filter(!is.na(annual_growth_cm) & !is.na(BZE3_DBH_cm)) %>% 
    group_by(state, SP_code) %>%
    # filter for plots that have at least 3 dbh growths measured per species and dbh class
    filter(n() >= 3)%>%    
    group_by(state, SP_code) %>%
    # model mean annual dbh growth in cm in relation to BZE3 diameter 
    nls_table( annual_growth_cm ~ b0 * (1 - exp( -b1 * BZE3_DBH_cm))^b2, 
               mod_start = c(b0=23, b1=0.03, b2 =1.3), 
               output = "table") %>%
    arrange(state, SP_code), 
  by = c("state", "SP_code")) %>%
  # mutating statistical predictors
  mutate(growth_est = b0 * (1 - exp( -b1 * BZE3_DBH_cm))^b2) %>% 
  group_by(state, SP_code) %>% 
  summarise( b0 = mean(b0), 
             b1 = mean(b1), 
             b2 = mean(b2), 
             #https://rdrr.io/cran/forestmangr/f/vignettes/eq_group_fit_en.Rmd
             bias = bias_per(y = annual_growth_cm, yhat = growth_est),
             rsme = rmse_per(y = annual_growth_cm, yhat = growth_est),
             #https://stackoverflow.com/questions/14530770/calculating-r2-for-a-nonlinear-least-squares-fit
             R2 = max(cor(annual_growth_cm, growth_est),0)^2,
             #https://stats.stackexchange.com/questions/11676/pseudo-r-squared-formula-for-glms
             mean_growth = mean(annual_growth_cm), 
             SSres = sum((annual_growth_cm-growth_est)^2), 
             SStot = sum((annual_growth_cm-mean_growth)^2), 
             pseu_R2 = 1-(SSres/SStot), 
             diff_growth = mean(annual_growth_cm - growth_est) ,
             n = n())


# 1.3. DBH estimation --------------------------------------------------------------------------------------
#  the goal is to add 4 times diameter growth to the diameter of the trees removed between HBI and BZE3
# we will build a loop that selects the growth per plot, stand, DBH class, species
# if we can´t find average growth in this group, select growth by plot, DBH_class, species
# if we can´t find average growth in this group use model that describes 

# 1.3.1. assinging DBH growth --------------------------------------------------------------------------------------
# prepare dataset for DBH at middle of harvesting period 

dbh_incl_growth.list <- vector("list", length = length(unique(trees_harvested$tree_ID)))
for (i in 1:nrow(unique(trees_harvested[, c("plot_ID", "tree_ID")])) ) {
  # i = 1 
  my.plot.id <- trees_harvested[, "plot_ID"][i]
  my.tree.id <- trees_harvested[, "tree_ID"][i]
  my.dbh.cm <- trees_harvested[, "DBH_cm"][i]
  my.sp <- trees_harvested[, "SP_code"][i]
  my.stand <- trees_harvested[, "stand"][i]
  my.dbh.class <- trees_harvested[, "DBH_class_10"][i]
  my.ld.icode <- trees_harvested[, "state"][i]#  ifelse(stringr::str_length(my.plot.id) == 5, substr(my.plot.id, 1, 1), substr(my.plot.id, 1, 2))
  
  # look for annual diameter growth in cm in the plot, species, stand and DBH class of my.tree
  growth.cm <- growth$annual_growth_cm[growth$plot_ID == my.plot.id &
                                         growth$stand == my.stand & 
                                         growth$DBH_class_10 == my.dbh.class & 
                                         growth$SP_code == my.sp]
  growth.metod <- "growth_df P S DBHC SP"
  
  # if we can´t find growth for the trees species, plot, stand and DBH class
  # look for annual diameter growth in cm in the plot, stand and species of my.tree
  if(length(growth.cm) == 0){
    growth.cm <- growth$annual_growth_cm[growth$plot_ID == my.plot.id &
                                           growth$stand == "all" & 
                                           growth$DBH_class_10 == my.dbh.class & 
                                           growth$SP_code == my.sp]
    growth.metod <- "growth_df P  DBHC SP"
    
    }
  
  # if we cant find growth for the tree species, plot, and DBH class, 
  # we use a model that describes the expectable DBH growth : mean annual growth = DBH_BZE3 grouped by state (bundesland) and species
  # but only if there is a r2 and r2 is => 0.7 
  if(isTRUE(length(growth.cm) == 0 & 
     growth_coeff$pseu_R2[growth_coeff$SP_code == my.sp & growth_coeff$state == my.ld.icode] >= 0.7 &
     !is.na(growth_coeff$pseu_R2[growth_coeff$SP_code == my.sp & growth_coeff$state == my.ld.icode]))== T ){
    
    b0.dbh <- growth_coeff$b0[growth_coeff$SP_code == my.sp &
                                growth_coeff$state == my.ld.icode]
    b1.dbh <- growth_coeff$b1[growth_coeff$SP_code == my.sp &
                               growth_coeff$state == my.ld.icode]
    b2.dbh <- growth_coeff$b2[growth_coeff$SP_code == my.sp &
                               growth_coeff$state == my.ld.icode]
    growth.cm <- b0.dbh * (1 - exp( -b1.dbh * my.dbh.cm))^b2.dbh
    growth.metod <- "growth_model"
  }
  
# if we cant use a model because the conditions are not fullfilled, we set the growth to 0 and just calcualte everything 
# based on the HBI data
  if(length(growth.cm) == 0){
    growth.cm <- 0
    growth.metod <- "no growth"
  }
  

  # add annual diameter growth times 4 to DBH of the tree 
  # (for 4 years, Mittlereumtriebszeit = represent the middle of the period between BZE3 and HBI)
  my.dbh.incl.growth.cm <- my.dbh.cm + 4*growth.cm
  
  # export diameters including 
  dbh_incl_growth.list[[i]] <- as.data.frame(cbind(
    "plot_ID" = c(as.integer(my.plot.id))
    ,"tree_ID" = c(as.integer(my.tree.id))
    ,"DBH_incl_growth" = c(as.numeric(my.dbh.incl.growth.cm))
    , "growth_method" = c(growth.metod)
    ))
  
}
dbh_incl_growth.df <- as.data.frame(rbindlist(dbh_incl_growth.list)) %>% distinct()


# 1.2.2. join DBH including growth to tree dataset ------------------------------------------------------------------------------
# add dbh with growth to HBI_trees dataframe to calcualte biomass 
trees_harvested <- trees_harvested %>% 
  left_join(dbh_incl_growth.df %>% 
              mutate(across(c("plot_ID", "tree_ID", "DBH_incl_growth"), as.numeric)), 
            by = c("plot_ID", "tree_ID")) %>% 
  mutate(BA_m2_incl_growth = c_A((DBH_incl_growth/100)/2))



# 1.4.1. height estimation ----------------------------------------------------------------------------------------------------------
# 1.4.1.1. get average dg and hg over both inventories -------------------------------------------------------------------------------
# calcualte average hg between the both inventories 
Hg_Dg_trees_total.df <- HBI_summary %>% 
  select(-inv) 



# 1.4.1.2. calcualte missing heights --------------------------------------------------------------------------------------------------
# as the tree grows not only in whith but also in height we´ll have to estimate a "new" height according to 
# the "new" diameter. therefore we follow the same procedure as in the LT_heights script
# another option woul dbe to calcualte the height growth similar to the diameter growth and then add it to the original height from HBI

trees_harvested <- trees_harvested %>% 
  mutate(height_inc_growth = NA, 
         DBH_h_m = DBH_h_cm/100) %>% 
  ## joining coefficients per specise and plot in 
  unite(SP_P_ID, plot_ID, SP_code, sep = "", remove = FALSE) %>%            # create column matching vectorised coefficients of coeff_SP_P (1.3. functions, h_nls_SP_P, dplyr::pull)
  left_join(.,coeff_H_SP_P %>% 
              mutate(plot_ID = as.integer(plot_ID)) %>% # joining R2 from coeff_SP_P -> R2.x
              select(plot_ID, SP_code, R2) %>% 
              unite(SP_P_ID, plot_ID, SP_code, sep = "", remove = FALSE),   # create column matching vectorised coefficients of coeff_SP_P (1.3. functions, h_nls_SP_P, dplyr::pull)
            by = c("plot_ID", "SP_code", "SP_P_ID")) %>% 
  ## joinign coefficients per species in 
  left_join(., coeff_H_SP %>% select(SP_code, R2),               # joing R2 from coeff_SP data set -> R2.y
            by = "SP_code") %>% 
  ## joining coefficients and Hg-Dg-data in 
  # this is joins in a tree dataset with mean BHD, d_g, h_g per species per plot per canopy layer which we need for SLOBODA 
  left_join(., Hg_Dg_trees_total.df%>%
              mutate(plot_ID = as.integer(plot_ID)),
            by = c("plot_ID", "stand", "SP_code")) %>% 
  mutate(R2_comb = f(R2.x, R2.y, R2.y, R2.x),                               # if R2 is na, put R2 from coeff_SP_P unless R2 from coeff_SP is higher
         H_new_method = case_when(is.na(height_inc_growth) & !is.na(R2.x) & R2.x > 0.70 | is.na(height_inc_growth) & R2.x > R2.y & R2.x > 0.70 ~ "coeff_SP_P", 
                              is.na(height_inc_growth) & is.na(R2.x) & R2.y > 0.70| is.na(height_inc_growth) & R2.x < R2.y & R2.y > 0.70 ~ "coeff_sp",
                              is.na(height_inc_growth) & is.na(R2_comb) & !is.na(Hg_m)| is.na(height_inc_growth) & R2_comb < 0.70 & !is.na(Hg_m) ~ "ehk_sloboda",
                              is.na(height_inc_growth) & is.na(R2_comb) & is.na(Hg_m)| is.na(height_inc_growth) & R2_comb < 0.70 & is.na(Hg_m) ~ "h_curtis", 
                              TRUE ~ "sampled")) %>% 
  # When h_m is na but there is a plot and species wise model with R2 above 0.7, use the model to predict the height
  mutate(height_inc_growth = as.numeric(case_when(is.na(height_inc_growth) & !is.na(R2.x) & R2.x > 0.70 | is.na(height_inc_growth) & R2.x > R2.y & R2.x > 0.70 ~ h_nls_SP_P(SP_P_ID, DBH_incl_growth),
                                                   # if H_m is na and there is an R2 from coeff_SP_P thats bigger then 0.75 or of theres no R2 from 
                                                   # coeff_SP_plot that´s bigger then R2 of coeff_SP_P while the given R2 from coeff_SP_P is above 
                                                   # 0.75 then use the SP_P models
                                                   is.na(height_inc_growth) & is.na(R2.x) & R2.y > 0.70 | is.na(height_inc_growth) & R2.x < R2.y & R2.y > 0.70 ~ h_nls_SP(SP_code, DBH_incl_growth),
                                                   # when there´s still no model per species or plot, or the R2 of both self-made models is below 0.7 
                                                   # and hm is na but there is a h_g and d_G
                                                   is.na(height_inc_growth) & is.na(R2_comb) & !is.na(Hg_m)| is.na(height_inc_growth) & R2_comb < 0.70 & !is.na(Hg_m) ~ ehk_sloboda(H_SP_group, DBH_incl_growth*10, mean_DBH_cm*10, Dg_cm*10, Hg_m*10),
                                                   # when there´s still no model per species or plot, or the R2 of both self-made models is below 0.7 
                                                   # and hm is na and the Slobody function cannot eb applied because there is no h_g calculatable use the curtis function
                                                   is.na(height_inc_growth) & is.na(R2_comb) & is.na(Hg_m)| is.na(height_inc_growth) & R2_comb < 0.70 & is.na(Hg_m) ~ h_curtis(H_SP_group, DBH_incl_growth*10), 
                                                  TRUE ~ height_inc_growth))) %>% 
  # as there were some trees that had an estimated height which was lower then the DBH measuring height. this is not only implausible but also won´t work for TapeS 
  # thus we correct these heights afterwards by estimating their height from the relation between the dg and hg and dg and the trees DBH (dreisatz, h_proportional function)
  mutate(height_inc_growth = ifelse(DBH_h_m > height_inc_growth, h_proportional(Dg_cm, Hg_m, DBH_incl_growth), height_inc_growth)) 




# 1.4. Biomass --------------------------------------------------------------------------------------------------------------------------
# now we will calcualte the Biomass of the trees with the "new" diameter at the middle of the harvest period
# which means we run all biomass loops from 04_01_LT_stocks again

# 1.4.1. biomass aboveground compartiments --------------------------------------------------------------------------------------------------
bio.ag.kg.list <- vector("list", length = nrow(unique(trees_harvested[, c("plot_ID", "tree_ID")])))
for (i in 1:nrow(unique(trees_harvested[, c("plot_ID", "tree_ID")]))) {
  # i = 381
  # i = trees_harvested %>%  select(plot_ID, tree_ID, LH_NH) %>% distinct() %>% mutate(r_no = row_number()) %>% filter(LH_NH == "LB") %>%slice(1)%>% pull(r_no)
  
  # basic tree info
  # select one tree ID and plot ID for each individual tree per plot through unique(trees_harvested[, c("plot_ID", "tree_ID")])
  my.plot.id <- unique(trees_harvested[, c("plot_ID", "tree_ID")])[,"plot_ID"][i]
  my.tree.id <- unique(trees_harvested[, c("plot_ID", "tree_ID")])[,"tree_ID"][i]
  BL.or.CF <- unique(trees_harvested$LH_NH[trees_harvested$plot_ID==my.plot.id & trees_harvested$tree_ID==my.tree.id])
  
  # select variales for tree object: tapes species, diameter, diameter measuring height, tree height
  spp = na.omit(unique(trees_harvested$tpS_ID[trees_harvested$plot_ID==my.plot.id & trees_harvested$tree_ID==my.tree.id]))
  Dm = na.omit(as.list(as.numeric(unique(trees_harvested$DBH_incl_growth[trees_harvested$plot_ID==my.plot.id & trees_harvested$tree_ID==my.tree.id])))) 
  Hm = na.omit(as.list(as.numeric(unique(trees_harvested$DBH_h_cm[trees_harvested$plot_ID==my.plot.id & trees_harvested$tree_ID==my.tree.id])/100)))
  Ht = na.omit(as.numeric(unique(trees_harvested$height_inc_growth[trees_harvested$plot_ID==my.plot.id & trees_harvested$tree_ID==my.tree.id])))
  # create tapes compartiments
  comp <- as.character(c("stw","stb","sw", "sb", "fwb", "ndl" ))
  
  # create object  
  obj.trees <- tprTrees(spp, Dm, Hm, Ht, inv = 4)
  
  # calculate biomass per compartiment
  bio.df <- as.data.frame(tprBiomass(obj = obj.trees, component = comp)) %>% 
    pivot_longer(cols = stw:ndl,
                 names_to = "compartiment", 
                 values_to = "B_kg_tree")
  
  
  bio.info.df <- as.data.frame(cbind(
    "plot_ID" = c(as.integer(trees_harvested$plot_ID[trees_harvested$plot_ID == my.plot.id & trees_harvested$tree_ID == my.tree.id])), 
    "tree_ID" = c(as.integer(trees_harvested$tree_ID[trees_harvested$plot_ID == my.plot.id & trees_harvested$tree_ID == my.tree.id])), 
    "inv" = c(trees_harvested$inv[trees_harvested$plot_ID == my.plot.id & trees_harvested$tree_ID == my.tree.id]), 
    "inv_year" = c(as.integer(trees_harvested$inv_year[trees_harvested$plot_ID == my.plot.id & trees_harvested$tree_ID == my.tree.id])),
    "LH_NH" = c(trees_harvested$LH_NH[trees_harvested$plot_ID == my.plot.id & trees_harvested$tree_ID == my.tree.id]),
    "compartiment" = c(bio.df$compartiment),
    "B_kg_tree" = c(as.numeric(bio.df$B_kg_tree))
  ) ) %>% 
    # if the tree is a broadleafed tree Tapes cannot calculate the foliage mass, 
    # thus we calculate this subsequently trough the biomass function by Wutzler (2008)
    mutate(B_kg_tree = ifelse(compartiment == "ndl" & LH_NH == "LB", 
                              Wutzler_fB_L1(as.numeric(Dm), as.numeric(Ht)),
                              B_kg_tree)) %>% 
    dplyr::select(-c("LH_NH"))
  
  
  bio.ag.kg.list[[i]] <- bio.info.df
  
  
}
bio_ag_kg_df <- as.data.frame(rbindlist(bio.ag.kg.list))



# 1.4.2. biomass belowground compartiments ---------------------------------------------------------------------------------------
bio.bg.kg.list <- vector("list", length = nrow(unique(trees_harvested[, c("plot_ID", "tree_ID")])))
for (i in 1:nrow(unique(trees_harvested[, c("plot_ID", "tree_ID")]))) {
  # i = 1
  # i = trees_harvested %>%  select(plot_ID, tree_ID, LH_NH) %>% distinct() %>% mutate(r_no = row_number()) %>% filter(LH_NH == "LB") %>%slice(1)%>% pull(r_no)
  
  # basic tree info
  my.plot.id <- unique(trees_harvested[, c("plot_ID", "tree_ID")])[,"plot_ID"][i]
  my.tree.id <- unique(trees_harvested[, c("plot_ID", "tree_ID")])[,"tree_ID"][i]
  #my.inv <-  unique(trees_harvested[, c("plot_ID", "tree_ID")])[,"inv"][i]
  BL.or.CF <- unique(trees_harvested$LH_NH[trees_harvested$plot_ID==my.plot.id & trees_harvested$tree_ID==my.tree.id])
  
  # select variales for tree object
  spp = unique(trees_harvested$Bio_SP_group[trees_harvested$plot_ID==my.plot.id & trees_harvested$tree_ID==my.tree.id])
  dbh.cm = as.numeric(unique(trees_harvested$DBH_incl_growth[trees_harvested$plot_ID==my.plot.id & trees_harvested$tree_ID==my.tree.id]))
  
  
  # calculate biomass per compartiment
  B_kg_tree <- as.data.frame(GHGI_bB(spp, dbh.cm))[,1]
  
  bio.info.df <- as.data.frame(cbind(
    "plot_ID" = c(as.integer(my.plot.id)), 
    "tree_ID" = c(as.integer(my.tree.id)), 
    "inv" = unique(trees_harvested$inv[trees_harvested$plot_ID==my.plot.id & trees_harvested$tree_ID==my.tree.id]), 
    "inv_year" = c(as.integer(unique(trees_harvested$inv_year[trees_harvested$plot_ID==my.plot.id & trees_harvested$tree_ID==my.tree.id]))),
    "compartiment" = c("bg"),
    "B_kg_tree" = c(as.numeric(B_kg_tree))
  ) ) 
  
  bio.bg.kg.list[[i]] <- bio.info.df
  
}
bio_bg_kg_df <- as.data.frame(rbindlist(bio.bg.kg.list))


# 1.4.3. biomass all compartiments - total ------------------------------------------------------------------------
bio_total_kg_df <- 
  rbind(
    # calculate total biomass (aboveground + belowground) by summing up biomass in kg per tree in all compartiments
    rbind(
      bio_ag_kg_df, bio_bg_kg_df) %>% 
      group_by(plot_ID, tree_ID, inv, inv_year) %>% 
      summarize(B_kg_tree = sum(as.numeric(B_kg_tree))) %>% 
      mutate(compartiment = "total") %>% 
      select("plot_ID", "tree_ID", "inv", 
             "inv_year", "compartiment", "B_kg_tree"),
    # calculate total aboveground biomass by summing up biomass in kg per tree in all aboveground compartiments
    bio_ag_kg_df%>% 
      group_by(plot_ID, tree_ID, inv, inv_year) %>% 
      summarize(B_kg_tree = sum(as.numeric(B_kg_tree))) %>% 
      mutate(compartiment = "ag")%>% 
      select("plot_ID", "tree_ID", "inv", 
             "inv_year", "compartiment", "B_kg_tree"))

# 1.4.4. harmonizing biomass strings and compartiment names ----------------------------------------------------------
#  harmonize strings of bio_total_kg_df  
# https://stackoverflow.com/questions/20637360/convert-all-data-frame-character-columns-to-factors
bio_total_kg_df[,c(1,2, 4, 6)] <- lapply(bio_total_kg_df[,c(1,2,4, 6)], as.numeric)
bio_ag_kg_df[,c(1,2, 4, 6)] <- lapply(bio_ag_kg_df[,c(1,2,4, 6)], as.numeric)
bio_bg_kg_df[,c(1,2, 4, 6)] <- lapply(bio_bg_kg_df[,c(1,2,4, 6)], as.numeric)

# 1.4.5. join biomass into tree dataset ----------------------------------------------------------------------------------
trees_harvested <- trees_harvested %>% distinct() %>% 
  left_join(., 
            rbind(bio_ag_kg_df , 
                  bio_bg_kg_df, 
                  bio_total_kg_df) %>% 
              distinct(), 
            by = c("plot_ID", "tree_ID", "inv", "inv_year"), 
            multiple = "all") 




# 1.5. Nitrogen calculation --------------------------------------------------------------------------------------
# 1.5.1. Nitrogen stock in abofeground and belowgroung compartiments-----------------------------------------------
N_ag_bg_kg_df <- trees_harvested %>%
  filter(!(compartiment %in% c("ag", "total")))  %>%  # make sure the aboveground& belowground dataset doesnt include summed up compartiments like total and aboveground
  mutate(N_kg_tree = N_all_com(B_kg_tree, N_SP_group, N_f_SP_group_MoMoK, N_bg_SP_group, compartiment)) %>% 
  select(plot_ID, tree_ID, inv, inv_year, compartiment, N_kg_tree) 

# 1.5.2. Nitrogen ston in all compartiments summed up - total & aboveground  ----------------------------------
N_total_kg_df <- 
  rbind(
    # calculate total biomass (aboveground + belowground) by summing up biomass in kg per tree in all compartiments
    N_ag_bg_kg_df %>% 
      group_by(plot_ID, tree_ID, inv, inv_year) %>% 
      summarize(N_kg_tree = sum(as.numeric(N_kg_tree))) %>% 
      mutate(compartiment = "total") %>% 
      select("plot_ID", "tree_ID", "inv", 
             "inv_year", "compartiment", "N_kg_tree"),
    # calculate total aboveground biomass by summing up biomass in kg per tree in all aboveground compartiments
    N_ag_bg_kg_df%>% 
      filter(compartiment != "bg") %>%  # select only aboveground compartiments by exxlduing bg compartiment from N.ab.bg. dataframe 
      group_by(plot_ID, tree_ID, inv, inv_year) %>% 
      summarize(N_kg_tree = sum(as.numeric(N_kg_tree))) %>% 
      mutate(compartiment = "ag")%>% 
      select("plot_ID", "tree_ID", "inv", 
             "inv_year", "compartiment", "N_kg_tree"))


# 1.5.3. join Nitrogen stocks into tree dataset -----------------------------------
trees_harvested <- trees_harvested %>% left_join(., 
                                       rbind(N_ag_bg_kg_df , 
                                             N_total_kg_df), 
                                       by = c("plot_ID", "tree_ID", "inv", "inv_year", "compartiment"), 
                                       multiple = "all")


# 1.6. carbon stock per tree & compartiment -------------------------------------------------------
trees_harvested <- trees_harvested %>% mutate(C_kg_tree = carbon(B_kg_tree))




# 1.7. summarize harvested trees per ha  ----------------------------------
# 1.2. number of speices per plot -----------------------------------------
LT_n_SP_plot <- trees_harvested %>%
  filter(compartiment == "ag") %>%
  select(plot_ID, inv, SP_code) %>% 
  group_by(plot_ID, inv) %>% 
  distinct() %>% 
  summarise(n_SP = n()) %>% 
  mutate(stand_component = "LT")


# 1.3. number of stand per plot -------------------------------------------
LT_n_stand_P <- trees_harvested %>% 
  filter(compartiment == "ag") %>%
  select(plot_ID, inv, stand) %>% 
  group_by(plot_ID, inv) %>% 
  distinct() %>% 
  summarise(n_stand = n()) %>% 
  mutate(stand_component = "LT")


# 1.7.1. summary per plot -------------------------------------------------
# 1.7.1.1. plot, species, stand: stocks per ha, finest summary --------------

if(exists('HBI_trees_stat_2') == TRUE && nrow(HBI_trees_stat_2)!= 0){
  LT_SP_ST_IST_P_BCNBAn_ha <- plyr::rbind.fill(
    trees_harvested  %>% 
      group_by(plot_ID, plot_A_ha, CCS_r_m, inv, stand, SP_code, tree_inventory_status, compartiment) %>% 
      # convert Biomass into tons per hectar and sum it up per sampling circuit 
       reframe(B_CCS_t_ha = sum(ton(B_kg_tree))/plot_A_ha, # plot are is the area of the respecitve samplign circuit in ha 
               C_CCS_t_ha = sum(ton(C_kg_tree))/plot_A_ha,
               N_CCS_t_ha = sum(ton(N_kg_tree))/plot_A_ha, 
               BA_CCS_m2_ha = sum(BA_m2_incl_growth)/plot_A_ha, 
               n_trees_CCS_ha = n()/plot_A_ha) %>% 
       distinct(), 
    trees_harvested  %>% 
      group_by(plot_ID, plot_A_ha, CCS_r_m, inv, stand, SP_code, compartiment) %>% 
      # convert Biomass into tons per hectar and sum it up per sampling circuit 
      reframe(B_CCS_t_ha = sum(ton(B_kg_tree))/plot_A_ha, # plot are is the area of the respecitve samplign circuit in ha 
              C_CCS_t_ha = sum(ton(C_kg_tree))/plot_A_ha,
              N_CCS_t_ha = sum(ton(N_kg_tree))/plot_A_ha, 
              BA_CCS_m2_ha = sum(BA_m2_incl_growth)/plot_A_ha, 
              n_trees_CCS_ha = n()/plot_A_ha) %>% 
      distinct() %>% 
      mutate(tree_inventory_status = "all"), 
     HBI_trees_stat_2 %>% 
       # this is in case in 01_00_RG_LT_DW_plot_inv_status_sorting there were stat_2 datasets produced that do not hold any data but only NAs
       filter(!is.na(plot_ID))) %>% 
    # now we summarise all the t/ha values of the cirlces per plot
    group_by(plot_ID, inv, stand, SP_code, tree_inventory_status, compartiment) %>% 
    summarise(B_t_ha = sum(B_CCS_t_ha), 
              C_t_ha = sum(C_CCS_t_ha), 
              N_t_ha = sum(N_CCS_t_ha), 
              BA_m2_ha = sum(BA_CCS_m2_ha), 
              n_ha = sum(n_trees_CCS_ha)) %>% 
    mutate(stand_component = "LT") %>% 
 ## join in datasets to calcualte species compostiion by calcualting the percent of the respective species contributes to the overall basal area 
    left_join(., 
              # we have to join in the total BA per ha over all species. once per plot and once per stand
          # per plot (stand == "all)
              plyr::rbind.fill(trees_harvested  %>% 
                                 filter(compartiment == "ag") %>% 
                                 group_by(plot_ID, plot_A_ha, CCS_r_m, inv) %>% 
                                 # convert Biomass into tons per hectar and sum it up per sampling circuit 
                                 reframe(BA_CCS_m2_ha = sum(BA_m2_incl_growth)/plot_A_ha) %>%
                                 distinct() %>% 
                                 group_by(plot_ID, inv) %>% 
                                 summarise(BA_m2_ha_total = sum(BA_CCS_m2_ha))%>% 
                                 mutate(stand = "all", 
                                        tree_inventory_status = "all") %>% 
                                 distinct(), 
            # per plot per stand
                               trees_harvested  %>% 
                                 filter(compartiment == "ag") %>% 
                                 group_by(plot_ID, plot_A_ha, stand, CCS_r_m, inv) %>% 
                                 # convert Biomass into tons per hectar and sum it up per sampling circuit 
                                 reframe(BA_CCS_m2_ha = sum(BA_m2_incl_growth)/plot_A_ha) %>%
                                 distinct() %>% 
                                 group_by(plot_ID, inv, stand) %>% 
                                 summarise(BA_m2_ha_total = sum(BA_CCS_m2_ha))%>% 
                                 distinct() %>% 
                                 mutate(tree_inventory_status = "all"), 
             # per plot per stand per inv status
                               trees_harvested  %>% 
                                 filter(compartiment == "ag") %>% 
                                 group_by(plot_ID, plot_A_ha, stand, CCS_r_m, inv, tree_inventory_status) %>% 
                                 # convert Biomass into tons per hectar and sum it up per sampling circuit 
                                 reframe(BA_CCS_m2_ha = sum(BA_m2_incl_growth)/plot_A_ha) %>%
                                 distinct() %>% 
                                 group_by(plot_ID, inv, stand, tree_inventory_status) %>% 
                                 summarise(BA_m2_ha_total = sum(BA_CCS_m2_ha))%>% 
                                 distinct()
              ) , # close bind  
              by = c("plot_ID", "inv", "stand", "tree_inventory_status"))  %>% 
    mutate(BA_percent = (BA_m2_ha/BA_m2_ha_total)*100) %>% 
    select(-"BA_m2_ha_total")
}else{
  LT_SP_ST_IST_P_BCNBAn_ha <- plyr::rbind.fill(
    trees_harvested  %>% 
      group_by(plot_ID, plot_A_ha, CCS_r_m, inv, stand, SP_code, tree_inventory_status, compartiment) %>% 
      # convert Biomass into tons per hectar and sum it up per sampling circuit 
      reframe(B_CCS_t_ha = sum(ton(B_kg_tree))/plot_A_ha, # plot are is the area of the respecitve samplign circuit in ha 
              C_CCS_t_ha = sum(ton(C_kg_tree))/plot_A_ha,
              N_CCS_t_ha = sum(ton(N_kg_tree))/plot_A_ha, 
              BA_CCS_m2_ha = sum(BA_m2_incl_growth)/plot_A_ha, 
              n_trees_CCS_ha = n()/plot_A_ha) %>% 
      distinct(), 
    trees_harvested  %>% 
      group_by(plot_ID, plot_A_ha, CCS_r_m, inv, stand, SP_code, compartiment) %>% 
      # convert Biomass into tons per hectar and sum it up per sampling circuit 
      reframe(B_CCS_t_ha = sum(ton(B_kg_tree))/plot_A_ha, # plot are is the area of the respecitve samplign circuit in ha 
              C_CCS_t_ha = sum(ton(C_kg_tree))/plot_A_ha,
              N_CCS_t_ha = sum(ton(N_kg_tree))/plot_A_ha, 
              BA_CCS_m2_ha = sum(BA_m2_incl_growth)/plot_A_ha, 
              n_trees_CCS_ha = n()/plot_A_ha) %>% 
      distinct() %>% 
      mutate(tree_inventory_status = "all")) %>%  
   # now we summarise all the t/ha values of the cirlces per plot
    group_by(plot_ID, inv, stand, SP_code, tree_inventory_status, compartiment) %>% 
    summarise(B_t_ha = sum(B_CCS_t_ha), 
              C_t_ha = sum(C_CCS_t_ha), 
              N_t_ha = sum(N_CCS_t_ha), 
              BA_m2_ha = sum(BA_CCS_m2_ha), 
              n_ha = sum(n_trees_CCS_ha)) %>% 
    mutate(stand_component = "LT") %>% 
 ## join in datasets to calcualte species compostiion by calcualting the percent of the respective species contributes to the overall basal area 
    left_join(., 
              # we have to join in the total BA per ha over all species. once per plot and once per stand
              # per plot (stand == "all)
              plyr::rbind.fill(trees_harvested  %>% 
                filter(compartiment == "ag") %>% 
                group_by(plot_ID, plot_A_ha, CCS_r_m, inv) %>% 
                # convert Biomass into tons per hectar and sum it up per sampling circuit 
                reframe(BA_CCS_m2_ha = sum(BA_m2_incl_growth)/plot_A_ha) %>%
                distinct() %>% 
                group_by(plot_ID, inv) %>% 
                summarise(BA_m2_ha_total = sum(BA_CCS_m2_ha))%>% 
                mutate(stand = "all", 
                       tree_inventory_status = "all") %>% 
                distinct(), 
              # per plot per stand
              trees_harvested  %>% 
                filter(compartiment == "ag") %>% 
                group_by(plot_ID, plot_A_ha, stand, CCS_r_m, inv) %>% 
                # convert Biomass into tons per hectar and sum it up per sampling circuit 
                reframe(BA_CCS_m2_ha = sum(BA_m2_incl_growth)/plot_A_ha) %>%
                distinct() %>% 
                group_by(plot_ID, inv, stand) %>% 
                summarise(BA_m2_ha_total = sum(BA_CCS_m2_ha))%>% 
               distinct() %>% 
                mutate(tree_inventory_status = "all"), 
              # per plot per stand per inv status
              trees_harvested  %>% 
                filter(compartiment == "ag") %>% 
                group_by(plot_ID, plot_A_ha, stand, CCS_r_m, inv, tree_inventory_status) %>% 
                # convert Biomass into tons per hectar and sum it up per sampling circuit 
                reframe(BA_CCS_m2_ha = sum(BA_m2_incl_growth)/plot_A_ha) %>%
                distinct() %>% 
                group_by(plot_ID, inv, stand, tree_inventory_status) %>% 
                summarise(BA_m2_ha_total = sum(BA_CCS_m2_ha))%>% 
                distinct()
              ) , # close bind  
              by = c("plot_ID", "inv", "stand", "tree_inventory_status"))  %>% 
    mutate(BA_percent = (BA_m2_ha/BA_m2_ha_total)*100) %>% 
    select(-"BA_m2_ha_total")
}

# 1.7.2. Plot, stand: stocks per hektar ------------------------------------------------------
LT_ST_BCNBAn_ha <- summarize_data(LT_SP_ST_IST_P_BCNBAn_ha, 
                                  c("plot_ID", "inv", "stand", "tree_inventory_status", "compartiment"), 
                                  c("B_t_ha", "C_t_ha", "N_t_ha", "BA_m2_ha", "n_ha"), 
                                  operation = "sum_df") %>% 
  mutate(stand_component = "LT", 
         SP_code = "all") 


# 1.7.3. Plot, species: stocks per hektar ------------------------------------------------------
LT_SP_BCNBA_ha <-summarize_data(LT_SP_ST_IST_P_BCNBAn_ha, 
                                c("plot_ID", "inv", "SP_code", "tree_inventory_status", "compartiment"), 
                                c("B_t_ha", "C_t_ha", "N_t_ha", "BA_m2_ha", "n_ha"), 
                                operation = "sum_df") %>% 
  mutate(stand_component = "LT", 
         stand = "all") %>% 
  #calcualte species compostiion by calcualting the percent of the respective species contributes to the overall basal area 
  left_join(., trees_harvested  %>% 
              filter(compartiment == "ag") %>% 
              group_by(plot_ID, plot_A_ha, CCS_r_m, inv) %>% 
              # convert Biomass into tons per hectar and sum it up per sampling circuit 
              reframe(BA_CCS_m2_ha = sum(BA_m2_incl_growth)/plot_A_ha) %>% 
              distinct() %>% 
              group_by(plot_ID, inv) %>% 
              summarise(BA_m2_ha_total = sum(BA_CCS_m2_ha))%>% 
              distinct(), 
            by = c("plot_ID", "inv"))  %>% 
  mutate(BA_percent = (BA_m2_ha/BA_m2_ha_total)*100) %>% 
  select(-"BA_m2_ha_total")

# 1.7.3. Plot: stocks per hektar ------------------------------------------------------
LT_BCNBAn_ha <- summarize_data(LT_SP_ST_IST_P_BCNBAn_ha, 
                  c("plot_ID", "inv", "tree_inventory_status", "compartiment"), 
                  c("B_t_ha", "C_t_ha", "N_t_ha", "BA_m2_ha", "n_ha"), 
                  operation = "sum_df") %>% 
  mutate(stand_component = "LT", 
         SP_code = "all", 
         stand = "all") 

# 1.7.4. average values per plot ------------------------------------------

# 1.6. average values ----------------------------------------------------
# 1.6.1. create "pseudo stands" -------------------------------------------
LT_avg_SP_ST_P_list <- vector("list", length = length(unique(trees_harvested$plot_ID))) 
LT_avg_SP_P_list <- vector("list", length = length(unique(trees_harvested$plot_ID))) 
LT_avg_P_list <- vector("list", length = length(unique(trees_harvested$plot_ID))) 
for (i in 1:length(unique(trees_harvested$plot_ID))) {
  # i = 1
  my.plot.id <- unique(trees_harvested$plot_ID)[i]
  # select all trees by only one compartiment of each tree to make sure the tree enters the dataframe only once
  my.tree.df <- trees_harvested[trees_harvested$plot_ID == my.plot.id & trees_harvested$compartiment == "ag", ] 
  my.n.ha.df <- trees_harvested %>% filter(compartiment == "ag" & plot_ID == my.plot.id) %>% group_by(plot_ID, CCS_r_m) %>% reframe(n_ha_CCS = n()/plot_A_ha) %>% distinct()
  my.n.plot.df <- trees_harvested %>% filter(compartiment == "ag" & plot_ID == my.plot.id) %>% group_by(plot_ID, CCS_r_m) %>% reframe(n_CCS = n()) %>% distinct()
  
  my.n.ha.df$n.rep.each.tree <- round(my.n.ha.df$n_ha_CCS/my.n.plot.df$n_CCS)
  
  # repeat every tree per circle by the number this tree would be repeated by to reach it´s ha number
  # so every tree id repeated as often as it would be represented on a hectar)
  # https://stackoverflow.com/questions/11121385/repeat-rows-of-a-data-frame
  my.tree.rep.df <- rbind(
    # 5m circle
    my.tree.df[my.tree.df$CCS_r_m == 5.64, ][rep(seq_len(nrow(my.tree.df[my.tree.df$CCS_r_m == 5.64, ])), 
                                                 each = my.n.ha.df$n.rep.each.tree[my.n.ha.df$CCS_r_m == 5.64]), ],
    # 12m circle
    my.tree.df[my.tree.df$CCS_r_m == 12.62, ][rep(seq_len(nrow(my.tree.df[my.tree.df$CCS_r_m == 12.62, ])), 
                                                  each = my.n.ha.df$n.rep.each.tree[my.n.ha.df$CCS_r_m == 12.62] ), ],
    # 17m circle
    my.tree.df[my.tree.df$CCS_r_m == 17.84, ][rep(seq_len(nrow(my.tree.df[my.tree.df$CCS_r_m == 17.84, ])), 
                                                  each = my.n.ha.df$n.rep.each.tree[my.n.ha.df$CCS_r_m == 17.84]), ])
  
  
  LT_avg_SP_ST_P_list[[i]] <- rbind(my.tree.rep.df,
                                    # now we bind in the same dataset but with the change the trees inventory status to "all" 
                                    # that way we achive a summary of inv_status 2, 7 and both together 
                                    my.tree.rep.df %>% mutate(tree_inventory_status  = "all")) %>% 
    group_by(plot_ID, inv,stand, SP_code, tree_inventory_status) %>% 
    summarise(mean_DBH_cm = mean(DBH_incl_growth), 
              sd_DBH_cm = sd(DBH_incl_growth),
              Dg_cm = ((sqrt(mean(BA_m2_incl_growth)/pi))*2)*100,  
              mean_BA_m2 = mean(BA_m2_incl_growth),
              mean_H_m = mean(height_inc_growth ), 
              sd_H_m = sd(height_inc_growth ), 
              Hg_m = sum(mean(na.omit(mean_H_m))*sum(BA_m2_incl_growth))/sum(sum(BA_m2_incl_growth))) %>% 
    mutate(stand_component = "LT")
  
  LT_avg_SP_P_list[[i]] <- rbind(my.tree.rep.df,
                                 # now we bind in the same dataset but with the change the trees inventory status to "all" 
                                 # that way we achive a summary of inv_status 2, 7 and both together 
                                 my.tree.rep.df %>% mutate(tree_inventory_status  = "all")) %>%
    group_by(plot_ID, inv, SP_code, tree_inventory_status) %>% 
    summarise(mean_DBH_cm = mean(DBH_incl_growth), 
              sd_DBH_cm = sd(DBH_incl_growth),
              Dg_cm = ((sqrt(mean(BA_m2_incl_growth)/pi))*2)*100,  
              mean_BA_m2 = mean(BA_m2_incl_growth),
              mean_H_m = mean(height_inc_growth ), 
              sd_H_m = sd(height_inc_growth ), 
              Hg_m = sum(mean(na.omit(mean_H_m))*sum(BA_m2_incl_growth))/sum(sum(BA_m2_incl_growth))) %>% 
    mutate(stand_component = "LT", 
           stand = "all")
  
  LT_avg_P_list[[i]] <-rbind(my.tree.rep.df,
                             # now we bind in the same dataset but with the change the trees inventory status to "all" 
                             # that way we achive a summary of inv_status 2, 7 and both together 
                             my.tree.rep.df %>% mutate(tree_inventory_status  = "all")) %>%
    group_by(plot_ID, inv, tree_inventory_status) %>% 
    summarise(mean_DBH_cm = mean(DBH_incl_growth), 
              sd_DBH_cm = sd(DBH_incl_growth),
              Dg_cm = ((sqrt(mean(BA_m2_incl_growth)/pi))*2)*100,  
              mean_BA_m2 = mean(BA_m2_incl_growth),
              mean_H_m = mean(height_inc_growth ), 
              sd_H_m = sd(height_inc_growth ), 
              Hg_m = sum(mean(na.omit(mean_H_m))*sum(BA_m2_incl_growth))/sum(sum(BA_m2_incl_growth))) %>% 
    mutate(stand_component = "LT", 
           SP_code = "all",
           stand = "all")
  
}
LT_avg_SP_ST_P <- as.data.frame(rbindlist(LT_avg_SP_ST_P_list))
LT_avg_SP_P <- as.data.frame(rbindlist(LT_avg_SP_P_list))
LT_avg_P <- as.data.frame(rbindlist(LT_avg_P_list))

# 1.7. binding LT data together -------------------------------------------------------------------------------------------------------

# 1.7.1. LT Species data -------------------------------------------------------------------------------------------------------------
LT_SP_ST_P <- LT_SP_ST_IST_P_BCNBAn_ha  %>% 
  left_join(LT_avg_SP_ST_P,  
            by = c("plot_ID", "inv", "stand_component", "SP_code", "stand", "tree_inventory_status")) %>% 
  select(-(n_ha))


# 1.7.2. LT Species data -------------------------------------------------------------------------------------------------------------
LT_SP_P <- LT_SP_BCNBA_ha  %>%  
  left_join(., LT_avg_SP_P, 
            by = c("plot_ID", "inv", "stand_component", "SP_code", "stand", "tree_inventory_status")) 


# 1.7.3. LT stand data ----------------------------------------------------
LT_ST_P <- LT_ST_BCNBAn_ha  


# 1.7.4. LT plot data ----------------------------------------------------------------------------------------------------------------
LT_P <- LT_BCNBAn_ha %>% 
  left_join(., LT_avg_P, 
            by = c("plot_ID", "inv", "stand_component", "SP_code", "stand","tree_inventory_status")) %>% 
  left_join(., LT_n_SP_plot, 
            by = c("plot_ID", "inv", "stand_component" ))

# 1.7.6. rbinding LT data together ----------------------------------------
LT_summary <- plyr::rbind.fill(LT_SP_ST_P, 
                               LT_SP_P,
                               LT_ST_P,
                               LT_P) %>% 
  arrange(plot_ID, stand, SP_code, tree_inventory_status, compartiment)










# data export ---------------------------------------------------------------------------------------------
write.csv(trees_harvested, paste0(out.path.BZE3, paste(unique(trees_harvested$inv)[1], unique(BZE3_trees_removed$inv)[1], "LT_stock_removed", sep = "_"), ".csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(LT_summary, paste0(out.path.BZE3, paste(unique(trees_harvested$inv)[1], unique(BZE3_trees_removed$inv)[1], "LT_stock_ha_removed", sep = "_"), ".csv"), row.names = FALSE, fileEncoding = "UTF-8")


stop("this is where 08_01 script for harvested tree stocks hbi bze3  ends")



# NOTES AND TESTS ---------------------------------------------------------
# NOTES -------------------------------------------------------------------------------
# N.1.3.1. old loop to assinging DBH growth --------------------------------------------------------------------------------------
# import growth dataset
growth <- read.delim(file = here(paste0(out.path.BZE3, "HBI_BZE3_LT_RG_DW_changes_all_groups.csv")), sep = ",", dec = ".") %>% 
  filter(stand_component == "LT") %>% 
  select(stand_component, plot_ID, stand, C_layer, SP_code, age_period ,annual_growth_cm) %>% distinct()

# prepare dataset for DBH at middle of harvesting period 
dbh_incl_growth.list <- vector("list", length = length(unique(trees_harvested$tree_ID)))
for (i in 1:length(unique(trees_harvested$tree_ID)) ) {
  # i = 1 
  my.inv <- trees_harvested[, "inv"][i]
  my.plot.id <- trees_harvested[, "plot_ID"][i]
  my.tree.id <- trees_harvested[, "tree_ID"][i]
  my.dbh.cm <- trees_harvested[, "DBH_cm"][i]
  my.sp <- trees_harvested[, "SP_code"][i]
  my.c.layer <- trees_harvested[, "C_layer"][i]
  my.stand <- trees_harvested[, "stand"][i]
  
  # look for annual diameter growth in cm in the plot, species, stand and canopy layer of my.tree
  growth.cm <- growth$annual_growth_cm[growth$plot_ID == my.plot.id &
                                         growth$stand == my.stand & 
                                         growth$C_layer == my.c.layer & # change to dbh class of 10 cm
                                         growth$SP_code == my.sp]
  
  # if we can´t find growth for the trees species, plot, stand and canopy layer
  # look for annual diameter growth in cm in the plot, stand and species of my.tree
  if(length(growth.cm) == 0){
    growth.cm <- growth$annual_growth_cm[growth$plot_ID == my.plot.id &
                                           growth$stand == my.stand & # set stand == "all"
                                           growth$C_layer == "all" &   # don´t use C_layer but dbh class
                                           growth$SP_code == my.sp]}
  
  # we have to adjust this part instead of just taking the mean annual growth of the species over all plots, 
  # we use a model: 
  # mean annual growth = DBH  BZE3 grouped by state (bundesland) and species
  
  # if we can´t find growth for the trees species, plot, stand 
  # look for annual diameter growth in cm in the plot and species of my.tree
  if(length(growth.cm) == 0){
    growth.cm <- growth$annual_growth_cm[growth$plot_ID == my.plot.id &
                                           growth$stand == "all" & 
                                           growth$C_layer == "all" & 
                                           growth$SP_code == my.sp]}
  
  
  
  # if we can´t find growth for the trees species, plot 
  # look for annual diameter growth in cm in the species group of my.tree
  if(length(growth.cm) == 0){
    growth.cm <- growth$annual_growth_cm[growth$plot_ID == "all" &
                                           growth$stand == "all" & 
                                           growth$C_layer == "all" & 
                                           growth$SP_code == my.sp] }
  
  # add annual diameter growth times 4 to DBH of the tree 
  # (for 4 years, Mittlereumtriebszeit = represent the middle of the period between BZE3 and HBI)
  my.dbh.incl.growth.cm <- my.dbh.cm + 4*growth.cm
  
  # export diameters including 
  dbh_incl_growth.list[[i]] <- as.data.frame(cbind(
    "inv" = c(my.inv)
    ,"plot_ID" = c(as.integer(my.plot.id))
    ,"tree_ID" = c(as.integer(my.tree.id))
    ,"DBH_incl_growth" = c(as.numeric(my.dbh.incl.growth.cm))
  ))
  
}
dbh_incl_growth.df <- as.data.frame(rbindlist(dbh_incl_growth.list))




# TEST --------------------------------------------------------------------------------------

# test for dg hg differences
Hg_Dg_trees_total.mean <- rbind(HBI_summary, 
                              BZE3_summary) %>% 
  group_by(plot_ID, stand, SP_code) %>% 
  summarise(Hg_m = mean(Hg_m), 
            Dg_cm = mean(Dg_cm), 
            mean_DBH_cm = mean(mean_DBH_cm)) %>% 
  mutate(plot_ID = as.double(plot_ID))

sloboda_H<- plyr::rbind.fill( 
trees_harvested %>% 
  left_join(Hg_Dg_trees_total.mean, by = c("plot_ID", "stand", "SP_code")) %>% 
  mutate(H_m_soloboda = ehk_sloboda(H_SP_group, DBH_incl_growth*10, mean_DBH_cm*10,Dg_cm*10, Hg_m*10 ), 
         H_meth = "means"), 
  trees_harvested %>% 
  left_join(HBI_summary %>% mutate(plot_ID = as.double(plot_ID)), by = c("plot_ID", "stand", "SP_code")) %>% 
  mutate(H_m_soloboda = ehk_sloboda(H_SP_group, DBH_incl_growth*10, mean_DBH_cm*10,Dg_cm*10, Hg_m*10), 
         H_meth = "HBI"), 
  trees_harvested %>% 
  left_join(BZE3_summary %>% mutate(plot_ID = as.double(plot_ID)), by = c("plot_ID", "stand", "SP_code")) %>% 
  mutate(H_m_soloboda = ehk_sloboda(H_SP_group, DBH_incl_growth*10, mean_DBH_cm*10,Dg_cm*10, Hg_m*10 ),
         H_meth = "BZE3"))

## compare mean of sloboda height of spruce by method
# method: mean HG and Dg
summary(sloboda_H %>% 
          filter(SP_code == "gfi" & 
                   H_meth == "means"))
# 22.122                                                           

# method: HBi Hg, Dg
summary(sloboda_H %>% 
          filter(SP_code == "gfi"& 
                   H_meth == "HBI"))
# 21.913                                                           

# method BZE3 Hg Dg
summary(sloboda_H %>% 
          filter(SP_code == "gfi"& 
                   H_meth == "BZE3"))
# 21.10                                                           

# ANOVA 
sloboda_H %>% 
  filter(SP_code == "gfi") %>% 
  aov(H_m_soloboda ~ H_meth, data = .) %>% 
  tidy()

# anova result: the method doesn´t matter for H differences
  # term         df  sumsq  meansq  statistic p.value
  # <chr>     <dbl>  <dbl>  <dbl>     <dbl>   <dbl>
  # H_meth        2   175.   87.5      2.23   0.108
  # Residuals  1076 42105.   39.1     NA      NA 


## compare mean of sloboda height of pine by method
# method: mean HG and Dg
summary(sloboda_H %>% 
          filter(SP_code == "gki" & 
                   H_meth == "means"))
# 18.50                                                             

# method: HBi Hg, Dg
summary(sloboda_H %>% 
          filter(SP_code == "gki"& 
                   H_meth == "HBI"))
# 17.74                                                             

# method BZE3 Hg Dg
summary(sloboda_H %>% 
          filter(SP_code == "gki"& 
                   H_meth == "BZE3"))
# 18.72                                                             

# ANOVA 
sloboda_H %>% 
  filter(SP_code == "gki") %>% 
  aov(H_m_soloboda ~ H_meth, data = .) %>% 
  tidy()

# anova result: the method doesn´t matter for H differences
 #  term         df  sumsq meansq statistic p.value
 # <chr>     <dbl>  <dbl>  <dbl>     <dbl>   <dbl>
 #  H_meth        2   59.0   29.5     2.51  0.0831
 #  Residuals   327 3849.    11.8     NA     NA     

view(sloboda_H %>% 
       filter(SP_code %in% c("gki", "gfi")) %>% 
  group_by(SP_code, H_meth) %>% 
  summarise(H_sloboda_mean = mean(na.omit(H_m_soloboda))))


# test for growth strata --------------------------------------------------
# this test aims to identify differences in the dbg growth of individual trees of the species 
# pine and spruce in sachsony over all plots and then among the sampling circles
# what we want to find out is if we can just use the growth of one species over all plots, no matter the social status
# of the tree or if we have to keep the data summarized in height / dbh classes

# here is the question what matters more, the region of the tree or the social class 
# we have to find out wich statistaical test to use... ANOVA? or GLM?
# GLM could tell me which variable has most impact on modeling dbh growth out of those that i select
# ANOVA would be quick and easy and probably more what we need: i just want to compare two groups actually, right? 
# actually i want to compare the same data grouped differently
# which is why i also considered to just calcualte the variance of dbh growth
    # among one sampling circle over all plots
     # over all plots no matter the cirlce
   # among the circle per plot (?)
# and then just do a t. test or something to comparte if they are significantly different. 

# variance within one species at every plot
# variance within one species over all plots in every sampling circuit 


# create test dataset for diameter model
# test start
dbh_growth_tree <- rbind(dbh_growth_tree,
                         dbh_growth_tree %>% mutate(annual_growth_cm = annual_growth_cm+0.1, 
                                                    BZE3_DBH_cm = BZE3_DBH_cm+ 1), 
                         dbh_growth_tree%>% mutate(annual_growth_cm = annual_growth_cm-0.1, 
                                                   BZE3_DBH_cm = BZE3_DBH_cm- 1))
# test start
dbh_growth_tree <- rbind(dbh_growth_tree,
                         dbh_growth_tree %>% mutate(annual_growth_cm = annual_growth_cm+0.1, 
                                                    BZE3_DBH_cm = BZE3_DBH_cm+ 1), 
                         dbh_growth_tree%>% mutate(annual_growth_cm = annual_growth_cm-0.1, 
                                                   BZE3_DBH_cm = BZE3_DBH_cm- 1))
# test end

# test end

# create test dataset 
# test begin
# as we dont have trees with the tree inv status 2 we will treat 7 trees as status 2 
trees_harvested <- HBI_trees %>% 
  semi_join(BZE3_trees_removed, by = c("plot_ID", "tree_ID")) %>% slice(2, 3)
# test end

# height calc test --------------------------------------------------------
# test beginn

# because I cannot combine 3 variabels in one vector, 
b0 <- coeff_H_SP_P %>% unite(SP_P_ID, plot_ID, SP_code, sep = "", remove = FALSE) %>% dplyr::pull(b0, SP_P_ID);
b1 <- coeff_H_SP_P %>% unite(SP_P_ID, plot_ID, SP_code, sep = "", remove = FALSE) %>% dplyr::pull(b1, SP_P_ID);
b2 <- coeff_H_SP_P %>% unite(SP_P_ID, plot_ID, SP_code, sep = "", remove = FALSE) %>% dplyr::pull(b2, SP_P_ID);

# our nls 
31.72121 *(1 - exp( -0.0609014  * 40.73636))^1.472459 
# height tree 140439, 2, hbi: 28.79518
# height tree 140439, 3, hbi: 27.89165

# pettersen function tapes
estHeight(45.23636, 1) 
# height tree 140439, 2, hbi: 29.91592
# height tree 140439, 3, hbi: 28.3137

# test end

# test wutzler when 140039, 2, would be BL
0.0377*45.2^2.43*28.8^-0.913 #  = 18.4849147214053
# test 


# test start 
# belowground biomass 140039, 2, hbi 
#   b0[spec]*dbh^b1[spec])
# 0.003720*45.2^2.792465 # 155.7611
# test end

