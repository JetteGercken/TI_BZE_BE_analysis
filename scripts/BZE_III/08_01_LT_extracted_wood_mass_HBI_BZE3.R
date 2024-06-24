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
BZE3_trees_removed <- read.delim(file = here(paste0(out.path.BZE3, "BZE3_LT_removed_2.csv")), sep = ",", dec = ".")
HBI_trees <- read.delim(file = here(paste0(out.path.BZE3, "HBI_LT_update_3.csv")), sep = ",", dec = ".")

# tree summaries 
HBI_summary <- read.delim(file = here(paste0(out.path.BZE3, "HBI_LT_RG_DW_stocks_ha_all_groups.csv")), sep = ",", dec = ".") %>% 
  filter(stand_component == "LT" & plot_ID != "all") %>% 
  select(inv, plot_ID, stand, SP_code, mean_DBH_cm,Dg_cm, Hg_m) %>% distinct()

BZE3_summary <- read.delim(file = here(paste0(out.path.BZE3, "BZE3_LT_RG_DW_stocks_ha_all_groups.csv")), sep = ",", dec = ".") %>% 
  filter(stand_component == "LT" & plot_ID != "all") %>% 
  select(inv, plot_ID, stand, SP_code, mean_DBH_cm,Dg_cm, Hg_m)%>% distinct()


# growth
growth <- read.delim(file = here(paste0(out.path.BZE3, "HBI_BZE3_LT_RG_DW_changes_all_groups.csv")), sep = ",", dec = ".") %>% 
  filter(stand_component == "LT") %>% 
  select(stand_component, plot_ID, stand, C_layer, SP_code, age_period ,annual_growth_cm) %>% distinct()
  
# height coefficient 
# recreate the same datasets we use in the height calculation script
coeff_H_SP_P <- read.delim(file = here(paste0(out.path.BZE3,"coef_H_HBI_BZE3.csv")), sep = ",", dec = ".") %>% filter(plot_ID != "all")
coeff_H_SP <- read.delim(file = here(paste0(out.path.BZE3,"coef_H_HBI_BZE3.csv")), sep = ",", dec = ".") %>% filter(plot_ID == "all")



# 1. calculations --------------------------------------------------------------------------------------
# 1.1. find  trees in HBI dataset that were found removed in BZE3  ------------------------------------
# filter join for trees that are labelled with tree inventory status 2 in the BZE3 (post) inventory 
# we have to look for them in HBI_update_3 because we need the trees with the height already estimated
trees_harvested <- HBI_trees %>% 
  semi_join(., BZE3_trees_removed %>% 
              filter(tree_inventory_status == 2), # filter for trees in HBI which have the same plot_ID and tree_ID of those marked 2 in BZE3
            by = c("plot_ID", "tree_ID")) %>% 
  distinct()



# 1.2. DBH estimation --------------------------------------------------------------------------------------
#  the goal is to add 4 times diameter growth to the diameter of the trees removed between HBI and BZE3
# we will build a loop that selects the growth per plot, stand, c_layer, species
# if we can´t find average growth in this group, select growth by plot, stand, species
# if we can´t find average growth in this group, select growth by plot, species
# if we can´t find average growth in this group, select growth by species

# 1.2.1. assinging DBH growth --------------------------------------------------------------------------------------
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
                                         growth$C_layer == my.c.layer & 
                                         growth$SP_code == my.sp]
  
  # if we can´t find growth for the trees species, plot, stand and canopy layer
  # look for annual diameter growth in cm in the plot, stand and species of my.tree
  if(length(growth.cm) == 0){
    growth.cm <- growth$annual_growth_cm[growth$plot_ID == my.plot.id &
                                           growth$stand == my.stand & 
                                           growth$C_layer == "all" & 
                                           growth$SP_code == my.sp]}
  
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


# 1.2.2. join DBH including growth to tree dataset ------------------------------------------------------------------------------
# add dbh with growth to HBI_trees dataframe to calcualte biomass 
trees_harvested <- trees_harvested %>% 
  left_join(dbh_incl_growth.df %>% 
              mutate(across(c("plot_ID", "tree_ID", "DBH_incl_growth"), as.numeric)), 
            by = c("plot_ID", "inv", "tree_ID"))



# 1.3.1. height estimation ----------------------------------------------------------------------------------------------------------
# 1.3.1.1. get average dg and hg over both inventories -------------------------------------------------------------------------------
# calcualte average hg between the both inventories 
Hg_Dg_trees_total.df <- rbind(HBI_summary, 
                              BZE3_summary) %>% 
  group_by(plot_ID, stand, SP_code) %>% 
  summarise(Hg_m = mean(Hg_m), 
            Dg_cm = mean(Dg_cm), 
            mean_DBH_cm = mean(mean_DBH_cm))

# 1.3.1.2. calcualte missing heights --------------------------------------------------------------------------------------------------
# as the tree grows not only in whith but also in height we´ll have to estimate a "new" height according to 
# the "new" diameter. therefore we follow the same procedure as in the LT_heights script
# another option woul dbe to calcualte the height growth similar to the diameter growth and then add it to the original height from HBI

trees_harvested <- trees_harvested %>% 
  mutate(height_inc_growth = NA, 
         DBH_h_m = DBH_h_cm/100) %>% 
  ## 2.3.1. joining coefficients and Hg-Dg-data in 
  unite(SP_P_ID, plot_ID, SP_code, sep = "", remove = FALSE) %>%            # create column matching vectorised coefficients of coeff_SP_P (1.3. functions, h_nls_SP_P, dplyr::pull)
  left_join(.,coeff_H_SP_P %>% 
              mutate(plot_ID = as.integer(plot_ID)) %>% # joining R2 from coeff_SP_P -> R2.x
              select(plot_ID, SP_code, R2) %>% 
              unite(SP_P_ID, plot_ID, SP_code, sep = "", remove = FALSE),   # create column matching vectorised coefficients of coeff_SP_P (1.3. functions, h_nls_SP_P, dplyr::pull)
            by = c("plot_ID", "SP_code", "SP_P_ID")) %>% 
  left_join(., coeff_H_SP %>% select(SP_code, R2),               # joing R2 from coeff_SP data set -> R2.y
            by = "SP_code") %>% 
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
  # i = 1
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














# data export ---------------------------------------------------------------------------------------------
write.csv(trees_harvested, paste0(out.path.BZE3, paste(unique(trees_harvested$inv)[1], unique(BZE3_trees_removed$inv)[1], "LT_stock_removed", sep = "_"), ".csv"), row.names = FALSE, fileEncoding = "UTF-8")


stop("this is where 08_01 script for harvested tree stocks ends")



# notes and tests ---------------------------------------------------------
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