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
BZE3_trees_removed <- read.delim(file = here("output/out_data/out_data_BZE/BZE3_LT_removed_2.csv"), sep = ",", dec = ".")
HBI_trees <- read.delim(file = here("output/out_data/out_data_BZE/HBI_LT_update_3.csv"), sep = ",", dec = ".")
# growth
growth <- read.delim(file = here("output/out_data/out_data_BZE/HBI_BZE3_LT_RG_DW_changes_all_groups.csv"), sep = ",", dec = ".") %>% 
  filter(stand_component == "LT") %>% 
  select(stand_component, plot_ID, stand, stand_type, C_layer, SP_code, age_period ,annual_growth_cm) %>% distinct()
  


# 1. calculations --------------------------------------------------------------------------------------
# 1.1. find  trees in HBI dataset that were found removed in BZE3  ------------------------------------
# filter join for trees that are labelled with tree inventory status 2 in the BZE3 (post) inventory 
trees_harvested <- HBI_trees %>% 
  semi_join(., BZE3_trees_removed %>% 
              filter(tree_inventory_status == 2), # filter for trees in HBI which have the same plot_ID and tree_ID of those marked 2 in BZE3
            by = c("plot_ID", "tree_ID")) %>% 
  distinct()


# 1.2. add 4 times diameter growth to the diameter of the trees removed between HBI and BZE3 --------
# we will have to build a loop that selects the growth per plot, stand, c_layer, species
# if we can´t find average growth in this group, select growth by plot, stand, species
# if we can´t find average growth in this group, select growth by plot, species
# if we can´t find average growth in this group, select growth by species

# prepare dataset for DBH at middle of harvesting period 
dbh_incl_growth.list <- vector("list", length = length(unique(trees_harvested$tree_ID)))
for (i in 1:length(unique(trees_harvested$tree_ID)) ) {
  # i = 57
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

# add dbh with growth to HBI_trees dataframe to calcualte biomass 
trees_harvested <- trees_harvested %>% 
  left_join(dbh_incl_growth.df %>% 
              mutate(across(c("plot_ID", "tree_ID"), as.integer)), 
            by = c("plot_ID", "inv", "tree_ID"))


# 1.3. Biomass ------------------------------------------------------------
# now we will calcualte the Biomass of the trees with the "new" diameter at the middle of the harvest period
# which means we run all biomass loops from 04_01_LT_stocks again
# 1.3.1. data preparation ---------------------------------------------------------
trees_harvested <- trees_harvested %>% mutate(H_m = as.numeric(H_m))


# 1.1. biomass -----------------------------------------------------------------
# 1.1.1. biomass aboveground compartiments ---------------------------------------
bio.ag.kg.list <- vector("list", length = nrow(unique(trees_harvested[, c("plot_ID", "tree_ID")])))
for (i in 1:nrow(unique(trees_harvested[, c("plot_ID", "tree_ID")]))) {
  # i = 60
  # i = trees_harvested %>%  select(plot_ID, tree_ID, LH_NH) %>% distinct() %>% mutate(r_no = row_number()) %>% filter(LH_NH == "LB") %>%slice(1)%>% pull(r_no)
  
  # basic tree info
  # select one tree ID and plot ID for each individual tree per plot through unique(trees_harvested[, c("plot_ID", "tree_ID")])
  my.plot.id <- unique(trees_harvested[, c("plot_ID", "tree_ID")])[,"plot_ID"][i]
  my.tree.id <- unique(trees_harvested[, c("plot_ID", "tree_ID")])[,"tree_ID"][i]
  BL.or.CF <- unique(trees_harvested$LH_NH[trees_harvested$plot_ID==my.plot.id & trees_harvested$tree_ID==my.tree.id])
  
  # select variales for tree object: tapes species, diameter, diameter measuring height, tree height
  spp = na.omit(unique(trees_harvested$tpS_ID[trees_harvested$plot_ID==my.plot.id & trees_harvested$tree_ID==my.tree.id]))
  Dm = na.omit(as.list(as.numeric(unique(trees_harvested$DBH_cm[trees_harvested$plot_ID==my.plot.id & trees_harvested$tree_ID==my.tree.id])))) 
  Hm = na.omit(as.list(as.numeric(unique(trees_harvested$DBH_h_cm[trees_harvested$plot_ID==my.plot.id & trees_harvested$tree_ID==my.tree.id])/100)))
  Ht = na.omit(as.numeric(unique(trees_harvested$H_m[trees_harvested$plot_ID==my.plot.id & trees_harvested$tree_ID==my.tree.id])))
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



# 1.1.2. biomass belowground compartiments ----------------------------------
bio.bg.kg.list <- vector("list", length = nrow(unique(trees_harvested[, c("plot_ID", "tree_ID")])))
for (i in 1:nrow(unique(trees_harvested[, c("plot_ID", "tree_ID")]))) {
  # i = 60
  # i = trees_harvested %>%  select(plot_ID, tree_ID, LH_NH) %>% distinct() %>% mutate(r_no = row_number()) %>% filter(LH_NH == "LB") %>%slice(1)%>% pull(r_no)
  
  # basic tree info
  my.plot.id <- unique(trees_harvested[, c("plot_ID", "tree_ID")])[,"plot_ID"][i]
  my.tree.id <- unique(trees_harvested[, c("plot_ID", "tree_ID")])[,"tree_ID"][i]
  #my.inv <-  unique(trees_harvested[, c("plot_ID", "tree_ID")])[,"inv"][i]
  BL.or.CF <- unique(trees_harvested$LH_NH[trees_harvested$plot_ID==my.plot.id & trees_harvested$tree_ID==my.tree.id])
  
  # select variales for tree object
  spp = unique(trees_harvested$Bio_SP_group[trees_harvested$plot_ID==my.plot.id & trees_harvested$tree_ID==my.tree.id])
  dbh.cm = as.numeric(unique(trees_harvested$DBH_cm[trees_harvested$plot_ID==my.plot.id & trees_harvested$tree_ID==my.tree.id]))
  
  
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


# 1.1.3. biomass all compartiments - total ----------------------------------

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

# 1.1.4. harmonizing biomass strings and compartiment names ---------------
#  harmonize strings of bio_total_kg_df  
# https://stackoverflow.com/questions/20637360/convert-all-data-frame-character-columns-to-factors
bio_total_kg_df[,c(1,2, 4, 6)] <- lapply(bio_total_kg_df[,c(1,2,4, 6)], as.numeric)
bio_ag_kg_df[,c(1,2, 4, 6)] <- lapply(bio_ag_kg_df[,c(1,2,4, 6)], as.numeric)
bio_bg_kg_df[,c(1,2, 4, 6)] <- lapply(bio_bg_kg_df[,c(1,2,4, 6)], as.numeric)


# 1.1.4. join biomass into tree dataset -----------------------------------

trees_harvested <- trees_harvested %>% distinct() %>% 
  left_join(., 
            rbind(bio_ag_kg_df , 
                  bio_bg_kg_df, 
                  bio_total_kg_df) %>% 
              distinct(), 
            by = c("plot_ID", "tree_ID", "inv", "inv_year"), 
            multiple = "all") 


# 1.2. Nitrogen calculation -----------------------------------------------
# 1.2.1. Nitrogen stock in abofeground and belowgroung compartiments-----------------------------------------------
N_ag_bg_kg_df <- trees_harvested %>%
  filter(!(compartiment %in% c("ag", "total")))  %>%  # make sure the aboveground& belowground dataset doesnt include summed up compartiments like total and aboveground
  mutate(N_kg_tree = N_all_com(B_kg_tree, N_SP_group, N_f_SP_group_MoMoK, N_bg_SP_group, compartiment)) %>% 
  select(plot_ID, tree_ID, inv, inv_year, compartiment, N_kg_tree) 

# 1.2.2. Nitrogen ston in all compartiments summed up - total & aboveground  ----------------------------------
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


# 1.2.3. join Nitrogen stocks into tree dataset -----------------------------------
trees_harvested <- trees_harvested %>% left_join(., 
                                       rbind(N_ag_bg_kg_df , 
                                             N_total_kg_df), 
                                       by = c("plot_ID", "tree_ID", "inv", "inv_year", "compartiment"), 
                                       multiple = "all")


# 1.3. carbon stock per tree & compartiment -------------------------------------------------------
trees_harvested <- trees_harvested %>% mutate(C_kg_tree = carbon(B_kg_tree))



# data export ---------------------------------------------------------------------------------------------
write.csv(trees_harvested, paste0(out.path.BZE3, paste(unique(trees_harvested$inv)[1], unique(BZE3_trees_removed$inv)[1], "LT_stock_removed", sep = "_"), ".csv"), row.names = FALSE, fileEncoding = "UTF-8")


stop("this is where 08_01 script for harvested tree stocks ends")


