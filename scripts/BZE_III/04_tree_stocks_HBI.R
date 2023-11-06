# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the national soil inventory
# estimating biomass, carbon and nitrogen stock on single tree level

# ----- 0. SETUP ---------------------------------------------------------------

# ----- 0.1. packages and functions --------------------------------------------
source(paste0(getwd(), "/scripts/00_functions_library.R"))

# ----- 0.2. working directory -------------------------------------------------
here::here()
getwd()

out.path.BZE3 <- ("output/out_data/out_data_BZE/") 

# ----- 0.3 data import --------------------------------------------------------
# LIVING TREES
# hbi BE dataset: this dataset contains the inventory data of the tree inventory accompanying the second national soil inventory
# here we should actually import a dataset called "HBI_trees_update_2.csv" which contains plot area and stand data additionally to 
# tree data
trees <- read.delim(file = here("output/out_data/out_data_BZE/HBI_trees_update_3.csv"), sep = ",", dec = ",") 
# HBI_trees <- read.delim(file = here("data/input/BZE2_HBI/BI_trees_update_3.csv"), sep = ",", dec = ",", stringsAsFactors=FALSE)
# trees %>% filter(H_m <0)


# 0.4 data preparation ---------------------------------------------------------
# colnames HBI
# colnames(HBI_trees) <- c("multi_stem", "D_mm", "DBH_class", "DBH_h_cm", "H_dm",
#                          "azi_gon", "SP_code", "tree_ID", "plot_ID", "tree_inventory_status", 
#                          "DBH_cm", "age", "C_layer", "C_h_dm", "Kraft", "Dist_cm", "age_meth")  
# HBI_trees <- HBI_trees %>% select(plot_ID,  tree_ID ,  tree_inventory_status ,  multi_stem ,
#                                   Dist_cm ,  azi_gon ,age ,  age_meth ,  SP_code , DBH_class ,  Kraft ,  
#                                   C_layer , H_dm ,  C_h_dm , D_mm ,   DBH_h_cm ,  DBH_cm )



# create column for with compartiments
  # here we create a column with compartiments, so that we can apply the tprBiomass fucntion without having to pivot the whola dataset later
  # we eill only create the categories: 
    # stw (=stump wood), 
    # stb (=stump bark), 
    # sw (=solid wood with diameter above 7cm over bark), 
    # sb (=bark of component sw), 
    # fwb (=fine wood incl. bark) 
    # ndl (=needles)


trees <- trees %>% mutate(H_m = as.numeric(H_m))

# trees.comp.list <- vector("list", length = length(trees$X))
# 
# for (i in 1:length(trees$X)) {
#   # select every single row of the dataframe and repeat it as many times as we have compartiments
#   df <- trees[i,]
#   comp <- as.character(c("stw","stb","sw", "sb", "fwb", "ndl" ))
#   # https://stackoverflow.com/questions/11121385/repeat-rows-of-a-data-frame
#   df <- cbind(df[rep(seq_len(nrow(df)), each = length(comp)), ], 
#               comp)
#   trees.comp.list[[i]] <- df
# }
# trees.comp.final <- rbindlist(trees.comp.list)
# trees <- as.data.frame(trees.comp.final)


# 1. calculations ---------------------------------------------------------


# 1.1. biomass -----------------------------------------------------------------
# 1.1.1. biomass aboveground compartiments ---------------------------------------

bio.ag.kg.list <- vector("list", length = nrow(unique(trees[, c("plot_ID", "tree_ID")])))
for (i in 1:nrow(unique(trees[, c("plot_ID", "tree_ID")]))) {
  # i = 60
  # i = trees %>%  select(plot_ID, tree_ID, LH_NH) %>% distinct() %>% mutate(r_no = row_number()) %>% filter(LH_NH == "LB") %>%slice(1)%>% pull(r_no)
  
  # basic tree info
  # select one tree ID and plot ID for each individual tree per plot through unique(trees[, c("plot_ID", "tree_ID")])
  my.plot.id <- unique(trees[, c("plot_ID", "tree_ID")])[,"plot_ID"][i]
  my.tree.id <- unique(trees[, c("plot_ID", "tree_ID")])[,"tree_ID"][i]
  BL.or.CF <- unique(trees$LH_NH[trees$plot_ID==my.plot.id & trees$tree_ID==my.tree.id])
  
  # select variales for tree object: tapes species, diameter, diameter measuring height, tree height
  spp = na.omit(unique(trees$tpS_ID[trees$plot_ID==my.plot.id & trees$tree_ID==my.tree.id]))
  Dm = na.omit(as.list(as.numeric(unique(trees$DBH_cm[trees$plot_ID==my.plot.id & trees$tree_ID==my.tree.id])))) 
  Hm = na.omit(as.list(as.numeric(unique(trees$DBH_h_cm[trees$plot_ID==my.plot.id & trees$tree_ID==my.tree.id])/100)))
  Ht = na.omit(as.numeric(unique(trees$H_m[trees$plot_ID==my.plot.id & trees$tree_ID==my.tree.id])))
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
    "plot_ID" = c(as.integer(trees$plot_ID[trees$plot_ID == my.plot.id & trees$tree_ID == my.tree.id])), 
    "tree_ID" = c(as.integer(trees$tree_ID[trees$plot_ID == my.plot.id & trees$tree_ID == my.tree.id])), 
    "inv" = c(trees$inv[trees$plot_ID == my.plot.id & trees$tree_ID == my.tree.id]), 
    "inv_year" = c(as.integer(trees$inv_year[trees$plot_ID == my.plot.id & trees$tree_ID == my.tree.id])),
    "LH_NH" = c(trees$LH_NH[trees$plot_ID == my.plot.id & trees$tree_ID == my.tree.id]),
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
bio.ag.kg.final <- rbindlist(bio.ag.kg.list)
bio.ag.kg.df <- as.data.frame(bio.ag.kg.final)



# 1.1.2. biomass belowground compartiments ----------------------------------
bio.bg.kg.list <- vector("list", length = nrow(unique(trees[, c("plot_ID", "tree_ID")])))
for (i in 1:nrow(unique(trees[, c("plot_ID", "tree_ID")]))) {
  # i = 60
  # i = trees %>%  select(plot_ID, tree_ID, LH_NH) %>% distinct() %>% mutate(r_no = row_number()) %>% filter(LH_NH == "LB") %>%slice(1)%>% pull(r_no)
  
  # basic tree info
  my.plot.id <- unique(trees[, c("plot_ID", "tree_ID")])[,"plot_ID"][i]
  my.tree.id <- unique(trees[, c("plot_ID", "tree_ID")])[,"tree_ID"][i]
  #my.inv <-  unique(trees[, c("plot_ID", "tree_ID")])[,"inv"][i]
  BL.or.CF <- unique(trees$LH_NH[trees$plot_ID==my.plot.id & trees$tree_ID==my.tree.id])
  
  # select variales for tree object
  spp = unique(trees$Bio_SP_group[trees$plot_ID==my.plot.id & trees$tree_ID==my.tree.id])
  dbh.cm = as.numeric(unique(trees$DBH_cm[trees$plot_ID==my.plot.id & trees$tree_ID==my.tree.id]))
   
 
  # calculate biomass per compartiment
  B_kg_tree <- as.data.frame(GHGI_bB(spp, dbh.cm))[,1]
 
  bio.info.df <- as.data.frame(cbind(
    "plot_ID" = c(as.integer(my.plot.id)), 
    "tree_ID" = c(as.integer(my.tree.id)), 
    "inv" = unique(trees$inv[trees$plot_ID==my.plot.id & trees$tree_ID==my.tree.id]), 
    "inv_year" = c(as.integer(unique(trees$inv_year[trees$plot_ID==my.plot.id & trees$tree_ID==my.tree.id]))),
    "compartiment" = c("bg"),
    "B_kg_tree" = c(as.numeric(B_kg_tree))
  ) ) 
  
  bio.bg.kg.list[[i]] <- bio.info.df
  
}

bio.bg.kg.final <- rbindlist(bio.bg.kg.list)
bio.bg.kg.df <- as.data.frame(bio.bg.kg.final)


# 1.1.3. biomass all compartiments - total ----------------------------------

bio.total.kg.df <- 
  rbind(
# calculate total biomass (aboveground + belowground) by summing up biomass in kg per tree in all compartiments
    rbind(
      bio.ag.kg.df, bio.bg.kg.df) %>% 
      group_by(plot_ID, tree_ID, inv, inv_year) %>% 
      summarize(B_kg_tree = sum(as.numeric(B_kg_tree))) %>% 
      mutate(compartiment = "total") %>% 
      select("plot_ID", "tree_ID", "inv", 
             "inv_year", "compartiment", "B_kg_tree"),
# calculate total aboveground biomass by summing up biomass in kg per tree in all aboveground compartiments
    bio.ag.kg.df%>% 
    group_by(plot_ID, tree_ID, inv, inv_year) %>% 
    summarize(B_kg_tree = sum(as.numeric(B_kg_tree))) %>% 
    mutate(compartiment = "ag")%>% 
    select("plot_ID", "tree_ID", "inv", 
           "inv_year", "compartiment", "B_kg_tree"))

# 1.1.4. harmonizing biomass strings and compartiment names ---------------
#  harmonize strings of bio.total.kg.df  
# https://stackoverflow.com/questions/20637360/convert-all-data-frame-character-columns-to-factors
bio.total.kg.df[,c(1,2, 4, 6)] <- lapply(bio.total.kg.df[,c(1,2,4, 6)], as.numeric)
bio.ag.kg.df[,c(1,2, 4, 6)] <- lapply(bio.ag.kg.df[,c(1,2,4, 6)], as.numeric)
bio.bg.kg.df[,c(1,2, 4, 6)] <- lapply(bio.bg.kg.df[,c(1,2,4, 6)], as.numeric)

# the foliage compartiment in all foliage dataset is, unlike in tapeS called "f" instead of "ndl"
# this is why we are going to change the name from "ndl" to "f" after having completed the calculations 
# with tapeS
# bio.ag.kg.df <- bio.ag.kg.df %>% 
#   mutate(compartiment = case_when(compartiment == "ndl" ~ "f", 
#                              compartiment == "sb" ~ "swb", 
#                              compartiment == "stb" ~"stwb",
#                              TRUE ~ compartiment))


# 1.1.4. join biomass into tree dataset -----------------------------------

trees <- trees %>% left_join(., 
                    rbind(bio.ag.kg.df , 
                          bio.bg.kg.df, 
                          bio.total.kg.df), 
                    by = c("plot_ID", "tree_ID", "inv", "inv_year"), 
                    multiple = "all") 

# 1.2. Nitrogen calculation -----------------------------------------------



N.ag.kg.list <- vector("list", length = nrow(unique(trees[, c("plot_ID", "tree_ID")])))
for (i in 1:nrow(unique(trees[, c("plot_ID", "tree_ID")]))) {
  # i = 1
  # i = trees %>%  select(plot_ID, tree_ID, LH_NH) %>% distinct() %>% mutate(r_no = row_number()) %>% filter(LH_NH == "LB") %>%slice(1)%>% pull(r_no)
  
  # basic tree info
  # select one tree ID and plot ID for each individual tree per plot through unique(trees[, c("plot_ID", "tree_ID")])
  my.plot.id <- unique(trees[, c("plot_ID", "tree_ID")])[,"plot_ID"][i]
  my.tree.id <- unique(trees[, c("plot_ID", "tree_ID")])[,"tree_ID"][i]
  
  # select aboveground biomass compartiments and belowgroung biomass for the respective tree in the respective plot
  my.tree.bio <- trees[trees$plot_ID == my.plot.id & 
                         trees$tree_ID == my.tree.id &
                         !(trees$compartiment %in% c("ag", "total")), ][, c("compartiment", "B_kg_tree")]
  # species group for the woody compartiments according to Rumpf et al. 2018
  my.tree.N.SP.w <- trees[trees$plot_ID == my.plot.id & trees$tree_ID == my.tree.id,][, "N_SP_group"]
  my.tree.N.SP.f <- trees[trees$plot_ID == my.plot.id & trees$tree_ID == my.tree.id,][, "N_f_SP_group_MoMoK"]
  my.tree.N.SP.bg <- trees[trees$plot_ID == my.plot.id & trees$tree_ID == my.tree.id,][, "N_bg_SP_group"]
  
  
  n_con_w <- N_con_w %>% 
    mutate(compartiment = case_when(compartiment == "f" ~ "ndl", 
                                    compartiment == "swb" ~ "sb", 
                                    compartiment == "stwb" ~"stb",
                                    compartiment == "fw" ~ "fwb", 
                                    TRUE ~ compartiment))
    filter(compartiment %in% c(my.tree.bio$compartiment) & 
                                   compartiment != "f" &
                                    SP_BWI == my.tree.N.SP.w[1]) %>% dplyr::pull(N_con, SP_com);
  n_con_f <- N_con_f %>% dplyr::pull(N_con, N_f_SP_group_MoMoK) 
  # this function may have to be be adapted to the new dataset of the NSI which provides accurate N cocntents for all species and foliage
  # proably I will also have to assign new species groups to acces the foliage dataset correctly
  n_con_bg <- (c(EI = 3.71, BU = 3.03, FI = 4.14, KI = 1.77, 
                    KIN = 1.76, BI = 3.7, LA = 2.8))/1000; # divide concentration in mg per g by 1000 to get concentration in percent/ decimal number of percent 
  # unite the compartiment and species to select the correct nitrogen content
  SP_compart_Rumpf <- paste0(my.tree.N.SP.w, "_", my.tree.bio$compartiment);
  
  switch(
    comp.function, 
    f = B*n_con_f[N_spec_f], 
    stw = B*n_con_w[SP_compart_Rumpf], 
    stb = B*n_con_w[SP_compart_Rumpf], 
    sw = B*n_con_w[SP_compart_Rumpf], 
    sb = B*n_con_w[SP_compart_Rumpf], 
    fwb = B*n_con_w[SP_compart_Rumpf], 
    f = B*n_con_w[SP_compart_Rumpf],   
    ag.not.foliage =  B*n_con_w[SP_compart_Rumpf], 
    bg = B*n_con_bg[N_spec_Jacobsen]
  )
  
  
  # calculate Nitrogen per compartiment
  ifelse(my.tree.bio$compartiment == "f", N_all_com (my.tree.bio$B_kg_tree, 
                                                     my.tree.N.SP.w, 
                                                     my.tree.N.SP.f, 
                                                     my.tree.N.SP.bg, 
                                                     my.tree.bio$compartiment, 
                                                     comp.function = "f"), 
         ifelse(my.tree.bio$compartiment == "bg", N_all_com(my.tree.bio$B_kg_tree, 
                                                             my.tree.N.SP.w, 
                                                             my.tree.N.SP.f, 
                                                             my.tree.N.SP.bg, 
                                                             my.tree.bio$compartiment, 
                                                             comp.function = "bg"), 
                ifelse(!(my.tree.bio$compartiment %in% c("f", "bg")), N_all_com(my.tree.bio$B_kg_tree, 
                                                                   my.tree.N.SP.w, 
                                                                   my.tree.N.SP.f, 
                                                                   my.tree.N.SP.bg, 
                                                                   my.tree.bio$compartiment, 
                                                                   comp.function = "ag.not.foliage"), 
                       NA)))
  
  
  N_all_com (my.tree.bio$B_kg_tree, 
             my.tree.N.SP.w, 
             my.tree.N.SP.f, 
             my.tree.N.SP.bg, 
             my.tree.bio$compartiment, 
             comp.function = as.list(my.tree.bio$compartiment))
    
  N_all_com (B, N_spec_rumpf, N_spec_f, N_spec_Jacobsen, comp, tree.part)
  
  N.df <- as.data.frame(tprBiomass(obj = obj.trees, component = comp)) %>% 
    pivot_longer(cols = stw:ndl,
                 names_to = "compartiment", 
                 values_to = "B_kg_tree")
  
  
  N.info.df <- as.data.frame(cbind(
    "plot_ID" = c(as.integer(trees$plot_ID[trees$plot_ID == my.plot.id & trees$tree_ID == my.tree.id])), 
    "tree_ID" = c(as.integer(trees$tree_ID[trees$plot_ID == my.plot.id & trees$tree_ID == my.tree.id])), 
    "inv" = c(trees$inv[trees$plot_ID == my.plot.id & trees$tree_ID == my.tree.id]), 
    "inv_year" = c(as.integer(trees$inv_year[trees$plot_ID == my.plot.id & trees$tree_ID == my.tree.id])),
    "LH_NH" = c(trees$LH_NH[trees$plot_ID == my.plot.id & trees$tree_ID == my.tree.id]),
    "compartiment" = c(bio.df$compartiment),
    "B_kg_tree" = c(as.numeric(bio.df$B_kg_tree))
  ) ) %>% 
    # if the tree is a broadleafed tree Tapes cannot calculate the foliage mass, 
    # thus we calculate this subsequently trough the biomass function by Wutzler (2008)
    mutate(N_kg_tree = ifelse(compartiment == "ndl" & LH_NH == "LB", 
                              Wutzler_fB_L1(as.numeric(Dm), as.numeric(Ht)),
                              B_kg_tree)) %>% 
    dplyr::select(-c("LH_NH"))
  
  N.ag.kg.list[[i]] <- N.info.df
  
  
}
N.ag.kg.final <- rbindlist(N.ag.kg.list)
N.ag.kg.df <- as.data.frame(N.ag.kg.final)


rbind(bio.bg.kg.df, bio.bg.kg.df) %>% 
  mutate(N_kg_tree = case_when(
    comp == "bg" ~ N_N_all_com
  ))


# data export -------------------------------------------------------------

# HBI dataset including estimated heights
write.csv(HBI_trees_update_3, paste0(out.path.BZE3, paste(unique(HBI_trees_update_3$inv)[1], "trees_update_3", sep = "_"), ".csv"))
