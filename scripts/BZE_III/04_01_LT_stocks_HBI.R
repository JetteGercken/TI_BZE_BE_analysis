# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the national soil inventory
# estimating biomass, carbon and nitrogen stock on single tree level

# ----- 0. SETUP ---------------------------------------------------------------

# ----- 0.1. packages and functions --------------------------------------------
source(paste0(getwd(), "/scripts/00_00_functions_library.R"))

# ----- 0.2. working directory -------------------------------------------------
here::here()

out.path.BZE3 <- ("output/out_data/out_data_BZE/") 

# ----- 0.3 data import --------------------------------------------------------
# LIVING TREES
# hbi BE dataset: this dataset contains the inventory data of the tree inventory accompanying the second national soil inventory
# here we should actually import a dataset called "HBI_trees_update_3.csv" which contains plot area and stand data additionally to 
# tree data
trees_data <- read.delim(file = here(paste0(out.path.BZE3, "HBI_LT_update_3.csv")), sep = ";", dec = ",") 

# 0.4 data preparation ---------------------------------------------------------
trees_data <- trees_data %>% mutate(H_m = as.numeric(H_m))

# 1. calculations ---------------------------------------------------------


# 1.1. biomass -----------------------------------------------------------------
# 1.1.1. biomass aboveground compartiments ---------------------------------------
bio.ag.kg.list <- vector("list", length = nrow(unique(trees_data[, c("plot_ID", "tree_ID")])))
for (i in 1:nrow(unique(trees_data[, c("plot_ID", "tree_ID")]))) {
  # i = 60
  # i = trees_data %>%  select(plot_ID, tree_ID, LH_NH) %>% distinct() %>% mutate(r_no = row_number()) %>% filter(LH_NH == "LB") %>%slice(1)%>% pull(r_no)
  
  # basic tree info
  # select one tree ID and plot ID for each individual tree per plot through unique(trees_data[, c("plot_ID", "tree_ID")])
  my.plot.id <- unique(trees_data[, c("plot_ID", "tree_ID")])[,"plot_ID"][i]
  my.tree.id <- unique(trees_data[, c("plot_ID", "tree_ID")])[,"tree_ID"][i]
  BL.or.CF <- unique(trees_data$LH_NH[trees_data$plot_ID==my.plot.id & trees_data$tree_ID==my.tree.id])
  
  # select variales for tree object: tapes species, diameter, diameter measuring height, tree height
  spp = na.omit(unique(trees_data$tpS_ID[trees_data$plot_ID==my.plot.id & trees_data$tree_ID==my.tree.id]))
  Dm = na.omit(as.list(as.numeric(unique(trees_data$DBH_cm[trees_data$plot_ID==my.plot.id & trees_data$tree_ID==my.tree.id])))) 
  Hm = na.omit(as.list(as.numeric(unique(trees_data$DBH_h_cm[trees_data$plot_ID==my.plot.id & trees_data$tree_ID==my.tree.id])/100)))
  Ht = na.omit(as.numeric(unique(trees_data$H_m[trees_data$plot_ID==my.plot.id & trees_data$tree_ID==my.tree.id])))
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
    "plot_ID" = c(as.integer(trees_data$plot_ID[trees_data$plot_ID == my.plot.id & trees_data$tree_ID == my.tree.id])), 
    "tree_ID" = c(as.integer(trees_data$tree_ID[trees_data$plot_ID == my.plot.id & trees_data$tree_ID == my.tree.id])), 
    "inv" = c(trees_data$inv[trees_data$plot_ID == my.plot.id & trees_data$tree_ID == my.tree.id]), 
    "inv_year" = c(as.integer(trees_data$inv_year[trees_data$plot_ID == my.plot.id & trees_data$tree_ID == my.tree.id])),
    "LH_NH" = c(trees_data$LH_NH[trees_data$plot_ID == my.plot.id & trees_data$tree_ID == my.tree.id]),
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
bio.bg.kg.list <- vector("list", length = nrow(unique(trees_data[, c("plot_ID", "tree_ID")])))
for (i in 1:nrow(unique(trees_data[, c("plot_ID", "tree_ID")]))) {
  # i = 60
  # i = trees_data %>%  select(plot_ID, tree_ID, LH_NH) %>% distinct() %>% mutate(r_no = row_number()) %>% filter(LH_NH == "LB") %>%slice(1)%>% pull(r_no)
  
  # basic tree info
  my.plot.id <- unique(trees_data[, c("plot_ID", "tree_ID")])[,"plot_ID"][i]
  my.tree.id <- unique(trees_data[, c("plot_ID", "tree_ID")])[,"tree_ID"][i]
  #my.inv <-  unique(trees_data[, c("plot_ID", "tree_ID")])[,"inv"][i]
  BL.or.CF <- unique(trees_data$LH_NH[trees_data$plot_ID==my.plot.id & trees_data$tree_ID==my.tree.id])
  
  # select variales for tree object
  spp = unique(trees_data$Bio_SP_group[trees_data$plot_ID==my.plot.id & trees_data$tree_ID==my.tree.id])
  dbh.cm = as.numeric(unique(trees_data$DBH_cm[trees_data$plot_ID==my.plot.id & trees_data$tree_ID==my.tree.id]))
   
 
  # calculate biomass per compartiment
  B_kg_tree <- as.data.frame(GHGI_bB(spp, dbh.cm))[,1]
 
  bio.info.df <- as.data.frame(cbind(
    "plot_ID" = c(as.integer(my.plot.id)), 
    "tree_ID" = c(as.integer(my.tree.id)), 
    "inv" = unique(trees_data$inv[trees_data$plot_ID==my.plot.id & trees_data$tree_ID==my.tree.id]), 
    "inv_year" = c(as.integer(unique(trees_data$inv_year[trees_data$plot_ID==my.plot.id & trees_data$tree_ID==my.tree.id]))),
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

trees_data <- trees_data %>% left_join(., 
                    rbind(bio_ag_kg_df , 
                          bio_bg_kg_df, 
                          bio_total_kg_df), 
                    by = c("plot_ID", "tree_ID", "inv", "inv_year"), 
                    multiple = "all") 

# 1.2. Nitrogen calculation -----------------------------------------------
# 1.2.1. Nitrogen stock in abofeground and belowgroung compartiments-----------------------------------------------
N_ag_bg_kg_df <- trees_data %>%
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
trees_data <- trees_data %>% left_join(., 
                             rbind(N_ag_bg_kg_df , 
                                   N_total_kg_df), 
                             by = c("plot_ID", "tree_ID", "inv", "inv_year", "compartiment"), 
                             multiple = "all")


# 1.3. carbon stock per tree & compartiment -------------------------------------------------------
trees_data <- trees_data %>% mutate(C_kg_tree = carbon(B_kg_tree))



# data export ---------------------------------------------------------------------------------------------
trees_update_4 <- trees_data 

# HBI dataset including estimated heights (use write.csv2 to make ";" as separator between columns)
write.csv2(trees_update_4, paste0(out.path.BZE3, paste(unique(trees_update_4$inv)[1], "LT_update_4", sep = "_"), ".csv"))










# NOTES:  -----------------------------------------------------------------


# this was meant to create a columnwith the compartiments before adding the biomass in them but it took a lot of time and was after all not necesarry as
# one can also introduce the column with in the loop
 trees.comp.list <- vector("list", length = length(trees$X))
 
 for (i in 1:length(trees$X)) {
   # select every single row of the dataframe and repeat it as many times as we have compartiments
   df <- trees[i,]
   comp <- as.character(c("stw","stb","sw", "sb", "fwb", "ndl" ))
   # https://stackoverflow.com/questions/11121385/repeat-rows-of-a-data-frame
   df <- cbind(df[rep(seq_len(nrow(df)), each = length(comp)), ], 
               comp)
   trees.comp.list[[i]] <- df
 }
 trees.comp.final <- rbindlist(trees.comp.list)
 trees <- as.data.frame(trees.comp.final)


## this was a trial of the previous N_all_com function that still involved a switch for foliage, belowgornd and aboveground woody compartiments
 trees[1:500,] %>% mutate(
   N_kg_test = case_when(
     compartiment == "ndl" ~ N_all_com(B_kg_tree, N_SP_group, N_f_SP_group_MoMoK, N_bg_SP_group, compartiment , comp.function = "f"), 
     compartiment == "bg" ~ N_all_com(B_kg_tree, N_SP_group, N_f_SP_group_MoMoK, N_bg_SP_group, compartiment , comp.function = "bg"), 
     !(compartiment %in% ("ag, total, ndl, bg")) ~ N_all_com(B_kg_tree, N_SP_group, N_f_SP_group_MoMoK, N_bg_SP_group, compartiment , comp.function = "ag.not.foliage"),
     TRUE ~ 0)
 )
 

## this is a loop to calculate the nitrogen stocks but it takes so much more time then using a function and mutate, that is just doesnt make sense to apply it

N.ag.kg.list <- vector("list", length = nrow(unique(trees[, c("plot_ID", "tree_ID")])))
for (i in 1:nrow(unique(trees[, c("plot_ID", "tree_ID")]))) {
  # i = 1
  # i = trees %>%  select(plot_ID, tree_ID, LH_NH) %>% distinct() %>% mutate(r_no = row_number()) %>% filter(LH_NH == "LB") %>%slice(1)%>% pull(r_no)
  
  # basic tree info
  # select one tree ID and plot ID for each individual tree per plot through unique(trees[, c("plot_ID", "tree_ID")])
  my.plot.id <- unique(trees[, c("plot_ID", "tree_ID")])[,"plot_ID"][i]
  my.tree.id <- unique(trees[, c("plot_ID", "tree_ID")])[,"tree_ID"][i]
  
  # select aboveground biomass compartiments and belowgroung biomass for the respective tree in the respective plot
  my.tree.bio <- trees[trees$plot_ID == my.plot.id & trees$tree_ID == my.tree.id & !(trees$compartiment %in% c("ag", "total")), ][, c("compartiment", "B_kg_tree")]
  # species group for the woody compartiments according to Rumpf et al. 2018
  my.tree.N.SP.w <- unique(trees[trees$plot_ID == my.plot.id & trees$tree_ID == my.tree.id,][, "N_SP_group"])
  # species groups for foliage ompartiments: foliage nitrogen content is not going to be calculatet
  #my.tree.N.SP.f <- unique(trees[trees$plot_ID == my.plot.id & trees$tree_ID == my.tree.id,][, "N_f_SP_group_MoMoK"])
  my.tree.N.SP.bg <- unique(trees[trees$plot_ID == my.plot.id & trees$tree_ID == my.tree.id,][, "N_bg_SP_group"])
  # save woody compartiments in list to select them easier later
  my.tree.comp.N.w  <- my.tree.bio$compartiment[!(my.tree.bio$compartiment %in% c("ndl", "bg"))]
  
  # select the nitrogen content of the compartiments and species of the respective tree i by nitorgen species group and compartiment
  ## woody compartiments
  n_con_w <- N_con_w[N_con_w$SP_BWI == my.tree.N.SP.w & N_con_w$compartiment %in% c(my.tree.comp.N.w),][, c("N_con", "compartiment")]
  ## foliage compartiment
     #n_con_f <-  N_con_f[N_con_f$N_f_SP_group_MoMoK == my.tree.N.SP.f,][, c("N_con", "compartiment")]
  ## belowground compartiment
  # proably I will also have to assign new species groups since those were only created for MoMoK
  n_con_bg <- N_con_bg[N_con_bg$SP_group == my.tree.N.SP.bg,][, c("N_con", "compartiment")]
  
  # calculate niotrogen stock per tree per compartiment
  N.df <- rbind(
    ## stock in woody compartiments: merge biomass and content together by compartiment and 
    merge(my.tree.bio, n_con_w, by="compartiment") %>% mutate(N_kg_tree =  B_kg_tree *as.numeric(N_con)),
    #merge(my.tree.bio, n_con_f, by="compartiment") %>% mutate(N_kg_tree =  B_kg_tree *as.numeric(N_con)),
    merge(my.tree.bio, n_con_bg, by="compartiment") %>% mutate(N_kg_tree =  B_kg_tree *as.numeric(N_con))
  ) 
  
  
  N.info.df <- as.data.frame(cbind(
    "plot_ID" = c(rep(my.plot.id, times = length(N.df$compartiment))), 
    "tree_ID" = c(rep(my.tree.id, times = length(N.df$compartiment))), 
    "inv" = c(rep(unique(trees$inv[trees$plot_ID == my.plot.id & trees$tree_ID == my.tree.id]), times = length(N.df$compartiment))), 
    "inv_year" =  c(rep(unique(trees$inv_year[trees$plot_ID == my.plot.id & trees$tree_ID == my.tree.id]), times = length(N.df$compartiment))),
    "compartiment" = c(N.df$compartiment),
    "N_kg_tree" = c(as.numeric(N.df$N_kg_tree))
  ))
  
  N.ag.bg.kg.list[[i]] <- N.info.df
  
  
}
N.ag.bg.kg.final <- rbindlist(N.ag..bg.kg.list)
N_ag_bg_kg_df <- as.data.frame(N.ag.bg.kg.final)





ifelse(my.tree.bio$compartiment == "f", N_all_com (my.tree.bio$B_kg_tree[my.tree.bio$compartiment == "ndl"], 
                                                   my.tree.N.SP.w, 
                                                   my.tree.N.SP.f, 
                                                   my.tree.N.SP.bg, 
                                                   my.tree.bio$compartiment[my.tree.bio$compartiment == "ndl"], 
                                                   comp.function = "f"), 
       ifelse(my.tree.bio$compartiment == "bg", N_all_com(my.tree.bio$B_kg_tree[my.tree.bio$compartiment == "bg"], 
                                                          my.tree.N.SP.w, 
                                                          my.tree.N.SP.f, 
                                                          my.tree.N.SP.bg, 
                                                          my.tree.bio$compartiment[my.tree.bio$compartiment == "bg"], 
                                                          comp.function = "bg"), 
              ifelse(!(my.tree.bio$compartiment %in% c("f", "bg")), N_all_com(my.tree.bio$B_kg_tree, 
                                                                              my.tree.N.SP.w, 
                                                                              my.tree.N.SP.f, 
                                                                              my.tree.N.SP.bg, 
                                                                              my.tree.bio$compartiment, 
                                                                              comp.function = "ag.not.foliage"), 
                     0)))


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


## divenrent ways to calcualte nitrogen content in multiple woody compartiments
# N_kg_tree.w <- merge(my.tree.bio, n_con_w, by="compartiment")$B_kg_tree * as.numeric(merge(my.tree.bio, n_con_w, by="compartiment")$N_con)
# my.tree.bio$B_kg_tree[my.tree.bio$compartiment %in% c(my.tree.comp.N.w)]*as.numeric(n_con_w$N_con[n_con_w$compartiment %in% c(my.tree.comp.N.w)])


merg.N.w.bio <- merge(my.tree.bio, n_con_w, by="compartiment")
merg.N.w.bio$N_kg_tree <- merg.N.w.bio$B_kg_tree*as.numeric(merg.N.w.bio$N_con)
# calculate Nitrogen per compartiment
