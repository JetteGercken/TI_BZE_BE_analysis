# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the  national soil inventory
# stock calculations for regeneration 
# BZE3


# ----- 0. SETUP ---------------------------------------------------------------

# ----- 0.1. packages and functions --------------------------------------------
source(paste0(getwd(), "/scripts/01_00_functions_library.R"))

# ----- 0.2. working directory -------------------------------------------------
here::here()
getwd()

out.path.BZE3 <- ("output/out_data/out_data_BZE/") 


# ----- 0.3 data import --------------------------------------------------------
# regeneration
# this dataset contains the plant specific inventory data of the regenertaion inventory of the HBI (BZE2), including stand and area info
RG_data <- read.delim(file = here(paste0(out.path.BZE3, "BZE3_RG_update_2.csv")), sep = ",", dec = ".")


# 1. calculations ---------------------------------------------------------

# 1.1. size class to diameter ---------------------------------------------
# translate size class into diameter
RG_data <- RG_data %>% 
  mutate(
    D_cm = sizeclass_to_d(D_class_cm), 
    H_m = H_cm/100) %>% 
  # join in species names and codes from x_bart
  left_join(., SP_names_com_ID_tapeS %>% 
              mutate(char_code_ger_lowcase = tolower(Chr_code_ger)), 
            by = c("SP_code" = "char_code_ger_lowcase"))



# 1.2 biomass -------------------------------------------------------------
# 1.2.1. biomass for RG trees height > 1.3m -------------------------------
# subset those trees that have a height above 1.3m and thus a DBH which allows them to be processed in TapeS
RG_above_1.3 <- RG_data[RG_data$H_m > 1.3, ]
# 1.2.1.1. aboveground biomass for RG trees height > 1.3m -------------------------------
# create output list
bio.ag.bg.kg.RG.above.1.3 <- vector("list", length = nrow(RG_above_1.3))
for (i in 1:nrow(RG_above_1.3)) {
  # i = 1
  
  # basic tree info
  # select one tree ID and plot ID for each individual tree per plot and sampling circuit
  my.plot.id <- RG_above_1.3[,"plot_ID"][i]
  my.ccs.id <- RG_above_1.3[,"CCS_nr"][i]
  my.tree.id <- RG_above_1.3[,"tree_ID"][i]
  
  # select varibales for aboveground functions & calcualte aboveground biomass as input for Poorter 
  # nationla greenhousegas inventory function for trees below 1.3m
  spp = unique(RG_above_1.3$Bio_SP_group[RG_above_1.3$plot_ID==my.plot.id & RG_above_1.3$tree_ID==my.tree.id & RG_above_1.3$CCS_nr==my.ccs.id])
  h.m = as.numeric(unique(RG_above_1.3$H_cm[RG_above_1.3$plot_ID==my.plot.id & RG_above_1.3$tree_ID==my.tree.id & RG_above_1.3$CCS_nr==my.ccs.id]))/100
  d.cm = as.numeric(unique(RG_above_1.3$D_cm[RG_above_1.3$plot_ID==my.plot.id & RG_above_1.3$tree_ID==my.tree.id & RG_above_1.3$CCS_nr==my.ccs.id]))
  ag_GHGI = as.data.frame(GHGI_aB_H1.3_DBHb10( spp, d.cm))[1,]
  # poorter et al. function for aboveground biomass for trees 
  spp_LHNH = unique(RG_above_1.3$LH_NH[RG_above_1.3$plot_ID==my.plot.id & RG_above_1.3$tree_ID==my.tree.id & RG_above_1.3$CCS_nr==my.ccs.id])
  
 
 
   # calculate biomass per compartiment
  bio.df <- as.data.frame(cbind(
    "compartiment" = c("fwb", "ndl", "ag", "bg"), 
    # sw+ fwb compartiment: 
    # for conifers we have to deduct the leaf biomass estimated by poorter because it is included in the aboveground biomass
    # for broadleafed trees we don´t have to 
    "B_kg_tree" = c(
      # fine wood: for RG it is called fine wood "fwb" but it means all woody compartiments (stemwood, stembark, stumpwood, stumpbark)
      ifelse(spp_LHNH == "NB", ag_GHGI - as.data.frame(Poorter_rg_RSR_RLR(ag_GHGI, spp_LHNH, compartiment = "foliage"))[1,], ag_GHGI),
      # foliage "ndl"
      as.data.frame(Poorter_rg_RSR_RLR(ag_GHGI, spp_LHNH, compartiment = "foliage"))[1,],
      # abovegorund "ag":
      # for broadleafed we have to add the leaf biomass estimated by poorter because it is included in the aboveground biomass
      # for conifers trees we don´t have to 
      ifelse(spp_LHNH == "LB", ag_GHGI + as.data.frame(Poorter_rg_RSR_RLR(ag_GHGI, spp_LHNH, compartiment = "foliage"))[1,],ag_GHGI),
      # belowground "bg"  
      as.data.frame(Poorter_rg_RSR_RLR(ag_GHGI, spp_LHNH, compartiment = "bg"))[1,]
    ))) 
  
  
  bio.info.df <- as.data.frame(cbind(
    "plot_ID" = c(my.plot.id), 
    "CCS_nr" = c(my.ccs.id),
    "tree_ID" = c(my.tree.id),
    "inv" = c(RG_above_1.3[,"inv"][i]),
    "inv_year" = c(RG_above_1.3[,"inv_year"][i]),
    "compartiment" = c(bio.df$compartiment),
    "B_kg_tree" = c(as.numeric(bio.df$B_kg_tree))
  ) )
  
  bio.ag.bg.kg.RG.above.1.3[[i]] <- bio.info.df
  
  # print(paste(my.plot.id, ",", my.tree.id))
  
}
bio.ag.bg.kg.RG.above.1.3.df <- as.data.frame(rbindlist(bio.ag.bg.kg.RG.above.1.3))
bio.ag.bg.kg.RG.above.1.3.df[,c(1,2, 3, 5,7)] <- lapply(bio.ag.bg.kg.RG.above.1.3.df[,c(1,2, 3, 5,7)], as.numeric)




# 1.2.1.3. total and total aboveground biomass for RG trees height > 1.3m -------------------------------
bio.total.kg.RG.above.1.3.df <- 
  # calculate total biomass (aboveground + belowground) by summing up biomass in kg per tree in all compartiments
  bio.ag.bg.kg.RG.above.1.3.df%>% 
  filter(compartiment %in% c("ag", "bg")) %>% 
  group_by(plot_ID, CCS_nr, tree_ID, inv, inv_year) %>% 
  summarize(B_kg_tree = sum(as.numeric(B_kg_tree))) %>% 
  mutate(compartiment = "total") %>% 
  select("plot_ID","CCS_nr", "tree_ID", "inv", 
         "inv_year", "compartiment", "B_kg_tree")



# 1.2.2. biomass for RG trees height < 1.3m -------------------------------
# 1.2.1.1. aboveground biomass for RG trees height > 1.3m -------------------------------
# 1.2.1.1.1. GHGI aboveground biomass for RG trees height > 1.3m -------------------------------
bio.ag.kg.RG.below.1.3.df <- RG_data %>% 
  filter(H_m <= 1.3) %>% 
  mutate(compartiment = "ag") %>% 
  mutate(B_kg_tree = (GHGI_aB_Hb1.3(LH_NH, H_m))) %>% 
  select("plot_ID","CCS_nr", "tree_ID", "inv", 
         "inv_year", "compartiment", "B_kg_tree") 


# 1.2.3. join RG biomass for trees <1.3m and >1.3m height  ----------------
RG_data <- RG_data %>% left_join(., rbind(
  bio.ag.bg.kg.RG.above.1.3.df,
  bio.total.kg.RG.above.1.3.df, 
  bio.ag.kg.RG.below.1.3.df),
  by = c("plot_ID", "CCS_nr", "tree_ID", "inv", "inv_year"),
  multiple = "all") 




# 1.3 Nitrogen stock ------------------------------------------------------
# 1.3.1. Nitrogen stock for aboveground  & belowgrowground ---------------------------------
N_ag_bg_kg_comps_df <- RG_data %>% 
  filter(H_m <= 1.3 & compartiment %in% c("ag") |
           H_m > 1.3 & !(compartiment %in% c("ag", "total"))) %>% 
  # calcualte nitrogen content for RG items with comppartiments "ndl", "fwb", "bg"
  mutate(N_kg_tree = case_when(H_m > 1.3 & !(compartiment %in% c("ag", "total")) ~ N_all_com(B_kg_tree, N_SP_group, N_f_SP_group_MoMoK, N_bg_SP_group, compartiment),
                               # small trees are treated as "fwb" compartiment tho their only available compartiment is "ag" which is treated as "fwb"
                               H_m <= 1.3 & compartiment == "ag" ~ N_all_com(B_kg_tree, N_SP_group, N_f_SP_group_MoMoK, N_bg_SP_group, "fwb"),
                               TRUE ~ NA)) %>% 
  select("plot_ID","CCS_nr", "tree_ID", "inv", 
         "inv_year", "compartiment", "N_kg_tree")





# 1.3.2. total Nitrogen stock ---------------------------------------------------------------
N_total_kg_df <- 
  # total N_stock in kg
  rbind(N_ag_bg_kg_comps_df %>% 
          # filter for trees with 1.3m height or more because those are the only trees that´ll have compartiments
          semi_join(RG_data %>% filter(H_m > 1.3), by = c("plot_ID","CCS_nr", "tree_ID", "inv", "inv_year")) %>% 
          # select compartiments "ndl", "fwb",  and add them together
          filter(!(compartiment %in% c("ag", "bg", "total"))) %>% 
          group_by(plot_ID, CCS_nr, tree_ID, inv, inv_year) %>% 
          summarize(N_kg_tree = sum(as.numeric(N_kg_tree))) %>% 
          mutate(compartiment = "ag") %>% 
          select("plot_ID","CCS_nr", "tree_ID", "inv", 
                 "inv_year", "compartiment", "N_kg_tree"),
        # total N_stock in kg
        N_ag_bg_kg_comps_df %>% 
          # filter for trees with 1.3m height or more because those are the only trees that´ll have compartiments
          semi_join(RG_data %>% filter(H_m > 1.3), by = c("plot_ID","CCS_nr", "tree_ID", "inv", "inv_year")) %>% 
          # select compartiments "ndl", "fwb", "bg" and add them together
          filter(!(compartiment %in% c("ag", "total"))) %>% 
          group_by(plot_ID, CCS_nr, tree_ID, inv, inv_year) %>% 
          summarize(N_kg_tree = sum(as.numeric(N_kg_tree))) %>% 
          mutate(compartiment = "total") %>% 
          select("plot_ID","CCS_nr", "tree_ID", "inv", 
                 "inv_year", "compartiment", "N_kg_tree")) %>% 
  arrange(plot_ID, CCS_nr, tree_ID)


# 1.3.3. join RG nitrogen stock for trees <1.3m and >1.3m height  ----------------
RG_data <- RG_data %>% left_join(., rbind(
  N_ag_bg_kg_comps_df,
  N_total_kg_df),
  by = c("plot_ID", "CCS_nr", "tree_ID", "inv", "inv_year", "compartiment"),
  multiple = "all") 




# 1.4. Carbon stock -------------------------------------------------------
RG_data <- RG_data %>% mutate(C_kg_tree = B_kg_tree*0.5)







# 2. data export ----------------------------------------------------------
RG_update_4 <- RG_data %>% anti_join(., RG_data %>% filter(B_kg_tree <0) %>% select(plot_ID, tree_ID) %>% distinct(), by = c("plot_ID", "tree_ID"))
RG_removed_4 <- RG_data %>% semi_join(., RG_data %>% filter(B_kg_tree <0) %>% select(plot_ID, tree_ID) %>% distinct(), by = c("plot_ID", "tree_ID"))

# HBI dataset including estimated heights 
write.csv(RG_update_4, paste0(out.path.BZE3, paste(unique(RG_update_4$inv)[1], "RG_update_4", sep = "_"), ".csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(RG_removed_4, paste0(out.path.BZE3, paste(unique(RG_update_4$inv)[1], "RG_removed_4", sep = "_"), ".csv"), row.names = FALSE, fileEncoding = "UTF-8")

stop("notes and biomass comparisson of BZE3 RG stocks starts here")









# 3. Biomass comparisson  ---------------------------------------
# 3.1. Biomass comp RG trees height > 1.3m -------------------------------
# 3.1.1. Biomass calculation RG trees height > 1.3m -------------------------------
# RG_above_1.3 <- RG_above_1.3 %>% filter(D_class_cm %in% c(2)) 
comparisson.bio.bg.kg.RG.above.1.3 <- vector("list", length = nrow(RG_above_1.3))
for (i in 1:nrow(RG_above_1.3)) {
  # i = 13
  
  # basic tree info
  # select one tree ID and plot ID for each individual tree per plot and sampling circuit
  my.plot.id <- RG_above_1.3[,"plot_ID"][i]
  my.ccs.id <- RG_above_1.3[,"CCS_nr"][i]
  my.tree.id <- RG_above_1.3[,"tree_ID"][i]
  my.tree.sp <- RG_above_1.3[,"SP_code"][i]
  
  
  # select variales for aboveground  functions
  # for GHGI
  spp = SP_names_com_ID_tapeS$Bio_SP_group[tolower(SP_names_com_ID_tapeS$Chr_code_ger) == my.tree.sp]
  dbh.cm = as.numeric(unique(RG_above_1.3$D_cm[RG_above_1.3$plot_ID==my.plot.id & RG_above_1.3$tree_ID==my.tree.id & RG_above_1.3$CCS_nr==my.ccs.id]))
  # for poorter
  bg_GHG = as.numeric(bio.bg.kg.RG.above.1.3.df$B_kg_tree[bio.bg.kg.RG.above.1.3.df$plot_ID==my.plot.id & bio.bg.kg.RG.above.1.3.df$tree_ID==my.tree.id & bio.bg.kg.RG.above.1.3.df$CCS_nr==my.ccs.id])
  spp_LHNH = SP_names_com_ID_tapeS$LH_NH[tolower(SP_names_com_ID_tapeS$Chr_code_ger) == my.tree.sp]
  # for Tapes
  spp_tapes = SP_names_com_ID_tapeS$tpS_ID[tolower(SP_names_com_ID_tapeS$Chr_code_ger) == my.tree.sp]
  Dm = na.omit(as.list(as.numeric(unique(RG_above_1.3$D_cm[RG_above_1.3$plot_ID==my.plot.id & RG_above_1.3$tree_ID==my.tree.id & RG_above_1.3$CCS_nr==my.ccs.id])))) 
  Hm = na.omit(as.list(as.numeric(1.3)))
  Ht = na.omit(as.numeric(unique(RG_above_1.3$H_m[RG_above_1.3$plot_ID==my.plot.id & RG_above_1.3$tree_ID==my.tree.id & RG_above_1.3$CCS_nr==my.ccs.id])))
  # create tapes compartiments
  comp <- as.character(c("stw","stb","sw", "sb", "fwb", "ndl", "agb"))
  
  # create object  
  obj.trees <- tprTrees(spp_tapes, Dm, Hm, Ht, inv = 4)
  # calculate biomass per compartiment
  tapes.bio.df <- as.data.frame(tprBiomass(obj = obj.trees, component = comp, mono =T)) %>% 
    pivot_longer(cols = stw:agb,
                 names_to = "compartiment", 
                 values_to = "B_kg_tree") %>% 
    mutate(bio_method = rep("tapes", times = length(comp))) %>% 
    select(compartiment, bio_method, B_kg_tree )
  
  # calculate biomass per compartiment
  bio.df <- as.data.frame(cbind(
    # "bio_method" = c("poorter+GHG", "poorter+GHG", "GHGI", "GHGI", "GHGI-poorter", "poorter+poorter", "GHGI-poorter", "tapes","GHGI-tapes"),
    #"compartiment" = c("sw", "ndl", "ag", "bg", "sw", "ag", "ag_diff", "ag","ag_diff"), 
    "compartiment" = c("stw","stb","sw", "sb", "fwb", "ndl", "ag", "bg"),
    # poorter biomassees based onGHGI belowground input biomass
    "poorter_B_kg_tree" = c(0 #stw 
                            ,0 # stb
                            ,as.data.frame(Poorter_rg_with_bg(bg_GHG, spp_LHNH, compartiment = "stem"))[1,]     # sw
                            , 0
                            , 0
                            ,as.data.frame(Poorter_rg_with_bg(bg_GHG, spp_LHNH, compartiment = "foliage"))[1,]  # ndl
                            # avboveground biomass consitsting of foliage+stem fom poorter
                            ,(as.data.frame(Poorter_rg_with_bg(bg_GHG, spp_LHNH, compartiment = "stem"))[1,]+as.data.frame(Poorter_rg_with_bg(bg_GHG, spp_LHNH, compartiment = "foliage"))[1,])
                            , 0),
    "GHG_B_kg_tree" = c(0 #stw
                        ,0# stb
                        #sw: biomass of stem if we would deduct only the foliage compartiment from the GHGI abovegro9und biomass
                        ,(as.data.frame(GHGI_aB_H1.3_DBHb10(spp, dbh.cm))[1,] - as.data.frame(Poorter_rg_with_bg(bg_GHG, spp_LHNH, compartiment = "foliage"))[1,]) # ag-ndl predicted by poorter
                        ,0 # sb
                        , 0 # fwb
                        # this is the foliage biomass estiamted by poorter with GHGI belowgeround input
                        ,as.data.frame(Poorter_rg_with_bg(bg_GHG, spp_LHNH, compartiment = "foliage"))[1,]  # ndl
                        # GHGI ag
                        ,as.data.frame(GHGI_aB_H1.3_DBHb10(spp, dbh.cm))[1,]                                # ag
                        ,bg_GHG),  # bg 
    "tapes_B_kg_tree" = c(tapes.bio.df$B_kg_tree 
                          ,0) # belowgroun)                   
  )) 
  
  
  bio.info.df <- as.data.frame(cbind(
    "plot_ID" = c(as.integer(my.plot.id)), 
    "CCS_nr" = c(my.ccs.id),
    "tree_ID" = c(as.integer(my.tree.id)), 
    "inv" = unique(RG_above_1.3$inv[RG_above_1.3$plot_ID==my.plot.id & RG_above_1.3$tree_ID==my.tree.id]), 
    "inv_year" = c(as.integer(unique(RG_above_1.3$inv_year[RG_above_1.3$plot_ID==my.plot.id & RG_above_1.3$tree_ID==my.tree.id]))),
    "compartiment" = c(bio.df$compartiment),
    "poorter_B_kg_tree" = c(as.numeric(bio.df$poorter_B_kg_tree)), 
    "GHG_B_kg_tree" = c(as.numeric(bio.df$GHG_B_kg_tree)),
    "tapes_B_kg_tree" = c(as.numeric(bio.df$tapes_B_kg_tree))
  ) ) 
  
  comparisson.bio.bg.kg.RG.above.1.3[[i]] <- bio.info.df
  
}
comparisson_bio_kg_RG_above_1.3_df <- as.data.frame(rbindlist(comparisson.bio.bg.kg.RG.above.1.3))
comparisson_bio_kg_RG_above_1.3_df[,c(1,2, 3, 5,7, 8, 9)] <- lapply(comparisson_bio_kg_RG_above_1.3_df[,c(1,2, 3, 5,7, 8, 9)], as.numeric)
comparisson_bio_kg_RG_above_1.3_df %>% filter(tapes_B_kg_tree <0 )

# 3.1.2. calcualte differences aboveground biomass ---------------------------------------------------
comparisson_bio_kg_RG_above_1.3_df <- comparisson_bio_kg_RG_above_1.3_df%>% 
  filter(compartiment %in% c("sw", "ndl", "ag")) %>% # select only compartiments all methods have in common
  mutate(diff_poorter_GHG = poorter_B_kg_tree - GHG_B_kg_tree,
         diff_poorter_tapes =  poorter_B_kg_tree - tapes_B_kg_tree,
         diff_GHG_tapes =  GHG_B_kg_tree - tapes_B_kg_tree) %>% 
  group_by( compartiment ) %>% 
  summarise(mean_diff_poorter_GHG = mean(diff_poorter_GHG), 
            mean_diff_poorter_tapeS = mean(diff_poorter_tapes), 
            mean_diff_GHG_tapes = mean(diff_GHG_tapes))


# 3.1.3. comparisson belowgroung biomass for RG trees height > 1.3m -------------------------------
bio.bg.kg.RG.above.1.3 <- vector("list", length = nrow(RG_above_1.3))
for (i in 1:nrow(RG_above_1.3)) {
  # i = 1
  
  # basic tree info
  # select one tree ID and plot ID for each individual tree per plot and sampling circuit
  my.plot.id <- RG_above_1.3[,"plot_ID"][i]
  my.ccs.id <- RG_above_1.3[,"CCS_nr"][i]
  my.tree.id <- RG_above_1.3[,"tree_ID"][i]
  my.tree.sp <- RG_above_1.3[,"SP_code"][i]
  
  # select variales for belowground functions
  spp = SP_names_com_ID_tapeS$Bio_SP_group[tolower(SP_names_com_ID_tapeS$Chr_code_ger) == my.tree.sp]
  dbh.cm = as.numeric(unique(RG_above_1.3$D_cm[RG_above_1.3$plot_ID==my.plot.id & RG_above_1.3$tree_ID==my.tree.id & RG_above_1.3$CCS_nr==my.ccs.id]))
  
  
  # calculate biomass per compartiment
  B_kg_tree <- as.data.frame(GHGI_bB(spp, dbh.cm))[,1]
  
  bio.info.df <- as.data.frame(cbind(
    "plot_ID" = c(as.integer(my.plot.id)), 
    "CCS_nr" = c(my.ccs.id),
    "tree_ID" = c(as.integer(my.tree.id)), 
    "inv" = unique(RG_above_1.3$inv[RG_above_1.3$plot_ID==my.plot.id & RG_above_1.3$tree_ID==my.tree.id]), 
    "inv_year" = c(as.integer(unique(RG_above_1.3$inv_year[RG_above_1.3$plot_ID==my.plot.id & RG_above_1.3$tree_ID==my.tree.id]))),
    "compartiment" = c("bg"),
    "B_kg_tree" = c(as.numeric(B_kg_tree))
  ) ) 
  
  bio.bg.kg.RG.above.1.3[[i]] <- bio.info.df
  
}
bio.bg.kg.RG.above.1.3.df <- as.data.frame(rbindlist(bio.bg.kg.RG.above.1.3))
bio.bg.kg.RG.above.1.3.df[,c(1,2, 3, 5,7)] <- lapply(bio.bg.kg.RG.above.1.3.df[,c(1,2, 3, 5,7)], as.numeric)

# calcualte differences
diff_bg <- abs(bio.bg.kg.RG.above.1.3.df$B_kg_tree - bio.ag.bg.kg.RG.above.1.3.df$B_kg_tree[bio.ag.bg.kg.RG.above.1.3.df$compartiment == "bg"])
summary(diff_bg)
summary(bio.bg.kg.RG.above.1.3.df$B_kg_tree)
summary(bio.ag.bg.kg.RG.above.1.3.df$B_kg_tree[bio.ag.bg.kg.RG.above.1.3.df$compartiment == "bg"])



# 3.2. biomas comp RG trees < 1.3m height ---------------------------------
# subset those trees that have a height above 1.3m and thus a DBH which allows them to be processed in TapeS
HBI.RG.below.1 <- RG_data[RG_data$H_m <= 1, ]

# 3.2.1 calculate biomass via Wolff, Poorter, GHGI --------------------------

# comparing the biomass per compartiment for trees under 1m: 
#      - compartiments of poorter with with GHGI vs. Wollf input biomass 
#      - compartiments of poorter with wolff input biomas vs. compartiment of wolff with wolff input biomass 
poorter.bio.ag.bg.kg.RG.below.1 <- vector("list", length = nrow(HBI.RG.below.1))
for (i in 1:nrow(HBI.RG.below.1)) {
  # i = 1
  
  # basic tree info
  # select one tree ID and plot ID for each individual tree per plot and sampling circuit
  my.plot.id <- HBI.RG.below.1[,"plot_ID"][i]
  my.ccs.id <- HBI.RG.below.1[,"CCS_nr"][i]
  my.tree.id <- HBI.RG.below.1[,"tree_ID"][i]
  
  # select varibales for aboveground functions & calcualte aboveground biomass as input for Poorter 
  # nationla greenhousegas inventory function for trees below 1.3m
  spp_LHNH = unique(HBI.RG.below.1$LH_NH[HBI.RG.below.1$plot_ID==my.plot.id & HBI.RG.below.1$tree_ID==my.tree.id & HBI.RG.below.1$CCS_nr==my.ccs.id])
  h.m = as.numeric(unique(HBI.RG.below.1$H_cm[HBI.RG.below.1$plot_ID==my.plot.id & HBI.RG.below.1$tree_ID==my.tree.id & HBI.RG.below.1$CCS_nr==my.ccs.id]))/100
  ag_GHGI = as.data.frame(GHGI_aB_Hb1.3(spp_LHNH, h.m))[1,]
  # Wolff et al. function for aboveground biomass for trees below 1m
  spp = unique(HBI.RG.below.1$RG_Wolff_bio[HBI.RG.below.1$plot_ID==my.plot.id & HBI.RG.below.1$tree_ID==my.tree.id & HBI.RG.below.1$CCS_nr==my.ccs.id])
  h.cm = as.numeric(unique(HBI.RG.below.1$H_cm[HBI.RG.below.1$plot_ID==my.plot.id & HBI.RG.below.1$tree_ID==my.tree.id & HBI.RG.below.1$CCS_nr==my.ccs.id]))
  whd.mm = as.numeric(h.to.whd(h.cm, spp))
  stem_WOLFF_kg = as.data.frame(wolff.bio.below.1m(h.cm, spp, compartiment = "stem"))[1,]/1000 # divide by 1000 to transform in kg
  
  
  # calculate biomass per compartiment
  poorter_B_kg_tree <- as.data.frame(cbind(
    "bio_method" = c(rep("poorter", times = 4)),
    "compartiment" = c("sw+fw", "ndl", "ag", "bg"), 
    "B_kg_tree" = c(as.data.frame(Poorter_rg_RSR_RLR(ag_GHGI, spp_LHNH, compartiment = "stem"))[1,], 
                    as.data.frame(Poorter_rg_RSR_RLR(ag_GHGI, spp_LHNH, compartiment = "foliage"))[1,], 
                    ag_GHGI + as.data.frame(Poorter_rg_RSR_RLR(ag_GHGI, spp_LHNH, compartiment = "foliage"))[1,], 
                    as.data.frame(Poorter_rg_RSR_RLR(ag_GHGI, spp_LHNH, compartiment = "bg"))[1,]
    )))
  
  wolff_poorter_B_kg_tree <- as.data.frame(cbind(
    "bio_method" = c(rep("wolff+poorter", times = 4)),
    "compartiment" = c("sw+fw", "ndl", "ag", "bg"), 
    "B_kg_tree" = c(as.data.frame(Poorter_rg_RSR_RLR(as.numeric(stem_WOLFF_kg), spp_LHNH, compartiment = "stem"))[1,], 
                    as.data.frame(Poorter_rg_RSR_RLR(as.numeric(stem_WOLFF_kg), spp_LHNH, compartiment = "foliage"))[1,], 
                    as.numeric(stem_WOLFF_kg)+as.numeric(as.data.frame(Poorter_rg_RSR_RLR(as.numeric(stem_WOLFF_kg), spp_LHNH, compartiment = "foliage"))[1,]), 
                    as.data.frame(Poorter_rg_RSR_RLR(as.numeric(stem_WOLFF_kg), spp_LHNH, compartiment = "bg"))[1,]
                    
    )))
  
  wolff_B_kg_tree <- as.data.frame(cbind(
    "bio_method" = c(rep("wolff", times = 3)),
    "compartiment" = c("sw+fw", "ndl", "ag"), 
    "B_kg_tree" = c(as.data.frame(wolff.bio.below.1m(h.cm, spp, compartiment = "stem"))[1,]/1000,    # /1000 to transform from g into kg
                    as.data.frame(wolff.bio.below.1m(h.cm, spp, compartiment = "foliage"))[1,]/1000, # /1000 to transform from g into kg
                    as.data.frame(wolff.bio.below.1m(h.cm, spp, compartiment = "ag"))[1,]/1000       # /1000 to transform from g into kg
    )))
  
  
  B_kg_tree <-  rbind(poorter_B_kg_tree, wolff_poorter_B_kg_tree, wolff_B_kg_tree)
  
  bio.info.df <- as.data.frame(cbind(
    "plot_ID" = c(rep(as.integer(my.plot.id), times = nrow(B_kg_tree))), 
    "CCS_nr" = c(rep(as.integer(my.ccs.id), times = nrow(B_kg_tree))),
    "tree_ID" = c(rep(as.integer(my.tree.id), times = nrow(B_kg_tree))), 
    "inv" = c(rep(unique(HBI.RG.below.1$inv[HBI.RG.below.1$plot_ID==my.plot.id & HBI.RG.below.1$tree_ID==my.tree.id & HBI.RG.below.1$CCS_nr==my.ccs.id]), times = nrow(B_kg_tree))), 
    "inv_year" = c(rep(as.integer(unique(HBI.RG.below.1$inv_year[HBI.RG.below.1$plot_ID==my.plot.id & HBI.RG.below.1$tree_ID==my.tree.id & HBI.RG.below.1$CCS_nr==my.ccs.id])), times = nrow(B_kg_tree))),
    "compartiment" = c(B_kg_tree$compartiment),
    "B_kg_tree" = c(as.numeric(B_kg_tree$B_kg_tree)), 
    "bio_method" = c(B_kg_tree$bio_method)
  )
  )  
  
  poorter.bio.ag.bg.kg.RG.below.1[[i]] <- bio.info.df
  
}
poorter.bio.ag.bg.kg.RG.below.1.df <- as.data.frame(rbindlist(poorter.bio.ag.bg.kg.RG.below.1))
poorter.bio.ag.bg.kg.RG.below.1.df[,c(1,2, 3, 5,7)] <- lapply(poorter.bio.ag.bg.kg.RG.below.1.df[,c(1,2, 3, 5,7)], as.numeric)

summary(poorter.bio.ag.bg.kg.RG.below.1.df %>% filter(bio_method == "wolff+poorter" & compartiment == "sw+fw") %>% select(B_kg_tree))
summary(poorter.bio.ag.bg.kg.RG.below.1.df %>% filter(bio_method == "wolff+poorter" & compartiment == "ndl") %>% select(B_kg_tree))
summary(poorter.bio.ag.bg.kg.RG.below.1.df %>% filter(bio_method == "wolff+poorter" & compartiment == "ag") %>% select(B_kg_tree))

# 3.2.2  differences in RG compartimentition ----------------------------------------------
# 3.2.2.1. statistical characteristics of difference between Poorter bg options -------------------
RG_data_poorter_x_comparisson <- HBI.RG.below.1 %>% 
  mutate(stem_kg = wolff.bio.below.1m(H_cm, RG_Wolff_bio, compartiment = "stem")/1000,
         bg_kg = Poorter_rg_RSR_RLR(wolff.bio.below.1m(H_cm, RG_Wolff_bio, compartiment = "stem"), LH_NH, compartiment = "bg"), 
         x1 = Poorter_rg_RSR_RLR(wolff.bio.below.1m(H_cm, RG_Wolff_bio, compartiment = "stem")/1000, LH_NH, compartiment = "x1"),
         x2 = Poorter_rg_RSR_RLR(wolff.bio.below.1m(H_cm, RG_Wolff_bio, compartiment = "stem")/1000, LH_NH, compartiment = "x2"),
         x_chhosen = ifelse(x1 == bg_kg, "x1", "x2"),
         diff_x = x1 - x2)

summary(RG_data_poorter_x_comparisson)

mean(RG_data_poorter_x_comparisson$diff_x)
cv <- sd(RG_data_poorter_x_comparisson$diff_x) / mean(RG_data_poorter_x_comparisson$diff_x) * 100

# 3.1.2. tests for significant differences ----------------------------------------------
# diffrences between poorter compartiments based on wolff input mass and 
# test if biomass is normally distributed to then compare 
shapiro.test(as.numeric(poorter.bio.ag.bg.kg.RG.below.1.df$B_kg_tree[poorter.bio.ag.bg.kg.RG.below.1.df$bio_method == "wolff"][1:5000]))
# result: p-value < 2.2e-16
# From the output, the p-value > 0.05 implying that the distribution of the data are not significantly different from normal distribution.
# In other words, we can assume the normality.
shapiro.test(as.numeric(poorter.bio.ag.bg.kg.RG.below.1.df$B_kg_tree[poorter.bio.ag.bg.kg.RG.below.1.df$bio_method == "wolff+poorter"][1:5000]))
# result: p-value < 2.2e-16
# From the output, the p-value > 0.05 implying that the distribution of the data are not significantly different from normal distribution.
# In other words, we can assume the normality.
shapiro.test(as.numeric(poorter.bio.ag.bg.kg.RG.below.1.df$B_kg_tree[poorter.bio.ag.bg.kg.RG.below.1.df$bio_method == "poorter"][1:5000]))
# result: p-value < 2.2e-16
# From the output, the p-value > 0.05 implying that the distribution of the data are not significantly different from normal distribution.
# In other words, we can assume the normality.

t.test(as.numeric(poorter.bio.ag.bg.kg.RG.below.1.df$B_kg_tree[poorter.bio.ag.bg.kg.RG.below.1.df$bio_method == "wolff" & poorter.bio.ag.bg.kg.RG.below.1.df$compartiment != "bg"]), 
       as.numeric(poorter.bio.ag.bg.kg.RG.below.1.df$B_kg_tree[poorter.bio.ag.bg.kg.RG.below.1.df$bio_method == "wolff+poorter"]))
# result: 
# data:  as.numeric(poorter.bio.ag.bg.kg.RG.below.1.df$B_kg_tree[poorter.bio.ag.bg.kg.RG.below.1.df$bio_method == "poorter" & poorter.bio.ag.bg.kg.RG.below.1.df$compartiment != "bg"]) and as.numeric(poorter.bio.ag.bg.kg.RG.below.1.df$B_kg_tree[poorter.bio.ag.bg.kg.RG.below.1.df$bio_method == "wolff+poorter"])
# t = -26.655, df = 10131, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -6.672096e+60 -5.757997e+60
# sample estimates:
#   mean of x    mean of y 
# 8.448797e-03 6.215046e+60 
## there is a significant difference in the compartiments biomass


wilcox.test(as.numeric(poorter.bio.ag.bg.kg.RG.below.1.df$B_kg_tree[poorter.bio.ag.bg.kg.RG.below.1.df$bio_method == "wolff" & poorter.bio.ag.bg.kg.RG.below.1.df$compartiment != "bg"]), 
            as.numeric(poorter.bio.ag.bg.kg.RG.below.1.df$B_kg_tree[poorter.bio.ag.bg.kg.RG.below.1.df$bio_method == "wolff+poorter"]))
# Wilcoxon rank sum test with continuity correction
# 
# data:  as.numeric(poorter.bio.ag.bg.kg.RG.below.1.df$B_kg_tree[poorter.bio.ag.bg.kg.RG.below.1.df$bio_method == "wolff" & poorter.bio.ag.bg.kg.RG.below.1.df$compartiment != "bg"]) and as.numeric(poorter.bio.ag.bg.kg.RG.below.1.df$B_kg_tree[poorter.bio.ag.bg.kg.RG.below.1.df$bio_method == "wolff+poorter"])
# W = 50512647, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0
## there is a significant difference in the compartiments biomass



# visualizing biomass comparisson < 1.3m height poorter vs. Wolff -----------------------
for (i in 1:nrow(unique(poorter.bio.ag.bg.kg.RG.below.1.df [,c("plot_ID", "CCS_nr", "tree_ID")]))){
  # i=1
  my.plot.id <- unique(poorter.bio.ag.bg.kg.RG.below.1.df[, c("plot_ID", "CCS_nr", "tree_ID")])[,"plot_ID"][i]
  my.ccs.id <- unique(poorter.bio.ag.bg.kg.RG.below.1.df[, c("plot_ID", "CCS_nr", "tree_ID")])[,"CCS_nr"][i]
  my.tree.id <- unique(poorter.bio.ag.bg.kg.RG.below.1.df[, c("plot_ID", "CCS_nr", "tree_ID")])[,"tree_ID"][i]
  
  print(ggplot()+ 
          geom_bar(data = (poorter.bio.ag.bg.kg.RG.below.1.df  %>% 
                             filter(tree_ID == my.tree.id, plot_ID == my.plot.id, CCS_nr == my.ccs.id)) , 
                   aes(x = compartiment, y = as.numeric(B_kg_tree), fill = bio_method), 
                   stat="identity", position = "dodge")+ 
          ggtitle(paste0(my.plot.id, ",", my.ccs.id,",", my.tree.id)))
}











# NOTES -------------------------------------------------------------------

# N.tapes biomass > 1.3m height-------------------------------------------------------
# N.1.2.1.1. aboveground biomass for RG trees height > 1.3m -------------------------------
# create output list
bio.ag.kg.RG.above.1.3 <- vector("list", length = nrow(RG_above_1.3))
for (i in 1:nrow(RG_above_1.3)) {
  # i = 13
  
  # basic tree info
  # select one tree ID and plot ID for each individual tree per plot through unique(trees[, c("plot_ID", "tree_ID")])
  my.plot.id <- RG_above_1.3[,"plot_ID"][i]
  my.ccs.id <- RG_above_1.3[,"CCS_nr"][i]
  my.tree.id <- RG_above_1.3[,"tree_ID"][i]
  my.tree.sp <- RG_above_1.3[,"SP_code"][i]
  BL.or.CF <-  SP_names_com_ID_tapeS$LH_NH[tolower(SP_names_com_ID_tapeS$Chr_code_ger) == my.tree.sp]
  
  
  # select variales for tree object: tapes species, diameter, diameter measuring height, tree height
  spp = SP_names_com_ID_tapeS$tpS_ID[tolower(SP_names_com_ID_tapeS$Chr_code_ger) == my.tree.sp]
  Dm = na.omit(as.list(as.numeric(unique(RG_above_1.3$D_cm[RG_above_1.3$plot_ID==my.plot.id & RG_above_1.3$tree_ID==my.tree.id & RG_above_1.3$CCS_nr==my.ccs.id])))) 
  Hm = na.omit(as.list(as.numeric(1.3)))
  Ht = na.omit(as.numeric(unique(RG_above_1.3$H_m[RG_above_1.3$plot_ID==my.plot.id & RG_above_1.3$tree_ID==my.tree.id & RG_above_1.3$CCS_nr==my.ccs.id])))
  # create tapes compartiments
  comp <- as.character(c("stw","stb","sw", "sb", "fwb", "ndl"))
  
  # create object  
  obj.trees <- tprTrees(spp, Dm, Hm, Ht, inv = 4)
  
  # calculate biomass per compartiment
  bio.df <- as.data.frame(tprBiomass(obj = obj.trees, component = comp, mono =T)) %>% 
    pivot_longer(cols = stw:ndl,
                 names_to = "compartiment", 
                 values_to = "B_kg_tree")
  
  
  bio.info.df <- as.data.frame(cbind(
    "plot_ID" = c(as.integer(RG_above_1.3$plot_ID[RG_above_1.3$plot_ID == my.plot.id & RG_above_1.3$tree_ID == my.tree.id & RG_above_1.3$CCS_nr==my.ccs.id])), 
    "CCS_nr" = c(my.ccs.id),
    "tree_ID" = c(as.integer(RG_above_1.3$tree_ID[RG_above_1.3$plot_ID == my.plot.id & RG_above_1.3$tree_ID == my.tree.id & RG_above_1.3$CCS_nr==my.ccs.id])), 
    "inv" = c(RG_above_1.3$inv[RG_above_1.3$plot_ID == my.plot.id & RG_above_1.3$tree_ID == my.tree.id & RG_above_1.3$CCS_nr==my.ccs.id]), 
    "inv_year" = c(as.integer(RG_above_1.3$inv_year[RG_above_1.3$plot_ID == my.plot.id & RG_above_1.3$tree_ID == my.tree.id & RG_above_1.3$CCS_nr==my.ccs.id])),
    "LH_NH" = c(RG_above_1.3$LH_NH[RG_above_1.3$plot_ID == my.plot.id & RG_above_1.3$tree_ID == my.tree.id & RG_above_1.3$CCS_nr==my.ccs.id]),
    "compartiment" = c(bio.df$compartiment),
    "B_kg_tree" = c(as.numeric(bio.df$B_kg_tree))
  ) ) %>% 
    # if the tree is a broadleafed tree Tapes cannot calculate the foliage mass, 
    # thus we calculate this subsequently trough the biomass function by Wutzler (2008)
    mutate(B_kg_tree = ifelse(compartiment == "ndl" & LH_NH == "LB", 
                              Wutzler_fB_L1(as.numeric(Dm), as.numeric(Ht)),
                              B_kg_tree)) %>% 
    dplyr::select(-c("LH_NH"))
  
  bio.ag.kg.RG.above.1.3[[i]] <- bio.info.df
  
}
bio.ag.kg.RG.above.1.3.df <- as.data.frame(rbindlist(bio.ag.kg.RG.above.1.3))
bio.ag.kg.RG.above.1.3.df[,c(1,2, 3, 5,7)] <- lapply(bio.ag.kg.RG.above.1.3.df[,c(1,2, 3, 5,7)], as.numeric)



# N. one to conquer them all ----------------------------------------------
RG_data %>% left_join(., 
                      rbind(
                        # join in aboegrond compartiments and belowground compartients in tree dataset 
                        bio.ag.kg.RG.above.1.3.df, 
                        bio.bg.kg.RG.above.1.3.df,
                        # 1.2.1.3. total and total aboveground biomass for RG trees height > 1.3m -------------------------------
                        # calculate total biomass (aboveground + belowground) by summing up biomass in kg per tree in all compartiments
                        rbind(
                          bio.ag.kg.RG.above.1.3.df, bio.bg.kg.RG.above.1.3.df) %>% 
                          group_by(plot_ID, CCS_nr, tree_ID, inv, inv_year) %>% 
                          summarize(B_kg_tree = sum(as.numeric(B_kg_tree))) %>% 
                          mutate(compartiment = "total") %>% 
                          select("plot_ID", "CCS_nr", "tree_ID", "inv", 
                                 "inv_year", "compartiment", "B_kg_tree"),
                        # calculate total aboveground biomass by summing up biomass in kg per tree in all aboveground compartiments
                        bio.ag.kg.RG.above.1.3.df%>% 
                          group_by(plot_ID, CCS_nr, tree_ID, inv, inv_year) %>% 
                          summarize(B_kg_tree = sum(as.numeric(B_kg_tree))) %>% 
                          mutate(compartiment = "ag")%>% 
                          select("plot_ID","CCS_nr", "tree_ID", "inv", 
                                 "inv_year", "compartiment", "B_kg_tree"), 
                        # 1.2.2. biomass for RG trees height < 1.3m -------------------------------
                        # 1.2.1.1. aboveground biomass for RG trees height > 1.3m -------------------------------
                        # 1.2.1.1.1. GHGI aboveground biomass for RG trees height > 1.3m -------------------------------
                        (RG_data %>% 
                           filter(H_m <= 1.3) %>% 
                           mutate(compartiment = "ag", 
                                  B_kg_tree = GHGI_aB_Hb1.3(LH_NH, H_m)) %>% 
                           select("plot_ID","CCS_nr", "tree_ID", "inv", "inv_year", "compartiment", "B_kg_tree"))), # close rbind  for all compartiments and trees
                      by = c("plot_ID", "CCS_nr", "tree_ID", "inv", "inv_year"), 
                      multiple = "all") # close left join in RG_data