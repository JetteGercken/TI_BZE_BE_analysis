# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the  national soil inventory
# stock calculations for regeneration 


# ----- 0. SETUP ---------------------------------------------------------------

# ----- 0.1. packages and functions --------------------------------------------
source(paste0(getwd(), "/scripts/00_00_functions_library.R"))

# ----- 0.2. working directory -------------------------------------------------
here::here()
getwd()

out.path.BZE3 <- ("output/out_data/out_data_BZE/") 


# ----- 0.3 data import --------------------------------------------------------
# regeneration
# this dataset contains the plant specific inventory data of the regenertaion inventory of the HBI (BZE2), including stand and area info
HBI_RG <- read.delim(file = here("data/input/BZE2_HBI/bejb.csv"), sep = ",", dec = ",")%>% mutate(inv_year = 2012, inv = inv_name(inv_year)) 
#  "bund_nr"  "pk_nr"  "lfd_nr"   "bart"  "hoehe"    "grklasse"
colnames(HBI_RG) <- c("plot_ID", "CCS_no", "tree_ID", "SP_code", "H_cm", "D_class_cm", "inv_year", "inv")

# this dataset contains the position and extend of the sampling circle satelites of the regeneration inventory of the HBI (BZE2) including stand and area info
HBI_RG_loc <- read.delim(file = here(paste0(out.path.BZE3, "HBI_RG_loc_update_1.csv")), sep = ",", dec = ",") 
SP_names_com_ID_tapeS <- read.delim(file = here("output/out_data/x_bart_tapeS.csv"), sep = ",", dec = ",") 


# 1. calculations ---------------------------------------------------------

# 1.1. size class to diameter ---------------------------------------------
# translate size class into diameter
HBI_RG <- HBI_RG %>% 
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
HBI.RG.above.1.3 <- HBI_RG[HBI_RG$H_m > 1.3, ]
# 1.2.1.1. aboveground biomass for RG trees height > 1.3m -------------------------------
# create output list
bio.ag.kg.RG.above.1.3 <- vector("list", length = nrow(HBI.RG.above.1.3))
for (i in 1:nrow(HBI.RG.above.1.3)) {
  # i = 1
  
  # basic tree info
  # select one tree ID and plot ID for each individual tree per plot through unique(trees[, c("plot_ID", "tree_ID")])
  my.plot.id <- HBI.RG.above.1.3[,"plot_ID"][i]
  my.ccs.id <- HBI.RG.above.1.3[,"CCS_no"][i]
  my.tree.id <- HBI.RG.above.1.3[,"tree_ID"][i]
  BL.or.CF <- unique(HBI.RG.above.1.3$LH_NH[HBI.RG.above.1.3$plot_ID==my.plot.id & HBI.RG.above.1.3$tree_ID==my.tree.id & HBI.RG.above.1.3$CCS_no==my.ccs.id])
  
  # select variales for tree object: tapes species, diameter, diameter measuring height, tree height
  spp = na.omit(unique(HBI.RG.above.1.3$tpS_ID[HBI.RG.above.1.3$plot_ID==my.plot.id & HBI.RG.above.1.3$tree_ID==my.tree.id & HBI.RG.above.1.3$CCS_no==my.ccs.id]))
  Dm = na.omit(as.list(as.numeric(unique(HBI.RG.above.1.3$D_cm[HBI.RG.above.1.3$plot_ID==my.plot.id & HBI.RG.above.1.3$tree_ID==my.tree.id & HBI.RG.above.1.3$CCS_no==my.ccs.id])))) 
  Hm = na.omit(as.list(as.numeric(1.3)))
  Ht = na.omit(as.numeric(unique(HBI.RG.above.1.3$H_m[HBI.RG.above.1.3$plot_ID==my.plot.id & HBI.RG.above.1.3$tree_ID==my.tree.id & HBI.RG.above.1.3$CCS_no==my.ccs.id])))
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
    "plot_ID" = c(as.integer(HBI.RG.above.1.3$plot_ID[HBI.RG.above.1.3$plot_ID == my.plot.id & HBI.RG.above.1.3$tree_ID == my.tree.id & HBI.RG.above.1.3$CCS_no==my.ccs.id])), 
    "CCS_no" = c(my.ccs.id),
    "tree_ID" = c(as.integer(HBI.RG.above.1.3$tree_ID[HBI.RG.above.1.3$plot_ID == my.plot.id & HBI.RG.above.1.3$tree_ID == my.tree.id & HBI.RG.above.1.3$CCS_no==my.ccs.id])), 
    "inv" = c(HBI.RG.above.1.3$inv[HBI.RG.above.1.3$plot_ID == my.plot.id & HBI.RG.above.1.3$tree_ID == my.tree.id & HBI.RG.above.1.3$CCS_no==my.ccs.id]), 
    "inv_year" = c(as.integer(HBI.RG.above.1.3$inv_year[HBI.RG.above.1.3$plot_ID == my.plot.id & HBI.RG.above.1.3$tree_ID == my.tree.id & HBI.RG.above.1.3$CCS_no==my.ccs.id])),
    "LH_NH" = c(HBI.RG.above.1.3$LH_NH[HBI.RG.above.1.3$plot_ID == my.plot.id & HBI.RG.above.1.3$tree_ID == my.tree.id & HBI.RG.above.1.3$CCS_no==my.ccs.id]),
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

  
# 1.2.1.1. aboveground biomass for RG trees height > 1.3m -------------------------------
bio.bg.kg.RG.above.1.3 <- vector("list", length = nrow(HBI.RG.above.1.3))
for (i in 1:nrow(HBI.RG.above.1.3)) {
  # i = 1
  
  # basic tree info
  # select one tree ID and plot ID for each individual tree per plot and sampling circuit
  my.plot.id <- HBI.RG.above.1.3[,"plot_ID"][i]
  my.ccs.id <- HBI.RG.above.1.3[,"CCS_no"][i]
  my.tree.id <- HBI.RG.above.1.3[,"tree_ID"][i]
  BL.or.CF <- unique(HBI.RG.above.1.3$LH_NH[HBI.RG.above.1.3$plot_ID==my.plot.id & HBI.RG.above.1.3$tree_ID==my.tree.id & HBI.RG.above.1.3$CCS_no==my.ccs.id])
  
  # select variales for belowground functions
  spp = unique(HBI.RG.above.1.3$Bio_SP_group[HBI.RG.above.1.3$plot_ID==my.plot.id & HBI.RG.above.1.3$tree_ID==my.tree.id & HBI.RG.above.1.3$CCS_no==my.ccs.id])
  dbh.cm = as.numeric(unique(HBI.RG.above.1.3$D_cm[HBI.RG.above.1.3$plot_ID==my.plot.id & HBI.RG.above.1.3$tree_ID==my.tree.id & HBI.RG.above.1.3$CCS_no==my.ccs.id]))
  
  
  # calculate biomass per compartiment
  B_kg_tree <- as.data.frame(GHGI_bB(spp, dbh.cm))[,1]
  
  bio.info.df <- as.data.frame(cbind(
    "plot_ID" = c(as.integer(my.plot.id)), 
    "CCS_no" = c(my.ccs.id),
    "tree_ID" = c(as.integer(my.tree.id)), 
    "inv" = unique(HBI.RG.above.1.3$inv[HBI.RG.above.1.3$plot_ID==my.plot.id & HBI.RG.above.1.3$tree_ID==my.tree.id]), 
    "inv_year" = c(as.integer(unique(HBI.RG.above.1.3$inv_year[HBI.RG.above.1.3$plot_ID==my.plot.id & HBI.RG.above.1.3$tree_ID==my.tree.id]))),
    "compartiment" = c("bg"),
    "B_kg_tree" = c(as.numeric(B_kg_tree))
  ) ) 
  
  bio.bg.kg.RG.above.1.3[[i]] <- bio.info.df
  
}
bio.bg.kg.RG.above.1.3.df <- as.data.frame(rbindlist(bio.bg.kg.RG.above.1.3))



# 1.2.1.1. total and total aboveground biomass for RG trees height > 1.3m -------------------------------
bio.total.kg.RG.df <- 
  rbind(
    # calculate total biomass (aboveground + belowground) by summing up biomass in kg per tree in all compartiments
    rbind(
      bio.ag.kg.RG.above.1.3.df, bio.bg.kg.RG.above.1.3.df) %>% 
      group_by(plot_ID, CCS_no, tree_ID, inv, inv_year) %>% 
      summarize(B_kg_tree = sum(as.numeric(B_kg_tree))) %>% 
      mutate(compartiment = "total") %>% 
      select("plot_ID", "CCS_no", "tree_ID", "inv", 
             "inv_year", "compartiment", "B_kg_tree"),
    # calculate total aboveground biomass by summing up biomass in kg per tree in all aboveground compartiments
    bio.ag.kg.RG.above.1.3.df%>% 
      group_by(plot_ID, CCS_no, tree_ID, inv, inv_year) %>% 
      summarize(B_kg_tree = sum(as.numeric(B_kg_tree))) %>% 
      mutate(compartiment = "ag")%>% 
      select("plot_ID","CCS_no", "tree_ID", "inv", 
             "inv_year", "compartiment", "B_kg_tree"))




# 1.2.2. biomass for RG trees height < 1.3m -------------------------------
# subset those trees that have a height above 1.3m and thus a DBH which allows them to be processed in TapeS
HBI.RG.below.1 <- HBI_RG[HBI_RG$H_m < 1, ]
# 1.2.1.1. aboveground biomass for RG trees height > 1.3m -------------------------------
# estimate shoot-base-diameter through height of tree
# esitmate Biomasse in compartiments 

bio.ag.kg.RG.below.1 <- vector("list", length = nrow(HBI.RG.below.1))
for (i in 1:nrow(HBI.RG.below.1)) {
  # i = 1
  
  # basic tree info
  # select one tree ID and plot ID for each individual tree per plot and sampling circuit
  my.plot.id <- HBI.RG.below.1[,"plot_ID"][i]
  my.ccs.id <- HBI.RG.below.1[,"CCS_no"][i]
  my.tree.id <- HBI.RG.below.1[,"tree_ID"][i]
  BL.or.CF <- unique(HBI.RG.below.1$LH_NH[HBI.RG.below.1$plot_ID==my.plot.id & HBI.RG.below.1$tree_ID==my.tree.id & HBI.RG.below.1$CCS_no==my.ccs.id])
  
  # select variales for belowground functions
  spp = unique(HBI.RG.below.1$RG_Wolff_bio[HBI.RG.below.1$plot_ID==my.plot.id & HBI.RG.below.1$tree_ID==my.tree.id & HBI.RG.below.1$CCS_no==my.ccs.id])
  h.cm = as.numeric(unique(HBI.RG.below.1$H_cm[HBI.RG.below.1$plot_ID==my.plot.id & HBI.RG.below.1$tree_ID==my.tree.id & HBI.RG.below.1$CCS_no==my.ccs.id]))
  whd.cm = as.numeric(h.to.whd(h.cm, spp))
  
  
  # calculate biomass per compartiment
  B_kg_tree <- as.data.frame(cbind(
    "compartiment" = c("sw+fw", "ndl", "ag"), 
    "B_kg_tree" = c(as.data.frame(wolff.bio.below.1m(whd.cm, h.cm, spp, compartiment = "stem"))[1,], 
                    as.data.frame(wolff.bio.below.1m(whd.cm, h.cm, spp, compartiment = "foliage"))[1,], 
                    as.data.frame(wolff.bio.below.1m(whd.cm, h.cm, spp, compartiment = "ag"))[1,]
                    )))

  
  bio.info.df <- as.data.frame(cbind(
    "plot_ID" = c(as.integer(my.plot.id)), 
    "tree_ID" = c(as.integer(my.tree.id)), 
    "inv" = unique(HBI.RG.below.1$inv[HBI.RG.below.1$plot_ID==my.plot.id & HBI.RG.below.1$tree_ID==my.tree.id & HBI.RG.below.1$CCS_no==my.ccs.id]), 
    "inv_year" = c(as.integer(unique(HBI.RG.below.1$inv_year[HBI.RG.below.1$plot_ID==my.plot.id & HBI.RG.below.1$tree_ID==my.tree.id & HBI.RG.below.1$CCS_no==my.ccs.id]))),
    "compartiment" = c(B_kg_tree$compartiment),
    "B_kg_tree" = c(as.numeric(B_kg_tree$B_kg_tree))
    )
  )  
  
  bio.ag.kg.RG.below.1[[i]] <- bio.info.df
  
}
bio.ag.kg.RG.below.1.df <- as.data.frame(rbindlist(bio.ag.kg.RG.below.1))





poorter.bio.ag.bg.kg.RG.below.1 <- vector("list", length = nrow(HBI.RG.below.1))
for (i in 1:nrow(HBI.RG.below.1)) {
  # i = 1
  
  # basic tree info
  # select one tree ID and plot ID for each individual tree per plot and sampling circuit
  my.plot.id <- HBI.RG.below.1[,"plot_ID"][i]
  my.ccs.id <- HBI.RG.below.1[,"CCS_no"][i]
  my.tree.id <- HBI.RG.below.1[,"tree_ID"][i]
  
  # select varibales for aboveground functions & calcualte aboveground biomass as input for Poorter 
  # nationla greenhousegas inventory function for trees below 1.3m
  spp_LHNH = unique(HBI.RG.below.1$LH_NH[HBI.RG.below.1$plot_ID==my.plot.id & HBI.RG.below.1$tree_ID==my.tree.id & HBI.RG.below.1$CCS_no==my.ccs.id])
  h.m = as.numeric(unique(HBI.RG.below.1$H_cm[HBI.RG.below.1$plot_ID==my.plot.id & HBI.RG.below.1$tree_ID==my.tree.id & HBI.RG.below.1$CCS_no==my.ccs.id]))/100
  ag_GHGI = as.data.frame(GHGI_aB_Hb1.3(spp_LHNH, h.m))[1,]
  # Wolff et al. function for aboveground biomass for trees below 1m
  spp = unique(HBI.RG.below.1$RG_Wolff_bio[HBI.RG.below.1$plot_ID==my.plot.id & HBI.RG.below.1$tree_ID==my.tree.id & HBI.RG.below.1$CCS_no==my.ccs.id])
  h.cm = as.numeric(unique(HBI.RG.below.1$H_cm[HBI.RG.below.1$plot_ID==my.plot.id & HBI.RG.below.1$tree_ID==my.tree.id & HBI.RG.below.1$CCS_no==my.ccs.id]))
  whd.cm = as.numeric(h.to.whd(h.cm, spp))
  ag_WOLFF = as.data.frame(wolff.bio.below.1m(whd.cm, h.cm, spp, compartiment = "ag"))[1,]
  
  # calculate biomass per compartiment
  poorter_B_kg_tree <- as.data.frame(cbind(
    "bio_method" = c(rep("poorter", times = 4)),
    "compartiment" = c("sw+fw", "ndl", "ag", "bg"), 
    "B_kg_tree" = c(as.data.frame(Poorter_rg_RSR_RLR(ag_GHGI, spp_LHNH, compartiment = "stem"))[1,], 
                    as.data.frame(Poorter_rg_RSR_RLR(ag_GHGI, spp_LHNH, compartiment = "foliage"))[1,], 
                    ag_GHGI, 
                    as.data.frame(Poorter_rg_RSR_RLR(ag_GHGI, spp_LHNH, compartiment = "bg"))[1,]
                    )))
  
  wolff_B_kg_tree <- as.data.frame(cbind(
    "bio_method" = c(rep("wolff", times = 4)),
    "compartiment" = c("sw+fw", "ndl", "ag", "bg"), 
    "B_kg_tree" = c(as.data.frame(Poorter_rg_RSR_RLR(as.numeric(ag_WOLFF), spp_LHNH, compartiment = "stem"))[1,], 
                    as.data.frame(Poorter_rg_RSR_RLR(as.numeric(ag_WOLFF), spp_LHNH, compartiment = "foliage"))[1,], 
                    as.numeric(ag_WOLFF), 
                    as.data.frame(Poorter_rg_RSR_RLR(as.numeric(ag_WOLFF), spp_LHNH, compartiment = "bg"))[1,]
                    )))
  
  B_kg_tree <-  rbind(poorter_B_kg_tree, wolff_B_kg_tree)
  
  bio.info.df <- as.data.frame(cbind(
    "plot_ID" = c(rep(as.integer(my.plot.id), times = nrow(B_kg_tree))), 
    "tree_ID" = c(rep(as.integer(my.tree.id), times = nrow(B_kg_tree))), 
    "inv" = c(rep(unique(HBI.RG.below.1$inv[HBI.RG.below.1$plot_ID==my.plot.id & HBI.RG.below.1$tree_ID==my.tree.id & HBI.RG.below.1$CCS_no==my.ccs.id]), times = nrow(B_kg_tree))), 
    "inv_year" = c(rep(as.integer(unique(HBI.RG.below.1$inv_year[HBI.RG.below.1$plot_ID==my.plot.id & HBI.RG.below.1$tree_ID==my.tree.id & HBI.RG.below.1$CCS_no==my.ccs.id])), times = nrow(B_kg_tree))),
    "compartiment" = c(B_kg_tree$compartiment),
    "B_kg_tree" = c(as.numeric(B_kg_tree$B_kg_tree))
  )
  )  
  
  poorter.bio.ag.bg.kg.RG.below.1[[i]] <- bio.info.df
  
}
poorter.bio.ag.bg.kg.RG.below.1.df <- as.data.frame(rbindlist(poorter.bio.ag.bg.kg.RG.below.1))




# visualizing biomass comparisson poorter vs. Wolff -----------------------
ggplot()+ 
  geom_bar(data = (bio.ag.kg.RG.below.1.df %>% mutate(bio_method = "wolff"))[1:3,], 
           aes(x = compartiment, y = as.numeric(B_kg_tree), fill = bio_method), 
           stat="identity", position = "dodge")
ggplot()+ 
  geom_bar(data = (poorter.bio.ag.bg.kg.RG.below.1.df %>% mutate(bio_method = "poorter"))[1:4,], 
           aes(x = compartiment, y = as.numeric(B_kg_tree), fill = bio_method), 
           stat="identity", position = "dodge")


