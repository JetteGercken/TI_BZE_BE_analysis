# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the peat land soil inventory
# Deadwood biomass, carbon and nitrogen stock


# ----- 0. SETUP ---------------------------------------------------------------
# ----- 0.1. Packages & functions  ---------------------------------------------------------
source(paste0(getwd(), "/scripts/00_00_functions_library.R"))

# ----- 0.2. working directory -------------------------------------------------
here::here()
getwd()

out.path.BZE3 <- ("output/out_data/out_data_BZE/") 

# ----- 0.3 data import --------------------------------------------------------
# DEAD trees
HBI_DW <- read.delim(file = here("data/input/BZE2_HBI/bedw_liste.csv"), sep = ",", dec = ",")
                    #  bund_nr lfd_nr t     yp      baumgruppe anzahl  durchmesser laenge zersetzung
colnames(HBI_DW) <- c("plot_ID", "tree_ID", "dw_type", "dw_sp", "count", "d_cm", "l_dm", "decay")

# HBI point info
HBI_inv_info <- read.delim(file = here("data/input/BZE2_HBI/tit_1.csv"), sep = ",", dec = ",", stringsAsFactors=FALSE)
# HBI point/ inventory info
HBI_inv_info <- HBI_inv_info %>% dplyr::select(bund_nr, datum, status )
colnames(HBI_inv_info) <- c("plot_ID", "date", "plot_inventory_status")

# HBI forest type info per plot  (Bestandestyp)
# this i deed to later say "if the stocking species are mainly coniferous i need this secies group from tapeS
# and if th estocking species fall in the category broadleafes the other tapes species code"
HBI_forest_info <- read.delim(file = here("data/input/BZE2_HBI/be.csv"), sep = ",", dec = ",", stringsAsFactors=FALSE)





# 0.4 dataprep  -----------------------------------------------------------

# 0.4.1. Inventory year & name --------------------------------------------------------
# create column that just contains year of inventory: https://www.geeksforgeeks.org/how-to-extract-year-from-date-in-r/
HBI_inv_info$date <- as.Date(HBI_inv_info$date)
HBI_inv_info$inv_year <- as.numeric(format(HBI_inv_info$date, "%Y"))
# this line can be removed later
HBI_inv_info <- HBI_inv_info %>% mutate(inv_year = ifelse(inv_year < 2012, 2012,inv_year), 
                                        inv = inv_name(inv_year))

# join inventory jear and name into deadwood tree dataset
HBI_DW <- HBI_DW %>% left_join(., HBI_inv_info %>% select(inv_year, inv, plot_ID), by = "plot_ID")

# 0.4.2. species names -------------------------------------------------------------
# 1 Nadelholz
# 2 Laubholz (außer Eiche)
# 3 Eiche
# 4 unbekannt

HBI_DW <- HBI_DW %>% left_join(., 
                     HBI_forest_info %>% 
                       mutate(LH_NH = case_when(besttyp %in% c(4, 5, 7, 8, 10, 91 ) ~ "LB", 
                                                besttyp %in% c(92, 1, 2, 3, 6, 9 ) ~ "NB", 
                                                TRUE ~ NA)) %>% 
                       select(bund_nr, LH_NH), 
                     by = c("plot_ID" = "bund_nr")) %>% 
  mutate(tapes_ID =  case_when(dw_sp == 1 | (dw_sp == 4 & LH_NH == "NB") ~ 1,  # Fi
                             dw_sp == 2 | (dw_sp == 4 & LH_NH == "LB") ~ 15, # BU
                             dw_sp == 3 ~ 17,                                   # EI   
                             TRUE ~ NA) )
# 1. calculations ---------------------------------------------------------------
# 1 liegend; starkes Totholz; umfasst Stamm, Äste, Zweige,  abgebrochene Kronen, D ≥ 10 cm am dickeren Ende
# 2 stehend, ganzer Baum; stehendes Totholz mit Ästen BHD ≥ 10 cm
# 3 stehend, Bruchstück; Baumstumpf ohne Äste BHD ≥ 10 cm, Höhe ≥ 13 dm
# 4 Wurzelstock Ø Schnittflächendurchmesser ≥ 10 cm, Höhe < 13 dm
# 5 liegend; ganzer Baum BHD ≥ 10 cm
# 6 in Haufen vorkommendes Totholz D ≥ 10 cm am dickeren Ende


# 1.3 biomass -------------------------------------------------------------------------------

# 1.3.1 biomass whole deadwood trees (ganzer Baum stehend 2/ liegend 5) ------------------------------------------------------------------------
# for whole standing or laying deadwood trees all compartiments except foliage ("ndl" ) are calculated via TapeS
HBW_DW_whole <- HBI_DW[HBI_DW$dw_type %in% c(2, 5), ]
# export list for biomasse
bio.dw.whole.kg.list <- vector("list", length = nrow(HBW_DW_whole))
# export list for volume
for (i in 1:nrow(HBW_DW_whole)){
  # i = 1
  
  # select general info about the DW item
  my.plot.id <- HBW_DW_whole[,"plot_ID"][i]
  my.tree.id <- HBW_DW_whole[,"tree_ID"][i]
  my.decay.type <- HBW_DW_whole[,"decay"][i]
  my.dw.spec <- HBW_DW_whole[,"dw_sp"][i]
  my.CF.BL <- HBW_DW_whole[,"LH_NH"][i]
  
  # select variables fot TprTrees object
  # translating Species groups into TapeS codes
  spp =  na.omit(as.numeric(unique(HBW_DW_whole$tapes_ID[HBW_DW_whole$plot_ID==my.plot.id & HBW_DW_whole$tree_ID==my.tree.id]))) 
  Dm = na.omit(as.list(as.numeric(unique(HBW_DW_whole$d_cm[HBW_DW_whole$plot_ID==my.plot.id & HBW_DW_whole$tree_ID==my.tree.id])))) # diameter in cm
  Hm = as.list(as.numeric(1.3))
  Ht = na.omit(as.numeric(unique(HBW_DW_whole$l_dm[HBW_DW_whole$plot_ID==my.plot.id & HBW_DW_whole$tree_ID==my.tree.id]))/10) # lenth in meter m
  # create tapes compartiments
  comp <- as.character(c("stw","stb","sw", "sb", "fwb"))
  
  # create object  
  obj.dw <- tprTrees(spp, Dm, Hm, Ht, inv = 4)

  # calculate biomass
  bio.df <- as.data.frame(tprBiomass(obj = obj.dw[obj.dw@monotone == TRUE], component = comp)) %>% 
    pivot_longer(cols = stw:fwb,
                 names_to = "compartiment", 
                 values_to = "B_kg_tree") %>% 
          # apply the biomass reduction factor to the biomass of deadwoodto account for decay state
    mutate(B_kg_tree = rdB_DW(B_kg_tree, paste0(my.decay.type, "_", my.dw.spec)))
  
  # create export dataframe
  bio.info.df <- as.data.frame(cbind(
    "plot_ID" = c(as.integer(HBW_DW_whole$plot_ID[HBW_DW_whole$plot_ID == my.plot.id & HBW_DW_whole$tree_ID == my.tree.id])), 
    "tree_ID" = c(as.integer(HBW_DW_whole$tree_ID[HBW_DW_whole$plot_ID == my.plot.id & HBW_DW_whole$tree_ID == my.tree.id])), 
    "inv" = c(HBW_DW_whole$inv[HBW_DW_whole$plot_ID == my.plot.id & HBW_DW_whole$tree_ID == my.tree.id]), 
    "inv_year" = c(as.integer(HBW_DW_whole$inv_year[HBW_DW_whole$plot_ID == my.plot.id & HBW_DW_whole$tree_ID == my.tree.id])),
    "compartiment" = c(bio.df$compartiment),
    "B_kg_tree" = c(as.numeric(bio.df$B_kg_tree))
  ))
  
  bio.dw.whole.kg.list[[i]] <- bio.info.df

}
bio_dw_whole_kg.df <- as.data.frame(rbindlist(bio.dw.whole.kg.list))

# sum up deadwood ag compartiments 
bio_dw_whole_ag_kg.df <- bio_dw_whole_kg.df %>% 
  select(-compartiment) %>% 
  mutate(compartiment = "ag") %>% 
  group_by(plot_ID, tree_ID, inv, inv_year, compartiment) %>% 
  summarise(B_kg_tree = sum(as.numeric(B_kg_tree)))



# 1.3.2. biomass broken deadwood trees (bruchstücke, 3) ------------------------------------------------------------------------
# for broken deadwood trees above 1.3 m all compartiments except foliage ("ndl" ) are calculated via TapeS
# export list for biomasse
bio.dw.broken.kg.list <- vector("list", length = nrow(HBW_DW_broken))
for (i in 1:nrow(HBW_DW_broken)){
  # i = 1
  
  # select general info about the DW item
  my.plot.id <- HBW_DW_broken[,"plot_ID"][i]
  my.tree.id <- HBW_DW_broken[,"tree_ID"][i]
  my.decay.type <- HBW_DW_broken[,"decay"][i]
  my.dw.spec <- HBW_DW_broken[,"dw_sp"][i]
  my.CF.BL <- HBW_DW_broken[,"LH_NH"][i]
  
  # select variables fot TprTrees object
  spp =  na.omit(as.numeric(unique(HBW_DW_broken$tapes_ID[HBW_DW_broken$plot_ID==my.plot.id & HBW_DW_broken$tree_ID==my.tree.id]))) 
  Dm = na.omit(as.list(as.numeric(unique(HBW_DW_broken$d_cm[HBW_DW_broken$plot_ID==my.plot.id & HBW_DW_broken$tree_ID==my.tree.id])))) # diameter in cm
  Hm = as.list(as.numeric(1.3))
  Ht = na.omit(as.numeric(unique(HBW_DW_broken$l_dm[HBW_DW_broken$plot_ID==my.plot.id & HBW_DW_broken$tree_ID==my.tree.id]))/10) # lenth in meter m
  
  # create object  
  obj.dw <- tprTrees(spp, Dm, Hm, Ht, inv = 4)
  
  # create the deimitation of the stem section we want TapeS to caluculate the volume for
  A <- 0 # lower limit
  B <- Ht # upper limit = lenght
  
  # calcualte volume for stem segment 0 to length 
  bio.df <- as.data.frame(cbind(
    "vol_m3" = c((tprVolume(obj.dw[obj.dw@monotone == TRUE], bark = TRUE, AB = list(A = A, B = B), iAB = "H") - tprVolume(obj.dw[obj.dw@monotone == TRUE], bark = FALSE, AB = list(A = A, B = B), iAB = "H")), 
                 (tprVolume(obj.dw[obj.dw@monotone == TRUE], bark = FALSE, AB = list(A = A, B = B), iAB = "H")),
                 (tprVolume(obj.dw[obj.dw@monotone == TRUE], bark = TRUE, AB = list(A = A, B = B), iAB = "H"))), 
    "compartiment" = c("sb", "sw", "ag"))) %>% 
    # calculate biomass
    mutate(B_kg_tree = B_DW(as.numeric(vol_m3), my.decay.type, my.dw.spec))

  bio.info.df <- as.data.frame(cbind(
    "plot_ID" = c(my.plot.id), 
    "tree_ID" = c(my.tree.id), 
    "inv" = c(HBW_DW_broken$inv[HBW_DW_broken$plot_ID == my.plot.id & HBW_DW_broken$tree_ID == my.tree.id]), 
    "inv_year" = c(as.integer(HBW_DW_broken$inv_year[HBW_DW_broken$plot_ID == my.plot.id & HBW_DW_broken$tree_ID == my.tree.id])),
    "compartiment" = c(bio.df$compartiment),
    "B_kg_tree" = c(as.numeric(bio.df$B_kg_tree))
  ) )
  
  bio.dw.broken.kg.list[[i]] <- bio.info.df
  
}
bio_dw_broken_kg.df <- as.data.frame(rbindlist(bio.dw.broken.kg.list))



# 1.3.3. biomass for deadwood pieces --------------------------------------------------------
bio_dw_pieces_kg.df <- HBI_DW %>% 
  filter(dw_type %in% c(1, 4, 6)) %>% 
  mutate(
  compartiment =  "ag", 
  B_kg_tree = B_DW(V_DW_cylinder(d_cm/100, l_dm/10), decay, dw_sp)) %>% 
  select("plot_ID", "tree_ID", "inv", "inv_year", "compartiment", "B_kg_tree")


# 1.3.4. add biomass to DW dataframe -----------------------------
# harmonise strings
bio_dw_whole_kg.df[,c(1,2, 4, 6)] <- lapply(bio_dw_whole_kg.df[,c(1,2,4, 6)], as.numeric)
bio_dw_whole_ag_kg.df[,c(1,2, 4, 6)] <- lapply(bio_dw_whole_ag_kg.df[,c(1,2,4, 6)], as.numeric)
bio_dw_broken_kg.df[,c(1,2, 4, 6)] <- lapply(bio_dw_broken_kg.df[,c(1,2,4, 6)], as.numeric)
bio_dw_pieces_kg.df[,c(1,2, 4, 6)] <- lapply(bio_dw_pieces_kg.df[,c(1,2,4, 6)], as.numeric)


# join biomass in
HBI_DW <- HBI_DW %>% 
  left_join(., rbind(
            bio_dw_whole_kg.df,
            bio_dw_whole_ag_kg.df,
            bio_dw_broken_kg.df, 
            bio_dw_pieces_kg.df),
            by = c("plot_ID", "tree_ID", "inv", "inv_year"),
            multiple = "all") 



# 1.4. Nitrogen stock -----------------------------------------------------




# NOTES -------------------------------------------------------------------


# N. calculate volume in loop ---------------------------------------------

# calcualte volume 
# # export list for volume
# vol.dw.broken.kg.list <- vector("list", length = nrow(HBW_DW_broken))
# {
# vol.df <- as.data.frame(cbind(
#   "vol_m3" = c((tprVolume(obj.dw[obj.dw@monotone == TRUE], bark = TRUE) - tprVolume(obj.dw[obj.dw@monotone == TRUE], bark = FALSE)), 
#                (tprVolume(obj.dw[obj.dw@monotone == TRUE], bark = FALSE)),
#                 (tprVolume(obj.dw[obj.dw@monotone == TRUE], bark = TRUE))), 
#   "compartiment" = c("sb", "sw", "ag")))
# # save volume and resective tree info in export df 
# vol.info.df <- as.data.frame(cbind(
#   "plot_ID" = c(as.integer(HBW_DW_whole$plot_ID[HBW_DW_whole$plot_ID == my.plot.id & HBW_DW_whole$tree_ID == my.tree.id])), 
#   "tree_ID" = c(as.integer(HBW_DW_whole$tree_ID[HBW_DW_whole$plot_ID == my.plot.id & HBW_DW_whole$tree_ID == my.tree.id])), 
#   "inv" = c(HBW_DW_whole$inv[HBW_DW_whole$plot_ID == my.plot.id & HBW_DW_whole$tree_ID == my.tree.id]), 
#   "inv_year" = c(as.integer(HBW_DW_whole$inv_year[HBW_DW_whole$plot_ID == my.plot.id & HBW_DW_whole$tree_ID == my.tree.id])),
#   "compartiment" = c(vol.df$compartiment),
#   "V_m3_tree" = c(as.numeric(vol.df$vol_m3))
# ) )
# 
# vol.dw.whole.kg.list[[i]] <- vol.info.df
# }
# vol_dw_whole_kg.df <- as.data.frame(rbindlist(vol.dw.whole.kg.list))

