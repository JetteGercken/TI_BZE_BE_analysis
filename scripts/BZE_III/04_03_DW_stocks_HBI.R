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
DW_data <-  read.delim(file = here("output/out_data/out_data_BZE/HBI_DW_update_1.csv"), sep = ";", dec = ",")


# HBI forest type info per plot  (Bestandestyp)
# this i deed to later say "if the stocking species are mainly coniferous i need this secies group from tapeS
# and if th estocking species fall in the category broadleafes the other tapes species code"
HBI_forest_info <- read.delim(file = here("data/input/BZE2_HBI/be.csv"), sep = ",", dec = ",", stringsAsFactors=FALSE)

HBI_inv_info <- read.delim(file = here(paste0(out.path.BZE3,"HBI_inv_info.csv")), sep = ";", dec = ",") 

# 0.4 dataprep  -----------------------------------------------------------

# 0.4.2. species names -------------------------------------------------------------
# 1 Nadelholz
# 2 Laubholz (außer Eiche)
# 3 Eiche
# 4 unbekannt

DW_data <- DW_data %>% 
  # join in forest type from HBI forst.csv
  left_join(., 
            HBI_forest_info %>% 
   # assign forest types into coniferus vs broadleaved categories based on x_forest code table
              mutate(LH_NH_stand = case_when(besttyp %in% c(4, 5, 7, 8, 10, 91 ) ~ "LB",
                                       besttyp %in% c(92, 1, 2, 3, 6, 9 ) ~ "NB", 
                                       TRUE ~ NA)) %>% 
                       select(bund_nr, LH_NH_stand), 
                     by = c("plot_ID" = "bund_nr")) %>% 
  mutate(SP_code =  case_when(dw_sp == 1 | (dw_sp == 4 & LH_NH_stand == "NB") ~ "gfi",  # Fi
                             dw_sp == 2 | (dw_sp == 4 & LH_NH_stand == "LB") ~ "rbu", # BU
                             dw_sp == 3 ~ "sei",                                   # EI   
                             TRUE ~ NA) ) %>% 
  left_join(., SP_names_com_ID_tapeS %>% 
              mutate(char_code_ger_lowcase = tolower(Chr_code_ger)),
            by = c("SP_code" = "char_code_ger_lowcase")) %>%  
  # transforming Biosoil decay types into BWI decay types by joining Biosoil decay type 1 & 2 
  mutate(dec_type_BWI = case_when(decay == 1 | decay == 2 ~ 1, 
                                  decay == 3 ~ 2, 
                                  decay == 4 ~ 3, 
                                  TRUE ~ 4))

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
HBW_DW_whole <- DW_data[DW_data$dw_type %in% c(2, 5)  & DW_data$decay  %in% c(1,2), ]
# export list for biomasse
bio.dw.whole.kg.list <- vector("list", length = nrow(HBW_DW_whole))
# export list for volume
for (i in 1:nrow(HBW_DW_whole)){
  # i = 1
  
  # select general info about the DW item
  my.plot.id <- HBW_DW_whole[,"plot_ID"][i]
  my.tree.id <- HBW_DW_whole[,"tree_ID"][i]
  my.decay.type <- HBW_DW_whole[,"dec_type_BWI"][i]
  my.dw.spec <- HBW_DW_whole[,"dw_sp"][i]
  my.CF.BL <- HBW_DW_whole[,"LH_NH_stand"][i]
  
  # select variables fot TprTrees object
  # translating Species groups into TapeS codes
  spp =  na.omit(as.numeric(unique(HBW_DW_whole$tpS_ID[HBW_DW_whole$plot_ID==my.plot.id & HBW_DW_whole$tree_ID==my.tree.id]))) 
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
    mutate(B_kg_tree = rdB_DW(B_kg_tree, my.decay.type, my.dw.spec))
  
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
bio_dw_whole_kg_df <- as.data.frame(rbindlist(bio.dw.whole.kg.list))

# sum up deadwood ag compartiments 
bio_dw_whole_ag_kg_df <- bio_dw_whole_kg_df %>% 
  select(-compartiment) %>% 
  mutate(compartiment = "ag") %>% 
  group_by(plot_ID, tree_ID, inv, inv_year, compartiment) %>% 
  summarise(B_kg_tree = sum(as.numeric(B_kg_tree)))



# 1.3.2. biomass broken deadwood trees (bruchstücke, 3) ------------------------------------------------------------------------
# for broken deadwood trees above 1.3 m all compartiments except foliage ("ndl" ) are calculated via TapeS
# export list for biomasse
DW_data_broken <- DW_data[DW_data$dw_type == 3  & DW_data$decay  %in% c(1,2), ]
bio.dw.broken.kg.list <- vector("list", length = nrow(DW_data_broken))
for (i in 1:nrow(DW_data_broken)){
  # i = 1
  
  # select general info about the DW item
  my.plot.id <- DW_data_broken[,"plot_ID"][i]
  my.tree.id <- DW_data_broken[,"tree_ID"][i]
  my.decay.type <- DW_data_broken[,"dec_type_BWI"][i]
  my.dw.spec <- DW_data_broken[,"dw_sp"][i]
  
  # select variables fot TprTrees object
  spp =  na.omit(as.numeric(unique(DW_data_broken$tpS_ID[DW_data_broken$plot_ID==my.plot.id & DW_data_broken$tree_ID==my.tree.id]))) 
  Dm = na.omit(as.list(as.numeric(unique(DW_data_broken$d_cm[DW_data_broken$plot_ID==my.plot.id & DW_data_broken$tree_ID==my.tree.id])))) # diameter in cm
  Hm = as.list(as.numeric(1.3))
  Ht = na.omit(as.numeric(unique(DW_data_broken$l_dm[DW_data_broken$plot_ID==my.plot.id & DW_data_broken$tree_ID==my.tree.id]))/10) # lenth in meter m
  
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
    "inv" = c(DW_data_broken$inv[DW_data_broken$plot_ID == my.plot.id & DW_data_broken$tree_ID == my.tree.id]), 
    "inv_year" = c(as.integer(DW_data_broken$inv_year[DW_data_broken$plot_ID == my.plot.id & DW_data_broken$tree_ID == my.tree.id])),
    "compartiment" = c(bio.df$compartiment),
    "B_kg_tree" = c(as.numeric(bio.df$B_kg_tree))
  ) )
  
  bio.dw.broken.kg.list[[i]] <- bio.info.df
  
}
bio_dw_broken_kg_df <- as.data.frame(rbindlist(bio.dw.broken.kg.list))



# 1.3.3. biomass for stumps -----------------------------------------------
DW_data_stump <- DW_data[DW_data$dw_type == 4 & DW_data$decay  %in% c(1,2),]
bio.dw.stump.kg.list <- vector("list", length = nrow(DW_data_stump))
for (i in 1:nrow(DW_data_stump)){
  # i = 3
  
  # select general info about the DW item
  my.plot.id <- DW_data_stump[,"plot_ID"][i]
  my.tree.id <- DW_data_stump[,"tree_ID"][i]
  my.decay.type <- DW_data_stump[,"dec_type_BWI"][i]
  my.dw.spec <- DW_data_stump[,"dw_sp"][i]
  
  # select variables fot TprTrees object
  spp =  na.omit(as.numeric(unique(DW_data_stump$tpS_ID[DW_data_stump$plot_ID==my.plot.id & DW_data_stump$tree_ID==my.tree.id]))) 
  # calculate the DBH: diameter a tree with the measured stump diameter would have at 1.3m height 
  bwi.spp = na.omit((unique(DW_data_stump$BWI[DW_data_stump$plot_ID==my.plot.id & DW_data_stump$tree_ID==my.tree.id]))) 
  d.cm = as.numeric(unique(DW_data_stump$d_cm[DW_data_stump$plot_ID==my.plot.id & DW_data_stump$tree_ID==my.tree.id])) # diameter in cm
  l.m = as.numeric(unique(DW_data_stump$l_dm[DW_data_stump$plot_ID==my.plot.id & DW_data_stump$tree_ID==my.tree.id]))/10
  Dm = as.list(DBH_Dahm(my.plot.id, as.numeric(d.cm)*10, l.m, bwi.spp))
  # estimate height a tree with the estimated DBH diameter would have
  Hm = as.list(as.numeric(1.3))
  Ht = (as.numeric(estHeight(d13 = as.numeric(Dm), sp = spp))) # lenth in meter m
  
  
  # compartiments
  comp <- c("stw", "stb")
  
  # create object with estimated DBH and height 
  obj.dw <- tprTrees(spp, Dm, Hm, Ht, inv = 4)
 
  
  
  ## calcualte biomass and bark-stump ratio for the "pseudo" tree 
  # deal with error for small trees: # https://stackoverflow.com/questions/2158780/catching-an-error-and-then-branching-logic
  t <- try(tprBiomass(obj = obj.dw[obj.dw@monotone == T], component = comp))
  if("try-error" %in% class(t)){
    ratio.df <- as.data.frame(
      tprBiomass(obj = obj.dw[obj.dw@monotone == F], component = comp)
    ) %>% # momo = F if heigh low, mono = T if height normal
      mutate(ag = stw + stb) %>% 
      #apply the biomass reduction factor to the biomass of deadwoodto account for decay state
      mutate(across(stw:ag, ~rdB_DW( .x, my.decay.type, my.dw.spec) )) %>% 
      # calcualte bark:total stump biomass
      mutate(bark_ag_ratio = stb/ag, 
             wood_ag_ratio = stw/ag)
  }else{
    ratio.df <- as.data.frame(
    tprBiomass(obj = obj.dw[obj.dw@monotone == T], component = comp)
  ) %>% # momo = F if heigh low, mono = T if height normal
    mutate(ag = stw + stb) %>% 
    #apply the biomass reduction factor to the biomass of deadwoodto account for decay state
    mutate(across(stw:ag, ~rdB_DW( .x, my.decay.type, my.dw.spec) )) %>% 
    # calcualte bark:total stump biomass
    mutate(bark_ag_ratio = stb/ag, 
           wood_ag_ratio = stw/ag)
  }
  
  # calcualte Biomass vie Volume cylinder function and wood density 
  ag.B.kg = as.data.frame(B_DW(V_DW_cylinder(as.numeric(d.cm)/100, as.numeric(l.m)), my.decay.type, my.dw.spec))[,1]
  
  # claculate komaprtimetn biomass with ratios and ag
  bio.df <- as.data.frame(cbind(
    "compartiment" = c("ag", "stw", "stb"), 
    "B_kg_tree" = c(ag.B.kg, # ag biomass
                    ag.B.kg*as.numeric(ratio.df$wood_ag_ratio), # stump wood biomass
                    ag.B.kg*as.numeric(ratio.df$bark_ag_ratio)) # stump bark biomass
                    ))

  bio.info.df <- as.data.frame(cbind(
    "plot_ID" = c(my.plot.id), 
    "tree_ID" = c(my.tree.id), 
    "inv" = c(DW_data_stump$inv[DW_data_stump$plot_ID == my.plot.id & DW_data_stump$tree_ID == my.tree.id]), 
    "inv_year" = c(as.integer(DW_data_stump$inv_year[DW_data_stump$plot_ID == my.plot.id & DW_data_stump$tree_ID == my.tree.id])),
    "compartiment" = c(bio.df$compartiment),
    "B_kg_tree" = c(as.numeric(bio.df$B_kg_tree))
  ) )
  
   
  bio.dw.stump.kg.list[[i]] <- bio.info.df
  
}
bio_dw_stump_kg_df <- as.data.frame(rbindlist(bio.dw.stump.kg.list))



# 1.3.4. biomass for deadwood pieces --------------------------------------------------------
bio_dw_pieces_kg_df <- DW_data %>% 
  filter(dw_type %in% c(1, 6) |
           dw_type %in% c(2, 5, 3, 4) & decay > 2) %>% 
  mutate(
  compartiment =  "ag", 
  V_m3_tree = V_DW_cylinder(as.numeric(d_cm)/100, as.numeric(l_dm/10)),
  B_kg_tree = B_DW(V_m3_tree, dec_type_BWI, dw_sp)) %>% 
  select("plot_ID", "tree_ID", "inv", "inv_year", "compartiment", "B_kg_tree")



# 1.3.4. add biomass to DW dataframe -----------------------------
# harmonise strings
all_dw_bio_df <- rbind(
  bio_dw_whole_kg_df,
  bio_dw_whole_ag_kg_df,
  bio_dw_broken_kg_df,
  bio_dw_stump_kg_df,
  bio_dw_pieces_kg_df)
all_dw_bio_df[,c(1,2, 4, 6)] <- lapply(all_dw_bio_df[,c(1,2,4, 6)], as.numeric)



# join biomass in deadwood 
DW_data <- DW_data %>% 
  left_join(., all_dw_bio_df,
            by = c("plot_ID", "tree_ID", "inv", "inv_year"),
            multiple = "all") 




# 1.4. Nitrogen stock -----------------------------------------------------
# for the sums we shoud ldecide iwf we want to give them the compartiment "ag" 
# because that´s what they actually are 
# or the compartiment "total" because that way it will be easier to link the total available 
# deadwood biomass, nitrogen and carbon stock with the total stocks of trees and RG

# 1.4.1. Nitrogen stock in compartiments -----------------------------------------
N_dw_ag_comps_kg_df <- DW_data %>%
  # compartitioned deadwood trees
  filter(dw_type %in% c(2, 5, 3, 4) & decay <=2 & compartiment != "ag" | # deselect summed up compartiments for whole trees, stumps and broken trees
  # deadwood trees that could have been compartitioned if htey would´t be to decayed
  # --> biomass is always "ag"
           dw_type %in% c(2, 5, 3, 4) & decay > 2|
  # uncompartionable deadwood trees --> biomass is always "ag"
           dw_type %in% c(1, 6)) %>% 
  mutate(N_kg_tree = case_when(dw_type %in% c(2, 5, 3, 4) & compartiment != "ag" ~ N_all_com(B_kg_tree, N_SP_group, N_f_SP_group_MoMoK, N_bg_SP_group, compartiment), 
                               # for all trees that are not copmartioned (meaning all trees that don´t have )
                               dw_type %in% c(1, 6) & compartiment == "ag" | dw_type %in% c(2, 5, 3, 4) & decay > 2 & compartiment =="ag" ~ N_all_com(B_kg_tree, N_SP_group, N_f_SP_group_MoMoK, N_bg_SP_group,"sb"), 
                               TRUE ~ NA)) %>% 
  select(plot_ID, tree_ID, inv, inv_year, dw_type, compartiment, N_kg_tree) 


# 1.4.2. total nitrogen stocks: sum up Nitrogen stock in compartiments -----------------------------------------
# summ up the aboveground compartiments 
N_dw_ag_kg_df <- N_dw_ag_comps_kg_df %>%
  # select only compartitionated trees 
  filter(dw_type %in% c(2, 5, 3, 4) & compartiment != "ag" )%>% 
  group_by(plot_ID, tree_ID, inv, inv_year, dw_type) %>% 
  summarize(N_kg_tree = sum(as.numeric(N_kg_tree))) %>% 
  mutate(compartiment = "ag") %>% 
  select("plot_ID", "tree_ID", "inv", 
         "inv_year", "dw_type", "compartiment", "N_kg_tree")



# 1.4.3. join Nitrogen stocks into deadwood dataset -----------------------------------
DW_data <- DW_data %>% left_join(., 
                             rbind(N_dw_ag_comps_kg_df , 
                                   N_dw_ag_kg_df), 
                             by = c("plot_ID", "tree_ID", "inv", "inv_year",
                                    "dw_type", "compartiment"), 
                             multiple = "all")



# 1.5 carbon stock per tree & compartiment -------------------------------------------------------
DW_data <- DW_data %>% mutate(C_kg_tree = carbon(B_kg_tree))



# 2. data export ----------------------------------------------------------
# create export dataset
DW_data_update_4 <- DW_data

write.csv2(DW_data_update_4, paste0(out.path.BZE3, paste(unique(DW_data_update_4$inv)[1], "DW_update_4", sep = "_"), ".csv"))


#DW_data %>% filter(is.na(B_kg_tree))



# NOTES ---------------------------------------------------------------------------------------------------

# thoughts & questions --------------------------------------------------------------------------------------

## compartiment names for ag compartiment which is also the only total compartiment available for DW
# for the sums of the stocks we should decide if we want to give them the compartiment "ag" 
# because that´s what they actually are 
# or the compartiment "total" because that way it will be easier to link the total available 
# deadwood biomass, nitrogen and carbon stock with the total stocks of trees and RG

## deadwood biomass differences to momok 
# pseudo trees where not averaged over dw group, plot and decay state but calculated for each dw item separately
# for those trees that have compartiments (like whole trees, stumps, broken pieces) all compatiments were calculated 
# regardless their state of decay 



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




ratio.df <- as.data.frame(
  tprBiomass(obj = obj.dw[obj.dw@monotone == F], component = comp)
) %>% # momo = F if heigh low, mono = T if height normal
  mutate(ag = stw + stb) %>% 
  #apply the biomass reduction factor to the biomass of deadwoodto account for decay state
  mutate(across(stw:ag, ~rdB_DW( .x, my.decay.type, my.dw.spec) )) %>% 
  # calcualte bark:total stump biomass
  mutate(bark_ag_ratio = stb/ag, 
         wood_ag_ratio = stw/ag)

# for larger trees the tpr function monotone has to be changed to "True"
if(exists('ratio.df') == FALSE){
  ratio.df <- as.data.frame(tprBiomass(obj = obj.dw[obj.dw@monotone == T], component = comp)) %>% # momo = F if heigh low, mono = T if height normal
    mutate(ag = stw + stb) %>% 
    #apply the biomass reduction factor to the biomass of deadwoodto account for decay state
    mutate(across(stw:ag, ~rdB_DW( .x, my.decay.type, my.dw.spec) )) %>% 
    # calcualte bark:total stump biomass
    mutate(bark_ag_ratio = stb/ag, 
           wood_ag_ratio = stw/ag)
}