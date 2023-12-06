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
colnames(HBI_DW) <- c("plot_ID", "tree_ID", "dw_type", "dw_sp", "count", "d_mm", "l_dm", "decay")

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

HBI_DW %>% left_join(., 
                     HBI_forest_info %>% select(bund_nr, besttyp)) %>% 
  mutate(LH_NH = case_when(besttyp %in% c() ~ LB, 
                           besttyp %in% c() ~ NB, 
                           TRUE ~ NA))
# 1. calculations ---------------------------------------------------------------

# 1.2. volume --------------------------------------------------------------------
#1 liegend; starkes Totholz; umfasst Stamm, Äste, Zweige,  abgebrochene Kronen, D ≥ 10 cm am dickeren Ende
# 2 stehend, ganzer Baum; stehendes Totholz mit Ästen BHD ≥ 10 cm
# 3 stehend, Bruchstück; Baumstumpf ohne Äste BHD ≥ 10 cm, Höhe ≥ 13 dm
# 4 Wurzelstock Ø Schnittflächendurchmesser ≥ 10 cm, Höhe < 13 dm
# 5 liegend; ganzer Baum BHD ≥ 10 cm
# 6 in Haufen vorkommendes Totholz D ≥ 10 cm am dickeren Ende


# 1.3.1. whole trees  -----------------------------
# Das Volumen der Totholzart 2 = stehend oder liegend, ganzer Baum wird wie ein lebender Baum behandelt, d. h. es werden BHD und Höhe gemessen und über die Programmbibliothek BDat das Volumen hergeleitet. 
# Bei der Totholzart 3 = stehend oder liegend, Bruchstück (mit Wurzelanlauf) wird ebenfalls der BHD und die Höhe erfasst und anschließend mit BDat das Volumen für gebrochene Bäume berechnet. 
   # -->  Für Bruchstücke mit Wurzelanlauf < 3 m Länge bzw. Höhe liefert BDat jedoch unplausible Totholzvolumina, da es für diesen Wertebereich nicht 
#         entsprechend parametrisiert wurde. Alternativ wurde für diese Totholzstücke die Zylinderformel angewandt, wobei deren BHD als Prädiktor für den Durchmesser einfließt

# For whole trees (laying 5, standing 2) and for broken trees with L_m > 1.3 the volume 
# estimaion preceedes like for living trees
# For broken deadwood items, stumps and piles of deadwood the zylinder function is applied 



# 1.3 biomass ------------------------------------------------------------------------
HBW_DW_whole <- HBI_DW[HBI_DW$dw_type %in% c(2, 5, 3), ]
for (i in 1:nrow(HBW_DW_whole)){
  # i = 1
  
  # select general info about the DW item
  my.plot.id <- HBW_DW_whole[,"plot_ID"][i]
  my.tree.id <- HBW_DW_whole[,"tree_ID"][i]
  # my.decay.type <- HBW_DW_whole[,"decay"][i]
  # my.dw.type <- HBW_DW_whole[,"dw_type"][i]
  my.dw.spec <- HBW_DW_whole[,"dw_sp"][i]
  
  # select variables fot TprTrees object
  # translating Species groups into TapeS codes
  spp = case_when(SP_group == 1 | (SP_group == 4 & LH_NH == "NB") ~ 1,  # Fi
                          SP_group == 2 | (SP_group == 4 & LH_NH == "LB") ~ 15, # BU
                          SP_group == 3 ~ 17,                                   # EI   
                          TRUE ~ NA), 
  Dm = na.omit(as.list(as.numeric(unique(HBW_DW_whole$D_mm[trees$plot_ID==my.plot.id & trees$tree_ID==my.tree.id])/10))) # diameter in cm
  Hm =  as.list(as.numeric(1.3))
  Ht = na.omit(as.numeric(unique(trees$H_m[trees$plot_ID==my.plot.id & trees$tree_ID==my.tree.id])))
  # create tapes compartiments
  comp <- as.character(c("stw","stb","sw", "sb", "fwb", "ndl" ))
  
  # create object  
  obj.trees <- tprTrees(spp, Dm, Hm, Ht, inv = 4)
  
  
}
 



