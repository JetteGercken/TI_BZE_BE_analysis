# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the peat land soil inventory
# sorting the sampling circuits according to their inventory status 
# HBI BZE2

# this script should sort every sampling circle, may it be a living trees plot, 
# a regeneration plot or a deadwood plot accoridnt to it´s invenory status (Aufnahmemöglichkeit)

# the inventory status code displays the following information: 
## ALL PLOTS & STAND COMPONENTS
# Punktstatus (x_plotstatus_bze)
    # -9 Merkmal vergessen, nicht rekonstruierbar oder unbekannt
    # -1 Merkmal nicht erhoben
    # 0 BZE-Punkt wurde in allen Erhebungen beprobt
    # 1 BZE-Punkt wurde bei BZE II und III, aber nicht BZE I beprobt
    # 3 BZE-Punkt wurde bei BZE I und III, aber nicht BZE II beprobt
  # -- Neuanlage bei der BZE III
    # 11 Neuanlage: Erstaufforstung
    # 12 Neuanlage: Wiederaufforstung
    # 13 Neuanlage: Sukzession
    # 14 Neuanlage: Rasterverschiebung
    # 15 Neuanlage: Rasterumstellung
    # 16 Neuanlage: BZE-Punkt wurde nicht gefunden / nicht beprobt
    # 17 Neuanlage: Punkt wurde bisher nicht aufgenommen, obwohl zur Waldfläche gehörend
    # 19 Neuanlage: Sonstiges
  # -- Ausfall bei der BZE III
    # 21 Ausfall: Umwandlung in Nichtwald oder Nichtholzboden
    # 22 Ausfall: Punkt nicht gefunden und deswegen Neuanlage
    # 23 Ausfall: Punkt nicht mehr erreichbar (z. B: Truppenübungsplatz, Moorrenaturierung)
    # 24 Ausfall: keine Probenahmeerlaubnis
    # 25 Ausfall: Rasterverschiebung
    # 26 Ausfall: Rasterumstellung (Netzweite geändert)
    # 27 Ausfall: Rasterüberprüfung (Punkt gehört nicht zum Rasternetz)
    # 28 Ausfall: Eichenprozessionsspinner (EPS)
    # 29 Ausfall: Gesundheitsgefahr (außer EPS)
    # 30 Ausfall: Arbeitsschutz
    # 39 Ausfall: sonstiges
    # 40 Ausfall: Koordinaten oder Punktnummernfehler
    # 50 Ausfall: WZE-Punkt aber kein BZE-Punkt
    # 60 Ausfall: Level II-Punkt
# plots to exclude: everything with ID >=21


## LIVING TREES
# Status der Bestandsaufnahmeplots (x_hbi_status) --> reffers to whole assessment of trees 
    # -9 = Merkmal vergessen, nicht rekonstruierbar oder unbekannt - Status is unknown, was not assesed or canot be reconstructed
    # -1 = Merkmal nicht erhoben  - status was not assessed
    #  1 = Aufnahme erfolgte am HBI-Mittelpunkt  - the plot is at the same position as an HBI plot --> repetitive inventory
    #  2 = Aufnahme erfolgte an neuem Bezugspunkt - plot in not at the same posticion as in the previous invenotry --> new inventory
    #  3 = Aufnahme erfolgte an BWI-Punkt (nur BB/BY) - the plot is at the same position as an BWI plot --> repetitive inventory
   ## plot stati to exclude: 3 ????
   ## plot stati to change:
   ## only trees with have bestandesaufnahmeplotstatus of 2 can be used for growth calc

# status der Bestandesaufnahme --> reffers to the respective samping circle 
    # -9 Merkmal vergessen, nicht rekonstruierbar oder unbekannt
    # -1 Merkmal nicht erhoben
    # 1 Aufnahme wurde erfolgreich durchgeführt
    # 2 Aufnahme war nicht möglich, keine Objekte vorhanden
    # 3 Aufnahme war nicht möglich, sonst. Gründe (Störung etc.)
## plot stati to exclude: 3  --> find trees that match the CCS and remove them 
## create "LT_CCS_to_exclude" dataset


## REGENERATION
# status der Verjüngungsaufnahme
    # -9 Merkmal vergessen, nicht rekonstruierbar oder unbekannt
    # -2 Merkmal nicht vorhanden
    # -1 Merkmal nicht erhoben
    #  1 Aufnahme wurde erfolgreich durchgeführt
    #  2 Aufnahme war nicht möglich, keine Objekte vorhanden
    #  3 Aufnahme war nicht möglich, sonst. Gründe (Störung etc.)
## plot stati to exclude: 3  --> find trees that match the CCS and remove them 
## create "RT_CCS_to_exclude" dataset


## DEADWOOD
# status der Totholzaufnahme
    # -9 Merkmal vergessen, nicht rekonstruierbar oder unbekannt
    # -1 Merkmal nicht erhoben
    # 1 Aufnahme wurde erfolgreich durchgeführt
    # 2 Aufnahme war nicht möglich, keine Objekte vorhanden
    # 3 Aufnahme war nicht möglich, sonst. Gründe (Störung etc.)
    # 4 Aufnahme auf 0,5 der Probekreisfläche
    # 5 Aufnahme auf 0,25 der Probekreisfläche
## plot stati to exclude: 3  --> find trees that match the CCS and remove them 
## create "DW_CCS_to_exclude" dataset

# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the peat land soil inventory
# Functions & require


# ----- 0.1. packages and functions --------------------------------------------
source(paste0(getwd(), "/scripts/01_00_functions_library.R"))

# ----- 0.2. working directory -------------------------------------------------
here::here()

out.path.BZE3 <- ("output/out_data/out_data_BZE/") 

# ----- 0.4 importing data -----------------------------------------------------
## BZE 2
# this dataset contains the BZE file tit_1 which displays info about the BZE inventory in general
# so info that´s base of all sub inventories like trees, deadwood, regeneration
inv_info <- read.delim(file = here("data/input/BZE2_HBI/tit.csv"), sep = ",", dec = ".", stringsAsFactors=FALSE) %>% select(-c("re_form", "re_lage", "neigung", "exposition", "anmerkung"))
colnames(inv_info) <- c("plot_ID", "team", "date", "plot_inv_status")
# create column that just contains year of inventory: https://www.geeksforgeeks.org/how-to-extract-year-from-date-in-r/
inv_info$date <- as.Date(inv_info$date)
inv_info$inv_year <- as.numeric(format(inv_info$date, "%Y"))
# this line can be removed later
inv_info <- inv_info %>% mutate(inv = inv_name(inv_year))


## LIVING TREES
# this dataset contains information about the inventory of the respective individual sampling circuits as well as stand realted info like stand type & - structure
tree_inv_info <-  read.delim(file = here("data/input/BZE2_HBI/be.csv"), sep = ",", dec = ".", stringsAsFactors=FALSE) %>% # be
  select(bund_nr, team,  datum,  beart, besttyp, struktur,  pk1_aufnahme,   pk2_aufnahme, pk3_aufnahme, hbi_status)
colnames(tree_inv_info) <- c("plot_ID", "team", "date", "stand_spec", "stand_type", "structure", 
                             "CCS_5_inv_status",  "CCS_12_inv_status",  "CCS_17_inv_status" , "hbi_status")
tree_inv_info <- tree_inv_info %>% mutate(hbi_status = case_when(stringr::str_detect(plot_ID, '^9') ~ 3,
                                                                 stringr::str_detect(plot_ID, '^11') ~ 3,
                                                                 stringr::str_detect(plot_ID, '^12') ~ 3,
                                                                 hbi_status == -9 ~ 1,
                                                                 TRUE ~ hbi_status)) 
# create column that just contains year of inventory: https://www.geeksforgeeks.org/how-to-extract-year-from-date-in-r/
tree_inv_info$date <- as.Date(tree_inv_info$date)
tree_inv_info$inv_year <- as.numeric(format(tree_inv_info$date, "%Y"))
# this line can be removed later
tree_inv_info <- tree_inv_info %>% mutate(inv = inv_name(inv_year))



# HBI BE dataset: this dataset contains the inventory data of the tree inventory accompanying the second national soil inventory
trees_data <- read.delim(file = here("data/input/BZE2_HBI/beab.csv"), sep = ",", dec = ".")
# HBI trees
colnames(trees_data) <- c("plot_ID", "tree_ID", "tree_inventory_status", "multi_stem",  "SP_code", "age", 
                          "age_meth", "D_mm", "DBH_h_cm", "H_dm", "C_h_dm", "azi_gon", "dist_cm", "Kraft",  "C_layer")
trees_data <- trees_data %>% dplyr::select(plot_ID,  tree_ID ,  tree_inventory_status ,  multi_stem , dist_cm ,  azi_gon ,
                                           age ,  age_meth ,  SP_code ,  Kraft , C_layer , H_dm ,  C_h_dm , D_mm ,   DBH_h_cm )
# HBI forest edges
forest_edges <- read.delim(file = here("data/input/BZE2_HBI/be_waldraender.csv"), sep = ",", dec = ".")
colnames(forest_edges) <- c("plot_ID", "e_ID", "e_type", "e_form", "A_dist", "A_azi",  "B_dist", "B_azi", "T_dist", "T_azi") # t = turning point




## REGENERATION                                                                                                  
# this dataset contains the inventory status, position and extend of the sampling circle satelites of the regeneration inventory of the HBI (BZE2) 
RG_loc_info <- read.delim(file = here("data/input/BZE2_HBI/bej.csv"), sep = ",", dec = ".", stringsAsFactors=FALSE) %>% 
  select(bund_nr, pk_nr, pk_richtung, pk_dist, pk_aufnahme ,pk_maxdist)
# assign column names    # bund_nr     pk_nr      pk_richtung     pk_dist     pk_aufnahme      pk_maxdist
colnames(RG_loc_info) <- c("plot_ID", "CCS_nr", "CCS_position",  "CCS_dist", "CCS_RG_inv_status", "CCS_max_dist_cm")
# this dataset contains the plant specific inventory data of the regenertaion inventory of the HBI (BZE2), including stand and area info
RG_data <- read.delim(file = here("data/input/BZE2_HBI/bejb.csv"), sep = ",", dec = ",")
#  "bund_nr"  "pk_nr"  "lfd_nr"   "bart"  "hoehe"    "grklasse"
colnames(RG_data) <- c("plot_ID", "CCS_nr", "tree_ID", "SP_code", "H_cm", "D_class_cm")

 

##DEADWOOD
# deadwood inventory info 
DW_inv_info <- read.delim(file = here("data/input/BZE2_HBI/be_totholz_punkt.csv"), sep = ",", dec = ".", stringsAsFactors=FALSE) 
colnames(DW_inv_info) <- c("plot_ID", "CCS_DW_inv_status",  "dist_cm", "azi")
# deadwood single item data
DW_data <- read.delim(file = here("data/input/BZE2_HBI/be_totholz_liste.csv"), sep = ",", dec = ".") %>% 
  select( bund_nr, lfd_nr, typ, baumgruppe, anzahl,  durchmesser, laenge, zersetzung)
#  bund_nr lfd_nr typ      baumgruppe anzahl  durchmesser laenge zersetzung
colnames(DW_data) <- c("plot_ID", "tree_ID", "dw_type", "dw_sp", "count", "d_cm", "l_dm", "decay")


# 1. data prep  --------------------------------------
# 1.1. ALL - all plots & stand components ------------------------------------------------------------------------------------------------------------------------
# create a list with the BZE plots that should be excluded -----------------
# select plots that have a "Punktstatus (x_plotstatus_bze)" 
plots_to_exclude <- inv_info %>% 
  filter(plot_inv_status >= 21 | plot_inv_status <0) %>% 
  # select(plot_ID) %>% 
  mutate(rem_reason = "whole plot excluded during inventory status sorting")


# 1.2. LIVING TREES ----------------------------------------------------------------------------------------------------------------------------------------

# 1.2.1. prepare tree data: add old data to removed trees for stock calculations later ----------------------------------------------------------------
#### this is only for post inventory data, so BZE3 it´s only here in the code to keep the scripts equal
# there may be trees that are labelled as "lost" (removed or died of but for further processing we) by their tree inventory status and by that do not have 
# we still need their data from the previous inventory to calcualte their sampling circuit and assing ther species groups etc. 
#  dist_cm azi_gon age age_meth SP_code Kraft C_layer H_dm C_h_dm D_mm 
if(exists('trees_HBI')== TRUE){
  BZE3_trees_old_data <- trees_data %>%
    # selectonly trees with inventory status indicating the tree was lost or the status is unknown (eversthing other then 0 or 1)
    filter(!(tree_inventory_status %in% c(0,1)))%>% 
    # then filter for those rows that do not contain the old data of the respective tree that was passed on from the previous inventory
    # https://stackoverflow.com/questions/33520854/filtering-data-frame-based-on-na-on-multiple-columns
    filter_at(vars(multi_stem:DBH_h_cm),any_vars(is.na(.))) %>% 
    #replace cells with NA by values from HBI for the respective plot_ID und tree_ID: https://stackoverflow.com/questions/32899580/update-rows-of-data-frame-in-r
    rows_patch(trees_HBI %>% 
                 # filter for HBI trees that have the same plot and tree ID as those trees in BZE3 with 
                 # status != c(0, 1) and no old data passed on from HBI by semi_join 
                 semi_join(., 
                           trees_data %>%
                             filter(!(tree_inventory_status %in% c(0,1)))%>% 
                             filter_at(vars(multi_stem:DBH_h_cm),any_vars(is.na(.))), 
                           by = c("plot_ID", "tree_ID")) %>% 
                 select(-tree_inventory_status), 
               by = c("plot_ID", "tree_ID"))
  
  # add trees with "removed" status for which old data was found in HBI to BZE3 trees_data
  trees_data <- 
    trees_data %>% 
    # 1. remove trees that have a tree_inv_stat indicates they were lost from the trees dataset but have replacement data in 
    anti_join(., BZE3_trees_old_data, by = c("plot_ID", "tree_ID")) %>% 
    # 2. add trees that have a "tree was lost" inventory status in BZE3 but have data in: https://dplyr.tidyverse.org/reference/rows.html 
    rows_insert(., BZE3_trees_old_data, by = c("plot_ID", "tree_ID")) %>% 
    arrange(plot_ID, tree_ID)
} 

# 1.2.2. prepare tree data:species & inventory names -------------------------------------------------------------------------------------------------------------------------------------
trees_data <- trees_data %>% 
  # join in inventory info 
  left_join(., tree_inv_info %>% dplyr::select("plot_ID", "inv_year", "inv"), by = "plot_ID")  %>% 
  # join in the species names from x_bart to ensure the Dahm DBH correction function
  left_join(., SP_names_com_ID_tapeS %>% 
              mutate(char_code_ger_lowcase = tolower(Chr_code_ger)), 
            by = c("SP_code" = "char_code_ger_lowcase")) %>% 
  mutate(DBH_h_cm = ifelse(is.na(DBH_h_cm), 130, DBH_h_cm),        # assign DBH measuring height of 130cm when missing 
         # calculate corrected BDH if measuringheight != 1.3m
         DBH_cm = ifelse(DBH_h_cm == 130, as.numeric(D_mm)/10, DBH_Dahm(plot_ID, D_mm, DBH_h_cm, BWI))) %>% 
  # asssing corect samling circle diameter according to DBH of the tree to be able to join in the right plot area
  mutate(CCS_r_m = case_when(DBH_cm >= 7  & DBH_cm < 10 ~ 5.64, 
                             DBH_cm >= 10 & DBH_cm < 30 ~ 12.62, 
                             DBH_cm >= 30 ~ 17.84, 
                             TRUE ~ NA)) %>% 
  arrange(plot_ID, tree_ID)



# check if there are no trees left that don´t have a SP_code in xBart/ SP_names_com_ID_tapeS
SP_NAs <- trees_data %>% 
  anti_join(SP_names_com_ID_tapeS %>% 
              mutate(char_code_ger_lowcase = tolower(Chr_code_ger)), 
            by = c("SP_code" = "char_code_ger_lowcase"))

if(nrow(SP_NAs) != 0){print("There are species names or codes in the trees dataset that do not match
                                the species names and codes listed in x_bart")}else{"all fine"}


# 1.2.3. filter trees in processable and removed  -----------------------------------------------------------

# save trees to be removed in other dataset
trees_removed <- trees_data %>% 
  # trees that don´t have a species code or are outside the widest CCS
  filter(is.na(SP_code) | is.na(plot_ID) | dist_cm > 1784) %>% 
  mutate(rem_reason = "single LT excluded during inventory status sorting")


# remove trees without species code or plot ID from the dataset
trees_data <- trees_data %>% 
  # we cannot do this via antijoining "trees removed" as the species and the plot id 
  # that we also use to join the data will be NA for removed trees 
  # exclude those trees that don´t have a species code 
  filter(!(is.na(SP_code)) & !(is.na(plot_ID) ))%>% 
  # exclude trees outside the widest CCS
  filter(dist_cm <= 1784) 

# 1.2.3. forest edges dataset ---------------------------------------------
forest_edges <- forest_edges %>% 
  # join in inventory info 
  left_join(., tree_inv_info %>% dplyr::select("plot_ID", "inv_year", "inv"), by = "plot_ID") 




# 1.3. REGENRATION --------------------------------------------------------
RG_data <- RG_data %>%
  # join  in inventory info
  left_join(., tree_inv_info %>% select(plot_ID, inv_year, inv) %>% distinct(), by = "plot_ID") %>% 
  arrange(plot_ID, CCS_nr, tree_ID)
# if the CCR no is not a an integer but a character, we have to change that 


RG_loc_info <- RG_loc_info %>% 
  # join  in inventory info
  left_join(., tree_inv_info %>% select(plot_ID, inv_year, inv) %>% distinct(), by = "plot_ID") %>% 
  arrange(plot_ID, CCS_nr)


# 1.4. DEADWOOD -----------------------------------------------------------
DW_inv_info <- DW_inv_info %>% 
  # join  in inventory info
  left_join(., tree_inv_info %>% select(plot_ID, inv_year, inv) %>% distinct(), by = c("plot_ID"))  


DW_data <- DW_data %>% 
  # join in inventory info 
  left_join(., tree_inv_info %>% dplyr::select("plot_ID", "inv_year", "inv")%>% distinct(), 
            by = "plot_ID") 





# 2. data processing ------------------------------------------------------------------------------------------------------------------------------------------------------
# 2.2. LIVING TREES -------------------------------------------------------------------------------------------------------------------------------------------------------
# 2.2.1. remove not preocessable plots and sampling circuits form tree_inventory_info dataset ------------------------------------------------------------
tree_inv_info <- tree_inv_info %>% 
  # pivoting B, C: https://stackoverflow.com/questions/70700654/pivot-longer-with-names-pattern-and-pairs-of-columns
  pivot_longer(., "CCS_5_inv_status":"CCS_17_inv_status", names_to = "CCS_r_m", values_to = "CCS_LT_inv_status") %>% 
  mutate(CCS_r_m = as.numeric(case_when(CCS_r_m == "CCS_5_inv_status" ~ 5.64, 
                                        CCS_r_m == "CCS_12_inv_status" ~ 12.62,
                                        CCS_r_m == "CCS_17_inv_status" ~ 17.84,
                                        TRUE~ NA))) %>% 
  distinct() %>% 
  arrange(plot_ID)

# 2.2.2. create dataset with LT CCS to remove from trees data df ------------------------------------------------------------------------------------------------------------------------------------------------------------
# remove CCS that were not inventorable from the trees df and filter NFI (BWI) plots as well
LT_CCS_to_exclude <- plyr::rbind.fill(
  tree_inv_info %>% 
    # selects plots from dataset where non of the inventories was carried out at the NSI (BZE) inventory ("Ausfall") 
    semi_join(., plots_to_exclude, by = "plot_ID") %>% 
    mutate(rem_reason = "whole plot excluded during inventory status sorting"), 
  # remove CCS that were not inventorable from the trees df and filter NFI (BWI) plots as well
  tree_inv_info%>%
    # remove those plots that were allready "pulled" into the bind by the previous semi join with the removed plot
    # so that we don´t pull then in twice and they remain with one removal reason
    anti_join(., plots_to_exclude, by = "plot_ID") %>% 
    filter(!(CCS_LT_inv_status %in% c(1, 2)) |# CCS that were not inventorable
             !(hbi_status %in% c(1,2)) |
             inv == "warning") %>%      # plot was part of NFI not BSI
    mutate(rem_reason = case_when(
      !(CCS_LT_inv_status %in% c(1, 2)) ~ "LT circle excluded during inventory status sorting", 
      !(hbi_status %in% c(1,2)) | inv == "warning" ~ "all LT circles excluded during inventory status sorting", 
      TRUE ~ NA)))


#  2.2.3. correct CCS_inv_status == 2 if necesarry -------------------------------------------------------------------------------------------------------------------------
# check if CCS_LT_inv_status is actually accurate: 
# this means if there is a CCS with status 2 there shouldn´t be any tree in that circuit
tree_inv_info <- tree_inv_info %>% 
  # remove plots from dataset where non of the inventories was carried out at the NSI (BZE) inventory ("Ausfall") 
  anti_join(., plots_to_exclude, by = "plot_ID") %>% 
  # remove plots where one of the three sampling circuits was not inventorable (status == 3)
  anti_join(., LT_CCS_to_exclude, by = c("plot_ID", "CCS_r_m", "inv_year", "inv"))



#  2.2.4. creating "empty" LT CCS for status 2 circuits -------------------------------------------------------------------------------------------------------------
#  plot_ID inv_year compartiment  B_t_ha C_t_ha  N_t_ha
# here i create a dataset with DW plots that have status 2 
# which only contains info we can catually give so the plot area , the plot ID and the stocks which are set to 0
trees_stat_2 <- as.data.frame(tree_inv_info[tree_inv_info$CCS_LT_inv_status == 2, ])
LT.data.stat.2.list <- vector("list", length = nrow(trees_stat_2))
for (i in 1:nrow(trees_stat_2)) {
  # i = 2
  my.plot.id <- trees_stat_2[, "plot_ID"][i]
  my.ccs.r <- trees_stat_2[, "CCS_r_m"][i]
  my.plot.area <- c_A(my.ccs.r)/10000
  my.inv.year <- trees_stat_2[, "inv_year"][i]
  
  if(nrow(trees_stat_2) != 0){
    LT.staus.2.df <- as.data.frame(cbind(
      plot_ID = c(my.plot.id),
      CCS_r_m = c(my.ccs.r),
      plot_A_ha = c(my.plot.area), 
      inv_year = c(my.inv.year),
      compartiment = c("ag", "bg", "total"),
      B_CCS_t_ha = c(0, 0, 0), 
      C_CCS_t_ha = c(0, 0, 0), 
      N_CCS_t_ha = c(0, 0, 0), 
      BA_CCS_m2_ha = c(0, 0, 0), 
      n_trees_CCS_ha = c(0, 0, 0)))}else{
        LT.staus.2.df =  as.data.frame(cbind(
          plot_ID = NA,
          CCS_r_m = NA,
          plot_A_ha = NA, 
          inv_year = NA,
          compartiment = NA,
          B_CCS_t_ha = NA, 
          C_CCS_t_ha = NA, 
          N_CCS_t_ha = NA, 
          BA_CCS_m2_ha = NA, 
          n_trees_CCS_ha = NA))
      }
  try(LT.data.stat.2.list[[i]] <- LT.staus.2.df, silent = T)
  
}
LT_data_stat_2 <- as.data.frame(rbindlist(LT.data.stat.2.list))


#  2.2.5. clearing tree data and prepare for export (beab) ---------------------------------------------------------------------------------------
# by this step we create a dataset that is going to only contain inventorable/ procesabble trees from CCS and plots that are inventorable
trees_update_0 <- trees_data %>% 
  # select only trees in CCSs that are assigned status 1 
  # and are already sorted for uninventorable plots (Plotstatus >= 21)
  semi_join(., tree_inv_info %>% filter(CCS_LT_inv_status == 1),
            by = c("plot_ID", "CCS_r_m", "inv_year", "inv"))

# update trees removed dataset by those trees lost after sorting out CCS status != 1  
trees_removed <- 
  plyr::rbind.fill(
    trees_removed, 
    trees_data %>% 
      semi_join(., tree_inv_info %>% filter(CCS_LT_inv_status != 1),
                by = c("plot_ID", "CCS_r_m", "inv_year", "inv")) %>% 
      mutate(rem_reason = "LT cirlce excluded during inventory status sorting")
  )



#  2.2.6. clearing forest edges dataset and prepare for export (waldraender.csv) ---------------------------------------------------------------------------------------
forest_edges_update_1 <- forest_edges %>% 
  # here we remove those plots from the edges dataset that are not analysed for the HBI/ BZE3
  # we cannot sort for LT_CCS_inv_status in trees_inv_info because there may be plots that have RG (which can be alllocated to stands) but no LT yet
  anti_join(., plots_to_exclude,  by = c("plot_ID")) %>% 
  # remove those forest edges with a problematic inventory: 
  filter(inv != "warning")



forest_edges_removed <-  
  plyr::rbind.fill(
    forest_edges %>% 
      # make sure we don´t pull in edges that already were removed because the whole plot was removed
      anti_join(., plots_to_exclude,  by = "plot_ID") %>% 
      # remove those forest edges with a problematic inventory: 
      filter(inv == "warning") %>% 
      mutate(rem_reason = "all LT circles excluded during inventory status sorting"), 
    forest_edges %>% 
      # here we filter those plots from the edges dataset that are not analysed for the HBI/ BZE3
      semi_join(., plots_to_exclude,  by = "plot_ID") %>% 
      mutate(rem_reason = "whole plot excluded during inventory status sorting")
  ) %>% 
  distinct()


# 2.2.7. create dataset with NFI plots/ BWI plots -------------------------
trees_BWI <- trees_data %>% 
  semi_join(LT_CCS_to_exclude %>% filter(hbi_status == 3),
            by = c("plot_ID", "CCS_r_m", "inv_year", "inv"))


# 2.3. RG dataset ---------------------------------------------------------------------------------------------------------------------------------------------------

# 2.3.1. create dataset with RG CCS that are not processable  ------------------------------------------------------------
RG_CCS_to_exclude <- 
  plyr::rbind.fill(
    # RG CCS that were removed because the whole plot was removed
    RG_loc_info %>% 
      semi_join(plots_to_exclude, by = "plot_ID") %>% 
      mutate(rem_reason = "whole plot excluded during inventory status sorting"),
    # individual RG CCS that were removed
    RG_loc_info %>% 
      # to keep one removal reason per plot we remove the CCS that were already "pulled" 
      # into the bind by the "plots to remove" dataframe from this semi join 
      anti_join(plots_to_exclude, by = "plot_ID") %>% 
      # remove plots where one of the four sampling circuits was not inventorable
      filter(!(CCS_RG_inv_status %in% c(1, 2))) %>% 
      mutate(rem_reason = "RG circle excluded during inventory status sorting"))


# 2.3.2. remove not processable plots and sampling circuits form RG_loc_info dataset ------------------------------------------------------------
RG_loc_info <- RG_loc_info %>% 
  distinct() %>% 
  # remove plots from dataset where non of the inventories was carried out at the NSI (BZE) inventory ("Ausfall") 
  anti_join(., plots_to_exclude, by = "plot_ID") %>% 
  # exclude non inventoryble RG CCS
  anti_join(., RG_CCS_to_exclude, by = c("plot_ID", "CCS_nr", "inv_year", "inv")) %>% 
  arrange(plot_ID, CCS_nr) %>% 
  # change the maximum distance to the default setting of 500cm if its NA or -9
  mutate(CCS_max_dist_cm = ifelse(is.na(CCS_max_dist_cm) | 
                                    CCS_max_dist_cm == -9 |
                                    CCS_RG_inv_status == 2, 500, CCS_max_dist_cm))



#  2.3.4. creating "empty" RG CCS for status 2 circuits ----------------------
# here i create a dataset with RG plots that have status 2 
# which only contains info we can catually give so the plot area , the plot ID and the stocks which are set to 0
RG_stat_2 <- RG_loc_info[RG_loc_info$CCS_RG_inv_status == 2, ]
RG.data.stat.2.list <- vector("list", length = nrow(RG_stat_2))
for (i in 1:nrow(RG_stat_2)) {
  # i = 1
  my.plot.id <- RG_stat_2[, "plot_ID"][i]
  my.ccs.no <- RG_stat_2[, "CCS_nr"][i]
  my.plot.area <- (c_A(as.numeric(RG_stat_2[, "CCS_max_dist_cm"][i])/100))/10000 # plot are in hectar 
  my.inv.year <- RG_stat_2[, "inv_year"][i]
  
  if(nrow(RG_stat_2) != 0){
    RG.status.2.df <- as.data.frame(cbind(
      plot_ID = c(my.plot.id),
      CCS_nr = c(my.ccs.no),
      plot_A_ha = c(my.plot.area), 
      inv_year = c(my.inv.year),
      compartiment = c("ag", "bg", "total"),
      B_t_ha = c(0, 0, 0), 
      C_t_ha = c(0, 0, 0), 
      N_t_ha = c(0, 0, 0)))
  }else{
    RG.status.2.df = as.data.frame(cbind(
      plot_ID = NA,
      CCS_nr = NA,
      plot_A_ha = NA, 
      inv_year = NA,
      compartiment = NA,
      B_t_ha = NA, 
      C_t_ha = NA, 
      N_t_ha = NA))
  }
  try(RG.data.stat.2.list[[i]] <- RG.status.2.df, silent = T)
  
}
RG_data_stat_2 <- as.data.frame(rbindlist(RG.data.stat.2.list))
# there will appear the error "Fehler in RG.data.stat.2.list[[i]] <- as.data.frame(cbind(plot_ID = c(my.plot.id),  
# :  Versuch weniger als ein Element aus integerOneIndex zu wählen" when the RG_loc_info doesn´t have any RG circles with the 
# status 2 



#  2.3.5. clearing tree data and prepare for export (beab) ---------------------------------------------------------------------------------------
# after this step there are only RG plants remaining which are locate in inventorable and processable CCS
# generally there are two reasons why an RG tree could be excluded from the processing. 
# 1. the RG is located in a plot that was removed
# 2. the RG is located in a sampling cirlce that was removed
# 3. the RG doesn´t have a size class or species ?? this should actually not be possible due to the plausi check
RG_removed <- 
  plyr::rbind.fill(
    # RG items removed because plot is excluded
    RG_data %>% 
      semi_join(plots_to_exclude, by = "plot_ID") %>% 
      mutate(rem_reason = "whole plot excluded during inventory status sorting"),
    # RG plants in removed circles 
    RG_data %>% 
      # anti join those RG trees that were already "pulled in" by the previous semi join to avoid doubles in the dataset
      anti_join(plots_to_exclude, by = "plot_ID") %>% 
      # filter/ pull those RG trees from the RG_data that are located in circles with status 2 or 3
      semi_join(., RG_loc_info %>% filter(CCS_RG_inv_status != 1), by = c("plot_ID", "CCS_nr")) %>% 
      mutate(rem_reason = "RG circle excluded during inventory status sorting"))

RG_update_1 <- RG_data %>% 
  # select only RG plants in circles remove plots from dataset where non of the inventories was carried out at the NSI (BZE) inventory ("Ausfall") 
  anti_join(., RG_removed, by = c("plot_ID", "CCS_nr", "tree_ID"))







# 2.4. DW dataset --------------------------------------------------------------------------------------------------------------------------------
# 2.4.1. remove not process able plots and sampling circuits form DW_inv_info data set ------------------------------------------------------------
DW_CCS_to_exclude <- plyr::rbind.fill(
  DW_inv_info %>% # remove plots from dataset where non of the inventories was carried out at the NSI (BZE) inventory ("Ausfall") 
    semi_join(., plots_to_exclude, by = "plot_ID") %>% 
    mutate(rem_reason = "whole plot excluded during inventory status sorting"),
  DW_inv_info %>% 
    # anti join dw cirlces where whole plot was removed since we already pulled them in right above
    anti_join(., plots_to_exclude, by = "plot_ID") %>% 
    # remove plots where one of the four sampling circuits was not inventorable: so status -9. -1, 4
    filter(!(CCS_DW_inv_status %in% c(1,2, 4, 5))) %>% 
    distinct() %>% 
    mutate(rem_reason = "DW circle excluded during inventory status sorting")) %>% 
  distinct()




# 2.4.2. create dataset with CCS that are not inventorable ------------------------------------------------------------
DW_inv_info <- DW_inv_info %>% 
  # remove plots from dataset where non of the inventories was carried out at the NSI (BZE) inventory ("Ausfall") 
  anti_join(., DW_CCS_to_exclude, by = c("plot_ID", "inv_year", "inv")) %>% 
  mutate(plot_A_ha = case_when(CCS_DW_inv_status == 4 ~ (c_A(data_circle$r0[2])/10000)*0.5, 
                               CCS_DW_inv_status == 5 ~ (c_A(data_circle$r0[2])/10000)*0.25,
                               TRUE ~  (c_A(data_circle$r0[2])/10000)))





# 2.4.4. creating empty plots for circuits labelled empty ------------------------------------------------------------------
# here i create a dataset with DW plots that have status 2 
# which only contains info we can catually give so the plot area , the plot ID and the stocks which are set to 0
DW_stat_2 <- DW_inv_info[DW_inv_info$CCS_DW_inv_status == 2, ]
DW.data.stat.2.list <- vector("list")
for (i in 1:nrow(DW_stat_2)) {
  # i = 1
  my.plot.id <- DW_stat_2[, "plot_ID"][i]
  my.plot.area <- DW_stat_2[, "plot_A_ha"][i]
  my.inv.year <- DW_stat_2[, "inv_year"][i]
  
  
  if(nrow(DW_stat_2) != 0){
    DW.status.2.df <- as.data.frame(cbind(
      plot_ID = c(my.plot.id),
      plot_A_ha = c(my.plot.area), 
      inv_year = c(my.inv.year),
      compartiment = c("ag", "bg", "total"),
      B_t_ha = c(0, 0, 0), 
      C_t_ha = c(0, 0, 0), 
      N_t_ha = c(0, 0, 0)))
  }else{
    DW.status.2.df = as.data.frame(cbind(
      plot_ID = NA,
      plot_A_ha = NA, 
      inv_year = NA,
      compartiment = NA,
      B_t_ha = NA, 
      C_t_ha = NA, 
      N_t_ha = NA))
  }
  
  try(DW.data.stat.2.list[[i]] <- DW.status.2.df, silent = T)
}
DW_data_stat_2 <- as.data.frame(rbindlist(DW.data.stat.2.list))


# 2.4.5. prepare DW_data for export ---------------------------------------
DW_removed <-  
  plyr::rbind.fill(
    ## DW trees from removed plots or removed CCS 
    # use DW_CCS_to remove, to "pull out" the trees from DW data that should be removed
    # while keeping the reason for the removal at the same time
    DW_data %>% 
      semi_join( 
        DW_CCS_to_exclude %>% 
          select(plot_ID, inv, inv_year), 
        by = c("plot_ID", "inv", "inv_year")) %>% 
      # join in reason for removal
      left_join(DW_CCS_to_exclude %>% 
                  select(plot_ID, inv, inv_year, rem_reason), 
                by = c("plot_ID", "inv", "inv_year")) ,
    ## DW trees with CCS status 3, -9, -1, 2 
    DW_data %>% 
      # make sure that trees pulled in by the removed DW circles are nt selected double 
      anti_join(DW_CCS_to_exclude,  by = c("plot_ID", "inv", "inv_year")) %>% 
      # select trees in CCS with status 3,  -9, -1 , 2, 
      # even though status 2 doesn´t count as being removed, we list it here, since status 2 CCS are not supposed to have DW items anyways 
      semi_join(., DW_inv_info %>% 
                  filter(!(CCS_DW_inv_status %in% c(1, 4, 5))),
                by = c("plot_ID", "inv", "inv_year")) %>% 
      mutate(rem_reason = "DW circle excluded during inventory status sorting"),
    ## trees with na , -9 or -2 in one of their variables that are relevant for processing 
    DW_data %>% 
      # make sure that trees pulled in by the wrong length are not selected double cause they were allready removed in the steps before 
      anti_join(., DW_data %>% 
                  semi_join( 
                    DW_CCS_to_exclude %>% 
                      select(plot_ID, inv, inv_year), 
                    by = c("plot_ID", "inv", "inv_year")) %>% 
                  # join in reason for removal
                  left_join(DW_CCS_to_exclude %>% 
                              select(plot_ID, inv, inv_year, rem_reason), 
                            by = c("plot_ID", "inv", "inv_year")) ,
                DW_data %>% 
                  # make sure that trees pulled in by the removed DW circles are nt selected double 
                  anti_join(DW_CCS_to_exclude,  by = c("plot_ID", "inv", "inv_year")) %>% 
                  # select trees in CCS with status 3,  -9, -1 , 2, 
                  # even though status 2 doesn´t count as being removed, we list it here, since status 2 CCS are not supposed to have DW items anyways 
                  semi_join(., DW_inv_info %>% 
                              filter(!(CCS_DW_inv_status %in% c(1, 4, 5))),
                            by = c("plot_ID", "inv", "inv_year")),
                by = (c("plot_ID", "inv", "inv_year"))) %>% 
      filter(if_any(c( "tree_ID", "dw_type", "dw_sp", "d_cm", "l_dm", "decay"), ~ is.na(.x) | .x <0 )) %>% 
      mutate(rem_reason = "single DW removed")
  ) # close rbindfill

# select only DW items in CCS with status 1, 4, 5 because status 2 CCSs we have in a separate dataset 
DW_update_1 <- DW_data %>% 
  # remove DW trees from removed plots, removed CCS, or sinlge removed trees because of flaws in their processing-relevant variables
   anti_join(.,DW_removed, by = c("plot_ID", "inv", "inv_year", "tree_ID")) %>% 
  # join in the plot area for the deadwood 
  left_join(., DW_inv_info %>% select(plot_ID, inv, inv_year, plot_A_ha),
            by = c("plot_ID", "inv", "inv_year"))



# 3. export dataset --------------------------------------------------------------------------------------------------------------
# deadwood
write.csv(DW_inv_info, paste0(out.path.BZE3, paste(unique(DW_inv_info$inv)[1], "DW_inv_update_1", sep = "_"), ".csv"), row.names = FALSE)
write.csv(DW_update_1, paste0(out.path.BZE3, paste(unique(DW_update_1$inv)[1], "DW_update_1", sep = "_"), ".csv"), row.names = FALSE)
write.csv(DW_data_stat_2, paste0(out.path.BZE3, paste(unique(DW_inv_info$inv)[1], "DW_stat_2", sep = "_"), ".csv"), row.names = FALSE)
write.csv(DW_CCS_to_exclude, paste0(out.path.BZE3, paste(unique(DW_inv_info$inv)[1], "DW_circles_removed", sep = "_"), ".csv"), row.names = FALSE)
write.csv(DW_removed, paste0(out.path.BZE3, paste(unique(DW_inv_info$inv)[1], "DW_removed", sep = "_"), ".csv"), row.names = FALSE)



# living trees
write.csv(tree_inv_info, paste0(out.path.BZE3, paste(unique(tree_inv_info$inv)[1], "LT_inv_update_1", sep = "_"), ".csv"), row.names = FALSE)
write.csv(LT_data_stat_2, paste0(out.path.BZE3, paste(unique(tree_inv_info$inv)[1], "LT_stat_2", sep = "_"), ".csv"), row.names = FALSE)
write.csv(trees_update_0, paste0(out.path.BZE3, paste(unique(trees_update_0$inv)[1], "LT_update_0", sep = "_"), ".csv"), row.names = FALSE)
write.csv(LT_CCS_to_exclude, paste0(out.path.BZE3, paste(unique(tree_inv_info$inv)[1], "LT_circles_removed", sep = "_"), ".csv"), row.names = FALSE)
write.csv(trees_removed, paste0(out.path.BZE3, paste(unique(tree_inv_info$inv)[1], "LT_removed", sep = "_"), ".csv"), row.names = FALSE)

# regeneration
write.csv(RG_loc_info, paste0(out.path.BZE3, paste(unique(RG_loc_info$inv)[1], "RG_loc_update_1", sep = "_"), ".csv"), row.names = FALSE)
write.csv(RG_data_stat_2, paste0(out.path.BZE3, paste(unique(RG_loc_info$inv)[1], "RG_stat_2", sep = "_"), ".csv"), row.names = FALSE)
write.csv(RG_update_1, paste0(out.path.BZE3, paste(unique(RG_update_1$inv)[1], "RG_update_1", sep = "_"), ".csv"), row.names = FALSE)
write.csv(RG_removed, paste0(out.path.BZE3, paste(unique(RG_loc_info$inv)[1], "RG_removed", sep = "_"), ".csv"), row.names = FALSE)
write.csv(RG_CCS_to_exclude, paste0(out.path.BZE3, paste(unique(RG_loc_info$inv)[1], "RG_circles_removed", sep = "_"), ".csv"), row.names = FALSE)

# all trees
# this we just export so the inventory name and year are in the dataset and we don´t have to 
# extract the date in the next data processing steps
write.csv(inv_info, paste0(out.path.BZE3, paste(unique(inv_info$inv)[1], "inv_info", sep = "_"), ".csv"), row.names = FALSE)
write.csv(plots_to_exclude, paste0(out.path.BZE3, paste(unique(inv_info$inv)[1], "plots_to_exclude", sep = "_"), ".csv"), row.names = FALSE)

# forest edges 
write.csv(forest_edges_update_1, paste0(out.path.BZE3, paste(unique(tree_inv_info$inv)[1], "forest_edges_update_1", sep = "_"), ".csv"), row.names = FALSE)
write.csv(forest_edges_removed, paste0(out.path.BZE3, paste(unique(tree_inv_info$inv)[1], "forest_edges_removed", sep = "_"), ".csv"), row.names = FALSE)


# NFI trees/ BWI trees
write.csv(trees_BWI, paste0(out.path.BZE3, paste(unique(tree_inv_info$inv)[1], "trees_BWI", sep = "_"), ".csv"), row.names = FALSE)







stop("Notes of inventory status sorting hbi start here")

# NOTES -------------------------------------------------------------------

# N.1. in case that data arrives in BZE3 format not BZE erfassungs --------

forest_edges <- read.delim(file = here("data/input/BZE2_HBI/be.csv"), sep = ",", dec = ",") %>% 
  # select only forest edge relevant column
  select(bund_nr,randtyp_1 , randtyp_2, randform_1 , randform_2, anfang_dist_1, anfang_dist_2, anfang_azi_1, anfang_azi_2, end_dist_1, end_dist_2, 
         end_azi_1, end_azi_2,  knick_dist_1, knick_dist_2, knick_azi_1, knick_azi_2) %>% 
  # pivoting edge 1 and two into same column and establisch edge ID: https://stackoverflow.com/questions/70700654/pivot-longer-with-names-pattern-and-pairs-of-columns
  to_long(keys = c("e_ID", "e_form_name", "A_dist_name", "A_azi_name", "B_dist_name", "B_azi_name", "T_dist_name", "T_azi_name"), 
          values = c("e_type", "e_form", "A_dist", "A_azi", "B_dist", "B_azi", "T_dist", "T_azi"),  
          names(.)[2:3], names(.)[4:5], names(.)[6:7], names(.)[8:9], names(.)[10:11], names(.)[12:13], names(.)[14:15], names(.)[16:17]) %>% 
  # remove unecessary name columns: https://stackoverflow.com/questions/15666226/how-to-drop-columns-by-name-pattern-in-r
  select(-contains("name")) %>% 
  # introduce edge ID by selecting only last letter from "randform_1", "randform_2":https://stackoverflow.com/questions/7963898/extracting-the-last-n-characters-from-a-string-in-r
  mutate(e_ID = str_sub(e_ID, start= -1)) %>% 
  distinct() %>%
  # select only plots that have an edge
  filter(!is.na(e_form)) %>% 
  rename("plot_ID" = "bund_nr")



# N.1.1. for BZE3 inventory data: import old data for removed trees to process them --------
# 1.2.1. prepare tree data: add old data to removed trees for stock calculations later ----------------------------------------------------------------
# there may be trees that are labelled as "lost" (removed or died of but for further processing we) by their tree inventory status and by that do not have 
# we still need their data from the previous inventory to calcualte their sampling circuit and assing ther species groups etc. 
#  dist_cm azi_gon age age_meth SP_code Kraft C_layer H_dm C_h_dm D_mm 
BZE3_trees_old_data <- trees_data %>%
  # selectonly trees with inventory status indicating the tree was lost or the status is unknown (eversthing other then 0 or 1)
  filter(!(tree_inventory_status %in% c(0,1)))%>% 
  # then filter for those rows that do not contain the old data of the respective tree that was passed on from the previous inventory
  # https://stackoverflow.com/questions/33520854/filtering-data-frame-based-on-na-on-multiple-columns
  filter_at(vars(multi_stem:DBH_h_cm),any_vars(is.na(.))) %>% 
  #replace cells with NA by values from HBI for the respective plot_ID und tree_ID: https://stackoverflow.com/questions/32899580/update-rows-of-data-frame-in-r
  rows_patch(trees_HBI %>% 
               # filter for HBI trees that have the same plot and tree ID as those trees in BZE3 with 
               # status != c(0, 1) and no old data passed on from HBI by semi_join 
               semi_join(., 
                         trees_data %>%
                           filter(!(tree_inventory_status %in% c(0,1)))%>% 
                           filter_at(vars(multi_stem:DBH_h_cm),any_vars(is.na(.))), 
                         by = c("plot_ID", "tree_ID")) %>% 
               select(-tree_inventory_status), 
             by = c("plot_ID", "tree_ID")) 

# add trees with "removed" status for which old data was found in HBI to BZE3 trees_data
trees_data <- 
  trees_data %>% 
  # 1. remove trees that have a tree_inv_stat indicates they were lost from the trees dataset but have replacement data in 
  anti_join(., BZE3_trees_old_data, by = c("plot_ID", "tree_ID")) %>% 
  # 2. add trees that have a "tree was lost" inventory status in BZE3 but have data in: https://dplyr.tidyverse.org/reference/rows.html 
  rows_insert(., BZE3_trees_old_data, by = c("plot_ID", "tree_ID")) %>% 
  arrange(plot_ID, tree_ID)





# n.2. correct status 2 to status 1 for CCS that have trees while their CCS status tells otherwise  --------
# we had to remove this part because some inventories keept the empty CCS in their dataset, giving the max 10 plants numbers (treeIDs) but 
# leaving their other data "empty" which caused R to count these CCS as "not empty" as they had a tree count and changed the CCS status to 1 
# tho it should remain CCS_status 2

# n.2.1. living trees -----------------------------------------------------

# this was meant to correct the CCS status 2 in case the CCS was labbelled 2 but does have trees, but we canot ensure our 
# assumtions are correct, thus we will remove this part and leave it to the plausbility tests
tree_inv_info <- tree_inv_info %>% 
  # joining in tree inventory info with only status 2 CCS 
  # in an optimal case they should not find any matches for circuits with the CCS_LT_inv_status == 2 in the tree dataset 
  # cause trees should have not been assessed for circuits that were labelled as empty by assiging status 2 
  left_join(.,trees_data , by = c("plot_ID", "CCS_r_m", "inv_year", "inv"), 
            multiple = "all") %>%
  # remove plots from dataset where non of the inventories was carried out at the NSI (BZE) inventory ("Ausfall") 
  anti_join(., plots_to_exclude, by = "plot_ID") %>% 
  # assign new inventory status to those circuits that were lablled 2 but still have trees inside that fit the 
  # requirements to be in the respective sampling circle, so trees that have a diameter and/ or distance that fits into circuit 3 (17.84 m radius) 
  # tho the cricuit is lablled "empty" by the status 2 
  # remove plots where one of the three sampling circuits was not inventorable (status == 3)
  anti_join(., LT_CCS_to_exclude, by = c("plot_ID", "CCS_r_m", "inv_year", "inv"))%>% 
  mutate(CCS_LT_inv_status_new = case_when(
    CCS_LT_inv_status == 2 & CCS_r_m == 17.84 & DBH_cm >= 30 |
      CCS_LT_inv_status == 2 & CCS_r_m == 17.84 & dist_cm >= 12.62 |
      CCS_LT_inv_status == 2 & CCS_r_m == 17.84 & dist_cm  >= 12.62 & DBH_cm >= 30 | 
      CCS_LT_inv_status == 2 & CCS_r_m == 12.62 & DBH_cm >= 10 & DBH_cm < 30  |
      CCS_LT_inv_status == 2 & CCS_r_m == 5.64 & DBH_cm >= 7  & DBH_cm < 10  ~ 1,
    TRUE ~ CCS_LT_inv_status
  )) %>% 
  # change name of old inventory status to "..._old"
  rename("CCS_LT_inv_status_old" = "CCS_LT_inv_status") %>% 
  # change name of new iventory status to plain "inventory status" without "new"
  rename("CCS_LT_inv_status" = "CCS_LT_inv_status_new")  %>% 
  # create new tree_inv_info dataset with new and old tree sampling circuit status to export as update 
  select(plot_ID, team, date, stand_spec, stand_type, structure, inv_year, inv, CCS_r_m, CCS_LT_inv_status, CCS_LT_inv_status_old) %>% 
  distinct()%>% 
  arrange(plot_ID)



# n.2.2. regeneration -----------------------------------------------------

RG_loc_info <-  RG_loc_info%>%
  # join  Plant data into RG_lock info 
  ###change_back_later : here is an issue with the species codes .... they are somehow -2 but they have IDs
  ##change_back_later : there are plot_IDs with NA but still have trees 
  left_join(., RG_data %>% 
              mutate(across(c("plot_ID", "CCS_nr", "inv_year"), as.numeric)), by = c("plot_ID", "CCS_nr", "inv_year", "inv"), 
            multiple = "all") %>% 
  # exclude CCS with status 3
  anti_join(., RG_CCS_to_exclude, by = c("plot_ID", "CCS_nr", "inv_year", "inv")) %>% 
  # remove plots from dataset where non of the inventories was carried out at the NSI (BZE) inventory ("Ausfall") 
  anti_join(., plots_to_exclude, by = "plot_ID") %>% 
  # if there are trees that match a plot and sampling circuit that is actually not supposed to be in the list 
  # because its labelled "empty" we have to change the circuit to inv_status 1 
  # we test this by looking for circuits with the label "2" that have tree_IDs that are not na (because the tree columns joined into empty 
  # CCS should actually be "empty/ NA")
  mutate(CCS_RG_inv_status_new = case_when(
    CCS_RG_inv_status == 2 & !is.na(tree_ID) | SP_code == -2 ~ 1, 
    TRUE ~ CCS_RG_inv_status)) %>% 
  # change name of old inventory status to "..._old"
  rename("CCS_RG_inv_status_old" = "CCS_RG_inv_status") %>% 
  # change name of new iventory status to plain "inventory status" without "new"
  rename("CCS_RG_inv_status" = "CCS_RG_inv_status_new") %>% 
  select("plot_ID", "CCS_nr", "CCS_position", "CCS_dist" , "CCS_RG_inv_status", "CCS_RG_inv_status_old", 
         "CCS_max_dist_cm", "inv_year", "inv") %>% 
  distinct()%>% 
  arrange(plot_ID, CCS_nr) %>% 
  # change the maximum distance to the default setting of 500cm if its NA or -9
  mutate(CCS_max_dist_cm = ifelse(is.na(CCS_max_dist_cm) | CCS_max_dist_cm == -9, 500, CCS_max_dist_cm))

# N.2.4.3. correcting status 2 circles that actually have trees ------------------------------------------------------------------
DW_inv_info <-  DW_inv_info%>%
  # join  DW_data  to DW_inv_info
  left_join(., DW_data , by = c("plot_ID", "inv_year", "inv"), multiple = "all") %>% 
  # remove plots from dataset where non of the inventories was carried out at the NSI (BZE) inventory ("Ausfall") 
  anti_join(., plots_to_exclude, by = "plot_ID") %>% 
  # remove CCS with inv status 3
  anti_join(., DW_CCS_to_exclude, by = c("plot_ID", "inv_year", "inv")) %>% 
  # if there are trees that match a plot and sampling circuit that is actually not supposed to be in the list 
  # because its labelled "empty"
  mutate(CCS_DW_inv_status_new = case_when(
    CCS_DW_inv_status == 2  & !is.na(tree_ID) ~ 1, 
    TRUE ~ CCS_DW_inv_status)) %>% 
  # change name of old inventory status to "..._old"
  rename("CCS_DW_inv_status_old" = "CCS_DW_inv_status") %>% 
  # change name of new iventory status to plain "inventory status" without "new"
  rename("CCS_DW_inv_status" = "CCS_DW_inv_status_new") %>%
  select("plot_ID", "inv", "inv_year", "CCS_DW_inv_status", "CCS_DW_inv_status_old", "dist_cm", "azi", "plot_A_ha") %>% 
  distinct()



