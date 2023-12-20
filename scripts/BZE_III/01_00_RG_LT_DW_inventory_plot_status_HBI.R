# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the peat land soil inventory
# sorting the sampling circuits according to their inventory status 

# this script should sort every sampling circle, may it be a living trees plot, 
# a regeneration plot or a deadwood plot 

# the inventory status code displays the following information: 
## living trees 
# Plotstatus der Bestandeserhebung
    # -9 = Merkmal vergessen, nicht rekonstruierbar oder unbekannt - Status is unknown, was not assesed or canot be reconstructed
    # -1 = Merkmal nicht erhoben  - status was not assessed
    #  1 = Aufnahme erfolgte am HBI-Mittelpunkt  - the plot is at the same position as an HBI plot --> repetitive inventory
    #  2 = Aufnahme erfolgte an neuem Bezugspunkt - plot in not at the same posticion as in the previous invenotry --> new inventory
    #  3 = Aufnahme erfolgte an BWI-Punkt (nur BB/BY) - the plot is at the same position as an BWI plot --> repetitive inventory
   ## plot stati to exclude: 3 
   ## plot stati to change:

# status der Verjüngungsaufnahme
    # -9 Merkmal vergessen, nicht rekonstruierbar oder unbekannt
    # -2 Merkmal nicht vorhanden
    # -1 Merkmal nicht erhoben
    #  1 Aufnahme wurde erfolgreich durchgeführt
    #  2 Aufnahme war nicht möglich, keine Objekte vorhanden
    #  3 Aufnahme war nicht möglich, sonst. Gründe (Störung etc.)
  ## plot stati to exclude: 3 
  ## plot stati to change: 


# status der Totholzaufnahme
    # -9 Merkmal vergessen, nicht rekonstruierbar oder unbekannt
    # -1 Merkmal nicht erhoben
    # 1 Aufnahme wurde erfolgreich durchgeführt
    # 2 Aufnahme war nicht möglich, keine Objekte vorhanden
    # 3 Aufnahme war nicht möglich, sonst. Gründe (Störung etc.)
    # 4 Aufnahme auf 0,5 der Probekreisfläche
    # 5 Aufnahme auf 0,25 der Probekreisfläche
   ## plot stati to exclude: 3
   ## plot stati to change:

# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the peat land soil inventory
# Functions & require


# ----- 0.1. packages and functions --------------------------------------------
source(paste0(getwd(), "/scripts/00_00_functions_library.R"))

# ----- 0.2. working directory -------------------------------------------------
here::here()
getwd()

out.path.BZE3 <- ("output/out_data/out_data_BZE/") 

# ----- 0.4 importing data -----------------------------------------------------
# this dataset contains the BZE file tit_1 which display< info about the BZE inventory in general
# so info that´s base of all sub. interniry
HBI_inv_info <- read.delim(file = here("data/input/BZE2_HBI/tit_1.csv"), sep = ",", dec = ",", stringsAsFactors=FALSE) %>% select(-c("re_form", "re_lage", "neigung", "exposition", "anmerkung"))
colnames(HBI_inv_info) <- c("plot_ID", "team", "date", "plot_inv_status")
#BZE3_inv_info <- read.delim(file = here("data/input/BZE3/tit_1.csv"), sep = ",", dec = ",", stringsAsFactors=FALSE)

# create column that just contains year of inventory: https://www.geeksforgeeks.org/how-to-extract-year-from-date-in-r/
HBI_inv_info$date <- as.Date(HBI_inv_info$date)
HBI_inv_info$inv_year <- as.numeric(format(HBI_inv_info$date, "%Y"))
# this line can be removed later
HBI_inv_info <- HBI_inv_info %>% mutate(inv_year = ifelse(inv_year < 2012, 2012,inv_year), 
                                        inv = inv_name(inv_year))



# living trees
tree_inv_info <-  read.delim(file = here("data/input/BZE2_HBI/be.csv"), sep = ",", dec = ",", stringsAsFactors=FALSE) %>% # be
  select(-c(geraet,randtyp_1,randform_1,anfang_dist_1,anfang_azi_1,end_dist_1,end_azi_1,
            knick_dist_1,knick_azi_1,randtyp_2,randform_2,anfang_dist_2,anfang_azi_2,end_dist_2,
            end_azi_2,knick_dist_2,knick_azi_2, anmerkung, schlussgrad_schi1, schlussgrad_schi2, mischung)) 
colnames(tree_inv_info) <- c("plot_ID", "team", "date", "stand_spec", "stand_type", "structure", "CCS_5_inv_status",  "CCS_12_inv_status",  "CCS_17_inv_status")

# regeneration inventory info
# regeneration                                                                                                   inv = inv_name(inv_year))
# this dataset contains the position and extend of the sampling circle satelites of the regeneration inventory of the HBI (BZE2) 
RG_loc_info <- read.delim(file = here("data/input/BZE2_HBI/bej.csv"), sep = ",", dec = ",", stringsAsFactors=FALSE)
# assign column names    # bund_nr     pk_nr      pk_richtung     pk_dist     pk_aufnahme      pk_maxdist
colnames(RG_loc_info) <- c("plot_ID", "CCS_nr", "CCS_position",  "CCS_dist", "CCS_RG_inv_status", "CCS_max_dist_cm")

##DEADWOOD
# deadwood inventory info 
DW_inv_info <- read.delim(file = here("data/input/BZE2_HBI/bedw.csv"), sep = ",", dec = ",", stringsAsFactors=FALSE)  
colnames(DW_inv_info) <- c("plot_ID", "CCS_DW_inv_status",  "dist_cm", "azi")

DW_data <- read.delim(file = here("data/input/BZE2_HBI/bedw_liste.csv"), sep = ",", dec = ",")
#  bund_nr lfd_nr t     yp      baumgruppe anzahl  durchmesser laenge zersetzung
colnames(DW_data) <- c("plot_ID", "tree_ID", "dw_type", "dw_sp", "count", "d_cm", "l_dm", "decay")
# join inventory jear and name into deadwood tree dataset




# 1. create a list with the BZE plots that should be excluded -----------------
# select plots that have a "Punktstatus (x_plotstatus_bze)" 
HBI_plots_to_exclude <- HBI_inv_info %>% 
  filter(plot_inv_status >= 21) %>% 
  select(plot_ID)


# 2. remove not processable plots from LT, RG and DW ----------------------------
# 2.1. trees dataset ------------------------------------------------------------
tree_inv_info <- tree_inv_info %>% 
  # join  in inventory info
  left_join(., HBI_inv_info %>% select(plot_ID, inv_year, inv), by = "plot_ID") %>% 
# remove plots from dataset where non of the inventories was carried out at the NSI (BZE) inventory ("Ausfall") 
  anti_join(., HBI_plots_to_exclude, by = "plot_ID") %>% 
# remove plots where one of the three sampling circuits was not inventorable
  filter("CCS_5_inv_status" != 3 | "CCS_12_inv_status" != 3 | "CCS_17_inv_status" !=3) %>% 
  # pivoting B, C: https://stackoverflow.com/questions/70700654/pivot-longer-with-names-pattern-and-pairs-of-columns
 pivot_longer(., "CCS_5_inv_status":"CCS_17_inv_status", names_to = "CCS_r_m", values_to = "CCS_LT_inv_status") %>% 
  mutate(CCS_r_m = as.numeric(case_when(CCS_r_m == "CCS_5_inv_status" ~ 5.64, 
                             CCS_r_m == "CCS_12_inv_status" ~ 12.62,
                             CCS_r_m == "CCS_17_inv_status" ~ 17.84,
                             TRUE~ NA))) %>% 
  distinct() %>% 
  arrange(plot_ID)


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
  
  
  LT.data.stat.2.list[[i]] <- as.data.frame(cbind(
    plot_ID = c(my.plot.id),
    CCS_r_m = c(my.ccs.r),
    plot_A_ha = c(my.plot.area), 
    inv_year = c(my.inv.year),
    compartiment = c("ag", "bg", "total"),
    B_CCS_t_ha = c(0, 0, 0), 
    C_CCS_t_ha = c(0, 0, 0), 
    N_CCS_t_ha = c(0, 0, 0)))
}
LT_data_stat_2 <- as.data.frame(rbindlist(LT.data.stat.2.list))





# 2.2. RG dataset ------------------------------------------------------------
RG_loc_info <- RG_loc_info %>% 
  # join  in inventory info
  left_join(., HBI_inv_info %>% select(plot_ID, inv_year, inv), by = "plot_ID") %>% 
  # remove plots from dataset where non of the inventories was carried out at the NSI (BZE) inventory ("Ausfall") 
  anti_join(., HBI_plots_to_exclude, by = "plot_ID") %>% 
  # remove plots where one of the four sampling circuits was not inventorable
  filter("CCS_RG_inv_status" != 3)


# here i create a dataset with RG plots that have status 2 
# which only contains info we can catually give so the plot area , the plot ID and the stocks which are set to 0
RG_stat_2 <- RG_loc_info[RG_loc_info$CCS_RW_inv_status == 2, ]
RG.data.stat.2.list <- vector("list", length = nrow(RG_stat_2))
for (i in 1:nrow(RG_stat_2)) {
  # i = 1
  my.plot.id <- RG_stat_2[, "plot_ID"][i]
  my.ccs.no <- RG_stat_2[, "CCS_nr"][i]
  my.plot.area <- c_A(as.numeric(RG_stat_2[, "CCS_max_dist_cm"][i])/100)
  my.inv.year <- RG_stat_2[, "inv_year"][i]
  
  RG.data.stat.2.list[[i]] <- as.data.frame(cbind(
    plot_ID = c(my.plot.id),
    CCS_no = c(my.ccs.no),
    plot_A_ha = c(my.plot.area), 
    inv_year = c(my.inv.year),
    compartiment = c("ag", "bg", "total"),
    B_CCS_t_ha = c(0, 0, 0), 
    C_CCS_t_ha = c(0, 0, 0), 
    N_CCS_t_ha = c(0, 0, 0)))
}
RG_data_stat_2 <- as.data.frame(rbindlist(RG.data.stat.2.list))




# 2.3. DW dataset ------------------------------------------------------------
DW_inv_info <- DW_inv_info %>% 
  left_join(HBI_inv_info %>% select(plot_ID, inv, inv_year), by = "plot_ID") %>% 
  # remove plots from dataset where non of the inventories was carried out at the NSI (BZE) inventory ("Ausfall") 
  anti_join(., HBI_plots_to_exclude, by = "plot_ID") %>% 
  # remove plots where the DW sampling circuit not inventorable
  filter("CCS_DW_inv_status" != 3) %>% 
  mutate(plot_A_ha = case_when(CCS_DW_inv_status == 4 ~ (c_A(data_circle$r0[2])/10000)*0.5, 
                               CCS_DW_inv_status == 5 ~ (c_A(data_circle$r0[2])/10000)*0.25,
                               TRUE ~  (c_A(data_circle$r0[2])/10000)))


# here i create a dataset with DW plots that have status 2 
# which only contains info we can catually give so the plot area , the plot ID and the stocks which are set to 0
DW_stat_2 <- DW_inv_info[DW_inv_info$CCS_DW_inv_status == 2, ]
DW.data.stat.2.list <- vector("list", length = nrow(DW_stat_2))
for (i in 1:nrow(DW_stat_2)) {
  # i = 1
  my.plot.id <- DW_stat_2[, "plot_ID"][i]
  my.plot.area <- DW_stat_2[, "plot_A_ha"][i]
  my.inv.year <- DW_stat_2[, "inv_year"][i]
  
  DW.data.stat.2.list[[i]] <- as.data.frame(cbind(
    plot_ID = c(my.plot.id),
    plot_A_ha = c(my.plot.area), 
    inv_year = c(my.inv.year),
    compartiment = c("ag", "total"),
    B_CCS_t_ha = c(0, 0), 
    C_CCS_t_ha = c(0, 0), 
    N_CCS_t_ha = c(0, 0)))
}
DW_data_stat_2 <- as.data.frame(rbindlist(DW.data.stat.2.list))



DW_update_1 <- DW_data %>% 
  semi_join(., DW_inv_info %>% select(plot_ID), by = "plot_ID") %>% 
  left_join(., DW_inv_info %>% select(plot_ID, inv, inv_year, CCS_DW_inv_status, plot_A_ha), by = "plot_ID")




 



# 3. export dataset --------------------------------------------------------------------------------------------------------------
write.csv2(DW_inv_info, paste0(out.path.BZE3, paste(unique(DW_inv_info$inv)[1], "DW_inv_update_1", sep = "_"), ".csv"))
write.csv2(DW_update_1, paste0(out.path.BZE3, paste(unique(DW_update_1$inv)[1], "DW_update_1", sep = "_"), ".csv"))
write.csv2(DW_data_stat_2, paste0(out.path.BZE3, paste(unique(DW_inv_info$inv)[1], "DW_stat_2", sep = "_"), ".csv"))

write.csv2(tree_inv_info, paste0(out.path.BZE3, paste(unique(tree_inv_info$inv)[1], "LT_inv_update_1", sep = "_"), ".csv"))
write.csv2(LT_data_stat_2, paste0(out.path.BZE3, paste(unique(tree_inv_info$inv)[1], "LT_stat_2", sep = "_"), ".csv"))

write.csv2(RG_loc_info, paste0(out.path.BZE3, paste(unique(RG_loc_info$inv)[1], "RG_loc_update_1", sep = "_"), ".csv"))
write.csv2(RG_data_stat_2, paste0(out.path.BZE3, paste(unique(RG_loc_info$inv)[1], "RG_stat_2", sep = "_"), ".csv"))

write.csv2(HBI_inv_info, paste0(out.path.BZE3, paste(unique(HBI_inv_info$inv)[1], "inv_info", sep = "_"), ".csv"))







