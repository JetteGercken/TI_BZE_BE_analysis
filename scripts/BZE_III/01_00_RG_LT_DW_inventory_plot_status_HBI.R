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
RG_loc_info <-  read.delim(file = here(paste0(out.path.BZE3, "HBI_RG_loc_update_1.csv")), sep = ";", dec = ",") %>% 
  rename(CCS_RG_inv_status = plot_inventory_status ) # bej
  
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
  filter("CCS_5_inv_status" != 3 | "CCS_12_inv_status" != 3 | "CCS_17_inv_status" !=3)

# 2.2. RG dataset ------------------------------------------------------------
RG_loc_info <- RG_loc_info %>% 
  # join  in inventory info
  left_join(., HBI_inv_info %>% select(plot_ID, inv_year, inv), by = "plot_ID") %>% 
  # remove plots from dataset where non of the inventories was carried out at the NSI (BZE) inventory ("Ausfall") 
  anti_join(., HBI_plots_to_exclude, by = "plot_ID") %>% 
  # remove plots where one of the four sampling circuits was not inventorable
  filter("CCS_RG_inv_status" != 3)



# 2.3. DW dataset ------------------------------------------------------------
DW_inv_info <- DW_inv_info %>% 
  left_join(HBI_inv_info %>% select(plot_ID, inv, inv_year), by = "plot_ID") %>% 
  # remove plots from dataset where non of the inventories was carried out at the NSI (BZE) inventory ("Ausfall") 
  anti_join(., HBI_plots_to_exclude, by = "plot_ID") %>% 
  # remove plots where the DW sampling circuit not inventorable
  filter("CCS_DW_inv_status" != 3) %>% 
  mutate(plot_A_ha = case_when(CCS_DW_inv_status == 4 ~ (c_A(data_circle$r0[2])/10000)*0.5, 
                               CCS_DW_inv_status == 5 ~ (c_A(data_circle$r0[2])/10000)*0.25,
                               TRUE ~  (c_A(data_circle$r0[2])/10000) ))

# I need to find a way how to deal with plots that have status 2: 
  # these plots will be in bedw
  # but they will not be in the dw inventory itself
  # however, I´ll have to include them in the biomass hectare calculations which means in script 05_00 
  # here I´ll need to set the area of the circuit to 12.62^2*pi and the biomass to 0 t/ha
  # the plots with status 2 will, however most likely not run through the inventory data
# so I´ll create a dataset that holds only stock data which i can bind to the other stock data later in 05_00
DW_update_1 <- DW_data %>% 
  semi_join(., DW_inv_info %>% select(plot_ID), by = "plot_ID") %>% 
  left_join(., DW_inv_info %>% select(plot_ID, inv, inv_year, CCS_DW_inv_status, plot_A_ha), by = "plot_ID")

# here i create a dataset with DW plots that have status 2 
# which only contains info we can catually give so the plot area , the plot ID and the stocks which are set to 0

DW_data_stat_2 <- DW_inv_info[DW_inv_info$CCS_DW_inv_status == 2, ]
DW.data.stat.2.list <- vector("list", length = nrow(DW_data_stat_2))
for (i in DW_data_stat_2) {
  my.plot.id <- DW_data_stat_2[, "plot_ID"][i]
  my.plot.area <- DW_data_stat_2[, "plot_A_ha"][i]
  my.inv.year <- DW_data_stat_2[, "inv_year"][i]
  
  cbind(
    plot_ID = c(my.plot.id),
  )
  
  
}
plot_ID CCS_A_ha inv_year compartiment    B_t_ha   C_t_ha     N_t_ha



# 3. export dataset -------------------------------------------------------
write.csv2(DW_inv_info, paste0(out.path.BZE3, paste(unique(DW_inv_info$inv)[1], "DW_inv_update_1", sep = "_"), ".csv"))
write.csv2(DW_update_1, paste0(out.path.BZE3, paste(unique(DW_update_1$inv)[1], "DW_update_1", sep = "_"), ".csv"))












