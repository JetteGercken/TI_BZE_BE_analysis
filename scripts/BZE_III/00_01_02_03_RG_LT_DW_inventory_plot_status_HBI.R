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







