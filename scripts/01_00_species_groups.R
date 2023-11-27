# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the peat land soil inventory
# Functions & require


# ----- 0. SETUP ---------------------------------------------------------------
# ----- 0.1. Packages  ---------------------------------------------------------
## datamanagement
# install.packages("usethis")
#  install.packages("here")
#  install.packages("readr")
#  install.packages("tidyverse")
#  install.packages("tibble")
#  install.packages("dplyr")
#  install.packages("data.table")
#  install.packages("broom")
#  install.packages("purrr")
#  install.packages("devtools")
#  ## laTex
#  install.packages("stargazer")  #for compatability with Latex
#  install.packages("tikzDevice") #for compatability with Latex#
#  # visualisation
#  install.packages("ggthemes")
#  install.packages("ggplot2")
#  install.packages("reshape2") #for multiple y values
#  install.packages("ggforce") #for zooming in parts of the plot
# install.packages("ggforce")             # Install ggforce package
#  options(tz="CA")
#  install.packages("reshape2")
#  # analysis
#  install.packages("corrplot")
#  install.packages("AICcmodavg")
#  # forest related
#   install.packages("forestmangr")
#  install.packages("rBDAT")
#  install.packages("TapeR")
# install.packages("pkgbuild")
#  require("devtools")
#  if (! require("remotes")) 
#    install.packages("remotes")
#  remotes::install_gitlab("vochr/TapeS", build_vignettes = TRUE)
# install.packages("magrittr")
# install.packages("sjmisc")
# if(!require(devtools)) install.packages("devtools")
# devtools::install_github("kassambara/ggcorrplot")
## spatial
# install.packages("sf")
# install.packages("rgdal")
# install.packages("terra")
#install.packages("sfheaders")


# ----- 0.2. require   ---------------------------------------------------------
# datamanagement
require(usethis)
require(here)
require(readr)
require(tidyverse)
require(tibble)
require(dplyr)
require(data.table)
require(broom)
require(purrr)
require(remotes)
require(devtools)
# laTex
require(stargazer)  #for compatability with Latex
require(tikzDevice) #for compatability with Latex
# visualisation
require(ggthemes)
require(ggplot2)
require(reshape2) #for multiple y values
require(ggforce) #for zooming in parts of the plot
options(tz="CA")
# analysis
require(corrplot)
require(AICcmodavg)
require(ggcorrplot)
# forest related
require(forestmangr)
require(rBDAT)
require(TapeR)
if (! require("remotes")) 
  install.packages("remotes")
require(remotes)
#devtools::install_gitlab("vochr/TapeS", build_vignettes = TRUE)
#remotes::install_gitlab("vochr/TapeS", build_vignettes = TRUE)
require(TapeS)
require(magrittr)
require(sjmisc)
require(ggforce)                      # Load ggforce package
# spatial 
require(sf)
require(rgdal)
require(terra)
require(sfheaders)

# ----- 0.3. working directory -------------------------------------------------
here::here()


# ----- 0.4 import species codes dataset  --------------------------
# species names & codes 
SP_names_com_ID_tapeS <- read.delim(file = here("output/out_data/x_bart_tapeS.csv"), sep = ",", dec = ",") 
# the join always works like this: 
# left_join(., SP_names_com_ID_tapeS %>% 
#             mutate(char_code_ger_lowcase = tolower(Chr_code_ger)), 
#           by = c("SP_code" = "char_code_ger_lowcase"))


# this table displaying the species codes and names used for the MoMoK forest inventory was extracted from the latest working paper published in the MoMok folder:  
# \\fswo01-ew\INSTITUT\a7forum\LEVEL I\BZE\Moormonitoring\Arbeitsanleitungen\MoMoK
# I´l use it to assign the latin names to the assessed speices to then use them in TapeR and BDAT 
SP_names <- read.delim(file = here("data/input/General/x_bart_neu.csv"), sep = ";", dec = ",") %>% 
  select(- c(anmerkung, beginn, ende)) %>% 
  # https://stackoverflow.com/questions/21003311/how-to-combine-multiple-character-columns-into-a-single-column-in-an-r-data-fram
  unite(bot_name, genus, species, sep = " ", remove = FALSE) %>%  # creating one column with complete botanic name
  mutate(bot_name = ifelse(bot_name == "-2 -2", -2, bot_name))   # the error codes are joined in one column too, which i don´t want, so I´ll keep them single
SP_TapeS <- TapeS::tprSpeciesCode(inSp = NULL, outSp = NULL)
SP_TapeS_test <- TapeS::tprSpeciesCode(inSp = NULL, outSp = NULL) #to test if species codes correspong between TapeS dataset and SP_names from BZE 

# ----- 1.2.2. species list BZE --------------------------------------------------------------
colnames(SP_names) <- c("Nr_code", "Chr_code_ger", "name", "bot_name", "bot_genus", 
                        "bot_species", "Flora_EU", "LH_NH", "IPC", "WZE", "BWI",  
                        "BZE_al")



# ----- 1.4.2. tree species -----------------------------------------
# Goal 1: assiging the correct latin name to the individual trees through SP_names dataset
# when trying to assign the correct latinn manes to the respective trees via their species code or species number 
# it became evident that neither the areviations of the common species names, nor the species numbers correspond
# further the species numbers are wronlgy assigned in the trees dataset
# the most coherent or common variables appears to be the species common names abbreviations in the SP_names and trees_total dataset
# though the character codes in the SP_names dataset are assessed in a mix of capital and not capital letters, 
# while all abbreviations of common species names in the trees_total dataset are all in capital letters
# thus, I´ll transform all the abbreviations of common species names in the SP_names dataset into capital letters, 
# to enable the join. Chr_ger_cap
# Goal 2: assingin the correct species codes from the TapeS SP file to trees_total to use TapeS later
# the most correspondent variable/ column between TapeS and SP_names, and by that trees_total, which can access & join all SP_names columns 
# but no or few tapeS_SP columns is the "BWI" column of SP_names and the "kurz" column of TapeS_SP when transformed into capital letters. 



# creating a dataset with the species names from x-bart and the accordint TapeS codes 
SP_names_com_ID_tapeS <- left_join(rbind(
  # selecting those rows in SP_names (x_bart) that have a match in "scientific" of TapeS 
  # and create column called com_ID That holds that scientific names that are common between TapeS and SP_names x_bart
  inner_join(SP_names, SP_TapeS_test %>% select(scientific), by = c("bot_name" = "scientific")) %>% 
    mutate(tpS_SP_com_name = bot_name), 
  # selecting those rows in SP_names (x_bart) that do not have a match in "scientific" of TapeS 
  anti_join(SP_names, SP_TapeS_test  %>% select(scientific), by = c("bot_name" = "scientific")) %>% 
    # create column in SP_names that corresponds with TapeS species
    # --> the changes are carried out according to the anti join between trees_total & TapeS species, not
    # the join between SP_codes from BZE and TapeSP, this has to be done later
    # every acer not campestre, etc. is assigned to Acer spp. (the other species do have a match in TapeS_SP)
    mutate(tpS_SP_com_name = case_when(bot_genus == "Abies" & !(bot_species %in% c("grandis", "alba")) | bot_genus == "abies …" & !(bot_species %in% c("grandis", "alba")) ~ "Abies alba",
                                       # all Larix not kaemperi & decidua are assigned to Larix spp.
                                       bot_genus == "Larix" & !(bot_species %in% c("decidua", "kaempferi")) ~ "Larix spp.",
                                       # all picea are allocated to Picea abies cause TapeS doesn´t distinguish
                                       bot_genus == "Picea"  ~ "Picea abies",
                                       # all Pinus not "nigra", "strobus" are assigned to Pinus sylvestris
                                       bot_genus == "Pinus" & !(bot_species %in% c("nigra", "strobus")) ~  "Pinus sylvestris",
                                       # there is a spelling mistake in x-Bart spelling Pseudotsuga menziestii with a t wich hampers the join with TapeS_SP
                                       bot_genus == "Pseudotsuga" ~ "Pseudotsuga menziesii", 
                                       # all thuja species (whcih x_bart doesnt distinguish anyways) are treated as Thuja plicata
                                       bot_genus == "Thuja" ~ "Thuja plicata",
                                       # all tsuga are treated as tsuga heterophyllia cause TapeS only has that species of the genus
                                       bot_genus == "Tsuga" ~ "Tsuga heterophylla",
                                       # everything else NH belongs to other coniferous trees
                                       LH_NH == "NB" & !(bot_genus %in% c("Abies","Larix", "Picea","Pinus", "Pseudotsuga", "Thuja", "Tsuga"))~ "Coniferales trees", 
                                       bot_genus == "Acer" & !(bot_species %in% c("campestre", "platanoides",  "pseudoplatanus", "spp.")) ~ "Acer spp.",
                                       bot_genus == "Alnus" ~ "Alnus spp.", 
                                       bot_genus == "Betula" ~ "Betula spp.", 
                                       # all Carpinus species are treated as Carpinus betulus
                                       bot_genus == "Carpinus" ~ "Carpinus betulus",
                                       # all fagus species are treated as Fagus sylvatica
                                       bot_genus == "Fagus" ~ "Fagus sylvatica", 
                                       # all Fraxinus species are treated as Fraxinus excelsior
                                       bot_genus == "Fraxinus" ~ "Fraxinus excelsior",
                                       #all Populus species except populus balsamifera are assigned to Populus spp. 
                                       bot_genus == "Populus" & bot_species != "balsamifera"  ~ "Populus spp.", 
                                       # all Prunus species are treated as Prunus avium
                                       bot_genus == "Prunus"  ~ "Prunus avium",
                                       #all Quercus species except rubra balsamifera are assigned to Quercus spp. 
                                       bot_genus == "Quercus" & bot_species != "rubra"  ~ "Quercus spp.",
                                       # all Salix species are allocated to Salix spp.
                                       bot_genus == "Salix"  ~ "Salix spp.",
                                       #all Sorbus species except torminalis are assigned to Sorbus aucuparia 
                                       bot_genus == "Sorbus" & bot_species != "torminalis"  ~ "Sorbus aucuparia",
                                       # all Tilia species are allocated to Tilia spp. cause TapeS doesnt distinguish between the species
                                       bot_genus == "Tilia"  ~ "Tilia spp.",
                                       # all Ulmus species are allocated to Ulmus spp. cause TapeS doesnt distinguish between the species
                                       bot_genus == "Ulmus"  ~  "Ulmus spp.",
                                       bot_name == '-2' ~ "missing", 
                                       # everything else belongs to other broadleafed trees
                                       TRUE ~ "Magnoliopsida trees"))), 
  SP_TapeS_test %>% select(scientific, ID) %>% rename(tpS_ID = ID), 
  by = c("tpS_SP_com_name" = "scientific")) %>% 
  # height species groups to assing correct parameters for height functions according to species
  # create species groups, BWI uses for their volume calculations to use curtis & sloboda functions
  # BWI Methodikband: 
  # für die Hoehenmessung wurde nach folgenden Baumartengruppen differenziert:
  # Fichte, Tanne, Douglasie, Kiefer, Lärche, Buche, Eiche. 
  # Alle anderen Nadelbäume werden der Fichte und alle anderen Laubbäume der Buche zugeordnet.
  mutate(H_SP_group = case_when(bot_genus == "Quercus"~ 'ei', 
                                LH_NH == "LB" & bot_genus != "Quercus" ~ 'bu', 
                                bot_genus == "Abies" ~ 'ta', 
                                bot_genus == "Pinus" ~ 'ki', 
                                bot_genus == "Pseudotsuga" ~ 'dgl',
                                LH_NH == "NB" & bot_genus == "Larix" ~ 'lae', 
                                TRUE ~ 'fi'),
         # BWI species groups according to Methodikband zur 3. Waldinventur 2012: ei, bu, aLh, aLn, ki, fi
         BWI_SP_group = case_when(LH_NH == "LB" & bot_genus == "Quercus"~ 'ei', 
                                  LH_NH == "LB" & bot_genus == "Fagus"~ 'bu',
                                  LH_NH == "LB" & bot_genus %in% c("Acer", 
                                                                   "Platanus", 
                                                                   "Fraxinus",
                                                                   "Tilia", 
                                                                   "Juglans", 
                                                                   "Corylus", 
                                                                   "Robinia", 
                                                                   "Castanea", 
                                                                   "Carpinus", 
                                                                   "Aesculus", 
                                                                   "Sorbus",
                                                                   "Ulmus", 
                                                                   "Rhamnus") | LH_NH == "LB" & bot_name == "Prunus dulcis" ~ 'aLh',
                                  LH_NH == "LB" & !(bot_genus %in% c("Quercus", 
                                                                     "Fagus",
                                                                     "Acer", 
                                                                     "Platanus", 
                                                                     "Fraxinus",
                                                                     "Tilia", 
                                                                     "Juglans", 
                                                                     "Corylus", 
                                                                     "Robinia", 
                                                                     "Castanea", 
                                                                     "Carpinus", 
                                                                     "Aesculus", 
                                                                     "Sorbus",
                                                                     "Ulmus", 
                                                                     "Rhamnus")) | LH_NH == "LB" & bot_name != "Prunus dulcis" ~ 'aLn',
                                  LH_NH == "NB" & bot_genus %in% c("Pinus", "Larix") ~ 'ki', 
                                  LH_NH == "NB" & !(bot_genus %in% c("Pinus", "Larix"))  ~ 'fi', 
                                  TRUE ~ 'other'), 
         # Biomass species groups to assgn the correct coefficients for the biomass functions of GHGI
         Bio_SP_group = case_when(LH_NH == "LB" & bot_genus == "Quercus"~ 'ei',
                                  # https://www.statology.org/not-in-r/
                                  LH_NH == "LB" & bot_genus %in% c("Fagus",     # all species that are labelled "aLh" in the BWI are treated as  beech 
                                                                   "Acer", 
                                                                   "Platanus", 
                                                                   "Fraxinus",
                                                                   "Tilia", 
                                                                   "Juglans", 
                                                                   "Corylus", 
                                                                   "Robinia", 
                                                                   "Castanea", 
                                                                   "Carpinus", 
                                                                   "Aesculus", 
                                                                   "Sorbus",
                                                                   "Ulmus", 
                                                                   "Rhamnus") | LH_NH == "LB" & bot_name == "Prunus dulcis" ~ 'bu',
                                  LH_NH == "LB" & !(bot_genus %in% c("Quercus",  # all species that would be labelled "aLn" in the BWI species groups are allocated to soft hardwoods
                                                                     "Fagus",
                                                                     "Acer", 
                                                                     "Platanus", 
                                                                     "Fraxinus",
                                                                     "Tilia", 
                                                                     "Juglans", 
                                                                     "Corylus", 
                                                                     "Robinia", 
                                                                     "Castanea", 
                                                                     "Carpinus", 
                                                                     "Aesculus", 
                                                                     "Sorbus",
                                                                     "Ulmus", 
                                                                     "Rhamnus")) | LH_NH == "LB" & bot_name != "Prunus dulcis" ~ 'shw',
                                  LH_NH == "NB" & bot_genus %in% c("Pinus", "Larix") ~ 'ki', 
                                  LH_NH == "NB" & !(bot_genus %in% c("Pinus", "Larix"))  ~ 'fi', # all coniferous species that are not Pine or larch are treated as spruce
                                  TRUE ~ 'other'), 
         # species groups to assign the correct woody biomass nitrogen content 
         # available groups/ species : BU, EI, ES, AH, BI, ERL, FI, KI, DGL
         N_SP_group = case_when(LH_NH == "LB" & bot_genus == "Quercus"~ 'EI', 
                                # LH_NH == "LB" & bot_genus == "Fagus" ~ 'BU',
                                LH_NH == "LB" & bot_genus == "Acer"~ 'AH',
                                LH_NH == "LB" & bot_genus == "Fraxinus"~ 'ES',
                                LH_NH == "LB" & bot_genus == "Betula"~ 'BI',
                                LH_NH == "LB" & bot_genus == "Alnus"~ 'ERL',
                                LH_NH == "LB" & bot_genus %in% c("Fagus",
                                                                 "Platanus", 
                                                                 "Tilia", 
                                                                 "Juglans", 
                                                                 "Corylus", 
                                                                 "Robinia", 
                                                                 "Castanea", 
                                                                 "Carpinus", 
                                                                 "Aesculus", 
                                                                 "Sorbus",
                                                                 "Ulmus", 
                                                                 "Rhamnus") | LH_NH == "LB" & bot_name == "Prunus dulcis" ~ 'BU', # species that are ususally "anderes Laubholz Hoher lebenserwartung are allocated to BU
                                LH_NH == "LB" & !(bot_genus %in% c("Quercus", 
                                                                   "Fagus",
                                                                   "Acer", 
                                                                   "Platanus", 
                                                                   "Fraxinus",
                                                                   "Tilia", 
                                                                   "Juglans", 
                                                                   "Corylus", 
                                                                   "Robinia", 
                                                                   "Castanea", 
                                                                   "Carpinus", 
                                                                   "Aesculus", 
                                                                   "Sorbus",
                                                                   "Ulmus", 
                                                                   "Rhamnus", 
                                                                   "Betula", 
                                                                   "Alnus")) | LH_NH == "LB" & bot_name != "Prunus dulcis" ~ 'BI',  # species that are usually "anderes laubholz niedriger Lebenserwartung are allovated to BI
                                LH_NH == "NB" & bot_genus %in% c("Pinus", "Larix") ~ 'KI', 
                                LH_NH == "NB" & bot_genus %in% c("Pseudotzuga") ~ 'DGL',
                                LH_NH == "NB" & !(bot_genus %in% c("Pinus", "Larix", "Pseudotzuga"))  ~ 'FI', # all species not Pinus, Larix or DOuglas fir are treated as Spruce 
                                TRUE ~ 'other'),
         # species groups to select the correct belowground nitrogen content
         N_bg_SP_group = case_when(LH_NH == "LB" & bot_genus == "Quercus"~ 'EI', 
                                   LH_NH == "LB" & bot_genus == "Fagus"~ 'BU',
                                   LH_NH == "LB" & bot_genus %in% c("Acer", 
                                                                    "Platanus", 
                                                                    "Fraxinus",
                                                                    "Tilia", 
                                                                    "Juglans", 
                                                                    "Corylus", 
                                                                    "Robinia", 
                                                                    "Castanea", 
                                                                    "Carpinus", 
                                                                    "Aesculus", 
                                                                    "Sorbus",
                                                                    "Ulmus", 
                                                                    "Rhamnus") | LH_NH == "LB" & bot_name == "Prunus dulcis" ~ 'BU',
                                   LH_NH == "LB" & !(bot_genus %in% c("Quercus", 
                                                                      "Fagus",
                                                                      "Acer", 
                                                                      "Platanus", 
                                                                      "Fraxinus",
                                                                      "Tilia", 
                                                                      "Juglans", 
                                                                      "Corylus", 
                                                                      "Robinia", 
                                                                      "Castanea", 
                                                                      "Carpinus", 
                                                                      "Aesculus", 
                                                                      "Sorbus",
                                                                      "Ulmus", 
                                                                      "Rhamnus")) | LH_NH == "LB" & bot_name != "Prunus dulcis" ~ 'BI',
                                   LH_NH == "NB" & bot_genus == "Pinus" & bot_name != "Pinus nigra" ~ 'KI', 
                                   LH_NH == "NB" & bot_genus == "Pinus" & bot_name == "Pinus nigra" ~ 'KIN',
                                   LH_NH == "NB" & bot_genus == "Larix" ~ 'LA',
                                   LH_NH == "NB" & !(bot_genus %in% c("Pinus", "Larix"))  ~ 'FI', 
                                   TRUE ~ 'other'), 
         # specie group to assing the correct foliage biomass nitrogen content
         N_f_SP_group_MoMoK = case_when(LH_NH == "LB" & bot_genus == "Alnus" ~ 'ERL',
                                        LH_NH == "LB" & bot_genus == "Betula" ~ 'BI',
                                        LH_NH == "LB" & !(bot_genus %in% c("Alnus", "Betula")) ~ 'aLB', # other broadleafed tree (anderer Laubbaum)
                                        LH_NH == "NB" & bot_genus == "Pinus" ~ 'KI', 
                                        LH_NH == "NB" & bot_genus == "Picea" ~ 'FI', 
                                        LH_NH == "NB" & !(bot_genus %in% c("Pinus", "Picea")) ~ 'aNB',   # # other coniferous tree (anderer Nadelbaum)
                                        TRUE ~ "other"),
         # species groups to apply the biomass functions by Wolff to the regeneration trees
         RG_Wolff_bio = case_when(bot_genus == "Acer" ~ "BAH", 
                                  bot_genus == "Fraxinus" | 
                                    BWI_SP_group == "aLh"  & 
                                    !(bot_genus %in% c("Acer",
                                                       "Fagus", 
                                                       "Quercus", 
                                                       "Rhamnus", 
                                                       "Fagus", 
                                                       "Sorbus", 
                                                       "Sambucus")) ~ "ES",
                                  bot_genus == "Betulus" | 
                                    BWI_SP_group == "aLn" &
                                    !(bot_genus %in% c("Acer",
                                                       "Fagus", 
                                                       "Quercus", 
                                                       "Rhamnus", 
                                                       "Fagus", 
                                                       "Sorbus", 
                                                       "Sambucus")) ~ "BI",
                                  bot_genus == "Sambucus" ~ "HOL", 
                                  bot_genus == "Fagus" ~ "BU", 
                                  bot_genus == "Quercus" ~ "EI", 
                                  bot_genus == "Rhamnus" ~ "FKD", 
                                  bot_genus == "Sorbus" ~ "VB", 
                                  bot_genus == "Picea" |
                                    LH_NH == "NB" & !(bot_genus %in% c("Pinus")) ~ "FI",
                                  bot_genus == "Pinus"~ "KI", 
                                  TRUE ~ "other") 
  )
# export x_bart with TapeS common ID: https://stackoverflow.com/questions/53089219/specify-path-in-write-csv-function
write.csv(SP_names_com_ID_tapeS, "output/out_data/x_bart_tapeS.csv")



 



