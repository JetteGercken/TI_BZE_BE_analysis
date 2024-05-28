# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the peat land soil inventory
# Data frangling BZE3 CSV format to Erfassungssoftware format



# Tables we need: 
## tit_1.csv: tit_1.csv only appears in database datasets not in software
    # database: "bund_nr"    "team"       "datum"      "status"     "re_form"    "re_lage"    "neigung"    "exposition" "anmerkung" 


## be_waldraender.csv 
    # database: we have to build it from --> from be.csv  read.delim(file = here("data/input/BZE2_HBI/SN/BZE_database_format/be.csv"), sep = ",", dec = ".", stringsAsFactors=FALSE)
    # software: read.delim(file = here("data/input/BZE2_HBI/SN/BZE_BE_software_format/be_waldraender.csv"), sep = ",", dec = ".", stringsAsFactors=FALSE) 
      # "bund_nr"     "lfd_nr"      "randtyp"     "randform"    "anfang_dist" "end_dist"    "knick_dist"  "anfang_azi"  "end_azi"     "knick_azi"


## be.csv: be.csv in database contains columns that are usually found in be_waldraender.csv
  # database: read.delim(file = here("data/input/BZE2_HBI/SN/BZE_database_format/be.csv"), sep = ",", dec = ".", stringsAsFactors=FALSE) 
      # "bund_nr"                "team"                   "datum"                  "geraet"                 "randtyp_1"              "randform_1"             "anfang_dist_1"         
      # "anfang_azi_1"           "end_dist_1"             "end_azi_1"              "knick_dist_1"           "knick_azi_1"            "randtyp_2"              "randform_2"            
      # "anfang_dist_2"          "anfang_azi_2"           "end_dist_2"             "end_azi_2"              "knick_dist_2"           "knick_azi_2"            "hbi_status"            
      # "beart"                  "besttyp"                "struktur"               "schlussgrad_schi1"      "schlussgrad_schi2"      "mischung"               "pk1_aufnahme"          
      # "pk2_aufnahme"           "pk3_aufnahme"           "beschreibungbestockung" "anmerkung" 
  # software:  read.delim(file = here("data/input/BZE2_HBI/SN/BZE_BE_software_format/be.csv"), sep = ",", dec = ".", stringsAsFactors=FALSE) 
      # "bund_nr"                "ld_bze"                 "bwi_tnr"                "bwi_eck"                "team"                   "datum"                  "srid_ist"              
      # "istre"                  "istho"                  "hbi_status"             "beart"                  "besttyp"                "struktur"               "schlussgrad_schi1"     
      # "schlussgrad_schi2"      "mischung"               "pk1_aufnahme"           "pk2_aufnahme"           "pk3_aufnahme"           "geraet"                 "beschreibungbestockung"
      # "anmerkung"


## beab.csv: beab.csv in software has additional column bhd
  # database:  read.delim(file = here("data/input/BZE2_HBI/SN/BZE_database_format/beab.csv"), sep = ",", dec = ".", stringsAsFactors=FALSE) 
      # "bund_nr"       "lfd_nr"        "baumkennzahl"  "zwiesel"       "bart"          "alter"         "alter_methode" "d_mess"        "bhd_hoehe"     "hoehe"         "kransatz"     
      # "azi"           "hori"          "kraft"         "schi" 
  # software: read.delim(file = here("data/input/BZE2_HBI/SN/BZE_BE_software_format/beab.csv"), sep = ",", dec = ".", stringsAsFactors=FALSE) 
      # "bund_nr"       "lfd_nr"        "baumkennzahl"  "zwiesel"        "bart"         "alter"         "alter_methode"  "d_mess"        "bhd_hoehe"    "hoehe"         "kransatz"      
      # "azi"           "hori"          "kraft"         "schi"           "bhd"                         


## be_totholz_liste.csv --> bedw_liste.csv
  # database:   read.delim(file = here("data/input/BZE2_HBI/SN/BZE_database_format/bedw_liste.csv"), sep = ",", dec = ".", stringsAsFactors=FALSE)
      # "bund_nr"     "lfd_nr"      "typ"         "baumgruppe"  "anzahl"      "durchmesser" "laenge"      "zersetzung" 
  # software: read.delim(file = here("data/input/BZE2_HBI/SN/BZE_BE_software_format/be_totholz_liste.csv"), sep = ",", dec = ".", stringsAsFactors=FALSE) 
      # "bund_nr"     "lfd_nr"       "typ"         "baumgruppe"  "anzahl"      "durchmesser" "laenge"      "zersetzung"  "anmerkung"  


## be_totholz_punkt.csv --> bedw_punkt.csv
  # database:  read.delim(file = here("data/input/BZE2_HBI/SN/BZE_database_format/bedw.csv"), sep = ",", dec = ".", stringsAsFactors=FALSE)
      # "bund_nr" "status"  "pk_dist" "pk_azi" 
  # software:  read.delim(file = here("data/input/BZE2_HBI/SN/BZE_BE_software_format/be_totholz_punkt.csv"), sep = ",", dec = ".", stringsAsFactors=FALSE)
      # "bund_nr" "status"  "pk_dist" "pk_azi" 


## bej.csv
  # database: read.delim(file = here("data/input/BZE2_HBI/SN/BZE_database_format/bej.csv"), sep = ",", dec = ".", stringsAsFactors=FALSE)
      # "bund_nr"      "pk_nr"        "pk_richtung"  "pk_dist"      "pk_aufnahme"  "pk_maxdist"   "pk_anmerkung"
  # software: read.delim(file = here("data/input/BZE2_HBI/SN/BZE_BE_software_format/bej.csv"), sep = ",", dec = ".", stringsAsFactors=FALSE)
      # "bund_nr"      "pk_nr"       "pk_richtung"   "pk_dist"     "pk_aufnahme"   "pk_maxdist" 


## bejb.csv
  # database: read.delim(file = here("data/input/BZE2_HBI/SN/BZE_database_format/bejb.csv"), sep = ",", dec = ".", stringsAsFactors=FALSE)
      # "bund_nr"  "pk_nr"    "lfd_nr"   "bart"     "hoehe"    "grklasse"
  # software: read.delim(file = here("data/input/BZE2_HBI/SN/BZE_BE_software_format/bejb.csv"), sep = ",", dec = ".", stringsAsFactors=FALSE)
      # "bund_nr"  "pk_nr"    "lfd_nr"   "bart"     "hoehe"    "grklasse" 


# be_waldränder from be.csv
forest_edges <- read.delim(file = here("data/input/BZE2_HBI/SN/BZE_database_format/be.csv"), sep = ",", dec = ".", stringsAsFactors=FALSE) %>% 
  # these are the columns we want to acchive: "bund_nr"     "lfd_nr"      "randtyp"     "randform"    "anfang_dist" "end_dist"    "knick_dist"  "anfang_azi"  "end_azi"     "knick_azi"
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
