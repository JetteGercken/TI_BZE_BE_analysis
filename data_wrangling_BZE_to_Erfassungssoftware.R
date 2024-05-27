# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the peat land soil inventory
# Data frangling BZE3 CSV format to Erfassungssoftware format



# Tables we need: 
## be_waldraender.csv --> from be.csv

## be.csv
  # database:  bund_nr,lfd_nr,baumkennzahl,zwiesel,bart,alter,alter_methode,d_mess,bhd_hoehe,hoehe,kransatz,azi,hori,kraft,schi
  # software: 

## beab.csv
  # database:  
  # software: 

## be_totholz_liste.csv --> bedw_liste.csv
  # database:  
  # software: 

## be_totholz_punkt.csv --> bedw_punkt.csv
  # database:  
  # software: 

## bej.csv
  # database:  
  # software: 

## bejb.csv
  # database:  
  # software: 

# be_waldränder from be.csv
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