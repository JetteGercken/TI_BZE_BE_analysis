# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the  national soil inventory
# FSI - Forst structural index dataprep: wrangle bark and fruit diversity datasets into BZE BE compartible species groups


# 0.SETUP --------------------------------------------------------------------------------------------------------------------
# 0.1. packages and functions -------------------------------------------------------------------------------------------------
source(paste0(getwd(), "/scripts/01_00_functions_library.R"))



# 0.2. improt data --------------------------------------------------------
# bark diversity for storch FSI index
# bark_div <- read.delim(file = here("data/input/general/barkdiv_FSI_storch_2018.csv"),  sep = ",", dec = ".")
# we don´t import but write da dataframe here, ebcause we cannot be sure that any user of this code has acces to it
# the following dataframe is, however identiacal to the tables of the annex of Felix storchs paper
bark_div <- structure(list(Tree.species = c("Abies alba", "Acer campestre", 
                                            "Acer platanoides", "Acer pseudoplatanus", "Alnus spp.", "Betula spp.", 
                                            "broadleaf species", "Carpinus betulus", "Castanea sativa", "conifer species", 
                                            "Fagus sylvatica", "Fraxinus excelsior", "Larix decidua", "Larix kaempferi", 
                                            "Malus sylvestris", "Picea abies", "Pinus nigra", "Pinus spp.", 
                                            "Populus balsamifera", "Populus spp.", "Prunus avium", "Pseudotsuga menziesii", 
                                            "Pyrus pyraster", "Quercus rubra", "Quercus spp.", "Robinia pseudoacacia", 
                                            "Salix spp.", "Sorbus aria", "Sorbus domestica", "Sorbus spp.", 
                                            "Sorbus torminalis", "Taxus baccata", "Tilia spp.", "Ulmus spp."), 
                           Bark.Type = c("scaly", "scaly", "scaly", "scaly", "furrowed", 
                                         "furrowed", "?", "smooth", "furrowed", "?", "smooth", "furrowed", 
                                         "furrowed", "furrowed", "scaly", "scaly", "scaly", "scaly", "furrowed", 
                                         "furrowed", "smooth", "furrowed", "scaly", "furrowed", "furrowed", 
                                         "furrowed", "furrowed", "smooth/scaly", "scaly", "smooth", "scaly", 
                                         "scaly", "furrowed", "furrowed"), 
                 DBH.Type.1 = c("< 20 cm", "< 20 cm", 
                                "< 15 cm", "< 20 cm", "< 15 cm", "< 15 cm", "?", "< 30 cm", "< 20 cm", 
                                "?", "omitted", "< 20 cm", "< 10 cm", "< 10 cm", "< 20 cm", "< 20 cm", 
                                "< 15 cm", "< 15 cm", "< 15 cm", "< 15 cm", "omitted", "< 20 cm", 
                                "< 20 cm", "< 20 cm", "< 10 cm", "< 10 cm", "< 20 cm", "< 20 cm", 
                                "< 20 cm", "omitted", "< 15 cm", "< 20 cm", "< 20 cm", "< 20 cm"), 
                 DBH.Type.2 = c("20 - 40 cm", ">20 cm", "15 - 35 cm", "20 - 40 cm", 
                                   "15 - 30 cm", "15 - 25 cm", "?", ">30 cm", "20 - 35 cm", "?", 
                                   "omitted", "20 - 35 cm", "10 - 30 cm", "10 - 30 cm", "20 - 40 cm", 
                                   "20 - 40 cm", "15 - 30 cm", "15 - 30 cm", "15 - 25 cm", "15 - 25 cm", 
                                   "omitted", "20 - 35 cm", "20 - 40 cm", "20 - 40 cm", "10 - 30 cm", 
                                   "10 - 25 cm", "20 - 35 cm", ">20 cm", "20 - 40 cm", "omitted", 
                                   ">15 cm", ">20 cm", "20 - 35 cm", "20 - 35 cm"), 
                 DBH.Type.3 = c("> 40 cm", 
                                 "omitted", "> 35 cm", "> 40 cm", "> 30 cm", "> 25 cm", "?", "omitted", 
                                 "> 35 cm", "?", "omitted", "> 35 cm", "> 30 cm", "> 30 cm", "> 40 cm", 
                                 "> 40 cm", "> 30 cm", "> 30 cm", "> 25 cm", "> 25 cm", "omitted", 
                                 "> 35 cm", "> 40 cm", "> 40 cm", "> 30 cm", "> 25 cm", "> 35 cm", 
                                 "omitted", "> 40 cm", "omitted", "omitted", "omitted", "> 35 cm", 
                                 "> 35 cm")), class = "data.frame", row.names = c(NA, -34L))
# export
write.csv(bark_div, paste0(getwd(), "/data/raw/general", "/", "barkdiv_FSI_storch_2018.csv"), row.names = FALSE)
write.csv(bark_div, paste0(getwd(), "/data/input/general", "/", "barkdiv_FSI_storch_2018.csv"), row.names = FALSE)


## fruit diversity for storch FSI index
# fruit_div <- read.delim(file = here("data/input/general/fruitdiv_FSI_storch_2018.csv"),  sep = ",", dec = ".")
# we don´t import but write da dataframe here, ebcause we cannot be sure that any user of this code has acces to it
# the following dataframe is, however identiacal to the tables of the annex of Felix storchs paper
fruit_div <- as.data.frame(structure(list(
  Tree.species = c("Acer pseudoplatanus", "Betula spp.", 
                   "Populus balsamifera", "Fagus sylvatica", "Pseudotsuga menziesii", 
                   "Quercus spp.", "Sorbus torminalis", "Larix decidua", "Alnus spp.", 
                   "Fraxinus excelsior", "Acer campestre", "Picea abies", "Carpinus betulus", 
                   "Larix kaempferi", "Castanea sativa", "Pinus spp.", "Prunus avium", 
                   "Tilia spp.", "Populus balsamifera", "Quercus rubra", "Robinia pseudoacacia", 
                   "Acer platanoides", "Pinus nigra", "broadleaf species", "conifer species", 
                   "Abies alba", "Ulmus spp", "Sorbus spp.", "Salix spp.", "Sorbus domestica", 
                   "Taxus baccata", "Sorbus aria", "Malus sylvestris", "Pyrus pyraster"), 
  Fruct..age = c(30L, 25L, 10L, 60L, 25L, 65L, 15L, 35L, 25L, 
                  40L, 40L, 55L, 25L, 35L, 25L, 40L, 20L, 40L, 10L, 50L, 20L, 30L, 
                  40L, 0L, 0L, 60L, 35L, 10L, 15L, 10L, 30L, 15L, 15L, 15L), 
  Pollination = c("cross + animal", 
               "cross + wind", "cross + wind", "cross + wind", "cross + wind", 
               "cross + wind", "cross + animal", "cross + wind", "cross + wind", 
               "cross + wind", "cross + animal", "cross + wind", "cross + wind", 
               "cross + wind", "cross + animal", "cross + wind", "cross + wind", 
               "cross + animal", "cross + wind", "cross + wind", "cross + animal", 
               "cross + animal", "cross + wind", "0", "0", "cross + wind", "self", 
               "cross + animal", "cross + animal", "cross + animal", "wind", 
               "cross + animal", "cross + animal", "cross + animal"), 
  Fruit.type = c("schizocarpic fruit", 
               "wingnut", "capsule fruit", "nut", "cone", "nut", "apple fruit", 
               "cone", "cone", "nut", "schizocarpic fruit", "cone", "nut", "cone", 
               "capsule fruit", "cone", "drupe", "nut", "capsule fruit", "nut", 
               "legume", "schizocarpic fruit", "cone", "0", "0", "cone", "wingnut", 
               "apple fruit", "capsule fruit", "apple fruit", "cone", "apple fruit", 
               "apple fruit", "apple fruit")), class = "data.frame", row.names = c(NA, 
                                                                                   -34L)))
# export FSI fruitdiv "raw" original dataframe by felix storch   
write.csv(fruit_div, paste0(getwd(), "/data/raw/general", "/", "fruitdiv_FSI_storch_2018.csv"), row.names = FALSE)
write.csv(fruit_div, paste0(getwd(), "/data/raw/general", "/", "fruitdiv_FSI_storch_2018.csv"), row.names = FALSE)
SP_names <- read.delim(file = paste0(getwd(), "/data/input/general/x_bart.csv"), sep = ",", dec = ".")



# ----- 0.3. dataprep & colnames --------------------------------------------------------------
# species list NSI / BZE
SP_names <- SP_names  %>% 
  select(- c(gueltig_ab, gueltig_bis)) %>% 
  # https://stackoverflow.com/questions/21003311/how-to-combine-multiple-character-columns-into-a-single-column-in-an-r-data-fram
  tidyr::unite(bot_name, genus, species, sep = " ", remove = FALSE) %>%  # creating one column with complete botanic name
 mutate(bot_name = ifelse(bot_name == "-2 -2", -2, bot_name))   # the error codes are joined in one column too, which i don´t want, so I´ll keep them single
colnames(SP_names) <- c("Nr_code", "Chr_code_ger", "name", "icode", "bot_name", "bot_genus", 
                        "bot_species","BWI",  "LH_NH")

# barkdiversity dataset 
colnames(bark_div) <- c("species", "bark_type", "DBH_type_1", "DBH_type_2", "DBH_type_3")
bark_div <- bark_div %>%  mutate(bot_genus = gsub(" .*", "", species), 
                                 bot_species = gsub(".* ", "", species))

# fruitdiversity dataset
colnames(fruit_div) <- c("species", "fruct_age", "pollination_type", "fruit_type")  
fruit_div <- fruit_div %>% mutate(bot_genus = gsub(" .*", "", species), 
                                  bot_species = gsub(".* ", "", species)) %>% 
  mutate(species = ifelse(species == "Ulmus spp", "Ulmus spp.", species))


# 1.2.3. identify and create missing species groups for bark types-------------------------------------------------------------------------
# first check for those that we can join by their full botanical name 
bark_TY_species_groups_1 <- SP_names %>% 
  mutate(bot_genus = ifelse(bot_genus == "abies", "Abies", bot_genus)) %>% 
  left_join(., bark_div %>% select(species, bot_genus, bot_species, bark_type), 
            by = c("bot_name" = "species", "bot_genus", "bot_species")) 
# seect those bark species that are meant to be apllied to a whole botanic genus --> ending with spp. 
bark_TY_species_groups_2 <- 
  bark_TY_species_groups_1 %>% filter(is.na(bark_type)) %>% 
  left_join(., bark_div %>%
              filter(bot_species == "spp.") %>% 
              select(bot_genus, bark_type),
            by = "bot_genus") 

# now we select those species that do not account for a whole bot_genus and that dont have a bot_genus and species combination
bark_TY_species_groups_3 <- bark_TY_species_groups_2 %>% 
  filter(is.na(bark_type.y)) %>%
  left_join(., bark_div %>% 
              anti_join(bark_div %>% filter(bot_species == "spp.") %>% select(bot_genus), 
                        by = "bot_genus") %>% 
              arrange(species) %>% 
              select(bot_genus, bark_type) %>% 
              distinct(), 
            by = "bot_genus")

## bark diversity 
try( # "try" statement needed so the run_NDI script can run, since the coercion creates NAs which causes a warning which stops the external run
  {bark_div <- 
  plyr::rbind.fill(bark_div %>% 
                     mutate(., across(c("DBH_type_1", "DBH_type_2", "DBH_type_3"), ~ replace(., is.na(.), "omitted"))) %>% 
                     # define upper border for type 1
                     mutate(u_border_cm_TY1 = ifelse(DBH_type_1 %in% c("omitted", "?"), DBH_type_1, gsub('^.|..$', '', DBH_type_1))) %>% 
                     # define lower border for type 2
                     mutate(l_border_cm_TY2 = ifelse(DBH_type_2 %in% c("omitted", "?") | startsWith(DBH_type_2, ">"), DBH_type_2, gsub('.......$', '', DBH_type_2))) %>% 
                     # if the lower of type 2 is equal to the upper boarder of type 1 repleace type 2 l border with type 1 l border
                     mutate(l_border_cm_TY2 = ifelse(startsWith(l_border_cm_TY2, ">"), u_border_cm_TY1, l_border_cm_TY2)) %>% 
                     mutate(u_border_cm_TY2 = ifelse(DBH_type_2 %in% c("omitted", "?") | startsWith(DBH_type_2, ">"), DBH_type_2, gsub('^....|..$', '', DBH_type_2))) %>% 
                     mutate(u_border_cm_TY2 = ifelse(startsWith(u_border_cm_TY2, ">"), "omitted", u_border_cm_TY2)) %>% 
                     mutate(l_border_cm_TY3 = ifelse(DBH_type_3 %in% c("omitted", "?"), DBH_type_3, gsub('^.|..$', '', DBH_type_3))) , 
                   (bark_div %>% 
                      mutate(., across(c("DBH_type_1", "DBH_type_2", "DBH_type_3"), ~ replace(., is.na(.), "omitted"))) %>% 
                      # define upper border for type 1
                      mutate(u_border_cm_TY1 = ifelse(DBH_type_1 %in% c("omitted", "?"), DBH_type_1, gsub('^.|..$', '', DBH_type_1))) %>% 
                      # define lower border for type 2
                      mutate(l_border_cm_TY2 = ifelse(DBH_type_2 %in% c("omitted", "?") | startsWith(DBH_type_2, ">"), DBH_type_2, gsub('.......$', '', DBH_type_2))) %>% 
                      # if the lower of type 2 is equal to the upper boarder of type 1 repleace type 2 l border with type 1 l border
                      mutate(l_border_cm_TY2 = ifelse(startsWith(l_border_cm_TY2, ">"), u_border_cm_TY1, l_border_cm_TY2)) %>% 
                      mutate(u_border_cm_TY2 = ifelse(DBH_type_2 %in% c("omitted", "?") | startsWith(DBH_type_2, ">"), DBH_type_2, gsub('^....|..$', '', DBH_type_2))) %>% 
                      mutate(u_border_cm_TY2 = ifelse(startsWith(u_border_cm_TY2, ">"), "omitted", u_border_cm_TY2)) %>% 
                      mutate(l_border_cm_TY3 = ifelse(DBH_type_3 %in% c("omitted", "?"), DBH_type_3, gsub('^.|..$', '', DBH_type_3))) %>% 
                      # semi join (filter) for those trees that have multiple species listed and summarise their bark type and create a common group for them 
                      # withthe bot_species spp. 
                      semi_join(
                        bark_div %>%
                          # filter for those trees that are not already summarised to spp. groups or that are not conifer/ broadleaf overall group
                          filter(bot_species != "spp." & !(bot_genus %in% c("conifer","broadleaf"))) %>% 
                          # sort those species out that allready have a spp. summary but also separate species (e.g. Pinus nigra, Pinus spp.)
                          anti_join(., bark_div %>%
                                      filter(bot_species == "spp." | bot_genus %in% c("conifer","broadleaf")) %>%
                                      select(bot_genus), 
                                    by = "bot_genus") %>% 
                          # select only bot_genus and bark type 
                          select(bot_genus, bark_type) %>% 
                          group_by(bot_genus) %>% 
                          # count rows per species
                          summarise(n_bark = n()) %>% 
                          # filter for bot_geni that have more then one representative in the Storch table 
                          filter(n_bark > 1),
                        # finish the semi join 
                        by = "bot_genus") %>% 
                      # take the selected species and bark types and narrow them down 
                      select(bot_genus, bark_type,u_border_cm_TY1, l_border_cm_TY2, u_border_cm_TY2, l_border_cm_TY3) %>% 
                      distinct() %>%
                      # create "bot_species" column indicating summary with spp. 
                      mutate(bot_species = "spp.") %>%
                      # replace "omitted" with NA: https://dplyr.tidyverse.org/reference/na_if.html
                    #  mutate(across(u_border_cm_TY1:l_border_cm_TY3,  ~na_if(.,"omitted"))) %>% 
                      # calcaulte mean per bot_genus, bark_type
                      group_by(bot_genus, bot_species, bark_type) %>% 
                      summarise(u_border_cm_TY1 = mean(na.omit(as.numeric(u_border_cm_TY1))), 
                                l_border_cm_TY2 = mean(na.omit(as.numeric(l_border_cm_TY2))), 
                                u_border_cm_TY2 = mean(na.omit(as.numeric(u_border_cm_TY2))), 
                                l_border_cm_TY3 = mean(na.omit(as.numeric(l_border_cm_TY3)))) %>% 
                      # create column "species" 
                      unite("species", c(bot_genus, bot_species), sep = " ", remove = FALSE))) %>% 
  distinct() %>% arrange(species) %>% 
  mutate(across(u_border_cm_TY1:l_border_cm_TY3, as.numeric)) %>% 
  mutate(across(u_border_cm_TY1:l_border_cm_TY3,  ~na_if(., 0))) %>% 
  mutate(bark_type = ifelse(bark_type == "?", "unkown", bark_type))
  }  ,silent = T)
  


# fruitdiversity
# first check for those species that we can join by their full botanical name 
fruit_species_groups_1 <- SP_names %>%
  mutate(bot_genus = ifelse(bot_genus == "abies", "Abies", bot_genus), 
         bot_name = ifelse(bot_name == "abies", "Abies", bot_name)) %>% 
  left_join(., fruit_div %>% select(species, fruit_type) %>% distinct(), 
            by = c("bot_name" = "species")) 
# select those fruit type species that are meant to be apllied to a whole botanic genus --> ending with spp. 
fruit_species_groups_2 <- 
  fruit_species_groups_1 %>% filter(is.na(fruit_type)) %>% 
  left_join(., fruit_div %>%
              filter(bot_species == "spp.") %>% 
              select(bot_genus, fruit_type),
            by = "bot_genus") 
# now we select those species that do not account for a whole bot_genus and that dont have a bot_genus and species combination
fruit_species_groups_3 <- fruit_species_groups_2 %>% 
  filter(is.na(fruit_type.y)) %>%
  left_join(., fruit_div %>% 
              anti_join(fruit_div %>% filter(bot_species == "spp.") %>% select(bot_genus), 
                        by = "bot_genus") %>% 
              arrange(species) %>% 
              select(bot_genus, fruit_type) %>% 
              distinct(), 
            by = "bot_genus")


# the remaining, unassiged species remaining after sorting the fruit type/ bark type data into x_bart 
# are identical thus we well proceede the same way as for bark type 
identical(fruit_species_groups_3 %>% filter(is.na(fruit_type)) %>% 
            select(bot_genus) %>% distinct() %>% arrange(bot_genus), 
          bark_TY_species_groups_3 %>% filter(is.na(bark_type)) %>% 
            select(bot_genus) %>% distinct() %>% arrange(bot_genus))



fruit_div <- 
  plyr::rbind.fill(
    fruit_div, 
    ## this semi join identifies those geni that do not have a spp. sumamry but have multiple species in the fruits dataset(        fruit_div %>% 
    (fruit_div %>% 
       semi_join(
         fruit_div %>% distinct() %>% 
           # filter for those trees that are not already summarised to spp. groups and that are not conifer/ broadleaf overall group
           filter(bot_species != "spp." & !(bot_genus %in% c("conifer","broadleaf"))) %>% 
           # sort those species out that allready have a spp. summary but also separate species (e.g. Pinus nigra, Pinus spp.)
           anti_join(., fruit_div %>%
                       filter(bot_species == "spp." | bot_genus %in% c("conifer","broadleaf")) %>%
                       select(bot_genus), by = "bot_genus") %>% 
           # select only bot_genus and bark type
           select(bot_genus, fruit_type) %>% 
           group_by(bot_genus) %>% 
           # count rows per genus --> are there mutliple species of one genus? 
           summarise(n_fruits = n()) %>% 
           # filter for bot_geni that have more then one representative in the Storch table 
           filter(n_fruits > 1), 
         by = "bot_genus") %>% ## close semi join 
       # take the selected species and fruits and pollination types and narrow them down 
       select(bot_genus, fruct_age, pollination_type, fruit_type) %>% 
       distinct() %>% # narrow them down --> there are different pollitation ages for the Acer types 
       # thus we´ll avearge them, while the pollination and fruit type, which are identical, remain the same 
       group_by(bot_genus) %>% 
       reframe(fruct_age = mean(fruct_age), 
               pollination_type = pollination_type, 
               fruit_type = fruit_type) %>% distinct() %>% # the distinct is just to errase the doubles the reframe introduces
       # create "bot_species" column indicating summary with spp. 
       mutate(bot_species = "spp.") %>% 
       # create column "species" 
       unite("species", c(bot_genus, bot_species), sep = " ", remove = FALSE))
  ) %>% 
  mutate(fruct_age = ifelse(species %in% c("broadleaf species", "conifer species"), NA , as.numeric(fruct_age))) %>% 
  # change pollynation and fruit type to unknown: https://stackoverflow.com/questions/63048292/r-mutate-multiple-columns-with-ifelse
  mutate(across(pollination_type:fruit_type, ~ifelse(.x == 0, "unknown", .x)) ) %>% arrange(species) %>% distinct()



# 2. export data --------------------------------------------------------
write.csv(fruit_div, paste0("data/input/general/", paste0("fruitdiv_FSI_modified", ".csv")), row.names = FALSE)
write.csv(bark_div, paste0("data/input/general/", paste0("barkdiv_FSI_modified", ".csv")), row.names = FALSE)



stop("this is where notes of FSI species groups script start")

# 3. explore differnces between species groups of fruit vs. bark  --------
# check for differences in the fruit types species groups and bark types species groups
# there are non, if we compare fruits type species groups with bark types species groups
fruit_div %>% 
  anti_join(bark_div, 
            by = "species")
# the bark type species groups, however, have one more species or rather species summary for populus called Populus spp. 
bark_div %>% 
  anti_join(fruit_div, 
            by = "species")

