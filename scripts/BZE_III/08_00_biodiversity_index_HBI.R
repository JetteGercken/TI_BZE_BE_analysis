# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the  national soil inventory
# biodiverseity index
# HBI

# 1) quadratic mean diameter at breast height (DBH)
# 2) standard deviation of DBH
# 3) standard deviation of stand height
# 10) trees pecies richness and 
# 5) bark-diversity index
# 6) volume of trees with DBH ≥ 40 cm
# 7) diversity of flowering and fructification trees

# 4) number of decay classes
# 8) average mean diameter of downed deadwood
# 9) mean DBH of standing deadwood

# 11) tree species richness in the regeneration layer



# NOTE: 
# I have to make sure that the empty plos are included in the index as well
# meaning for example DW plot 50145 which has no deadwood items and by that an average diameter of 0 has to be included in the caclualtions
# therefore it would be better to work only with the summaries, or, if summaries are created afterwards, join at least the plots in that have no
# values 

# 0.SETUP --------------------------------------------------------------------------------------------------------------------
# 0.1. packages and functions -------------------------------------------------------------------------------------------------
source(paste0(getwd(), "/scripts/01_00_functions_library.R"))


# 0.2. working directory ------------------------------------------------------------------------------------------------------
here::here()
out.path.BZE3 <- ("output/out_data/out_data_BZE/") 


# 0.3 import data --------------------------------------------------------------------------------------------------------------
# livign trees
trees_data <- read.delim(file = here(paste0(out.path.BZE3, "HBI_LT_update_4.csv")), sep = ";", dec = ",")
LT_summary <-  read.delim(file = here(paste0(out.path.BZE3, trees_data$inv[1], "_LT_stocks_ha_all_groups.csv")), sep = ";", dec = ",")
trees_stat_2 <- read.delim(file = here(paste0(out.path.BZE3, trees_data$inv[1], "_LT_stat_2.csv")), sep = ";", dec = ",") %>% select(-X)

# regeneration 
RG_data <- read.delim(file = here(paste0(out.path.BZE3, trees_data$inv[1], "_RG_update_4.csv")), sep = ";", dec = ",")
RG_summary <-  read.delim(file = here(paste0(out.path.BZE3, trees_data$inv[1], "_RG_stocks_ha_all_groups.csv")), sep = ";", dec = ",")
RG_stat_2 <- read.delim(file = here(paste0(out.path.BZE3, trees_data$inv[1], "_RG_stat_2.csv")), sep = ";", dec = ",")

# deadwood
DW_data <- read.delim(file = here(paste0(out.path.BZE3, trees_data$inv[1], "_DW_update_4.csv")), sep = ";", dec = ",")
DW_summary <-  read.delim(file = here(paste0(out.path.BZE3, trees_data$inv[1], "_DW_stocks_ha_all_groups.csv")), sep = ";", dec = ",")
DW_stat_2 <- read.delim(file = here(paste0(out.path.BZE3, trees_data$inv[1], "_DW_stat_2.csv")), sep = ";", dec = ",")

SP_names_com_ID_tapeS <- read.delim(file = here("output/out_data/x_bart_tapeS.csv"), sep = ",", dec = ",")

bark_div <- read.delim(file = here("data/input/General/barkdiv_FSI_storch_2018.csv"), sep = ";", dec = ",")
colnames(bark_div) <- c("species", "bark_type", "DBH_type_1", "DBH_type_2", "DBH_type_3")

fruit_div <- read.delim(file = here("data/input/General/fruitdiv_FSI_storch_2018.csv"), sep = ";", dec = ",")
colnames(fruit_div) <- c("species", "fruct_age", "pollination_type", "fruit_type")  
fruit_div <- fruit_div %>% mutate(bot_genus = gsub(" .*", "", species), 
                                  bot_species = gsub(".* ", "", species))




# 0.4. data prep -----------------------------------------------------------------------------------------
bark_div <- bark_div %>%  mutate(bot_genus = gsub(" .*", "", species), 
                                 bot_species = gsub(".* ", "", species))
bark_div <- 
  plyr::rbind.fill(bark_div, 
                   (bark_div %>% 
                      # semi join (filter) for those trees that have multiple species listed and summarize their bark type and create a common group for them 
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
                      select(bot_genus, bark_type) %>% distinct() %>%
                      # create "bot_species" column indicating summary with spp. 
                      mutate(bot_species = "spp.") %>% 
                      # create column "species" 
                      unite("species", c(bot_genus, bot_species), sep = " ", remove = FALSE))) %>% 
  distinct() %>% arrange(species) %>%
  mutate(., across(c("DBH_type_1", "DBH_type_2", "DBH_type_3"), ~ replace(., is.na(.), "omitted"))) %>% 
  # define upper border for type 1
  mutate(u_border_cm_TY1 = ifelse(DBH_type_1 %in% c("omitted", "?"), DBH_type_1, gsub('^.|..$', '', DBH_type_1))) %>% 
  # define lower border for type 2
  mutate(l_border_cm_TY2 = ifelse(DBH_type_2 %in% c("omitted", "?") | startsWith(DBH_type_2, ">"), DBH_type_2, gsub('.......$', '', DBH_type_2))) %>% 
  # if the lower of type 2 is equal to the upper boarder of type 1 repleace type 2 l border with type 1 l border
  mutate(l_border_cm_TY2 = ifelse(startsWith(l_border_cm_TY2, ">"), u_border_cm_TY1, l_border_cm_TY2)) %>% 
  mutate(u_border_cm_TY2 = ifelse(DBH_type_2 %in% c("omitted", "?") | startsWith(DBH_type_2, ">"), DBH_type_2, gsub('^....|..$', '', DBH_type_2))) %>% 
  mutate(u_border_cm_TY2 = ifelse(startsWith(u_border_cm_TY2, ">"), "omitted", u_border_cm_TY2)) %>% 
  mutate(l_border_cm_TY3 = ifelse(DBH_type_3 %in% c("omitted", "?"), DBH_type_3, gsub('^.|..$', '', DBH_type_3))) 
    
    
    
    
    
     
# 1. calculations -----------------------------------------------------------------------------------------------------------------------

# 1.1. living trees -------------------------------------------------------------------------------------------------------------------

# 1.1.1. LT quadratic mean diameter at breast height (DBH) ---------------------------------------------------------------------------------
# The quadratic mean (also called the root mean square*) is a type of average. 
# It measures the absolute magnitude of a set of numbers, and is calculated by:
# RMS = sqrt(sum(x)^2/n)

LT_DBH_RMS <- trees_data %>% 
  group_by(plot_ID) %>% 
  reframe(LT_RMS_DBH = RMS(DBH_cm)) %>% 
  distinct()
  

if(exists('trees_stat_2') == TRUE && nrow(trees_stat_2)!= 0){
  FSI_df <- plyr::rbind.fill(LT_DBH_RMS, 
                           # select only those plots with empty sampling circuits that have all 3 circuits empty
                           # by counting the circuits per plot and filtering for those with n_CCS ==3
                           trees_stat_2 %>% 
                             select(plot_ID, CCS_r_m) %>% 
                             distinct()%>% 
                             group_by(plot_ID) %>% 
                             summarise(n_CCS = n()) %>% 
                             filter(n_CCS == 3) %>% 
                             select(plot_ID)) %>% 
  # if the Rbind caused NAs to appear because there were whole plots without a any tree CCS then we have to set the respective variable to 0
  mutate(LT_RMS_DBH = ifelse(is.na(LT_RMS_DBH), 0, LT_RMS_DBH), 
         LT_FSI_DBH_RMS =  as.numeric(FSI(LT_RMS_DBH)))
}else{
  FSI_df <- LT_DBH_RMS %>% 
    mutate(LT_FSI_DBH_RMS =  as.numeric(FSI(LT_RMS_DBH)))
  }



# 1.1.2. LT standard deviation of DBH ------------------------------------------------------------------
FSI_df <- FSI_df %>% 
  left_join(., 
            LT_summary %>% 
              filter(plot_ID != "all" & SP_code == "all" & stand == "all") %>% 
              select(plot_ID, sd_DBH_cm) %>% 
              distinct() %>% 
              mutate(LT_FSI_DBH_SD =  as.numeric(FSI(sd_DBH_cm)), 
                     plot_ID = as.integer(plot_ID)) %>% 
              rename("LT_sd_DBH_cm" = "sd_DBH_cm"), 
            by = "plot_ID")


# 1.1.3. LT standard deviation of stand height ------------------------------------------------------------------
FSI_df <- FSI_df %>% 
  left_join(., 
            LT_summary %>% 
              filter(plot_ID != "all" & SP_code == "all" & stand == "all") %>% 
              select(plot_ID, sd_H_m) %>% 
              distinct() %>% 
              mutate(LT_FSI_H_SD =  as.numeric(FSI(sd_H_m)), 
                     plot_ID = as.integer(plot_ID)) %>% 
              rename("LT_sd_H_m" = "sd_H_m"), 
            by = "plot_ID")



# 1.1.4. tree species richness ------------------------------------------------------------------
FSI_df <- FSI_df %>% 
  left_join(., 
            LT_summary %>% 
              filter(plot_ID != "all" & SP_code == "all" & stand == "all") %>% 
              select(plot_ID, n_SP) %>% 
              distinct() %>% 
              mutate(LT_FSI_n_SP = as.numeric(FSI(n_SP)), 
                     plot_ID = as.integer(plot_ID)) %>% 
              rename("LT_n_SP" = "n_SP"), 
            by = "plot_ID")



# 1.1.5. bark-diversity index ------------------------------------------------------------------
# Diversity of bark types (smooth, fissured, peeling, scaly, cracked, etc.) in forest stands implies a variety of habitats
# for many species to be found there (insects, fungi, yeasts, spiders, epiphytes). 
# Tree diameter and bark-development phases are considered


trees_sub <- trees_data %>% filter(compartiment == "ag")
bark_TY_list <- vector("list", length = length(unique(trees_sub$tree_ID)))
for (i in 1:length(unique(trees_sub$tree_ID))) {
  # i = 2
  my.tree.id <- trees_sub[i, "tree_ID"]
  my.plot.id <- trees_sub[i, "plot_ID"]
  my.inv <- trees_sub[i, "inv"]
  my.dbh.cm <-  trees_sub[i, "DBH_cm"]
  my.bark.spp <- SP_names_com_ID_tapeS$bark_type_SP_group[SP_names_com_ID_tapeS$bot_name == SP_names_com_ID_tapeS$bot_name[SP_names_com_ID_tapeS$Chr_code_ger == trees_sub[i, "Chr_code_ger"]]]
  u_border_cm_TY1 <- as.numeric(bark_div$u_border_cm_TY1[bark_div$species == my.bark.spp])
  l_border_cm_TY2 <- as.numeric(bark_div$l_border_cm_TY2[bark_div$species == my.bark.spp])
  u_border_cm_TY2 <- as.numeric(bark_div$u_border_cm_TY2[bark_div$species == my.bark.spp])
  l_border_cm_TY3 <- as.numeric(bark_div$l_border_cm_TY3[bark_div$species == my.bark.spp])
  
  
  my.bark.ty <- 
  case_when(# if there is an upper border for type 1 and the diameter is within it
            !is.na(u_border_cm_TY1) & my.dbh.cm < u_border_cm_TY1 ~ paste0(bark_div$bark_type[bark_div$species == my.bark.spp],"_TY_1"),
            # if there is an upper and lower border for type 2 and the diamter is within it
            !is.na(u_border_cm_TY1) & !is.na(l_border_cm_TY2) & !is.na(u_border_cm_TY2) & 
              between(my.dbh.cm, l_border_cm_TY2, u_border_cm_TY2) ~ paste0(bark_div$bark_type[bark_div$species == my.bark.spp],"_TY_2"),
           # if there is only a lower border for type 2 and the diameter is bejond it 
           !is.na(l_border_cm_TY2) & is.na(u_border_cm_TY2) & is.na(l_border_cm_TY3) &
             my.dbh.cm >= l_border_cm_TY2 ~ paste0(bark_div$bark_type[bark_div$species == my.bark.spp],"_TY_2"),
           # if there is a lower border for type 2 and for type 3 but no upper for type 2 and the diameter is between type 2 and 3
           !is.na(l_border_cm_TY2) & is.na(u_border_cm_TY2) & !is.na(l_border_cm_TY3) &
             between(my.dbh.cm, l_border_cm_TY2, l_border_cm_TY3) ~ paste0(bark_div$bark_type[bark_div$species == my.bark.spp],"_TY_2"),
           !is.na(l_border_cm_TY3) & my.dbh.cm >= l_border_cm_TY3 ~ paste0(bark_div$bark_type[bark_div$species == my.bark.spp],"_TY_3"),
            # if there are no diameter specific bark types --> for most of the spp. species groups 
           is.na(u_border_cm_TY1) & is.na(l_border_cm_TY2) & is.na(u_border_cm_TY2) & is.na(l_border_cm_TY3)~ bark_div$bark_type[bark_div$species == my.bark.spp],
           TRUE ~ NA) 
  
  bark_TY_list[[i]] <- as.data.frame(cbind(
    "plot_ID" = c(my.plot.id), 
    "tree_ID" = c(my.tree.id),
    "inv" = c(my.inv),
    "bark_SP" = c(my.bark.spp),
    "bark_TY" = c(my.bark.ty))
  )
}
bark_type_df <- as.data.frame(rbindlist(bark_TY_list)) %>% 
  mutate(across(c("plot_ID", "tree_ID"), as.integer))

FSI_df <- FSI_df %>% left_join(., cbind(
  # dataset with calcualted number of bark types per plot ID 
  trees_data %>% 
    left_join(., bark_type_df, by = c("plot_ID", "tree_ID", "inv")) %>% 
    select(plot_ID, inv, bark_TY) %>% 
    distinct() %>% 
    group_by(plot_ID, inv) %>% 
    summarise(LT_n_bark_TY = as.numeric(n())), 
  # calculate FSI of bark diversity 
  "LT_FSI_bark_TY" = c(FSI(as.numeric((trees_data %>% 
                                         left_join(., bark_type_df, by = c("plot_ID", "tree_ID", "inv")) %>% 
                                         select(plot_ID, inv, bark_TY) %>%
                                         distinct() %>% 
                                         group_by(plot_ID, inv) %>% 
                                         summarise(LT_n_bark_TY = as.numeric(n())))$LT_n_bark_TY)))), 
  by = c("plot_ID")) %>% 
  distinct()




# 1.1.6. volume of trees with DBH ≥ 40 cm ------------------------------------------------------------------
trees_above_DBH_40 <- trees_data %>% filter(compartiment == "ag" & DBH_cm > 40) %>% distinct()
V_above_DBH_40_list <- vector("list", length = nrow(trees_above_DBH_40))
for (i in 1:nrow(trees_above_DBH_40)) {
  # i = 1

  # basic tree info
  # select one tree ID and plot ID for each individual tree per plot through unique(trees_data[, c("plot_ID", "tree_ID")])
  my.plot.id <- trees_above_DBH_40[i,"plot_ID"]
  my.tree.id <- trees_above_DBH_40[i ,"tree_ID"]
  my.inv <- trees_above_DBH_40[i ,"inv"]
  
  # select variales for tree object: tapes species, diameter, diameter measuring height, tree height
  spp = na.omit(unique(trees_above_DBH_40$tpS_ID[trees_above_DBH_40$plot_ID==my.plot.id & trees_above_DBH_40$tree_ID==my.tree.id]))
  Dm = na.omit(as.list(as.numeric(unique(trees_above_DBH_40$DBH_cm[trees_above_DBH_40$plot_ID==my.plot.id & trees_above_DBH_40$tree_ID==my.tree.id])))) 
  Hm = na.omit(as.list(as.numeric(unique(trees_above_DBH_40$DBH_h_cm[trees_above_DBH_40$plot_ID==my.plot.id & trees_above_DBH_40$tree_ID==my.tree.id])/100)))
  Ht = na.omit(as.numeric(unique(trees_above_DBH_40$H_m[trees_above_DBH_40$plot_ID==my.plot.id & trees_above_DBH_40$tree_ID==my.tree.id])))
  
  # create object  
  obj.trees <- tprTrees(spp, Dm, Hm, Ht, inv = 4)
  
  # calculate biomass per compartiment
  V.df <- as.data.frame(tprVolume(obj = obj.trees))[, 1] 
  
  # save volume and tree info into dataframe 
  V.info.df <- as.data.frame(cbind(
    "plot_ID" = c(my.plot.id), 
    "tree_ID" = c(my.tree.id), 
    "inv" = c(my.inv), 
    "V_m3_tree" = c(as.numeric(V.df)))) %>% 
    distinct()
  
  # export volume dataframe
  V_above_DBH_40_list[[i]] <- V.info.df
  
}
LT_V_m3_DBH40 <- as.data.frame(rbindlist(V_above_DBH_40_list))



# calculate FSI of trees with diameter above 40 cm over all plots 
FSI_df<- 
  FSI_df %>% 
  left_join(., cbind(
  # dataset with volume of trees over 40cm DBH per plot summed up in m3
  LT_V_m3_DBH40 %>% 
  group_by(plot_ID, inv) %>% 
  summarise(LT_Vm340_plot = sum(as.numeric(V_m3_tree))),
  # FSI of dataset with volume of trees over 40 cm DBH in m3 per plot 
 "LT_FSI_V40" = FSI(as.numeric((LT_V_m3_DBH40 %>% 
                      group_by(plot_ID, inv) %>% 
                      summarise(LT_Vm3_40_plot = sum(as.numeric(V_m3_tree))))$LT_Vm3_40_plot))
 ), 
 by = c("plot_ID", "inv"))




# 1.1.7. diversity of flowering and fructification trees ------------------------------------------------------------------
# i guess this reffers to how many different fructificating and flowering trees are at a plot? or per ha? 
# here we have to open a new column in x_bart divifing the trees in flowering & fructifying or not 



# 1.2. DEADWOOD -----------------------------------------------------------


# 1.2.1. number of decay classes ------------------------------------------------------------------
FSI_df <- FSI_df %>% 
  left_join(., DW_summary %>% 
  filter(plot_ID != "all" & decay == "all" & dw_type == "all" & dw_sp == "all") %>% 
  select(plot_ID, n_dec) %>% 
  distinct() %>% 
  mutate(DW_FSI_n_dec = as.numeric(FSI(n_dec))) %>% 
  rename("DW_n_dec" = "n_dec"), 
  by = "plot_ID")

# 1.2.2. average mean diameter of downed deadwood -------------------------------------------------------------------
FSI_df <- FSI_df %>% 
  left_join(., DW_data %>% 
  filter(ST_LY_type == "L") %>% 
  group_by(plot_ID) %>%
  summarise(DW_LY_mean_D_cm = mean(d_cm)) %>% 
  distinct() %>% 
  mutate(DW_FSI_LY_mean_D_cm = as.numeric(FSI(DW_LY_mean_D_cm))), 
  by = "plot_ID")
  

# 1.2.3. mean DBH of standing deadwood -------------------------------------------------------------------
FSI_df <- FSI_df %>% 
  left_join(., DW_data %>% 
  filter(ST_LY_type == "S") %>% 
  group_by(plot_ID) %>%
  summarise(DW_ST_mean_D_cm = mean(d_cm)) %>% 
  distinct() %>% 
  mutate(DW_FSI_ST_mean_D_cm = as.numeric(FSI(DW_ST_mean_D_cm))), 
  by = "plot_ID")









