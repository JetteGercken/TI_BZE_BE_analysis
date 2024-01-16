# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the  national soil inventory
# stock per hectar summarising for regeneration (RG), living trees (LT) and 
# deadwood (DW)


# ----- 0. SETUP ---------------------------------------------------------------

# ----- 0.1. packages and functions --------------------------------------------
source(paste0(getwd(), "/scripts/00_00_functions_library.R"))

# ----- 0.2. working directory -------------------------------------------------
here::here()
getwd()

out.path.BZE3 <- ("output/out_data/out_data_BZE/") 


# ----- 0.3 data import --------------------------------------------------------
# regeneration
# this dataset contains the plant specific inventory data of the regenertaion inventory of the HBI (BZE2), including stand and area info,  species groups and B, C, N stocks per tree 
RG_data <- read.delim(file = here("output/out_data/out_data_BZE/HBI_RG_update_4.csv"), sep = ";", dec = ",")
RG_stat_2 <- read.delim(file = here("output/out_data/out_data_BZE/HBI_RG_stat_2.csv"), sep = ";", dec = ",")
# this dataset contains the data of the deadwood inventory of the HBI (BZE2), including info about species groups and B, C, N stocks per tree 
DW_data <- read.delim(file = here("output/out_data/out_data_BZE/HBI_DW_update_4.csv"), sep = ";", dec = ",")
DW_stat_2 <- read.delim(file = here("output/out_data/out_data_BZE/HBI_DW_stat_2.csv"), sep = ";", dec = ",")
# this dataset contains the data of the tree inventory of the HBI (BZE2), including stand and area info,  species groups and B, C, N stocks per tree 
trees_data <- read.delim(file = here("output/out_data/out_data_BZE/HBI_LT_update_4.csv"), sep = ";", dec = ",")
trees_stat_2 <- read.delim(file = here("output/out_data/out_data_BZE/HBI_LT_stat_2.csv"), sep = ";", dec = ",") %>% select(-X)




# CALCULATIONS ------------------------------------------------------------


# 1. LIVING TREES -----------------------------------------------


# 1.2. number of speices per plot -----------------------------------------
n_SP_plot <- trees_data %>%
  filter(compartiment == "ag") %>%
  select(plot_ID, inv_year, SP_code) %>% 
  group_by(plot_ID, inv_year) %>% 
  distinct() %>% 
  summarise(n_SP = n())
  


# 1.2. stocks per hektar ------------------------------------------------------
# 1.2.1. Plot: stocks per hektar ------------------------------------------------------

if(nrow(trees_stat_2)!= 0){
  LT_BCNBAn_ha <- rbind(trees_data  %>% 
                          group_by(plot_ID, plot_A_ha, CCS_r_m, inv_year, compartiment) %>% 
                          # convert Biomass into tons per hectar and sum it up per sampling circuit 
                          reframe(B_CCS_t_ha = sum(ton(B_kg_tree))/plot_A_ha, # plot are is the area of the respecitve samplign circuit in ha 
                                  C_CCS_t_ha = sum(ton(C_kg_tree))/plot_A_ha,
                                  N_CCS_t_ha = sum(ton(N_kg_tree))/plot_A_ha, 
                                  BA_CCS_m2_ha = sum(BA_m2)/plot_A_ha, 
                                  n_trees_CCS_ha = n()/plot_A_ha) %>% 
                          distinct(), 
                        trees_stat_2)%>% 
    # now we summarise all the t/ha values of the cirlces per plot
    group_by(plot_ID, inv_year, compartiment) %>% 
    summarise(B_t_ha = sum(B_CCS_t_ha), 
              C_t_ha = sum(C_CCS_t_ha), 
              N_t_ha = sum(N_CCS_t_ha), 
              BA_m2_ha = sum(BA_CCS_m2_ha), 
              n_ha = sum(n_trees_CCS_ha)) %>% 
    mutate(stand_component = "LT")}else{
      LT_BCNBAn_ha <- trees_data %>% 
        group_by(plot_ID, CCS_r_m, inv_year, compartiment) %>% 
        # convert Biomass into tons per hectar and sum it up per sampling circuit 
        reframe(B_CCS_t_ha = sum(ton(B_kg_tree))/plot_A_ha, # plot are is the area of the respecitve samplign circuit in ha 
                C_CCS_t_ha = sum(ton(C_kg_tree))/plot_A_ha,
                N_CCS_t_ha = sum(ton(N_kg_tree))/plot_A_ha, 
                BA_CCS_m2_ha = sum(BA_m2)/plot_A_ha, 
                n_trees_CCS_ha = n()/plot_A_ha) %>% 
        distinct()%>% 
        # now we summarise all the t/ha values of the cirlces per plot
        group_by(plot_ID, inv_year, compartiment) %>% 
        summarise(B_t_ha = sum(B_CCS_t_ha), 
                  C_t_ha = sum(C_CCS_t_ha), 
                  N_t_ha = sum(N_CCS_t_ha),
                  BA_m2_ha = sum(BA_CCS_m2_ha), 
                  n_ha = sum(n_trees_CCS_ha)) %>% 
        mutate(stand_component = "LT")
    }


# 1.2.2. Plot, species: stocks per hektar ------------------------------------------------------
LT_SP_BCNBA_ha <- trees_data %>% 
  group_by(plot_ID, CCS_r_m, inv_year, SP_code, compartiment) %>% 
  # convert Biomass into tons per hectar and sum it up per sampling circuit 
  reframe(B_CCS_t_ha = sum(ton(B_kg_tree))/plot_A_ha, # plot are is the area of the respecitve samplign circuit in ha 
          C_CCS_t_ha = sum(ton(C_kg_tree))/plot_A_ha,
          N_CCS_t_ha = sum(ton(N_kg_tree))/plot_A_ha, 
          BA_CCS_m2_ha = sum(BA_m2)/plot_A_ha) %>% 
  distinct()%>% 
  # now we summarise all the t/ha values of the cirlces per plot
  group_by(plot_ID, inv_year, SP_code, compartiment) %>% 
  summarise(B_t_ha = sum(B_CCS_t_ha), 
            C_t_ha = sum(C_CCS_t_ha), 
            N_t_ha = sum(N_CCS_t_ha), 
            BA_m2_ha = sum(BA_CCS_m2_ha)) %>% 
  mutate(stand_component = "LT") %>% 
  #calcualte species compostiion by calcualting the percent of the respective species contributes to the overall basal area 
  left_join(LT_BCNBAn_ha %>% 
              select(plot_ID, inv_year, compartiment, BA_m2_ha) %>% 
              rename(BA_m2_ha_total = BA_m2_ha),
            by = c("plot_ID", "inv_year", "compartiment"), ) %>% 
  distinct() %>% 
  mutate(BA_percent = (BA_m2_ha/BA_m2_ha_total)*100) %>% 
  select(-"BA_m2_ha_total")

# 1.2.3. plot: species composition ------------------------------------------------------
besttype_list <- vector("list", length = length(unique(trees_data$plot_ID)))
for (i in 1:length(unique(trees_data$plot_ID))) {
  # i = 1
  my.plot.id <- unique(trees_data$plot_ID)[i]
  my.inv.year <- unique(trees_data$inv_year[trees_data$plot_ID == my.plot.id])
  my.sp.p.df <- unique(LT_SP_BCNBA_ha[LT_SP_BCNBA_ha$plot_ID == my.plot.id, ][, c("plot_ID", 
                                                                           "inv_year", 
                                                                           "SP_code",
                                                                           "BA_m2_ha", "BA_percent")])
  
  # calcaulte the composition / ration of coniferous and broadaleafed trees per plot  
  my.BLCF.p.df <- my.sp.p.df %>% 
    left_join(., SP_names_com_ID_tapeS %>% 
                 mutate(char_code_ger_lowcase = tolower(Chr_code_ger)) %>% 
                select(char_code_ger_lowcase, LH_NH), 
             by = c("SP_code" = "char_code_ger_lowcase")) %>% 
    group_by(plot_ID, inv_year, LH_NH) %>% 
    summarize(BA_m2_ha = sum(BA_m2_ha), 
              BA_per_LHNH = sum(BA_percent))
 
   # exptract the share of coniferous or broadleafed species at the plot
    # if there are no broadleafed/ coniferous species and the search returns an empty variable, set the share to 0 
  my.CF.share <- ifelse(length(my.BLCF.p.df$BA_per_LHNH[my.BLCF.p.df$LH_NH == "NB"]) == 0, 0, my.BLCF.p.df$BA_per_LHNH[my.BLCF.p.df$LH_NH == "NB"])
  my.BL.share <- ifelse(length(my.BLCF.p.df$BA_per_LHNH[my.BLCF.p.df$LH_NH == "LB"]) == 0, 0, my.BLCF.p.df$BA_per_LHNH[my.BLCF.p.df$LH_NH == "LB"]) 
  
  # select the species with the highest basal area share
  main.sp.p.df <- (my.sp.p.df %>% arrange(desc(BA_percent)))[1,] 
  # assign the stand type group to the species with the highest basal area share
  my.standtype.spec <- standtype(SP_names_com_ID_tapeS$bot_genus[tolower(SP_names_com_ID_tapeS$Chr_code_ger) == main.sp.p.df$SP_code],
                                SP_names_com_ID_tapeS$LH_NH[tolower(SP_names_com_ID_tapeS$Chr_code_ger) == main.sp.p.df$SP_code])
    
  # assign standtype to mono-species stand, if basal area is >= 70%
  besttype.mono <- case_when(my.standtype.spec == "FI" & main.sp.p.df$BA_percent >= 70 ~ "Fi-Rein" , 
                             my.standtype.spec == "KI" & main.sp.p.df$BA_percent >= 70 ~ "Ki-Rein",
                             my.standtype.spec == "aNH" & main.sp.p.df$BA_percent >= 70 ~ "sonst-Nd",
                             my.standtype.spec == "BU" & main.sp.p.df$BA_percent >= 70 ~ "Bu-Rein" , 
                             my.standtype.spec == "EI" & main.sp.p.df$BA_percent >= 70 ~ "Ei-Rein",
                             my.standtype.spec == "aLH" & main.sp.p.df$BA_percent >= 70 ~ "sonst-Ld", 
                          TRUE ~ NA)
  
  # if its not a single species stand we have to reassess the stand conditions
    # check if we can identify a Nadelholzmischbestand or Laubbolzmischbestand 
    # which means the overall share of conifers or broadleaved trees  
    besttype.strong.mix <- ifelse(is.na(besttype.mono) &  
                             # if there area more CF then BL trees (CF min 50%, BL <50%)
                               my.CF.share > my.BL.share & 
                             # but there is still a high amount of BL trees >30%
                                my.BL.share < 50 & my.BL.share > 30, "Nd-Lb-Misch", 
                             ifelse(is.na(besttype.mono) & 
                                      # if there are more BL then CF (BL min 50%, BL <50%)
                                      my.BL.share > my.CF.share & 
                                      # but there is still a high amount of BL trees >30%
                                      my.CF.share < 50 & my.CF.share > 30, "Lb-Nd-Misch", 
                                    NA))
  
  # assign stand types for stands wich are dominated by one catedory (CF, BL) but have a low amount 
  # of 
  besttype.mix  <- ifelse(is.na(besttype.mono) & is.na(besttype.strong.mix) & 
                            # if there area more CF then BL trees (CF min 50%, BL <50%)
                            my.CF.share >= 70 & 
                              # but there is still a high amount of BL trees >30%
                              my.BL.share <= 30, "Nd-Lb<30", 
                            ifelse(is.na(besttype.mono) & 
                                     # if there are more BL then CF (BL min 50%, BL <50%)
                                     my.BL.share >= 70 & 
                                     # but there is still a high amount of CF trees >30%
                                     my.CF.share <= 30, "Lb-Nd<30", 
                                   NA))
    
 
  besttype.final <- ifelse(!is.na(besttype.mono) & 
                             is.na(besttype.strong.mix) & 
                             is.na(besttype.mix), besttype.mono, 
                           ifelse(is.na(besttype.mono) &
                                    !is.na(besttype.strong.mix) &
                                    is.na(besttype.mix), besttype.strong.mix, 
                                  ifelse(is.na(besttype.mono) & 
                                           is.na(besttype.strong.mix) &
                                           !is.na(besttype.mix), besttype.mix, NA))) 
     
  
  besttype_list[[i]] <- as.data.frame(cbind(
    plot_ID = c(my.plot.id), 
    inv_year = c(my.inv.year), 
    dom_SP = c(main.sp.p.df$SP_code), 
    stand_type = c(besttype.final))) %>% 
    distinct()
  
}
stand_TY_P <- as.data.frame(rbindlist(besttype_list))   



# 1.3 forestry summary ----------------------------------------------------

# this shoudl contain the BA, Dg, Hg, 
trees_data %>% 
  filter(C_layer == 1 & compartiment == "ag") %>% 
  group_by(plot_ID, CCS_r_m, inv_year, SP_code) %>% 
  summarize(mean_DBH_CCS = mean(DBH_cm), 
            sd_DBH_CCS = sd(DBH_cm),
            mean_H_CCS = mean(H_m), 
            sd_H_CCS = sd(H_m), 
            mean_BA_m2_CCS = mean(BA_m2), 
            n_trees_CCS = n()) %>% 
  group_by(plot_ID, inv_year, SP_code) %>% 
  reframe(mean_DBH_cm = mean(mean_DBH_CCS), 
            mean_H_m = mean(mean_H_CCS), 
            mean_BA_m2 = mean(mean_BA_m2_CCS), 
            mean_BA_m2_tree = mean_BA_m2_CCS/n_trees_CCS, 
          D_g = ((sqrt((mean_BA_m2_tree/pi)))*2)*100
          )
    

# trees_data %>%     
#  filter(C_layer == 1 & compartiment == "ag") %>% 
#   group_by(inv, plot_ID, stand, SP_code, CCS_r_m) %>%    # group by plot and species, canopy layer and sampling circuit to calcualte all paremeters needed 
#   reframe(no_trees_CC = n(),
#             BA_m2_CCS = sum(BA_m2)/plot_A_ha,                        # sum up basal  area per sampling circuit to then reffer it to the hektar value of the respective circuit
#             no_trees_CCS = no_trees_CC/CC_A_ha,
#             mean_DBH_mm_CC = mean(DBH_cm*10),          # calculate mean DBH per sampling circuit and species and C layer and plot 
#             mean_H_m_CC = mean(na.omit(H_m))) %>%      # calculate mean height per sampling circuit and species and C layer and plot    
#   group_by(inv, plot_ID, stand, C_layer, SP_code )%>%            # group by plot and species,  canopy layer and sampling circuit to calcualte dg, hg 
#   summarize(no_trees_ha = sum(no_trees_CC_ha),                                # calculate number of trees per plot
#             BA_m2_ha = sum(BA_m2_CCS),               # calculate sum of BA across all sampling circuit to account for represnation of different trees in the sampling circuits
#             mean_DBH_mm = mean(mean_DBH_mm_CC),        # calculate mean of DBH across all sampling circuit to account for represnation of different trees in the sampling circuits
#             mean_H_m = mean(mean_H_m_CC),              # calculate mean of height across all sampling circuit to account for represnation of different trees in the sampling circuits
#             mean_BA_m2_tree = BA_m2_ha/no_trees_ha,
#             H_g = sum(mean(na.omit(mean_H_m))*BA_m2_ha)/sum(BA_m2_ha),    # Hoehe des Grundflächemittelstammes, calculation according to S. Schnell
#             mean_DBH_mm = mean(mean_DBH_mm),                           # mean diameter per species per canopy layer per plot
#             D_g = ((sqrt((mean_BA_m2_tree/pi)))*2)*100)


  


# 2. REGENERATION ---------------------------------------------------------
# 2.1. plot area - sum off all sampling circuits ---------------------------
# if there are plots that are labelled empty but have to included in the eare calcualtion 
if(nrow(RG_stat_2) != 0 && isTRUE(RG_stat_2)){
  RG_plot_A_ha <- rbind(RG_data %>% 
                          mutate(plot_A_ha = area_m2/10000) %>% 
                          select(plot_ID, inv_year, CCS_nr, plot_A_ha) %>% 
                          distinct(), 
                        RG_stat_2 %>% 
                          select(plot_ID, inv_year, CCS_nr, plot_A_ha)) %>% 
    group_by(plot_ID, inv_year) %>% 
    summarise(plot_A_ha = sum(plot_A_ha))}else{
      RG_plot_A_ha <- RG_data %>% 
        mutate(plot_A_ha = area_m2/10000) %>% 
        select(plot_ID, inv_year, CCS_nr, plot_A_ha) %>% 
        distinct() %>% 
        group_by(plot_ID, inv_year) %>%
        summarise(plot_A_ha = sum(plot_A_ha))
    }



# 2.1. number of RG plants  per hectar ----------------------------------------------
n_ha_RG <- RG_data %>% 
  left_join(., RG_plot_A_ha, by = c("plot_ID", "inv_year")) %>% 
  filter(compartiment == "ag") %>% 
  group_by(plot_ID, CCS_nr, plot_A_ha, inv_year) %>% 
  # sum number of trees  per sampling circuit
  summarise(n_trees_CCS = n()) %>% 
  # summarise all the tree numers values of the individual cirlces per plot and reffer them to the plot area
  group_by(plot_ID, inv_year, plot_A_ha) %>% 
  reframe(n_ha = sum(n_trees_CCS)/plot_A_ha) %>% 
  distinct()





# 3. DEADWOOD -------------------------------------------------------------
# 3.1. number of DW items per hectar ----------------------------------------------
n_ha_DW <- DW_data %>% 
  filter(compartiment == "ag") %>% 
  group_by(plot_ID, inv_year) %>% 
  reframe(n_plot = n()/plot_A_ha) %>% 
  distinct()



# 3.2. stocks per hectar deadwood -----------------------------------------
if(nrow(DW_stat_2)!=0 && isTRUE(DW_stat_2)){
  DW_stocks_ha<- rbind(DW_data %>% 
  group_by(plot_ID, plot_A_ha, inv_year, compartiment) %>% 
  # convert Biomass into tons per hectar and divide it by the plot area to calculate stock per hectar
  reframe(B_t_ha = sum(ton(B_kg_tree))/plot_A_ha, # plot are is the area of the respecitive sampling circuit in ha 
          C_t_ha = sum(ton(C_kg_tree))/plot_A_ha,
          N_t_ha = sum(ton(N_kg_tree))/plot_A_ha) %>% 
  distinct(), 
 DW_stat_2) %>% 
  mutate(stand_component = "DW")}else{
    DW_stocks_ha<- DW_data %>% 
      group_by(plot_ID, plot_A_ha, inv_year, compartiment) %>% 
      # convert Biomass into tons per hectar and divide it by the plot area to calculate stock per hectar
      reframe(B_t_ha = sum(ton(B_kg_tree))/plot_A_ha, # plot are is the area of the respecitive sampling circuit in ha 
            C_t_ha = sum(ton(C_kg_tree))/plot_A_ha,
            N_t_ha = sum(ton(N_kg_tree))/plot_A_ha) %>% 
      distinct()
  }
 
  






