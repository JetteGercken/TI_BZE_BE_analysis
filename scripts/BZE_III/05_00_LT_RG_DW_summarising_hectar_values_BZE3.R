# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the  national soil inventory
# BZE3 
# stock per hectar summarising for regeneration (RG), living trees (LT) and 
# deadwood (DW)


# ----- 0. SETUP ---------------------------------------------------------------

# ----- 0.1. packages and functions --------------------------------------------
source(paste0(getwd(), "/scripts/01_00_functions_library.R"))

# ----- 0.2. working directory -------------------------------------------------
here::here()
getwd()

out.path.BZE3 <- ("output/out_data/out_data_BZE/") 


# ----- 0.3 data import --------------------------------------------------------
# livgn trees
# this dataset contains the data of the tree inventory of the BZE3, including stand and area info,  species groups and B, C, N stocks per tree 
trees_data <- read.delim(file = here(paste0(out.path.BZE3, "BZE3_LT_update_4.csv")), sep = ",", dec = ".")
trees_stat_2 <- read.delim(file = here(paste0(out.path.BZE3, trees_data$inv[1], "_LT_stat_2.csv")), sep = ",", dec = ".") %>% 
  mutate(inv = inv_name(inv_year))

# regeneration
# this dataset contains the plant specific inventory data of the regenertaion inventory of the HBI (BZE2), including stand and area info,  species groups and B, C, N stocks per tree 
RG_data <- read.delim(file = here(paste0(out.path.BZE3, trees_data$inv[1], "_RG_update_4.csv")),sep = ",", dec = ".")
RG_stat_2 <- read.delim(file = here(paste0(out.path.BZE3, trees_data$inv[1], "_RG_stat_2.csv")), sep = ",", dec = ".") %>% 
  mutate(inv = inv_name(inv_year))

# deadwood
# this dataset contains the data of the deadwood inventory of the HBI (BZE2), including info about species groups and B, C, N stocks per tree 
DW_data <- read.delim(file = here(paste0(out.path.BZE3, trees_data$inv[1], "_DW_update_4.csv")), sep = ",", dec = ".")
DW_stat_2 <- read.delim(file = here(paste0(out.path.BZE3, trees_data$inv[1], "_DW_stat_2.csv")), sep = ",", dec = ".") %>% 
  mutate(inv = inv_name(inv_year))






# CALCULATIONS ------------------------------------------------------------

# 1. LIVING TREES -----------------------------------------------

# 1.2. number of speices per plot -----------------------------------------
LT_n_SP_plot <- trees_data %>%
  filter(compartiment == "ag") %>%
  select(plot_ID, inv, SP_code) %>% 
  group_by(plot_ID, inv) %>% 
  distinct() %>% 
  summarise(n_SP = n()) %>% 
  mutate(stand_component = "LT")


# 1.3. number of stand per plot -------------------------------------------
LT_n_stand_P <- trees_data %>% 
  filter(compartiment == "ag") %>%
  select(plot_ID, inv, stand) %>% 
  group_by(plot_ID, inv) %>% 
  distinct() %>% 
  summarise(n_stand = n()) %>% 
  mutate(stand_component = "LT")


# 1.4. stocks per hektar ------------------------------------------------------
# 1.4.1. Plot: stocks per hektar ------------------------------------------------------
if(exists('trees_stat_2') == TRUE && nrow(trees_stat_2)!= 0){
  LT_BCNBAn_ha <- plyr::rbind.fill(trees_data  %>% 
                                     group_by(plot_ID, plot_A_ha, CCS_r_m, inv, compartiment) %>% 
                                     # convert Biomass into tons per hectar and sum it up per sampling circuit 
                                     reframe(B_CCS_t_ha = sum(ton(B_kg_tree))/plot_A_ha, # plot are is the area of the respecitve samplign circuit in ha 
                                             C_CCS_t_ha = sum(ton(C_kg_tree))/plot_A_ha,
                                             N_CCS_t_ha = sum(ton(N_kg_tree))/plot_A_ha, 
                                             BA_CCS_m2_ha = sum(BA_m2)/plot_A_ha, 
                                             n_trees_CCS_ha = n()/plot_A_ha) %>% 
                                     distinct(), 
                                   trees_stat_2 %>% 
                                     # this is in case in 01_00_RG_LT_DW_plot_inv_status_sorting there were stat_2 datasets produced that do not hold any data but only NAs
                                     filter(!is.na(plot_ID)) ) %>% 
    # now we summarise all the t/ha values of the cirlces per plot
    group_by(plot_ID, inv, compartiment) %>% 
    summarise(B_t_ha = sum(B_CCS_t_ha), 
              C_t_ha = sum(C_CCS_t_ha), 
              N_t_ha = sum(N_CCS_t_ha), 
              BA_m2_ha = sum(BA_CCS_m2_ha), 
              n_ha = sum(n_trees_CCS_ha)) %>% 
    mutate(stand_component = "LT",
           stand = "all", 
           SP_code = "all")}else{
             LT_BCNBAn_ha <- trees_data %>% 
               group_by(plot_ID, CCS_r_m, inv, compartiment) %>% 
               # convert Biomass into tons per hectar and sum it up per sampling circuit 
               reframe(B_CCS_t_ha = sum(ton(B_kg_tree))/plot_A_ha, # plot are is the area of the respecitve samplign circuit in ha 
                       C_CCS_t_ha = sum(ton(C_kg_tree))/plot_A_ha,
                       N_CCS_t_ha = sum(ton(N_kg_tree))/plot_A_ha, 
                       BA_CCS_m2_ha = sum(BA_m2)/plot_A_ha, 
                       n_trees_CCS_ha = n()/plot_A_ha) %>% 
               distinct()%>% 
               # now we summarise all the t/ha values of the cirlces per plot
               group_by(plot_ID, inv, compartiment) %>% 
               summarise(B_t_ha = sum(B_CCS_t_ha), 
                         C_t_ha = sum(C_CCS_t_ha), 
                         N_t_ha = sum(N_CCS_t_ha),
                         BA_m2_ha = sum(BA_CCS_m2_ha), 
                         n_ha = sum(n_trees_CCS_ha)) %>% 
               mutate(stand_component = "LT", 
                      stand = "all", 
                      SP_code = "all")
           }



# 1.4.2. plot, species, stand: stocks per ha, finest summary --------------
if(exists('trees_stat_2') == TRUE && nrow(trees_stat_2)!= 0){
  LT_SP_ST_P_BCNBAn_ha <- plyr::rbind.fill(trees_data  %>% 
                                             group_by(plot_ID, plot_A_ha, CCS_r_m, inv, stand, SP_code, compartiment) %>% 
                                             # convert Biomass into tons per hectar and sum it up per sampling circuit 
                                             reframe(B_CCS_t_ha = sum(ton(B_kg_tree))/plot_A_ha, # plot are is the area of the respecitve samplign circuit in ha 
                                                     C_CCS_t_ha = sum(ton(C_kg_tree))/plot_A_ha,
                                                     N_CCS_t_ha = sum(ton(N_kg_tree))/plot_A_ha, 
                                                     BA_CCS_m2_ha = sum(BA_m2)/plot_A_ha, 
                                                     n_trees_CCS_ha = n()/plot_A_ha) %>% 
                                             distinct(), 
                                           trees_stat_2 %>% 
                                             # this is in case in 01_00_RG_LT_DW_plot_inv_status_sorting there were stat_2 datasets produced that do not hold any data but only NAs
                                             filter(!is.na(plot_ID))) %>% 
    # now we summarise all the t/ha values of the cirlces per plot
    group_by(plot_ID, inv, stand, SP_code, compartiment) %>% 
    summarise(B_t_ha = sum(B_CCS_t_ha), 
              C_t_ha = sum(C_CCS_t_ha), 
              N_t_ha = sum(N_CCS_t_ha), 
              BA_m2_ha = sum(BA_CCS_m2_ha), 
              n_ha = sum(n_trees_CCS_ha)) %>% 
    mutate(stand_component = "LT") %>% 
    #calcualte species compostiion by calcualting the percent of the respective species contributes to the overall basal area 
    left_join(LT_BCNBAn_ha %>% 
                select(plot_ID, inv, compartiment, BA_m2_ha) %>% 
                rename(BA_m2_ha_total = BA_m2_ha),
              by = c("plot_ID", "inv", "compartiment"), ) %>% 
    distinct() %>% 
    mutate(BA_percent = (BA_m2_ha/BA_m2_ha_total)*100) %>% 
    select(-"BA_m2_ha_total")
}else{
  LT_SP_ST_P_BCNBAn_ha <- trees_data %>% 
    group_by(plot_ID, CCS_r_m, inv, stand, SP_code, compartiment) %>% 
    # convert Biomass into tons per hectar and sum it up per sampling circuit 
    reframe(B_CCS_t_ha = sum(ton(B_kg_tree))/plot_A_ha, # plot are is the area of the respecitve samplign circuit in ha 
            C_CCS_t_ha = sum(ton(C_kg_tree))/plot_A_ha,
            N_CCS_t_ha = sum(ton(N_kg_tree))/plot_A_ha, 
            BA_CCS_m2_ha = sum(BA_m2)/plot_A_ha) %>% 
    distinct()%>% 
    # now we summarise all the t/ha values of the cirlces per plot
    group_by(plot_ID, inv, stand, SP_code, compartiment) %>% 
    summarise(B_t_ha = sum(B_CCS_t_ha), 
              C_t_ha = sum(C_CCS_t_ha), 
              N_t_ha = sum(N_CCS_t_ha),
              BA_m2_ha = sum(BA_CCS_m2_ha)) %>% 
    mutate(stand_component = "LT") %>% 
    #calcualte species compostiion by calcualting the percent of the respective species contributes to the overall basal area 
    left_join(LT_BCNBAn_ha %>% 
                select(plot_ID, inv, compartiment, BA_m2_ha) %>% 
                rename(BA_m2_ha_total = BA_m2_ha),
              by = c("plot_ID", "inv", "compartiment"), ) %>% 
    distinct() %>% 
    mutate(BA_percent = (BA_m2_ha/BA_m2_ha_total)*100) %>% 
    select(-"BA_m2_ha_total")
}

# 1.4.3. Plot, stand: stocks per hektar ------------------------------------------------------
LT_ST_BCNBAn_ha <- summarize_data(LT_SP_ST_P_BCNBAn_ha, 
                                  c("plot_ID", "inv", "compartiment", "stand"), 
                                  c("B_t_ha", "C_t_ha", "N_t_ha", "BA_m2_ha", "n_ha"), 
                                  operation = "sum_df") %>% 
  mutate(stand_component = "LT", 
         SP_code = "all") 


# 1.4.4. Plot, species: stocks per hektar ------------------------------------------------------
LT_SP_BCNBA_ha <- summarize_data(LT_SP_ST_P_BCNBAn_ha, 
                                 c("plot_ID", "inv", "compartiment", "SP_code"), 
                                 c("B_t_ha", "C_t_ha", "N_t_ha", "BA_m2_ha"), 
                                 operation = "sum_df") %>% 
  mutate(stand_component = "LT", 
         stand = "all") %>% 
  #calcualte species compostiion by calcualting the percent of the respective species contributes to the overall basal area 
  left_join(LT_BCNBAn_ha %>% 
              select(plot_ID, inv, compartiment, BA_m2_ha) %>% 
              rename(BA_m2_ha_total = BA_m2_ha),
            by = c("plot_ID", "inv", "compartiment"), ) %>% 
  distinct() %>% 
  mutate(BA_percent = (BA_m2_ha/BA_m2_ha_total)*100) %>% 
  select(-"BA_m2_ha_total")



# 1.5. plot: stand type ------------------------------------------------------
# 1.5.1. calcualte species composition and assing stand type ------------------------------------------------------
# requires the species plot wise summary
besttype_list <- vector("list", length = length(unique(trees_data$plot_ID)))
for (i in 1:length(unique(trees_data$plot_ID))) {
  # i = 1
  my.plot.id <- unique(trees_data$plot_ID)[i]
  my.inv <- unique(trees_data$inv[trees_data$plot_ID == my.plot.id])
  my.n.stand <- LT_n_stand_P$n_stand[LT_n_stand_P$plot_ID == my.plot.id]
  
  my.sp.p.df <- trees_data %>% 
    # only determine the stand type for the main stand
    filter(plot_ID == my.plot.id & stand == "A") %>% 
    group_by(plot_ID, CCS_r_m, inv, SP_code, compartiment) %>% 
    # convert Biomass into tons per hectar and sum it up per sampling circuit 
    reframe(BA_CCS_m2_ha = sum(BA_m2)/plot_A_ha) %>% 
    distinct()%>% 
    # now we summarise all the t/ha values of the cirlces per plot
    group_by(plot_ID, inv, SP_code, compartiment) %>% 
    summarise(BA_m2_ha = sum(BA_CCS_m2_ha)) %>% 
    mutate(stand_component = "LT") %>% 
    #calcualte species compostiion by calcualting the percent of the respective species contributes to the overall basal area 
    # join in total BA_m2_ha dataset to calculate relationship between species & standwise BA and standwise BA
    left_join(., 
              trees_data %>% 
                filter(plot_ID == my.plot.id & stand == "A") %>% 
                group_by(plot_ID, CCS_r_m, inv, compartiment) %>% 
                # convert Biomass into tons per hectar and sum it up per sampling circuit 
                reframe(BA_CCS_m2_ha = sum(BA_m2)/plot_A_ha) %>% 
                distinct()%>% 
                # now we summarise all the t/ha values of the cirlces per plot
                group_by(plot_ID, inv, compartiment) %>% 
                summarise(BA_m2_ha_total = sum(BA_CCS_m2_ha)) %>% 
                mutate(stand_component = "LT") %>% 
                distinct(), 
              by = c("plot_ID", "inv", "stand_component", "compartiment")) %>%
    mutate(BA_percent = (BA_m2_ha/BA_m2_ha_total)*100) %>% 
    select(c("plot_ID","inv", "SP_code", "BA_m2_ha", "BA_percent")) %>% 
    distinct() 
  
  
  
  # calcaulte the composition / ration of coniferous and broadaleafed trees per plot  
  my.BLCF.p.df <- my.sp.p.df %>% 
    left_join(., SP_names_com_ID_tapeS %>% 
                mutate(char_code_ger_lowcase = tolower(Chr_code_ger)) %>% 
                select(char_code_ger_lowcase, LH_NH), 
              by = c("SP_code" = "char_code_ger_lowcase")) %>% 
    group_by(plot_ID, inv, LH_NH) %>% 
    summarize(BA_m2_ha = sum(BA_m2_ha), 
              BA_per_LHNH = sum(BA_percent))
  
  # exptract the share of coniferous or broadleafed species at the plot
  # if there are no broadleafed/ coniferous species and the search returns an empty variable, set the share to 0 
  my.CF.share <- ifelse(length(my.BLCF.p.df$BA_per_LHNH[my.BLCF.p.df$LH_NH == "NB"]) == 0, 0, my.BLCF.p.df$BA_per_LHNH[my.BLCF.p.df$LH_NH == "NB"])
  my.BL.share <- ifelse(length(my.BLCF.p.df$BA_per_LHNH[my.BLCF.p.df$LH_NH == "LB"]) == 0, 0, my.BLCF.p.df$BA_per_LHNH[my.BLCF.p.df$LH_NH == "LB"]) 
  
  # select the species with the highest basal area share: 
  # this only selects the one row with the highest value, 
  # which enables to ensure that even if there are multiple species of the category "sonstiges Laubholz"/ "sonstiges Nadelholz" only one species will be selected
  # which is a requirement to assing a single species stand as it has to be dominated by ONE kind of species of the species groups (BU, EI, FI, KI, oBL, oCF)
  main.sp.p.df <- (my.sp.p.df %>% arrange(desc(BA_percent)))[1,] 
  # assign the stand type group to the species with the highest basal area share
  my.standtype.spec <- standtype(SP_names_com_ID_tapeS$bot_genus[tolower(SP_names_com_ID_tapeS$Chr_code_ger) == main.sp.p.df$SP_code],
                                 SP_names_com_ID_tapeS$LH_NH[tolower(SP_names_com_ID_tapeS$Chr_code_ger) == main.sp.p.df$SP_code])
  
  # assign standtype to mono-species stand, if basal area is >= 70%
  # the number codes of the stand types are listed in neu_x_besttyp_bestand
  besttype.mono <- case_when(my.standtype.spec == "FI" & main.sp.p.df$BA_percent >= 70 ~  1,  # "Fi-Rein"
                             my.standtype.spec == "KI" & main.sp.p.df$BA_percent >= 70 ~ 2,   # "Ki-Rein",
                             my.standtype.spec == "aNH" & main.sp.p.df$BA_percent >= 70 ~ 3,  #"sonst-Nd",
                             my.standtype.spec == "BU" & main.sp.p.df$BA_percent >= 70 ~ 4,   # "Bu-Rein" , 
                             my.standtype.spec == "EI" & main.sp.p.df$BA_percent >= 70 ~ 5,   # "Ei-Rein",
                             my.standtype.spec == "aLH" & main.sp.p.df$BA_percent >= 70 ~ 8,  # "sonst-Ld", 
                             TRUE ~ NA)
  
  # if its not a single species stand we have to reassess the stand conditions
  # check if we can identify a Nadelholzmischbestand or Laubbolzmischbestand 
  # which means the overall share of conifers or broadleaved trees  
  besttype.strong.mix <- ifelse(is.na(besttype.mono) &  
                                  # if there area more CF then BL trees (CF min 50%, BL <50%)
                                  my.CF.share > my.BL.share & 
                                  # but there is still a high amount of BL trees >30%
                                  my.BL.share < 50 & my.BL.share > 30, 6,        # "Nd-Lb-Misch", 
                                ifelse(is.na(besttype.mono) & 
                                         # if there are more BL then CF (BL min 50%, BL <50%)
                                         my.BL.share > my.CF.share & 
                                         # but there is still a high amount of BL trees >30%
                                         my.CF.share < 50 & my.CF.share > 30, 7,  # "Lb-Nd-Misch", 
                                       NA))
  
  # assign stand types for stands wich are dominated by one catedory (CF, BL) but have a low amount 
  # of 
  besttype.mix  <- ifelse(is.na(besttype.mono) & is.na(besttype.strong.mix) & 
                            # if there area more CF then BL trees (CF min 50%, BL <50%)
                            my.CF.share >= 70 & 
                            # but there is still a high amount of BL trees >30%
                            my.BL.share <= 30, 9,         # "Nd-Lb<30", 
                          ifelse(is.na(besttype.mono) & 
                                   # if there are more BL then CF (BL min 50%, BL <50%)
                                   my.BL.share >= 70 & 
                                   # but there is still a high amount of CF trees >30%
                                   my.CF.share <= 30, 10, # "Lb-Nd<30", 
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
    inv = c(my.inv), 
    dom_SP = c(main.sp.p.df$SP_code), 
    stand_type = c(besttype.final),
    n_stands = c(my.n.stand), 
    stand_component = c("LT")
  )) %>% 
    distinct()
  
}
LT_stand_TY_P <- as.data.frame(rbindlist(besttype_list)) 



# 1.6. average values ----------------------------------------------------
# 1.6.1. create "pseudo stands" -------------------------------------------
LT_avg_SP_ST_P_list <- vector("list", length = length(unique(trees_data$plot_ID))) 
LT_avg_SP_P_list <- vector("list", length = length(unique(trees_data$plot_ID))) 
LT_avg_P_list <- vector("list", length = length(unique(trees_data$plot_ID))) 
for (i in 1:length(unique(trees_data$plot_ID))) {
  # i = 1
  my.plot.id <- unique(trees_data$plot_ID)[i]
  # select all trees by only one compartiment of each tree to make sure the tree enters the dataframe only once
  my.tree.df <- trees_data[trees_data$plot_ID == my.plot.id & trees_data$compartiment == "ag", ] 
  my.n.ha.df <- trees_data %>% filter(compartiment == "ag" & plot_ID == my.plot.id) %>% group_by(plot_ID, CCS_r_m) %>% reframe(n_ha_CCS = n()/plot_A_ha) %>% distinct()
  my.n.plot.df <- trees_data %>% filter(compartiment == "ag" & plot_ID == my.plot.id) %>% group_by(plot_ID, CCS_r_m) %>% reframe(n_CCS = n()) %>% distinct()
  
  my.n.ha.df$n.rep.each.tree <- round(my.n.ha.df$n_ha_CCS/my.n.plot.df$n_CCS)
  
  # repeat every tree per circle by the number this tree would be repeated by to reach it´s ha number
  # so every tree id repeated as often as it would be represented on a hectar)
  # https://stackoverflow.com/questions/11121385/repeat-rows-of-a-data-frame
  my.tree.rep.df <- rbind(
    # 5m circle
    my.tree.df[my.tree.df$CCS_r_m == 5.64, ][rep(seq_len(nrow(my.tree.df[my.tree.df$CCS_r_m == 5.64, ])), 
                                                 each = my.n.ha.df$n.rep.each.tree[my.n.ha.df$CCS_r_m == 5.64]), ],
    # 12m circle
    my.tree.df[my.tree.df$CCS_r_m == 12.62, ][rep(seq_len(nrow(my.tree.df[my.tree.df$CCS_r_m == 12.62, ])), 
                                                  each = my.n.ha.df$n.rep.each.tree[my.n.ha.df$CCS_r_m == 12.62] ), ],
    # 17m circle
    my.tree.df[my.tree.df$CCS_r_m == 17.84, ][rep(seq_len(nrow(my.tree.df[my.tree.df$CCS_r_m == 17.84, ])), 
                                                  each = my.n.ha.df$n.rep.each.tree[my.n.ha.df$CCS_r_m == 17.84]), ])
  
  
  LT_avg_SP_ST_P_list[[i]] <- my.tree.rep.df %>% 
    group_by(plot_ID, inv, SP_code, stand) %>% 
    summarise(mean_DBH_cm = mean(DBH_cm), 
              sd_DBH_cm = sd(DBH_cm),
              Dg_cm = ((sqrt(mean(BA_m2)/pi))*2)*100,  
              mean_BA_m2 = mean(BA_m2),
              mean_H_m = mean(H_m), 
              sd_H_m = sd(H_m), 
              Hg_m = sum(mean(na.omit(mean_H_m))*sum(BA_m2))/sum(sum(BA_m2))) %>% 
    mutate(stand_component = "LT")
  
  LT_avg_SP_P_list[[i]] <- my.tree.rep.df %>% 
    group_by(plot_ID, inv, SP_code) %>% 
    summarise(mean_DBH_cm = mean(DBH_cm), 
              sd_DBH_cm = sd(DBH_cm),
              Dg_cm = ((sqrt(mean(BA_m2)/pi))*2)*100,  
              mean_BA_m2 = mean(BA_m2),
              mean_H_m = mean(H_m), 
              sd_H_m = sd(H_m), 
              Hg_m = sum(mean(na.omit(mean_H_m))*sum(BA_m2))/sum(sum(BA_m2))) %>% 
    mutate(stand_component = "LT", 
           stand = "all")
  
  LT_avg_P_list[[i]] <- my.tree.rep.df %>% 
    group_by(plot_ID, inv) %>% 
    summarise(mean_DBH_cm = mean(DBH_cm), 
              sd_DBH_cm = sd(DBH_cm),
              Dg_cm = ((sqrt(mean(BA_m2)/pi))*2)*100,  
              mean_BA_m2 = mean(BA_m2),
              mean_H_m = mean(H_m), 
              sd_H_m = sd(H_m), 
              Hg_m = sum(mean(na.omit(mean_H_m))*sum(BA_m2))/sum(sum(BA_m2))) %>% 
    mutate(stand_component = "LT", 
           SP_code = "all",
           stand = "all")
  
}
LT_avg_SP_ST_P <- as.data.frame(rbindlist(LT_avg_SP_ST_P_list))
LT_avg_SP_P <- as.data.frame(rbindlist(LT_avg_SP_P_list))
LT_avg_P <- as.data.frame(rbindlist(LT_avg_P_list))



# 1.7. binding LT data together -------------------------------------------------------------------------------------------------------

# 1.7.1. LT Species data -------------------------------------------------------------------------------------------------------------
LT_SP_ST_P <- LT_SP_ST_P_BCNBAn_ha  %>% 
  left_join(., LT_stand_TY_P %>% 
              mutate_at(c('plot_ID'), as.integer),
            by = c("plot_ID", "inv", "stand_component"))  %>% 
  left_join(LT_avg_SP_ST_P,  
            by = c("plot_ID", "inv", "stand_component", "SP_code", "stand")) %>% 
  select(-(n_ha))


# 1.7.2. LT Species data -------------------------------------------------------------------------------------------------------------
LT_SP_P <- LT_SP_BCNBA_ha  %>%  
  left_join(., LT_stand_TY_P %>% 
              mutate_at(c('plot_ID', 'n_stands'), as.integer),
            by = c("plot_ID", "inv", "stand_component")) %>% 
  left_join(., LT_avg_SP_P, 
            by = c("plot_ID", "inv", "stand_component", "SP_code", "stand")) 


# 1.7.3. LT stand data ----------------------------------------------------
LT_ST_P <- LT_ST_BCNBAn_ha  %>%  
  left_join(., LT_stand_TY_P %>% 
              mutate_at(c('plot_ID'), as.integer),
            by = c("plot_ID", "inv", "stand_component"))


# 1.7.4. LT plot data ----------------------------------------------------------------------------------------------------------------
LT_P <- LT_BCNBAn_ha %>% 
  left_join(., LT_stand_TY_P %>% 
              mutate_at(c('plot_ID'), as.integer),
            by = c("plot_ID", "inv", "stand_component")) %>% 
  left_join(., LT_avg_P, 
            by = c("plot_ID", "inv", "stand_component", "SP_code", "stand")) %>% 
  left_join(., LT_n_SP_plot, 
            by = c("plot_ID", "inv", "stand_component"))



# 1.7.5. summrizing LT data by stand type ---------------------------------
LT_TY <- LT_P %>%
  select(stand_type, compartiment, stand_component, inv, B_t_ha, C_t_ha, N_t_ha) %>% 
  distinct() %>%
  group_by(stand_type, compartiment, stand_component, inv) %>% 
  summarise(B_t_ha = mean(B_t_ha),
            C_t_ha = mean(C_t_ha), 
            N_t_ha = mean(N_t_ha))%>%
  left_join(., 
            LT_P %>% 
              select(stand_type, compartiment, stand_component, inv, BA_m2_ha, n_ha, n_SP) %>% 
              # select only compartiment "ag" so ba, n_ha, n_SP are only calculated for compartiment "ag", meaning with 1 row per plot not 6 as we have compartiments
              filter(compartiment == "ag") %>%
              group_by(stand_type, compartiment, stand_component, inv) %>% 
              summarise(BA_m2_ha = mean(BA_m2_ha), 
                        n_ha = mean(n_ha), 
                        n_SP = mean(n_SP)) %>% 
              ungroup() %>%
              select(-c("compartiment")), 
            by = c("stand_type", "stand_component", "inv")) %>%
  arrange(as.numeric(stand_type)) %>% 
  mutate(plot_ID = "all", 
         stand = "all",
         dom_SP = "all", 
         SP_code = "all") %>% 
  distinct()

# 1.7.6. rbinding LT data together ----------------------------------------
LT_summary <- plyr::rbind.fill(LT_SP_ST_P, 
                               LT_SP_P,
                               LT_ST_P,
                               LT_P, 
                               LT_TY) %>% 
  arrange(plot_ID, stand, SP_code, compartiment)

# to get only stand type summarised data one had to filter for: 
# plot_ID == "all" & SP_code == "all" & stand  == "all"
# to get the plotwise summarised data one has to filter for: 
# plot_ID != "all" & SP_code == "all" & stand  == "all"
# to get the species & plotwise wise summarised data one has to filter for: 
# plot_ID != "all" & SP_code != "all" & stand == "all"
# to get the stand & plotwise  wise summarised data one has to filter for: 
# plot_ID != "all" & SP_code == "all" & stand != "all"
# to get the stand , species & plotwise  wise summarised data one has to filter for: 
# plot_ID != "all" & SP_code != "all" & stand != "all"

# 2. REGENERATION ---------------------------------------------------------
# 2.1. plot area - sum off all sampling circuits ---------------------------
# if there are plots that are labelled empty but have to included in the eare calcualtion 
if(exists('RG_stat_2') == TRUE && nrow(RG_stat_2) != 0){
  RG_plot_A_ha <- rbind(RG_data %>% 
                          mutate(plot_A_ha = as.numeric(area_m2)/10000) %>% 
                          select(plot_ID, inv, CCS_nr, plot_A_ha) %>% 
                          distinct(), 
                        RG_stat_2 %>% 
                          # this is in case in 01_00_RG_LT_DW_plot_inv_status_sorting there were stat_2 datasets produced that do not hold any data but only NAs
                          filter(!is.na(plot_ID))  %>% 
                          select(plot_ID, inv, CCS_nr, plot_A_ha)) %>% 
    group_by(plot_ID, inv) %>% 
    summarise(plot_A_ha = sum(as.numeric(plot_A_ha)))}else{
      RG_plot_A_ha <- RG_data %>% 
        mutate(plot_A_ha = as.numeric(area_m2)/10000) %>% 
        select(plot_ID, inv, CCS_nr, plot_A_ha) %>% 
        distinct() %>% 
        group_by(plot_ID, inv) %>%
        summarise(plot_A_ha = sum(as.numeric(plot_A_ha)))
    }


# 2.2. number of RG  plants  per hectar ----------------------------------------------
RG_n_ha <- RG_data %>% 
  filter(compartiment == "ag") %>% 
  left_join(., RG_plot_A_ha, by = c("plot_ID", "inv")) %>% 
  group_by(plot_ID, inv) %>% 
  # sum number of trees  per sampling circuit
  reframe(n_ha = n()/plot_A_ha) %>% 
  distinct() %>% 
  mutate(stand_component = "RG")

RG_n_ha_ST <- RG_data %>% 
  filter(compartiment == "ag") %>% 
  left_join(., RG_plot_A_ha, by = c("plot_ID", "inv")) %>% 
  group_by(plot_ID, inv, stand) %>% 
  # sum number of trees  per sampling circuit
  reframe(n_ha = n()/plot_A_ha) %>% 
  distinct() %>% 
  mutate(stand_component = "RG")

# 2.3. number of RG  species per hectar ----------------------------------------------
RG_n_SP_plot <- RG_data %>%
  filter(compartiment == "ag") %>%
  select(plot_ID, inv, SP_code) %>% 
  group_by(plot_ID, inv) %>% 
  distinct() %>% 
  summarise(n_SP = n()) %>% 
  mutate(stand_component = "RG")


# 2.4. RG big summary combining all grouping variables --------------------------------------------------------
# 2.4.1. RG summary by plot, inventory, compartiment, species and  -------------------------------------------
if(exists('RG_stat_2') == TRUE && nrow(RG_stat_2) != 0){
  RG_SP_ST_BCN_ha <- plyr::rbind.fill(
    RG_data %>%
      left_join(., RG_plot_A_ha, by = c("plot_ID", "inv")) %>% 
      group_by(plot_ID, CCS_nr, plot_A_ha, inv, stand, compartiment, SP_code) %>% 
      # sum number of trees  per sampling circuit
      reframe(B_t_ha = sum(ton(B_kg_tree))/plot_A_ha, # plot are is the area of the respecitve samplign circuit in ha 
              C_t_ha = sum(ton(C_kg_tree))/plot_A_ha,
              N_t_ha = sum(ton(N_kg_tree))/plot_A_ha) %>% 
      distinct() , 
    RG_stat_2 %>% 
      # this is in case in 01_00_RG_LT_DW_plot_inv_status_sorting there were stat_2 datasets produced that do not hold any data but only NAs
      filter(!is.na(plot_ID))  %>% 
      select(plot_ID, CCS_nr, plot_A_ha, inv, compartiment, B_t_ha, C_t_ha, N_t_ha)
  ) %>% 
    arrange(plot_ID) %>% 
    group_by(plot_ID, plot_A_ha, inv, stand, compartiment, SP_code) %>%
    summarise(B_t_ha = sum(B_t_ha),
              C_t_ha = sum(C_t_ha),
              N_t_ha = sum(N_t_ha))%>% 
    mutate(stand_component = "RG") 
}else{
  RG_SP_ST_BCN_ha <-     RG_data %>%
    left_join(., RG_plot_A_ha, by = c("plot_ID", "inv")) %>% 
    group_by(plot_ID, CCS_nr, plot_A_ha, inv, stand, compartiment, SP_code) %>% 
    # sum stocks of trees  per sampling circuit, stand, compartiment, and SP_code
    reframe(B_t_ha = sum(ton(B_kg_tree))/plot_A_ha, # plot are is the area of the respecitve samplign circuit in ha 
            C_t_ha = sum(ton(C_kg_tree))/plot_A_ha,
            N_t_ha = sum(ton(N_kg_tree))/plot_A_ha) %>% 
    distinct() %>% 
    arrange(plot_ID) %>% 
    group_by(plot_ID, plot_A_ha, inv, stand, compartiment, SP_code) %>%
    summarise(B_t_ha = sum(B_t_ha),
              C_t_ha = sum(C_t_ha),
              N_t_ha = sum(N_t_ha)) %>% 
    mutate(stand_component = "RG")
}


## RG big summary final
RG_summary <- plyr::rbind.fill(
  # RG summray by plot, species, stand 
  RG_SP_ST_BCN_ha, 
  # 2.4.2. RG big summary by plot and species, without grouping by stand ---------------------------------------------------------
  summarize_data(RG_SP_ST_BCN_ha,
                 c("stand_component", "plot_ID", "inv", "compartiment", "SP_code"),  # variables to group by
                 c("B_t_ha", "C_t_ha", "N_t_ha"), # variables to sum up
                 operation = "sum_df") %>% # statistical operation 
    mutate(stand = "all"),
  # 2.4.3. RG summary by plot and stand, without grouping by species ---------------------------------------------------------
  summarize_data(RG_SP_ST_BCN_ha,
                 c("stand_component", "plot_ID", "inv", "compartiment", "stand"),  # variables to group by
                 c("B_t_ha", "C_t_ha", "N_t_ha"), # variables to sum up
                 operation = "sum_df") %>% # statistical operation
    left_join(., RG_n_ha_ST, by = c("plot_ID", "inv", "stand_component", "stand")) %>% 
    mutate(SP_code = "all"),
  # 2.4.4. RG summary by plot, inventory, compartiment, not by speci --------
  summarize_data(RG_SP_ST_BCN_ha,
                 c("stand_component", "plot_ID", "inv", "compartiment"),  # variables to group by
                 c("B_t_ha", "C_t_ha", "N_t_ha"), # variables to sum up
                 operation = "sum_df") %>% # statistical operation 
    # join in number of plants and species per ha to plowise summary 
    left_join(., RG_n_ha %>% select(plot_ID, inv,stand_component, n_ha), 
              by = c("plot_ID", "inv", "stand_component")) %>% 
    left_join(., RG_n_SP_plot, 
              by = c("plot_ID", "inv", "stand_component")) %>% 
    mutate(n_ha = ifelse(is.na(n_ha), 0, n_ha), 
           n_SP = ifelse(is.na(n_SP), 0, n_SP), 
           stand = "all", 
           SP_code = "all")
) %>% # close rbind
  distinct() %>% 
  arrange(plot_ID)






# 3. DEADWOOD -------------------------------------------------------------
# 3.1. DW summary per plot per SP per DW type per Dec state ---------------------------------------------------------
# create one very fine grouped summary for deadwood which we sum up into different groups later on 
if(isTRUE(exists('DW_stat_2') == TRUE && nrow(DW_stat_2)!=0) ==T ){
  DW_BCN_ha_SP_TY_DEC_P <- plyr::rbind.fill(DW_data %>% 
                                              group_by(plot_ID, inv, dw_sp, dw_type, decay, compartiment) %>% 
                                              # convert Biomass into tons per hectar and divide it by the plot area to calculate stock per hectar
                                              reframe(B_t_ha = sum(ton(B_kg_tree))/plot_A_ha, # plot are is the area of the respecitive sampling circuit in ha 
                                                      C_t_ha = sum(ton(C_kg_tree))/plot_A_ha,
                                                      N_t_ha = sum(ton(N_kg_tree))/plot_A_ha) %>% 
                                              distinct(), 
                                            DW_stat_2 %>% filter(!is.na(plot_ID)) %>% select(-c( plot_A_ha))) %>% 
    mutate(stand_component = "DW")
}else{
  DW_BCN_ha_SP_TY_DEC_P <- DW_data %>% 
    group_by(plot_ID, inv, dw_sp, dw_type, decay, compartiment, plot_A_ha) %>% 
    # convert Biomass into tons per hectar and divide it by the plot area to calculate stock per hectar
    reframe(B_t_ha = sum(ton(B_kg_tree))/plot_A_ha, # plot are is the area of the respecitive sampling circuit in ha 
            C_t_ha = sum(ton(C_kg_tree))/plot_A_ha,
            N_t_ha = sum(ton(N_kg_tree))/plot_A_ha) %>% 
    distinct()
}



# 3.4. DW big summary including all grouping variables and combinations -------------------------

# 3.4.1. grouped by species, decay type, deadwoodtype, plot, compartiment, inventory ------------------------------------------------------------------
DW_summary <- 
  plyr::rbind.fill(
    DW_BCN_ha_SP_TY_DEC_P,
    # 3.4.2. grouped by species, deadwoodtype, plot, compartiment, inventory. not by decay type anymore------------------------------------------------------------------
    summarize_data(DW_BCN_ha_SP_TY_DEC_P, 
                   c("plot_ID", "inv", "dw_sp", "dw_type", "compartiment"), 
                   c("B_t_ha", "C_t_ha", "N_t_ha"), 
                   operation = "sum_df") %>%
      mutate(decay = "all"),
    
    # 3.4.3. DW grouped by species, decay, plot, compartiment, inventory, not by deadwood type anymore --------------------------------------------------------------
    summarize_data(DW_BCN_ha_SP_TY_DEC_P, 
                   c("plot_ID", "inv", "dw_sp", "decay", "compartiment"), 
                   c("B_t_ha", "C_t_ha", "N_t_ha"), 
                   operation = "sum_df") %>% 
      mutate(dw_type = "all") ,
    
    # 3.4.4. DW grouped by deadwoodtype, decay, plot, compartiment, inventory, not by species type anymore ---------------------------------------------------------------
    summarize_data(DW_BCN_ha_SP_TY_DEC_P, 
                   c("plot_ID", "inv", "dw_type", "decay", "compartiment"), 
                   c("B_t_ha", "C_t_ha", "N_t_ha"), 
                   operation = "sum_df") %>% 
      mutate(dw_sp = "all"),
    
    # 3.4.5. DW grouped by deadwoodtype, plot, compartiment, inventory, not by species and decay type anymore ---------------------------------------------------------------
    summarize_data(DW_BCN_ha_SP_TY_DEC_P, 
                   c("plot_ID", "inv", "dw_type", "compartiment"), 
                   c("B_t_ha", "C_t_ha", "N_t_ha"), 
                   operation = "sum_df") %>%
      left_join(., DW_data %>% 
                  filter(compartiment == "ag") %>% 
                  distinct() %>% 
                  group_by(plot_ID, inv, ST_LY_type, dw_type) %>% 
                  summarise(mean_d_cm = mean(d_cm),
                            sd_d_cm = sd(d_cm),
                            mean_l_m = mean(l_dm/10),
                            sd_l_m = sd(l_dm/10)),
                by = c("plot_ID", "inv", "dw_type"), 
                multiple = "all") %>% 
      mutate(dw_sp = "all", 
             decay = "all"),
    
    # 3.4.6. DW grouped by decay, plot, compartiment, inventory, not by species and deadwood type anymore ---------------------------------------------------------------
    summarize_data(DW_BCN_ha_SP_TY_DEC_P, 
                   c("plot_ID", "inv", "decay", "compartiment"), 
                   c("B_t_ha", "C_t_ha", "N_t_ha"), 
                   operation = "sum_df") %>%
      left_join(., DW_data %>% 
                  filter(compartiment == "ag") %>% 
                  distinct() %>% 
                  group_by(plot_ID, inv, decay) %>% 
                  summarise(mean_d_cm = mean(d_cm),
                            sd_d_cm = sd(d_cm),
                            mean_l_m = mean(l_dm/10),
                            sd_l_m = sd(l_dm/10)), 
                by = c("plot_ID", "inv", "decay"), 
                multiple = "all") %>%
      mutate(dw_sp = "all", 
             dw_type = "all") ,
    
    # 3.4.7. DW grouped by species group, plot, compartiment, inventory, not by decay and deadwood type anymore ---------------------------------------------------------------
    summarize_data(DW_BCN_ha_SP_TY_DEC_P, 
                   c("plot_ID", "inv", "dw_sp", "compartiment"), 
                   c("B_t_ha", "C_t_ha", "N_t_ha"), 
                   operation = "sum_df") %>% 
      # mean and sd of length and diameter of deadwood 
      left_join(., DW_data %>% 
                  filter(compartiment == "ag") %>% 
                  distinct() %>% 
                  group_by(plot_ID, inv, dw_sp) %>% 
                  summarise(mean_d_cm = mean(d_cm),
                            sd_d_cm = sd(d_cm),
                            mean_l_m = mean(l_dm/10),
                            sd_l_m = sd(l_dm/10)), 
                by = c("plot_ID", "inv", "dw_sp"), 
                multiple = "all") %>%
      mutate(decay = "all", 
             dw_type = "all") ,
    
    # 3.4.8.DW grouped by species group, plot, compartiment, inventory, not by decay, species and deadwood type anymore ----------------------------------------------------------------
    summarize_data(DW_BCN_ha_SP_TY_DEC_P, 
                   c("plot_ID", "inv", "compartiment"), 
                   c("B_t_ha", "C_t_ha", "N_t_ha"), 
                   operation = "sum_df") %>%
      distinct() %>% 
      # average values over all deadwood items per plot
      left_join(., DW_data %>% 
                  filter(compartiment == "ag") %>% 
                  distinct() %>% 
                  group_by(plot_ID, inv) %>% 
                  summarise(mean_d_cm = mean(d_cm),
                            sd_d_cm = sd(d_cm),
                            mean_l_m = mean(l_dm/10),
                            sd_l_m = sd(l_dm/10)), 
                by = c("plot_ID", "inv"), 
                multiple = "all") %>%  
      # number of DW items per ha
      left_join(., DW_data %>% 
                  filter(compartiment == "ag") %>% 
                  group_by(plot_ID, inv) %>% 
                  reframe(n_ha = n()/plot_A_ha) %>% 
                  distinct(), 
                multiple = "all",
                by = c("plot_ID", "inv")) %>% 
      # number of decay types per plot
      left_join(DW_data %>% 
                  filter(compartiment == "ag") %>% 
                  select(plot_ID, inv, decay) %>% 
                  distinct() %>% 
                  group_by(plot_ID, inv) %>% 
                  summarise(n_dec = n()), 
                multiple = "all",
                by = c("plot_ID", "inv")) %>% 
      # number of deadwood types per plot
      left_join(DW_data %>% 
                  filter(compartiment == "ag") %>% 
                  select(plot_ID, inv, dw_type) %>% 
                  distinct() %>% 
                  group_by(plot_ID, inv) %>% 
                  summarise(n_dw_TY = n()), 
                multiple = "all",
                by = c("plot_ID", "inv")) %>% 
      mutate(decay = "all", 
             dw_type = "all", 
             dw_sp = "all") 
  ) %>%  # close rbind
  # add stand component for those datasets where it´s not included yet
  mutate(stand_component = "DW") %>% 
  distinct() %>% 
  arrange(plot_ID)






# 4. creating dataset with all stand components ---------------------------
LT_RG_DW_P <- 
  plyr::rbind.fill(
    plyr::rbind.fill(
      #living tree summary all group combination possible , without stocks grouped by stand type tho
      LT_summary %>% filter(plot_ID != "all") %>% select(-c(dom_SP, stand_type, n_stands))
      #regeneration summary all group combination possible  
      ,RG_summary
      #deadwood summary all group combination possible  
      ,DW_summary,
      # dataset with all stand compnents, stand and species combined
      # select only tree plots with plot_ID, not stand type summaries
      (rbind(LT_summary %>% filter(plot_ID != "all" & stand == "all" & SP_code == "all") %>% select(plot_ID, inv, stand_component, compartiment, B_t_ha, C_t_ha, N_t_ha) %>% filter(compartiment %in% c("ag", "bg", "total")), 
             RG_summary %>% filter(stand == "all" & SP_code == "all") %>% select(plot_ID, inv, stand_component, compartiment, B_t_ha, C_t_ha, N_t_ha) %>% filter(compartiment %in% c("ag", "bg", "total")),
             DW_summary %>% filter(decay == "all" & dw_type == "all" & dw_sp == "all") %>% select(plot_ID, inv, stand_component, compartiment, B_t_ha, C_t_ha, N_t_ha) %>% filter(compartiment %in% c("ag", "bg", "total")) 
      ) %>% 
        arrange(plot_ID)%>% 
        group_by(plot_ID, inv, compartiment) %>% 
        summarise(B_t_ha = sum(B_t_ha),
                  C_t_ha = sum(C_t_ha),
                  N_t_ha = sum(N_t_ha)) %>% 
        mutate(stand_component = "all", 
               stand = "all", 
               SP_code = "all"))
    )%>%  
      left_join(., LT_stand_TY_P %>% 
                  select(-stand_component) %>% 
                  mutate_at(c('inv', 'plot_ID'), as.character),
                by = c("plot_ID", "inv")),
    # add standtype wise dataset
    LT_summary %>% filter(plot_ID == "all")) %>%
  arrange(plot_ID)



# 4. data export ----------------------------------------------------------
write.csv(LT_summary, paste0(out.path.BZE3, paste(LT_summary$inv[1], "LT_stocks_ha_all_groups", sep = "_"), ".csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(RG_summary, paste0(out.path.BZE3, paste(RG_summary$inv[1], "RG_stocks_ha_all_groups", sep = "_"), ".csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(DW_summary, paste0(out.path.BZE3, paste(DW_summary$inv[1], "DW_stocks_ha_all_groups", sep = "_"), ".csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(LT_RG_DW_P, paste0(out.path.BZE3, paste(LT_RG_DW_P$inv[1], "LT_RG_DW_stocks_ha_all_groups", sep = "_"), ".csv"), row.names = FALSE, fileEncoding = "UTF-8")






stop("there the visualization of 05_00_RG_LT_DW_summarizing_hevtar_values BZE3 starts")



view(LT_RG_DW_P %>%
  filter(plot_ID == 140037 | plot_ID == "all") %>% 
 select(stand_component, plot_ID, inv, inv_year, stand,  SP_code, dw_sp,	dw_type,	decay,	ST_LY_type, compartiment) %>% 
  distinct())

# 5. visuals --------------------------------------------------------------

# 5.1. biomass compartiments ----------------------------------------------
# pie chart of the percentage of biomass a compartiment averagely accumulates 
# per single tree 
# per plot 

# 5.1.1. average hectar Biomass per compartiment by species  ------------------------
LT_B_percent_SP <- LT_summary %>% 
  filter(SP_code != "all" & 
           plot_ID != "all") %>% 
  select(stand_component, plot_ID, inv_year, SP_code, compartiment, B_t_ha) %>% 
  group_by(stand_component, inv_year, SP_code, compartiment) %>% 
  summarise(B_t_ha = mean(B_t_ha)) %>% 
  pivot_wider(names_from = compartiment, values_from = B_t_ha) %>% 
  # calcualte the percentage each compartiment contributes to the total bioass of a tree of 
  # the respective species at the respective plot
  # https://stackoverflow.com/questions/47821241/how-to-divide-a-number-of-columns-by-one-column
  mutate(across(c(ag:total),.fns = ~./total*100)) %>% 
  pivot_longer(., ag:total, 
               names_to = "compartiment", 
               values_to = "B_t_ha_percent")

# prepare list for ag compartiment plots to arrage them in grid
p_ag_comp <- list()
# prepare list for ag compartiment plots to arrage them in grid
p_total <- list()
for (i in 1:length(unique(LT_B_percent_SP$SP_code))) {
  # i = 1
  my.sp <- unique(LT_B_percent_SP$SP_code)[i]
  
  # plot all aboveground compartiments in relation to the total biomass
  p.ag.comp <- LT_B_percent_SP %>% 
    filter(!(compartiment %in% c("total", "ag")) & SP_code == my.sp) %>% 
    mutate(csum = rev(cumsum(rev(B_t_ha_percent))), 
           pos = B_t_ha_percent/2 + lead(csum, 1),
           pos = if_else(is.na(pos), B_t_ha_percent/2, pos)) %>% 
    ggplot(., aes(x="", y=B_t_ha_percent, fill = compartiment)) +
    geom_col(width = 1, color = 1) +
    # ggtitle(paste("average Biomass in kg per singe tree per compartiment of species", my.sp," across all plots"))+
    ggtitle(paste(my.sp))+
    coord_polar("y", start=0)+
    geom_label_repel(aes(y = pos, 
                         label = paste0(compartiment,": ", as.integer(B_t_ha_percent), "%")),
                     size = 4.5, nudge_x = 1, show.legend = F) +
    guides(fill = "none")+ 
    # guides(fill = guide_legend(title = "compartiment")) +
    theme_void()
  print(p.ag.comp)
  
  # print ag and bg compartiment in relation to 
  p.total.comp <- LT_B_percent_SP %>% 
    filter(compartiment %in% c("ag", "bg") & SP_code == my.sp) %>% 
    mutate(csum = rev(cumsum(rev(B_t_ha_percent))), 
           pos = B_t_ha_percent/2 + lead(csum, 1),
           pos = if_else(is.na(pos), B_t_ha_percent/2, pos)) %>% 
    ggplot(., aes(x="", y=B_t_ha_percent, fill = compartiment)) +
    geom_col(width = 1, color = 1) +
    # ggtitle(paste("average Biomass in kg per singe tree per compartiment of species", my.sp," across all plots"))+
    ggtitle(paste(my.sp))+
    coord_polar("y", start=0)+
    geom_label_repel(aes(y = pos, 
                         label = paste0(compartiment,": ", 
                                        as.integer(B_t_ha_percent), "%")),
                     size = 4.5, nudge_x = 1, show.legend = F) +
    # remove legend for  https://statisticsglobe.com/remove-legend-ggplot2-r
    guides(fill = "none")+ 
    # guides(fill = guide_legend(title = "compartiment")) +
    theme_void() 
  print(p.total.comp)
  
  # save the ag compartiment plots in list
  p_ag_comp[[i]] <- p.ag.comp
  
  # save the total compartiment plots in list
  p_total[[i]] <- p.total.comp
  
  
}
# https://stackoverflow.com/questions/9315611/grid-of-multiple-ggplot2-plots-which-have-been-made-in-a-for-loop
do.call(grid.arrange, p_ag_comp)
do.call(grid.arrange, p_total)





# 5.1. biomass compartiments ----------------------------------------------
# pie chart of the percentage of biomass a compartiment averagely accumulates 
# per single tree 
# per plot 

# 5.1.1. average hectar Biomass per compartiment by species  ------------------------
LT_N_percent_SP <- LT_summary %>% 
  filter(SP_code != "all" & 
           plot_ID != "all") %>% 
  select(stand_component, plot_ID, inv_year, SP_code, compartiment, N_t_ha) %>% 
  group_by(stand_component, inv_year, SP_code, compartiment) %>% 
  summarise(N_t_ha = mean(N_t_ha)) %>% 
  pivot_wider(names_from = compartiment, values_from = N_t_ha) %>% 
  # calcualte the percentage each compartiment contributes to the total bioass of a tree of 
  # the respective species at the respective plot
  # https://stackoverflow.com/questions/47821241/how-to-divide-a-number-of-columns-by-one-column
  mutate(across(c(ag:total),.fns = ~./total*100)) %>% 
  pivot_longer(., ag:total, 
               names_to = "compartiment", 
               values_to = "N_t_ha_percent")

# prepare list for ag compartiment plots to arrage them in grid
p_ag_comp <- list()
# prepare list for ag compartiment plots to arrage them in grid
p_total <- list()
for (i in 1:length(unique(LT_N_percent_SP$SP_code))) {
  # i = 1
  my.sp <- unique(LT_N_percent_SP$SP_code)[i]
  
  # plot all aboveground compartiments in relation to the total biomass
  p.ag.comp <- LT_N_percent_SP %>% 
    filter(!(compartiment %in% c("total", "ag", "bg")) & SP_code == my.sp) %>% 
    mutate(csum = rev(cumsum(rev(N_t_ha_percent))), 
           pos = N_t_ha_percent/2 + lead(csum, 1),
           pos = if_else(is.na(pos), N_t_ha_percent/2, pos)) %>% 
    ggplot(., aes(x="", y=N_t_ha_percent, fill = compartiment)) +
    geom_col(width = 1, color = 1) +
    # ggtitle(paste("average Biomass in kg per singe tree per compartiment of species", my.sp," across all plots"))+
    ggtitle(paste(my.sp))+
    coord_polar("y", start=0)+
    geom_label_repel(aes(y = pos, 
                         label = paste0(compartiment,": ", as.integer(N_t_ha_percent), "%")),
                     size = 4.5, nudge_x = 1, show.legend = F) +
    guides(fill = "none")+ 
    # guides(fill = guide_legend(title = "compartiment")) +
    theme_void()
  print(p.ag.comp)
  
  # print ag and bg compartiment in relation to 
  p.total.comp <- LT_N_percent_SP %>% 
    filter(compartiment %in% c("ag", "bg") & SP_code == my.sp) %>% 
    mutate(csum = rev(cumsum(rev(N_t_ha_percent))), 
           pos = N_t_ha_percent/2 + lead(csum, 1),
           pos = if_else(is.na(pos), N_t_ha_percent/2, pos)) %>% 
    ggplot(., aes(x="", y=N_t_ha_percent, fill = compartiment)) +
    geom_col(width = 1, color = 1) +
    # ggtitle(paste("average Biomass in kg per singe tree per compartiment of species", my.sp," across all plots"))+
    ggtitle(paste(my.sp))+
    coord_polar("y", start=0)+
    geom_label_repel(aes(y = pos, 
                         label = paste0(compartiment,": ", 
                                        as.integer(N_t_ha_percent), "%")),
                     size = 4.5, nudge_x = 1, show.legend = F) +
    # remove legend for  https://statisticsglobe.com/remove-legend-ggplot2-r
    guides(fill = "none")+ 
    # guides(fill = guide_legend(title = "compartiment")) +
    theme_void() 
  print(p.total.comp)
  
  # save the ag compartiment plots in list
  p_ag_comp[[i]] <- p.ag.comp
  
  # save the total compartiment plots in list
  p_total[[i]] <- p.total.comp
  
  
}
# https://stackoverflow.com/questions/9315611/grid-of-multiple-ggplot2-plots-which-have-been-made-in-a-for-loop
do.call(grid.arrange, p_ag_comp)
do.call(grid.arrange, p_total)








# N and B together in compartiments per species group ---------------------



# prepare list for ag compartiment plots to arrage them in grid
p_ag_comp <- list()
# prepare list for ag compartiment plots to arrage them in grid
p_total <- list()
# prepare list for ag compartiment plots to arrage them in grid
N_p_ag_comp <- list()
# prepare list for ag compartiment plots to arrage them in grid
N_p_total <- list()
for (i in 1:length(unique(LT_B_percent_SP$SP_code))) {
  # i = 1
  my.sp <- unique(LT_B_percent_SP$SP_code)[i]
  
  
  ## biomass
  # plot all aboveground compartiments in relation to the total biomass
  p.ag.comp <- LT_B_percent_SP %>% 
    filter(!(compartiment %in% c("total", "ag")) & SP_code == my.sp) %>% 
    mutate(csum = rev(cumsum(rev(B_t_ha_percent))), 
           pos = B_t_ha_percent/2 + lead(csum, 1),
           pos = if_else(is.na(pos), B_t_ha_percent/2, pos)) %>% 
    ggplot(., aes(x="", y=B_t_ha_percent, fill = compartiment)) +
    geom_col(width = 1, color = 1) +
    # ggtitle(paste("average Biomass in kg per singe tree per compartiment of species", my.sp," across all plots"))+
    ggtitle(paste(my.sp))+
    coord_polar("y", start=0)+
    geom_label_repel(aes(y = pos, 
                         label = paste0(compartiment,": ", as.integer(B_t_ha_percent), "%")),
                     size = 4.5, nudge_x = 1, show.legend = F) +
    guides(fill = "none")+ 
    # guides(fill = guide_legend(title = "compartiment")) +
    theme_void()
  #print(p.ag.comp)
  
  # print ag and bg compartiment in relation to 
  p.total.comp <- LT_B_percent_SP %>% 
    filter(compartiment %in% c("ag", "bg") & SP_code == my.sp) %>% 
    mutate(csum = rev(cumsum(rev(B_t_ha_percent))), 
           pos = B_t_ha_percent/2 + lead(csum, 1),
           pos = if_else(is.na(pos), B_t_ha_percent/2, pos)) %>% 
    ggplot(., aes(x="", y=B_t_ha_percent, fill = compartiment)) +
    geom_col(width = 1, color = 1) +
    # ggtitle(paste("average Biomass in kg per singe tree per compartiment of species", my.sp," across all plots"))+
    ggtitle(paste("biomass", my.sp))+
    coord_polar("y", start=0)+
    geom_label_repel(aes(y = pos, 
                         label = paste0(compartiment,": ", 
                                        as.integer(B_t_ha_percent), "%")),
                     size = 4.5, nudge_x = 1, show.legend = F) +
    # remove legend for  https://statisticsglobe.com/remove-legend-ggplot2-r
    guides(fill = "none")+ 
    # guides(fill = guide_legend(title = "compartiment")) +
    theme_void() 
  #print(p.total.comp)
  
  
  
  ## nitrogen 
  
  # plot all aboveground compartiments in relation to the total biomass
  N.p.ag.comp <- LT_N_percent_SP %>% 
    filter(!(compartiment %in% c("total", "ag")) & SP_code == my.sp) %>% 
    mutate(csum = rev(cumsum(rev(N_t_ha_percent))), 
           pos = N_t_ha_percent/2 + lead(csum, 1),
           pos = if_else(is.na(pos), N_t_ha_percent/2, pos)) %>% 
    ggplot(., aes(x="", y=N_t_ha_percent, fill = compartiment)) +
    geom_col(width = 1, color = 1) +
    # ggtitle(paste("average Biomass in kg per singe tree per compartiment of species", my.sp," across all plots"))+
    ggtitle(paste("nitrogen", my.sp))+
    coord_polar("y", start=0)+
    geom_label_repel(aes(y = pos, 
                         label = paste0(compartiment,": ", as.integer(N_t_ha_percent), "%")),
                     size = 4.5, nudge_x = 1, show.legend = F) +
    guides(fill = "none")+ 
    # guides(fill = guide_legend(title = "compartiment")) +
    theme_void()
  #print(N.p.ag.comp)
  
  # print ag and bg compartiment in relation to 
  N.p.total.comp <- LT_N_percent_SP %>% 
    filter(compartiment %in% c("ag", "bg") & SP_code == my.sp) %>% 
    mutate(csum = rev(cumsum(rev(N_t_ha_percent))), 
           pos = N_t_ha_percent/2 + lead(csum, 1),
           pos = if_else(is.na(pos), N_t_ha_percent/2, pos)) %>% 
    ggplot(., aes(x="", y=N_t_ha_percent, fill = compartiment)) +
    geom_col(width = 1, color = 1) +
    # ggtitle(paste("average Biomass in kg per singe tree per compartiment of species", my.sp," across all plots"))+
    ggtitle(paste(my.sp))+
    coord_polar("y", start=0)+
    geom_label_repel(aes(y = pos, 
                         label = paste0(compartiment,": ", 
                                        as.integer(N_t_ha_percent), "%")),
                     size = 4.5, nudge_x = 1, show.legend = F) +
    # remove legend for  https://statisticsglobe.com/remove-legend-ggplot2-r
    guides(fill = "none")+ 
    # guides(fill = guide_legend(title = "compartiment")) +
    theme_void() 
  #print(N.p.total.comp)
  
  
  # print N and B together in grod arrange
  print(grid.arrange (p.ag.comp, N.p.ag.comp,  ncol = 2, nrow  = 1))
  
  # save the ag compartiment plots in list
  p_ag_comp[[i]] <- p.ag.comp
  N_p_ag_comp[[i]] <- N.p.ag.comp
  
  # save the total compartiment plots in list
  p_total[[i]] <- p.total.comp
  N_p_total[[i]] <- N.p.total.comp
  
}




# single tree compartiment visuals ----------------------------------------
LT_B_percent_SP <- trees_data  %>% 
  #mutate(B_t_tree = ton(B_kg_tree)) %>% 
  group_by(inv_year, SP_code, compartiment) %>% 
  summarise(B_kg_tree = mean(B_kg_tree)) %>% 
  #filter(compartiment %in% c("ag", "bg")) %>% 
  pivot_wider(names_from = compartiment, values_from = B_kg_tree) %>%
  arrange(SP_code) %>%
  # calcualte the percentage each compartiment contributes to the total bioass of a tree of 
  # the respective species at the respective plot
  # https://stackoverflow.com/questions/47821241/how-to-divide-a-number-of-columns-by-one-column
  mutate(across(c(ag:total),.fns = ~./total*100)) %>% 
  pivot_longer(., ag:total, 
               names_to = "compartiment", 
               values_to = "B_kg_tree_percent")


LT_N_percent_SP <- trees_data  %>% 
  #mutate(B_t_tree = ton(B_kg_tree)) %>% 
  group_by(inv_year, SP_code, compartiment) %>% 
  summarise(N_kg_tree = mean(N_kg_tree)) %>% 
  #filter(compartiment %in% c("ag", "bg")) %>% 
  pivot_wider(names_from = compartiment, values_from = N_kg_tree) %>%
  arrange(SP_code) %>%
  # calcualte the percentage each compartiment contributes to the total bioass of a tree of 
  # the respective species at the respective plot
  # https://stackoverflow.com/questions/47821241/how-to-divide-a-number-of-columns-by-one-column
  mutate(across(c(ag:total),.fns = ~./total*100)) %>% 
  pivot_longer(., ag:total, 
               names_to = "compartiment", 
               values_to = "N_kg_tree_percent")


# prepare list for ag compartiment plots to arrage them in grid
p_ag_comp <- list()
# prepare list for ag compartiment plots to arrage them in grid
p_total <- list()
# prepare list for ag compartiment plots to arrage them in grid
N_p_ag_comp <- list()
# prepare list for ag compartiment plots to arrage them in grid
N_p_total <- list()
for (i in 1:length(unique(LT_B_percent_SP$SP_code))) {
  # wie viel n 
  # wie viel n in durchmesserklasse
  # average biomass over all trees in this species group over all plots
  # percent were calcualted in regard to total biomass of the repsective stem (ag + bg compartiment) 
  
  # i = 1
  my.sp <- unique(LT_B_percent_SP$SP_code)[i]
  
  
  ## biomass
  # plot all aboveground compartiments in relation to the total biomass
  p.ag.comp <- LT_B_percent_SP %>% 
    filter(!(compartiment %in% c("total", "ag")) & SP_code == my.sp) %>% 
    mutate(csum = rev(cumsum(rev(B_kg_tree_percent))), 
           pos = B_kg_tree_percent/2 + lead(csum, 1),
           pos = if_else(is.na(pos), B_kg_tree_percent/2, pos)) %>% 
    ggplot(., aes(x="", y=B_kg_tree_percent, fill = compartiment)) +
    geom_col(width = 1, color = 1) +
    # ggtitle(paste("average Biomass in kg per singe tree per compartiment of species", my.sp," across all plots"))+
    ggtitle(paste(my.sp))+
    coord_polar("y", start=0)+
    geom_label_repel(aes(y = pos, 
                         label = paste0(compartiment,": ", as.integer(B_kg_tree_percent), "%")),
                     size = 4.5, nudge_x = 1, show.legend = F) +
    guides(fill = "none")+ 
    # guides(fill = guide_legend(title = "compartiment")) +
    theme_void()
  #print(p.ag.comp)
  
  # print ag and bg compartiment in relation to 
  p.total.comp <- LT_B_percent_SP %>% 
    filter(compartiment %in% c("ag", "bg") & SP_code == my.sp) %>% 
    mutate(csum = rev(cumsum(rev(B_kg_tree_percent))), 
           pos = B_kg_tree_percent/2 + lead(csum, 1),
           pos = if_else(is.na(pos), B_kg_tree_percent/2, pos)) %>% 
    ggplot(., aes(x="", y=B_kg_tree_percent, fill = compartiment)) +
    geom_col(width = 1, color = 1) +
    # ggtitle(paste("average Biomass in kg per singe tree per compartiment of species", my.sp," across all plots"))+
    ggtitle(paste("biomass", my.sp))+
    coord_polar("y", start=0)+
    geom_label_repel(aes(y = pos, 
                         label = paste0(compartiment,": ", 
                                        as.integer(B_kg_tree_percent), "%")),
                     size = 4.5, nudge_x = 1, show.legend = F) +
    # remove legend for  https://statisticsglobe.com/remove-legend-ggplot2-r
    guides(fill = "none")+ 
    # guides(fill = guide_legend(title = "compartiment")) +
    theme_void() 
  #print(p.total.comp)
  
  
  
  ## nitrogen 
  
  # plot all aboveground compartiments in relation to the total biomass
  N.p.ag.comp <- LT_N_percent_SP %>% 
    filter(!(compartiment %in% c("total", "ag")) & SP_code == my.sp) %>% 
    mutate(csum = rev(cumsum(rev(N_kg_tree_percent))), 
           pos = N_kg_tree_percent/2 + lead(csum, 1),
           pos = if_else(is.na(pos), N_kg_tree_percent/2, pos)) %>% 
    ggplot(., aes(x="", y=N_kg_tree_percent, fill = compartiment)) +
    geom_col(width = 1, color = 1) +
    # ggtitle(paste("average Biomass in kg per singe tree per compartiment of species", my.sp," across all plots"))+
    ggtitle(paste("nitrogen", my.sp))+
    coord_polar("y", start=0)+
    geom_label_repel(aes(y = pos, 
                         label = paste0(compartiment,": ", as.integer(N_kg_tree_percent), "%")),
                     size = 4.5, nudge_x = 1, show.legend = F) +
    guides(fill = "none")+ 
    # guides(fill = guide_legend(title = "compartiment")) +
    theme_void()
  #print(N.p.ag.comp)
  
  # print ag and bg compartiment in relation to 
  N.p.total.comp <- LT_N_percent_SP %>% 
    filter(compartiment %in% c("ag", "bg") & SP_code == my.sp) %>% 
    mutate(csum = rev(cumsum(rev(N_kg_tree_percent))), 
           pos = N_kg_tree_percent/2 + lead(csum, 1),
           pos = if_else(is.na(pos), N_kg_tree_percent/2, pos)) %>% 
    ggplot(., aes(x="", y=N_kg_tree_percent, fill = compartiment)) +
    geom_col(width = 1, color = 1) +
    # ggtitle(paste("average Biomass in kg per singe tree per compartiment of species", my.sp," across all plots"))+
    ggtitle(paste(my.sp))+
    coord_polar("y", start=0)+
    geom_label_repel(aes(y = pos, 
                         label = paste0(compartiment,": ", 
                                        as.integer(N_kg_tree_percent), "%")),
                     size = 4.5, nudge_x = 1, show.legend = F) +
    # remove legend for  https://statisticsglobe.com/remove-legend-ggplot2-r
    guides(fill = "none")+ 
    # guides(fill = guide_legend(title = "compartiment")) +
    theme_void() 
  #print(N.p.total.comp)
  
  
  # print N and B together in grod arrange
  print(grid.arrange (p.ag.comp, N.p.ag.comp,  ncol = 2, nrow  = 1))
  
  # save the ag compartiment plots in list
  p_ag_comp[[i]] <- p.ag.comp
  N_p_ag_comp[[i]] <- N.p.ag.comp
  
  # save the total compartiment plots in list
  p_total[[i]] <- p.total.comp
  N_p_total[[i]] <- N.p.total.comp
  
}








