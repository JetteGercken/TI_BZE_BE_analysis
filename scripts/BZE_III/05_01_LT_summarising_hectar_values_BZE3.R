# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the  national soil inventory
# BZE3 
# summarising stock per hectar summarising for living trees (LT) and stand types (TY)


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


# CALCULATIONS ------------------------------------------------------------

# 1. LIVING TREES -----------------------------------------------

# 1.2. number of speices per plot -----------------------------------------
LT_n_SP_plot <- trees_data %>%
  select(plot_ID, inv, SP_code) %>% 
  group_by(plot_ID, inv) %>% 
  distinct() %>% 
  summarise(n_SP = n()) %>% 
  mutate(stand_component = "LT")


# 1.3. number of stand per plot -------------------------------------------
n_stand_P <- plyr::rbind.fill(trees_data %>% 
                                filter(compartiment == "ag"),
                              # we have to include RG data too, since there is the possibility, that there are plots that dont have LT but RG and the RG habe stands
                              RG_data %>%
                                filter(compartiment == "ag" & !is.na(stand)) ) %>% 
  select(plot_ID, inv, stand) %>% 
  distinct() %>% 
  group_by(plot_ID, inv) %>% 
  summarise(n_stands = n()) %>% 
  mutate(stand_component = "all")

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
                                   # add status 2 plots if all circles are existing but empty 
                                   trees_stat_2 %>% 
                                     # this is in case in 01_00_RG_LT_DW_plot_inv_status_sorting there were stat_2 datasets produced that do not hold any data but only NAs
                                     filter(!is.na(plot_ID)) %>% 
                                     semi_join(., 
                                               trees_stat_2 %>% 
                                                 select(plot_ID, CCS_r_m) %>% 
                                                 distinct() %>% 
                                                 group_by(plot_ID) %>% 
                                                 summarize(n_CCS = n()) %>% 
                                                 filter(n_CCS == 3), 
                                               by = "plot_ID")) %>%  
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
}else{
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

# 1.4.2. plot, stand: stocks per plot per stand hektar ------------------------------------------------------
if(exists('trees_stat_2') == TRUE && nrow(trees_stat_2)!= 0){
  LT_ST_BCNBAn_ha <- plyr::rbind.fill(trees_data  %>% 
                                     group_by(plot_ID, stand_plot_A_ha, CCS_r_m, stand, inv, compartiment) %>% 
                                     # convert Biomass into tons per hectar and sum it up per sampling circuit 
                                     reframe(B_CCS_t_ha = sum(ton(B_kg_tree))/stand_plot_A_ha, # plot are is the area of the respecitve samplign circuit in ha 
                                             C_CCS_t_ha = sum(ton(C_kg_tree))/stand_plot_A_ha,
                                             N_CCS_t_ha = sum(ton(N_kg_tree))/stand_plot_A_ha, 
                                             BA_CCS_m2_ha = sum(BA_m2)/stand_plot_A_ha, 
                                             n_trees_CCS_ha = n()/stand_plot_A_ha) %>% 
                                     distinct(),
                                   # add status 2 plots if all circles are existing but empty 
                                   trees_stat_2 %>% 
                                     # this is in case in 01_00_RG_LT_DW_plot_inv_status_sorting there were stat_2 datasets produced that do not hold any data but only NAs
                                     filter(!is.na(plot_ID)) %>% 
                                     semi_join(., 
                                               trees_stat_2 %>% 
                                                 select(plot_ID, CCS_r_m) %>% 
                                                 distinct() %>% 
                                                 group_by(plot_ID) %>% 
                                                 summarize(n_CCS = n()) %>% 
                                                 filter(n_CCS == 3), 
                                               by = "plot_ID")) %>%  
    # now we summarise all the t/ha values of the cirlces per plot
    group_by(plot_ID, inv, stand, compartiment) %>% 
    summarise(B_t_ha = sum(B_CCS_t_ha), 
              C_t_ha = sum(C_CCS_t_ha), 
              N_t_ha = sum(N_CCS_t_ha), 
              BA_m2_ha = sum(BA_CCS_m2_ha), 
              n_ha = sum(n_trees_CCS_ha)) %>% 
    mutate(stand_component = "LT",
           SP_code = "all")
}else{
  LT_ST_BCNBAn_ha <- trees_data  %>% 
    group_by(plot_ID, stand_plot_A_ha, CCS_r_m, stand, inv, compartiment) %>% 
    # convert Biomass into tons per hectar and sum it up per sampling circuit 
    reframe(B_CCS_t_ha = sum(ton(B_kg_tree))/stand_plot_A_ha, # plot are is the area of the respecitve samplign circuit in ha 
            C_CCS_t_ha = sum(ton(C_kg_tree))/stand_plot_A_ha,
            N_CCS_t_ha = sum(ton(N_kg_tree))/stand_plot_A_ha, 
            BA_CCS_m2_ha = sum(BA_m2)/stand_plot_A_ha, 
            n_trees_CCS_ha = n()/stand_plot_A_ha) %>% 
    distinct() %>%  
    # now we summarise all the t/ha values of the cirlces per plot
    group_by(plot_ID, inv, stand, compartiment) %>% 
    summarise(B_t_ha = sum(B_CCS_t_ha), 
              C_t_ha = sum(C_CCS_t_ha), 
              N_t_ha = sum(N_CCS_t_ha),
              BA_m2_ha = sum(BA_CCS_m2_ha), 
              n_ha = sum(n_trees_CCS_ha)) %>% 
    mutate(stand_component = "LT", 
           SP_code = "all")
}

# 1.4.3. plot, species, stand: stocks per ha, finest summary --------------
# there is an issue here. we have to think of summarising the hectar values 
# at the moment we calcualte the value per CCR and then sum them up. 
# what we have to consider when we do that is that empty, yet inventorisable cirlces with status 2
# still contribute to the oveall plot area. unlike CCRs that are status 3 and by that reduce the actual plot area by this circle
# our current methodology does actually not require the empty status 2 circles, as we never relate the 
#  stock to the total area of all cirlces, so the step is sort of useless, as we only add 0 to the total stock or whatever

# however this is whats been causing the plots that have a species summary but also summaries for NA
# solutions: 
# a solution could be to just filter for CCSr that have all CCRs set to status 2 so the empty plots are summarized with the others but the empty plots are not
# i wonder, however, whats the effect of just ignoring circles with status 2... shouldn´t we somehow include them , 
# like in the RG calculation where we first caclulate the whole plot area and then relate the whole stock per strata to the whole area


if(exists('trees_stat_2') == TRUE && nrow(trees_stat_2)!= 0){
  LT_SP_ST_P_BCNBAn_ha <- plyr::rbind.fill(
    trees_data  %>% 
      group_by(plot_ID, stand_plot_A_ha, CCS_r_m, inv, stand, SP_code, compartiment) %>% 
      # convert Biomass into tons per hectar and sum it up per sampling circuit 
      reframe(B_CCS_t_ha = sum(ton(B_kg_tree))/stand_plot_A_ha, # plot are is the area of the respecitve samplign circuit in ha 
              C_CCS_t_ha = sum(ton(C_kg_tree))/stand_plot_A_ha,
              N_CCS_t_ha = sum(ton(N_kg_tree))/stand_plot_A_ha, 
              BA_CCS_m2_ha = sum(BA_m2)/stand_plot_A_ha, 
              n_trees_CCS_ha = n()/stand_plot_A_ha) %>% 
      distinct(), 
    trees_stat_2 %>% 
      # this is in case in 01_00_RG_LT_DW_plot_inv_status_sorting there were stat_2 datasets produced that do not hold any data but only NAs
      filter(!is.na(plot_ID)) %>% 
      # this filters only those plotIDs of plots that have 3empty but inventorable CCS (stat 2 CCS)
      semi_join(., 
                trees_stat_2 %>% 
                  select(plot_ID, CCS_r_m) %>% 
                  distinct() %>% 
                  group_by(plot_ID) %>% 
                  summarize(n_CCS = n()) %>% 
                  filter(n_CCS == 3), 
                by = "plot_ID")
    ) %>% 
    # now we summarise all the t/ha values of the cirlces per plot
    group_by(plot_ID, inv, stand, SP_code, compartiment) %>% 
    summarise(B_t_ha = sum(B_CCS_t_ha), 
              C_t_ha = sum(C_CCS_t_ha), 
              N_t_ha = sum(N_CCS_t_ha), 
              BA_m2_ha = sum(BA_CCS_m2_ha), 
              n_ha = sum(n_trees_CCS_ha)) %>% 
    mutate(stand_component = "LT") %>% 
    #calcualte species compostiion by calcualting the percent of the respective species contributes to the overall basal area 
    left_join(LT_ST_BCNBAn_ha %>% 
                select(plot_ID, inv, stand, compartiment, BA_m2_ha) %>% 
                rename(BA_m2_ha_total = BA_m2_ha),
              by = c("plot_ID", "inv", "stand", "compartiment"), ) %>% 
    distinct() %>% 
    mutate(BA_percent = (BA_m2_ha/BA_m2_ha_total)*100) %>% 
    select(-"BA_m2_ha_total")
}else{
  LT_SP_ST_P_BCNBAn_ha <- trees_data  %>% 
    group_by(plot_ID, stand_plot_A_ha, CCS_r_m, inv, stand, SP_code, compartiment) %>% 
    # convert Biomass into tons per hectar and sum it up per sampling circuit 
    reframe(B_CCS_t_ha = sum(ton(B_kg_tree))/stand_plot_A_ha, # plot are is the area of the respecitve samplign circuit in ha 
            C_CCS_t_ha = sum(ton(C_kg_tree))/stand_plot_A_ha,
            N_CCS_t_ha = sum(ton(N_kg_tree))/stand_plot_A_ha, 
            BA_CCS_m2_ha = sum(BA_m2)/stand_plot_A_ha, 
            n_trees_CCS_ha = n()/stand_plot_A_ha) %>% 
    distinct()%>% 
    # now we summarise all the t/ha values of the cirlces per plot
    group_by(plot_ID, inv, stand, SP_code, compartiment) %>% 
    summarise(B_t_ha = sum(B_CCS_t_ha), 
              C_t_ha = sum(C_CCS_t_ha), 
              N_t_ha = sum(N_CCS_t_ha),
              BA_m2_ha = sum(BA_CCS_m2_ha)) %>% 
    mutate(stand_component = "LT") %>% 
    #calcualte species compostiion by calcualting the percent of the respective species contributes to the overall basal area 
    left_join(LT_ST_BCNBAn_ha %>% 
                select(plot_ID, inv, stand, compartiment, BA_m2_ha) %>% 
                rename(BA_m2_ha_total = BA_m2_ha),
              by = c("plot_ID", "inv", "stand", "compartiment"), ) %>% 
    distinct() %>% 
    mutate(BA_percent = (BA_m2_ha/BA_m2_ha_total)*100) %>% 
    select(-"BA_m2_ha_total")
}



# 1.4.4. Plot, species: stocks per hektar ------------------------------------------------------
if(exists('trees_stat_2') == TRUE && nrow(trees_stat_2)!= 0){
  LT_SP_BCNBA_ha <- plyr::rbind.fill(
    trees_data  %>% 
      group_by(plot_ID, plot_A_ha, CCS_r_m, inv, SP_code, compartiment) %>% 
      # convert Biomass into tons per hectar and sum it up per sampling circuit 
      reframe(B_CCS_t_ha = sum(ton(B_kg_tree))/plot_A_ha, # plot are is the area of the respecitve samplign circuit in ha 
              C_CCS_t_ha = sum(ton(C_kg_tree))/plot_A_ha,
              N_CCS_t_ha = sum(ton(N_kg_tree))/plot_A_ha, 
              BA_CCS_m2_ha = sum(BA_m2)/plot_A_ha) %>% 
      distinct(), 
    trees_stat_2 %>% 
      # this is in case in 01_00_RG_LT_DW_plot_inv_status_sorting there were stat_2 datasets produced that do not hold any data but only NAs
      filter(!is.na(plot_ID)) %>% 
      # this filters only those plotIDs of plots that have 3empty but inventorable CCS (stat 2 CCS)
      semi_join(., 
                trees_stat_2 %>% 
                  select(plot_ID, CCS_r_m) %>% 
                  distinct() %>% 
                  group_by(plot_ID) %>% 
                  summarize(n_CCS = n()) %>% 
                  filter(n_CCS == 3), 
                by = "plot_ID")
  ) %>% 
    # now we summarise all the t/ha values of the cirlces per plot
    group_by(plot_ID, inv, SP_code, compartiment) %>% 
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
    mutate(BA_percent = (BA_m2_ha/BA_m2_ha_total)*100, 
           stand = "all") %>% 
    select(-"BA_m2_ha_total")
  
}else{
### if there is no stat_2_LT dataset we do the same just without the stat_2 empty plots: 
  LT_SP_BCNBA_ha <- trees_data  %>% 
    group_by(plot_ID, plot_A_ha, CCS_r_m, inv, SP_code, compartiment) %>% 
    # convert Biomass into tons per hectar and sum it up per sampling circuit 
    reframe(B_CCS_t_ha = sum(ton(B_kg_tree))/plot_A_ha, # plot are is the area of the respecitve samplign circuit in ha 
            C_CCS_t_ha = sum(ton(C_kg_tree))/plot_A_ha,
            N_CCS_t_ha = sum(ton(N_kg_tree))/plot_A_ha, 
            BA_CCS_m2_ha = sum(BA_m2)/plot_A_ha) %>% 
    distinct()%>% 
    # now we summarise all the t/ha values of the cirlces per plot
    group_by(plot_ID, inv, SP_code, compartiment) %>% 
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
    mutate(BA_percent = (BA_m2_ha/BA_m2_ha_total)*100, 
           stand = "all") %>% 
    select(-"BA_m2_ha_total")
}


# 1.5. plot, main stand: stand type ------------------------------------------------------
# 1.5.1. calcualte species composition and assing stand type ------------------------------------------------------
# requires the species plot wise summary
besttype_list <- vector("list", length = length(unique(trees_data$plot_ID)))
for (i in 1:length(unique(trees_data$plot_ID))) {
  # i = 7
  my.plot.id <- unique(trees_data$plot_ID)[i]
  my.inv <- unique(trees_data$inv[trees_data$plot_ID == my.plot.id])
  my.n.stand <- n_stand_P$n_stands[n_stand_P$plot_ID == my.plot.id]
  
  my.sp.p.df <- trees_data %>% 
    # only determine the stand type for the main stand
    filter(plot_ID == my.plot.id & stand == "A") %>% 
    group_by(plot_ID, CCS_r_m, inv, SP_code, compartiment) %>% 
    # calculate m2 per hectar and sum it up per sampling circuit 
    reframe(BA_CCS_m2_ha = sum(BA_m2)/stand_plot_A_ha) %>% # here we have to refer it to the stand area instead of the plot area! 
    distinct()%>% 
    # now we summarise all the m2/ha values of the cirlces per plot
    group_by(plot_ID, inv, SP_code, compartiment) %>% 
    summarise(BA_m2_ha = sum(BA_CCS_m2_ha)) %>% 
    mutate(stand_component = "LT") %>% 
    #calcualte species compostiion by calcualting the percent of the respective 
    # species contributes to the overall basal area in the stand A
    # join in total BA_m2_ha dataset to calculate relationship between species & standwise BA and standwise BA
    left_join(., 
              trees_data %>% 
                filter(plot_ID == my.plot.id & stand == "A") %>% 
                group_by(plot_ID, CCS_r_m, inv, compartiment) %>% 
                # convert Biomass into tons per hectar and sum it up per sampling circuit 
                reframe(BA_CCS_m2_ha = sum(BA_m2)/stand_plot_A_ha) %>% 
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
  
  
  
  # note to myself: i cannot use this CF BL share for the plotwise summary as it´s only for the main stand. 
  
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
LT_stand_TY_P <- as.data.frame(rbindlist(besttype_list)) %>% mutate(plot_ID = as.integer(plot_ID))



# 1.6. average values ----------------------------------------------------
# 1.6.1. plot: create "pseudo stands" -------------------------------------------
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
  
  # calcualte average values
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
LT_avg_SP_P <- as.data.frame(rbindlist(LT_avg_SP_P_list))
LT_avg_P <- as.data.frame(rbindlist(LT_avg_P_list))

# 1.6.2. stand: create "pseudo stands" -------------------------------------------
LT_avg_SP_ST_P_list <- vector("list", length = length(unique(trees_data$plot_ID))) 
for (i in 1:nrow(unique(trees_data[,c("plot_ID", "stand")])) ) {
  # i = 3
  my.plot.id <- as.numeric(unique(trees_data[,c("plot_ID", "stand")])[i ,"plot_ID"])                # plot id
  my.stand <- as.character(unique(trees_data[,c("plot_ID", "stand")])[i, "stand"])                  # one of the stands at the respective plot
  # select all trees by only one compartiment of each tree to make sure the tree enters the dataframe only once
  my.tree.df <- trees_data[trees_data$plot_ID == my.plot.id & trees_data$stand == my.stand & trees_data$compartiment == "ag", ] 
  # count trees per hectar per CCS per stand and plot
  my.n.ha.df <- trees_data %>% filter(compartiment == "ag" & plot_ID == my.plot.id & stand == my.stand) %>% group_by(plot_ID, stand, CCS_r_m) %>% reframe(n_ha_CCS = n()/stand_plot_A_ha) %>% distinct()
  # count trees per CCS per stand per plot 
  my.n.plot.df <- trees_data %>% filter(compartiment == "ag" & plot_ID == my.plot.id & stand == my.stand) %>% group_by(plot_ID, stand, CCS_r_m) %>% reframe(n_CCS = n()) %>% distinct()
  
  # calculate how often each tree has to be dublicated to resebmle a hectar
  # by dividing number of trees in that circle by number of trees per hectar in that CCS  
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
  
  
}
LT_avg_SP_ST_P <- as.data.frame(rbindlist(LT_avg_SP_ST_P_list))



# 1.7. binding LT data together ----------------------------------------
LT_summary <- plyr::rbind.fill(
# 1.7.1. LT Species stand plot data -------------------------------------------------------------------------------------------------------------
  LT_SP_ST_P_BCNBAn_ha  %>% 
    left_join(LT_avg_SP_ST_P,  
              by = c("plot_ID", "inv", "stand_component", "SP_code", "stand")) , 
# 1.7.2. LT Species plot  data -------------------------------------------------------------------------------------------------------------
  LT_SP_BCNBA_ha  %>%  
    left_join(., LT_avg_SP_P, 
              by = c("plot_ID", "inv", "stand_component", "stand", "SP_code")) ,
# 1.7.3. LT stand plot data ----------------------------------------------------
  LT_ST_BCNBAn_ha,
# 1.7.4. LT plot data ----------------------------------------------------------------------------------------------------------------
  LT_BCNBAn_ha %>% 
    # join in statistical stats by plot
    left_join(., LT_avg_P, by = c("plot_ID", "inv", "stand_component", "stand", "SP_code")) %>%
    # join in number of tree species per plot
    left_join(., LT_n_SP_plot, by = c("plot_ID", "inv", "stand_component"))
) %>% 
  # join in stand type 
  left_join(., LT_stand_TY_P %>% 
              mutate_at(c('plot_ID'), as.integer),
            by = c("plot_ID", "inv", "stand_component")) %>% 
  arrange(plot_ID, stand, SP_code, compartiment)


# to get the plotwise summarised data one has to filter for: 
# plot_ID != "all" & SP_code == "all" & stand  == "all"
# to get the species & plotwise wise summarised data one has to filter for: 
# plot_ID != "all" & SP_code != "all" & stand == "all"
# to get the stand & plotwise  wise summarised data one has to filter for: 
# plot_ID != "all" & SP_code == "all" & stand != "all"
# to get the stand , species & plotwise  wise summarised data one has to filter for: 
# plot_ID != "all" & SP_code != "all" & stand != "all"





# 2. data export ----------------------------------------------------------
write.csv(LT_summary, paste0(out.path.BZE3, paste(LT_summary$inv[1], "LT_stocks_ha_all_groups", sep = "_"), ".csv"), row.names = FALSE, fileEncoding = "UTF-8")







