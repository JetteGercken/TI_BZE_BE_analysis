# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the  national soil inventory
# cheking plausibility of the data 


# ----- 0. SETUP --------------------------------------------------------------------------------------------------------------------

# ----- 0.1. packages and functions -------------------------------------------------------------------------------------------------
source(paste0(getwd(), "/scripts/01_00_functions_library.R"))

# ----- 0.2. working directory ------------------------------------------------------------------------------------------------------
here::here()
getwd()

out.path.BZE3 <- ("output/out_data/out_data_BZE/") 


# 0.3 import data --------------------------------------------------------------------------------------------------------------
trees_data <- read.delim(file = here(paste0(out.path.BZE3,"HBI_LT_update_4.csv")), sep = ";", dec = ",")
trees_stat_2 <- read.delim(file = here("output/out_data/out_data_BZE/HBI_LT_stat_2.csv"), sep = ";", dec = ",") %>% select(-X)
HBI_summary <- read.delim(file = here(paste0(out.path.BZE3,"HBI_LT_stocks_ha_P_SP_TY.csv")), sep = ";", dec = ",")
BWI_LT_stocks <- read.delim(file = here("data/input/General/BWI_NW_LT_stocks_zielmerkmale.csv"), sep = ";", dec = ",")

# create pseudo monocultures:
# carbon stock of species per hectar divided by 1 Hektar mulitplied with the actual area covered by the species according to basal area 
# mutate(C_t_ha_mono = C_t_P_SP_ha/(1*(SP_BA_m2ha/tot_BA_m2ha)),


# 0.4. colnames, hamonising strings --------------------------------------------------------------------------------------------
colnames(BWI_LT_stocks) <- c("variable", "unit_BWI", "ei", "bu", "aLh", "aLn", "LB","fi", "ta", "dgl", "ki", "lae", "NB", "all")


# 1. data preparation ----------------------------------------------------------------------------------------------------------
# introduce the BWI Kohlentoffinventur species names/ groups listed in BWI_LT_stocks
SP_names_com_ID_tapeS <- SP_names_com_ID_tapeS %>% 
  mutate(BWI_comparisson_group = case_when(LH_NH == "LB" & bot_genus == "Quercus"~ 'ei', 
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
                                           LH_NH == "NB" & bot_genus %in% c("Pinus") ~ 'ki', 
                                           LH_NH == "NB" & bot_genus %in% c("Pseudotsuga") ~ 'dgl',
                                           LH_NH == "NB" & bot_genus %in% c("Abies") ~ 'ta',
                                           LH_NH == "NB" & bot_genus %in% c("Larix") ~ 'lae', 
                                           LH_NH == "NB" & bot_genus %in% c("Picea") |
                                             LH_NH == "NB" & !(bot_genus %in% c("Pinus", 
                                                                                "Pseudotsuga", "Abies",
                                                                                "Larix")) ~ 'fi',
                                           TRUE ~ 'other'))


# 1.2. BWI LT summary data wrangling ----------------------------------------------------------------------------------------------------------
BWI_LT_stocks <- BWI_LT_stocks %>% 
  # pivot all species names in one column
  pivot_longer(c(ei:all), names_to = "BWI_comparisson_group", values_to = "values") %>% 
  # filter for hectar values and percent 
  filter(endsWith(unit_BWI, "ha]") | endsWith(unit_BWI, "%]")) %>% 
  # remove unit
  select(- unit_BWI) %>%
  # rename variables from german to english
  mutate(variable = case_when(variable == "absorbiertes Kohlendioxid [kg/ha]" ~ "CO2_kg_ha",
                              variable == "Anteil an der HolzbodenflÃ¤che [%]" ~ "BA_percent",
                              variable == "Biomasse [kg/ha]" ~ "B_kg_ha",
                              variable == "GrundflÃ¤che [mÂ²/ha]" ~ "BA_m2_ha",
                              variable == "Kohlenstoffmasse [kg/ha]" ~ "C_kg_ha", 
                              variable == "oberirdische Biomasse [kg/ha]" ~ "ag_B_kg_ha", 
                              variable == "oberirdische Kohlenstoffmasse [kg/ha]" ~ "ag_C_kg_ha",
                              variable == "Stammzahl [1/ha]" ~ "n_ha", 
                              variable == "unterirdische Biomasse [kg/ha]" ~ "bg_B_kg_ha",
                              variable == "unterirdische Kohlenstoffmasse [kg/ha]" ~ "bg_C_kg_ha", 
                              variable == "Vorrat [mÂ³/ha]" ~ "V_m3_ha",
                              variable == "Vorrat (FAO-Definition) [mÂ³/ha]" ~ "FAO_V_m3_ha",
                              variable == "WaldflÃ¤che (gemÃ¤ÃŸ StandflÃ¤chenanteil) [ha]" ~ "SP_forest_A_ha",
                              variable == "ZugehÃ¶rige HolzbodenflÃ¤che des Auswertungsgebietes [ha]" ~ "forest_A_ha", 
                              TRUE ~ NA)) %>% 
  # pivot variables into different columns 
  pivot_wider(names_from = "variable", values_from = "values") %>% 
  ##reorder columns to pivot it lateron
  select(BWI_comparisson_group, BA_percent, BA_m2_ha, n_ha, B_kg_ha, ag_B_kg_ha, bg_B_kg_ha, 
         C_kg_ha, ag_C_kg_ha,bg_C_kg_ha)   %>% 
  # pivoting B, C: https://stackoverflow.com/questions/70700654/pivot-longer-with-names-pattern-and-pairs-of-columns
  to_long(keys = c("B_compartiment",  "C_compartiment"), 
          values = c("B_kg_ha", "C_kg_ha"),  
          names(.)[5:7], names(.)[8:10]) %>% 
  # hanging the compartiments names and deselect the other compartiment columns: https://stackoverflow.com/questions/61425318/using-mutate-and-starts-with
  mutate(B_compartiment = case_when(startsWith(B_compartiment, "bg") ~ "bg", 
                                    startsWith(B_compartiment, "ag") ~ "ag",
                                    TRUE ~ "total")) %>% 
  select(-C_compartiment) %>% 
  rename(compartiment = B_compartiment) %>% 
  mutate(stand_component = "LT")
         #,inv_year = 2017)
 


# 1.3. calculating HBI stocks in BWI groups ---------------------------------------------------------------------------------------------------------
# 1.3.1. introducing BWI species group --------------------------------------------------------------------------------------------------------------
# introduce BWI comparisson species groups to trees_data dataset so we can summariye all hectar values according to it 
trees_data <- trees_data %>% 
  left_join(., SP_names_com_ID_tapeS %>% 
              select(Chr_code_ger, BWI_comparisson_group), 
            by = "Chr_code_ger")


# 1.3.2. stocks per ha per plot ----------------------------------------------------------------------------------------------------------------
if(exists('trees_stat_2') == TRUE && nrow(trees_stat_2)!= 0){
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


# 1.3.3. stocks per ha per plot per SP_group ----------------------------------------------------------------------------------------------------------------
LT_comparisson_BZE_BWI_SP <- trees_data %>% 
  group_by(plot_ID, CCS_r_m, inv_year, BWI_comparisson_group, compartiment) %>% 
  # convert Biomass into tons per hectar and sum it up per sampling circuit 
  reframe(B_CCS_t_ha = sum(ton(B_kg_tree))/plot_A_ha, # plot are is the area of the respecitve samplign circuit in ha 
          C_CCS_t_ha = sum(ton(C_kg_tree))/plot_A_ha,
          N_CCS_t_ha = sum(ton(N_kg_tree))/plot_A_ha, 
          BA_CCS_m2_ha = sum(BA_m2)/plot_A_ha, 
          n_ha_CCS = n()) %>% 
  distinct()%>% 
  # now we summarise all the t/ha values of the cirlces per plot
  group_by(plot_ID, inv_year, BWI_comparisson_group, compartiment) %>% 
  summarise(B_t_ha = sum(B_CCS_t_ha), 
            C_t_ha = sum(C_CCS_t_ha), 
            N_t_ha = sum(N_CCS_t_ha), 
            BA_m2_ha = sum(BA_CCS_m2_ha), 
            n_ha = sum(n_ha_CCS)) %>% 
  mutate(stand_component = "LT") %>% 
  #calcualte species compostiion by calcualting the percent of the respective species contributes to the overall basal area 
  left_join(LT_BCNBAn_ha %>% 
              select(plot_ID, inv_year, compartiment, BA_m2_ha) %>% 
              rename(BA_m2_ha_total = BA_m2_ha),
            by = c("plot_ID", "inv_year", "compartiment"), ) %>% 
  distinct() %>% 
  mutate(BA_percent_BZE = (BA_m2_ha/BA_m2_ha_total)*100) %>% 
  # filter for comparitments that are represented in BWI dataset too
  filter(compartiment %in% c("ag", "bg", "total")) %>% 
# 1.3.4. create pseudo-mono-species stands --------------------------------------------------------------------------------------------------------------------------------------------------------------------
  mutate(B_t_ha_BZE = B_t_ha/(BA_percent_BZE/100), 
         C_t_ha_BZE = C_t_ha/(BA_percent_BZE/100),
         BA_m2_ha_BZE= BA_m2_ha/(BA_percent_BZE), 
         n_ha_BZE = n_ha/(BA_percent_BZE/100)) %>% 
  select(- c(BA_m2_ha_total,  B_t_ha, C_t_ha,  N_t_ha, BA_m2_ha, n_ha)) %>% 
  left_join(., BWI_LT_stocks %>% 
              mutate(across(c("B_kg_ha", "C_kg_ha"), ~ ton(.x))) %>% 
              # https://stackoverflow.com/questions/71389851/in-r-dplyr-package-a-question-about-function-rename-with
              rename_with(., ~ paste0(str_sub(c("B_kg_ha", "C_kg_ha"), end=-6), "t_ha_BWI"), c("B_kg_ha", "C_kg_ha")) %>% 
              # https://rstats101.com/add-prefix-or-suffix-to-column-names-of-dataframe-in-r/
              rename_with(.fn = function(.x){paste0(.x,"_BWI")}, .cols= c(BA_percent, BA_m2_ha, n_ha)),
            by = c("BWI_comparisson_group", "compartiment", "stand_component"))


# 1.3.5. calcualte differences between BWI and BZE ------------------------------------------------------------------------------------------------------------------------------------
# substact columns edning on BWI from columns ednign with BZE 
# https://stackoverflow.com/questions/47478125/create-new-columns-by-substracting-column-pairs-from-each-other-in-r
BZE_vars <- sort(grep("_BZE", colnames(LT_comparisson_BZE_BWI_SP), value=TRUE) )
BWI_vars <- sort(grep("_BWI", colnames(LT_comparisson_BZE_BWI_SP), value=TRUE))
LT_comparisson_BZE_BWI_SP[, paste0(str_sub(BZE_vars, end=-5), "_diff")] <- LT_comparisson_BZE_BWI_SP[, BZE_vars] - LT_comparisson_BZE_BWI_SP[, BWI_vars]


summary(LT_comparisson_BZE_BWI_SP)

# 1.3.6. linear model to explain differences in Biomass between BW ------------------------------------------------------------------
LT_comparisson_BZE_BWI_SP <- 
LT_comparisson_BZE_BWI_SP %>% 
  # 1. introducte table with coefficients from lm with for B_t_ha_diff = b0 + b1*BA_m2_ha_diff + b2*Nt_diff
  left_join(.,LT_comparisson_BZE_BWI_SP %>% 
              # filter(compartiment == "ag") %>% 
              # 1.2. fit linear model with 
            lm_table(B_t_ha_diff ~ BA_m2_ha_diff + n_ha_diff, c("BWI_comparisson_group", "compartiment"), output = "table") %>% 
            select(BWI_comparisson_group,compartiment, b0, b1, b2, Rsqr) %>% 
            # 1.3. replace NAs with 0 
            mutate(., across(c("b0","b1","b2", "Rsqr"), ~ replace(., is.na(.), 0))),
          by = c("BWI_comparisson_group", "compartiment")) %>% 
  # 2. predict the expectable B_diff at the given basal area and tree number difference
  mutate(B_diff_pred = b0 + b1*BA_m2_ha_diff + b2*n_ha_diff,  
  # 3. calculate difference between calculated values and predicted values 
         B_pred_vs_B_calc = B_t_ha_diff - B_diff_pred)

 # 4. calcualte SD 
LT_comparisson_BZE_BWI_SP <- LT_comparisson_BZE_BWI_SP %>% 
  left_join(., LT_comparisson_BZE_BWI_SP %>% 
              group_by(BWI_comparisson_group) %>%
              summarize(SD_SP_B_diff = sd(B_pred_vs_B_calc)),
            by = "BWI_comparisson_group") %>% 
  mutate(SD_class = SD_class(SD_SP_B_diff, B_pred_vs_B_calc)) 



# 1.3.7. visualizing SD differences  --------------------------------------
for (i in 1:length(unique(LT_comparisson_BZE_BWI_SP$BWI_comparisson_group))) {
   #i = 8
  my.bwi.sp.group <- unique(LT_comparisson_BZE_BWI_SP$BWI_comparisson_group)[i]
  
print(
  LT_comparisson_BZE_BWI_SP %>% 
    filter(BWI_comparisson_group == my.bwi.sp.group & compartiment == "ag") %>% 
    # ggplot(., aes(x = n_ha_diff))+
    ggplot(., aes(x = BA_m2_ha_diff))+
    geom_point(aes(y = B_t_ha_diff))+
    # https://stackoverflow.com/questions/15624656/label-points-in-geom-point
    geom_text(aes(y = B_t_ha_diff, label=ifelse(SD_class %in% c(3, 4, 5),as.character(plot_ID),'')),hjust=0,vjust=0)+
    geom_smooth(aes(y = B_diff_pred, color = "B_diff_pred"), se = FALSE)+
    geom_smooth(aes(y = (B_diff_pred+SD_SP_B_diff), color = "SD_class 1"), se = FALSE)+
    geom_smooth(aes(y = (B_diff_pred-SD_SP_B_diff), color = "SD_class 1"), se = FALSE)+
    geom_smooth(aes(y = (B_diff_pred+2*SD_SP_B_diff), color = "SD_class 2"), se = FALSE)+
    geom_smooth(aes(y = (B_diff_pred-2*SD_SP_B_diff), color = "SD_class 2"), se = FALSE)+
    #geom_vline(xintercept = 0)+
    geom_hline(yintercept=0)+
    #xlim(max(LT_comparisson_BZE_BWI_SP$BA_m2_ha_diff)*(-1), max(LT_comparisson_BZE_BWI_SP$BA_m2_ha_diff))+
    #ylim(max(LT_comparisson_BZE_BWI_SP$B_t_ha_diff)*(-1), max(LT_comparisson_BZE_BWI_SP$B_t_ha_diff))+
    # geom_line()+
    # facet_wrap(~BWI_comparisson_group)+
    xlab("difference BA m2 per hectar (BZE-BWI)") +
    ylab("difference biomass t per hectar (BZE-BWI)")+
    ggtitle(paste("comparisson stands: B diff vs. BA diff by BWI species:", my.bwi.sp.group))+
    theme_light()+
    theme(legend.position = "right")
  )
  
  
}



