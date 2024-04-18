
# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the the national soil inventory
  # estimating missing tree heights based on sampled height and diameter pairs
  # via nls per plot and species or just species and via SLOBODA and CURTIS

# ----- 0. SETUP ---------------------------------------------------------------

# ----- 0.1. packages and functions --------------------------------------------
source(paste0(getwd(), "/scripts/01_00_functions_library.R"))


# ----- 0.2. working directory -------------------------------------------------
here::here()

out.path.BZE3 <- ("output/out_data/out_data_BZE/") 

# ----- 0.3 data import --------------------------------------------------------
# LIVING TREES
# hbi BE dataset: 
  # this dataset contains the inventory data of the tree inventory accompanying the second national soil inventory
  # here we import a dataset called "HBI_LT_update_2.csv" which contains plot area and stand data additionally to the original tree data
# currently we can´t tho, cause the sorting regarding tree inventory status is just a simulation at the moment so the data are manipulated
HBI_trees <- read.delim(file = here(paste0(out.path.BZE3, "HBI_LT_update_1.csv")), sep = ";", dec = ",") %>% 
  select(-c("X.2", "X.1", "X"))
BZE3_trees <- read.delim(file = here(paste0(out.path.BZE3, "BZE3_LT_update_2.csv")), sep = ";", dec = ",")%>% 
    select(-c("X.2", "X.1", "X.3", "X"))


# ----- 0.6 harmonising column names & structure  -----------------------------------------------------------------
## HBI
 HBI_trees[,c("plot_A_ha", "area_m2", "X_tree",  "Y_tree",
             "DBH_cm", "dist_m","CCS_r_m")] <- lapply(HBI_trees[,c("plot_A_ha", "area_m2", "X_tree",  "Y_tree",
                                                "DBH_cm", "dist_m",  "CCS_r_m")], as.numeric)
# change -2 in H_dm and C_h_dm to NA: https://stackoverflow.com/questions/14737773/replacing-occurrences-of-a-number-in-multiple-columns-of-data-frame-with-another
HBI_trees[,c("H_dm","C_h_dm")][HBI_trees[,c("H_dm","C_h_dm")]== -2] <- NA
HBI_trees[,c("H_dm","C_h_dm")][HBI_trees[,c("H_dm","C_h_dm")]== -9] <- NA



# BZE3_trees
BZE3_trees[,c("plot_A_ha", "area_m2", "X_tree",  "Y_tree",
"DBH_cm", "dist_m","CCS_r_m")] <- lapply(BZE3_trees[,c("plot_A_ha", "area_m2", "X_tree",  "Y_tree", 
                                                                     "DBH_cm", "dist_m",  "CCS_r_m")], as.numeric)
 # change -2 in H_dm and C_h_dm to NA: https://stackoverflow.com/questions/14737773/replacing-occurrences-of-a-number-in-multiple-columns-of-data-frame-with-another
 BZE3_trees[,c("H_dm","C_h_dm")][BZE3_trees[,c("H_dm","C_h_dm")]== -2] <- NA
 BZE3_trees[,c("H_dm","C_h_dm")][BZE3_trees[,c("H_dm","C_h_dm")]== -9] <- NA


# 1. joining in external info  -------------------------------------------------
trees_total <-  rbind(HBI_trees, BZE3_trees %>% 
            select(c(colnames(HBI_trees)))) %>%
# calcualte diameter and change units -----------------------------------
  mutate(H_m = H_dm/10, 
         DBH_h_m = DBH_h_cm/100) %>%                               # change unit of DBH measuring height from cm into m by dividing by 100  
  #  apply regression of BWI (5.5.1.2.) for DBH estimation when mesasuring height differs from 1.3 m 
  mutate(DBH_class = DBH_c_function(DBH_cm), 
         BA_m2 = c_A(DBH_cm/2)*0.0001) %>% # *0.0001 to convert cm2 in m2 %>% 
 arrange(plot_ID, inv)




# 2. estimating tree height -----------------------------------------------

# 2.1. fitting own models H_m ~ nls(DBH_cm) -------------------------------

# 2.1.1. non linear height model h ~ DBH per species and plot ----------------------------
# to calculate individual tree heights for trees of the samme species and plot 
# where the height has not been sampled we create a non-linear regression for the heights
# in the following a dataframe with regression coefficients per 
# species per plot is created if there are more then 3 heights measured per species and plot

# coefficents of non-linear height model per species and plot
# https://rdrr.io/cran/forestmangr/f/vignettes/eq_group_fit_en.Rmd
coeff_H_SP_P <- left_join(
  # select variables needed for modeling 
  trees_total %>% select(plot_ID, SP_code, H_m, DBH_cm) %>% 
    # filter for measured heights that also the necessary info about the diameter and measuring height of diameter
    filter(!is.na(H_m) & !is.na(DBH_cm)) %>% 
    group_by(plot_ID, SP_code) %>%
    # filter for plots that have at least 3 heights measured per species
    filter(n() >= 3),
  # creaing & joining in coeff_H_SP_P dataset 
  trees_total %>% 
    select(plot_ID, SP_code, H_m, DBH_cm) %>% 
    filter(!is.na(H_m) & !is.na(DBH_cm)) %>% 
    #filter(DBH_cm <= 150) %>% 
    group_by(plot_ID, SP_code) %>% 
    # filter for plots where there is at least 3 heights measured for each species
    #https://stackoverflow.com/questions/20204257/subset-data-frame-based-on-number-of-rows-per-group
    filter(n() >= 3)%>%    
    group_by(plot_ID, SP_code) %>%
    nls_table( H_m ~ b0 * (1 - exp( -b1 * DBH_cm))^b2, 
               mod_start = c(b0=23, b1=0.03, b2 =1.3), 
               output = "table") %>%
    arrange(plot_ID, SP_code), 
  by = c("plot_ID", "SP_code"))%>%
  # mutating statistical precictors
  mutate(H_est = b0 * (1 - exp( -b1 * DBH_cm))^b2) %>% 
  group_by(plot_ID, SP_code) %>% 
  summarise( b0 = mean(b0), 
             b1 = mean(b1), 
             b2 = mean(b2), 
             #https://rdrr.io/cran/forestmangr/f/vignettes/eq_group_fit_en.Rmd
             bias = bias_per(y = H_m, yhat = H_est),
             rsme = rmse_per(y = H_m, yhat = H_est),
             #https://stackoverflow.com/questions/14530770/calculating-r2-for-a-nonlinear-least-squares-fit
             R2 = max(cor(H_m, H_est),0)^2,
             #https://stats.stackexchange.com/questions/11676/pseudo-r-squared-formula-for-glms
             mean_h = mean(H_m), 
             #N = length(H_m), 
             SSres = sum((H_m-H_est)^2), 
             SStot = sum((H_m-mean_h)^2), 
             pseu_R2 = 1-(SSres/SStot), 
             diff_h = mean(H_m - H_est))

# 2.1.2. non linear height model h ~ DBH per species over all plots ----------------------------
# coefficents of non-linear height model per species but over all plots: 
# https://rdrr.io/cran/forestmangr/f/vignettes/eq_group_fit_en.Rmd
#  building separate dataframe for speicies soecific models adding adding bias, rmse and rsqrd 

coeff_H_SP <- left_join(trees_total %>% 
                          select(SP_code, H_m, DBH_cm, DBH_class) %>% 
                          filter(!is.na(H_m) & !is.na(DBH_cm) & !is.na(DBH_class)) %>% 
                          group_by(SP_code) %>% 
                          filter(n() >= 3),
                        trees_total %>% 
                          select(SP_code, H_m, DBH_cm, DBH_class) %>% 
                          filter(!is.na(H_m) & !is.na(DBH_cm) & !is.na(DBH_class) ) %>% 
                          #filter(DBH_cm <= 150) %>% 
                          group_by(SP_code) %>% 
                          # filter for plots where there is at least 3 heights measured for each species
                          #https://stackoverflow.com/questions/20204257/subset-data-frame-based-on-number-of-rows-per-group
                          filter(n() >= 3)%>%    
                          group_by(SP_code) %>%
                          nls_table( H_m ~ b0 * (1 - exp( -b1 * DBH_cm))^b2, 
                                     mod_start = c(b0=23, b1=0.03, b2 =1.3), 
                                     output = "table"), 
                        by = c("SP_code"))%>%
  mutate(H_est = b0 * (1 - exp( -b1 * DBH_cm))^b2) %>% 
  group_by(SP_code) %>% 
  summarise( b0 = mean(b0), 
             b1 = mean(b1), 
             b2 = mean(b2), 
             #https://rdrr.io/cran/forestmangr/f/vignettes/eq_group_fit_en.Rmd
             bias = bias_per(y = H_m, yhat = H_est),
             rsme = rmse_per(y = H_m, yhat = H_est),
             #https://stackoverflow.com/questions/14530770/calculating-r2-for-a-nonlinear-least-squares-fit
             R2 = max(cor(H_m, H_est),0)^2,
             #https://stats.stackexchange.com/questions/11676/pseudo-r-squared-formula-for-glms
             mean_h = mean(H_m), 
             #N = length(H_m), 
             SSres = sum((H_m-H_est)^2), 
             SStot = sum((H_m-mean_h)^2), 
             pseu_R2 = 1-(SSres/SStot), 
             diff_h = mean(H_m - H_est)) %>% 
  mutate(plot_ID = as.factor('all')) %>% 
  select(plot_ID, SP_code, b0, b1, b2, bias, rsme, R2, mean_h, SSres, SStot, pseu_R2, diff_h)

 

 # 2.1.3 combined coefficients of height models ---------------------
coeff_H_comb <- rbind(coeff_H_SP_P %>% mutate(plot_ID = as.factor(plot_ID)), coeff_H_SP)

# 2.2 calculating Hg, Dg etc. per plot, stand, species--------------------------
# sampling circuits: 
# 5.64 7cm <= 10cm
#12.62 >10cm <= 30
# 17.84 >= 30

#calcualte the height and diameter of a stem reprensenting the mean basal area 
# this is creates a tree dataset with mean BHD, d_g, h_g per species per plot per canopy layer which we need for SLOBODA 
Hg_Dg_trees_total.df <- trees_total %>%                              
  group_by(inv, plot_ID, stand, C_layer, SP_code, CCS_r_m) %>%    # group by plot and species, canopy layer and sampling circuit to calcualte all paremeters needed 
  summarise(no_trees_CC = n(),
            BA_CC = sum(BA_m2),                        # sum up basal  area per sampling circuit to then reffer it to the hektar value of the respective circuit
            CC_A_ha = mean(plot_A_ha),                   # mean area in ha per sampling circuit
            BA_CC_m2_ha = BA_CC/CC_A_ha,               # calculating the BA hectare value of each tree species per c layer to account for the different sampling circuits
            no_trees_CC_ha = no_trees_CC/CC_A_ha,
            mean_DBH_mm_CC = mean(DBH_cm*10),          # calculate mean DBH per sampling circuit and species and C layer and plot 
            mean_H_m_CC = mean(na.omit(H_m))) %>%      # calculate mean height per sampling circuit and species and C layer and plot    
  group_by(inv, plot_ID, stand, C_layer, SP_code )%>%            # group by plot and species,  canopy layer and sampling circuit to calcualte dg, hg 
  summarize(no_trees_ha = sum(no_trees_CC_ha),                                # calculate number of trees per plot
            BA_m2_ha = sum(BA_CC_m2_ha),               # calculate sum of BA across all sampling circuit to account for represnation of different trees in the sampling circuits
            mean_DBH_mm = mean(mean_DBH_mm_CC),        # calculate mean of DBH across all sampling circuit to account for represnation of different trees in the sampling circuits
            mean_H_m = mean(mean_H_m_CC),              # calculate mean of height across all sampling circuit to account for represnation of different trees in the sampling circuits
           mean_BA_m2_tree = BA_m2_ha/no_trees_ha,
            H_g = sum(mean(na.omit(mean_H_m))*BA_m2_ha)/sum(BA_m2_ha),    # Hoehe des Grundflächemittelstammes, calculation according to S. Schnell
            mean_DBH_mm = mean(mean_DBH_mm),                           # mean diameter per species per canopy layer per plot
            D_g = ((sqrt((mean_BA_m2_tree/pi)))*2)*100) %>%               #  Durchmesser des Grundflächenmittelstammes; *100 to get from 1m -> 100cm    
   arrange(plot_ID)


# 2.3. height calculation -------------------------------------------------
# 2.3.1. height calculation HBI -------------------------------------------------
HBI_trees_update_3 <-     # this should actually be the BZE3 Datset 
  trees_total %>% 
  filter(inv=="HBI")%>% 
## joining coefficients and Hg-Dg-data in
  unite(SP_P_ID, plot_ID, SP_code, sep = "", remove = FALSE) %>%            # create column matching vectorised coefficients of coeff_SP_P (1.3. functions, h_nls_SP_P, dplyr::pull)
  left_join(.,coeff_H_SP_P %>%                                              # joining R2 from coeff_SP_P -> R2.x
              select(plot_ID, SP_code, R2) %>% 
              unite(SP_P_ID, plot_ID, SP_code, sep = "", remove = FALSE),   # create column matching vectorised coefficients of coeff_SP_P (1.3. functions, h_nls_SP_P, dplyr::pull)
            by = c("plot_ID", "SP_code", "SP_P_ID")) %>% 
  left_join(., coeff_H_SP %>% select(SP_code, R2),               # joing R2 from coeff_SP data set -> R2.y
            by = "SP_code") %>% 
  # this is joins in a tree dataset with mean BHD, d_g, h_g per species per plot per canopy layer which we need for SLOBODA 
  left_join(., Hg_Dg_trees_total.df,
            by = c("inv", "plot_ID", "stand", "SP_code", "C_layer")) %>% 
  mutate(R2_comb = f(R2.x, R2.y, R2.y, R2.x),                               # if R2 is na, put R2 from coeff_SP_P unless R2 from coeff_SP is higher
         H_method = case_when(is.na(H_m) & !is.na(R2.x) & R2.x > 0.50 | is.na(H_m) & R2.x > R2.y & R2.x > 0.50 ~ "coeff_SP_P", 
                              is.na(H_m) & is.na(R2.x) & R2.y > 0.50| is.na(H_m) & R2.x < R2.y & R2.y > 0.50 ~ "coeff_sp",
                              is.na(H_m) & is.na(R2_comb) & !is.na(H_g)| is.na(H_m) & R2_comb < 0.50 & !is.na(H_g) ~ "ehk_sloboda",
                              is.na(H_m) & is.na(R2_comb) & is.na(H_g)| is.na(H_m) & R2_comb < 0.50 & is.na(H_g) ~ "h_curtis", 
                              TRUE ~ "sampled")) %>% 
  # When h_m is na but there is a plot and species wise model with R2 above 0.7, use the model to predict the height
  mutate(H_m = as.numeric(case_when(is.na(H_m) & !is.na(R2.x) & R2.x > 0.50 | is.na(H_m) & R2.x > R2.y & R2.x > 0.50 ~ h_nls_SP_P(SP_P_ID, DBH_cm),
                         # if H_m is na and there is an R2 from coeff_SP_P thats bigger then 0.75 or of theres no R2 from 
                         # coeff_SP_plot that´s bigger then R2 of coeff_SP_P while the given R2 from coeff_SP_P is above 
                         # 0.75 then use the SP_P models
                         is.na(H_m) & is.na(R2.x) & R2.y > 0.50 | is.na(H_m) & R2.x < R2.y & R2.y > 0.50 ~ h_nls_SP(SP_code, DBH_cm),
                         # when there´s still no model per species or plot, or the R2 of both self-made models is below 0.7 
                         # and hm is na but there is a h_g and d_G
                         is.na(H_m) & is.na(R2_comb) & !is.na(H_g)| is.na(H_m) & R2_comb < 0.50 & !is.na(H_g) ~ ehk_sloboda(H_SP_group, DBH_cm*10, mean_DBH_mm, D_g, H_g),
                         # when there´s still no model per species or plot, or the R2 of both self-made models is below 0.7 
                         # and hm is na and the Slobody function cannot eb applied because there is no h_g calculatable use the curtis function
                         is.na(H_m) & is.na(R2_comb) & is.na(H_g)| is.na(H_m) & R2_comb < 0.50 & is.na(H_g) ~ h_curtis(H_SP_group, DBH_cm*10), 
                         TRUE ~ H_m))) %>% 
   # as there were some trees that had an estimated height which was lower then the DBH measuring height. this is not only implausible but also won´t work for TapeS 
   # thus we correct these heights afterwards by estimating their height from the relation between the dg and hg and dg and the trees DBH (dreisatz, h_proportional function)
   mutate(H_m = ifelse(DBH_h_m > H_m, h_proportional(D_g, H_g, DBH_cm), H_m))  %>% 
  # select columns that should enter the next step of data processing
    select(plot_ID, inv, inv_year, stand, tree_ID,  tree_inventory_status,  multi_stem, dist_cm,  azi_gon, age, age_meth,  
           SP_code, Chr_code_ger, tpS_ID, LH_NH, H_SP_group, BWI_SP_group, Bio_SP_group, N_SP_group, N_bg_SP_group, N_f_SP_group_MoMoK,
            DBH_class,  Kraft, C_layer, H_dm, H_m, H_method, C_h_dm, D_mm,   DBH_h_cm,  DBH_cm, BA_m2,
           CCS_r_m, stand, stand_plot_A_ha, plot_A_ha)

 

# 2.3.2. height calculation BZE -------------------------------------------------
BZE3_trees_update_3 <-  trees_total %>% 
  filter(inv=="BZE3")%>% 
## 2.3.1. joining coefficients and Hg-Dg-data in 
  unite(SP_P_ID, plot_ID, SP_code, sep = "", remove = FALSE) %>%            # create column matching vectorised coefficients of coeff_SP_P (1.3. functions, h_nls_SP_P, dplyr::pull)
  left_join(.,coeff_H_SP_P %>%                                              # joining R2 from coeff_SP_P -> R2.x
              select(plot_ID, SP_code, R2) %>% 
              unite(SP_P_ID, plot_ID, SP_code, sep = "", remove = FALSE),   # create column matching vectorised coefficients of coeff_SP_P (1.3. functions, h_nls_SP_P, dplyr::pull)
            by = c("plot_ID", "SP_code", "SP_P_ID")) %>% 
  left_join(., coeff_H_SP %>% select(SP_code, R2),               # joing R2 from coeff_SP data set -> R2.y
            by = "SP_code") %>% 
  # this is joins in a tree dataset with mean BHD, d_g, h_g per species per plot per canopy layer which we need for SLOBODA 
  left_join(., Hg_Dg_trees_total.df,
            by = c("plot_ID", "inv","stand", "SP_code", "C_layer")) %>% 
  mutate(R2_comb = f(R2.x, R2.y, R2.y, R2.x),                               # if R2 is na, put R2 from coeff_SP_P unless R2 from coeff_SP is higher
         H_method = case_when(is.na(H_m) & !is.na(R2.x) & R2.x > 0.50 | is.na(H_m) & R2.x > R2.y & R2.x > 0.50 ~ "coeff_SP_P", 
                              is.na(H_m) & is.na(R2.x) & R2.y > 0.50| is.na(H_m) & R2.x < R2.y & R2.y > 0.50 ~ "coeff_sp",
                              is.na(H_m) & is.na(R2_comb) & !is.na(H_g)| is.na(H_m) & R2_comb < 0.50 & !is.na(H_g) ~ "ehk_sloboda",
                              is.na(H_m) & is.na(R2_comb) & is.na(H_g)| is.na(H_m) & R2_comb < 0.50 & is.na(H_g) ~ "h_curtis", 
                              TRUE ~ "sampled")) %>% 
  # When h_m is na but there is a plot and species wise model with R2 above 0.7, use the model to predict the height
  mutate(H_m = as.numeric(case_when(is.na(H_m) & !is.na(R2.x) & R2.x > 0.50 | is.na(H_m) & R2.x > R2.y & R2.x > 0.50 ~ h_nls_SP_P(SP_P_ID, DBH_cm),
                         # if H_m is na and there is an R2 from coeff_SP_P thats bigger then 0.75 or of theres no R2 from 
                         # coeff_SP_plot that´s bigger then R2 of coeff_SP_P while the given R2 from coeff_SP_P is above 
                         # 0.75 then use the SP_P models
                         is.na(H_m) & is.na(R2.x) & R2.y > 0.50 | is.na(H_m) & R2.x < R2.y & R2.y > 0.50 ~ h_nls_SP(SP_code, DBH_cm),
                         # when there´s still no model per species or plot, or the R2 of both self-made models is below 0.7 
                         # and hm is na but there is a h_g and d_G
                         is.na(H_m) & is.na(R2_comb) & !is.na(H_g)| is.na(H_m) & R2_comb < 0.50 & !is.na(H_g) ~ ehk_sloboda(H_SP_group, DBH_cm*10, mean_DBH_mm, D_g, H_g),
                         # when there´s still no model per species or plot, or the R2 of both self-made models is below 0.7 
                         # and hm is na and the Slobody function cannot eb applied because there is no h_g calculatable use the curtis function
                         is.na(H_m) & is.na(R2_comb) & is.na(H_g)| is.na(H_m) & R2_comb < 0.50 & is.na(H_g) ~ h_curtis(H_SP_group, DBH_cm*10), 
                         TRUE ~ H_m))) %>% 
    # as there were some trees that had an estimated height which was lower then the DBH measuring height. this is not only implausible but also won´t work for TapeS 
  # thus we correct these heights afterwards by estimating their height from the relation between the dg and hg and dg and the trees DBH (dreisatz, h_proportional function)
  mutate(H_m = ifelse(DBH_h_m > H_m, h_proportional(d_g, H_g, DBH_cm), H_m))  %>% 
    # select columns that should enter the next step of data processing
    select(plot_ID, inv, inv_year, stand, tree_ID,  tree_inventory_status,  multi_stem, dist_cm,  azi_gon, age, age_meth,  
           SP_code, Chr_code_ger, tpS_ID, LH_NH, H_SP_group, BWI_SP_group, Bio_SP_group, N_SP_group, N_bg_SP_group, N_f_SP_group_MoMoK,
           DBH_class,  Kraft, C_layer, H_dm, H_m, H_method, C_h_dm, D_mm,   DBH_h_cm,  DBH_cm, BA_m2,
           CCS_r_m, stand, stand_plot_A_ha, plot_A_ha)

 

# 1.1.2.6. remove problematik trees ---------------------------------------
 HBI_trees_removed_3 <- HBI_trees_update_3 %>% filter(DBH_h_cm/100 >= H_m | H_m >50)
 BZE3_trees_removed_3 <- BZE3_trees_update_3 %>% filter(DBH_h_cm/100 >= H_m | H_m >50) 
 

# ---- 1.1.2.6. exporting dataset --------------------------

 
 # height nls coefficients
write.csv2(coeff_H_comb, paste0(out.path.BZE3, paste("coef_H", unique(HBI_trees_update_3$inv)[1], unique(BZE3_trees_update_3$inv)[1], sep = "_"), ".csv"))
                               
# HBI dataset including estimated heights
write.csv2(HBI_trees_update_3, paste0(out.path.BZE3, paste(unique(HBI_trees_update_3$inv)[1], "LT_update_3", sep = "_"), ".csv"))
write.csv2(HBI_trees_removed_3, paste0(out.path.BZE3, paste(unique(HBI_trees_update_3$inv)[1], "LT_removed_3", sep = "_"), ".csv"))

# BZE3 dataset including estimated heights
write.csv2(BZE3_trees_update_3, paste0(out.path.BZE3, paste(unique(BZE3_trees_update_3$inv)[1], "LT_update_3", sep = "_"), ".csv"))
write.csv2(BZE3_trees_removed_3, paste0(out.path.BZE3, paste(unique(BZE3_trees_update_3$inv)[1], "LT_removed_3", sep = "_"), ".csv"))



#write.csv(HBI_trees_update_3 %>% filter( H_m > 50),  paste0(out.path.BZE3, paste(unique(BZE3_trees_update_3$inv)[1], "weird_sloboda_heights", sep = "_"), ".csv"))


