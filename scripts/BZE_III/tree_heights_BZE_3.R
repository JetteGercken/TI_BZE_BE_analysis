<<<<<<< HEAD
# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the peat land soil inventory
# sorting trees according to tree 
# ----- 0. SETUP ---------------------------------------------------------------

# ----- 0.1. packages and functions --------------------------------------------


source(paste0(getwd(), "/scripts/functions_library.R"))


# ----- 0.2. working directory -------------------------------------------------
here::here()
getwd()

out.path.BZE3 <- ("output/out_data/out_data_BZE/") 

# ----- 0.3 data import --------------------------------------------------------
# LIVING TREES
# BZE3 BE dataset: this dataset contains the inventory data of the tree inventory accompanying the third national soil inventory
HBI_trees <- read.delim(file = here("data/input/BZE2_HBI/beab.csv"), sep = ",", dec = ",", stringsAsFactors=FALSE)
# stand info od individual trees 
stand_info_HBI_trees <-  read.delim(file = here("output/out_data/out_data_BZE/all_trees_status.csv"), sep = ",", dec = ",", stringsAsFactors=FALSE)
plot_areas_HBI <- read.delim(file = here("output/out_data/out_data_BZE/all_edges_rem_circles_area.csv"), sep = ",", dec = ",", stringsAsFactors=FALSE)
  

# species names & codes 
SP_names_com_ID_tapeS <- read.delim(file = here("output/out_data/x_bart_tapeS.csv"), sep = ",", dec = ",") 



# ----- 0.6 harmonising column names & structure  -----------------------------------------------------------------
# HBI 
colnames(HBI_trees) <- c("multi_stem", "D_mm", "DBH_class", "DBH_h_cm", "H_dm",
                         "azi_gon", "SP_code", "tree_ID", "plot_ID", "tree_inventory_status", 
                         "DBH_cm", "age", "C_layer", "C_h_dm", "Kraft", "Dist_cm", "age_meth")  
HBI_trees <- HBI_trees %>% 
  select(plot_ID,  tree_ID,  tree_inventory_status,  multi_stem, Dist_cm,  azi_gon, age, age_meth,  
         SP_code, DBH_class,  Kraft, C_layer, H_dm,  C_h_dm, D_mm,   DBH_h_cm,  DBH_cm ) 
 
# harmonize strings of plot_area_HBI  
# https://stackoverflow.com/questions/20637360/convert-all-data-frame-character-columns-to-factors
plot_areas_HBI[,c(1,2, 3, 4)] <- lapply(plot_areas_HBI[,c(1,2, 3, 4)], as.numeric)



# 1. joining in external info  -------------------------------------------------

trees_total <- HBI_trees %>%
# add inventory info -----------------------------------------------------
  mutate(inv = "HBI",
         inv_year = 2012) %>% 
  # here we would actually rbind the both inventory datasets together 
# join in species codes --------------------------------------------------
  left_join(., SP_names_com_ID_tapeS %>% 
              mutate(char_code_ger_lowcase = tolower(Chr_code_ger)), 
            by = c("SP_code" = "char_code_ger_lowcase")) %>% 
# calcualte diameter and change units -----------------------------------
  mutate(DBH_h_cm = ifelse(is.na(DBH_h_cm), 130, DBH_h_cm),        # assign DBH measuring height of 130cm when missing
         DBH_h_m = DBH_h_cm/100) %>%                               # change unit of DBH measuring height from cm into m by dividing by 100  
  #  apply regression from BWI
  mutate(DBH_cm = ifelse(DBH_h_cm == 130, D_mm/10, DBH_BWI(D_mm, DBH_h_cm)),
         DBH_class = ifelse(is.na(DBH_class), DBH_c_function(DBH_cm), DBH_class), 
         BA_m2 = c_A(DBH_cm/2)*0.0001) %>% 
# join in tree stand info ---------------------------------------------------------
  left_join(., stand_info_HBI_trees %>% 
              mutate(inv = "HBI") %>% 
              select(plot_ID, tree_ID, inv, t_stat), 
            by = c("plot_ID", "tree_ID", "inv"), 
            multiple = "all") %>% 
  rename(stand = t_stat) %>% 
  filter(stand != "warning") %>% 
# join in plot area depednign on diameter of tree and stand of tree ---------------
  # asssing corect samling circle diameter according to DBH of the tree
  mutate(CCS_r_m = case_when(DBH_cm >= 7  & DBH_cm < 10 ~ 5.64, 
                             DBH_cm >= 10 & DBH_cm < 30 ~ 12.62, 
                             DBH_cm >= 30 ~ 17.84, 
                             TRUE ~ NA)) %>% 
  left_join(., plot_areas_HBI %>% 
              mutate(inv = "HBI"),
            by = c("plot_ID", "CCS_r_m", "inv", "stand")) %>% 
  mutate(area_m2 = ifelse(stand == "A" & is.na(e_ID) & is.na(area_m2), c_A(CCS_r_m), area_m2), 
         plot_A_ha = as.numeric(area_m2)/10000)



# check if there are no trees left that don´t have a SP_code in xBart/ SP_names_com_ID_tapeS
SP_NAs_HBI <- HBI_trees %>% 
  anti_join(SP_names_com_ID_tapeS %>% 
              mutate(char_code_ger_lowcase = tolower(Chr_code_ger)), 
            by = c("SP_code" = "char_code_ger_lowcase"))

if(nrow(SP_NAs_HBI) != 0){print("There are species names or codes in HBI dataset that do not match
                                the species names and codes listed in x_bart")}else{""}

# BZE3_trees <- BZE3_trees %>% 
#   mutate(inventory = "HBI") %>% 
#   left_join(., SP_names_com_ID_tapeS %>% 
#               mutate(char_code_ger_lowcase = tolower(Chr_code_ger)), 
#             by = c("SP_code" = "char_code_ger_lowcase"))
# 
# 
# # check if there are no trees left that don´t have a SP_code in xBart/ SP_names_com_ID_tapeS
# BZE3_trees %>% 
#   anti_join(., SP_names_com_ID_tapeS %>% 
#               mutate(char_code_ger_lowcase = tolower(Chr_code_ger)), 
#             by = c("SP_code" = "char_code_ger_lowcase"))



# 2. estimating tree height -----------------------------------------------

      


# ----- 1.1.2.3. non linear height model h ~ DBH per species and plot ----------------------------
# to calculate individual tree heights for trees of the samme species and plot 
# where the height has not been sampled we create a non-linear regression for the heights
# in the following a dataframe with regression coefficients per 
# species per plot is created if there are more then 3 heights measured per species and plot

# coefficents of non-linear height model per species and plot
# https://rdrr.io/cran/forestmangr/f/vignettes/eq_group_fit_en.Rmd
coeff_H_SP_P <- left_join(
  # select variables needed for modeling 
  trees_total %>% select(plot_ID, SP_code, H_m, DBH_cm, DBH_class) %>% 
    # filter for measured heights that also the necessary info about the diameter and measuring height of diameter
    filter(!is.na(H_m) & !is.na(DBH_cm) & !is.na(DBH_class)) %>% 
    group_by(plot_ID, SP_code) %>%
    # filter for plots that have at least 3 heights measured per species
    filter(n() >= 3),
  # creaing & joining in coeff_H_SP_P dataset 
  trees_total %>% 
    select(plot_ID, SP_code, H_m, DBH_cm, DBH_class) %>% 
    filter(!is.na(H_m) & !is.na(DBH_cm) & !is.na(DBH_class) ) %>% 
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


# ----- 1.1.2.3. non linear height model h ~ DBH per species over all plots ----------------------------

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

# ----- 1.1.2.4. combined coefficients of height models ---------------------
coeff_H_comb <- rbind(coeff_H_SP_P %>% mutate(plot_ID = as.factor(plot_ID)), coeff_H_SP)

# ---- 1.1.2.5. joining coordinates into trees datset --------------------------
# sampling circuits: 
# 5.64 7cm <= 10cm
#12.62 >10cm <= 30
# 17.84 >= 30

trees_total <-     # this should actually be the BZE3 Datset 
  trees_total %>% 
  unite(SP_P_ID, plot_ID, SP_code, sep = "", remove = FALSE) %>%            # create column matching vectorised coefficients of coeff_SP_P (1.3. functions, h_nls_SP_P, dplyr::pull)
  left_join(.,coeff_H_SP_P %>%                                              # joining R2 from coeff_SP_P -> R2.x
              select(plot_ID, SP_code, R2) %>% 
              unite(SP_P_ID, plot_ID, SP_code, sep = "", remove = FALSE),   # create column matching vectorised coefficients of coeff_SP_P (1.3. functions, h_nls_SP_P, dplyr::pull)
            by = c("plot_ID", "SP_code", "SP_P_ID")) %>% 
  left_join(., coeff_H_SP %>% select(SP_code, R2),               # joing R2 from coeff_SP data set -> R2.y
            by = "SP_code") %>% 
  # this is creates a tree dataset with mean BHD, d_g, h_g per species per plot per canopy layer which we need for SLOBODA 
  left_join(., trees_total %>%      # this should be the BZE3 Dataset                           
              group_by(plot_ID, C_layer, SP_code, CC_no) %>%    # group by plot and species, canopy layer and sampling circuit to calcualte all paremeters needed 
              summarise(BA_CC = sum(BA_m2),                        # sum up basal  area per sampling circuit to then reffer it to the hektar value of the respective circuit
                        CC_A_ha = mean(CC_A_ha),                   # mean area in ha per sampling circuit
                        BA_CC_m2_ha = BA_CC/CC_A_ha,               # calculating the BA hectare value of each tree species per c layer to account for the different sampling circuits
                        mean_DBH_mm_CC = mean(DBH_cm*10),          # calculate mean DBH per sampling circuit and species and C layer and plot 
                        mean_H_m_CC = mean(na.omit(H_m))) %>%      # calculate mean height per sampling circuit and species and C layer and plot    
              group_by(plot_ID, C_layer, SP_code)%>%            # group by plot and species,  canopy layer and sampling circuit to calcualte dg, hg 
              summarize(BA_m2_ha = sum(BA_CC_m2_ha),               # calculate sum of BA across all sampling circuit to account for represnation of different trees in the sampling circuits
                        mean_DBH_mm = mean(mean_DBH_mm_CC),        # calculate mean of DBH across all sampling circuit to account for represnation of different trees in the sampling circuits
                        mean_H_m = mean(mean_H_m_CC),              # calculate mean of height across all sampling circuit to account for represnation of different trees in the sampling circuits
                        H_g = sum(mean(na.omit(mean_H_m))*BA_m2_ha)/sum(BA_m2_ha),    # Hoehe des Grundflächemittelstammes, calculation according to S. Schnell
                        mean_DBH_mm = mean(mean_DBH_mm),                           # mean diameter per species per canopy layer per plot
                        D_g = ((sqrt((mean(BA_m2_ha)/pi)))*2)*100),              #  Durchmesser des Grundflächenmittelstammes; *100 to get from 1m -> 100cm    
            by = c("plot_ID", "SP_code", "C_layer")) %>% 
  mutate(R2_comb = f(R2.x, R2.y, R2.y, R2.x),                               # if R2 is na, put R2 from coeff_SP_P unless R2 from coeff_SP is higher
         H_method = case_when(is.na(H_m) & !is.na(R2.x) & R2.x > 0.70 | is.na(H_m) & R2.x > R2.y & R2.x > 0.7 ~ "coeff_SP_P", 
                              is.na(H_m) & is.na(R2.x) & R2.y > 0.70| is.na(H_m) & R2.x < R2.y & R2.y > 0.70 ~ "coeff_sp",
                              is.na(H_m) & is.na(R2_comb) & !is.na(H_g)| is.na(H_m) & R2_comb < 0.70 & !is.na(H_g) ~ "ehk_sloboda",
                              is.na(H_m) & is.na(R2_comb) & is.na(H_g)| is.na(H_m) & R2_comb < 0.70 & is.na(H_g) ~ "h_curtis", 
                              TRUE ~ "sampled")) %>% 
  # When h_m is na but there is a plot and species wise model with R2 above 0.7, use the model to predict the height
  mutate(H_m = case_when(is.na(H_m) & !is.na(R2.x) & R2.x > 0.70 | is.na(H_m) & R2.x > R2.y & R2.x > 0.7 ~ h_nls_SP_P(SP_P_ID, DBH_cm),
                         # if H_m is na and there is an R2 from coeff_SP_P thats bigger then 0.75 or of theres no R2 from 
                         # coeff_SP_plot that´s bigger then R2 of coeff_SP_P while the given R2 from coeff_SP_P is above 
                         # 0.75 then use the SP_P models
                         is.na(H_m) & is.na(R2.x) & R2.y > 0.70 | is.na(H_m) & R2.x < R2.y & R2.y > 0.70 ~ h_nls_SP(SP_code, DBH_cm),
                         # when there´s still no model per species or plot, or the R2 of both self-made models is below 0.7 
                         # and hm is na but there is a h_g and d_G
                         is.na(H_m) & is.na(R2_comb) & !is.na(H_g)| is.na(H_m) & R2_comb < 0.70 & !is.na(H_g) ~ ehk_sloboda(H_SP_group, DBH_cm*10, mean_DBH_mm, D_g, H_g),
                         # when there´s still no model per species or plot, or the R2 of both self-made models is below 0.7 
                         # and hm is na and the Slobody function cannot eb applied because there is no h_g calculatable use the curtis function
                         is.na(H_m) & is.na(R2_comb) & is.na(H_g)| is.na(H_m) & R2_comb < 0.70 & is.na(H_g) ~ h_curtis(H_SP_group, DBH_cm*10), 
                         TRUE ~ H_m))

# ---- 1.1.2.6. exporting height coefficient dataset --------------------------

write.csv(coeff_H_comb, "output/out_data/coeff_H_BZE.csv")


=======
# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the peat land soil inventory
# sorting trees according to tree 
# ----- 0. SETUP ---------------------------------------------------------------

# ----- 0.1. packages and functions --------------------------------------------


source(paste0(getwd(), "/scripts/functions_library.R"))


# ----- 0.2. working directory -------------------------------------------------
here::here()
getwd()

out.path.BZE3 <- ("output/out_data/out_data_BZE/") 

# ----- 0.3 data import --------------------------------------------------------
# LIVING TREES
# BZE3 BE dataset: this dataset contains the inventory data of the tree inventory accompanying the third national soil inventory
HBI_trees <- read.delim(file = here("data/input/BZE2_HBI/beab.csv"), sep = ",", dec = ",", stringsAsFactors=FALSE)
# stand info od individual trees 
stand_info_HBI_trees <-  read.delim(file = here("output/out_data/out_data_BZE/all_trees_status.csv"), sep = ",", dec = ",", stringsAsFactors=FALSE)
plot_areas_HBI <- read.delim(file = here("output/out_data/out_data_BZE/all_edges_rem_circles_area.csv"), sep = ",", dec = ",", stringsAsFactors=FALSE)
  

# species names & codes 
SP_names_com_ID_tapeS <- read.delim(file = here("output/out_data/x_bart_tapeS.csv"), sep = ",", dec = ",") 



# ----- 0.6 harmonising column names & structure  -----------------------------------------------------------------
# HBI 
colnames(HBI_trees) <- c("multi_stem", "D_mm", "DBH_class", "DBH_h_cm", "H_dm",
                         "azi_gon", "SP_code", "tree_ID", "plot_ID", "tree_inventory_status", 
                         "DBH_cm", "age", "C_layer", "C_h_dm", "Kraft", "Dist_cm", "age_meth")  
HBI_trees <- HBI_trees %>% 
  select(plot_ID,  tree_ID,  tree_inventory_status,  multi_stem, Dist_cm,  azi_gon, age, age_meth,  
         SP_code, DBH_class,  Kraft, C_layer, H_dm,  C_h_dm, D_mm,   DBH_h_cm,  DBH_cm ) 
 
# harmonize strings of plot_area_HBI  
# https://stackoverflow.com/questions/20637360/convert-all-data-frame-character-columns-to-factors
plot_areas_HBI[,c(1,2, 3, 4)] <- lapply(plot_areas_HBI[,c(1,2, 3, 4)], as.numeric)



# 1. joining in external info  -------------------------------------------------

HBI_trees <- HBI_trees %>%
# add inventory info -----------------------------------------------------
  mutate(inv = "HBI",
         inv_year = 2012) %>% 
# join in species codes --------------------------------------------------
  left_join(., SP_names_com_ID_tapeS %>% 
              mutate(char_code_ger_lowcase = tolower(Chr_code_ger)), 
            by = c("SP_code" = "char_code_ger_lowcase")) %>% 
# calcualte diameter and change units -----------------------------------
  mutate(DBH_h_cm = ifelse(is.na(DBH_h_cm), 130, DBH_h_cm),        # assign DBH measuring height of 130cm when missing
         DBH_h_m = DBH_h_cm/100) %>%                               # change unit of DBH measuring height from cm into m by dividing by 100  
  #  apply regression from BWI
  mutate(DBH_cm = ifelse(DBH_h_cm == 130, D_mm/10, DBH_BWI(D_mm, DBH_h_cm)),
         DBH_class = ifelse(is.na(DBH_class), DBH_c_function(DBH_cm), DBH_class), 
         BA_m2 = c_A(DBH_cm/2)*0.0001) %>% 
# join in tree stand info ---------------------------------------------------------
  left_join(., stand_info_HBI_trees %>% 
              select(plot_ID, tree_ID, t_stat), 
            by = c("plot_ID", "tree_ID"), 
            multiple = "all") %>% 
  rename(stand = t_stat) %>% 
  filter(stand != "warning") %>% 
# join in plot area depednign on diameter of tree and stand of tree ---------------
  # asssing corect samling circle diameter according to DBH of the tree
  mutate(CCS_r_m = case_when(DBH_cm >= 7  & DBH_cm < 10 ~ 5.64, 
                             DBH_cm >= 10 & DBH_cm < 30 ~ 12.62, 
                             DBH_cm >= 30 ~ 17.84, 
                             TRUE ~ NA)) %>% 
  left_join(., plot_areas_HBI,
            by = c("plot_ID", "CCS_r_m", "stand")) %>% 
  mutate(area_m2 = ifelse(stand == "A" & is.na(e_ID) & is.na(area_m2), c_A(CCS_r_m), area_m2), 
         plot_A_ha = as.numeric(area_m2)/10000)



# check if there are no trees left that don´t have a SP_code in xBart/ SP_names_com_ID_tapeS
HBI_trees %>% 
  anti_join(SP_names_com_ID_tapeS %>% 
              mutate(char_code_ger_lowcase = tolower(Chr_code_ger)), 
            by = c("SP_code" = "char_code_ger_lowcase"))

# BZE3_trees <- BZE3_trees %>% 
#   mutate(inventory = "HBI") %>% 
#   left_join(., SP_names_com_ID_tapeS %>% 
#               mutate(char_code_ger_lowcase = tolower(Chr_code_ger)), 
#             by = c("SP_code" = "char_code_ger_lowcase"))
# 
# 
# # check if there are no trees left that don´t have a SP_code in xBart/ SP_names_com_ID_tapeS
# BZE3_trees %>% 
#   anti_join(., SP_names_com_ID_tapeS %>% 
#               mutate(char_code_ger_lowcase = tolower(Chr_code_ger)), 
#             by = c("SP_code" = "char_code_ger_lowcase"))



# 2. estimating tree height -----------------------------------------------

      


# ----- 1.1.2.3. non linear height model h ~ DBH per species and plot ----------------------------
# to calculate individual tree heights for trees of the samme species and plot 
# where the height has not been sampled we create a non-linear regression for the heights
# in the following a dataframe with regression coefficients per 
# species per plot is created if there are more then 3 heights measured per species and plot

# coefficents of non-linear height model per species and plot
# https://rdrr.io/cran/forestmangr/f/vignettes/eq_group_fit_en.Rmd
coeff_H_SP_P <- left_join(
  # select variables needed for modeling 
  trees_total %>% select(plot_ID, SP_code, H_m, DBH_cm, DBH_class) %>% 
    # filter for measured heights that also the necessary info about the diameter and measuring height of diameter
    filter(!is.na(H_m) & !is.na(DBH_cm) & !is.na(DBH_class)) %>% 
    group_by(plot_ID, SP_code) %>%
    # filter for plots that have at least 3 heights measured per species
    filter(n() >= 3),
  # creaing & joining in coeff_H_SP_P dataset 
  trees_total %>% 
    select(plot_ID, SP_code, H_m, DBH_cm, DBH_class) %>% 
    filter(!is.na(H_m) & !is.na(DBH_cm) & !is.na(DBH_class) ) %>% 
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


# ----- 1.1.2.3. non linear height model h ~ DBH per species over all plots ----------------------------

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

# ----- 1.1.2.4. combined coefficients of height models ---------------------
coeff_H_comb <- rbind(coeff_H_SP_P %>% mutate(plot_ID = as.factor(plot_ID)), coeff_H_SP)

# ---- 1.1.2.5. joining coordinates into trees datset --------------------------
# sampling circuits: 
# 5.64 7cm <= 10cm
#12.62 >10cm <= 30
# 17.84 >= 30

trees_total <-     # this should actually be the BZE3 Datset 
  trees_total %>% 
  unite(SP_P_ID, plot_ID, SP_code, sep = "", remove = FALSE) %>%            # create column matching vectorised coefficients of coeff_SP_P (1.3. functions, h_nls_SP_P, dplyr::pull)
  left_join(.,coeff_H_SP_P %>%                                              # joining R2 from coeff_SP_P -> R2.x
              select(plot_ID, SP_code, R2) %>% 
              unite(SP_P_ID, plot_ID, SP_code, sep = "", remove = FALSE),   # create column matching vectorised coefficients of coeff_SP_P (1.3. functions, h_nls_SP_P, dplyr::pull)
            by = c("plot_ID", "SP_code", "SP_P_ID")) %>% 
  left_join(., coeff_H_SP %>% select(SP_code, R2),               # joing R2 from coeff_SP data set -> R2.y
            by = "SP_code") %>% 
  # this is creates a tree dataset with mean BHD, d_g, h_g per species per plot per canopy layer which we need for SLOBODA 
  left_join(., trees_total %>%      # this should be the BZE3 Dataset                           
              group_by(plot_ID, C_layer, SP_code, CC_no) %>%    # group by plot and species, canopy layer and sampling circuit to calcualte all paremeters needed 
              summarise(BA_CC = sum(BA_m2),                        # sum up basal  area per sampling circuit to then reffer it to the hektar value of the respective circuit
                        CC_A_ha = mean(CC_A_ha),                   # mean area in ha per sampling circuit
                        BA_CC_m2_ha = BA_CC/CC_A_ha,               # calculating the BA hectare value of each tree species per c layer to account for the different sampling circuits
                        mean_DBH_mm_CC = mean(DBH_cm*10),          # calculate mean DBH per sampling circuit and species and C layer and plot 
                        mean_H_m_CC = mean(na.omit(H_m))) %>%      # calculate mean height per sampling circuit and species and C layer and plot    
              group_by(plot_ID, C_layer, SP_code)%>%            # group by plot and species,  canopy layer and sampling circuit to calcualte dg, hg 
              summarize(BA_m2_ha = sum(BA_CC_m2_ha),               # calculate sum of BA across all sampling circuit to account for represnation of different trees in the sampling circuits
                        mean_DBH_mm = mean(mean_DBH_mm_CC),        # calculate mean of DBH across all sampling circuit to account for represnation of different trees in the sampling circuits
                        mean_H_m = mean(mean_H_m_CC),              # calculate mean of height across all sampling circuit to account for represnation of different trees in the sampling circuits
                        H_g = sum(mean(na.omit(mean_H_m))*BA_m2_ha)/sum(BA_m2_ha),    # Hoehe des Grundflächemittelstammes, calculation according to S. Schnell
                        mean_DBH_mm = mean(mean_DBH_mm),                           # mean diameter per species per canopy layer per plot
                        D_g = ((sqrt((mean(BA_m2_ha)/pi)))*2)*100),              #  Durchmesser des Grundflächenmittelstammes; *100 to get from 1m -> 100cm    
            by = c("plot_ID", "SP_code", "C_layer")) %>% 
  mutate(R2_comb = f(R2.x, R2.y, R2.y, R2.x),                               # if R2 is na, put R2 from coeff_SP_P unless R2 from coeff_SP is higher
         H_method = case_when(is.na(H_m) & !is.na(R2.x) & R2.x > 0.70 | is.na(H_m) & R2.x > R2.y & R2.x > 0.7 ~ "coeff_SP_P", 
                              is.na(H_m) & is.na(R2.x) & R2.y > 0.70| is.na(H_m) & R2.x < R2.y & R2.y > 0.70 ~ "coeff_sp",
                              is.na(H_m) & is.na(R2_comb) & !is.na(H_g)| is.na(H_m) & R2_comb < 0.70 & !is.na(H_g) ~ "ehk_sloboda",
                              is.na(H_m) & is.na(R2_comb) & is.na(H_g)| is.na(H_m) & R2_comb < 0.70 & is.na(H_g) ~ "h_curtis", 
                              TRUE ~ "sampled")) %>% 
  # When h_m is na but there is a plot and species wise model with R2 above 0.7, use the model to predict the height
  mutate(H_m = case_when(is.na(H_m) & !is.na(R2.x) & R2.x > 0.70 | is.na(H_m) & R2.x > R2.y & R2.x > 0.7 ~ h_nls_SP_P(SP_P_ID, DBH_cm),
                         # if H_m is na and there is an R2 from coeff_SP_P thats bigger then 0.75 or of theres no R2 from 
                         # coeff_SP_plot that´s bigger then R2 of coeff_SP_P while the given R2 from coeff_SP_P is above 
                         # 0.75 then use the SP_P models
                         is.na(H_m) & is.na(R2.x) & R2.y > 0.70 | is.na(H_m) & R2.x < R2.y & R2.y > 0.70 ~ h_nls_SP(SP_code, DBH_cm),
                         # when there´s still no model per species or plot, or the R2 of both self-made models is below 0.7 
                         # and hm is na but there is a h_g and d_G
                         is.na(H_m) & is.na(R2_comb) & !is.na(H_g)| is.na(H_m) & R2_comb < 0.70 & !is.na(H_g) ~ ehk_sloboda(H_SP_group, DBH_cm*10, mean_DBH_mm, D_g, H_g),
                         # when there´s still no model per species or plot, or the R2 of both self-made models is below 0.7 
                         # and hm is na and the Slobody function cannot eb applied because there is no h_g calculatable use the curtis function
                         is.na(H_m) & is.na(R2_comb) & is.na(H_g)| is.na(H_m) & R2_comb < 0.70 & is.na(H_g) ~ h_curtis(H_SP_group, DBH_cm*10), 
                         TRUE ~ H_m))

# ---- 1.1.2.6. exporting height coefficient dataset --------------------------

write.csv(coeff_H_comb, "output/out_data/coeff_H_BZE.csv")


>>>>>>> 7f2671c053e538979709e8d0d68770753c50f515
