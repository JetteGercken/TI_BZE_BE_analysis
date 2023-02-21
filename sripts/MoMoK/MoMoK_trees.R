# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the peat land soil inventory
# Trees 


# ----- 0. SETUP ---------------------------------------------------------------
# ----- 0.1. Packages  ---------------------------------------------------------
## datamanagement
# install.packages("usethis")
# install.packages("here")
# install.packages("readr")
# install.packages("tidyverse")
# install.packages("tibble")
# install.packages("dplyr")
# install.packages("data.table")
# install.packages("broom")
# install.packages("purrr")
## laTex
# install.packages("stargazer")  #for compatability with Latex
# install.packages("tikzDevice") #for compatability with Latex
## visualisation
# install.packages("ggthemes")
# install.packages("ggplot2")
# install.packages("reshape2") #for multiple y values
# install.packages("ggforce") #for zooming in parts of the plot
# options(tz="CA")
# install.packages("reshape2")
## analysis
# install.packages("corrplot")
# install.packages("AICcmodavg")
# # forest related
# install.packages("forestmangr")
# install.packages("rBDAT")

# ----- 0.2. library   ---------------------------------------------------------
# datamanagement
library("usethis")
library("here")
library("readr")
library("tidyverse")
library("tibble")
library("dplyr")
library("data.table")
library("broom")
library("purrr")
# laTex
library("stargazer")  #for compatability with Latex
library("tikzDevice") #for compatability with Latex
# visualisation
library("ggthemes")
library("ggplot2")
library("reshape2") #for multiple y values
library("ggforce") #for zooming in parts of the plot
options(tz="CA")
library("reshape2")
# analysis
library("corrplot")
library("AICcmodavg")
# forest related
library("forestmangr")
library("rBDAT")

# ----- 0.3. working directory -------------------------------------------------
here::here()
getwd()

# ----- 1. DATA ----------------------------------------------------------------
# ----- 1.1. import ------------------------------------------------------------
# as the CSVs come from excel with German settings, the delimiter is ';' and the decimals are separated by ','
# which is why I use "delim" to import the data: https://biostats-r.github.io/biostats/workingInR/005_Importing_Data_in_R.html
trees_total <- read.delim(file = here("data/input/trees_MoMoK_total.csv"), sep = ";", dec = ",") %>% 
  select(-Bemerkung)
# ----- 1.2. colnames, vector type ---------------------------------------------
colnames(trees_total) <- c("plot_ID", "loc_name", "state", "date", "CCS_nr", 
                            "t_ID", "st_ID", "pieces", "SP_nr", "SP_code", "C_layer", 
                            "Kraft", "age", "age_m", "DBH_mm", "DBH_h_cm", 
                            "DBH_p_mm", "DBH_class", "H_dm", "CH_dm", 
                            "azimut_g", "azimut_d", "dist_m")
trees_total$C_layer <- as.numeric(trees_total$C_layer)
#trees_total$Kraft <- as.numeric(trees_total$Kraft)
trees_total$SP_code <- as.factor(trees_total$SP_code)

# ----- 1.3. dealing with NAs ---------------------------------------------------
# check for variabels with NAs
summary(trees_total)

# ----- 1.3.1 assign DBH class to trees where DBH_class == 'NA' -----------------
# find min./ max to set create label 
summary(trees_total$DBH_mm)
#    Min. 1st Qu.  Median    Mean  3rd Qu.    Max. 
#.  71.0   107.0   164.0   299.3   234.8    5445.0 --> that´s 544.5cm so 5.445m --> TYPO? 

# create label for diameter classes according to BZE3 Bestandesaufnahmeanleitung
labs <- c(seq(5, 550, by = 5)) 
# replace missing DBH_class values with labels according to DBH_cm
trees_total <- trees_total%>%
  # change unit of height and diameter
  mutate(H_m = H_dm*0.1,#transform height in dm into height in m 
         DBH_cm = DBH_mm*0.1) %>%  # transform DBH in mm into DBH in cm 
  mutate(DBH_class = ifelse(is.na(DBH_class),  # mutate the column DBH_class if DBH_class is NA
                            cut(DBH_cm,        # cut the diameter
                                breaks = c(seq(5, 550, by = 5), Inf),  # in sequences of 5
                                labels = labs,                         # and label it according to labs
                                right = FALSE),
                            as.numeric(DBH_class)))  # else keep existing DBH class as numeric

# ----- 2. CALCULATIONS --------------------------------------------------------
# ----- 3. linear regression for missing tree heights --------------------------
# ----- 3.1. get coefficents per SP and plot when >= 3 heights measured --------
# to calculate individual tree heights for trees of the samme species and plot 
# where the height has not been sampled I will create a linear regression for the heights
# in the following i will create a dataframe with regression coefficients per 
# species per plot if there are more then 3 heights measured per species and plot

#forestmanager package
# https://search.r-project.org/CRAN/refmans/forestmangr/html/lm_table.html
coeff_heights <-  trees_total %>% 
   select(plot_ID, SP_code, H_m, DBH_cm, DBH_class) %>% 
   filter(!is.na(H_m) & !is.na(DBH_cm)) %>% 
   group_by(plot_ID, SP_code) %>% 
  # filter for plots where there is at least 3 heights measured for each species
  #https://stackoverflow.com/questions/20204257/subset-data-frame-based-on-number-of-rows-per-group
   filter(n() >= 3)%>%    
   #group_by(plot_ID, SP_code) %>% 
   lm_table(H_m ~ DBH_cm) %>%      # the lm models the height based on the diamater at breast heigt 
   arrange(plot_ID, SP_code) 

# coefficents of non-linear height model 
# https://rdrr.io/cran/forestmangr/f/vignettes/eq_group_fit_en.Rmd
coeff_heights_nls <-  trees_total %>% 
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
  arrange(plot_ID, SP_code)

# adding bias, rmse and rsqrd to the coefficent dataframe
coeff_heights_nls <- left_join(trees_total %>% 
  select(plot_ID, SP_code, H_m, DBH_cm, DBH_class) %>% 
  filter(!is.na(H_m) & !is.na(DBH_cm) & !is.na(DBH_class)) %>% 
  group_by(plot_ID, SP_code) %>% 
  filter(n() >= 3),coeff_heights_nls, by = c("plot_ID", "SP_code"))%>%
  mutate(H_est = b0 * (1 - exp( -b1 * DBH_cm))^b2) %>% 
  group_by(plot_ID, SP_code) %>% 
  summarise( b0 = mean(b0), 
             b1 = mean(b1), 
             b2 = mean(b2), 
#https://rdrr.io/cran/forestmangr/f/vignettes/eq_group_fit_en.Rmd
             bias = bias_per(y = H_m, yhat = H_est),
             rsme = rmse_per(y = H_m, yhat = H_est),
#https://stackoverflow.com/questions/14530770/calculating-r2-for-a-nonlinear-least-squares-fit
             rsqrd = max(cor(H_m, H_est),0)^2,
#https://stats.stackexchange.com/questions/11676/pseudo-r-squared-formula-for-glms
             mean_h = mean(H_m), 
             N = length(H_m), 
             SSres = sum((H_m-H_est)^2), 
             SStot = sum((H_m-mean_h)^2), 
             pseu_R2 = 1-(SSres/SStot), 
             diff_h = mean(H_m - H_est))

# nls: plot estimated heights against dbh 
ggplot(data = (left_join(trees_total %>% 
            select(plot_ID, SP_code, H_m, DBH_cm, DBH_class) %>% 
            filter(!is.na(H_m) & !is.na(DBH_cm)) %>% 
            group_by(plot_ID, SP_code) %>% 
              #filter(DBH_cm <= 150) %>% 
            filter(n() >= 3),coeff_heights_nls, by = c("plot_ID", "SP_code"))%>%
  mutate(H_est = b0 * (1 - exp( -b1 * DBH_cm))^b2)), 
  aes(x = DBH_cm, y = H_est, color = SP_code))+
  geom_point()+
  geom_smooth(method = "loess", se=TRUE)+
  facet_wrap(SP_code~plot_ID)+
  xlab("diameter ") +
  ylab("estimated height [m]")+
  #ylim(0, 50)+
  ggtitle("height estimated vs. diameter")+
  theme_light()+
  theme(legend.position = "non")

# nls: plot sampled heights against DBH
ggplot(data = trees_total %>% 
                           select(plot_ID, SP_code, H_m, DBH_cm, DBH_class) %>% 
                           filter(!is.na(H_m) & !is.na(DBH_cm)) %>% 
                           group_by(plot_ID, SP_code) %>% 
                           filter(n() >= 3),
       aes(x = DBH_cm, y = H_m, color = SP_code))+
  geom_point()+
  geom_smooth(method = "loess", se=TRUE)+
  facet_wrap(SP_code~plot_ID)+
  xlab("diameter ") +
  ylab("sampled height [m]")+
  ylim(0, 50)+
  ggtitle("height sampled vs. diameter")+
  theme_light()+
  theme(legend.position = "bottom")

# nls: plot estimated vs. sampled heights 
ggplot(data = (left_join(trees_total %>% 
                           select(plot_ID, SP_code, H_m, DBH_cm, DBH_class) %>% 
                           filter(!is.na(H_m) & !is.na(DBH_cm)) %>% 
                           group_by(plot_ID, SP_code) %>% 
                           filter(n() >= 3),coeff_heights_nls, by = c("plot_ID", "SP_code"))%>%
                 mutate(H_est = b0 * (1 - exp( -b1 * DBH_cm))^b2)), 
       aes(x = H_m, y = H_est, color = SP_code))+
  geom_point()+
  geom_smooth(method = "lm", se=TRUE)+
  facet_wrap(plot_ID ~ SP_code)+
  xlab("sampled height [m]") +
  ylab("estimated height nls model")+
  ggtitle("height estimated via nls vs. sampled height")+
  theme_light()+
  theme(legend.position = "non")



# find the plots and species that won´t have a height regression model
# ---> think about way to deal with them !!!!
trees_total %>% 
  select(plot_ID, SP_code, H_dm, DBH_mm, Kraft) %>% 
  filter(!is.na(H_dm) & !is.na(DBH_mm)) %>% 
  group_by(plot_ID, SP_code) %>% 
  filter(n() <= 3)%>%    # filter for plots where there are less then 3 heights measured for each species
  #group_by(plot_ID, SP_code) %>% 
  #lm_table(H_m ~ DBH_cm) %>% 
  arrange(plot_ID, SP_code)


# find the plots/ trees with particularly high diameters --> measurement/ assemssment errors
view(trees_total %>% filter(DBH_mm >= 1000) %>% 
            select(plot_ID, loc_name, state, date, CCS_nr, t_ID,
                   SP_code, DBH_mm, DBH_cm, H_m, H_dm))

# ----- 3.2. analysing the quality of the models  ------------------------------
 # from my previous attempts to fit and validate species specific but also total 
 # dataset including regression models for the height, I know that actually the 
 # DBH class is the better predictor 
 # but on the other hand the diameter class is also less precise
summary(coeff_heights)
# the R2 is pretty poor for some plots 
coeff_heights %>% filter(Rsqr <= 0.3)
view(trees_total %>% filter(plot_ID == 32080))


summary(coeff_heights_nls)
coeff_heights_nls %>% filter(diff_h >= 0.75)
 
# ----- 3.3. join coefficients to the main dataset  ----------------------------
trees_total <- left_join(trees_total, coeff_heights %>%
                           select(plot_ID, SP_code, b0, b1), by = c("plot_ID", "SP_code")) %>% 
  mutate(H_m = ifelse(is.na(H_m),
                      (b0 + b1*as.numeric(DBH_cm)),
                      as.numeric(H_m)))

trees_total %>% filter(is.na(H_m))


# ----- 3.4. estimate missing heights  -----------------------------------------


















# ----- NOTES ------------------------------------------------------------------
# ----- N.1. Notes regarding linear regression of height for total dataset -----
# ----- N.1.0. adding missing DBH classes  -------------------------------------
# https://stackoverflow.com/questions/32683599/r-ifelse-to-replace-values-in-a-column
trees_total$DBH_class <- ifelse(is.na(trees_total$DBH_class),  # if DBH_class is NA
                                cut(trees_total$DBH_cm,                    # cut the diameter 
                                    breaks = c(seq(5, 550, by = 5), Inf),  # in sequences of 5 
                                    labels = labs,                         # and label it according to labs
                                    right = FALSE),
                                as.numeric(trees_total$DBH_class)) # else keep DBH class


# https://stackoverflow.com/questions/19379081/how-to-replace-na-values-in-a-table-for-selected-columns
trees_total_3 <- trees_total  %>%
  mutate(H_m = H_dm*0.1, 
         H_cm =H_dm*10, 
         DBH_cm = DBH_mm*0.1)  %>%
  mutate(DBH_class, ~replace_na(., cut(DBH_cm,
                                       breaks = c(seq(0, 545, by = 5), Inf),
                                       labels = labs,
                                       right = FALSE)))
# ----- N.1.1. units & filter dataset ------------------------------------------
# convert height in dm to m via mutate at 
# --> doesn´t work # https://suzan.rbind.io/2018/02/dplyr-tutorial-2/#mutate-at-to-change-specific-columns
# msleep %>%
#   select(name, sleep_total:awake) %>%
#   mutate_at(vars(contains("sleep")), ~(.*60))

trees_height_total <- trees_total %>%
  filter(!is.na(H_dm) & !is.na(DBH_mm)) %>% # & !is.na(`Alt`) )%>%  # filter for those rows where height is measured
  mutate(H_m = H_dm*0.1,                    # convert height in dm to m
         H_cm =H_dm*10, 
         DBH_cm = DBH_mm*0.1)# %>% 
#group_by(plot_ID, SP_code)# %>% 
# mutate_at(.cols = `Hoehe [dm]`, ~(.*0.1))  

# ----- N.1.2. attempts to extract regression coefficents form whole dataset grouped by SP and plot ID ------------------------------------------
# filter dataframe for heights for >= 3 height measurements per SP_code and plot_ID
min3h_plot_SP <- trees_total %>% 
  dplyr::select(plot_ID, 
                SP_code, SP_nr, 
                C_layer, Kraft, CH_dm,
                H_dm, DBH_mm, H_m, DBH_cm, DBH_class, 
                age) %>% 
  filter(!is.na(H_dm) & !is.na(DBH_mm)) %>% 
  group_by(plot_ID, SP_code) %>% 
  filter(n() >= 3)%>% 
  arrange(plot_ID, SP_code)

# data.table package
# https://www.tutorialspoint.com/how-to-perform-group-wise-linear-regression-for-a-data-frame-in-r
min3h_plot_SP.dt <- data.table(min3h_plot_SP)
min3h_plot_SP.dt[,as.list(coef(lm(H_m ~ DBH_cm))), by= SP_code]

# broom package
# https://stackoverflow.com/questions/1169539/linear-regression-and-group-by-in-r
min3h_plot_SP %>% 
  group_by(plot_ID, SP_code) %>% 
  do(model = lm(as.numeric(H_m) ~ as.numeric(DBH_cm), data = .)) %>% 
  rowwise() %>% 
  tidy(model)

#tidyverse package
# https://stackoverflow.com/questions/1169539/linear-regression-and-group-by-in-r
min3h_plot_SP.df <- data.frame(min3h_plot_SP)

min3h_plot_SP.df %>% 
  group_by(plot_ID, SP_code) %>% 
  nest() %>% 
  mutate(model = map(min3h_plot_SP.df, 
                     ~lm(H_m ~ DBH_cm, data = min3h_plot_SP.df)))





# ----- N.2. properly fit height model -----------------------------------------

# ----- N.2.2. height model for all species together ---------------------------
# ----- N.2.2.2. create training and testing / validation dataset --------------
# https://stackoverflow.com/questions/17200114/how-to-split-data-into-training-testing-sets-using-sample-function
## set sample size to 50% of the dataset --> split data in half
smp_size <- floor(0.75 * nrow(min3h_plot_SP))
## set the seed to make your partition reproducible
set.seed(123) 
train_ind <- sample(seq_len(nrow(min3h_plot_SP)), size = smp_size)
# create training and testin/ validation dataset
h_train <- min3h_plot_SP[train_ind, ]
h_test <- min3h_plot_SP[-train_ind, ]

# ----- N.2.2.3. check out potential explainatory variables  ---------------------
pairs(h_train %>% dplyr::select(H_m, as.numeric(SP_code), as.numeric(SP_nr), C_layer, Kraft, 
                                DBH_class,
                                CH_dm, DBH_cm))

# potetial explainatory variables: 
#   - SP_code
#   - DBH_class
#   - age    --> including age did not work because of to little variability and little data availability
#   - Kraft
#   - DBH_cm

# ----- N.2.2.4 build model with train data --------------------------------------
# create different models with different combinations of the explainatory variables 
h.tot.1 <- lm(formula = H_m ~ DBH_class + as.factor(SP_code) + Kraft + DBH_cm, 
              data = h_train)
h.tot.2 <- lm(formula = H_m ~ DBH_class + as.factor(SP_code) + Kraft, 
              data = h_train)
h.tot.3 <- lm(formula = H_m ~ DBH_class + as.factor(SP_code), 
              data = h_train)
h.tot.4 <- lm(formula = H_m ~ DBH_class, 
              data = h_train)
h.tot.5 <- lm(formula = H_m ~ DBH_class + Kraft + DBH_cm, 
              data = h_train)
h.tot.6 <- lm(formula = H_m ~ DBH_class + DBH_cm, 
              data = h_train)
h.tot.7 <- lm(formula = H_m ~  as.factor(SP_code) + Kraft + DBH_cm, 
              data = h_train)
h.tot.8 <- lm(formula = H_m ~  as.factor(SP_code) + DBH_cm, 
              data = h_train)

# check AIC of the models: 
# https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/AIC
# https://www.statology.org/aic-in-r/
models.h.tot <- list(h.tot.1, h.tot.2, h.tot.3, h.tot.4, h.tot.5, h.tot.6, h.tot.7, h.tot.8)
AIC(h.tot.1, h.tot.2, h.tot.3, h.tot.4, h.tot.5, h.tot.6, h.tot.7, h.tot.8)
# RESULTS AIC:
# model    df  AIC
# h.tot.1  9  644.6449
# h.tot.2  8  643.2544 ---> lowest AIC --> best model
# h.tot.3  8  797.3503 ---> summary and ANOVA, however, indicate that h.tot.3 is the best model
# h.tot.4  3  841.9807
# h.tot.5  5  685.6593
# h.tot.6  4  843.2195
# h.tot.7  8  961.1056
# h.tot.8  8 1249.8632

# check summary and anova of model with best AIC
summary(h.tot.2) # --> only SP_code and DBH_class are significant
anova(h.tot.2)   # --> only SP_code and DBH_class are significant
# check summar yand anova for model h.tot.3 with only SP_code and DBH_class
summary(h.tot.3) # --> only SP_code and DBH_class are significant
anova(h.tot.3)   # --> only SP_code and DBH_class are significant
# --> h.tot.3. is slightly simpler and shows a better R2 

# diagonostic plots: 
plot(h.tot.2) # --> cooks distance looks more evenly spread
plot(h.tot.3)


# RESULTS h.tot.3:  H_m ~ DBH_class + as.factor(SP_code), data = h_train)
#                           Estimate   Std. Error t value  Pr(>|t|)    
# (Intercept)                1.1088     0.7686    1.443    0.151    
# DBH_class                  2.6664     0.1179    22.618   < 2e-16 ***
# as.factor(SP_code)GFI      4.4935     0.8060    5.575    1.03e-07 ***
# as.factor(SP_code)GKI      1.2593     2.6968    0.467    0.641    
# as.factor(SP_code)MBI      6.7342     0.9992    6.740    2.72e-10 ***
# as.factor(SP_code)RER      5.9254     0.8556    6.926    1.00e-10 ***
# as.factor(SP_code)STK      2.5248     1.9608    1.288    0.200    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.565 on 160 degrees of freedom
# (46 Beobachtungen als fehlend gelöscht)
# Multiple R-squared:  0.8063,	Adjusted R-squared:  0.799 
# F-statistic:   111 on 6 and 160 DF,  p-value: < 2.2e-16

# resuting function: 
# h_m_est = 1.1088 + DBH_class*2.6664 + GKI*1.2593 + RER*5.9254 + STK*2.5248 + GFI*4.4935 + MBI*6.7342 + BKI 

# ----- N.2.2.5 validate model with test data ------------------------------------
# ----- N.2.2.5.1. fit h.tot.3 for test data -------------------------------------
h.3.val <- lm(formula = H_m ~ DBH_class + as.factor(SP_code), data = h_test)
summary(h.3.val)
anova(h.3.val) # only DBH_class is significant
plot(h.3.val)

# Coefficients:
#                         Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             1.7339     2.3834   0.727   0.4702    
# DBH_class               2.5774     0.2566   10.045  1.11e-13 ***
# as.factor(SP_code)GFI   4.1261     2.4448   1.688   0.0976 .  
# as.factor(SP_code)MBI   5.7992     2.6626   2.178   0.0341 *  
# as.factor(SP_code)RER   5.1307     2.6426   1.942   0.0577 .  
# as.factor(SP_code)STK   2.8500     3.3264   0.857   0.3956    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 3.326 on 51 degrees of freedom
# (14 Beobachtungen als fehlend gelöscht)
# Multiple R-squared:  0.7349,	Adjusted R-squared:  0.7089 
# F-statistic: 28.28 on 5 and 51 DF,  p-value: 1.324e-13

# ----- N.2.2.5.2. estimate heights via model h.tot.3 for height dataset -------
# assign trees species as variables
GKI <- trees_height_total$SP_code[trees_height_total$SP_code == "GKI"]
RER <- trees_height_total$SP_code[trees_height_total$SP_code == "RER"]
STK <- trees_height_total$SP_code[trees_height_total$SP_code == "STK"]
GFI <- trees_height_total$SP_code[trees_height_total$SP_code == "GFI"]
MBI <- trees_height_total$SP_code[trees_height_total$SP_code == "MBI"]
BKI <- trees_height_total$SP_code[trees_height_total$SP_code == "BKI"]

# ass column with estimated tree heights 
trees_height_total <- trees_height_total %>% 
  mutate(H_m_est = 1.1088 + DBH_class*2.6664+
           as.numeric(SP_code)) # I know that the numeric doesn´t make sense
#as.numeric(GKI)*1.2593 + as.numeric(RER)*5.9254 + as.numeric(STK)*2.5248 + as.numeric(GFI)*4.4935 + as.numeric(MBI)*6.7342 + as.numeric(BKI))
# actually I would have to write it like this: https://advstats.psychstat.org/book/mregression/catpredictor.php
#as.numeric(GKI)*1.2593 + as.numeric(RER)*5.9254 + as.numeric(STK)*2.5248 + as.numeric(GFI)*4.4935 + as.numeric(MBI)*6.7342)
#GKI*1.2593 + RER*5.9254 + STK*2.5248 + GFI*4.4935 + MBI*6.7342)

# checking if the heights are signidicantly diffferent
shapiro.test(trees_height_total$H_m) # not normally distributed
wilcox.test(trees_height_total$H_m, trees_height_total$H_m_est) 
# --> means of predicted and sampled height are significantly different

# ----- N.2.2.5.3. visualize estim. vs. sampled heights ------------------------
# plot estiamted vs. mesured tree height
# scatter plot
ggplot(data=trees_height_total, aes(x = H_m_est, y = H_m))+
  geom_point()+
  stat_smooth(method="lm",se=TRUE)+
  xlab("height [m] predicted by model h.tot.3") +
  ylab("height [m] measured")+
  ggtitle("all species: measured height [m] vs. height [m] predicted by model h.tot.3")+
  theme_light()+
  theme(legend.position = "non")

# boxlot
# create dataset for boxplot
box_h_mes <- trees_height_total %>% 
  dplyr::select(H_m) %>% 
  mutate(H_meth = "sampled",
         height_m = H_m) 
box_h_est <- trees_height_total %>% 
  dplyr::select(H_m_est) %>% 
  mutate(H_meth = "estimated", 
         height_m = H_m_est)
box_h <- rbind(box_h_mes %>% select(H_meth, height_m), box_h_est%>% select(H_meth, height_m))

ggplot(box_h, aes(x = as.factor(H_meth), y = height_m))+
  geom_boxplot()+
  xlab("predicted by model h.tot.3 vs. sampled tree height") +
  ylab("height [m]")+
  ggtitle("all species: measured height [m] vs. height [m] predicted by model h.tot.3")+
  theme_light()+
  theme(legend.position = "non")



# ----- N.2.3. properly fit one linear model per species -----------------------
# ----- N.2.3.1. find out how many species are there ---------------------------
trees_total %>% group_by(SP_code) %>% 
  distinct(SP_code)
# RER   Rot Erle                      Alnus rubra
# STK   Spätblühende Traubenkirsche   Prunus serotina 
# GFI   Gemeine Fichte                Picea abies 
# MBI   Moorbirke                     Betula pubescens
# BKI   Bergkiefer                    Pinus mugo
# GKI   Gemeine Kiefer                Pinus silvatica


# ----- N.2.3.2. Rot Erle Alnus rubra --------------------------------------------
# ----- N.2.3.2.1. filer rows where height != NA and species = Alnus rubra --------
height_RER <- min3h_plot_SP %>% 
  filter(!is.na(H_m) & !is.na(Kraft) & !is.na(C_layer) & SP_code == "RER") #%>% 
# mutate(H_m = `Hoehe [dm]`*0.1, 
#        H_cm = `Hoehe [dm]`*10, 
#        DBH_cm = `BHD [mm]`/10)   # add column with height in meter 1dm = 0.1m

# ----- N.2.3.2.2. create training and testing / validation dataset --------------
# https://stackoverflow.com/questions/17200114/how-to-split-data-into-training-testing-sets-using-sample-function
## set sample size to 50% of the dataset --> split data in half
smp_size <- floor(0.75 * nrow(height_RER))
## set the seed to make your partition reproducible
set.seed(123) 
train_ind <- sample(seq_len(nrow(height_RER)), size = smp_size)
# create training and testin/ validation dataset
h_RER_train <- height_RER[train_ind, ]
h_RER_test <- height_RER[-train_ind, ]

# ----- N.2.3.2.3. check out potetial explainatory variables ---------------------
pairs(height_RER %>% dplyr::select(H_dm, H_m, #as.numeric(C_layer), 
                                   #as.numeric(Kraft), 
                                   DBH_class, 
                                   CH_dm, DBH_mm))
# --> DBH class seems most promissing

# ----- N.2.3.2.3. create model based on diameter --------------------------------
h.RER <- lm(formula = H_m ~ DBH_class, 
            data = h_RER_train)
summary(h.RER) # model summary
anova(h.RER)   # --> Pr(>F) = 0.0005993 *** ---> DBH class is significant
plot(h.RER)

# RESULTS h_RER: 
# Coefficients:
#              Estimate    Std. Error t value  Pr(>|t|)    
# (Intercept)  12.5041     1.9774     6.323    2.31e-06 ***
#   DBH_class  1.6400      0.4097     4.003    0.000599 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 409 on 22 degrees of freedom
# (45 Beobachtungen als fehlend gelöscht)
# Multiple R-squared:  0.4214,	Adjusted R-squared:  0.3951 
# F-statistic: 16.02 on 1 and 22 DF,  p-value: 0.0005993

# ----- N.2.3.2.4. validate model based on diameter with test data----------------
h.RER.val <- lm(formula = H_m ~ DBH_class, data = h_RER_test)
summary(h.RER.val)
anova(h.RER.val) # --> Pr(>F) = 0.001278 ** --> significant
plot(h.RER.val)


# Coefficients:
#              Estimate   Std. Error t value  Pr(>|t|)   
# (Intercept)  10.2311    2.0548     4.979    0.00108 **
#   DBH_class  2.0900     0.4313     4.846    0.00128 **
---
  #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  # 
  # Residual standard error: 331 on 8 degrees of freedom
  # (14 Beobachtungen als fehlend gelöscht)
  # Multiple R-squared:  0.7459,	Adjusted R-squared:  0.7141 
  # F-statistic: 23.48 on 1 and 8 DF,  p-value: 0.001278
  
  
# create column for predicted height and plot predicated and measured height against each other 
  height_RER <- height_RER %>% 
  mutate(H_m_est = (12.5041 + DBH_class*1.6400)) 

# test for differences between predicted and sampled height
t.test(height_RER$H_m, height_RER$H_m_est) # p-value = 2.907e-06 --> significantly different :/

# visualize differences between estimated and measured height
ggplot(data=height_RER, aes(x = H_m_est, y = H_m))+
  geom_point()+
  stat_smooth(method="lm",se=TRUE)+
  xlab("height [m] predicted by model h_RER") +
  ylab("height [m] measured")+
  ggtitle("Alnus rubra: measured height [m] vs. height [m] predicted by model h_RER")+
  theme_light()+
  theme(legend.position = "non")



# ----- N.2.3.3.1. fit non linear model for RER heights -----------------------------
# https://stackoverflow.com/questions/33033176/using-r-to-fit-a-sigmoidal-curve
# https://data-flair.training/blogs/r-nonlinear-regression/
# model<- nls(bone~a-b*exp(-c*age),start=list(a=120,b=110,c=0.064))

h.RER.nls <- nls(H_m~a-b*exp(-c*DBH_cm),
             start=list(a=110,b=120,c=0.1),
            data = h_RER_train)
summary(h.RER.nls) # model summary
plot(h.RER.nls)


# ----- N.2.3.2.4. validate model based on diameter with test data----------------
h.RER.val.nls <- nls(H_m~a-b*exp(-c*DBH_cm),
                     start=list(a=21.73725,b=32.54252,c=0.09471),
                     data = h_RER_test)
summary(h.RER.val.nls)
plot(h.RER.val.nls)


# create column for predicted height and plot predicated and measured height against each other 
  height_RER <- height_RER %>% 
  mutate(H_m_est_nls = 21.73725-32.54252*exp(-0.09471*DBH_cm))
            

# test for differences between predicted and sampled height
t.test(height_RER$H_m, height_RER$H_m_est_nls)

# visualize differences between estimated and measured height
ggplot(data=height_RER, aes(x = H_m_est_nls, y = H_m))+
  geom_point()+
  stat_smooth(method="lm",se=TRUE)+
  xlab("height [m] predicted by model h_RER") +
  ylab("height [m] measured")+
  ggtitle("Alnus rubra: measured height [m] vs. height [m] predicted by model h_RER")+
  theme_light()+
  theme(legend.position = "non")

ggplot(data=height_RER, aes(x = DBH_class, y = H_m_est_nls))+
  geom_point()+
  #geom_point(aes(x = DBH_cm, y = H_m))+
  stat_smooth(method = "loess", se=TRUE)+
  xlab("DBH in cm ") +
  ylab("height [m] predicted by model h_RER")+
  ggtitle("Alnus rubra: height [m] predicted by model h_RER vs. DBH")+
  theme_light()+
  theme(legend.position = "non")

ggplot(data=height_RER, aes(x = DBH_class, y = H_m))+
  geom_point()+
  #geom_point(aes(x = DBH_cm, y = H_m))+
  stat_smooth(method = "loess", se=TRUE)+
  xlab("DBH in cm ") +
  ylab("height [m] sampled ")+
  ggtitle("Alnus rubra: height [m] sampled vs. DBH")+
  theme_light()+
  theme(legend.position = "non")
