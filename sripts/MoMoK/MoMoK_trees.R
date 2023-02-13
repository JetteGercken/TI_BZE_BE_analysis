# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the peat land soil inventory
# Trees 


# ----- 0. SETUP ---------------------------------------------------------------
# ----- 0.1. Packages  ---------------------------------------------------------
# install.packages("usethis")
# install.packages("tidyverse")
# install.packages("ggplot2")
# install.packages("here")
# install.packages("readr")
# install.packages("tibble")
# install.packages("dplyr")
# install.packages("stargazer")
# install.packages("corrplot")
# install.packages("AICcmodavg")


# ----- 0.2. library   ---------------------------------------------------------
library("usethis")
library("tidyverse")
library("ggplot2")
library("here")
library("readr")
library("tibble")
library("dplyr")
library("stargazer")
library("corrplot")
library("AICcmodavg")

# ----- 0.3. working directory -------------------------------------------------
here::here()
getwd()

# ----- 1. DATA ----------------------------------------------------------------
# ----- 1.1. import ------------------------------------------------------------
# as the CSVs come from excel with German settings, the delimiter is ';' and the decimals are separated by ','
# which is why I use "delim" to import the data: https://biostats-r.github.io/biostats/workingInR/005_Importing_Data_in_R.html

trees_total <- read_delim(file = here("data/input/trees_MoMoK_total.csv")) %>% 
  select(-Bemerkung)
colnames(trees_total) <- c("MoMoK_nr", "location_name", "state", "date", "CCS_nr", 
                            "t_ID", "st_ID", "pieces", "SP_nr", "SP_code", "C_layer", 
                            "Kraft", "age", "age_m", "DBH_mm", "DBH_h_cm", 
                            "DBH_p_mm", "DBH_class", "H_dm", "CH_dm", 
                            "azimut_g", "azimut_d", "dist_m")
trees_total$C_layer <- as.numeric(trees_total$C_layer)
trees_total$Kraft <- as.numeric(trees_total$Kraft)
trees_total$SP_code <- as.factor(trees_total$SP_code)



# ----- 2. linear regression height ---------------------------------------

# ----- 2.1. change unit height from dm to m ------------------------------
# to calculate individual tree heights I will create a linear regression for the heights

# convert height in dm to m
# https://suzan.rbind.io/2018/02/dplyr-tutorial-2/#mutate-at-to-change-specific-columns
  # msleep %>%
  #   select(name, sleep_total:awake) %>%
  #   mutate_at(vars(contains("sleep")), ~(.*60))

# i cannot use the hanged variable names it seems 
# https://www.appsloveworld.com/r/100/32/error-attempt-to-use-zero-length-variable-name

# filter for those rows where height is measured
trees_height_total <- trees_total %>%
  filter(!is.na(H_dm)) %>% # & !is.na(`Alt`) )%>% 
  mutate(H_m = H_dm*0.1, 
         H_cm =H_dm*10, 
         DBH_cm = DBH_mm*0.1) # %>% 
 # mutate_at(.cols = `Hoehe [dm]`, ~(.*0.1))  # --> doesn´t work


# ----- 2.2. create one height model for all species ---------------------------
# ----- 2.2.2. create training and testing / validation dataset ----------------
# https://stackoverflow.com/questions/17200114/how-to-split-data-into-training-testing-sets-using-sample-function
## set sample size to 50% of the dataset --> split data in half
smp_size <- floor(0.75 * nrow(trees_height_total))
## set the seed to make your partition reproducible
set.seed(123) 
train_ind <- sample(seq_len(nrow(trees_height_total)), size = smp_size)
# create training and testin/ validation dataset
h_train <- trees_height_total[train_ind, ]
h_test <- trees_height_total[-train_ind, ]


# ----- 2.2.3. check out potential explainatory variables  ---------------------
pairs(h_train %>% dplyr::select(H_m, SP_code, SP_nr, C_layer, Kraft, 
                                   DBH_class, age,
                                   CH_dm, DBH_cm))

# potetial explainatory variables: 
#   - SP_code
#   - DBH_class
#   - age    --> including age did not work because of to little variability and little data availability
#   - Kraft
#   - DBH_cm

# ----- 2.2.4 build model with train data --------------------------------------
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


# ----- 2.2.5 validate model with test data --------------------------------------
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

  
# calcuate estimated heights via model h.tot.3: 
  # assign trees species as variables
GKI <- trees_height_total$SP_code[trees_height_total$SP_code == "GKI"]
RER <- trees_height_total$SP_code[trees_height_total$SP_code == "RER"]
STK <- trees_height_total$SP_code[trees_height_total$SP_code == "STK"]
GFI <- trees_height_total$SP_code[trees_height_total$SP_code == "GFI"]
MBI <- trees_height_total$SP_code[trees_height_total$SP_code == "MBI"]
BKI <- trees_height_total$SP_code[trees_height_total$SP_code == "BKI"]
 
  trees_height_total <- trees_height_total %>% 
  mutate(H_m_est = 1.1088 + DBH_class*2.6664+
            as.numeric(SP_code)) # I know that the numeric doesn´t make sense
           #as.numeric(GKI)*1.2593 + as.numeric(RER)*5.9254 + as.numeric(STK)*2.5248 + as.numeric(GFI)*4.4935 + as.numeric(MBI)*6.7342 + as.numeric(BKI))
  # actually I would have to write it like this: https://advstats.psychstat.org/book/mregression/catpredictor.php
           #as.numeric(GKI)*1.2593 + as.numeric(RER)*5.9254 + as.numeric(STK)*2.5248 + as.numeric(GFI)*4.4935 + as.numeric(MBI)*6.7342)
           #GKI*1.2593 + RER*5.9254 + STK*2.5248 + GFI*4.4935 + MBI*6.7342)

ggplot(data=trees_height_total, aes(x = H_m_est, y = H_m))+
  geom_point()+
  stat_smooth(method="lm",se=TRUE)+
  xlab("height [m] predicted by model h.tot.3") +
  ylab("height [m] measured")+
  ggtitle("all species: measured height [m] vs. height [m] predicted by model h.tot.3")+
  theme_light()+
  theme(legend.position = "non")

t.test(trees_height_total$H_m, trees_height_total$H_m_est)



# ----- 2.3. create one linear model per species -------------------------------
# ----- 2.3.1. find out how many species are there -----------------------------
trees_total %>% group_by(SP_code) %>% 
  distinct(SP_code)
# RER   Rot Erle                      Alnus rubra
# STK   Spätblühende Traubenkirsche   Prunus serotina 
# GFI   Gemeine Fichte                Picea abies 
# MBI   Moorbirke                     Betula pubescens
# BKI   Bergkiefer                    Pinus mugo
# GKI   Gemeine Kiefer                Pinus silvatica


# ----- 2.3.2. Rot Erle Alnus rubra --------------------------------------------
# ----- 2.3.2.1. filer rows where height = NA and species = Alnus rubra --------
height_RER <- trees_height_total %>% 
  filter(!is.na(H_cm) & !is.na(Kraft) & !is.na(C_layer) & SP_code == "RER") #%>% 
  # mutate(H_m = `Hoehe [dm]`*0.1, 
  #        H_cm = `Hoehe [dm]`*10, 
  #        DBH_cm = `BHD [mm]`/10)   # add column with height in meter 1dm = 0.1m
  
# ----- 2.3.2.2. create training and testing / validation dataset --------------
# https://stackoverflow.com/questions/17200114/how-to-split-data-into-training-testing-sets-using-sample-function
## set sample size to 50% of the dataset --> split data in half
smp_size <- floor(0.75 * nrow(height_RER))
## set the seed to make your partition reproducible
set.seed(123) 
train_ind <- sample(seq_len(nrow(height_RER)), size = smp_size)
# create training and testin/ validation dataset
h_RER_train <- height_RER[train_ind, ]
h_RER_test <- height_RER[-train_ind, ]

# ----- 2.3.2.3. check out potetial explainatory variables ---------------------
pairs(height_RER %>% dplyr::select(H_dm, H_m, C_layer, Kraft, 
                                            DBH_class, 
                                            CH_dm, DBH_mm))

# DBH class seems most promissing

# ----- 2.3.2.3. create model based on diameter --------------------------------
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


# ----- 2.3.2.4. validate model based on diameter with test data----------------
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

t.test(height_RER$H_m, height_RER$H_m_est) # p-value = 2.907e-06 --> significantly different :/

ggplot(data=height_RER, aes(x = H_m_est, y = H_m))+
  geom_point()+
  stat_smooth(method="lm",se=TRUE)+
  xlab("height [m] predicted by model h_RER") +
  ylab("height [m] measured")+
  ggtitle("Alnus rubra: measured height [m] vs. height [m] predicted by model h_RER")+
  theme_light()+
  theme(legend.position = "non")






# ----- NOTES ------------------------------------------------------------------




