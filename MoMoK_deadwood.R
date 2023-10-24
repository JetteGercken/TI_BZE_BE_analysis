# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the peat land soil inventory
# Deadwood 


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
# ## laTex
# install.packages("stargazer")  #for compatability with Latex
# install.packages("tikzDevice") #for compatability with Latex
# # visualisation
# install.packages("ggthemes")
# install.packages("ggplot2")
# install.packages("reshape2") #for multiple y values
# install.packages("ggforce") #for zooming in parts of the plot
# options(tz="CA")
# install.packages("reshape2")
# # analysis
# install.packages("corrplot")
# install.packages("AICcmodavg")
# # forest related
# install.packages("forestmangr")
# install.packages("rBDAT")
# install.packages("TapeR")
# install.packages("pkgbuild")
# if (! require("remotes")) 
#   install.packages("remotes")
# remotes::install_gitlab("vochr/TapeS", build_vignettes = TRUE)


# ----- 0.2. library   ---------------------------------------------------------
# datamanagement
library("usethis")
library("here")
library("readr")
library("tidyverse")
library("tibble")
library("dplyr")
library("data.table")
require(data.table)
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
library("TapeR")
library("TapeS")
require(TapeS)

# ----- 0.3. working directory -------------------------------------------------
here::here()
getwd()

# ----- 1. DATA ----------------------------------------------------------------
# ----- 1.1. import ------------------------------------------------------------
DW_total <- read.delim(file = here("data/input/MoMoK/DW_MoMoK_total.csv"), sep = ";") 






