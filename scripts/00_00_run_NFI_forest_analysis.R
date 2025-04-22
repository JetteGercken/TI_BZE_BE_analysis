# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the  national soil inventory
# run this script to run all scripts for the BZE analysis


##### general info ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# general scripts are saved right in the script folder
# all particulary analysis related scripts are saved in the BZE folder

# the first set of numbers in the script name displays the order in which the scripts should be run
# second set of numbers displays the stand component the script is dealing with 
# both sets of numbers together arrange the scripts in the order they are suppossed to be run in 

# second set of numbers decoded: 
  # xx_00 --> general operations or those concerning all datasets together (LT, RG, DW)
  # xx_01 --> LT = living trees datasets
  # xx_02 --> RG = regeneration datasets
  # xx_03 --> DW = deadwood datasets

# first set of numbers decoded:
  # 00_xx --> run to run all scripts
  # 01_xx --> general: functions, scripts: forest edges
  # 02_xx --> general: species groups, scripts: inventory status
  # 03_xx --> height
  # 04_xx --> stocks
  # 05_xx --> summarising hectar values
  # 06_xx --> growth
  # 07_xx --> extracted wood biomass
  # 08_xx --> biodiversity index
  # 09_xx --> plausibility tests

# the inventory in concern is indicated by the end of the scripts name 
  # _HBI --> Harmonisierende Bestandesinventur
  # _BZE3 --> Bestandeserhebung zur Bodenzustandserhebung 3 



########################################################################
# !!!!!!!!!!!!!! ENTER YOUR CREDENTIAL HERE !!!!!!!!!!!!!!!!!!!!!!!!!!
########################################################################
 db_name <- "bze3_altdaten"
 db_server <- "134.110.100.88"
 db_port <- "5432"
 db_user <-  rstudioapi::askForPassword(prompt = "Please enter your username")
 my_db_password <- rstudioapi::askForPassword(prompt = "Please enter your password")
 
 

#### common/ general operations -------------------------------------------------------------------------------------------------------------------------------------------------------------------
# functions & packages
 # if you run the script for the first time you will have to install all the packages
 # so this part will ask you if if its necesarry to run the installations or not: 
install_necessary <- rstudioapi::showQuestion(title,  "Are you running the script for the first time?",   ok = "yes",  cancel = "no", timeout = 90)
 if(isTRUE(install_necessary) == F){
   source(paste0(getwd(), "/scripts/01_00_functions_library.R"))  # installation not necessary
  }else{
    source(paste0(getwd(), "/scripts/01_00_functions_library_install.R"))  }                   # installation necessary

# run all functions and librabries
source(paste0(getwd(), "/scripts/01_00_functions_library.R"))
# datasets import from postgres databank
source(paste0(getwd(),"/scripts/02_00_connect_R_PostgreSQL.R"))
# data wrangling & sorting for bark and fruit types for forest structural indeyx calcualtion
source(paste0(getwd(), "/scripts/03_00_bark_fruit_types_FSI.R"))
# sort species into species groups required for data sorting & analysis
source(paste0(getwd(), "/scripts/04_00_species_groups.R"))




##### inventory status of the plot and sampling circles -------------------------------------------------------------------------------------------------------------------------------------------------------
## HBI sorting by inventory status of the plot
source(paste0(getwd(), "/scripts/BZE_III/01_00_RG_LT_DW_inventory_plot_status_HBI.R"))
## BZE3 sorting by inventory status of the plot
source(paste0(getwd(), "/scripts/BZE_III/01_00_RG_LT_DW_inventory_plot_status_BZE3.R"))


##### forest edges -------------------------------------------------------------------------------------------------------------------------------------------------------
## HBI forest edges
# LT
 georef_on_off(source(paste0(getwd(), "/scripts/BZE_III/01_01_01_LT_forest_edges_HBI.R")), 
               source(paste0(getwd(), "/scripts/BZE_III/01_01_02_LT_forest_edges_georef_HBI.R"))
      ## !!!! here one can select to work with the georefferenced or not georeffrenced plots and forest edges, default is not georefferenced
               , georefference = "not_georefferenced")
# RG
 georef_on_off(source(paste0(getwd(), "/scripts/BZE_III/01_02_01_RG_forest_edges_HBI.R")), 
               source(paste0(getwd(), "/scripts/BZE_III/01_02_02_RG_forest_edges_georef_HBI.R"))
               ## !!!! here one can select to work with the georefferenced or not georeffrenced plots and forest edges, default is not georefferenced
               , georefference = "not_georefferenced")
 

## BZE forest edges
# LT
 georef_on_off(source(paste0(getwd(), "/scripts/BZE_III/01_01_01_LT_forest_edges_BZE3.R")), 
               source(paste0(getwd(), "/scripts/BZE_III/01_01_02_LT_forest_edges_georef_BZE3.R"))
     ## !!!! here one can select to work with the georefferenced or not georeffrenced plots and forest edges, default is not georefferenced
               , georefference = "not_georefferenced")
 # RG
 georef_on_off(source(paste0(getwd(), "/scripts/BZE_III/01_02_01_RG_forest_edges_BZE3.R")), 
               source(paste0(getwd(), "/scripts/BZE_III/01_02_02_RG_forest_edges_georef_BZE3.R"))
               ## !!!! here one can select to work with the georefferenced or not georeffrenced plots and forest edges, default is not georefferenced
               , georefference = "not_georefferenced")
 



##### tree inventory status ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- 
source(paste0(getwd(), "/scripts/BZE_III/02_01_LT_invetory_status_HBI_BZE3.R"))



##### tree heights ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- 
## HBI & BZE3 together: LT tree height
source(paste0(getwd(), "/scripts/BZE_III/03_01_LT_heights_HBI_BZE3.R"))



##### stocks ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## HBI stocks
# LT
source(paste0(getwd(), "/scripts/BZE_III/04_01_LT_stocks_HBI.R"))
# RG
source(paste0(getwd(), "/scripts/BZE_III/04_02_RG_stocks_HBI.R"))
# DW
source(paste0(getwd(), "/scripts/BZE_III/04_03_DW_stocks_HBI.R"))


## BZE3 stocks
# LT
source(paste0(getwd(), "/scripts/BZE_III/04_01_LT_stocks_BZE3.R"))
# RG
source(paste0(getwd(), "/scripts/BZE_III/04_02_RG_stocks_BZE3.R"))
# DW
source(paste0(getwd(), "/scripts/BZE_III/04_03_DW_stocks_BZE3.R"))



##### summarising hectar values ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## HBI summarising hectar values
# living trees
source(paste0(getwd(), "/scripts/BZE_III/05_01_LT_summarising_hectar_values_HBI.R"))
# regeneration trees
source(paste0(getwd(), "/scripts/BZE_III/05_02_RG_summarising_hectar_values_HBI.R"))
# deadwood
source(paste0(getwd(), "/scripts/BZE_III/05_03_DW_summarising_hectar_values_HBI.R"))
# all stand components
source(paste0(getwd(), "/scripts/BZE_III/05_04_LT_RG_DW_summarising_hectar_values_HBI.R"))

## BZE3 summarising hectar values
# living trees
source(paste0(getwd(), "/scripts/BZE_III/05_01_LT_summarising_hectar_values_BZE3.R"))
# regeneration
source(paste0(getwd(), "/scripts/BZE_III/05_02_RG_summarising_hectar_values_BZE3.R"))
# deadwood
source(paste0(getwd(), "/scripts/BZE_III/05_03_DW_summarising_hectar_values_BZE3.R"))
# all stand components 
source(paste0(getwd(), "/scripts/BZE_III/05_04_LT_RG_DW_summarising_hectar_values_BZE3.R"))



##### biodiversity: forest structural diversity index (FSI) ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## HBI structural diversity
source(paste0(getwd(), "/scripts/BZE_III/06_00_biodiversity_index_HBI.R"))

## BZE3 structural diversity
source(paste0(getwd(), "/scripts/BZE_III/06_00_biodiversity_index_BZE3.R"))



##### growth & changes -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# LT, RG, DW growth HBI & BZE3 together
source(paste0(getwd(), "/scripts/BZE_III/07_01_LT_RG_DW_changes_HBI_BZE3.R"))

# extraced wood mass
source(paste0(getwd(), "/scripts/BZE_III/08_01_LT_extracted_wood_mass_HBI_BZE3.R"))
 
 
 
##### output files  -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

## copy all main output files to output_BZE_main
 # save everything imported from database from raw folder to input folder
 # 1. create raw data path: 
 output.path.all <- paste0(getwd(), "/output/out_data/out_data_BZE/")
 # 2. get names of all files in the momok outout folder: https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/list.files
 all.out.files <- list.files(output.path.all) 
 # 3. select only main out files
 main.out.files <- c(
    all.out.files[endsWith(all.out.files, "LT_RG_DW_stocks_ha_all_groups.csv")] 
   , all.out.files[endsWith(all.out.files, "FSI.csv")]
   , all.out.files[endsWith(all.out.files, "LT_RG_DW_changes_all_groups.csv")]
   , all.out.files[endsWith(all.out.files, "update_4.csv")]
   , all.out.files[endsWith(all.out.files, "extracted.csv")])
 # 4. create input path
 output.path.main <- paste0(getwd(), "/output/out_data/out_data_BZE_main/")
 # 5. copy the files from one filder to the other: https://statisticsglobe.com/move-files-between-folders-r
 file.copy(from = paste0(output.path.all, main.out.files),
           to = paste0(output.path.main, main.out.files),
           overwrite = TRUE)
 
 
 
## copy main warning files containing removed data to out_data_BZE_warning
 # 3. select only main out files
 removed.out.files <- all.out.files[endsWith(all.out.files, "removed.csv")] 
 # 4. create input path
 output.path.warning <- paste0(getwd(), "/output/out_data/out_data_BZE_warning/")
 # 5. copy the files from one filder to the other: https://statisticsglobe.com/move-files-between-folders-r
 file.copy(from = paste0(output.path.all, removed.out.files),
           to = paste0(output.path.warning, removed.out.files),
           overwrite = TRUE)
 
 
 
 

##### optional: Plausibility -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#source(paste0(getwd(), "/scripts/BZE_III/10_00_RG_LT_DW_stock_plausi.R"))

