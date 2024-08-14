# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the  national soil inventory
# comparing org soil types in bze2 database and those occuring in bze2 analysis 

# goal of this script is to compare if the plots with org soil types in bze2 
# database and those occuring in bze2 analysis are identical





# 0.SETUP --------------------------------------------------------------------------------------------------------------------
# 0.1. packages and functions -------------------------------------------------------------------------------------------------
source(paste0(getwd(), "/scripts/01_00_functions_library.R"))


# 0.2 connect to database -------------------------------------------------
# name of database
# db <- 'bze2'  #provide the name of your db
# # host of database: thuenen server --> VPN proably need to be activated 
# host_db <- '134.110.100.88'   # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'  
# # database port or any other port specified by the DBA
# db_port <- '5432'  # this info you can find in the PGadmin properties of the server
# # database username
# db_user <- 'hgercken'  # 'henriette.gercken@thuenen.de'  
# # database password
# db_password <-  'Ao1ieDahthaheoPh' # 'Jette$Thuenen_2024'

# 0.2.1. coencction details --------------------------------------------------
db_name <- "bze2"
db_server <- "134.110.100.88"
db_port <- "5432"
db_user <-  rstudioapi::askForPassword(prompt = "Please enter your username")
my_db_password <- rstudioapi::askForPassword(prompt = "Please enter your password")



# 0.2.2. enable connection ----------------------------------------------------------
con <- sqlconnection(db_name, db_server,db_port, db_user, my_db_password)
# datasets name 
my.schema.name = "bze2_extern" 
my.table.name = "vm_allgemeintab_2"

# 0.2.3. import dataset ----------------------------------------------------------------
org_soil_types <- c("GH", "GM", "GMg", "HH", "HN", "HN-SG", "KH", "KV", "KV-KM")

# soil types database
soil_types_db <- dbGetQuery(con, paste0("SELECT * FROM"," ", my.schema.name,".", my.table.name)) %>% 
  mutate(bodentyp_2 = toupper(bodentyp_2))

# select only organic soil types from database dataset
org_soils_db <- soil_types_db %>% 
  filter(bodentyp_2 %in% org_soil_types & erhebjahr_2 > 2000) %>% 
  select(bfhnr_2, bodentyp_2) %>% 
  rename(., bfhnr = bfhnr_2) %>% 
  distinct()

# soil types analysis
soil_types_analysis <-  read.delim(file = here("data/input/paper/org_soil_types_BZE2.csv"), sep = ",", dec = ".")%>% 
  mutate(plot_bodtyp = toupper(plot_bodtyp)) %>% 
  filter(plot_bodtyp %in% org_soil_types) %>% 
  select(bfhnr, plot_bodtyp ) %>% distinct()





# 1. comparisson org soil types bd and analysis --------------------------------------------------------
anti_join(org_soils_db, soil_types_analysis, by = "bfhnr" )
# plot present in org_soils_db but not in soil_types_analysis 
#     bfhnr    bodentyp_2
# 1   10026         HN
# 2   10029         GH
# 3   30169         GH
# 4   30521         GH
# 5   70109         HN
# 6   80058      HN-SG
# 7   80206         KV
# 8   80223         KV
# 9   90525         HN
# 10  90557         HN
# 11  90594         GM
# 12  90630         HN
# 13  90660         HN
# 14  90677         HH
# 15  90845         HN
# 16  90849         HN
# 17  90877         GM
# 18 120080         GH
# 19 120098         GH
# 20 120119         GM
# 21 120132         KV
# 22 120154         GM
# 23 130069         GH

nrow(org_soils_db)
# numrber of rows: 37

anti_join(soil_types_analysis, org_soils_db, by = "bfhnr" )
# plot present in soil_types_analysis but not in org_soils_db 
#     bfhnr    plot_bodtyp
# 1   30035          HH
# 2   30113          HH
# 3   30143          HH
# 4   30145          HH
# 5   30190          HH
# 6   30636          HH
# 7   90063          HH
# 8   90156          HH
# 9   90187          HH
# 10  90261          HH
# 11  90277          HH
# 12  90305          HH
# 13  90381          HH
# 14 130003          HH
# 15 130004          HH
# 16 130009          HH
# 17 130034          HH
# 18 130058          HH

nrow(soil_types_analysis)
# numrber of rows: 32
