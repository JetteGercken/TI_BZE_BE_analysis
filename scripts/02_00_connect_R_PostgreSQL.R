# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the  national soil inventory
# connecting R and PostGres database


# list of tables in BZE3 and HBI that i need 
  # neu_x_ld.csv
  # neu_k_tangenz.csv
  # x_bart_neu.csv
  # tit_1.csv
  # be.csv
  # beab.csv
  # be_waldraender.csv
  # bej.csv
  # bejb.csv
  # bedw.csv
  # bedw_liste.csv
  # punkt.csv
  # HBI_location.csv


# name of database
# db <- 'bze2'  #provide the name of your db
# # host of database: thuenen server --> VPN proably need to be activated 
# host_db <- '134.110.100.88'   # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'  
# # database port or any other port specified by the DBA
# db_port <- '5432'  # this info you can find in the PGadmin properties of the server
# # database username
# db_user <- 'hgercken'  # 'henriette.gercken@thuenen.de'  



# 0.SETUP --------------------------------------------------------------------------------------------------------------------
# 0.1. packages and functions -------------------------------------------------------------------------------------------------
source(paste0(getwd(), "/scripts/01_00_functions_library.R"))


# 1. database -------------------------------------------------------------
# postegres packages: https://www.datacareer.de/blog/connect-to-postgresql-with-r-a-step-by-step-example/
# postgreSQL package: https://hevodata.com/learn/rpostgresql/


# 1.1. establish connection with database via R ---------------------------
# https://stackoverflow.com/questions/18580066/how-to-allow-remote-access-to-postgresql-database
# https://www.cyberciti.biz/tips/postgres-allow-remote-access-tcp-connection.html
# https://www.postgresql.org/docs/9.1/auth-pg-hba-conf.html
#con <- dbConnect(RPostgres::Postgres())


#  1.2. PostgreSQL credentials --------------------------------------------------
# db = "bze2"
# server =  "134.110.100.88"
# port = "5432"
# username = "hgercken"

#  1.3. estabish connection to database --------------------------------------------------
con <- sqlconnection(db_name, db_server,db_port, db_user, my_db_password)

# 2. data -----------------------------------------------------------------
# now we call for the tables in the database that we need
# as we ahve nested tables/ databases we have to add the server, database, sheme and table 
#  https://stackoverflow.com/questions/58289494/how-do-i-access-nested-sql-tables-in-r
# dbListTables(con,  "bze2.bze2_bestock" ) 
# 
# # https://stackoverflow.com/questions/15520361/permission-denied-for-relation-in-postgresql
# res <- dbSendQuery(con, "SELECT * FROM bze2_hbi.v_b2beab_auf;")
#   dbFetch(res)
# dbClearResult(res)
# dbGetQuery(con1, paste0("SELECT * FROM"," ", "bze2_hbi",".", "v_b2beab_auf"))


# names of the tables we want to import to our raw data folder: 
code_table_names <- c("x_ld", "k_tangenz", "x_bart")
data_table_names <- c("tit", "be", "beab", "be_waldraender", "bej", "bejb", "be_totholz_punkt", "be_totholz_liste", "punkt")

# 2.1. code tables ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 2.1.1. import code tables from database to raw folder -----------------------------------------------------------------------------------------------------------------------------------------
for (i in 1:length(code_table_names)) {
  # i = 1
  # get table name
  my.table.name <- code_table_names[i]
  # set schema name
  my.schema.name <- "code"
  # set database name 
  db_name <- 'bze3_altdaten'
  # set db connection
  con <- sqlconnection(db_name, db_server, db_port, db_user, my_db_password)
  # get table from database and transform into dataframe
  df <- dbGetQuery(con, paste0("SELECT * FROM"," ", my.schema.name,".", my.table.name))
  # name dataframe and export it to raw data folder
  write.csv(df, paste0(here("data/raw/general"), "/", my.table.name, ".csv"), row.names = FALSE)
}


# 2.1.2. copy code files from raw data general to input general fo -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# copy everything imported from database from raw folder to input folder
# 1. create raw data path: 
raw.path.code <- paste0(here("data/raw/general"), "/")
# 2. get names of all files in the momok outout folder: https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/list.files
code.in.files <- list.files(raw.path.code) 
# 3. create input path
input.path.code <- paste0(here("data/input/general"), "/")
# copy the files from one filder to the other: https://statisticsglobe.com/move-files-between-folders-r
file.copy(from = paste0(raw.path.code, code.in.files),
          to = paste0(input.path.code, code.in.files),
          overwrite = TRUE)




# 2.2. HBI data tables --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 2.2.1. import data tables from bze2 database to raw folder -----------------------------------------------------------------------------------------------------------------------------------------
for (i in 1:length(data_table_names)) {
  # get table name
  my.table.name <- data_table_names[i]
  # set schema name
  my.schema.name <- "data" # we have to change this later
  # set database name 
  db_name <- 'bze3_altdaten'
  # set db connection
  con <- sqlconnection(db_name, db_server, db_port, db_user, my_db_password)
  # get table from database and transform into dataframe
  df <- dbGetQuery(con, paste0("SELECT * FROM"," ", my.schema.name,".", my.table.name))
  # name dataframe and export it to raw data folder
  write.csv(df, paste0(here("data/raw/BZE2_HBI"), "/", my.table.name, ".csv"), row.names = FALSE)
  
}

# get HBI locations
data_table_names_2 <- c("vm_lokation_hbi")
for (i in 1:length(data_table_names_2)) {
  # get table name
  my.table.name <-data_table_names_2[i]
  # set schema name
  my.schema.name <- "bze2_extern" # we have to change this later
  # set database name 
  db_name <- 'bze2'
  # set db connection
  con <- sqlconnection(db_name, db_server, db_port, db_user, my_db_password)
  # get table from database and transform into dataframe
  df <- dbGetQuery(con, paste0("SELECT * FROM"," ", my.schema.name,".", my.table.name))
  # name dataframe and export it to raw data folder
  write.csv(df, paste0(here("data/raw/BZE2_HBI"), "/", "location_HBI", ".csv"), row.names = FALSE)
  
}
# 2.2.2. copy data files from raw data bze2 to input bze2 fo -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# save everything imported from database from raw folder to input folder
# 1. create raw data path: 
raw.path.bze2 <- paste0(here("data/raw/BZE2_HBI"), "/")
# 2. get names of all files in the momok outout folder: https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/list.files
bze2.in.files <- list.files(raw.path.bze2) 
# 3. create input path
input.path.bze2 <- paste0(here("data/input/BZE2_HBI"), "/")
# copy the files from one filder to the other: https://statisticsglobe.com/move-files-between-folders-r
file.copy(from = paste0(raw.path.bze2, bze2.in.files),
          to = paste0(input.path.bze2, bze2.in.files),
                        overwrite = TRUE)



# 2.3. BZE3 data tables -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 2.3.1. import data tables from bze3 database to raw folder -----------------------------------------------------------------------------------------------------------------------------------------
for (i in 1:length(data_table_names)) {
  # i = 1
  my.table.name <- data_table_names[i]
  # set schema name
  my.schema.name <- "data" # we have to change this later
  # set database name 
  db_name <- 'bze3'
  # set db connection
  con <- sqlconnection(db_name, db_server, db_port, db_user, my_db_password)
  # get table from database and transform into dataframe
  df <- dbGetQuery(con, paste0("SELECT * FROM"," ", my.schema.name,".", my.table.name))
  # name dataframe and export it to raw data folder
  write.csv(df, paste0(here("data/raw/BZE2_HBI"), "/", "location_HBI", ".csv"), row.names = FALSE)
  # name dataframe 
  write.csv(df, paste0(here("data/input/BZE3"), "/", my.table.name, ".csv"), row.names = FALSE)

}
# 2.3.2. copy data files from raw data bze3 to input bze3 fo -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# save everything imported from database from raw folder to input folder
# 1. create raw data path: 
raw.path.bze3 <- paste0(here("data/raw/BZE3"), "/")
# 2. get names of all files in the momok outout folder: https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/list.files
bze3.in.files <- list.files(raw.path.bze3) 
# 3. create input path
input.path.bze3 <- paste0(here("data/input/BZE3"), "/")
# copy the files from one filder to the other: https://statisticsglobe.com/move-files-between-folders-r
file.copy(from = paste0(raw.path.bze3, bze3.in.files),
          to = paste0(input.path.bze3, bze3.in.files),
          overwrite = TRUE)





stop("this is where notes and trials of dataimport start")



# # ----- 0.4.1. diameter correction Dahm parameters ------------------------
# # this we´ll have to remove later 
# # change region sheet to x_ld_neu aus code tables
# DBH_region <- read.delim(file = here("data/input/general/x_ld.csv"), sep = ",", dec = ".")
# DBH_region <- DBH_region %>% dplyr::select(id, kurz,lang, region)
# colnames(DBH_region) <- c("icode_reg", "reg_shortG", "reg_longG","country",  "region")
# 
# # change tangenz csv to neu_k_tangens from code tabellen in 
# DBH_tan <- read.delim(file = here("data/input/general/k_tangenz.csv"), sep = ",", dec = ".")
# DBH_tan <- DBH_tan %>% dplyr::select(ba_bwi, region, tangenz)
# colnames(DBH_tan) <- c("SP_BWI1",  "region", "tangenz")
# dput(DBH_tan)
# 
# 
# # ----- 0.4.2. nitrogen content datasets ----------------------------------
# ## nitrogen content in foliage based on nitrgen content in leafe samples of the national soil inventory 
# # import
# N_con_f <-  read.delim(file = here("output/out_data/out_data_momok/N_con_foliage_MOMOK.csv"), sep = ",", dec = ",")
# # harmonize N_con_f compartiment names with trees compartiments names, which are based on TapeS compartiment names
# N_con_f <- N_con_f %>% mutate(compartiment = case_when(compartiment == "f" ~ "ndl",
#                                                        TRUE ~ compartiment))
# ## nitrogen content in woody compartiments and needles 
# # reference: 
# # Rumpf, Sabine & Schoenfelder, Egbert & Ahrends, Bernd. (2018). Biometrische Schätzmodelle für Nährelementgehalte in Baumkompartimenten.
# # https://www.researchgate.net/publication/329912524_Biometrische_Schatzmodelle_fur_Nahrelementgehalte_in_Baumkompartimenten, 
# # Tab.: 3.2 - 3.6, S. 10
# # import
# N_con_w <-  read.delim(file = here("output/out_data/out_data_momok/N_con_wood_Rumpf.csv"), sep = ",", dec = ",")
# # hamronizing compartimebnt names between nitrogen datasets and TapeS based trees dataset compartiment names
# N_con_w <- N_con_w %>% mutate(compartiment = case_when(compartiment == "f" ~ "ndl", 
#                                                        compartiment == "swb" ~ "sb", 
#                                                        compartiment == "stwb" ~"stb",
#                                                        compartiment == "fw" ~ "fwb", 
#                                                        TRUE ~ compartiment))
# 
# ## belowground biomass notrogen contents in percent (mgg/1000)
# # reference: 
# # Jacobsen et al. 2003; 
# # Gehalte chemischer Elemente in Baumkompartimenten Literaturstudie und Datensammlung, 
# # Berichte des Forschungszentrums Waldökosysteme, Reihe B, Bd. 69, 2003
# # Carsten Jacobsen, Peter Rademacher, Henning Meesenburg und Karl Josef Meiwes
# # Niedersächsische Forstliche Versuchsanstalt;
# # N Gehalte Grobwurzeln (D > 2mm), Tab. 7
# # import
# N_con_bg <- as.data.frame(cbind("SP_group" = c("EI", "BU" , "FI" , "KI", "KIN" , "BI" , "LA"), 
#                                 "N_con" = c(3.71,3.03, 4.14, 1.77,  1.76, 3.7, 2.8)/1000, 
#                                 "compartiment" = c("bg", "bg", "bg", "bg", "bg", "bg", "bg")))
# 
# 
# # 0.4.3. import species names dataest x_bart ------------------------------
# # species names & codes 
# SP_names_com_ID_tapeS <- read.delim(file = here("output/out_data/x_bart_tapeS.csv"), sep = ",", dec = ",") 
# # the join always works like this: 
# # left_join(., SP_names_com_ID_tapeS %>% 
# #             mutate(char_code_ger_lowcase = tolower(Chr_code_ger)), 
# #           by = c("SP_code" = "char_code_ger_lowcase"))
# 
# 
# 
# # 0.4.4. create sampling cuicits dataset ------------------------------
# # creating dataset with information about the concentric sampling circles
# data_circle <- data.frame(x0 = c(0,0,0),       # x of centre point of all 3 circles is 0 
#                           y0 = c(0,0,0),       # y of centre point of all 3 circles is 0 
#                           r0 = c(5.64, 12.62, 17.84), # darius in m
#                           rmax = c(30.00, 30.00, 30.00)) # these are the radi of the sampling circuits in m
# 
# 


