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

con <-dbConnect(RPostgres::Postgres())


#  1.2. PostgreSQL credentials --------------------------------------------------
# name of database
db <- 'bze2'  #provide the name of your db
# host of database: thuenen server --> VPN proably need to be activated 
host_db <- '134.110.100.88'   # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'  
# database port or any other port specified by the DBA
db_port <- '5432'  # this info you can find in the PGadmin properties of the server
# database username
db_user <- 'hgercken'  # 'henriette.gercken@thuenen.de'  
# database password
db_password <-  'Ao1ieDahthaheoPh' # 'Jette$Thuenen_2024' #
# estabish connection
con <- dbConnect(RPostgres::Postgres(), dbname = db, host=host_db, port=db_port, user=db_user, password=db_password)  



# 2. data -----------------------------------------------------------------
# now we call for the tables in the database that we need
# as we ahve nested tables/ databases we have to add the server, database, sheme and table 
#  https://stackoverflow.com/questions/58289494/how-do-i-access-nested-sql-tables-in-r
dbListTables(con,  "bze2.bze2_bestock" ) 

# https://stackoverflow.com/questions/15520361/permission-denied-for-relation-in-postgresql
res <- dbSendQuery(con, "SELECT * FROM bze2_bestock.b2beab;")
dbFetch(res)
dbClearResult(res)



as.data.frame(dbFetch(res))



# ----- 0.4.1. diameter correction Dahm parameters ------------------------
# change region sheet to x_ld_neu aus code tables
DBH_region <- read.delim(file = here("data/input/general/neu_x_ld.csv"), sep = ";", dec = ",")
DBH_region <- DBH_region %>% dplyr::select(ICode, KurzD,  LangD, bl, region)
colnames(DBH_region) <- c("icode_reg", "reg_shortG", "reg_longG","country",  "region")

# change tangenz csv to neu_k_tangens from code tabellen in 
DBH_tan <- read.delim(file = here("data/input/general/neu_k_tangenz.csv"), sep = ";", dec = ",")
DBH_tan <- DBH_tan %>% dplyr::select( ba_bwi, region, tangenz, Icode)
colnames(DBH_tan) <- c("SP_BWI1",  "region", "tangenz", "icode")
dput(DBH_tan)


# ----- 0.4.2. nitrogen content datasets ----------------------------------
## nitrogen content in foliage based on nitrgen content in leafe samples of the national soil inventory 
# import
N_con_f <-  read.delim(file = here("output/out_data/out_data_momok/N_con_foliage_MOMOK.csv"), sep = ",", dec = ",")
# harmonize N_con_f compartiment names with trees compartiments names, which are based on TapeS compartiment names
N_con_f <- N_con_f %>% mutate(compartiment = case_when(compartiment == "f" ~ "ndl",
                                                       TRUE ~ compartiment))
## nitrogen content in woody compartiments and needles 
# reference: 
# Rumpf, Sabine & Schoenfelder, Egbert & Ahrends, Bernd. (2018). Biometrische Schätzmodelle für Nährelementgehalte in Baumkompartimenten.
# https://www.researchgate.net/publication/329912524_Biometrische_Schatzmodelle_fur_Nahrelementgehalte_in_Baumkompartimenten, 
# Tab.: 3.2 - 3.6, S. 10
# import
N_con_w <-  read.delim(file = here("output/out_data/out_data_momok/N_con_wood_Rumpf.csv"), sep = ",", dec = ",")
# hamronizing compartimebnt names between nitrogen datasets and TapeS based trees dataset compartiment names
N_con_w <- N_con_w %>% mutate(compartiment = case_when(compartiment == "f" ~ "ndl", 
                                                       compartiment == "swb" ~ "sb", 
                                                       compartiment == "stwb" ~"stb",
                                                       compartiment == "fw" ~ "fwb", 
                                                       TRUE ~ compartiment))

## belowground biomass notrogen contents in percent (mgg/1000)
# reference: 
# Jacobsen et al. 2003; 
# Gehalte chemischer Elemente in Baumkompartimenten Literaturstudie und Datensammlung, 
# Berichte des Forschungszentrums Waldökosysteme, Reihe B, Bd. 69, 2003
# Carsten Jacobsen, Peter Rademacher, Henning Meesenburg und Karl Josef Meiwes
# Niedersächsische Forstliche Versuchsanstalt;
# N Gehalte Grobwurzeln (D > 2mm), Tab. 7
# import
N_con_bg <- as.data.frame(cbind("SP_group" = c("EI", "BU" , "FI" , "KI", "KIN" , "BI" , "LA"), 
                                "N_con" = c(3.71,3.03, 4.14, 1.77,  1.76, 3.7, 2.8)/1000, 
                                "compartiment" = c("bg", "bg", "bg", "bg", "bg", "bg", "bg")))


# 0.4.3. import species names dataest x_bart ------------------------------
# species names & codes 
SP_names_com_ID_tapeS <- read.delim(file = here("output/out_data/x_bart_tapeS.csv"), sep = ",", dec = ",") 
# the join always works like this: 
# left_join(., SP_names_com_ID_tapeS %>% 
#             mutate(char_code_ger_lowcase = tolower(Chr_code_ger)), 
#           by = c("SP_code" = "char_code_ger_lowcase"))



# 0.4.4. create sampling cuicits dataset ------------------------------
# creating dataset with information about the concentric sampling circles
data_circle <- data.frame(x0 = c(0,0,0),       # x of centre point of all 3 circles is 0 
                          y0 = c(0,0,0),       # y of centre point of all 3 circles is 0 
                          r0 = c(5.64, 12.62, 17.84), # darius in m
                          rmax = c(30.00, 30.00, 30.00)) # these are the radi of the sampling circuits in m

