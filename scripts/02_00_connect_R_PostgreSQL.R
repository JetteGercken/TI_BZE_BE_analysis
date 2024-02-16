# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the  national soil inventory
# connecting R and PostGres database


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
db <- 'bze3_altdaten'  #provide the name of your db
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
dbListTables(con,  "bze3_altdaten.data" ) 


# https://stackoverflow.com/questions/15520361/permission-denied-for-relation-in-postgresql
dbSendQuery(con, "GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA data TO hgercken;")
res <- dbSendQuery(con, "SELECT * FROM data.beab;")
dbFetch(res)
dbClearResult(res)
DBI::dbGetQuery(con, "SELECT * FROM data.beab;")
