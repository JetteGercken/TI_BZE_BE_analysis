# this script is to test if the csv requirements can be ensured with an r script
# the csv requirements for BZE data are: 
# UTF-8 format
# separated by ","
# decimals as "."
# characters have to be in high quotes " " 
# date should be in yyyy-mm-dd format

source(paste0(getwd(), "/scripts/01_00_functions_library.R"))
input.path <- "//wo-sfs-001v-ew/INSTITUT/a7bze/ZZ_BZE3_Bestand/spezielle_Themen/temp/"

be <- read.delim(file = paste0(input.path, "be.csv"), sep = ",", dec = ".")
be_2 <- read.delim(file = paste0(input.path, "be - Kopie.csv"), sep = ";", dec = ",")
# https://stackoverflow.com/questions/7439977/changing-date-format-in-r
# sollte das datumsformat nicht bereits yyyy-mm-dd sein: 
be_2$datum <- format(as.Date(be$datum, format = "%y-%m-%d"), "%Y-%m-%d")
# andernfalls: 
be_2$datum <- as.Date(be$datum)
be$beschreibungbestockung[1] <- "ABCDEFG, hallo"

be$beschreibungbestockung <- as.character(be$beschreibungbestockung)




write.table(
  x = be_2,
  file = paste0(input.path, "be_manipulated_out.csv"),
  sep = ",",               # Columns separated by comma
  dec = ".",               # Decimal delimiter is a dot
  row.names = FALSE,       # Do not include row names
  quote = TRUE,            # Characters are indicated by double quotes
  fileEncoding = "UTF-8",  # UTF-8 format
  #col.names = NA          # Do not include column names
)


write.csv(
  x = be_2,
  file = paste0(input.path, "be_manipulated_out.csv"),
  quote = TRUE,            # Characters are indicated by double quotes
  fileEncoding = "UTF-8",  # UTF-8 format
)


