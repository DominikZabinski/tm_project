# The aim of this script is to convert .xml files into database structure
# download .xml files as .zip from https://manage.legis.nlp.ipipan.waw.pl/download/ppc-anno.tar.gz and extract to ppc-nanno/ directory
# libraries ----
library(DBI)
library(xml2)
# functions ----
source("scripts/__functions.R")
# do ---
# establish connection to the database
database_connection <- dbConnect(RSQLite::SQLite(), "project_database.sqlite")

# schema
schema <- list("metadata" = c("ID" = "INT", "AUTHOR" = "TEXT"),
               "ipcontent" = c("ID" = "INT", "CONTENT" = "TEXT"),
               "metadatapp" = c("ID" = "INT", "DATE" = "TIME"),
               "ppcontent" = c("ID" = "INT", "CONTENT" = "TEXT", "ID_INT" = "TEXT", "AUTHOR" = "TEXT"))
# create tables
create_tables(this_schema = schema, conn = database_connection)

# ip - Interpelacje Poselskie
ip_dir_list <- list.files("ppc-nanno/1997-2001/sejm/interpelacje/ip/", full.names = T)

for (ip_dir in ip_dir_list) {
    message(sprintf("%s from %s", which(ip_dir_list == ip_dir), length(ip_dir_list)))
    update_database_ip(ip_path = ip_dir, conn = database_connection)    
}

# pp - posiedzenie Plenarne
pp_dir_list <- list.files(path = "ppc-nanno/1997-2001/sejm/posiedzenia/pp/", full.names = T)

for (pp_dir in pp_dir_list) {
    message(sprintf("%s from %s", which(pp_dir_list == pp_dir), length(pp_dir_list)))
    update_database_pp(pp_path = pp_dir, conn = database_connection)    
}
