# The aim of this script is to convert .xml files into database structure
# download .xml files as .zip from https://manage.legis.nlp.ipipan.waw.pl/download/ppc-anno.tar.gz and extract to ppc-nanno/ directory
# libraries ----
library(DBI)
library(xml2)
library(googledrive)
library(progress)
# functions ----
source("scripts/__functions.R")
# processing data regarding parliamentary interpellation (interpelacje poselskie) ----
# create directory to store processed files
dir.create(path = "data/", showWarnings = FALSE)
# what periods are available and have data regarding parliamentary interpellation?
periods <- list.dirs(path = "ppc-nanno/", recursive = FALSE, full.names = FALSE)
ip_dirs <- lapply(X = periods, FUN = function(p) {
  list.files(path = file.path("ppc-nanno/", p, "sejm/interpelacje/ip"))
})

periods_avail <- periods[sapply(ip_dirs, length) > 0]
periods_avail

# for each period the data will be processed in form of .sqlite database

# schema
pi_schema <- list("metadata" = c("ID" = "INT", "AUTHOR" = "TEXT", "DOC" = "TEXT"),
                  "ipcontent" = c("ID" = "INT", "CONTENT" = "TEXT"))

# it takes several minutes to process each of those periods
for (period in periods_avail) {
  # establish connection to the database
  database_connection <- dbConnect(RSQLite::SQLite(), file.path("data/", paste0("pi_", period, ".sqlite")))
  
  # create tables
  create_tables(this_schema = pi_schema, conn = database_connection)
  
  # get .xml files
  ip_dir_list <- list.files(path = file.path("ppc-nanno/", period, "sejm/interpelacje/ip"), full.names = T)
  
  # create progress bar
  pb <- progress_bar$new(format = " processing :what [:bar] :percent eta: :eta elapsed :elapsed",
                         clear = FALSE, total = length(ip_dir_list), width = 100)
  # process each file
  for (ip_dir in ip_dir_list) {
    pb$tick(tokens = list(what = period))
    update_database_ip(ip_path = ip_dir, conn = database_connection)    
  }
  
  # close connection
  dbDisconnect(conn = database_connection)
  
  # upload data to Google Drive
  file_name <- paste0("pi_", period, ".sqlite")
  drive_upload(media = file.path("data/", file_name), path = "tm_project", name = file_name, overwrite = TRUE)
}

# sanity check on number of documents
pi_files <- list.files("data", pattern = "^(pi_)")
for (i in pi_files){
  mydb <- dbConnect(RSQLite::SQLite(), file.path("data", i))
  n_data <- dbGetQuery(conn = mydb, statement = "select count(distinct DOC) as number from metadata")$number
  dbDisconnect(mydb)
  period <- gsub("pi_", replacement = "", x = gsub(pattern = ".sqlite", replacement = "", x = i))
  n_files <- length(list.files(path = file.path("ppc-nanno/", period, "sejm/interpelacje/ip"), full.names = T))
  message(sprintf("# docs: %s, # docs database: %s, # diff: %s", n_files, n_data, n_files - n_data))
}

# sanity check on quality of read docs
all_pi <- do.call(rbind, lapply(X = pi_files, FUN = function(i){
  mydb <- dbConnect(RSQLite::SQLite(), file.path("data", i))
  this_cont <- dbGetQuery(conn = mydb, statement = "select metadata.*, ipcontent.content from ipcontent left join metadata on metadata.id = ipcontent.id")
  dbDisconnect(mydb)
  this_cont$period <- gsub("pi_", replacement = "", x = gsub(pattern = ".sqlite", replacement = "", x = i))
  this_cont
}))

all_pi <- all_pi %>% 
  mutate(len = nchar(CONTENT))

all_pi %>% group_by(period) %>% 
  summarise(count = n(), autohrs = length(unique(AUTHOR)), av_len = mean(len), sd_len = sd(len), min_len = min(len), max_len = max(len))

# there is some documents with missing content

# processing data regarding plenary session (posiedzenia parlamentarne) ----

# what periods are available and have data regarding parliamentary interpellation?
ps_dirs <- lapply(X = periods, FUN = function(p) {
  list.files(path = file.path("ppc-nanno/", p, "sejm/posiedzenia/pp"))
})

ps_periods_avail <- periods[sapply(ps_dirs, length) > 0]
ps_periods_avail

# take same periods as in previous step

ps_schema <- list("metadata" = c("ID" = "INT", "DATE" = "TIME", "DOC" = "TEXT"),
                  "pscontent" = c("ID" = "INT", "CONTENT" = "TEXT", "ID_INT" = "TEXT", "AUTHOR" = "TEXT"))

# it takes several minutes to process all of those periods
for (period in periods_avail) {
  # establish connection to the database
  database_connection <- dbConnect(RSQLite::SQLite(), file.path("data/", paste0("ps_", period, ".sqlite")))
  
  # create tables
  create_tables(this_schema = ps_schema, conn = database_connection)
  
  # get .xml files
  ps_dir_list <- list.files(path = file.path("ppc-nanno/", period, "sejm/posiedzenia/pp"), full.names = T)
  
  # create progress bar
  pb <- progress_bar$new(format = " processing :what [:bar] :percent eta: :eta elapsed :elapsed",
                         clear = FALSE, total = length(ps_dir_list), width = 100)
  # process each file
  for (ps_dir in ps_dir_list) {
    pb$tick(tokens = list(what = period))
    update_database_ps(pp_path = ps_dir, conn = database_connection)    
  }
  
  # close connection
  dbDisconnect(conn = database_connection)
  
  # upload data to Google Drive
  file_name <- paste0("ps_", period, ".sqlite")
  drive_upload(media = file.path("data/", file_name),
               path = "tm_project",
               name = file_name)
}
