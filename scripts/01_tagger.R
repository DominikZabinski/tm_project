# libraries ----
library(DBI)
library(googledrive)
library(progress)
# functions ----
source("scripts/__functions.R")
# tagging ----
max_docs <- 10
project_files <- drive_ls(path = "tm_project")$name
project_files <- project_files[regexpr(pattern = "^(ps|pi).*sqlite$", text = project_files) > 0]
dir.create(path = "data/", showWarnings = FALSE)
for (sqlite_file in project_files) {
  # download file from Google Drive (if needed)
  if (!file.exists(file.path("data", sqlite_file)))
    googledrive::drive_download(file = file.path("tm_project", sqlite_file), path = paste0("data/", sqlite_file))
  
  # create connection to .sqlite file
  database_connection <- dbConnect(RSQLite::SQLite(), file.path("data", sqlite_file))
  
  # create directory to store the tagged data
  tg_path <- paste0("tagged_", gsub(pattern = ".sqlite", replacement = "", x = sqlite_file, fixed = TRUE))
  dir.create(path = file.path("data", tg_path), showWarnings = FALSE)
  
  # find out how many documents there is to process
  document_to_process <- dbGetQuery(conn = database_connection, 
                                    statement = "select id from metadata")$ID
  
  # cross check it with existing files
  existing_files <- gsub(pattern = ".txt", replacement = "", x = list.files(path = file.path("data", tg_path)), fixed = TRUE)
  document_to_process <- setdiff(document_to_process, existing_files)
  if (!is.null(max_docs)) document_to_process <- document_to_process[1:min(length(document_to_process), max_docs)]
  if (length(document_to_process) == 0) next()
  # start tagging
  pb <- progress_bar$new(
    format = " processing :what [:bar] :percent eta: :eta elapsed :elapsed",
    clear = FALSE, total = length(document_to_process), width = 100)
  table_name <- ifelse(substr(sqlite_file, 1, 2) == "ps", "pscontent", "ipcontent")
  
  # for each document
  for (document_id in document_to_process) {
    pb$tick(tokens = list(what = sqlite_file))
    # tag the text and save it in the file
    doc_content <- dbGetQuery(conn = database_connection, statement = sprintf("select * from %s where id = %s", table_name, document_id))
    res <- tag_text(txt = paste0(doc_content$CONTENT, collapse = " ")) # in Parliamentary session ID is for Parliamentary session, not for statement of one person
    res$id <- doc_content$ID[1]
    write.table(x = res, file = file.path("data", tg_path, paste0(document_id, ".txt")), sep = ";")
  }
  dbDisconnect(database_connection)
}

# combine and upload to Google Drive
for (tagged_dir in list.dirs(path = "data", recursive = FALSE, full.names = FALSE)) {
  new_connection <- dbConnect(RSQLite::SQLite(), file.path("data", paste0(tagged_dir, ".sqlite")))
  tagged_docs <- list.files(path = file.path("data", tagged_dir))
  tagged_docs_ids <- as.numeric(gsub(pattern = ".txt", replacement = "", x = tagged_docs))
  
  if (!"tagged" %in% dbListTables(new_connection)) dbCreateTable(new_connection, name = "tagged", fields = c("ID" = "INT", "ORG" = "TEXT", "TAGGED" = "TEXT"))
  existing_docs <- dbGetQuery(conn = new_connection, "select distinct id from tagged")$ID
  tagged_docs_ids <- setdiff(tagged_docs_ids, existing_docs)
  
  if (length(tagged_docs_ids) == 0) next()
  
  pb <- progress_bar$new(
    format = " appending :what [:bar] :percent eta: :eta elapsed :elapsed",
    clear = FALSE, total = length(tagged_docs), width = 100)
  
  for (i in tagged_docs_ids) {
    pb$tick(tokens = list(what = sqlite_file))
    res <- read.table(file = file.path("data", tagged_dir, paste0(i, ".txt")), sep = ";")
    dbAppendTable(conn = new_connection, name = "tagged", value = data.frame(ID = i, ORG = res$original, TAGGED = res$tagged))
  }
  dbDisconnect(new_connection)
  drive_upload(media = file.path("data", paste0(tagged_dir, ".sqlite")), path = "tm_project/", name = paste0(tagged_dir, ".sqlite"))
}
