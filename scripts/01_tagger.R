# libraries ----
library(DBI)
library(googledrive)
# functions ----
source("scripts/__functions.R")
# tagging ----
max_docs <- 10
for (sqlite_file in list.files(path = "data", pattern = "^(ps|pi).*sqlite$")) {
  # download file from Google Drive (if needed)
  if (!file.exists(file.path("tm_project", sqlite_file)))
    googledrive::drive_download(file = file.path("tm_project", sqlite_file), path = "data/")
  
  # create connection to .sqlite file
  database_connection <- dbConnect(RSQLite::SQLite(), file.path("data", paste0("tagged_", sqlite_file)))
  # create directory to store the tagged data
  tg_path <- paste0("tagged_", gsub(pattern = ".sqlite", replacement = "", x = sqlite_file, fixed = TRUE))
  dir.create(path = tg_path, showWarnings = FALSE)
  # find out how many documents there is to process
  document_to_process <- dbGetQuery(conn = database_connection, 
                                    statement = "select id from metadata")$ID
  
  # cross check it with existing files
  existing_files <- gsub(pattern = ".txt", replacement = "", x = list.files(path = tg_path), fixed = TRUE)
  document_to_process <- setdiff(document_to_process, existing_files)
  if (!is.null(max_docs)) document_to_process <- document_to_process[1:min(length(document_to_process), max_docs)]
  # start tagging
  pb <- progress_bar$new(
    format = " processing :what [:bar] :percent eta: :eta elapsed :elapsed",
    clear = FALSE, total = length(document_to_process), width = 100)
  
  # for each document
  for (document_id in document_to_process) {
    pb$tick(tokens = list(what = sqlite_file))
    # tag the text and save it in the file
    res <- tag_text(txt = ipcontent$CONTENT[x])
    res$id <- ipcontent$ID[x]
    write.table(x = res, file = , sep = ";")
  }
}

# combine and upload to Google Drive
for (sqlite_file in list.dirs(path = "data", pattern = "^(ps|pi).*sqlite$")) {
  
}

# download file from Google Drive
# library(go)C
# establish connection to the database
database_connection <- dbConnect(RSQLite::SQLite(), "project_database.sqlite")
ipcontent <- dbGetQuery(conn = database_connection, statement = "select * from metadata left join ipcontent on ipcontent.id=metadata.id")
# ~1 hour per 7500 ip
res <- lapply(X = 1:nrow(ipcontent), 
       FUN = function(x) {
         message(sprintf("%s [%s from %s]", Sys.time(), x, nrow(ipcontent)))
         res <- tag_text(txt = ipcontent$CONTENT[x])
         res$id <- ipcontent$ID[x]
         res
       })
res_df <- do.call(rbind, res)

tagged_connection <- dbConnect(RSQLite::SQLite(), "tagged_database.sqlite")
create_tables(this_schema = list("ipcontenttagged" = c("original" = "TEXT", "tagged" = "TEXT", "id" = "INT")), conn = tagged_connection)
DBI::dbAppendTable(conn = tagged_connection, name = "ipcontenttagged", value = res_df)

ppcontent <- dbGetQuery(conn = database_connection, statement = "select * from metadatapp left join ppcontent on ppcontent.id=metadatapp.id")
nrow(ppcontent)
library(data.table)
ppcontent2 <- data.table(ppcontent)
ppcontent2[, ii := substr(x = ID_INT, 3, regexpr(pattern = ".", text = ID_INT, fixed = T) - 1)]
ppcontent2[, i2 := as.numeric(substr(x = ID_INT, regexpr(pattern = ".", text = ID_INT, fixed = T) + 1, nchar(ID_INT)))]
ppcontent2[, ii := as.numeric(ii)]
ppcontent2 <- ppcontent2[order(ID, ii, i2)]
ppcontent2[, rl := rleid(AUTHOR), by = .(ID)]
ppcontent2 <- ppcontent2[,.(CONTENT = paste0(CONTENT, collapse = "")), by = .(ID, AUTHOR, rl)]

library(progress)
pb <-progress_bar$new(
  format = " processing :what [:bar] :percent eta: :eta elapsed :elapsed",
  clear = FALSE, total = nrow(ppcontent2), width = 60)
start_time <- Sys.time()
res_pp <- lapply(X = 1:nrow(ppcontent2), 
                 FUN = function(x) {
                   pb$tick(tokens = list(what = "pp"))
                   # message(sprintf("%s [%s from %s]", Sys.time(), x, nrow(ppcontent2)))
                   tryCatch(expr = {
                     res <- tag_text(txt = ppcontent2$CONTENT[x])
                   res$id <- ppcontent2$ID[x]
                   res$rl <- ppcontent2$rl[x]
                   res
                   }, error = function(e) message(x))
                 })
print(Sys.time() - start_time)
res_df_pp <- do.call(rbind, res_pp)

tagged_connection <- dbConnect(RSQLite::SQLite(), "tagged_database.sqlite")
create_tables(this_schema = list("ipcontenttagged" = c("original" = "TEXT", "tagged" = "TEXT", "id" = "INT")), conn = tagged_connection)
DBI::dbAppendTable(conn = tagged_connection, name = "ipcontenttagged", value = res_df)
