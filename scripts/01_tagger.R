# libraries ----
library(DBI)
# functions ----
source("scripts/__functions.R")
# tagging ----
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
