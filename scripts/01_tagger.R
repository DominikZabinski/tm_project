# libraries ----
library(DBI)
# functions ----
source("scripts/__functions.R")
# tagging ----
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
