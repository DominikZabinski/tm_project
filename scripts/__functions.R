#' Title
#'
#' @param table_name 
#' @param table_fields 
#' @param conn 
#'
#' @return
#' @export
#'
#' @examples
#' create_table("metadata", c("ID" = "INT", "AUTHOR" = "TEXT"))
create_table <- function(table_name, table_fields, conn) {
  if (table_name %in% DBI::dbListTables(conn = conn)) {
    message(sprintf("skipping %s", table_name))
    return(NULL)
  }
  DBI::dbCreateTable(conn = conn, name = table_name, fields = table_fields)
}

#' Title
#'
#' @param this_schema 
#' @param conn 
#'
#' @return
#' @export
#'
#' @examples
#' create_tables(schema = list(a = c("ID" = "INT", "TT" = "TEXT")))
create_tables <- function(this_schema, conn) {
  for (i in 1:length(this_schema)) create_table(names(this_schema)[i], this_schema[[i]], conn)
}

reset_tables <- function(tables_to_del = NULL, conn) {
  if (is.null(tables_to_del)) tables_to_del <- DBI::dbListTables(conn = conn)
  for (tab in tables_to_del) DBI::dbRemoveTable(conn = conn, name = tab)
}

metadata_ip <- function(file_path) {
  xx <- xml2::as_list(x = read_xml(file_path))$teiHeader$fileDesc    
  list(title = xx$sourceDesc$bibl$title[[1]],
       auth = xx$sourceDesc$bibl$author[[1]])
}

content_ip <- function(file_path) {
  xx <- xml2::as_list(x = read_xml(file_path))$teiCorpus$TEI$text$body
  paste0(unlist(lapply(1:length(xx), FUN = function(i) xx[i]$p[[1]])), collapse = " ")
}

transform_ip <- function(ip_path) {
  c(metadata_ip(file_path = file.path(ip_path, "header.xml")),
    list(content = content_ip(file_path = file.path(ip_path, "text_structure.xml")))
  )
}

transform_pp <- function(pp_path) {
  content_as_list <- as_list(read_xml(x = file.path(pp_path, "text_structure.xml")))$teiCorpus$TEI$text$body
  res_cont <- do.call(rbind, lapply(X = content_as_list, FUN = function(tt1){
    res_cont <- do.call(rbind, lapply(X = tt1, FUN = function(tt2){
      res_cont <- do.call(rbind, lapply(X = tt2, FUN = function(i){
        data.frame(CONTENT = i)
      }))
      res_cont$ID_INT <- attributes(tt2)$id
      res_cont$AUTHOR <- attributes(tt2)$who
      res_cont
    }))
    row.names(res_cont) <- NULL
    res_cont
  }))
  row.names(res_cont) <- NULL
  list(cont = res_cont, 
       date = as_list(read_xml(x = file.path(pp_path, "header.xml")))$teiHeader$fileDesc$sourceDesc$bibl$date[[1]])
}

est_new_id <- function(table = "metadata", conn) {
  max_id <- DBI::dbGetQuery(conn = conn, statement = sprintf("select max(ID) as MAX from %s", table))$MAX
  if (is.na(max_id)) {
    new_id <- 1
  } else {
    new_id <- max_id + 1
  }
}

update_database_ip <- function(ip_path, conn) {
  ip_transformed <- transform_ip(ip_path = ip_path)
  for (i in names(ip_transformed)) {
    if (is.null(ip_transformed[[i]])) ip_transformed[[i]] <- "na"
  }
  new_id <- est_new_id("metadata", conn)
  DBI::dbAppendTable(conn = conn, name = "metadata", value = data.frame(ID = new_id, AUTHOR = ip_transformed$auth))
  DBI::dbAppendTable(conn = conn, name = "ipcontent", value = data.frame(ID = new_id, CONTENT = ip_transformed$content))
}

update_database_pp <- function(pp_path, conn) {
  pp_transformed <- transform_pp(pp_path = pp_path)
  new_id <- est_new_id("metadatapp", conn)
  DBI::dbAppendTable(conn = conn, name = "metadatapp", 
                     value = data.frame(ID = new_id, DATE = pp_transformed$date))
  DBI::dbAppendTable(conn = conn, name = "ppcontent", 
                     value = cbind(data.frame(ID = new_id), pp_transformed$cont))
}

#' Title
#'
#' @param txt 
#'
#' @return
#' @export
#'
#' @examples
#' tag_text("Litwo, ojczyzno moja")
tag_text <- function(txt) {
  tmp_ <- tempfile(fileext = ".txt")
  res <- system(command = sprintf('curl -XPOST \"localhost:9003/?output_format=conll\" -d \"%s\" -o %s', txt, tmp_), intern = T)
  res <- readLines(tmp_, encoding = "UTF-8")
  res <- res[nchar(res) > 0]
  res <- res[regexpr(pattern = "--:--:--", text = res, fixed = T) < 0]
  res <- res[regexpr(pattern = "\tinterp", text = res, fixed = T) < 0]
  writeLines(text = res, con = tmp_)
  res <- read.table(file = tmp_, sep = "\t", quote = "")[, c("V1", "V2")]
  names(res) <- c("original", "tagged")
  res
}
