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
  paste0(unlist(lapply(X = 1:length(xx), 
                       FUN = function(i){
                         tryCatch(expr = {
                           xx[i]$p[[1]]
                         }, error = function(e) "")
                       })), collapse = " ")
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

update_database_ps <- function(pp_path, conn) {
  ps_transformed <- transform_pp(pp_path = pp_path)
  new_id <- est_new_id("metadata", conn)
  DBI::dbAppendTable(conn = conn, name = "metadata", 
                     value = data.frame(ID = new_id, DATE = ps_transformed$date))
  DBI::dbAppendTable(conn = conn, name = "pscontent", 
                     value = cbind(data.frame(ID = new_id), ps_transformed$cont))
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
  txt <- gsub(pattern = '"', replacement = "", x = txt)
  res <- system(command = sprintf('curl -XPOST \"localhost:9003/?output_format=conll\" -d \"%s\" -o %s', txt, tmp_), intern = T)
  res <- readLines(tmp_, encoding = "UTF-8")
  res <- res[nchar(res) > 0]
  res <- res[regexpr(pattern = "--:--:--", text = res, fixed = T) < 0]
  res <- res[regexpr(pattern = "\tinterp", text = res, fixed = T) < 0]
  if (length(res) == 0) {
    return(data.frame(original = "", tagged = ""))
  }
  writeLines(text = res, con = tmp_)
  res <- read.table(file = tmp_, sep = "\t", quote = "")[, c("V1", "V2")]
  names(res) <- c("original", "tagged")
  res
}

polish_stop_words <- function() {
  base_stop <- c(
    # A
    "a", "aby", "ach", "acz", "aczkolwiek", "aj", "albo", "ale", "ależ", "ani", "aż", "art", 
    # B
    "bardziej", "bardzo", "bo", "bowiem", "by", "byli", "bynajmniej", "być", "był", "była", "było", "były", "będzie", 
    "będą", 
    # C
    "cali", "cała", "cały", "ci", "cię", "ciebie", "co", "cokolwiek", "coś", "czasami", "czasem", "czemu", "czy", 
    "czyli", 
    # D
    "daleko", "dla", "dlaczego", "dlatego", "do", "dobrze", "dokąd", "dość", "dużo", "dwa", "dwaj", "dwie", "dwoje", 
    "dziś", "dzisiaj", 
    # G
    "gdy", "gdyby", "gdyż", "gdzie", "gdziekolwiek", "gdzieś", 
    # I
    "i", "ich", "ile", "im", "inna", "inne", "inny", "innych", "iż",
    # J
    "ja", "ją", "jak", "jakaś", "jakby", "jaki", "jakichś", "jakie", "jakiś", "jakiż", "jakkolwiek", "jako", "jakoś", 
    "je", "jeden", "jedna", "jedno", "jednak", "jednakże", "jego", "jej", "jemu", "jest", "jestem", "jeszcze", "jeśli", 
    "jeżeli", "już", "ją", 
    # K
    "każdy", "kiedy", "kilka", "kimś", "kto", "ktokolwiek", "ktoś", "która", "które", "którego", "której", "który", 
    "których", "którym", "którzy", "ku", 
    # L
    "lat", "lecz", "lub", 
    # M
    "ma", "mają", "mało", "mam", "mi", "mimo", "między", "mną", "mnie", "mogą", "moi", "moim", "moja", "moje", "może", 
    "możliwe", "można", "mój", "mu", "musi", "my", 
    # N
    "na", "nad", "nam", "nami", "nas", "nasi", "nasz", "nasza", "nasze", "naszego", "naszych", "natomiast", 
    "natychmiast", "nawet", "nią", "nic", "nich", "nie", "niech", "niego", "niej", "niemu", "nigdy", "nim", "nimi", 
    "niż", "no", 
    # O
    "o", "obok", "od", "około", "on", "ona", "one", "oni", "ono", "oraz", "oto", "owszem", 
    # P
    "pan", "pana", "pani", "po", "pod", "podczas", "pomimo", "ponad", "ponieważ", "powinien", "powinna", "powinni", 
    "powinno", "poza", "prawie", "przecież", "przed", "przede", "przedtem", "przez", "przy", 
    # R
    "roku", "również", 
    # S
    "sam", "sama", "są", "się", "skąd", "sobie", "sobą", "sposób", "swoje", 
    # T
    "ta", "tak", "taka", "taki", "takie", "także", "tam", "te", "tego", "tej", "temu", "ten", "teraz", "też", "tę", "to", 
    "tobą", "tobie", "toteż", "trzeba", "tu", "tutaj", "twoi", "twoim", "twoja", "twoje", "twym", "twój", "ty", "tych", 
    "tylko", "tym", 
    # U
    "u", 
    # W
    "w", "wam", "wami", "was", "wasz", "wasza", "wasze", "we", "według", "wiele", "wielu", "więc", "więcej", "wszyscy", 
    "wszystkich", "wszystkie", "wszystkim", "wszystko", "wtedy", "wy", "właśnie", 
    # Z
    "z", "za", "zaś", "zapewne", "zawsze", "ze", "zł", "znowu", "znów", "został", 
    # Ż
    "żaden", "żadna", "żadne", "żadnych", "że", "żeby")
  c(base_stop, c("ad", "vocem"))
}
