#'@importFrom rJava .jaddClassPath
.get_driver <- function() {
  jarlist <- dir(system.file("drv", package = .packageName), "^datanucleus", full.names = TRUE)
  if (length(jarlist) != 3) {
    get_driver()
  }
  jarlist <- dir(system.file("drv", package = .packageName), "^datanucleus", full.names = TRUE)
  lapply(jarlist, .jaddClassPath)
  assembly <- dir(system.file("drv", package = .packageName), "^spark-assembly", full.names = TRUE)
  RJDBC::JDBC("org.apache.hive.jdbc.HiveDriver", assembly)
}

# borrowed from dplyr
`%||%` <- function(x, y) if(is.null(x)) y else x

#'@export
src_desc.src_hive2 <- function(x) {
  "Spark SQL"
}

#'@importClassesFrom RJDBC JDBCConnection
setClass("Hive2Connection", representation(), contains = structure("JDBCConnection", package = "RJDBC"))

setMethod("initialize", signature("Hive2Connection"), definition = function(.Object, obj) {
  .Object@jc <- obj@jc
  .Object@identifier.quote <- obj@identifier.quote
  .Object
})

#'@export
src_spark_sql <- function(host, port, user, password = NULL) {
  drv <- .get_driver()
  connection_string <- sprintf("jdbc:hive2://%s:%s", host, port)
  con <- new ("Hive2Connection", DBI::dbConnect(drv, connection_string, user, password %||% ""))
  src_sql("hive2", con)
}

#'@export
db_list_tables.src_hive2 <- function(con) {
  db_list_tables(con$con)
}

#'@export
db_list_tables.Hive2Connection <- function(con) {
  DBI::dbGetQuery(con, "SHOW TABLES")$tableName
}

#'@export
db_has_table.src_hive2 <- function(con, table) {
  table %in% db_list_tables(con)
}

#'@export
db_has_table.Hive2Connection <- function(con, table) {
  table %in% db_list_tables(con)
}

#'@export
tbl.src_hive2 <- function(src, from, ...) {
  tbl_sql("hive2", src = src, from = from, ...)
}

#'@export
db_begin.Hive2Connection <- function(con, ...) TRUE

#'@export
db_commit.Hive2Connection <- function(con, ...) TRUE

#'@export
db_rollback.Hive2Connection <- function(con, ...) TRUE

hive2DataType <- function(dbObj, obj, ...) {
  if (is.integer(obj)) "INT"
  else if (is.numeric(obj)) "DOUBLE"
  else "STRING"
}

#'@export
db_data_type.src_hive2 <- function(con, fields) {
  db_data_type(con$con, fields)
}

#'@export 
db_data_type.Hive2Connection <- function(con, fields) {
  vapply(fields, hive2DataType, dbObj = con, FUN.VALUE = character(1))
}

#'@export
db_create_table.src_hive2 <- function(con, table, types, temporary = FALSE, 
  stored_as = c("PARQUET", "JSON"), location = sprintf("file://%s", tempfile()), ...) {
  db_create_table(con$con, table, types, temporary, stored_as, location, ...)
}

#'@export
db_create_table.Hive2Connection <- function(con, table, types, temporary = FALSE, 
  stored_as = c("PARQUET", "JSON"), location = sprintf("file://%s", tempfile()), ...) {
  assertthat::assert_that(assertthat::is.string(table))
  stored_as <- pmatch(stored_as[1], c("PARQUET", "JSON"))
  assertthat::assert_that(!is.na(stored_as))
  stored_as_location <- switch(stored_as, 
         "1" = sprintf("USING org.apache.spark.sql.parquet OPTIONS (PATH %s)", escape(location)),
         "2" = sprintf("USING org.apache.spark.sql.json OPTIONS (PATH %s)", escape(location))
    )
  sql <- paste("CREATE", if (temporary) "TEMPORARY" else "", "TABLE", ident(table), stored_as_location)
  DBI::dbGetQuery(con, sql)
}

#'@export
db_drop_table.src_hive2 <- function(con, table, force = FALSE, ...) {
  db_drop_table.Hive2Connection(con$con, table, force, ...)
}

#'@export
db_drop_table.Hive2Connection <- function(con, table, force = FALSE, ...) {
  DBI::dbRemoveTable(con, table)
}

#'@export
db_insert_into.Hive2Connection <- function(con, table, values, ...) {
  stop("Spark SQL does not support INSERT INTO")
#   browser()
#   cols <- lapply(values, escape, collapse = NULL, parens = FALSE, con = con)
#   col_mat <- matrix(unlist(cols, use.names = FALSE), nrow = nrow(values))
# 
#   for(i in seq_len(nrow(col_mat) / 1000)) {
#     if (i * 1000 <= nrow(col_mat)) {
#       indexes <- seq((i - 1) * 1000 + 1, i * 1000, by = 1)    
#     } else {
#       indexes <- seq((i - 1) * 1000 + 1, nrow(col_mat), by = 1)
#     }
#     rows <- apply(col_mat[indexes, ], 1, paste0, collapse = ", ")
#     values <- paste0("(", rows, ")", collapse = "\n, ")
#     sql <- paste("INSERT INTO TABLE", table, "VALUES", sql(values))
#     dbSendQuery(con, "INSERT INTO TABLE test1 VALUES (1, 1.0, 'a')")
}

#'@export
db_query_fields.src_hive2 <- function(con, sql, ...) {
  db_query_fields(con$con, sql, ...)
}

#'@export
db_query_fields.Hive2Connection <- function(con, sql, ...) {
  fields <- sprintf("SELECT * FROM %s WHERE 0=1", sql)
  df <- DBI::dbGetQuery(con, fields)
  colnames(df)
}

#'@export
src_translate_env.src_hive2 <- function(x) {
  sql_variant(base_scalar, sql_translator(.parent = base_agg,
                                          n = function() sql("count(*)")))
}

#'@export
head.tbl_hive2 <- function(x, n = 6L, ...) {
  dplyr:::build_query(x, limit = as.integer(n))$fetch()
}

wrap <- dplyr:::wrap

commas <- dplyr:::commas

#'@export
print.tbl_hive2 <- function(x, ...) {
  cat("Source: ", src_desc(x$src), "\n", sep = "")
  if (inherits(x$from, "ident")) {
    cat(paste0("From: ", x$from, " ", dim_desc(x), collapse = ""))
  }
  else {
    cat(wrap("From: <derived table> ", dim_desc(x)))
  }
  cat("\n")
  if (!is.null(x$where)) {
    cat(wrap("Filter: ", commas(x$where)), "\n")
  }
  if (!is.null(x$order_by)) {
    cat(wrap("Arrange: ", commas(x$order_by)), "\n")
  }
  if (!is.null(x$group_by)) {
    cat(wrap("Grouped by: ", commas(x$group_by)), "\n")
  }
  cat("\n")
  print(trunc_mat(x))
  invisible(x)
}

#'@export
sql_escape_ident.Hive2Connection <- function(con, x) {
  as.character(x)
}

# #'@export
# sql_escape_string.Hive2Connection <- function(con, x) {
#   print(x)
#   dplyr:::sql_escape_string.DBIConnection(con, x)
# }