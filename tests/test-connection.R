library(dplyr)
library(dplyrSparkSQL)
library(nycflights13)

src <- src_spark_sql("localhost", "10000", Sys.info()["nodename"])

tbs <- db_list_tables(src)
if (length(tbs) > 0) {
  for(i in seq_along(tbs)) stopifnot(db_has_table(src, tbs[i]))
  for(i in seq_along(tbs)) dbRemoveTable(src$con, tbs[i])
}

db_create_table(src, "people", stored_as = "JSON", temporary = TRUE, 
                location = sprintf("file://%s", system.file("examples/people.json", package = "dplyrSparkSQL")))


dbGetQuery(src$con, 'CREATE TABLE "flights" (year INT, month INT)')


# ---

src2 <- src_sqlite(tempfile(), T)
flights_sqlite <- copy_to(src2, flights, temporary = FALSE, indexes = list(
  c("year", "month", "day"), "carrier", "tailnum"))
db_data_type(src2$con, "year")
