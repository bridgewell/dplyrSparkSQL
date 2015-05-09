library(dplyr)
library(dplyrSparkSQL)
library(nycflights13)

src <- src_spark_sql("localhost", "10000", Sys.info()["nodename"])

tbs <- db_list_tables(src)
if (length(tbs) > 0) {
  for(i in seq_along(tbs)) stopifnot(db_has_table(src, tbs[i]))
  for(i in seq_along(tbs)) db_drop_table(src, tbs[i])
}

db_create_table(src, "people", stored_as = "JSON", temporary = TRUE, 
                location = sprintf("file://%s", system.file("examples/people.json", package = "dplyrSparkSQL")))

people <- tbl(src, "people")
people
nrow(people)

db_create_table(src, "users", stored_as = "PARQUET", temporary = TRUE, 
                location = sprintf("file://%s", system.file("examples/users.parquet", package = "dplyrSparkSQL")))
users <- tbl(src, "users")
users
nrow(users)

filter(people, age < 20)
filter(people, is.na(age))
select(users, name, favorite_color)
mutate(users, test_column = 1)
mutate(users, test_column = 1) %>% collect

group_by(people, name)
summarise(group_by(people, name), count = length(name))

left_join(people, users, by = "name")

# ---

DBI::dbGetQuery(src$con, "SELECT * FROM people WHERE 0=1")

# ---

src2 <- src_sqlite(tempfile(), T)
flights_sqlite <- copy_to(src2, flights, temporary = FALSE, indexes = list(
  c("year", "month", "day"), "carrier", "tailnum"))
db_data_type(src2$con, "year")
# debug(dplyr:::db_query_fields.SQLiteConnection)
flights <- tbl(src2, "flights")
