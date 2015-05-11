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
select(users, name, favorite_color)
mutate(users, test_column = 1)
mutate(users, test_column = 1) %>% collect
group_by(people, name)

# following commands producing errors

filter(people, is.na(age))
summarise(group_by(people, name), count = length(name)) %>%
  collect
left_join(people, users, by = "name") %>%
  collect

