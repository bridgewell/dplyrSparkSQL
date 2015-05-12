context("left_join")

test_that("left_join works correctly", {
  
  src <- src_spark_sql("localhost", "10000", Sys.info()["nodename"])
  
  tbs <- db_list_tables(src)
  if (length(tbs) > 0) {
    for(i in seq_along(tbs)) stopifnot(db_has_table(src, tbs[i]))
    for(i in seq_along(tbs)) db_drop_table(src, tbs[i])
  }
  db_create_table(src, "people", stored_as = "JSON", temporary = TRUE, 
                  location = sprintf("file://%s", system.file("examples/people.json", package = "dplyrSparkSQL")))
  
  people <- tbl(src, "people")
  expect_equal(people %>% collect %>% as.data.frame, data.frame(age = c(NA, 30, 19, 15), name = c("Michael", "Andy", "Justin", "Alyssa"), stringsAsFactors = FALSE))
  db_create_table(src, "users", stored_as = "PARQUET", temporary = TRUE, 
                  location = sprintf("file://%s", system.file("examples/users.parquet", package = "dplyrSparkSQL")))
  users <- tbl(src, "users")
  spark_result <- left_join(people, users, by = "name") %>% arrange(name) %>% collect %>% as.data.frame
  local_result <- left_join(people %>% collect, users %>% collect, by = "name") %>% arrange(name) %>% as.data.frame
  expect_equal(spark_result, local_result)

  tbs <- db_list_tables(src)
  if (length(tbs) > 0) {
    for(i in seq_along(tbs)) stopifnot(db_has_table(src, tbs[i]))
    for(i in seq_along(tbs)) db_drop_table(src, tbs[i])
  }
  
})