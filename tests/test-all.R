library("testthat")
library("dplyrSparkSQL")

if (Sys.getenv("LOCAL_SPARK_SERVER") == "true") test_check("dplyrSparkSQL")