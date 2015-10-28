# dplyr backend for Spark SQL

[![Build Status](https://api.travis-ci.org/wush978/dplyrSparkSQL.png)](https://travis-ci.org/wush978/dplyrSparkSQL)
[![Coverage Status](https://coveralls.io/repos/wush978/dplyrSparkSQL/badge.svg)](https://coveralls.io/r/wush978/dplyrSparkSQL)

## Introduction

Apache Spark SQL has a Thrift JDBC/ODBC server mode which implements the [HiveServer2](https://cwiki.apache.org/confluence/display/Hive/Setting+Up+HiveServer2) in Hive 0.13.
Please see the document of Apache Spark: [Distributed SQL Engine - Running the Thrift JDBC/ODBC server](https://spark.apache.org/docs/latest/sql-programming-guide.html#running-the-thrift-jdbcodbc-server) for details.

*dplyrSparkSQL* is an experimental project to build a Spark SQL backend for dplyr. 

## Getting Started

### Install manually

1. Download the prebuild binary manually from <https://spark.apache.org/downloads.html>. 
1. Checkout `https://github.com/wush978/dplyrSparkSQL"` to `dplyrSparkSQL`.
1. Extract the `.tgz` file and copy the jars in `<spark-home>/lib` to `dplyrSparkSQL/inst/drv`
1. Install the package from `dplyrSparkSQL`.

### Install via devtools

```r
library(devtools)
install_github("bridgewell/dplyrSparkSQL")
```

If you install in this way, the `dplyrSparkSQL` will trying to download the spark binaries automatically to
retrieve the driver.

### Connect to the Spark Thrift Server

```r
src <- src_spark_sql(host = "localhost", port = "10000", user = Sys.info()["nodename"])
```

Please change the `host`, `port` and `user` accordingly. 

### Create Table

The following command create the table `people` from JSON.

```r
db_create_table(src, "people", stored_as = "JSON", temporary = TRUE, 
                location = sprintf("file://%s", system.file(file.path("examples", "people.json"), package = "dplyrSparkSQL")))
```

Note that the `src` here connects to a spark in local mode, so it could access
the file in local file system. If we connect to a real spark cluster, then the
`location` should be a directory or a file on HDFS.

The following command create a table `users` from Parquet.

```r
db_create_table(src, "users", stored_as = "PARQUET", temporary = TRUE, 
                location = sprintf("file://%s", system.file("examples/users.parquet", package = "dplyrSparkSQL")))
```

The dplyr obtains the tbl object via

```r
people <- tbl(src, "people")
users <- tbl(src, "users")
```

### dplyr features

We could apply the verbs of dplyr on `people` and `users`

```r
people
nrow(people)
filter(people, age < 20)
select(users, name, favorite_color)
mutate(users, test_column = 1)
mutate(users, test_column = 1) %>% collect
```
