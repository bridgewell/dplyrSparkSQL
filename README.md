# dplyr backend for Spark SQL (Experimental)

## Introduction

Apache Spark SQL has a Thrift JDBC/ODBC server mode which implements the [HiveServer2](https://cwiki.apache.org/confluence/display/Hive/Setting+Up+HiveServer2) in Hive 0.13.
Please see the document of Apache Spark: [Distributed SQL Engine - Running the Thrift JDBC/ODBC server](https://spark.apache.org/docs/latest/sql-programming-guide.html#running-the-thrift-jdbcodbc-server) for details.

*dplyrSparkSQL* is an experimental project to build a Spark SQL backend for dplyr. 
