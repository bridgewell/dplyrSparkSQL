#! /bin/bash

PACKAGE_PATH=`pwd`
ls /tmp/dplyrSparkSQLTestServer || rm -rf /tmp/dplyrSparkSQLTestServer
git clone https://github.com/wush978/dplyrSparkSQLTestServer.git /tmp/dplyrSparkSQLTestServer
cd /tmp/dplyrSparkSQLTestServer && sbt "run $PACKAGE_PATH" || mv /tmp/dplyrSparkSQLTestServer/dplyrSparkSQL.Rcheck