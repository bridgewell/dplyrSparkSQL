get_driver <- function() {
  download_page <- readLines("http://www.apache.org/dyn/closer.cgi/spark/spark-1.3.1/spark-1.3.1-bin-hadoop2.6.tgz")
  link_list <- regmatches(download_page, regexec("<strong>(.*)</strong>", download_page)) %>%
    Filter(f = function(x) length(x) > 0)
  link <- link_list[[1]][2]
  download.file(link, destfile = tmp_path <- tempfile(fileext = ".tar.gz"))
  stopifnot(untar(tmp_path, files = file.path("spark-1.3.1-bin-hadoop2.6", "lib"), compressed = "gzip", verbose = TRUE, exdir = tempdir()) == 0)
  stopifnot(all(file.copy(
    dir(file.path(tempdir(), "spark-1.3.1-bin-hadoop2.6", "lib"), "^datanucleus-", full.names = TRUE),
    system.file("drv", package = .packageName))))
  stopifnot(file.copy(
    dir(file.path(tempdir(), "spark-1.3.1-bin-hadoop2.6", "lib"), "^spark-assembly-", full.names = TRUE),
    system.file("drv", package = .packageName)))
}