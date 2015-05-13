get_driver <- function() {
  .wd0 <- getwd()
  tryCatch({
    setwd(system.file("drv", package = .packageName))
    stopifnot(system("mvn -DoutputDirectory=. dependency:copy-dependencies") == 0)
  }, finally = setwd(.wd0))
}