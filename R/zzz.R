.globals <- new.env(hash = TRUE)
.cache_tmpls <- new.env(hash = TRUE)

.onLoad <- function(libname, pkgname) {
  .globals$renderer <- NULL
  .globals$cache_tmpls <- FALSE
  .globals$wsc <- list()
}
