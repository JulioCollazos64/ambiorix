.globals <- new.env(hash = TRUE)
.cache_tmpls <- new.env(hash = TRUE)

.onLoad <- function(libname, pkgname) {
  .globals$cookieParser <- default_cookie_parser
  .globals$renderer <- NULL
  .globals$cache_tmpls <- FALSE
  .globals$wsc <- list()
}
