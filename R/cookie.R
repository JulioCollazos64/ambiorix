#' Cookie Parser
#'
#' Parses the cookie string.
#'
#' @param req A [Request].
#' @examples
#' if (interactive()) {
#'   library(ambiorix)
#'
#'   #' Handle GET at '/greet'
#'   #'
#'   #' @export
#'   say_hello <- function(req, res) {
#'     cookies <- default_cookie_parser(req)
#'     print(cookies)
#'
#'     res$send("hello there!")
#'   }
#'
#'   app <- Ambiorix$new()
#'   app$get("/greet", say_hello)
#'   app$start()
#' }
#'
#' @return A `list` of key value pairs or cookie values.
#' @export
default_cookie_parser <- function(req) {
  cookie_new <- list()

  if (is.null(req$HTTP_COOKIE)) {
    return(cookie_new)
  }

  if (req$HTTP_COOKIE == "") {
    return(list())
  }

  split <- strsplit(req$HTTP_COOKIE, ";")[[1]]
  split <- strsplit(split, "=")
  for (i in seq_along(split)) {
    value <- trimws(split[[i]])

    if (length(value) < 2) {
      next
    }

    if (value[1] == "") {
      next
    }

    cookie_new[[value[1]]] <- value[2]
  }

  return(cookie_new)
}

#' Cookie
#'
#' Create a cookie object.
#'
#' @param name Name of the cookie.
#' @param value value of the cookie.
#' @param expires Expiry, if an integer assumes it's the number of seconds
#' from now. Otherwise accepts an object of class `POSIXct` or `Date`.
#' If a `character` string then it is set as-is and not pre-processed.
#' If unspecified, the cookie becomes a session cookie. A session finishes
#' when the client shuts down, after which the session cookie is removed.
#' @param max_age Indicates the number of seconds until the cookie expires.
#' A zero or negative number will expire the cookie immediately.
#' If both `expires` and `max_age` are set, the latter has precedence.
#' @param domain Defines the host to which the cookie will be sent.
#' If omitted, this attribute defaults to the host of the current document URL,
#' not including subdomains.
#' @param path Indicates the path that must exist in the requested URL for the
#' browser to send the Cookie header.
#' @param secure Indicates that the cookie is sent to the server only when a
#' request is made with the https: scheme (except on localhost), and therefore,
#' is more resistant to man-in-the-middle attacks.
#' @param http_only Forbids JavaScript from accessing the cookie, for example,
#' through the document.cookie property.
#' @param same_site Controls whether or not a cookie is sent with cross-origin
#' requests, providing some protection against cross-site request forgery
#' attacks (CSRF). Accepts `Strict`, `Lax`, or `None`.
#'
#' @keywords internal
#' @noRd
cookie <- function(
  name,
  value,
  expires = NULL,
  max_age = NULL,
  domain = NULL,
  path = NULL,
  secure = TRUE,
  http_only = TRUE,
  same_site = NULL
) {
  opts <- as.list(environment())
  structure(
    opts,
    class = c(
      "cookie",
      class(opts)
    )
  )
}

#' @export
print.cookie <- function(x, ...) {
  cli::cli_alert_info("A cookie: {.field {x$name}} = {.val  {x$value}}")
}
