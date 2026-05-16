#' Ambiorix
#'
#' Web server.
#'
#' @field on_stop Callback function to run when the app stops, takes no argument.
#' @field port Port to run the application.
#' @field host Host to run the application.
#' @field limit Max body size, defaults to `5 * 1024 * 1024`.
#'
#' @importFrom assertthat assert_that
#' @importFrom utils browseURL
#' @importFrom methods formalArgs
#'
#' @examples
#' app <- Ambiorix$new()
#'
#' app$get("/", function(req, res){
#'  res$send("Using {ambiorix}!")
#' })
#'
#' app$on_stop <- function(){
#'  cat("Bye!\n")
#' }
#'
#' if(interactive())
#'  app$start()
#'
#' @return An object of class `Ambiorix` from which one can
#' add routes, routers, and run the application.
#'
#' @export
Ambiorix <- R6::R6Class(
  "Ambiorix",
  inherit = Router,
  public = list(
    on_stop = NULL,
    #' @details Define the webserver.
    #'
    #' @param host A string defining the host.
    #' @param port Integer defining the port, defaults to `ambiorix.port` option: uses a random port if `NULL`.
    initialize = function(
      host = getOption("ambiorix.host", "0.0.0.0"),
      port = getOption("ambiorix.port", NULL),
      ...
    ) {
      super$initialize(...)

      private$.host <- host
      private$.port <- get_port(host, port)
    },
    #' @details Cache templates in memory instead of reading
    #' them from disk.
    cache_templates = function() {
      .globals$cache_tmpls <- TRUE
      invisible(self)
    },
    #' @details Specifies the port to listen on.
    #' @param port Port number.
    #'
    #' @examples
    #' app <- Ambiorix$new()
    #'
    #' app$listen(3000L)
    #'
    #' app$get("/", function(req, res){
    #'  res$send("Using {ambiorix}!")
    #' })
    #'
    #' if(interactive())
    #'  app$start()
    listen = function(port) {
      assert_that(not_missing(port))
      private$.port <- as.integer(port)
      invisible(self)
    },
    #' @details Start
    #' Start the webserver.
    #' @param host A string defining the host.
    #' @param port Integer defining the port, defaults to `ambiorix.port` option: uses a random port if `NULL`.
    #' @param open Whether to open the app the browser.
    #'
    #' @examples
    #' app <- Ambiorix$new()
    #'
    #' app$get("/", function(req, res){
    #'  res$send("Using {ambiorix}!")
    #' })
    #'
    #' if(interactive())
    #'  app$start(port = 3000L)
    start = function(
      port = NULL,
      host = NULL,
      open = interactive()
    ) {
      if (private$.is_running) {
        cli::cli_alert_warning("Server is already running")
        return()
      }
      if (is.null(port)) {
        port <- private$.port
      }

      if (is.null(host)) {
        host <- private$.host
      }

      port <- get_port(host, port)

      private$.server <- httpuv::startServer(
        host = host,
        port = port,
        app = list(
          call = function(req) {
            request <- Request$new(req)
            res <- Response$new()
            super$handle(request, res, routing::finalHandler(request, res))
          },
          staticPaths = private$statics,
          onWSOpen = self$websocket,
          staticPathOptions = httpuv::staticPathOptions(
            html_charset = "utf-8",
            headers = list(
              "X-UA-Compatible" = "IE=edge,chrome=1"
            )
          ),
          onHeaders = function(req) {
            size <- 0L
            if (private$.limit <= 0) {
              return(NULL)
            }

            if (length(req$CONTENT_LENGTH) > 0) {
              size <- as.numeric(req$CONTENT_LENGTH)
            } else if (length(req$HTTP_TRANSFER_ENCODING) > 0) {
              size <- Inf
            }

            if (size > private$.limit) {
              cli::cli_alert_warning("Request size exceeded, see app$limit")
              return(
                response(
                  "Maximum upload size exceeded",
                  status = 413L,
                  headers = list("Content-Type" = "text/plain")
                )
              )
            }

            return(NULL)
          }
        )
      )

      browser_host <- switch(
        EXPR = host,
        "0.0.0.0" = "127.0.0.1",
        host
      )

      browser_url <- sprintf("http://%s:%s", browser_host, port)

      cli::cli_alert_success("Listening on {browser_url}")

      # runs
      private$.is_running <- TRUE

      # open
      browse_ambiorix(open, browser_url)

      on.exit({
        self$stop()
      })

      # continually process requests:
      httpuv::service(timeoutMs = Inf)

      invisible(self)
    },
    #' @details Define Serialiser
    #' @param handler Function to use to serialise.
    #' This function should accept two arguments: the object to serialise and `...`.
    #'
    #' @examples
    #' app <- Ambiorix$new()
    #'
    #' app$serialiser(function(data, ...){
    #'  jsonlite::toJSON(x, ..., pretty = TRUE)
    #' })
    #'
    #' app$get("/", function(req, res){
    #'  res$send("Using {ambiorix}!")
    #' })
    #'
    #' if(interactive())
    #'  app$start()
    serialiser = function(handler) {
      assert_that(is_function(handler))
      options(AMBIORIX_SERIALISER = handler)
      invisible(self)
    },
    #' @details Stop
    #' Stop the webserver.
    stop = function() {
      if (!private$.is_running) {
        return(invisible())
      }

      # run on stop
      if (!is.null(self$on_stop)) {
        self$on_stop()
      }

      private$.server$stop()
      cli::cli_alert_info("Server stopped")
      private$.is_running <- FALSE

      invisible(self)
    },
    #' @details Receive Websocket Message
    #' @param name Name of message.
    #' @param handler Function to run when message is received.
    #'
    #' @examples
    #' app <- Ambiorix$new()
    #'
    #' app$get("/", function(req, res){
    #'  res$send("Using {ambiorix}!")
    #' })
    #'
    #' app$receive("hello", function(msg, ws){
    #'  print(msg) # print msg received
    #'
    #'  # send a message back
    #'  ws$send("hello", "Hello back! (sent from R)")
    #' })
    #'
    #' if(interactive())
    #'  app$start()
    receive = function(name, handler) {
      private$.receivers <- append(
        private$.receivers,
        list(WebsocketHandler$new(name, handler))
      )

      invisible(self)
    }
  ),
  active = list(
    port = function(value) {
      if (missing(value)) {
        return(private$.port)
      }

      private$.port <- as.integer(value)
    },
    host = function(value) {
      if (missing(value)) {
        return(private$.host)
      }

      private$.host <- value
    },
    limit = function(value) {
      if (missing(value)) {
        return(private$.limit)
      }

      private$.limit <- as.integer(value)
    },
    websocket = function(ws) {
      if (missing(ws) && !is.null(private$.wss_custom)) {
        return(private$.wss_custom)
      }

      if (missing(ws) && is.null(private$.wss_custom)) {
        return(private$.wss)
      }

      private$.wss_custom <- ws
      invisible(self)
    }
  ),
  private = list(
    .wss = function(ws) {
      .globals$wsc <- append(.globals$wsc, Websocket$new(ws))

      # receive
      ws$onMessage(function(binary, message) {
        # don't run if no receiver
        if (length(private$.receivers) == 0) {
          return(NULL)
        }

        message <- yyjsonr::read_json_str(message)

        for (i in seq_along(private$.receivers)) {
          if (private$.receivers[[i]]$is_handler(message)) {
            cli::cli_alert_info(
              "Received websocket message: {.val {message$name}}"
            )
            return(private$.receivers[[i]]$receive(message, ws))
          }
        }
      })
    },
    .host = "0.0.0.0",
    .port = 3000,
    .server = NULL,
    .is_running = FALSE,
    .limit = 5 * 1024 * 1024,
    .receivers = list(),
    .wss_custom = NULL
  ),
  lock_objects = FALSE
)
