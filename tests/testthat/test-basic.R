test_that("Ambiorix", {
  # default
  app <- Ambiorix$new()
  expect_s3_class(app, "Ambiorix")

  # bindings
  app <- Ambiorix$new()
  app$port <- 3000L
  app$host <- "xxx"
  expect_equal(app$port, 3000L)
  expect_equal(app$host, "xxx")

  # set
  app <- Ambiorix$new(
    port = 8080L,
    host = "127.0.0.1"
  )

  expect_equal(app$port, 8080L)
  expect_equal(app$host, "127.0.0.1")

  expect_error(app$serialiser("error"))
  expect_s3_class(
    app$serialiser(function(data) {
      return("data")
    }),
    "Ambiorix"
  )
})
