testthat::test_that("can create boolean schema (with all attributes)", {
  testthat::expect_no_error({
    js_schema(
      js_boolean()
    )
  })
})

testthat::test_that("can create boolean schema and validate successfully", {
  schema <- js_schema(
    js_boolean()
  )

  request_valid_true <- TRUE %>%
    jsonlite::toJSON(auto_unbox = TRUE)

  testthat::expect_no_error({
    parse(schema, request_valid_true)
  })

  request_valid_false <- FALSE %>%
    jsonlite::toJSON(auto_unbox = TRUE)

  testthat::expect_no_error({
    parse(schema, request_valid_false)
  })

  request_invalid_na <- NA %>%
    jsonlite::toJSON(auto_unbox = TRUE)

  testthat::expect_error({
    parse(schema, request_invalid_na)
  })

  request_invalid_int <- 0 %>%
    jsonlite::toJSON(auto_unbox = TRUE)

  testthat::expect_error(
    parse(schema, request_invalid_int)
  )
})
