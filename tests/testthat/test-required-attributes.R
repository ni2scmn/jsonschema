testthat::test_that("can create and parse minimal required schema", {
  schema_required <- js_schema(
    js_numeric(
      .required = TRUE
    )
  )

  schema_not_required <- js_schema(
    js_numeric(
      .required = FALSE
    )
  )

  request_no_value <- NULL %>%
    jsonlite::toJSON(auto_unbox = TRUE, null = "null")

  request_value <- 34 %>%
    jsonlite::toJSON(auto_unbox = TRUE, null = "null")

  testthat::expect_error({
    parse(schema_required, request_no_value)
  })

  testthat::expect_no_error({
    parse(schema_required, request_value)
  })

  testthat::expect_no_error({
    parse(schema_not_required, request_no_value)
  })

  testthat::expect_no_error({
    parse(schema_not_required, request_value)
  })
})
