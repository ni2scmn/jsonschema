test_that("can create object schema", {
  expect_no_error({
    js_schema(
      js_object()
    )
  })
})

test_that("can create object schema with all attributes", {
  expect_no_error({
    js_schema(
      js_object(
        myname = js_string(.required = TRUE),
        myage = js_integer(.required = FALSE),
        .min_properties = 1,
        .max_properties = 10,
        .pattern_properties = "^[a-z]+$",
        .additional_properties = TRUE,
        .required = TRUE
      )
    )
  })
})

test_that("can create object schema with all attributes and parse sucessfully", {
  schema <- js_schema(
    js_object(
      myname = js_string(.required = TRUE),
      myage = js_integer(.required = FALSE),
      .min_properties = 1,
      .max_properties = 10,
      .additional_properties = FALSE,
      .required = TRUE
    )
  )

  request_valid <- list(
    myname = "test",
    myage = 123
  ) %>%
    jsonlite::toJSON(auto_unbox = TRUE)

  expect_no_error({
    parse(schema, request_valid)
  })

  request_too_short <- list(
    myage = 123
  ) %>%
    jsonlite::toJSON(auto_unbox = TRUE)

  expect_error({
    parse(schema, request_too_short)
  })

  request_too_long <- list(
    myname = "test",
    myage = 123,
    myaddress = "123"
  ) %>%
    jsonlite::toJSON(auto_unbox = TRUE)

  expect_error({
    parse(schema, request_too_long)
  })
})
