test_that("can create string schema", {
  expect_no_error({
    js_schema(
      js_string()
    )
  })
})

test_that("can create string schema with all attributes", {
  expect_no_error({
    js_schema(
      js_string(
        .min_length = 1,
        .max_length = 10,
        .pattern = "^[a-z]+$",
        .required = TRUE
      )
    )
  })
})

test_that("can create string schema with all attributes and parse sucessfully", {
  schema <- js_schema(
    js_string(
      .min_length = 1,
      .max_length = 10,
      .pattern = "^[a-z]+$",
      .required = TRUE
    )
  )

  request_valid <- "test" %>% 
    jsonlite::toJSON(auto_unbox = TRUE)

  expect_no_error({
    parse(schema, request_valid)
  })

  request_too_short <- "" %>% 
    jsonlite::toJSON(auto_unbox = TRUE)

  expect_error({
    parse(schema, request_too_short)
  })

  request_too_long <- "this is too long" %>% 
    jsonlite::toJSON(auto_unbox = TRUE)

  expect_error({
    parse(schema, request_too_long)
  })

  request_invalid_pattern <- "123" %>% 
    jsonlite::toJSON(auto_unbox = TRUE)

  expect_error({
    parse(schema, request_invalid_pattern)
  })
})