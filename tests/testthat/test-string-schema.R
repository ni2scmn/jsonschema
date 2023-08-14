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
        .pattern = "^[a-z]+$"
      )
    )
  })
})

test_that("can create string schema with all attributes and parse sucessfully", {
  schema <- js_schema(
    js_string(
      .min_length = 1,
      .max_length = 10,
      .pattern = "^[a-z]+$"
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

testthat::test_that("can create string schema with enumeration", {
  demo_title <- "My title"
  demo_description <- "My description"
  demo_comment <- "My comment"
  demo_default <- "My enum 1"
  demo_enum <- c("My enum 1", "My enum 2")
  demo_examples <- c("My example 1", "My example 2")
  demo_required <- TRUE

  schema <- js_schema(
    js_string(
      .title = demo_title,
      .description = demo_description,
      .comment = demo_comment,
      .default = demo_default,
      .enum = demo_enum,
      .examples = demo_examples,
      .required = demo_required
    )
  )

  testthat::expect_equal(schema$root$title, demo_title)
  testthat::expect_equal(schema$root$description, demo_description)
  testthat::expect_equal(schema$root$comment, demo_comment)
  testthat::expect_equal(schema$root$default, demo_default)
  testthat::expect_equal(schema$root$enum, demo_enum)
  testthat::expect_equal(schema$root$examples, demo_examples)
  testthat::expect_equal(schema$root$required, demo_required)
})
