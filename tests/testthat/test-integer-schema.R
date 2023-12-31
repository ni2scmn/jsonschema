test_that("can create integer schema", {
  testthat::expect_no_error({
    js_schema(
      js_integer()
    )
  })
})

test_that("can create integer schema with all attributes", {
  testthat::expect_no_error({
    js_schema(
      js_integer(
        .multiple_of = 2,
        .minimum = 1,
        .maximum = 10,
        .exclusive_minimum = 2,
        .exclusive_maximum = 9
      )
    )
  })
})

test_that("can create integer schema with all attributes and parse sucessfully", {
  schema <- js_schema(
    js_integer(
      .multiple_of = 2,
      .minimum = 1,
      .maximum = 10,
      .exclusive_minimum = 2,
      .exclusive_maximum = 9
    )
  )

  request_valid <- 8 %>%
    jsonlite::toJSON(auto_unbox = TRUE)

  testthat::expect_no_error({
    parse(schema, request_valid)
  })

  request_no_multiple <- 9 %>%
    jsonlite::toJSON(auto_unbox = TRUE)

  testthat::expect_error({
    parse(schema, request_no_multiple)
  })

  request_too_low <- 0 %>%
    jsonlite::toJSON(auto_unbox = TRUE)

  testthat::expect_error({
    parse(schema, request_too_low)
  })

  request_too_high <- 11 %>%
    jsonlite::toJSON(auto_unbox = TRUE)

  testthat::expect_error({
    parse(schema, request_too_high)
  })
})

testthat::test_that("can create and validate integer schema with const attribute", {
  demo_title <- "My title"
  demo_description <- "My description"
  demo_comment <- "My comment"
  demo_const <- 3

  schema <- js_schema(
    js_integer(
      .title = demo_title,
      .description = demo_description,
      .comment = demo_comment,
      .const = demo_const
    )
  )

  testthat::expect_equal(schema$root$title, demo_title)
  testthat::expect_equal(schema$root$description, demo_description)
  testthat::expect_equal(schema$root$comment, demo_comment)
  testthat::expect_equal(schema$root$const, demo_const)

  request_valid <- 3 %>%
    jsonlite::toJSON(auto_unbox = TRUE)

  testthat::expect_no_error({
    parse(schema, request_valid)
  })

  request_invalid <- 1 %>%
    jsonlite::toJSON(auto_unbox = TRUE)

  # TODO test when implemented
  # testthat::expect_error({
  #   parse(schema, request_invalid)
  # })
})
