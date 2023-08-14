testthat::test_that("can create null schema (with all attributes)", {
  testthat::expect_no_error({
    js_schema(
      js_null()
    )
  })
})

testthat::test_that("can create null schema and validate successfully", {
  schema <- js_schema(
    js_null()
  )

  request_valid <- NULL %>%
    jsonlite::toJSON(auto_unbox = TRUE, null = "null")

  testthat::expect_no_error({
    parse(schema, request_valid)
  })

  request_invalid_false <- FALSE %>%
    jsonlite::toJSON(auto_unbox = TRUE)

  testthat::expect_error({
    parse(schema, request_invalid_false)
  })

  request_invalid_na <- NA %>%
    jsonlite::toJSON(auto_unbox = TRUE)

  testthat::expect_no_error({
    parse(schema, request_invalid_na)
  })

  request_invalid_int <- 0 %>%
    jsonlite::toJSON(auto_unbox = TRUE)

  testthat::expect_error(
    parse(schema, request_invalid_int)
  )
})


testthat::test_that("can create null schema with all common attributes", {
  # TODO check no examples is valid
  # TODO check no enum is valid
  # TODO check no const is valid
  # TODO check no default is valid
  # TODO check required is valid

  demo_title <- "My title"
  demo_description <- "My description"
  demo_comment <- "My comment"

  schema <- js_schema(
    js_null(
      .title = demo_title,
      .description = demo_description,
      .comment = demo_comment
    )
  )

  testthat::expect_equal(schema$root$title, demo_title)
  testthat::expect_equal(schema$root$description, demo_description)
  testthat::expect_equal(schema$root$comment, demo_comment)
})
