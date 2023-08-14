# TODO check example
# TODO check enum
# TODO check const
# TODO check required
# TODO check default

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
