library(testthat)

test_that("can convert integer schema sucessfully", {
  expect_no_error({
    title <- "Boolean Schema"
    description <- "Schema for Boolean"
    default <- FALSE
    enum <- c(FALSE, TRUE)
    const <- FALSE
    examples <- c(FALSE, TRUE)

    schema <- js_schema(
      js_boolean(
        .title = title,
        .description = description,
        .default = default,
        .enum = enum,
        .const = const,
        .examples = examples
      )
    )

    schema_conv_read_yaml <- schema %>%
      to_json_schema(output_format = "yaml") %>%
      yaml::yaml.load()

    schema_conv_read_json <- schema %>%
      to_json_schema(output_format = "json") %>%
      jsonlite::fromJSON()

    expect_equal(schema_conv_read_json$title, title)
    expect_equal(schema_conv_read_yaml$title, title)

    expect_equal(schema_conv_read_json$description, description)
    expect_equal(schema_conv_read_yaml$description, description)

    expect_equal(schema_conv_read_json$default, default)
    expect_equal(schema_conv_read_yaml$default, default)

    expect_equal(schema_conv_read_json$enum, enum)
    expect_equal(schema_conv_read_yaml$enum, enum)

    expect_equal(schema_conv_read_json$const, const)
    expect_equal(schema_conv_read_yaml$const, const)

    expect_equal(schema_conv_read_json$examples, examples)
    expect_equal(schema_conv_read_yaml$examples, examples)
  })
})
