library(testthat)

test_that("can convert integer schema sucessfully", {
  expect_no_error({
    multiple_of <- 33
    minimum <- 11
    maximum <- 99
    exclusive_minimum <- 22
    exclusive_maximum <- 88
    title <- "Integer Schema"
    description <- "Schema for integers"
    default <- 55
    enum <- c(11, 22, 33)
    const <- 33
    examples <- c(11, 22, 33)

    schema <- js_schema(
      js_integer(
        .multiple_of = multiple_of,
        .minimum = minimum,
        .maximum = maximum,
        .exclusive_minimum = exclusive_minimum,
        .exclusive_maximum = exclusive_maximum,
        .title = title,
        .description = description,
        # .default = default,
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

    expect_equal(schema_conv_read_json$multipleOf, multiple_of)
    expect_equal(schema_conv_read_yaml$multipleOf, multiple_of)

    expect_equal(schema_conv_read_json$minimum, minimum)
    expect_equal(schema_conv_read_yaml$minimum, minimum)

    expect_equal(schema_conv_read_json$maximum, maximum)
    expect_equal(schema_conv_read_yaml$maximum, maximum)

    expect_equal(schema_conv_read_json$exclusiveMinimum, exclusive_minimum)
    expect_equal(schema_conv_read_yaml$exclusiveMinimum, exclusive_minimum)

    expect_equal(schema_conv_read_json$exclusiveMaximum, exclusive_maximum)
    expect_equal(schema_conv_read_yaml$exclusiveMaximum, exclusive_maximum)

    expect_equal(schema_conv_read_json$title, title)
    expect_equal(schema_conv_read_yaml$title, title)

    expect_equal(schema_conv_read_json$description, description)
    expect_equal(schema_conv_read_yaml$description, description)

    # expect_equal(schema_conv_read_json$default, default)
    # expect_equal(schema_conv_read_yaml$default, default)

    expect_equal(schema_conv_read_json$enum, enum)
    expect_equal(schema_conv_read_yaml$enum, enum)

    expect_equal(schema_conv_read_json$const, const)
    expect_equal(schema_conv_read_yaml$const, const)

    expect_equal(schema_conv_read_json$examples, examples)
    expect_equal(schema_conv_read_yaml$examples, examples)
  })
})
