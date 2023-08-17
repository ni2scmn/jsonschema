library(testthat)

test_that("can convert object schema sucessfully", {
  expect_no_error({
    title <- "Object Schema"
    description <- "Schema for Object"
    min_properties <- 1
    max_properties <- 2

    schema <- js_schema(
      js_object(
        .title = title,
        .description = description,
        .min_properties = min_properties,
        .max_properties = max_properties,
        container = js_object(
          container_inner_a = js_object(
            a = js_integer(.title = "atitle"),
            b = js_string()
          ),
          container_inner_b = js_object(
            c = js_boolean(),
            d = js_array(
              js_integer(.title = "dtitle")
            )
          )
        )
      )
    )

    schema_conv_read_yaml <- schema %>%
      to_json_schema(output_format = "yaml") %>%
      yaml::yaml.load()

    schema_conv_read_json <- schema %>%
      to_json_schema(output_format = "json") %>%
      jsonlite::fromJSON()

    expect_equal(schema_conv_read_json$minProperties, min_properties)
    expect_equal(schema_conv_read_yaml$minProperties, min_properties)

    expect_equal(schema_conv_read_json$maxProperties, max_properties)
    expect_equal(schema_conv_read_yaml$maxProperties, max_properties)

    expect_equal(schema_conv_read_yaml$type, "object")
    expect_equal(schema_conv_read_json$type, "object")

    expect_equal(schema_conv_read_yaml$properties$container$type, "object")
    expect_equal(schema_conv_read_json$properties$container$type, "object")

    expect_equal(
      schema_conv_read_yaml$
        properties$
        container$
        properties$
        container_inner_a$
        properties$
        a$
        title,
      "atitle"
    )

    expect_equal(
      schema_conv_read_json$
        properties$
        container$
        properties$
        container_inner_a$
        properties$
        a$
        title,
      "atitle"
    )

    expect_equal(
      schema_conv_read_yaml$
        properties$
        container$
        properties$
        container_inner_b$
        properties$
        d$
        items$
        title,
      "dtitle"
    )

    expect_equal(
      schema_conv_read_yaml$
        properties$
        container$
        properties$
        container_inner_b$
        properties$
        d$
        items$
        title,
      "dtitle"
    )
  })
})
