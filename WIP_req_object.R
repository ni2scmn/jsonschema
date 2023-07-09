devtools::load_all()
library(magrittr)

# Example 1: Schema containing simple string
string_schema1 <- js_schema(
  js_string()
  )

string_object1 <- "Hallo Welt" %>% 
  jsonlite::toJSON(auto_unbox = TRUE)

parse(string_schema1, string_object1)

# Example 2: Schema containing string with min and max length
string_schema2 <- js_schema(
  js_string(
    .min_length = 3, 
    .max_length = 10
    )
)


# Example 3: Schema containing string with min and max length and pattern
string_schema3 <- js_schema(
  js_string(
    .min_length = 3,
    .max_length = 10,
    .pattern = "^[a-zA-Z0-9]*$"
  )
)


# Example 4: Schema containing numeric with min and max
numeric_schema1 <- js_schema(
  js_numeric(
    .minimum ∞= 0,
    .maximum = 100
    )
)

# Example 5: Schema containing Integer with exclusive min and max and multiple of
integer_schema1 <- js_schema(
  js_integer(
    .exclusive_minimum = 0,
    .maximum = 100,
    .multiple_of = 2
  )
)

integer_object_valid1 <- 4 %>% 
  jsonlite::toJSON(auto_unbox = TRUE)

parse(integer_schema1, integer_object_valid1)

# Example 6: Schema containing a simple object with a required string and optional numeric
object_schema1 <- js_schema(
  js_object(
    name = js_string(.required = TRUE),
    age = js_numeric()
  )
)


# Example 7: Schema containing a simple array with a required string and optional numeric
array_schema1 <- js_schema(
  js_array(
    items = js_string(.required = TRUE),
    .min_items = 1,
    .max_items = 10,
    .unique_items = TRUE
  )
)



demo_schema <- ps_schema(
  name = ps_object(
    forename = ps_string(min_length = 3),
    surname = ps_string(max_length = 10),
    required = TRUE
  ),
  age = ps_numeric(min = 0, max = 100),
  type = ps_enum(
    values = c("NEW", "EXISTING", "TBD")
  )
)

demo_object <- list(
  name = list(
    forename = "Hallo",
    surname = "World"
  ),
  age = 30,
  type = "NEW"
)

demo_object_json <- jsonlite::toJSON(demo_object)


parse(
  schema = demo_schema,
  request = demo_object_json
)
