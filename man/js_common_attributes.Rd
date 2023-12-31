% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/comp_common.R
\name{js_common_attributes}
\alias{js_common_attributes}
\title{Utility function for common schema keywords}
\usage{
js_common_attributes(
  .element,
  .title = NULL,
  .description = NULL,
  .comment = NULL,
  .examples = NULL,
  .enum = NULL,
  .const = NULL,
  .default = NULL,
  .required = FALSE
)
}
\arguments{
\item{.element}{js_schema_component}

\item{.title}{title for schema component}

\item{.description}{longer description for schema component}

\item{.comment}{'$comment' tag for internal comments of schema component}

\item{.examples}{TODO doc}

\item{.enum}{restrict element by list of valid elements}

\item{.const}{restrict element by constant value that must be met}

\item{.default}{default value for component if not explicitly set}

\item{.required}{logical indicating if element is required}
}
\value{
modified .element
}
\description{
Utility function for common schema keywords
}
