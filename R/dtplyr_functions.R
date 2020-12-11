#' Add a dimnames function so that colnames works properly for dtplyr objects.
#' @param x a dtplyr_step object
#' @return list with NULL for rownames, colnames of the dtplyr object.
dimnames.dtplyr_step <- function(x) { list(NULL, x$vars) }

#' Is this a dtplyr table?
#' @param x an object
#' @return TRUE if class dtplyr_step, FALSE otherwise
is_dtplyr_table <- function(x) {
  x %>% inherits("dtplyr_step")
}


#' Schema (column classes) of the table
#' @param x table
#' @return named character vector of classes for each column
schema <- function(x, ...) UseMethod("schema")
schema.default <- function(x) {
  lapply(x, . %>% class %>% first) %>% unlist()
}
schema.dtplyr_step <- function(x) {
  x %>% summarize_all(~ . %>% class %>% first) %>%
    as.data.frame() %>% unlist()
}


#' Helper function to address checks to convert data to a tibble.
#' To avoid modifying the dtplyr table, the various step functions
#' need to accept tibbles or dtplyr tables.
#' Given that these are often checks done before returning the table,
#' it probably makes sense to run compute on the dtplyr table.
#' @param x an object
#' @return The object, converted to a tibble or left as a dtplyr table.
confirm_table_format <- function(x) {
  if(is_dtplyr_table(x)) return(x %>% compute())
  if (!is_tibble(x)) x <- as_tibble(x)
  return(x)
}
