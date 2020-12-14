#' Add a dimnames function so that colnames works properly for dtplyr objects.
#' @param x a dtplyr_step object
#' @return list with NULL for rownames, colnames of the dtplyr object.
#' @export
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


#' Take a named vector with subnames indicated by "." and convert
#' to a list with sublists for each "."
#' for example, mpg.values = 1, mpg.ordered = 2, cyl.values = 3, cyl.ordered = 4
#' becomes list(mpg = list(values = 1, ordered = 2), cyl = list(values = 3, ordered = 4))
#' Used to replace things like lapply(data, get_levels)
convert_named_vector_to_hierarchical_list <- function(vec, sep = ".") {
  top_lvls <- lapply(strsplit(names(vec), split = sep, fixed = TRUE), first) %>% unlist

  lst <- vector("list", length = length(unique(top_lvls))) %>% setNames(unique(top_lvls))
  for(lvl in unique(top_lvls)) {
    idx <- which(top_lvls == lvl)
    if(length(idx) < 1) {
      next;
    } else if(length(idx) == 1) {
      lst[[lvl]] <- vec[[idx]]

    } else {
      subvec <- vec[idx]
      names(subvec) <- lapply(strsplit(names(subvec), split = sep, fixed = TRUE), nth, n = 2) %>% unlist
      lst[[lvl]] <- convert_named_vector_to_hierarchical_list(vec = subvec, sep = sep)
    }
  }

  return(lst)
}

#' Bind a dtplyr table with another table
#' Uses join to avoid errors when using bind_cols on dtplyr tables.
#' Uses bind_cols otherwise
#' @param ... two or more tables
#' @param .name_repair unique, universal, check_unique, minimal
#' ignored for dtplyr tables
bind_cols_dtplyr <- function(...,
                             .name_repair = c("unique", "universal", "check_unique", "minimal")) {
  dots <- rlang::list2(...)
  dots <- rlang::squash_if(dots, vctrs::vec_is_list)
  dots <- purrr::discard(dots, is.null)
  is_dtplyr_table <- purrr::map_lgl(dots, recipes:::is_dtplyr_table)

  if(!any(is_dtplyr_table)) {
    return(dplyr::bind_cols(..., .name_repair = .name_repair))
  }

  if(length(dots) == 1) return(dots[[1]])

  # add index to each table, attempt to join columns
  dots <- lapply(dots, function(tbl) {
    tbl %>% dplyr::mutate(idx_bind_cols_dtplyr = row_number())
  })

  out <- dots[[1]]
  for(df in dots[2:length(dots)]) {
    out <- out %>%
      dplyr::full_join(y = df,
                       by = "idx_bind_cols_dtplyr",
                       copy = TRUE)
  }

  out %>%
    dplyr::select(-idx_bind_cols_dtplyr) %>%
    compute()
}
