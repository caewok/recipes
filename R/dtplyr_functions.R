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
  x <- x %>% head() %>% as_tibble()
  NextMethod()
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

#' Bind a dtplyr table with another table by row
#' Uses full join to avoid errors when using bind_rows on dtplyr tables.
#' Uses bind_rows otherwise
#' @param ... two or more tables
#' @param .name_repair unique, universal, check_unique, minimal
#' ignored for dtplyr tables
#' importFrom data.table rbindlist as.data.table
bind_rows_dtplyr <- function(...,
                             .name_repair = c("unique", "universal", "check_unique", "minimal")) {
  dots <- rlang::list2(...)
  dots <- rlang::squash_if(dots, vctrs::vec_is_list)
  dots <- purrr::discard(dots, is.null)
  is_dtplyr_table <- purrr::map_lgl(dots, is_dtplyr_table)

  if(any(is_dtplyr_table)) {
    dots[is_dtplyr_table] <- lapply(dots[is_dtplyr_table], data.table::as.data.table)
  }

  out <- data.table::rbindlist(dots)
  if(is_dtplyr_table[[1]]) out <- lazy_dt(out)
  out %>% confirm_table_format()
}


#' Drop the top list level, keeping everything under it
#' Keep the names from the top level
drop_top_list_level <- function(lst) {
   lapply(lst, function(l) { return(l[[1]]) })
}

#' Handle complete cases for dtplyr tables
complete_cases_dtplyr <- function(...) {
  args <- list(...)
  if(!any(sapply(args, is_dtplyr_table))) return(complete.cases(...))

  len <- sapply(args, function(x) {
    if(is.null(dim(x))) return(length(x))
    return(nrow(x %>% compute()))
  })

  if(any(len != len[[1]])) stop("not all arguments have the same length")

  out <- rep(TRUE, times = len) # start by assuming everything is complete unless told otherwise
  for(x in args) {
    if(is_dtplyr_table(x)) {
      # currently rowwise is not understood by dtplyr
      # mtcars %>%
      #   dplyr::mutate_all(~ is.na(.)) %>%
      #   rowwise() %>%
      #   mutate(sumVar = sum(c_across())) %>%
      #   dplyr::pull(sumVar)

      # which rows have NAs
      tmp <- x %>%
        dplyr::mutate_all(~ is.na(.)) %>%
        dplyr::mutate(sumVar = rowSums(.)) %>%
        dplyr::pull(sumVar) %>%
        as.logical()

      # which rows *do not* have NAs
      out <- out & !tmp

    } else {
      out <- out & complete.cases(x)
    }
  }

  return(out)
}

#' Trick to summarize a function with an arbitrary return.
#' For example, where the function returns an object of different sizes, like unique().
#' Returns a named list by column names.
dplyr_summarize_object <- function(dat, cols, fn, ...) {
  list_fn <- function(x, fn, ...) list(fn(x, ...))

  out <- dat %>%
    dplyr::summarize_at(cols, list_fn, fn = fn, ...) %>%
    collect()

  lapply(out, function(lst) lst[[1]])
}


dplyr_table <- function(dat, cols, ...) {
  nums <- dat %>%
    dplyr::summarize_at(cols, ~ list(table(.))) %>%
    collect()

  labels <- dat %>%
    dplyr::summarize_at(cols, ~ names(table(.)))

}
