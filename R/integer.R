#' Convert values to predefined integers
#'
#' `step_integer` creates a *specification* of a recipe
#'  step that will convert new data into a set of integers based
#'  on the original data values.
#'
#' @inheritParams step_center
#' @inherit step_center return
#' @param ... One or more selector functions to choose which
#'  variables will be used to create the integer variables. See
#'  [selections()] for more details. For the `tidy` method, these
#'  are not currently used.
#' @param role For model terms created by this step, what analysis
#'  role should they be assigned?. By default, the function assumes
#'  that the new columns created by the original variables will be
#'  used as predictors in a model.
#' @param key A list that contains the information needed to
#'  create integer variables for each variable contained in
#'  `terms`. This is `NULL` until the step is trained by
#'  [prep.recipe()].
#' @param strict A logical for whether the values should be returned as
#'  integers (as opposed to double).
#' @param zero_based A logical for whether the integers should start at zero and
#'  new values be appended as the largest integer.
#' @return An updated version of `recipe` with the new step added
#'  to the sequence of existing steps (if any). For the `tidy`
#'  method, a tibble with columns `terms` (the selectors or
#'  variables selected) and `value` is a _list column_ with the
#'  conversion key.
#' @keywords datagen
#' @concept preprocessing
#' @concept variable_encodings
#' @export
#' @details `step_integer` will determine the unique values of
#'  each variable from the training set (excluding missing values),
#'  order them, and then assign integers to each value. When baked,
#'  each data point is translated to its corresponding integer or a
#'  value of zero for yet unseen data (although see the `zero_based`
#'  argument above). Missing values propagate.
#'
#' Factor inputs are ordered by their levels. All others are
#'  ordered by `sort`.
#'
#' Despite the name, the new values are returned as numeric unless
#'  `strict = TRUE`, which will coerce the results to integers.
#'
#'
#' @seealso [step_factor2string()], [step_string2factor()],
#'  [step_regex()], [step_count()],
#'  [step_ordinalscore()], [step_unorder()], [step_other()]
#'  [step_novel()], [step_dummy()]
#' @examples
#' library(modeldata)
#' data(okc)
#'
#' okc$location <- factor(okc$location)
#'
#' okc_tr <- okc[1:100, ]
#' okc_tr$age[1] <- NA
#'
#' okc_te <- okc[101:105, ]
#' okc_te$age[1] <- NA
#' okc_te$diet[1] <- "fast food"
#' okc_te$diet[2] <- NA
#'
#' rec <- recipe(Class ~ ., data = okc_tr) %>%
#'   step_integer(all_predictors()) %>%
#'   prep(training = okc_tr)
#'
#' bake(rec, okc_te, all_predictors())
#' tidy(rec, number = 1)

step_integer <-
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           strict = FALSE,
           zero_based = FALSE,
           key = NULL,
           skip = FALSE,
           id = rand_id("integer")) {
    add_step(
      recipe,
      step_integer_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        strict = strict,
        zero_based = zero_based,
        key = key,
        skip = skip,
        id = id
      )
    )
  }

step_integer_new <-
  function(terms, role, trained, strict, zero_based, key, skip, id) {
    step(
      subclass = "integer",
      terms = terms,
      role = role,
      trained = trained,
      strict = strict,
      zero_based = zero_based,
      key = key,
      skip = skip,
      id = id
    )
  }

get_unique_values <- function(x, zero = FALSE) {
  if(is.factor(x)) {
    res <- levels(x)
  } else {
    res <- sort(unique(x))
  }
  res <- res[!is.na(res)]
  ints <- seq_along(res)
  if (zero) {
    ints <- ints - 1
  }
  tibble(value = res, integer = ints)
}

#' @export
prep.step_integer <- function(x, training, info = NULL, ...) {
  col_names <- eval_select_recipes(x$terms, training, info)

  keys <- training %>%
    dplyr::select(!!!col_names) %>%
    dplyr::summarize_all(~ list(sort(unique(.)))) %>% # ~ list(. %>% unique %>% sort) does not work for tibbles, only for dtplyr
    collect()



  # change factor keys to their levels; drop the extra lists
  keys <- lapply(keys, function(lst) {
    if(is.factor(lst[[1]])) {
      lst[[1]] <- levels(lst[[1]])
    }

    return(lst[[1]])
  })

  step_integer_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    strict = x$strict,
    zero_based = x$zero_based,
    key = keys,
    skip = x$skip,
    id = x$id
  )
}

map_key_to_int <- function(dat, key, key_name, strict = FALSE, zero = FALSE) {
  key_dat <- tibble(key, seq_along(key)) %>% setNames(c(key_name, "int"))
  if(!strict) key_dat$int <- as.numeric(key_dat$int)
}

map_key_to_int <- function(dat, key, strict = FALSE, zero = FALSE) {
  if (is.factor(dat))
    dat <- as.character(dat)

  res <- full_join(tibble(value = dat, .row = seq_along(dat)), key, by = "value")
  res <- dplyr::filter(res, !is.na(.row))
  res <- arrange(res, .row)
  if (zero) {
    res$integer[is.na(res$integer) & !is.na(res$value)] <-
      max(key$integer, na.rm = TRUE) + 1
  } else {
    res$integer[is.na(res$integer) & !is.na(res$value)] <- 0
  }
  if (strict)
    res$integer <- as.integer(res$integer)
  res[["integer"]]
}

#' @export
bake.step_integer <- function(object, new_data, ...) {

  for(n in names(object$key)) {
    int_col <- paste(n, "int", sep = "_")

    dat_to_join <- tibble(value = object$key[[n]],
                          int = seq_along(object$key[[n]]))
    colnames(dat_to_join) <- c(n, int_col)

    if(object$zero_based) {
      dat_to_join[[int_col]] <- dat_to_join[[int_col]] - 1
      na_replacement <- as.integer(max(dat_to_join[[int_col]]) + 1)
    } else {
      na_replacement <- 0L
    }

    if(!object$strict) {
      na_replacement <- as.numeric(na_replacement)
      dat_to_join[[int_col]] <- as.numeric(dat_to_join[[int_col]])
    }

    # env <- new.env()
    env <- environment()
    # assign(x = "n", value = n, envir = env)
    assign(x = "na_replacement", value = na_replacement, envir = env)

    lazy_mutate <- parse_quos(sprintf('case_when(is.na(%s) & !is.na(%s) ~ na_replacement,
                                        TRUE ~ %s)',
                                      int_col, n, int_col),
                              env = env) %>% setNames(int_col)

    new_data <- new_data %>%
      dplyr::left_join(y = dat_to_join) %>%
      dplyr::mutate(!!!lazy_mutate) %>%
      compute()
  }

  # drop the old columns and rename the integer columns accordingly.
  rename_vec <- paste(names(object$key), "int", sep = "_") %>% setNames(names(object$key))
  new_data %>%
    dplyr::select(-one_of(names(object$key))) %>%
    dplyr::rename(!!!rename_vec) %>%
    confirm_table_format()

}

print.step_integer <-
  function(x, width = max(20, options()$width - 20), ...) {
    if (x$trained) {
      cat("Integer encoding for ")
      cat(format_ch_vec(names(x$key), width = width))
    } else {
      cat("Integer encoding for ", sep = "")
      cat(format_selectors(x$terms, width = width))
    }
    if (x$trained)
      cat(" [trained]\n")
    else
      cat("\n")
    invisible(x)
  }

#' @rdname step_integer
#' @param x A `step_integer` object.
#' @export
tidy.step_integer <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = names(x$key), value = x$key)
  } else {
    res <- tibble(terms = sel2char(x$terms))
    res$value = NA
  }
  res$id <- x$id
  res
}
