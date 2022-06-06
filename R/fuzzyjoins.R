

# regex -------------------------------------------------------------------

#' Regex Join
#'
#' Join a table with a string column by a regular expression column in another table
#'
#' @param x `tbl` X
#' @param y `tbl Y`
#' @param by Columns of each to join
#' @param mode One of "inner", "left", "right", "full" "semi", or "anti"
#' @param ignore_case Whether to be case insensitive (default no)
#'
#' @return `tibble`
#' @export
#'
#' @examples
#' library(dplyr)
#' library(entities)
#' library(ggplot2)
#' data(diamonds)
#' diamonds <- tbl_df(diamonds)
#' d <- data_frame(regex_name = c("^Idea", "mium", "Good"),
#' type = 1:3)
#' # When they are inner_joined, only Good<->Good matches
#' diamonds %>%
#' inner_join(d, by = c(cut = "regex_name"))
#'
#' # but we can regex match them
#' diamonds %>%
#' tbl_regex_join(d, by = c(cut = "regex_name"), mode = "inner")
#'
#'
tbl_regex_join <-
  function (x,
            y,
            by = NULL,
            mode = "inner",
            ignore_case = FALSE)  {
    data <-
      regex_join(
        x = x,
        y = y,
        by = by,
        mode = mode,
        ignore_case = ignore_case
      ) %>%
      as_tibble()

    data
  }



# string ------------------------------------------------------------------


.tbl_stringdist_join <-
  function(x,
           y,
           by = NULL,
           max_dist = 2,
           method = "osa",
           mode = "left",
           ignore_case = FALSE,
           distance_col = NULL,
           ...) {
    data <-
      stringdist_join(
        x = x,
        y = y,
        by = by,
        max_dist = max_dist,
        method = method,
        mode = mode,
        ignore_case = ignore_case,
        distance_col = distance_col,
        ...
      )

    data <- data %>%
      mutate(distance_method = method,
             maximum_distance = max_dist) %>%
      dplyr::select(distance_method, maximum_distance, everything())

    data
  }

#' String Distance Fuzzy Joins
#'
#' Join two tables based on fuzzy string matching of their columns. This is useful, for example, in matching free-form inputs in a survey or online form, where it can catch misspellings and small personal changes.
#'
#' @param x
#' @param y
#' @param by
#' @param methods \itemize{
#' \item osa
#' \item lv
#' \item hamming
#' \item lcs
#' \item qgram
#' \item cosine
#' \item jaccard
#' \item jw
#'
#' }
#' @param mode join method \itemize{
#' \item left
#' \item right
#' \item full
#' \item inner
#' \item semi
#' \item anti
#' }
#' @param ignore_case if `TRUE` ignores case
#' @param distance_col if given, will add a column with this name containing the difference between the two
#' @param max_dist Maximum distance to use for joining
#' @param ... Arguments passed on to stringdist
#'
#' @return `tibble`
#' @export
#'
#' @examples
#' library(dplyr)
#' library(entities)
#' library(ggplot2)
#' data(diamonds)
#' d <- data_frame(approximate_name = c("Idea", "Premiums", "Premioom",
#' "VeryGood", "VeryGood", "Faiir"),
#' type = 1:6)
#' # no matches when they are inner-joined:
#' diamonds %>%
#' inner_join(d, by = c(cut = "approximate_name"))
#'
#' diamonds %>%
#' tbl_stringdist_join(d, mode = "inner", by = c(cut = "approximate_name"), distance_col = NULL)
#'
tbl_stringdist_join  <-
  function(x,
           y,
           by = NULL,
           max_dist = 2,
           methods = "osa",
           mode = "inner",
           ignore_case = FALSE,
           distance_col = "string_distance",
           ...) {
    df_input <-
      expand.grid(method = methods,
                  max_dist = max_dist,
                  stringsAsFactors = F) %>%
      as_tibble()

    1:nrow(df_input) %>%
      map_dfr(function(row_x) {
        df_row <- df_input[row_x, ]

        max_d <- df_row$max_dist
        method <- df_row$method
        .tbl_stringdist_join(
          x = x,
          y = y,
          max_dist = max_d,
          method = method,
          mode = mode,
          ignore_case = ignore_case,
          distance_col = distance_col,
          by = by,
          ...
        )

      })
  }


# distance_join -----------------------------------------------------------


.tbl_distance_join <-
  function(x,
           y,
           by = NULL,
           max_dist = 1,
           method = c("euclidean",
                      "manhattan"),
           mode = "inner",
           distance_col = NULL) {
    distance_join(
      x = x,
      y = y,
      by = by,
      max_dist = max_dist,
      method = method,
      mode = mode,
      distance_col = distance_col
    ) %>%
      as_tibble() %>%
      mutate(distance_method = method,
             maximum_distance = max_dist) %>%
      dplyr::select(distance_method, maximum_distance, everything())
  }

#' Join two tables based on a distance metric of one or more columns
#'
#' This differs from difference_join in that it considers all of the columns together when computing distance. This allows it to use metrics such as Euclidean or Manhattan that depend on multiple columns. Note that if you are computing with longitude or latitude, you probably want to use geo_join.
#'
#' @param x A tbl
#' @param y A tbl
#' @param by Columns by which to join the two tables
#' @param max_dist Maximum distance to use for joining
#' @param methods Method to use for computing distance, either `euclidean` (default) or `manhattan.`
#' @param mode One of "inner", "left", "right", "full" "semi", or "anti"
#' @param distance_col If given, will add a column with this name containing the distance between the two
#'
#' @return `tibble`
#' @export
#'
#' @examples
#' library(dplyr)
#' library(entities)
#' head(iris)
#' sepal_lengths <- data_frame(Sepal.Length = c(5, 6, 7),
#' Sepal.Width = 1:3)
#'
#' iris |>
#' tbl_distance_join(sepal_lengths, mode = "inner", max_dist = 2)
#'
#'
tbl_distance_join <-
  function(x,
           y,
           by = NULL,
           max_dist = 1,
           methods = c("euclidean",
                       "manhattan"),
           mode = "inner",
           distance_col = NULL) {
    df_input <-
      expand.grid(method = methods,
                  max_dist = max_dist,
                  stringsAsFactors = F) %>%
      as_tibble()

    1:nrow(df_input) %>%
      map_dfr(function(row_x) {
        df_row <- df_input[row_x, ]

        max_d <- df_row$max_dist
        method <- df_row$method
        .tbl_distance_join(
          x = x,
          y = y,
          max_dist = max_d,
          method = method,
          mode = mode,
          distance_col = distance_col
        )
      })
  }

# difference_join ---------------------------------------------------------


.tbl_difference_join <-
  function(x,
           y,
           by = NULL,
           max_dist = 1,
           mode = "inner",
           distance_col = NULL) {
    difference_join(
      x = x,
      y = y,
      by = by,
      max_dist = max_dist,
      mode = mode,
      distance_col = distance_col
    ) %>%
      as_tibble() %>%
      mutate(maximum_distance = max_dist) %>%
      select(maximum_distance, everything())
  }


#' Join two tables based on absolute difference between their columns
#'
#' Join two tables based on absolute difference between their columns
#'
#' @param x `tbl` x
#' @param y `tbl` y
#' @param by Columns by which to join the two tables
#' @param max_dist Maximum distance to use for joining
#' @param mode One of "inner", "left", "right", "full" "semi", or "anti"
#' @param distance_col If given, will add a column with this name containing the difference between the two
#'
#' @return `tibble(`
#' @export
#'
#' @examples
#' library(dplyr)
#' library(entities)
#' head(iris)
#' sepal_lengths <- data_frame(Sepal.Length = c(5, 6, 7),
#' Sepal.Width = 1:3)
#'
#' iris |>
#' tbl_difference_join(sepal_lengths, max_dist =  .5)
#'
tbl_difference_join <-
  function(x,
           y,
           by = NULL,
           max_dist = 1,
           mode = "inner",
           distance_col = NULL) {
    max_dist %>%
      map_dfr(function(m) {
        .tbl_difference_join(
          x = x,
          y = y,
          by = by,
          max_dist = m,
          mode = mode,
          distance_col = distance_col
        )
      })
  }

# fuzzy_join --------------------------------------------------------------


#' Fuzzy Join
#'
#' @param x `tbl` X
#' @param y `tbl` Y
#' @param by Columns of each to join
#' @param match_fun Vectorized function given two columns, returning TRUE or FALSE as to whether they are a match. Can be a list of functions one for each pair of columns specified in by (if a named list, it uses the names in x). If only one function is given it is used on all column pairs.
#' @param multi_by Columns to join, where all columns will be used to test matches together
#' @param multi_match_fun Function to use for testing matches, performed on all columns in each data frame simultaneously
#' @param index_match_fun Function to use for matching tables. Unlike match_fun and index_match_fun, this is performed on the original columns and returns pairs of indices.
#' @param mode One of "inner", "left", "right", "full" "semi", or "anti"
#' @param ... Extra arguments passed to match_fun
#'
#' @return `tbl`
#' @export
#'
#' @examples
#' library(entities)
#' tbl_fuzzy_join(x = mtcars, y = mtcars, by = c("gear" = "cyl", "carb" = "cyl"), match_fun = list(`==`, `==`), mode = 'inner')
#' tbl_fuzzy_join(mtcars, mtcars, by = "wt", match_fun = ~ .x > .y, mode = 'inner')
#'

tbl_fuzzy_join <-
  function(x,
           y,
           by = NULL,
           match_fun = NULL,
           multi_by = NULL,
           multi_match_fun = NULL,
           index_match_fun = NULL,
           mode = "inner",
           ...) {
    fuzzy_join(
      x = x,
      y = y,
      by =  by,
      match_fun = match_fun,
      multi_by = multi_by,
      multi_match_fun = multi_match_fun,
      index_match_fun = index_match_fun,
      mode = mode,
      ...
    ) %>%
      as_tibble()
  }


# geo ---------------------------------------------------------------------

.tbl_geo_join <- function(x,
                          y,
                          by = NULL,
                          max_dist  = NULL,
                          method = c("haversine"),
                          unit = c("miles", "km"),
                          mode = "inner",
                          distance_col = NULL,
                          ...) {
  if (length(max_dist) == 0) {
    stop("Enter distance")
  }

  geo_join(
    x = x,
    y = y,
    by = by,
    max_dist  = max_dist,
    method = method,
    unit = unit,
    mode = mode,
    distance_col = distance_col,
    ...
  ) %>%
    as_tibble() %>%
    mutate(
      unit = unit,
      distance_method = method,
      maximum_distance = max_dist
    ) %>%
    dplyr::select(distance_method, maximum_distance, everything())

}

#' Join two tables based on a geo distance of longitudes and latitudes
#'
#' @param x
#' @param y
#' @param by Columns by which to join the two tables
#' @param max_dist Maximum distance to use for joining
#' @param methods Method to use for computing distance: one of "haversine" (default), "geo", "cosine", "meeus", "vincentysphere", "vincentyellipsoid"
#' @param unit Unit of distance for threshold (default "miles")

#' @param mode One of "inner", "left", "right", "full" "semi", or "anti"


#' @param distance_col If given, will add a column with this name containing the geographical distance between the two
#' @param x `tbl` X
#' @param y  `tbl` T
#' @param by Columns by which to join the two tables
#' @param max_dist Maximum distance to use for joining
#' @param methods Method to use for computing distance: one of "haversine" (default), "geo", "cosine", "meeus", "vincentysphere", "vincentyellipsoid"
#' @param unit Unit of distance for threshold (default "miles")
#' @param mode One of "inner", "left", "right", "full" "semi", or "anti"
#' @param ... Extra arguments passed on to the distance method
#'
#' @return `tibble`
#' @export
#'
#' @examples
#'



tbl_geo_join <-
  function(x,
           y,
           by = NULL,
           max_dist  = NULL,
           methods = c("haversine",
                       "geo",
                       "cosine",
                       "meeus",
                       "vincentysphere",
                       "vincentyellipsoid"),
           unit = c("miles"),
           mode = "inner",
           distance_col = NULL,
           ...) {
    df_input <-
      expand.grid(
        method = methods,
        unit = unit,
        max_dist = max_dist,
        stringsAsFactors = F
      ) %>%
      as_tibble()

    1:nrow(df_input) %>%
      map_dfr(function(row_x) {
        df_row <- df_input[row_x, ]

        max_d <- df_row$max_dist
        method <- df_row$method
        unit <- df_row$unit
        .tbl_geo_join(
          x = x,
          y = y,
          max_dist = max_d,
          method = method,
          mode = mode,
          unit = unit,
          distance_col = distance_col,
          by = by,
          ...
        )

      })
  }



# combine -----------------------------------------------------------------

#' Create combonation of a variable
#'
#' @param data a `tibble`
#' @param variable variable name
#' @param override_names if `TRUE` overrides `from` `to` with actual variable name
#'
#' @return `tibble`
#' @export
#'
#' @examples
#' library(entities)
#' tbl_combine_all_variable(data = ggplot2::diamonds, variable = 'color')
#'
#'
tbl_combine_all_variable <-
  function(data, variable, override_names = F) {
    data <-
      data |>
      dplyr::select(!!!syms(variable)) |>
      distinct() |>
      mutate_if(is.factor, as.character) |>
      tidystringdist::tidy_comb_all(!!sym(variable)) |>
      setNames(c("from", "to"))

    if (override_names) {
      actual_names <- c("from", "to")
      data <- data |>
        setNames(glue("{variable}_{actual_names}") |> as.character())
    }

    data <- data |>
      mutate(variable) |>
      select(variable, everything())

    data
  }

# variables_string_distance ---------------------------------------------------------------



.tbl_variable_stringdist <-
  function(data,
           variable = NULL,
           method = c("osa",
                      "lv",
                      "dl",
                      "hamming",
                      "lcs",
                      "qgram",
                      "cosine",
                      "jaccard",
                      "jw",
                      "soundex"),
           clean_names = T,
           return_wide = T,
           ...) {
    if (length(variable) == 0) {
      stop("Enter variable")
    }
    data <-
      data |>
      filter(!is.na(!!sym(variable))) |>
      dplyr::select(!!!syms(variable)) |>
      distinct() |>
      mutate_if(is.factor, as.character) |>
      tidystringdist::tidy_comb_all(!!sym(variable)) |>
      tidy_stringdist(method = method, ...) |>
      mutate(variable) |>
      select(variable, everything())

    data <- data %>% mutate_if(is.numeric, list(function(x) {
      case_when(is.infinite(x) ~ NA_real_, TRUE ~ x)
    }))


    if (!return_wide) {
      data <- data %>%
        pivot_longer(
          cols  = data %>% select(-c(variable, V1, V2)) %>% names(),
          names_to = "metric",
          values_to = "score"
        ) %>%
        filter(!score %>% is.infinite())
    }

    if (clean_names) {
      v1 <- glue("{variable}") %>% as.character()
      v2 <- glue("{variable}_match") %>% as.character()
      data <-
        data %>%
        rename(UQ(v1) := V1,
               UQ(v2) := V2)
    }

    data
  }


#' Tidy String Distance Calculation from Variable
#'
#' Calculate tidy string distance for a set of variables
#' @param data a `tibble`
#' @param variables vector of varibles to caluclate distance
#' @param method method	one of the methods implemented in the stringdist package — "osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw", "soundex". See stringdist-metrics
#' @param clean_names if `TRUE` clean names
#' @param return_wide if `TRUE` widen long data
#' @param ... other parameters passed to stringdist
#'
#' @return
#' @export
#'
#' @examples
#' library(entities)
#' tbl_proust <- tibble(person = c("Albertine", "Françoise", "Gilberte", "Odette", "Charles"))
#' tbl_variable_stringdist(data = tbl_proust, variables = "person")
#' tbl_variable_stringdist(ggplot2::diamonds, variable = "cut")
#'

tbl_variable_stringdist <-
  function(data,
           variables = NULL,
           method = c("osa",
                      "lv",
                      "dl",
                      "hamming",
                      "lcs",
                      "qgram",
                      "cosine",
                      "jaccard",
                      "jw",
                      "soundex"),
           clean_names = T,
           return_wide = T,
           ...) {
    if (length(variables) == 0) {
      stop("Enter variables")
    }

    if (length(variables) > 1) {
      clean_names <- F
    }

    data <-
      variables %>%
      map_dfr(function(x) {
        .tbl_variable_stringdist(
          data = data,
          variable = x,
          method = method,
          clean_names = F,
          return_wide = return_wide
        )
      })

    if (clean_names) {
      data <- data |> janitor::clean_names()
    }

    data <- data |>
      rename(from = v1, to = v2)

    data

  }
