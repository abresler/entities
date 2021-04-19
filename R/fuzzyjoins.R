









# regex -------------------------------------------------------------------

#' Regex Join
#'
#' @param x
#' @param y
#' @param by
#' @param mode
#' @param ignore_case
#'
#' @return
#' @export
#'
#' @examples
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
#' @param x
#' @param y
#' @param by
#' @param distance
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
#' @param ... Arguments passed on to stringdist


#'
#' @return
#' @export
#'
#' @examples
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
        df_row <- df_input[row_x,]

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
#' @return
#' @export
#'
#' @examples
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
        df_row <- df_input[row_x,]

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
#' @param x
#' @param y
#' @param by
#' @param max_dist
#' @param mode
#' @param distance_col
#'
#' @return
#' @export
#'
#' @examples
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
#' @param x
#' @param y
#' @param by
#' @param match_fun
#' @param multi_by
#' @param multi_match_fun
#' @param index_match_fun
#' @param mode
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
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
                          method = c("haversine",
                                     "geo",
                                     "cosine",
                                     "meeus",
                                     "vincentysphere",
                                     "vincentyellipsoid"),
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
#' @param method Method to use for computing distance: one of "haversine" (default), "geo", "cosine", "meeus", "vincentysphere", "vincentyellipsoid"
#' @param unit Unit of distance for threshold (default "miles")

#' @param mode One of "inner", "left", "right", "full" "semi", or "anti"


#' @param distance_col If given, will add a column with this name containing the geographical distance between the two
#'
#' @param ... Extra arguments passed on to the distance method
#' @param x
#' @param y
#' @param by
#' @param max_dist
#' @param method
#' @param unit
#' @param mode
#'
#' @return
#' @export
#'
#' @examples
tbl_geo_join <-
  function(x,
           y,
           by = NULL,
           max_dist  = NULL,
           method = c("haversine",
                      "geo",
                      "cosine",
                      "meeus",
                      "vincentysphere",
                      "vincentyellipsoid"),
           unit = c("miles", "km"),
           mode = "inner",
           distance_col = NULL,
           ...) {
    df_input <-
      expand.grid(
        method = method,
        unit = unit,
        max_dist = max_dist,
        stringsAsFactors = F
      ) %>%
      as_tibble()

    1:nrow(df_input) %>%
      map_dfr(function(row_x) {
        df_row <- df_input[row_x,]

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


# variables ---------------------------------------------------------------

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
      data %>%
      filter(!is.na(!!sym(variable))) %>%
      tidy_comb_all(!!sym(variable)) %>%
      tidy_stringdist(method = method, ...) %>%
      mutate(variable) %>%
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


#' Variable String Distance
#'
#' @param data
#' @param variables
#' @param method
#' @param clean_names
#' @param return_wide
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
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
      return_wide <- F
    }

    variables %>%
      map_dfr(function(x){
        .tbl_variable_stringdist(data = data,
                                 variable = x,
                                 method = method,
                                 clean_names = clean_names,
                                 return_wide = return_wide)
      })

  }
