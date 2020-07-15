#' Clean Text
#'
#' @param x
#' @param case
#' @param split_characters
#' @param remove_digits
#' @param remove_commas
#' @param remove_periods
#' @param remove_html
#' @param remove_kern
#' @param fix_comma_space
#' @param ...
#' @param remove_bracketed_text
#' @param remove_dash
#' @param remove_symbols
#' @param remove_ampersand
#' @param remove_entity_types
#'
#' @return
#' @export
#'
#' @examples
clean_text <- function(x,
                       case = NULL,
                       split_characters = NULL,
                       remove_bracketed_text = T,
                       remove_digits = F,
                       remove_commas = F,
                       remove_periods = F,
                       remove_html = T,
                       remove_dash = F,
                       remove_kern = F,
                       remove_symbols = F,
                       fix_comma_space = T,
                       remove_ampersand = F,
                       remove_entity_types = F,
                       ...) {
  if (class(x) != "character") {
    "Not character" %>% message()
    return(x)
  }

  x <-
    x %>% replace_non_ascii() %>%
    replace_incomplete(replacement = "") %>%
    replace_curly_quote() %>%
    replace_white() %>%
    replace_emoticon()

  if (length(split_characters) > 0) {
    split_slugs <-
      str_c(split_characters, collapse = "|")

    x <- x %>% str_split(split_slugs) %>% map_chr(function(x) {
      x[[length(x)]]
    }) %>%
      str_squish()
  }

  if (remove_bracketed_text) {
    x <- x %>% qdap::bracketX()
  }

  if (length(case) > 0) {
    x <-
      case_when(
        str_to_lower(case) %>% str_detect("upper") ~ str_to_upper(x),
        TRUE ~ str_to_lower(x)
      )
  }

  if (remove_digits) {
    x <-
      x %>% str_remove_all("[0-9]")
  }

  if (remove_symbols) {
    x <-
      x %>% qdap::replace_symbol()
  }

  if (remove_dash) {
    x <-
      x %>% str_remove_all("\\-")
  }

  if (fix_comma_space) {
    x <-
      x %>% textclean::add_comma_space()
  }

  if (remove_html) {
    x <- x %>% replace_html()
  }

  if (remove_ampersand) {
    x <- x %>% str_remove_all("\\&") %>% str_squish()
  }

  if (remove_kern) {
    x <-
      x %>% replace_kern()
  }

  if (remove_commas) {
    x <- x %>% str_remove_all("\\,")
  }

  if (remove_periods) {
    x <- x %>% str_remove_all("\\.")
  }

  if (remove_entity_types) {
    "Removing entity abbrevitions" %>% message()
    abbrvs <- entity_abbreviations(case = case, remove_commas = remove_commas, remove_periods = remove_periods) %>% str_c(collapse = "|")
    x <- x %>% str_remove_all(pattern = abbrvs) %>% str_squish()
  }

  x
}


#' Clean Variables
#'
#' @param data
#' @param variables
#' @param all_character_columns
#' @param exclude_url_columns
#' @param case
#' @param split_characters
#' @param remove_bracketed_text
#' @param remove_digits
#' @param remove_commas
#' @param remove_periods
#' @param remove_html
#' @param remove_dash
#' @param remove_kern
#' @param remove_symbols
#' @param fix_comma_space
#' @param remove_entity_types
#' @param ...
#' @param snake_names
#' @param overwrite_variables
#'
#' @return
#' @export
#'
#' @examples
tbl_clean_variables <-
  function(data,
           variables = NULL,
           all_character_columns = F,
           exclude_url_columns = T,
           case = "upper",
           split_characters = NULL,
           remove_bracketed_text = T,
           remove_digits = F,
           remove_commas = T,
           remove_periods = T,
           remove_html = T,
           remove_dash = F,
           remove_kern = F,
           remove_symbols = F,
           fix_comma_space = T,
           remove_ampersand = F,
           remove_entity_types = F,
           snake_names = T,
           overwrite_variables = F,
           ...
  ) {
    if (all_character_columns) {
      variables <- data %>% select_if(is.character) %>% names()
    }

    if (length(variables) == 0) {
      "Enter variables" %>% message()
      return(data)
    }

    if (exclude_url_columns) {
      variables <- variables %>% discard((function(x){
        x %>% str_detect("^url")
      }))
    }

    if (length(variables) == 0) {
      return(data)
    }


    if (overwrite_variables) {
      data <- data %>%
        mutate_at(variables, list(function(x) {
          clean_text(
            x = x,
            case = case,
            split_characters = case,
            remove_bracketed_text = remove_bracketed_text,
            remove_digits = remove_digits,
            remove_commas = remove_commas,
            remove_periods = remove_periods,
            remove_html = remove_html,
            remove_dash = remove_dash,
            remove_kern = remove_kern,
            remove_symbols = remove_symbols,
            fix_comma_space = fix_comma_space,
            remove_entity_types = remove_entity_types
          )
        }))
    }

    if (!overwrite_variables) {
      df_clean <- data %>%
        transmute_at(variables, list(function(x) {
          clean_text(
            x = x,
            case = case,
            split_characters = case,
            remove_bracketed_text = remove_bracketed_text,
            remove_digits = remove_digits,
            remove_commas = remove_commas,
            remove_periods = remove_periods,
            remove_html = remove_html,
            remove_dash = remove_dash,
            remove_kern = remove_kern,
            remove_symbols = remove_symbols,
            fix_comma_space = fix_comma_space,
            remove_entity_types = remove_entity_types
          )
        }))

      names(df_clean) <-
        names(df_clean) %>%
        str_c("_clean")

      data <- data %>%
        bind_cols(df_clean)
    }


    if (snake_names) {
      data <-
        data %>%
        janitor::clean_names()
    }

    data
  }
