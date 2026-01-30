#' Clean Text
#'
#' Cleans and normalizes text strings with various options for removing
#' special characters, adjusting case, and removing entity type abbreviations.
#'
#' @param x character vector to clean
#' @param case text case: "upper", "lower", or NULL for no change
#' @param split_characters characters to split on and keep last element
#' @param remove_digits if \code{TRUE} removes numeric digits
#' @param remove_commas if \code{TRUE} removes commas
#' @param remove_periods if \code{TRUE} removes periods
#' @param remove_html if \code{TRUE} removes HTML tags
#' @param remove_kern if \code{TRUE} removes kern characters
#' @param fix_comma_space if \code{TRUE} adds space after commas
#' @param ... additional arguments (unused)
#' @param remove_bracketed_text if \code{TRUE} removes text in brackets
#' @param remove_dash if \code{TRUE} removes dashes
#' @param remove_symbols if \code{TRUE} removes symbols
#' @param remove_ampersand if \code{TRUE} removes ampersands
#' @param remove_entity_types if \code{TRUE} removes entity type abbreviations
#' @param custom_regex custom regex patterns to remove
#' @param keep_entity_type entity types to keep when removing entity types
#'
#' @return A cleaned character vector
#' @export
#'
#' @examples
#' clean_text("ACME, INC.")
#' clean_text("Hello World  ", case = "lower")
clean_text <- function(x,
                       case = NULL,
                       split_characters = NULL,
                       custom_regex = NULL,
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
                       keep_entity_type = c("Association"),
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

  if (length(custom_regex) > 0 ) {
    custom_slugs <-
      glue("\\b{custom_regex}\\b") %>%
      as.character() %>%
      str_c(collapse = "|")

    x <- x %>% str_remove_all(custom_slugs) %>% str_squish()
  }

  if (remove_bracketed_text) {
    x <- x %>% bracketX()
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
      x %>% replace_symbol()
  }

  if (remove_dash) {
    x <-
      x %>% str_remove_all("\\-")
  }

  if (fix_comma_space) {
    x <-
      x %>% add_comma_space()
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
    abbrvs <- entity_abbreviations(case = case, remove_commas = remove_commas, remove_periods = remove_periods)

    if (length(keep_entity_type) > 0) {
      abbrvs <-       abbrvs %>%
        discard(function(x) {
          x %>% str_to_upper() %>% str_detect(str_to_upper(keep_entity_type))
        })
    }

    abbrvs <- abbrvs %>% str_c(collapse = "|")
    x <- x %>% str_remove_all(pattern = abbrvs) %>% str_squish()
  }

  x
}


#' Clean Variables
#'
#' Cleans multiple columns in a data frame using \code{clean_text()}.
#'
#' @param data a data frame or tibble
#' @param variables character vector of column names to clean
#' @param all_character_columns if \code{TRUE} cleans all character columns
#' @param exclude_url_columns if \code{TRUE} excludes columns starting with "url"
#' @param case text case: "upper", "lower", or NULL
#' @param split_characters characters to split on
#' @param remove_bracketed_text if \code{TRUE} removes bracketed text
#' @param remove_digits if \code{TRUE} removes numeric digits
#' @param remove_commas if \code{TRUE} removes commas
#' @param remove_periods if \code{TRUE} removes periods
#' @param remove_html if \code{TRUE} removes HTML tags
#' @param remove_dash if \code{TRUE} removes dashes
#' @param remove_kern if \code{TRUE} removes kern characters
#' @param remove_symbols if \code{TRUE} removes symbols
#' @param fix_comma_space if \code{TRUE} adds space after commas
#' @param remove_entity_types if \code{TRUE} removes entity abbreviations
#' @param ... additional arguments passed to clean_text
#' @param snake_names if \code{TRUE} converts column names to snake_case
#' @param overwrite_variables if \code{TRUE} overwrites original columns
#' @param custom_regex custom regex patterns to remove
#' @param remove_ampersand if \code{TRUE} removes ampersands
#' @param keep_entity_type entity types to keep when removing entity types
#'
#' @return A tibble with cleaned variables
#' @export
#'
#' @examples
#' \dontrun{
#' data %>% tbl_clean_variables(variables = c("name", "company"))
#' }
tbl_clean_variables <-
  function(data,
           variables = NULL,
           all_character_columns = F,
           exclude_url_columns = T,
           case = "upper",
           split_characters = NULL,
           custom_regex = NULL,
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
           keep_entity_type = c("Association"),
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
            remove_entity_types = remove_entity_types,
            remove_ampersand = remove_ampersand,
            custom_regex = custom_regex,
            keep_entity_type = keep_entity_type,
            ...
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
            remove_entity_types = remove_entity_types,
            custom_regex = custom_regex,
            remove_ampersand = remove_ampersand,
            keep_entity_type = keep_entity_type,
            ...
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
        clean_names()
    }

    data
  }
