#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import dplyr stringr tibble stringi tidyr jsonlite readr xml2 lubridate refinr stringdist wru phonics countrycode janitor fuzzyjoin tidystringdist memoise
#' @importFrom glue glue
#' @importFrom lifecycle deprecate_warn
#' @importFrom cachem cache_disk
#' @importFrom stats setNames
#' @importFrom utils URLdecode URLencode hasName
#' @importFrom scales comma
#' @importFrom purrr map map_chr map_dfr map_dbl map_lgl reduce walk keep possibly safely discard
#' @importFrom curl curl_download
#' @importFrom rvest html_nodes html_text html_attr read_html html_table
#' @importFrom humaniformat first_name last_name middle_name suffix parse_names
#' @importFrom textclean replace_non_ascii replace_incomplete replace_curly_quote replace_white replace_emoticon replace_hash add_comma_space replace_html replace_kern
#' @import qdap
#' @import qdapRegex
#' @importFrom formattable percent
## usethis namespace: end
NULL

# Global variable declarations for NSE
utils::globalVariables(c(
  ".", ".build_folder", ".first", ".first_clean", ".first_norm",
  ".id_left", ".id_right", ".id_x", ".id_y", ".last", ".last_clean",
  ".last_norm", ".middle", ".name_left", ".name_parts", ".name_right",
  ".row_id", "category", "continent", "country.name.en", "dataParties",
  "distance", "distance_method", "entity_1", "entity_2",
  "entity_abbreviation", "frequency", "idCountry", "idDocument",
  "idRow", "isDisputed", "item", "maximum_distance", "nameActual",
  "nameCountry", "nameEntityParty", "nameFull", "nameIF", "nameLast",
  "nameParty", "nameReligion", "pctAdherants", "pctNameCountry",
  "pctRegistered", "prob", "race", "read_rda_file", "score",
  "similarity", "slugFile", "slug_ln", "state.name", "surname", "termSearch",
  "titles", "typeWRUPrediction", "v1", "v2", "value", "V1", "V2"
))
