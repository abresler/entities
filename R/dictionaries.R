# https://github.com/tkuehner/Open-Data-Sources/blob/e90ca7e657a966d2ff134d51e064277cda593000/Legal%20Entity%20Names
# https://github.com/tkuehner/Open-Data-Sources/blob/e90ca7e657a966d2ff134d51e064277cda593000/Legal%20Entity%20Names
.dictioanry_legal_entity_types <-
  function() {
    page <- read_html("https://en.wikipedia.org/wiki/List_of_legal_entity_types_by_country")
  }

#' GLEIF entity list
#'
#' Dictionary of GLEIF (Global Legal Entity Identifier Foundation) entity types
#' from the ISO 20275 Entity Legal Forms Code List.
#'
#' Results are cached to disk for cross-session persistence. Cache expires after
#' 30 days or can be cleared with \code{dictionary_gleif_entity_types_clear_cache()}.
#'
#' @param url URL with most recent CSV file from GLEIF (v1.5, September 2023)
#'
#' @return A tibble containing entity legal forms with columns including
#'   elf_code, country_of_formation, entity_legal_form_name_local_name,
#'   entity_abbreviation, and related metadata
#' @export
#'
#' @examples
#' \dontrun{
#' dictionary_gleif_entity_types()
#' }
#' @seealso \url{https://www.gleif.org/en/about-lei/iso-20275-entity-legal-forms-code-list}
dictionary_gleif_entity_types <- local({
  # Disk-based cache with 30-day expiration

  cache_dir <- file.path(tools::R_user_dir("entities", "cache"), "gleif")
  disk_cache <- cachem::cache_disk(
    dir = cache_dir,
    max_age = 60 * 60 * 24 * 30  # 30 days in seconds
  )

  memoise::memoise(
    function(url = "https://www.gleif.org/lei-data/code-lists/iso-20275-entity-legal-forms-code-list/2023-09-28-elf-code-list-v1.5.csv") {
      data <- tryCatch(
        readr::read_csv(url, show_col_types = FALSE) |> janitor::clean_names(),
        error = function(e) {
          warning(
            "Failed to download GLEIF data. Using cached version if available. ",
            "Error: ", conditionMessage(e),
            call. = FALSE
          )
          return(NULL)
        }
      )

      if (is.null(data)) {
        stop("GLEIF data unavailable and no cache exists.", call. = FALSE)
      }

      data <-
        data %>%
        dplyr::mutate(dplyr::across(
          dplyr::where(is.character),
          ~ stringr::str_replace_all(.x, "\\;", "\\|")
        ))

      data <- data %>%
        dplyr::mutate(
          entity_abbreviation = dplyr::case_when(
            !is.na(abbreviations_transliterated) ~ abbreviations_transliterated,
            TRUE ~ abbreviations_local_language
          )
        ) %>%
        dplyr::select(
          elf_code,
          country_of_formation,
          entity_legal_form_name_local_name,
          entity_legal_form_name_transliterated_name_per_iso_01_140_10,
          entity_abbreviation,
          dplyr::everything()
        )

      data
    },
    cache = disk_cache
  )
})

#' Clear GLEIF dictionary cache
#'
#' Clears the disk-based cache for \code{dictionary_gleif_entity_types()}.
#' Use this to force a fresh download of the GLEIF data.
#'
#' @return Invisible NULL
#' @export
#'
#' @examples
#' \dontrun{
#' dictionary_gleif_entity_types_clear_cache()
#' }
dictionary_gleif_entity_types_clear_cache <- function() {
  cache_dir <- file.path(tools::R_user_dir("entities", "cache"), "gleif")
  if (dir.exists(cache_dir)) {
    unlink(cache_dir, recursive = TRUE)
    message("GLEIF cache cleared. Next call will download fresh data.")
  } else {
    message("No cache to clear.")
  }
  invisible(NULL)
}


#' Returns descriptions for countries corporate entity types
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated because the original data source
#' (corporateinformation.com) is no longer available.
#' Use \code{\link{dictionary_gleif_entity_types}()} instead, which provides
#' comprehensive entity legal form data from the official ISO 20275 standard.
#'
#' @param case text case \itemize{
#' \item `NULL`
#' \item `upper`
#' \item `lower`
#' }
#' @param remove_periods if \code{TRUE} removes periods from text
#'
#' @return Always returns NULL with a deprecation warning
#' @export
#'
#' @examples
#' \dontrun{
#' # Deprecated - use instead:
#' dictionary_gleif_entity_types()
#' }
dictionary_countries_legal_entity_types <-
  function(case = "upper", remove_periods = FALSE) {
    lifecycle::deprecate_warn(
      when = "0.2.0",
      what = "dictionary_countries_legal_entity_types()",
      with = "dictionary_gleif_entity_types()",
      details = c(
        "The corporateinformation.com data source is permanently unavailable.",
        "dictionary_gleif_entity_types() provides comprehensive entity legal forms ",
        "from the official GLEIF ISO 20275 standard."
      )
    )
    NULL
  }


#' Entity Abbreviations
#'
#' Returns a vector of regex patterns for common legal entity abbreviations
#' (e.g., LLC, Inc., Ltd.) derived from the GLEIF entity types dictionary.
#'
#' @param case text case for abbreviations: "upper" or "lower"
#' @param remove_commas if \code{TRUE} removes commas from abbreviations
#' @param remove_periods if \code{TRUE} removes periods from abbreviations
#'
#' @return A character vector of regex patterns with word boundaries for
#'   matching entity type abbreviations
#' @export
#'
#' @examples
#' \dontrun{
#' entity_abbreviations()
#' }
entity_abbreviations <-
  function(case = "upper", remove_commas = TRUE, remove_periods = TRUE) {
    slugs <-
      dictionary_gleif_entity_types() %>%
      dplyr::select(entity_abbreviation) %>%
      dplyr::filter(!is.na(entity_abbreviation)) %>%
      dplyr::distinct() %>%
      dplyr::pull() %>%
      stringr::str_split("\\|") %>%
      purrr::list_c() %>%
      unique()



   slugs <- clean_text(
     x = slugs,
     case =  case,
     remove_commas = remove_commas,
     remove_periods = remove_periods,
     remove_entity_types = F
   ) %>%
     discard(function(x){
       x == ""
     }) %>%
     unique()

   glue("\\b{slugs}\\b") %>%
     as.character()


  }
