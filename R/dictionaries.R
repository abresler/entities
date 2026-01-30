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
#' @param url URL with most recent CSV file from GLEIF
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
dictionary_gleif_entity_types <-
  memoise::memoise(function(url = "https://www.gleif.org/lei-data/code-lists/iso-20275-entity-legal-forms-code-list/2023-09-28-elf-code-list-v1.5.csv") {
    data <-
      read_csv(url) |> janitor::clean_names()

    data <-
      data %>%
      mutate_if(is.character, list(function(x) {
        x %>% str_replace_all("\\;", "\\|")
      }))


    data <- data %>%
      mutate(
        entity_abbreviation = case_when(
          !is.na(abbreviations_transliterated) ~ abbreviations_transliterated,
          TRUE ~  abbreviations_local_language
        )
      ) %>%
      select(
        elf_code,
        country_of_formation,
        entity_legal_form_name_local_name,
        entity_legal_form_name_transliterated_name_per_iso_01_140_10,
        entity_abbreviation,
        everything()
      )

    data
  })


#' Returns descriptions for countries corporate entity types
#'
#' Note: The original data source (corporateinformation.com) is no longer available.
#' This function now returns NULL with a warning. Consider using
#' \code{dictionary_gleif_entity_types()} as an alternative source for entity type data.
#'
#' @param case text case \itemize{
#' \item `NULL`
#' \item `upper`
#' \item `lower`
#' }
#' @param remove_periods if \code{TRUE} removes periods from text
#'
#' @return \code{tibble} or NULL if data source unavailable
#' @export
#'
#' @examples
#' \dontrun{
#' dictionary_countries_legal_entity_types()
#' }
dictionary_countries_legal_entity_types <-
  function(case = "upper", remove_periods = F) {
    url <- "https://www.corporateinformation.com/Company-Extensions-Security-Identifiers.aspx"
    page <- tryCatch(
      read_html(url),
      error = function(e) {
        warning(
          "The corporateinformation.com data source is no longer available. ",
          "Consider using dictionary_gleif_entity_types() as an alternative. ",
          "Error: ", conditionMessage(e),
          call. = FALSE
        )
        return(NULL)
      }
    )
    if (is.null(page)) return(NULL)

    codes <-
      page %>% html_nodes(".StockPrice+ .bodyTxt td:nth-child(1)") %>% html_text() %>% str_squish() %>%
      clean_text(case = case, remove_periods = remove_periods)

    countries <-
      page %>% html_nodes(".StockPrice+ .bodyTxt td:nth-child(2)") %>% html_text() %>% str_squish() %>% clean_text(case = case)

    descriptions <-
      page %>% html_nodes(".StockPrice+ .bodyTxt td:nth-child(3)") %>% html_text() %>% str_squish() %>%
      clean_text(case = case)

    tibble(code_legal_entity = codes, country = countries, description = descriptions)
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
  function(case = "upper", remove_commas = T, remove_periods = T) {
    slugs <-
      dictionary_gleif_entity_types() %>%
      select(entity_abbreviation) %>%
      filter(!is.na(entity_abbreviation)) %>%
      distinct() %>%
      pull() %>%
      str_split("\\|") %>%
      flatten_chr() %>%
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
