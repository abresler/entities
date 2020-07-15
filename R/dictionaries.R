# https://github.com/tkuehner/Open-Data-Sources/blob/e90ca7e657a966d2ff134d51e064277cda593000/Legal%20Entity%20Names
# https://github.com/tkuehner/Open-Data-Sources/blob/e90ca7e657a966d2ff134d51e064277cda593000/Legal%20Entity%20Names
.dictioanry_legal_entity_types <-
  function() {
    page <- read_html("https://en.wikipedia.org/wiki/List_of_legal_entity_types_by_country")
  }

#' GLEIF entity list
#'
#' Dictionary of GLEIF entities
#'
#' @return
#' @export
#'
#' @examples
dictionary_gleif_entity_types <-
  memoise::memoise(function() {
    data <-
      "https://www.gleif.org/content/2-about-lei/7-code-lists/2-iso-20275-entity-legal-forms-code-list/2020-06-10_elf-code-list-v1.2.csv" %>%
      read_csv() %>%
      janitor::clean_names()

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
#' @param case text case \itemize{
#' \item `NULL`
#' \item `lower`
#' \item `lower`
#' }
#' @param remove_periods
#'
#' @return \code{tibble}
#' @export
#'
#' @examples
dictionary_countries_legal_entity_types <-
  memoise::memoise(function(case = "upper", remove_periods = F) {
    page <-
      read_html(
        "https://www.corporateinformation.com/Company-Extensions-Security-Identifiers.aspx"
      )

    codes <-
      page %>% html_nodes(".StockPrice+ .bodyTxt td:nth-child(1)") %>% html_text() %>% str_squish() %>%
      clean_text(case = case, remove_periods = remove_periods)

    countries <-
      page %>% html_nodes(".StockPrice+ .bodyTxt td:nth-child(2)") %>% html_text() %>% str_squish() %>% clean_text(case = case)

    descriptions <-
      page %>% html_nodes(".StockPrice+ .bodyTxt td:nth-child(3)") %>% html_text() %>% str_squish() %>%
      clean_text(case = case)

    tibble(code_legal_entity = codes, country = countries, description = descriptions)
  })


#' Entity Abbreviations
#'
#' @param case
#' @param remove_commas
#' @param remove_periods
#'
#' @return
#' @export
#'
#' @examples
entity_abbreviations <-
  memoise::memoise(function(case = "upper", remove_commas = T, remove_periods = T) {
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


  })
