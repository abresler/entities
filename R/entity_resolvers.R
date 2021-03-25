.dictionary.person_names <-
  function() {
    tibble(
      nameIF = c(
        "salutation",
        "first_name",
        "middle_name",
        "last_name",
        "suffix",
        "full_name"
      ),
      nameActual = c(
        "nameSalutation",
        "nameFirst",
        "nameMiddle",
        "nameLast",
        "nameSuffix",
        "nameFull"
      )
    )
  }

#' Entity dictionary words
#'
#' Dictionary of words that indicate entity status
#'
#' @return a \code{tibble}
#' @export
#'
#' @examples
#' dictionary_entity_slugs()
dictionary_entity_slugs <-
  function() {
    words <-
      c("investors$",
        "INC.ETC$",
        "ES/OF$",
        ",INC$",
        " SAVS ",
        " FA$",
        "FEDERATION",
        " FEDERAL ",
        "FEDERATION",
        "SUPERMARKET",
        "INVESTORS$",
        " ASSN ",
        "S&L",
        "FIRST ",
        "PRUDENTIAL",
        "MORTGAGE",
        "MTGECP",
        "CREDIT$",
        " LIBERTY ",
        " FEDERAL ",
        "EPISCOPAL",
        "HSG$",
        " CP$",
        " AVE ",
        " REAL EST ",

        "UNITED NATIONS",
        "ORGANIZATNINC",
        "LNASC$",
        "ENTERPR.,INC$",
        " CP$",
        "TRCO.",
        "H.U.D.$",
        "PAINEETC$",
        " (NY) ",
        " & ", " ag$", " and ", " as ", " at ", " authority ", " bank ",
        " benefit ", " bk", " building ", " ca ", " capital", " center",
        " city ", " co", " co ", " co$", " corp", " dated ", " east ",
        " equities", " fee ", " for ", " fund ", " global ", " gp", " gp ",
        " group", " group ", " guardian ", " health ", " holdings", " in ",
        " inc", " inc ", " land ", " living ", " llc", " llc ", " llp ",
        " ltd", " majesty ", " pc", " revocable trust", " services",
        " street ", " street ", " surviving ", " the ", " to ", " trust ",
        " west ", "(estate)", "\\, ab ", "^[0-9]", "$[0-9]", "academy",
        "acquisition", "admin of est", "administrator", "$[ag]", "amended",
        "america", "american", "ancillary", "and&prptsinc", "apartment",
        "apartments", "apartments", "architect", "architects", " as ",
        "as administrator", "as receiver", "as trustee ", "assembly",
        "asset", "assoc", "associated", "associates", "assocs", "banc",
        "bancorp", "bank", "banorp", "benefit", "building", "capital",
        "catholic", "center", "charitable", "china", "chinese", " christ ",
        "church", "college", "community", "company", "congregation",
        "consulate", "corp", "corporation", "corporation", "crp", "cultural",
        "custodian", "dated", "dds", "deacons", "debtor", "defined benefit plan",
        "department", "dept ", "development", "devisee", "devisee", "dpt",
        "e/o", "enterprise", "equities", "equity", "est of ", "estate of",
        "exc", "exec", "executrix", "finance", "financial", "financing",
        "foundation", "fsb", "fund", "global", "government", "govt",
        "grammar", "group", "hadassah", "hdfc", "heir at law", "heir-at-law",
        "homes", "hospital", " house ", "housing", "housing development",
        "immobiliare", " inc "," inc$", "incorp", "incorporated", "individually",
        "institute", "insurance", "interest", "international", "inversiones",
        "investment", " jew  ", "jewish", "jv", "land title insurance",
        "land trust", "last will", "lcc", "life", "limited", "limited partnership",
        "llc", "llp", "local", "lp", "ltd", "management", "manhattan", " mers ",
        " na ", " na$", " companies ", "companies", "american",
        "memorial", "metropolitan", "mgmt", "ministers", "ministries",
        "missionaries", "mngmnt", "museum", "national", "neighborhood",
        "new york city", "nyc", "nys", "organization", "orthodox", "overseas",
        "owner", "partners", "partnership", "partnersship", "partnrs",
        "pension plan", "pentecostal", "personal", " plan ", "plc", "preservation",
        "production", "properties", "property", "qualified", "real estate",
        "realty", "referee", "relocation", "remainder", "republic", "residential",
        "restated", "retirement", "revocable", "revocable trust",
        "savings", "school", "services", "settlement", "society", "society",
        "specific", "srl", "street", "surviving", "svgs", "technologies",
        "temple", "temple", "tenant", "tenants",  "the ", "title insurance",
        "towers", "trust", "ttee", " under ", "university", "venture",
        "ltd.", " cdo ","associates", ", L.P.",
        "rlty corp", "svg", "NYCDOT", "nyc", 'svgs', 'bnk', "rlty","asscn",
        "BRD OF MGRS", "AUTHORITY", "^UNIT ",
        "ventures", "wealth", "worldwide", "yeshiva", "EQUITSGRP", " SEVENTY ") %>%
      unique() %>%
      stringr::str_to_lower() %>%
      sort()

    countries <-
      countrycode::codelist %>%
      as_tibble() %>%
      filter(!country.name.en %>% str_detect("Jordan|Chad")) %>%
      pull(country.name.en) %>%
      str_to_lower()

    countries <-
      glue::glue(" {countries} ") %>% as.character()


    continents <-
      countrycode::codelist %>%
      filter(!is.na(continent)) %>%
      pull(continent) %>%
      str_to_lower() %>%
      unique()

    continents <-
      glue::glue(" {continents} ") %>% as.character()

    entities <- c(state.name, countries, continents)

    all <-
      words %>% append(entities) %>% stringr::str_to_lower()
    all
  }

.parse_person_name <-
  function(person_name = "Harvey Weinstein",
           fix_columns = TRUE) {
    if (person_name %>% str_count("\\ ") == 0) {
      return(tibble(nameFirst = "", nameLast = person_name))
    }

    is_reversed_name <-
      (
        person_name %>% str_count("\\,") >= 1 &&
          humaniformat::suffix(x = person_name) %>% is.na()
      ) |
      (
        person_name %>% str_count("\\,") >= 1 &&
          humaniformat::salutation(x = person_name) %>% is.na()
      )


    if (is_reversed_name) {
      person_name <- person_name %>% humaniformat::format_reverse()
    }


    is_period <-
      str_count(person_name, "\\.") > 1 &&
      (person_name %>% salutation() %>% is.na()) &&
      (person_name %>% suffix() %>% is.na())


    if (is_period) {
      person_name <-
        person_name %>% humaniformat::format_period()
    }

    df_person <- person_name %>% humaniformat::parse_names()

    df_names <- .dictionary.person_names()



    actual_names <- names(df_person) %>%
      map_chr(function(x) {
        df_names %>% filter(nameIF == x) %>% pull(nameActual)
      })

    df_person %>%
      purrr::set_names(c(actual_names)) %>%
      dplyr::select(which(colMeans(is.na(.)) < 1))
  }

.parse_party_data <-
  function(party = "LIU, YUAN-CHI",
           entity_words = dictionary_entity_slugs(),
           variable = "nameGranteeBuyer") {
    if (party %>% is.na()) {
      return(invisible())
    }
    party_original <- party

    party <-
      party %>% str_replace_all(" , ", ", ") %>%
      gsub("\\s+", " ", .) %>%
      str_squish()

    hit_words <-
      entity_words %>% str_to_lower() %>% str_c(collapse = "|")

    word_lower <-
      party %>% str_replace_all("\\.", "") %>%
      str_to_lower()

    hit_slugs <- hit_words %>%
      str_to_upper() %>%
      str_c(
        " LP|ASSOCIATES|LLC|L.L.C.|LP|CORP| INC|COMPANY|COMPANIES|REALTY|TRUST|DEPARTMENT|CORPORATION|BANCORP|GOVERNMENT|INSTITUE|EQUITIES|FUND| NA | NA| BANK| BANK |NYCDOT|SVNG|BNK|BANK OF|RLTY| CORP| BK | BK|ENTERPRISESINC| ENTERPRISES|CITY |COMANY|NEW YORK|CONSTRUCTORS|CONSTRUCTRS|STREETCORP| STREET | STREET| HOLDING| HOLDING |RLTY|NATIONAL|INVESTORS|SAVINGS|PARTNERS|PARTNERSHIP|HOLDINGS|HOLDCO|L.P.|\\ LP",
        collapse = "|"
      )

    not_entity <-
      !party %>%
      str_to_upper() %>%
      str_detect(hit_slugs)

    has_comma <-
      str_detect(party, ",")

    party_length <-
      party %>% str_split("\\,") %>% flatten_chr() %>% length()


    is_entity <-
      word_lower %>%
      str_detect(hit_words)

    is_variable <- variable
    is_variable <-
      is_variable %>% substr(1, 1) %>% str_to_upper() %>%
      str_c(is_variable %>% substr(2, nchar(is_variable)))

    is_var <-
      glue::glue("is{is_variable}Entity") %>%  as.character()
    new_var <- glue::glue("{variable}Resolved") %>% as.character()
    isEstate <- party %>% str_detect("ESTATE|AS TRUSTEE")

    if (has_comma && party_length %in% c(2,3) && not_entity) {
      party <-
        party %>%
        str_replace_all("ESQ.,", "ESQ. ")
      data <-
        .parse_person_name(person_name = party) %>%
        mutate(UQ(variable) := party_original,
               UQ(new_var) := nameFull %>%  str_remove_all("\\,$"),
               UQ(is_var) := FALSE,
               isEstate)
      return(data)
    }


    if (party %>% str_count("\\ ") == 0) {
      is_entity <- TRUE
    }
    if (is_entity) {
      party_clean <-
        party_original %>% str_remove_all("\\,|\\.") %>% gsub("\\s+", " ", .) %>% str_remove_all("\\,$")

      data <-
        tibble(UQ(variable) := party_original,
               UQ(new_var) := party_clean,
               isEstate) %>%
        mutate(UQ(is_var) := TRUE)
      return(data)
    }

    party <- party %>%
      str_replace_all("ESQ.,", "ESQ. ")
    data <-
      .parse_person_name(person_name = party) %>%
      mutate(UQ(variable) := party_original,
             UQ(new_var) := nameFull %>% str_remove_all("\\,$"),
             UQ(is_var) := FALSE,
             isEstate)

    # suffixes <- c("ESQ", "MD")

    #if (has_suffix) {}

    data
  }

#' Resolve List of Parties
#'
#' @param parties vector of parties
#' @param entity_words dictionary of entity words coming from \code{dictionary_entity_slugs}
#' @param variable output variable
#' @param remove_first_last if \code{TRUE} removes first and last name
#'
#' @return a \code{tibble}
#' @export
#' @import dplyr purrr stringr humaniformat countrycode
#' @importFrom glue glue
#' @examples
#' resolve_data_parties(parties = c("LIU, YUAN-CHI", "ASBC LLC"),
#' remove_first_last = TRUE,
#' variable = "nameOwnerPrimary")
resolve_data_parties <-
  function(parties = c("LIU, YUAN-CHI", "ASBC LLC"),
           entity_words = NULL,
           remove_first_last = TRUE,
           variable = "nameOwnerPrimary",
           snake_names = F
           ) {
    options(warn = -1)
    if (length(entity_words) == 0) {
      entity_words <- dictionary_entity_slugs()
    }
    .parse_party_data_safe <-
      purrr::possibly(.parse_party_data, tibble())

    data <-
      parties %>%
      map_dfr(function(party) {
        glue::glue("Resolving: {party}") %>% message()
        .parse_party_data_safe(party = party,
                               entity_words = entity_words,
                               variable = variable)
      }) %>%
      dplyr::select(dplyr::matches("is[A-Z]"), dplyr::matches(variable), everything()) %>%
      dplyr::as_tibble() %>%
      suppressWarnings()



    if (remove_first_last) {
      data <-
        data %>%
        dplyr::select(-one_of(c("nameFull", "nameFirst", "nameMiddle", "nameLast"))) %>%
        suppressWarnings()
    }
    data <-
      data %>%
      dplyr::select(which(colMeans(is.na(.)) < 1))

    if (snake_names) {
      data <-
        data %>%
        janitor::clean_names()
    }

    data
  }
