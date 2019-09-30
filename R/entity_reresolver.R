
# parties -----------------------------------------------------------------
.resolve_parties <-
  function(data) {
    column_order <- names(data)

    df_parties <-
      data %>%
      select(idDocument, dataParties) %>%
      unnest_legacy()

    if (df_parties %>% tibble::has_name("nameEntityParty")) {
      df_parties <-
        df_parties %>%
        mutate(nameParty = ifelse(is.na(nameParty), nameEntityParty, nameParty))
    }

    df_parties <-
      df_parties %>%
      select(idDocument:nameActual)

    df_distinct_parties <-
      df_parties %>%
      distinct(nameParty)

    parties <- df_distinct_parties$nameParty

    df_distinct_parties <-
      resolve_data_parties(parties = parties, variable = "nameParty")

    df_parties <-
      df_parties %>%
      left_join(df_distinct_parties, by = "nameParty")

    df_parties_new <-
      df_parties %>%
      nest(-idDocument, .key = "dataParties")

    data <-
      data %>%
      select(-dataParties) %>%
      left_join(df_parties_new, by = "idDocument") %>%
      mutate(isNULLParties = dataParties %>%  map_dbl(length) == 0) %>%
      select(one_of(names(data)))

    rm(df_parties)
    rm(df_distinct_parties)
    gc()

    data
  }



# file --------------------------------------------------------------------

.re_resolve_document <-
  function(document = "zoning lot description",
           borough = "manhattan",
           base_path = "Desktop/nyc_real_estate_data/ACRIS/") {
    old_wd <- getwd()
    setwd("~")
    doc_slugs <-
      document %>% str_to_lower() %>% str_replace_all("\\ ", "\\_")
    borough_slugs <-
      borough %>% str_to_lower() %>% str_replace_all("\\ ", "\\_")
    path <-
      glue::glue("{base_path}{doc_slugs}/{borough_slugs}/") %>% as.character()

    setwd(path)

    files <- list.files()
    glue::glue("Re-resolving parties in {borough} for {document}") %>% message()
    if (length(files) == 0) {
      if (getwd() != old_wd) {
        setwd(old_wd)
      }
        return(invisible())
      }

    data <-
      files %>%
      map_dfr(function(file) {
        glue::glue("Reading {file}") %>% message()
        file %>% read_rda_file() %>%
          mutate(slugFile = file) %>%
          select(slugFile, everything())
      })

    .resolve_parties_safe <- possibly(.resolve_parties, tibble())

    data <- .resolve_parties_safe(data = data)

    files <- data$slugFile %>% unique()

    files %>%
      walk(function(file){
        glue::glue("Saving {file}") %>% message()
        data %>%
          filter(slugFile == file) %>%
          select(-slugFile) %>%
          save(file = file)
      })

    rm(data)
    gc()

    BRRR::skrrrahh(sound = 1:500 %>% sample(1))

    glue::glue("Finished re-resolving {document} in {borough}") %>% message()

    if (getwd() != old_wd) {
      setwd(old_wd)
    }
    return(invisible())
  }

#' Re - Resolve ACRIS Parties
#'
#' @param base_path base ACRIS data path
#' @param documents vector of document types
#' @param boroughs vector of boroughs
#'
#' @return invisible
#' @export
#'
#' @examples
re_resolve_documents <-
  function(base_path = "Desktop/nyc_real_estate_data/ACRIS/",
           documents = "ucc3 assignment",
           boroughs = c("brooklyn", "manhattan")
  ) {
    df_inputs <-
      expand.grid(document = documents,
                borough = boroughs,
                stringsAsFactors = F) %>%
      as_tibble()

    .re_resolve_document_safe  <- possibly(.re_resolve_document, tibble())

    1:nrow(df_inputs) %>%
      walk(function(x){
        df_row <- df_inputs[x,]
        .re_resolve_document(document = df_row$document,
                             borough = df_row$borough,
                             base_path = base_path)
      })

    message("Finished")
  }
