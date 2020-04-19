.refine_column <-
  function(data,
           entity_column = NULL,
           use_business_suffix = T,
           phonics_methods = "soundex",
           phonics_length = 16L,
           use_n_gram_merge = T,
           edit_threshold = 1,
           ignore_words = c("UNIVERSITY", "UNV"),
           numgram = 2,
           weight = c(d = 0.33,
                      i = 0.33,
                      s = 1,
                      t = 0.5),
           ...) {

    if (length(entity_column) == 0) {
      stop("Enter entity column")
    }
    if (!data %>% hasName(entity_column)) {
      return(data)
    }
    glue::glue("Cleaning {entity_column}") %>% message()


    data <-
      data %>%
      mutate_if(is.character, str_trim)

    new_col <-
      glue::glue("{entity_column}Clean") %>% as.character()

    x <-
      (data %>% pull(entity_column)) %>% str_to_upper() %>% str_trim() %>%
      str_remove_all("\\,|\\.") %>%
      gsub("\\s+", " ", .)

    if (length(ignore_words) > 0) {
      ignores <-
        ignore_words %>% str_to_upper()

      x <-
        x %>%
        refinr::key_collision_merge(ignore_strings = ignores, bus_suffix = use_business_suffix)
    } else {
      x <-
        x %>%
        refinr::key_collision_merge(bus_suffix = use_business_suffix)
    }


    if (use_n_gram_merge) {
      x <-
        refinr::n_gram_merge(
          vect = x,
          ignore_strings = ignore_words,
          edit_threshold = edit_threshold,
          numgram = numgram,
          bus_suffix = use_business_suffix,
          weight =  weight,
          ...
        )
    }


    data <-
      data %>%
      mutate(UQ(new_col) := x)


    if (length(phonics_methods) > 0) {
      phonics_methods <- str_to_lower(phonics_methods)
      part <- entity_column %>% str_remove_all("^name|^type|^description")
      if ("soundex" %in% phonics_methods) {
        data <-
          data %>%
          mutate(UQ(glue::glue("slugSoundex{part}")) := x %>% phonics::soundex(maxCodeLen = phonics_length, clean = F))
      }

      if ("caverphone" %in% phonics_methods) {
        data <-
          data %>%
          mutate(
            UQ(glue::glue("slugCaverphone{part}")) := x %>% phonics::caverphone(maxCodeLen = phonics_length, clean = F)
          )
      }

      if ("nysiis" %in% phonics_methods) {
        data <-
          data %>%
          mutate(
            UQ(glue::glue("slugNYSIIS{part}")) := x %>% nysiis(maxCodeLen = phonics_length, clean = F)
          )
      }
      if ("metaphone" %in% phonics_methods) {
        data <-
          data %>%
          mutate(UQ(glue::glue("slugMetaphone{part}")) := x %>% metaphone(maxCodeLen = phonics_length, clean = F))
      }
    }

    data


  }

#' Refine entity columns
#'
#' @param data
#' @param entity_columns
#' @param use_business_suffix
#' @param phonics_methods
#' @param phonics_length
#' @param use_n_gram_merge
#' @param edit_threshold
#' @param ignore_words
#' @param numgram
#' @param weight
#' @param ...
#'
#' @return
#' @import refinr phonics
#' @export
#'
#' @examples
refine_columns <-
  function(data,
           entity_columns = NULL,
           use_business_suffix = T,
           phonics_methods = "soundex",
           phonics_length = 16L,
           use_n_gram_merge = T,
           edit_threshold = 1,
           ignore_words = NULL,
           numgram = 2,
           weight = c(d = 0.33,
                      i = 0.33,
                      s = 1,
                      t = 0.5),
           snake_names = F,
           ...) {
    if (length(entity_columns) == 0) {
      "No columns"
      return(data)
    }
    .refine_column_safe <- possibly(.refine_column, tibble())
    entity_columns %>%
      walk(function(entity_column) {
        data <<- .refine_column_safe(
          data = data,
          entity_column = entity_column,
          use_business_suffix = use_business_suffix,
          phonics_methods = phonics_methods,
          phonics_length = phonics_length,
          use_n_gram_merge = use_n_gram_merge ,
          edit_threshold = edit_threshold,
          ignore_words = ignore_words,
          numgram = numgram,
          weight = weight,
          ...
        )
      })

    if (snake_names) {
      data <-
        data %>%
        janitor::clean_names()
    }

    data
  }
