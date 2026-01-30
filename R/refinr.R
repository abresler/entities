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
    glue("Cleaning {entity_column}") %>%
      as.character() %>%
      message()


    data <-
      data %>%
      mutate_if(is.character, str_trim)

    new_col <-
      glue("{entity_column}Clean") %>% as.character()

    x <-
      (data %>% pull(entity_column)) %>% str_to_upper() %>% str_trim() %>%
      str_remove_all("\\,|\\.") %>%
      gsub("\\s+", " ", .)

    if (length(ignore_words) > 0) {
      ignores <-
        ignore_words %>% str_to_upper()

      x <-
        x %>%
        key_collision_merge(ignore_strings = ignores, bus_suffix = use_business_suffix)
    } else {
      x <-
        x %>%
        key_collision_merge(bus_suffix = use_business_suffix)
    }


    if (use_n_gram_merge) {
      x <-
        n_gram_merge(
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
          mutate(UQ(glue("slugSoundex_{part}")) := x %>% soundex(maxCodeLen = phonics_length, clean = F))
      }

      if ("caverphone" %in% phonics_methods) {
        data <-
          data %>%
          mutate(
            UQ(glue("slugCaverphone_{part}")) := x %>% caverphone(maxCodeLen = phonics_length, clean = F)
          )
      }

      if ("nysiis" %in% phonics_methods) {
        data <-
          data %>%
          mutate(
            UQ(glue("slugNYSIIS_{part}")) := x %>% nysiis(maxCodeLen = phonics_length, clean = F)
          )
      }
      if ("metaphone" %in% phonics_methods) {
        data <-
          data %>%
          mutate(UQ(glue("slugMetaphone_{part}")) := x %>% metaphone(maxCodeLen = phonics_length, clean = F))
      }
    }

    data


  }

#' Refine entity columns
#'
#' @param data A data frame or tibble containing entity data
#' @param entity_columns Character vector of column names to refine
#' @param use_business_suffix Logical, whether to treat business suffixes specially (default TRUE)
#' @param phonics_methods Character vector of phonetic methods to apply (e.g., "soundex", "metaphone")
#' @param phonics_length Integer, maximum length of phonetic encodings (default 16L)
#' @param use_n_gram_merge Logical, whether to use n-gram merging (default TRUE)
#' @param edit_threshold Numeric, maximum edit distance threshold (default 1)
#' @param ignore_words Character vector of words to ignore during clustering
#' @param numgram Integer, number of grams for n-gram merge (default 2)
#' @param weight Numeric vector, indicating the weights to assign to the four edit operations (see details below), for the purpose of approximate string matching. Default values are c(d = 0.33, i = 0.33, s = 1, t = 0.5). This parameter gets passed along to the stringdist function. Must be either a numeric vector of length four, or NA.
#' @param ... Additional arguments passed to internal functions
#' @param snake_names Logical, whether to convert column names to snake_case
#'
#' @return A tibble with refined entity columns and optional phonetic encodings
#' @import refinr phonics
#' @export
#' @details The values of arg \code{weight} are edit distance values that
#'  get passed to the \code{stringdist} edit distance function. The
#'  param takes four arguments, each one is a specific type of edit, with
#'  default penalty value.
#'  \itemize{
#'  \item d: deletion, default value is 0.33
#'  \item i: insertion, default value is 0.33
#'  \item s: substitution, default value is 1
#'  \item t: transposition, default value is 0.5
#'  }
#'
#' @examples
#' \dontrun{
#' # Refine multiple entity columns simultaneously
#' entities <- tibble::tibble(
#'   supplier = c("ABC Corp", "A.B.C. Corporation", "ABC CORP."),
#'   customer = c("XYZ Ltd", "XYZ Limited", "X.Y.Z. LTD")
#' )
#' entities |> refine_columns(entity_columns = c("supplier", "customer"))
#'
#' # Generate multiple phonetic encodings for fuzzy matching
#' people <- tibble::tibble(
#'   lastName = c("Smith", "Smyth", "Schmidt", "Smithe")
#' )
#' people |> refine_columns(
#'   entity_columns = "lastName",
#'   phonics_methods = c("soundex", "metaphone"),
#'   use_business_suffix = FALSE
#' )
#'
#' # Ignore common words during clustering
#' universities <- tibble::tibble(
#'   school = c("University of Texas", "UNIV OF TEXAS", "UT Austin")
#' )
#' universities |> refine_columns(
#'   entity_columns = "school",
#'   ignore_words = c("UNIVERSITY", "UNIV", "OF"),
#'   use_business_suffix = FALSE
#' )
#' }
refine_columns <-
  function(data,
           entity_columns = NULL,
           use_business_suffix = TRUE,
           phonics_methods = "soundex",
           phonics_length = 16L,
           use_n_gram_merge = TRUE,
           edit_threshold = 1,
           ignore_words = NULL,
           numgram = 2,
           weight = c(d = 0.33,
                      i = 0.33,
                      s = 1,
                      t = 0.5),
           snake_names = TRUE,
           ...) {
    if (length(entity_columns) == 0) {
      return(data)
    }

    .refine_column_safe <- purrr::possibly(.refine_column, tibble::tibble())

    # Use reduce instead of walk with global assignment (<<-)
    # This is a pure functional approach without side effects
    data <- purrr::reduce(
      entity_columns,
      function(df, entity_column) {
        .refine_column_safe(
          data = df,
          entity_column = entity_column,
          use_business_suffix = use_business_suffix,
          phonics_methods = phonics_methods,
          phonics_length = phonics_length,
          use_n_gram_merge = use_n_gram_merge,
          edit_threshold = edit_threshold,
          ignore_words = ignore_words,
          numgram = numgram,
          weight = weight,
          ...
        )
      },
      .init = data
    )

    if (snake_names) {
      data <- janitor::clean_names(data)
    }

    data
  }
