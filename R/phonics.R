.phonics_column <-
  function(data,
          name_column = "name",
         phonics_methods = c("soundex"),
         phonics_length = 12L) {
  phonics_methods <- str_to_lower(phonics_methods)
  part <- name_column %>% str_remove_all("^name|^type|^description")
  x <- data %>% pull(name_column)
  if (part == "") {
    part <- name_column
    part <-
      str_c(substr(part, 1,1) %>% str_to_upper(), substr(part, 2, nchar(part)))
  }
  if ("soundex" %in% phonics_methods) {
    data <-
      data %>%
      mutate(UQ(glue("slugSoundex{part}")) := x %>% phonics::soundex(maxCodeLen = phonics_length, clean = F))
  }

  if ("caverphone" %in% phonics_methods) {
    data <-
      data %>%
      mutate(UQ(glue("slugCaverphone{part}")) := x %>% phonics::caverphone(maxCodeLen = phonics_length, clean = F))
  }

  if ("nysiis" %in% phonics_methods) {
    data <-
      data %>%
      mutate(UQ(glue("slugNYSIIS{part}")) := x %>% phonics::nysiis(maxCodeLen = phonics_length, clean = F))
  }
  if ("metaphone" %in% phonics_methods) {
    data <-
      data %>%
      mutate(UQ(glue("slugMetaphone{part}")) := x %>% phonics::metaphone(maxCodeLen = phonics_length, clean = F))
  }

  data
}

#' Append phonics methods to a tbl
#'
#' @param data A data frame or tibble containing name columns
#' @param name_columns Character vector of column names to generate phonetic encodings for
#' @param phonics_methods Character vector of phonetic methods to apply (e.g., "soundex", "metaphone", "caverphone", "nysiis")
#' @param phonics_length Integer, maximum length of phonetic encodings (default 12L)
#'
#' @return A tibble with additional phonetic encoding columns
#' @export
#' @import phonics
#'
#' @examples
#' \dontrun{
#' # Match person names with similar sounds but different spellings
#' people <- tibble::tibble(
#'   name_first = c("Katherine", "Catherine", "Kathryn"),
#'   name_last = c("Johnson", "Johnston", "Johnsen")
#' )
#' people |> tbl_phonics(
#'   name_columns = c("name_first", "name_last"),
#'   phonics_methods = c("soundex", "metaphone")
#' )
#'
#' # Entity deduplication with custom encoding length
#' companies <- tibble::tibble(
#'   nameEntity = c("Acme Corp", "Akme Inc", "ACME LLC")
#' )
#' companies |> tbl_phonics(
#'   name_columns = "nameEntity",
#'   phonics_methods = c("soundex", "caverphone"),
#'   phonics_length = 8L
#' )
#'
#' # Compare all available phonetic encoding methods
#' names_df <- tibble::tibble(name = c("Wright", "Right", "Rite"))
#' names_df |> tbl_phonics(
#'   name_columns = "name",
#'   phonics_methods = c("soundex", "metaphone", "caverphone", "nysiis")
#' )
#' }
tbl_phonics <-
  function(data,
           name_columns = NULL,
           phonics_methods = c("soundex"),
           phonics_length = 12L) {

    if (length(name_columns) == 0) {
      "Enter name columns" %>% message()
      return(data)
    }

    name_columns %>%
      walk(function(name_column){
        data <<-
          .phonics_column(
            data = data,
            name_column = name_column,
            phonics_methods = phonics_methods,
            phonics_length = phonics_length
          )
      })

    data
  }
