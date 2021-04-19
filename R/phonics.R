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
#' @param data
#' @param name_columns
#' @param phonics_methods
#' @param phonics_length
#'
#' @return
#' @export
#' @import phonics
#'
#' @examples
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
