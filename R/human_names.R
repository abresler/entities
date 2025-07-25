



# munge -------------------------------------------------------------------


#' Name Parser
#'
#' @param data a data frame or tibble containing human names
#' @param first_name vector referencing first names
#' @param last_name vector referencing last names
#' @param name_column name of the `name` you wish to create
#'
#' @returns
#' @export
#'
#' @examples
tbl_munge_human_names <- function(data, first_name, last_name, name_column) {
  stopifnot(is.character(first_name),
            is.character(last_name),
            is.character(name_column))

  title_col <- glue("title_{name_column}")
  cat_col   <- glue("category_{name_column}")

  title_dict <- list(
    civilian = c("MR", "MSTR", "MASTER", "MRS", "MS", "MISS", "MLLE", "MME", "MX"),
    academic = c(
      "DR",
      "DOCTOR",
      "PROF",
      "PROFESSOR",
      "PHD",
      "MD",
      "DDS",
      "JD",
      "ESQ"
    ),
    military = c(
      "PVT",
      "PRIVATE",
      "PFC",
      "CORPORAL",
      "CPL",
      "SGT",
      "STAFF SGT",
      "SSG",
      "SSGT",
      "WARRANT OFFICER",
      "CW2",
      "CW3",
      "CW4",
      "CW5",
      "LT",
      "LIEUTENANT",
      "2ND LT",
      "2LT",
      "2D",
      "1ST LT",
      "1LT",
      "CAPT",
      "CAPTAIN",
      "MAJ",
      "MAJOR",
      "LT COL",
      "LTCOL",
      "LIEUTENANT COLONEL",
      "LCDR",
      "COL",
      "BRIG GEN",
      "BRIGADIER GENERAL",
      "MAJOR GENERAL",
      "LT GEN",
      "GENERAL",
      "GEN",
      "ADM",
      "ADMIRAL",
      "REAR ADM",
      "VICE ADM",
      "FLEET ADM",
      "CMDR",
      "COMMANDER",
      "CDR",
      "COMMODORE",
      "CMSGT",
      "SMSGT",
      "MSGT",
      "CHIEF MASTER SERGEANT",
      "SGT MAJ",
      "CSM",
      "1SG",
      "SENIOR ENLISTED ADVISOR",
      "SEA"
    ),
    religious = c(
      "FR",
      "FATHER",
      "REV",
      "REVEREND",
      "PASTOR",
      "MINISTER",
      "RABBI",
      "IMAM",
      "BISHOP",
      "ARCHBISHOP",
      "CARDINAL",
      "MONSIGNOR",
      "POPE",
      "DEACON",
      "ELDER",
      "PROPHET",
      "SHEIKH",
      "AYATOLLAH"
    ),
    nobility = c(
      "SIR",
      "DAME",
      "LORD",
      "LADY",
      "BARON",
      "BARONESS",
      "COUNT",
      "COUNTESS",
      "DUKE",
      "DUCHESS",
      "PRINCE",
      "PRINCESS",
      "KING",
      "QUEEN",
      "MARQUIS",
      "MARQUESS",
      "MARCHIONESS",
      "EARL",
      "VISCOUNT",
      "VISCOUNTESS",
      "YOUR GRACE",
      "HIS MAJESTY",
      "HER MAJESTY",
      "RT HON",
      "HON",
      "HONOURABLE"
    )
  )

  title_aliases <- c("2LT" = "2ND LT",
                     "2D" = "2ND LT",
                     "1LT" = "1ST LT")

  all_titles <- unlist(title_dict)
  title_pattern <- regex(paste0("\\b(", paste(all_titles, collapse = "|"), ")\\b"), ignore_case = TRUE)

  title_lookup <- enframe(title_dict, name = "category", value = "titles") |>
    unnest(titles) |>
    mutate(titles = str_to_upper(titles))

  data |>
    mutate(
      across(
        c(first_name, last_name),
        ~ .x |>
          str_replace_all("[^\\x20-\\x7E]", "") |>
          str_replace_all("\\.", "") |>
          str_replace_all(";", "") |>
          str_replace_all("^,+|,+$", "") |>
          na_if("")
      )
    ) |>

    mutate(
      .first_norm = str_to_upper(.data[[first_name]]) |> str_replace_all(fixed(title_aliases)),
      .last_norm  = str_to_upper(.data[[last_name]])  |> str_replace_all(fixed(title_aliases))
    ) |>

    mutate(!!title_col := coalesce(
      str_extract(.first_norm, title_pattern),
      str_extract(.last_norm, title_pattern)
    ) |> str_to_upper()) |>

    left_join(title_lookup |> rename(!!title_col := titles), by = title_col) |>
    rename(!!cat_col := category) |>

    mutate(
      .first_clean = .first_norm |>
        str_remove_all(title_pattern) |>
        str_remove_all("^,+|,+$") |>
        str_trim() |>
        str_to_upper(),

      .last_clean = .last_norm |>
        str_remove_all(title_pattern) |>
        str_remove(",\\s?[A-Z]{2}$") |>
        str_remove_all("^,+|,+$") |>
        str_trim() |>
        str_to_upper(),

      # Remove last name from end of first name if duplicated
      .first_clean = if_else(
        !is.na(.first_clean) & !is.na(.last_clean) &
          str_detect(.first_clean, glue("\\b{.last_clean}$")),
        str_remove(.first_clean, glue("\\s*{.last_clean}$")) |> str_trim(),
        .first_clean
      ),

      .first_clean = na_if(.first_clean, ""),
      .last_clean  = na_if(.last_clean, "")
    ) |>

    mutate(
      .last_clean = if_else(.first_clean == .last_clean, NA_character_, .last_clean),
      !!name_column := case_when(
        !is.na(.first_clean) &
          !is.na(.last_clean) ~ str_c(.first_clean, .last_clean, sep = " "),
        !is.na(.first_clean) & is.na(.last_clean)  ~ .first_clean,
        is.na(.first_clean) & !is.na(.last_clean)  ~ .last_clean,
        TRUE ~ NA_character_
      )
    ) |>

    mutate(!!name_column := str_remove(!!sym(name_column), "^\\s*,?\\s*")) |>

    mutate(!!first_name := .first_clean, !!last_name  := .last_clean) |>
    select(-starts_with(".first_"),
           -starts_with(".last_"),
           -.first_norm,
           -.last_norm) |>
    relocate(all_of(c(title_col, cat_col)), .before = all_of(name_column))
}


# extract -----------------------------------------------------------------


.tbl_extract_human_name_parts <- function(data, name_column) {
  stopifnot(is.character(name_column))

  suffix <- sub("^name_", "", name_column)

  first_col  <- paste0("name_first_", suffix)
  middle_col <- paste0("name_middle_", suffix)
  last_col   <- paste0("name_last_", suffix)
  title_col  <- paste0("name_title_", suffix)
  cat_col    <- paste0("name_category_", suffix)

  title_dict <- list(
    civilian = c("MR", "MSTR", "MASTER", "MRS", "MS", "MISS", "MLLE", "MME", "MX"),
    academic = c("DR", "DOCTOR", "PROF", "PROFESSOR", "PHD", "MD", "DDS", "JD", "ESQ", "PI"),
    military = c(
      "PVT", "PRIVATE", "PFC", "CORPORAL", "CPL", "SGT", "STAFF SGT", "SSG", "SSGT",
      "WARRANT OFFICER", "CW2", "CW3", "CW4", "CW5", "LT", "LIEUTENANT",
      "2ND LT", "2LT", "2D", "1ST LT", "1LT",
      "CAPT", "CAPTAIN", "MAJ", "MAJOR", "LT COL", "LTCOL", "LIEUTENANT COLONEL", "LCDR",
      "COL", "BRIG GEN", "BRIGADIER GENERAL", "MAJOR GENERAL", "LT GEN", "GENERAL", "GEN",
      "ADM", "ADMIRAL", "REAR ADM", "VICE ADM", "FLEET ADM",
      "CMDR", "COMMANDER", "CDR", "COMMODORE",
      "CMSGT", "SMSGT", "MSGT", "CHIEF MASTER SERGEANT",
      "SGT MAJ", "CSM", "1SG",
      "SENIOR ENLISTED ADVISOR", "SEA"
    ),
    religious = c("FR", "FATHER", "REV", "REVEREND", "PASTOR", "MINISTER", "RABBI", "IMAM",
                  "BISHOP", "ARCHBISHOP", "CARDINAL", "MONSIGNOR", "POPE",
                  "DEACON", "ELDER", "PROPHET", "SHEIKH", "AYATOLLAH"),
    nobility = c("SIR", "DAME", "LORD", "LADY", "BARON", "BARONESS", "COUNT", "COUNTESS",
                 "DUKE", "DUCHESS", "PRINCE", "PRINCESS", "KING", "QUEEN",
                 "MARQUESS", "MARCHIONESS", "EARL", "VISCOUNT", "VISCOUNTESS",
                 "YOUR GRACE", "HIS MAJESTY", "HER MAJESTY", "RT HON", "HON", "HONOURABLE")
  )

  title_aliases <- c("2LT" = "2ND LT", "2D" = "2ND LT", "1LT" = "1ST LT")
  suffix_keywords <- c("JR", "JUNIOR", "SR", "SENIOR", "III", "IV")
  compound_last_patterns <- regex("ST\\.?\\s+[A-Z]+", ignore_case = TRUE)
  all_titles <- setdiff(unlist(title_dict), "MARQUIS")
  title_pattern <- regex(paste0("\\b(", paste(all_titles, collapse = "|"), ")\\b"), ignore_case = TRUE)

  title_lookup <- enframe(title_dict, name = "category", value = "titles") |>
    unnest(titles) |>
    filter(titles != "MARQUIS") |>
    mutate(titles = str_to_upper(titles))

  data |>
    mutate(
      !!name_column := .data[[name_column]] |>
        str_replace_all("[^\\x20-\\x7E]", "") |>
        str_replace_all(fixed(title_aliases)) |>
        str_replace_all('[\\.\\(\\)";]', "") |>
        str_replace_all(",\\s*PI\\b", "") |>
        str_replace_all("\"\"", NA_character_) |>
        str_squish() |> str_to_upper() |>
        str_remove_all("^-+")
    ) |>
    mutate(
      !!title_col := str_extract(.data[[name_column]], title_pattern),
      !!title_col := str_to_upper(!!sym(title_col))
    ) |>
    left_join(title_lookup |> rename(!!title_col := titles), by = title_col) |>
    rename(!!cat_col := category) |>
    mutate(
      !!name_column := str_remove_all(.data[[name_column]], title_pattern) |> str_squish() |>
        str_remove_all("^-+")
    ) |>
    mutate(
      .name_parts = str_split(.data[[name_column]], "\\s+"),
      .suffix = map_chr(.name_parts, ~ if (length(.x) > 0 && tail(.x, 1) %in% suffix_keywords) tail(.x, 1) else NA_character_),
      .name_parts = map(.name_parts, ~ if (tail(.x, 1) %in% suffix_keywords) .x[-length(.x)] else .x),
      .first = map_chr(.name_parts, ~ .x[1] %||% NA_character_),
      .last = map_chr(.name_parts, ~ {
        if (length(.x) >= 2 && .x[length(.x)] == .x[length(.x) - 1]) .x[length(.x)] else .x[length(.x)]
      }),
      .middle = map_chr(.name_parts, ~ if (length(.x) > 2) paste(.x[2:(length(.x) - 1)], collapse = " ") else NA_character_),
      .last = if_else(str_detect(.data[[name_column]], compound_last_patterns),
                      str_extract(.data[[name_column]], compound_last_patterns),
                      .last)
    ) |>
    mutate(
      across(
        c(!!name_column, .first, .middle, .last, !!title_col),
        ~ str_squish(.x) |> str_remove_all("^[\\s.,-]+|[\\s.,-]+$")
      )
    ) |>
    rename(
      !!first_col := .first,
      !!middle_col := .middle,
      !!last_col := .last
    ) |>
    select(-any_of(c(".name_parts", ".suffix"))) |>
    relocate(all_of(c(title_col, cat_col)))
}


#' Extract human name parts from a data frame
#'
#' @param data a data frame or tibble containing human names
#' @param name_columns a character vector of column names containing human names to be processed
#'
#' @returns
#' @export
#'
#' @examples
tbl_extract_human_name_parts <-
  function(data, name_columns) {
  stopifnot(is.data.frame(data))
  stopifnot(all(name_columns %in% names(data)))

  for (col in name_columns) {
    data <- .tbl_extract_human_name_parts(data, col)

    suffix <- sub("^name_", "", col)
    new_cols <- c(
      paste0("name_title_", suffix),
      paste0("name_category_", suffix),
      paste0("name_first_", suffix),
      paste0("name_middle_", suffix),
      paste0("name_last_", suffix)
    )

    orig_idx <- match(col, names(data))
    data <- data[, append(
      setdiff(seq_along(data), match(new_cols, names(data), nomatch = 0)),
      match(new_cols, names(data)),
      after = orig_idx
    )]
  }

  return(data)
}
