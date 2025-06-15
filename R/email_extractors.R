.extract_email_domain_url <- function(data, name_email_column) {
  stopifnot(is.data.frame(data))
  stopifnot(is.character(name_email_column))
  stopifnot(name_email_column %in% names(data))

  suffix <- sub("^name_", "", name_email_column)

  domain_col <- paste0("domain_", suffix)
  url_col    <- paste0("url_", suffix)

  data |>
    mutate(
      # Ensure email column is lower case before parsing
      !!name_email_column := str_to_lower(.data[[name_email_column]]),
      !!domain_col := str_extract(.data[[name_email_column]], "(?<=@)[^\\s>]+"),
      !!url_col := if_else(!is.na(.data[[domain_col]]),
                           paste0("https://", .data[[domain_col]]),
                           NA_character_)
    ) |>
    relocate(all_of(c(domain_col, url_col)), .after = name_email_column)
}

tbl_extract_email_domains <- function(data, name_email_columns) {
  stopifnot(is.data.frame(data))
  stopifnot(is.character(name_email_columns))
  stopifnot(all(name_email_columns %in% names(data)))

  for (col in name_email_columns) {
    data <- .extract_email_domain_url(data, col)

    suffix <- sub("^name_", "", col)
    domain_col <- paste0("domain_", suffix)
    url_col    <- paste0("url_", suffix)

    # Keep new columns next to the original input
    orig_idx <- match(col, names(data))
    data <- data[, append(
      setdiff(seq_along(data), match(c(domain_col, url_col), names(data), nomatch = 0)),
      match(c(domain_col, url_col), names(data)),
      after = orig_idx
    )]
  }

  return(data)
}
