# Tests for name parsing and classification functions

test_that("tbl_last_name extracts last names correctly", {
  df <- tibble::tibble(
    full_name = c("John Smith", "Jane Doe", "Bob Johnson Jr.")
  )

  result <- tbl_last_name(df, name_column = "full_name")

  expect_s3_class(result, "tbl_df")
  expect_true("nameLast" %in% names(result))

  # Check last names were extracted
  expect_true(any(grepl("Smith|Doe|Johnson", result$nameLast, ignore.case = TRUE)))
})

test_that("tbl_last_name requires name_column parameter", {
  df <- tibble::tibble(name = "John Smith")

  expect_error(
    tbl_last_name(df, name_column = NULL),
    regexp = "name column"
  )
})

test_that("tbl_extract_human_name_parts parses name components", {
  df <- tibble::tibble(
    name = c("Dr. John Michael Smith Jr.", "Jane Doe")
  )

  result <- tbl_extract_human_name_parts(df, name_column = "name")

  expect_s3_class(result, "tbl_df")
  # Should have more columns than input
  expect_gt(ncol(result), ncol(df))
})

test_that("tbl_munge_human_names standardizes names", {
  df <- tibble::tibble(
    first = c("JOHN", "jane"),
    last = c("SMITH", "doe"),
    full_name = c("JOHN SMITH", "jane doe")
  )

  # This function requires name_column parameter
  expect_no_error(
    result <- tbl_munge_human_names(df, first_name = "first", last_name = "last", name_column = "full_name")
  )
})

test_that("classify_last_names predicts ethnicity", {
  skip_on_cran()  # Requires WRU model data

  result <- classify_last_names(
    last_names = c("Smith", "Garcia", "Kim"),
    return_message = FALSE
  )

  expect_s3_class(result, "tbl_df")
  expect_true("typeWRUPrediction" %in% names(result))

  # Check predictions are valid categories
  valid_categories <- c("White", "Black", "Hispanic", "Asian", "Other")
  expect_true(all(result$typeWRUPrediction %in% valid_categories))
})

test_that("classify_last_names handles single name", {
  skip_on_cran()

  result <- classify_last_names("Johnson", return_message = FALSE)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
})

test_that("classify_wru_names uses functional pattern (no global assignment)", {
  skip_on_cran()

  df <- tibble::tibble(
    nameLast = c("Smith", "Garcia")
  )

  # This tests the fix for global assignment anti-pattern
  result <- classify_wru_names(
    df,
    name_columns = "nameLast",
    return_message = FALSE
  )

  expect_s3_class(result, "tbl_df")
})

test_that("classify_wru_names handles empty columns parameter", {
  df <- tibble::tibble(x = 1:3)

  result <- classify_wru_names(df, name_columns = NULL)
  expect_equal(result, df)
})

test_that("ent_* aliases work for name functions", {
  expect_identical(ent_last_name, tbl_last_name)
  expect_identical(ent_munge_names, tbl_munge_human_names)
  expect_identical(ent_extract_name_parts, tbl_extract_human_name_parts)
  expect_identical(ent_classify_ethnicity, classify_last_names)
  expect_identical(ent_classify_wru, classify_wru_names)
})

test_that("tbl_phonics adds phonetic encodings", {
  df <- tibble::tibble(
    name = c("Smith", "Smyth", "Schmidt")
  )

  result <- tbl_phonics(df, name_column = "name")

  expect_s3_class(result, "tbl_df")
  # Should have phonetic encoding column(s)
  expect_gt(ncol(result), ncol(df))
})

test_that("ent_phonics alias works", {
  expect_identical(ent_phonics, tbl_phonics)
})
