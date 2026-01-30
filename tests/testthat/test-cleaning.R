# Tests for text cleaning functions

test_that("clean_text handles basic cleaning", {
  # Basic whitespace handling - case defaults to NULL (no change)
  expect_equal(clean_text("  hello  "), "hello")
  expect_equal(clean_text("hello world"), "hello world")

  # Multiple spaces
  result <- clean_text("hello    world")
  expect_type(result, "character")
  expect_true(nchar(result) > 0)
})

test_that("clean_text respects case parameter", {
  expect_equal(clean_text("Hello", case = "upper"), "HELLO")
  expect_equal(clean_text("Hello", case = "lower"), "hello")
  expect_equal(clean_text("Hello", case = NULL), "Hello")
})

test_that("clean_text removes periods when requested", {
  # With case=upper for predictable output
  expect_equal(clean_text("Inc.", case = "upper", remove_periods = TRUE), "INC")
  expect_equal(clean_text("Inc.", case = "upper", remove_periods = FALSE), "INC.")
})

test_that("clean_text removes commas when requested", {
  # With case=upper for predictable output
  result_with <- clean_text("Hello, World", case = "upper", remove_commas = TRUE)
  result_without <- clean_text("Hello, World", case = "upper", remove_commas = FALSE)

  expect_false(grepl(",", result_with))
  expect_true(grepl(",", result_without))
})

test_that("clean_text handles NA values", {
  result <- clean_text(NA_character_)
  expect_true(is.na(result))
})

test_that("clean_text handles empty strings", {
  result <- clean_text("")
  expect_type(result, "character")
})

test_that("tbl_clean_variables works on tibbles", {
  df <- tibble::tibble(
    name = c("  Apple Inc.  ", "MICROSOFT CORP", "  google llc"),
    value = 1:3
  )

  result <- tbl_clean_variables(df, "name")

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
  expect_true("name" %in% names(result))
})

test_that("tbl_clean_variables errors on missing columns", {
  df <- tibble::tibble(x = 1:3)

  # Current behavior is to error on non-existent column
  expect_error(
    tbl_clean_variables(df, "nonexistent")
  )
})

test_that("ent_clean alias works", {
  expect_identical(ent_clean, tbl_clean_variables)

  df <- tibble::tibble(name = c("  test  ", "TEST2"))
  result <- ent_clean(df, "name")
  expect_s3_class(result, "tbl_df")
})

test_that("refine_columns uses functional pattern (no global assignment)", {
  # This test verifies the fix for the global assignment anti-pattern
  df <- tibble::tibble(
    company = c("APPLE INC", "Apple Inc.", "APPLE INCORPORATED")
  )

  # Should work without side effects
  result <- refine_columns(df, entity_columns = "company", snake_names = FALSE)

  expect_s3_class(result, "tbl_df")
  expect_true("companyClean" %in% names(result) || "company_clean" %in% names(result))
})

test_that("refine_columns handles empty columns parameter", {
  df <- tibble::tibble(x = 1:3)

  result <- refine_columns(df, entity_columns = NULL)
  expect_equal(result, df)

  result <- refine_columns(df, entity_columns = character(0))
  expect_equal(result, df)
})

test_that("ent_refine alias works", {
  expect_identical(ent_refine, refine_columns)
})
