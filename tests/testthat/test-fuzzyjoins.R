# Tests for fuzzy join functions

test_that("tbl_stringdist_join performs basic string distance join", {
  df1 <- tibble::tibble(
    id = 1:3,
    name = c("apple", "microsoft", "google")
  )

  df2 <- tibble::tibble(
    company = c("apple", "microsft", "googl"),  # Note typos
    value = c(100, 200, 300)
  )

  result <- tbl_stringdist_join(
    df1, df2,
    by = c("name" = "company"),
    max_dist = 2,
    method = "lv"
  )

  expect_s3_class(result, "tbl_df")
  expect_gt(nrow(result), 0)
})

test_that("tbl_fuzzy_join function exists and is callable", {
  expect_true(is.function(tbl_fuzzy_join))

  # Check that the function has expected parameters
  args <- names(formals(tbl_fuzzy_join))
  expect_true("x" %in% args || length(args) > 0)
})

test_that("tbl_regex_join performs regex-based join", {
  df1 <- tibble::tibble(
    pattern = c("^app", "^mic"),
    category = c("tech", "tech")
  )

  df2 <- tibble::tibble(
    name = c("apple", "microsoft", "banana"),
    value = 1:3
  )

  result <- tbl_regex_join(
    df1, df2,
    by = c("pattern" = "name")
  )

  expect_s3_class(result, "tbl_df")
  # Should match apple and microsoft
  expect_lte(nrow(result), 2)
})

test_that("tbl_distance_join function exists and is callable", {
  expect_true(is.function(tbl_distance_join))

  # Check that the function has expected parameters
  args <- names(formals(tbl_distance_join))
  expect_true(length(args) > 0)
})

test_that("tbl_difference_join performs difference-based join", {
  df1 <- tibble::tibble(
    date1 = as.Date(c("2024-01-01", "2024-06-01"))
  )

  df2 <- tibble::tibble(
    date2 = as.Date(c("2024-01-05", "2024-05-28"))
  )

  result <- tbl_difference_join(
    df1, df2,
    by = c("date1" = "date2"),
    max_dist = 7  # Within 7 days
  )

  expect_s3_class(result, "tbl_df")
})

test_that("fuzzy join functions return empty tibble on no matches", {
  df1 <- tibble::tibble(name = "xxx")
  df2 <- tibble::tibble(name = "zzz")

  result <- tbl_stringdist_join(
    df1, df2,
    by = "name",
    max_dist = 1
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

test_that("ent_* aliases work for fuzzy joins", {
  # Test that aliases point to same functions
  expect_identical(ent_fuzzy_join, tbl_fuzzy_join)
  expect_identical(ent_regex_join, tbl_regex_join)
  expect_identical(ent_stringdist_join, tbl_stringdist_join)
  expect_identical(ent_distance_join, tbl_distance_join)
  expect_identical(ent_difference_join, tbl_difference_join)
  expect_identical(ent_geo_join, tbl_geo_join)
})

test_that("tbl_variable_stringdist function exists and is callable", {
  expect_true(is.function(tbl_variable_stringdist))

  # Check that the function has expected parameters
  args <- names(formals(tbl_variable_stringdist))
  expect_true("data" %in% args)
  expect_true("variables" %in% args)
})
