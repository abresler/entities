# Snapshot Baseline Tests
# Created by r-package-cleanup workflow - 2026-01-29
# These tests capture current package state before cleanup

test_that("package loads successfully", {
  expect_true(requireNamespace("entities", quietly = TRUE))
})

test_that("core exported functions exist", {
  exports <- getNamespaceExports("entities")
  expect_gt(length(exports), 20)

  # Verify key function groups exist
  expect_true("resolve_data_parties" %in% exports)
  expect_true("tbl_fuzzy_join" %in% exports)
  expect_true("clean_text" %in% exports)
  expect_true("classify_last_names" %in% exports)
})

test_that("text cleaning works", {
  test_text <- "  HELLO   WORLD  "
  result <- clean_text(test_text)
  expect_type(result, "character")
  expect_true(nchar(result) > 0)
  expect_false(grepl("^\\s", result))  # No leading whitespace
  expect_false(grepl("\\s$", result))  # No trailing whitespace
})

test_that("basic fuzzy join functions exist", {
  # Just verify functions are callable (don't test execution yet)
  expect_true(is.function(tbl_regex_join))
  expect_true(is.function(tbl_stringdist_join))
  expect_true(is.function(tbl_fuzzy_join))
})

# Baseline structure test - skipped in R CMD check context
test_that("package has expected file structure", {
  skip_on_cran()
  skip_if_not(file.exists("../../DESCRIPTION"), "Not in source package context")
  pkg_root <- rprojroot::find_package_root_file()
  expect_true(file.exists(file.path(pkg_root, "DESCRIPTION")))
  expect_true(file.exists(file.path(pkg_root, "NAMESPACE")))
  expect_true(dir.exists(file.path(pkg_root, "R")))
  expect_true(dir.exists(file.path(pkg_root, "man")))
})

# NOTE: Data download URLs have been fixed in cleanup
# - dictionary_gleif_entity_types() - URL path corrected
# - dictionary_countries_legal_entity_types() - gracefully returns NULL
# - forebears functions - graceful error handling added
