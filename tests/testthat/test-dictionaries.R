# Tests for dictionary functions

test_that("dictionary_gleif_entity_types returns valid data", {
  skip_on_cran()
  skip_if_offline()

  df <- dictionary_gleif_entity_types()

  expect_s3_class(df, "tbl_df")
  expect_gt(nrow(df), 3000)  # Should have 3000+ entity types

  # Check required columns exist
  expect_true("elf_code" %in% names(df))
  expect_true("country_of_formation" %in% names(df))
  expect_true("entity_abbreviation" %in% names(df))

  # Check data quality
  expect_true(all(nchar(df$elf_code) == 4))  # ELF codes are 4 chars
})

test_that("dictionary_gleif_entity_types caches results", {
  skip_on_cran()
  skip_if_offline()

  # First call
  df1 <- dictionary_gleif_entity_types()

  # Second call should return identical cached results
  df2 <- dictionary_gleif_entity_types()

  # Verify caching by checking data is identical
  expect_equal(nrow(df1), nrow(df2))
  expect_equal(ncol(df1), ncol(df2))
  expect_identical(df1$elf_code, df2$elf_code)
})

test_that("dictionary_gleif_entity_types_clear_cache function exists", {
  # Just verify the function exists and is callable
  # Don't actually call it as it will break subsequent tests
  expect_true(is.function(dictionary_gleif_entity_types_clear_cache))
})

test_that("dictionary_countries_legal_entity_types is deprecated", {
  expect_warning(
    result <- dictionary_countries_legal_entity_types(),
    regexp = "deprecated"
  )
  expect_null(result)
})

test_that("entity_abbreviations returns regex patterns", {
  skip_on_cran()
  skip_if_offline()

  abbrevs <- entity_abbreviations()

  expect_type(abbrevs, "character")
  expect_gt(length(abbrevs), 100)  # Should have many abbreviations

# Check they're valid regex patterns with word boundaries
  expect_true(all(grepl("^\\\\b.*\\\\b$", abbrevs)))

  # Check common abbreviations are present
  expect_true(any(grepl("LLC", abbrevs, ignore.case = TRUE)))
  expect_true(any(grepl("INC", abbrevs, ignore.case = TRUE)))
})

test_that("ent_* aliases work for dictionaries", {
  skip_on_cran()
  skip_if_offline()

  # Test that aliases point to same functions
  expect_identical(ent_gleif_types, dictionary_gleif_entity_types)
  expect_identical(ent_abbreviations, entity_abbreviations)

  # Test that aliases work
  df <- ent_gleif_types()
  expect_s3_class(df, "tbl_df")
})
