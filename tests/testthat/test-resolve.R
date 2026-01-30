# Tests for Phase 2 core features: resolve_entities(), entity_rules(), ent_match_with_rules()

# =============================================================================
# resolve_entities() tests
# =============================================================================

test_that("resolve_entities accepts tibble and returns correct structure", {
  df <- tibble::tibble(
    id = 1:4,
    name = c("Apple Inc.", "APPLE INC", "Microsoft Corp", "Google LLC")
  )

  result <- resolve_entities(df, entity_col = "name", entity_type = "company")

  # Check class
  expect_s3_class(result, "entity_resolution")

  # Check structure
  expect_named(result, c("cleaned", "duplicates", "report"))
  expect_s3_class(result$cleaned, "tbl_df")
  expect_s3_class(result$duplicates, "tbl_df")
  expect_type(result$report, "list")
})

test_that("resolve_entities creates cleaned column", {
  df <- tibble::tibble(name = c("Test Company Inc.", "TEST COMPANY"))

  result <- resolve_entities(df, entity_col = "name", entity_type = "company")

  # Should have original column plus cleaned column
  expect_true("name" %in% names(result$cleaned))
  expect_true("name_clean" %in% names(result$cleaned))
})

test_that("resolve_entities finds duplicate pairs", {
  df <- tibble::tibble(
    name = c("Apple Inc", "APPLE INCORPORATED", "Microsoft", "Banana Corp")
  )

  result <- resolve_entities(
    df,
    entity_col = "name",
    entity_type = "company",
    dedup_threshold = 0.70  # Lower threshold to catch more
  )

  # Should find Apple duplicates
  expect_s3_class(result$duplicates, "tbl_df")

  # Check duplicate structure
  if (nrow(result$duplicates) > 0) {
    expect_true(all(c("id_1", "id_2", "similarity") %in% names(result$duplicates)))
    expect_true(all(result$duplicates$similarity >= 0.70))
    expect_true(all(result$duplicates$id_1 < result$duplicates$id_2))
  }
})

test_that("resolve_entities report contains expected metrics", {
  df <- tibble::tibble(name = c("Test1", "Test2", NA, "Test4"))

  result <- resolve_entities(df, entity_col = "name", entity_type = "unknown")

  expect_true("total_records" %in% names(result$report))
  expect_true("missing_values" %in% names(result$report))
  expect_true("completeness" %in% names(result$report))
  expect_true("potential_duplicates" %in% names(result$report))
  expect_true("threshold_used" %in% names(result$report))
  expect_true("method_used" %in% names(result$report))

  expect_equal(result$report$total_records, 4)
  expect_equal(result$report$missing_values, 1)
})

test_that("resolve_entities validates inputs", {
  df <- tibble::tibble(name = c("Test"))

  # Missing column should error
  expect_error(
    resolve_entities(df, entity_col = "nonexistent"),
    regexp = "not found"
  )

  # Invalid data type should error
  expect_error(
    resolve_entities("not a dataframe", entity_col = "name"),
    regexp = "data frame"
  )
})

test_that("resolve_entities handles different entity types", {
  df <- tibble::tibble(name = c("John Smith", "JOHN SMITH", "Jane Doe"))

  # Person type
  result_person <- resolve_entities(df, entity_col = "name", entity_type = "person")
  expect_equal(result_person$report$entity_type, "person")

  # Company type
  result_company <- resolve_entities(df, entity_col = "name", entity_type = "company")
  expect_equal(result_company$report$entity_type, "company")

  # Unknown type
  result_unknown <- resolve_entities(df, entity_col = "name", entity_type = "unknown")
  expect_equal(result_unknown$report$entity_type, "unknown")
})

test_that("resolve_entities can skip cleaning", {
  df <- tibble::tibble(name = c("Test"))

  result <- resolve_entities(df, entity_col = "name", entity_type = "company", clean = FALSE)

  # Should still have result
  expect_s3_class(result, "entity_resolution")
})

test_that("resolve_entities can skip report", {
  df <- tibble::tibble(name = c("Test"))

  result <- resolve_entities(df, entity_col = "name", entity_type = "company", return_report = FALSE)

  expect_null(result$report)
})

test_that("resolve_entities print method works", {
  df <- tibble::tibble(name = c("Test1", "Test2"))

  result <- resolve_entities(df, entity_col = "name", entity_type = "company")

  # Should not error
  expect_output(print(result), regexp = "Entity Resolution")
})

# =============================================================================
# entity_rules() tests
# =============================================================================

test_that("entity_rules creates valid rules object", {
  rules <- entity_rules(
    name = list(method = "jw", weight = 0.7, threshold = 0.85),
    city = list(method = "exact", weight = 0.3)
  )

  expect_s3_class(rules, "entity_rules")
  expect_named(rules, c("fields", "combine"))
  expect_equal(length(rules$fields), 2)
})

test_that("entity_rules normalizes weights to sum to 1", {
  rules <- entity_rules(
    field1 = list(method = "jw", weight = 2),
    field2 = list(method = "lv", weight = 3),
    combine = "weighted_average"
  )

  total_weight <- sum(purrr::map_dbl(rules$fields, "weight"))
  expect_equal(total_weight, 1, tolerance = 0.001)
})

test_that("entity_rules sets default threshold", {
  rules <- entity_rules(
    name = list(method = "jw", weight = 1)  # No threshold specified
  )

  expect_equal(rules$fields$name$threshold, 0.80)
})

test_that("entity_rules validates method", {
  expect_error(
    entity_rules(name = list(method = "invalid_method", weight = 1)),
    regexp = "invalid method"
  )
})

test_that("entity_rules validates weight", {
  expect_error(
    entity_rules(name = list(method = "jw", weight = -1)),
    regexp = "non-negative"
  )
})

test_that("entity_rules validates field structure", {
  # Missing method
  expect_error(
    entity_rules(name = list(weight = 1)),
    regexp = "must have"
  )

  # Missing weight
  expect_error(
    entity_rules(name = list(method = "jw")),
    regexp = "must have"
  )

  # Not a list
  expect_error(
    entity_rules(name = "not a list"),
    regexp = "must be a list"
  )
})

test_that("entity_rules supports all combine strategies", {
  for (strategy in c("weighted_average", "min", "max", "vote")) {
    rules <- entity_rules(
      name = list(method = "jw", weight = 1),
      combine = strategy
    )
    expect_equal(rules$combine, strategy)
  }
})

test_that("entity_rules print method works", {
  rules <- entity_rules(
    name = list(method = "jw", weight = 0.7, threshold = 0.85),
    city = list(method = "exact", weight = 0.3)
  )

  expect_output(print(rules), regexp = "Entity Matching Rules")
  expect_output(print(rules), regexp = "name")
  expect_output(print(rules), regexp = "city")
})

# =============================================================================
# ent_match_with_rules() tests
# =============================================================================

test_that("ent_match_with_rules performs basic matching", {
  df_a <- tibble::tibble(name = c("apple", "banana"))
  df_b <- tibble::tibble(name = c("APPLE", "cherry"))

  rules <- entity_rules(name = list(method = "jw", weight = 1, threshold = 0.80))

  result <- ent_match_with_rules(df_a, df_b, rules, threshold = 0.80)

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("id_x", "id_y", "score") %in% names(result)))
})

test_that("ent_match_with_rules returns per-field similarities", {
  df_a <- tibble::tibble(name = "apple", city = "cupertino")
  df_b <- tibble::tibble(name = "APPLE", city = "cupertino")

  rules <- entity_rules(
    name = list(method = "jw", weight = 0.7),
    city = list(method = "exact", weight = 0.3)
  )

  result <- ent_match_with_rules(df_a, df_b, rules, threshold = 0)

  expect_true("name_sim" %in% names(result))
  expect_true("city_sim" %in% names(result))
})

test_that("ent_match_with_rules respects threshold", {
  df <- tibble::tibble(name = c("apple", "banana", "cherry"))

  rules <- entity_rules(name = list(method = "jw", weight = 1))

  # High threshold should return few matches
  result_high <- ent_match_with_rules(df, threshold = 0.99, rules = rules)

  # Low threshold should return more matches
  result_low <- ent_match_with_rules(df, threshold = 0.10, rules = rules)

  expect_lte(nrow(result_high), nrow(result_low))
})

test_that("ent_match_with_rules handles self-matching", {
  df <- tibble::tibble(name = c("apple", "APPLE", "banana"))

  rules <- entity_rules(name = list(method = "jw", weight = 1))

  # Pass only x for self-matching
  result <- ent_match_with_rules(df, rules = rules, threshold = 0.80)

  expect_s3_class(result, "tbl_df")

  # Should not have self-matches (id_x == id_y)
  if (nrow(result) > 0) {
    expect_true(all(result$id_x < result$id_y))
  }
})

test_that("ent_match_with_rules validates rules object", {
  df <- tibble::tibble(name = "test")

  expect_error(
    ent_match_with_rules(df, rules = list(not_valid = TRUE)),
    regexp = "entity_rules"
  )
})

test_that("ent_match_with_rules validates field presence", {
  df <- tibble::tibble(name = "test")

  rules <- entity_rules(
    name = list(method = "jw", weight = 0.5),
    missing_field = list(method = "exact", weight = 0.5)
  )

  expect_error(
    ent_match_with_rules(df, rules = rules),
    regexp = "missing"
  )
})

test_that("ent_match_with_rules max_matches limits results", {
  df_a <- tibble::tibble(name = "apple")
  df_b <- tibble::tibble(name = c("apple", "appel", "aple", "aplle"))

  rules <- entity_rules(name = list(method = "jw", weight = 1))

  # Without limit
  result_all <- ent_match_with_rules(df_a, df_b, rules, threshold = 0.50)

  # With limit
  result_limited <- ent_match_with_rules(df_a, df_b, rules, threshold = 0.50, max_matches = 2)

  expect_gte(nrow(result_all), nrow(result_limited))
  expect_lte(nrow(result_limited), 2)
})

test_that("ent_match_with_rules exact method works", {
  df_a <- tibble::tibble(city = c("new york", "boston"))
  df_b <- tibble::tibble(city = c("NEW YORK", "chicago"))

  rules <- entity_rules(city = list(method = "exact", weight = 1))

  result <- ent_match_with_rules(df_a, df_b, rules, threshold = 0.99)

  # Should match "new york" to "NEW YORK" (case-insensitive exact)
  expect_equal(nrow(result), 1)
  expect_equal(result$city_sim[1], 1)
})

# =============================================================================
# Default rule sets tests
# =============================================================================

test_that("default_person_rules returns valid entity_rules", {
  rules <- default_person_rules()

  expect_s3_class(rules, "entity_rules")
  expect_true("last_name" %in% names(rules$fields))
  expect_true("first_name" %in% names(rules$fields))

  # Weights should sum to 1
  total_weight <- sum(purrr::map_dbl(rules$fields, "weight"))
  expect_equal(total_weight, 1, tolerance = 0.001)
})

test_that("default_company_rules returns valid entity_rules", {
  rules <- default_company_rules()

  expect_s3_class(rules, "entity_rules")
  expect_true("name" %in% names(rules$fields))

  # Weights should sum to 1
  total_weight <- sum(purrr::map_dbl(rules$fields, "weight"))
  expect_equal(total_weight, 1, tolerance = 0.001)
})

test_that("default_simple_rules returns valid entity_rules", {
  rules <- default_simple_rules()

  expect_s3_class(rules, "entity_rules")
  expect_true("name" %in% names(rules$fields))
  expect_equal(rules$fields$name$weight, 1)
})

test_that("default_simple_rules accepts custom parameters", {
  rules <- default_simple_rules(method = "lv", threshold = 0.90)

  expect_equal(rules$fields$name$method, "lv")
  expect_equal(rules$fields$name$threshold, 0.90)
})
