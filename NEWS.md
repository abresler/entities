# entities (development version)

## entities 0.2.0

### Breaking changes

* `dictionary_countries_legal_entity_types()` is now deprecated and always returns `NULL`. The original data source (corporateinformation.com) is permanently unavailable. Use `dictionary_gleif_entity_types()` instead, which provides comprehensive entity legal form data from the official ISO 20275 standard (#1).
### New features

* `dictionary_gleif_entity_types()` now uses disk-based caching via `{cachem}` for cross-session persistence. Cache expires after 30 days.

* New `dictionary_gleif_entity_types_clear_cache()` function to manually clear the GLEIF cache when needed.

* Improved error handling in `dictionary_gleif_entity_types()` with graceful fallback to cached data when network requests fail.

* New `ent_*` prefix aliases for all `tbl_*` functions. The `ent_*` naming convention is recommended going forward:
  - `ent_fuzzy_join()`, `ent_regex_join()`, `ent_stringdist_join()`, `ent_distance_join()`, `ent_difference_join()`, `ent_geo_join()`
  - `ent_clean()` (alias for `tbl_clean_variables()`)
  - `ent_refine()` (alias for `refine_columns()`)
  - `ent_last_name()`, `ent_munge_names()`, `ent_extract_name_parts()`
  - `ent_phonics()`, `ent_classify_ethnicity()`, `ent_classify_wru()`
  - `ent_gleif_types()`, `ent_abbreviations()`

* **New `resolve_entities()` unified workflow function** - Single entry point for end-to-end entity resolution that chains cleaning → deduplication → quality reporting. Returns structured list with `$cleaned`, `$duplicates`, and `$report` components. Supports "person", "company", and "unknown" entity types.

* **New `entity_rules()` matching rules DSL** - Define custom matching rules with field-specific methods, weights, and thresholds. Pure data structure design (not stateful) enables reproducible, shareable configurations. Supports "weighted_average", "min", "max", and "vote" combination strategies.

* **New `ent_match_with_rules()` function** - Perform entity matching using custom rules created by `entity_rules()`. Returns tibble with match scores and per-field similarities.

* **New default rule sets**:
  - `default_person_rules()` - Optimized for matching person names (last_name, first_name, middle_name)
  - `default_company_rules()` - Optimized for matching company names (name, city, state)
  - `default_simple_rules()` - Minimal single-field matching

### Internal

* **Fixed global assignment anti-pattern** in `refine_columns()` and `classify_wru_names()`. Both functions now use `purrr::reduce()` instead of `walk()` with `<<-` for pure functional approach without side effects.

* Removed `library(wru)` call inside `.classify_last_name()` - now uses proper namespace imports via `wru::predict_race()`.

* Added `{lifecycle}` and `{cachem}` to Imports.

* Updated deprecated `mutate_if()` to `mutate(across())` pattern in GLEIF dictionary function.

* Fixed deprecated `rlang::flatten_chr()` usage - now uses `purrr::list_c()`.

* Improved error handling in GLEIF dictionary with tryCatch and informative messages.

* Added comprehensive test suite with 170 tests covering dictionaries, cleaning functions, fuzzy joins, name parsing, entity resolution workflow, and rules-based matching.
