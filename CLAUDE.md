# CLAUDE.md - entities Package

## Overview

The `entities` package provides tools for cleaning, resolving, and parsing entity data (companies, people, names). It focuses on:
- Entity name cleaning and standardization
- Legal entity type recognition (LLC, Inc., Ltd., etc.)
- Surname ethnicity classification using WRU model
- Fuzzy string matching and joining
- Name parsing (extracting first/last names from full names)

## Build/Test Commands

```bash
# Standard R/devtools workflow
Rscript -e "devtools::document()"
Rscript -e "devtools::load_all()"
Rscript -e "devtools::test()"
Rscript -e "devtools::check()"
```

## Key Exports (29 functions)

### Entity Cleaning
- `clean_text()` - Clean and normalize text strings
- `tbl_clean_variables()` - Clean multiple columns in a data frame
- `entity_abbreviations()` - Get regex patterns for legal entity abbreviations

### Dictionaries
- `dictionary_gleif_entity_types()` - GLEIF entity legal forms (primary source)
- `dictionary_countries_legal_entity_types()` - **DEPRECATED** - source unavailable
- `dictionary_entity_slugs()` - Entity slug patterns
- `dictionary_fb_countries()` - Forebears country list

### Name Classification
- `classify_last_names()` - Classify surnames using WRU model
- `classify_wru_names()` - Classify name columns for ethnicity
- `tbl_last_name()` - Extract last name from full name column
- `tbl_extract_human_name_parts()` - Parse names into components
- `tbl_munge_human_names()` - Standardize human names

### Fuzzy Joins
- `tbl_fuzzy_join()` - General fuzzy join wrapper
- `tbl_stringdist_join()` - String distance based join
- `tbl_distance_join()` - Numeric distance join
- `tbl_difference_join()` - Difference-based join
- `tbl_geo_join()` - Geographic distance join
- `tbl_regex_join()` - Regex pattern join

### Forebears Data (external API)
- `fb_last_names()` - Surname demographics from forebears.io
- `fb_locations()` - Country surname data
- `us_political_affiliation()` - Political affiliation by surname (may fail)
- `us_religions()` - Religion by surname (may fail)

## Architecture

### Data Sources
1. **GLEIF** - Primary source for entity legal forms (URL updated 2026-01)
2. **Forebears.io** - Surname demographics (some endpoints deprecated)
3. **WRU** - R package for race/ethnicity prediction from surnames

### Dependencies
Core: dplyr, purrr, stringr, tibble, rvest, jsonlite, glue
Specialized: wru, humaniformat, phonics, fuzzyjoin, stringdist, refinr

### Known Issues
- `dictionary_countries_legal_entity_types()` returns NULL (source 404)
- Some forebears.io endpoints may return 404 (website restructured)
- Functions with external data sources use tryCatch for graceful failure

## Conventions

- Column naming: camelCase (nameColumn, typeEntity)
- Return type: Always tibble
- Error handling: tryCatch with informative warnings for URL failures
- Internal functions: Start with `.` prefix
