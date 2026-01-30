# Entity Function Aliases
#
# This file provides standardized `ent_*` prefix aliases for all `tbl_*` functions.
# The `ent_*` naming convention is the recommended style going forward.
# The original `tbl_*` functions remain available for backwards compatibility.
#
# Migration guide:
#   tbl_fuzzy_join()        -> ent_fuzzy_join()
#   tbl_clean_variables()   -> ent_clean()
#   etc.

# =============================================================================
# Fuzzy Join Aliases
# =============================================================================

#' @rdname tbl_fuzzy_join
#' @export
ent_fuzzy_join <- tbl_fuzzy_join

#' @rdname tbl_regex_join
#' @export
ent_regex_join <- tbl_regex_join
#' @rdname tbl_stringdist_join
#' @export
ent_stringdist_join <- tbl_stringdist_join

#' @rdname tbl_distance_join
#' @export
ent_distance_join <- tbl_distance_join

#' @rdname tbl_difference_join
#' @export
ent_difference_join <- tbl_difference_join

#' @rdname tbl_geo_join
#' @export
ent_geo_join <- tbl_geo_join

# =============================================================================
# String Distance Aliases
# =============================================================================

#' @rdname tbl_variable_stringdist
#' @export
ent_variable_stringdist <- tbl_variable_stringdist

#' @rdname tbl_combine_all_variable
#' @export
ent_combine_variables <- tbl_combine_all_variable

# =============================================================================
# Cleaning Aliases
# =============================================================================

#' @rdname tbl_clean_variables
#' @export
ent_clean <- tbl_clean_variables

#' @rdname refine_columns
#' @export
ent_refine <- refine_columns

# =============================================================================
# Name Parsing Aliases
# =============================================================================

#' @rdname tbl_last_name
#' @export
ent_last_name <- tbl_last_name

#' @rdname tbl_munge_human_names
#' @export
ent_munge_names <- tbl_munge_human_names

#' @rdname tbl_extract_human_name_parts
#' @export
ent_extract_name_parts <- tbl_extract_human_name_parts

# =============================================================================
# Phonics Aliases
# =============================================================================

#' @rdname tbl_phonics
#' @export
ent_phonics <- tbl_phonics

# =============================================================================
# Classification Aliases
# =============================================================================

#' @rdname classify_last_names
#' @export
ent_classify_ethnicity <- classify_last_names

#' @rdname classify_wru_names
#' @export
ent_classify_wru <- classify_wru_names

# =============================================================================
# Dictionary Aliases
# =============================================================================

#' @rdname dictionary_gleif_entity_types
#' @export
ent_gleif_types <- dictionary_gleif_entity_types

#' @rdname entity_abbreviations
#' @export
ent_abbreviations <- entity_abbreviations
