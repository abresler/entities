# Entity Resolution Core Features
#
# This file contains the core entity resolution workflow functions:
# - resolve_entities(): Unified end-to-end entity resolution
# - entity_rules(): DSL for defining matching rules
# - ent_match_with_rules(): Match using custom rules
# - default_person_rules(), default_company_rules(): Pre-configured rule sets

# =============================================================================
# resolve_entities() - Unified Workflow
# =============================================================================

#' Resolve entities with full pipeline
#'
#' Single entry point for entity resolution that chains cleaning, deduplication,
#' and quality reporting. Returns a structured list with cleaned data, potential
#' duplicates, and quality metrics.
#'
#' @param data A tibble containing entity data
#' @param entity_col Name of the column containing entity names (quoted string)
#' @param entity_type Type of entity: "person", "company", or "unknown".
#'   Unlike auto-detection approaches, this parameter is explicit to avoid
#'   misclassification of edge cases.
#' @param dedup_threshold Similarity threshold for deduplication (0-1).
#'   Higher values require closer matches. Default 0.85.
#' @param dedup_method String distance method for deduplication.
#'   Options: "jw" (Jaro-Winkler, default), "lv" (Levenshtein),
#'   "cosine", "jaccard", "lcs" (longest common substring).
#' @param clean Logical. If TRUE (default), applies entity-type-specific
#'   cleaning before deduplication.
#' @param return_report Logical. If TRUE (default), includes quality metrics
#'   in the output.
#'
#' @return A list with three components:
#'   \describe{
#'     \item{cleaned}{Tibble with cleaned entity data}
#'     \item{duplicates}{Tibble of potential duplicate pairs with similarity scores}
#'     \item{report}{List of quality metrics (if return_report = TRUE)}
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Resolve company names
#' companies <- tibble::tibble(
#'   id = 1:4,
#'   name = c("Apple Inc.", "APPLE INC", "Microsoft Corp", "MSFT Corporation")
#' )
#'
#' result <- resolve_entities(
#'   companies,
#'   entity_col = "name",
#'   entity_type = "company",
#'   dedup_threshold = 0.85
#' )
#'
#' # Access components
#' result$cleaned      # Cleaned data
#' result$duplicates   # Potential duplicate pairs
#' result$report       # Quality metrics
#' }
resolve_entities <- function(data,
                             entity_col,
                             entity_type = c("person", "company", "unknown"),
                             dedup_threshold = 0.85,
                             dedup_method = c("jw", "lv", "cosine", "jaccard", "lcs"),
                             clean = TRUE,
                             return_report = TRUE) {


  # Validate inputs

  if (!inherits(data, "data.frame")) {
    stop("`data` must be a data frame or tibble", call. = FALSE)
  }

  if (!entity_col %in% names(data)) {
    stop(glue::glue("Column '{entity_col}' not found in data"), call. = FALSE)
  }

 entity_type <- match.arg(entity_type)
  dedup_method <- match.arg(dedup_method)

  # Add row ID for tracking
  data <- data %>%
    dplyr::mutate(.row_id = dplyr::row_number())

  # Create cleaned column name
  clean_col <- paste0(entity_col, "_clean")

  # Step 1: Clean based on entity type
  if (clean) {
    cleaned <- switch(entity_type,
      person = .clean_person_names(data, entity_col),
      company = .clean_company_names(data, entity_col),
      unknown = .clean_unknown_entities(data, entity_col)
    )
  } else {
    # When not cleaning, just copy the original column as the "clean" column
    cleaned <- data %>%
      dplyr::mutate(!!clean_col := !!rlang::sym(entity_col))
  }

  # Step 2: Find potential duplicates via self-join
  # Convert threshold to distance (1 - similarity)
  max_dist <- 1 - dedup_threshold

  # Prepare for self-join
  df_left <- cleaned %>%
    dplyr::select(.row_id, !!rlang::sym(clean_col)) %>%
    dplyr::rename(.id_left = .row_id, .name_left = !!rlang::sym(clean_col))

  df_right <- cleaned %>%
    dplyr::select(.row_id, !!rlang::sym(clean_col)) %>%
    dplyr::rename(.id_right = .row_id, .name_right = !!rlang::sym(clean_col))

  # Perform fuzzy self-join
  duplicates <- tryCatch({
    fuzzyjoin::stringdist_join(
      df_left, df_right,
      by = c(".name_left" = ".name_right"),
      max_dist = max_dist,
      method = dedup_method,
      mode = "inner",
      distance_col = "distance"
    ) %>%
      # Remove self-matches and keep only one direction
      dplyr::filter(.id_left < .id_right) %>%
      # Calculate similarity score
      dplyr::mutate(similarity = 1 - distance) %>%
      # Join back original values
      dplyr::left_join(
        cleaned %>% dplyr::select(.row_id, !!rlang::sym(entity_col)),
        by = c(".id_left" = ".row_id")
      ) %>%
      dplyr::rename(entity_1 = !!rlang::sym(entity_col)) %>%
      dplyr::left_join(
        cleaned %>% dplyr::select(.row_id, !!rlang::sym(entity_col)),
        by = c(".id_right" = ".row_id")
      ) %>%
      dplyr::rename(entity_2 = !!rlang::sym(entity_col)) %>%
      dplyr::select(
        id_1 = .id_left,
        id_2 = .id_right,
        entity_1,
        entity_2,
        clean_1 = .name_left,
        clean_2 = .name_right,
        similarity,
        distance
      ) %>%
      dplyr::arrange(dplyr::desc(similarity))
  }, error = function(e) {
    # Return empty tibble on error
    tibble::tibble(
      id_1 = integer(),
      id_2 = integer(),
      entity_1 = character(),
      entity_2 = character(),
      clean_1 = character(),
      clean_2 = character(),
      similarity = numeric(),
      distance = numeric()
    )
  })

  # Step 3: Generate quality report
  report <- if (return_report) {
    n_total <- nrow(cleaned)
    n_missing <- sum(is.na(cleaned[[entity_col]]) | cleaned[[entity_col]] == "")
    n_duplicates <- nrow(duplicates)

    list(
      total_records = n_total,
      missing_values = n_missing,
      completeness = round((n_total - n_missing) / n_total, 4),
      potential_duplicates = n_duplicates,
      duplicate_rate = round(n_duplicates / n_total, 4),
      threshold_used = dedup_threshold,
      method_used = dedup_method,
      entity_type = entity_type
    )
  } else {
    NULL
  }

  # Clean up internal columns
  cleaned <- cleaned %>%
    dplyr::select(-.row_id)

  # Return structured result
  structure(
    list(
      cleaned = cleaned,
      duplicates = duplicates,
      report = report
    ),
    class = c("entity_resolution", "list")
  )
}

#' @keywords internal
.clean_person_names <- function(data, entity_col) {
  clean_col <- paste0(entity_col, "_clean")

  data %>%
    dplyr::mutate(
      !!clean_col := !!rlang::sym(entity_col) %>%
        stringr::str_to_upper() %>%
        stringr::str_squish() %>%
        stringr::str_remove_all("[^A-Z\\s\\-']") %>%
        stringr::str_squish()
    )
}

#' @keywords internal
.clean_company_names <- function(data, entity_col) {
  clean_col <- paste0(entity_col, "_clean")

  # Get entity abbreviations for removal
  abbrevs <- tryCatch(
    entity_abbreviations() %>% paste(collapse = "|"),
    error = function(e) "\\b(INC|LLC|LTD|CORP|CO|PLC|LP|LLP)\\b"
  )

  data %>%
    dplyr::mutate(
      !!clean_col := !!rlang::sym(entity_col) %>%
        stringr::str_to_upper() %>%
        stringr::str_squish() %>%
        stringr::str_remove_all("[,\\.]+") %>%
        stringr::str_remove_all(abbrevs) %>%
        stringr::str_squish()
    )
}

#' @keywords internal
.clean_unknown_entities <- function(data, entity_col) {
  clean_col <- paste0(entity_col, "_clean")

  data %>%
    dplyr::mutate(
      !!clean_col := !!rlang::sym(entity_col) %>%
        stringr::str_to_upper() %>%
        stringr::str_squish() %>%
        stringr::str_remove_all("[,\\.]+") %>%
        stringr::str_squish()
    )
}

#' Print method for entity_resolution objects
#'
#' @param x An entity_resolution object
#' @param ... Additional arguments (ignored)
#'
#' @export
print.entity_resolution <- function(x, ...) {
  cat("Entity Resolution Results\n")
  cat("=========================\n\n")

  if (!is.null(x$report)) {
    cat("Summary:\n")
    cat("  Total records:", x$report$total_records, "\n")
    cat("  Completeness:", scales::percent(x$report$completeness, accuracy = 0.1), "\n")
    cat("  Potential duplicates:", x$report$potential_duplicates, "\n")
    cat("  Duplicate rate:", scales::percent(x$report$duplicate_rate, accuracy = 0.1), "\n")
    cat("  Entity type:", x$report$entity_type, "\n")
    cat("  Method:", x$report$method_used, "@ threshold", x$report$threshold_used, "\n\n")
  }

  cat("Components:\n")
  cat("  $cleaned    - Tibble with", nrow(x$cleaned), "rows,", ncol(x$cleaned), "columns\n")
  cat("  $duplicates - Tibble with", nrow(x$duplicates), "potential duplicate pairs\n")
  cat("  $report     - Quality metrics\n")

  invisible(x)
}

# =============================================================================
# entity_rules() - Matching Rules DSL
# =============================================================================

#' Define entity matching rules
#'
#' Creates a rules specification for entity matching. Rules are pure data
#' structures (not stateful objects) that define how fields should be compared
#' and combined. This enables reproducible, shareable matching configurations.
#'
#' @param ... Named field specifications. Each field should be a list with:
#'   \describe{
#'     \item{method}{String distance method: "jw", "lv", "cosine", "jaccard", "lcs", "exact"}
#'     \item{weight}{Numeric weight for this field (weights are normalized to sum to 1)}
#'     \item{threshold}{Optional minimum similarity threshold (0-1). Default 0.80.}
#'   }
#' @param combine How to combine field scores into final score:
#'   \describe{
#'     \item{"weighted_average"}{Weighted mean of field similarities (default)}
#'     \item{"min"}{Minimum similarity across all fields}
#'     \item{"max"}{Maximum similarity across all fields}
#'     \item{"vote"}{Proportion of fields meeting their thresholds}
#'   }
#'
#' @return An object of class "entity_rules" (a list) containing:
#'   \describe{
#'     \item{fields}{List of field specifications}
#'     \item{combine}{Combination strategy}
#'   }
#'
#' @export
#'
#' @examples
#' # Define rules for matching companies
#' company_rules <- entity_rules(
#'   name = list(method = "jw", weight = 0.6, threshold = 0.85),
#'   city = list(method = "exact", weight = 0.2),
#'   state = list(method = "exact", weight = 0.2),
#'   combine = "weighted_average"
#' )
#'
#' # Define rules for matching people
#' person_rules <- entity_rules(
#'   last_name = list(method = "jw", weight = 0.5, threshold = 0.90),
#'   first_name = list(method = "jw", weight = 0.3, threshold = 0.85),
#'   city = list(method = "exact", weight = 0.2),
#'   combine = "weighted_average"
#' )
#'
#' # Save rules for reuse
#' # saveRDS(company_rules, "my_company_rules.rds")
entity_rules <- function(...,
                         combine = c("weighted_average", "min", "max", "vote")) {

  combine <- match.arg(combine)
  fields <- list(...)

  # Validate each field specification
  for (nm in names(fields)) {
    spec <- fields[[nm]]

    if (!is.list(spec)) {
      stop(glue::glue("Field '{nm}' must be a list with 'method' and 'weight'"),
           call. = FALSE)
    }

    if (!all(c("method", "weight") %in% names(spec))) {
      stop(glue::glue("Field '{nm}' must have 'method' and 'weight' components"),
           call. = FALSE)
    }

    valid_methods <- c("jw", "lv", "cosine", "jaccard", "lcs", "exact",
                       "osa", "dl", "hamming", "qgram", "soundex")
    if (!spec$method %in% valid_methods) {
      stop(glue::glue("Field '{nm}' has invalid method '{spec$method}'. ",
                      "Valid methods: {paste(valid_methods, collapse = ', ')}"),
           call. = FALSE)
    }

    if (!is.numeric(spec$weight) || spec$weight < 0) {
      stop(glue::glue("Field '{nm}' weight must be a non-negative number"),
           call. = FALSE)
    }

    # Set default threshold if not provided
    if (is.null(spec$threshold)) {
      fields[[nm]]$threshold <- 0.80
    }
  }

  # Normalize weights to sum to 1 for weighted_average
  if (combine == "weighted_average") {
    total_weight <- sum(purrr::map_dbl(fields, "weight"))
    if (total_weight == 0) {
      stop("Total weight cannot be zero", call. = FALSE)
    }
    if (abs(total_weight - 1) > 0.001) {
      # Normalize weights
      fields <- purrr::map(fields, function(spec) {
        spec$weight <- spec$weight / total_weight
        spec
      })
    }
  }

  structure(
    list(fields = fields, combine = combine),
    class = c("entity_rules", "list")
  )
}

#' Print method for entity_rules objects
#'
#' @param x An entity_rules object
#' @param ... Additional arguments (ignored)
#'
#' @export
print.entity_rules <- function(x, ...) {
  cat("Entity Matching Rules\n")
  cat("=====================\n\n")
  cat("Combination strategy:", x$combine, "\n\n")
  cat("Fields:\n")

  for (nm in names(x$fields)) {
    spec <- x$fields[[nm]]
    cat(sprintf("  %s: method=%s, weight=%.2f, threshold=%.2f\n",
                nm, spec$method, spec$weight, spec$threshold))
  }

  invisible(x)
}

#' Validate entity_rules object
#'
#' @param rules An entity_rules object
#' @return TRUE if valid, otherwise throws an error
#' @keywords internal
.validate_rules <- function(rules) {
  if (!inherits(rules, "entity_rules")) {
    stop("rules must be an entity_rules object created by entity_rules()",
         call. = FALSE)
  }
  TRUE
}

# =============================================================================
# ent_match_with_rules() - Rules-based Matching
# =============================================================================

#' Match entities using custom rules
#'
#' Performs entity matching using a custom rules specification created by
#' \code{\link{entity_rules}}. Computes per-field similarities and combines
#' them according to the specified strategy.
#'
#' @param x First tibble containing entities to match
#' @param y Second tibble containing entities to match against.
#'   If NULL, performs self-matching (deduplication) on x.
#' @param rules An entity_rules object created by \code{\link{entity_rules}}
#' @param threshold Minimum combined score to consider a match (0-1). Default 0.80.
#' @param max_matches Maximum number of matches to return per entity in x.
#'   NULL (default) returns all matches above threshold.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{id_x}{Row index from x}
#'     \item{id_y}{Row index from y}
#'     \item{score}{Combined similarity score}
#'     \item{...}{Per-field similarity scores (named as field_sim)}
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create sample data
#' companies_a <- tibble::tibble(
#'   name = c("Apple Inc", "Microsoft Corp"),
#'   city = c("Cupertino", "Redmond")
#' )
#'
#' companies_b <- tibble::tibble(
#'   name = c("APPLE INCORPORATED", "MSFT Corporation", "Google LLC"),
#'   city = c("Cupertino", "Redmond", "Mountain View")
#' )
#'
#' # Define matching rules
#' rules <- entity_rules(
#'   name = list(method = "jw", weight = 0.7, threshold = 0.80),
#'   city = list(method = "exact", weight = 0.3)
#' )
#'
#' # Find matches
#' matches <- ent_match_with_rules(companies_a, companies_b, rules)
#' }
ent_match_with_rules <- function(x, y = NULL, rules, threshold = 0.80, max_matches = NULL) {

  # Validate inputs
  .validate_rules(rules)

  if (!inherits(x, "data.frame")) {
    stop("`x` must be a data frame or tibble", call. = FALSE)
  }

  # Self-match mode
  self_match <- is.null(y)
  if (self_match) {
    y <- x
  }

  if (!inherits(y, "data.frame")) {
    stop("`y` must be a data frame or tibble", call. = FALSE)
  }

  # Check that all rule fields exist in data
  rule_fields <- names(rules$fields)
  missing_x <- setdiff(rule_fields, names(x))
  missing_y <- setdiff(rule_fields, names(y))

  if (length(missing_x) > 0) {
    stop(glue::glue("Fields missing from x: {paste(missing_x, collapse = ', ')}"),
         call. = FALSE)
  }
  if (length(missing_y) > 0) {
    stop(glue::glue("Fields missing from y: {paste(missing_y, collapse = ', ')}"),
         call. = FALSE)
  }

  # Add row IDs
  x <- x %>% dplyr::mutate(.id_x = dplyr::row_number())
  y <- y %>% dplyr::mutate(.id_y = dplyr::row_number())

  # Generate all pairs (cartesian product for small data)
  # For large data, this should use blocking - future enhancement
  n_x <- nrow(x)
  n_y <- nrow(y)

  if (n_x * n_y > 1000000) {
    warning("Large comparison space (", n_x, " x ", n_y, " = ", n_x * n_y,
            " pairs). Consider using blocking for better performance.",
            call. = FALSE)
  }

  # Create pair indices
  pairs <- tidyr::expand_grid(.id_x = 1:n_x, .id_y = 1:n_y)

  # In self-match mode, only keep upper triangle (avoid self and duplicates)
  if (self_match) {
    pairs <- pairs %>% dplyr::filter(.id_x < .id_y)
  }

  if (nrow(pairs) == 0) {
    return(tibble::tibble(
      id_x = integer(),
      id_y = integer(),
      score = numeric()
    ))
  }

  # Compute per-field similarities
  field_sims <- purrr::map(rule_fields, function(field) {
    spec <- rules$fields[[field]]
    vals_x <- x[[field]][pairs$.id_x]
    vals_y <- y[[field]][pairs$.id_y]

    if (spec$method == "exact") {
      # Exact match: 1 if equal (case-insensitive), 0 otherwise
      sim <- as.numeric(tolower(vals_x) == tolower(vals_y))
    } else {
      # String distance methods
      sim <- stringdist::stringsim(vals_x, vals_y, method = spec$method)
    }

    # Handle NAs
    sim[is.na(sim)] <- 0

    sim
  })
  names(field_sims) <- paste0(rule_fields, "_sim")

  # Add field similarities to pairs
  for (nm in names(field_sims)) {
    pairs[[nm]] <- field_sims[[nm]]
  }

  # Combine scores based on strategy
  pairs$score <- switch(rules$combine,
    weighted_average = {
      weights <- purrr::map_dbl(rules$fields, "weight")
      purrr::pmap_dbl(field_sims, function(...) {
        sims <- c(...)
        sum(sims * weights)
      })
    },
    min = {
      purrr::pmap_dbl(field_sims, min)
    },
    max = {
      purrr::pmap_dbl(field_sims, max)
    },
    vote = {
      thresholds <- purrr::map_dbl(rules$fields, "threshold")
      purrr::pmap_dbl(field_sims, function(...) {
        sims <- c(...)
        sum(sims >= thresholds) / length(thresholds)
      })
    }
  )

  # Filter by threshold
  pairs <- pairs %>%
    dplyr::filter(score >= threshold) %>%
    dplyr::arrange(dplyr::desc(score))

  # Limit matches if specified
  if (!is.null(max_matches)) {
    pairs <- pairs %>%
      dplyr::group_by(.id_x) %>%
      dplyr::slice_head(n = max_matches) %>%
      dplyr::ungroup()
  }

  # Clean up column names
  pairs %>%
    dplyr::rename(id_x = .id_x, id_y = .id_y)
}

# =============================================================================
# Default Rule Sets
# =============================================================================

#' Default rules for matching person names
#'
#' Returns a pre-configured entity_rules object optimized for matching
#' person names. Uses Jaro-Winkler distance which performs well on names.
#'
#' @return An entity_rules object
#' @export
#'
#' @examples
#' rules <- default_person_rules()
#' print(rules)
default_person_rules <- function() {
  entity_rules(
    last_name = list(method = "jw", weight = 0.50, threshold = 0.90),
    first_name = list(method = "jw", weight = 0.35, threshold = 0.85),
    middle_name = list(method = "jw", weight = 0.15, threshold = 0.80),
    combine = "weighted_average"
  )
}

#' Default rules for matching company names
#'
#' Returns a pre-configured entity_rules object optimized for matching
#' company/organization names. Emphasizes name similarity with optional
#' location matching.
#'
#' @return An entity_rules object
#' @export
#'
#' @examples
#' rules <- default_company_rules()
#' print(rules)
default_company_rules <- function() {
  entity_rules(
    name = list(method = "jw", weight = 0.70, threshold = 0.85),
    city = list(method = "exact", weight = 0.15, threshold = 1.0),
    state = list(method = "exact", weight = 0.15, threshold = 1.0),
    combine = "weighted_average"
  )
}

#' Default rules for simple name matching
#'
#' Returns a minimal entity_rules object for simple single-field name matching.
#'
#' @param method String distance method. Default "jw" (Jaro-Winkler).
#' @param threshold Minimum similarity threshold. Default 0.85.
#'
#' @return An entity_rules object
#' @export
#'
#' @examples
#' rules <- default_simple_rules()
#' print(rules)
default_simple_rules <- function(method = "jw", threshold = 0.85) {
  entity_rules(
    name = list(method = method, weight = 1.0, threshold = threshold),
    combine = "weighted_average"
  )
}
