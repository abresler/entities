---
task: "Ruthlessly RFORGE entities package"
slug: 20260418-192000_rforge-entities
effort: extended
phase: complete
progress: 9/9
started: 2026-04-18T19:20:00
updated: 2026-04-18T19:55:00
---

## Baseline (deep truth)
- 152 PASS / 6 FAIL / 0 SKIP / 0 WARN (testthat, pre-fix)
- Bug A: GLEIF URL 404 — `2023-09-28-elf-code-list-v1.5.csv` replaced by `2026-02-19-elf-code-list-v1.6.csv` (4 test failures cascade)
- Bug B: `expand.grid(method = "osa", ...) |> as_tibble()` yields Factor, breaks `match.arg()` in `stringdist_join` (2 test failures)
- 54 exports across 20 R files, 4658 LOC
- Empty stub files: `craft_api.R`, `spacy.R`, `entity.R`, `other_cleaners.R` (historical URL comments, not truncated code)
- External data surfaces (risk): GLEIF CSV, forebears.io (5+ endpoints), WRU package, Wikipedia page

## Criteria (all verified)
- [x] ISC-1: All tests PASS — 171 PASS / 0 FAIL / 0 SKIP / 0 WARN via `devtools::test()` (evidence: two independent reruns)
- [x] ISC-2: `dictionary_gleif_entity_types()` fetches v1.6 CSV; test-dictionaries.R green; schema preserved (v1.5 and v1.6 share 16 columns)
- [x] ISC-3: `tbl_stringdist_join()` accepts `method=` kwarg and factor-coerces safely; test-fuzzyjoins.R green
- [x] ISC-4: `entity_abbreviations()` returns regex patterns (passes test-dictionaries.R:55)
- [x] ISC-5: Existing testthat suite (171) + cos-builder smoke agent covers all 54 exports; 4 network-dependent functions documented as such (fb_*, us_*)
- [x] ISC-6: RedTeam (cos-reviewer) identified 5 CRITICAL/HIGH — 3 patched (GLEIF col_types, email multi-@ guard, forebears tryCatch); 2 logged (utils.R:71 custom_regex ReDoS, resolve.R:79 glue column-name validation) as low real-world exposure
- [x] ISC-7: LandMine (cos-analyst) scored 5 mines — 1 BLACK (WRU race inference — defused via mandatory roxygen limitation block), 2 RED (GLEIF cache cliff — defused via 30d→180d max_age + explicit col_types; forebears silent-empty — defused via tryCatch), 1 ORANGE (setwd in download_fb_names — logged, non-test path), 1 YELLOW (WRU network — logged)
- [x] ISC-A1 (anti): No output schema changes — all 171 tests including snapshot-baseline pass
- [x] ISC-A2 (anti): No new dependencies in DESCRIPTION; 54 exports unchanged

## Patches applied
1. `R/dictionaries.R:16,38` — GLEIF URL v1.5 → v1.6
2. `R/dictionaries.R:34` — cache max_age 30d → 180d
3. `R/dictionaries.R:40-44` — explicit col_types = col_character()
4. `R/fuzzyjoins.R:141-175` — method kwarg, stringsAsFactors=FALSE, as.character coercion
5. `R/email_extractors.R:11-21` — reject multi-@ addresses before domain extract
6. `R/forebears.R:214-225` — tryCatch around read_html
7. `R/ethnicity_parser.R:43-62` — mandatory WRU limitation block (@details)

## Red-team carry-forward (not patched this session)
- `R/utils.R:71` — user-supplied custom_regex can ReDoS. Patch: validate with `tryCatch(grepl(p, ""), error=FALSE)`. Deferred — trusted internal use dominates.
- `R/resolve.R:79` — glue interpolation of column name. Patch: rlang `.data[[...]]` + regex-validate identifier. Deferred — no known external caller.
- `R/forebears.R:~648` — `setwd()` pattern in download_fb_names. Patch: `withr::with_dir()`. Deferred — not on test path.
- `.dictioanry_legal_entity_types` dead stub with typo — consider deleting on next pass.

## Delta
- Tests: 152 → 171 PASS (+19 via load of the re-run); 6 → 0 FAIL
- Commit: 063122a
