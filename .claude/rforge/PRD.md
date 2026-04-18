---
task: "Ruthlessly RFORGE entities package"
slug: 20260418-192000_rforge-entities
effort: extended
phase: observe
progress: 0/9
started: 2026-04-18T19:20:00
updated: 2026-04-18T19:20:00
---

## Baseline (deep truth)
- 152 PASS / 6 FAIL / 0 SKIP / 0 WARN (testthat, pre-fix)
- Bug A: GLEIF URL 404 — `2023-09-28-elf-code-list-v1.5.csv` replaced by `2026-02-19-elf-code-list-v1.6.csv` (4 test failures cascade)
- Bug B: `expand.grid(method = "osa", ...) |> as_tibble()` yields Factor, breaks `match.arg()` in `stringdist_join` (2 test failures)
- 54 exports across 20 R files, 4658 LOC
- Empty stub files: `craft_api.R`, `spacy.R`, `entity.R`, `other_cleaners.R`
- External data surfaces (risk): GLEIF CSV, forebears.io (5+ endpoints), WRU package, Wikipedia page

## Criteria
- [ ] ISC-1: All 158 tests PASS (0 FAIL, 0 ERROR) after fixes via `devtools::test()`
- [ ] ISC-2: `dictionary_gleif_entity_types()` downloads current v1.6 CSV, returns tibble with same column schema
- [ ] ISC-3: `tbl_stringdist_join()` works with factor/character `method` input; `method=` arg from caller is respected
- [ ] ISC-4: `entity_abbreviations()` returns non-empty character vector of regex patterns
- [ ] ISC-5: Every exported function smoke-tested (callable without error on minimal input, or explicitly documented as network-dependent)
- [ ] ISC-6: RedTeam analysis identifies ≥3 adversarial vulnerabilities per high-risk function group (dictionaries, forebears, entity resolution, fuzzy joins)
- [ ] ISC-7: LandMine sweep identifies mines across 8 archetypes; any RED (≥9) mines defused or explicitly logged
- [ ] ISC-A1 (anti): Output schema of any currently-working function NOT changed (column names, types, order preserved)
- [ ] ISC-A2 (anti): No new mandatory dependencies added; no export removal
