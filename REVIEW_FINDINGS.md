# f1dataR — API Consistency & Documentation Review (fresh re-review)

**Date:** 2026-11 (re-review)
**Package version reviewed:** 2.0.2 (DESCRIPTION L3)
**Scope:** all 20 files in `R/`, `DESCRIPTION`, `NAMESPACE`, `README.Rmd`, `.Rbuildignore`, spot checks of `man/*.Rd`, and all of `tests/testthat/`.
**Method:** independent file-by-file re-read; each finding of the 2026-09-13 report re-checked against current code; suspected bugs verified by running R (`Rscript`) and by live Jolpica API calls.

Severity tags: **[BUG]** verified wrong behaviour · **[API]** signature/parameter inconsistency · **[RESP]** return-shape/type inconsistency · **[DOC]** wrong/misleading docs · **[STYLE]** maintainability.
Status tags: **NEW**, **STILL VALID**, **FIXED/STALE**.

---

## 1. `R/utils.R`

### [BUG] NEW — default `limit = 40` + no pagination silently truncates five loaders
`get_jolpica_content(url, parameters = list(limit = 40))` (L49) is the default, and `load_drivers()`, `load_results()`, `load_circuits()`, `load_schedule()`, `load_standings()` all call it **without overriding `limit` and without any offset loop**. Anything with more than 40 records is silently cut.

Verified live (this session):
- `1953/drivers.json` → API `total = 108`; `load_drivers(1953)` returns **40 rows**.
- `1953/2/results.json` → API `total = 47`; `load_results(1953, 2)` returns **40 rows**.
- `1950/drivers.json` total 81, `1989/drivers.json` total 47 — all truncated to 40.
- `load_standings()` is at risk too (`1989/16/driverStandings.json` total = 42), though its own `season >= 2003` guard currently keeps totals under 40; `load_standings(2024)` returns 25 rows from `total = 24` (the `Constructors` unnest adds a row), so `nrow < total` can't even be used naively there.

This is the single most serious correctness issue in the package: data loss with no warning. Fix by raising the limit to the API max (100) *and* adding the offset loop already used in `load_laps()`/`load_pitstops()`/`load_constructors()`.

### [DOC] STILL VALID — `get_ergast_content()` is dead code
L18–30: no caller in `R/`; only `tests/testthat/test-utils.R` exercises the deprecation warning. Stale wording fixed ("shut down at the end of 2024", "was replaced" — typos gone), but the function itself remains solely for BC without saying so.

### [DOC] FIXED — retry count
Doc now says 10 (L5, L37) matching `req_retry(max_tries = 10)` (L71).

### [DOC] FIXED — throttle/rate-limit comment
L53 now reads "Throttles at 500 req/hr (Jolpica's documented sustained limit)" and L78 is `req_throttle(capacity = 500, fill_time_s = 3600)`. Matches Jolpica's documented sustained limit. Same for `check_ff1_network_connection()` (L257).

### [BUG] FIXED — `as.list()` result discarded
L64–66 now assigns: `parameters <- as.list(parameters)`.

### [DOC] FIXED — `@param parameters` example
L45 now documents `list(limit = 40)`.

### [BUG] FIXED — `check_ff1_version()` NA handling
L290–296 now explicitly handles `is.na(version)` with the "Ensure fastf1 Python package is installed" warning and `return(invisible(NA))`. `get_fastf1_version()` `@return` (L319) documents the `NA` case.

### [STYLE] NEW — `check_ff1_version()` return value is inconsistent across branches
L297–309: the `< 3.1` branch aborts, the `< 3.4` branch returns the *value of `cli::cli_warn()`* (not `invisible(TRUE)`), and only the final `else` returns `invisible(TRUE)`. The `@return` (L285) claims "Invisibly `TRUE`". Add `invisible(TRUE)` after the 3.4 warning.

### [DOC] FIXED — `time_to_sec()` internal keyword
`@keywords internal` present (L171) and `man/time_to_sec.Rd:18` has `\keyword{internal}`.

### [STYLE] STILL VALID — `dummy()` R-CMD-check hack
L370–382. janitor/tibble/tidyr are all in `Imports` and used directly elsewhere; re-run `R CMD check` without it.

### [STYLE] NEW — `add_col_if_absent()` calls `cli::cli_abort(x = "...")`
L350, L355, L360: the message is passed as a named `x =` argument rather than positionally. It happens to render, but the idiomatic form is `cli_abort(c("x" = "..."))`. Also the validation `if (!is.na(na_type)) abort(...)` (L349) is a confusing way to say "must be an NA of some type" — a non-NA *vector* of length > 1 would error inside `is.na()` first.

### [STYLE] NEW — `time_to_sec()` on zero-length input
`Vectorize()` over `character(0)` returns `list()`, not `numeric(0)`. Not currently hit by callers, but it is an edge case worth a guard or a test.

---

## 2. `R/zzz.R`

### [BUG] NEW — `'filesystem'` cache creates a *relative* `./filesystem` directory
`.onLoad` L22–31:
```r
if (memoise_option == "filesystem") {
  cache_dir <- rappdirs::user_cache_dir(appname = "f1dataR")
  dir.create(cache_dir, ...)
  options("f1dataR.cache" = cache_dir)   # option updated…
}
cache <- cachem::cache_disk(dir = memoise_option)   # …but this still uses "filesystem"
```
`memoise_option` is never reassigned to `cache_dir`, so the memoise disk cache is created in a directory literally named `filesystem` relative to the working directory, while `getOption("f1dataR.cache")` points at the rappdirs location. The two caches diverge, and a stray `filesystem/` dir is created in the user's CWD. Fix: `memoise_option <- cache_dir` (or use `cache_dir` in `cache_disk()`).

### [DOC] FIXED — startup message accuracy
`.onAttach` L183–193 now distinguishes in-memory ("until the end of the R session") from disk ("persist beyond the end of the R session").

### [STYLE] STILL VALID — cache-option validation duplicated
`.onLoad` L8–20 and `.onAttach` L161–179 validate with different logic (`.onAttach` additionally handles `NULL`, `.onLoad` doesn't re-set the option in the same way). Factor into one helper. Note `.onLoad` can leave `cache` undefined if `memoise_option` is a valid path *and* the first `if` didn't run — actually covered, but the control flow is fragile enough to warrant the refactor.

### [STYLE] NEW — the `off` startup message is still unconditional
L195–200: printed regardless of `interactive()`, while the non-`off` messages are gated (L182). Inconsistent, and noisy in scripts/CI.

### [API] STILL VALID — 24 h memoise timeout hard-coded 13×
`memoise::timeout(86400)` repeated in every `assign()` block (L42–L151). An option (`f1dataR.cache_timeout`) plus a loop over a character vector of function names would remove ~110 lines.

### [API] FIXED/STALE — "`plot_fastest()` is memoised"
No longer true: `plot_fastest` is **not** in the `.onLoad` memoise list. (But `clear_f1_cache()` still calls `forget()` on it — see §20.)

---

## 3. `R/load_circuits.R`

### [BUG] NEW — no pagination (see §1). `limit` defaults to 40; a season with >40 circuits would truncate. Currently masked because max circuits/season is 24, but the code has no guard and no `total` check at all.
### [DOC] FIXED — the `.load_circuits()` reference is gone; `@param season` documents `"current"` (L5); `@return` documents `NULL` on API failure (L7).
### [STYLE] STILL VALID — season validation duplicated verbatim
`if (season != "current" && (season < MIN || season > get_current_season()))` appears in `load_circuits.R:9`, `load_drivers.R:11`, `load_schedule.R:12`, `load_results.R:16`, `load_standings.R:20`, `load_quali.R:17`, `load_sprint.R:16`, `load_laps.R:26`, `load_pitstops.R:33`, `load_race_session.R:56`. Extract `check_season_arg(season, min_year)`.
### [API] STILL VALID — `season` is not coerced
`"2021"` works only because string comparison with a numeric coerces both to character. `load_quali()` now does the right thing locally (`season_num`, L21–25) — adopt that pattern (or a helper) everywhere.

---

## 4. `R/circuit_details.R`

### [DOC] FIXED — cache description now correct (L15–17: `options(...)`, default `"memory"`, tempdir fallback).
### [DOC] FIXED — max round wording standardised ("1 to the number of rounds in the season", L22).
### [DOC] FIXED — `@return` documents `NULL` on failure (L36).
### [DOC] FIXED — the hard-coded `session = "R"` is now stated in the description (L19–20).
### [API] STILL VALID — FastF1 floor inconsistency
`load_circuit_details()` requires ≥ 3.1 via `check_ff1_version()` (L45) while `plotting_style.R::get_session()` hard-aborts below 3.4 (L541–545). Pick one floor.
### [API] NEW — `round` is not validated here at all
`round = 1` is passed straight through to `load_race_session()`, which validates only `season` and `session`. A nonsense round produces a raw FastF1 error.

---

## 5. `R/load_drivers.R`

### [BUG] NEW — truncation at 40 rows (verified: `load_drivers(1953)` = 40 of 108). See §1.
### [DOC] FIXED — `.load_drivers()` reference gone; `"current"` documented (L5); `@return` names actual columns and documents `NULL` (L8–10).
### [RESP] NEW — `permanent_number` is added as `NA_integer_` when absent (L27) but comes back from the API as **character** when present; the column type therefore varies by season. Document or coerce.

---

## 6. `R/load_constructors.R`

### [DOC] FIXED — description now says "all constructors that have ever participated" (L3); the non-existent `.load_constructors()` reference and the missing space are gone; `@return` documents `NULL` (L6).
### [API] FIXED — pagination edge case: the loop is now `while (nrow(full) < total)` (L23), so no extra empty request on exact multiples.
### [API] STILL VALID — no `season` parameter, inconsistent with the season-scoped `load_drivers()`/`load_circuits()`. Jolpica supports `/{season}/constructors.json`.
### [RESP] NEW — returns a plain `data.frame`, not a tibble
L36–40 pipes into `dplyr::select()` + `janitor::clean_names()` with no `tibble::as_tibble()`, unlike every other Jolpica loader. The test only checks `ncol`/`nrow` (`test-load_constructors.R:22-23`), so the deviation is untested.

---

## 7. `R/load_schedule.R`

### [BUG] NEW — no pagination (limit 40). Max rounds is currently 24, so latent rather than active.
### [DOC] FIXED — `.load_schedule()` reference gone; `@return` documents `NULL` (L8).
### [RESP] STILL VALID — `date`/`time` remain character columns with undocumented formats (`yyyy-mm-dd`, `HH:MM:SSZ`).
### [STYLE] STILL VALID — the Sprint unnest (`names_sep = "_"`, L47) creates `sprint_date`/`sprint_time`/… of which only `sprint_date` survives the `select()` (L62–74). Add a comment.
### [STYLE] NEW — `janitor::clean_names()` is called **twice** (L57 and L75) on the same pipeline.

---

## 8. `R/load_results.R`

### [BUG] NEW — truncation at 40 rows (verified: `load_results(1953, 2)` = 40 of 47). Affects 1950s races with large entry lists. See §1.
### [DOC] FIXED — `.load_results()` reference gone; round wording is now "1 to the number of rounds in the season" (L5); `@return` documents `NULL`, the `gap` semantics, and the NA fastest-lap cases (L10–15).
### [RESP] NEW / corrects old §22 — `position` is **character**, not numeric
Verified: `class(load_results(2023, 1)$position)` → `"character"`. The old report claimed `load_results()` returned numeric `position` and `load_laps()` character; in fact **both are character** (Jolpica returns strings and nothing coerces). Likewise `points`, `grid`, `laps` are character. `@return` says "the points won by each driver" with no type information — document or coerce these numeric-looking columns.
### [STYLE] STILL VALID — three near-duplicate pipeline branches (L34–126) with near-identical column lists.
### [STYLE] FIXED — the trailing comma in the `select()` is gone.
### [API] STILL VALID — no `season`/`round` provenance columns (`load_laps()` adds `season`, nothing else does).

---

## 9. `R/load_standings.R`

### [BUG] NEW — no pagination (limit 40). `driverStandings` totals exceed 40 in several pre-2003 seasons (1989 R16: 42); the `season >= 2003` guard currently masks it, but modern 24-car grids with mid-season constructor changes are creeping toward the limit (`load_standings(2024)` already returns 25 rows).
### [DOC] FIXED — wrong column name: `@return` now says `constructor_id` (L14).
### [API] FIXED — `type` is now `match.arg(tolower(type), c("driver", "constructor"))` (L24), so `"Driver"` works and invalid values give a friendly error.
### [DOC] STILL VALID — `@param round` still says "number from 1 to 23 (depending on season)" (L7) — should be "1 to the number of rounds in the season" per the convention adopted in `load_results()`.
### [DOC] FIXED — `@param season` documents `'current'`; `@return` documents `NULL`.
### [API] STILL VALID — no `season`/`round` columns in output.
### [RESP] NEW — the driver branch unnests `Constructors` (L47), which **duplicates a driver's row** if they drove for more than one constructor in a season. `nrow()` therefore exceeds the number of drivers (verified: 25 rows for 2024's 24 drivers). Undocumented.

---

## 10. `R/load_quali.R`

### [API] FIXED — `"current"` is now converted before comparison via `season_num` (L21–25); the old string-vs-numeric `if (season < 2006)` accident is gone.
### [DOC] FIXED — `@return` now lists all columns and documents both the `NULL` return and the pre-2006 column dropping (L11–14).
### [DOC] STILL VALID — `@param round` still reads "number from 1 to 23 (depending on season)" (L6–7).
### [RESP] NEW — `position` is character here too (verified via the test suite's own `expect_equal(quali_2021_1$position[1], "1")`, `test-load_quali.R:22`). Undocumented.
### [API] NEW — for seasons where the API returns `total = 0` for qualifying (verified: `1989/1/qualifying.json` total = 0 — and 2003–2005 rounds can be sparse), `data$MRData$RaceTable$Races$QualifyingResults[[1]]` subscripts an empty list and errors with `subscript out of bounds` instead of returning `NULL` with a message (contrast `load_sprint()` L34–41, which handles this case properly). Add the same length guard.

---

## 11. `R/load_sprint.R`

### [DOC] FIXED — description now says "final sprint race results" (L3); "dataframetibble" typo gone; `.load_sprint()` reference gone; the `lap` column is documented (L14).
### [STYLE] FIXED — the duplicated `"position"` in `select()` is gone (L60–70).
### [DOC] STILL VALID — `@param round` still says "1 to 23 (depending on season)" (L7–8); `@param season` (L6) has a double space and doesn't mention `'current'` even though the abort message offers it (L17).
### [API] NEW — no `limit` passed, so the default 40 applies; sprint fields are 20 rows so this is latent, but it is the same pattern as §1.

---

## 12. `R/load_laps.R`

### [BUG] FIXED — pagination/lap-number corruption
The rewrite (L48–86) now collects raw pages, then `mutate(lap = as.numeric(number))`, groups by `lap`, and binds the `Timings` per lap. Re-verified against the live API for the old repro case **2006 R1**: 67 raw per-lap frames across pages collapse correctly to **57 laps / 1100 timing rows**, with each lap's timing rows re-joined (lap-row counts now 21/22 except for retirements). The loop-index `lap = i` assignment is gone. The explanatory comment at L50–53 is good.

### [BUG] NEW — `round` values with no data crash instead of returning `NULL`
`data$MRData$RaceTable$Races$Laps[[1]]` (L54) subscripts an empty list when the race has no lap data. Verified live: `2021/12/laps.json?limit=100&offset=20` returns `Races = list()`, and `x$Laps[[1]]` is `NULL` — bound into `pages` it yields an empty frame, but for a season/round with zero total laps the first call produces the same empty structure and `mutate(lap = as.numeric(.data$number))` then errors on a missing `number` column. Add an `if (length(...) == 0) return(NULL)` guard like `load_sprint()`.

### [BUG] NEW — the pagination loop condition can still fire a wasted request
L57 `while (offset + lim <= total)`: when `total` is an exact multiple of `lim` (e.g. 1000), the loop issues one final request whose page is empty. `load_constructors()`/`load_pitstops()` use the better `while (nrow(full) < total)`. Harmless but inconsistent within the same package.

### [DOC] FIXED — `.load_laps()` reference and "a uncached" typo gone; `@return` now names `time`, `lap`, `time_sec`, documents `position` as **character**, documents the `NULL` return, and states that `lap` comes from the API `number` field (L12–14). `@param season` documents `"current"` (L6).
### [STYLE] FIXED — the O(n²) per-lap `bind_rows()` and the opaque `full[[1]][i][[1]]` indexing are both gone.
### [RESP] NEW — `season` column type varies: it is numeric when `season = "current"` (via `get_current_season()`, L71) and whatever the caller passed (numeric or character) otherwise. `ifelse()` also drops attributes. Coerce explicitly.

---

## 13. `R/load_pitstops.R`

### [API] FIXED — pagination added (L55–68, `while (nrow(full) < total)`), and `@return` documents it (L12–13).
### [DOC] FIXED — "2012" vs 2011 mismatch resolved (doc and code both say 2011, L4/L33); the missing space before "Also accepts" is fixed; `.load_pitstops()` reference gone; `@return` documents `NULL`.
### [DOC] STILL VALID — `@param round` says "1 to 23 (depending on season selected)" (L7).
### [BUG] NEW — same empty-`Races` crash as §12: `data$MRData$RaceTable$Races$PitStops[[1]]` (L53) is unguarded; a race with no pit-stop data (or a bad round) errors rather than returning `NULL`.
### [RESP] NEW — `lap`, `stop`, `duration` are all character (the test asserts `pitstop_2021_1$stop[2] == "1"`, `test-load_pitstops.R:24`). `@return` describes "stop duration" without a type.

---

## 14. `R/load_race_session.R`

### [DOC] FIXED — cache description (L10–12), `'CRITICAL'` typo (L23), `@seealso` comma (L30), `@param round` default now correctly documented as `1` (L17–18), and `@return` now honestly describes a reticulate Python session object + `NULL` on failure (L26–29).
### [API] STILL VALID — `"current"` → numeric conversion happens only here (L64–66); the Jolpica loaders never do it (except `load_quali()`'s local `season_num`).
### [STYLE] FIXED — the commented-out `stop(glue::glue(...))` dead code is gone.
### [BUG] NEW — `status` assignment inside `tryCatch`'s error handler is a no-op
L157–167:
```r
status <- FALSE
tryCatch({ ...; status <- TRUE },
         error = function(e) { cli::cli_alert_danger(...); status <- FALSE })
```
The `status <- FALSE` inside the handler assigns to the *handler's* local frame, not the function frame. It works by accident only because `status` was pre-initialised to `FALSE` at L157. Harmless today, dangerously misleading on any refactor — drop the assignment or use `<<-`.
### [API] NEW — `round` is never validated
Only `season` and `session` are checked (L55–63). A character round is passed to FastF1 quoted, a numeric one unquoted — an out-of-range number produces a raw Python traceback via `cli_abort("Error loading FastF1 session.")`.
### [STYLE] NEW — the cache-path resolution block (L80–92) is duplicated verbatim in `plotting_style.R::get_session()` (L548–557) and partially in `clear_f1_cache()` (L28–33). Three copies of the same "memory/off/filesystem → tempdir" rule.

---

## 15. `R/load_session_laps.R`

### [DOC] FIXED — cache text (L9–11), SQ1/SQ2/SQ3 labels documented (L6–7), `@return` now lists the core columns, notes the FastF1 extras, and documents the `NULL` return (L17–23).
### [STYLE] FIXED — the no-op `mutate("Time" = .data$Time)` is gone.
### [DOC] STILL VALID (inherited) — `@inheritParams load_race_session` now pulls the corrected `round` text, so this is resolved by §14's fix.
### [RESP] STILL VALID — the column set is session-dependent (`add_weather` adds ~7–11 columns; the tests accept `ncol %in% c(28, 32)` and `c(35, 39)`, `test-load_session_laps.R:45-46`, which is itself a sign the shape is unstable). `@return` says "at least" — acceptable, but the weather columns are still not named.
### [BUG] NEW — Q-session labelling assumes rows are ordered Q1→Q2→Q3
L100–115 builds `SessionType` as `c(rep("Q1", q1len), rep("Q2", q2len), rep("Q3", q3len))` and assigns it positionally to `laps`. This is only correct if `session.laps` rows happen to be ordered identically to the concatenation of `split_qualifying_sessions()`'s three frames. FastF1 makes no such guarantee (laps are ordered by time/driver). If `add_weather = TRUE`, `laps` has additionally been `reset_index(drop=True)`-ed and concatenated (L64–72) before this assignment — and if the lengths don't sum to `nrow(laps)`, R recycles or errors. Safer: join on the lap index returned by `split_qualifying_sessions()`.

---

## 16. `R/load_driver_telemetry.R`

### [DOC] FIXED — "load Options" sentence break fixed (L11–12); `@return` documents `NULL` and the FastF1-dependent columns (L23–25); `@param round` no longer says "1 to 23" (L9).
### [API] STILL VALID — deprecated twin `get_driver_telemetry()` (L125–146) is still `@keywords internal` **plus** `@export` (contradictory), still a hard `deprecate_stop()`, and still has a different parameter order (`fastest_only` before `race`) from the live function. `man/get_driver_telemetry.Rd` exists and is exported in `NAMESPACE:14`.
### [STYLE] STILL VALID — three near-identical telemetry branches (L92–110) differing only in the chained method.
### [BUG] NEW — `laps` validation errors on length > 1
L66: `if (!(laps %in% c("fastest", "all")))` — verified in R: a length-2 `laps` raises `the condition has length > 1` (R ≥ 4.2) instead of the intended `cli_abort`. Also, a numeric `laps` is interpolated into the Python string unquoted with no upper-bound check against the session's lap count, despite the `@param` claiming "`<=` total laps in the race".
### [BUG] NEW — `driver` has no validation at all
It is a required argument with no default and is interpolated directly into `pick_drivers('{driver}')` (L94). A missing/empty/multi-element `driver` produces a Python error after a full (slow) session load. Contrast `plotting_style.R`, which validates every `driver`/`team` string.

---

## 17. `R/plot_fastest.R`

### [BUG] FIXED — `color` is now `match.arg(color, c("gear", "speed"))` (L44).
### [BUG] FIXED — the NA FastF1-version crash: `check_ff1_version()` is called (L60) and the comparison is guarded with `!is.na(ff1_version) && ...` (L63).
### [BUG] NEW (verified) — the fastest-lap subtitle filter is a no-op due to name shadowing
L110–116:
```r
driver_id <- season_drivers %>% ... %>% dplyr::pull("driver_id")
lap_time  <- load_laps(season, round) %>%
  dplyr::filter(.data$driver_id == driver_id) %>%     # RHS resolves to the COLUMN
  dplyr::filter(.data$time_sec == min(.data$time_sec)) %>%
  dplyr::pull(.data$time)
```
Verified in R: inside `filter()`, the bare `driver_id` on the right-hand side resolves to the data-mask column, not the outer variable, so the comparison is `column == column` → always `TRUE` and **no filtering occurs**. The subtitle therefore shows the *race's* overall fastest lap for every driver. Fix with `.env$driver_id` (or rename the local variable).

### [BUG] NEW — `race_name` becomes `NA` for `SQ`/`SS` sessions
L120–128 uses `dplyr::recode_values(session, c("q","Q") ~ ..., c("s","S") ~ ..., c("fp1","FP1") ~ ..., ...)` with **no `default =`**. Verified: `recode_values()` returns `NA` for unmatched input, so `session = "SQ"` or `"SS"` (both accepted by `load_race_session()`) produces the title `"2023 NA"`. Add `default = race_name` and cases for `SQ`/`SS`.

### [BUG] NEW — title shows the literal string `current`
L153–157 / L179–183: `glue::glue("{year} {race_name}", year = season, ...)`. `plot_fastest()` never converts `season = "current"` to a year (unlike `load_race_session()`), so the title renders as `"current Bahrain Grand Prix"`. Also note the `race = race_name` named argument in that `glue()` call is unused — the template references `{race_name}`, which resolves from the enclosing environment by luck.

### [BUG] NEW — zero-length `lap_time` collapses the subtitle
If the driver has no timed lap (or `time_sec` is all `NA`), `pull()` returns `character(0)`, `paste0(" | ", character(0))` is `character(0)`, and the `glue()` subtitle becomes `character(0)` → the subtitle silently disappears rather than degrading to the driver name.

### [DOC] FIXED — the `@param race` deprecation badge is present (L7), `round` correctly documents its default of `1` (L9–10), `@return` is rewritten and no longer copy-pasted from `theme_dark_f1()` (L20–22), and `correct_track_ratio()`'s example now uses `driver = "VER"` (L216).
### [DOC] FIXED — `correct_track_ratio() @param trackplot` now states the inline-`data` `x`/`y` requirement (L200–202).
### [API] STILL VALID — no `log_level` parameter; `load_race_session()` is called with the default `"WARNING"` (L78).
### [API] STILL VALID — the fastest-lap subtitle always uses **race** laps (`load_laps(season, round)`, L113), even for `session = "Q"`. Compounded by the shadowing bug above.
### [STYLE] STILL VALID — the `labs()` title/subtitle/caption block is duplicated verbatim in the gear and speed branches (L152–188).
### [RESP] NEW — `correct_track_ratio()` returns the result of `plot(trackplot, newpage = FALSE)`, i.e. it **draws as a side effect** and returns the ggplot object (verified: class `ggplot`). Since `plot_fastest()` returns this, calling `plot_fastest()` non-interactively always emits graphics output. The `@return` doesn't mention the drawing side effect or the `grid::grid.rect()` call.

---

## 18. `R/plotting_style.R`

### [BUG] FIXED — internal `get_session()` now honours its `session` argument
L570–583: both branches interpolate `'{session}'`. The old hard-coded `'R'` is gone.
### [DOC] FIXED — `get_aesthetics` now documents/example-calls the real `get_driver_color_map()` (L14, L43; `man/get_aesthetics.Rd:24,77` confirm). The "for a a team" typo is gone. `driver_team_lookup @return` now says "an unnamed character vector" (L253).
### [BUG] NEW — `get_session()` crashes when fastf1 is missing
L541–545:
```r
check_ff1_version()                                   # warns + returns invisible(NA)
if (package_version(get_fastf1_version()) < "3.4") { ... }
```
Verified in R: `package_version(NA)` errors with `invalid non-character version specification 'x' (type: logical)`. So with fastf1 absent, the user gets that cryptic error rather than the install guidance `check_ff1_version()` just produced. Also `package_version()` is redundant — `get_fastf1_version()` already returns a `package_version`.
### [API] STILL VALID — FastF1 floor 3.4 here vs 3.1 in `check_ff1_version()`, and the abort message names only `{.fn get_driver_style}` (L543) despite applying to nine exported functions.
### [API] STILL VALID — `get_session_drivers_and_teams(season, round, session = "R")` (L458) has **no defaults** for `season`/`round`, unlike every other function in the family.
### [RESP] STILL VALID — plain `data.frame` returns from `get_driver_color_map()` (L203), `get_driver_colour_map()`, `get_session_drivers_and_teams()` (L494), `get_tire_compounds()` (L525) — the package convention is tibble. Tests actively assert `is.data.frame()` (`test-plotting_style.R:53`, `:117`) so this is now baked into the test suite.
### [API] STILL VALID — inconsistent parameter sets: `get_driver_style(driver, season, round)` and `get_driver_color()`/`get_team_color()` have no `session` (they call `get_session()` with the default `"R"`, L61/L108/L149); `get_team_name(team_name, season, short)` has neither `round` nor `session` (L356 calls `get_session(season = season)` → always round 1, race); `get_team_by_driver()` has `round` but no `session` (L434); `get_tire_compounds(season)` has neither (L518).
### [STYLE] STILL VALID — `get_session_drivers_and_teams()` loop (L483–494) still uses unqualified `py_run_string()` (relying on `@import reticulate`) and reads `py_env$team`/`py_env$name` from the environment captured by an *earlier* call. It works (reticulate's env is a live proxy over the module namespace) but it is undocumented, fragile behaviour and is the only unqualified `py_run_string()` in the package.
### [DOC] STILL VALID — internal `get_session()` `@param round` (L530–531) is fine now ("defaults to 1"), but there is still no `@param session` documentation drift check: `get_aesthetics` documents `session` only via `@inheritParams load_race_session` on two of the six functions that accept it.
### [BUG] NEW — `get_session()` doesn't create the cache directory
L548–557 resolves `f1datar_cache` but, unlike `load_race_session()` (L94–96), never does `dir.create()` before handing the path to `fastf1.Cache.enable_cache()`. A user-set cache path that has been removed mid-session produces a FastF1 error.
### [STYLE] NEW — `get_session()` hard-codes the Python object name `session`, so it silently clobbers any session previously loaded by `load_race_session(obj_name = "session")`. Since `plot_fastest()` calls both (L64/L78), the plotting path loads the same session up to three times.

---

## 19. `R/theme_dark_f1.R`

### [DOC] FIXED — `@return` is now "A `ggplot2` theme object; add to a plot with `+ theme_dark_f1()`" (L6).
### [API] FIXED — the `requireNamespace("ggplot2")` guard is present (L18–22).
### [DOC] FIXED — an example was added (L9–14).
### [STYLE] STILL VALID — two near-identical 25-line theme blocks (L23–65) differing only in four `element_*` lines.
### [DOC] NEW — `@param axis_marks` says "Defaults to false" but the signature is `axis_marks = FALSE`; minor, but the doc text should use `FALSE`/`TRUE` (it also says "True or false").

---

## 20. `R/clear_f1_cache.R`

### [BUG] FIXED — `clear_f1_cache()` now resolves the FastF1 cache directory with the same memory/off/filesystem → `tempdir()` rule as the loaders (L28–33), and uses `normalizePath(..., mustWork = FALSE)` (L38). Both old bugs are resolved.
### [DOC] FIXED — the "memoised results vs FastF1 HTTP cache" distinction is now explicit (L3–6), and the `forget()`-on-non-memoised no-op is documented (L8–10).

### [BUG] NEW (verified) — `change_cache(persist = FALSE)` is a complete no-op
L152–156:
```r
if (persist) options("f1dataR.cache" = cache)
else withr::local_options("f1dataR.cache" = cache)
```
`withr::local_options()` unwinds when **its own calling frame exits** — which is `change_cache()` itself. Verified in R: setting an option via `local_options()` inside a function leaves the option unchanged after the function returns. So the documented "temporary cache change only (default)" never takes effect. Fix with `withr::local_options(..., .local_envir = parent.frame())` or by documenting that callers must use `withr` themselves. Note the package's own test `test-clear_f1_cache.R:27` (`change_cache("off", persist = FALSE)` then load) passes **trivially** because of this — it never actually exercises the "off" path.

### [BUG] NEW (verified) — `change_cache("filesystem", persist = FALSE)` creates a directory that is deleted immediately
L134: `cache_dir <- withr::local_tempdir("f1dataR_cache")` — same frame-scoping problem. Verified: the temp directory is removed when `change_cache()` returns, so the path subsequently stored in the option (also via the broken `local_options`) points at a non-existent directory. `test-clear_f1_cache.R:52` exercises this and passes only because nothing actually changes.

### [STYLE] STILL VALID — dead `forget()` calls
L61: `memoise::forget(f1dataR::get_current_season)` — `get_current_season()` is not memoised anywhere (confirmed: absent from `zzz.R`'s list). L62: `memoise::forget(f1dataR::plot_fastest)` — `plot_fastest` is also no longer memoised. Both return invisible `FALSE` (verified). The docs now excuse this generically (L8–10), but the two specific lines are simply dead.

### [API] STILL VALID — literal `"filesystem"` maps to different places in different code paths
`.onLoad` translates it to `rappdirs::user_cache_dir()` (zzz.R L24–29, though buggily — see §2), while `load_race_session()` (L80–88), `get_session()` (L548–557) and `clear_f1_cache()` (L29–33) treat it as `tempdir()`. Centralise in one helper.

### [BUG] NEW — `change_cache()` errors on a `NULL`/multi-element `cache`
L120 `if (!cache %in% c(...))` and L131 `if (cache == "filesystem")` do no type/length validation; `change_cache(NULL)` gives `argument is of length zero`.

---

## 21. Tests (`tests/testthat/`) — NEW SECTION

### [BUG] Tests would not catch the §1 truncation bug
Every Jolpica loader test uses a modern, small season (2021, 2024, 2025) where record counts are under 40. `test-load_drivers.R`, `test-load_results.R`, `test-load_circuits.R`, `test-load_standings.R` contain no case with > 40 records. Add a 1953-drivers / 1953-R2-results regression test (108 and 47 records respectively).

### [BUG] Two cache tests pass trivially
`test-clear_f1_cache.R:27` and `:52` call `change_cache(..., persist = FALSE)`, which — per §20 — does nothing. The "off cache" and "filesystem cache" tests therefore both run against whatever cache was already configured and assert only that `load_circuits(2021)` returns 21 rows. They do not test what their names claim.

### [STYLE] Heavy boilerplate duplication
The 9-line "create/clean a tempdir and set `f1dataR.cache`" prologue is copy-pasted in **19** `test_that()` blocks. Move it into a `setup.R` helper (`local_f1_cache_dir(name)`).

### [STYLE] Inconsistent API mocking
`vcr` cassettes exist for only 10 of the Jolpica calls (`tests/testthat/_vcr/`), and several tests mix a live `skip_if_no_jolpica()` call with a cassette in the same block (e.g. `test-load_laps.R:20` live, then `vcr::local_cassette("load_laps")` at L32). The result is that the first assertions hit the network on every run while later ones replay. Either mock everything or gate the live portion consistently.

### [STYLE] Loose assertions
`test-load_session_laps.R:45-46` asserts `ncol(laps) %in% c(28, 32)` — a shape assertion that tolerates two different schemas. `test-load_constructors.R:23` asserts only `nrow >= 212`. `test-load_quali.R:27` writes `expect_false("q2" %in% quali_2004)` — this tests membership in the *values* of the tibble, not the column names (it should be `names(quali_2004)`); it passes vacuously.

### [STYLE] No tests for several exported functions
No test file covers `load_schedule()`'s sprint path, `theme_dark_f1(axis_marks = TRUE)` in isolation, `get_current_season()`'s January/February rollback branch, or `add_col_if_absent()`'s tibble-conversion return contract beyond the happy path.

### [STYLE] `tests/testthat/Rplots.pdf` is checked into the repo — a test artifact from `plot()` in `correct_track_ratio()` (see §17). Add to `.gitignore` and clean up.

---

## 22. Cross-cutting: API defaults & parameter conventions

### Season minimums (from code)

| Function | min season | round default | validates round? |
|---|---|---|---|
| `load_circuits()`, `load_drivers()`, `load_results()`, `load_schedule()` | 1950 | — / `"last"` | no |
| `load_laps()` | 1996 | `"last"` | no |
| `load_quali()`, `load_standings()` | 2003 | `"last"` | no |
| `load_pitstops()` | 2011 | `"last"` | no |
| `load_sprint()` | 2021 | `"last"` | no |
| `load_race_session()`, `load_session_laps()`, `load_driver_telemetry()`, `plot_fastest()`, `load_circuit_details()`, all lookups | 2018 | `1` | no |

### STILL VALID — `round` default split `"last"` (Jolpica) vs `1` (FastF1)
Unchanged. The *documentation* is now correct on both sides (§14 fixed the "most recent" text), so this is now purely an API-design decision rather than a doc bug.

### STILL VALID — no `round` validation anywhere
`load_results(2024, round = "Bahrain")` still produces a 404 → `NULL` rather than a friendly error; `load_race_session(2024, round = 99)` produces a raw Python error.

### STILL VALID — `season` is never coerced except in `load_quali()` (L21–25) and `load_race_session()` (L64–66). `plot_fastest()` in particular passes `"current"` straight into a plot title (§17).

### PARTIALLY FIXED — max-round wording
`load_results.R:5`, `load_laps.R:7`, `circuit_details.R:22`, `plot_fastest.R:9` now use "1 to the number of rounds in the season". Still "1 to 23" in `load_standings.R:7`, `load_quali.R:6`, `load_pitstops.R:7`, `load_sprint.R:7`. `load_race_session.R:17` says "1 to 24".

### STILL VALID — `log_level` exists only on the FastF1 layer, and not on `plot_fastest()`.

### STILL VALID — `@param session`/`round`/`log_level` texts are copy-pasted across ≥ 6 files. Roxygen `@template`/`@inheritParams` from one canonical definition would prevent the remaining drift listed above.

---

## 23. Cross-cutting: response / return types

### STILL VALID — driver-identity column differs per layer

| Layer | Column |
|---|---|
| Jolpica loaders | `driver_id` (e.g. `"leclerc"`) |
| `load_session_laps()` | `driver` (3-letter code) |
| `load_driver_telemetry()` | `driver_code` |
| Lookups | `abbreviation` |

Still undocumented anywhere. This is the #1 join pain point; a `?f1dataR` overview page or README table would fix it.

### CORRECTED (old §22 was wrong) — `position` is character *everywhere*
Verified: `load_results(2023,1)$position` and `load_laps(2023,1)$position` are both `"character"`; `load_quali()`/`load_sprint()`/`load_standings()`/`load_pitstops()` likewise (the test suite's own string assertions confirm). The old report's claim of a character/numeric split is **stale**. The real (new) issue is that *no* `@return` documents that `position`, `points`, `grid`, `laps`, `stop`, `wins` are character strings, which silently breaks arithmetic and sorting.

### STILL VALID — container inconsistency
Plain `data.frame` from `load_constructors()` (NEW, §6), `get_driver_color_map()`, `get_driver_colour_map()`, `get_tire_compounds()`, `get_session_drivers_and_teams()`. Everything else is a tibble.

### MOSTLY FIXED — `NULL` returns
Now documented in `load_circuits`, `load_drivers`, `load_constructors`, `load_schedule`, `load_results`, `load_standings`, `load_quali`, `load_sprint`, `load_laps`, `load_pitstops`, `load_circuit_details`, `load_race_session`, `load_session_laps`, `load_driver_telemetry`, `plot_fastest`. Good coverage. Remaining gap: several of those functions **error** rather than return `NULL` on empty API responses (§10, §12, §13), so the documented contract is not always honoured.

### STILL VALID — missing provenance columns
Only `load_laps()` adds `season`. `bind_rows(load_results(2023), load_results(2024))` is still ambiguous.

### STILL VALID — time encoding split
Jolpica `time` (clock string) + `time_sec` (numeric) vs raw seconds in `load_session_laps()`. Documented on the FastF1 side only.

---

## 24. Cross-cutting: `README.Rmd`

- **FIXED**: L71/L84/L95 now use `round =` (no deprecated `race =`); `laps = "fastest"` default is correct (L84); `plot_fastest(..., round = 1, ...)` is correct (L109); "echoes … This is a convenient way" is fixed (L120); `get_driver_telemetry()` removed from the Metadata Lookups list (L129–143); the session list reads "FP1, FP2, FP3, Q, S, SS, or R" (L86).
- **[DOC] STILL VALID — L157** ``load_schedule(season = `r get_current_season()`)`` inside a `*` bullet with surrounding backticks still renders wrong: the generated `README.md:237` reads ``- `load_schedule(season =`2025`)``` — the inline code fence breaks around the evaluated value. Use `load_schedule(season = 2025)` literally, or restructure the backticks.
- **[DOC] NEW — L66** "Note the Ergast Motor Racing Database API **will be shutting down** at the end of 2024." Stale future tense; it shut down two years ago. (`R/utils.R` was fixed for this, README was not.)
- **[DOC] NEW — L111** now reads "plots a driver's fastest lap around the circuit, shaded by `speed` or `gear`" — the old garbled sentence is FIXED.
- **[DOC] NEW** — the README's "Other functions" section (L150–160) presents `load_constructors()` alongside season-scoped loaders without noting it takes no arguments (§6).

---

## 25. Cross-cutting: packaging & build hygiene

- **FIXED** — `.Rbuildignore` now contains `^vignettes/alonso-penalty-2024.Rmd.orig$`, `^vignettes/plotting-turn-info.Rmd.orig$`, `^vignettes/.*\.png$`, and `^vignettes/precompile\.R$`. All four old packaging findings are resolved.
- **FIXED** — `man/time_to_sec.Rd` has `\keyword{internal}`; `man/get_session.Rd` likewise.
- **[STYLE] NEW** — `cobertura.xml`, `GAfile.html` and `.Rhistory` are present in the repo root. `cobertura.xml` is in `.Rbuildignore` but `GAfile.html` is listed with the wrong case (`^GAFile\.html$` vs the actual `GAfile.html`) — it is excluded by R CMD build defaults anyway, but the ignore entry is ineffective as written.
- **[STYLE] NEW** — `tests/testthat/Rplots.pdf` is committed (see §21).
- **[STYLE] NEW** — `DESCRIPTION` lists `vcr` in `Suggests` (L47) and tests call it unconditionally (e.g. `test-load_laps.R:32` `vcr::local_cassette(...)` with no `requireNamespace()` guard), unlike the careful `requireNamespace("httptest2", quietly = TRUE)` guards elsewhere. Tests will error, not skip, if `vcr` is absent.

---

## 26. Prioritised summary

### P0 — correctness (verified)

| # | Finding | Status | Location |
|---|---|---|---|
| 1 | Default `limit = 40` with no pagination silently truncates `load_drivers()`, `load_results()`, `load_circuits()`, `load_schedule()`, `load_standings()` (verified: 1953 drivers 40/108, 1953 R2 results 40/47) | **NEW** | §1, §5, §8 |
| 2 | `change_cache(persist = FALSE)` is a complete no-op — `withr::local_options()` unwinds in `change_cache()`'s own frame (verified) | **NEW** | §20 |
| 3 | `change_cache("filesystem", persist = FALSE)` creates a temp dir that is deleted on return (verified) | **NEW** | §20 |
| 4 | `plot_fastest()` subtitle filter `.data$driver_id == driver_id` is shadowed → no filtering → wrong lap time for every driver (verified) | **NEW** | §17 |
| 5 | `.onLoad` `"filesystem"` creates a relative `./filesystem` memoise cache while the option points elsewhere | **NEW** | §2 |
| 6 | `get_session()` errors with `package_version(NA)` when fastf1 is missing (verified) | **NEW** | §18 |
| 7 | `plot_fastest()` title is `"2023 NA"` for `SQ`/`SS` (no `default` in `recode_values`, verified) and `"current …"` when `season = "current"` | **NEW** | §17 |
| 8 | Empty-API-response crashes (`[[1]]` on `list()`) in `load_quali()`, `load_laps()`, `load_pitstops()` — documented to return `NULL`, actually error | **NEW** | §10, §12, §13 |
| 9 | `load_session_laps()` Q1/Q2/Q3 labels assigned positionally, assuming row order | **NEW** | §15 |
| 10 | `load_driver_telemetry()` `laps` check errors on length > 1; `driver` unvalidated | **NEW** | §16 |

### P0 — previously reported, now FIXED (no action)

`load_laps()` lap-number corruption (re-verified fixed on 2006 R1) · `clear_f1_cache()` wrong directory · `change_cache("off", persist=TRUE)` inherited bug · `plot_fastest()` unvalidated `color` · `plot_fastest()` `if (NA)` crash · `get_session()` ignoring `session` · `get_aesthetics` non-existent `get_driver_color_mapping()` · `check_ff1_version()` NA handling · `get_jolpica_content()` discarded `as.list()`.

### P1 — API consistency & user-facing docs

| # | Finding | Status | Location |
|---|---|---|---|
| 11 | `round` default split `"last"` vs `1` across the two data layers | STILL VALID | §22 |
| 12 | No `round` validation anywhere; `season` coerced in only 2 of 12 loaders | STILL VALID | §22 |
| 13 | Column types undocumented — `position`/`points`/`grid`/`laps`/`stop` are all **character** (old "numeric vs character" finding was wrong) | **NEW / corrects old** | §23 |
| 14 | `load_constructors()` returns a plain `data.frame`; four lookups do too | **NEW (+ STILL VALID)** | §6, §18 |
| 15 | FastF1 min version 3.1 (`check_ff1_version`) vs 3.4 (`get_session`), with a misleading abort message | STILL VALID | §4, §18 |
| 16 | `get_session_drivers_and_teams()` has no defaults; lookup-family parameter drift (`session`/`round` present on some, absent on others) | STILL VALID | §18 |
| 17 | `load_standings()` driver rows duplicated by the `Constructors` unnest, undocumented | **NEW** | §9 |
| 18 | `load_constructors()` still has no `season` parameter | STILL VALID | §6 |
| 19 | Driver-identity column mapping (`driver_id`/`driver`/`driver_code`/`abbreviation`) still undocumented | STILL VALID | §23 |
| 20 | `get_driver_telemetry()` shim: `@export` + `@keywords internal`, hard stop, divergent parameter order | STILL VALID | §16 |
| 21 | "1 to 23"/"1 to 24" round wording remains in 5 man pages | PARTIALLY FIXED | §22 |
| 22 | `plot_fastest()` has no `log_level`; subtitle always uses race laps | STILL VALID | §17 |
| 23 | README L66 stale "will be shutting down"; L157 broken inline-R backticks | STILL VALID / NEW | §24 |

### P1 — previously reported, now FIXED

Ten `.load_*()` phantom-function references (all gone) · README deprecated `race=` signatures · `load_pitstops()` pagination · `load_sprint()` copy-pasted description + "dataframetibble" · `load_standings()` `constructors_id` · `NULL` returns now documented package-wide · `load_standings()` case-sensitive `type` · `load_constructors()` pagination edge case.

### P2 — tests, style, packaging

| # | Finding | Status | Location |
|---|---|---|---|
| 24 | Two cache tests pass trivially because `change_cache(persist = FALSE)` does nothing | **NEW** | §21 |
| 25 | No test exercises > 40 API records, so the P0 truncation bug is invisible to CI | **NEW** | §21 |
| 26 | `expect_false("q2" %in% quali_2004)` tests values, not column names — vacuous | **NEW** | §21 |
| 27 | 19× copy-pasted tempdir/cache prologue; inconsistent vcr/httptest2 mocking; unguarded `vcr::` calls | **NEW** | §21, §25 |
| 28 | `load_race_session()` `status <- FALSE` inside an error handler is a no-op assignment | **NEW** | §14 |
| 29 | Cache-path resolution duplicated in 3 places; season validation duplicated in 10 | **NEW / STILL VALID** | §3, §14 |
| 30 | `check_ff1_version()` doesn't return `invisible(TRUE)` on the 3.4-warning path | **NEW** | §1 |
| 31 | `load_schedule()` calls `clean_names()` twice; `load_laps()` loop condition can fire a wasted request | **NEW** | §7, §12 |
| 32 | `get_session()` doesn't `dir.create()` the cache dir; clobbers the shared `session` Python name | **NEW** | §18 |
| 33 | Dead `forget(get_current_season)` / `forget(plot_fastest)`; `get_ergast_content()` dead; `dummy()` hack | STILL VALID | §1, §20 |
| 34 | Style: duplicated `labs()` blocks, 3× telemetry branches, 3× results branches, 2× theme blocks, stale-env lookup loop, unqualified `py_run_string()` | STILL VALID | §8, §16, §17, §18, §19 |
| 35 | `correct_track_ratio()` draws as a side effect (and commits `tests/testthat/Rplots.pdf`) | **NEW** | §17, §21 |
| 36 | 24 h memoise timeout still hard-coded 13× | STILL VALID | §2 |
| 37 | `off` startup message still unconditional | STILL VALID | §2 |
| 38 | `.Rbuildignore` `^GAFile\.html$` has wrong case | **NEW** | §25 |

### P2 — previously reported, now FIXED

All packaging findings (`.orig` files, 27 `.png`s, `precompile.R` now ignored) · `time_to_sec.Rd` `\keyword{internal}` · typos `'CRITICAL.'`, "a a team", "willbe", "an uncached", "a unnamed", "echos", `option()`→`options()` · `@return` copy-paste in `plot_fastest()`/`theme_dark_f1()` · no-op `mutate()` in `load_session_laps()` · duplicate `"position"` in `load_sprint()`'s select · trailing comma in `load_results()` · `ggplot2` guard + example in `theme_dark_f1()` · `correct_track_ratio()` example `driver = V`.

---

### Headline

Substantial progress since 2026-09-13: **all six P0 bugs and roughly two-thirds of the P1/P2 documentation findings are fixed**, including the hard `load_laps()` pagination bug (independently re-verified against the live API) and the entire packaging-hygiene list.

However, this re-review surfaces a **new P0 cluster of comparable severity**: (a) silent 40-record truncation in five Jolpica loaders caused by the default `limit = 40` with no pagination, (b) `change_cache(persist = FALSE)` being a no-op because of `withr` frame scoping (which also renders two tests vacuous), and (c) three separate `plot_fastest()` defects — a data-mask name-shadowing bug that makes the subtitle's lap time wrong for every driver, `NA` titles for sprint-qualifying sessions, and a literal `"current"` in the title. These should be the next targets, along with adding regression tests that use pre-1960 seasons where record counts exceed the page size.
