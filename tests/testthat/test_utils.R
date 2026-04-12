# ===================================================================== #
#  An R package by Certe:                                               #
#  https://github.com/certe-medical-epidemiology                        #
#                                                                       #
#  Licensed as GPL-v2.0.                                                #
# ===================================================================== #

# Tests for internal utility functions (no LLM required):
#   .summarise_df_for_llm()
#   .format_diver_schema_hint()
#   read_preset()

# Convenience: write a temp YAML file and point secrets_file at it.
# Returns the temp file path; the caller registers on.exit cleanup.
.make_secrets_yaml <- function(lines) {
  tmpfile <- tempfile(fileext = ".yaml")
  writeLines(lines, tmpfile)
  tmpfile
}

.set_secrets_env <- function(path) {
  old <- Sys.getenv("secrets_file", unset = NA_character_)
  Sys.setenv(secrets_file = path)
  old  # return old value for restoring
}

# ===========================================================================
# .summarise_df_for_llm()
# ===========================================================================

test_that(".summarise_df_for_llm() returns a single character string", {
  result <- certellm:::.summarise_df_for_llm(data.frame(x = 1:5, y = letters[1:5]))
  expect_type(result, "character")
  expect_length(result, 1L)
})

test_that(".summarise_df_for_llm() includes 'Data frame summary' header", {
  result <- certellm:::.summarise_df_for_llm(data.frame(x = 1:5))
  expect_match(result, "Data frame summary", fixed = TRUE)
})

test_that(".summarise_df_for_llm() reports correct dimensions", {
  df <- data.frame(a = 1:10, b = 1:10, c = 1:10)
  result <- certellm:::.summarise_df_for_llm(df)
  expect_match(result, "10 rows")
  expect_match(result, "3 columns")
})

test_that(".summarise_df_for_llm() summarises numeric columns with min/mean/max", {
  df <- data.frame(val = c(1, 2, 3, 4, 5))
  result <- certellm:::.summarise_df_for_llm(df)
  expect_match(result, "min=1")
  expect_match(result, "mean=3")
  expect_match(result, "max=5")
})

test_that(".summarise_df_for_llm() rounds numeric stats to 3 decimals", {
  df <- data.frame(val = c(1.1111, 2.2222, 3.3333))
  result <- certellm:::.summarise_df_for_llm(df)
  # Should not have more than 3 decimal places for min
  expect_match(result, "min=1.111")
})

test_that(".summarise_df_for_llm() summarises logical columns with TRUE/FALSE counts", {
  df <- data.frame(flag = c(TRUE, FALSE, TRUE, TRUE, FALSE))
  result <- certellm:::.summarise_df_for_llm(df)
  expect_match(result, "TRUE: 3")
  expect_match(result, "FALSE: 2")
})

test_that(".summarise_df_for_llm() summarises character columns with unique values", {
  df <- data.frame(name = c("alpha", "beta", "gamma"), stringsAsFactors = FALSE)
  result <- certellm:::.summarise_df_for_llm(df)
  expect_match(result, '"alpha"')
  expect_match(result, '"beta"')
  expect_match(result, '"gamma"')
})

test_that(".summarise_df_for_llm() shows all values when <= 8 unique character values", {
  df <- data.frame(x = letters[1:8], stringsAsFactors = FALSE)
  result <- certellm:::.summarise_df_for_llm(df)
  expect_false(grepl("more", result, fixed = TRUE))
})

test_that(".summarise_df_for_llm() truncates character summary when > 8 unique values", {
  df <- data.frame(x = letters[1:9], stringsAsFactors = FALSE)
  result <- certellm:::.summarise_df_for_llm(df)
  expect_match(result, "more")
})

test_that(".summarise_df_for_llm() sorts character summary by frequency", {
  # 'a' appears 5 times, 'b' 3 times — 'a' should appear first
  df <- data.frame(
    x = c(rep("a", 5), rep("b", 3), "c", "d", "e", "f", "g", "h", "i"),
    stringsAsFactors = FALSE
  )
  result <- certellm:::.summarise_df_for_llm(df)
  a_pos <- regexpr('"a"', result)
  b_pos <- regexpr('"b"', result)
  expect_true(a_pos < b_pos)
})

test_that(".summarise_df_for_llm() summarises Date columns with range", {
  df <- data.frame(dt = as.Date(c("2024-01-01", "2024-06-01", "2024-12-31")))
  result <- certellm:::.summarise_df_for_llm(df)
  expect_match(result, "range:")
  expect_match(result, "2024-01-01")
  expect_match(result, "2024-12-31")
})

test_that(".summarise_df_for_llm() summarises POSIXct columns with range", {
  df <- data.frame(ts = as.POSIXct(c("2024-01-01 10:00", "2024-06-01 12:00")))
  result <- certellm:::.summarise_df_for_llm(df)
  expect_match(result, "range:")
})

test_that(".summarise_df_for_llm() reports NA count for columns with NAs", {
  df <- data.frame(x = c(1, NA, 3, NA, 5))
  result <- certellm:::.summarise_df_for_llm(df)
  expect_match(result, "NA: 2")
})

test_that(".summarise_df_for_llm() omits NA indicator for complete columns", {
  df <- data.frame(x = 1:5)
  result <- certellm:::.summarise_df_for_llm(df)
  expect_false(grepl("[NA:", result, fixed = TRUE))
})

test_that(".summarise_df_for_llm() handles factor columns without error", {
  df <- data.frame(cat = factor(c("A", "B", "A", "C")))
  result <- certellm:::.summarise_df_for_llm(df)
  expect_type(result, "character")
  expect_match(result, "cat")
})

test_that(".summarise_df_for_llm() includes column names in output", {
  df <- data.frame(mycolumn = 1:5, anothercol = 1:5)
  result <- certellm:::.summarise_df_for_llm(df)
  expect_match(result, "mycolumn")
  expect_match(result, "anothercol")
})

test_that(".summarise_df_for_llm() includes a row preview block", {
  df <- data.frame(x = 1:5)
  result <- certellm:::.summarise_df_for_llm(df, max_rows_preview = 5)
  expect_match(result, "First 5 rows")
  expect_match(result, "```")
})

test_that(".summarise_df_for_llm() respects max_rows_preview", {
  df <- data.frame(x = 1:20)
  result <- certellm:::.summarise_df_for_llm(df, max_rows_preview = 2)
  expect_match(result, "First 2 rows")
  expect_false(grepl("First 20 rows", result, fixed = TRUE))
})

test_that(".summarise_df_for_llm() truncates very wide data frames (> max_cols)", {
  wide_df <- as.data.frame(
    setNames(lapply(seq_len(35), function(i) 1:5), paste0("col", seq_len(35)))
  )
  result <- certellm:::.summarise_df_for_llm(wide_df)
  expect_match(result, "35 columns total")
  expect_match(result, "Note:")
})

test_that(".summarise_df_for_llm() respects custom max_cols", {
  wide_df <- as.data.frame(
    setNames(lapply(seq_len(10), function(i) 1:5), paste0("col", seq_len(10)))
  )
  result <- certellm:::.summarise_df_for_llm(wide_df, max_cols = 5)
  expect_match(result, "10 columns total")
})

test_that(".summarise_df_for_llm() handles a single-row data frame", {
  df <- data.frame(x = 42, y = "hello", stringsAsFactors = FALSE)
  result <- certellm:::.summarise_df_for_llm(df)
  expect_match(result, "1 rows")
})

test_that(".summarise_df_for_llm() handles a single-column data frame", {
  df <- data.frame(only = 1:10)
  result <- certellm:::.summarise_df_for_llm(df)
  expect_match(result, "1 columns")
})

test_that(".summarise_df_for_llm() handles all-NA numeric column", {
  df <- data.frame(x = rep(NA_real_, 5))
  result <- certellm:::.summarise_df_for_llm(df)
  expect_type(result, "character")
  expect_match(result, "NA: 5")
})

test_that(".summarise_df_for_llm() is deterministic across calls", {
  df <- data.frame(x = 1:10, y = letters[1:10], stringsAsFactors = FALSE)
  r1 <- certellm:::.summarise_df_for_llm(df)
  r2 <- certellm:::.summarise_df_for_llm(df)
  expect_identical(r1, r2)
})

# ===========================================================================
# .format_diver_schema_hint()
# ===========================================================================

test_that(".format_diver_schema_hint() returns a single character string", {
  result <- certellm:::.format_diver_schema_hint()
  expect_type(result, "character")
  expect_length(result, 1L)
})

test_that(".format_diver_schema_hint() is non-empty", {
  result <- certellm:::.format_diver_schema_hint()
  expect_true(nchar(result) > 100)
})

test_that(".format_diver_schema_hint() contains all expected Diver column names", {
  result <- certellm:::.format_diver_schema_hint()
  expected_cols <- c(
    "jaar", "maand", "kwartaal", "datum", "regio", "organisme",
    "materiaal", "geslacht", "leeftijd", "antibioticum", "resistentie",
    "uitslag", "ziekenhuis", "afdeling", "agens"
  )
  for (col in expected_cols) {
    expect_true(
      grepl(col, result, fixed = TRUE),
      info = paste("Expected Diver column not found:", col)
    )
  }
})

test_that(".format_diver_schema_hint() contains get_diver_data() example call", {
  result <- certellm:::.format_diver_schema_hint()
  expect_match(result, "get_diver_data", fixed = TRUE)
})

test_that(".format_diver_schema_hint() contains a ```r code block", {
  result <- certellm:::.format_diver_schema_hint()
  expect_match(result, "```r", fixed = TRUE)
})

test_that(".format_diver_schema_hint() mentions the Diver database", {
  result <- certellm:::.format_diver_schema_hint()
  expect_match(result, "Diver", fixed = TRUE)
})

test_that(".format_diver_schema_hint() is deterministic", {
  r1 <- certellm:::.format_diver_schema_hint()
  r2 <- certellm:::.format_diver_schema_hint()
  expect_identical(r1, r2)
})

# ===========================================================================
# read_preset()
# ===========================================================================

test_that("read_preset() errors immediately on empty name string", {
  expect_error(certellm:::read_preset(""), "No preset specified")
})

test_that("read_preset() errors when secrets file has no 'llm.presets' key", {
  tmp <- .make_secrets_yaml("other.key: value")
  old <- .set_secrets_env(tmp)
  on.exit({
    unlink(tmp)
    if (is.na(old)) Sys.unsetenv("secrets_file") else Sys.setenv(secrets_file = old)
  }, add = TRUE)

  expect_error(certellm:::read_preset("anything"), "No presets found")
})

test_that("read_preset() errors when the named preset does not exist", {
  tmp <- .make_secrets_yaml(c(
    "llm.presets:",
    "  existing:",
    "    provider: ollama",
    "    model: llama3"
  ))
  old <- .set_secrets_env(tmp)
  on.exit({
    unlink(tmp)
    if (is.na(old)) Sys.unsetenv("secrets_file") else Sys.setenv(secrets_file = old)
  }, add = TRUE)

  expect_error(certellm:::read_preset("nonexistent"), "not found")
})

test_that("read_preset() error message lists available preset names", {
  tmp <- .make_secrets_yaml(c(
    "llm.presets:",
    "  alpha:",
    "    provider: ollama",
    "    model: llama3",
    "  beta:",
    "    provider: anthropic",
    "    model: claude-3"
  ))
  old <- .set_secrets_env(tmp)
  on.exit({
    unlink(tmp)
    if (is.na(old)) Sys.unsetenv("secrets_file") else Sys.setenv(secrets_file = old)
  }, add = TRUE)

  err <- tryCatch(certellm:::read_preset("gamma"), error = function(e) e$message)
  expect_match(err, "alpha")
  expect_match(err, "beta")
})

test_that("read_preset() errors when preset is missing 'model' field", {
  tmp <- .make_secrets_yaml(c(
    "llm.presets:",
    "  nope:",
    "    provider: ollama"
  ))
  old <- .set_secrets_env(tmp)
  on.exit({
    unlink(tmp)
    if (is.na(old)) Sys.unsetenv("secrets_file") else Sys.setenv(secrets_file = old)
  }, add = TRUE)

  expect_error(certellm:::read_preset("nope"), "missing required field")
})

test_that("read_preset() errors when preset is missing 'provider' field", {
  tmp <- .make_secrets_yaml(c(
    "llm.presets:",
    "  nope:",
    "    model: llama3"
  ))
  old <- .set_secrets_env(tmp)
  on.exit({
    unlink(tmp)
    if (is.na(old)) Sys.unsetenv("secrets_file") else Sys.setenv(secrets_file = old)
  }, add = TRUE)

  expect_error(certellm:::read_preset("nope"), "missing required field")
})

test_that("read_preset() error mentions both missing fields when both absent", {
  tmp <- .make_secrets_yaml(c(
    "llm.presets:",
    "  nope:",
    "    url: http://localhost"
  ))
  old <- .set_secrets_env(tmp)
  on.exit({
    unlink(tmp)
    if (is.na(old)) Sys.unsetenv("secrets_file") else Sys.setenv(secrets_file = old)
  }, add = TRUE)

  err <- tryCatch(certellm:::read_preset("nope"), error = function(e) e$message)
  expect_match(err, "provider")
  expect_match(err, "model")
})

test_that("read_preset() returns a list with provider and model for a valid preset", {
  tmp <- .make_secrets_yaml(c(
    "llm.presets:",
    "  mypreset:",
    "    provider: ollama",
    "    model: llama3"
  ))
  old <- .set_secrets_env(tmp)
  on.exit({
    unlink(tmp)
    if (is.na(old)) Sys.unsetenv("secrets_file") else Sys.setenv(secrets_file = old)
  }, add = TRUE)

  result <- certellm:::read_preset("mypreset")
  expect_type(result, "list")
  expect_equal(result$provider, "ollama")
  expect_equal(result$model, "llama3")
})

test_that("read_preset() includes optional url field when present", {
  tmp <- .make_secrets_yaml(c(
    "llm.presets:",
    "  mypreset:",
    "    provider: ollama",
    "    model: llama3",
    "    url: http://localhost:11434"
  ))
  old <- .set_secrets_env(tmp)
  on.exit({
    unlink(tmp)
    if (is.na(old)) Sys.unsetenv("secrets_file") else Sys.setenv(secrets_file = old)
  }, add = TRUE)

  result <- certellm:::read_preset("mypreset")
  expect_equal(result$url, "http://localhost:11434")
})

test_that("read_preset() url is NULL when the field is absent", {
  tmp <- .make_secrets_yaml(c(
    "llm.presets:",
    "  mypreset:",
    "    provider: ollama",
    "    model: llama3"
  ))
  old <- .set_secrets_env(tmp)
  on.exit({
    unlink(tmp)
    if (is.na(old)) Sys.unsetenv("secrets_file") else Sys.setenv(secrets_file = old)
  }, add = TRUE)

  result <- certellm:::read_preset("mypreset")
  expect_null(result$url)
})

test_that("read_preset() selects the correct preset from multiple presets", {
  tmp <- .make_secrets_yaml(c(
    "llm.presets:",
    "  first:",
    "    provider: ollama",
    "    model: llama3",
    "  second:",
    "    provider: anthropic",
    "    model: claude-3"
  ))
  old <- .set_secrets_env(tmp)
  on.exit({
    unlink(tmp)
    if (is.na(old)) Sys.unsetenv("secrets_file") else Sys.setenv(secrets_file = old)
  }, add = TRUE)

  r1 <- certellm:::read_preset("first")
  r2 <- certellm:::read_preset("second")
  expect_equal(r1$provider, "ollama")
  expect_equal(r2$provider, "anthropic")
  expect_equal(r2$model, "claude-3")
})
