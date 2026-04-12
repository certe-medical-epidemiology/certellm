# ===================================================================== #
#  An R package by Certe:                                               #
#  https://github.com/certe-medical-epidemiology                        #
#                                                                       #
#  Licensed as GPL-v2.0.                                                #
# ===================================================================== #

# Tests for input validation and happy-path behaviour of all LLM-facing
# functions.  A mock chat object is injected wherever needed so that the
# code under test never attempts a real LLM connection.
#
# Validation that runs BEFORE .fresh_clone() (match.arg, explicit stops)
# can be tested without a mock.
# Everything else requires setting pkg_env$chat_object to a mock first.

# Small data frame used across tests
sample_df <- data.frame(
  x   = 1:10,
  y   = as.numeric(seq(0.1, 1.0, by = 0.1)),
  grp = rep(c("A", "B"), 5L),
  stringsAsFactors = FALSE
)

# Helper: set mock and register cleanup in the caller via explicit on.exit.
# Because on.exit registers cleanup in the *current* function frame, tests
# that call this must also call on.exit themselves (see pattern below).
.set_mock <- function() {
  certellm:::pkg_env$chat_object    <- make_mock_chat()
  certellm:::pkg_env$tools_supported <- TRUE
}
.clear_mock <- function() {
  certellm:::pkg_env$chat_object    <- NULL
  certellm:::pkg_env$tools_supported <- NULL
}

# ===========================================================================
# llm_write_chunk() – validation before .fresh_clone()
# ===========================================================================

test_that("llm_write_chunk() errors when neither code nor goal is provided", {
  expect_error(llm_write_chunk(), "Provide at least `goal`")
})

# ===========================================================================
# match.arg() validation – all occur before .fresh_clone(), no mock needed
# ===========================================================================

test_that("llm_code() errors on an invalid style value", {
  expect_error(llm_code(task = "do something", style = "invalid"))
})

test_that("llm_explain() errors on an invalid detail value", {
  expect_error(llm_explain(code = "x <- 1", detail = "wrong"))
})

test_that("llm_describe() errors on an invalid tone value", {
  expect_error(llm_describe(sample_df, tone = "casual"))
})

test_that("llm_describe() errors on an invalid length value", {
  expect_error(llm_describe(sample_df, length = "very_long"))
})

test_that("llm_review_document() errors on an invalid focus value", {
  expect_error(llm_review_document(text = "some text", focus = "unknown"))
})

test_that("llm_write_section() errors on an invalid style value", {
  expect_error(llm_write_section("Results", style = "blog_post"))
})

# ===========================================================================
# llm_review_document() – file/text validation (after .fresh_clone, need mock)
# ===========================================================================

test_that("llm_review_document() errors when neither file nor text is provided", {
  .set_mock()
  on.exit(.clear_mock(), add = TRUE)

  expect_error(llm_review_document(), "Provide either `file` or `text`")
})

test_that("llm_review_document() errors when file does not exist", {
  .set_mock()
  on.exit(.clear_mock(), add = TRUE)

  expect_error(
    llm_review_document(file = "/tmp/certellm_nonexistent_test_file.Rmd"),
    "File not found"
  )
})

# ===========================================================================
# llm_review_document() – happy paths
# ===========================================================================

test_that("llm_review_document() returns a character string for text input", {
  .set_mock()
  on.exit(.clear_mock(), add = TRUE)

  result <- llm_review_document(text = "# Title\n\nSome content here.")
  expect_type(result, "character")
})

test_that("llm_review_document() returns a character string for file input", {
  .set_mock()
  on.exit(.clear_mock(), add = TRUE)

  tmpfile <- tempfile(fileext = ".Rmd")
  writeLines(c("# Title", "", "Content of the document."), tmpfile)
  on.exit(unlink(tmpfile), add = TRUE)

  result <- llm_review_document(file = tmpfile)
  expect_type(result, "character")
})

test_that("llm_review_document() works with every valid focus value", {
  .set_mock()
  on.exit(.clear_mock(), add = TRUE)

  for (f in c("all", "language", "structure", "completeness")) {
    result <- llm_review_document(text = "Some text.", focus = f)
    expect_type(result, "character", info = paste("focus =", f))
  }
})

test_that("llm_review_document() truncates files longer than 3000 lines silently", {
  .set_mock()
  on.exit(.clear_mock(), add = TRUE)

  tmpfile <- tempfile(fileext = ".Rmd")
  writeLines(rep("line", 4000L), tmpfile)
  on.exit(unlink(tmpfile), add = TRUE)

  result <- llm_review_document(file = tmpfile)
  expect_type(result, "character")
})

# ===========================================================================
# llm_write_chunk() – happy paths
# ===========================================================================

test_that("llm_write_chunk() returns a character string when only goal is given", {
  .set_mock()
  on.exit(.clear_mock(), add = TRUE)

  result <- llm_write_chunk(goal = "plot the data")
  expect_type(result, "character")
})

test_that("llm_write_chunk() returns a character string when only code is given", {
  .set_mock()
  on.exit(.clear_mock(), add = TRUE)

  result <- llm_write_chunk(code = "x <- 1 + 1")
  expect_type(result, "character")
})

test_that("llm_write_chunk() returns a character string when both code and goal", {
  .set_mock()
  on.exit(.clear_mock(), add = TRUE)

  result <- llm_write_chunk(code = "x <- 1 + 1", goal = "compute a sum")
  expect_type(result, "character")
})

# ===========================================================================
# llm_write_section() – happy paths
# ===========================================================================

test_that("llm_write_section() returns a character string", {
  .set_mock()
  on.exit(.clear_mock(), add = TRUE)

  result <- llm_write_section("Methods")
  expect_type(result, "character")
})

test_that("llm_write_section() works with every valid style value", {
  .set_mock()
  on.exit(.clear_mock(), add = TRUE)

  for (s in c("report", "presentation", "article")) {
    result <- llm_write_section("Discussion", style = s)
    expect_type(result, "character", info = paste("style =", s))
  }
})

test_that("llm_write_section() works with optional data argument", {
  .set_mock()
  on.exit(.clear_mock(), add = TRUE)

  result <- llm_write_section("Results", data = sample_df)
  expect_type(result, "character")
})

test_that("llm_write_section() works with optional context argument", {
  .set_mock()
  on.exit(.clear_mock(), add = TRUE)

  result <- llm_write_section("Introduction", context = "Background info.")
  expect_type(result, "character")
})

test_that("llm_write_section() works with both data and context arguments", {
  .set_mock()
  on.exit(.clear_mock(), add = TRUE)

  result <- llm_write_section("Results", data = sample_df, context = "Context text.")
  expect_type(result, "character")
})

# ===========================================================================
# llm_analyse()
# ===========================================================================

test_that("llm_analyse() returns a character string with default arguments", {
  .set_mock()
  on.exit(.clear_mock(), add = TRUE)

  result <- llm_analyse(sample_df)
  expect_type(result, "character")
})

test_that("llm_analyse() returns a character string when a question is provided", {
  .set_mock()
  on.exit(.clear_mock(), add = TRUE)

  result <- llm_analyse(sample_df, question = "What trends do you see?")
  expect_type(result, "character")
})

# ===========================================================================
# llm_code()
# ===========================================================================

test_that("llm_code() returns a character string with tidyverse style (default)", {
  .set_mock()
  on.exit(.clear_mock(), add = TRUE)

  result <- llm_code(task = "filter rows where x > 5")
  expect_type(result, "character")
})

test_that("llm_code() returns a character string with base R style", {
  .set_mock()
  on.exit(.clear_mock(), add = TRUE)

  result <- llm_code(task = "filter rows where x > 5", style = "base")
  expect_type(result, "character")
})

test_that("llm_code() works when a data frame is provided", {
  .set_mock()
  on.exit(.clear_mock(), add = TRUE)

  result <- llm_code(data = sample_df, task = "summarise by group")
  expect_type(result, "character")
})

# ===========================================================================
# llm_explain()
# ===========================================================================

test_that("llm_explain() returns a character string for brief detail (default)", {
  .set_mock()
  on.exit(.clear_mock(), add = TRUE)

  result <- llm_explain(code = "x <- iris |> subset(Species == 'setosa')")
  expect_type(result, "character")
})

test_that("llm_explain() returns a character string for detailed explanation", {
  .set_mock()
  on.exit(.clear_mock(), add = TRUE)

  result <- llm_explain(code = "x <- 1 + 1\ny <- x * 2", detail = "detailed")
  expect_type(result, "character")
})

# ===========================================================================
# llm_suggest_plot()
# ===========================================================================

test_that("llm_suggest_plot() returns a character string", {
  .set_mock()
  on.exit(.clear_mock(), add = TRUE)

  result <- llm_suggest_plot(sample_df)
  expect_type(result, "character")
})

test_that("llm_suggest_plot() works with a goal description", {
  .set_mock()
  on.exit(.clear_mock(), add = TRUE)

  result <- llm_suggest_plot(sample_df, goal = "Show distribution of y by grp")
  expect_type(result, "character")
})

# ===========================================================================
# llm_describe()
# ===========================================================================

test_that("llm_describe() returns a character string with default arguments", {
  .set_mock()
  on.exit(.clear_mock(), add = TRUE)

  result <- llm_describe(sample_df)
  expect_type(result, "character")
})

test_that("llm_describe() works with every valid tone value", {
  .set_mock()
  on.exit(.clear_mock(), add = TRUE)

  for (t in c("formal", "accessible")) {
    result <- llm_describe(sample_df, tone = t)
    expect_type(result, "character", info = paste("tone =", t))
  }
})

test_that("llm_describe() works with every valid length value", {
  .set_mock()
  on.exit(.clear_mock(), add = TRUE)

  for (l in c("short", "medium", "long")) {
    result <- llm_describe(sample_df, length = l)
    expect_type(result, "character", info = paste("length =", l))
  }
})

test_that("llm_describe() works with optional topic, period and region", {
  .set_mock()
  on.exit(.clear_mock(), add = TRUE)

  result <- llm_describe(
    sample_df,
    topic  = "E. coli resistance",
    period = "2023-2024",
    region = "Groningen"
  )
  expect_type(result, "character")
})

# ===========================================================================
# llm_interpret()
# ===========================================================================

test_that("llm_interpret() returns a character string", {
  .set_mock()
  on.exit(.clear_mock(), add = TRUE)

  result <- llm_interpret(sample_df)
  expect_type(result, "character")
})

test_that("llm_interpret() works with an optional hypothesis", {
  .set_mock()
  on.exit(.clear_mock(), add = TRUE)

  result <- llm_interpret(sample_df, hypothesis = "There is a seasonal trend")
  expect_type(result, "character")
})

test_that("llm_interpret() works with an optional comparison data frame", {
  .set_mock()
  on.exit(.clear_mock(), add = TRUE)

  comparison_df <- data.frame(x = 11:20, y = as.numeric(1:10))
  result <- llm_interpret(sample_df, comparison = comparison_df)
  expect_type(result, "character")
})

test_that("llm_interpret() works with both hypothesis and comparison", {
  .set_mock()
  on.exit(.clear_mock(), add = TRUE)

  comparison_df <- data.frame(x = 11:20, y = as.numeric(1:10))
  result <- llm_interpret(
    sample_df,
    hypothesis = "Hypothesis text",
    comparison = comparison_df
  )
  expect_type(result, "character")
})

# ===========================================================================
# llm_conclude()
# ===========================================================================

test_that("llm_conclude() returns a character string from plain text input", {
  .set_mock()
  on.exit(.clear_mock(), add = TRUE)

  result <- llm_conclude("Resistance rates increased by 5% in 2024.")
  expect_type(result, "character")
})

test_that("llm_conclude() works with a data frame input", {
  .set_mock()
  on.exit(.clear_mock(), add = TRUE)

  result <- llm_conclude(sample_df)
  expect_type(result, "character")
})

test_that("llm_conclude() works with mixed data frame and text inputs", {
  .set_mock()
  on.exit(.clear_mock(), add = TRUE)

  result <- llm_conclude(sample_df, "Additional text context.")
  expect_type(result, "character")
})

test_that("llm_conclude() works with multiple text inputs", {
  .set_mock()
  on.exit(.clear_mock(), add = TRUE)

  result <- llm_conclude("First result.", "Second result.", "Third result.")
  expect_type(result, "character")
})

# ===========================================================================
# llm_diver_query()
# ===========================================================================

test_that("llm_diver_query() returns a character string", {
  .set_mock()
  on.exit(.clear_mock(), add = TRUE)

  result <- llm_diver_query("All E. coli urine cultures from 2024")
  expect_type(result, "character")
})

test_that("llm_diver_query() works with an optional table hint", {
  .set_mock()
  on.exit(.clear_mock(), add = TRUE)

  result <- llm_diver_query(
    "Blood cultures from Groningen 2023",
    table = "microbiologie"
  )
  expect_type(result, "character")
})

test_that("llm_diver_query() works with known column name hints", {
  .set_mock()
  on.exit(.clear_mock(), add = TRUE)

  result <- llm_diver_query(
    "E. coli cultures by region",
    columns_hint = c("jaar", "organisme", "regio", "materiaal")
  )
  expect_type(result, "character")
})

test_that("llm_diver_query() works with both table and columns_hint", {
  .set_mock()
  on.exit(.clear_mock(), add = TRUE)

  result <- llm_diver_query(
    description  = "Resistance data from 2022",
    table        = "resistentie",
    columns_hint = c("jaar", "antibioticum", "resistentie")
  )
  expect_type(result, "character")
})

# ===========================================================================
# llm_diver_explain()
# ===========================================================================

test_that("llm_diver_explain() returns a character string", {
  .set_mock()
  on.exit(.clear_mock(), add = TRUE)

  result <- llm_diver_explain(
    'get_diver_data(where = jaar == 2024 & organisme == "Escherichia coli")'
  )
  expect_type(result, "character")
})

test_that("llm_diver_explain() works with a complex query string", {
  .set_mock()
  on.exit(.clear_mock(), add = TRUE)

  query <- paste0(
    'get_diver_data(where = jaar >= 2022 & regio == "Groningen" ',
    '& materiaal == "urine")'
  )
  result <- llm_diver_explain(query)
  expect_type(result, "character")
})
