# ===================================================================== #
#  An R package by Certe:                                               #
#  https://github.com/certe-medical-epidemiology                        #
#                                                                       #
#  Licensed as GPL-v2.0.                                                #
# ===================================================================== #

# Tests for LLM state accessor functions:
#   get_chat_object(), get_tools(), get_system_prompt(), get_tokens()
#
# Two scenarios per function:
#   1. No LLM initiated (pkg_env$chat_object is NULL)
#   2. A mock LLM is present

# ===========================================================================
# get_chat_object()
# ===========================================================================

test_that("get_chat_object() emits a message and returns NULL when no LLM", {
  old_obj <- .pkg_env$chat_object
  .pkg_env$chat_object <- NULL
  on.exit(.pkg_env$chat_object <- old_obj, add = TRUE)

  expect_message(result <- get_chat_object(), "No LLM initiated")
  expect_null(result)
})

test_that("get_chat_object() returns the mock object when an LLM is set", {
  old_obj <- .pkg_env$chat_object
  mock    <- make_mock_chat()
  .pkg_env$chat_object <- mock
  on.exit(.pkg_env$chat_object <- old_obj, add = TRUE)

  result <- get_chat_object()
  expect_identical(result, mock)
})

# ===========================================================================
# get_tools()
# ===========================================================================

test_that("get_tools() emits 'No LLM initiated' and returns NULL when no LLM", {
  old_obj <- .pkg_env$chat_object
  .pkg_env$chat_object <- NULL
  on.exit(.pkg_env$chat_object <- old_obj, add = TRUE)

  expect_message(result <- get_tools(), "No LLM initiated")
  expect_null(result)
})

test_that("get_tools() emits 'Tools are disabled' and returns NULL when tools off", {
  old_obj  <- .pkg_env$chat_object
  old_tool <- .pkg_env$tools_supported
  .pkg_env$chat_object    <- make_mock_chat()
  .pkg_env$tools_supported <- FALSE
  on.exit({
    .pkg_env$chat_object    <- old_obj
    .pkg_env$tools_supported <- old_tool
  }, add = TRUE)

  expect_message(result <- get_tools(), "Tools are disabled")
  expect_null(result)
})

test_that("get_tools() returns a list when tools are supported", {
  old_obj  <- .pkg_env$chat_object
  old_tool <- .pkg_env$tools_supported
  .pkg_env$chat_object    <- make_mock_chat()
  .pkg_env$tools_supported <- TRUE
  on.exit({
    .pkg_env$chat_object    <- old_obj
    .pkg_env$tools_supported <- old_tool
  }, add = TRUE)

  result <- get_tools()
  expect_type(result, "list")
})

# ===========================================================================
# get_system_prompt()
# ===========================================================================

test_that("get_system_prompt() emits a message and then errors when no LLM", {
  old_obj <- .pkg_env$chat_object
  .pkg_env$chat_object <- NULL
  on.exit(.pkg_env$chat_object <- old_obj, add = TRUE)

  # The function emits "No LLM initiated" then calls NULL$get_system_prompt()
  # which raises an error.
  expect_error(suppressMessages(get_system_prompt()))
})

test_that("get_system_prompt() returns the prompt string when an LLM is set", {
  old_obj <- .pkg_env$chat_object
  .pkg_env$chat_object <- make_mock_chat()
  on.exit(.pkg_env$chat_object <- old_obj, add = TRUE)

  result <- get_system_prompt()
  expect_type(result, "character")
  expect_equal(result, "Mock system prompt")
})

# ===========================================================================
# get_tokens()
# ===========================================================================

test_that("get_tokens() emits a message and then errors when no LLM", {
  old_obj <- .pkg_env$chat_object
  .pkg_env$chat_object <- NULL
  on.exit(.pkg_env$chat_object <- old_obj, add = TRUE)

  expect_error(suppressMessages(get_tokens()))
})

test_that("get_tokens() returns a numeric value when an LLM is set", {
  old_obj <- .pkg_env$chat_object
  .pkg_env$chat_object <- make_mock_chat()
  on.exit(.pkg_env$chat_object <- old_obj, add = TRUE)

  result <- get_tokens()
  expect_true(is.numeric(result) || is.integer(result))
  expect_equal(as.numeric(result), 0)
})
