# ===================================================================== #
#  An R package by Certe:                                               #
#  https://github.com/certe-medical-epidemiology                        #
#                                                                       #
#  Licensed as GPL-v2.0.                                                #
# ===================================================================== #

# Tests for chat history management:
#   list_chats(), save_chat(), restore_chat(), reset_chat()

# ===========================================================================
# list_chats() – no LLM required
# ===========================================================================

test_that("list_chats() returns an empty character vector when history is NULL", {
  old <- .pkg_env$chat_history
  .pkg_env$chat_history <- NULL
  on.exit(.pkg_env$chat_history <- old, add = TRUE)

  result <- list_chats()
  expect_equal(result, character(0))
  expect_type(result, "character")
})

test_that("list_chats() returns all saved snapshot names", {
  old <- .pkg_env$chat_history
  .pkg_env$chat_history <- list(snap1 = list(), snap2 = list(), snap3 = list())
  on.exit(.pkg_env$chat_history <- old, add = TRUE)

  result <- list_chats()
  expect_equal(result, c("snap1", "snap2", "snap3"))
})

test_that("list_chats() preserves insertion order of names", {
  old <- .pkg_env$chat_history
  .pkg_env$chat_history <- list(z = list(), a = list(), m = list())
  on.exit(.pkg_env$chat_history <- old, add = TRUE)

  expect_equal(list_chats(), c("z", "a", "m"))
})

# ===========================================================================
# save_chat() – requires a mock chat object
# ===========================================================================

test_that("save_chat() errors when no LLM is initiated", {
  old_obj <- .pkg_env$chat_object
  .pkg_env$chat_object <- NULL
  on.exit(.pkg_env$chat_object <- old_obj, add = TRUE)

  expect_error(save_chat("test"), "No LLM initiated")
})

test_that("save_chat() stores turns under the supplied name", {
  old_obj  <- .pkg_env$chat_object
  old_hist <- .pkg_env$chat_history
  .pkg_env$chat_object  <- make_mock_chat()
  .pkg_env$chat_history <- NULL
  on.exit({
    .pkg_env$chat_object  <- old_obj
    .pkg_env$chat_history <- old_hist
  }, add = TRUE)

  mock_turns <- list("turn_a", "turn_b")
  .pkg_env$chat_object$set_turns(mock_turns)

  expect_message(save_chat("my_snap"), "Chat saved as 'my_snap'")
  expect_true("my_snap" %in% names(.pkg_env$chat_history))
  expect_equal(.pkg_env$chat_history[["my_snap"]], mock_turns)
})

test_that("save_chat() creates the chat_history list when it does not exist yet", {
  old_obj  <- .pkg_env$chat_object
  old_hist <- .pkg_env$chat_history
  .pkg_env$chat_object  <- make_mock_chat()
  .pkg_env$chat_history <- NULL
  on.exit({
    .pkg_env$chat_object  <- old_obj
    .pkg_env$chat_history <- old_hist
  }, add = TRUE)

  expect_message(save_chat("first"), "Chat saved as 'first'")
  expect_false(is.null(.pkg_env$chat_history))
  expect_true("first" %in% names(.pkg_env$chat_history))
})

test_that("save_chat() can save multiple snapshots under different names", {
  old_obj  <- .pkg_env$chat_object
  old_hist <- .pkg_env$chat_history
  .pkg_env$chat_object  <- make_mock_chat()
  .pkg_env$chat_history <- NULL
  on.exit({
    .pkg_env$chat_object  <- old_obj
    .pkg_env$chat_history <- old_hist
  }, add = TRUE)

  expect_message(save_chat("snap_a"), "Chat saved as 'snap_a'")
  expect_message(save_chat("snap_b"), "Chat saved as 'snap_b'")
  expect_equal(sort(names(.pkg_env$chat_history)), c("snap_a", "snap_b"))
})

test_that("save_chat() overwrites an existing snapshot with the same name", {
  old_obj  <- .pkg_env$chat_object
  old_hist <- .pkg_env$chat_history
  .pkg_env$chat_object  <- make_mock_chat()
  .pkg_env$chat_history <- list(old_snap = list("old_turn"))
  on.exit({
    .pkg_env$chat_object  <- old_obj
    .pkg_env$chat_history <- old_hist
  }, add = TRUE)

  .pkg_env$chat_object$set_turns(list("new_turn"))
  suppressMessages(save_chat("old_snap"))
  expect_equal(.pkg_env$chat_history[["old_snap"]], list("new_turn"))
})

test_that("save_chat() default name follows YYYYMMDD_HHMMSS format", {
  old_obj  <- .pkg_env$chat_object
  old_hist <- .pkg_env$chat_history
  .pkg_env$chat_object  <- make_mock_chat()
  .pkg_env$chat_history <- NULL
  on.exit({
    .pkg_env$chat_object  <- old_obj
    .pkg_env$chat_history <- old_hist
  }, add = TRUE)

  expect_message(save_chat(), regexp = "Chat saved as '")
  nms <- names(.pkg_env$chat_history)
  expect_true(grepl("^[0-9]{8}_[0-9]{6}$", nms[1]))
})

test_that("save_chat() returns invisible NULL", {
  old_obj  <- .pkg_env$chat_object
  old_hist <- .pkg_env$chat_history
  .pkg_env$chat_object  <- make_mock_chat()
  .pkg_env$chat_history <- NULL
  on.exit({
    .pkg_env$chat_object  <- old_obj
    .pkg_env$chat_history <- old_hist
  }, add = TRUE)

  result <- suppressMessages(save_chat("x"))
  expect_null(result)
})

# ===========================================================================
# restore_chat() – requires a mock chat object
# ===========================================================================

test_that("restore_chat() errors when no LLM is initiated", {
  old_obj <- .pkg_env$chat_object
  .pkg_env$chat_object <- NULL
  on.exit(.pkg_env$chat_object <- old_obj, add = TRUE)

  expect_error(restore_chat("any"), "No LLM initiated")
})

test_that("restore_chat() errors when chat_history is NULL", {
  old_obj  <- .pkg_env$chat_object
  old_hist <- .pkg_env$chat_history
  .pkg_env$chat_object  <- make_mock_chat()
  .pkg_env$chat_history <- NULL
  on.exit({
    .pkg_env$chat_object  <- old_obj
    .pkg_env$chat_history <- old_hist
  }, add = TRUE)

  expect_error(restore_chat("nonexistent"), "No saved chat named 'nonexistent'")
})

test_that("restore_chat() errors when snapshot name is not in history", {
  old_obj  <- .pkg_env$chat_object
  old_hist <- .pkg_env$chat_history
  .pkg_env$chat_object  <- make_mock_chat()
  .pkg_env$chat_history <- list(exists = list())
  on.exit({
    .pkg_env$chat_object  <- old_obj
    .pkg_env$chat_history <- old_hist
  }, add = TRUE)

  expect_error(restore_chat("missing"), "No saved chat named 'missing'")
})

test_that("restore_chat() error message mentions list_chats()", {
  old_obj  <- .pkg_env$chat_object
  old_hist <- .pkg_env$chat_history
  .pkg_env$chat_object  <- make_mock_chat()
  .pkg_env$chat_history <- list()
  on.exit({
    .pkg_env$chat_object  <- old_obj
    .pkg_env$chat_history <- old_hist
  }, add = TRUE)

  err <- tryCatch(restore_chat("x"), error = function(e) e$message)
  expect_match(err, "list_chats()", fixed = TRUE)
})

test_that("restore_chat() restores the correct turns", {
  old_obj  <- .pkg_env$chat_object
  old_hist <- .pkg_env$chat_history
  saved_turns <- list("t1", "t2", "t3")
  .pkg_env$chat_object  <- make_mock_chat()
  .pkg_env$chat_history <- list(mysnapshot = saved_turns)
  on.exit({
    .pkg_env$chat_object  <- old_obj
    .pkg_env$chat_history <- old_hist
  }, add = TRUE)

  expect_message(restore_chat("mysnapshot"), "Chat 'mysnapshot' restored")
  expect_equal(.pkg_env$chat_object$get_turns(), saved_turns)
})

test_that("restore_chat() replaces current turns with the saved ones", {
  old_obj  <- .pkg_env$chat_object
  old_hist <- .pkg_env$chat_history
  .pkg_env$chat_object  <- make_mock_chat()
  .pkg_env$chat_history <- list(old = list("old_a", "old_b"))
  .pkg_env$chat_object$set_turns(list("current"))
  on.exit({
    .pkg_env$chat_object  <- old_obj
    .pkg_env$chat_history <- old_hist
  }, add = TRUE)

  suppressMessages(restore_chat("old"))
  expect_equal(.pkg_env$chat_object$get_turns(), list("old_a", "old_b"))
})

test_that("restore_chat() returns invisible NULL", {
  old_obj  <- .pkg_env$chat_object
  old_hist <- .pkg_env$chat_history
  .pkg_env$chat_object  <- make_mock_chat()
  .pkg_env$chat_history <- list(s = list())
  on.exit({
    .pkg_env$chat_object  <- old_obj
    .pkg_env$chat_history <- old_hist
  }, add = TRUE)

  result <- suppressMessages(restore_chat("s"))
  expect_null(result)
})

# ===========================================================================
# reset_chat() – requires a mock chat object
# ===========================================================================

test_that("reset_chat() errors when no LLM is initiated", {
  old_obj <- .pkg_env$chat_object
  .pkg_env$chat_object <- NULL
  on.exit(.pkg_env$chat_object <- old_obj, add = TRUE)

  expect_error(reset_chat(), "No LLM initiated")
})

test_that("reset_chat() clears all turns", {
  old_obj <- .pkg_env$chat_object
  .pkg_env$chat_object <- make_mock_chat()
  .pkg_env$chat_object$set_turns(list("turn1", "turn2", "turn3"))
  on.exit(.pkg_env$chat_object <- old_obj, add = TRUE)

  expect_message(reset_chat(), "Conversation history cleared")
  expect_length(.pkg_env$chat_object$get_turns(), 0L)
})

test_that("reset_chat() succeeds when turns are already empty", {
  old_obj <- .pkg_env$chat_object
  .pkg_env$chat_object <- make_mock_chat()
  on.exit(.pkg_env$chat_object <- old_obj, add = TRUE)

  expect_message(reset_chat(), "Conversation history cleared")
  expect_equal(.pkg_env$chat_object$get_turns(), list())
})

test_that("reset_chat() does not touch saved chat_history snapshots", {
  old_obj  <- .pkg_env$chat_object
  old_hist <- .pkg_env$chat_history
  .pkg_env$chat_object  <- make_mock_chat()
  .pkg_env$chat_history <- list(kept = list("t"))
  .pkg_env$chat_object$set_turns(list("active"))
  on.exit({
    .pkg_env$chat_object  <- old_obj
    .pkg_env$chat_history <- old_hist
  }, add = TRUE)

  suppressMessages(reset_chat())
  expect_equal(names(.pkg_env$chat_history), "kept")
})

test_that("reset_chat() returns invisible NULL", {
  old_obj <- .pkg_env$chat_object
  .pkg_env$chat_object <- make_mock_chat()
  on.exit(.pkg_env$chat_object <- old_obj, add = TRUE)

  result <- suppressMessages(reset_chat())
  expect_null(result)
})

# ===========================================================================
# Round-trip: save → reset → restore
# ===========================================================================

test_that("save/reset/restore round-trip preserves the conversation", {
  old_obj  <- .pkg_env$chat_object
  old_hist <- .pkg_env$chat_history
  .pkg_env$chat_object  <- make_mock_chat()
  .pkg_env$chat_history <- NULL
  on.exit({
    .pkg_env$chat_object  <- old_obj
    .pkg_env$chat_history <- old_hist
  }, add = TRUE)

  original_turns <- list("msg1", "msg2")
  .pkg_env$chat_object$set_turns(original_turns)

  suppressMessages(save_chat("checkpoint"))
  suppressMessages(reset_chat())
  expect_length(.pkg_env$chat_object$get_turns(), 0L)

  suppressMessages(restore_chat("checkpoint"))
  expect_equal(.pkg_env$chat_object$get_turns(), original_turns)
})
