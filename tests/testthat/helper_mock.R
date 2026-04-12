# ===================================================================== #
#  An R package by Certe:                                               #
#  https://github.com/certe-medical-epidemiology                        #
#                                                                       #
#  Licensed as GPL-v2.0.                                                #
# ===================================================================== #

# Shared helpers loaded automatically by testthat before all test files.

#' Create a lightweight mock Chat object
#'
#' Returns an environment-based stand-in for an ellmer Chat R6 object that
#' satisfies the interface used by certellm package internals:
#'   $get_turns(), $set_turns(), $chat(), $clone(),
#'   $get_tools(), $get_system_prompt(), $get_tokens()
#'
#' @param initial_turns  List to pre-populate the turn history.
#' @param chat_response  Value returned verbatim by $chat().
make_mock_chat <- function(initial_turns  = list(),
                           chat_response  = "mock response") {
  turns <- initial_turns

  self <- new.env(parent = emptyenv())
  self$get_turns         <- function() turns
  self$set_turns         <- function(x) { turns <<- x; invisible(NULL) }
  self$chat              <- function(...) chat_response
  self$get_tools         <- function() list()
  self$get_system_prompt <- function() "Mock system prompt"
  self$get_tokens        <- function() 0L
  self$clone             <- function() make_mock_chat(turns, chat_response)

  class(self) <- c("MockChat", "environment")
  self
}
