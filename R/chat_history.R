# ===================================================================== #
#  An R package by Certe:                                               #
#  https://github.com/certe-medical-epidemiology                        #
#                                                                       #
#  Licensed as GPL-v2.0.                                                #
#                                                                       #
#  Developed at non-profit organisation Certe Medical Diagnostics &     #
#  Advice, department of Medical Epidemiology.                          #
#                                                                       #
#  This R package is free software; you can freely use and distribute   #
#  it for both personal and commercial purposes under the terms of the  #
#  GNU General Public License version 2.0 (GNU GPL-2), as published by  #
#  the Free Software Foundation.                                        #
#                                                                       #
#  We created this package for both routine data analysis and academic  #
#  research and it was publicly released in the hope that it will be    #
#  useful, but it comes WITHOUT ANY WARRANTY OR LIABILITY.              #
# ===================================================================== #

#' Save and Restore Chat Conversations
#'
#' @description
#' These functions manage the turn history of the active LLM conversation so
#' that sessions can be saved, restored, listed, or reset.
#'
#' - `save_chat()` snapshots the current conversation under a name.
#' - `restore_chat()` replaces the current conversation with a saved snapshot.
#' - `list_chats()` returns the names of all saved snapshots.
#' - `reset_chat()` clears the current conversation turn history.
#'
#' @param name Name under which to save or from which to restore the snapshot.
#'   Defaults to a timestamp (`"YYYYMMDD_HHMMSS"`).
#'
#' @return `save_chat()` and `reset_chat()` return `invisible(NULL)`.
#'   `restore_chat()` returns `invisible(NULL)`. `list_chats()` returns a
#'   character vector of snapshot names (or an empty vector if none exist).
#' @rdname chat_history
#' @name chat_history
#' @export
save_chat <- function(name = format(Sys.time(), "%Y%m%d_%H%M%S")) {
  if (is.null(pkg_env$chat_object)) {
    stop("No LLM initiated. Run initiate_llm() first.", call. = FALSE)
  }
  if (is.null(pkg_env$chat_history)) {
    pkg_env$chat_history <- list()
  }
  pkg_env$chat_history[[name]] <- pkg_env$chat_object$get_turns()
  message("Chat saved as '", name, "'.")
  invisible(NULL)
}

#' @rdname chat_history
#' @export
restore_chat <- function(name) {
  if (is.null(pkg_env$chat_object)) {
    stop("No LLM initiated. Run initiate_llm() first.", call. = FALSE)
  }
  if (is.null(pkg_env$chat_history) || !name %in% names(pkg_env$chat_history)) {
    stop("No saved chat named '", name, "'. Use list_chats() to see available names.",
         call. = FALSE)
  }
  pkg_env$chat_object$set_turns(pkg_env$chat_history[[name]])
  message("Chat '", name, "' restored.")
  invisible(NULL)
}

#' @rdname chat_history
#' @export
list_chats <- function() {
  if (is.null(pkg_env$chat_history)) return(character(0))
  names(pkg_env$chat_history)
}

#' @rdname chat_history
#' @export
reset_chat <- function() {
  if (is.null(pkg_env$chat_object)) {
    stop("No LLM initiated. Run initiate_llm() first.", call. = FALSE)
  }
  pkg_env$chat_object$set_turns(list())
  message("Conversation history cleared.")
  invisible(NULL)
}
