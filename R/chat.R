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

#' Chat With Local LLM
#' 
#' @param input Text for input.
#' @param address Server address, defaults to local.
#' @param port Server port, defaults to local.
#' @param ... Argument passed on to [ellmer::chat_ollama()].
#' @inheritParams ellmer::chat_ollama
#' @importFrom ellmer chat_ollama
#' @rdname chat
#' @name chat
#' @export
initiate_llm <- function(address = "http://localhost",
                           port = 11434,
                           system_prompt = read_secret("llm.system_prompt"),
                           model = read_secret("llm.default.model"),
                           ...) {
  base_url <- paste0(address, ":", port)
  system_prompt <- paste0(system_prompt,
                          "# Over de gebruiker\n",
                          "Je gebruiker heet ", read_secret(paste0("user.", Sys.info()["user"], ".fullname")),
                          ", die bij Certe werkt als ", tolower(read_secret(paste0("user.", Sys.info()["user"], ".jobtitle"))),
                          ". Je bent diens persoonlijke, behulpzame assistent.")
  pkg_env$chat_model <- chat_ollama(system_prompt = system_prompt,
                                    base_url = base_url,
                                    model = read_secret("llm.default.model"),
                                    ...)
  # pkg_env$chat_model$register_tool(tool_plot2)
  message("LLM opgestart: ",
          pkg_env$chat_model$get_provider()@name, "/",
          pkg_env$chat_model$get_provider()@model, " via ",
          pkg_env$chat_model$get_provider()@base_url)
}

#' @rdname chat
#' @export
chat <- function(input,
                 ...) {
  if (is.null(pkg_env$chat_model)) {
    initiate_llm(...)
  }
  pkg_env$chat_model$chat(input)
}

#' @rdname chat
#' @importFrom ellmer live_browser
#' @export
chat_in_browser <- function(...) {
  if (is.null(pkg_env$chat_model)) {
    initiate_llm(...)
  }
  cli::cat_boxx(c("Chat starten in browser.", "Gebruik Ctrl+C om af te sluiten."), 
                padding = c(0, 1, 0, 1), border_style = "double")
  live_browser(pkg_env$chat_model, quiet = TRUE)
}

#' @rdname chat
#' @importFrom ellmer live_console
#' @export
chat_in_console <- function(...) {
  if (is.null(pkg_env$chat_model)) {
    initiate_llm(...)
  }
  cli::cat_boxx(c("Chat starten in console.", "Gebruik \"\"\" voor multi-line input.", 
                  "Type 'Q' to af te sluiten."), padding = c(0, 1, 0, 1), border_style = "double")
  live_console(pkg_env$chat_model, quiet = TRUE)
}

#' @rdname chat
#' @export
get_provider <- function() {
  if (!is.null(pkg_env$chat_model)) {
    pkg_env$chat_model$get_provider()
  }
}

#' @rdname chat
#' @export
get_system_prompt <- function() {
  if (!is.null(pkg_env$chat_model)) {
    pkg_env$chat_model$get_system_prompt()
  }
}

#' @rdname chat
#' @export
get_tokens <- function() {
  if (!is.null(pkg_env$chat_model)) {
    pkg_env$chat_model$get_tokens()
  }
}

