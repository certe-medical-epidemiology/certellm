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

#' Chat With (Local) LLM
#'
#' These functions initialise and interact with the active LLM, backed by `ellmer::chat()`. The connection is configured via named presets defined in the secrets YAML under `llm.presets`; individual `provider`, `model`, and `url` arguments override the preset when supplied.
#' @param input Text for input.
#' @param preset Name of a preset defined under `llm.presets` in the secrets
#'   YAML. Defaults to `llm.default.preset`. Each preset must contain a
#'   `provider` (e.g. `"ollama"`, `"anthropic"`) and a `model` field; an
#'   optional `url` sets a non-default base URL (e.g. an internal Ollama
#'   server). Individual `provider`, `model`, and `url` arguments override
#'   the preset when supplied.
#' @param provider Override the provider from the preset.
#' @param model Override the model from the preset.
#' @param url Override the base URL from the preset. Passed as `base_url` to
#'   the provider function; omit for providers with fixed endpoints
#'   (e.g. Anthropic).
#' @param system_prompt System prompt text. Defaults to `llm.system_prompt`
#'   from the secrets file.
#' @param new Initiate new LLM instance.
#' @param ... Arguments passed on to [ellmer::chat()].
#' @details
#' Functions:
#' 
#' - `initiate_llm()` creates a new `ellmer` Chat object, appends user context to the system prompt when the user's name and job title are found in the secrets file, registers the session tools (`get_df_summary`, `list_objects`, `get_colnames`), and probes the model to verify actual tool support. Models that do not support tools are silently recreated without them, so the function always leaves a working instance. The active object is stored in `pkg_env$chat_object`.
#' - `list_presets()` prints all configured presets with their provider, model, and URL, marking the default.
#' - `chat()` sends a single message to the active LLM, initiating a new session first if none exists or if `new = TRUE`.
#' - `new_chat()` is a shorthand that always initiates a fresh session before sending the message.
#' - `chat_in_browser()` and `chat_in_console()` open an interactive session via `ellmer::live_browser()` and `ellmer::live_console()` respectively.
#' - `get_chat_object()`, `get_provider()`, `get_system_prompt()`, `get_tools()`, and `get_tokens()` are thin accessors that expose the underlying `ellmer` Chat object's state for inspection.
#' @importFrom cli cli_alert_info
#' @rdname chat
#' @name chat
#' @export
initiate_llm <- function(preset = read_secret("llm.default.preset"),
                         provider = NULL,
                         model = NULL,
                         url = NULL,
                         system_prompt = read_secret("llm.system_prompt"),
                         ...) {
  # Resolve connection settings: preset provides the base, explicit arguments
  # override individual fields when supplied.
  preset_config     <- read_preset(preset)
  resolved_provider <- if (!is.null(provider)) provider else preset_config$provider
  resolved_model    <- if (!is.null(model))    model    else preset_config$model
  resolved_url      <- if (!is.null(url))      url      else preset_config$url  # may be NULL
  
  # Add user info
  user_name <- read_secret(paste0("user.", Sys.info()["user"], ".fullname"))
  user_jobtitle <- read_secret(paste0("user.", Sys.info()["user"], ".jobtitle")) 
  if (user_name != "" && user_jobtitle != "") {
    user_context <- paste0(
      "\n## Over de gebruiker\n",
      "Je gebruiker heet ", user_name,
      ", die bij Certe werkt als ", tolower(user_jobtitle),
      ". Je bent diens persoonlijke, behulpzame assistent."
    )
    system_prompt <- paste0(system_prompt, user_context)
  }
  
  pkg_env$chat_object <- .create_chat_model(resolved_provider, resolved_model,
                                            resolved_url, system_prompt, ...)
  
  # Register tools. register_tool() itself never errors on unsupported models;
  # Ollama returns HTTP 400 only at inference time. So after registration we send
  # a minimal probe to confirm actual tool support. On failure the model is
  # recreated without tools, so initiate_llm() always leaves a working instance.
  tools_registered <- tryCatch({
    # if (requireNamespace("plot2", quietly = TRUE)) {
    #   pkg_env$chat_object$register_tool(tool_plot2)
    # }
    pkg_env$chat_object$register_tool(tool_get_df_summary)
    pkg_env$chat_object$register_tool(tool_list_objects)
    pkg_env$chat_object$register_tool(tool_get_colnames)
    TRUE
  }, error = function(e) {
    warning("Tool registration failed: ", conditionMessage(e), call. = FALSE)
    FALSE
  })
  
  if (tools_registered) {
    probe <- tryCatch({
      suppressMessages(pkg_env$chat_object$chat("."))
      pkg_env$chat_object$set_turns(list())
      TRUE
    }, error = function(e) conditionMessage(e))
    
    if (!isTRUE(probe)) {
      if (grepl("does not support tools", probe, fixed = TRUE)) {
        pkg_env$chat_object <- .create_chat_model(resolved_provider, resolved_model,
                                                  resolved_url, system_prompt, ...)
        pkg_env$tools_supported <- FALSE
      } else {
        stop(probe, call. = FALSE)
      }
    } else {
      pkg_env$tools_supported <- TRUE
    }
  } else {
    pkg_env$tools_supported <- FALSE
  }
  
  # Build startup message; base_url is not present on all provider objects
  prov_obj <- pkg_env$chat_object$get_provider()
  url_str  <- tryCatch(paste0(" via {.url ", prov_obj@base_url, "}"), error = function(e) "")
  cli_alert_info(paste0("LLM opgestart: {.field ", prov_obj@name, "/", prov_obj@model, "}", url_str,
                        if (isTRUE(pkg_env$tools_supported)) " (tools: enabled)" else " (tools: disabled)"))
}

#' @rdname chat
#' @importFrom cli cli_text
#' @export
list_presets <- function() {
  presets <- read_secret("llm.presets")
  if (!is.list(presets) || length(presets) == 0) {
    message("No presets found under 'llm.presets' in the secrets file.")
    return(invisible(character(0)))
  }
  default <- read_secret("llm.default.preset")
  nms     <- names(presets)
  width   <- max(nchar(nms))
  
  info <- vapply(nms, function(nm) {
    p        <- presets[[nm]]
    padding  <- strrep("\u00a0", width - nchar(nm))
    flag     <- if (nzchar(default) && nm == default) " [default]" else ""
    url_str  <- if (!is.null(p$url) && nzchar(p$url)) paste0(" @ {.url ", p$url, "}") else ""
    provider <- p$provider %||% ""
    model    <- p$model    %||% ""
    pm_str   <- if (nzchar(provider) || nzchar(model)) paste0(provider, "/", model) else "(no model)"
    paste0(cli::symbol$bullet, " {.val ", nm, "}", padding, " -> {.field ", pm_str, "}", url_str, flag)
  }, character(1))
  
  cli_text(paste(info, collapse = "\n\n"))
  invisible(nms)
}

#' @rdname chat
#' @export
new_chat <- function(input, ...) {
  initiate_llm(...)
  pkg_env$chat_object$chat(input)
}

#' @rdname chat
#' @export
chat <- function(input, new = FALSE, ...) {
  if (is.null(pkg_env$chat_object) || isTRUE(new)) {
    initiate_llm(...)
  }
  pkg_env$chat_object$chat(input)
}

#' @rdname chat
#' @importFrom cli cat_boxx
#' @importFrom ellmer live_browser
#' @export
chat_in_browser <- function(new = FALSE, ...) {
  if (is.null(pkg_env$chat_object) || isTRUE(new)) {
    initiate_llm(...)
  }
  cat_boxx(c("Chat starten in browser.", "Gebruik Ctrl+C om af te sluiten."), 
           padding = c(0, 1, 0, 1), border_style = "double")
  live_browser(pkg_env$chat_object, quiet = TRUE)
}

#' @rdname chat
#' @importFrom cli cat_boxx
#' @importFrom ellmer live_console
#' @export
chat_in_console <- function(new = FALSE, ...) {
  if (is.null(pkg_env$chat_object) || isTRUE(new)) {
    initiate_llm(...)
  }
  cat_boxx(c("Chat starten in console.", "Gebruik \"\"\" voor multi-line input.", 
             "Type 'Q' to af te sluiten."), padding = c(0, 1, 0, 1), border_style = "double")
  live_console(pkg_env$chat_object, quiet = TRUE)
}

#' @rdname chat
#' @export
get_chat_object <- function() {
  if (is.null(pkg_env$chat_object)) {
    message("No LLM initiated")
  }
  pkg_env$chat_object
}

#' @rdname chat
#' @export
get_provider <- function() {
  if (is.null(pkg_env$chat_object)) {
    message("No LLM initiated")
  }
  pkg_env$chat_object$get_provider()
}

#' @rdname chat
#' @export
get_system_prompt <- function() {
  if (is.null(pkg_env$chat_object)) {
    message("No LLM initiated")
  }
  pkg_env$chat_object$get_system_prompt()
}

#' @rdname chat
#' @export
get_tools <- function() {
  if (is.null(pkg_env$chat_object)) {
    message("No LLM initiated")
    return(invisible(NULL))
  }
  if (isTRUE(pkg_env$tools_supported)) {
    pkg_env$chat_object$get_tools()
  } else {
    message("Tools are disabled for this model.")
    invisible(NULL)
  }
}

#' @rdname chat
#' @export
get_tokens <- function() {
  if (is.null(pkg_env$chat_object)) {
    message("No LLM initiated")
  }
  pkg_env$chat_object$get_tokens()
}
