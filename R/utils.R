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

pkg_env <- new.env(hash = FALSE)
# - pkg_env$chat_object   - the active ellmer Chat R6 object
# - pkg_env$chat_history - named list of saved turn snapshots (save_chat / restore_chat)

# Internal: resolve a named preset from llm.presets in the secrets YAML.
# Returns a list with required elements $provider and $model, plus optional
# $url (full base URL, used as base_url for providers that support it, e.g.
# Ollama with a non-default endpoint). Stops with an informative error when
# the preset name is not found or required fields are missing.
read_preset <- function(name) {
  if (!nzchar(name)) {
    stop("No preset specified and 'llm.default.preset' is not set in the secrets file.",
         call. = FALSE)
  }
  presets <- read_secret("llm.presets")
  if (!is.list(presets) || length(presets) == 0) {
    stop("No presets found under 'llm.presets' in the secrets file.", call. = FALSE)
  }
  if (!name %in% names(presets)) {
    stop("Preset '", name, "' not found. ",
         "Available presets: ", paste(names(presets), collapse = ", "), ".",
         call. = FALSE)
  }
  p <- presets[[name]]
  missing_fields <- setdiff(c("provider", "model"), names(p))
  if (length(missing_fields) > 0) {
    stop("Preset '", name, "' is missing required field(s): ",
         paste(missing_fields, collapse = ", "), ".", call. = FALSE)
  }
  p
}

# Internal: create an ellmer Chat object using ellmer::chat() with
# "provider/model" routing. Passes base_url only when url is non-NULL so that
# providers without a configurable endpoint (e.g. Anthropic) are not affected.
#' @importFrom ellmer chat
.create_chat_model <- function(provider, model, url, system_prompt, ...) {
  args <- c(
    list(name = paste0(provider, "/", model), system_prompt = system_prompt),
    if (!is.null(url)) list(base_url = url) else list(),
    list(...)
  )
  do.call(ellmer::chat, args)
}

# Internal: clone the active LLM model with a fresh turn history so that
# task-specific functions do not pollute the main conversation.
.fresh_clone <- function(...) {
  if (is.null(pkg_env$chat_object)) {
    initiate_llm(...)
  }
  cl <- pkg_env$chat_object$clone()
  cl$set_turns(list())
  cl
}

# Internal: produce a compact markdown summary of a data frame suitable for
# injection into an LLM prompt. Targets ~2000 tokens. Never returns raw rows
# that could expose patient-level data in a usable form.
.summarise_df_for_llm <- function(df, max_rows_preview = 5, max_cols = 30) {
  nr <- nrow(df)
  nc <- ncol(df)
  
  # Warn if very wide; cap columns shown
  cols_shown <- names(df)
  truncated <- FALSE
  if (nc > max_cols) {
    cols_shown <- c(names(df)[seq_len(20)], names(df)[(nc - 9):nc])
    truncated <- TRUE
    df <- df[, cols_shown, drop = FALSE]
  }
  
  # Build column info table
  col_info <- vapply(cols_shown, function(col) {
    x <- df[[col]]
    cls <- paste(class(x), collapse = "/")
    
    # Value examples / summary depending on type
    if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) {
      rng <- tryCatch(
        paste0(format(min(x, na.rm = TRUE)), " \u2013 ", format(max(x, na.rm = TRUE))),
        error = function(e) "?"
      )
      summary_str <- paste0("range: ", rng)
    } else if (is.numeric(x)) {
      summary_str <- tryCatch(
        paste0(
          "min=", round(min(x, na.rm = TRUE), 3),
          " mean=", round(mean(x, na.rm = TRUE), 3),
          " max=", round(max(x, na.rm = TRUE), 3)
        ),
        error = function(e) "?"
      )
    } else if (is.logical(x)) {
      n_true <- sum(x, na.rm = TRUE)
      summary_str <- paste0("TRUE: ", n_true, " / FALSE: ", sum(!x, na.rm = TRUE))
    } else {
      # character, factor, or other
      uvals <- unique(x[!is.na(x)])
      n_unique <- length(uvals)
      if (n_unique <= 8) {
        examples <- paste(paste0('"', uvals, '"'), collapse = ", ")
      } else {
        # top 5 by frequency
        tbl <- sort(table(x), decreasing = TRUE)
        top5 <- names(tbl)[seq_len(min(5, length(tbl)))]
        examples <- paste(paste0('"', top5, '"'), collapse = ", ")
        examples <- paste0(examples, " (+ ", n_unique - 5, " more)")
      }
      summary_str <- examples
    }
    
    n_na <- sum(is.na(x))
    na_str <- if (n_na == 0) "" else paste0(" [NA: ", n_na, "]")
    paste0("- **", col, "** (", cls, ")", na_str, ": ", summary_str)
  }, character(1))
  
  # First few rows as a plain text table (no patient IDs shown individually —
  # the structured summary above already captures the relevant distribution)
  preview_rows <- min(max_rows_preview, nr)
  preview <- tryCatch({
    utils::capture.output(print(as.data.frame(df[seq_len(preview_rows), , drop = FALSE])))
  }, error = function(e) character(0))
  preview_block <- if (length(preview) > 0) {
    paste0("\n**First ", preview_rows, " rows:**\n```\n",
           paste(preview, collapse = "\n"),
           "\n```")
  } else {
    ""
  }
  
  truncation_note <- if (truncated) {
    paste0("\n> *Note: data frame has ", nc, " columns total; showing first 20 and last 10.*\n")
  } else {
    ""
  }
  
  paste0(
    "**Data frame summary**\n\n",
    "Dimensions: ", nr, " rows \u00D7 ", length(cols_shown), " columns",
    if (truncated) paste0(" (of ", nc, " total)") else "",
    "\n\n**Columns:**\n",
    paste(col_info, collapse = "\n"),
    truncation_note,
    preview_block
  )
}

# Internal: static Diver schema hint injected into diver-related LLM prompts.
# Contains known table names, common column naming conventions, and syntax notes.
# This is curated knowledge — updating it requires a package release.
.format_diver_schema_hint <- function() {
  paste0(
    "## Diver database\n\n",
    "Diver is an internal columnar data warehouse at Certe. ",
    "Data is retrieved with `get_diver_data(where = <R expression>)` from the ",
    "`certedb` package. The `where` argument is an R expression (not SQL) that ",
    "filters rows, evaluated against Diver column names directly.\n\n",
    "### Common Diver column names\n",
    "- `jaar` (integer): year of the record\n",
    "- `maand` (integer): month (1\u201312)\n",
    "- `kwartaal` (integer): quarter (1\u20134)\n",
    "- `datum` (Date): exact date\n",
    "- `regio` (character): region, e.g. `\"Groningen\"`, `\"Friesland\"`, `\"Drenthe\"`\n",
    "- `ziekenhuis` / `instelling` (character): hospital or institution name\n",
    "- `afdeling` (character): department or ward\n",
    "- `organisme` (character): micro-organism name (Latin), e.g. `\"Escherichia coli\"`\n",
    "- `agens` (character): pathogen or agent\n",
    "- `materiaal` (character): sample material, e.g. `\"urine\"`, `\"bloed\"`\n",
    "- `geslacht` (character): sex (`\"M\"` or `\"V\"`)\n",
    "- `leeftijd` (numeric): patient age in years\n",
    "- `leeftijdscategorie` (character): age group, e.g. `\"0-4\"`, `\"65+\"`\n",
    "- `antibioticum` (character): antibiotic name\n",
    "- `resistentie` (character): resistance result, e.g. `\"S\"`, `\"I\"`, `\"R\"`\n",
    "- `uitslag` (character): test result or outcome\n",
    "- `n` (integer): count (pre-aggregated tables)\n\n",
    "### Example get_diver_data() calls\n",
    "```r\n",
    "# All E. coli urine cultures from 2024\n",
    "get_diver_data(where = jaar == 2024 & organisme == \"Escherichia coli\" & materiaal == \"urine\")\n\n",
    "# Resistance data for Groningen, last 3 years\n",
    "get_diver_data(where = jaar >= (as.integer(format(Sys.Date(), \"%Y\")) - 3) & regio == \"Groningen\")\n",
    "```\n\n",
    "### Notes\n",
    "- Column names are case-sensitive and lowercase.\n",
    "- String values are case-sensitive; organism names use Latin notation.\n",
    "- Date filtering uses `jaar`, `maand`, and `datum`; avoid `Sys.Date()` in `where` when possible.\n",
    "- When the exact column name is uncertain, prefer generating a comment noting the assumption.\n"
  )
}
