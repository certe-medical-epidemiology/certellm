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

#' @importFrom ellmer chat
.create_chat_model <- function(provider, model, url, system_prompt, ...) {
  args <- c(
    list(name = paste0(provider, "/", model), system_prompt = system_prompt),
    if (!is.null(url)) list(base_url = url) else list(),
    list(...)
  )
  do.call(ellmer::chat, args)
}

.fresh_clone <- function(...) {
  # clone the active LLM model with a fresh turn history so that task-specific functions do not pollute the main conversation.
  if (is.null(pkg_env$chat_object)) {
    initiate_llm(...)
  }
  cl <- pkg_env$chat_object$clone()
  cl$set_turns(list())
  cl
}

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

.format_diver_schema_hint <- function() {
  paste0(
    "## Diver-database\n\n",
    "Diver is een intern kolomgericht datawarehouse van Certe. ",
    "Data wordt opgehaald met `get_diver_data(where = <R-expressie>)` uit het ",
    "`certedb`-pakket. Het `where`-argument is een R-expressie (geen SQL) die ",
    "rijen filtert, direct ge\u00EBvalueerd tegen Diver-kolomnamen.\n\n",
    "### Veelvoorkomende Diver-kolomnamen\n",
    "- `jaar` (integer): jaar van het record\n",
    "- `maand` (integer): maand (1\u201312)\n",
    "- `kwartaal` (integer): kwartaal (1\u20134)\n",
    "- `datum` (Date): exacte datum\n",
    "- `regio` (character): regio, bijv. `\"Groningen\"`, `\"Friesland\"`, `\"Drenthe\"`\n",
    "- `ziekenhuis` / `instelling` (character): ziekenhuis- of instellingsnaam\n",
    "- `afdeling` (character): afdeling\n",
    "- `organisme` (character): micro-organismenaam (Latijn), bijv. `\"Escherichia coli\"`\n",
    "- `agens` (character): pathogeen of verwekker\n",
    "- `materiaal` (character): monstermateriaal, bijv. `\"urine\"`, `\"bloed\"`\n",
    "- `geslacht` (character): geslacht (`\"M\"` of `\"V\"`)\n",
    "- `leeftijd` (numeric): leeftijd in jaren\n",
    "- `leeftijdscategorie` (character): leeftijdsgroep, bijv. `\"0-4\"`, `\"65+\"`\n",
    "- `antibioticum` (character): antibioticumnaam\n",
    "- `resistentie` (character): resistentieresultaat, bijv. `\"S\"`, `\"I\"`, `\"R\"`\n",
    "- `uitslag` (character): testuitslag of uitkomst\n",
    "- `n` (integer): telling (voorgeaggregeerde tabellen)\n\n",
    "### Voorbeeld get_diver_data()-aanroepen\n",
    "```r\n",
    "# Alle E. coli urinekweken uit 2024\n",
    "get_diver_data(where = jaar == 2024 & organisme == \"Escherichia coli\" & materiaal == \"urine\")\n\n",
    "# Resistentiedata voor Groningen, laatste 3 jaar\n",
    "get_diver_data(where = jaar >= (as.integer(format(Sys.Date(), \"%Y\")) - 3) & regio == \"Groningen\")\n",
    "```\n\n",
    "### Opmerkingen\n",
    "- Kolomnamen zijn hoofdlettergevoelig en in kleine letters.\n",
    "- Stringwaarden zijn hoofdlettergevoelig; organismenamen gebruiken Latijnse notatie.\n",
    "- Datumfiltering gebruikt `jaar`, `maand` en `datum`; vermijd `Sys.Date()` in `where` waar mogelijk.\n",
    "- Bij onzekerheid over de exacte kolomnaam, voeg bij voorkeur een commentaarregel toe met de aanname.\n"
  )
}
