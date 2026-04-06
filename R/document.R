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

#' Rmd / Quarto Document Assistance
#'
#' @description
#' These functions use the active LLM to assist with writing and reviewing
#' R Markdown and Quarto documents. Each call uses an isolated chat context.
#'
#' - `llm_review_document()` reviews a document for language, structure, or
#'   completeness and returns Dutch-language feedback.
#' - `llm_write_section()` writes a Dutch section (e.g. Methods, Results) for
#'   a report, presentation, or article.
#' - `llm_write_chunk()` generates or improves an R code chunk for a document.
#'
#' @param file Path to an `.Rmd` or `.qmd` file. The first 3000 lines are read.
#'   Either `file` or `text` must be supplied.
#' @param text The document content as a character string. Either `file` or
#'   `text` must be supplied.
#' @param focus What to focus on during review: `"language"`, `"structure"`,
#'   `"completeness"`, or `"all"` (default).
#' @param section Name of the section to write, e.g. `"Methods"`, `"Results"`,
#'   `"Discussion"`.
#' @param data Optional data frame providing context for the section. Summarised
#'   before sending to the LLM.
#' @param context Optional character string with additional background context
#'   for the section (e.g. a description of the analysis).
#' @param style Document style: `"report"` (default), `"presentation"`, or
#'   `"article"`.
#' @param code Optional existing R code to improve (as a character string).
#' @param goal Description of what the code chunk should accomplish. Required
#'   when `code` is `NULL`.
#' @param new Logical. If `TRUE`, re-initiates the LLM before the task call.
#'   Default is `FALSE`.
#' @param ... Additional arguments passed to the LLM chat method.
#'
#' @return A character string with the generated or reviewed text.
#' @rdname document
#' @name document
#' @export
llm_review_document <- function(file = NULL,
                                text = NULL,
                                focus = c("all", "language", "structure", "completeness"),
                                new = FALSE,
                                ...) {
  focus <- match.arg(focus)
  if (isTRUE(new)) initiate_llm(...)
  cl <- .fresh_clone(...)

  if (is.null(file) && is.null(text)) {
    stop("Provide either `file` or `text`.", call. = FALSE)
  }

  if (!is.null(file)) {
    if (!file.exists(file)) stop("File not found: ", file, call. = FALSE)
    lines <- readLines(file, warn = FALSE)
    if (length(lines) > 3000) lines <- lines[seq_len(3000)]
    text <- paste(lines, collapse = "\n")
  }

  focus_instruction <- switch(
    focus,
    language     = "Focus je uitsluitend op taalgebruik, grammatica en stijl.",
    structure    = "Focus je uitsluitend op de structuur en opbouw van het document.",
    completeness = "Focus je uitsluitend op de volledigheid: ontbreken er secties, resultaten of verwijzingen?",
    all          = paste0(
      "Bespreek achtereenvolgens: (1) taalgebruik en stijl, ",
      "(2) structuur en opbouw, (3) volledigheid."
    )
  )

  prompt <- paste0(
    "Lees het onderstaande Rmd/Quarto-document en geef een review in het Nederlands. ",
    focus_instruction, " ",
    "Geef concrete suggesties voor verbetering. ",
    "Gebruik een genummerde lijst voor je bevindingen.\n\n",
    "```markdown\n", text, "\n```"
  )

  cl$chat(prompt)
}

#' @rdname document
#' @export
llm_write_section <- function(section,
                              data = NULL,
                              context = NULL,
                              style = c("report", "presentation", "article"),
                              new = FALSE,
                              ...) {
  style <- match.arg(style)
  if (isTRUE(new)) initiate_llm(...)
  cl <- .fresh_clone(...)

  style_instruction <- switch(
    style,
    report       = "Schrijf in de stijl van een epidemiologisch rapport.",
    presentation = "Schrijf beknopte opsommingspunten, geschikt voor een presentatie.",
    article      = paste0(
      "Schrijf in de stijl van een wetenschappelijk artikel, ",
      "inclusief passief gebruik en verwijzingen naar de data."
    )
  )

  data_block <- if (!is.null(data) && is.data.frame(data)) {
    paste0("**Beschikbare data:**\n", .summarise_df_for_llm(data), "\n\n")
  } else {
    ""
  }

  context_block <- if (!is.null(context)) {
    paste0("**Aanvullende context:**\n", context, "\n\n")
  } else {
    ""
  }

  prompt <- paste0(
    "Schrijf de sectie '", section, "' in het Nederlands voor een ",
    "epidemiologisch document. ", style_instruction, " ",
    "Schrijf in lopende tekst, tenzij het gaat om een presentatie. ",
    "Gebruik Markdown-opmaak (koppen met ##, vet, cursief).\n\n",
    data_block,
    context_block
  )

  cl$chat(prompt)
}

#' @rdname document
#' @export
llm_write_chunk <- function(code = NULL,
                            goal = NULL,
                            new = FALSE,
                            ...) {
  if (is.null(code) && is.null(goal)) {
    stop("Provide at least `goal` (what the chunk should do) or `code` to improve.",
         call. = FALSE)
  }
  if (isTRUE(new)) initiate_llm(...)
  cl <- .fresh_clone(...)

  if (!is.null(code) && !is.null(goal)) {
    # Improve existing code towards a stated goal
    prompt <- paste0(
      "Improve the following R code chunk for use in an Rmd/Quarto document. ",
      "Goal: ", goal, "\n\n",
      "Use |> pipes and tidyverse/dplyr style. ",
      "Use certeplot2::plot2() for visualisations. ",
      "Return only the improved R code in a ```r code block, ",
      "with brief inline English comments. ",
      "Do not add prose outside the code block.\n\n",
      "```r\n", code, "\n```"
    )
  } else if (!is.null(goal)) {
    # Generate new code chunk from scratch
    prompt <- paste0(
      "Write an R code chunk for use in an Rmd/Quarto document. ",
      "Goal: ", goal, "\n\n",
      "Use |> pipes and tidyverse/dplyr style. ",
      "Use certeplot2::plot2() for visualisations and ",
      "certedb::get_diver_data() for data retrieval where applicable. ",
      "Return only the R code in a ```r code block, ",
      "with brief inline English comments. ",
      "Do not add prose outside the code block."
    )
  } else {
    # Review / clean up existing code, no specific goal
    prompt <- paste0(
      "Review and improve the following R code chunk for use in an Rmd/Quarto document. ",
      "Fix any issues, improve readability, and ensure it follows |> pipe and tidyverse style. ",
      "Return only the improved R code in a ```r code block. ",
      "Do not add prose outside the code block.\n\n",
      "```r\n", code, "\n```"
    )
  }

  cl$chat(prompt)
}
