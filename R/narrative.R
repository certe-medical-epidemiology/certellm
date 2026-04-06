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

#' Generate Dutch Epidemiological Narratives
#'
#' @description
#' These functions use the active LLM to produce Dutch-language text based on
#' data from the Medical Epidemiology department. Each call uses an isolated
#' chat context so the main conversation history is not affected.
#'
#' - `llm_describe()` writes a Dutch epidemiological description of a dataset.
#' - `llm_interpret()` interprets patterns and suggests epidemiological explanations.
#' - `llm_conclude()` writes a Dutch conclusion paragraph from results or data.
#'
#' @param data A data frame to describe or interpret. Summarised before sending
#'   to the LLM; raw rows are never sent verbatim.
#' @param topic Topic or subject of the analysis, e.g. `"E. coli resistance"`.
#' @param period Time period covered, e.g. `"2023-2024"`.
#' @param region Geographic region, e.g. `"Noord-Nederland"` or `"Groningen"`.
#' @param tone Writing tone: `"formal"` (default) or `"accessible"`.
#' @param length Approximate length of the output: `"short"` (1 paragraph),
#'   `"medium"` (2-3 paragraphs), or `"long"` (4+ paragraphs).
#' @param hypothesis Optional hypothesis to evaluate against the data.
#' @param comparison Optional second data frame for comparison. Also summarised
#'   before sending.
#' @param ... Additional arguments passed to [initiate_llm()].
#' @param new Logical. If `TRUE`, re-initiates the LLM before the task call.
#'   Default is `FALSE`.
#'
#' @return A character string with the generated Dutch text.
#' @rdname narrative
#' @name narrative
#' @export
llm_describe <- function(data,
                         topic = NULL,
                         period = NULL,
                         region = NULL,
                         tone = c("formal", "accessible"),
                         length = c("short", "medium", "long"),
                         new = FALSE,
                         ...) {
  tone <- match.arg(tone)
  length <- match.arg(length)

  if (isTRUE(new)) initiate_llm(...)
  cl <- .fresh_clone(...)

  data_summary <- .summarise_df_for_llm(data)

  tone_nl <- switch(tone, formal = "formeel", accessible = "toegankelijk")
  length_instruction <- switch(
    length,
    short  = "Schrijf \u00E9\u00E9n alinea.",
    medium = "Schrijf 2 tot 3 alinea's.",
    long   = "Schrijf 4 of meer alinea's."
  )

  context_parts <- character(0)
  if (!is.null(topic))  context_parts <- c(context_parts, paste0("Onderwerp: ", topic))
  if (!is.null(period)) context_parts <- c(context_parts, paste0("Periode: ", period))
  if (!is.null(region)) context_parts <- c(context_parts, paste0("Regio: ", region))
  context_block <- if (length(context_parts) > 0) {
    paste0(paste(context_parts, collapse = "\n"), "\n\n")
  } else {
    ""
  }

  prompt <- paste0(
    "Schrijf een epidemiologische beschrijving in het Nederlands, in een ",
    tone_nl, " schrijfstijl. ", length_instruction, " ",
    "Gebruik geen bullet points; schrijf lopende tekst. ",
    "Vermeld geen absolute aantallen als die niet in de data staan.\n\n",
    context_block,
    "**Data:**\n", data_summary
  )

  cl$chat(prompt)
}

#' @rdname narrative
#' @export
llm_interpret <- function(data,
                          hypothesis = NULL,
                          comparison = NULL,
                          new = FALSE,
                          ...) {
  if (isTRUE(new)) initiate_llm(...)
  cl <- .fresh_clone(...)

  data_summary <- .summarise_df_for_llm(data)

  hypothesis_block <- if (!is.null(hypothesis)) {
    paste0("\n**Hypothese om te evalueren:** ", hypothesis, "\n")
  } else {
    ""
  }

  comparison_block <- if (!is.null(comparison) && is.data.frame(comparison)) {
    paste0("\n**Vergelijkingsdata:**\n", .summarise_df_for_llm(comparison), "\n")
  } else {
    ""
  }

  prompt <- paste0(
    "Interpreteer de patronen in de onderstaande epidemiologische data. ",
    "Geef mogelijke verklaringen vanuit een epidemiologisch perspectief. ",
    "Als je onzeker bent over een interpretatie, maak dat dan expliciet. ",
    "Schrijf in het Nederlands, in lopende tekst.\n\n",
    "**Data:**\n", data_summary,
    hypothesis_block,
    comparison_block
  )

  cl$chat(prompt)
}

#' @rdname narrative
#' @export
llm_conclude <- function(...,
                         new = FALSE) {
  if (isTRUE(new)) initiate_llm(...)
  cl <- .fresh_clone(...)

  # Collect ... arguments: data frames are summarised, character strings are
  # used as-is
  inputs <- list(...)
  parts <- vapply(inputs, function(x) {
    if (is.data.frame(x)) {
      .summarise_df_for_llm(x)
    } else {
      as.character(x)
    }
  }, character(1))

  combined <- paste(parts, collapse = "\n\n")

  prompt <- paste0(
    "Schrijf een concluderende alinea in het Nederlands op basis van de ",
    "onderstaande resultaten. De conclusie moet geschikt zijn voor een ",
    "epidemiologisch rapport. Schrijf in lopende tekst zonder bullet points.\n\n",
    combined
  )

  cl$chat(prompt)
}
