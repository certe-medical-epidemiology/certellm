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

#' Data Analysis and Code Assistance
#'
#' @description
#' These functions use the active LLM to assist with data analysis and R code
#' tasks. Each call uses an isolated chat context. Generated code is returned
#' as a character string for the user to review and run manually.
#'
#' - `llm_analyse()` suggests analyses or answers a question about a dataset.
#' - `llm_code()` generates R code for a described task.
#' - `llm_explain()` explains R code in Dutch.
#' - `llm_suggest_plot()` suggests a `plot2()` call for a dataset.
#'
#' @param data A data frame. Summarised before sending to the LLM; raw rows are
#'   never sent verbatim.
#' @param question A specific question about the data. If `NULL`, the LLM is
#'   asked to suggest appropriate analyses.
#' @param task Description of what the R code should accomplish.
#' @param style Coding style for generated R code: `"tidyverse"` (default) or
#'   `"base"`.
#' @param code A character string containing R code to explain or improve.
#' @param detail Level of detail for explanations: `"brief"` (one paragraph,
#'   default) or `"detailed"` (step-by-step).
#' @param goal Optional description of what the plot should show.
#' @param new Logical. If `TRUE`, re-initiates the LLM before the task call.
#'   Default is `FALSE`.
#' @param ... Additional arguments passed to [initiate_llm()].
#'
#' @return A character string with the LLM response (suggestions, code, or
#'   explanation).
#' @rdname analyse
#' @name analyse
#' @export
llm_analyse <- function(data,
                        question = NULL,
                        new = FALSE,
                        ...) {
  if (isTRUE(new)) initiate_llm(...)
  cl <- .fresh_clone(...)

  data_summary <- .summarise_df_for_llm(data)

  question_block <- if (!is.null(question)) {
    paste0("**Vraag:** ", question, "\n\n")
  } else {
    paste0(
      "**Opdracht:** Stel op basis van bovenstaande datastructuur 3-5 concrete analyses voor ",
      "die relevant zijn voor de afdeling Medische Epidemiologie van Certe. ",
      "Beschrijf per analyse kort wat deze laat zien en geef de bijbehorende R-code.\n\n"
    )
  }

  prompt <- paste0(
    question_block,
    "**Data:**\n", data_summary, "\n\n",
    "Gebruik R met |> pipes en tidyverse/dplyr-stijl. ",
    "Gebruik plot2::plot2() voor visualisaties. ",
    "Zet code in ```r codeblokken."
  )

  cl$chat(prompt)
}

#' @rdname analyse
#' @export
llm_code <- function(data = NULL,
                     task,
                     style = c("tidyverse", "base"),
                     new = FALSE,
                     ...) {
  style <- match.arg(style)
  if (isTRUE(new)) initiate_llm(...)
  cl <- .fresh_clone(...)

  style_instruction <- switch(
    style,
    tidyverse = paste0(
      "Schrijf R-code met |> pipes en dplyr/tidyverse-stijl. ",
      "Gebruik plot2::plot2() voor grafieken, certedb::get_diver_data() voor data-opvraging. "
    ),
    base = "Schrijf R-code met base R zonder tidyverse-afhankelijkheden. "
  )

  data_block <- if (!is.null(data) && is.data.frame(data)) {
    paste0("**Beschikbare data:**\n", .summarise_df_for_llm(data), "\n\n")
  } else {
    ""
  }

  prompt <- paste0(
    "Genereer R-code voor de volgende opdracht:\n\n> ", task, "\n\n",
    data_block,
    style_instruction,
    "Geef alleen de R-code in een ```r codeblok, met korte inline commentaren. ",
    "Voeg geen lopende tekst toe buiten het codeblok."
  )

  cl$chat(prompt)
}

#' @rdname analyse
#' @export
llm_explain <- function(code,
                        detail = c("brief", "detailed"),
                        new = FALSE,
                        ...) {
  detail <- match.arg(detail)
  if (isTRUE(new)) initiate_llm(...)
  cl <- .fresh_clone(...)

  detail_instruction <- switch(
    detail,
    brief    = "Geef een beknopte uitleg in \u00E9\u00E9n alinea.",
    detailed = paste0(
      "Geef een uitgebreide, stap-voor-stap uitleg. ",
      "Bespreek elke relevante regel of blok code afzonderlijk."
    )
  )

  prompt <- paste0(
    "Leg de volgende R-code uit in het Nederlands. ", detail_instruction, "\n\n",
    "```r\n", code, "\n```"
  )

  cl$chat(prompt)
}

#' @rdname analyse
#' @export
llm_suggest_plot <- function(data,
                             goal = NULL,
                             new = FALSE,
                             ...) {
  if (isTRUE(new)) initiate_llm(...)
  cl <- .fresh_clone(...)

  data_summary <- .summarise_df_for_llm(data)

  goal_block <- if (!is.null(goal)) {
    paste0("**Doel van de grafiek:** ", goal, "\n\n")
  } else {
    ""
  }

  prompt <- paste0(
    "Stel op basis van onderstaande dataset de meest geschikte `plot2()`-aanroep voor ",
    "(uit het plot2-pakket). Kies het plottype, x, y, category en ",
    "facet-argumenten die de data het beste visualiseren voor een epidemiologisch publiek. ",
    "Ga ervan uit dat het dataframe `data` heet. ",
    "Geef alleen de R-code. ",
    "Voer de plot niet uit, stel alleen de code voor ter beoordeling door de gebruiker.\n\n",
    goal_block,
    "**Data:**\n", data_summary
  )

  cl$chat(prompt)
}
