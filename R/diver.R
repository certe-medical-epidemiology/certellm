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

#' Diver Database Query Assistance
#'
#' @description
#' These functions help construct and explain `get_diver_data()` calls (from
#' the `certedb` package) using the active LLM. Each call uses an isolated
#' chat context so the main conversation history is not affected.
#'
#' - `llm_diver_query()` generates a `get_diver_data()` call from a natural
#'   language description.
#' - `llm_diver_explain()` explains an existing `get_diver_data()` call in
#'   plain Dutch.
#'
#' The LLM is primed with a static schema hint describing known Diver column
#' names and conventions. When the exact column name is uncertain, the generated
#' code includes a comment noting the assumption.
#'
#' @param description Natural language description of the data to retrieve,
#'   e.g. `"All E. coli urine cultures from 2024 in Groningen"`.
#' @param table Optional hint for the Diver table to query.
#' @param columns_hint Optional character vector of known column names to
#'   assist the LLM in forming the query.
#' @param query An existing `get_diver_data()` call as a character string,
#'   to be explained.
#' @param new Logical. If `TRUE`, re-initiates the LLM before the task call.
#'   Default is `FALSE`.
#' @param ... Additional arguments passed to [initiate_llm()].
#'
#' @return A character string: the generated R code (`llm_diver_query()`) or
#'   the Dutch explanation (`llm_diver_explain()`).
#' @rdname diver
#' @name diver
#' @export
llm_diver_query <- function(description,
                            table = NULL,
                            columns_hint = NULL,
                            new = FALSE,
                            ...) {
  if (isTRUE(new)) initiate_llm(...)
  cl <- .fresh_clone(...)

  schema_hint <- .format_diver_schema_hint()

  table_block <- if (!is.null(table)) {
    paste0("The user expects to query the table: **", table, "**.\n\n")
  } else {
    ""
  }

  columns_block <- if (!is.null(columns_hint) && length(columns_hint) > 0) {
    paste0("Known column names available: ", paste(columns_hint, collapse = ", "), ".\n\n")
  } else {
    ""
  }

  prompt <- paste0(
    "Generate a valid `get_diver_data()` R function call that retrieves the ",
    "following data:\n\n> ", description, "\n\n",
    table_block,
    columns_block,
    "Use the schema information below to form the `where =` argument. ",
    "Return only the R code, wrapped in a ```r code block. ",
    "If a column name is uncertain, add a comment in the code noting the assumption. ",
    "Do not include any prose outside the code block.\n\n",
    schema_hint
  )

  cl$chat(prompt)
}

#' @rdname diver
#' @export
llm_diver_explain <- function(query,
                              new = FALSE,
                              ...) {
  if (isTRUE(new)) initiate_llm(...)
  cl <- .fresh_clone(...)

  schema_hint <- .format_diver_schema_hint()

  prompt <- paste0(
    "Leg in het Nederlands uit wat de volgende `get_diver_data()`-aanroep ophaalt ",
    "uit de Diver-database. Schrijf in lopende tekst, \u00E9\u00E9n of twee alinea's. ",
    "Vermeld welke filters worden toegepast en welk soort data het resultaat is.\n\n",
    "```r\n", query, "\n```\n\n",
    schema_hint
  )

  cl$chat(prompt)
}
