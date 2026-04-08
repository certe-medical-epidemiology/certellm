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

# Tools that let the LLM inspect the user's R session during interactive
# chat (chat_in_browser / chat_in_console). All tools are read-only: they
# only access .GlobalEnv by name lookup and never call eval() on arbitrary
# code, never write files, and never return raw patient-level data rows.

#' @importFrom ellmer tool type_string
tool_get_df_summary <- tool(
  function(object_name) {
    if (!exists(object_name, envir = .GlobalEnv, inherits = FALSE)) {
      return(paste0("Object '", object_name, "' not found in the global environment."))
    }
    obj <- get(object_name, envir = .GlobalEnv)
    if (!is.data.frame(obj)) {
      return(paste0("'", object_name, "' is not a data frame (class: ",
                    paste(class(obj), collapse = "/"), ")."))
    }
    .summarise_df_for_llm(obj)
  },
  name = "get_df_summary",
  description = paste0(
    "Haal een gestructureerde samenvatting op van een dataframe in de R-sessie van de gebruiker. ",
    "Gebruik dit wanneer de gebruiker een dataset bij naam noemt (bijv. 'data', 'data_resistentie'). ",
    "Geeft kolomnamen, typen, waardeverdeling en een klein rijvoorbeeld terug. ",
    "Geeft nooit ruwe pati\u00EBntgegevens terug in bruikbare vorm."
  ),
  arguments = list(
    object_name = type_string(
      "De naam van het R-object (dataframe) om samen te vatten, zoals het in de globale omgeving staat."
    )
  )
)

#' @importFrom ellmer tool
tool_list_objects <- tool(
  function() {
    objs <- ls(envir = .GlobalEnv)
    if (length(objs) == 0) {
      return("The global environment contains no objects.")
    }
    classes <- vapply(objs, function(x) {
      tryCatch(paste(class(get(x, envir = .GlobalEnv)), collapse = "/"),
               error = function(e) "?")
    }, character(1))
    sizes <- vapply(objs, function(x) {
      tryCatch({
        obj <- get(x, envir = .GlobalEnv)
        if (is.data.frame(obj)) {
          paste0(nrow(obj), " x ", ncol(obj))
        } else {
          paste0(length(obj), " elements")
        }
      }, error = function(e) "?")
    }, character(1))
    lines <- paste0("- `", objs, "` (", classes, ", ", sizes, ")")
    paste0("**Global environment objects:**\n", paste(lines, collapse = "\n"))
  },
  name = "list_objects",
  description = paste0(
    "Geef een overzicht van alle objecten in de globale R-omgeving van de gebruiker, ",
    "met hun typen en dimensies. Gebruik dit om te ontdekken welke data beschikbaar is ",
    "voordat je de gebruiker vraagt een dataframenaam op te geven."
  ),
  arguments = list()
)

#' @importFrom ellmer tool type_string
tool_get_colnames <- tool(
  function(object_name) {
    if (!exists(object_name, envir = .GlobalEnv, inherits = FALSE)) {
      return(paste0("Object '", object_name, "' not found in the global environment."))
    }
    obj <- get(object_name, envir = .GlobalEnv)
    if (!is.data.frame(obj)) {
      return(paste0("'", object_name, "' is not a data frame."))
    }
    paste0("Columns of `", object_name, "` (", ncol(obj), " total):\n",
           paste(names(obj), collapse = ", "))
  },
  name = "get_colnames",
  description = paste0(
    "Haal de kolomnamen op van een dataframe in de globale R-omgeving van de gebruiker. ",
    "Gebruik dit voor een snel kolomoverzicht wanneer een volledige samenvatting niet nodig is, ",
    "bijv. wanneer de gebruiker vraagt welke kolommen beschikbaar zijn voor een grafiek."
  ),
  arguments = list(
    object_name = type_string(
      "De naam van het dataframe in de globale omgeving."
    )
  )
)
