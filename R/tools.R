#' # ===================================================================== #
#' #  An R package by Certe:                                               #
#' #  https://github.com/certe-medical-epidemiology                        #
#' #                                                                       #
#' #  Licensed as GPL-v2.0.                                                #
#' #                                                                       #
#' #  Developed at non-profit organisation Certe Medical Diagnostics &     #
#' #  Advice, department of Medical Epidemiology.                          #
#' #                                                                       #
#' #  This R package is free software; you can freely use and distribute   #
#' #  it for both personal and commercial purposes under the terms of the  #
#' #  GNU General Public License version 2.0 (GNU GPL-2), as published by  #
#' #  the Free Software Foundation.                                        #
#' #                                                                       #
#' #  We created this package for both routine data analysis and academic  #
#' #  research and it was publicly released in the hope that it will be    #
#' #  useful, but it comes WITHOUT ANY WARRANTY OR LIABILITY.              #
#' # ===================================================================== #
#' 
#' .parse_plot2_arg <- function(x) {
#'   if (grepl("(", x, fixed = TRUE)) {
#'     rlang::parse_expr(x)
#'   } else {
#'     rlang::sym(x)
#'   }
#' }
#' 
#' #' @importFrom ellmer tool type_string type_enum type_number type_integer type_boolean type_array type_ignore
#' #' @importFrom rlang sym syms
#' tool_plot2 <- tool(
#'   plot2::plot2,
#'   name = "plot2",
#'   description = paste0(
#'     "Create a plot using plot2(), a convenience wrapper around ggplot2. ",
#'     "Provide column names as strings for the plotting directions x, y, ",
#'     "category, and facet. The data is supplied separately and must not be ",
#'     "specified here. The function automatically determines sensible defaults ",
#'     "for plot type, axis labels, datalabels, sorting, and theming. Only set ",
#'     "arguments where you want to override the automatic behaviour. ",
#'     "Supported plot types include: 'col' (column/bar), 'point', 'line', ",
#'     "'boxplot', 'violin', 'histogram', 'jitter', 'area', 'ribbon', ",
#'     "'beeswarm', 'sankey', 'spider', 'dumbbell', 'linedot', 'back-to-back', ",
#'     "'upset', 'barpercent', and 'blank'. Abbreviations are accepted (e.g. ",
#'     "'c' for col, 'p' for point, 'l' for line, 'b' for boxplot, 'h' for ",
#'     "histogram, 'j' for jitter, 'v' for violin, 'a' for area). ",
#'     "Leave type as NULL to let plot2 determine the best type automatically. ",
#'     "For y, you can specify a function call as a string such as 'n()', ",
#'     "'median(column_name)', 'mean(column_name)', 'n_distinct(column_name)', ",
#'     "or 'max(column_name)'. Multiple columns for y can be given as a vector ",
#'     "of column name strings."
#'   ),
#'   arguments = list(
#'     `...` = type_ignore(),
#' 
#'     # -- data is injected, never supplied by the LLM -------------------------
#'     .data = type_ignore(),
#' 
#'     # -- core plotting directions --------------------------------------------
#'     x = type_string(
#'       paste0(
#'         "Column name (as string) to map to the X-axis, e.g. 'species'. ",
#'         "Leave NULL to let plot2 determine automatically."
#'       ),
#'       required = FALSE
#'     ),
#'     y = type_string(
#'       paste0(
#'         "Column name or summary expression (as string) for the Y-axis. ",
#'         "Examples: 'column_name', 'n()', 'median(age)', 'mean(value)', ",
#'         "'n_distinct(patient_id)'. Leave NULL for automatic determination."
#'       ),
#'       required = FALSE
#'     ),
#'     category = type_string(
#'       paste0(
#'         "Column name (as string) for the colour/fill grouping (called ",
#'         "'category' in plot2, equivalent to 'fill'/'colour' in ggplot2). ",
#'         "Leave NULL for automatic determination or no grouping."
#'       ),
#'       required = FALSE
#'     ),
#'     facet = type_string(
#'       paste0(
#'         "Column name (as string) for facetting (small multiples). ",
#'         "Leave NULL for no facetting."
#'       ),
#'       required = FALSE
#'     ),
#' 
#'     # -- plot type -----------------------------------------------------------
#'     type = type_string(
#'       paste0(
#'         "Type of plot. Common values: 'col', 'point', 'line', 'boxplot', ",
#'         "'violin', 'histogram', 'jitter', 'area', 'sankey', 'spider', ",
#'         "'dumbbell', 'barpercent'. Abbreviations work: 'c', 'p', 'l', 'b', ",
#'         "'h', 'j', 'v', 'a'. Leave NULL for automatic type detection."
#'       ),
#'       required = FALSE
#'     ),
#' 
#'     # -- titles --------------------------------------------------------------
#'     x.title = type_string(
#'       "Title for the X-axis. Set to a custom string, or leave default.",
#'       required = FALSE
#'     ),
#'     y.title = type_string(
#'       "Title for the Y-axis. Set to a custom string, or leave default.",
#'       required = FALSE
#'     ),
#'     category.title = type_string(
#'       "Title for the legend/category. Leave NULL for default.",
#'       required = FALSE
#'     ),
#'     title = type_string(
#'       "Main title of the plot. Supports markdown syntax (e.g. *italic*, **bold**).",
#'       required = FALSE
#'     ),
#'     subtitle = type_string(
#'       "Subtitle below the main title. Supports markdown.",
#'       required = FALSE
#'     ),
#'     caption = type_string(
#'       "Caption shown at the bottom of the plot. Supports markdown.",
#'       required = FALSE
#'     ),
#' 
#'     # -- colour & appearance -------------------------------------------------
#'     colour = type_string(
#'       paste0(
#'         "Colour scheme. Can be a colour palette name such as 'viridis', ",
#'         "'magma', 'inferno', 'plasma', 'cividis', 'rocket', 'mako', ",
#'         "'turbo', or 'ggplot2' (default). Can also be a single colour name."
#'       ),
#'       required = FALSE
#'     ),
#'     colour_fill = type_string(
#'       "Colour(s) for fill. Determined automatically if left blank.",
#'       required = FALSE
#'     ),
#'     colour_opacity = type_number(
#'       "Opacity for colour/fill: 0 = solid (default), 1 = fully transparent.",
#'       required = FALSE
#'     ),
#' 
#'     # -- NA handling ---------------------------------------------------------
#'     na.rm = type_boolean(
#'       "Remove NA values from the plot. Default is FALSE.",
#'       required = FALSE
#'     ),
#'     na.replace = type_string(
#'       "Character string to replace NA values with. Default is empty string.",
#'       required = FALSE
#'     ),
#' 
#'     # -- X-axis settings -----------------------------------------------------
#'     x.sort = type_string(
#'       paste0(
#'         "Sorting of the X-axis. Options: 'asc' or 'alpha' (ascending), ",
#'         "'desc' (descending), 'freq' or 'freq-desc' (most frequent first), ",
#'         "'freq-asc' (least frequent first), 'order' or 'inorder' (data order). ",
#'         "NULL means default sorting."
#'       ),
#'       required = FALSE
#'     ),
#'     x.max_items = type_integer(
#'       paste0(
#'         "Maximum number of items on the X-axis. All remaining values are ",
#'         "grouped and summarised. Use together with x.sort to show e.g. the ",
#'         "top N most frequent values."
#'       ),
#'       required = FALSE
#'     ),
#'     x.lbl_angle = type_number(
#'       paste0(
#'         "Counter-clockwise rotation angle for X-axis labels in degrees. ",
#'         "Common values: 0 (horizontal, default), 45, 90 (vertical)."
#'       ),
#'       required = FALSE
#'     ),
#'     x.lbl_italic = type_boolean(
#'       "Whether X-axis labels should be in italics. Default is FALSE.",
#'       required = FALSE
#'     ),
#'     x.lbl_taxonomy = type_boolean(
#'       paste0(
#'         "Whether to italicise microbial taxonomy names on the X-axis using ",
#'         "the AMR package. Default is FALSE."
#'       ),
#'       required = FALSE
#'     ),
#'     x.remove = type_boolean(
#'       "Remove X-axis labels and title. Default is FALSE.",
#'       required = FALSE
#'     ),
#'     x.mic = type_boolean(
#'       paste0(
#'         "Whether the X-axis should be formatted as MIC (minimum inhibitory ",
#'         "concentration) values, dropping all factor levels and adding missing ",
#'         "factors of 2. Default is FALSE."
#'       ),
#'       required = FALSE
#'     ),
#'     x.date_breaks = type_string(
#'       paste0(
#'         "Breaks for date-type X-axis, e.g. '1 day', '2 weeks', '1 month', ",
#'         "'1 year'. Determined automatically if NULL."
#'       ),
#'       required = FALSE
#'     ),
#'     x.date_labels = type_string(
#'       paste0(
#'         "Date format for X-axis labels in Excel-style notation, e.g. ",
#'         "'d mmmm yyyy', 'mmm yy'. Determined automatically if NULL."
#'       ),
#'       required = FALSE
#'     ),
#'     x.transform = type_string(
#'       paste0(
#'         "Transformation for the X-axis. Options include: 'identity' ",
#'         "(default), 'log', 'log2', 'log10', 'sqrt', 'reverse'."
#'       ),
#'       required = FALSE
#'     ),
#'     x.zoom = type_boolean(
#'       paste0(
#'         "Zoom X-axis to the data range instead of starting at 0. ",
#'         "Default is FALSE."
#'       ),
#'       required = FALSE
#'     ),
#'     x.drop = type_boolean(
#'       "Drop unused factor levels on the X-axis. Default is FALSE.",
#'       required = FALSE
#'     ),
#' 
#'     # -- Y-axis settings -----------------------------------------------------
#'     y.remove = type_boolean(
#'       "Remove Y-axis labels and title. Default is FALSE.",
#'       required = FALSE
#'     ),
#'     y.percent = type_boolean(
#'       "Format Y-axis labels as percentages. Default is FALSE.",
#'       required = FALSE
#'     ),
#'     y.transform = type_string(
#'       paste0(
#'         "Transformation for the Y-axis. Options include: 'identity' ",
#'         "(default), 'log', 'log2', 'log10', 'sqrt', 'reverse'."
#'       ),
#'       required = FALSE
#'     ),
#'     y.zoom = type_boolean(
#'       "Zoom Y-axis to the data range instead of starting at 0. Default is FALSE.",
#'       required = FALSE
#'     ),
#'     y.age = type_boolean(
#'       "Format Y-axis labels and breaks as ages in years. Default is FALSE.",
#'       required = FALSE
#'     ),
#'     y.24h = type_boolean(
#'       "Format Y-axis labels and breaks as 24-hour time. Default is FALSE.",
#'       required = FALSE
#'     ),
#'     y.position = type_enum(
#'       "Position of the Y-axis.",
#'       values = c("left", "right"),
#'       required = FALSE
#'     ),
#' 
#'     # -- category settings ---------------------------------------------------
#'     category.focus = type_string(
#'       paste0(
#'         "A value of the category to highlight, greying out all other values. ",
#'         "Can be a category label string."
#'       ),
#'       required = FALSE
#'     ),
#'     category.sort = type_boolean(
#'       "Sort the category values. Default is TRUE.",
#'       required = FALSE
#'     ),
#'     category.max_items = type_integer(
#'       "Maximum number of category items. Remaining values are grouped.",
#'       required = FALSE
#'     ),
#'     category.type = type_string(
#'       paste0(
#'         "Aesthetic type for the category. Options: 'colour' (default), ",
#'         "'shape', 'size', 'linetype', 'linewidth', 'alpha'. ",
#'         "Multiple can be combined with a comma."
#'       ),
#'       required = FALSE
#'     ),
#' 
#'     # -- facet settings ------------------------------------------------------
#'     facet.nrow = type_integer(
#'       "Number of rows for facet layout. NULL for automatic.",
#'       required = FALSE
#'     ),
#'     facet.sort = type_boolean(
#'       "Sort facet values. Default is TRUE.",
#'       required = FALSE
#'     ),
#'     facet.fixed_y = type_boolean(
#'       paste0(
#'         "Whether all facet panels should share the same Y-axis limits. ",
#'         "Determined automatically by default."
#'       ),
#'       required = FALSE
#'     ),
#'     facet.relative = type_boolean(
#'       "Use relative (proportional) values per facet. Default is FALSE.",
#'       required = FALSE
#'     ),
#' 
#'     # -- datalabels ----------------------------------------------------------
#'     datalabels = type_boolean(
#'       paste0(
#'         "Show data labels on the plot. Default is TRUE for column-type plots. ",
#'         "Set to FALSE to hide them."
#'       ),
#'       required = FALSE
#'     ),
#'     datalabels.format = type_string(
#'       paste0(
#'         "Format for data labels. Use '%n' for count, '%p' for percentage. ",
#'         "Example: '%n (%p)'. Default is '%n'."
#'       ),
#'       required = FALSE
#'     ),
#'     datalabels.colour = type_string(
#'       "Colour for data label text. Default is 'grey25'.",
#'       required = FALSE
#'     ),
#'     datalabels.size = type_number(
#'       "Font size for data labels.",
#'       required = FALSE
#'     ),
#' 
#'     # -- layout and stacking -------------------------------------------------
#'     stacked = type_boolean(
#'       "Stack the bars/areas. Default is FALSE.",
#'       required = FALSE
#'     ),
#'     stacked_fill = type_boolean(
#'       paste0(
#'         "Stack bars to 100%% (proportional stacking). Default is FALSE. ",
#'         "Overrides 'stacked' if TRUE."
#'       ),
#'       required = FALSE
#'     ),
#'     horizontal = type_boolean(
#'       "Flip the plot 90 degrees (horizontal bars). Default is FALSE.",
#'       required = FALSE
#'     ),
#' 
#'     # -- smoothing -----------------------------------------------------------
#'     smooth = type_boolean(
#'       paste0(
#'         "Add a smooth/trend line. Default is NULL (FALSE for most types, ",
#'         "TRUE for histograms to show density)."
#'       ),
#'       required = FALSE
#'     ),
#'     smooth.method = type_string(
#'       paste0(
#'         "Smoothing method: 'loess' (default for small n), 'lm', 'glm', ",
#'         "'gam'. NULL for automatic."
#'       ),
#'       required = FALSE
#'     ),
#'     smooth.se = type_boolean(
#'       "Show confidence interval around smooth. Default is TRUE.",
#'       required = FALSE
#'     ),
#' 
#'     # -- geom appearance -----------------------------------------------------
#'     size = type_number(
#'       "Size of the geom (point size, etc.). NULL for automatic.",
#'       required = FALSE
#'     ),
#'     linetype = type_integer(
#'       "Linetype: 1 = solid (default), 2 = dashed, 3 = dotted, 4 = dotdash.",
#'       required = FALSE
#'     ),
#'     linewidth = type_number(
#'       "Line width. NULL for automatic defaults.",
#'       required = FALSE
#'     ),
#'     binwidth = type_number(
#'       "Bin width for histograms. NULL for automatic.",
#'       required = FALSE
#'     ),
#'     width = type_number(
#'       "Width of the geom (bars, boxes, etc.). NULL for automatic.",
#'       required = FALSE
#'     ),
#' 
#'     # -- legend --------------------------------------------------------------
#'     legend.position = type_enum(
#'       "Position of the legend.",
#'       values = c("top", "right", "bottom", "left", "none"),
#'       required = FALSE
#'     ),
#'     legend.reverse = type_boolean(
#'       "Reverse the legend order. Default is FALSE.",
#'       required = FALSE
#'     ),
#'     legend.nrow = type_integer(
#'       "Number of rows in the legend. NULL for automatic.",
#'       required = FALSE
#'     ),
#' 
#'     # -- sankey-specific -----------------------------------------------------
#'     sankey.alpha = type_number(
#'       "Alpha (transparency) of flows in a Sankey plot. Default is 0.5.",
#'       required = FALSE
#'     ),
#' 
#'     # -- overall zoom --------------------------------------------------------
#'     zoom = type_boolean(
#'       paste0(
#'         "Zoom both axes to the data range, i.e. neither axis starts at 0. ",
#'         "Default is FALSE."
#'       ),
#'       required = FALSE
#'     ),
#' 
#'     # -- separator -----------------------------------------------------------
#'     sep = type_string(
#'       paste0(
#'         "Separator character when multiple columns are combined into one ",
#'         "direction. Default is ' / '."
#'       ),
#'       required = FALSE
#'     ),
#' 
#'     # -- theming -------------------------------------------------------------
#'     font = type_string(
#'       paste0(
#'         "Font family. Can be any installed system font or Google Font name. ",
#'         "NULL for default."
#'       ),
#'       required = FALSE
#'     ),
#'     text_factor = type_number(
#'       "Scaling factor for all text in the plot. Default is 1.",
#'       required = FALSE
#'     ),
#'     theme = type_string(
#'       paste0(
#'         "ggplot2 theme name as string, e.g. 'theme_minimal2' (default), ",
#'         "'theme_bw', 'theme_classic', 'theme_minimal'. NULL for ggplot2 default."
#'       ),
#'       required = FALSE
#'     ),
#'     background = type_string(
#'       "Background colour of the entire plot. Default is 'white'.",
#'       required = FALSE
#'     ),
#'     markdown = type_boolean(
#'       paste0(
#'         "Enable markdown in titles and labels (e.g. *italic*, **bold**). ",
#'         "Default is TRUE."
#'       ),
#'       required = FALSE
#'     ),
#' 
#'     # -- arguments the LLM should never touch --------------------------------
#'     tag = type_ignore(),
#'     title.linelength = type_ignore(),
#'     title.colour = type_ignore(),
#'     subtitle.linelength = type_ignore(),
#'     subtitle.colour = type_ignore(),
#'     facet.position = type_ignore(),
#'     facet.fill = type_ignore(),
#'     facet.bold = type_ignore(),
#'     facet.italic = type_ignore(),
#'     facet.size = type_ignore(),
#'     facet.margin = type_ignore(),
#'     facet.repeat_lbls_x = type_ignore(),
#'     facet.repeat_lbls_y = type_ignore(),
#'     facet.fixed_x = type_ignore(),
#'     facet.drop = type_ignore(),
#'     facet.max_items = type_ignore(),
#'     facet.max_txt = type_ignore(),
#'     x.date_remove_years = type_ignore(),
#'     x.lbl_align = type_ignore(),
#'     x.position = type_ignore(),
#'     x.max_txt = type_ignore(),
#'     category.max_txt = type_ignore(),
#'     x.breaks = type_ignore(),
#'     x.n_breaks = type_ignore(),
#'     x.expand = type_ignore(),
#'     x.limits = type_ignore(),
#'     x.labels = type_ignore(),
#'     x.character = type_ignore(),
#'     y.scientific = type_ignore(),
#'     y.percent_break = type_ignore(),
#'     y.breaks = type_ignore(),
#'     y.n_breaks = type_ignore(),
#'     y.limits = type_ignore(),
#'     y.labels = type_ignore(),
#'     y.expand = type_ignore(),
#'     y_secondary = type_ignore(),
#'     y_secondary.type = type_ignore(),
#'     y_secondary.title = type_ignore(),
#'     y_secondary.colour = type_ignore(),
#'     y_secondary.colour_fill = type_ignore(),
#'     y_secondary.scientific = type_ignore(),
#'     y_secondary.percent = type_ignore(),
#'     y_secondary.labels = type_ignore(),
#'     category.labels = type_ignore(),
#'     category.percent = type_ignore(),
#'     category.breaks = type_ignore(),
#'     category.limits = type_ignore(),
#'     category.expand = type_ignore(),
#'     category.midpoint = type_ignore(),
#'     category.transform = type_ignore(),
#'     category.date_breaks = type_ignore(),
#'     category.date_labels = type_ignore(),
#'     category.character = type_ignore(),
#'     x.complete = type_ignore(),
#'     category.complete = type_ignore(),
#'     facet.complete = type_ignore(),
#'     datalabels.round = type_ignore(),
#'     datalabels.colour_fill = type_ignore(),
#'     datalabels.angle = type_ignore(),
#'     datalabels.lineheight = type_ignore(),
#'     decimal.mark = type_ignore(),
#'     big.mark = type_ignore(),
#'     summarise_function = type_ignore(),
#'     reverse = type_ignore(),
#'     smooth.formula = type_ignore(),
#'     smooth.level = type_ignore(),
#'     smooth.alpha = type_ignore(),
#'     smooth.linewidth = type_ignore(),
#'     smooth.linetype = type_ignore(),
#'     smooth.colour = type_ignore(),
#'     jitter_seed = type_ignore(),
#'     violin_scale = type_ignore(),
#'     legend.title = type_ignore(),
#'     legend.barheight = type_ignore(),
#'     legend.barwidth = type_ignore(),
#'     legend.nbin = type_ignore(),
#'     legend.italic = type_ignore(),
#'     sankey.node_width = type_ignore(),
#'     sankey.node_whitespace = type_ignore(),
#'     sankey.remove_axes = type_ignore(),
#'     print = type_ignore(),
#'     data = type_ignore()
#'   )
#' )
#' 
