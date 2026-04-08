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

#' @importFrom ellmer tool type_string type_enum type_number type_integer type_boolean type_array type_ignore
tool_plot2 <- tool(
  plot2::plot2,
  name = "plot2",
  description = paste0(
    "Maak een grafiek met plot2(), een convenience-wrapper rond ggplot2. ",
    "Geef kolomnamen als strings op voor de plotrichtingen x, y, ",
    "category en facet. De data wordt apart aangeleverd en mag hier niet ",
    "worden opgegeven. De functie bepaalt automatisch verstandige standaarden ",
    "voor plottype, aslabels, datalabels, sortering en thema. Stel alleen ",
    "argumenten in waar je het automatische gedrag wilt overschrijven. ",
    "Ondersteunde plottypes zijn: 'col' (kolom/staaf), 'point', 'line', ",
    "'boxplot', 'violin', 'histogram', 'jitter', 'area', 'ribbon', ",
    "'beeswarm', 'sankey', 'spider', 'dumbbell', 'linedot', 'back-to-back', ",
    "'upset', 'barpercent' en 'blank'. Afkortingen zijn toegestaan (bijv. ",
    "'c' voor col, 'p' voor point, 'l' voor line, 'b' voor boxplot, 'h' voor ",
    "histogram, 'j' voor jitter, 'v' voor violin, 'a' voor area). ",
    "Laat type als NULL om plot2 automatisch het beste type te laten bepalen. ",
    "Voor y kun je een functieaanroep als string opgeven, zoals 'n()', ",
    "'median(kolomnaam)', 'mean(kolomnaam)', 'n_distinct(kolomnaam)' ",
    "of 'max(kolomnaam)'. Meerdere kolommen voor y kunnen als vector ",
    "van kolomnaamstrings worden opgegeven."
  ),
  arguments = list(
    `...` = type_ignore(),

    # data is injected externally, never supplied by the LLM
    .data = type_ignore(),

    # core plotting directions
    x = type_string(
      paste0(
        "Kolomnaam (als string) voor de X-as, bijv. 'species'. ",
        "Laat NULL om plot2 automatisch te laten bepalen."
      ),
      required = FALSE
    ),
    y = type_string(
      paste0(
        "Kolomnaam of samenvattingsexpressie (als string) voor de Y-as. ",
        "Voorbeelden: 'kolomnaam', 'n()', 'median(leeftijd)', 'mean(waarde)', ",
        "'n_distinct(patient_id)'. Laat NULL voor automatische bepaling."
      ),
      required = FALSE
    ),
    category = type_string(
      paste0(
        "Kolomnaam (als string) voor de kleur/vulling-groepering (in plot2 ",
        "'category' genoemd, equivalent aan 'fill'/'colour' in ggplot2). ",
        "Laat NULL voor automatische bepaling of geen groepering."
      ),
      required = FALSE
    ),
    facet = type_string(
      paste0(
        "Kolomnaam (als string) voor facetten (kleine veelvouden). ",
        "Laat NULL voor geen facetten."
      ),
      required = FALSE
    ),

    # plot type
    type = type_string(
      paste0(
        "Type grafiek. Veelgebruikte waarden: 'col', 'point', 'line', 'boxplot', ",
        "'violin', 'histogram', 'jitter', 'area', 'sankey', 'spider', ",
        "'dumbbell', 'barpercent'. Afkortingen werken: 'c', 'p', 'l', 'b', ",
        "'h', 'j', 'v', 'a'. Laat NULL voor automatische typedetectie."
      ),
      required = FALSE
    ),

    # titles
    x.title = type_string(
      "Titel voor de X-as. Stel in als aangepaste string, of laat de standaard.",
      required = FALSE
    ),
    y.title = type_string(
      "Titel voor de Y-as. Stel in als aangepaste string, of laat de standaard.",
      required = FALSE
    ),
    category.title = type_string(
      "Titel voor de legenda/categorie. Laat NULL voor standaard.",
      required = FALSE
    ),
    title = type_string(
      "Hoofdtitel van de grafiek. Ondersteunt markdown-syntax (bijv. *cursief*, **vet**).",
      required = FALSE
    ),
    subtitle = type_string(
      "Ondertitel onder de hoofdtitel. Ondersteunt markdown.",
      required = FALSE
    ),
    caption = type_string(
      "Bijschrift onderaan de grafiek. Ondersteunt markdown.",
      required = FALSE
    ),

    # colour and appearance
    colour = type_string(
      paste0(
        "Kleurenschema. Kan een kleurpaletnaam zijn zoals 'viridis', ",
        "'magma', 'inferno', 'plasma', 'cividis', 'rocket', 'mako', ",
        "'turbo' of 'ggplot2' (standaard). Kan ook een enkele kleurnaam zijn."
      ),
      required = FALSE
    ),
    colour_fill = type_string(
      "Kleur(en) voor vulling. Wordt automatisch bepaald indien leeg.",
      required = FALSE
    ),
    colour_opacity = type_number(
      "Transparantie voor kleur/vulling: 0 = dekkend (standaard), 1 = volledig transparant.",
      required = FALSE
    ),

    # NA handling
    na.rm = type_boolean(
      "Verwijder NA-waarden uit de grafiek. Standaard is FALSE.",
      required = FALSE
    ),
    na.replace = type_string(
      "Tekenreeks om NA-waarden mee te vervangen. Standaard is een lege string.",
      required = FALSE
    ),

    # X-axis settings
    x.sort = type_string(
      paste0(
        "Sortering van de X-as. Opties: 'asc' of 'alpha' (oplopend), ",
        "'desc' (aflopend), 'freq' of 'freq-desc' (meest voorkomend eerst), ",
        "'freq-asc' (minst voorkomend eerst), 'order' of 'inorder' (datavolgorde). ",
        "NULL betekent standaardsortering."
      ),
      required = FALSE
    ),
    x.max_items = type_integer(
      paste0(
        "Maximum aantal items op de X-as. Alle overige waarden worden ",
        "gegroepeerd en samengevat. Gebruik samen met x.sort om bijv. de ",
        "top N meest voorkomende waarden te tonen."
      ),
      required = FALSE
    ),
    x.lbl_angle = type_number(
      paste0(
        "Rotatiehoek tegen de klok in voor X-aslabels in graden. ",
        "Veelgebruikte waarden: 0 (horizontaal, standaard), 45, 90 (verticaal)."
      ),
      required = FALSE
    ),
    x.lbl_italic = type_boolean(
      "Of X-aslabels cursief moeten zijn. Standaard is FALSE.",
      required = FALSE
    ),
    x.lbl_taxonomy = type_boolean(
      paste0(
        "Of microbi\u00EBle taxonomienamen op de X-as cursief moeten worden weergegeven ",
        "met het AMR-pakket. Standaard is FALSE."
      ),
      required = FALSE
    ),
    x.remove = type_boolean(
      "Verwijder X-aslabels en -titel. Standaard is FALSE.",
      required = FALSE
    ),
    x.mic = type_boolean(
      paste0(
        "Of de X-as moet worden opgemaakt als MIC-waarden (minimale inhiberende ",
        "concentratie), waarbij alle factorniveaus worden verwijderd en ontbrekende ",
        "factoren van 2 worden toegevoegd. Standaard is FALSE."
      ),
      required = FALSE
    ),
    x.date_breaks = type_string(
      paste0(
        "Intervallen voor een datum-X-as, bijv. '1 day', '2 weeks', '1 month', ",
        "'1 year'. Wordt automatisch bepaald bij NULL."
      ),
      required = FALSE
    ),
    x.date_labels = type_string(
      paste0(
        "Datumnotatie voor X-aslabels in Excel-stijl, bijv. ",
        "'d mmmm yyyy', 'mmm yy'. Wordt automatisch bepaald bij NULL."
      ),
      required = FALSE
    ),
    x.transform = type_string(
      paste0(
        "Transformatie voor de X-as. Opties zijn: 'identity' ",
        "(standaard), 'log', 'log2', 'log10', 'sqrt', 'reverse'."
      ),
      required = FALSE
    ),
    x.zoom = type_boolean(
      paste0(
        "Zoom de X-as in op het databereik in plaats van bij 0 te beginnen. ",
        "Standaard is FALSE."
      ),
      required = FALSE
    ),
    x.drop = type_boolean(
      "Verwijder ongebruikte factorniveaus op de X-as. Standaard is FALSE.",
      required = FALSE
    ),

    # Y-axis settings
    y.remove = type_boolean(
      "Verwijder Y-aslabels en -titel. Standaard is FALSE.",
      required = FALSE
    ),
    y.percent = type_boolean(
      "Toon Y-aslabels als percentages. Standaard is FALSE.",
      required = FALSE
    ),
    y.transform = type_string(
      paste0(
        "Transformatie voor de Y-as. Opties zijn: 'identity' ",
        "(standaard), 'log', 'log2', 'log10', 'sqrt', 'reverse'."
      ),
      required = FALSE
    ),
    y.zoom = type_boolean(
      "Zoom de Y-as in op het databereik in plaats van bij 0 te beginnen. Standaard is FALSE.",
      required = FALSE
    ),
    y.age = type_boolean(
      "Toon Y-aslabels en -intervallen als leeftijden in jaren. Standaard is FALSE.",
      required = FALSE
    ),
    y.24h = type_boolean(
      "Toon Y-aslabels en -intervallen als 24-uursnotatie. Standaard is FALSE.",
      required = FALSE
    ),
    y.position = type_enum(
      "Positie van de Y-as.",
      values = c("left", "right"),
      required = FALSE
    ),

    # category settings
    category.focus = type_string(
      paste0(
        "Een waarde van de categorie om uit te lichten, waarbij alle andere waarden grijs worden. ",
        "Kan een categorielabel als string zijn."
      ),
      required = FALSE
    ),
    category.sort = type_boolean(
      "Sorteer de categoriewaarden. Standaard is TRUE.",
      required = FALSE
    ),
    category.max_items = type_integer(
      "Maximum aantal categorie-items. Overige waarden worden gegroepeerd.",
      required = FALSE
    ),
    category.type = type_string(
      paste0(
        "Esthetisch type voor de categorie. Opties: 'colour' (standaard), ",
        "'shape', 'size', 'linetype', 'linewidth', 'alpha'. ",
        "Meerdere kunnen gecombineerd worden met een komma."
      ),
      required = FALSE
    ),

    # facet settings
    facet.nrow = type_integer(
      "Aantal rijen voor de facetindeling. NULL voor automatisch.",
      required = FALSE
    ),
    facet.sort = type_boolean(
      "Sorteer facetwaarden. Standaard is TRUE.",
      required = FALSE
    ),
    facet.fixed_y = type_boolean(
      paste0(
        "Of alle facetpanelen dezelfde Y-aslimieten moeten delen. ",
        "Wordt standaard automatisch bepaald."
      ),
      required = FALSE
    ),
    facet.relative = type_boolean(
      "Gebruik relatieve (proportionele) waarden per facet. Standaard is FALSE.",
      required = FALSE
    ),

    # datalabels
    datalabels = type_boolean(
      paste0(
        "Toon datalabels op de grafiek. Standaard is TRUE voor kolomgrafieken. ",
        "Stel in op FALSE om ze te verbergen."
      ),
      required = FALSE
    ),
    datalabels.format = type_string(
      paste0(
        "Opmaak voor datalabels. Gebruik '%n' voor telling, '%p' voor percentage. ",
        "Voorbeeld: '%n (%p)'. Standaard is '%n'."
      ),
      required = FALSE
    ),
    datalabels.colour = type_string(
      "Kleur voor datalabeltekst. Standaard is 'grey25'.",
      required = FALSE
    ),
    datalabels.size = type_number(
      "Lettergrootte voor datalabels.",
      required = FALSE
    ),

    # layout and stacking
    stacked = type_boolean(
      "Stapel de staven/gebieden. Standaard is FALSE.",
      required = FALSE
    ),
    stacked_fill = type_boolean(
      paste0(
        "Stapel staven tot 100%% (proportionele stapeling). Standaard is FALSE. ",
        "Overschrijft 'stacked' indien TRUE."
      ),
      required = FALSE
    ),
    horizontal = type_boolean(
      "Draai de grafiek 90 graden (horizontale staven). Standaard is FALSE.",
      required = FALSE
    ),

    # smoothing
    smooth = type_boolean(
      paste0(
        "Voeg een smooth/trendlijn toe. Standaard is NULL (FALSE voor de meeste types, ",
        "TRUE voor histogrammen om dichtheid te tonen)."
      ),
      required = FALSE
    ),
    smooth.method = type_string(
      paste0(
        "Smoothingmethode: 'loess' (standaard bij kleine n), 'lm', 'glm', ",
        "'gam'. NULL voor automatisch."
      ),
      required = FALSE
    ),
    smooth.se = type_boolean(
      "Toon betrouwbaarheidsinterval rond de smooth. Standaard is TRUE.",
      required = FALSE
    ),

    # geom appearance
    size = type_number(
      "Grootte van het geom (puntgrootte, etc.). NULL voor automatisch.",
      required = FALSE
    ),
    linetype = type_integer(
      "Lijntype: 1 = doorgetrokken (standaard), 2 = gestreept, 3 = gestippeld, 4 = stippelstreep.",
      required = FALSE
    ),
    linewidth = type_number(
      "Lijndikte. NULL voor automatische standaarden.",
      required = FALSE
    ),
    binwidth = type_number(
      "Binbreedte voor histogrammen. NULL voor automatisch.",
      required = FALSE
    ),
    width = type_number(
      "Breedte van het geom (staven, boxen, etc.). NULL voor automatisch.",
      required = FALSE
    ),

    # legend
    legend.position = type_enum(
      "Positie van de legenda.",
      values = c("top", "right", "bottom", "left", "none"),
      required = FALSE
    ),
    legend.reverse = type_boolean(
      "Keer de legendavolgorde om. Standaard is FALSE.",
      required = FALSE
    ),
    legend.nrow = type_integer(
      "Aantal rijen in de legenda. NULL voor automatisch.",
      required = FALSE
    ),

    # sankey-specific
    sankey.alpha = type_number(
      "Alpha (transparantie) van stromen in een Sankey-grafiek. Standaard is 0.5.",
      required = FALSE
    ),

    # overall zoom
    zoom = type_boolean(
      paste0(
        "Zoom beide assen in op het databereik, d.w.z. geen van beide assen begint bij 0. ",
        "Standaard is FALSE."
      ),
      required = FALSE
    ),

    # separator
    sep = type_string(
      paste0(
        "Scheidingsteken wanneer meerdere kolommen in \u00E9\u00E9n richting worden gecombineerd. ",
        "Standaard is ' / '."
      ),
      required = FALSE
    ),

    # theming
    font = type_string(
      paste0(
        "Lettertypefamilie. Kan elk ge\u00EFnstalleerd systeemlettertype of Google Font-naam zijn. ",
        "NULL voor standaard."
      ),
      required = FALSE
    ),
    text_factor = type_number(
      "Schaalfactor voor alle tekst in de grafiek. Standaard is 1.",
      required = FALSE
    ),
    theme = type_string(
      paste0(
        "ggplot2-themanaam als string, bijv. 'theme_minimal2' (standaard), ",
        "'theme_bw', 'theme_classic', 'theme_minimal'. NULL voor ggplot2-standaard."
      ),
      required = FALSE
    ),
    background = type_string(
      "Achtergrondkleur van de hele grafiek. Standaard is 'white'.",
      required = FALSE
    ),
    markdown = type_boolean(
      paste0(
        "Schakel markdown in voor titels en labels (bijv. *cursief*, **vet**). ",
        "Standaard is TRUE."
      ),
      required = FALSE
    ),

    # arguments the LLM should never touch
    tag = type_ignore(),
    title.linelength = type_ignore(),
    title.colour = type_ignore(),
    subtitle.linelength = type_ignore(),
    subtitle.colour = type_ignore(),
    facet.position = type_ignore(),
    facet.fill = type_ignore(),
    facet.bold = type_ignore(),
    facet.italic = type_ignore(),
    facet.size = type_ignore(),
    facet.margin = type_ignore(),
    facet.repeat_lbls_x = type_ignore(),
    facet.repeat_lbls_y = type_ignore(),
    facet.fixed_x = type_ignore(),
    facet.drop = type_ignore(),
    facet.max_items = type_ignore(),
    facet.max_txt = type_ignore(),
    x.date_remove_years = type_ignore(),
    x.lbl_align = type_ignore(),
    x.position = type_ignore(),
    x.max_txt = type_ignore(),
    category.max_txt = type_ignore(),
    x.breaks = type_ignore(),
    x.n_breaks = type_ignore(),
    x.expand = type_ignore(),
    x.limits = type_ignore(),
    x.labels = type_ignore(),
    x.character = type_ignore(),
    y.scientific = type_ignore(),
    y.percent_break = type_ignore(),
    y.breaks = type_ignore(),
    y.n_breaks = type_ignore(),
    y.limits = type_ignore(),
    y.labels = type_ignore(),
    y.expand = type_ignore(),
    y_secondary = type_ignore(),
    y_secondary.type = type_ignore(),
    y_secondary.title = type_ignore(),
    y_secondary.colour = type_ignore(),
    y_secondary.colour_fill = type_ignore(),
    y_secondary.scientific = type_ignore(),
    y_secondary.percent = type_ignore(),
    y_secondary.labels = type_ignore(),
    category.labels = type_ignore(),
    category.percent = type_ignore(),
    category.breaks = type_ignore(),
    category.limits = type_ignore(),
    category.expand = type_ignore(),
    category.midpoint = type_ignore(),
    category.transform = type_ignore(),
    category.date_breaks = type_ignore(),
    category.date_labels = type_ignore(),
    category.character = type_ignore(),
    x.complete = type_ignore(),
    category.complete = type_ignore(),
    facet.complete = type_ignore(),
    datalabels.round = type_ignore(),
    datalabels.colour_fill = type_ignore(),
    datalabels.angle = type_ignore(),
    datalabels.lineheight = type_ignore(),
    decimal.mark = type_ignore(),
    big.mark = type_ignore(),
    summarise_function = type_ignore(),
    reverse = type_ignore(),
    smooth.formula = type_ignore(),
    smooth.level = type_ignore(),
    smooth.alpha = type_ignore(),
    smooth.linewidth = type_ignore(),
    smooth.linetype = type_ignore(),
    smooth.colour = type_ignore(),
    jitter_seed = type_ignore(),
    violin_scale = type_ignore(),
    legend.title = type_ignore(),
    legend.barheight = type_ignore(),
    legend.barwidth = type_ignore(),
    legend.nbin = type_ignore(),
    legend.italic = type_ignore(),
    sankey.node_width = type_ignore(),
    sankey.node_whitespace = type_ignore(),
    sankey.remove_axes = type_ignore(),
    print = type_ignore(),
    data = type_ignore()
  )
)
