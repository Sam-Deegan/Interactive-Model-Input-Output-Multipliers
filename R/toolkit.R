################################################################################
## Project: ECON42550 Macroeconomics                                          ##
## Dublin Toy-Model Toolkit: Shared Look and Machinery                        ##
################################################################################

## Author:      Sam Deegan
## Affiliation: University College Dublin
## Email:       sam.deegan@ucdconnect.ie

## Usage:
##   The layout-and-helper file shared by Sam Deegan's interactive model apps.
##   Each app carries a copy at R/toolkit.R, which Shiny sources on its own,
##   because shinylive needs every app folder to be self-contained. Nothing
##   here is run directly.
##
##   An app supplies four things and gets the rest from here:
##     T_03_01_controls_lst   one entry per slider: label, min, max, step, from
##     T_03_02_help_lst       one hover explanation per slider
##     T_03_03_equations_lst  the staged equations (see T_06)
##     T_03_04_notation_lst   the symbol key (see T_06)
##   These are named by the app in its own B_03, then passed to the builders.
##   Toolkit objects use the section letter T so they never collide with an
##   app's own A-G sections.
##
## Inputs:
##   None.
##
## Outputs:
##   None written to disk.
##
## Packages:
##   shiny, bslib, ggplot2; every call is namespaced, nothing is attached.

#-------------------------------- Script Begin --------------------------------#

################################################################################
## T: Toolkit ##################################################################
################################################################################
# Note: Palette, plot theme, CSS, control and tile builders, and the engine
#   behind the equations and notation tabs.

#### T_01: Palette #############################################################
# Note: The Dublin Beamer theme's colours, and nothing else.

###### T_01_01: Theme Colours ##################################################
# Note: Taken from dublin-theme.tex. Green is the single accent; everything
#   else is the navy-blue ramp or a grey. No red, no amber.

T_01_01_palette_vec <- c(
  navy   = "#04204C",
  blue   = "#0056A4",
  bluedk = "#003C77",
  light  = "#9FC4E0",
  green  = "#61B77C",
  ink    = "#212529",
  muted  = "#6C757D",
  rule   = "#D8E0E6",
  wash   = "#F2F6F9",
  ground = "#FFFFFF"
)

###### T_01_01: Zero-Line Colour ###############################################
# Note: Between rule and muted, so a zero line shows on a gridded panel
#   without competing with the resting points.

T_01_01_zero_chr <- "#AEB8C2"

###### T_01_02: Series Colours #################################################
# Note: Realised series in navy, comparison dashed in green, bands in light
#   blue, reference lines grey dashed. See CONVENTIONS.md 6.

T_01_02_series_vec <- c(
  main       = T_01_01_palette_vec[["navy"]],
  compare    = T_01_01_palette_vec[["green"]],
  band       = T_01_01_palette_vec[["light"]],
  reference  = T_01_01_palette_vec[["muted"]],
  third      = T_01_01_palette_vec[["blue"]],
  fourth     = T_01_01_palette_vec[["muted"]]
)

###### T_01_03: Plot Text Size #################################################
# Note: Large enough to read when projected in a lecture theatre.

T_01_03_base_size_int <- 14L

###### T_01_04: The Ghost ######################################################
# Note: Opacity of the ghost: the figure redrawn at the loaded example's
#   values, in the live colour, under the live layers. See CONVENTIONS.md 6.

T_01_04_ghost_alpha_num <- 0.5

#### T_02: Plot Look ###########################################################
# Note: The theme function and the small formatters every app needs.

###### T_02_01: Plot Theme #####################################################
# Note: The Dublin deck's figure look: white ground, no border, navy titles,
#   muted tick labels, legend along the bottom. grid is "h", "v" or "none".

T_02_01_theme_fn <- function(base_size = T_01_03_base_size_int,
                             grid = c("h", "v", "none")) {
  grid <- match.arg(grid)
  ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      panel.background   = ggplot2::element_rect(
        fill = "white", colour = NA),
      plot.background    = ggplot2::element_rect(
        fill = "white", colour = NA),
      legend.background  = ggplot2::element_rect(
        fill = "white", colour = NA),
      legend.key         = ggplot2::element_rect(
        fill = "white", colour = NA),
      panel.border       = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major.x = if (grid == "v") {
        ggplot2::element_line(colour = T_01_01_palette_vec[["rule"]],
                              linewidth = 0.3)
      } else {
        ggplot2::element_blank()
      },
      panel.grid.major.y = if (grid == "h") {
        ggplot2::element_line(colour = T_01_01_palette_vec[["rule"]],
                              linewidth = 0.3)
      } else {
        ggplot2::element_blank()
      },
      axis.line          = ggplot2::element_line(
        colour = T_01_01_palette_vec[["muted"]], linewidth = 0.4),
      # A secondary axis brings a spine with it; blank it, keep the ticks
      axis.line.x.top    = ggplot2::element_blank(),
      axis.line.y.right  = ggplot2::element_blank(),
      axis.ticks         = ggplot2::element_line(
        colour = T_01_01_palette_vec[["muted"]], linewidth = 0.4),
      strip.background   = ggplot2::element_rect(
        fill = "white", colour = NA),
      strip.text         = ggplot2::element_text(
        colour = T_01_01_palette_vec[["navy"]], face = "bold", hjust = 0),
      # Title flush with the plot edge, not the panel
      plot.title.position = "plot",
      plot.title         = ggplot2::element_text(
        colour = T_01_01_palette_vec[["navy"]], face = "bold", hjust = 0),
      plot.subtitle      = ggplot2::element_text(
        colour = T_01_01_palette_vec[["muted"]], size = ggplot2::rel(0.85)),
      # Captions are drawn under the card by T_02_01c_draw_fn, not here
      plot.caption       = ggplot2::element_blank(),
      # Axis titles at subtitle size, bold: "Inflation (pi[t])"
      axis.title         = ggplot2::element_text(
        colour = T_01_01_palette_vec[["navy"]], face = "bold",
        size = ggplot2::rel(0.85)),
      axis.text          = ggplot2::element_text(
        colour = T_01_01_palette_vec[["muted"]]),
      legend.position    = "bottom",
      legend.title       = ggplot2::element_blank(),
      legend.text        = ggplot2::element_text(
        colour = T_01_01_palette_vec[["ink"]])
    )
}

###### T_02_01a: Angle of a Line on Screen #####################################
# Note: Angle in degrees of a line of given slope as drawn on a panel with
#   these limits and aspect ratio; the plot sets theme(aspect.ratio) to match.

T_02_01a_angle_fn <- function(slope, xlim, ylim, aspect = 1) {
  atan(slope * (diff(range(xlim)) / diff(range(ylim))) * aspect) * 180 / pi
}

###### T_02_01b: Fold a Long Label #############################################
# Note: ggplot2 does not wrap a title or caption; this folds one onto as
#   many lines as it needs.

T_02_01b_fold_fn <- function(txt, width) {
  if (is.null(txt) || !is.character(txt) || !nzchar(txt)) return(txt)
  paste(strwrap(txt, width = width), collapse = "\n")
}

###### T_02_01c: Draw a Plot ###################################################
# Note: The last step for every figure. Folds the title and moves the caption
#   into the store in T_02_01d, keyed by output id, for T_07_07d_cap_fn.

T_02_01c_draw_fn <- function(p, cap_width = 95, title_width = 60) {
  if (!inherits(p, "ggplot")) return(p)
  p$labels$title <- T_02_01b_fold_fn(p$labels$title, title_width)

  cap <- p$labels$caption
  id  <- tryCatch(shiny::getCurrentOutputInfo()$name, error = function(e) NULL)
  if (!is.null(id)) {
    store <- T_02_01d_capstore_fn()
    if (!is.null(store)) {
      store[[id]] <- if (is.null(cap) || !nzchar(cap)) "" else cap
      p$labels$caption <- NULL
    }
  }
  p
}

###### T_02_01d: Where Lifted Captions Live ####################################
# Note: One reactiveValues per connected viewer, kept in userData, holding
#   the captions T_02_01c_draw_fn lifts out of the plots.

T_02_01d_capstore_fn <- function() {
  s <- shiny::getDefaultReactiveDomain()
  if (is.null(s)) return(NULL)
  if (is.null(s$userData$fig_caps)) {
    s$userData$fig_caps <- shiny::reactiveValues()
  }
  s$userData$fig_caps
}

###### T_02_02: Reference Lines and Their Labels ###############################
# Note: Zero lines dashed and faint, resting points (r*, pi*, a steady state)
#   dotted and darker, their symbols on the opposite axis. See CONVENTIONS.md 6.

T_02_02_zero_fn <- function(h = TRUE, v = TRUE) {
  out <- list()
  # Mid-grey rather than the gridline colour, so it shows on grid = "h"
  if (h) out <- c(out, list(ggplot2::geom_hline(
    yintercept = 0, linetype = "dashed", linewidth = 0.3,
    colour = T_01_01_zero_chr)))
  if (v) out <- c(out, list(ggplot2::geom_vline(
    xintercept = 0, linetype = "dashed", linewidth = 0.3,
    colour = T_01_01_zero_chr)))
  out
}

T_02_02_rest_fn <- function(h = NULL, v = NULL) {
  out <- list()
  if (!is.null(h)) out <- c(out, list(ggplot2::geom_hline(
    yintercept = h, linetype = "dotted", linewidth = 0.4,
    colour = T_01_01_palette_vec[["muted"]])))
  if (!is.null(v)) out <- c(out, list(ggplot2::geom_vline(
    xintercept = v, linetype = "dotted", linewidth = 0.4,
    colour = T_01_01_palette_vec[["muted"]])))
  out
}

T_02_02_mark_y_fn <- function(at, lab, labels = ggplot2::waiver(),
                              breaks = ggplot2::waiver(),
                              expand = ggplot2::waiver(), ...) {
  # A whole scale, so the primary axis's labels, breaks and expand pass through
  ggplot2::scale_y_continuous(
    labels = labels, breaks = breaks, expand = expand, ...,
    sec.axis = ggplot2::dup_axis(name = NULL, breaks = at, labels = lab))
}

T_02_02_mark_x_fn <- function(at, lab, labels = ggplot2::waiver(),
                              breaks = ggplot2::waiver(),
                              expand = ggplot2::waiver(), ...) {
  ggplot2::scale_x_continuous(
    labels = labels, breaks = breaks, expand = expand, ...,
    sec.axis = ggplot2::dup_axis(name = NULL, breaks = at, labels = lab))
}

###### T_02_02: Placeholder Panel ##############################################
# Note: What a figure shows when there is nothing to draw yet. Keeps the wash
#   ground so the card does not flash white.

T_02_02_placeholder_fn <- function(text) {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0, size = 5, label = text,
                      colour = T_01_01_palette_vec[["muted"]]) +
    ggplot2::theme_void() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(
        fill = "white", colour = NA),
      plot.background  = ggplot2::element_rect(
        fill = "white", colour = NA)
    )
}

###### T_02_03: Equilibrium Point ##############################################
# Note: The open marker the deck uses for a point on a line: a white disc with
#   a coloured ring.

T_02_03_point_fn <- function(x, y, colour = T_01_02_series_vec[["main"]],
                             size = 3.4) {
  ggplot2::annotate("point", x = x, y = y, shape = 21, stroke = 1.3,
                    size = size, colour = colour,
                    fill = T_01_01_palette_vec[["ground"]])
}

###### T_02_03a: The Ghost Layers ##############################################
# Note: The live marks at T_01_04_ghost_alpha_num with inherit.aes = FALSE,
#   added before the live layers. T_02_03b_ghost_off_fn says when to skip them.

T_02_03a_ghost_line_fn <- function(data, mapping,
                                   colour = T_01_02_series_vec[["main"]],
                                   linewidth = 1, ...) {
  ggplot2::geom_line(data = data, mapping = mapping, colour = colour,
                     alpha = T_01_04_ghost_alpha_num, linewidth = linewidth,
                     inherit.aes = FALSE, ...)
}

T_02_03a_ghost_path_fn <- function(data, mapping,
                                   colour = T_01_02_series_vec[["main"]],
                                   linewidth = 1, ...) {
  ggplot2::geom_path(data = data, mapping = mapping, colour = colour,
                     alpha = T_01_04_ghost_alpha_num, linewidth = linewidth,
                     inherit.aes = FALSE, ...)
}

T_02_03a_ghost_point_fn <- function(x, y, colour = T_01_02_series_vec[["main"]],
                                    size = 3.4) {
  ggplot2::annotate("point", x = x, y = y, shape = 21, stroke = 1.3,
                    size = size, colour = colour,
                    alpha = T_01_04_ghost_alpha_num,
                    fill = T_01_01_palette_vec[["ground"]])
}

T_02_03b_ghost_off_fn <- function(par, ref) {
  is.null(ref) || isTRUE(all.equal(par, ref))
}

###### T_02_04: Euro Formatter #################################################
# Note: Amounts in billions unless the app says otherwise. The sign is the
#   escape "\u20ac", never a literal: shinylive sources R/*.R in a C locale.

T_02_04_eur_fn <- function(x, digits = 0, unit = "bn") {
  paste0("\u20ac", formatC(x, format = "f", digits = digits, big.mark = ","),
         unit)
}

###### T_02_05: Number Formatter ###############################################
# Note: Fixed decimals without the currency sign.

T_02_05_num_fn <- function(x, digits = 2) {
  formatC(x, format = "f", digits = digits, big.mark = ",")
}

###### T_02_06: Percentage Formatter ###########################################
# Note: Integer percentages, as the style guide asks for.

T_02_06_pct_fn <- function(x, digits = 0) {
  paste0(formatC(x * 100, format = "f", digits = digits), "%")
}

#### T_03: Controls ############################################################
# Note: The slider-plus-typing-box control, and the server side that keeps the
#   two in step.

###### T_03_01: Control Builder ################################################
# Note: A label with a hover explanation, then a slider with a box beside it
#   for typing an exact value. "controls" and "help" are the app's own lists.

T_03_01_control_fn <- function(id, controls, help, defaults, min = NULL) {
  spec  <- controls[[id]]
  value <- defaults[[id]]
  shiny::tags$div(
    class = "ctl",
    shiny::tags$div(class = "ctl-label", shiny::HTML(spec$label)),
    shiny::tags$div(
      class = "ctl-row",
      shiny::tags$div(
        class = "ctl-slider",
        shiny::sliderInput(id, NULL,
                           min = if (is.null(min)) spec$min else min,
                           max = spec$max, value = value, step = spec$step,
                           width = "100%")),
      shiny::tags$div(
        class = "ctl-box",
        shiny::numericInput(paste0(id, "_box"), NULL, value = value,
                            step = spec$step, width = "100%"))
    ),
    T_03_05_note_fn(help[[id]])
  )
}

###### T_03_02: Slider and Box in Step #########################################
# Note: The box holds the exact value; the slider follows it to the nearest
#   step and is not copied back. Call once from the server.

T_03_02_sync_fn <- function(input, session, controls) {
  near <- function(slider_value, box_value, spec) {
    abs(slider_value - min(max(box_value, spec$min), spec$max)) <=
      spec$step / 2 + 1e-8
  }
  lapply(names(controls), function(id) {
    box  <- paste0(id, "_box")
    spec <- controls[[id]]

    shiny::observeEvent(input[[id]], {
      b <- input[[box]]
      if (!is.null(b) && !is.na(b) && near(input[[id]], b, spec)) return()
      shiny::updateNumericInput(session, box, value = input[[id]])
    }, ignoreInit = TRUE)

    box_typed <- shiny::debounce(shiny::reactive(input[[box]]), 500)

    shiny::observeEvent(box_typed(), {
      b <- box_typed()
      if (is.null(b) || is.na(b)) return()
      v <- min(max(b, spec$min), spec$max)
      if (!near(input[[id]], v, spec)) {
        shiny::updateSliderInput(session, id, value = v)
      }
    }, ignoreInit = TRUE)
  })
  invisible(NULL)
}

###### T_03_03: Set One Control ################################################
# Note: Moves the slider and the box together, for scenarios and reset.

T_03_03_set_fn <- function(session, controls, id, value) {
  spec <- controls[[id]]
  shiny::updateSliderInput(session, id,
                           value = min(max(value, spec$min), spec$max))
  shiny::updateNumericInput(session, paste0(id, "_box"), value = value)
  invisible(NULL)
}

###### T_03_04: Read a Control #################################################
# Note: The exact value: the typed box if it holds one, else the slider.

T_03_04_val_fn <- function(input, id) {
  b <- input[[paste0(id, "_box")]]
  if (is.null(b) || is.na(b)) input[[id]] else b
}

###### T_03_05: A Grey Note Under a Control ####################################
# Note: The explanation of what a control does, shown in grey under it.

T_03_05_note_fn <- function(txt) {
  if (is.null(txt) || !nzchar(txt)) return(NULL)
  shiny::tags$div(class = "ctl-note", shiny::HTML(txt))
}

#### T_04: Tiles ###############################################################
# Note: The readouts above the figures.

###### T_04_01: Readout Tile ###################################################
# Note: A read-only tile. "class" takes good or bad for the accent stripe.

T_04_01_tile_fn <- function(label, value, note = NULL, class = "") {
  # HTML() so a \uXXXX escape in the value survives shinylive's C locale
  shiny::tags$div(
    class = paste("stat-tile", class),
    shiny::tags$div(class = "stat-label", shiny::HTML(label)),
    shiny::tags$div(class = "stat-value",
                    if (is.character(value)) shiny::HTML(value) else value),
    if (!is.null(note)) shiny::tags$div(class = "stat-note",
                                        shiny::HTML(note))
  )
}

###### T_04_02: Typed Readout Tile #############################################
# Note: A tile whose number can be typed over, so the class can ask "what
#   would deliver this?" and let the app invert the algebra.

T_04_02_typed_fn <- function(id, label, value, hint, step = 0.05) {
  shiny::tags$div(
    class = "stat-tile",
    shiny::tags$div(class = "stat-label", shiny::HTML(label)),
    shiny::tags$div(class = "stat-input",
                    shiny::numericInput(id, NULL, value = round(value, 3),
                                        step = step, width = "100%")),
    shiny::tags$div(class = "stat-hint", shiny::HTML(hint))
  )
}

###### T_04_03: Tile Row #######################################################
# Note: Wraps tiles in the flex row. Pass tiles as a list; NULLs are dropped,
#   so an app can gate a tile on the stage with an if().

T_04_03_row_fn <- function(...) {
  shiny::tags$div(class = "stat-row mb-2", ...)
}

#### T_05: Scenarios ###########################################################
# Note: The scenario menu and the story panel under it.

###### T_05_01: Scenario Choices ###############################################
# Note: Grouped by stage for the drop-down and numbered within each stage in
#   the order defined, so adding one renumbers the rest.

T_05_01_choices_fn <- function(scenarios, stage_word = "Stage") {
  out <- c(
    list("Choose a scenario" = c("Custom (set sliders yourself)" = "custom")),
    lapply(
      split(scenarios, vapply(scenarios, `[[`, "", "stage")),
      function(grp) {
        stats::setNames(names(grp),
                        paste0("Scenario ", seq_along(grp), ": ",
                               vapply(grp, `[[`, "", "label")))
      }
    )
  )
  names(out)[-1] <- paste(stage_word, names(out)[-1])
  out
}

###### T_05_02: Scenario Story #################################################
# Note: One paragraph, with each symbol named in brackets where it comes up.
#   An app that supplies "key" gets a definition list underneath. See
#   CONVENTIONS.md 3.

T_05_02_story_fn <- function(scenario, controls, help) {
  if (is.null(scenario)) return(NULL)
  legacy <- if (is.null(scenario$key)) NULL else {
    ids <- unlist(scenario$key, use.names = FALSE)
    list(
      shiny::tags$div(class = "story-key", "What matters here"),
      shiny::tags$dl(lapply(ids, function(id) {
        shiny::tagList(
          shiny::tags$dt(shiny::HTML(controls[[id]]$label)),
          shiny::tags$dd(shiny::HTML(help[[id]])))
      }))
    )
  }
  shiny::tags$div(class = "story", shiny::HTML(scenario$story), legacy)
}

###### T_05_03: Apply a Scenario ###############################################
# Note: Sets the stage, then every control: the scenario's own values where
#   it names them and the defaults everywhere else.

T_05_03_apply_fn <- function(session, scenario, controls, defaults,
                             stage_id = "stage") {
  if (is.null(scenario)) return(invisible(NULL))
  shiny::updateRadioButtons(session, stage_id, selected = scenario$stage)
  for (id in names(controls)) {
    T_03_03_set_fn(session, controls, id,
                   if (!is.null(scenario$values[[id]])) {
                     scenario$values[[id]]
                   } else {
                     defaults[[id]]
                   })
  }
  invisible(NULL)
}

###### T_05_04: Worked-Example Presets #########################################
# Note: The worked-example buttons for the stage on screen, in the main window
#   above the prompt; built once at UI time. See CONVENTIONS.md 1 and 2.

T_05_04_presets_fn <- function(scenarios, stages, stage_word = "Stage",
                               stage_id = "stage",
                               title_id = "preset_title",
                               story_id = "scenario_story") {
  by_stage <- split(names(scenarios), vapply(scenarios, `[[`, "", "stage"))
  panels <- lapply(names(by_stage), function(st) {
    shiny::conditionalPanel(
      sprintf("input.%s == '%s'", stage_id, st),
      shiny::tags$div(
        class = "preset-row",
        lapply(by_stage[[st]], function(k)
          shiny::actionButton(
            paste0("preset_", k), scenarios[[k]]$label,
            class = "btn btn-outline-primary btn-sm preset-btn"))
      )
    )
  })
  shiny::conditionalPanel(
    sprintf("[%s].indexOf(input.%s) >= 0",
            paste0("'", names(by_stage), "'", collapse = ", "), stage_id),
    bslib::card(
      bslib::card_header(shiny::uiOutput(title_id, inline = TRUE)),
      shiny::tags$div(
        class = "preset-lead",
        "Load another example, or move any slider \u2014 they stay live."),
      panels,
      shiny::uiOutput(story_id)
    )
  )
}

###### T_05_04: Name of a Stage ################################################
# Note: Pulls the descriptive half out of a stage name such as "Stage 2:
#   Population Growth", for the equations card title.

T_05_04_stage_name_fn <- function(stages, stage) {
  hit <- names(stages)[match(stage, unname(stages))]
  if (is.na(hit)) return(paste("Stage", stage))
  hit
}

###### T_05_05: Which Preset Is Loaded #########################################
# Note: Marks the loaded preset's button. The server calls
#   session$sendCustomMessage("dgPreset", key) when the loaded preset changes.

T_05_05_preset_js_chr <- "
Shiny.addCustomMessageHandler('dgPreset', function (key) {
  $('.preset-row .preset-btn').removeClass('is-loaded').blur();
  if (key) { $('#preset_' + key).addClass('is-loaded'); }
});
"

###### T_05_06: Preset Card Title ##############################################
# Note: renderUI, not renderText, so the middle dot survives the C locale
#   shinylive runs in.

T_05_06_preset_title_fn <- function(scenario, stage, stages,
                                    stage_word = "Stage") {
  nm  <- trimws(names(stages)[match(stage, stages)])
  # The stage word is added only when the name does not already carry it
  lab <- if (nzchar(stage_word) && !startsWith(nm, stage_word)) {
    paste(stage_word, nm)
  } else {
    nm
  }
  shiny::HTML(if (is.null(scenario)) {
    paste0("Worked Examples &middot; ", lab)
  } else {
    paste0("Worked Example: ", scenario$label, " &middot; ", lab)
  })
}

###### T_05_07: Preset Card Styling ############################################
# Note: Mounted by each app in the page head beside its own style tag:
#   shiny::tags$style(shiny::HTML(T_05_07_preset_css_chr)).

T_05_07_preset_css_chr <- "
  .preset-lead { font-size: 0.82rem; color: #6C757D; margin: 0 0 0.55rem 0; }
  .preset-row { display: flex; flex-wrap: wrap; gap: 0.5rem;
                align-items: center; }
  .preset-btn { white-space: normal; text-align: left; }
  .preset-btn:focus { box-shadow: none; }
  .preset-btn.is-loaded { background: #04204C; border-color: #04204C;
                          color: #FFFFFF; }
  .preset-custom { color: #6C757D; text-decoration: none; }
  .preset-custom:hover { color: #04204C; text-decoration: underline; }
  .card .story { margin-top: 0.75rem; }
"

#### T_06: Equations and Notation ##############################################
# Note: The three-tab panel showing the model at this stage. An equations item
#   is list(group, label, versions, notes), the last two keyed by stage.

###### T_06_01: Inline Maths ###################################################
# Note: Wraps LaTeX for MathJax.

T_06_01_mj_fn <- function(tex) shiny::HTML(paste0("\\(", tex, "\\)"))

###### T_06_02: New or Changed Flag ############################################
# Note: The little badge beside a row's label.

T_06_02_flag_fn <- function(status) {
  if (status != "") {
    shiny::tags$span(class = paste0("eq-flag eq-", status), status)
  }
}

###### T_06_03: Items in Force #################################################
# Note: Every item that has appeared by this stage, with its current version,
#   whether it is new or changed here, and what it was before.

T_06_03_items_fn <- function(equations, stage) {
  items <- Filter(function(it) min(as.numeric(names(it$versions))) <= stage,
                  equations)
  lapply(items, function(it) {
    keys   <- as.numeric(names(it$versions))
    cur    <- max(keys[keys <= stage])
    cur_nm <- names(it$versions)[keys == cur]
    status <- if (cur != stage) "" else if (cur == min(keys)) "new" else
      "changed"
    was <- if (status == "changed") {
      it$versions[[names(it$versions)[keys == max(keys[keys < cur])]]]
    }
    list(group = it$group, label = it$label, tex = it$versions[[cur_nm]],
         status = status, was = was, note = it$notes[[cur_nm]])
  })
}

###### T_06_04: Equations Tab ##################################################
# Note: The equations alone, in their groups, two columns wide.

T_06_04_model_fn <- function(items, groups, empty_text) {
  group_col <- function(grp) {
    rows <- lapply(Filter(function(x) x$group == grp, items), function(x) {
      shiny::tags$tr(
        shiny::tags$td(class = "eq-label", shiny::HTML(x$label),
                       T_06_02_flag_fn(x$status)),
        shiny::tags$td(T_06_01_mj_fn(x$tex)))
    })
    shiny::tags$div(
      class = "eq-group",
      shiny::tags$div(class = "eq-group-title", groups[[grp]]),
      if (length(rows) == 0) {
        shiny::tags$div(class = "chg-note text-muted", empty_text)
      } else {
        shiny::tags$table(class = "eq-table", do.call(shiny::tagList, rows))
      }
    )
  }
  shiny::withMathJax(shiny::tagList(
    do.call(bslib::layout_columns, c(
      list(col_widths = bslib::breakpoints(sm = 12, md = c(6, 6, 6, 6),
                                           xl = c(7, 5, 7, 5))),
      lapply(names(groups), group_col)
    )),
    shiny::tags$div(
      class = "eq-legend",
      shiny::tags$span(class = "eq-flag eq-new", "new"), " and ",
      shiny::tags$span(class = "eq-flag eq-changed", "changed"),
      " mark what this stage adds to the one before. The Explanations tab",
      " says what each one does.")
  ))
}

###### T_06_05: Notation Tab ###################################################
# Note: Every symbol in force, in one column per group. A notation item is
#   list(grp = , sym = , txt = , from = ).

T_06_05_notation_fn <- function(notation, stage, columns, first_stage) {
  items <- Filter(function(x) x$from <= stage, notation)
  col <- function(grps, title) {
    its <- Filter(function(x) x$grp %in% grps, items)
    shiny::tags$div(
      shiny::tags$div(class = "eq-group-title", title),
      shiny::tags$table(class = "nota-table", lapply(its, function(x) {
        shiny::tags$tr(
          shiny::tags$td(T_06_01_mj_fn(x$sym)),
          shiny::tags$td(paste0(toupper(substr(x$txt, 1, 1)),
                                substring(x$txt, 2)),
                         if (x$from == stage && stage > first_stage) {
                           shiny::tags$span(class = "eq-flag eq-new", "new")
                         }))
      }))
    )
  }
  shiny::withMathJax(do.call(bslib::layout_columns, c(
    list(col_widths = bslib::breakpoints(
      sm = 12, lg = rep(floor(12 / length(columns)), length(columns)))),
    lapply(names(columns), function(title) col(columns[[title]], title))
  )))
}

###### T_06_06: Explanations Tab ###############################################
# Note: Every equation with its explanation, and what it was before.

T_06_06_explain_fn <- function(items, groups) {
  blocks <- lapply(names(groups), function(grp) {
    its <- Filter(function(x) x$group == grp, items)
    if (length(its) == 0) return(NULL)
    shiny::tagList(
      shiny::tags$tr(shiny::tags$td(colspan = "3", class = "eq-group-title",
                                    groups[[grp]])),
      lapply(its, function(x) {
        shiny::tags$tr(
          shiny::tags$td(class = "eq-label", shiny::HTML(x$label),
                         T_06_02_flag_fn(x$status)),
          shiny::tags$td(class = "eq-math",
                         shiny::tags$div(T_06_01_mj_fn(x$tex)),
                         if (!is.null(x$was)) {
                           shiny::tags$div(class = "chg-was", "was ",
                                           T_06_01_mj_fn(x$was))
                         }),
          shiny::tags$td(class = "chg-note", shiny::HTML(x$note))
        )
      })
    )
  })
  shiny::withMathJax(shiny::tags$table(class = "eq-table eq-explain", blocks))
}

#### T_07: Page Furniture ######################################################
# Note: The bslib theme, the CSS, the title bar, the QR block and the footer.

###### T_07_01: Author Credit ##################################################
# Note: Shown in the title bar, the browser tab and the footer, so every app
#   carries its credit wherever it is linked from.

T_07_01_author_chr <- "Sam Deegan"

###### T_07_02: Author Website #################################################
# Note: Linked from the title bar and footer.

T_07_02_site_chr <- "https://sam-deegan.com"

###### T_07_03: Course Line ####################################################
# Note: Footer text naming the module the apps were built for.

T_07_03_course_chr <- "ECON42550 Macroeconomics, University College Dublin"

###### T_07_04: QR Code Source #################################################
# Note: The image source for the QR: the data URI in T_07_04b.

T_07_04_qr_fn <- function(file = "qr-sam-deegan.png",
                          shared = file.path("..", "..", "_shared")) {
  T_07_04b_qr_data_chr
}

###### T_07_04b: The QR Code Itself ############################################
# Note: The QR as a data URI, so a shinylive export, which drops www/, still
#   shows it.

T_07_04b_qr_data_chr <- paste0(
  "data:image/png;base64,",
  "iVBORw0KGgoAAAANSUhEUgAAAdAAAAHQCAIAAACeP6xXAAAG6UlEQVR42u3cwW",
  "0cMRBFQa9BB6KAFK0DciA+0DdDJwEEppe/yaoAJE5r5oHQoV9zzh8A1PtpBACC",
  "CyC4AAgugOACCC4AggsguAAILoDgAgguAIILILgACC6A4AIILgCCCyC4AAgugO",
  "ACCC4AggsguAAILoDgAgguAIILILgACC6A4AIILgDVRvUv+PXxacoP+vvnd+n8",
  "V39+9ftQ/bzV8/d9nf19ueEChBJcAMEFEFwABBdAcAEE1wgABBdAcAEQXADBBR",
  "BcAAQXQHABEFyAVCPtQGn7Rqul7TO9bZ/sbe+b78sNF+AKggsguACCC4DgAggu",
  "gOAaAYDgAgguAIILILgAgguA4AIILgCCC5BqdH+AtH2X3feNrp4/bf5p70/398",
  "H35YYLILgACC6A4AIILgCCCyC4AAgugOACCC4AggsguAAILoDgAgguAA8aRnC2",
  "6v2h1ftz086/Ku15ccMFEFwABBdAcAEQXADBBRBcAAQXQHABEFwAwQUQXAAEF0",
  "BwARBcgI3swz1c9T7W7vtt7avFDRdAcAEQXADBBUBwAQQXQHABEFwAwQVAcAEE",
  "F0BwARBcAMEFQHAB3qb9Plz7SffOp/rnd99X2/399H254QIILgCCCyC4AIILgO",
  "ACCC4AggsguACCC4DgAgguAIILILgAggvAg+L24a7uP+XZea7uP+2+r7b7/H1f",
  "brgACC6A4AIILgCCCyC4AAgugOACCC4AggsguAAILoDgAgguAIIL0Ej5Ptzb9p",
  "+m6T5/+3l9X264AAgugOACCC4AggsguAAILoDgAgguAIILILgACC6A4AIILgCC",
  "C3CM8n24aftJV89zG/tze72f3c+T9j1Wz9MNF+BNBBdAcAEEFwDBBRBcAME1Ag",
  "DBBRBcAAQXQHABBBcAwQUQXAAEFyDVa8551QNX79+0n3Qvz3v286Z9j264AKEE",
  "F0BwAQQXAMEFEFwAwTUCAMEFEFwABBdAcAEEFwDBBRBcAAQXINXo/gDd98PaT/",
  "rsfLq/D7edp/v+aDdcAP9SABBcAAQXQHABEFwAwQUQXAAEF0BwARBcAMEFEFwA",
  "BBdAcAFYUr4Pt3ofZdq+TvtAneed0vbnVs/fDRcAwQUQXADBBUBwAQQXAMEFEF",
  "wAwQVAcAEEFwDBBRBcAMEFQHABWhtGsFf1PtDu+2TT9infNp+09y3t+3LDBQgl",
  "uACCCyC4AAgugOACCK4RAAgugOACILgAggsguAAILoDgAiC4AKnsw31Y2j7QVW",
  "n7Q51/7/vmPG64AC0JLoDgAgguAIILILgAgmsEAIILILgACC6A4AIILgCCCyC4",
  "AAguQKq4fbjd911W72Ndfd7qeXrevT+/+77a23rihgvgXwoAgguA4AIILoDgGg",
  "GA4AIILgCCCyC4AIILgOACCC4AggsguACXi9uHm7aP9bZ9r91VP699wWc/rxsu",
  "gH8pACC4AIILILgACC6A4AIguACCCyC4AAgugOACILgAggsguACUGkawV9q+zr",
  "R9u/ar7j1P2v7c7vug3XABBBdAcAEQXADBBRBcIwAQXADBBUBwAQQXQHABEFwA",
  "wQVAcAEEF+ByrzmnKRwsbX9r2v7c286Tdv60fcduuACHEFwAwQUQXAAEF0BwAQ",
  "TXCAAEF0BwARBcAMEFEFwABBdAcAEQXIBUo/oXpO1j7S5tv2f1/tPVn582n7Tv",
  "q/v7030+brgAbyK4AIILILgACC6A4AIIrhEACC6A4AIguACCCyC4AAgugOACIL",
  "gAqUbagewzNf+T5unvZT5uuACCCyC4AAgugOACILgAggsguAAILoDgAiC4AIIL",
  "ILgACC6A4ALw3+j+APbJ7p1n9fN2P0/39xk3XADBBUBwAQQXQHABEFwAwQVAcA",
  "EEF0BwARBcAMEFQHABBBdAcAF40DACvlrdJ1u9vzXtPNXnX5W2L7ha2vvmhgsQ",
  "SnABBBdAcAEQXADBBRBcIwAQXADBBUBwAQQXQHABEFwAwQVAcAFS2Yd7uOr9nr",
  "ftq60+f9p+27S/lxsuAIILILgAgguA4AIILgCCCyC4AIILgOACCC4AggsguACC",
  "C4DgArTWfh9u9T5Qvle9vzVtP2z397l6v233/chuuACCC4DgAggugOACILgAgg",
  "uA4AIILoDgAiC4AIILgOACCC6A4AJQKm4f7m37MbvP87b9p2nPu3qetH273ff/",
  "uuEChBJcAMEFEFwABBdAcAEE1wgABBdAcAEQXADBBRBcAAQXQHABEFyAVK85py",
  "kAuOECCC4AggsguACCC4DgAgguAIILILgAgguA4AIILgCCCyC4AIILgOACCC4A",
  "ggsguACCC4DgAgguAIILILgAgguA4AIILgCCCyC4AIILgOACnOAfrk+XUEDcDq",
  "kAAAAASUVORK5CYII="
)

###### T_07_05: bslib Theme ####################################################
# Note: Dublin colours, so the apps match the slides.

T_07_05_theme_fn <- function() {
  bslib::bs_theme(
    version   = 5,
    primary   = T_01_01_palette_vec[["blue"]],
    secondary = T_01_01_palette_vec[["muted"]],
    success   = T_01_01_palette_vec[["green"]],
    info      = T_01_01_palette_vec[["light"]],
    bg        = T_01_01_palette_vec[["ground"]],
    fg        = T_01_01_palette_vec[["ink"]],
    base_font = bslib::font_collection(
      bslib::font_google("IBM Plex Sans", local = FALSE),
      "Segoe UI", "Helvetica", "Arial", "sans-serif"),
    heading_font = bslib::font_collection(
      bslib::font_google("IBM Plex Sans", local = FALSE),
      "Segoe UI", "Helvetica", "Arial", "sans-serif")
  )
}

###### T_07_06: Extra CSS ######################################################
# Note: Stat tiles, prompt, story, the slider-plus-box controls, sidebar
#   headings, tab strips and the site nav. Only theme colours appear here.

T_07_06_css_chr <- "
  /* Headings take the body line height of 1.5; tighten them. */
  h1, h2, h3, h4, h5, h6,
  .bslib-page-title, .card-header { line-height: 1.2; }
  .bslib-page-title { line-height: 1.15; letter-spacing: -0.01em; }
  .stat-row { display: flex; flex-wrap: wrap; gap: 0.75rem; }
  .stat-caption { font-size: 0.8rem; color: #6C757D; margin: 0.2rem 0; }
  .stat-slot { flex: 1 1 11rem; display: flex; }
  .stat-slot > * { flex: 1 1 auto; }
  .stat-input .form-group { margin-bottom: 0; }
  .stat-input input { font-size: 1.35rem; font-weight: 600; color: #04204C;
    padding: 0.05rem 0.4rem; border: 1px solid #D8E0E6; background: #FFFFFF; }
  .stat-hint { font-size: 0.72rem; color: #6C757D; font-style: italic; }
  .side-qr { flex: 0 0 auto; }
  .side-qr img { width: 56px; height: 56px; display: block; }
  .sidebar-qr { text-align: center; margin-top: 1rem; font-size: 0.8rem; }
  .sidebar-qr img { width: 110px; height: 110px; }
  .sidebar-qr-name { font-weight: 700; color: #04204C; margin-top: 0.3rem; }
  .bslib-page-title { display: flex; align-items: center; gap: 0.3rem;
    width: 100%; }
  .title-qr { margin-left: auto; }
  .title-qr img { height: 40px; width: 40px; }
  .stat-tile { flex: 1 1 11rem; border: none;
    border-left: 5px solid #0056A4; border-radius: 4px;
    padding: 0.45rem 0.8rem; background: #F2F6F9; }
  .stat-tile.good { border-left-color: #61B77C; }
  .stat-tile.bad  { border-left-color: #04204C; }
  .stat-label { font-size: 0.8rem; color: #6C757D; }
  .stat-value { font-size: 1.5rem; font-weight: 600; color: #04204C; }
  .stat-note  { font-size: 0.78rem; color: #212529; }
  .eq-table td { padding: 0.15rem 0.9rem 0.15rem 0; vertical-align: middle;
    border-bottom: 1px solid #F2F6F9; }
  .eq-label { color: #6C757D; font-size: 0.85rem; white-space: nowrap; }
  .eq-group { overflow-x: auto; }
  .eq-group-title { font-weight: 700; color: #04204C; font-size: 0.9rem;
    border-bottom: 2px solid #D8E0E6; margin-bottom: 0.3rem; }
  .eq-flag { display: inline-block; font-size: 0.65rem; font-weight: 700;
    text-transform: uppercase; letter-spacing: 0.04em; color: #FFFFFF;
    padding: 0.05rem 0.35rem; border-radius: 3px; margin-left: 0.35rem; }
  .eq-new     { background: #61B77C; }
  .eq-changed { background: #0056A4; }
  .eq-legend  { font-size: 0.78rem; color: #6C757D; margin-top: 0.3rem; }
  .eq-explain { width: 100%; table-layout: fixed; }
  .eq-explain td.eq-label { width: 22%; white-space: normal; }
  .eq-explain td.eq-math { width: 42%; }
  .eq-explain td.chg-note { width: 36%; }
  .eq-explain td.eq-group-title { padding-top: 0.6rem; }
  .nota-table { width: 100%; font-size: 0.88rem; }
  .nota-table td { padding: 0.25rem 0.6rem 0.25rem 0; vertical-align: top;
    border-bottom: 1px solid #F2F6F9; }
  .nota-table td:first-child { white-space: nowrap; width: 6.5rem; }
  .chg-was  { color: #6C757D; font-size: 0.85rem; }
  .chg-note { font-size: 0.85rem; }
  .prompt { background: #F2F6F9; border-left: 4px solid #61B77C;
    padding: 0.6rem 0.9rem; border-radius: 4px; font-size: 0.95rem; }
  .problem { background: #F2F6F9; border-left: 4px solid #04204C;
    padding: 0.6rem 0.9rem; border-radius: 4px; }
  .story { background: #F2F6F9; border-radius: 4px; font-size: 0.86rem;
    padding: 0.55rem 0.75rem; margin: -0.4rem 0 0.7rem 0; }
  .story-key { font-weight: 700; color: #04204C; margin-top: 0.45rem; }
  .story dl { margin: 0.2rem 0 0 0; }
  .story dt { font-weight: 600; color: #0056A4; }
  .story dd { margin: 0 0 0.3rem 0; }
  .narrative { border: 1px solid #D8E0E6; border-left: 4px solid #61B77C;
    border-radius: 4px; padding: 0.55rem 0.75rem; font-size: 0.88rem;
    margin-bottom: 0.7rem; background: #FFFFFF; }
  .nar-head { font-weight: 700; color: #04204C; margin-bottom: 0.2rem; }
  .nar-source { font-size: 0.86em; color: #6C757D; margin-top: 0.5rem; }
  .ctl { margin-bottom: 0.4rem; }
  .ctl-label { font-size: 0.88rem; font-weight: 600; color: #0056A4;
    margin-bottom: -0.25rem; }
  .ctl-help { color: #0056A4; cursor: help; font-size: 0.85rem; }
  .ctl-row { display: flex; gap: 0.5rem; align-items: center; }
  .ctl-slider { flex: 1 1 auto; min-width: 0; }
  .ctl-box { flex: 0 0 4.9rem; }
  .ctl .form-group { margin-bottom: 0; }
  .ctl-box input { padding: 0.15rem 0.35rem; font-size: 0.85rem;
    text-align: right; }
  .sidebar .accordion-button { font-weight: 700; color: #04204C; }
  .sidebar h6 { color: #04204C; font-weight: 700; margin-top: 0.5rem; }
  .title-credit { font-size: 0.8rem; font-weight: 400; margin-left: 0.8rem;
    opacity: 0.8; }
  .title-credit a { color: inherit; }
  .credit { font-size: 0.8rem; color: #6C757D; text-align: center;
    padding: 1rem 0 0.5rem 0; }
  .nav-tabs .nav-link { color: #6C757D; font-weight: 600;
    border-color: transparent; }
  .nav-tabs .nav-link:hover { color: #0056A4; background: #F2F6F9; }
  .nav-tabs .nav-link.active { color: #FFFFFF; font-weight: 700;
    background: #0056A4; border-color: #0056A4; }
  .site-nav { background: #FFFFFF; border-bottom: 1px solid #D8E0E6;
    padding: 1rem 2rem; margin: -0.5rem -0.5rem 0.75rem -0.5rem; }
  .site-nav-row { display: flex; align-items: center; gap: 2rem;
    max-width: 1200px; margin: 0 auto; }
  .site-nav ul { list-style: none; display: flex; justify-content: center;
    gap: 2rem; flex: 1 1 auto; margin: 0; padding: 0; flex-wrap: wrap; }
  .site-nav a { color: #04204C; text-decoration: none; font-weight: 500;
    font-size: 0.95rem; padding-bottom: 0.25rem; position: relative;
    transition: color 0.3s ease; }
  .site-nav a:hover, .site-nav a.active { color: #0056A4; }
  .site-nav a.active::after { content: ''; position: absolute;
    bottom: -0.5rem; left: 0; right: 0; height: 2px; background: #0056A4; }
  .page-title-wrap { display: flex; width: 100%; align-items: center;
    justify-content: space-between; gap: 1rem; }
  .page-title-text { min-width: 0; }
  .page-byline { color: #6C757D; font-size: 0.95rem; font-weight: 500;
    margin: -0.35rem 0 0.6rem 0; }
  .page-byline a { color: #6C757D; text-decoration: none;
    display: inline-flex; align-items: center; gap: 0.45rem; }
  .page-byline a:hover { color: #0056A4; }
  .page-byline img { width: 24px; height: 24px; border-radius: 50%; }
  @media (max-width: 700px) {
    .site-nav { padding: 0.6rem 0.75rem; }
    .site-nav ul { gap: 1rem; font-size: 0.85rem; }
    .site-nav-row { gap: 0.75rem; flex-wrap: wrap; }
  }
  #stage-label, #scenario-label { font-weight: 700; color: #04204C;
    font-size: 0.95rem; margin-bottom: 0.35rem; }
  #stage .radio label, #stage .shiny-options-group label { font-weight: 400;
    color: #212529; }
  .ctl-note { font-size: 0.78rem; color: #6C757D; line-height: 1.45;
    margin: 0.15rem 0 0.1rem 0; }
  .fig-head { display: flex; align-items: center; gap: 0.5rem; }
  .fig-note { color: #6C757D; font-size: 0.82rem; line-height: 1.45;
    padding: 0.15rem 0.15rem 0 0.15rem; }
  .fig-note p { margin: 0; }
  .fig-save { margin-left: auto; border: 1px solid #D8E0E6; background: #FFFFFF;
    color: #0056A4; font-size: 0.72rem; font-weight: 600; border-radius: 3px;
    padding: 0.1rem 0.5rem; cursor: pointer; line-height: 1.5;
    transition: background 0.2s ease, color 0.2s ease; }
  .fig-save:hover { background: #0056A4; color: #FFFFFF;
    border-color: #0056A4; }
  .fig-save:active { background: #003C77; border-color: #003C77; }
"

###### T_07_06b: The MathJax Library ###########################################
# Note: MathJax 2.7.9 from cdnjs, loaded in the page head because the tag
#   withMathJax() emits is a singleton that renderUI drops. Keeps MathJax.Hub.

T_07_06b_mathjax_src_chr <- paste0(
  "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.9/MathJax.js",
  "?config=TeX-AMS-MML_HTMLorMML")

###### T_07_07: Re-Typeset MathJax #############################################
# Note: Re-typesets when a tab is shown and whenever Shiny delivers a new
#   value; a no-op until the library has loaded.

T_07_07_mathjax_js_chr <- paste(
  "(function() {",
  "  var pending = null;",
  "  function typeset() {",
  "    if (!window.MathJax || !MathJax.Hub) return;",
  "    clearTimeout(pending);",
  "    pending = setTimeout(function() {",
  "      MathJax.Hub.Queue(['Typeset', MathJax.Hub]);",
  "    }, 50);",
  "  }",
  "  document.addEventListener('shown.bs.tab', typeset);",
  "  $(document).on('shiny:value shiny:visualchange', typeset);",
  "})();",
  sep = "\n"
)

###### T_07_07b: Save a Figure as PNG ##########################################
# Note: Points an <a download> at the PNG Shiny has already rendered, so no
#   R graphics device is involved and it works the same under shinylive.

T_07_07b_save_js_chr <- paste(
  "document.addEventListener('click', function(ev) {",
  "  var btn = ev.target.closest('.fig-save');",
  "  if (!btn) return;",
  "  var box = document.getElementById(btn.dataset.plot);",
  "  var img = box ? box.querySelector('img') : null;",
  "  if (!img || !img.src) { return; }",
  "  var a = document.createElement('a');",
  "  a.href = img.src;",
  "  a.download = (btn.dataset.name || 'figure') + '.png';",
  "  document.body.appendChild(a);",
  "  a.click();",
  "  document.body.removeChild(a);",
  "});",
  sep = "\n"
)

###### T_07_07c: Figure Card ###################################################
# Note: A card holding one figure, with a Save PNG button in its header. Use
#   in place of card(card_header(title), plotOutput(id, height)).

T_07_07c_figcard_fn <- function(id, title, height, file = NULL) {
  T_07_07e_add_fn(id)
  stem <- if (is.null(file)) {
    gsub("(^-|-$)", "",
         gsub("-+", "-", gsub("[^a-z0-9]+", "-", tolower(title))))
  } else {
    file
  }
  bslib::card(
    bslib::card_header(
      shiny::tags$div(
        class = "fig-head",
        shiny::tags$span(title),
        shiny::tags$button(type = "button", class = "fig-save",
                           `data-plot` = id, `data-name` = stem,
                           title = "Save this figure as a PNG",
                           "Save PNG")
      )
    ),
    shiny::plotOutput(id, height = height),
    shiny::uiOutput(paste0(id, "__cap"), class = "fig-note")
  )
}

###### T_07_07d: Wire Up the Lifted Captions ###################################
# Note: Called once from the server. Defines, for every registered figure id,
#   the output that prints the caption T_02_01c_draw_fn lifted out.

T_07_07d_cap_fn <- function(output) {
  ids <- T_07_07e_ids_fn()
  for (id in ids) {
    local({
      this <- id
      output[[paste0(this, "__cap")]] <- shiny::renderUI({
        store <- T_02_01d_capstore_fn()
        txt   <- if (is.null(store)) NULL else store[[this]]
        if (is.null(txt) || !nzchar(txt)) return(NULL)
        shiny::tags$p(txt)
      })
    })
  }
  invisible(ids)
}

###### T_07_07e: The Figure Register ###########################################
# Note: Every figcard records its id here as the UI is constructed, which
#   happens once when the app loads.

T_07_07e_env <- new.env(parent = emptyenv())
T_07_07e_env$ids <- character(0)

T_07_07e_ids_fn <- function() T_07_07e_env$ids

T_07_07e_add_fn <- function(id) {
  if (!id %in% T_07_07e_env$ids) {
    T_07_07e_env$ids <- c(T_07_07e_env$ids, id)
  }
  invisible(NULL)
}

###### T_07_08: Page Head ######################################################
# Note: The style block, IBM Plex Sans (the website's face, loaded here so
#   it is on the page before bslib asks for it), MathJax and the scripts.

T_07_08_head_fn <- function() {
  shiny::tags$head(
    shiny::tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    shiny::tags$link(
      rel = "stylesheet",
      href = paste0("https://fonts.googleapis.com/css2?",
                    "family=IBM+Plex+Sans:wght@400;500;600;700&display=swap")),
    shiny::tags$style(shiny::HTML(T_07_06_css_chr)),
    shiny::tags$script(src = T_07_06b_mathjax_src_chr),
    shiny::tags$script(shiny::HTML(T_07_07_mathjax_js_chr)),
    shiny::tags$script(shiny::HTML(T_07_07b_save_js_chr))
  )
}

###### T_07_08b: Site Navigation ###############################################
# Note: The website's nav bar, not sticky. Links are absolute with
#   target = "_top" because shinylive runs the app in an iframe.

T_07_08b_nav_fn <- function(active = "Resources") {
  pages <- c(Bio = "index.html", Papers = "papers.html",
             Teaching = "teaching.html", Experience = "experience.html",
             Presentations = "talks.html", Resources = "resources.html",
             Contact = "contact.html")
  shiny::tags$nav(
    class = "site-nav",
    shiny::tags$div(
      class = "site-nav-row",
      shiny::tags$ul(
        lapply(names(pages), function(nm) {
          shiny::tags$li(shiny::tags$a(
            href   = paste0(T_07_02_site_chr, "/", pages[[nm]]),
            target = "_top",
            class  = if (identical(nm, active)) "active" else NULL,
            nm))
        })
      )
    )
  )
}

###### T_07_09: Title Bar ######################################################
# Note: The app's name with a byline under it, as on the website.

T_07_09_title_fn <- function(name, qr_src = NULL,
                             logo_src = "sd-logo.png") {
  # Name and byline at the left, the QR block at the right
  shiny::tags$div(
    class = "page-title-wrap",
    shiny::tags$div(
      class = "page-title-text",
      shiny::tags$h1(class = "bslib-page-title", name),
      shiny::tags$div(
        class = "page-byline",
        shiny::tags$a(href = T_07_02_site_chr, target = "_blank",
                      shiny::tags$img(src = logo_src, alt = ""),
                      T_07_01_author_chr))),
    T_07_10_sideqr_fn(qr_src)
  )
}

###### T_07_10: QR Block #######################################################
# Note: The QR code, linked to the site; sits at the right of the title.

T_07_10_sideqr_fn <- function(qr_src) {
  if (is.null(qr_src)) return(NULL)
  shiny::tags$div(
    class = "side-qr",
    shiny::tags$a(href = T_07_02_site_chr, target = "_blank",
                  shiny::tags$img(src = qr_src,
                                  alt = paste("QR code for",
                                              T_07_02_site_chr)))
  )
}

###### T_07_10b: Sidebar QR Block #############################################
# Note: The larger QR code at the foot of the sidebar, with the name and site
#   address under it. The title bar carries the small one (T_07_10).

T_07_10b_sidebarqr_fn <- function(qr_src) {
  if (is.null(qr_src)) return(NULL)
  shiny::tags$div(
    class = "sidebar-qr",
    shiny::tags$a(href = T_07_02_site_chr, target = "_blank",
                  shiny::tags$img(src = qr_src,
                                  alt = paste("QR code for",
                                              T_07_02_site_chr))),
    shiny::tags$div(class = "sidebar-qr-name", T_07_01_author_chr),
    shiny::tags$div(shiny::tags$a(href = T_07_02_site_chr, target = "_blank",
                                  sub("^https?://", "", T_07_02_site_chr)))
  )
}

###### T_07_11: Footer #########################################################
# Note: Who built it and what for. "extra" adds a sentence, such as whose
#   notation the app follows.

T_07_11_footer_fn <- function(extra = NULL, repo = NULL) {
  shiny::tags$footer(
    class = "credit",
    "Built by ",
    shiny::tags$a(href = T_07_02_site_chr, target = "_blank",
                  T_07_01_author_chr),
    paste0(" for ", T_07_03_course_chr, "."),
    if (!is.null(extra)) paste0(" ", extra),
    if (!is.null(repo)) shiny::tagList(
      " ", shiny::tags$a(href = repo, target = "_blank",
                         "Source and download on GitHub"), "."
    )
  )
}

###### T_07_12: Prompt Panel ###################################################
# Note: The guidance line above the figures: a loaded scenario's line is
#   labelled Example, the stage's own line Note.

T_07_12_prompt_fn <- function(scenario, stage, prompts) {
  on_stage <- !is.null(scenario) && scenario$stage == stage
  txt   <- if (on_stage) scenario$prompt else prompts[[stage]]
  label <- if (on_stage) "Example. " else "Note. "
  shiny::tags$div(class = "prompt", shiny::tags$strong(label), txt)
}

###### T_07_13: Problems Panel #################################################
# Note: Warnings shown when the calibration stops making sense.

T_07_13_problems_fn <- function(problems) {
  if (length(problems) == 0) return(NULL)
  shiny::tags$div(class = "problem", lapply(problems, shiny::tags$p))
}
