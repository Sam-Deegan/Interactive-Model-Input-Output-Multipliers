################################################################################
## Project: ECON42550 Macroeconomics                                          ##
## Input-Output App: Plots                                                    ##
################################################################################

## Author:      Sam Deegan
## Affiliation: University College Dublin
## Email:       sam.deegan@ucdconnect.ie

## Usage:
##   Sourced by app.R (B_01_02) after R/io_model.R. Every function returns
##   a ggplot object; the palette, measures and effect names come from B_03
##   in app.R.
##
## Inputs:
##   Results from R/io_model.R; theme and reference-line helpers from
##   R/toolkit.R.
##
## Outputs:
##   D_* functions returning ggplot objects. Nothing on disk.

#-------------------------------- Script Begin --------------------------------#

################################################################################
## A: Table of Contents ########################################################
################################################################################
# Note: D only.
#
#   D: Plots
#     D_01  Theme and helpers
#     D_02  Multipliers (by source, and where each euro of demand ends up)
#     D_03  Demand shock (where output is produced, round by round)
#     D_04  Input coefficients
#     D_05  Trends across the years
#     D_06  Cost a project

################################################################################
## D: Plots ####################################################################
################################################################################
# Note: Stacked bars read dark to light from the left: direct navy,
#   indirect blue, induced green, and the five-part split of the euro
#   (D_02_02) carries the ramp on to light and tint. A pale band marks the
#   chosen row and a black tick the net total of a stacked bar. The reading
#   note under a figure is its caption, lifted out of the image by
#   T_02_01c_draw_fn and set under the card. See CONVENTIONS.md 6.

#### D_01: Theme and Helpers ###################################################
# Note: theme_bw with the Dublin colours; euro formatting; long data.

###### D_01_01: Plot Theme #####################################################
# Note: The shared theme (T_02_01_theme_fn) plus what this app needs on
#   top: a "v" grid for a value read off the x axis of a horizontal bar
#   chart, small legend keys and a longer subtitle. grid = "h" for a value
#   read off the y axis, "none" for a diagram.

D_01_01_theme_fn <- function(base_size = B_03_09_base_size_int,
                             grid = c("h", "v", "none")) {
  grid <- match.arg(grid)
  pal  <- B_03_01_palette_vec
  T_02_01_theme_fn(base_size = base_size,
                   grid = if (identical(grid, "h")) "h" else "none",
                   ratio = NULL) +
    theme(
      panel.grid.major.x = if (identical(grid, "v")) {
        element_line(colour = pal[["rule"]], linewidth = 0.3)
      } else {
        element_blank()
      },
      # Legends carry up to five fills plus a line type and a tick
      legend.key.size    = grid::unit(0.9, "lines"),
      legend.box         = "vertical",
      legend.margin      = margin(0, 0, 0, 0),
      plot.margin        = margin(6, 18, 6, 6)
    )
}

###### D_01_02: Euro Formatter #################################################
# Note: EUR million with a thousands separator, for the graphics device.
#   The euro sign and the minus are escapes; see CONVENTIONS.md 6.

D_01_02_eur_fn <- function(x, digits = 0) {
  paste0(ifelse(x < 0, "\u2212\u20ac", "\u20ac"),
         formatC(abs(x), format = "f", digits = digits, big.mark = ","), "m")
}

###### D_01_03: Effect Sources #################################################
# Note: The effect columns to stack for a multiplier type, named by their
#   legend labels: direct, indirect, and for Type II induced as well.

D_01_03_sources_fn <- function(type) {
  src <- B_03_05_effects_vec
  switch(type,
         t1 = src[c("direct", "indirect")],
         src)
}

###### D_01_04: Wrap Labels ####################################################
# Note: Breaks long axis labels onto two lines.

D_01_04_wrap_fn <- function(x, width = 34) {
  vapply(x, function(s) paste(strwrap(s, width = width), collapse = "\n"), "")
}

###### D_01_05: Row Order ######################################################
# Note: The order of the rows of a horizontal bar chart. "size": smallest
#   at the bottom. "group": section letter then first NACE division,
#   reading down the chart. Falls back to size when the table carries no
#   classification columns.

D_01_05_order_fn <- function(df, value, sort_by = "size") {
  if (identical(sort_by, "group") && !is.null(df$cso_sec_cat)) {
    rev(order(df$cso_sec_cat, df$cso_first_int, df$cso_group_cat))
  } else {
    order(value)
  }
}

###### D_01_06: Net Marker #####################################################
# Note: A short black tick across each bar at its net total, which a
#   negative part (a subsidised sector's net production taxes) can put
#   inside the stack. df needs row, the numeric position of its label.

D_01_06_net_mark_fn <- function(df, net, key) {
  df$net  <- net
  df$mark <- key
  list(
    geom_segment(data = df, aes(x = net, xend = net, y = row - 0.44,
                                yend = row + 0.44, colour = mark),
                 inherit.aes = FALSE, linewidth = 0.7),
    scale_colour_manual(values = stats::setNames("black", key))
  )
}

###### D_01_07: Amount Formatter ###############################################
# Note: Euro million for every measure except jobs, which are counted in
#   FTE job-years. Used by the demand-shock charts, which show a total,
#   not a per-euro multiplier.

D_01_07_amount_fn <- function(measure) {
  if (identical(measure, "jobs")) {
    function(v) formatC(v, format = "f", digits = 0, big.mark = ",")
  } else {
    function(v) D_01_02_eur_fn(v)
  }
}

###### D_01_08: Mark the Chosen Row ############################################
# Note: A pale band behind the chosen row, whose label also carries an
#   arrow. Added as the first layer, so the bar is drawn over it.

D_01_08_band_fn <- function(df) {
  hl <- df[df$is_hl, ]
  if (nrow(hl) == 0) return(NULL)
  ggplot2::annotate("rect", xmin = -Inf, xmax = Inf,
                    ymin = hl$row[1] - 0.5, ymax = hl$row[1] + 0.5,
                    fill = B_03_01_palette_vec[["rule"]], alpha = 0.55)
}

###### D_01_09: Bold Axis Title ################################################
# Note: A plotmath expression ignores element_text(face = "bold"), so the
#   axis title carries its own bold(). ASCII only: an axis title says the
#   unit in words. See CONVENTIONS.md 6.

D_01_09_bold_fn <- function(txt) bquote(bold(.(txt)))

###### D_01_10: Multiplier Axis Title ##########################################
# Note: The word, then the unit. No symbol: m is already imported inputs
#   per euro of output on the Equations tab.

D_01_10_mult_axis_fn <- function(measure) {
  D_01_09_bold_fn(switch(
    measure,
    output    = "Output per euro of final demand",
    gva       = "Value added per euro of final demand",
    wages     = "Wages per euro of final demand",
    gos       = "Profits and depreciation per euro of final demand",
    exchequer = "Taxes per euro of final demand",
    jobs      = "FTE job-years per million euro of final demand",
    imports   = "Imports per euro of final demand",
    "Per euro of final demand"
  ))
}

###### D_01_11: Amount Axis Title ##############################################
# Note: A total, not a ratio: the effect of one shock, in euro million, or in
#   FTE job-years for jobs.

D_01_11_amount_axis_fn <- function(measure) {
  eff <- B_03_03_measures_lst[[measure]]$effect
  D_01_09_bold_fn(if (identical(measure, "jobs")) {
    eff
  } else {
    paste0(eff, " (euro million)")
  })
}

###### D_01_12: The Unit Benchmark #############################################
# Note: Whether a multiplier of one is drawn as the resting point
#   (T_02_02_rest_fn): only for output. The other measures count a part of
#   the euro and sit well under one; jobs are not a per-euro ratio.

D_01_12_bench_fn <- function(measure) identical(measure, "output")

###### D_01_13: The Type in Words ##############################################
# Note: Every title says which multiplier it is drawing.

D_01_13_type_fn <- function(type) {
  if (identical(type, "t2")) "Type II" else "Type I"
}

###### D_01_14: Mark the Value, and the Net Where It Differs ###################
# Note: Two marks, one scale, for D_02_02. The value mark goes on every
#   row at the value-added multiplier, where the printed number sits; the
#   net mark only on rows whose parts do not add to 100% (every row under
#   Type II, none under Type I). One geom and one scale_colour_manual, so
#   neither mark loses its legend entry.

D_01_14_marks_fn <- function(df) {
  key_val <- "Value Added (the Number)"
  key_net <- "Net (What the Parts Add Up To)"
  mark_df <- data.frame(row = df$row, at = df$gva, mark = key_val,
                        stringsAsFactors = FALSE)
  off_lgl <- abs(df$total - 100) > 0.05
  if (any(off_lgl)) {
    mark_df <- rbind(mark_df, data.frame(
      row = df$row[off_lgl], at = df$total[off_lgl], mark = key_net,
      stringsAsFactors = FALSE))
  }
  list(
    geom_segment(data = mark_df,
                 aes(x = at, xend = at, y = row - 0.44, yend = row + 0.44,
                     colour = mark),
                 inherit.aes = FALSE, linewidth = 0.7),
    scale_colour_manual(
      values = stats::setNames(c("black", B_03_01_palette_vec[["muted"]]),
                               c(key_val, key_net)),
      breaks = if (any(off_lgl)) c(key_val, key_net) else key_val)
  )
}

#### D_02: Multipliers #########################################################
# Note: One horizontal bar per product, division or section.

###### D_02_01: Multipliers by Source ##########################################
# Note: Bars stacked into direct, indirect and (Type II) induced effects
#   per euro of final demand; the chosen row is banded, the dashed line is
#   the output-weighted average and the black tick is the net effect, the
#   multiplier itself.

D_02_01_mult_plot_fn <- function(df, measure, type, highlight,
                                 sort_by = "size", year = NULL) {
  pal <- B_03_01_palette_vec
  msr <- B_03_03_measures_lst[[measure]]
  digits <- if (is.null(msr$digits)) 2 else msr$digits
  dir <- df[[paste0("mlt_", measure, "_direct_val")]]
  t1  <- df[[paste0("mlt_", measure, "_t1_val")]]
  t2  <- df[[paste0("mlt_", measure, "_t2_val")]]
  df$value <- switch(type, t2 = t2, t1)
  df$is_hl <- df$cso_group_cat == highlight
  lab <- df$cso_label_str
  lab[df$is_hl] <- paste("\u2192", lab[df$is_hl])
  df$label <- factor(lab, levels = lab[D_01_05_order_fn(df, df$value, sort_by)])
  df$row   <- as.numeric(df$label)
  avg <- sum(df$value * df$cso_output_amt) / sum(df$cso_output_amt)

  # Zero, and one for output, named along the top; see CONVENTIONS.md 6
  bench <- D_01_12_bench_fn(measure)
  x_sc  <- T_02_02_mark_x_fn(
    if (bench) c(0, 1) else 0,
    if (bench) c("0", "1") else "0")
  x_sc$expand <- expansion(mult = c(0.06, 0.12))

  parts <- list(direct = dir, indirect = t1 - dir, induced = t2 - t1)
  src   <- D_01_03_sources_fn(type)
  long  <- do.call(rbind, lapply(names(src), function(p) {
    data.frame(label = df$label, part = src[[p]], value = parts[[p]])
  }))
  long$part <- factor(long$part, levels = rev(unname(src)))
  fills <- stats::setNames(pal[c("navy", "blue", "green")],
                           B_03_05_effects_vec)

  ggplot(long, aes(x = value, y = label, fill = part)) +
    D_01_08_band_fn(df) +
    T_02_02_zero_fn(h = FALSE) +
    (if (bench) T_02_02_rest_fn(v = 1) else NULL) +
    geom_col(width = 0.72) +
    geom_vline(aes(xintercept = avg, linetype = "Output-Weighted Average"),
               colour = pal[["muted"]]) +
    D_01_06_net_mark_fn(df, df$value, "Net Effect (The Multiplier)") +
    geom_text(data = df, aes(x = pmax(value, 0), y = label,
                             label = formatC(value, format = "f",
                                             digits = digits)),
              inherit.aes = FALSE, hjust = -0.15, size = 3.2,
              colour = pal[["ink"]]) +
    scale_fill_manual(values = fills, breaks = unname(src)) +
    scale_linetype_manual(values = c(`Output-Weighted Average` = "dashed")) +
    x_sc +
    labs(x = D_01_10_mult_axis_fn(measure), y = NULL,
         title = paste0(D_01_13_type_fn(type), " ", msr$label,
                        " Multipliers", if (!is.null(year)) {
                          paste0(", ", year, " Table")
                        }),
         # Reading note: lifted out of the image by T_02_01c_draw_fn
         caption = paste(
           "Black tick: the net effect, which is the multiplier itself.",
           "The parts of each bar add up to it."
         )) +
    D_01_01_theme_fn(grid = "v") +
    guides(fill = guide_legend(order = 1), colour = guide_legend(order = 2),
           linetype = guide_legend(order = 3))
}

###### D_02_02: Where Each Euro of Demand Ends Up ##############################
# Note: For one euro of final demand, the wages, operating surplus, taxes
#   and imports it becomes along the supply chain, in per cent. Under Type
#   I the parts add to exactly 100%; under Type II the induced household
#   spending is on top. A part can be negative (net production taxes in a
#   subsidised sector), so the ticks of D_01_14 mark the value and the net.

D_02_02_split_plot_fn <- function(df, type, highlight, sort_by = "size",
                                  year = NULL) {
  pal  <- B_03_01_palette_vec
  comp <- B_03_06_components_vec
  get  <- function(m) df[[paste0("mlt_", m, "_", type, "_val")]] * 100
  df$is_hl <- df$cso_group_cat == highlight
  lab <- df$cso_label_str
  lab[df$is_hl] <- paste("\u2192", lab[df$is_hl])
  df$total <- Reduce(`+`, lapply(names(comp), get))
  gva <- get("gva")
  df$gva <- gva
  df$label <- factor(lab, levels = lab[D_01_05_order_fn(df, gva, sort_by)])
  df$row   <- as.numeric(df$label)
  long <- do.call(rbind, lapply(names(comp), function(m) {
    data.frame(label = df$label, part = comp[[m]], value = get(m))
  }))
  long$part <- factor(long$part, levels = rev(unname(comp)))
  # Dark to light from the left, wages first
  fills <- stats::setNames(
    pal[c("navy", "blue", "green", "light", "tint")], unname(comp)
  )
  # Zero and the whole euro, named along the top
  x_sc <- T_02_02_mark_x_fn(c(0, 100), c("0%", "100%"))
  x_sc$labels <- function(v) paste0(v, "%")
  x_sc$expand <- expansion(mult = c(0.06, 0.14))
  ggplot(long, aes(x = value, y = label, fill = part)) +
    D_01_08_band_fn(df) +
    T_02_02_zero_fn(h = FALSE) +
    T_02_02_rest_fn(v = 100) +
    geom_col(width = 0.72) +
    # Tick at the value-added multiplier; net tick where the parts differ
    D_01_14_marks_fn(df) +
    # The number reads at the end of the row, clear of the slices
    geom_text(data = df, aes(x = pmax(total, 0), y = label,
                             label = formatC(gva / 100, format = "f",
                                             digits = 2)),
              inherit.aes = FALSE, hjust = -0.15, size = 3.2,
              colour = pal[["ink"]]) +
    scale_fill_manual(values = fills, breaks = unname(comp)) +
    x_sc +
    labs(x = D_01_09_bold_fn("Share of each euro of final demand (%)"),
    y = NULL,
    title = paste0("Where Each Euro of Final Demand Goes (",
                   D_01_13_type_fn(type), ")",
                   if (!is.null(year)) paste0(", ", year, " Table")),
    # Reading note: lifted out of the image by T_02_01c_draw_fn
    caption = paste(
      if (identical(type, "t2")) {
        paste("Type II: the slices add to more than 100%, because the",
              "induced household spending is on top of the euro.")
      } else {
        "Type I: the slices add to exactly 100% of the euro."
      },
      "Every bar runs to the whole euro, because the euro is fully",
      "accounted for. The number sits at the boundary where imports begin:",
      "it is the value-added multiplier, the part of the euro that stays",
      "as domestic income, in euro per euro of final demand.",
      "Black tick: the number, at the boundary where imports begin. A part",
      "can be negative, so a bar can run past the sum of its own parts:",
      "other taxes on production are net",
      "of subsidies, so a sector paid more in subsidies than it pays in",
      "production taxes (crops, forestry under the CAP) has a slice below",
      "zero, and another slice has to be that much bigger to reach the",
      "net."
    )) +
    D_01_01_theme_fn(grid = "v") +
    guides(fill = guide_legend(ncol = 2, byrow = TRUE, order = 1),
           colour = guide_legend(order = 2))
}

#### D_03: Demand Shock ########################################################
# Note: Where the extra output ends up, and how it builds up round by round.

###### D_03_01: Where the Output Is Produced ###################################
# Note: The largest rows of a table of effects (by product, division or
#   section), stacked by source. out: data frame with label, direct,
#   indirect, induced, in the units of the measure (EUR million, or FTE
#   job-years for jobs). measure: which of B_03_03 the rows count, so the
#   chart follows the sidebar rather than always showing output.

D_03_01_where_plot_fn <- function(out, type, top_n, title,
                                  measure = "output", year = NULL) {
  pal <- B_03_01_palette_vec
  src <- D_01_03_sources_fn(type)
  out$total <- out$direct + out$indirect +
    (if (type == "t2") out$induced else 0)
  top <- utils::head(out[order(-abs(out$total)), ], top_n)
  top$label <- factor(D_01_04_wrap_fn(top$label),
                      levels = rev(D_01_04_wrap_fn(top$label)))
  long <- do.call(rbind, lapply(names(src), function(p) {
    data.frame(label = top$label, part = src[[p]], value = top[[p]])
  }))
  long$part <- factor(long$part, levels = rev(unname(src)))
  fills <- stats::setNames(pal[c("navy", "blue", "green")],
                           B_03_05_effects_vec)
  # A total, not a ratio, so zero is the only reference level
  x_sc <- T_02_02_mark_x_fn(0, "0")
  x_sc$labels <- D_01_07_amount_fn(measure)
  x_sc$expand <- expansion(mult = c(0.06, 0.08))
  ggplot(long, aes(x = value, y = label, fill = part)) +
    T_02_02_zero_fn(h = FALSE) +
    geom_col(width = 0.72) +
    scale_fill_manual(values = fills, breaks = unname(src)) +
    x_sc +
    labs(x = D_01_11_amount_axis_fn(measure), y = NULL,
         # Folded to the half-width card
         title = D_01_04_wrap_fn(
           paste0(title, " (", D_01_13_type_fn(type), ")",
                  if (!is.null(year)) paste0(", ", year, " Table")), 50)) +
    D_01_01_theme_fn(grid = "v")
}

###### D_03_02: Round by Round #################################################
# Note: What each round adds: the spending itself (round 0, direct), then
#   suppliers (indirect) and what workers buy with their wages (induced).
#   Lines: running totals. measure: which of B_03_03 the rounds count, so
#   a student can watch wages or imports build up, not only output. ref:
#   the same rounds table at the worked example's own settings,
#   list(rounds, type), or NULL; the running totals are ghosted, the bars
#   are not.

D_03_02_rounds_plot_fn <- function(rounds, type, measure = "output",
                                   year = NULL, ref = NULL) {
  pal <- B_03_01_palette_vec
  eff <- B_03_05_effects_vec
  bars <- data.frame(
    round = rep(rounds$rnd_round_n, 2),
    part  = rep(c("t1", "induced"), each = nrow(rounds)),
    value = c(rounds$rnd_t1_amt, rounds$rnd_induced_amt)
  )
  bars$part <- ifelse(bars$part == "induced", eff[["induced"]],
                      ifelse(bars$round == 0, eff[["direct"]],
                             eff[["indirect"]]))
  keep <- switch(type, t1 = unname(eff[1:2]), unname(eff))
  bars <- bars[bars$part %in% keep, ]
  bars$part <- factor(bars$part, levels = rev(unname(eff)))
  lines <- data.frame(round = rounds$rnd_round_n,
                      value = rounds$rnd_cum_t1_amt,
                      series = "Total, Type I")
  if (type == "t2") {
    lines <- rbind(lines, data.frame(round = rounds$rnd_round_n,
                                     value = rounds$rnd_cum_t2_amt,
                                     series = "Total, Type II"))
  }
  fills <- stats::setNames(pal[c("navy", "blue", "green")], eff)
  cols  <- c(`Total, Type I` = pal[["blue"]],
             `Total, Type II` = pal[["green"]])
  # The value is read off the y axis here
  y_sc <- T_02_02_mark_y_fn(0, "0")
  y_sc$labels <- D_01_07_amount_fn(measure)
  y_sc$expand <- expansion(mult = c(0.04, 0.08))

  # Ghost: the running totals at the reference settings, one per live line
  ghost_lyr <- if (T_02_03b_ghost_off_fn(list(rounds = rounds, type = type),
                                         ref)) {
    NULL
  } else {
    g <- ref$rounds
    c(list(T_02_03a_ghost_line_fn(
             data.frame(x = g$rnd_round_n, y = g$rnd_cum_t1_amt),
             aes(x = x, y = y), colour = cols[["Total, Type I"]])),
      if (identical(type, "t2")) {
        list(T_02_03a_ghost_line_fn(
          data.frame(x = g$rnd_round_n, y = g$rnd_cum_t2_amt),
          aes(x = x, y = y), colour = cols[["Total, Type II"]]))
      })
  }

  ggplot() +
    T_02_02_zero_fn(v = FALSE) +
    geom_col(data = bars, aes(x = round, y = value, fill = part),
             width = 0.6) +
    ghost_lyr +
    geom_line(data = lines, aes(x = round, y = value, colour = series),
              linewidth = 1) +
    geom_point(data = lines, aes(x = round, y = value, colour = series),
               size = 2.2) +
    scale_fill_manual(values = fills, breaks = keep) +
    scale_colour_manual(values = cols) +
    scale_x_continuous(breaks = rounds$rnd_round_n) +
    y_sc +
    labs(x = D_01_09_bold_fn(
           "Term of the series, not a year: 0 is the spending itself"),
         y = D_01_11_amount_axis_fn(measure),
         title = D_01_04_wrap_fn(
           paste0("Round by Round: I + A + A^2 + ...",
                  if (!is.null(year)) paste0(", ", year, " Table")), 50),
         caption = paste(
           "Rounds are the terms of (I - A)^-1 = I + A + A^2 + ..., not time",
           "periods. The input-output model is static: A is one year's",
           "average input requirements read off that year's accounts, and the",
           "multiplier is the fixed point of a simultaneous system, so every",
           "round sits inside the one year of the table. Supply chains do take",
           "time, so whatever lands within twelve months is smaller than the",
           "multiplier here, but the table carries no timing information and",
           "cannot say how much smaller.")) +
    D_01_01_theme_fn(grid = "h") +
    guides(fill = guide_legend(order = 1), colour = guide_legend(order = 2))
}

#### D_04: Input Coefficients ##################################################
# Note: The technical coefficients at any level of detail, and the inputs
#   of one product.

###### D_04_01: Coefficient Heatmap ############################################
# Note: Cell (row, column): cents of input from the row per euro of the
#   column's output. The chosen product's row and column are outlined.
#   Numbers are printed when the grid is small enough to read them.

D_04_01_heatmap_fn <- function(a_g, labels, highlight, year = NULL) {
  pal  <- B_03_01_palette_vec
  k    <- nrow(a_g)
  # Past 25 rows the labels are classification codes; the caption says so
  code_only <- k > 25
  labs <- if (code_only) rownames(a_g) else
    D_01_04_wrap_fn(labels[rownames(a_g)], 30)
  txt_size <- if (k > 45) 8.5 else if (code_only) 9.5 else 10
  long <- data.frame(
    from  = factor(rep(labs, times = k), levels = rev(labs)),
    to    = factor(rep(labs, each = k), levels = labs),
    cents = as.vector(a_g) * 100
  )
  h   <- which(rownames(a_g) == highlight)
  plt <- ggplot(long, aes(x = to, y = from, fill = cents)) +
    geom_tile(colour = "white", linewidth = if (k > 45) 0.1 else 0.4)
  if (k <= 45) {
    plt <- plt + geom_text(aes(label = ifelse(cents >= 1, round(cents), "")),
                           size = if (k > 25) 2.4 else 3,
                           colour = pal[["ink"]])
  }
  if (length(h) == 1) {
    plt <- plt +
      annotate("rect", xmin = h - 0.5, xmax = h + 0.5, ymin = 0.5,
               ymax = k + 0.5, fill = NA, colour = pal[["navy"]],
               linewidth = 0.8) +
      annotate("rect", xmin = 0.5, xmax = k + 0.5, ymin = k - h + 0.5,
               ymax = k - h + 1.5, fill = NA, colour = pal[["green"]],
               linewidth = 0.8)
  }
  # Two discrete axes: no grid, no zero, no benchmark
  plt +
    scale_fill_gradient(low = "white", high = pal[["blue"]],
                        name = "Cents per Euro of Output",
                        limits = c(0, NA), trans = "sqrt") +
    scale_x_discrete(position = "top") +
    labs(x = D_01_09_bold_fn("Buyer (column)"),
         y = D_01_09_bold_fn("Seller (row)"),
         title = paste0("Technical Coefficients (A)",
                        if (!is.null(year)) paste0(", ", year, " Table")),
         caption = paste(
           "Cell: cents of input from the row per euro of the column's",
           "output.",
           if (code_only) paste(
             "Rows and columns are classification codes at this level of",
             "detail, because the full names cannot be set large enough to",
             "read; they are listed in full on the Classification tab."))) +
    D_01_01_theme_fn(base_size = 12, grid = "none") +
    theme(legend.position = "bottom", legend.title = element_text(size = 10),
          legend.key.width = grid::unit(2.5, "lines"),
          axis.text = element_text(size = txt_size),
          axis.text.x.top = element_text(angle = 90, hjust = 0, vjust = 0.5,
                                         size = txt_size))
}

###### D_04_02: Inputs of One Product ##########################################
# Note: The product's column of A: its biggest domestic suppliers, then its
#   imports and value added, per euro of output.

D_04_02_suppliers_fn <- function(lt, labels, j, top_n, year = NULL) {
  pal <- B_03_01_palette_vec
  col <- lt$a[, j]
  ord <- order(-col)
  top <- utils::head(ord[col[ord] > 0], top_n)
  df  <- rbind(
    data.frame(label = labels[top], cents = col[top] * 100,
               kind = "Irish Suppliers"),
    data.frame(label = c("Imported Inputs", "Value Added (Irish Income)",
                         "Taxes on Products"),
               cents = c(lt$coef$imports[j], lt$coef$gva[j],
                         lt$coef$prodtax[j]) * 100,
               kind = c("Imports", "Value Added", "Taxes"))
  )
  df$label <- factor(D_01_04_wrap_fn(df$label),
                     levels = rev(D_01_04_wrap_fn(df$label)))
  x_sc <- T_02_02_mark_x_fn(0, "0")
  x_sc$expand <- expansion(mult = c(0.02, 0.12))
  ggplot(df, aes(x = cents, y = label, fill = kind)) +
    T_02_02_zero_fn(h = FALSE) +
    geom_col(width = 0.72) +
    geom_text(aes(label = formatC(cents, format = "f", digits = 1)),
              hjust = -0.15, size = 3.2) +
    scale_fill_manual(values = c(`Irish Suppliers` = pal[["light"]],
                                 Imports = pal[["grey"]],
                                 `Value Added` = pal[["green"]],
                                 Taxes = pal[["tint"]])) +
    x_sc +
    labs(x = D_01_09_bold_fn("Cents per euro of output"), y = NULL,
         title = D_01_04_wrap_fn(
           paste0("Inputs per Euro of ", labels[j],
                  if (!is.null(year)) paste0(", ", year, " Table")), 90)) +
    D_01_01_theme_fn(grid = "v")
}

#### D_05: Trends across the Years #############################################
# Note: One product, and its division or section, across the years.

###### D_05_01: One Product over Time ##########################################
# Note: The chosen product (filled) or the closest product in years that
#   group products differently (hollow), against the output-weighted
#   average of all products (dashed). one: year, value, exact; avg: year,
#   value. ref: the same pair at the worked example's own settings,
#   list(one, avg), or NULL; both series are ghosted.

D_05_01_time_product_fn <- function(one, avg, name, measure, years,
                                    ref = NULL) {
  pal <- B_03_01_palette_vec
  # Ghost: the same two lines at the reference settings, drawn first
  ghost_lyr <- if (T_02_03b_ghost_off_fn(list(one = one, avg = avg), ref)) {
    NULL
  } else {
    list(
      T_02_03a_ghost_line_fn(ref$avg, aes(x = year, y = value),
                             colour = pal[["muted"]], linewidth = 0.5),
      T_02_03a_ghost_line_fn(ref$one, aes(x = year, y = value),
                             colour = pal[["blue"]], linewidth = 1.1)
    )
  }
  one$kind <- ifelse(one$exact, "This Product",
                     "Closest Product That Year")
  avg$kind <- "Average of All Products"
  # One for output, zero for the other measures
  bench <- D_01_12_bench_fn(measure)
  y_sc  <- T_02_02_mark_y_fn(if (bench) 1 else 0,
                             if (bench) "1" else "0")
  yrs   <- sort(as.integer(years))
  ggplot() +
    (if (bench) T_02_02_rest_fn(h = 1) else T_02_02_zero_fn(v = FALSE)) +
    ghost_lyr +
    geom_line(data = avg, aes(x = year, y = value, linetype = kind),
              colour = pal[["muted"]]) +
    geom_point(data = avg, aes(x = year, y = value), colour = pal[["muted"]],
               size = 1.8) +
    geom_line(data = one, aes(x = year, y = value), colour = pal[["blue"]],
              linewidth = 1.1) +
    geom_point(data = one, aes(x = year, y = value, fill = kind),
               shape = 21, colour = pal[["blue"]], size = 3.4, stroke = 1.2) +
    scale_fill_manual(values = c(`This Product` = pal[["blue"]],
                                 `Closest Product That Year` = "white"),
                      breaks = c("This Product",
                                 "Closest Product That Year")) +
    scale_linetype_manual(values = c(`Average of All Products` = "dashed")) +
    scale_x_continuous(breaks = yrs) +
    y_sc +
    labs(x = D_01_09_bold_fn("Year of the table"),
         y = D_01_10_mult_axis_fn(measure),
         title = D_01_04_wrap_fn(
           paste0(name, ": ", B_03_03_measures_lst[[measure]]$label,
                  " Multiplier, ", min(yrs), " to ", max(yrs),
                  " Tables"), 50),
         # Names the product a hollow marker stands in for, when known
         caption = paste(
           if (any(!one$exact) && !is.null(one$cso_short_str)) {
             sub <- !one$exact
             paste0("Hollow markers are not this product: the CSO regrouped ",
                    "its categories between releases, so the closest match ",
                    "that year is shown instead - ",
                    paste0(one$cso_short_str[sub], " (", one$year[sub], ")",
                           collapse = "; "), ".")
           },
           "Each table is a snapshot of the economy in that year's own",
           "prices and classification, so the levels are not strictly",
           "comparable across years; the shape of the series is the thing",
           "to read.")) +
    D_01_01_theme_fn(grid = "h") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

###### D_05_02: Groups over Time ###############################################
# Note: Every group (or section) in grey, the chosen one in blue. T
#   (households as employers) and U (extraterritorial bodies) are left out
#   unless chosen: their tiny output makes them outliers. ref: list(df,
#   highlight) at the worked example's own settings, or NULL; only the
#   chosen group's line is ghosted.

D_05_02_time_group_fn <- function(df, highlight, title, measure, ref = NULL) {
  pal <- B_03_01_palette_vec
  # The same line pulled out of the live and the reference frame
  pick_fn <- function(d, key) {
    d <- d[d$cso_group_cat == key, ]
    data.frame(year = d$cso_year_int, value = d$value)
  }
  ref_df <- if (is.null(ref)) NULL else pick_fn(ref$df, ref$highlight)
  ghost_lyr <- if (is.null(ref_df) || nrow(ref_df) == 0 ||
                   T_02_03b_ghost_off_fn(pick_fn(df, highlight), ref_df)) {
    NULL
  } else {
    T_02_03a_ghost_line_fn(ref_df, aes(x = year, y = value),
                           colour = pal[["blue"]], linewidth = 1.3)
  }
  df$year <- df$cso_year_int
  df <- df[!df$cso_group_cat %in% setdiff(c("T", "U"), highlight), ]
  hl  <- df[df$cso_group_cat == highlight, ]
  df$kind <- "Other Groups"
  hl$kind <- "Chosen Group"
  bench <- D_01_12_bench_fn(measure)
  y_sc  <- T_02_02_mark_y_fn(if (bench) 1 else 0,
                             if (bench) "1" else "0")
  yrs   <- sort(unique(df$year))
  plt <- ggplot(df, aes(x = year, y = value, group = cso_group_cat)) +
    (if (bench) T_02_02_rest_fn(h = 1) else T_02_02_zero_fn(v = FALSE)) +
    ghost_lyr +
    geom_line(aes(colour = kind), linewidth = 0.35, alpha = 0.6) +
    geom_point(aes(colour = kind), size = 1, alpha = 0.6)
  if (nrow(hl) > 0) {
    plt <- plt +
      geom_line(data = hl, aes(colour = kind), linewidth = 1.3) +
      geom_point(data = hl, aes(colour = kind), size = 3)
  }
  plt +
    scale_colour_manual(values = c(`Chosen Group` = pal[["blue"]],
                                   `Other Groups` = pal[["grey"]]),
                        breaks = c("Chosen Group", "Other Groups")) +
    scale_x_continuous(breaks = yrs) +
    y_sc +
    labs(x = D_01_09_bold_fn("Year of the table"),
         y = D_01_10_mult_axis_fn(measure),
         title = D_01_04_wrap_fn(
           paste0(title, ": ", B_03_03_measures_lst[[measure]]$label,
                  " Multiplier, ", min(yrs), " to ", max(yrs),
                  " Tables"), 50)) +
    D_01_01_theme_fn(grid = "h") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

#### D_06: Cost a Project ######################################################
# Note: Effects by measure and source, and the supply-side comparison.

###### D_06_01: Effects by Measure #############################################
# Note: One bar per measure (EUR million), stacked by source.

D_06_01_project_plot_fn <- function(eff, type, year = NULL) {
  pal <- B_03_01_palette_vec
  shown <- B_03_08_project_rows_vec
  eff <- eff[match(names(shown), eff$prj_measure_str), ]
  eff$label <- factor(shown, levels = rev(shown))
  src <- D_01_03_sources_fn(type)
  cols <- c(direct = "prj_direct_amt", indirect = "prj_indirect_amt",
            induced = "prj_induced_amt")
  long <- do.call(rbind, lapply(names(src), function(p) {
    data.frame(label = eff$label, part = src[[p]], value = eff[[cols[[p]]]])
  }))
  long$part <- factor(long$part, levels = rev(unname(src)))
  fills <- stats::setNames(pal[c("navy", "blue", "green")],
                           B_03_05_effects_vec)
  x_sc <- T_02_02_mark_x_fn(0, "0")
  x_sc$labels <- function(v) D_01_02_eur_fn(v)
  x_sc$expand <- expansion(mult = c(0.02, 0.18))
  ggplot(long, aes(x = value, y = label, fill = part)) +
    T_02_02_zero_fn(h = FALSE) +
    geom_col(width = 0.7) +
    geom_text(data = eff, aes(x = pmax(prj_total_amt, 0), y = label,
                              label = D_01_02_eur_fn(prj_total_amt, 1)),
              inherit.aes = FALSE, hjust = -0.1, size = 3.3) +
    scale_fill_manual(values = fills, breaks = unname(src)) +
    x_sc +
    labs(x = D_01_09_bold_fn("Effect of the project (euro million)"),
         y = NULL,
         title = D_01_04_wrap_fn(
           paste0("Effects of the Project (", D_01_13_type_fn(type), ")",
                  if (!is.null(year)) paste0(", ", year, " Table")), 56),
         caption = paste(
           "The rows are nested, not additive. Gross value added is part of",
           "gross output, and compensation of employees and gross operating",
           "surplus are parts of gross value added. Imports are a leakage and",
           "are never part of Irish output. The Exchequer return is drawn from",
           "the wages, profits and product taxes above, not additional to",
           "them. Gross value added is the contribution to GDP; gross output",
           "counts intermediate sales at every stage they pass through.")) +
    D_01_01_theme_fn(grid = "v")
}

###### D_06_02: Supply Constraints #############################################
# Note: Standard input-output effects against the same effects with
#   capacity limits and with displacement, for the main measures.

D_06_02_constraint_plot_fn <- function(con, year = NULL) {
  pal <- B_03_01_palette_vec
  # Row names as in D_06_01, without the "of which" prefixes
  shown <- B_03_07_project_measures_vec[c("output", "gva", "wages",
                                          "exchequer")]
  shown[["output"]] <- "Gross Output"
  shown[["gva"]]    <- "Gross Value Added"
  con <- con[match(names(shown), con$prj_measure_str), ]

  # The capacity series is drawn only when a limit binds
  binds <- any(abs(con$prj_capacity_amt - con$prj_standard_amt) > 1e-9)
  cases <- c("Standard Input-Output",
             if (binds) "With Capacity Limits",
             "Net of Displacement")
  vals  <- c(con$prj_standard_amt,
             if (binds) con$prj_capacity_amt,
             con$prj_net_amt)

  long <- data.frame(
    label = factor(rep(shown, length(cases)), levels = rev(shown)),
    case  = factor(rep(cases, each = length(shown)), levels = rev(cases)),
    value = vals
  )
  x_sc <- T_02_02_mark_x_fn(0, "0")
  x_sc$labels <- function(v) D_01_02_eur_fn(v)
  x_sc$expand <- expansion(mult = c(0.02, 0.2))
  ggplot(long, aes(x = value, y = label, fill = case)) +
    T_02_02_zero_fn(h = FALSE) +
    geom_col(position = position_dodge(width = 0.8), width = 0.75) +
    geom_text(aes(x = pmax(value, 0), label = D_01_02_eur_fn(value, 1)),
              position = position_dodge(width = 0.8), hjust = -0.1,
              size = 3) +
    scale_fill_manual(values = c(`Standard Input-Output` = pal[["blue"]],
                                 `With Capacity Limits` = pal[["light"]],
                                 `Net of Displacement` = pal[["green"]]),
                      breaks = cases, drop = TRUE) +
    guides(fill = guide_legend(nrow = if (binds) 2 else 1, byrow = TRUE)) +
    x_sc +
    labs(x = D_01_09_bold_fn("Effect of the project (euro million)"),
         y = NULL,
         title = D_01_04_wrap_fn(
           paste0("Effects with Supply Constraints",
                  if (!is.null(year)) paste0(", ", year, " Table")), 56)) +
    D_01_01_theme_fn(grid = "v")
}

#--------------------------------- Script End ---------------------------------#
