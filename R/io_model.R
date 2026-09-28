################################################################################
## Project: ECON42550 Macroeconomics                                          ##
## Input-Output Model: Leontief Inverse, Multipliers and Projects             ##
################################################################################

## Author:      Sam Deegan
## Affiliation: University College Dublin
## Email:       sam.deegan@ucdconnect.ie

## Usage:
##   Sourced automatically by app.R. Can be sourced alone from a lecture
##   .qmd so slide figures come from the same numbers:
##     source("R/io_model.R")
##     io <- C_01_01_read_data_fn("data")
##
## Inputs:
##   data/io_products.csv  one row per product and year: NACE key, output,
##                         inputs, final demand
##   data/io_flows.csv     domestic flows, long format (year, from, to)
##   data/io_meta.csv      household totals and tax rates that close the
##                         model, one row per key and year
##
## Outputs:
##   C_01_* functions returning matrices and data frames. Nothing on disk.
##
## The model (Miller and Blair 2009, ch. 2 and 6 notation):
##   x = A x + f,  A = Z diag(x)^-1   =>   x = L f,  L = (I - A)^-1
##   L = I + A + A^2 + ...  (each power is one more round of suppliers)
##   Type II: households become a sector. They sell labour (wages per euro
##   of output, in the row) and buy products (household spending shares,
##   column), so L2 = (I - A2)^-1 adds the induced round: what the workers
##   along the chain buy with their wages.
##   Effects of EUR 1 of final demand, for any coefficient vector c (wages,
##   profits, taxes, imports per euro of output):
##     direct = c,   Type I = c' L,   Type II = c' L2 + (leak) x_h,
##   where x_h is the household income the demand creates and the leak is
##   the share of household spending that goes on imports or product taxes.
##   The wage bill is taken gross, so the induced effect is an upper bound.
##   Indirect = Type I - direct; induced = Type II - Type I.
##   With Type I, wages + operating surplus + net production taxes +
##   product taxes + imports add up to exactly EUR 1.
##   Capacity limits: sectors that cannot grow beyond a cap are fixed at the
##   cap and the rest of the system is solved around them (the mixed model
##   of Miller and Blair, ch. 13); demand they cannot meet is imported.
##   Reproduces the CSO's published output multipliers to within their
##   rounding.
##
## References:
##   Miller, R. E. and Blair, P. D. (2009). Input-Output Analysis:
##     Foundations and Extensions, 2nd ed. Ch. 2 (the Leontief inverse),
##     ch. 6 (multipliers, Type II closure), ch. 13 (mixed models).
##   CSO. Supply and Use and Input-Output Tables for Ireland, 1998 to 2022.

#-------------------------------- Script Begin --------------------------------#

################################################################################
## A: Table of Contents ########################################################
################################################################################
# Note: C_01 holds the model. The app (app.R) holds B, C_02, E, F and G;
#   R/io_plots.R holds D.
#
#   C: Model
#     C_01_01  Read the data
#     C_01_02  Coefficients and Leontief inverses
#     C_01_03  Effects of any final demand, by measure
#     C_01_04  Multipliers by product
#     C_01_05  Effects of a demand shock, by product
#     C_01_06  Round-by-round build-up (Type I and II)
#     C_01_07  Groups of each product
#     C_01_08  Multipliers and coefficients by group
#     C_01_09  Everything for one year
#     C_01_10  Multipliers across years
#     C_01_11  Output with capacity limits
#     C_01_12  A costed project
#     C_01_13  NACE divisions of a product
#     C_01_14  Span of NACE divisions

################################################################################
## C: Model ####################################################################
################################################################################
# Note: Plain base R; the matrices are at most 63 x 63, so it is instant.

#### C_01: Model Functions #####################################################
# Note: All money amounts are EUR million at basic prices of each year.

###### C_01_01: Read the Data ##################################################
# Note: Returns one element per year (named by year, oldest first), each a
#   list of products (data frame), flows (matrix Z, rows sell to columns)
#   and meta (named list of household totals and source).

C_01_01_read_data_fn <- function(data_dir) {
  prd_all <- utils::read.csv(file.path(data_dir, "io_products.csv"),
                             stringsAsFactors = FALSE,
                             colClasses = c(cso_key_str = "character",
                                            cso_nace_str = "character",
                                            cso_match_str = "character"),
                             fileEncoding = "UTF-8")
  flw_all <- utils::read.csv(file.path(data_dir, "io_flows.csv"))
  met_all <- utils::read.csv(file.path(data_dir, "io_meta.csv"),
                             stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  years <- sort(unique(prd_all$cso_year_int))
  out <- lapply(years, function(yr) {
    prd <- prd_all[prd_all$cso_year_int == yr, ]
    prd <- prd[order(prd$cso_order_id), ]
    rownames(prd) <- NULL
    flw <- flw_all[flw_all$cso_year_int == yr, ]
    met <- met_all[met_all$cso_year_int == yr, ]
    n   <- nrow(prd)
    z   <- matrix(0, n, n)
    z[cbind(flw$cso_from_id, flw$cso_to_id)] <- flw$cso_flow_amt
    meta <- stats::setNames(as.list(met$cso_value_str), met$cso_key_str)
    list(year = yr, products = prd, flows = z, meta = meta)
  })
  stats::setNames(out, years)
}

###### C_01_02: Coefficients and Leontief Inverses #############################
# Note: a is input per euro of output and l the Type I inverse. a2 and l2f
#   close the model with households as an extra row (gross wages per euro
#   of output) and column (consumption shares of total household spending,
#   imports and taxes included): the Type II closure of Miller and Blair
#   (2009, ch. 6). Households spend the whole wage bill, so whatever is
#   taxed away is taken to be spent again in the same pattern. coef holds
#   every per-euro coefficient vector (jobs: FTE job-years per EUR 1
#   million of output; exchequer: taxes on products and production plus
#   taxes on wages and profits at the average rates given); leak the
#   shares of household spending that go on imports and product taxes.

C_01_02_leontief_fn <- function(io, rates = c(wage = 0, profit = 0)) {
  prd <- io$products
  x   <- prd$cso_output_amt
  n   <- length(x)
  a   <- sweep(io$flows, 2, x, "/")
  l   <- solve(diag(n) - a)

  hh_tot  <- as.numeric(io$meta$hh_total_amt)
  hh_col  <- prd$cso_hh_amt / hh_tot
  lab_row <- prd$cso_coe_amt / x
  a2      <- rbind(cbind(a, hh_col), c(lab_row, 0))
  l2f     <- solve(diag(n + 1) - a2)

  prodtax <- prd$cso_prodtax_amt / x
  prodn   <- prd$cso_nonprodtax_amt / x
  coef <- list(
    output  = rep(1, n),
    gva     = prd$cso_va_amt / x,
    wages   = lab_row,
    gos     = (prd$cso_surplus_amt + prd$cso_cfc_amt) / x,
    nos     = prd$cso_surplus_amt / x,
    prodn   = prodn,
    prodtax = prodtax,
    taxes   = prodtax + prodn,
    imports = prd$cso_imports_amt / x,
    jobs    = if (is.null(prd$cso_fte_amt)) rep(0, n) else {
      ifelse(is.na(prd$cso_fte_amt), 0, prd$cso_fte_amt / x)
    }
  )
  coef$exchequer <- coef$taxes + rates[["wage"]] / 100 * coef$wages +
    rates[["profit"]] / 100 * coef$nos

  leak <- c(imports = as.numeric(io$meta$hh_imports_amt) / hh_tot,
            prodtax = as.numeric(io$meta$hh_prodtax_amt) / hh_tot)
  leak[["taxes"]]     <- leak[["prodtax"]]
  leak[["exchequer"]] <- leak[["prodtax"]]

  list(a = a, l = l, a2 = a2, l2f = l2f, l2 = l2f[seq_len(n), seq_len(n)],
       coef = coef, leak = leak, n = n, rates = rates)
}

###### C_01_03: Effects of Any Final Demand ####################################
# Note: For a final-demand vector f (EUR million, one entry per product) or
#   a matrix of them (one column each), the direct, Type I and Type II
#   effect on one measure. Returns a list of three vectors (one entry per
#   column of f).

C_01_03_effects_fn <- function(lt, f, measure) {
  f   <- as.matrix(f)
  cf  <- lt$coef[[measure]]
  lk  <- if (measure %in% names(lt$leak)) lt$leak[[measure]] else 0
  n   <- lt$n
  hh  <- as.vector(lt$l2f[n + 1, seq_len(n), drop = FALSE] %*% f)
  list(direct = as.vector(cf %*% f),
       t1     = as.vector(cf %*% lt$l %*% f),
       t2     = as.vector(cf %*% lt$l2 %*% f) + lk * hh)
}

###### C_01_04: Multipliers by Product #########################################
# Note: One row per product: effects of one euro of final demand for it,
#   for every measure (direct, Type I, Type II), plus the first round of
#   suppliers (column sum of A) for the narration.

C_01_04_multipliers_fn <- function(io, lt, measures) {
  prd <- io$products
  eye <- diag(lt$n)
  out <- data.frame(
    cso_year_int     = prd$cso_year_int,
    cso_order_id     = prd$cso_order_id,
    cso_key_str      = prd$cso_key_str,
    cso_match_str    = prd$cso_match_str,
    cso_short_str    = prd$cso_short_str,
    cso_section_cat  = prd$cso_section_cat,
    cso_output_amt   = prd$cso_output_amt,
    mlt_first_val    = colSums(lt$a),
    stringsAsFactors = FALSE
  )
  for (m in measures) {
    e <- C_01_03_effects_fn(lt, eye, m)
    out[[paste0("mlt_", m, "_direct_val")]] <- e$direct
    out[[paste0("mlt_", m, "_t1_val")]]     <- e$t1
    out[[paste0("mlt_", m, "_t2_val")]]     <- e$t2
  }
  out
}

###### C_01_05: Effects of a Demand Shock ######################################
# Note: By product, split into direct (the spending itself), indirect
#   (supply chain) and induced (what workers buy with their wages), plus
#   the first round of suppliers for the narration. cf: a per-euro
#   coefficient vector (C_01_02), so the same split can be read in wages,
#   imports, taxes or FTE job-years instead of output; the default (one
#   per product) gives output. The household leak of Type II (spending
#   that goes straight abroad or to VAT) is not produced by any Irish
#   product, so it is not in these rows: use C_01_03 for the total.

C_01_05_shock_fn <- function(lt, f, cf = NULL) {
  if (is.null(cf)) cf <- rep(1, lt$n)
  type1 <- as.vector(lt$l %*% f)
  type2 <- as.vector(lt$l2 %*% f)
  data.frame(
    shk_direct_amt   = cf * f,
    shk_first_amt    = cf * as.vector(lt$a %*% f),
    shk_indirect_amt = cf * (type1 - f),
    shk_induced_amt  = cf * (type2 - type1),
    shk_type1_amt    = cf * type1,
    shk_type2_amt    = cf * type2
  )
}

###### C_01_06: Round-by-Round Build-up ########################################
# Note: Round 0 is the spending itself; round k is A^k f, what the
#   suppliers of round k - 1 buy. With households as a sector (A2), later
#   rounds also include what workers buy with their wages (the induced
#   part). cf: a per-euro coefficient vector, as in C_01_05, so the rounds
#   can be read in wages, imports, taxes or FTE job-years; the default
#   (one per product) gives output.

C_01_06_rounds_fn <- function(lt, f, n_rounds = 10, cf = NULL) {
  n    <- lt$n
  if (is.null(cf)) cf <- rep(1, n)
  s1   <- f
  s2   <- c(f, 0)
  t1   <- numeric(n_rounds + 1)
  t2   <- numeric(n_rounds + 1)
  t1[1] <- sum(cf * s1)
  t2[1] <- sum(cf * s2[seq_len(n)])
  for (k in seq_len(n_rounds)) {
    s1 <- as.vector(lt$a %*% s1)
    s2 <- as.vector(lt$a2 %*% s2)
    t1[k + 1] <- sum(cf * s1)
    t2[k + 1] <- sum(cf * s2[seq_len(n)])
  }
  data.frame(
    rnd_round_n     = 0:n_rounds,
    rnd_t1_amt      = t1,
    rnd_induced_amt = t2 - t1,
    rnd_cum_t1_amt  = cumsum(t1),
    rnd_cum_t2_amt  = cumsum(t2)
  )
}

###### C_01_07: Groups of Each Product #########################################
# Note: The section and division of each product in one classification.
#   def: sec_breaks, sec_codes (first division and letter of each section),
#   breaks, codes, names (the same at the A*38 or A31 level); sec_names:
#   section names. On screen the A*38 level is called divisions (NACE's
#   levels are section, division, group, class: Regulation 1893/2006); the
#   column names say "group". rev = "rev2" uses the product's NACE Rev 2
#   divisions (cso_match_str), "rev1" its own Rev 1.1 divisions (the "r1:"
#   key). A product's section is the one most of its divisions fall in
#   (ties: the first; for rev2 the section in the data is kept); a product
#   spanning several divisions gets a combined code such as "CF+CI".

C_01_07_groups_fn <- function(prd, def, sec_names, rev) {
  one <- lapply(seq_len(nrow(prd)), function(i) {
    divs <- if (rev == "rev1") {
      as.integer(strsplit(sub("^r1:", "", prd$cso_key_str[i]), ",")[[1]])
    } else {
      as.integer(strsplit(prd$cso_match_str[i], ",")[[1]])
    }
    secs <- def$sec_codes[findInterval(divs, def$sec_breaks)]
    tab  <- table(factor(secs, levels = unique(secs)))
    sec  <- if (rev == "rev2" && !is.null(prd$cso_section_cat)) {
      prd$cso_section_cat[i]
    } else {
      names(tab)[which.max(tab)]
    }
    grp  <- unique(def$codes[findInterval(divs, def$breaks)])
    in_s <- grp[substr(grp, 1, 1) == sec]
    if (length(in_s) == 0) in_s <- grp[1]
    code <- paste(in_s, collapse = "+")
    nm   <- if (length(in_s) == 1) def$names[[match(in_s, def$codes)]] else
      prd$cso_short_str[i]
    c(sec = sec, code = code, name = nm)
  })
  sec <- vapply(one, `[[`, "", "sec")
  data.frame(sec_cat = sec, sec_str = unname(sec_names[sec]),
             grp_cat = vapply(one, `[[`, "", "code"),
             grp_str = vapply(one, `[[`, "", "name"),
             stringsAsFactors = FALSE)
}

###### C_01_08: By Group #######################################################
# Note: For any grouping of products (a code per product), output-weighted
#   average multipliers and the aggregated coefficient matrix: the share
#   of each buying group's output that it buys from each selling group.
#   Groups are ordered by the first product in each. Every row also
#   carries its section letter (sec) and its first NACE division (from
#   nace), so a chart can sort rows by classification instead of by size.

C_01_08_aggregate_fn <- function(io, mult_df, group, labels, sec = NULL,
                                 nace = NULL) {
  prd  <- io$products
  grps <- unique(group)
  wt   <- prd$cso_output_amt
  cols <- grep("^mlt_", names(mult_df), value = TRUE)
  if (is.null(sec)) {
    sec <- if (is.null(prd$cso_nsec_cat)) prd$cso_section_cat else
      prd$cso_nsec_cat
  }
  if (is.null(nace)) nace <- prd$cso_nace_str

  mult_g <- do.call(rbind, lapply(grps, function(g) {
    k   <- group == g
    row <- lapply(cols, function(cl) sum(mult_df[[cl]][k] * wt[k]) / sum(wt[k]))
    div <- C_01_13_divisions_fn(nace[k])
    data.frame(cso_group_cat = g, cso_label_str = labels[[g]],
               cso_sec_cat   = sec[k][1],
               cso_first_int = if (length(div) == 0) NA_integer_ else div[1],
               cso_output_amt = sum(wt[k]), stats::setNames(row, cols),
               stringsAsFactors = FALSE)
  }))

  s_mat <- sapply(grps, function(g) as.numeric(group == g))
  z_g   <- t(s_mat) %*% io$flows %*% s_mat
  x_g   <- as.vector(t(s_mat) %*% wt)
  a_g   <- sweep(z_g, 2, x_g, "/")
  dimnames(a_g) <- list(grps, grps)
  list(multipliers = mult_g, a = a_g)
}

###### C_01_09: Everything for One Year ########################################
# Note: Inverses, product multipliers and three levels of detail
#   (sections, divisions, products) in the year's own classification
#   (NACE Rev 2, or Rev 1.1 for 1998 and 2005), plus groups and sections
#   mapped to NACE Rev 2 for comparisons across years. grp_def and
#   sec_names: lists with rev2 and rev1 elements (see C_01_07); rates:
#   average tax rates on wages and profits for the exchequer measure.

C_01_09_year_fn <- function(io, measures, grp_def, sec_names,
                            rates = c(wage = 0, profit = 0)) {
  rev  <- if (identical(io$meta$classification, "NACE Rev 1.1")) "rev1" else
    "rev2"
  lt   <- C_01_02_leontief_fn(io, rates)
  mult <- C_01_04_multipliers_fn(io, lt, measures)
  nat  <- C_01_07_groups_fn(io$products, grp_def[[rev]], sec_names[[rev]],
                            rev)
  r2   <- C_01_07_groups_fn(io$products, grp_def$rev2, sec_names$rev2, "rev2")
  add  <- data.frame(cso_nsec_cat = nat$sec_cat, cso_nsec_str = nat$sec_str,
                     cso_group_cat = nat$grp_cat, cso_group_str = nat$grp_str,
                     cso_rgrp_cat = r2$grp_cat, cso_rgrp_str = r2$grp_str,
                     cso_rsec_cat = r2$sec_cat, cso_rsec_str = r2$sec_str,
                     stringsAsFactors = FALSE)
  io$products <- cbind(io$products, add)
  mult <- cbind(mult, add)
  prd  <- io$products

  lab <- function(code, name) stats::setNames(paste(code, name), code)
  span <- function(code, name, sec, nace) {
    u   <- which(!duplicated(code))
    txt <- vapply(u, function(i) {
      d <- C_01_14_span_fn(C_01_13_divisions_fn(nace[code == code[i]]))
      paste0(sec[i], if (nzchar(d)) paste0(":", d) else "", " ", name[i])
    }, "")
    stats::setNames(txt, code[u])
  }
  levels <- list(
    product = C_01_08_aggregate_fn(
      io, mult, prd$cso_key_str,
      span(prd$cso_key_str, prd$cso_short_str, prd$cso_nsec_cat,
           prd$cso_nace_str)),
    group   = C_01_08_aggregate_fn(
      io, mult, prd$cso_group_cat,
      span(prd$cso_group_cat, prd$cso_group_str, prd$cso_nsec_cat,
           prd$cso_nace_str)),
    section = C_01_08_aggregate_fn(io, mult, prd$cso_nsec_cat,
                                   lab(prd$cso_nsec_cat, prd$cso_nsec_str))
  )
  levels_r2 <- list(
    group   = C_01_08_aggregate_fn(
      io, mult, prd$cso_rgrp_cat,
      span(prd$cso_rgrp_cat, prd$cso_rgrp_str, prd$cso_rsec_cat,
           prd$cso_match_str),
      sec = prd$cso_rsec_cat, nace = prd$cso_match_str),
    section = C_01_08_aggregate_fn(io, mult, prd$cso_rsec_cat,
                                   lab(prd$cso_rsec_cat, prd$cso_rsec_str),
                                   sec = prd$cso_rsec_cat)
  )
  list(io = io, lt = lt, mult = mult, rev = rev, levels = levels,
       levels_r2 = levels_r2,
       code_of = list(product = prd$cso_key_str, group = prd$cso_group_cat,
                      section = prd$cso_nsec_cat),
       code_r2 = list(group = prd$cso_rgrp_cat, section = prd$cso_rsec_cat))
}

###### C_01_10: Multipliers across Years #######################################
# Note: Stacks product multipliers, and division and section multipliers
#   mapped to NACE Rev 2, of every year for the "Trends" tab.

C_01_10_over_time_fn <- function(res_lst) {
  stack <- function(get) {
    out <- do.call(rbind, lapply(names(res_lst), function(yr) {
      data.frame(cso_year_int = as.integer(yr), get(res_lst[[yr]]),
                 stringsAsFactors = FALSE)
    }))
    rownames(out) <- NULL
    out
  }
  list(
    products = stack(function(r) r$mult[, setdiff(names(r$mult),
                                                  "cso_year_int")]),
    group    = stack(function(r) r$levels_r2$group$multipliers),
    section  = stack(function(r) r$levels_r2$section$multipliers)
  )
}

###### C_01_11: Output with Capacity Limits ####################################
# Note: cap: most extra output each product can supply (Inf = no limit).
#   Products whose required output would exceed their cap are fixed at the
#   cap, one round of fixing at a time, and the rest of the system (with
#   households, for Type II) is solved around them. Demand the capped
#   products cannot meet is imported. Returns output by product, household
#   income and the unmet demand by product.

C_01_11_capacity_fn <- function(lt, f, cap, type = "t1") {
  n    <- lt$n
  a    <- switch(type, t2 = lt$a2, lt$a)
  ff   <- switch(type, t2 = c(f, 0), f)
  capf <- switch(type, t2 = c(cap, Inf), cap)
  m    <- length(ff)
  x    <- as.vector(solve(diag(m) - a, ff))
  fixed <- rep(FALSE, m)
  repeat {
    over <- !fixed & x > capf + 1e-9
    if (!any(over)) break
    fixed <- fixed | over
    x[fixed] <- capf[fixed]
    u <- !fixed
    if (any(u)) {
      rhs  <- ff[u] + as.vector(a[u, fixed, drop = FALSE] %*% x[fixed])
      x[u] <- as.vector(solve(diag(sum(u)) - a[u, u, drop = FALSE], rhs))
    }
  }
  need  <- as.vector(a %*% x) + ff
  short <- pmax(need - x, 0)
  list(x = x[seq_len(n)],
       hh_income = if (type == "t2") x[n + 1] else 0,
       short = short[seq_len(n)], capped = fixed[seq_len(n)])
}

###### C_01_12: A Costed Project ###############################################
# Note: lines: data frame of project spending (cso_key_str, amount_amt in
#   EUR million, irish_pct = share bought from Irish producers; the rest is
#   imported directly). opt: type ("t1"/"t2"), tax_wage and tax_profit
#   (average tax rates, per cent, on wages and net operating surplus),
#   displace (share of indirect and induced effects that only displaces
#   activity elsewhere), cap_keys and cap_pct (products at capacity and
#   their spare capacity, per cent of output). measures must include
#   taxes, wages and nos. Returns effects by measure and source (plus the
#   exchequer), output by product, the exchequer account and the effects
#   with supply constraints.

C_01_12_project_fn <- function(lt, prd, lines, opt, measures) {
  measures <- setdiff(measures, "exchequer")
  n    <- lt$n
  j    <- match(lines$cso_key_str, prd$cso_key_str)
  ok   <- !is.na(j) & is.finite(lines$amount_amt)
  dom  <- lines$amount_amt[ok] * pmin(pmax(lines$irish_pct[ok], 0), 100) / 100
  f    <- numeric(n)
  for (i in seq_along(dom)) f[j[ok][i]] <- f[j[ok][i]] + dom[i]
  direct_imp <- sum(lines$amount_amt[ok]) - sum(dom)
  t2 <- opt$type == "t2"

  # Standard effects by measure and source
  eff <- do.call(rbind, lapply(measures, function(m) {
    e   <- C_01_03_effects_fn(lt, f, m)
    add <- if (m == "imports") direct_imp else 0
    data.frame(prj_measure_str = m,
               prj_direct_amt   = e$direct + add,
               prj_indirect_amt = e$t1 - e$direct,
               prj_induced_amt  = if (t2) e$t2 - e$t1 else 0,
               stringsAsFactors = FALSE)
  }))
  tax_row <- function(d) {
    pick <- function(m) d[d$prj_measure_str == m, -1]
    out  <- pick("taxes") + opt$tax_wage / 100 * pick("wages") +
      opt$tax_profit / 100 * pick("nos")
    data.frame(prj_measure_str = "exchequer", out, stringsAsFactors = FALSE)
  }
  eff <- rbind(eff, tax_row(eff))
  eff$prj_total_amt <- eff$prj_direct_amt + eff$prj_indirect_amt +
    eff$prj_induced_amt

  # Output by product and source
  sh <- C_01_05_shock_fn(lt, f)
  by_prd <- data.frame(
    cso_key_str      = prd$cso_key_str,
    prj_direct_amt   = sh$shk_direct_amt,
    prj_indirect_amt = sh$shk_indirect_amt,
    prj_induced_amt  = if (t2) sh$shk_induced_amt else 0,
    stringsAsFactors = FALSE
  )

  # Exchequer: taxes in the tables plus assumed taxes on wages and profits
  get_tot <- function(m) eff$prj_total_amt[eff$prj_measure_str == m]
  hh  <- as.vector(lt$l2f[n + 1, seq_len(n)] %*% f)
  exq <- data.frame(
    prj_item_str = c("Taxes on products (VAT, excise)",
                     "Other taxes on production, less subsidies",
                     paste0("Tax on wages: income tax, USC, PRSI (",
                            opt$tax_wage, "%)"),
                     paste0("Tax on profits: corporation tax (",
                            opt$tax_profit, "%)")),
    prj_amount_amt = c(get_tot("prodtax"), get_tot("prodn"),
                       opt$tax_wage / 100 * get_tot("wages"),
                       opt$tax_profit / 100 * get_tot("nos")),
    stringsAsFactors = FALSE
  )

  # Supply constraints: capacity limits, then displacement
  cap <- rep(Inf, n)
  ck  <- match(opt$cap_keys, prd$cso_key_str)
  ck  <- ck[!is.na(ck)]
  cap[ck] <- prd$cso_output_amt[ck] * opt$cap_pct / 100
  cc  <- C_01_11_capacity_fn(lt, f, cap, opt$type)
  con <- vapply(measures, function(m) {
    lk <- if (m %in% names(lt$leak) && t2) lt$leak[[m]] else 0
    v  <- sum(lt$coef[[m]] * cc$x) + lk * cc$hh_income
    if (m == "imports") v <- v + direct_imp + sum(cc$short)
    v
  }, 0)
  dir_con <- vapply(measures, function(m) {
    v <- sum(lt$coef[[m]] * pmin(f, cc$x))
    if (m == "imports") v <- v + direct_imp
    v
  }, 0)
  net <- dir_con + (1 - opt$displace / 100) * (con - dir_con)
  exq_of <- function(v) {
    v[["taxes"]] + opt$tax_wage / 100 * v[["wages"]] +
      opt$tax_profit / 100 * v[["nos"]]
  }
  constrained <- data.frame(
    prj_measure_str  = c(measures, "exchequer"),
    prj_standard_amt = eff$prj_total_amt[match(c(measures, "exchequer"),
                                               eff$prj_measure_str)],
    prj_capacity_amt = unname(c(con, exq_of(con))),
    prj_net_amt      = unname(c(net, exq_of(net))),
    stringsAsFactors = FALSE
  )

  list(f = f, cost = sum(lines$amount_amt[ok]), direct_imports = direct_imp,
       effects = eff, by_product = by_prd, exchequer = exq,
       household_income = if (t2) hh else 0,
       constrained = constrained, capped = prd$cso_key_str[cc$capped],
       unmet = sum(cc$short))
}

###### C_01_13: NACE Divisions of a Product ####################################
# Note: Turns the CSO's division strings ("10", "11,12", "5-9") into the
#   sorted set of division numbers they cover. Pass every product of a
#   group to get the divisions that group spans.

C_01_13_divisions_fn <- function(nace) {
  txt <- as.character(nace)
  txt <- txt[!is.na(txt)]
  bit <- trimws(unlist(strsplit(txt, ",")))
  bit <- bit[nzchar(bit)]
  out <- unlist(lapply(bit, function(p) {
    ends <- suppressWarnings(as.integer(strsplit(p, "[-\u2013]")[[1]]))
    ends <- ends[!is.na(ends)]
    if (length(ends) == 0) integer(0) else seq(ends[1], ends[length(ends)])
  }))
  sort(unique(out))
}

###### C_01_14: Span of NACE Divisions #########################################
# Note: The divisions a group covers, as compact ranges with an en dash:
#   10, 11, 12 becomes "10-12"; 21 and 26 stay "21, 26". An empty set
#   gives "", so the caller can leave the colon out.

C_01_14_span_fn <- function(divs) {
  if (length(divs) == 0) return("")
  brk  <- cumsum(c(1, diff(divs) != 1))
  runs <- vapply(split(divs, factor(brk, levels = unique(brk))), function(r) {
    if (length(r) == 1) as.character(r[1]) else {
      # Escape, not the character; see CONVENTIONS.md 6
      paste0(r[1], "\u2013", r[length(r)])
    }
  }, "")
  paste(runs, collapse = ", ")
}

#--------------------------------- Script End ---------------------------------#
