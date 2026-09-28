################################################################################
## Project: ECON42550 Macroeconomics                                          ##
## Input-Output Multipliers: Interactive Shiny App                            ##
################################################################################

## Author:      Sam Deegan
## Affiliation: University College Dublin
## Email:       sam.deegan@ucdconnect.ie

## Usage:
##   Open app.R in RStudio and click Run App, or from this folder:
##     shiny::runApp()
##   Needs R 4.1 or later with shiny, bslib, ggplot2, readxl and writexl
##   installed. A hosted copy runs in the browser at
##   https://sam-deegan.com/toy-models/io-multiplier/
##   Two modes, switched at the top of the sidebar:
##     Explore the Model  multipliers by source, or where each euro of
##                        demand ends up; a demand shock, round by round;
##                        trends across the years; the input coefficients
##     Cost a Project     one costing, with taxes, supply constraints and
##                        an Excel download
##   Three levels of detail: sections (NACE A*21), divisions (NACE A*38)
##   and the CSO's own product categories (48 to 62 a year). 1998 and 2005
##   use NACE Rev 1.1, mapped to the nearest Rev 2 division.
##   All text (worked examples, prompts, help, templates) lives in B_03.
##
## Inputs:
##   data/io_products.csv, data/io_flows.csv, data/io_meta.csv: the CSO's
##   symmetric input-output tables of domestic product flows for 1998,
##   2005, 2010, 2011, 2015, 2020, 2021 and 2022. R/io_model.R (the model),
##   R/io_plots.R (the figures) and R/toolkit.R (shared layout and
##   helpers), all sourced automatically by Shiny. www/ holds the logo.
##
## Outputs:
##   Only the Excel file a user downloads from "Cost a Project".
##
## Packages:
##   shiny, bslib, ggplot2, readxl (uploads), writexl (downloads).
##
## Version:
##   B_03_27_version_chr; history in CHANGELOG.md; git tag vX.Y.Z.
##
## References:
##   Miller, R. E. and Blair, P. D. (2009). Input-Output Analysis:
##     Foundations and Extensions, 2nd ed. Ch. 2 (the Leontief inverse),
##     ch. 6 (multipliers, Type II closure), ch. 13 (mixed models).
##   CSO. Supply and Use and Input-Output Tables for Ireland, 1998 to 2022.
##   Eurostat. nama_10_a64_e (employment) and gov_10a_taxag (tax receipts).
##   Regulation (EC) No 1893/2006 (NACE Rev 2).

#-------------------------------- Script Begin --------------------------------#

################################################################################
## A: Table of Contents ########################################################
################################################################################
# Note: C_01 (the model) lives in R/io_model.R and D (plots) in
#   R/io_plots.R, so the slides can reuse them.
#
#   B: Setup
#     B_01  Packages, model and plots
#     B_02  Settings
#     B_03  Soft-coded objects
#     B_04  Paths
#   C: Model (C_01 in R/io_model.R; C_02 precomputed results here)
#   D: Plots (R/io_plots.R)
#   E: User Interface
#     E_01  Theme and styles
#     E_02  Helpers and sidebar
#     E_03  Tabs and page
#   F: Server
#   G: Run

################################################################################
## B: Setup ####################################################################
################################################################################
# Note: Packages, settings and every value you might want to change.

#### B_01: Packages, Model and Plots ###########################################
# Note: Shiny, bslib and ggplot2 for the app; readxl and writexl for
#   project uploads and downloads.

###### B_01_01: Load Packages ##################################################
# Note: All on CRAN. readxl and writexl are called with :: when needed.

library(shiny)
library(bslib)
library(ggplot2)

###### B_01_02: Load the Model and Plots #######################################
# Note: Sourced into the app's own environment as well, so the plot
#   functions can see the palette and labels in B_03.

source(file.path("R", "io_model.R"), local = TRUE)
source(file.path("R", "io_plots.R"), local = TRUE)

#### B_02: Settings ############################################################
# Note: Standard options.

###### B_02_01: Global Options #################################################
# Note: No scientific notation; three digits in the console.

options(scipen = 999, digits = 3)

###### B_02_02: Seed ###########################################################
# Note: Nothing here is random; kept for consistency.

set.seed(42)

#### B_03: Soft-Coded Objects ##################################################
# Note: Palette, names, measures, defaults, text, templates and credits.

###### B_03_01: Palette ########################################################
# Note: Dublin Beamer theme colours (dublin-theme.tex). Direct effects
#   navy, indirect blue, induced green.

B_03_01_palette_vec <- c(
  navy  = "#04204C",
  blue  = "#0056A4",
  light = "#9FC4E0",
  green = "#61B77C",
  tint  = "#C4E3CE",
  ink   = "#212529",
  muted = "#6C757D",
  rule  = "#D8E0E6",
  wash  = "#F2F6F9",
  grey  = "#B8C2CC",
  flag  = "#6C757D"
)

###### B_03_02: NACE Section Names #############################################
# Note: Sections of NACE Rev 2 (A*21; U appears only in 1998) and of NACE
#   Rev 1.1 (A17, used by the 1998 and 2005 tables). The same letter can
#   mean different things in the two (D is Manufacturing in Rev 1.1).

B_03_02_sections_lst <- list(
  rev2 = c(
    A = "Agriculture, Forestry and Fishing",
    B = "Mining and Quarrying",
    C = "Manufacturing",
    D = "Electricity and Gas",
    E = "Water Supply and Waste",
    F = "Construction",
    G = "Wholesale and Retail Trade",
    H = "Transport and Storage",
    I = "Accommodation and Food",
    J = "Information and Communication",
    K = "Finance and Insurance",
    L = "Real Estate",
    M = "Professional and Scientific",
    N = "Administrative and Support",
    O = "Public Administration",
    P = "Education",
    Q = "Health and Social Work",
    R = "Arts and Recreation",
    S = "Other Services",
    T = "Households as Employers",
    U = "Extraterritorial Bodies"
  ),
  rev1 = c(
    A = "Agriculture, Hunting and Forestry",
    B = "Fishing",
    C = "Mining and Quarrying",
    D = "Manufacturing",
    E = "Electricity, Gas and Water Supply",
    F = "Construction",
    G = "Wholesale and Retail Trade",
    H = "Hotels and Restaurants",
    I = "Transport, Storage and Communication",
    J = "Financial Intermediation",
    K = "Real Estate, Renting and Business",
    L = "Public Administration and Defence",
    M = "Education",
    N = "Health and Social Work",
    O = "Other Community and Personal Services",
    P = "Households as Employers",
    Q = "Extraterritorial Bodies"
  )
)

###### B_03_03: Measures #######################################################
# Note: What a multiplier can count (the sidebar menu). "help" is shown in
#   the sidebar; "axis" labels the charts.

B_03_03_measures_lst <- list(
  output = list(
    label  = "Output",
    axis   = "Output per €1 of final demand",
    effect = "Extra gross output",
    help  = paste(
      "Total output across the economy from &euro;1 of extra final demand.",
      "It",
      "starts at 1 (the product itself) and adds what the supply chain, and",
      "with Type II households, produce in response.",
      "It is a <strong>gross</strong> measure: the same euro is counted at",
      "every stage it passes through, so it is always larger than value",
      "added and is <strong>not</strong> a contribution to GDP. Netting",
      "that double-counting out is exactly what value added does."
    )
  ),
  gva = list(
    label  = "Gross Value Added",
    axis   = "Value added (GDP) per €1 of final demand",
    effect = "Extra gross value added",
    help  = paste(
      "The part of that output that is income generated in Ireland: wages,",
      "profits, depreciation and net taxes on production."
    )
  ),
  wages = list(
    label  = "Compensation of Employees",
    axis   = "Wages per €1 of final demand",
    effect = "Extra wages",
    help  = paste(
      "Wages and salaries (with employer social contributions) paid along",
      "the supply chain. Labour-intensive services score highly."
    )
  ),
  gos = list(
    label  = "Gross Operating Surplus",
    axis   = "Profits and depreciation per €1 of final demand",
    effect = "Extra profits and depreciation",
    help  = paste(
      "Profits (net operating surplus) plus depreciation. In the",
      "multinational sectors this is most of value added. It is the one",
      "measure whose DIRECT effect can be negative: a sector that made a",
      "loss large enough to swamp its depreciation contributes negative",
      "surplus per euro of its own output. In 2022 that is true of basic",
      "metals (-0.30) and motor vehicles (-0.13). Output is always 1.00",
      "by construction and wages are never below zero."
    )
  ),
  exchequer = list(
    label  = "Exchequer Return (All Taxes)",
    axis   = "Taxes per €1 of final demand",
    effect = "Extra taxes (the Exchequer return)",
    help  = paste(
      "Everything the Exchequer collects along the chain: taxes on",
      "products (VAT, excise), other taxes on production less subsidies,",
      "and income tax, USC and PRSI on wages and corporation tax on",
      "profits at the year's average rates (from the government accounts)."
    )
  ),
  jobs = list(
    label  = "Jobs (FTE Job-Years)",
    axis   = "FTE job-years per €1 million of final demand",
    effect = "Extra FTE job-years",
    digits = 1,
    help   = paste(
      "Full-time-equivalent job-years supported per &euro;1 million: hours",
      "worked &divide; 1,800. <strong>Not permanent jobs</strong>: 100 FTE",
      "job-years could be 100 full-time jobs for one year or 20 for five.",
      "<strong>Not an extra benefit either</strong>: the pay these jobs",
      "earn is already inside compensation of employees, and so already",
      "inside gross value added and gross output. The job-years are the",
      "employment embodied in that same output, not something on top of it.",
      "Employment is by Eurostat industry, spread evenly over the CSO",
      "products within it. Per &euro;1m at each year's prices."
    )
  ),
  imports = list(
    label  = "Imports",
    axis   = "Imports per €1 of final demand",
    effect = "Extra imports",
    help  = paste(
      "The leak abroad: imported inputs along the supply chain (and, with",
      "Type II, imports bought by households). High import content is why",
      "multipliers in a small open economy are low."
    )
  )
)

###### B_03_04: Multiplier Types ###############################################
# Note: Labels and explanations for Type I and Type II.

B_03_04_types_lst <- list(
  t1 = list(
    label = "Type I: Direct + Indirect",
    help  = paste(
      "<strong>Direct</strong>: the spending itself.",
      "<strong>Indirect</strong>: every round of suppliers, their",
      "suppliers, and so on: L = (I &minus; A)<sup>&minus;1</sup>."
    )
  ),
  t2 = list(
    label = "Type II: + Induced",
    help  = paste(
      "Adds the <strong>induced</strong> effect: the people employed along",
      "the chain spend their wages, which creates more demand. Households",
      "are added to the table as one more sector, spending in the same",
      "pattern as household consumption in the table. The wage bill is",
      "taken <strong>gross</strong>: whatever is taxed away is assumed to",
      "be spent again in the same pattern, so this is an upper bound on",
      "the induced effect."
    )
  )
)

###### B_03_05: Effect Sources #################################################
# Note: The parts of every multiplier, in legend order: the spending
#   itself, the supply chain, and what the workers along it buy with
#   their wages.

B_03_05_effects_vec <- c(
  direct   = "Direct",
  indirect = "Indirect",
  induced  = "Induced"
)

###### B_03_06: Where Each Euro of Demand Ends Up ##############################
# Note: The components that one euro of final demand ends up as (Type I:
#   they add up to exactly one euro).

B_03_06_components_vec <- c(
  wages   = "Compensation of Employees",
  gos     = "Gross Operating Surplus",
  prodn   = "Other Taxes on Production, Net",
  prodtax = "Taxes on Products",
  imports = "Imports"
)

###### B_03_07: Project Measures ###############################################
# Note: The rows of the project results, in order.

B_03_07_project_measures_vec <- c(
  output    = "Output",
  gva       = "Gross Value Added",
  wages     = "Compensation of Employees",
  gos       = "Gross Operating Surplus",
  imports   = "Imports",
  exchequer = "Exchequer Return"
)

###### B_03_08: Project Measures, as Chart Rows ################################
# Note: The same measures labelled for the Effects of the Project figure,
#   where the rows share one euro axis and must not be added: GVA is part
#   of gross output, wages and GOS are parts of GVA, imports never enter
#   gross output, and the Exchequer return is a slice of the rows above.
#   The "of which" prefixes carry the nesting. ASCII only: drawn by the
#   graphics device. B_03_07 labels the Excel export, without prefixes.

B_03_08_project_rows_vec <- c(
  output    = "Gross Output",
  gva       = "- of which Gross Value Added",
  wages     = "-- of which Compensation of Employees",
  gos       = "-- of which Gross Operating Surplus",
  imports   = "Imports (leakage)",
  exchequer = "Exchequer Return (memo)"
)

###### B_03_09: Plot Text Size #################################################
# Note: Readable when projected.

B_03_09_base_size_int <- 13L

###### B_03_10: Model Measures #################################################
# Note: Every coefficient the model computes multipliers for (C_01_02).

B_03_10_model_measures_vec <- c("output", "gva", "wages", "gos", "nos",
                                "prodn", "prodtax", "taxes", "imports",
                                "jobs", "exchequer")

###### B_03_11: Divisions ######################################################
# Note: For each classification: the first division and letter of each
#   section, then the first division, code and name of each division
#   (NACE Rev 2 A*38; NACE Rev 1.1 A31, its sections and subsections).

B_03_11_groups_lst <- list(
  rev2 = list(
    sec_breaks = c(1, 5, 10, 35, 36, 41, 45, 49, 55, 58, 64, 68, 69, 77, 84,
                   85, 86, 90, 94, 97, 99, 100),
    sec_codes  = LETTERS[1:21],
    breaks = c(1, 5, 10, 13, 16, 19, 20, 21, 22, 24, 26, 27, 28, 29, 31, 35,
               36, 41, 45, 49, 55, 58, 61, 62, 64, 68, 69, 72, 73, 77, 84, 85,
               86, 87, 90, 94, 97, 99),
    codes  = c("A", "B", "CA", "CB", "CC", "CD", "CE", "CF", "CG", "CH", "CI",
               "CJ", "CK", "CL", "CM", "D", "E", "F", "G", "H", "I", "JA",
               "JB", "JC", "K", "L", "MA", "MB", "MC", "N", "O", "P", "QA",
               "QB", "R", "S", "T", "U"),
    names  = c(
      "Agriculture, Forestry and Fishing", "Mining and Quarrying",
      "Food, Beverages and Tobacco", "Textiles, Apparel and Leather",
      "Wood, Paper and Printing", "Coke and Refined Petroleum", "Chemicals",
      "Pharmaceuticals", "Rubber, Plastics and Minerals",
      "Basic and Fabricated Metals", "Computers, Electronics and Optics",
      "Electrical Equipment", "Machinery and Equipment",
      "Transport Equipment", "Furniture, Other Manufacturing, Repair",
      "Electricity and Gas", "Water, Sewerage and Waste", "Construction",
      "Wholesale and Retail Trade", "Transport and Storage",
      "Accommodation and Food", "Publishing, Film and Broadcasting",
      "Telecommunications", "Computer and Information Services",
      "Finance and Insurance", "Real Estate",
      "Legal, Accounting, Consultancy, Engineering",
      "Scientific Research and Development",
      "Advertising and Other Professional", "Administrative and Support",
      "Public Administration and Defence", "Education", "Human Health",
      "Social Work", "Arts, Entertainment and Recreation", "Other Services",
      "Households as Employers", "Extraterritorial Bodies"
    )
  ),
  rev1 = list(
    sec_breaks = c(1, 5, 10, 15, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90,
                   95, 99, 100),
    sec_codes  = LETTERS[1:17],
    breaks = c(1, 5, 10, 13, 15, 17, 19, 20, 21, 23, 24, 25, 26, 27, 29, 30,
               34, 36, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 99),
    codes  = c("A", "B", "CA", "CB", "DA", "DB", "DC", "DD", "DE", "DF", "DG",
               "DH", "DI", "DJ", "DK", "DL", "DM", "DN", "E", "F", "G", "H",
               "I", "J", "K", "L", "M", "N", "O", "P", "Q"),
    names  = c(
      "Agriculture, Hunting and Forestry", "Fishing",
      "Mining of Energy Materials", "Other Mining and Quarrying",
      "Food, Beverages and Tobacco", "Textiles and Clothing",
      "Leather Products", "Wood Products", "Paper, Publishing and Printing",
      "Coke, Refined Petroleum, Nuclear Fuel",
      "Chemicals and Man-Made Fibres", "Rubber and Plastics",
      "Other Non-Metallic Minerals", "Basic and Fabricated Metals",
      "Machinery and Equipment", "Electrical and Optical Equipment",
      "Transport Equipment", "Other Manufacturing and Recycling",
      "Electricity, Gas and Water Supply", "Construction",
      "Wholesale and Retail Trade", "Hotels and Restaurants",
      "Transport, Storage and Communication", "Financial Intermediation",
      "Real Estate, Renting and Business", "Public Administration and Defence",
      "Education", "Health and Social Work",
      "Other Community and Personal Services", "Households as Employers",
      "Extraterritorial Bodies"
    )
  )
)

###### B_03_12: Levels of Detail ###############################################
# Note: The sidebar choices, coarse to fine. The A*38 level is NACE's
#   division level (Regulation 1893/2006), so it is called divisions on
#   screen; the value keeps "group".

B_03_12_levels_vec <- c(
  "Sections (NACE A*21)"      = "section",
  "Divisions (NACE A*38)"     = "group",
  "Products (CSO Categories)" = "product"
)

###### B_03_13: Default Shock ##################################################
# Note: NACE divisions of the product shocked at start ("10" = food
#   products; in years where food is combined, the combined product) and
#   the extra final demand, EUR million.

B_03_13_shock_lst <- list(key = "10", size = 100)

###### B_03_14: Chart Sizes ####################################################
# Note: Supplier rounds shown; pixels per bar in the long charts; rows in
#   the "where is output produced" charts.

B_03_14_sizes_lst <- list(rounds = 10L, row_px = 17L, top_n = 12L)

###### B_03_15: Tab Notes ######################################################
# Note: One line per tab saying what it is for, and one thing to try.

B_03_15_prompts_lst <- list(
  mult = list(
    what = paste(
      "What this tab is for: comparing the multiplier of every sector side",
      "by side, and seeing what €1 of demand turns into."
    ),
    try = paste(
      "The multinational sectors (pharma and electronics, ICT services) are",
      "the largest by output, yet have some of the smallest multipliers.",
      "Switch the view to Where Each Euro of Demand Ends Up: in 2022 pharma",
      "turns €1 of demand into 67% value added but only 2% wages. Then",
      "switch the level",
      "of detail to Sections to see the wider pattern."
    )
  ),
  shock = list(
    what = paste(
      "What this tab is for: tracing one concrete shock, €x million of extra",
      "demand for one product, through the economy and round by round."
    ),
    try = paste(
      "Give Food Products an extra €100m of demand, then the same to Pharma,",
      "Computers, Electronics. Compare the value added and wages each",
      "creates. Switch to Type II: which products gain most from the induced",
      "effect, and why? Change the Measure: the charts follow it."
    )
  ),
  project = list(
    what = paste(
      "What this tab is for: costing a real project line by line and reading",
      "off its output, wages, jobs and Exchequer return."
    ),
    try = paste(
      "Load the school template. How much of the cost comes back to the",
      "Exchequer, and how many FTE job-years does it support? Now put",
      "Construction at capacity (0% spare) and add 30% displacement: how much",
      "of the multiplier survives when resources are fully employed?"
    )
  ),
  time = list(
    what = paste(
      "What this tab is for: watching one sector's multiplier move across",
      "the eight tables, 1998 to 2022."
    ),
    try = paste(
      "Choose the computer services product and follow its imports across",
      "the years. Then pick Construction, before and after the crash. Which",
      "industries became more or less connected to Irish suppliers, and why?"
    )
  ),
  table = list(
    what = paste(
      "What this tab is for: looking inside the A matrix, at who buys how",
      "much from whom."
    ),
    try = paste(
      "Choose Construction and read down its column: where do its inputs",
      "come from? Then choose the computer services product: why does it buy",
      "so little from Irish suppliers? Switch the level of detail to compare."
    )
  ),
  maths = list(
    what = paste(
      "What this tab is for: the algebra behind every number in the app, the",
      "notation, the classification and the data."
    ),
    try = paste(
      "The Leontief inverse is a geometric series, just like the Keynesian",
      "spending multiplier from lecture 1.1: each round of suppliers is a",
      "fraction of the one before."
    )
  )
)

###### B_03_16: Project Templates ##############################################
# Note: Illustrative splits of a project's cost (EUR million) by product
#   (NACE key, matched to the closest product in each year) and the share
#   bought from Irish producers. Not real costings. The names are the
#   drop-down's option labels, so they say EUR in ASCII.

B_03_16_templates_lst <- list(
  "New Primary School (EUR 20m)" = data.frame(
    desc   = c("Building works", "Architects and engineers", "Furniture",
               "IT equipment", "Legal and accounting"),
    key    = c("41,42,43", "71", "31,32", "26", "69"),
    amount = c(16, 2, 1, 0.6, 0.4),
    irish  = c(100, 100, 80, 20, 100)
  ),
  "Greenway, 40 km (EUR 10m)" = data.frame(
    desc   = c("Civil works", "Design and engineering", "Railings and bridges",
               "Stone and surfacing", "Plant hire"),
    key    = c("41,42,43", "71", "25", "23", "77"),
    amount = c(7.5, 1, 0.8, 0.5, 0.2),
    irish  = c(100, 100, 70, 90, 100)
  ),
  "Social Housing, 100 Homes (EUR 35m)" = data.frame(
    desc   = c("Construction", "Design and engineering",
               "Legal and conveyancing",
               "Kitchens and fittings", "Heat pumps and electrical"),
    key    = c("41,42,43", "71", "69", "31,32", "27"),
    amount = c(29, 2.5, 0.5, 1.5, 1.5),
    irish  = c(100, 100, 100, 70, 30)
  ),
  "Hospital Equipment (EUR 15m)" = data.frame(
    desc   = c("Scanners and monitors", "Other machinery", "Installation",
               "Staff training", "Project management"),
    key    = c("26", "28", "33", "85", "74,75"),
    amount = c(8, 4, 2, 0.5, 0.5),
    irish  = c(20, 30, 100, 100, 100)
  ),
  "Arts Festival (EUR 2m)" = data.frame(
    desc   = c("Performers and production", "Catering", "Accommodation",
               "Security and stewarding", "Transport"),
    key    = c("90,91,92", "56", "55", "80,81,82", "49"),
    amount = c(0.8, 0.4, 0.3, 0.3, 0.2),
    irish  = c(90, 100, 100, 100, 100)
  )
)

###### B_03_17: Template Loaded at the Start ###################################
# Note: The costing tab opens on this one already priced.

B_03_17_start_tpl_chr <- names(B_03_16_templates_lst)[1]

###### B_03_18: Project Assumptions ############################################
# Note: Defaults for displacement (per cent of indirect and induced
#   effects) and spare capacity in products at capacity (per cent of
#   output); tax rates used only when a year's io_meta.csv has none.
#   Displacement opens at 25 per cent, within the range the Public
#   Spending Code uses for construction in a tight labour market.

B_03_18_assume_lst <- list(tax_wage = 35, tax_profit = 9, displace = 25,
                           spare = 2)

###### B_03_19: Products at Capacity at the Start ##############################
# Note: Construction: the opening template is a school and building works
#   are three quarters of its cost.

B_03_19_cap_keys_chr <- "41,42,43"

###### B_03_20: Scenarios ######################################################
# Note: Worked examples. Each sets the controls ("set"), opens a tab and
#   tells the story they show; the numbers in the stories are the app's
#   own, and the year is always named. "stage" is the mode the example
#   belongs to: all five are in "Explore the Model", and the worked
#   examples of "Cost a Project" are the templates (B_03_16). Story order
#   and wording follow CONVENTIONS.md 3 to 5.

B_03_20_scenarios_lst <- list(
  multinational = list(
    label = "The Largest Producers Have the Smallest Multipliers",
    stage = "model",
    tab   = "mult",
    set   = list(year = "2022", level = "group", sort = "size", type = "t2",
                 measure = "output", view = "source", product = "21,26",
                 size = 100),
    story = paste(
      "The chart ranks every NACE division in the 2022 table by the output",
      "(x) that €1 of extra final demand (f) generates: the horizontal axis",
      "is that multiplier and the vertical axis is the divisions ordered by",
      "it, so how big a division is appears on neither. Pharma with",
      "computers and electronics is the largest producer in the table at",
      "about €217bn of output, and sits near the foot of the ranking at",
      "1.06; publishing and computer services (€227bn) reaches 1.27. What",
      "sets the length of a bar is the division's column of the technical",
      "coefficient matrix (A): €0.33 of pharma's euro and €0.58 of",
      "computer",
      "services' leaves at once as imported inputs (m), and €0.02 comes back",
      "as wages (w), so little is left for the inter-industry rounds Type I",
      "counts or the household spending Type II adds on top of them. Switch",
      "the Measure to Imports and the ranking turns over."
    )
  ),
  construction = list(
    label = "Construction's Inputs Are Mostly Irish",
    stage = "model",
    tab   = "shock",
    set   = list(year = "2022", level = "product", sort = "size", type = "t2",
                 measure = "output", view = "source", product = "41,42,43",
                 size = 100),
    story = paste(
      "€100m of extra final demand (f) is placed on construction, a product",
      "that buys most of its inputs from Irish producers and pays much of",
      "the rest out as wages (w). On the 2022 table that raises output (x)",
      "by €175m counting the inter-industry rounds alone (Type I) and by",
      "€208m once the wages earned along the chain are spent again",
      "(Type II), supporting about 880 FTE job-years. The rounds figure puts",
      "the supplier round on the horizontal axis and the cumulative euro",
      "total on the vertical: the Type I series is all but finished by round",
      "4, while the induced series is still adding at round 6. How far each",
      "goes is decided by how much of every round stays in Ireland — €0.43",
      "of construction's euro is still imported (m), and that leakage is",
      "where both series stop climbing."
    )
  ),
  subsidies = list(
    label = "Subsidies Make Forestry's Net Taxes Negative",
    stage = "model",
    tab   = "mult",
    set   = list(year = "2022", level = "product", sort = "size", type = "t1",
                 measure = "gva", view = "split", product = "2", size = 100),
    story = paste(
      "Where Each Euro of Demand Ends Up splits the €1 of final demand (f)",
      "into the wages",
      "(w), gross operating surplus (g), other taxes on production (n),",
      "taxes on products (t) and imports (m) it becomes along the chain. The",
      "horizontal axis is each slice as a per cent of that euro and the",
      "vertical axis lists the products; under Type I the slices add to",
      "exactly 100%, which the black tick marks. Forestry's bar runs off",
      "both ends of its tick because n is taxes <em>less</em> subsidies and",
      "forestry receives far more than it pays: −59% of the euro in 2022,",
      "against −9% for crops and animals. A slice below zero has to be paid",
      "for by a larger one elsewhere — here mostly wages, at €0.64 per",
      "euro — so w′L + g′L + n′L + t′L + m′L = ι′ still holds whatever",
      "sign the",
      "pieces take. This is the CAP inside an input-output table."
    )
  ),
  induced = list(
    label = "Education's Multiplier Is Almost All Induced",
    stage = "model",
    tab   = "mult",
    set   = list(year = "2022", level = "product", sort = "size", type = "t2",
                 measure = "wages", view = "source", product = "85",
                 size = 100),
    story = paste(
      "Education buys almost nothing from other industries, so its",
      "inter-industry rounds are tiny: its Type I output multiplier is 1.14",
      "in the 2022 table, near the bottom. The chart is set to wages (w):",
      "the horizontal axis is wages per €1 of final demand (f) and the",
      "vertical axis ranks the products by it, and education is high up",
      "because €0.75 of its own euro is pay. Toggle Type I to Type II and",
      "the green induced segment appears — those wages spent again —",
      "lifting",
      "education's wages multiplier from €0.80 to €1.03 and its output",
      "multiplier from 1.14 to 2.12. The induced effect is larger here than",
      "the supply chain, which is the one case where the choice between Type",
      "I and Type II decides the answer; social work, public administration",
      "and health behave the same way."
    )
  ),
  trends = list(
    label = "Construction's Import Content Since 1998",
    stage = "model",
    tab   = "time",
    set   = list(year = "2022", level = "group", sort = "group", type = "t1",
                 measure = "imports", view = "source", product = "41,42,43",
                 size = 100),
    story = paste(
      "Trends follows one product across the eight CSO tables. The",
      "horizontal axis is the year of the table and the vertical axis is",
      "imported inputs (m) per €1 of final demand (f) for construction:",
      "€0.31 in 1998, €0.48 by 2010 and about €0.43 since. More of what",
      "a",
      "building site uses is now bought abroad, so a larger share of every",
      "round leaks out before it can become Irish output (x), and",
      "construction's Type I output multiplier sits near 1.6 through the",
      "2010s against 1.77 in 1998. What keeps the two axes from moving",
      "cleanly together is measurement: 1998 and 2005 are NACE Rev 1.1,",
      "matched to the nearest Rev 2 division, and each table is in its own",
      "year's prices."
    )
  )
)

###### B_03_21: Modes ##########################################################
# Note: The two halves of the app; the preset card groups its worked
#   examples by mode, and the radio that switches them has id "mode".

B_03_21_modes_vec <- c("Explore the Model" = "model",
                        "Cost a Project"    = "project")

###### B_03_22: Example Loaded at the Start ####################################
# Note: The app opens on this worked example, with every control live.
#   See CONVENTIONS.md 2.

B_03_22_start_scn_chr <- names(B_03_20_scenarios_lst)[1]

###### B_03_23: Control Help ###################################################
# Note: One plain-English line per control, shown under it. Euro signs are
#   HTML entities.

B_03_23_help_lst <- list(
  mode = paste(
    "Explore the Model compares sectors; Cost a Project prices one project",
    "you specify line by line."
  ),
  year = paste(
    "Which CSO table to use. Each is a snapshot of the economy that year, in",
    "that year's prices, so multipliers are not directly comparable across",
    "years."
  ),
  level = paste(
    "How far to zoom out. Sections are the 21 NACE letters, divisions the 38",
    "NACE subdivisions, products the CSO's own categories. Coarser levels",
    "average multipliers over more products, using output as the weight."
  ),
  sort = paste(
    "Size puts the biggest bar at the top. Classification keeps the rows in",
    "NACE order, so all of manufacturing sits together and you can see",
    "whether a pattern is about the sector or about the industry."
  ),
  type = paste(
    "Type I counts the spending and its supply chain. Type II also counts",
    "what the workers along that chain buy with their wages, so it is always",
    "larger."
  ),
  measure = paste(
    "What the multiplier counts. Every chart and number on the Multipliers,",
    "Demand Shock and Trends tabs follows this choice."
  ),
  product = paste(
    "Which product gets the extra demand. It is also the row outlined in red",
    "on the charts and the column read out on Industry Interactions."
  ),
  size = paste(
    "How much extra final demand, in &euro; million. Negative values model",
    "a fall",
    "in demand; the model is linear, so the effects just change sign."
  ),
  view = paste(
    "By Source splits the multiplier into direct, indirect and induced:",
    "where the extra output comes from. Where Each Euro of Demand Ends Up",
    "asks the other question. Take one euro of final demand and follow it",
    "to what it finally becomes: wages, gross operating surplus, taxes and",
    "imports. Under Type I those shares add to exactly 1."
  ),
  tpl = paste(
    "An illustrative costing to start from. Load it, then edit, delete or",
    "add lines."
  ),
  tax_wage = paste(
    "Income tax, USC and PRSI as a share of wages, used only for the",
    "Exchequer return. Defaults to the year's own average from the",
    "government accounts."
  ),
  tax_profit = paste(
    "Corporation tax as a share of net operating surplus, again only for the",
    "Exchequer return."
  ),
  displace = paste(
    "How much of the knock-on effect only moves activity from somewhere",
    "else. At full employment this is close to 100%."
  ),
  cap_keys = paste(
    "Products that cannot supply much more. Demand they cannot meet is",
    "imported instead."
  ),
  cap_pct = paste(
    "How much those products can still expand, as a per cent of their",
    "current output. 0% means they are completely full."
  )
)

###### B_03_24: Author Credit ##################################################
# Note: Title bar, browser tab and footer.

B_03_24_author_chr <- "Sam Deegan"

###### B_03_25: Author Website #################################################
# Note: Linked from the byline, sidebar and footer.

B_03_25_site_chr <- "https://sam-deegan.com"

###### B_03_26: Course Line ####################################################
# Note: Footer text.

B_03_26_course_chr <- "ECON42550 Macroeconomics, University College Dublin"

###### B_03_27: Version ########################################################
# Note: Semantic version, shown in the footer; CHANGELOG.md has the history.

B_03_27_version_chr <- "1.0.6"

###### B_03_28: Source Repository ##############################################
# Note: The GitHub repo, linked from the footer.

B_03_28_repo_chr <- paste0("https://github.com/Sam-Deegan/",
                        "Interactive-Model-Input-Output-Multipliers")

#### B_04: Paths ###############################################################
# Note: The data folder and the QR code.

###### B_04_01: Data Folder ####################################################
# Note: The three CSVs the model reads (C_01_01).

B_04_01_data_dir <- "data"

###### B_04_02: QR Code Source #################################################
# Note: Embedded as a data URI because shinylive's export drops www/.

B_04_02_qr_src_chr <- paste0(
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

################################################################################
## C: Model ####################################################################
################################################################################
# Note: C_01, the model, is R/io_model.R. C_02 reads the data and computes
#   everything that does not depend on the controls once, at start-up.

#### C_02: Precomputed Results #################################################
# Note: Every year's inverses, multipliers and groups, plus the menus.

###### C_02_01: Read the Data ##################################################
# Note: One element per year.

C_02_01_io_lst <- C_01_01_read_data_fn(B_04_01_data_dir)

###### C_02_02: Results by Year ################################################
# Note: Inverses, multipliers and the three levels of detail for each year;
#   the exchequer measure uses the year's average tax rates.

C_02_02_res_lst <- lapply(C_02_01_io_lst, function(io) {
  m   <- io$meta
  get <- function(k, d) {
    v <- suppressWarnings(as.numeric(m[[k]]))
    if (length(v) == 0 || is.na(v)) d else v
  }
  C_01_09_year_fn(io, measures = B_03_10_model_measures_vec,
                  grp_def = B_03_11_groups_lst,
                  sec_names = B_03_02_sections_lst,
                  rates = c(wage = get("tax_wage_pct",
                                       B_03_18_assume_lst$tax_wage),
                            profit = get("tax_profit_pct",
                                         B_03_18_assume_lst$tax_profit)))
})

###### C_02_03: Years ##########################################################
# Note: Newest first, for the year menu.

C_02_03_years_vec <- rev(names(C_02_02_res_lst))

###### C_02_04: Multipliers across Years #######################################
# Note: For the "Trends" tab.

C_02_04_time_lst <- C_01_10_over_time_fn(C_02_02_res_lst)

###### C_02_05: Product Menu ###################################################
# Note: Products of one year grouped under their section ("C
#   Manufacturing"), each shown with its division code ("CA Food
#   Products"). Values are NACE keys, so a choice survives a change of year.

C_02_05_choices_fn <- function(prd) {
  sec <- factor(prd$cso_nsec_cat, levels = unique(prd$cso_nsec_cat))
  ch  <- lapply(split(prd, sec), function(g) {
    stats::setNames(g$cso_key_str, paste(g$cso_group_cat, g$cso_short_str))
  })
  names(ch) <- paste(names(ch), prd$cso_nsec_str[match(names(ch),
                                                       prd$cso_nsec_cat)])
  ch
}

###### C_02_06: NACE Rev 2 Divisions of Every Product ##########################
# Note: Key to the Rev 2 divisions it covers (for NACE Rev 1.1 products,
#   the rough Rev 2 equivalent), across all years.

C_02_06_match_vec <- local({
  all <- do.call(rbind, lapply(C_02_01_io_lst, function(io) {
    io$products[, c("cso_key_str", "cso_match_str")]
  }))
  all <- all[!duplicated(all$cso_key_str), ]
  stats::setNames(all$cso_match_str, all$cso_key_str)
})

###### C_02_07: Closest Product ################################################
# Note: The same product in another year's table if it exists; else the
#   largest product sharing a NACE Rev 2 division with it; else the first
#   product (or NA if fallback is FALSE). prd needs cso_key_str,
#   cso_match_str and cso_output_amt.

C_02_07_match_key_fn <- function(key, prd, fallback = TRUE) {
  if (key %in% prd$cso_key_str) return(key)
  want <- if (key %in% names(C_02_06_match_vec)) C_02_06_match_vec[[key]] else
    key
  want <- suppressWarnings(as.integer(strsplit(want, ",")[[1]]))
  share <- vapply(strsplit(prd$cso_match_str, ","), function(k) {
    any(as.integer(k) %in% want)
  }, TRUE)
  if (!any(share)) {
    return(if (fallback) prd$cso_key_str[1] else NA_character_)
  }
  hit <- which(share)
  prd$cso_key_str[hit[which.max(prd$cso_output_amt[hit])]]
}

###### C_02_08: Starting Products ##############################################
# Note: The newest year's products (with groups), for menus at start-up.

C_02_08_start_prd_df <- C_02_02_res_lst[[C_02_03_years_vec[1]]]$io$products

###### C_02_09: Classification by Year #########################################
# Note: "NACE Rev 2" or "NACE Rev 1.1", from each year's meta.

C_02_09_class_vec <- vapply(C_02_01_io_lst, function(io) {
  cl <- io$meta$classification
  if (is.null(cl)) "NACE Rev 2" else cl
}, "")

###### C_02_10: Years under NACE Rev 1.1 #######################################
# Note: Mentioned in the notes of the "Trends" tab.

C_02_10_rev1_years_vec <- names(C_02_09_class_vec)[
  C_02_09_class_vec == "NACE Rev 1.1"
]

###### C_02_11: Data Sources ###################################################
# Note: One line per year, for the "Data and Caveats" tab.

C_02_11_sources_vec <- vapply(names(C_02_01_io_lst), function(yr) {
  m <- C_02_01_io_lst[[yr]]$meta
  paste0(yr, ": ", m$source, " (", C_02_09_class_vec[[yr]], ")")
}, "")

###### C_02_12: Output by Level ################################################
# Note: Sums a product-level table of effects (direct, indirect, induced,
#   EUR million) to the chosen level of detail, with labels.

C_02_12_by_level_fn <- function(res, direct, indirect, induced, level) {
  code <- res$code_of[[level]]
  labs <- res$levels[[level]]$multipliers
  lab  <- stats::setNames(labs$cso_label_str, labs$cso_group_cat)
  grp  <- unique(code)
  data.frame(
    code     = grp,
    label    = unname(lab[grp]),
    direct   = vapply(grp, function(g) sum(direct[code == g]), 0),
    indirect = vapply(grp, function(g) sum(indirect[code == g]), 0),
    induced  = vapply(grp, function(g) sum(induced[code == g]), 0),
    stringsAsFactors = FALSE
  )
}

###### C_02_13: Lines from a Template or Upload ################################
# Note: Standardises a costing (description, key, amount, share bought in
#   Ireland) to the products of one year, matching keys or product names.

C_02_13_lines_fn <- function(desc, key, amount, irish, prd, name = NULL) {
  key <- as.character(key)
  if (!is.null(name)) {
    by_name <- prd$cso_key_str[match(tolower(trimws(name)),
                                     tolower(prd$cso_short_str))]
    key[is.na(key) | key == ""] <- by_name[is.na(key) | key == ""]
  }
  ok  <- !is.na(key) & key != "" & is.finite(as.numeric(amount))
  out <- data.frame(
    desc   = as.character(desc[ok]),
    key    = vapply(key[ok], C_02_07_match_key_fn, "", prd = prd),
    amount = as.numeric(amount[ok]),
    irish  = pmin(pmax(as.numeric(irish[ok]), 0), 100),
    stringsAsFactors = FALSE
  )
  out$irish[is.na(out$irish)] <- 100
  out$desc[is.na(out$desc) | out$desc == ""] <- "Spending"
  rownames(out) <- NULL
  out
}

###### C_02_14: Empty Costing ##################################################
# Note: What Clear All leaves.

C_02_14_empty_lines_df <- data.frame(desc = character(), key = character(),
                                     amount = numeric(), irish = numeric(),
                                     stringsAsFactors = FALSE)

###### C_02_15: Tax Rates of a Year ############################################
# Note: Average tax rates on wages and profits (per cent, one decimal)
#   from the year's meta, or the defaults of B_03_18 if it has none.

C_02_15_tax_rates_fn <- function(year) {
  m   <- C_02_01_io_lst[[year]]$meta
  get <- function(k, d) {
    v <- suppressWarnings(as.numeric(m[[k]]))
    if (length(v) == 0 || is.na(v)) d else round(v, 1)
  }
  list(wage   = get("tax_wage_pct", B_03_18_assume_lst$tax_wage),
       profit = get("tax_profit_pct", B_03_18_assume_lst$tax_profit))
}

###### C_02_16: Level Choices of a Year ########################################
# Note: The level-of-detail labels name the year's classification.

C_02_16_level_choices_fn <- function(year) {
  if (C_02_09_class_vec[[year]] == "NACE Rev 1.1") {
    c("Sections (NACE Rev 1.1 A17)" = "section",
      "Divisions (NACE Rev 1.1 A31)" = "group",
      "Products (CSO Categories)" = "product")
  } else {
    B_03_12_levels_vec
  }
}

###### C_02_17: The App's Own Settings #########################################
# Note: The value every control opens on; the sidebar reads from here, and
#   the ghost falls back to it when no worked example is loaded. In C
#   because the first year is whichever the data folder holds (C_02_03).

C_02_17_defaults_lst <- list(
  year    = C_02_03_years_vec[1],
  type    = "t2",
  level   = "product",
  sort    = "size",
  measure = "output",
  view    = "source",
  product = B_03_13_shock_lst$key,
  size    = B_03_13_shock_lst$size
)

################################################################################
## E: User Interface ###########################################################
################################################################################
# Note: Sidebar of controls and one tabbed card; the server hides the tabs
#   the current mode does not use, so each output keeps a single id.

#### E_01: Theme and Styles ####################################################
# Note: Dublin colours and IBM Plex Sans, as in the slides.

###### E_01_01: Bootstrap Theme ################################################
# Note: Plex loads from Google Fonts when online, system font otherwise.

E_01_01_theme_lst <- bs_theme(
  version   = 5,
  primary   = B_03_01_palette_vec[["blue"]],
  secondary = B_03_01_palette_vec[["muted"]],
  success   = B_03_01_palette_vec[["green"]],
  info      = B_03_01_palette_vec[["light"]],
  bg        = "#FFFFFF",
  fg        = B_03_01_palette_vec[["ink"]],
  base_font = font_collection(font_google("IBM Plex Sans", local = FALSE),
                              "Segoe UI", "Helvetica", "Arial", "sans-serif"),
  heading_font = font_collection(font_google("IBM Plex Sans", local = FALSE),
                                 "Segoe UI", "Helvetica", "Arial", "sans-serif")
)

###### E_01_02: Extra CSS ######################################################
# Note: Tiles, prompt, explanation boxes, tables, sidebar labels, site
#   navigation, figure headers, credits and the tab strips.

E_01_02_css_chr <- "
  /* Cards and panels are square: they organise the page, not decorate it */
  .card, .card-header, .card-body, .card-footer, .bslib-card,
  .bslib-sidebar-layout, .navset-card-tab, .nav-tabs .nav-link,
  .accordion-item, .accordion-button, .story, .prompt, .problem,
  .stat-tile, .stat-input input, .btn, .form-control, .form-select,
  .badge { border-radius: 0 !important; }
  .card, .bslib-card { border: none; box-shadow: none; }
  .card-header { border-bottom: none; background: transparent;
    color: #0056A4; font-weight: 700; }
  .card-footer { border-top: none; background: transparent; }
  .stat-row { display: flex; flex-wrap: wrap; gap: 0.6rem; }
  .stat-tile { flex: 1 1 8rem; border-left: 5px solid #9FC4E0;
    border-radius: 4px; padding: 0.4rem 0.75rem; background: #F2F6F9; }
  .stat-tile.key { border-left-color: #61B77C; }
  .stat-label { font-size: 0.8rem; color: #6C757D; }
  .stat-value { font-size: 1.35rem; font-weight: 600; color: #04204C; }
  .stat-note  { font-size: 0.78rem; }
  .prompt { background: #F2F6F9; border-left: 4px solid #61B77C;
    padding: 0.6rem 0.9rem; border-radius: 4px; font-size: 0.95rem;
    margin-bottom: 0.6rem; }
  .story { background: #F2F6F9; border-radius: 4px; font-size: 0.86rem;
    padding: 0.55rem 0.75rem; margin-bottom: 0.7rem; }
  .story-key { font-weight: 700; color: #0056A4; }
  .narrative { border: 1px solid #D8E0E6; border-left: 4px solid #9FC4E0;
    border-radius: 4px; padding: 0.55rem 0.75rem; font-size: 0.92rem;
    margin-bottom: 0.7rem; }
  .ctl-label { font-size: 0.9rem; font-weight: 700; color: #0056A4; }
  .ctl-help { font-size: 0.78rem; color: #6C757D; line-height: 1.35;
    margin: -0.35rem 0 0.85rem 0; }
  .prj .ctl-help { margin: -0.25rem 0 0.6rem 0; }
  .mode-note { background: #F2F6F9; border-left: 4px solid #0056A4;
    border-radius: 4px; padding: 0.55rem 0.75rem; font-size: 0.86rem;
    margin-bottom: 0.7rem; }
  .ctl-row { display: flex; gap: 0.5rem; align-items: center; }
  .ctl-slider { flex: 1 1 auto; min-width: 0; }
  .ctl-box { flex: 0 0 5.4rem; }
  .ctl .form-group { margin-bottom: 0; }
  .ctl-box input { padding: 0.15rem 0.35rem; font-size: 0.85rem;
    text-align: right; }
  .sidebar .control-label, .prj .control-label { font-weight: 700;
    color: #0056A4; }
  .view-switch .shiny-options-group { display: flex; flex-wrap: wrap;
    gap: 0 1.2rem; }
  .eq-table td { padding: 0.3rem 0.9rem 0.3rem 0; vertical-align: top;
    border-bottom: 1px solid #F2F6F9; }
  .eq-label { font-weight: 600; color: #0056A4; white-space: nowrap; }
  .eq-note  { font-size: 0.86rem; }
  .nota-table { width: 100%; font-size: 0.88rem; }
  .nota-table td { padding: 0.25rem 0.6rem 0.25rem 0; vertical-align: top;
    border-bottom: 1px solid #F2F6F9; }
  .nota-table td:first-child { white-space: nowrap; width: 5.5rem; }
  .nota-head { font-weight: 700; color: #0056A4; margin: 0.3rem 0; }
  .cls-table { width: 100%; font-size: 0.85rem; border-collapse: collapse; }
  .cls-table th { color: #6C757D; font-weight: 600; text-align: left;
    border-bottom: 2px solid #D8E0E6; padding: 0.3rem 0.5rem; }
  .cls-table td { padding: 0.2rem 0.5rem; border-bottom: 1px solid #F2F6F9;
    vertical-align: top; }
  .cls-table tr.cls-sec td { background: #F2F6F9; font-weight: 700;
    color: #04204C; padding-top: 0.45rem; }
  .cls-table td.num { text-align: right; white-space: nowrap; }
  .lines-table { width: 100%; font-size: 0.85rem; border-collapse: collapse; }
  .lines-table th { color: #6C757D; font-weight: 600; text-align: left;
    border-bottom: 2px solid #D8E0E6; padding: 0.25rem 0.4rem; }
  .lines-table td { padding: 0.2rem 0.4rem; border-bottom: 1px solid #F2F6F9; }
  .lines-table td.num { text-align: right; white-space: nowrap; }
  .lines-table tr.total td { font-weight: 700; border-top: 2px solid #D8E0E6; }
  .del-line { color: #6C757D; cursor: pointer; text-decoration: none; }
  .del-line:hover { color: #04204C; }
  .site-nav { background: #FFFFFF; border-bottom: 1px solid #D8E0E6;
    padding: 1rem 2rem; margin: -0.5rem -0.5rem 0.75rem -0.5rem; }
  .site-nav-row { display: flex; align-items: center; gap: 2rem;
    max-width: 1200px; margin: 0 auto; }
  .site-nav ul { list-style: none; display: flex; justify-content: center;
    gap: 2rem; max-width: 1200px; margin: 0 auto; padding: 0;
    flex-wrap: wrap; }
  .site-nav a { color: #04204C; text-decoration: none; font-weight: 500;
    font-size: 0.95rem; padding-bottom: 0.25rem; position: relative; }
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
  }
  .fig-head { display: flex; align-items: center; gap: 0.5rem;
    margin: 0 0 0.3rem 0; }
  .fig-name { font-weight: 600; color: #04204C; }
  .fig-save { margin-left: auto; border: 1px solid #D8E0E6; background: #FFFFFF;
    color: #0056A4; font-size: 0.72rem; font-weight: 600; border-radius: 3px;
    padding: 0.1rem 0.5rem; cursor: pointer; line-height: 1.5; }
  .fig-save:hover { background: #0056A4; color: #FFFFFF;
    border-color: #0056A4; }
  /* The reading note under a figure */
  .fig-note { color: #6C757D; font-size: 0.8rem; line-height: 1.45;
    margin: 0.35rem 0 0.2rem 0; max-width: 62em; }
  .fig-note p { margin: 0; }
  .prj .form-group { margin-bottom: 0.5rem; }
  .prj-card-title { font-weight: 700; color: #04204C; font-size: 1.05rem;
    margin-bottom: 0.4rem; }
  .side-qr { flex: 0 0 auto; }
  .side-qr img { width: 56px; height: 56px; display: block; }
  .bslib-page-title { display: flex; align-items: center; gap: 0.3rem;
    width: 100%; }
  .title-credit { font-size: 0.8rem; font-weight: 400; margin-left: 0.8rem;
    opacity: 0.8; }
  .title-credit a { color: inherit; }
  .title-qr { margin-left: auto; }
  .title-qr img { height: 40px; width: 40px; }
  .credit { font-size: 0.8rem; color: #6C757D; text-align: center;
    padding: 1rem 0 0.5rem 0; }
  .nav-tabs .nav-link { color: #6C757D; font-weight: 600;
    border-color: transparent; }
  .nav-tabs .nav-link:hover { color: #0056A4; background: #F2F6F9; }
  .nav-tabs .nav-link.active { color: #FFFFFF; font-weight: 700;
    background: #0056A4; border-color: #0056A4; }
"

###### E_01_03: MathJax on Tab Switch ##########################################
# Note: Re-typesets when a tab is shown; formulas laid out in a hidden tab
#   come out at the wrong size.

E_01_03_js_chr <- paste(
  "document.addEventListener('shown.bs.tab', function() {",
  "  if (window.MathJax && MathJax.Hub) {",
  "    MathJax.Hub.Queue(['Typeset', MathJax.Hub]);",
  "  }",
  "});"
)

###### E_01_04: Save a Figure as PNG ###########################################
# Note: Points an <a download> at the PNG Shiny has already drawn, so no R
#   graphics device is involved and it behaves the same under shinylive.

E_01_04_save_js_chr <- paste(
  "document.addEventListener('click', function(ev) {",
  "  var btn = ev.target.closest('.fig-save');",
  "  if (!btn) return;",
  "  var box = document.getElementById(btn.dataset.plot);",
  "  var img = box ? box.querySelector('img') : null;",
  "  if (!img || !img.src) { return; }",
  "  var a = document.createElement('a');",
  "  a.href = img.src;",
  "  a.download = (btn.dataset.name || 'figure') + '.png';",
  "  document.body.appendChild(a); a.click();",
  "  document.body.removeChild(a);",
  "});",
  sep = "\n"
)

###### E_01_05: Site Navigation ################################################
# Note: The website's nav bar, so the app reads as a page of the site.
#   Links are absolute, so they work from a local runApp too.

E_01_05_nav_fn <- function(active = "Resources") {
  pages <- c(Bio = "index.html", Papers = "papers.html",
             Teaching = "teaching.html", Experience = "experience.html",
             Presentations = "talks.html", Resources = "resources.html",
             Contact = "contact.html")
  tags$nav(
    class = "site-nav",
    tags$div(
      class = "site-nav-row",
      tags$ul(
        lapply(names(pages), function(nm) {
          tags$li(tags$a(href = paste0(B_03_25_site_chr, "/", pages[[nm]]),
                         target = "_top",
                         class = if (identical(nm, active)) "active" else NULL,
                         nm))
        })
      )
    )
  )
}

#### E_02: Helpers and Sidebar #################################################
# Note: Small builders the interface uses, then the sidebar and the
#   worked-example card.

###### E_02_01: Download Button ################################################
# Note: shiny::downloadButton without its download attribute, so Chromium
#   takes the file name from the Content-Disposition header under
#   shinylive (Chromium issue 468227).

E_02_01_download_fn <- function(id, label, ...) {
  tag <- shiny::downloadButton(id, label, ...)
  tag$attribs$download <- NULL
  tag
}

###### E_02_02: Control Help ###################################################
# Note: The one-line explanation of a control (B_03_23), shown under it.

E_02_02_help_fn <- function(id) {
  # HTML(), so the entities in B_03_23 render
  tags$div(class = "ctl-help", HTML(B_03_23_help_lst[[id]]))
}

###### E_02_03: A Figure with a Save Button ####################################
# Note: A plot with a Save PNG button and a slot for its reading note: the
#   builder writes the note as the caption, E_02_04_draw_fn lifts it into
#   the toolkit's caption store and T_07_07d_cap_fn fills the slot. The id
#   is registered with the toolkit's figure register.

E_02_03_figure_fn <- function(id, title, height) {
  T_07_07e_add_fn(id)
  stem <- gsub("(^-|-$)", "",
               gsub("-+", "-", gsub("[^a-z0-9]+", "-", tolower(title))))
  tags$div(
    class = "fig-wrap",
    tags$div(
      class = "fig-head",
      tags$span(class = "fig-name", title),
      tags$button(type = "button", class = "fig-save", `data-plot` = id,
                  `data-name` = stem, title = "Save this figure as a PNG",
                  "Save PNG")
    ),
    plotOutput(id, height = height),
    uiOutput(paste0(id, "__cap"), class = "fig-note")
  )
}

###### E_02_04: Draw a Figure ##################################################
# Note: The last step out of every renderPlot: T_02_01c_draw_fn lifts the
#   caption into the store for E_02_03's note slot. The title is put back
#   as the builder folded it, to the width of its own card.

E_02_04_draw_fn <- function(p) {
  ttl <- p$labels$title
  p   <- T_02_01c_draw_fn(p)
  p$labels$title <- ttl
  p
}

###### E_02_05: Sidebar Controls ###############################################
# Note: The mode switch, then year and multiplier (both modes need them),
#   then the level, sort, measure and shock controls of the model only.

E_02_05_sidebar_lst <- sidebar(
  width = 340,
  radioButtons("mode", NULL, inline = TRUE,
               choices = B_03_21_modes_vec, selected = "model"),
  E_02_02_help_fn("mode"),
  selectInput("year", "Year of the Table", choices = C_02_03_years_vec,
              selected = C_02_17_defaults_lst$year),
  E_02_02_help_fn("year"),
  radioButtons("type", "Multiplier",
               choices = stats::setNames(
                 names(B_03_04_types_lst),
                 vapply(B_03_04_types_lst, `[[`, "", "label")
               ),
               selected = C_02_17_defaults_lst$type),
  E_02_02_help_fn("type"),
  conditionalPanel(
    "input.mode == 'model'",
    radioButtons("level", "Level of Detail", choices = B_03_12_levels_vec,
                 selected = C_02_17_defaults_lst$level),
    E_02_02_help_fn("level"),
    radioButtons("sort", "Sort By",
                 choices = c("Size" = "size", "Classification" = "group"),
                 selected = C_02_17_defaults_lst$sort),
    E_02_02_help_fn("sort"),
    radioButtons("measure", "Measure",
                 choices = stats::setNames(
                   names(B_03_03_measures_lst),
                   vapply(B_03_03_measures_lst, `[[`, "", "label")
                 ),
                 selected = C_02_17_defaults_lst$measure),
    E_02_02_help_fn("measure"),
    uiOutput("explain"),
    selectInput("product", "Product Receiving the Shock",
                choices = C_02_05_choices_fn(C_02_08_start_prd_df),
                selected = C_02_07_match_key_fn(
                  C_02_17_defaults_lst$product, C_02_08_start_prd_df)),
    E_02_02_help_fn("product"),
    tags$div(
      class = "ctl",
      tags$div(class = "ctl-label",
               HTML("Extra Final Demand (&euro; Million)")),
      tags$div(
        class = "ctl-row",
        tags$div(class = "ctl-slider",
                 sliderInput("size", NULL, min = -500, max = 1000,
                             value = C_02_17_defaults_lst$size, step = 10,
                             width = "100%")),
        tags$div(class = "ctl-box",
                 numericInput("size_box", NULL,
                              value = C_02_17_defaults_lst$size, step = 10,
                              min = -500, max = 1000, width = "100%"))
      )
    ),
    E_02_02_help_fn("size")
  ),
  conditionalPanel(
    "input.mode == 'project'",
    uiOutput("prj_explain")
  ),
  T_07_10b_sidebarqr_fn(B_04_02_qr_src_chr)
)

###### E_02_06: Worked-Example Presets #########################################
# Note: The toolkit's preset card (T_05_04 to T_05_07), grouped by the mode
#   radio; it appears in "Explore the Model" only. See CONVENTIONS.md 1.

E_02_06_presets_lst <- T_05_04_presets_fn(
  B_03_20_scenarios_lst, B_03_21_modes_vec,
  stage_word = "", stage_id = "mode"
)

#### E_03: Tabs and Page #######################################################
# Note: Static content for the tabs, then the page; plots and tables are
#   filled by F.

###### E_03_01: Cost a Project Tab #############################################
# Note: Left: the costing (template, lines, upload, assumptions). Right:
#   the results and the Excel download.

E_03_01_project_tab_lst <- layout_columns(
  col_widths = breakpoints(sm = 12, lg = c(5, 7)),
  tags$div(
    class = "prj",
    tags$div(class = "prj-card-title", "Project Costing"),
    tags$div(
      class = "ctl-row",
      tags$div(class = "ctl-slider",
               selectInput("tpl", "Start from a Template",
                           choices = c("Choose a template" = "",
                                       names(B_03_16_templates_lst)))),
      actionButton("tpl_load", "Load", class = "btn-sm btn-outline-primary")
    ),
    E_02_02_help_fn("tpl"),
    tags$div(class = "ctl-label mt-2", "Add a Line"),
    textInput("line_desc", NULL, placeholder = "Description, e.g. Site works"),
    selectInput("line_key", NULL,
                choices = C_02_05_choices_fn(C_02_08_start_prd_df)),
    tags$div(
      class = "ctl-row",
      numericInput("line_amount", HTML("Amount (&euro;m)"), value = 1,
                   min = 0, step = 0.1),
      numericInput("line_irish", "Bought in Ireland (%)", value = 100,
                   min = 0, max = 100, step = 5)
    ),
    tags$div(
      class = "ctl-row mb-2",
      actionButton("line_add", "Add Line", class = "btn-sm btn-primary"),
      actionButton("lines_clear", "Clear All",
                   class = "btn-sm btn-outline-secondary")
    ),
    uiOutput("lines_table"),
    fileInput("lines_upload", "Upload a Costing (Excel or CSV)",
              accept = c(".xlsx", ".xls", ".csv"),
              placeholder = "Same columns as the Project sheet"),
    accordion(
      open = FALSE,
      accordion_panel(
        "Tax Rates (Averages from the Data)",
        numericInput("tax_wage", "Tax on Wages: Income Tax, USC, PRSI (%)",
                     value = C_02_15_tax_rates_fn(C_02_03_years_vec[1])$wage,
                     min = 0, max = 60, step = 0.5),
        E_02_02_help_fn("tax_wage"),
        numericInput("tax_profit", "Tax on Profits (% of Net Surplus)",
                     value = C_02_15_tax_rates_fn(C_02_03_years_vec[1])$profit,
                     min = 0, max = 60, step = 0.5),
        E_02_02_help_fn("tax_profit"),
        uiOutput("tax_note")
      ),
      accordion_panel(
        "Supply Constraints",
        sliderInput("displace",
                    "Displacement of Indirect and Induced Effects (%)",
                    min = 0, max = 100, value = B_03_18_assume_lst$displace,
                    step = 5, width = "100%"),
        E_02_02_help_fn("displace"),
        selectizeInput("cap_keys", "Products at Capacity",
                       choices = C_02_05_choices_fn(C_02_08_start_prd_df),
                       selected = B_03_19_cap_keys_chr,
                       multiple = TRUE,
                       options = list(placeholder = "None: supply is elastic")),
        E_02_02_help_fn("cap_keys"),
        sliderInput("cap_pct",
                    "Spare Capacity in Those Products (% of Output)",
                    min = 0, max = 10, value = B_03_18_assume_lst$spare,
                    step = 0.5, width = "100%"),
        E_02_02_help_fn("cap_pct")
      )
    )
  ),
  tags$div(
    uiOutput("prj_tiles"),
    uiOutput("prj_story"),
    E_02_03_figure_fn("prj_effects", "Project Effects by Source", "340px"),
    E_02_03_figure_fn("prj_constraints", "Capacity and Displacement", "320px"),
    E_02_03_figure_fn("prj_where",
                      "Where the Project's Output Is Produced", "420px"),
    uiOutput("prj_exchequer"),
    E_02_01_download_fn("prj_download", "Download Results (Excel)",
                        class = "btn-primary mt-2")
  )
)

###### E_03_02: Model Equations ################################################
# Note: The model, the multipliers and the project extensions.

E_03_02_model_lst <- withMathJax(tags$table(
  class = "eq-table",
  tags$tr(tags$td(class = "eq-label", "Accounting Identity"),
          tags$td("\\(x = A x + f\\)"),
          tags$td(class = "eq-note",
                  "Each product's output goes either to other producers as",
                  "inputs (Ax) or to final demand (f).")),
  tags$tr(tags$td(class = "eq-label", "Technical Coefficients"),
          tags$td("\\(a_{ij} = z_{ij} / x_j\\)"),
          tags$td(class = "eq-note",
                  "Euros of product i needed per euro of product j. Assumed",
                  "fixed: no substitution, constant returns.")),
  tags$tr(tags$td(class = "eq-label", "Leontief Inverse"),
          tags$td("\\(x = (I - A)^{-1} f = L f\\)"),
          tags$td(class = "eq-note",
                  "Output needed to deliver final demand f, counting every",
                  "round of the supply chain.")),
  tags$tr(tags$td(class = "eq-label", "Round by Round"),
          tags$td("\\(L = I + A + A^2 + A^3 + \\cdots\\)"),
          tags$td(class = "eq-note",
                  HTML(paste(
                    "Round k is what the suppliers of round k &minus; 1 buy.",
                    "Like the Keynesian multiplier 1/(1 &minus; c), a",
                    "geometric series that converges because each round is",
                    "smaller. It is not the Keynesian expenditure",
                    "multiplier: the rounds here are inter-industry",
                    "purchases, not consumption out of income.")))),
  tags$tr(tags$td(class = "eq-label", "Direct, Indirect, Induced"),
          tags$td("\\(c_j,\\quad c'L e_j - c_j,\\quad c'L_2 e_j - c'L e_j\\)"),
          tags$td(class = "eq-note",
                  "For any per-euro coefficient c (output, wages, taxes):",
                  "the product itself, its supply chain, and the spending of",
                  "the wages earned along it.")),
  tags$tr(tags$td(class = "eq-label", "Type II"),
          tags$td("\\(A_2 = \\begin{pmatrix} A & h \\\\",
                  "w' & 0 \\end{pmatrix}\\)"),
          tags$td(class = "eq-note",
                  "Households as a sector: they sell labour (w, wages per euro",
                  "of output) and spend it on products (h, household spending",
                  "shares). The whole wage is spent; their spending on imports",
                  "and product taxes leaks out.")),
  tags$tr(tags$td(class = "eq-label", "Where Each Euro of Demand Ends Up"),
          tags$td("\\(w'L + g'L + n'L + t'L + m'L = \\iota'\\)"),
          tags$td(class = "eq-note",
                  "With Type I every euro of final demand ends up as wages,",
                  "operating surplus, net taxes on production, taxes on",
                  "products or imports: the shares add to exactly 1, which the",
                  "Where Each Euro of Demand Ends Up chart draws as 100%.")),
  tags$tr(tags$td(class = "eq-label", "Exchequer Return"),
          tags$td("\\(T = (t + n)'x + \\tau_w\\, w'x + \\tau_p\\, s'x\\)"),
          tags$td(class = "eq-note",
                  "Taxes recorded in the tables plus each year's average tax",
                  "rates on wages and on net operating surplus, from the",
                  "government accounts.")),
  tags$tr(tags$td(class = "eq-label", "Jobs"),
          tags$td("\\(e'L f,\\quad e_j = \\text{FTE}_j / x_j\\)"),
          tags$td(class = "eq-note",
                  "FTE job-years supported, with FTE per euro of output the",
                  "same for every product within a Eurostat industry.")),
  tags$tr(tags$td(class = "eq-label", "Capacity Limits"),
          tags$td("\\(x_U = (I - A_{UU})^{-1}(f_U + A_{UC}\\bar{x}_C)\\)"),
          tags$td(class = "eq-note",
                  "Products at capacity (C) are fixed at their cap; the rest",
                  "(U) are solved around them. Demand the capped products",
                  "cannot meet is imported (Miller and Blair, ch. 13).")),
  tags$tr(tags$td(class = "eq-label", "Displacement"),
          tags$td("\\(\\text{net} = \\text{direct} + (1 - d)",
                  "(\\text{total} - \\text{direct})\\)"),
          tags$td(class = "eq-note",
                  "With resources fully employed, a share d of the knock-on",
                  "effects only moves activity from elsewhere, as in the",
                  "additionality adjustments of project appraisal."))
))

###### E_03_03: Notation Column ################################################
# Note: One column of symbols: a heading and a table of symbol, meaning.

E_03_03_notation_fn <- function(title, rows) {
  tags$div(
    tags$div(class = "nota-head", HTML(title)),
    tags$table(class = "nota-table", lapply(rows, function(r) {
      tags$tr(tags$td(paste0("\\(", r[1], "\\)")), tags$td(HTML(r[2])))
    }))
  )
}

###### E_03_04: Notation #######################################################
# Note: Three columns of symbols: matrices and vectors, coefficients per
#   euro of output, and effects.

E_03_04_notation_lst <- withMathJax(layout_columns(
  col_widths = breakpoints(sm = 12, lg = c(4, 4, 4)),
  E_03_03_notation_fn("Matrices and Vectors", list(
    c("x", "Output by product"),
    c("f", "Final demand by product"),
    c("Z", "Flows between products (row sells to column)"),
    c("A", "Technical coefficients"),
    c("L", "Leontief inverse, Type I"),
    c("A_2, L_2", "With households as a sector (Type II)"),
    c("e_j", "&euro;1 of final demand for product j"),
    c("\\iota", "A vector of ones")
  )),
  E_03_03_notation_fn("Per &euro;1 of Output", list(
    c("v", "Gross value added"),
    c("w", "Compensation of employees"),
    c("g", "Gross operating surplus"),
    c("s", "Net operating surplus (profits)"),
    c("n", "Other taxes on production, less subsidies"),
    c("t", "Taxes on products"),
    c("m", "Imported inputs"),
    c("e", "FTE job-years (per &euro;1 million)"),
    c("h", "Household spending shares (per &euro; spent)")
  )),
  E_03_03_notation_fn("Effects and Assumptions", list(
    c("x_h", "Household income generated (Type II)"),
    c("\\tau_w, \\tau_p", "Average tax rates on wages and profits (data)"),
    c("\\bar{x}_C", "Capacity limit of products at capacity"),
    c("d", "Displacement share"),
    c("T", "Exchequer return")
  ))
))

###### E_03_05: Page ###########################################################
# Note: The full UI object passed to shinyApp().

E_03_05_app_ui_lst <- tagList(
  E_01_05_nav_fn(),
  page_sidebar(
  title = tags$div(
    class = "page-title-wrap",
    tags$div(
      class = "page-title-text",
      tags$h1(class = "bslib-page-title", "Input-Output Multipliers"),
      tags$div(class = "page-byline",
               tags$a(href = B_03_25_site_chr, target = "_blank",
                      tags$img(src = "sd-logo.png", alt = ""),
                      B_03_24_author_chr))),
    tags$div(
      class = "side-qr",
      tags$a(href = B_03_25_site_chr, target = "_blank",
             tags$img(src = B_04_02_qr_src_chr,
                      alt = paste("QR code for", B_03_25_site_chr))))
  ),
  window_title = paste("Input-Output Multipliers ·", B_03_24_author_chr),
  fillable = FALSE,
  theme    = E_01_01_theme_lst,
  sidebar  = E_02_05_sidebar_lst,
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "stylesheet",
              href = paste0("https://fonts.googleapis.com/css2?",
                            "family=IBM+Plex+Sans:wght@400;500;600;700",
                            "&display=swap")),
    tags$style(HTML(E_01_02_css_chr)),
    tags$style(HTML(T_05_07_preset_css_chr)),
    tags$script(src = paste0(
      "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.9/MathJax.js",
      "?config=TeX-AMS-MML_HTMLorMML")),
    tags$script(HTML(E_01_03_js_chr)),
    tags$script(HTML(E_01_04_save_js_chr)),
    tags$script(HTML(T_05_05_preset_js_chr))
  ),
  E_02_06_presets_lst,
  uiOutput("prompt"),
  navset_card_tab(
    id = "tab",
    nav_panel(
      "Multipliers", value = "mult",
      tags$div(class = "view-switch",
               radioButtons("view", NULL, inline = TRUE,
                            choices = c(
                              "Multipliers by Source" = "source",
                              "Where Each Euro of Demand Ends Up" = "split"
                            ),
                            selected = C_02_17_defaults_lst$view)),
      E_02_02_help_fn("view"),
      uiOutput("mult_plot_ui")
    ),
    nav_panel(
      "Demand Shock", value = "shock",
      uiOutput("shock_story"),
      uiOutput("shock_tiles"),
      layout_columns(
        col_widths = breakpoints(sm = 12, xl = c(6, 6)),
        E_02_03_figure_fn("where_plot",
                          "Where the Output Is Produced", "470px"),
        E_02_03_figure_fn("rounds_plot", "The Spending Rounds", "470px")
      )
    ),
    nav_panel(
      "Trends", value = "time",
      uiOutput("time_note"),
      layout_columns(
        col_widths = breakpoints(sm = 12, xl = c(6, 6)),
        E_02_03_figure_fn("time_product", "This Product Over Time", "480px"),
        E_02_03_figure_fn("time_group", "Sectors Over Time", "480px")
      )
    ),
    nav_panel(
      "Industry Interactions", value = "table",
      uiOutput("heatmap_ui"),
      E_02_03_figure_fn("suppliers", "Who Supplies This Product", "420px")
    ),
    nav_panel(
      "Costing", value = "project",
      E_03_01_project_tab_lst
    ),
    nav_panel(
      "Equations", value = "maths",
      navset_pill(
        id = "maths_tab",
        nav_panel("Model", tags$div(class = "mt-3", E_03_02_model_lst)),
        nav_panel("Notation", tags$div(class = "mt-3", E_03_04_notation_lst)),
        nav_panel("Classification", tags$div(class = "mt-3",
                                             uiOutput("classification"))),
        nav_panel("Data", tags$div(class = "mt-3",
                                               uiOutput("caveats")))
      )
    )
  ),
  tags$footer(
    class = "credit",
    "Built by ", tags$a(href = B_03_25_site_chr, target = "_blank",
                        B_03_24_author_chr),
    " for ", B_03_26_course_chr, ". Data: CSO input-output tables, ",
    paste(sort(C_02_03_years_vec), collapse = ", "), ".",
    " Version ", B_03_27_version_chr, ".",
    " ", tags$a(href = B_03_28_repo_chr, target = "_blank",
               "Source and download on GitHub"), "."
  )
))

################################################################################
## F: Server ###################################################################
################################################################################
# Note: Reads the controls, assembles the run, draws.

#### F_01: Server Function #####################################################
# Note: Everything reactive lives here.

###### F_01_01: Server #########################################################
# Note: Local objects are plain snake_case.

F_01_01_app_server_fn <- function(input, output, session) {

  num_or <- function(v, d) {
    if (is.null(v) || length(v) == 0 || is.na(v)) d else v
  }
  # HTML path: euro sign and minus as entities; the figures use D_01_02
  eur    <- function(x, digits = NULL) {
    if (is.null(digits)) digits <- if (abs(x) < 10) 1 else 0
    paste0(if (x < 0) "&minus;&euro;" else "&euro;",
           formatC(abs(x), format = "f", digits = digits, big.mark = ","),
           "m")
  }
  fte    <- function(x) {
    formatC(x, format = "f", digits = if (abs(x) < 10) 1 else 0,
            big.mark = ",")
  }
  amt    <- function(m, v) {
    if (identical(m, "jobs")) paste(fte(v), "FTE job-years") else eur(v)
  }
  has_jobs <- reactive(any(lt_now()$coef$jobs > 0))

  # --- The reading notes under the figures ------------------------------------
  # E_02_03_figure_fn registers each figure as the UI is built; the two whose
  # cards come from renderUI are registered here.
  for (fig in c("mult_plot", "heatmap")) T_07_07e_add_fn(fig)
  T_07_07d_cap_fn(output)

  # --- The year and level in view ---------------------------------------------
  res_now <- reactive({
    req(input$year)
    C_02_02_res_lst[[input$year]]
  })
  lt_now  <- reactive(res_now()$lt)
  prd_now <- reactive(res_now()$io$products)
  level   <- reactive(if (is.null(input$level)) "product" else input$level)
  # The costing opens on the starting template
  lines_rv <- reactiveVal(local({
    t0 <- B_03_16_templates_lst[[B_03_17_start_tpl_chr]]
    C_02_13_lines_fn(t0$desc, t0$key, t0$amount, t0$irish,
                     C_02_08_start_prd_df)
  }))
  pending_rv <- reactiveVal(NULL)

  # --- Mode: which half of the app is on view ---------------------------------
  # The other mode's tabs are hidden, not rebuilt, so every output keeps its id.
  model_tabs <- c("mult", "shock", "time", "table")
  observeEvent(input$mode, {
    if (identical(input$mode, "project")) {
      nav_show("tab", "project")
      for (tb in model_tabs) nav_hide("tab", tb)
      nav_select("tab", "project")
    } else {
      for (tb in model_tabs) nav_show("tab", tb)
      nav_hide("tab", "project")
      if (identical(isolate(input$tab), "project")) nav_select("tab", "mult")
    }
  })

  # --- Worked-example presets -------------------------------------------------
  # One observer per preset; set_scenario_fn is the only place that sets the
  # loaded preset, so the button marker and card header cannot drift apart.
  # A preset that changes the year parks its product and level in pending_rv
  # for the year observer, which rebuilds both menus.
  scenario <- reactiveVal(B_03_22_start_scn_chr)

  scn_now <- reactive({
    k <- scenario()
    if (is.null(k) || !k %in% names(B_03_20_scenarios_lst)) NULL
    else B_03_20_scenarios_lst[[k]]
  })

  set_scenario_fn <- function(key) {
    scenario(if (is.null(key)) "" else key)
    session$sendCustomMessage("dgPreset", if (is.null(key)) "" else key)
    invisible(NULL)
  }

  load_preset_fn <- function(key) {
    if (is.null(key) || !key %in% names(B_03_20_scenarios_lst)) {
      return(invisible(NULL))
    }
    sc  <- B_03_20_scenarios_lst[[key]]
    set <- sc$set
    set_scenario_fn(key)
    if (!identical(isolate(input$year), set$year)) pending_rv(set)
    updateSelectInput(session, "year", selected = set$year)
    updateRadioButtons(session, "level", selected = set$level)
    updateRadioButtons(session, "sort", selected = set$sort)
    updateRadioButtons(session, "type", selected = set$type)
    updateRadioButtons(session, "measure", selected = set$measure)
    updateRadioButtons(session, "view", selected = set$view)
    updateSelectInput(session, "product",
                      selected = C_02_07_match_key_fn(set$product, prd_now()))
    updateSliderInput(session, "size", value = set$size)
    updateNumericInput(session, "size_box", value = set$size)
    nav_select("tab", sc$tab)
    invisible(NULL)
  }

  lapply(names(B_03_20_scenarios_lst), function(key) {
    observeEvent(input[[paste0("preset_", key)]],
                 load_preset_fn(key), ignoreInit = TRUE)
  })

  # Load the starting example once; see CONVENTIONS.md 2
  opened_lgl <- FALSE
  observe({
    req(input$year, input$mode)
    if (opened_lgl) return()
    load_preset_fn(B_03_22_start_scn_chr)
    opened_lgl <<- TRUE
  })

  output$preset_title <- renderUI({
    T_05_06_preset_title_fn(scn_now(), input$mode, B_03_21_modes_vec,
                            stage_word = "")
  })

  # A new year has its own products: keep the same one, else the closest
  # (C_02_07), in every menu and in the costing
  observeEvent(input$year, {
    pend <- isolate(pending_rv())
    prd <- prd_now()
    ch  <- C_02_05_choices_fn(prd)
    cur <- if (is.null(pend$product)) isolate(input$product) else pend$product
    if (is.null(cur) || !nzchar(cur)) cur <- B_03_13_shock_lst$key
    updateSelectInput(session, "product", choices = ch,
                      selected = C_02_07_match_key_fn(cur, prd))
    lk <- isolate(input$line_key)
    updateSelectInput(session, "line_key", choices = ch,
                      selected = if (!is.null(lk) && nzchar(lk)) {
                        C_02_07_match_key_fn(lk, prd)
                      })
    caps <- isolate(input$cap_keys)
    updateSelectizeInput(session, "cap_keys", choices = ch,
                         selected = unique(vapply(caps, C_02_07_match_key_fn,
                                                  "", prd = prd)))
    ln <- isolate(lines_rv())
    if (nrow(ln) > 0) {
      ln$key <- vapply(ln$key, C_02_07_match_key_fn, "", prd = prd)
      lines_rv(ln)
    }
    tr <- C_02_15_tax_rates_fn(input$year)
    updateNumericInput(session, "tax_wage", value = tr$wage)
    updateNumericInput(session, "tax_profit", value = tr$profit)
    updateRadioButtons(session, "level",
                       choices = C_02_16_level_choices_fn(input$year),
                       selected = if (is.null(pend$level)) isolate(level()) else
                         pend$level)
    pending_rv(NULL)
  }, ignoreInit = TRUE)

  # --- Shock size: slider and box in step -------------------------------------
  # The box holds the exact value; the slider follows to its nearest step.
  size_pushed <- new.env()
  observeEvent(input$size, {
    b <- input$size_box
    if (!is.null(b) && !is.na(b) && abs(input$size - b) <= 5) return()
    size_pushed$v <- utils::tail(c(size_pushed$v, input$size), 3)
    updateNumericInput(session, "size_box", value = input$size)
  }, ignoreInit = TRUE)

  box_typed <- debounce(reactive(input$size_box), 500)
  observeEvent(box_typed(), {
    b <- box_typed()
    if (is.null(b) || is.na(b)) return()
    hit <- which(abs(size_pushed$v - b) < 1e-9)
    if (length(hit) > 0) {
      size_pushed$v <- size_pushed$v[-seq_len(hit[1])]
      return()
    }
    if (abs(input$size - b) > 5) updateSliderInput(session, "size", value = b)
  }, ignoreInit = TRUE)

  size_now <- reactive({
    b <- input$size_box
    if (is.null(b) || is.na(b)) input$size else max(-500, min(1000, b))
  })

  # --- The chosen product and its group ---------------------------------------
  j_now <- reactive({
    j <- match(input$product, prd_now()$cso_key_str)
    if (length(j) == 0 || is.na(j)) 1L else j
  })
  code_at <- function(lv) res_now()$code_of[[lv]][j_now()]
  name_now <- reactive({
    p <- prd_now()[j_now(), ]
    paste(p$cso_group_cat, p$cso_short_str)
  })

  # --- Everything the controls decide, assembled in one place -----------------
  # A pure function of a list of control values, so the same code runs over
  # the live controls and over the loaded example's values for the ghost.
  # by: output by product; by_m and rounds: in the chosen measure;
  # group_time: every division or section across the years.
  assemble_fn <- function(v) {
    res <- C_02_02_res_lst[[v$year]]
    lt  <- res$lt
    prd <- res$io$products
    lvl <- if (is.null(v$level)) "product" else v$level
    # The closest product in this year's table (C_02_07)
    key <- C_02_07_match_key_fn(v$product, prd)
    j   <- match(key, prd$cso_key_str)
    if (length(j) == 0 || is.na(j)) j <- 1L
    size <- max(-500, min(1000, v$size))

    f  <- numeric(lt$n)
    f[j] <- size
    cf  <- lt$coef[[v$measure]]
    tot <- lapply(stats::setNames(nm = B_03_10_model_measures_vec),
                  function(m) C_01_03_effects_fn(lt, f, m))
    shk <- list(f = f, by = C_01_05_shock_fn(lt, f),
                by_m = C_01_05_shock_fn(lt, f, cf), tot = tot,
                rounds = C_01_06_rounds_fn(lt, f, B_03_14_sizes_lst$rounds,
                                           cf))

    # Trends: one product across the years, against the output-weighted average
    pdf <- C_02_04_time_lst$products
    pdf$value <- pdf[[paste0("mlt_", v$measure, "_", v$type, "_val")]]
    pdf$year  <- pdf$cso_year_int
    avg <- do.call(rbind, lapply(split(pdf, pdf$year), function(g) {
      data.frame(year = g$year[1],
                 value = sum(g$value * g$cso_output_amt) /
                   sum(g$cso_output_amt))
    }))
    one <- do.call(rbind, lapply(split(pdf, pdf$year), function(g) {
      k <- C_02_07_match_key_fn(key, g, fallback = FALSE)
      if (is.na(k)) NULL else g[g$cso_key_str == k, ]
    }))
    one$exact <- one$cso_key_str == key

    # Trends: the division or section family; the product level borrows the
    # division one
    lvg  <- if (identical(lvl, "product")) "group" else lvl
    gdf  <- C_02_04_time_lst[[lvg]]
    gdf$value <- gdf[[paste0("mlt_", v$measure, "_", v$type, "_val")]]
    ghl  <- res$code_r2[[lvg]][j]
    glab <- res$levels_r2[[lvg]]$multipliers

    list(year = v$year, type = v$type, measure = v$measure, level = lvl,
         sort = v$sort, view = v$view, product = key, size = size, j = j,
         res = res, lt = lt, prd = prd,
         shock = shk,
         time = list(one = one, avg = avg),
         group_time = list(df = gdf, highlight = ghl,
                           title = glab$cso_label_str[
                             glab$cso_group_cat == ghl][1]))
  }

  inputs_now <- reactive({
    req(input$year, input$measure, input$type, input$product, input$sort)
    assemble_fn(list(year = input$year, type = input$type,
                     measure = input$measure, level = level(),
                     sort = input$sort, view = input$view,
                     product = input$product, size = size_now()))
  })

  shock <- reactive(inputs_now()$shock)

  # --- The ghost: every figure at the worked example's own settings ----------
  # Reference values are the loaded preset's, or the defaults (C_02_17) when
  # none is loaded, run through assemble_fn like the live values.
  ref_vals <- reactive({
    k <- scenario()
    if (is.null(k) || !k %in% names(B_03_20_scenarios_lst)) {
      return(C_02_17_defaults_lst)
    }
    utils::modifyList(C_02_17_defaults_lst, B_03_20_scenarios_lst[[k]]$set)
  })

  ref_inputs <- reactive(assemble_fn(ref_vals()))

  # The measure sets the unit of every ghosted panel, so a reference in
  # another measure is dropped; T_02_03b_ghost_off_fn drops the ghost while
  # live and reference agree.
  ghost_ref <- reactive({
    ref <- ref_inputs()
    if (is.null(ref) || !identical(ref$measure, inputs_now()$measure)) {
      return(NULL)
    }
    ref
  })

  # --- Prompt and explanation -------------------------------------------------
  output$prompt <- renderUI({
    req(input$tab)
    pr <- B_03_15_prompts_lst[[input$tab]]
    tags$div(class = "prompt",
             tags$div(pr$what),
             tags$div(class = "mt-1", tags$strong("Try. "), pr$try))
  })

  output$prj_explain <- renderUI({
    req(input$type)
    tags$div(
      class = "mode-note",
      tags$div(class = "story-key", B_03_04_types_lst[[input$type]]$label),
      HTML(B_03_04_types_lst[[input$type]]$help),
      tags$div(class = "mt-2",
               paste("The costing uses the year and the multiplier above.",
                     "Everything else it needs is on the tab itself: the",
                     "lines, the tax rates and the supply constraints."))
    )
  })

  # The story is the paragraph alone; the card header names the example
  output$scenario_story <- renderUI({
    T_05_02_story_fn(scn_now(), NULL, NULL)
  })

  output$explain <- renderUI({
    tags$div(
      class = "story",
      tags$div(class = "story-key",
               B_03_03_measures_lst[[input$measure]]$label),
      HTML(B_03_03_measures_lst[[input$measure]]$help),
      tags$div(class = "story-key mt-2", B_03_04_types_lst[[input$type]]$label),
      HTML(B_03_04_types_lst[[input$type]]$help)
    )
  })

  # --- Tab 1: multipliers -----------------------------------------------------
  output$mult_plot_ui <- renderUI({
    k <- nrow(res_now()$levels[[level()]]$multipliers)
    px <- max(420, B_03_14_sizes_lst$row_px * k + 150)
    E_02_03_figure_fn("mult_plot", "Multipliers", paste0(px, "px"))
  })

  # No ghost: the rows change with the level and the year
  output$mult_plot <- renderPlot({
    lv <- level()
    df <- res_now()$levels[[lv]]$multipliers
    E_02_04_draw_fn(if (identical(input$view, "split")) {
      D_02_02_split_plot_fn(df, input$type, code_at(lv), input$sort,
                            input$year)
    } else {
      D_02_01_mult_plot_fn(df, input$measure, input$type, code_at(lv),
                           input$sort, input$year)
    })
  })

  # --- Tab 2: the shock -------------------------------------------------------
  # The toolkit tile passes its value through HTML(), so the entities render
  tile <- function(label, value, note, key = FALSE) {
    T_04_01_tile_fn(label, value, note, class = if (key) "key" else "")
  }

  # The chosen measure is always a tile, picked out in green
  output$shock_tiles <- renderUI({
    tt    <- shock()$tot
    ty    <- input$type
    size  <- size_now()
    shown <- unique(c("output", "gva", "wages", "jobs", "exchequer",
                      "imports", input$measure))
    if (!has_jobs()) shown <- setdiff(shown, "jobs")
    tags$div(class = "stat-row mb-2", lapply(shown, function(m) {
      v    <- tt[[m]][[ty]]
      note <- if (size == 0) "" else if (m == "jobs") {
        paste(formatC(v / size, format = "f", digits = 1), "per &euro;1m")
      } else {
        paste0(formatC(v / size, format = "f", digits = 2), " per &euro;1")
      }
      tile(B_03_03_measures_lst[[m]]$effect,
           if (m == "jobs") fte(v) else eur(v), note,
           identical(m, input$measure))
    }))
  })

  output$shock_story <- renderUI({
    tt   <- shock()$tot
    by   <- shock()$by
    size <- size_now()
    if (size == 0) {
      return(tags$div(class = "narrative",
                      "Set a non-zero shock in the sidebar to see its",
                      "effects."))
    }
    out1 <- tt$output$t1
    out2 <- tt$output$t2
    tags$div(
      class = "narrative",
      HTML(paste0(
        "In ", input$year, ", an extra ", eur(size),
        " of final demand for <strong>", name_now(), "</strong> raises ",
        "output across the economy by <strong>", eur(out1), "</strong> ",
        "(Type I): the ", eur(size), " <strong>direct</strong> effect plus ",
        eur(out1 - size), " of <strong>indirect</strong> effects along the ",
        "supply chain (", eur(sum(by$shk_first_amt)), " of it from direct ",
        "suppliers). When the workers involved spend their wages, the ",
        "<strong>induced</strong> effect adds ", eur(out2 - out1),
        " (Type II total ", eur(out2), "). Of the Type I total, ",
        eur(tt$gva$t1), " is value added in Ireland, ",
        eur(tt$exchequer$t1), " comes back to the Exchequer in taxes and ",
        eur(tt$imports$t1),
        " leaks abroad as imports.",
        if (has_jobs()) {
          paste0(" It supports about <strong>", fte(tt$jobs$t1), " FTE ",
                 "job-years</strong> (Type I; ", fte(tt$jobs$t2),
                 " with Type II): full-time jobs for one year each, not ",
                 "permanent jobs.")
        },
        if (!identical(input$measure, "output")) {
          ms <- input$measure
          paste0(" The charts below read the same shock in <strong>",
                 tolower(B_03_03_measures_lst[[ms]]$label), "</strong>: ",
                 amt(ms, tt[[ms]]$direct), " directly, ",
                 amt(ms, tt[[ms]]$t1), " with the supply chain (Type I) and ",
                 amt(ms, tt[[ms]]$t2), " once the induced effect is added ",
                 "(Type II).")
        }
      ))
    )
  })

  # The household leak of Type II is in the tiles, not in these rows
  output$where_plot <- renderPlot({
    by  <- shock()$by_m
    out <- C_02_12_by_level_fn(res_now(), by$shk_direct_amt,
                               by$shk_indirect_amt, by$shk_induced_amt,
                               level())
    # No ghost: the rows are the top products of this run
    E_02_04_draw_fn(D_03_01_where_plot_fn(
      out, input$type, B_03_14_sizes_lst$top_n,
      paste0("Where It Is Generated: ",
             B_03_03_measures_lst[[input$measure]]$effect),
      input$measure, input$year
    ))
  })

  # Ghosted: the running totals
  output$rounds_plot <- renderPlot({
    g <- ghost_ref()
    E_02_04_draw_fn(D_03_02_rounds_plot_fn(
      shock()$rounds, input$type, input$measure, input$year,
      ref = if (is.null(g)) NULL else list(rounds = g$shock$rounds,
                                           type = g$type)))
  })

  # --- Tab 3: cost a project --------------------------------------------------
  observeEvent(input$tpl_load, {
    req(nzchar(input$tpl))
    t <- B_03_16_templates_lst[[input$tpl]]
    lines_rv(C_02_13_lines_fn(t$desc, t$key, t$amount, t$irish, prd_now()))
  })

  observeEvent(input$line_add, {
    req(input$line_key, is.finite(num_or(input$line_amount, NA)))
    new <- C_02_13_lines_fn(input$line_desc, input$line_key,
                            input$line_amount, num_or(input$line_irish, 100),
                            prd_now())
    lines_rv(rbind(lines_rv(), new))
    updateTextInput(session, "line_desc", value = "")
  })

  observeEvent(input$lines_clear, lines_rv(C_02_14_empty_lines_df))

  observeEvent(input$del_line, {
    ln <- lines_rv()
    i  <- as.integer(input$del_line)
    if (!is.na(i) && i >= 1 && i <= nrow(ln)) lines_rv(ln[-i, , drop = FALSE])
  })

  observeEvent(input$lines_upload, {
    up  <- input$lines_upload
    ext <- tolower(tools::file_ext(up$name))
    raw <- tryCatch({
      if (ext == "csv") {
        utils::read.csv(up$datapath, check.names = FALSE,
                        stringsAsFactors = FALSE)
      } else {
        sh <- readxl::excel_sheets(up$datapath)
        as.data.frame(readxl::read_excel(
          up$datapath, sheet = if ("Project" %in% sh) "Project" else 1
        ))
      }
    }, error = function(e) NULL)
    if (is.null(raw) || nrow(raw) == 0) {
      showNotification("Could not read that file.", type = "error")
      return()
    }
    col <- function(pat) {
      nm <- names(raw)[grepl(pat, tolower(names(raw)))]
      if (length(nm) > 0) raw[[nm[1]]] else rep(NA, nrow(raw))
    }
    ln <- C_02_13_lines_fn(col("desc"), col("key"), col("amount"),
                           col("ireland|irish"), prd_now(),
                           name = col("^product"))
    if (nrow(ln) == 0) {
      showNotification(paste("No lines found. Use the columns of the Project",
                             "sheet in a downloaded file."), type = "error")
      return()
    }
    lines_rv(ln)
    showNotification(paste("Loaded", nrow(ln), "lines."), type = "message")
  })

  output$lines_table <- renderUI({
    ln <- lines_rv()
    if (nrow(ln) == 0) {
      return(tags$div(class = "text-muted small mb-2",
                      "No lines yet: load a template, add lines or upload",
                      "a costing."))
    }
    prd <- prd_now()
    nm  <- paste(prd$cso_group_cat, prd$cso_short_str)[
      match(ln$key, prd$cso_key_str)]
    tot <- sum(ln$amount)
    irl <- if (tot != 0) sum(ln$amount * ln$irish / 100) / tot * 100 else 100
    rows <- lapply(seq_len(nrow(ln)), function(i) {
      tags$tr(
        tags$td(ln$desc[i]), tags$td(nm[i]),
        tags$td(class = "num", formatC(ln$amount[i], format = "f", digits = 2)),
        tags$td(class = "num", paste0(round(ln$irish[i]), "%")),
        tags$td(tags$a(
          class = "del-line", title = "Remove this line",
          onclick = sprintf(paste0("Shiny.setInputValue('del_line', %d, ",
                                   "{priority: 'event'})"), i),
          HTML("&times;")
        ))
      )
    })
    tags$table(
      class = "lines-table mb-2",
      tags$tr(tags$th("Description"), tags$th("Product"),
              tags$th(HTML("&euro;m")),
              tags$th("Irish"), tags$th("")),
      rows,
      tags$tr(class = "total", tags$td("Total"), tags$td(""),
              tags$td(class = "num", formatC(tot, format = "f", digits = 2)),
              tags$td(class = "num", paste0(round(irl), "%")), tags$td(""))
    )
  })

  prj_opt <- reactive(list(
    type       = input$type,
    tax_wage   = num_or(input$tax_wage, 0),
    tax_profit = num_or(input$tax_profit, 0),
    displace   = num_or(input$displace, 0),
    cap_keys   = if (is.null(input$cap_keys)) character(0) else input$cap_keys,
    cap_pct    = num_or(input$cap_pct, 0)
  ))

  output$tax_note <- renderUI({
    m <- res_now()$io$meta
    if (is.null(m$tax_wage_pct)) return(NULL)
    bn <- function(k) paste0("&euro;", formatC(as.numeric(m[[k]]) / 1000,
                                              format = "f", digits = 1),
                             "bn")
    tags$div(
      class = "text-muted small",
      HTML(paste0(
        input$year, " averages from the government accounts (Eurostat) ",
        "over this table's wages and profits. Wages: household income taxes ",
        bn("tax_hh_income_amt"), " plus social contributions ",
        bn("tax_social_amt"), " = ",
        formatC(as.numeric(m$tax_wage_pct), format = "f", digits = 1),
        "% of compensation of employees (a little high: household income ",
        "tax also falls on pensions, rents and self-employment). Profits: ",
        "corporation tax ", bn("tax_corp_amt"), " = ",
        formatC(as.numeric(m$tax_profit_pct), format = "f", digits = 1),
        "% of net operating surplus. You can change either. They set the",
        "Exchequer return only: the induced effect spends gross wages."
      ))
    )
  })

  lt_prj <- reactive({
    opt <- prj_opt()
    C_01_02_leontief_fn(res_now()$io,
                        c(wage = opt$tax_wage, profit = opt$tax_profit))
  })

  prj <- reactive({
    ln <- lines_rv()
    req(nrow(ln) > 0)
    C_01_12_project_fn(
      lt_prj(), prd_now(),
      data.frame(cso_key_str = ln$key, amount_amt = ln$amount,
                 irish_pct = ln$irish, stringsAsFactors = FALSE),
      prj_opt(), B_03_10_model_measures_vec
    )
  })

  prj_tot <- function(p, m) {
    p$effects$prj_total_amt[p$effects$prj_measure_str == m]
  }

  output$prj_tiles <- renderUI({
    if (nrow(lines_rv()) == 0) {
      return(tags$div(class = "narrative",
                      "Cost a project on the left: load a template, add",
                      "lines by product, or upload a costing. The results",
                      "appear here."))
    }
    p    <- prj()
    cost <- p$cost
    per  <- function(v) {
      if (cost == 0) "" else
        paste0(formatC(v / cost, format = "f", digits = 2),
               " per &euro;1 spent")
    }
    exq <- prj_tot(p, "exchequer")
    tags$div(
      class = "stat-row mb-2",
      tile("Project Cost", eur(cost, 1),
           paste0(eur(p$direct_imports, 1), " bought abroad")),
      # Gross value added leads: it is the contribution to GDP
      tile("Extra Gross Value Added", eur(prj_tot(p, "gva"), 1),
           per(prj_tot(p, "gva")), TRUE),
      tile("Extra Gross Output", eur(prj_tot(p, "output"), 1),
           paste0(per(prj_tot(p, "output")),
                  if (cost == 0) "" else " &middot; not a GDP contribution")),
      tile("Extra Wages", eur(prj_tot(p, "wages"), 1),
           per(prj_tot(p, "wages"))),
      if (has_jobs()) {
        tile("Jobs (FTE Job-Years)", fte(prj_tot(p, "jobs")),
             if (cost == 0) "" else {
               paste0(formatC(prj_tot(p, "jobs") / cost, format = "f",
                              digits = 1),
                      " per &euro;1m spent &middot; their pay is in GVA above")
             })
      },
      tile("Exchequer Return", eur(exq, 1),
           if (cost == 0) "" else {
             paste0(round(100 * exq / cost), "% of the cost")
           })
    )
  })

  output$prj_story <- renderUI({
    req(nrow(lines_rv()) > 0)
    p   <- prj()
    opt <- prj_opt()
    con <- p$constrained
    gva <- function(col) con[[col]][con$prj_measure_str == "gva"]
    txt <- paste0(
      "The ", eur(p$cost, 1), " project raises gross value added by ",
      eur(prj_tot(p, "gva"), 1), " and gross output by ",
      eur(prj_tot(p, "output"), 1), " (",
      B_03_04_types_lst[[opt$type]]$label, ")",
      if (has_jobs()) {
        paste0(" and supports about ", fte(prj_tot(p, "jobs")),
               " FTE job-years (one full-time job for one year each; not ",
               "permanent jobs)")
      },
      ". The Exchequer gets back ", eur(prj_tot(p, "exchequer"), 1),
      " in taxes (", round(100 * prj_tot(p, "exchequer") / p$cost),
      "% of the cost; breakdown below).",
      " Gross value added is the contribution to GDP; gross output counts ",
      "intermediate sales at every stage they pass through, so it is the ",
      "larger figure and is not a contribution to GDP. Both are gross of ",
      "the capital used up in producing them.",
      if (has_jobs()) {
        paste0(" The job-years are the employment embodied in that same ",
               "output rather than a further benefit: the pay they earn is ",
               "already counted inside gross value added.")
      }
    )
    if (length(opt$cap_keys) > 0 && p$unmet > 0.005) {
      txt <- paste0(
        txt, " With capacity limits, value added falls to ",
        eur(gva("prj_capacity_amt"), 1), ": ", eur(p$unmet, 1), " of ",
        "demand cannot be met at home and is imported or crowds out other ",
        "buyers."
      )
    } else if (length(opt$cap_keys) > 0) {
      txt <- paste(txt, "The capacity limits do not bind at this scale.")
    }
    if (opt$displace > 0) {
      txt <- paste0(
        txt, " With ", opt$displace, "% displacement of the indirect and ",
        "induced effects, net value added is ", eur(gva("prj_net_amt"), 1),
        "."
      )
    }
    # HTML(), because eur() writes entities
    tags$div(class = "narrative", HTML(txt))
  })

  # No ghost on the costing figures: the presets set no project lines
  output$prj_effects <- renderPlot({
    E_02_04_draw_fn(D_06_01_project_plot_fn(prj()$effects, input$type,
                                            input$year))
  })

  output$prj_constraints <- renderPlot({
    E_02_04_draw_fn(D_06_02_constraint_plot_fn(prj()$constrained, input$year))
  })

  output$prj_where <- renderPlot({
    by  <- prj()$by_product
    out <- C_02_12_by_level_fn(res_now(), by$prj_direct_amt,
                               by$prj_indirect_amt, by$prj_induced_amt,
                               level())
    E_02_04_draw_fn(D_03_01_where_plot_fn(
      out, input$type, B_03_14_sizes_lst$top_n,
      "Where the Output Is Produced", "output", input$year))
  })

  output$prj_exchequer <- renderUI({
    p   <- prj()
    exq <- p$exchequer
    pct <- function(v) if (p$cost == 0) "" else
      paste0(formatC(100 * v / p$cost, format = "f", digits = 1), "%")
    rows <- lapply(seq_len(nrow(exq)), function(i) {
      tags$tr(tags$td(exq$prj_item_str[i]),
              tags$td(class = "num", HTML(eur(exq$prj_amount_amt[i], 2))),
              tags$td(class = "num", pct(exq$prj_amount_amt[i])))
    })
    tot <- sum(exq$prj_amount_amt)
    tags$div(
      class = "mt-2",
      tags$div(class = "prj-card-title", "Exchequer Return"),
      tags$table(
        class = "lines-table",
        tags$tr(tags$th("Source"), tags$th(HTML("&euro;m")),
                tags$th("Share of Cost")),
        rows,
        tags$tr(class = "total", tags$td("Total"),
                tags$td(class = "num", HTML(eur(tot, 2))),
                tags$td(class = "num", pct(tot)))
      )
    )
  })

  output$prj_download <- downloadHandler(
    filename = function() paste0("io-project-", input$year, ".xlsx"),
    content  = function(file) {
      ln  <- lines_rv()
      prd <- prd_now()
      res <- res_now()
      opt <- prj_opt()
      lab <- function(k) {
        paste(prd$cso_group_cat, prd$cso_short_str)[match(k,
                                                           prd$cso_key_str)]
      }
      sheets <- list(
        Project = data.frame(
          Description = ln$desc, Product = lab(ln$key), `NACE Key` = ln$key,
          `Amount (EUR m)` = ln$amount, `Bought in Ireland (%)` = ln$irish,
          check.names = FALSE
        )
      )
      if (nrow(ln) > 0) {
        p    <- prj()
        eff  <- p$effects
        mlab <- c(B_03_07_project_measures_vec, nos = "Net Operating Surplus",
                  prodn = "Other Taxes on Production, Net",
                  prodtax = "Taxes on Products",
                  taxes = "Taxes on Products and Production, Net",
                  jobs = "Jobs (FTE job-years, not permanent jobs)")
        unit <- function(m) ifelse(m == "jobs", "FTE job-years", "EUR m")
        sheets$Summary <- data.frame(
          Measure = unname(mlab[eff$prj_measure_str]),
          Units = unit(eff$prj_measure_str),
          Direct = eff$prj_direct_amt,
          Indirect = eff$prj_indirect_amt,
          Induced = eff$prj_induced_amt,
          Total = eff$prj_total_amt,
          `Per EUR 1 of Cost (Jobs: per EUR 1m)` = eff$prj_total_amt / p$cost,
          check.names = FALSE
        )
        sheets$Exchequer <- data.frame(
          Source = c(p$exchequer$prj_item_str, "Total"),
          `EUR m` = c(p$exchequer$prj_amount_amt,
                      sum(p$exchequer$prj_amount_amt)),
          check.names = FALSE
        )
        sheets$Exchequer$`Share of Cost (%)` <- 100 *
          sheets$Exchequer$`EUR m` / p$cost
        con <- p$constrained
        sheets$`Supply Constraints` <- data.frame(
          Measure = unname(mlab[con$prj_measure_str]),
          Units = unit(con$prj_measure_str),
          `Standard Input-Output` = con$prj_standard_amt,
          `With Capacity Limits` = con$prj_capacity_amt,
          `Net of Displacement` = con$prj_net_amt,
          check.names = FALSE
        )
        by <- p$by_product
        sheets$`By Product` <- data.frame(
          Section = paste(prd$cso_nsec_cat, prd$cso_nsec_str),
          Division = paste(prd$cso_group_cat, prd$cso_group_str),
          Product = prd$cso_short_str, `NACE Key` = prd$cso_key_str,
          `Direct Output (EUR m)` = by$prj_direct_amt,
          `Indirect Output (EUR m)` = by$prj_indirect_amt,
          `Induced Output (EUR m)` = by$prj_induced_amt,
          `Total Output (EUR m)` = by$prj_direct_amt + by$prj_indirect_amt +
            by$prj_induced_amt,
          `Jobs (FTE Job-Years)` = res$lt$coef$jobs *
            (by$prj_direct_amt + by$prj_indirect_amt + by$prj_induced_amt),
          check.names = FALSE
        )
        sheet_of <- c(group = "By Division", section = "By Section")
        for (lv in names(sheet_of)) {
          out <- C_02_12_by_level_fn(res, by$prj_direct_amt,
                                     by$prj_indirect_amt, by$prj_induced_amt,
                                     lv)
          sheets[[sheet_of[[lv]]]] <- data.frame(
            Group = out$label, `Direct Output (EUR m)` = out$direct,
            `Indirect Output (EUR m)` = out$indirect,
            `Induced Output (EUR m)` = out$induced,
            `Total Output (EUR m)` = out$direct + out$indirect + out$induced,
            check.names = FALSE
          )
        }
      }
      sheets$Assumptions <- data.frame(
        Setting = c("Year of the table", "Classification", "Multiplier",
                    "Tax on wages (%)", "Tax on profits (%)",
                    "Tax rates from the data (wages, profits, %)",
                    "Displacement (%)", "Products at capacity",
                    "Spare capacity in those products (%)", "Data source",
                    "Induced effect", "Jobs", "Jobs source", "Units",
                    "Created", "Tool"),
        Value = c(input$year, C_02_09_class_vec[[input$year]],
                  B_03_04_types_lst[[opt$type]]$label, opt$tax_wage,
                  opt$tax_profit,
                  paste(unlist(C_02_15_tax_rates_fn(input$year)),
                        collapse = ", "),
                  opt$displace,
                  if (length(opt$cap_keys) > 0) {
                    paste(lab(opt$cap_keys), collapse = "; ")
                  } else "None",
                  opt$cap_pct, res$io$meta$source,
                  paste("Households spend their gross wages, in the pattern",
                        "of household consumption in the table (an upper",
                        "bound on the induced effect)"),
                  paste("FTE job-years: hours worked / 1,800. One FTE",
                        "job-year is one full-time job for one year, not a",
                        "permanent job. Jobs per euro are the same for every",
                        "product within a Eurostat industry."),
                  if (is.null(res$io$meta$jobs_source)) "None" else
                    res$io$meta$jobs_source,
                  "EUR million at basic prices of the table's year",
                  format(Sys.time(), "%Y-%m-%d %H:%M"),
                  paste("Input-Output Multipliers app by", B_03_24_author_chr,
                        B_03_25_site_chr)),
        stringsAsFactors = FALSE
      )
      sheets$`Product List` <- data.frame(
        Section = paste(prd$cso_nsec_cat, prd$cso_nsec_str),
        Division = paste(prd$cso_group_cat, prd$cso_group_str),
        Product = prd$cso_short_str, `NACE Key` = prd$cso_key_str,
        `CSO Label` = prd$cso_product_str, check.names = FALSE
      )
      writexl::write_xlsx(sheets, file)
    }
  )

  # --- Tab 4: trends across the years ----------------------------------------
  time_data <- reactive(inputs_now()$time)

  output$time_note <- renderUI({
    if (length(C_02_03_years_vec) == 1) {
      return(tags$div(class = "narrative",
                      "Only one year of data is loaded. Run",
                      "data-raw/prepare_cso_io.R to add the other years."))
    }
    one  <- time_data()$one
    near <- one[!one$exact, ]
    tags$div(
      class = "narrative",
      "The CSO groups products differently in each release, from 48",
      "products in 1998 to 62 in 2022. Left: filled points are",
      tags$strong(name_now()), "itself; hollow points are the closest",
      "product in years that group it differently",
      HTML(paste0(
        if (nrow(near) > 0) {
          paste0(" (", paste0(near$year, ": ", near$cso_short_str,
                              collapse = "; "), ")")
        },
        ".")),
      "Right: every division (NACE A*38) or section (A*21) in grey,",
      "the chosen product's in blue.",
      if (length(C_02_10_rev1_years_vec) > 0) {
        paste(paste(C_02_10_rev1_years_vec, collapse = " and "),
              "use the older NACE Rev 1.1; on the right their products are",
              "mapped to the nearest NACE Rev 2 group.")
      }
    )
  })

  # Ghosted: both lines
  output$time_product <- renderPlot({
    td <- time_data()
    g  <- ghost_ref()
    E_02_04_draw_fn(D_05_01_time_product_fn(
      td$one, td$avg, name_now(), input$measure, C_02_03_years_vec,
      ref = if (is.null(g)) NULL else g$time))
  })

  # Ghosted: the chosen group's line only
  output$time_group <- renderPlot({
    gt <- inputs_now()$group_time
    g  <- ghost_ref()
    E_02_04_draw_fn(D_05_02_time_group_fn(
      gt$df, gt$highlight, gt$title, input$measure,
      ref = if (is.null(g)) NULL else g$group_time))
  })

  # --- Tab 5: input coefficients ----------------------------------------------
  output$heatmap_ui <- renderUI({
    k <- nrow(res_now()$levels[[level()]]$a)
    E_02_03_figure_fn("heatmap", "Input Coefficients", paste0(
      if (k > 45) 14 * k + 330 else if (k > 25) 20 * k + 300 else 600, "px"
    ))
  })

  output$heatmap <- renderPlot({
    lv  <- level()
    lvl <- res_now()$levels[[lv]]
    lab <- stats::setNames(lvl$multipliers$cso_label_str,
                           lvl$multipliers$cso_group_cat)
    # No ghost: the year swaps the whole table
    E_02_04_draw_fn(D_04_01_heatmap_fn(lvl$a, lab, code_at(lv), input$year))
  })

  output$suppliers <- renderPlot({
    prd <- prd_now()
    # No ghost: another product's column shares no rows with this one
    E_02_04_draw_fn(D_04_02_suppliers_fn(
      lt_now(), paste(prd$cso_group_cat, prd$cso_short_str),
      j_now(), 10L, input$year))
  })

  # --- Tab 6: equations, notation, classification ----------------------------
  output$classification <- renderUI({
    prd <- prd_now()
    rev <- C_02_09_class_vec[[input$year]]
    blocks <- lapply(unique(prd$cso_nsec_cat), function(s) {
      g <- prd[prd$cso_nsec_cat == s, ]
      first <- !duplicated(g$cso_group_cat)
      tagList(
        tags$tr(class = "cls-sec",
                tags$td(colspan = "5", paste(s, g$cso_nsec_str[1]))),
        lapply(seq_len(nrow(g)), function(i) {
          tags$tr(
            tags$td(if (first[i]) paste(g$cso_group_cat[i],
                                        g$cso_group_str[i])),
            tags$td(tags$div(g$cso_short_str[i]),
                    tags$div(class = "text-muted small",
                             tools::toTitleCase(g$cso_product_str[i]))),
            tags$td(g$cso_nace_str[i]),
            tags$td(class = "num", HTML(eur(g$cso_output_amt[i]))),
            tags$td(class = "num",
                    if (is.null(g$cso_fte_amt) || is.na(g$cso_fte_amt[i])) {
                      ""
                    } else fte(g$cso_fte_amt[i]))
          )
        })
      )
    })
    tagList(
      tags$p(class = "eq-note",
             paste0("How the CSO's product categories for ", input$year,
                    " (", rev, ") map to ",
                    if (rev == "NACE Rev 1.1") {
                      "divisions (the A31 subsections) and sections (A17)"
                    } else {
                      "divisions (NACE A*38) and sections (NACE A*21)"
                    },
                    ". A product that spans several groups gets a combined ",
                    "code, e.g. CF+CI for pharma with electronics. Jobs are ",
                    "FTE job-years in the year (Eurostat employment, shared ",
                    "evenly by output within each Eurostat industry).")),
      tags$table(
        class = "cls-table",
        tags$tr(tags$th("Division"), tags$th("CSO Product Category"),
                tags$th("NACE Divisions"), tags$th("Output"),
                tags$th("Jobs (FTE)")),
        blocks
      )
    )
  })

  output$caveats <- renderUI({
    tagList(
      tags$p(class = "eq-note",
             HTML(paste(
               "CSO symmetric input-output tables of domestic product flows",
               "at basic prices, &euro; million. Each release groups products",
               "differently, so products are matched across years by their",
               "NACE divisions; 1998 and 2005 use the older NACE Rev 1.1,",
               "mapped to the nearest Rev 2 group. The tables reproduce the",
               "CSO's own published output multipliers (1998 to within the",
               "rounding of its &euro;1m cells; 2005 has no published inverse",
               "but matches the CSO's import shares)."))),
      tags$p(class = "eq-note",
             "Multipliers assume fixed coefficients and spare capacity: no",
             "crowding out, no price changes. The Cost a Project tab relaxes",
             "this with capacity limits and displacement. Ireland's",
             "multinational sectors (pharma, ICT) are large but buy few",
             "Irish inputs and repatriate profits, so their multipliers are",
             "low and GVA overstates Irish income."),
      tags$p(class = "eq-note",
             tags$strong("T Households as Employers"),
             "(paying a childminder or cleaner directly) has no inputs:",
             "its whole output is wages, so its output and value-added",
             "multipliers are exactly 1."),
      tags$p(class = "eq-note",
             "Exchequer returns: taxes on products and on production come",
             "from the tables. Taxes on wages and profits use each year's",
             "average rates from the government accounts (Eurostat",
             "gov_10a_taxag): taxes on household income plus social",
             "contributions per euro of compensation of employees, and",
             "corporation tax per euro of net operating surplus. Average",
             "rates suit spending that scales up the economy; the tax on a",
             "marginal extra euro can differ."),
      tags$p(class = "eq-note",
             tags$strong("The induced effect spends gross wages:"),
             "households are assumed to spend the whole wage bill, in the",
             "same pattern as household consumption in the table (so part of",
             "it leaks to imports and VAT), in the same year. No tax, no",
             "saving and no delay. This is the standard Type II closure, and",
             "the one in Sam's workbook. It is an upper bound: income tax,",
             "USC and PRSI take about a third of wages, and households save",
             "some of the rest. Read the other way round, it assumes the tax",
             "is spent again too, though as if the government spent it the",
             "way households do."),
      tags$p(class = "eq-note",
             tags$strong("Jobs are FTE job-years,"),
             "not permanent jobs: hours worked (Eurostat nama_10_a64_e,",
             "domestic concept) divided by 1,800 hours. A project supporting",
             "100 FTE job-years might employ 100 people full time for a year,",
             "or 20 for five years. Employment is published for about 60",
             "industries, so jobs per euro of output are assumed the same for",
             HTML(paste(
               "every CSO product within an industry. They are per &euro;1m",
               "at each year's prices, so they fall over time with prices and",
               "productivity. T Households as Employers has very little",
               "recorded output for its employment, so its jobs multiplier is",
               "an outlier."))),
      tags$div(class = "nota-head", "Sources"),
      tags$ul(class = "eq-note", lapply(C_02_11_sources_vec, tags$li)),
      tags$p(class = "eq-note",
             "Employment and tax receipts: Eurostat, nama_10_a64_e and",
             "gov_10a_taxag (Ireland).")
    )
  })
}

################################################################################
## G: Run ######################################################################
################################################################################
# Note: Launch.

#### G_01: Launch ##############################################################
# Note: Returns the app object.

###### G_01_01: Build App ######################################################
# Note: UI from E, server from F.

shinyApp(ui = E_03_05_app_ui_lst, server = F_01_01_app_server_fn)

#--------------------------------- Script End ---------------------------------#
