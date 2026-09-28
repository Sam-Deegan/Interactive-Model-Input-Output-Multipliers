# Interactive Model: Input-Output Multipliers

A Shiny app for teaching input-output multipliers from the CSO's
supply-use and input-output tables for Ireland. Built by
[Sam Deegan](https://sam-deegan.com) for ECON42550 Macroeconomics,
University College Dublin.

**Try it in the browser (nothing to install):**
https://sam-deegan.com/toy-models/io-multiplier/

Current version: **1.0.3** (see [CHANGELOG.md](CHANGELOG.md)). The version
is shown in the app footer; releases are tagged `vX.Y.Z`.

## What it does

The app has two modes, switched at the top of the sidebar. *Explore the
Model* compares sectors; *Cost a Project* prices one project line by line.
Each mode has its own tabs:

| Tab | Mode | What it shows |
|---|---|---|
| Multipliers | Explore | Every sector's multiplier side by side, split into direct, indirect and induced effects, or the wages, profits, taxes and imports that each euro of demand ends up as |
| Demand Shock | Explore | One shock (€x million of extra demand for one product) traced through the economy: where the extra output is produced, and how it builds up round by round |
| Trends | Explore | One product's multiplier across the eight CSO tables, 1998 to 2022, against every division or section |
| Industry Interactions | Explore | The technical-coefficient matrix `A` as a heatmap, and the inputs of one product |
| Costing | Cost a Project | A costing from a template or an upload, with tax rates, capacity limits and displacement, the Exchequer return, and an Excel download |
| Equations | Both | The model, the notation, the classification of each year's products and the data caveats |

Every table can be read at three levels of detail: sections (NACE A\*21),
divisions (NACE A\*38) and the CSO's own product categories (48 to 62 a
year). Multipliers can be counted in output, gross value added, wages,
gross operating surplus, the Exchequer return, FTE job-years or imports,
as Type I (direct and indirect) or Type II (adding the induced spending of
wages). The app opens on a worked example, with every control live; five
worked examples sit above the tabs, and five project templates open the
costing.

## Run it locally

1. Install [R](https://cran.r-project.org/) (4.1 or later) and, ideally,
   [RStudio](https://posit.co/download/rstudio-desktop/).
2. Install the packages once:

   ```r
   install.packages(c("shiny", "bslib", "ggplot2", "readxl", "writexl"))
   ```

3. Open `app.R` in RStudio and click **Run App**, or from R in this folder:

   ```r
   shiny::runApp()
   ```

Equations are typeset with MathJax from a CDN, so they need an internet
connection; everything else runs offline.

## Files

```
app.R              the app: settings and text (section B), precomputed
                   results (C_02), interface (E), server (F)
R/io_model.R       the model: Leontief inverses, multipliers, shocks,
                   rounds, capacity limits, costed projects. Sources on
                   its own, so slides can reuse it.
R/io_plots.R       the figures, one function per chart
R/toolkit.R        layout and helpers shared with the other toy-model apps
data/              the CSO tables as three CSV files (see Data)
www/               logo
README.md          this file
CHANGELOG.md       version history
CONVENTIONS.md     how the figures and worked examples are laid out
LICENSE            CC BY-NC-ND 4.0
```

All text on screen (worked examples, prompts, control help, project
templates) is in section `B_03` of `app.R`, so it can be edited without
touching the rest.

## Data

The app bundles a snapshot of the CSO's symmetric input-output tables of
domestic product flows at basic prices, in € million, for 1998, 2005, 2010,
2011, 2015, 2020, 2021 and 2022, in three files read by `C_01_01`:

```
data/io_products.csv   one row per product and year: the CSO product
                       category, its NACE key and divisions, section,
                       output, household consumption, compensation of
                       employees, taxes on products, other taxes on
                       production, value added, operating surplus,
                       consumption of fixed capital, imports and (where
                       available) FTE job-years
data/io_flows.csv      the domestic flow matrix Z in long form: year,
                       selling product, buying product, amount
data/io_meta.csv       one row per key and year: total household spending
                       and its import and product-tax content (which
                       close the Type II model), the classification
                       (NACE Rev 2, or Rev 1.1 for 1998 and 2005), the
                       source table and the year's average tax rates on
                       wages and profits
```

Employment is Eurostat `nama_10_a64_e` (hours worked divided by 1,800,
spread evenly by output over the CSO products within each industry); the
tax rates are from Eurostat `gov_10a_taxag`. Each release groups products
differently, so products are matched across years by their NACE divisions.
The data is a snapshot taken when the app was built; it is not updated
automatically.

## The model

The open Leontief model is the workhorse of impact analysis: a fixed
technical-coefficient matrix says what each product buys from every other
per euro of output, and inverting it gives the total output needed to meet
a final demand, supply chain and all. It is the model of Miller and Blair's
*Input-Output Analysis* (2009, chapters 2 and 6), and it is what the CSO's
published output multipliers are computed from. All money amounts are €
million at each year's basic prices.

```
Accounting:   x = A x + f,       a_ij = z_ij / x_j
Leontief:     x = (I − A)^{−1} f = L f
Rounds:       L = I + A + A² + A³ + ⋯
Effects:      direct  c_j,   Type I  c'L e_j − c_j,   Type II  c'L₂ e_j − c'L e_j
Type II:      A₂ = [ A  h ; w'  0 ]
Euro split:   w'L + g'L + n'L + t'L + m'L = ι'
Exchequer:    T = (t + n)'x + τ_w w'x + τ_p s'x
Jobs:         e'L f,   e_j = FTE_j / x_j
Capacity:     x_U = (I − A_UU)^{−1} (f_U + A_UC x̄_C)
Displacement: net = direct + (1 − d)(total − direct)
```

**The accounting identity** says each product's output `x` goes either to
other producers as inputs (`Ax`) or to final demand (`f`). `a_ij` is the
euros of product `i` needed per euro of product `j`, assumed fixed: no
substitution, constant returns.

**The Leontief inverse** `L` gives the output needed to deliver a final
demand `f`, counting every round of the supply chain. Written out, `L` is a
geometric series: round `k` is what the suppliers of round `k − 1` buy, and
it converges because each round is smaller than the last. It is not the
Keynesian expenditure multiplier: the rounds are inter-industry purchases,
not consumption out of income.

**Effects by source.** For any per-euro coefficient `c` (output, value
added, wages, taxes, imports, jobs), the direct effect of €1 of demand for
product `j` is `c_j`, the indirect effect is its supply chain, and the
induced effect is the spending of the wages earned along it. **Type II**
adds households to the table as one more sector: they sell labour (`w`,
wages per euro of output) and spend it on products (`h`, household
spending shares). The whole wage bill is spent, so the induced effect is an
upper bound; the part of household spending that goes on imports and
product taxes leaks out.

**Where each euro ends up.** With Type I, every euro of final demand ends
as wages (`w`), gross operating surplus (`g`), other taxes on production
net of subsidies (`n`), taxes on products (`t`) or imports (`m`): the
shares add to exactly one. A share can be negative where subsidies exceed
production taxes.

**The Exchequer return** `T` is the taxes recorded in the tables plus each
year's average rates on wages (`τ_w`) and on net operating surplus (`τ_p`).
**Jobs** are FTE job-years, with FTE per euro of output the same for every
product within a Eurostat industry.

**Supply constraints.** Products at capacity (`C`) are fixed at their cap
`x̄_C` and the rest (`U`) are solved around them, the mixed model of Miller
and Blair (chapter 13); demand the capped products cannot meet is imported.
Displacement then removes a share `d` of the knock-on effects, as in the
additionality adjustments of project appraisal.

The model is solved in closed form: `solve(diag(n) - a)` gives `L` and
`L₂`, and the capacity solution fixes the capped products one round at a
time until none is over its cap. The matrices are at most 63 × 63, so
everything is instant.

**What the tabs show with it**

- *Multipliers* The largest producers in the table (pharma with
  electronics, computer services) have the smallest multipliers, because
  most of their euro leaves at once as imported inputs. Switch the view to
  see the euro split, and the level of detail to see the pattern by
  section.
- *Demand Shock* €100m of demand for construction, which buys most of its
  inputs at home, against the same for pharma; the rounds figure shows the
  Type I series all but finished by round 4 and the induced series still
  adding. Education's multiplier is almost all induced.
- *Trends* Construction's import content since 1998, and which industries
  became more or less connected to Irish suppliers.
- *Industry Interactions* Who buys how much from whom: read down a
  product's column of `A`.
- *Costing* A school, a greenway, social housing, hospital equipment or an
  arts festival; how much of the cost comes back to the Exchequer, how many
  FTE job-years it supports, and how much of the multiplier survives when
  construction is at capacity and 25% of the knock-on effects only displace
  activity elsewhere.

**Where it departs from the textbook.** The Type II closure spends gross
wages, so it is the standard textbook closure and an upper bound: income
tax, USC and PRSI take about a third of wages and households save some of
the rest. The Exchequer return applies average rather than marginal tax
rates. Jobs are spread evenly by output within each Eurostat industry,
which is a stronger assumption than the tables make anywhere else. The
model has fixed coefficients and, outside the Costing tab, spare capacity
everywhere: no crowding out, no price changes, no timing (every round sits
inside the one year of the table). Output is gross, so it counts
intermediate sales at every stage and is not a contribution to GDP; the
app puts gross value added first for that reason. Ireland's multinational
sectors repatriate profits, so even value added overstates Irish income
there.

## References

- Miller, R. E. and Blair, P. D. (2009). *Input-Output Analysis:
  Foundations and Extensions*, 2nd ed. Cambridge University Press.
  Chapters 2, 6 and 13.
- Central Statistics Office. *Supply and Use and Input-Output Tables for
  Ireland*, 1998 to 2022 releases.
- Eurostat. `nama_10_a64_e` (employment by industry) and `gov_10a_taxag`
  (tax receipts), Ireland.
- Regulation (EC) No 1893/2006 establishing NACE Rev 2.

## Licence

© Sam Deegan. Released under
[CC BY-NC-ND 4.0](https://creativecommons.org/licenses/by-nc-nd/4.0/):
free to use and share for teaching with attribution; not for commercial use
or redistribution in modified form.
