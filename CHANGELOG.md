# Changelog

All notable changes to this app. Versions follow [Semantic Versioning](https://semver.org/):
MAJOR for a change to the model or its notation, MINOR for new features
(a stage, a worked example, a figure), PATCH for fixes and wording.
Each release is tagged in git as `vX.Y.Z` and shown in the app footer.

## [1.0.6] - 2026-09-28

### App
- Card headers in the blue used for headings, not body grey.

## [1.0.5] - 2026-09-28

### App
- Cards have no border or header rule: figures, equations and stories sit
  on the page separated by whitespace alone.

## [1.0.4] - 2026-09-28

### App
- Cards, panels, tiles and buttons are square with no shadow: they organise
  the page rather than decorate it.

## [1.0.3] - 2026-09-28

### App
- The QR code returns to the foot of the sidebar, with the name and site
  address, alongside the small one in the title bar.

## [1.0.2] - 2026-09-28

### App
- No figure carries a title or subtitle inside the image; the card header
  and the caption under it name and explain the figure (CONVENTIONS.md 6).
- Figures are drawn on a white ground, so the image sits flat in its card
  instead of showing as a tinted tile.

## [1.0.1] - 2026-09-28

### App
- The In Words tab lays out its three columns at fixed widths, so an
  equation no longer collapses to one term per line beside its note.
- The preset card no longer doubles the word "Stage" in front of a stage
  name that already carries it.

## [1.0.0] - 2026-09-28

First public release as a standalone repository.

### Model
- Open Leontief model on the CSO's domestic input-output tables for 1998,
  2005, 2010, 2011, 2015, 2020, 2021 and 2022, with Type I and Type II
  (households endogenous) inverses following Miller and Blair (2009) ch. 2
  and 6.
- Multipliers for output, value added, wages, operating surplus, taxes,
  imports and FTE job-years, split into direct, indirect and induced
  effects, and the five-way split of each euro of final demand.
- A demand shock by product and round by round; multipliers aggregated to
  NACE sections and divisions, and mapped to NACE Rev 2 across years.
- Costed projects with an Exchequer account, capacity limits (the mixed
  model of Miller and Blair ch. 13) and displacement.

### App
- Two modes: Explore the Model (Multipliers, Demand Shock, Trends, Industry
  Interactions) and Cost a Project (Costing), with Equations, Notation,
  Classification and Data tabs in both.
- Five worked examples above the tabs, one loaded at the start, and five
  project templates; uploads from Excel or CSV and an Excel download.
- Ghost lines on the rounds and trends figures showing the loaded worked
  example alongside the live controls.
- Save PNG on every figure, with the reading note set under the card.
